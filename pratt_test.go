package pratt

import (
	"fmt"
	"testing"
)

// In order to test the Pratt parser, we need a lexer and a token type. These are
// both very hacky and are only used for testing, as the main object of the
// tests are to verify that the Pratt parser is working correctly, not that the
// lexer and token types are awesome. The two types are:
//
// - ContrivedToken: a simple struct that implements the Token interface. It
//   keeps track of the token's kind and its index in the input stream. This
//   is needed because the Pratt parser needs something that implements the
//   Tokenable interface.
// - ContrivedLexer: a simple state machine that returns a token on each call to
//   Next(). It also implements other methods needed by the Lexable interface.
//
// After these are defined we declare a simple struct (SimplePrattParser) with
// no fields that implements the Parsable interface. This is needed by the
// Pratt parser which calls the BindPower, ParsePrefix, and ParseInfix methods
// on the Parsable object.
//
// Finally, after all of this ceremony, we can write a simple test that
// verifies that the Pratt parser is working correctly.

type ContrivedToken struct {
	idx  uint
	kind string
}

func NewContrivedToken(kind string, idx uint) ContrivedToken {
	return ContrivedToken{kind: kind, idx: idx}
}
func (t ContrivedToken) Kind() string {
	return t.kind
}
func (t ContrivedToken) IsEOF() bool {
	return t.kind == "EOF"
}

type lexerState struct {
	tokens []ContrivedToken
	pos    int
}

type ContrivedLexer struct {
	state *lexerState
}

func NewContrivedLexer(tokens ...string) ContrivedLexer {
	tkns := make([]ContrivedToken, len(tokens))
	for i, t := range tokens {
		tkns[i] = NewContrivedToken(t, uint(i))
	}
	return ContrivedLexer{state: &lexerState{tokens: tkns}}
}

func (l ContrivedLexer) Next() ContrivedToken {
	if l.state.pos >= len(l.state.tokens) {
		return NewContrivedToken("EOF", 999)
	}
	t := l.state.tokens[l.state.pos]
	l.state.pos++
	return t
}

func (l ContrivedLexer) Peek() ContrivedToken {
	if l.state.pos >= len(l.state.tokens) {
		return NewContrivedToken("EOF", 999)
	}
	return l.state.tokens[l.state.pos]
}

func (l ContrivedLexer) MarkRead(t ContrivedToken) {
	for i, tkn := range l.state.tokens {
		if tkn.idx == t.idx {
			l.state.pos = i + 1
			return
		}
	}
}

type MyPrattParser = PrattParser[float64, ContrivedToken, ContrivedLexer]
type MyInfixContext = InfixContext[float64, ContrivedToken, ContrivedLexer]
type MyPrefixContext = PrefixContext[float64, ContrivedToken, ContrivedLexer]
type MyBinaryExpressionContext = BinaryExpressionContext[float64, ContrivedToken]

func NewSimplePrattParser() MyPrattParser {
	p := NewPrattParser[float64, ContrivedToken, ContrivedLexer]()

	var bp uint = 10
	// For purposes of testing, we only support the tokens "1", "2", "3", as the numbers 1, 2, and 3.
	// This is such a dirty hack, but it's only for testing...write real lexers if you use this library
	p.SetBindPower("1", bp)
	p.AddPrefixHandler("1", func(ctx MyPrefixContext) (*float64, error) {
		f := 1.0
		return &f, nil
	})
	p.SetBindPower("2", bp)
	p.AddPrefixHandler("2", func(ctx MyPrefixContext) (*float64, error) {
		f := 2.0
		return &f, nil
	})
	p.SetBindPower("3", bp)
	p.AddPrefixHandler("3", func(ctx MyPrefixContext) (*float64, error) {
		f := 3.0
		return &f, nil
	})

	p.SetBindPower("(", bp)
	p.AddPrefixHandler("(", func(ctx MyPrefixContext) (*float64, error) {
		result, err := p.ParseExpression(ctx.Lexer, 0)
		if err != nil {
			return nil, err
		}
		if ctx.Lexer.Peek().Kind() != ")" {
			return nil, fmt.Errorf("expected ')', got %v", ctx.Lexer.Peek().Kind())
		}
		ctx.Lexer.Next()
		return result, nil
	})

	bp += 10
	p.DefineBinaryOperator("+", bp, func(ctx MyBinaryExpressionContext) float64 {
		return ctx.Left + ctx.Right
	})

	bp += 10
	p.DefineBinaryOperator("*", bp, func(ctx MyBinaryExpressionContext) float64 {
		return ctx.Left * ctx.Right
	})

	return p
}

func parseExpression(l ContrivedLexer, p MyPrattParser) (*float64, error) {
	return ParseExpression[float64, ContrivedToken, ContrivedLexer](l, p, 0)
}

func TestParser(t *testing.T) {
	l := NewContrivedLexer("1", "+", "2", "*", "3")
	p := NewSimplePrattParser()

	result, err := parseExpression(l, p)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if *result != 7 {
		t.Errorf("expected 7, got %v", *result)
	}

	l = NewContrivedLexer("1", "*", "2", "+", "3")
	result, err = parseExpression(l, p)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if *result != 5 {
		t.Errorf("expected 5, got %v", *result)
	}

	l = NewContrivedLexer("(", "1", "+", "2", ")", "*", "3")
	result, err = parseExpression(l, p)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if *result != 9 {
		t.Errorf("expected 9, got %v", *result)
	}
}
