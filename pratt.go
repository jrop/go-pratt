package pratt

import "fmt"

type Tokenable interface {
	Kind() string
	IsEOF() bool
}

type Lexable[T Tokenable] interface {
	Next() T
	Peek() T
	MarkRead(T)
}

// Throughout this file, we'll use the following rule-of-thumb:
// - for fallible functions that might return an error, they return (*N, error)
//   so that the type of N does not have to be known (we can return nil, err)
// - for callback-style functions that recieve `N`, there is no need for that
//   to be a pointer, so it is left as a value-type

type PrefixContext[N any, T Tokenable, L Lexable[T]] struct {
	Parser PrattParser[N, T, L]
	Lexer  L
	Token  T
}

type InfixContext[N any, T Tokenable, L Lexable[T]] struct {
	Parser PrattParser[N, T, L]
	Lexer  L
	Left   N
	Token  T
}

type Parseable[N any, T Tokenable, L Lexable[T]] interface {
	BindPower(left N, t T) uint
	ParsePrefix(lex L, t T) (*N, error)
	ParseInfix(lex L, left N, t T) (*N, error)
}

func ParseExpression[N any, T Tokenable, L Lexable[T]](lex L, p Parseable[N, T, L], rbp uint) (*N, error) {
	t := lex.Next()
	left, err := p.ParsePrefix(lex, t)
	if err != nil {
		return left, err
	}

	// while the next precedence is greater than
	// the current precedence, keep going for
	// example, say rbp is `+`, and the next token
	// is `*`, we can handle that in the current
	// stack, otherwise we return and let a
	// higher-up-in-the-stack operation handle the
	// parsing:
	t = lex.Peek()
	for rbp < p.BindPower(*left, t) {
		lex.MarkRead(t)
		left, err = p.ParseInfix(lex, *left, t)
		if err != nil {
			return left, err
		}
		t = lex.Peek() // peek before the next loop iteration
	}
	return left, nil
}

type prattParserData[N any, T Tokenable, L Lexable[T]] struct {
	precedenceLevels map[string]uint
	prefixHandlers   map[string]func(PrefixContext[N, T, L]) (*N, error)
	infixHandlers    map[string]func(InfixContext[N, T, L]) (*N, error)
}
type PrattParser[N any, T Tokenable, L Lexable[T]] struct {
	data *prattParserData[N, T, L]
}

func NewPrattParser[N any, T Tokenable, L Lexable[T]]() PrattParser[N, T, L] {
	return PrattParser[N, T, L]{
		data: &prattParserData[N, T, L]{
			precedenceLevels: make(map[string]uint),
			prefixHandlers:   make(map[string]func(PrefixContext[N, T, L]) (*N, error)),
			infixHandlers:    make(map[string]func(InfixContext[N, T, L]) (*N, error)),
		},
	}
}

func (p PrattParser[N, T, L]) GetBindPower(left N, t T) uint {
	if level, ok := p.data.precedenceLevels[t.Kind()]; ok {
		return level
	}
	return 0
}

func (p *PrattParser[N, T, L]) SetBindPower(tokenKind string, level uint) {
	p.data.precedenceLevels[tokenKind] = level
}

func (p PrattParser[N, T, L]) SetStopToken(tokenKind string) {
	p.SetBindPower(tokenKind, 0)
}

func (p PrattParser[N, T, L]) AddPrefixHandler(tokenKind string, handler func(PrefixContext[N, T, L]) (*N, error)) {
	p.data.prefixHandlers[tokenKind] = handler
}

func (p PrattParser[N, T, L]) AddInfixHandler(tokenKind string, handler func(InfixContext[N, T, L]) (*N, error)) {
	p.data.infixHandlers[tokenKind] = handler
}

func (p PrattParser[N, T, L]) DefineBinaryOperator(tokenKind string, level uint, binaryExpressionCreator func(left N, t T, right N) N) {
	p.SetBindPower(tokenKind, level)
	p.AddInfixHandler(tokenKind, func(ctx InfixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, ctx.Parser, level)
		if err != nil {
			return nil, err
		}
		var result N = binaryExpressionCreator(ctx.Left, ctx.Token, *right)
		return &result, nil
	})
}

func (p PrattParser[N, T, L]) DefineBinaryOperatorRassoc(tokenKind string, level uint, binaryExpressionCreator func(left N, t T, right N) N) {
	p.SetBindPower(tokenKind, level)
	p.AddInfixHandler(tokenKind, func(ctx InfixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, ctx.Parser, level-1)
		if err != nil {
			return nil, err
		}
		var result N = binaryExpressionCreator(ctx.Left, ctx.Token, *right)
		return &result, nil
	})
}

func (p PrattParser[N, T, L]) DefineUnaryOperator(tokenKind string, level uint, unaryExpressionCreator func(t T, right N) N) {
	p.SetBindPower(tokenKind, level)
	p.AddPrefixHandler(tokenKind, func(ctx PrefixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, ctx.Parser, level)
		if err != nil {
			return nil, err
		}
		var result N = unaryExpressionCreator(ctx.Token, *right)
		return &result, nil
	})
}

func (p PrattParser[N, T, L]) ParseList(
	lex L,
	isOpener func(t T) bool,
	isCloser func(t T) bool,
	isSeparator func(t T) bool,
	elementParser func(parser PrattParser[N, T, L], lex L) (*N, error),
) ([]N, error) {
	var result []N

	// check that lex.Peek().Kind() is in openers:
	t := lex.Peek()
	if !isOpener(t) {
		return nil, fmt.Errorf("expected opener, got %s", t.Kind())
	}
	lex.MarkRead(t)

	for {
		t = lex.Peek()
		if isCloser(t) {
			break
		}

		// parse element:
		element, err := elementParser(p, lex)
		if err != nil {
			return nil, err
		}
		result = append(result, *element)

		// check for separator:
		t = lex.Peek()
		if isSeparator(t) {
			lex.MarkRead(t)
			continue
		} else {
			break
		}
	}

	// expect a closer:
	t = lex.Peek()
	if !isCloser(t) {
		return nil, fmt.Errorf("expected closer, got %s", t.Kind())
	}
	lex.MarkRead(t)

	return result, nil
}

// ParseListSimple: takes similar parameters as ParseList, but each parameter is not a predicate, but a single token kind:
func (p PrattParser[N, T, L]) ParseListSimple(
	lex L,
	opener string,
	closer string,
	separator string,
	elementParser func(parser PrattParser[N, T, L], lex L) (*N, error),
) ([]N, error) {
	return p.ParseList(
		lex,
		func(t T) bool { return t.Kind() == opener },
		func(t T) bool { return t.Kind() == closer },
		func(t T) bool { return t.Kind() == separator },
		elementParser,
	)
}

// Implement Parseable[N, T] interface for PrattParser[N, T]:
func (p PrattParser[N, T, L]) BindPower(left N, t T) uint {
	return p.GetBindPower(left, t)
}

func (p PrattParser[N, T, L]) ParsePrefix(lex L, t T) (*N, error) {
	if handler, ok := p.data.prefixHandlers[t.Kind()]; ok {
		ctx := PrefixContext[N, T, L]{Parser: p, Lexer: lex, Token: t}
		return handler(ctx)
	}
	return nil, fmt.Errorf("unexpected token in prefix posision: %s", t.Kind())
}

func (p PrattParser[N, T, L]) ParseInfix(lex L, left N, t T) (*N, error) {
	if handler, ok := p.data.infixHandlers[t.Kind()]; ok {
		ctx := InfixContext[N, T, L]{Parser: p, Lexer: lex, Left: left, Token: t}
		return handler(ctx)
	}
	return nil, fmt.Errorf("unexpected token in infix position: %s", t.Kind())
}
