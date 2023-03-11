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

// PrefixContext is the context passed to a prefix handler
// when parsing a prefix expression. It is easily aliased so that
// you don't have to keep typing out the generic types:
type PrefixContext[N any, T Tokenable, L Lexable[T]] struct {
	Lexer  L
	Token  T
}

// InfixContext is the context passed to an infix handler
// when parsing an infix expression. It is easily aliased so that
// you don't have to keep typing out the generic types:
type InfixContext[N any, T Tokenable, L Lexable[T]] struct {
	Lexer  L
	Left   N
	Token  T
}

// BinaryExpressionContext is the context passed to a binary expression
// handler when parsing a binary expression. It is easily aliased so that
// you don't have to keep typing out the generic types:
type BinaryExpressionContext[N any, T Tokenable] struct {
	Left  N
	Token T
	Right N
}

// An interface that provides the necessary methods for a Pratt
// parser to work. The parser will call BindPower to determine
// the precedence of the next token, and then call ParsePrefix
// or ParseInfix to drive forward expression parsing.
type Parseable[N any, T Tokenable, L Lexable[T]] interface {
	BindPower(left N, t T) uint
	ParsePrefix(PrefixContext[N, T, L]) (*N, error)
	ParseInfix(InfixContext[N, T, L]) (*N, error)
}

func ParseExpression[N any, T Tokenable, L Lexable[T]](lex L, p Parseable[N, T, L], rbp uint) (*N, error) {
	t := lex.Next()
	prefixContext := PrefixContext[N, T, L]{Lexer: lex, Token: t}
	left, err := p.ParsePrefix(prefixContext)
	if err != nil {
		return left, err
	}

	// While the next precedence is greater than the current precedence, keep
	// going. For example, say rbp is `+`, and the next token is `*`, we can
	// handle that in the current stack, otherwise we return and let a
	// higher-up-in-the-stack operation handle the parsing:
	t = lex.Peek()
	for rbp < p.BindPower(*left, t) {
		lex.MarkRead(t)
		infixContext := InfixContext[N, T, L]{Lexer: lex, Left: *left, Token: t}
		left, err = p.ParseInfix(infixContext)
		if err != nil {
			return left, err
		}
		t = lex.Peek() // peek before the next loop iteration
	}
	return left, nil
}

// Now we are going to implement a Pratt Parser **Builder**. This is a utility
// that allows you to define a Pratt parser in a declarative way, rather than
// having to implement the Parseable interface yourself. In order to make the outer
// struct copyable in an efficient way, we'll use a pointer to a struct that
// contains the actual data.

type prattParserData[N any, T Tokenable, L Lexable[T]] struct {
	precedenceLevels map[string]uint
	prefixHandlers   map[string]func(PrefixContext[N, T, L]) (*N, error)
	infixHandlers    map[string]func(InfixContext[N, T, L]) (*N, error)
}
type PrattParser[N any, T Tokenable, L Lexable[T]] struct {
	*prattParserData[N, T, L]
}

func NewPrattParser[N any, T Tokenable, L Lexable[T]]() PrattParser[N, T, L] {
	return PrattParser[N, T, L]{
		&prattParserData[N, T, L]{
			precedenceLevels: make(map[string]uint),
			prefixHandlers:   make(map[string]func(PrefixContext[N, T, L]) (*N, error)),
			infixHandlers:    make(map[string]func(InfixContext[N, T, L]) (*N, error)),
		},
	}
}

func (p PrattParser[N, T, L]) GetBindPower(left N, t T) uint {
	if level, ok := p.precedenceLevels[t.Kind()]; ok {
		return level
	}
	return 0
}

func (p *PrattParser[N, T, L]) SetBindPower(tokenKind string, level uint) {
	p.precedenceLevels[tokenKind] = level
}

func (p PrattParser[N, T, L]) SetStopToken(tokenKind string) {
	p.SetBindPower(tokenKind, 0)
}

func (p PrattParser[N, T, L]) AddPrefixHandler(tokenKind string, handler func(PrefixContext[N, T, L]) (*N, error)) {
	p.prefixHandlers[tokenKind] = handler
}

func (p PrattParser[N, T, L]) AddInfixHandler(tokenKind string, handler func(InfixContext[N, T, L]) (*N, error)) {
	p.infixHandlers[tokenKind] = handler
}

func (p PrattParser[N, T, L]) DefineBinaryOperator(
	tokenKind string,
	level uint,
	binaryExpressionCreator func(BinaryExpressionContext[N, T]) N,
) {
	p.SetBindPower(tokenKind, level)
	p.AddInfixHandler(tokenKind, func(ctx InfixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, p, level)
		if err != nil {
			return nil, err
		}
		var result N = binaryExpressionCreator(BinaryExpressionContext[N, T]{ctx.Left, ctx.Token, *right})
		return &result, nil
	})
}

func (p PrattParser[N, T, L]) DefineBinaryOperatorRassoc(
	tokenKind string,
	level uint,
	binaryExpressionCreator func(BinaryExpressionContext[N, T]) N,
) {
	p.SetBindPower(tokenKind, level)
	p.AddInfixHandler(tokenKind, func(ctx InfixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, p, level-1)
		if err != nil {
			return nil, err
		}
		var result N = binaryExpressionCreator(BinaryExpressionContext[N, T]{ctx.Left, ctx.Token, *right})
		return &result, nil
	})
}

func (p PrattParser[N, T, L]) DefineUnaryOperator(tokenKind string, level uint, unaryExpressionCreator func(t T, right N) N) {
	p.SetBindPower(tokenKind, level)
	p.AddPrefixHandler(tokenKind, func(ctx PrefixContext[N, T, L]) (*N, error) {
		right, err := ParseExpression[N, T, L](ctx.Lexer, p, level)
		if err != nil {
			return nil, err
		}
		var result N = unaryExpressionCreator(ctx.Token, *right)
		return &result, nil
	})
}

func ParseList[N any, T Tokenable, L Lexable[T]](
	p Parseable[N, T, L],
	lex L,
	isOpener func(t T) bool,
	isCloser func(t T) bool,
	isSeparator func(t T) bool,
	elementParser func() (*N, error),
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
		element, err := elementParser()
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
func ParseListSimple[N any, T Tokenable, L Lexable[T]](
	p Parseable[N, T, L],
	lex L,
	opener string,
	closer string,
	separator string,
	elementParser func() (*N, error),
) ([]N, error) {
	return ParseList(
		p,
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

func (p PrattParser[N, T, L]) ParsePrefix(ctx PrefixContext[N, T, L]) (*N, error) {
	if handler, ok := p.prefixHandlers[ctx.Token.Kind()]; ok {
		return handler(ctx)
	}
	return nil, fmt.Errorf("unexpected token in prefix posision: %s", ctx.Token.Kind())
}

func (p PrattParser[N, T, L]) ParseInfix(ctx InfixContext[N, T, L]) (*N, error) {
	if handler, ok := p.infixHandlers[ctx.Token.Kind()]; ok {
		return handler(ctx)
	}
	return nil, fmt.Errorf("unexpected token in infix position: %s", ctx.Token.Kind())
}
