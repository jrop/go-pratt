# go-pratt

This is a simple Pratt-parsing library for Go.

## Example

```go
type MyToken struct {}
// Implement this interface for MyToken:
// type Tokenable interface {
// 	Kind() string
// 	IsEOF() bool
// }

type MyLexer struct {}
// Implement this interface for MyLexer:
// type Lexable[T Tokenable] interface {
// 	Next() T
// 	Peek() T
// 	MarkRead(T)
// }

// The interface each AST Node:
interface MyNode {
  // ...
}

// Define some type aliases so we don't have to keep typing generics out all the
// time:
type MyPrattParser = pratt.PrattParser[MyNode, MyToken, MyLexer]
type MyPrefixContext = pratt.PrefixContext[MyNode, MyToken, MyLexer]
type MyInfixContext = pratt.InfixContext[MyNode, MyToken, MyLexer]

func NewPrattParser() MyPrattParser {
  p := pratt.NewPrattParser[MyNode, MyToken, MyLexer]()

	var bp uint = 10
	p.SetBindPower("(", bp)
	p.AddPrefixHandler("(", func(ctx MyPrefixContext) (*MyNode, error) {
		result, err := ParseExpression[MyNode, MyToken, MyLexer](ctx.Lexer, ctx.Parser, 0)
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
	p.DefineBinaryOperator("+", bp, func(left MyNode, t MyToken, right MyNode) MyNode {
		return makeBinaryExpression(left, "+", right)
	})

	bp += 10
	p.DefineBinaryOperator("*", bp, func(left MyNode, t MyToken, right MyNode) MyNode {
		return makeBinaryExpression(left, "*", right)
	})
  
  // There are many more available utilities you can use that are not enumerated
  // here...

	return p
}

// Then invoke like:
l := NewMyLexer(...)
p := NewPrattParser()
ast, err := pratt.ParseExpression(l, p, 0)
```
