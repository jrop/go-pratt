# go-pratt

This is a simple Pratt-parsing library for Go.

## Example

There are two different ways to use this library:
1. Implement `Parsable` for one of your own structs, and call
   `pratt.ParseExpression(myParseable, 0)`, OR
2. Use the built-in Pratt-parser builder that already implements `Parsable`, and
   call the convenience function `parser.ParseExpression(0)` on it

Implementing the `Parsable` interface means implementing the following:

```go
type Parseable[N any, T Tokenable, L Lexable[T]] interface {
	BindPower(left N, t T) uint
	ParsePrefix(PrefixContext[N, T, L]) (*N, error)
	ParseInfix(InfixContext[N, T, L]) (*N, error)
}
```

Using the builder looks like this:

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
type MyBinaryExpressionContext = pratt.BinaryExpressionContext[MyNode, MyToken]

func NewPrattParser() MyPrattParser {
  p := pratt.NewPrattParser[MyNode, MyToken, MyLexer]()

	var bp uint = 10
	p.SetBindPower("(", bp)
	p.AddPrefixHandler("(", func(ctx MyPrefixContext) (*MyNode, error) {
		result, err := ParseExpression[MyNode, MyToken, MyLexer](ctx.Lexer, p, 0)
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
	p.DefineBinaryOperator("+", bp, func(ctx MyBinaryExpressionContext) MyNode {
		return makeBinaryExpression(ctx.Left, "+", ctx.Right)
	})

	bp += 10
	p.DefineBinaryOperator("*", bp, func(ctx MyBinaryExpressionContext) MyNode {
		return makeBinaryExpression(ctx.Left, "*", ctx.Right)
	})
  
  // There are many more available utilities you can use that are not enumerated
  // here...

	return p
}

// Then invoke like:
l := NewMyLexer(...)
p := NewPrattParser()
ast, err := p.ParseExpression(l, 0)
```

# License (MIT)

Copyright (c) 2023 Jonathan Apodaca <jrapodaca@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
