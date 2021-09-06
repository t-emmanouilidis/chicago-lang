package lexer

import (
	"io/aegis/chicago/token"
	"testing"
)

func TestDivision(t *testing.T) {
	assertToken(t, New("/").NextToken(), token.DIVIDE, "/")
}

func TestMultiply(t *testing.T) {
	assertToken(t, New("*").NextToken(), token.MULTIPLY, "*")
}

func TestMinus(t *testing.T) {
	assertToken(t, New("-").NextToken(), token.MINUS, "-")
}

func TestOpposite(t *testing.T) {
	assertToken(t, New("!").NextToken(), token.BANG, "!")
}
func TestGreater(t *testing.T) {
	assertToken(t, New(">").NextToken(), token.GREATER, ">")
}

func TestLess(t *testing.T) {
	assertToken(t, New("<").NextToken(), token.LESS, "<")
}

func TestFn(t *testing.T) {
	assertToken(t, New("fn").NextToken(), token.FUNCTION, "fn")
}

func TestTrue(t *testing.T) {
	assertToken(t, New("true").NextToken(), token.TRUE, "true")
}

func TestFalse(t *testing.T) {
	assertToken(t, New("false").NextToken(), token.FALSE, "false")
}

func TestIf(t *testing.T) {
	assertToken(t, New("if").NextToken(), token.IF, "if")
}

func TestElse(t *testing.T) {
	assertToken(t, New("else").NextToken(), token.ELSE, "else")
}

func TestReturn(t *testing.T) {
	assertToken(t, New("return").NextToken(), token.RETURN, "return")
}

func TestEquality(t *testing.T) {
	assertToken(t, New("==").NextToken(), token.EQUAL, "==")
}

func TestUnequality(t *testing.T) {
	assertToken(t, New("!=").NextToken(), token.UNEQUAL, "!=")
}

func assertToken(t *testing.T, token token.Token, expectedType token.TokenType, expectedLiteral string) {
	if token.Type != expectedType {
		t.Fatalf("Token assertion error. Expected type=%q, got=%q", expectedType, token.Type)
	}
	if token.Literal != expectedLiteral {
		t.Fatalf("Token assertion error. Expected literal=%q, got=%q", expectedLiteral, token.Literal)
	}
}

func TestNextToken(t *testing.T) {
	input := `=+(){},;`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.ASSIGN, "="},
		{token.PLUS, "+"},
		{token.LPAREN, "("},
		{token.RPAREN, ")"},
		{token.LBRACE, "{"},
		{token.RBRACE, "}"},
		{token.COMMA, ","},
		{token.SEMICOLON, ";"},
		{token.EOF, ""},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokentype wrong. expected=%q, got=%q",
				i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q",
				i, tt.expectedLiteral, tok.Literal)
		}
	}
}

func TestAssignment(t *testing.T) {
	input := `let five = 5;`

	tests := []struct {
		expectedType    token.TokenType
		expectedLiteral string
	}{
		{token.LET, "let"},
		{token.IDENT, "five"},
		{token.ASSIGN, "="},
		{token.INT, "5"},
		{token.SEMICOLON, ";"},
	}

	l := New(input)

	for i, tt := range tests {
		tok := l.NextToken()

		if tok.Type != tt.expectedType {
			t.Fatalf("tests[%d] - tokenType wrong. expected=%q, got=%q", i, tt.expectedType, tok.Type)
		}
		if tok.Literal != tt.expectedLiteral {
			t.Fatalf("tests[%d] - literal wrong. expected=%q, got=%q", i, tt.expectedLiteral, tok.Literal)
		}
	}
}
