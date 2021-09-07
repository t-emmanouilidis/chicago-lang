package parser

import (
	"io/aegis/chicago/ast"
	"io/aegis/chicago/lexer"
	"testing"
)

func TestLetStatement(t *testing.T) {
	input := "let x = 5;"
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	if program == nil {
		t.Fatal("Could not parse program")
	}
	if len(program.Statements) != 1 {
		t.Fatal("Wrong number of parsed statements")
	}
	letStatement, _ := program.Statements[0].(*ast.LetStatement)
	if letStatement.TokenLiteral() != "let" {
		t.Error("Let statement token literal is not let")
	}
	if letStatement.Name.Value != "x" {
		t.Error("Let statement name is not x")
	}

	intLiteral, _ := letStatement.Value.(*ast.IntegerLiteral)
	if intLiteral.Value != 5 {
		t.Errorf("Expected integer literal with value 5 but got %d", intLiteral.Value)
	}
}

func TestReturnStatement(t *testing.T) {
	input := "return 5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	if program == nil {
		t.Fatal("Could not parse program")
	}
	returnStatement, _ := program.Statements[0].(*ast.ReturnStatement)
	if returnStatement.TokenLiteral() != "return" {
		t.Errorf("returnStatement.TokenLiteral is not 'return', go %q", returnStatement.TokenLiteral())
	}

	intLiteral, _ := returnStatement.ReturnValue.(*ast.IntegerLiteral)
	if intLiteral.Value != 5 {
		t.Errorf("Expected integer literal with value 5 but got %d", intLiteral.Value)
	}
}

func TestParsingError(t *testing.T) {
	p := New(lexer.New("let x"))
	p.ParseProgram()

	if p.noErrors() {
		t.Fatal("Expected parser errors but no errors were found")
	}
	for _, msg := range p.Errors() {
		println(msg)
	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d", len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	ident, ok := stmt.Expression.(*ast.Identifier)

	if !ok {
		t.Fatalf("exp not *ast.Identifier. got=%T", stmt.Expression)
	}
	if ident.Value != "foobar" {
		t.Errorf("ident.Value not foobar. got=%s", ident.Value)
	}
	if ident.TokenLiteral() != "foobar" {
		t.Errorf("ident.TokenLiteral not foobar. got=%s", ident.TokenLiteral())
	}
}

func TestIntegerLiteralIdentifier(t *testing.T) {
	input := "1;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	literal, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.IntegerLiteral)
	if literal.Value != 1 {
		t.Errorf("literal.Value not %d. got=%d", 5, literal.Value)
	}
	if literal.TokenLiteral() != "1" {
		t.Errorf("literal.TokenLiteral not 1. got=%s", literal.TokenLiteral())
	}
}

func TestParsingBangPrefixExpression(t *testing.T) {
	input := "!5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	exp, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.PrefixExpression)
	if exp.Operator != "!" {
		t.Fatalf("exp.Operator is not '!'. got=%s", exp.Operator)
	}
	integer, _ := exp.Right.(*ast.IntegerLiteral)
	if integer.Value != 5 {
		t.Errorf("literal.Value not 5. got=%d", integer.Value)
	}
}

func TestParsingMinusPrefixExpression(t *testing.T) {
	input := "-5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	exp, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.PrefixExpression)
	if exp.Operator != "-" {
		t.Fatalf("exp.Operator is not '-'. got=%s", exp.Operator)
	}
	integer, _ := exp.Right.(*ast.IntegerLiteral)
	if integer.Value != 5 {
		t.Errorf("literal.Value not 5. got=%d", integer.Value)
	}
}

func TestParsingPlusInfixExpression(t *testing.T) {
	input := "5 + 5;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	infix, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.InfixExpression)
	left := infix.Left.(*ast.IntegerLiteral)
	if left.Value != 5 {
		t.Errorf("infix.Left.Value is not 5. got=%d", left.Value)
	}
	right := infix.Right.(*ast.IntegerLiteral)
	if right.Value != 5 {
		t.Errorf("infix.Right.Value is not 5. got=%d", right.Value)
	}
	operator := infix.Operator
	if operator != "+" {
		t.Errorf("infix.Operator is not +. got =%s", operator)
	}
}

func TestPrecedence(t *testing.T) {
	input := "-a * b"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	asString := program.String()
	if asString != "((-a) * b)" {
		t.Errorf("expected=((-a) * b), got=%q", asString)
	}
}

func TestPrecedenceWithParentheses(t *testing.T) {
	input := "(5 + 5) * 2;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	asString := program.String()
	if asString != "((5 + 5) * 2)" {
		t.Errorf("expected=((5 + 5) * 2), got=%q", asString)
	}
}

func TestBooleanLiteral(t *testing.T) {
	input := "true;"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	b, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.BooleanLiteral)
	if !b.Value {
		t.Errorf("expected true but got false")
	}
}

func TestIfExpression(t *testing.T) {
	input := "if (x < y) {x}"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	ifExp, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.IfExpression)
	condition, _ := ifExp.Condition.(*ast.InfixExpression)
	if condition.Operator != "<" {
		t.Errorf("wrong condition infix operator, expected <, but got %s instead", condition.Operator)
	}
	consequence := ifExp.Consequence
	if len(consequence.Statements) != 1 {
		t.Errorf("wrong consequence statements number, expected 1 got %d instead", len(consequence.Statements))
	}
}

func TestIfElseExpression(t *testing.T) {
	input := "if (x < y) {x} else {y}"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	ifExp, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.IfExpression)
	alt := ifExp.Alternative
	if len(alt.Statements) != 1 {
		t.Errorf("Wrong number of alternate statements, expected 1 got %d instead", len(alt.Statements))
	}
}

func TestFunctionLiteral(t *testing.T) {
	input := `fn(x, y) { x + y; }`
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	functionLiteral, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.FunctionLiteral)

	if len(functionLiteral.Parameters) != 2 {
		t.Fatalf("Wrong number of function literal parameters. Expected 2 but got %d instead", len(functionLiteral.Parameters))
	}

	infixExpr, _ := functionLiteral.Body.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.InfixExpression)
	if infixExpr.Operator != "+" {
		t.Fatalf("Wrong operator for function literal nested infix expression")
	}
}

func TestCallExpression(t *testing.T) {
	input := "add(1, 2 * 3, 4 + 5);"
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()

	callExpr, _ := program.Statements[0].(*ast.ExpressionStatement).Expression.(*ast.CallExpression)
	functionIdent, _ := callExpr.Function.(*ast.Identifier)

	if functionIdent.Value != "add" {
		t.Fatalf("Wrong function name on call expression. Expected add but got %s instead", functionIdent.Value)
	}

	if len(callExpr.Arguments) != 3 {
		t.Fatalf("Wrong number of arguments. Expected 3 but got %d instead", len(callExpr.Arguments))
	}
}
