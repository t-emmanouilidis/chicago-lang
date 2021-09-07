package parser

import (
	"bytes"
	"fmt"
	"io/aegis/chicago/ast"
	"io/aegis/chicago/lexer"
	"io/aegis/chicago/token"
	"strconv"
)

const (
	_ int = iota
	LOWEST
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
)

var precedences = map[token.TokenType]int{
	token.LPAREN:   CALL,
	token.EQUAL:    EQUALS,
	token.UNEQUAL:  EQUALS,
	token.LESS:     LESSGREATER,
	token.GREATER:  LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.DIVIDE:   PRODUCT,
	token.MULTIPLY: PRODUCT,
}

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l *lexer.Lexer

	currentToken token.Token
	nextToken    token.Token

	errors []string

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}
	p.advance()
	p.advance()

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefix(token.IDENT, p.parseIdentifier)
	p.registerPrefix(token.INT, p.parseIntegerLiteral)
	p.registerPrefix(token.BANG, p.parsePrefixExpression)
	p.registerPrefix(token.MINUS, p.parsePrefixExpression)
	p.registerPrefix(token.TRUE, p.parseBooleanLiteral)
	p.registerPrefix(token.FALSE, p.parseBooleanLiteral)
	p.registerPrefix(token.LPAREN, p.parseGroupedExpression)
	p.registerPrefix(token.IF, p.parseIfExpression)
	p.registerPrefix(token.FUNCTION, p.parseFunctionLiteral)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfix(token.PLUS, p.parseInfixExpression)
	p.registerInfix(token.MINUS, p.parseInfixExpression)
	p.registerInfix(token.DIVIDE, p.parseInfixExpression)
	p.registerInfix(token.MULTIPLY, p.parseInfixExpression)
	p.registerInfix(token.EQUAL, p.parseInfixExpression)
	p.registerInfix(token.UNEQUAL, p.parseInfixExpression)
	p.registerInfix(token.LESS, p.parseInfixExpression)
	p.registerInfix(token.GREATER, p.parseInfixExpression)
	p.registerInfix(token.LPAREN, p.parseCallExpression)

	return p
}

func (p *Parser) registerPrefix(tokenType token.TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

func (p *Parser) registerInfix(tokenType token.TokenType, fn infixParseFn) {
	p.infixParseFns[tokenType] = fn
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for !p.currentTokenIsOfType(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.advance()
	}

	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.currentToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currentToken}
	stmt.Expression = p.parseExpression(LOWEST)
	if p.nextTokenIsOfType(token.SEMICOLON) {
		p.advance()
	}
	return stmt
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.advance()

	exp := p.parseExpression(LOWEST)
	if !p.nextTokenIsOfType(token.RPAREN) {
		p.tokenTypeError(token.RPAREN)
		return nil
	}
	p.advance()
	return exp
}

func (p *Parser) parseCallExpression(function ast.Expression) ast.Expression {
	exp := &ast.CallExpression{Token: p.currentToken, Function: function}
	exp.Arguments = p.parseCallArguments()
	return exp
}

func (p *Parser) parseCallArguments() []ast.Expression {
	args := []ast.Expression{}

	if p.nextTokenIsOfType(token.RPAREN) {
		p.advance()
		return args
	}

	p.advance()
	args = append(args, p.parseExpression(LOWEST))

	for p.nextTokenIsOfType(token.COMMA) {
		p.advance()
		p.advance()
		args = append(args, p.parseExpression(LOWEST))
	}

	if !p.nextTokenIsOfType(token.RPAREN) {
		p.tokenTypeError(token.RPAREN)
		return nil
	}

	p.advance()
	return args
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	fn := &ast.FunctionLiteral{Token: p.currentToken}

	if !p.nextTokenIsOfType(token.LPAREN) {
		p.tokenTypeError(token.LPAREN)
		return nil
	}

	p.advance()

	fn.Parameters = p.parseFunctionParameters()

	if !p.nextTokenIsOfType(token.LBRACE) {
		p.tokenTypeError(token.LBRACE)
		return nil
	}

	p.advance()

	fn.Body = p.parseBlockStatement()

	return fn
}

func (p *Parser) parseFunctionParameters() []*ast.Identifier {
	identifiers := []*ast.Identifier{}
	if p.nextTokenIsOfType(token.RPAREN) {
		p.advance()
		return identifiers
	}

	p.advance()

	ident := &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
	identifiers = append(identifiers, ident)

	for p.nextTokenIsOfType(token.COMMA) {
		p.advance()
		p.advance()
		ident := &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
		identifiers = append(identifiers, ident)
	}

	if !p.nextTokenIsOfType(token.RPAREN) {
		p.tokenTypeError(token.RPAREN)
		return nil
	}

	p.advance()
	return identifiers
}

func (p *Parser) parseIfExpression() ast.Expression {
	expr := &ast.IfExpression{Token: p.currentToken}

	if !p.nextTokenIsOfType(token.LPAREN) {
		p.tokenTypeError(token.LPAREN)
		return nil
	}
	p.advance()
	p.advance()

	expr.Condition = p.parseExpression(LOWEST)

	if !p.nextTokenIsOfType(token.RPAREN) {
		p.tokenTypeError(token.RPAREN)
		return nil
	}
	p.advance()

	if !p.nextTokenIsOfType(token.LBRACE) {
		p.tokenTypeError(token.LBRACE)
		return nil
	}
	p.advance()

	expr.Consequence = p.parseBlockStatement()

	if p.nextTokenIsOfType(token.ELSE) {
		p.advance()

		if !p.nextTokenIsOfType(token.LBRACE) {
			p.tokenTypeError(token.LBRACE)
			return nil
		}
		p.advance()

		expr.Alternative = p.parseBlockStatement()
	}

	return expr
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: p.currentToken}
	block.Statements = []ast.Statement{}

	// go to the token after {
	p.advance()

	for !p.currentTokenIsOfType(token.RBRACE) && !p.currentTokenIsOfType(token.EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			block.Statements = append(block.Statements, stmt)
		}
		p.advance()
	}

	return block
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.currentToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.currentToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.nextTokenIsOfType(token.SEMICOLON) && precedence < p.precedenceOfNextToken() {
		infix := p.infixParseFns[p.nextToken.Type]
		if infix == nil {
			return leftExp
		}
		p.advance()
		leftExp = infix(leftExp)
	}
	return leftExp
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
		Left:     left,
	}
	precedence := p.precedenceOfCurrentToken()
	p.advance()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.currentToken,
		Operator: p.currentToken.Literal,
	}
	p.advance()
	expression.Right = p.parseExpression(PREFIX)
	return expression
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) precedenceOfNextToken() int {
	if p, ok := precedences[p.nextToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) precedenceOfCurrentToken() int {
	if p, ok := precedences[p.currentToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}
}

func (p *Parser) advance() {
	p.currentToken = p.nextToken
	p.nextToken = p.l.NextToken()
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.currentToken}
	p.advance()
	stmt.ReturnValue = p.parseExpression(LOWEST)
	if p.nextTokenIsOfType(token.SEMICOLON) {
		p.advance()
	}
	return stmt
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.currentToken}

	if !p.nextTokenIsOfType(token.IDENT) {
		p.tokenTypeError(token.IDENT)
		return nil
	}
	p.advance()

	stmt.Name = &ast.Identifier{Token: p.currentToken, Value: p.currentToken.Literal}

	if !p.nextTokenIsOfType(token.ASSIGN) {
		p.tokenTypeError(token.ASSIGN)
		return nil
	}
	p.advance()
	p.advance()

	stmt.Value = p.parseExpression(LOWEST)

	if p.nextTokenIsOfType(token.SEMICOLON) {
		p.advance()
	}
	return stmt
}

func (p *Parser) currentTokenIsOfType(t token.TokenType) bool {
	return t == p.currentToken.Type
}

func (p *Parser) nextTokenIsOfType(t token.TokenType) bool {
	return t == p.nextToken.Type
}

func (p *Parser) ErrorsAsString() string {
	var out bytes.Buffer
	for _, err := range p.errors {
		out.WriteString(err)
		out.WriteString("\n")
	}
	return out.String()
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) AnyErrors() bool {
	return len(p.errors) > 0
}

func (p *Parser) noErrors() bool {
	return !p.AnyErrors()
}

func (p *Parser) tokenTypeError(t token.TokenType) {
	msg := fmt.Sprintf("Expected token type %s but got %s instead", t, p.nextToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.currentToken}
	value, err := strconv.ParseInt(p.currentToken.Literal, 10, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.currentToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	return &ast.BooleanLiteral{Token: p.currentToken, Value: p.currentTokenIsOfType(token.TRUE)}
}
