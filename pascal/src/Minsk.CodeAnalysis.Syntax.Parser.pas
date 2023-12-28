unit Minsk.CodeAnalysis.Syntax.Parser;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token,
  Minsk.CodeAnalysis.Syntax.AST,
  Minsk.CodeAnalysis.Syntax.Tree;

type
  TTokenArray  = array of TSyntaxToken;
  TStringArray = array of String;

  TParser = class
  private
    FTokens:      TTokenArray;
    FDiagnostics: TStringArray;
    FPosition:    Integer;

    function Peek(ADistance: Integer): TSyntaxToken;
    function Current: TSyntaxToken;
    function NextToken: TSyntaxToken;
    function MatchToken(AKind: TSyntaxKind): TSyntaxToken;
    function ParseExpression: TExpressionSyntax;
    function ParseTerm: TExpressionSyntax;
    function ParseFactor: TExpressionSyntax;
    function ParsePrimaryExpression: TExpressionSyntax;

  public
    constructor Create(AText: String);
    property Diagnostics: TStringArray read FDiagnostics;

    function Parse: TSyntaxTree;
  end;

implementation

uses
  Variants,
  SysUtils,
  Minsk.CodeAnalysis.Syntax.Lexer;

function TParser.Peek(ADistance: Integer): TSyntaxToken;
var
  index: Integer;
begin
  index := FPosition + ADistance;

  if index > High(FTokens) then
    Result := FTokens[High(FTokens)]
  else
    Result := FTokens[index];
end;

function TParser.Current: TSyntaxToken;
begin
  Result := Peek(0);
end;

function TParser.NextToken: TSyntaxToken;
begin
  Result := FTokens[FPosition];
  Inc(FPosition);
end;

function TParser.MatchToken(AKind: TSyntaxKind): TSyntaxToken;
begin
  if Current.Kind = AKind then
    Result := NextToken
  else
    begin
    SetLength(FDiagnostics, Length(FDiagnostics) + 1);
    FDiagnostics[High(FDiagnostics)] :=
      Format('ERROR: Unexpected token <%s>, expected <%s>.', [SyntaxKindToString(Current.Kind), SyntaxKindToString(AKind)]);
    Result := TSyntaxToken.Create(AKind, Current.Position, '', Null);
    end;
end;

function TParser.ParseExpression: TExpressionSyntax;
begin
  Result := ParseTerm;
end;

function TParser.ParseTerm: TExpressionSyntax;
var
  left:  TExpressionSyntax;
  operatorToken: TSyntaxToken;
  right: TExpressionSyntax;
begin
  left := ParseFactor;

  while (Current.Kind = SK_PlusToken) or (Current.Kind = SK_MinusToken) do
    begin
    operatorToken := NextToken;
    right := ParseFactor;
    left  := TBinaryExpressionSyntax.Create(left, operatorToken, right);
    end;

  Result := left;
end;

function TParser.ParseFactor: TExpressionSyntax;
var
  left:  TExpressionSyntax;
  operatorToken: TSyntaxToken;
  right: TExpressionSyntax;
begin
  left := ParsePrimaryExpression;

  while (Current.Kind = SK_StarToken) or (Current.Kind = SK_SlashToken) do
    begin
    operatorToken := NextToken;
    right := ParsePrimaryExpression;
    left  := TBinaryExpressionSyntax.Create(left, operatorToken, right);
    end;

  Result := left;
end;

function TParser.ParsePrimaryExpression: TExpressionSyntax;
var
  numberToken: TSyntaxToken;
  openParenthesisToken: TSyntaxToken;
  expression:  TExpressionSyntax;
  closeParenthesisToken: TSyntaxToken;
begin
  if Current.Kind = SK_OpenParenthesisToken then
    begin
    openParenthesisToken := NextToken;
    expression := ParseExpression;
    closeParenthesisToken := NextToken;
    Result := TParenthesizedExpressionSyntax.Create(openParenthesisToken, expression, closeParenthesisToken);
    end
  else
    begin
    numberToken := MatchToken(SK_NumberToken);
    Result := TLiteralExpressionSyntax.Create(numberToken);
    end;
end;

constructor TParser.Create(AText: String);
var
  lexer: TLexer;
  token: TSyntaxToken;
begin
  lexer := TLexer.Create(AText);

  FTokens := nil;

  repeat
    token := lexer.NextToken;

    if (token.Kind <> SK_WhitespaceToken) and (token.Kind <> SK_BadToken) then
      begin
      SetLength(FTokens, Length(FTokens) + 1);
      FTokens[High(FTokens)] := token;
      end;
  until token.Kind = SK_EndOfFileToken;

  FPosition := Low(FTokens);
  FDiagnostics := lexer.Diagnostics;
end;

function TParser.Parse: TSyntaxTree;
var
  expression: TExpressionSyntax;
  endOfFileToken: TSyntaxToken;
begin
  expression := ParseExpression;
  endOfFileToken := MatchToken(SK_EndOfFileToken);
  Result := TSyntaxTree.Create(FDiagnostics, expression, endOfFileToken);
end;

end.
