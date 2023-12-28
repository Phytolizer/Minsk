unit Minsk.CodeAnalysis.Syntax.AST;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token;

type
  TExpressionSyntax = class(TSyntaxNode)
  end;

  TLiteralExpressionSyntax = class(TExpressionSyntax)
  private
    FLiteralToken: TSyntaxToken;

  public
    constructor Create(ALiteralToken: TSyntaxToken);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property LiteralToken: TSyntaxToken read FLiteralToken;
  end;

  TParenthesizedExpressionSyntax = class(TExpressionSyntax)
  private
    FOpenParenthesisToken: TSyntaxToken;
    FExpression:           TExpressionSyntax;
    FCloseParenthesisToken: TSyntaxToken;

  public
    constructor Create(AOpenParenthesisToken: TSyntaxToken; AExpression: TExpressionSyntax; ACloseParenthesisToken: TSyntaxToken);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property OpenParenthesisToken: TSyntaxToken read FOpenParenthesisToken;
    property Expression: TExpressionSyntax read FExpression;
    property CloseParenthesisToken: TSyntaxToken read FCloseParenthesisToken;
  end;

  TBinaryExpressionSyntax = class(TExpressionSyntax)
  private
    FLeft:          TExpressionSyntax;
    FOperatorToken: TSyntaxToken;
    FRight:         TExpressionSyntax;

  public
    constructor Create(ALeft: TExpressionSyntax; AOperatorToken: TSyntaxToken; ARight: TExpressionSyntax);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property Left: TExpressionSyntax read FLeft;
    property OperatorToken: TSyntaxToken read FOperatorToken;
    property Right: TExpressionSyntax read FRight;
  end;

implementation

{ TLiteralExpressionSyntax }
constructor TLiteralExpressionSyntax.Create(ALiteralToken: TSyntaxToken);
begin
  FLiteralToken := ALiteralToken;
end;

function TLiteralExpressionSyntax.GetKind: TSyntaxKind;
begin
  Result := SK_LiteralExpression;
end;

function TLiteralExpressionSyntax.GetChildren: TSyntaxNodeChildren;
begin
  Result := nil;
  SetLength(Result, 1);
  Result[0] := FLiteralToken;
end;

{ TParenthesizedExpressionSyntax }
constructor TParenthesizedExpressionSyntax.Create(AOpenParenthesisToken: TSyntaxToken; AExpression: TExpressionSyntax; ACloseParenthesisToken: TSyntaxToken);
begin
  FOpenParenthesisToken := AOpenParenthesisToken;
  FExpression := AExpression;
  FCloseParenthesisToken := ACloseParenthesisToken;
end;

function TParenthesizedExpressionSyntax.GetKind: TSyntaxKind;
begin
  Result := SK_ParenthesizedExpression;
end;

function TParenthesizedExpressionSyntax.GetChildren: TSyntaxNodeChildren;
begin
  Result := nil;
  SetLength(Result, 3);
  Result[0] := FOpenParenthesisToken;
  Result[1] := FExpression;
  Result[2] := FCloseParenthesisToken;
end;

{ TBinaryExpressionSyntax }
constructor TBinaryExpressionSyntax.Create(ALeft: TExpressionSyntax; AOperatorToken: TSyntaxToken; ARight: TExpressionSyntax);
begin
  FLeft  := ALeft;
  FOperatorToken := AOperatorToken;
  FRight := ARight;
end;

function TBinaryExpressionSyntax.GetKind: TSyntaxKind;
begin
  Result := SK_BinaryExpression;
end;

function TBinaryExpressionSyntax.GetChildren: TSyntaxNodeChildren;
begin
  Result := nil;
  SetLength(Result, 3);
  Result[0] := FLeft;
  Result[1] := FOperatorToken;
  Result[2] := FRight;
end;

end.
