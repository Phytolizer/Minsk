﻿unit Minsk.CodeAnalysis.Syntax.AST;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token,
  Minsk.Runtime.Types;

type
  TExpressionSyntax = class(TSyntaxNode)
  end;

  TLiteralExpressionSyntax = class(TExpressionSyntax)
  private
    FLiteralToken: TSyntaxToken;
    FValue: TMinskValue;

  public
    constructor Create(ALiteralToken: TSyntaxToken); overload;
    constructor Create(ALiteralToken: TSyntaxToken; AValue: TMinskValue); overload;
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property LiteralToken: TSyntaxToken read FLiteralToken;
    property Value: TMinskValue read FValue;
  end;

  TParenthesizedExpressionSyntax = class(TExpressionSyntax)
  private
    FOpenParenthesisToken: TSyntaxToken;
    FExpression: TExpressionSyntax;
    FCloseParenthesisToken: TSyntaxToken;

  public
    constructor Create(AOpenParenthesisToken: TSyntaxToken; AExpression: TExpressionSyntax; ACloseParenthesisToken: TSyntaxToken);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property OpenParenthesisToken: TSyntaxToken read FOpenParenthesisToken;
    property Expression: TExpressionSyntax read FExpression;
    property CloseParenthesisToken: TSyntaxToken read FCloseParenthesisToken;
  end;

  TUnaryExpressionSyntax = class(TExpressionSyntax)
  private
    FOperatorToken: TSyntaxToken;
    FOperand: TExpressionSyntax;

  public
    constructor Create(AOperatorToken: TSyntaxToken; AOperand: TExpressionSyntax);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;
    property OperatorToken: TSyntaxToken read FOperatorToken;
    property Operand: TExpressionSyntax read FOperand;
  end;

  TBinaryExpressionSyntax = class(TExpressionSyntax)
  private
    FLeft: TExpressionSyntax;
    FOperatorToken: TSyntaxToken;
    FRight: TExpressionSyntax;

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
  FValue := ALiteralToken.Value;
end;

constructor TLiteralExpressionSyntax.Create(ALiteralToken: TSyntaxToken; AValue: TMinskValue);
begin
  FLiteralToken := ALiteralToken;
  FValue := AValue;
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

{ TUnaryExpressionSyntax }
constructor TUnaryExpressionSyntax.Create(AOperatorToken: TSyntaxToken; AOperand: TExpressionSyntax);
begin
  FOperatorToken := AOperatorToken;
  FOperand := AOperand;
end;

function TUnaryExpressionSyntax.GetKind: TSyntaxKind;
begin
  Result := SK_UnaryExpression;
end;

function TUnaryExpressionSyntax.GetChildren: TSyntaxNodeChildren;
begin
  Result := nil;
  SetLength(Result, 2);
  Result[0] := FOperatorToken;
  Result[1] := FOperand;
end;

{ TBinaryExpressionSyntax }
constructor TBinaryExpressionSyntax.Create(ALeft: TExpressionSyntax; AOperatorToken: TSyntaxToken; ARight: TExpressionSyntax);
begin
  FLeft := ALeft;
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
