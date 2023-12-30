unit Minsk.CodeAnalysis.Binding.Binder;

interface

uses
  Minsk.CodeAnalysis.Binding.Node,
  Minsk.CodeAnalysis.Syntax.AST,
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.Runtime.Types;

type
  TBinder = class
  private
    function BindBinaryOperatorKind(AKind: TSyntaxKind): TBoundBinaryOperatorKind;
    function BindUnaryOperatorKind(AKind: TSyntaxKind): TBoundUnaryOperatorKind;
    function BindBinaryExpression(ASyntax: TBinaryExpressionSyntax): TBoundExpression;
    function BindLiteralExpression(ASyntax: TLiteralExpressionSyntax): TBoundExpression;
    function BindParenthesizedExpression(ASyntax: TParenthesizedExpressionSyntax): TBoundExpression;
    function BindUnaryExpression(ASyntax: TUnaryExpressionSyntax): TBoundExpression;

  public
    function BindExpression(ASyntax: TExpressionSyntax): TBoundExpression;
  end;

implementation

{ TBinder }
function TBinder.BindBinaryOperatorKind(AKind: TSyntaxKind): TBoundBinaryOperatorKind;
begin
  case AKind of
    SK_PlusToken: Result := BBOK_Addition;
    SK_MinusToken: Result := BBOK_Subtraction;
    SK_StarToken: Result := BBOK_Multiplication;
    SK_SlashToken: Result := BBOK_Division;
    else
      raise TMinskException.Create('Unexpected binary operator ' + SyntaxKindToString(AKind));
    end;
end;

function TBinder.BindUnaryOperatorKind(AKind: TSyntaxKind): TBoundUnaryOperatorKind;
begin
  case AKind of
    SK_PlusToken: Result := BUOK_Identity;
    SK_MinusToken: Result := BUOK_ArithmeticNegation;
    else
      raise TMinskException.Create('Unexpected unary operator ' + SyntaxKindToString(AKind));
    end;
end;

function TBinder.BindBinaryExpression(ASyntax: TBinaryExpressionSyntax): TBoundExpression;
var
  left, right: TBoundExpression;
  operatorKind: TBoundBinaryOperatorKind;
begin
  left := BindExpression(ASyntax.Left);
  right := BindExpression(ASyntax.Right);
  operatorKind := BindBinaryOperatorKind(ASyntax.OperatorToken.Kind);
  Result := TBoundBinaryExpression.Create(operatorKind, left, right);
end;

function TBinder.BindLiteralExpression(ASyntax: TLiteralExpressionSyntax): TBoundExpression;
begin
  Result := TBoundLiteralExpression.Create(ASyntax.LiteralToken.Value);
end;

function TBinder.BindParenthesizedExpression(ASyntax: TParenthesizedExpressionSyntax): TBoundExpression;
begin
  Result := BindExpression(ASyntax.Expression);
end;

function TBinder.BindUnaryExpression(ASyntax: TUnaryExpressionSyntax): TBoundExpression;
var
  operand: TBoundExpression;
  operatorKind: TBoundUnaryOperatorKind;
begin
  operand := BindExpression(ASyntax.Operand);
  operatorKind := BindUnaryOperatorKind(ASyntax.OperatorToken.Kind);
  Result := TBoundUnaryExpression.Create(operatorKind, operand);
end;

function TBinder.BindExpression(ASyntax: TExpressionSyntax): TBoundExpression;
begin
  case ASyntax.Kind of
    SK_BinaryExpression: Result := BindBinaryExpression(TBinaryExpressionSyntax(ASyntax));
    SK_LiteralExpression: Result := BindLiteralExpression(TLiteralExpressionSyntax(ASyntax));
    SK_ParenthesizedExpression: Result := BindParenthesizedExpression(TParenthesizedExpressionSyntax(ASyntax));
    SK_UnaryExpression: Result := BindUnaryExpression(TUnaryExpressionSyntax(ASyntax));
    else
      raise TMinskException.Create('Unexpected syntax ' + SyntaxKindToString(ASyntax.Kind));
    end;
end;

end.
