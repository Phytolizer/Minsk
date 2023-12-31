unit Minsk.CodeAnalysis.Binding.Binder;

interface

uses
  SysUtils,
  Minsk.CodeAnalysis.Binding.Node,
  Minsk.CodeAnalysis.Syntax.AST,
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.Runtime.Types;

type
  TStringArray = array of String;

  TBinder = class
  private
    FDiagnostics: TStringArray;

    function BindBinaryExpression(ASyntax: TBinaryExpressionSyntax): TBoundExpression;
    function BindLiteralExpression(ASyntax: TLiteralExpressionSyntax): TBoundExpression;
    function BindParenthesizedExpression(ASyntax: TParenthesizedExpressionSyntax): TBoundExpression;
    function BindUnaryExpression(ASyntax: TUnaryExpressionSyntax): TBoundExpression;

  public
    constructor Create;
    function BindExpression(ASyntax: TExpressionSyntax): TBoundExpression;
    property Diagnostics: TStringArray read FDiagnostics;
  end;

implementation

{ TBinder }
constructor TBinder.Create;
begin
  FDiagnostics := nil;
end;

function TBinder.BindBinaryExpression(ASyntax: TBinaryExpressionSyntax): TBoundExpression;
var
  left, right: TBoundExpression;
  op: TBoundBinaryOperator;
begin
  left := BindExpression(ASyntax.Left);
  right := BindExpression(ASyntax.Right);
  if TBoundBinaryOperator.Bind(ASyntax.OperatorToken.Kind, left.ValueType, right.ValueType, op) then
    Result := TBoundBinaryExpression.Create(op, left, right)
  else
    begin
    SetLength(FDiagnostics, Length(FDiagnostics) + 1);
    FDiagnostics[High(FDiagnostics)] := Format('Binary operator ''%s'' is not defined for types %s and %s',
      [ASyntax.OperatorToken.Text, MinskTypeToString(left.ValueType), MinskTypeToString(right.ValueType)]);
    Result := left;
    end;
end;

function TBinder.BindLiteralExpression(ASyntax: TLiteralExpressionSyntax): TBoundExpression;
begin
  Result := TBoundLiteralExpression.Create(ASyntax.Value);
end;

function TBinder.BindParenthesizedExpression(ASyntax: TParenthesizedExpressionSyntax): TBoundExpression;
begin
  Result := BindExpression(ASyntax.Expression);
end;

function TBinder.BindUnaryExpression(ASyntax: TUnaryExpressionSyntax): TBoundExpression;
var
  operand: TBoundExpression;
  op: TBoundUnaryOperator;
begin
  operand := BindExpression(ASyntax.Operand);
  if TBoundUnaryOperator.Bind(ASyntax.OperatorToken.Kind, operand.ValueType, op) then
    Result := TBoundUnaryExpression.Create(op, operand)
  else
    begin
    SetLength(FDiagnostics, Length(FDiagnostics) + 1);
    FDiagnostics[High(FDiagnostics)] := Format('Unary operator ''%s'' is not defined for type %s',
      [ASyntax.OperatorToken.Text, MinskTypeToString(operand.ValueType)]);
    Result := operand;
    end;
end;

function TBinder.BindExpression(ASyntax: TExpressionSyntax): TBoundExpression;
begin
  case ASyntax.Kind of
    SK_BinaryExpression: Result := BindBinaryExpression(TBinaryExpressionSyntax(ASyntax));
    SK_LiteralExpression: Result := BindLiteralExpression(TLiteralExpressionSyntax(ASyntax));
    SK_ParenthesizedExpression: Result := BindParenthesizedExpression(TParenthesizedExpressionSyntax(ASyntax));
    SK_UnaryExpression: Result := BindUnaryExpression(TUnaryExpressionSyntax(ASyntax));
    else
      raise Exception.Create('Unexpected syntax ' + SyntaxKindToString(ASyntax.Kind));
    end;
end;

end.
