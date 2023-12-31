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

    function BindBinaryOperatorKind(
      AKind: TSyntaxKind; ALeftType: TMinskType; ARightType: TMinskType;
      var AOperatorKind: TBoundBinaryOperatorKind): Boolean;
    function BindUnaryOperatorKind(
      AKind: TSyntaxKind; AOperandType: TMinskType;
      var AOperatorKind: TBoundUnaryOperatorKind): Boolean;
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

function TBinder.BindBinaryOperatorKind(
  AKind: TSyntaxKind; ALeftType, ARightType: TMinskType;
  var AOperatorKind: TBoundBinaryOperatorKind): Boolean;
begin
  Result := false;
  if (ALeftType = mtInteger) and (ARightType = mtInteger) then
    begin
    Result := true;
    case AKind of
      SK_PlusToken: AOperatorKind := BBOK_Addition;
      SK_MinusToken: AOperatorKind := BBOK_Subtraction;
      SK_StarToken: AOperatorKind := BBOK_Multiplication;
      SK_SlashToken: AOperatorKind := BBOK_Division;
      else Result := false;
      end;
    end
  else if (ALeftType = mtBoolean) and (ARightType = mtBoolean) then
    begin
    Result := true;
    case AKind of
      SK_AmpersandAmpersandToken: AOperatorKind := BBOK_LogicalAnd;
      SK_PipePipeToken: AOperatorKind := BBOK_LogicalOr;
      else Result := false;
      end;
    end;
end;

function TBinder.BindUnaryOperatorKind(
  AKind: TSyntaxKind; AOperandType: TMinskType;
  var AOperatorKind: TBoundUnaryOperatorKind): Boolean;
begin
  Result := false;
  case AOperandType of
    mtInteger:
      begin
      Result := true;
      case AKind of
        SK_PlusToken: AOperatorKind := BUOK_Identity;
        SK_MinusToken: AOperatorKind := BUOK_ArithmeticNegation;
        else Result := false;
        end;
      end;
    mtBoolean:
      begin
      Result := true;
      case AKind of
        SK_BangToken: AOperatorKind := BUOK_LogicalNegation;
        else Result := false;
        end;
      end;
    end;
end;

function TBinder.BindBinaryExpression(ASyntax: TBinaryExpressionSyntax): TBoundExpression;
var
  left, right: TBoundExpression;
  operatorKind: TBoundBinaryOperatorKind;
begin
  left := BindExpression(ASyntax.Left);
  right := BindExpression(ASyntax.Right);
  if BindBinaryOperatorKind(ASyntax.OperatorToken.Kind, left.ValueType, right.ValueType, operatorKind) then
    Result := TBoundBinaryExpression.Create(operatorKind, left, right)
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
  operatorKind: TBoundUnaryOperatorKind;
begin
  operand := BindExpression(ASyntax.Operand);
  if BindUnaryOperatorKind(ASyntax.OperatorToken.Kind, operand.ValueType, operatorKind) then
    Result := TBoundUnaryExpression.Create(operatorKind, operand)
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
