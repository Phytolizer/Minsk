unit Minsk.CodeAnalysis.Syntax.Evaluator;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.AST;

type
  TEvaluator = class
  private
    FRoot: TExpressionSyntax;

    function EvaluateExpression(ANode: TExpressionSyntax): Integer;
    function EvaluateBinaryExpression(ANode: TBinaryExpressionSyntax): Integer;
    function EvaluateLiteralExpression(ANode: TLiteralExpressionSyntax): Integer;
    function EvaluateParenthesizedExpression(ANode: TParenthesizedExpressionSyntax): Integer;
    function EvaluateUnaryExpression(ANode: TUnaryExpressionSyntax): Integer;

  public
    constructor Create(ARoot: TExpressionSyntax);
    function Evaluate: Integer;
  end;

implementation

function TEvaluator.EvaluateExpression(ANode: TExpressionSyntax): Integer;
begin
  case ANode.Kind of
    SK_BinaryExpression: Result := EvaluateBinaryExpression(TBinaryExpressionSyntax(ANode));
    SK_ParenthesizedExpression: Result := EvaluateParenthesizedExpression(TParenthesizedExpressionSyntax(ANode));
    SK_LiteralExpression: Result := EvaluateLiteralExpression(TLiteralExpressionSyntax(ANode));
    SK_UnaryExpression: Result := EvaluateUnaryExpression(TUnaryExpressionSyntax(ANode));
    end;
end;

function TEvaluator.EvaluateBinaryExpression(ANode: TBinaryExpressionSyntax): Integer;
var
  left, right: Integer;
begin
  left := EvaluateExpression(ANode.Left);
  right := EvaluateExpression(ANode.Right);

  case ANode.OperatorToken.Kind of
    SK_PlusToken: Result := left + right;
    SK_MinusToken: Result := left - right;
    SK_StarToken: Result := left * right;
    SK_SlashToken: Result := left div right;
    end;
end;

function TEvaluator.EvaluateLiteralExpression(ANode: TLiteralExpressionSyntax): Integer;
begin
  Result := ANode.LiteralToken.Value;
end;

function TEvaluator.EvaluateParenthesizedExpression(ANode: TParenthesizedExpressionSyntax): Integer;
begin
  Result := EvaluateExpression(ANode.Expression);
end;

function TEvaluator.EvaluateUnaryExpression(ANode: TUnaryExpressionSyntax): Integer;
var
  operand: Integer;
begin
  operand := EvaluateExpression(ANode.Operand);

  case ANode.OperatorToken.Kind of
    SK_PlusToken: Result := +operand;
    SK_MinusToken: Result := -operand;
    end;
end;

constructor TEvaluator.Create(ARoot: TExpressionSyntax);
begin
  FRoot := ARoot;
end;

function TEvaluator.Evaluate: Integer;
begin
  Result := EvaluateExpression(FRoot);
end;

end.
