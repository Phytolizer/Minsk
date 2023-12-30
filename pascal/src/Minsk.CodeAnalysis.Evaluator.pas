unit Minsk.CodeAnalysis.Evaluator;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Binding.Node,
  Minsk.Runtime.Types;

type
  TEvaluator = class
  private
    FRoot: TBoundExpression;

    function EvaluateExpression(ANode: TBoundExpression): TMinskValue;
    function EvaluateBinaryExpression(ANode: TBoundBinaryExpression): TMinskValue;
    function EvaluateLiteralExpression(ANode: TBoundLiteralExpression): TMinskValue;
    function EvaluateUnaryExpression(ANode: TBoundUnaryExpression): TMinskValue;

  public
    constructor Create(ARoot: TBoundExpression);
    function Evaluate: TMinskValue;
  end;

implementation

function TEvaluator.EvaluateExpression(ANode: TBoundExpression): TMinskValue;
begin
  case ANode.Kind of
    BNK_BinaryExpression: Result := EvaluateBinaryExpression(TBoundBinaryExpression(ANode));
    BNK_LiteralExpression: Result := EvaluateLiteralExpression(TBoundLiteralExpression(ANode));
    BNK_UnaryExpression: Result := EvaluateUnaryExpression(TBoundUnaryExpression(ANode));
    end;
end;

function TEvaluator.EvaluateBinaryExpression(ANode: TBoundBinaryExpression): TMinskValue;
var
  left, right: TMinskValue;
begin
  left := EvaluateExpression(ANode.Left);
  right := EvaluateExpression(ANode.Right);

  case ANode.OperatorKind of
    BBOK_Addition: Result := MinskInteger(left.IntegerValue + right.IntegerValue);
    BBOK_Subtraction: Result := MinskInteger(left.IntegerValue - right.IntegerValue);
    BBOK_Multiplication: Result := MinskInteger(left.IntegerValue * right.IntegerValue);
    BBOK_Division: Result := MinskInteger(left.IntegerValue div right.IntegerValue);
    end;
end;

function TEvaluator.EvaluateLiteralExpression(ANode: TBoundLiteralExpression): TMinskValue;
begin
  Result := ANode.Value;
end;

function TEvaluator.EvaluateUnaryExpression(ANode: TBoundUnaryExpression): TMinskValue;
var
  operand: TMinskValue;
begin
  operand := EvaluateExpression(ANode.Operand);

  case ANode.OperatorKind of
    BUOK_Identity: Result := operand;
    BUOK_ArithmeticNegation: Result := MinskInteger(-operand.IntegerValue);
    end;
end;

constructor TEvaluator.Create(ARoot: TBoundExpression);
begin
  FRoot := ARoot;
end;

function TEvaluator.Evaluate: TMinskValue;
begin
  Result := EvaluateExpression(FRoot);
end;

end.
