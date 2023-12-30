unit Minsk.CodeAnalysis.Evaluator;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Binding.Node;

type
  TEvaluator = class
  private
    FRoot: TBoundExpression;

    function EvaluateExpression(ANode: TBoundExpression): Integer;
    function EvaluateBinaryExpression(ANode: TBoundBinaryExpression): Integer;
    function EvaluateLiteralExpression(ANode: TBoundLiteralExpression): Integer;
    function EvaluateUnaryExpression(ANode: TBoundUnaryExpression): Integer;

  public
    constructor Create(ARoot: TBoundExpression);
    function Evaluate: Integer;
  end;

implementation

function TEvaluator.EvaluateExpression(ANode: TBoundExpression): Integer;
begin
  case ANode.Kind of
    BNK_BinaryExpression: Result := EvaluateBinaryExpression(TBoundBinaryExpression(ANode));
    BNK_LiteralExpression: Result := EvaluateLiteralExpression(TBoundLiteralExpression(ANode));
    BNK_UnaryExpression: Result := EvaluateUnaryExpression(TBoundUnaryExpression(ANode));
    end;
end;

function TEvaluator.EvaluateBinaryExpression(ANode: TBoundBinaryExpression): Integer;
var
  left, right: Integer;
begin
  left := EvaluateExpression(ANode.Left);
  right := EvaluateExpression(ANode.Right);

  case ANode.OperatorKind of
    BBOK_Addition: Result := left + right;
    BBOK_Subtraction: Result := left - right;
    BBOK_Multiplication: Result := left * right;
    BBOK_Division: Result := left div right;
    end;
end;

function TEvaluator.EvaluateLiteralExpression(ANode: TBoundLiteralExpression): Integer;
begin
  Result := ANode.Value.IntegerValue;
end;

function TEvaluator.EvaluateUnaryExpression(ANode: TBoundUnaryExpression): Integer;
var
  operand: Integer;
begin
  operand := EvaluateExpression(ANode.Operand);

  case ANode.OperatorKind of
    BUOK_Identity: Result := +operand;
    BUOK_ArithmeticNegation: Result := -operand;
    end;
end;

constructor TEvaluator.Create(ARoot: TBoundExpression);
begin
  FRoot := ARoot;
end;

function TEvaluator.Evaluate: Integer;
begin
  Result := EvaluateExpression(FRoot);
end;

end.
