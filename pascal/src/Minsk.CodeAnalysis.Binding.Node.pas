unit Minsk.CodeAnalysis.Binding.Node;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.Runtime.Types;

type
  TBoundNodeKind = (
    BNK_BinaryExpression,
    BNK_LiteralExpression,
    BNK_UnaryExpression);

  TBoundNode = class
  public
    function GetKind: TBoundNodeKind; virtual; abstract;
    property Kind: TBoundNodeKind read GetKind;
  end;

  TBoundExpression = class(TBoundNode)
  public
    function GetValueType: TMinskType; virtual; abstract;
    property ValueType: TMinskType read GetValueType;
  end;

  TBoundLiteralExpression = class(TBoundExpression)
  private
    FValue: TMinskValue;

  public
    constructor Create(AValue: TMinskValue);
    function GetKind: TBoundNodeKind; override;
    function GetValueType: TMinskType; override;
    property Value: TMinskValue read FValue;
  end;

  TBoundUnaryOperatorKind = (
    BUOK_Identity,
    BUOK_ArithmeticNegation,
    BUOK_LogicalNegation);

  TBoundUnaryOperator = class
  private
    FSyntaxKind: TSyntaxKind;
    FKind: TBoundUnaryOperatorKind;
    FOperandType: TMinskType;
    FResultType: TMinskType;

    constructor Create(ASyntaxKind: TSyntaxKind; AKind: TBoundUnaryOperatorKind; AOperandType, AResultType: TMinskType); overload;
    constructor Create(ASyntaxKind: TSyntaxKind; AKind: TBoundUnaryOperatorKind; AOperandType: TMinskType); overload;

  public
    class function Bind(
      AOperatorKind: TSyntaxKind; AOperandType: TMinskType;
      out AOperator: TBoundUnaryOperator): Boolean; static;
    property SyntaxKind: TSyntaxKind read FSyntaxKind;
    property Kind: TBoundUnaryOperatorKind read FKind;
    property OperandType: TMinskType read FOperandType;
    property ResultType: TMinskType read FResultType;
  end;

  TBoundUnaryExpression = class(TBoundExpression)
  private
    FOperator: TBoundUnaryOperator;
    FOperand: TBoundExpression;

  public
    constructor Create(AOperator: TBoundUnaryOperator; AOperand: TBoundExpression);
    function GetKind: TBoundNodeKind; override;
    function GetValueType: TMinskType; override;
    property Op: TBoundUnaryOperator read FOperator;
    property Operand: TBoundExpression read FOperand;
  end;

  TBoundBinaryOperatorKind = (
    BBOK_Addition,
    BBOK_Subtraction,
    BBOK_Multiplication,
    BBOK_Division,
    BBOK_LogicalAnd,
    BBOK_LogicalOr);

  TBoundBinaryOperator = class
  private
    FSyntaxKind: TSyntaxKind;
    FKind: TBoundBinaryOperatorKind;
    FLeftType: TMinskType;
    FRightType: TMinskType;
    FResultType: TMinskType;

    constructor Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; ALeftType, ARightType, AResultType: TMinskType); overload;
    constructor Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; ALeftType, ARightType: TMinskType); overload;
    constructor Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; AType: TMinskType); overload;

  public
    class function Bind(
      AOperatorKind: TSyntaxKind; ALeftType, ARightType: TMinskType;
      out AOperator: TBoundBinaryOperator): Boolean; static;
    property SyntaxKind: TSyntaxKind read FSyntaxKind;
    property Kind: TBoundBinaryOperatorKind read FKind;
    property LeftType: TMinskType read FLeftType;
    property RightType: TMinskType read FRightType;
    property ResultType: TMinskType read FResultType;
  end;

  TBoundBinaryExpression = class(TBoundExpression)
  private
    FOperator: TBoundBinaryOperator;
    FLeft: TBoundExpression;
    FRight: TBoundExpression;

  public
    constructor Create(AOperator: TBoundBinaryOperator; ALeft, ARight: TBoundExpression);
    function GetKind: TBoundNodeKind; override;
    function GetValueType: TMinskType; override;
    property Op: TBoundBinaryOperator read FOperator;
    property Left: TBoundExpression read FLeft;
    property Right: TBoundExpression read FRight;
  end;

implementation

{ TBoundLiteralExpression }
constructor TBoundLiteralExpression.Create(AValue: TMinskValue);
begin
  inherited Create;
  FValue := AValue;
end;

function TBoundLiteralExpression.GetKind: TBoundNodeKind;
begin
  Result := BNK_LiteralExpression;
end;

function TBoundLiteralExpression.GetValueType: TMinskType;
begin
  Result := FValue.MinskType;
end;

{ TBoundUnaryOperator }
constructor TBoundUnaryOperator.Create(ASyntaxKind: TSyntaxKind; AKind: TBoundUnaryOperatorKind; AOperandType, AResultType: TMinskType);
begin
  inherited Create;
  FSyntaxKind := ASyntaxKind;
  FKind := AKind;
  FOperandType := AOperandType;
  FResultType := AResultType;
end;

constructor TBoundUnaryOperator.Create(ASyntaxKind: TSyntaxKind; AKind: TBoundUnaryOperatorKind; AOperandType: TMinskType);
begin
  Create(ASyntaxKind, AKind, AOperandType, AOperandType);
end;

var
  UnaryOperators: array of TBoundUnaryOperator;

class function TBoundUnaryOperator.Bind(
  AOperatorKind: TSyntaxKind; AOperandType: TMinskType;
  out AOperator: TBoundUnaryOperator): Boolean;
var
  op: TBoundUnaryOperator;
begin
  for op in UnaryOperators do
    if (op.SyntaxKind = AOperatorKind) and (op.OperandType = AOperandType) then
      begin
      AOperator := op;
      Exit(true);
      end;
  Result := false;
end;

{ TBoundUnaryExpression }
constructor TBoundUnaryExpression.Create(AOperator: TBoundUnaryOperator; AOperand: TBoundExpression);
begin
  inherited Create;
  FOperator := AOperator;
  FOperand := AOperand;
end;

function TBoundUnaryExpression.GetKind: TBoundNodeKind;
begin
  Result := BNK_UnaryExpression;
end;

function TBoundUnaryExpression.GetValueType: TMinskType;
begin
  Result := FOperator.ResultType;
end;

{ TBoundBinaryOperator }
constructor TBoundBinaryOperator.Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; ALeftType, ARightType, AResultType: TMinskType);
begin
  inherited Create;
  FSyntaxKind := ASyntaxKind;
  FKind := AKind;
  FLeftType := ALeftType;
  FRightType := ARightType;
  FResultType := AResultType;
end;

constructor TBoundBinaryOperator.Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; ALeftType, ARightType: TMinskType);
begin
  Create(ASyntaxKind, AKind, ALeftType, ARightType, ALeftType);
end;

constructor TBoundBinaryOperator.Create(ASyntaxKind: TSyntaxKind; AKind: TBoundBinaryOperatorKind; AType: TMinskType);
begin
  Create(ASyntaxKind, AKind, AType, AType, AType);
end;

var
  BinaryOperators: array of TBoundBinaryOperator;

class function TBoundBinaryOperator.Bind(
  AOperatorKind: TSyntaxKind; ALeftType, ARightType: TMinskType;
  out AOperator: TBoundBinaryOperator): Boolean;
var
  op: TBoundBinaryOperator;
begin
  for op in BinaryOperators do
    if (op.SyntaxKind = AOperatorKind) and (op.LeftType = ALeftType) and (op.RightType = ARightType) then
      begin
      AOperator := op;
      Exit(true);
      end;
  Result := false;
end;

{ TBoundBinaryExpression }
constructor TBoundBinaryExpression.Create(AOperator: TBoundBinaryOperator; ALeft, ARight: TBoundExpression);
begin
  inherited Create;
  FOperator := AOperator;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoundBinaryExpression.GetKind: TBoundNodeKind;
begin
  Result := BNK_BinaryExpression;
end;

function TBoundBinaryExpression.GetValueType: TMinskType;
begin
  Result := FOperator.ResultType;
end;

initialization

  UnaryOperators := [
    TBoundUnaryOperator.Create(SK_BangToken, BUOK_LogicalNegation, mtBoolean),
    TBoundUnaryOperator.Create(SK_PlusToken, BUOK_Identity, mtInteger),
    TBoundUnaryOperator.Create(SK_MinusToken, BUOK_ArithmeticNegation, mtInteger)];

  BinaryOperators := [
    TBoundBinaryOperator.Create(SK_PlusToken, BBOK_Addition, mtInteger),
    TBoundBinaryOperator.Create(SK_MinusToken, BBOK_Subtraction, mtInteger),
    TBoundBinaryOperator.Create(SK_StarToken, BBOK_Multiplication, mtInteger),
    TBoundBinaryOperator.Create(SK_SlashToken, BBOK_Division, mtInteger),
    TBoundBinaryOperator.Create(SK_AmpersandAmpersandToken, BBOK_LogicalAnd, mtBoolean),
    TBoundBinaryOperator.Create(SK_PipePipeToken, BBOK_LogicalOr, mtBoolean)];

end.
