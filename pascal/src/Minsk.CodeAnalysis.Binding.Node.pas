unit Minsk.CodeAnalysis.Binding.Node;

interface

uses
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

  TBoundUnaryExpression = class(TBoundExpression)
  private
    FOperatorKind: TBoundUnaryOperatorKind;
    FOperand: TBoundExpression;

  public
    constructor Create(AOperatorKind: TBoundUnaryOperatorKind; AOperand: TBoundExpression);
    function GetKind: TBoundNodeKind; override;
    function GetValueType: TMinskType; override;
    property OperatorKind: TBoundUnaryOperatorKind read FOperatorKind;
    property Operand: TBoundExpression read FOperand;
  end;

  TBoundBinaryOperatorKind = (
    BBOK_Addition,
    BBOK_Subtraction,
    BBOK_Multiplication,
    BBOK_Division,
    BBOK_LogicalAnd,
    BBOK_LogicalOr);

  TBoundBinaryExpression = class(TBoundExpression)
  private
    FOperatorKind: TBoundBinaryOperatorKind;
    FLeft: TBoundExpression;
    FRight: TBoundExpression;

  public
    constructor Create(AOperatorKind: TBoundBinaryOperatorKind; ALeft, ARight: TBoundExpression);
    function GetKind: TBoundNodeKind; override;
    function GetValueType: TMinskType; override;
    property OperatorKind: TBoundBinaryOperatorKind read FOperatorKind;
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

{ TBoundUnaryExpression }
constructor TBoundUnaryExpression.Create(AOperatorKind: TBoundUnaryOperatorKind; AOperand: TBoundExpression);
begin
  inherited Create;
  FOperatorKind := AOperatorKind;
  FOperand := AOperand;
end;

function TBoundUnaryExpression.GetKind: TBoundNodeKind;
begin
  Result := BNK_UnaryExpression;
end;

function TBoundUnaryExpression.GetValueType: TMinskType;
begin
  Result := FOperand.ValueType;
end;

{ TBoundBinaryExpression }
constructor TBoundBinaryExpression.Create(AOperatorKind: TBoundBinaryOperatorKind; ALeft, ARight: TBoundExpression);
begin
  inherited Create;
  FOperatorKind := AOperatorKind;
  FLeft := ALeft;
  FRight := ARight;
end;

function TBoundBinaryExpression.GetKind: TBoundNodeKind;
begin
  Result := BNK_BinaryExpression;
end;

function TBoundBinaryExpression.GetValueType: TMinskType;
begin
  Result := FLeft.ValueType;
end;

end.
