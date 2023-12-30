unit Minsk.CodeAnalysis.Binding.Node;

interface

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

implementation

end.
