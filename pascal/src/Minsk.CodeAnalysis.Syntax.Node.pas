unit Minsk.CodeAnalysis.Syntax.Node;

interface

type
  TSyntaxKind = (
    SK_BadToken,
    SK_EndOfFileToken,

    SK_NumberToken,
    SK_WhitespaceToken,

    SK_PlusToken,
    SK_MinusToken,
    SK_StarToken,
    SK_SlashToken,
    SK_OpenParenthesisToken,
    SK_CloseParenthesisToken,

    SK_LiteralExpression,
    SK_BinaryExpression,
    SK_ParenthesizedExpression);

  TSyntaxNode         = class;
  TSyntaxNodeChildren = array of TSyntaxNode;

  TSyntaxNode = class
  private
    procedure PrettyPrint(AIndent: String; AIsLast: Boolean);
  public
    function GetKind: TSyntaxKind; virtual; abstract;
    property Kind: TSyntaxKind read GetKind;

    function GetChildren: TSyntaxNodeChildren; virtual; abstract;
    property Children: TSyntaxNodeChildren read GetChildren;

    procedure PrettyPrint;
  end;

function SyntaxKindToString(SyntaxKind: TSyntaxKind): String;

implementation

uses
  TypInfo,
  StrUtils,
  Variants,
  Minsk.CodeAnalysis.Syntax.Token;

function SyntaxKindToString(SyntaxKind: TSyntaxKind): String;
begin
  Result := GetEnumName(TypeInfo(TSyntaxKind), Ord(SyntaxKind));
  Delete(Result, 1, 3);
end;

procedure TSyntaxNode.PrettyPrint(AIndent: String; AIsLast: Boolean);
var
  LMarker: String;
  LChildren: TSyntaxNodeChildren;
  LI: Integer;
begin
  if AIsLast then
    LMarker := '└──'
  else
    LMarker := '├──';

  Write(AIndent, LMarker, SyntaxKindToString(Kind));
  if EndsStr('Token', SyntaxKindToString(Kind)) and not VarIsNull(TSyntaxToken(Self).Value) then
    Write(' ', TSyntaxToken(Self).Value);
  WriteLn;

  if AIsLast then
    AIndent := Concat(AIndent, '   ')
  else
    AIndent := Concat(AIndent, '│  ');

  LChildren := Children;
  for LI := 0 to Length(LChildren) - 1 do
    LChildren[LI].PrettyPrint(AIndent, LI = Length(LChildren) - 1);
end;

procedure TSyntaxNode.PrettyPrint;
begin
  PrettyPrint('', true);
end;

end.
