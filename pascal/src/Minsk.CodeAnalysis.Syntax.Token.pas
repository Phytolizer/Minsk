unit Minsk.CodeAnalysis.Syntax.Token;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Variants;

type
  TSyntaxToken = class(TSyntaxNode)
  private
    FKind: TSyntaxKind;
    FPosition: Integer;
    FText: String;
    FValue: Variant;

  public
    constructor Create(AKind: TSyntaxKind; APosition: Integer; AText: String; AValue: Variant);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;

    property Kind: TSyntaxKind read FKind;
    property Position: Integer read FPosition;
    property Text: String read FText;
    property Value: Variant read FValue;
  end;


implementation

{ TSyntaxToken }
constructor TSyntaxToken.Create(AKind: TSyntaxKind; APosition: Integer; AText: String; AValue: Variant);
begin
  FKind := AKind;
  FPosition := APosition;
  FText := AText;
  FValue := AValue;
end;

function TSyntaxToken.GetKind: TSyntaxKind;
begin
  Result := FKind;
end;

function TSyntaxToken.GetChildren: TSyntaxNodeChildren;
begin
  Result := nil;
end;

end.
