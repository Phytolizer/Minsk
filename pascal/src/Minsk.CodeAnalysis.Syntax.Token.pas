unit Minsk.CodeAnalysis.Syntax.Token;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.Runtime.Types;

type
  TSyntaxToken = class(TSyntaxNode)
  private
    FKind: TSyntaxKind;
    FPosition: Integer;
    FText: String;
    FValue: TMinskValue;

  public
    constructor Create(AKind: TSyntaxKind; APosition: Integer; AText: String; AValue: TMinskValue);
    function GetKind: TSyntaxKind; override;
    function GetChildren: TSyntaxNodeChildren; override;

    property Kind: TSyntaxKind read FKind;
    property Position: Integer read FPosition;
    property Text: String read FText;
    property Value: TMinskValue read FValue;
  end;


implementation

{ TSyntaxToken }
constructor TSyntaxToken.Create(AKind: TSyntaxKind; APosition: Integer; AText: String; AValue: TMinskValue);
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
