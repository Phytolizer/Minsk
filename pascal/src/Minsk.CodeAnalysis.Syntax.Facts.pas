unit Minsk.CodeAnalysis.Syntax.Facts;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node;

function GetBinaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
function GetUnaryOperatorPrecedence(AKind: TSyntaxKind): Integer;

implementation

function GetBinaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
begin
  case AKind of
    SK_StarToken,
    SK_SlashToken:
      Result := 2;
    SK_PlusToken,
    SK_MinusToken:
      Result := 1;
    else
      Result := 0;
    end;
end;

function GetUnaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
begin
  case AKind of
    SK_PlusToken,
    SK_MinusToken:
      Result := 3;
    else
      Result := 0;
    end;
end;

end.
