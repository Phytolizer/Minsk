unit Minsk.CodeAnalysis.Syntax.Facts;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node;

function GetBinaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
function GetUnaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
function GetKeywordKind(AText: string): TSyntaxKind;

implementation

function GetBinaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
begin
  case AKind of
    SK_StarToken,
    SK_SlashToken:
      Result := 4;
    SK_PlusToken,
    SK_MinusToken:
      Result := 3;
    SK_AmpersandAmpersandToken:
      Result := 2;
    SK_PipePipeToken:
      Result := 1;
    else
      Result := 0;
    end;
end;

function GetUnaryOperatorPrecedence(AKind: TSyntaxKind): Integer;
begin
  case AKind of
    SK_PlusToken,
    SK_MinusToken,
    SK_BangToken:
      Result := 5;
    else
      Result := 0;
    end;
end;

function GetKeywordKind(AText: string): TSyntaxKind;
begin
  if AText = 'true' then
    Result := SK_TrueKeyword
  else if AText = 'false' then
    Result := SK_FalseKeyword
  else
    Result := SK_IdentifierToken;
end;

end.
