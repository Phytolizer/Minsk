unit Minsk.CodeAnalysis.Syntax.Lexer;

interface

uses
  Variants,
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token;

type
  TStringArray = array of String;

  TLexer = class
  private
    FText:        String;
    FPosition:    Integer;
    FDiagnostics: TStringArray;

    function Peek(distance: Integer): Char;
    function Current: Char;
    class function IsDigit(c: Char): Boolean; static;
    class function IsWhiteSpace(c: Char): Boolean; static;
    function LexNumberToken: TSyntaxToken;
    function LexWhitespaceToken: TSyntaxToken;

  public
    constructor Create(AText: String);

    property Diagnostics: TStringArray read FDiagnostics;
    function NextToken: TSyntaxToken;
  end;

implementation

uses
  SysUtils;

{ TLexer }
function TLexer.Peek(distance: Integer): Char;
var
  index: Integer;
begin
  index := FPosition + distance;
  if index > High(FText) then
    Result := #0
  else
    Result := FText[index];
end;

function TLexer.Current: Char;
begin
  Result := Peek(0);
end;

class function TLexer.IsDigit(c: Char): Boolean;
begin
  case c of
    '0'..'9': Result := true;
    else
      Result := false;
    end;
end;

class function TLexer.IsWhiteSpace(c: Char): Boolean;
begin
  case c of
    ' ', #9, #10, #13: Result := true;
    else
      Result := false;
    end;
end;

function TLexer.LexNumberToken: TSyntaxToken;
var
  start: Integer;
  svalue: String;
  ivalue: Integer;
  code: Integer;
begin
  start := FPosition;
  while IsDigit(Current) do
    Inc(FPosition);

  svalue := Copy(FText, start, FPosition - start);
  Val(svalue, ivalue, code);
  Result := TSyntaxToken.Create(SK_NumberToken, start, svalue, ivalue);
end;

function TLexer.LexWhitespaceToken: TSyntaxToken;
var
  start: Integer;
begin
  start := FPosition;
  while IsWhiteSpace(Current) do
    Inc(FPosition);

  Result := TSyntaxToken.Create(SK_WhitespaceToken, start, Copy(FText, start, FPosition - start), Null);
end;

constructor TLexer.Create(AText: String);
begin
  FText := AText;
  FPosition := Low(FText);
  FDiagnostics := nil;
end;

function TLexer.NextToken: TSyntaxToken;
begin
  case Current of
    #0: Result := TSyntaxToken.Create(SK_EndOfFileToken, FPosition, '', Null);
    '+':
      begin
      Result := TSyntaxToken.Create(SK_PlusToken, FPosition, '+', Null);
      Inc(FPosition);
      end;
    '-':
      begin
      Result := TSyntaxToken.Create(SK_MinusToken, FPosition, '-', Null);
      Inc(FPosition);
      end;
    '*':
      begin
      Result := TSyntaxToken.Create(SK_StarToken, FPosition, '*', Null);
      Inc(FPosition);
      end;
    '/':
      begin
      Result := TSyntaxToken.Create(SK_SlashToken, FPosition, '/', Null);
      Inc(FPosition);
      end;
    '(':
      begin
      Result := TSyntaxToken.Create(SK_OpenParenthesisToken, FPosition, '(', Null);
      Inc(FPosition);
      end;
    ')':
      begin
      Result := TSyntaxToken.Create(SK_CloseParenthesisToken, FPosition, ')', Null);
      Inc(FPosition);
      end;
    else
      if IsDigit(Current) then
        Result := LexNumberToken
      else if IsWhiteSpace(Current) then
        Result := LexWhitespaceToken
      else
        begin
        SetLength(FDiagnostics, Length(FDiagnostics) + 1);
        FDiagnostics[High(FDiagnostics)] := Format('ERROR: bad character input: ''%s''', [Current]);
        Result := TSyntaxToken.Create(SK_BadToken, FPosition, Copy(FText, FPosition, 1), Null);
        Inc(FPosition);
        end;
    end;
end;

end.
