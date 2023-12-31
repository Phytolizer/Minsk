unit Minsk.CodeAnalysis.Syntax.Lexer;

interface

uses
  Minsk.CodeAnalysis.Syntax.Facts,
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token,
  Minsk.Runtime.Types;

type
  TStringArray = array of String;

  TLexer = class
  private
    FText: String;
    FPosition: Integer;
    FDiagnostics: TStringArray;

    function Peek(distance: Integer): Char;
    function Current: Char;
    class function IsDigit(c: Char): Boolean; static;
    class function IsWhiteSpace(c: Char): Boolean; static;
    class function IsLetter(c: Char): Boolean; static;
    procedure LexNumberToken;
    procedure LexWhitespaceToken;
    procedure LexIdentifierOrKeyword;

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

class function TLexer.IsLetter(c: Char): Boolean;
begin
  case c of
    'a'..'z', 'A'..'Z', '_': Result := true;
    else
      Result := false;
    end;
end;

procedure TLexer.LexNumberToken;
begin
  while IsDigit(Current) do
    Inc(FPosition);
end;

procedure TLexer.LexWhitespaceToken;
begin
  while IsWhiteSpace(Current) do
    Inc(FPosition);
end;

procedure TLexer.LexIdentifierOrKeyword;
begin
  while IsLetter(Current) or IsDigit(Current) do
    Inc(FPosition);
end;

constructor TLexer.Create(AText: String);
begin
  FText := AText;
  FPosition := Low(FText);
  FDiagnostics := nil;
end;

function TLexer.NextToken: TSyntaxToken;
var
  kind: TSyntaxKind = SK_BadToken;
  start: Integer;
  text: String;
  code: Integer;
  value: TMinskValue;
begin
  start := FPosition;
  text := '';
  value := MinskNull;
  case Current of
    #0: kind := SK_EndOfFileToken;
    '+':
      begin
      kind := SK_PlusToken;
      Inc(FPosition);
      end;
    '-':
      begin
      kind := SK_MinusToken;
      Inc(FPosition);
      end;
    '*':
      begin
      kind := SK_StarToken;
      Inc(FPosition);
      end;
    '/':
      begin
      kind := SK_SlashToken;
      Inc(FPosition);
      end;
    '(':
      begin
      kind := SK_OpenParenthesisToken;
      Inc(FPosition);
      end;
    ')':
      begin
      kind := SK_CloseParenthesisToken;
      Inc(FPosition);
      end;
    '!':
      if Peek(1) = '=' then
        begin
        kind := SK_BangEqualsToken;
        Inc(FPosition, 2);
        end
      else
        begin
        kind := SK_BangToken;
        Inc(FPosition);
        end;
    '=':
      if Peek(1) = '=' then
        begin
        kind := SK_EqualsEqualsToken;
        Inc(FPosition, 2);
        end;
    '&':
      if Peek(1) = '&' then
        begin
        kind := SK_AmpersandAmpersandToken;
        Inc(FPosition, 2);
        end;
    '|':
      if Peek(1) = '|' then
        begin
        kind := SK_PipePipeToken;
        Inc(FPosition, 2);
        end;
    else
      if IsDigit(Current) then
        begin
        LexNumberToken;
        text := Copy(FText, start, FPosition - start);
        value.MinskType := mtInteger;
        Val(text, value.IntegerValue, code);
        Kind := SK_NumberToken;
        end
      else if IsWhiteSpace(Current) then
        begin
        LexWhitespaceToken;
        Kind := SK_WhitespaceToken;
        end
      else if IsLetter(Current) then
        begin
        LexIdentifierOrKeyword;
        text := Copy(FText, start, FPosition - start);
        kind := GetKeywordKind(text);
        end;
    end;
  if kind = SK_BadToken then
    begin
    SetLength(FDiagnostics, Length(FDiagnostics) + 1);
    FDiagnostics[High(FDiagnostics)] := Format('ERROR: bad character input: ''%s''', [Current]);
    Result := TSyntaxToken.Create(SK_BadToken, FPosition, Copy(FText, FPosition, 1), MinskNull);
    Inc(FPosition);
    end;

  if text = '' then
    text := Copy(FText, start, FPosition - start);

  Result := TSyntaxToken.Create(kind, start, text, value);
end;

end.
