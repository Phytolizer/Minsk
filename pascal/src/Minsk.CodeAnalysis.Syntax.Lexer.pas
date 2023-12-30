﻿unit Minsk.CodeAnalysis.Syntax.Lexer;

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
    function LexNumberToken: TSyntaxToken;
    function LexWhitespaceToken: TSyntaxToken;
    function LexIdentifierOrKeyword: TSyntaxToken;

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
  Result := TSyntaxToken.Create(SK_NumberToken, start, svalue, MinskInteger(ivalue));
end;

function TLexer.LexWhitespaceToken: TSyntaxToken;
var
  start: Integer;
begin
  start := FPosition;
  while IsWhiteSpace(Current) do
    Inc(FPosition);

  Result := TSyntaxToken.Create(SK_WhitespaceToken, start, Copy(FText, start, FPosition - start), MinskNull);
end;

function TLexer.LexIdentifierOrKeyword: TSyntaxToken;
var
  start: Integer;
  svalue: String;
  kind: TSyntaxKind;
begin
  start := FPosition;
  while IsLetter(Current) or IsDigit(Current) do
    Inc(FPosition);

  svalue := Copy(FText, start, FPosition - start);
  kind := GetKeywordKind(svalue);

  Result := TSyntaxToken.Create(kind, start, svalue, MinskNull);
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
    #0: Result := TSyntaxToken.Create(SK_EndOfFileToken, FPosition, '', MinskNull);
    '+':
      begin
      Result := TSyntaxToken.Create(SK_PlusToken, FPosition, '+', MinskNull);
      Inc(FPosition);
      end;
    '-':
      begin
      Result := TSyntaxToken.Create(SK_MinusToken, FPosition, '-', MinskNull);
      Inc(FPosition);
      end;
    '*':
      begin
      Result := TSyntaxToken.Create(SK_StarToken, FPosition, '*', MinskNull);
      Inc(FPosition);
      end;
    '/':
      begin
      Result := TSyntaxToken.Create(SK_SlashToken, FPosition, '/', MinskNull);
      Inc(FPosition);
      end;
    '(':
      begin
      Result := TSyntaxToken.Create(SK_OpenParenthesisToken, FPosition, '(', MinskNull);
      Inc(FPosition);
      end;
    ')':
      begin
      Result := TSyntaxToken.Create(SK_CloseParenthesisToken, FPosition, ')', MinskNull);
      Inc(FPosition);
      end;
    else
      if IsDigit(Current) then
        Result := LexNumberToken
      else if IsWhiteSpace(Current) then
        Result := LexWhitespaceToken
      else if IsLetter(Current) then
        Result := LexIdentifierOrKeyword
      else
        begin
        SetLength(FDiagnostics, Length(FDiagnostics) + 1);
        FDiagnostics[High(FDiagnostics)] := Format('ERROR: bad character input: ''%s''', [Current]);
        Result := TSyntaxToken.Create(SK_BadToken, FPosition, Copy(FText, FPosition, 1), MinskNull);
        Inc(FPosition);
        end;
    end;
end;

end.
