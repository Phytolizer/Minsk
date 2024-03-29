﻿program MC;

uses
  Minsk.CodeAnalysis.Syntax.Tree,
  Minsk.CodeAnalysis.Binding.Binder,
  Minsk.CodeAnalysis.Binding.Node,
  Minsk.CodeAnalysis.Evaluator,
  Minsk.Runtime.Types,
  DynLibs,
  CTypes;

const
  libreadline = 'readline';
  histfile = '.minsk_history';

function EditLine(prompt: PChar): PChar; cdecl; external libreadline name 'readline';
procedure AddHistory(line: PChar); cdecl; external libreadline name 'add_history';
function LoadHistory(filename: PChar): cint; cdecl; external libreadline name 'read_history';
function SaveHistory(filename: PChar): cint; cdecl; external libreadline name 'write_history';

type
  TPFree = procedure(p: Pointer); cdecl;

var
  showTree: Boolean;
  rawLine: PChar;
  line: String;
  libc: TLibHandle;
  CFree: TPFree;
  syntaxTree: TSyntaxTree;
  diagnostic: String;
  evaluator: TEvaluator;
  diagnostics: TStringArray;
  binder: TBinder;
  boundExpression: TBoundExpression;

begin
  libc := LoadLibrary('libc.so');
  CFree := TPFree(GetProcedureAddress(libc, 'free'));
  showTree := false;
  LoadHistory(histfile);

  while true do
    begin
    rawLine := EditLine('> ');
    if rawLine = nil then
      break;
    line := rawLine;
    if line <> '' then
      AddHistory(rawLine);
    CFree(rawLine);

    if line = '#showTree' then
      begin
      showTree := not showTree;
      if showTree then
        WriteLn('Showing parse trees.')
      else
        WriteLn('Not showing parse trees.');
      continue;
      end;

    if line = '#cls' then
      begin
      Write(#27'[2J', #27'[H');
      Flush(Output);
      continue;
      end;

    syntaxTree := TSyntaxTree.Parse(line);
    binder := TBinder.Create;
    boundExpression := binder.BindExpression(syntaxTree.Root);
    diagnostics := Concat(syntaxTree.Diagnostics, binder.Diagnostics);

    if showTree then
      syntaxTree.Root.PrettyPrint;

    if Length(diagnostics) > 0 then
      for diagnostic in diagnostics do
        WriteLn(diagnostic)
    else
      begin
      evaluator := TEvaluator.Create(boundExpression);
      WriteMinskValue(evaluator.Evaluate);
      WriteLn;
      end;
    end;

  SaveHistory(histfile);
end.
