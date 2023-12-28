program fpmake;

uses
  fpmkunit;

var
  p: TPackage;
  t: TTarget;
  d: TDependency;

begin
  with Installer do
    begin
    p := AddPackage('Minsk');
    // mode: objfpc
    p.Options.Append('-Mobjfpc');
    // use AnsiString, not ShortString
    p.Options.Append('-Sh');
    // pointers are typed
    p.Options.Append('-Sy');
    // allow up to 5 errors, warnings are errors
    p.Options.Append('-Se5');
    // debug info
    p.Options.Append('-gw3');
    p.OSes := [win32, linux];
    p.Directory := 'src';
    p.Dependencies.Add('rtl-objpas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Node.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Facts.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Token.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Lexer.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.AST.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Parser.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Tree.pas');
    p.Targets.AddUnit('Minsk.CodeAnalysis.Syntax.Evaluator.pas');

    p.Targets.AddProgram('MC.pas');

    Run;
    end;
end.
