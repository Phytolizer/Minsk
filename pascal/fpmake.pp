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
    p.OSes := [win32, linux];
    p.Directory := 'src';
    t := p.Targets.AddUnit('Minsk.pas');
    with p.Targets.AddProgram('MC.pas') do
      begin
      d := Dependencies.AddUnit('Minsk.pas');
      d.Target := t;
      end;

    Run;
    end;
end.
