unit Minsk.CodeAnalysis.Syntax.Tree;

interface

uses
  Minsk.CodeAnalysis.Syntax.Node,
  Minsk.CodeAnalysis.Syntax.Token,
  Minsk.CodeAnalysis.Syntax.AST;

type
  TStringArray = array of String;

  TSyntaxTree = class
  private
    FDiagnostics:    TStringArray;
    FRoot:           TExpressionSyntax;
    FEndOfFileToken: TSyntaxToken;

  public
    constructor Create(ADiagnostics: TStringArray; ARoot: TExpressionSyntax; AEndOfFileToken: TSyntaxToken);
    property Diagnostics: TStringArray read FDiagnostics;
    property Root: TExpressionSyntax read FRoot;
    property EndOfFileToken: TSyntaxToken read FEndOfFileToken;

    class function Parse(AText: String): TSyntaxTree; static;
  end;

implementation

uses
  Minsk.CodeAnalysis.Syntax.Parser;

{ TSyntaxTree }
constructor TSyntaxTree.Create(ADiagnostics: TStringArray; ARoot: TExpressionSyntax; AEndOfFileToken: TSyntaxToken);
begin
  FDiagnostics := ADiagnostics;
  FRoot := ARoot;
  FEndOfFileToken := AEndOfFileToken;
end;

class function TSyntaxTree.Parse(AText: String): TSyntaxTree;
var
  Parser: TParser;
begin
  parser := TParser.Create(AText);
  Result := parser.Parse;
end;

end.
