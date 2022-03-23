namespace Minsk.CodeAnalysis;

public sealed record VariableSymbol(string Name, bool IsReadOnly, Type Type)
{
}
