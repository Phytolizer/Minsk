namespace mc;

internal static class Program
{
    private static void Main(string[] args)
    {
        while (true)
        {
            Console.Write("> ");
            var line = Console.ReadLine();
            if (line == null)
            {
                break;
            }

            foreach (var token in new Minsk.Lexer(line))
            {
                Console.WriteLine(token);
            }
        }
    }
}
