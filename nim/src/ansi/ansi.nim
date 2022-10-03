proc clear*(f: File) =
  f.write "\e[2J\e[H"
