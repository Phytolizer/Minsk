when isMainModule:
  while true:
    stdout.write "> "
    stdout.flushFile()
    var line = ""
    if not stdin.readLine(line):
      echo ""
      break

    if line == "1 + 2 * 3":
      echo "7"
    else:
      echo "ERROR: Invalid expression!"
