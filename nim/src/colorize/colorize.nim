const
  colorReset* = "\e[0m"

  styleBold* = "\e[1m"
  styleDim* = "\e[2m"
  styleItalic* = "\e[3m"
  styleUnderline* = "\e[4m"
  styleBlink* = "\e[5m"
  styleReverse* = "\e[7m"
  styleHidden* = "\e[8m"

  fgBlack* = "\e[30m"
  fgRed* = "\e[31m"
  fgGreen* = "\e[32m"
  fgYellow* = "\e[33m"
  fgBlue* = "\e[34m"
  fgMagenta* = "\e[35m"
  fgCyan* = "\e[36m"
  fgWhite* = "\e[37m"

  bgBlack* = "\e[40m"
  bgRed* = "\e[41m"
  bgGreen* = "\e[42m"
  bgYellow* = "\e[43m"
  bgBlue* = "\e[44m"
  bgMagenta* = "\e[45m"
  bgCyan* = "\e[46m"
  bgWhite* = "\e[47m"

  fgBrightBlack* = "\e[90m"
  fgBrightRed* = "\e[91m"
  fgBrightGreen* = "\e[92m"
  fgBrightYellow* = "\e[93m"
  fgBrightBlue* = "\e[94m"
  fgBrightMagenta* = "\e[95m"
  fgBrightCyan* = "\e[96m"
  fgBrightWhite* = "\e[97m"

  bgBrightBlack* = "\e[100m"
  bgBrightRed* = "\e[101m"
  bgBrightGreen* = "\e[102m"
  bgBrightYellow* = "\e[103m"
  bgBrightBlue* = "\e[104m"
  bgBrightMagenta* = "\e[105m"
  bgBrightCyan* = "\e[106m"
  bgBrightWhite* = "\e[107m"

proc fg256*(color: int): string = "\e[38;5;" & $color & "m"

proc bg256*(color: int): string = "\e[48;5;" & $color & "m"

proc fgRGB*(r, g, b: int): string = "\e[38;2;" & $r & ";" & $g & ";" & $b & "m"

proc bgRGB*(r, g, b: int): string = "\e[48;2;" & $r & ";" & $g & ";" & $b & "m"

proc fgRGB*(rgb: tuple[r, g, b: int]): string = fgRGB(rgb.r, rgb.g, rgb.b)

proc bgRGB*(rgb: tuple[r, g, b: int]): string = bgRGB(rgb.r, rgb.g, rgb.b)

proc fgRGB*(rgb: uint32): string = fgRGB((
  r: ((rgb shr 16) and 0xff).int,
  g: ((rgb shr 8) and 0xff).int,
  b: (rgb and 0xff).int
))

proc bgRGB*(rgb: uint32): string = bgRGB((
  r: ((rgb shr 16) and 0xff).int,
  g: ((rgb shr 8) and 0xff).int,
  b: (rgb and 0xff).int
))

proc setColor*(file: File, args: varargs[string]) =
  for arg in args:
    file.write(arg)

proc resetColor*(file: File) =
  file.write(colorReset)
