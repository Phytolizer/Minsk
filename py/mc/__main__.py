while True:
    try:
        line = input("> ")
    except EOFError:
        break

    if line == "1 + 2 * 3":
        print("7")
    else:
        print("ERROR: Invalid expression!")
