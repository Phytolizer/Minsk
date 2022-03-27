# Known Bugs

- The scoping semantics in the REPL don't seem to work very well.

  To reproduce the error:
  ```
    > var a = 10
    > a
    > a
  ```

  Guess: it doesn't properly look multiple layers deep into parent scopes.
