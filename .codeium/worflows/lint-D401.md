## Objective

- Goal: eliminate all PEP D401 (pydocstyle) violations: “First line should be in
  imperative mood”.
- Before you start: run `uvx ruff format .`
- Then run: `uvx ruff check --select D401 .`
  - Note: Ruff does not auto-fix D401; you must edit docstrings manually.
- Stay in the current project (the one with `pyproject.toml`).
- After you finish, run `uvx ruff check --select D401 .` again and verify that
  all D401 messages are gone.

## What D401 requires

- The first line of a docstring must:
  - Be a short, standalone summary in the imperative mood (command form).
  - Start with a verb in present tense (e.g., “Return”, “Create”, “Compute”,
    “Use”).
  - End with a period.
  - Not start with “This function…”, “Returns…”, “A…”, “The…”, “It…”, or
    “Deprecated: …”.

- Additional notes:
  - Keep all existing information, but move non-summary details (including
    “Deprecated:” notes) to subsequent lines/paragraphs.
  - Leave a blank line between the summary line and the remaining description.
  - Preserve meaning; do not change code behavior.

## How to fix

For each D401 violation:

1) Rewrite the first line to imperative mood and ensure it ends with a period.
2) If the first line included a deprecation note, move that note to the next
paragraph (after a blank line), starting with “Deprecated: …”. 3) Keep all
existing explanatory text below; do not delete information.

## Examples

- Bad:
  ```
  """This function returns the current value."""
  ```
  Good:
  ```
  """Return the current value."""
  ```

- Bad:
  ```
  """Returns the current user."""
  ```
  Good:
  ```
  """Return the current user."""
  ```

- Bad:
  ```
  """A context manager that opens a session."""
  ```
  Good (choose the correct verb for what the function does):
  ```
  """Return a context manager that opens a session."""
  ```
  or
  ```
  """Provide a context manager that opens a session."""
  ```

- Bad (deprecation in the first line):
  ```
  """Deprecated: Use register_livekit_session() instead.

  This function is kept for backwards compatibility but is deprecated.
  Please use register_livekit_session() for new code.
  """
  ```
  Good (imperative summary first; keep deprecation note after a blank line):
  ```
  """Use register_livekit_session() instead.

  Deprecated: This function is kept for backwards compatibility but is deprecated.
  Please use register_livekit_session() for new code.
  """
  ```

## Verification

- Run: `uvx ruff check --select D401 .`
- If any D401 remain, scan and fix again until the check passes.
