# Global Rules

## General

## Python

### Clear code

- Use type hints consistently
- If the project contains a Makefile, use its targets to run commands
- Use uv for Python package management
- Use pyproject.toml to adhere to PEP8 style for Python codel
- Use UTF-8 encoding for all files
- Use Git for version control
- If present, use CONTRIBUTING.rst for contribution guidelines and always follow them strictly
- Use docstrings to document functions and classes
- Break long lines into multiple lines, both for logging and when using print

### Long lines

Good example:

```python
log_message = (
    f"User '{user}' triggered error "
    f"at '{timestamp}'. "
    f"Returned error code {error_code}."
)
```

Bad Example:

```python
log_message = f"User '{user}' triggered error at '{timestamp}'. Returned error code {error_code}. All this stuff causes a PEP8 error."
```

### Use audeer as utility package

- For path management, rely on `audeer` features  rather than `pathlib.Path`

Example:

```python
import audeer
audeer.mkdir("path/to/directory")
audeer.path("a/b/c", "test")
```

### Early returns

Use early returns when possible

Rather than using logs of try except blocks, catch early whereever you can

Bad Example:

```python
try:
    # do something
except Exception as e:
    logger.error(f"Error: {e}")
    raise
```

Good Example:

```python
if not condition:
    raise ValueError("Condition not met")
```


### Avoid patching sys.path

- Rather than just adding a path in order to fix import issues,
use relative imports or request a package structure.


