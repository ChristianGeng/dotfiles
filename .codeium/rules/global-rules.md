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

Use https://github.com/audeering/audeer/ for path management and other utilities.

- For path management, rely on `audeer` features  rather than `pathlib.Path`

Example:

```python
import audeer
audeer.mkdir("path/to/directory")
audeer.path("a/b/c", "test")
```

### use audiofile for audio processing

Use https://github.com/audeering/audiofile/ for audio processing.

- read audio file
- write audio file
- convert to wav
- resample audio file
- count the number of samples in an audio file
- obtain the sampling rate of an audio file
- obtain the number of channels in an audio file
- obtain the duration of an audio file
- check if an audio file has video
- obtain bitdepth of audio files


### click_preferred
Category: Python / CLI Design

- Always use the click library for command-line interface (CLI) implementations in Python. 
- Avoid using argparse unless explicitly required for compatibility reasons.

#### Rationale:
*click* provides a more declarative, composable, and Pythonic way to define CLI commands and options. It supports decorators, reusable options, and better user experience out of the box.


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


### Docstrings

- Docstring for all public methods please
- Use google/napoleon style
- Respect long lines aka PEP e501
- D401 First line of docstring should be in imperative mood

### Avoid patching sys.path

- Rather than just adding a path in order to fix import issues,
use relative imports or request a package structure.


### Tests

- tests use pytest
- all tests are in `tests` relative to the project root
- All test requirements are set in `pyproject.toml`

### Sphinx and Restructured Text

- Documentation is in the directory docs/
- `index.rst` contains the the top level file of the documentation
- Instead of peppering `README.md` files over the repo, use `docs/install.rst` and `docs/usage.rst` for this kind of documentation


