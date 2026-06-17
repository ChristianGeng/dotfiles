# Lint Long Lines 

---
description: Lint e501
auto_execution_mode: 3
---

## Objective

- Goal: at the end I want all PEP E501 long line issues to be gone! 
- Before you start: Fix linting and use `uvx ruff check --fix --unsafe-fixes .`
- Before you start: Format code `uvx ruff format .`
- Stay in the current project - where you find `pyproject.toml`
- After you finish run the linter again as above and verify
that all E501 messages are gone. Otherwise scan again

Fix ALL long log messages:


```python 
log_message = (
    f"User '{user}' triggered error "
    f"at '{timestamp}'. "
    f"Returned error code {error_code}."
)
```

Also include commented log messages:

```
# logger.debug(f"EchoProcessor: Pre-rolling InputAudioRawFrame - {size} bytes (not speaking)")
```
These should stay commented out. But they should be in several lines such they conform to
PEP standards.


Fix Semantic line breaks of docstrings:

Example(s):


```python
        # This DOES add fixed latency: lead_in_chunks * frame_duration. Set to 0 to disable.
```

could become 


```python
        # This DOES add fixed latency: 
        # lead_in_chunks * frame_duration. 
        # Set to 0 to disable.
```

Do NEVER EVER change the text. Just fix the linebreaking
to use semantic line breaking.



