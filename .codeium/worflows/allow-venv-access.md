
# Description

Relax `.venv/.gitignore` so Cascade can inspect `bin/` and `lib/`.

## Step 1 · Locate the `.gitignore`
- **Check existence**: Confirm that ``{{project_path}}/{{venv_dir}}/.gitignore`` is present. If missing, create an empty file so the workflow can manage it.

## Step 2 · Backup current rules
- **Preserve the original**: Run ``cp "{{project_path}}/{{venv_dir}}/.gitignore" "{{project_path}}/{{venv_dir}}/.gitignore.bak"`` to safeguard the previous configuration.

## Step 3 · Apply relaxed ignore rules
- **Append allowlist**: Add the following block to the end of ``{{project_path}}/{{venv_dir}}/.gitignore`` (create the file if it doesn’t exist):
  ```
  *
  !bin/
  !lib/
  ```
  Use your editor or run:
  ```bash
  printf '%s\n' '*' '!bin/' '!lib/' >> "{{project_path}}/{{venv_dir}}/.gitignore"
  ```

## Step 4 · Verify accessibility
- **Refresh view**: Reopen the folder in Cascade and ensure files like ``{{venv_dir}}/lib/python*/site-packages/__init__.py`` are now readable.
- **Optional cleanup**: If you need to revert, restore the backup with ``mv "{{project_path}}/{{venv_dir}}/.gitignore.bak" "{{project_path}}/{{venv_dir}}/.gitignore"``.
