## Description: Generate Sphinx documentation that mirrors the `audfoo` layout, including API stubs in `docs/api-source/`.

## Step 1 · Validate project layout
- **Confirm structure**: Ensure `{{project_path}}/{{docs_dir}}/conf.py` exists
  so the workflow can reuse the existing Sphinx configuration.
- **Align expectations**: The workflow assumes the `conf.py` copies files from
  `docs/api-source/` to `docs/api/`, just like `audfoo/docs/conf.py`.

## Step 2 · Install documentation dependencies
- **Use uv first**: If `pyproject.toml` declares a docs group, run `uv
  sync --group docs` inside `{{project_path}}`.
- **Fallback**: Otherwise run `uv pip install -r
  {{project_path}}/{{docs_dir}}/requirements.txt`.
- **Notebook support**: If notebooks are present, ensure `jupyter` and
  `nbsphinx` are installed.

## Step 3 · Refresh API source files
- **Clean target**: Remove stale API sources. Cascade will prompt before running the destructive command.
  - `rm -rf {{project_path}}/{{docs_dir}}/api-source {{project_path}}/{{docs_dir}}/api`
- **Regenerate stubs**: Run a Python helper that walks the declared packages,
  runs `sphinx-apidoc` per top-level module, and mirrors the `audfoo` copy step.
  - ```bash
    python - <<'PY'
    import os
    import shlex
    import subprocess
    import sys

    import audeer

    project_path = audeer.path(sys.argv[1])
    docs_dir = audeer.path(project_path, sys.argv[2])
    package_dirs = [part.strip() for part in sys.argv[3].split(',') if part.strip()]
    builder = sys.argv[4]

    api_src_dir = audeer.path(docs_dir, 'api-source')
    api_dir = audeer.path(docs_dir, 'api')

    audeer.rmdir(api_src_dir)
    audeer.rmdir(api_dir)
    audeer.mkdir(api_src_dir)
    audeer.mkdir(api_dir)

    package_roots = []
    for candidate in package_dirs:
        root = audeer.path(project_path, candidate)
        if not os.path.exists(root):
            continue
        for entry in os.listdir(root):
            entry_path = audeer.path(root, entry)
            init_py = audeer.path(entry_path, '__init__.py')
            if os.path.isdir(entry_path) and os.path.isfile(init_py):
                package_roots.append(entry_path)

    if not package_roots:
        raise SystemExit('No Python packages found to document.')

    env = os.environ.copy()
    env.setdefault('PYTHONPATH', project_path)

    for pkg in package_roots:
        cmd = [
            'sphinx-apidoc',
            '--module-first',
            '--separate',
            '--force',
            '--output-dir', api_src_dir,
            pkg,
        ]
        subprocess.run(cmd, check=True, env=env)

    # Mirror audfoo: copy generated files into docs/api/
    for rst_file in audeer.list_file_names(api_src_dir, filetype='.rst'):
        target = audeer.path(api_dir, os.path.basename(rst_file))
        audeer.copy_file(rst_file, target)

    print('Generated API docs for:')
    for pkg in package_roots:
        print(f'  - {pkg}')
    PY
    "{{project_path}}" "{{docs_dir}}" "{{package_dirs}}" "{{builder}}"
    ```
- **Review output**: Confirm a `.rst` exists per module inside `{{docs_dir}}/api-source/`.

## Step 4 · Build the documentation
- **Prepare build directories**: `audeer.mkdir(audeer.path({{project_path}},
  'build/sphinx/html'))` if needed.
- **Invoke Sphinx**: `python -m sphinx -W {{project_path}}/{{docs_dir}} -b
  {{builder}} -d {{project_path}}/build/sphinx/doctrees
  {{project_path}}/build/sphinx/{{builder}}`
- **Optional checks**: Run `python -m sphinx -W {{project_path}}/{{docs_dir}}
  {{project_path}}/build/sphinx/linkcheck -b linkcheck` when link validation is
  required.

## Step 5 · Surface artifacts
- **HTML preview**: Open `{{project_path}}/build/sphinx/{{builder}}/index.html`
  in Google Chrome.
- **Persist for CI**: Upload `build/sphinx/{{builder}}` as the workflow artifact
  if running in automation.

## Step 6 · Wrap-up
- **Summarize**: Report generated module list and any Sphinx warnings.
- **Next actions**: Suggest commit commands or CI follow-ups if everything built
  cleanly.
