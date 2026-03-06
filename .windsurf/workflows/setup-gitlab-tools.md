# Setup GitLab Tools with daitools

---
description: Add daitools GitLab integration to a project Makefile
auto_execution_mode: 2
---

## Objective

Set up daitools GitLab CLI tools in a project by:
1. Adding GitLab targets to the project's Makefile via include
2. Configuring project-specific variables
3. Setting the appropriate daitools command (local dev or remote)

## Prerequisites

- Project must have a Makefile
- Project must be a GitLab project (has gitlab group/project structure)
- User has GITLAB_API_TOKEN environment variable set

## Unified Approach

**All projects** (daitools) comes with a  Makefile include in `./Makefile.gitlab.mk` in the daitools repo.
- Configurable via `DAITOOLS_CMD` variable
- Same targets, different execution context

## Steps

### 1. Add to Project Makefile

At the top of the project's Makefile, after any existing variables, add:

**For daitools project** (local development):
```makefile
# GitLab Integration Variables
GITLAB_GROUP = data/tools
GITLAB_PROJECT_NAME = daitools
GITLAB_PROJECT = $(GITLAB_GROUP)/$(GITLAB_PROJECT_NAME)

# Use local development command
DAITOOLS_CMD = uv run daitools

# Include GitLab targets
-include ~/.config/makefiles/Makefile.gitlab.mk
```

**For other projects** (use released version):
```makefile
# GitLab Integration Variables
GITLAB_GROUP = data/tools
GITLAB_PROJECT_NAME = myproject
GITLAB_PROJECT = $(GITLAB_GROUP)/$(GITLAB_PROJECT_NAME)

# DAITOOLS_CMD defaults to: uvx --from git+...@main daitools
# (no need to set it explicitly)

# Include GitLab targets
-include ~/.config/makefiles/Makefile.gitlab.mk
```

Replace `myproject` with your actual project name (without group).

### 2. Verify Setup

Test the integration:

```bash
make list-issues
make help | grep -i gitlab
```

## Expected Results

After completion:
- Project Makefile includes GitLab targets via include statement
- All GitLab commands work: `make list-issues`, `make create-mr ISSUE=123`, etc.
- Minimal Makefile footprint (4-7 lines added)
- For daitools: Uses local development code
- For others: Uses released version from main

## Key Configuration

The shared include uses this logic:
```makefile
# Default: Use uvx to run from main branch
DAITOOLS_CMD ?= uvx --from git+...@main daitools

# Override in your Makefile for local dev:
DAITOOLS_CMD = uv run daitools
```

## Benefits

1. **Single Source**: One shared include for all projects
2. **Configurable**: Set command appropriate for your context
3. **Maintainable**: Update one file, all projects benefit
4. **Consistent**: Same targets everywhere
5. **Clean**: Minimal lines added to each project

## Notes

- The `-include` with dash means it won't fail if file doesn't exist yet
- Variables are set with `?=` so they can be overridden
- All targets validate required parameters with helpful error messages
- daitools sets `DAITOOLS_CMD = uv run daitools` (local)
- Other projects use default (uvx from main)
