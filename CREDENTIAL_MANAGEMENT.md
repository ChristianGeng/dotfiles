# Credential Management System

This document describes the integrated credential management system for dotfiles, which securely manages environment variables using the Unix `pass` password manager.

## Overview

The system generates `~/.local_configs` from credentials stored in your pass store, keeping secrets out of version control while providing automated deployment through Stow.

## Components

### 1. Template System

- **`bash/.local_configs.template`** - Template file with variable placeholders
- **`bash/.local_configs.spec`** - Maps pass entries to environment variables

### 2. Generation Script

- **`scripts/generate-local-configs`** - Generates configs from template and pass store

### 3. Integration Points

- **`stow-deploy.sh`** - Optional `--generate-configs` flag
- **Doom Emacs** - Interactive commands in `defuns/credentials.el`
- **Shell function** - `generate_local_configs()` in `.shell_scripts`

## Setup

### Prerequisites

```bash
# Install pass (password manager)
sudo apt-get install pass

# Initialize pass (if not already done)
pass init "your-gpg-key-id"
```

### Pass Store Organization

The pass store is organized hierarchically:

```text
Password Store
├── work/           # Work-related credentials
│   └── aud/        # Audeering work
│       └── api/    # API keys for work services
│           ├── openai/openai_api_key
│           └── anthropic/anthropic_api_key_aud
└── personal/       # Personal credentials
    └── api/        # Personal API keys
        ├── perplexity/perplexity_api_key
        └── xai/xai_api_key
```

### 1. Store Credentials in Pass

```bash
# Work credentials
pass insert work/aud/api/openai/openai_api_key
pass insert work/aud/api/anthropic/anthropic_api_key_aud

# Personal credentials
pass insert personal/api/perplexity/perplexity_api_key
pass insert personal/api/xai/xai_api_key

# List the store structure
pass list
pass list work/aud/api
```

### 2. Configure Mappings

Edit `bash/.local_configs.spec` to match your credentials:

```elisp
((openai-work        :pass "work/aud/api/openai/openai_api_key"       :env "OPENAI_API_KEY")
  (anthropic-work     :pass "work/aud/api/anthropic/anthropic_api_key_aud" :env "ANTHROPIC_API_KEY")
  (perplexity-personal :pass "personal/api/perplexity/perplexity_api_key" :env "PERPLEXITY_API_KEY")
  (xai-personal       :pass "personal/api/xai/xai_api_key"                :env "XAI_API_KEY"))
```

### Adding New Credentials

1. Store in pass with proper hierarchy:

   ```bash
   # Work service
   pass insert work/aud/api/service_name/api_key
   
   # Personal service
   pass insert personal/api/service_name/api_key
   ```

2. Add to `.local_configs.spec`:

   ```elisp
   (service-name :pass "work/aud/api/service_name/api_key" :env "SERVICE_API_KEY")
   ```

3. Regenerate: `generate_local_configs`

### 3. Generate Local Configs

```bash
# Method 1: Using the script directly
./scripts/generate-local-configs

# Method 2: Via stow deployment
./stow-deploy.sh --generate-configs

# Method 3: From shell (after sourcing .shell_scripts)
generate_local_configs

# Method 4: From Doom Emacs
M-x cg/update-local-configs
```

## Usage

### Daily Workflow

1. Update credentials in pass store as needed
2. Run `generate_local_configs` to update `~/.local_configs`
3. Source the file or restart shell: `source ~/.local_configs`

### Doom Emacs Commands

- `SPC c u` - Update local configs
- `SPC c p` - Preview what would be generated
- `SPC c e` - Edit credential specifications
- `SPC c l` - List all credentials
- `SPC c a` - Add new credential
- `SPC c k` - Update credential from kill ring
- `SPC c s` - Show specific credential

## Security

### What's Tracked in Git

- Template files (`.local_configs.template`)
- Specification files (`.local_configs.spec`)
- Generation scripts

### What's NOT Tracked

- `~/.local_configs` (contains actual secrets)
- Pass store itself (encrypted separately)

### Best Practices

1. Use GPG-protected pass store
2. Regularly backup your pass store
3. Use hierarchical paths (work/aud/api/ or personal/api/)
4. Group credentials by service and context
5. Review `.local_configs.spec` before committing

### Migration from Legacy Code Tree

If you have credentials in the old `code/` tree:

1. Move credentials to new hierarchy:
   ```bash
   # Move to work tree
   pass mv code/openai_api_key work/aud/api/openai/openai_api_key
   pass mv code/anthropic_api_key_aud work/aud/api/anthropic/anthropic_api_key_aud
   
   # Move to personal tree
   pass mv code/perplexity_api_key personal/api/perplexity/perplexity_api_key
   pass mv code/xai_api_key personal/api/xai/xai_api_key
   ```

2. Update `.local_configs.spec` to use new paths

3. Regenerate configs: `generate_local_configs`

4. Verify all credentials are accessible

## Troubleshooting

### Missing Credentials

```bash
# Check what's available
pass list
pass list work/aud/api
pass list personal/api

# Dry run to see what would be generated
generate_local_configs --dry-run
```

### Pass Not Found

```bash
# Install pass
sudo apt-get install pass

# Initialize with your GPG key
pass init $(gpg --list-secret-keys --keyid-format LONG | grep sec | awk '{print $2}' | cut -d'/' -f2)
```

### Emacs Integration Issues

Ensure `pass-simple` package is installed in Doom Emacs:

```elisp
;; In packages.el
(package! pass-simple
  :recipe (:host github :repo "ChristianGeng/pass-simple"))
```

## Advanced Usage

### Multiple Contexts

Create different spec files for different contexts:

```bash
# Work credentials
./scripts/generate-local-configs --spec bash/.local_configs.work.spec

# Personal credentials  
./scripts/generate-local-configs --spec bash/.local_configs.personal.spec
```

### Custom Templates

Modify `.local_configs.template` to include:

- Additional export statements
- Conditional logic
- Comments and documentation
- Non-API key configurations

## Integration with Other Tools

### Systemd Services

```bash
# Source configs in systemd service
EnvironmentFile=%h/.local_configs
```

### Docker Compose

```yaml
# In docker-compose.yml
env_file:
  - ~/.local_configs
```

### VS Code

```json
// In .env files
{
  "terminal.integrated.env.linux": "~/.local_configs"
}
```

## Migration from Manual Setup

If you currently have manual exports in `.bashrc` or `.local_configs`:

1. Move each secret to pass store
2. Add mapping to `.local_configs.spec`
3. Remove manual exports
4. Generate new `.local_configs`
5. Commit the changes

## Related Documentation

- [pass password manager](https://www.passwordstore.org/)
- [pass-simple Emacs package](https://github.com/ChristianGeng/pass-simple)
- [GNU Stow](https://www.gnu.org/software/stow/)
- [Doom Emacs](https://github.com/doomemacs/doomemacs)
