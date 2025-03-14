# dotfiles

Configuration files for various tools and applications, managed using GNU Stow.

## Installation

First, install GNU Stow:

```bash
# Debian/Ubuntu
$ sudo apt-get install stow

# Fedora
$ sudo dnf install stow

# macOS with Homebrew
$ brew install stow
```

Clone the repository:

```bash
$ git clone git@github.com:yourusername/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
```

If you store the repository outside your home directory, the included `.stowrc` file ensures symlinks are created in your home directory.

## Quick Deploy

The recommended way to deploy your dotfiles is using the included deployment script:

```bash
$ ./stow-deploy.sh
```

### Script Options

- `--force, -f`: Override existing files (use with caution)
- `--help, -h`: Show help message
- Example: `./stow-deploy.sh --force bash git` to forcefully deploy bash and git configs

## Manual Usage

You can also use stow commands directly:

```bash
# Deploy a single package
$ stow bash

# Deploy multiple packages
$ stow bash git node

# Remove symlinks (unstow)
$ stow -D bash
```

## Available Packages

### bash/
- `.bashrc`: Main bash configuration
- `.bash_aliases`: Custom command aliases
- `.bash_path`: PATH environment variable configuration
- `.bash_variables`: Environment variable settings (e.g., SLURM configuration)
- `.shell_scripts`: Collection of useful shell scripts

### git/
- `.gitconfig`: Global git configuration and aliases

### node/
- `.nvmrc`: Node.js version configuration for nvm

### doom/
- Doom Emacs configuration (based on [Derek Taylor's configuration](https://gitlab.com/dwt1/dotfiles/-/tree/master/.config/doom))
- Configuration symlinked to `~/.config/doom/` 
- Main files:
  - `config.el`: Main configuration file
  - `init.el`: Initialization and module selection
  - `packages.el`: Custom package configuration
  - Additional theme and utility files
- Deploy with: `./stow-deploy.sh doom`

### emacs/
- `.emacs-profiles.el`: Defines profiles for:
  - Doom Emacs (`doom` profile)
  - Default configuration (`default`)
  - Spacemacs (`spacemacs`)
  - Deploy with: `./stow-deploy.sh emacs`

## Specific Package Deployment

### Doom Emacs Configuration
To deploy only the Doom Emacs configuration:
```bash
# Standard deployment
./stow-deploy.sh doom

# Force deployment (overwrite existing files)
./stow-deploy.sh --force doom
```

This will deploy the Doom Emacs configuration to `~/.config/doom/`.

### Bash Configuration
```bash
./stow-deploy.sh bash
```

### Git Configuration
```bash
./stow-deploy.sh git
```

### Node Configuration
```bash
./stow-deploy.sh node
```

### Emacs Profiles
```bash
./stow-deploy.sh emacs
```

## Emacs Configuration Profiles

Three predefined profiles are available:
- `doom`: Doom Emacs configuration (~/doom-emacs)
- `default`: Primary custom configuration (~/emacs-conf)
- `spacemacs`: Spacemacs configuration (~/spacemacs)

Usage after deployment:
```bash
emacs-doom      # Launch Doom configuration
emacs-default   # Launch default config
emacs-spacemacs # Launch Spacemacs
```

Deploy with: `./stow-deploy.sh emacs`

## Deployment

### Recommended Method
Use the `stow-deploy.sh` script with options:
```bash
./stow-deploy.sh [--force] [--help]
```

## Troubleshooting

### Existing Files
If you have existing configuration files, the deployment might fail with "existing target is not owned by stow" errors. To resolve:

1. Backup your existing files:
```bash
$ mkdir -p ~/.dotfiles.backup
$ cp -r ~/.bash* ~/.gitconfig ~/.nvmrc ~/.shell_scripts ~/.config/doom ~/.dotfiles.backup/
```

2. Remove existing files:
```bash
$ rm -f ~/.bash* ~/.gitconfig ~/.nvmrc ~/.shell_scripts
$ rm -rf ~/.config/doom  # Be careful with this command
```

3. Run the deployment script:
```bash
$ ./stow-deploy.sh
```

### Conflict Resolution
Use `--force` flag to override conflicting files:
```bash
./stow-deploy.sh --force emacs
```

### Doom Emacs Configuration Issues

If you encounter issues with the Doom Emacs configuration:

1. Ensure your `~/.config/doom` directory is properly linked:
```bash
ls -la ~/.config/doom  # Should show a symlink to your dotfiles repository
```

2. If you have an existing Doom Emacs configuration that you want to preserve:
```bash
# Backup existing configuration
mkdir -p ~/.config-backup
cp -r ~/.config/doom ~/.config-backup/

# Remove existing configuration
rm -rf ~/.config/doom

# Deploy new configuration
./stow-deploy.sh doom
```

3. After deploying, you may need to rebuild Doom Emacs:
```bash
~/.emacs.d/bin/doom sync
```

### Manual Conflict Resolution

If automated force mode fails:
```bash
# Remove conflicting file
rm ~/.emacs-profiles.el

# Retry deployment
./stow-deploy.sh emacs
```

### Common Issues

#### Existing File Conflicts
```bash
ERROR: File conflict detected for ~/.bashrc
  Existing file: /home/user/.bashrc
  New file: dotfiles/bash/.bashrc
```
Solution: Use `--force` to overwrite (backup created automatically)

#### Broken Symlinks After Package Deletion
```bash
find ~/ -type l -xtype l -delete  # Remove broken symlinks
```

#### Profile-Specific Configuration Issues
1. Verify profile exists in ~/.emacs-profiles.el
2. Check corresponding directory exists (e.g. ~/doom-emacs)
3. Test with: `emacs --with-profile PROFILE --eval '(kill-emacs)'`

### Backup/Restore Procedure

#### Create Backup
```bash
./stow-deploy.sh backup-$(date +%s)  # Creates timestamped backup
```

#### Restore Backup
```bash
./stow-deploy.sh --restore backup-1678901234
```

### Backup/Restore

1. Create backup:
```bash
./stow-deploy.sh backup
```
2. Restore from backup:
```bash
./stow-deploy.sh restore
```

### Restore Backup
To restore your original configuration:
```bash
$ cp -r ~/.dotfiles.backup/* ~/
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
