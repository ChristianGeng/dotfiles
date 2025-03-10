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
- `.shell_scripts`: Collection of useful shell scripts

### git/
- `.gitconfig`: Global git configuration and aliases

### node/
- `.nvmrc`: Node.js version configuration for nvm

### emacs/
- `.emacs-profiles.el`: Defines profiles for:
  - Doom Emacs (`doom` profile)
  - Default configuration (`default`)
  - Spacemacs (`spacemacs`)

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
$ cp -r ~/.bash* ~/.gitconfig ~/.nvmrc ~/.shell_scripts ~/.dotfiles.backup/
```

2. Remove existing files:
```bash
$ rm -f ~/.bash* ~/.gitconfig ~/.nvmrc ~/.shell_scripts
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
