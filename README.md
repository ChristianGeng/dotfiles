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

## Deploying Configuration with stow-deploy.sh

The `stow-deploy.sh` script is the recommended way to deploy your dotfiles. It provides a consistent interface for managing all configuration packages and handles special cases properly.

### Basic Usage

```bash
# Deploy all default packages (bash, git, node, doom)
./stow-deploy.sh

# Deploy a specific package
./stow-deploy.sh doom

# Deploy multiple specific packages
./stow-deploy.sh bash git
```

### Available Options

| Option | Description |
|--------|-------------|
| `--force` | Force deployment by removing existing symlinks first. Useful when files already exist or for updating configurations. |
| `--absolute` | Use absolute symlinks instead of relative ones. Useful in certain environments or when symlinks span filesystems. |
| `--target=DIR` | Set a custom target directory (default is $HOME). |
| `--help` | Display help information about stow-deploy.sh usage. |

### Examples

```bash
# Force deployment of doom configuration, overwriting existing files
./stow-deploy.sh --force doom

# Deploy bash configuration with absolute symlinks
./stow-deploy.sh --absolute bash

# Deploy to a custom target directory
./stow-deploy.sh --target=/path/to/target doom
```

### Order of Operations

The script performs these steps for each package:

1. Checks if the package directory exists
2. Handles special cases (e.g., .config directory for doom)
3. Creates necessary directories if they don't exist
4. Unstows existing symlinks if --force is specified
5. Stows the package to the target directory

## Package Details

### bash/
- **Purpose**: Provides a comprehensive bash shell configuration
- **Main Files**:
  - `.bashrc`: Main bash configuration with common settings
  - `.bash_aliases`: Defines useful command aliases to improve terminal workflow
  - `.bash_path`: Configures PATH environment variable for proper command discovery
  - `.bash_variables`: Sets environment variables (like SLURM configuration)
  - `.shell_scripts`: Collection of utility scripts for common tasks
- **Benefits**: Consistent shell experience across multiple machines

### git/
- **Purpose**: Git version control configuration
- **Main Files**:
  - `.gitconfig`: Global git settings and aliases for improved workflow
- **Benefits**: Consistent git behavior and shortcuts across systems

### node/
- **Purpose**: Node.js development environment configuration
- **Main Files**:
  - `.nvmrc`: Node.js version configuration for nvm (Node Version Manager)
- **Benefits**: Ensures consistent Node.js version across projects

### doom/
- **Purpose**: Doom Emacs configuration based on Derek Taylor's setup
- **Main Files**:
  - `config.el`: Main Doom Emacs configuration file (compiled from config.org)
  - `config.org`: Literate programming configuration with detailed documentation
  - `init.el`: Initializes Doom modules and packages
  - `packages.el`: Defines additional packages to be installed
  - `start.org`: Custom start page with helpful keybindings
  - `eshell/`: Configuration for the Emacs shell environment
  - `images/`: Graphics used in the configuration
- **Benefits**: Powerful, pre-configured Emacs environment for coding and productivity

### emacs/
- **Purpose**: Emacs profile configuration for Chemacs
- **Main Files**:
  - `.emacs-profiles.el`: Defines multiple Emacs configurations (Doom, Spacemacs, etc.)
- **Benefits**: Allows switching between different Emacs configurations

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

## Backup and Restore

### Backup Your Existing Configuration

Before deploying new dotfiles, it's recommended to back up your existing configuration files:

```bash
# Create a backup directory
mkdir -p ~/.dotfiles.backup/$(date +%Y%m%d)

# Backup bash files
cp -r ~/.bash* ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# Backup git configuration
cp -r ~/.gitconfig ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# Backup Node.js configuration
cp -r ~/.nvmrc ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# Backup Doom Emacs configuration
cp -r ~/.config/doom ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# Backup shell scripts
cp -r ~/.shell_scripts ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# Backup Emacs profiles
cp -r ~/.emacs-profiles.el ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true

# List the backed up files
echo "Backed up the following files to ~/.dotfiles.backup/$(date +%Y%m%d)/:"
ls -la ~/.dotfiles.backup/$(date +%Y%m%d)/
```

### Restore From Backup

If you need to revert to your previous configuration:

```bash
# Choose the backup directory (replace YYYYMMDD with the actual date)
BACKUP_DIR=~/.dotfiles.backup/YYYYMMDD

# List available backups if date unknown
ls -la ~/.dotfiles.backup/

# Restore bash files
cp -r $BACKUP_DIR/.bash* ~/ 2>/dev/null || true

# Restore git configuration
cp -r $BACKUP_DIR/.gitconfig ~/ 2>/dev/null || true

# Restore Node.js configuration
cp -r $BACKUP_DIR/.nvmrc ~/ 2>/dev/null || true

# Restore Doom Emacs configuration (create directory if it doesn't exist)
mkdir -p ~/.config
cp -r $BACKUP_DIR/doom ~/.config/ 2>/dev/null || true

# Restore shell scripts
cp -r $BACKUP_DIR/.shell_scripts ~/ 2>/dev/null || true

# Restore Emacs profiles
cp -r $BACKUP_DIR/.emacs-profiles.el ~/ 2>/dev/null || true
```

### Automated Backup Script

For convenience, you can use this one-liner to back up all your configuration files before deploying:

```bash
mkdir -p ~/.dotfiles.backup/$(date +%Y%m%d) && cp -r ~/.bash* ~/.gitconfig ~/.nvmrc ~/.config/doom ~/.shell_scripts ~/.emacs-profiles.el ~/.dotfiles.backup/$(date +%Y%m%d)/ 2>/dev/null || true
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
If you encounter errors when deploying the Doom Emacs configuration:

#### "Existing target is not owned by stow: .config" Error
This error occurs when GNU Stow tries to manage the `.config` directory that already exists in your home directory:

```
WARNING! stowing doom would cause conflicts:
  * existing target is not owned by stow: .config
All operations aborted.
```

**Solution:**
The `stow-deploy.sh` script has been updated to properly handle this scenario by:
1. Creating the `.config` directory if it doesn't exist
2. Using the `--force` flag to override any existing files
3. Using the correct target directory for the doom package

To resolve this issue:
```bash
# Use the force flag to handle existing files
./stow-deploy.sh --force doom
```

#### Package Directory Not Found
If you see a "Package directory not found" message:

```
Package directory doom not found, skipping.
```

Ensure that the doom directory exists in your dotfiles repository and has the correct structure:
```
dotfiles/
└── doom/
    └── .config/
        └── doom/
            ├── config.el
            ├── init.el
            └── packages.el
```

#### Doom Emacs Not Loading Configuration
If Doom Emacs isn't loading your configuration after stowing:

1. Make sure the symlinks were created properly:
```bash
ls -la ~/.config/doom/
```

2. Rebuild Doom Emacs to recognize the new configuration:
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

## License

This project is licensed under the MIT License - see the LICENSE file for details.
