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

### Restore Backup
To restore your original configuration:
```bash
$ cp -r ~/.dotfiles.backup/* ~/
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.
