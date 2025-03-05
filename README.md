# dotfiles

Configuration files for various tools and applications, managed using GNU Stow.

## Installation

First, install GNU Stow:

```
# Debian/Ubuntu
$ sudo apt-get install stow

# Fedora
$ sudo dnf install stow

# macOS with Homebrew
$ brew install stow
```

Clone the repository to ~/.dotfiles

```
$ git clone git@github.com:yourusername/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
```

If you store the repository at a place outside of your home directory,
the included `.stowrc` file will ensure that symlinks are created in your home directory.

## Usage

To create symlinks to the configuration files in this repository run

```
$ stow bash
```

This will create symlinks in your home directory to the files in the bash package.
You can also stow multiple packages at once:

```
$ stow bash git node
```

To remove the symlinks (unstow):

```
$ stow -D bash
```

## Available Packages

The repository is organized into "packages" that can be individually stowed:

- `bash/`: Contains bash configuration files (.bashrc, .bash_aliases, etc.)
- `git/`: Contains git configuration files (.gitconfig)
- `node/`: Contains Node.js configuration files (.nvmrc)

## License

This project is licensed under the MIT License - see the LICENSE file for details.
