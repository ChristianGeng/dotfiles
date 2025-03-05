#!/bin/bash

# deploy.sh - Deploy dotfiles to a new machine
# This script copies dotfiles to ~/.dotfiles and creates symlinks in the home directory

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print colored message
print_msg() {
  echo -e "${GREEN}[DEPLOY]${NC} $1"
}

print_warn() {
  echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
  echo -e "${RED}[ERROR]${NC} $1"
}

# Create .dotfiles directory in home if it doesn't exist
DOTFILES_DIR="$HOME/.dotfiles"
SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

print_msg "Source directory: $SOURCE_DIR"
print_msg "Target directory: $DOTFILES_DIR"

# Create .dotfiles directory if it doesn't exist
if [ ! -d "$DOTFILES_DIR" ]; then
  print_msg "Creating $DOTFILES_DIR directory"
  mkdir -p "$DOTFILES_DIR"
else
  print_warn "$DOTFILES_DIR already exists"
fi

# Copy bash files
copy_and_link() {
  local src="$1"
  local dest="$2"
  local link_target="$3"
  
  # Create parent directories if they don't exist
  mkdir -p "$(dirname "$dest")"
  
  # Copy the file
  if [ -f "$src" ]; then
    print_msg "Copying $src to $dest"
    cp "$src" "$dest"
    
    # Create symlink
    if [ -L "$link_target" ]; then
      print_warn "Symlink $link_target already exists, removing it"
      rm "$link_target"
    elif [ -f "$link_target" ]; then
      print_warn "File $link_target already exists, backing it up"
      mv "$link_target" "${link_target}.backup.$(date +%Y%m%d%H%M%S)"
    fi
    
    print_msg "Creating symlink from $link_target to $dest"
    ln -s "$dest" "$link_target"
  else
    print_error "Source file $src does not exist"
  fi
}

# Deploy bash files
deploy_bash_files() {
  print_msg "Deploying bash configuration files"
  
  # .bashrc
  copy_and_link "$SOURCE_DIR/bash/.bashrc" "$DOTFILES_DIR/bash/.bashrc" "$HOME/.bashrc"
  
  # .bash_aliases
  copy_and_link "$SOURCE_DIR/bash/.bash_aliases" "$DOTFILES_DIR/bash/.bash_aliases" "$HOME/.bash_aliases"
  
  # .bash_path
  copy_and_link "$SOURCE_DIR/bash/.bash_path" "$DOTFILES_DIR/bash/.bash_path" "$HOME/.bash_path"
  
  # .shell_scripts
  copy_and_link "$SOURCE_DIR/bash/.shell_scripts" "$DOTFILES_DIR/bash/.shell_scripts" "$HOME/.shell_scripts"
}

# Deploy git files
deploy_git_files() {
  print_msg "Deploying git configuration files"
  
  # .gitconfig
  copy_and_link "$SOURCE_DIR/.dotfiles/.gitconfig" "$DOTFILES_DIR/.gitconfig" "$HOME/.gitconfig"
  
  # .nvmrc
  copy_and_link "$SOURCE_DIR/.dotfiles/.nvmrc" "$DOTFILES_DIR/.nvmrc" "$HOME/.nvmrc"
}

# Main deployment
main() {
  print_msg "Starting deployment of dotfiles"
  
  deploy_bash_files
  deploy_git_files
  
  print_msg "Deployment complete!"
  print_msg "You may need to source your .bashrc: source ~/.bashrc"
}

# Run the main function
main
