#!/bin/bash

# stow-deploy.sh - Deploy dotfiles using GNU Stow
# Usage: ./stow-deploy.sh [OPTIONS] [PACKAGES...]
#
# Options:
#   --force       Force overwrite of existing files
#   --absolute    Use absolute symlinks instead of relative ones
#   --target=DIR  Target directory (default: $HOME)
#   --help        Display this help message
#
# If no packages are specified, all default packages will be deployed.
# Default packages: bash git node doom
#
# Examples:
#   ./stow-deploy.sh                # Deploy all default packages
#   ./stow-deploy.sh doom           # Deploy only the doom package
#   ./stow-deploy.sh --force bash   # Force deployment of bash package
#   ./stow-deploy.sh bash git       # Deploy bash and git packages

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

# Check if stow is installed
if ! command -v stow &> /dev/null; then
  print_error "GNU Stow is not installed. Please install it first."
  print_msg "On Debian/Ubuntu: sudo apt-get install stow"
  print_msg "On Fedora: sudo dnf install stow"
  print_msg "On macOS with Homebrew: brew install stow"
  exit 1
fi

# Directory containing this script
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Display help
show_help() {
  echo "stow-deploy.sh - Deploy dotfiles using GNU Stow"
  echo "Usage: ./stow-deploy.sh [OPTIONS] [PACKAGES...]"
  echo
  echo "Options:"
  echo "  --force         Force overwrite of existing files"
  echo "  --absolute      Use absolute symlinks instead of relative ones"
  echo "  --target=DIR    Target directory (default: \$HOME)"
  echo "  --help          Display this help message"
  echo
  echo "Available packages:"
  echo "  bash            Bash shell configuration files"
  echo "  git             Git version control configuration"
  echo "  node            Node.js development environment settings"
  echo "  doom            Doom Emacs configuration (based on Derek Taylor's setup)"
  echo "  emacs           Emacs profile configuration for Chemacs"
  echo
  echo "If no packages are specified, the default packages will be deployed."
  echo "Default packages: bash git node doom"
  echo
  echo "Examples:"
  echo "  ./stow-deploy.sh                # Deploy all default packages"
  echo "  ./stow-deploy.sh doom           # Deploy only the doom package"
  echo "  ./stow-deploy.sh --force bash   # Force deployment of bash package"
  echo "  ./stow-deploy.sh bash git       # Deploy bash and git packages"
}

# Parse command line arguments
FORCE=false
ABSOLUTE=false
TARGET_DIR="$HOME"
PACKAGES=("bash" "git" "node" "doom")

while [[ $# -gt 0 ]]; do
  case $1 in
    --force)
      FORCE=true
      shift
      ;;
    --absolute)
      ABSOLUTE=true
      shift
      ;;
    --target=*)
      TARGET_DIR="${1#*=}"
      shift
      ;;
    --help)
      show_help
      exit 0
      ;;
    *)
      if [[ -d "$1" ]]; then
        PACKAGES+=("$1")
      else
        echo "Warning: Package directory '$1' not found, skipping."
      fi
      shift
      ;;
  esac
done

# Stow options
STOW_OPTS="-v -t $TARGET_DIR"

if $FORCE; then
  print_warn "Force mode enabled. Existing files will be overridden."
  STOW_OPTS="$STOW_OPTS --override=*"
fi

if $ABSOLUTE; then
  print_msg "Creating absolute symlinks."
  STOW_OPTS="$STOW_OPTS --absolute"
fi

# Loop through all packages and run stow
for package in "${PACKAGES[@]}"; do
  echo "[DEPLOY] Stowing $package..."
  
  # Handle special case for doom package (deploying to .config/doom directory)
  if [ "$package" = "doom" ]; then
    echo "Using special handling for the doom package..."
    
    # Create the .config/doom directory if it doesn't exist
    echo "Creating target directory..."
    mkdir -p "$TARGET_DIR/.config/doom"
    
    # Directory to copy from
    DOOM_SRC="$SCRIPT_DIR/doom/.config/doom"
    DOOM_DST="$TARGET_DIR/.config/doom"
    
    if [ "$FORCE" = true ]; then
      # Backup existing .config/doom directory if needed
      if [ -d "$DOOM_DST" ] && [ "$(ls -A $DOOM_DST)" ]; then
        BACKUP_DIR="$TARGET_DIR/.config/doom.backup.$(date +%Y%m%d-%H%M%S)"
        echo "Backing up existing doom configuration to $BACKUP_DIR"
        cp -r "$DOOM_DST" "$BACKUP_DIR"
      fi
    fi
    
    # Copy/sync the content
    echo "Copying doom configuration to $DOOM_DST..."
    rsync -av --exclude "*.backup*" "$DOOM_SRC/" "$DOOM_DST/"
    
    echo "Doom configuration successfully deployed to $DOOM_DST"
  else
    # Normal stow operation for other packages
    if [ "$FORCE" = true ]; then
      echo "Force flag enabled, removing existing symlinks first..."
      stow --no-folding -v $STOW_OPTS -D "$package" 2>/dev/null || true
    fi
    
    # Stow the package
    echo "Stowing $package to $TARGET_DIR..."
    stow --no-folding -v $STOW_OPTS "$package"
  fi
done

print_msg "Deployment complete!"
