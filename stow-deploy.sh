#!/bin/bash

# stow-deploy.sh - Deploy dotfiles using GNU Stow
# This script uses GNU Stow to create symlinks from your home directory to your dotfiles

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

# Parse command line arguments
FORCE=false
ABSOLUTE=false
PACKAGES=("bash" "git" "node" "doom")

while [[ $# -gt 0 ]]; do
  case $1 in
    --force|-f)
      FORCE=true
      shift
      ;;
    --absolute|-a)
      ABSOLUTE=true
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [options]"
      echo "Options:"
      echo "  --force, -f     Force creation of symlinks, overriding existing files"
      echo "  --absolute, -a  Create absolute symlinks instead of relative ones"
      echo "  --help, -h      Show this help message"
      exit 0
      ;;
    --force)
      FORCE_FLAG="--override='.*\.emacs-profiles\.el'"
      shift
      ;;
    *)
      # Add any specified packages to the list
      PACKAGES+=("$1")
      shift
      ;;
  esac
done

# Stow options
STOW_OPTS="-v -t $HOME"

if $FORCE; then
  print_warn "Force mode enabled. Existing files will be overridden."
  STOW_OPTS="$STOW_OPTS --override=*"
fi

if $ABSOLUTE; then
  print_msg "Creating absolute symlinks."
  STOW_OPTS="$STOW_OPTS --absolute"
fi

# Stow each package
for pkg in "${PACKAGES[@]}"; do
  if [ -d "$pkg" ]; then
    print_msg "Stowing $pkg..."
    stow $STOW_OPTS "$pkg"
  else
    print_warn "Package directory $pkg not found, skipping."
  fi
done

print_msg "Deployment complete!"
