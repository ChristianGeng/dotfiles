#!/bin/bash

# stow-deploy.sh - Deploy dotfiles using GNU Stow
# Usage: ./stow-deploy.sh [OPTIONS] [PACKAGES...]
#
# Options:
#   --force       Force overwrite of existing files
#   --absolute    Use absolute symlinks instead of relative ones
#   --target=DIR  Target directory (default: $HOME)
#   --generate-configs  Generate ~/.local_configs from pass store
#   --help        Display this help message
#
# If no packages are specified, all default packages will be deployed.
# Default packages: bash git node doom tmux
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

# Install kitty's terminfo (xterm-kitty) into ~/.terminfo if it is missing.
# Lets a plain `ssh` into this host work from kitty without "Terminal type
# xterm-kitty is not defined". Idempotent and best-effort: skips when already
# present and never fails the deploy if there is no network or no tic.
install_kitty_terminfo() {
  if infocmp xterm-kitty &> /dev/null; then
    print_msg "kitty terminfo (xterm-kitty) already present, skipping."
    return 0
  fi
  if ! command -v tic &> /dev/null; then
    print_warn "tic not found (install ncurses-bin); skipping kitty terminfo."
    return 0
  fi

  local url="https://raw.githubusercontent.com/kovidgoyal/kitty/master/terminfo/kitty.terminfo"
  local tmp
  tmp="$(mktemp)"
  print_msg "Installing kitty terminfo into ~/.terminfo ..."
  if command -v curl &> /dev/null; then
    curl -fsSL "$url" -o "$tmp" || { print_warn "Could not download kitty terminfo; skipping."; rm -f "$tmp"; return 0; }
  elif command -v wget &> /dev/null; then
    wget -qO "$tmp" "$url" || { print_warn "Could not download kitty terminfo; skipping."; rm -f "$tmp"; return 0; }
  else
    print_warn "Neither curl nor wget found; skipping kitty terminfo."
    rm -f "$tmp"
    return 0
  fi

  if tic -x -o "$HOME/.terminfo" "$tmp" &> /dev/null && infocmp xterm-kitty &> /dev/null; then
    print_msg "kitty terminfo installed."
  else
    print_warn "Failed to compile kitty terminfo; skipping."
  fi
  rm -f "$tmp"
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
  echo "  --generate-configs  Generate ~/.local_configs from pass store"
  echo "  --help          Display this help message"
  echo
  echo "Available packages:"
  echo "  bash            Bash shell configuration files"
  echo "  git             Git version control configuration"
  echo "  node            Node.js development environment settings"
  echo "  doom            Doom Emacs configuration (based on Derek Taylor's setup)"
  echo "  emacs           Emacs profile configuration for Chemacs"
  echo "  tmux            Tmux configuration (includes OSC 52 clipboard passthrough)"
  echo
  echo "If no packages are specified, the default packages will be deployed."
  echo "Default packages: bash git node doom tmux"
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
GENERATE_CONFIGS=false
TARGET_DIR="$HOME"
PACKAGES=("bash" "git" "node" "doom" "tmux")

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
    --generate-configs)
      GENERATE_CONFIGS=true
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
  
  # Handle special case for doom package (ensuring .config directory exists)
  if [ "$package" = "doom" ]; then
    echo "Preparing for doom package..."
    # Create the .config directory if it doesn't exist
    mkdir -p "$TARGET_DIR/.config"
  fi

  # Stow operation for all packages
  if [ "$FORCE" = true ]; then
    echo "Force flag enabled, removing existing symlinks first..."
    stow --no-folding -v $STOW_OPTS -D "$package" 2>/dev/null || true
  fi
  
  # Stow the package
  echo "Stowing $package to $TARGET_DIR..."
  stow --no-folding -v $STOW_OPTS "$package"
done

# Ensure kitty's terminfo is available so plain `ssh` from kitty works here
install_kitty_terminfo

# Generate local configs if requested
if $GENERATE_CONFIGS; then
  echo "[DEPLOY] Generating local configurations..."
  
  # Check if generate script exists
  GEN_SCRIPT="$SCRIPT_DIR/scripts/generate-local-configs"
  if [ -f "$GEN_SCRIPT" ]; then
    $GEN_SCRIPT
  else
    print_warn "Generate script not found: $GEN_SCRIPT"
    print_warn "Skipping local config generation"
  fi
fi

print_msg "Deployment complete!"
