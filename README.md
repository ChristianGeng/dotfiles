# Dotfiles

This repository contains configuration files (dotfiles) for various tools and applications. The files are organized to be easily deployed to a new machine.

## Structure

- `bash/`: Contains bash configuration files (.bashrc, .bash_aliases, etc.)
- `.dotfiles/`: Contains other configuration files (.gitconfig, .nvmrc, etc.)
- `deploy.sh`: Script to deploy the dotfiles to a new machine

## Deployment

To deploy these dotfiles to a new machine:

1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/dotfiles.git
   cd dotfiles
   ```

2. Make the deployment script executable:
   ```bash
   chmod +x deploy.sh
   ```

3. Run the deployment script:
   ```bash
   ./deploy.sh
   ```

The script will:
- Create a `.dotfiles` directory in your home directory
- Copy all configuration files to this directory
- Create symlinks from your home directory to the files in `.dotfiles`
- Back up any existing files before replacing them

## Manual Deployment

If you prefer to manually deploy the files:

1. Create a `.dotfiles` directory in your home directory:
   ```bash
   mkdir -p ~/.dotfiles
   ```

2. Copy the configuration files:
   ```bash
   cp -r bash/ ~/.dotfiles/
   cp -r .dotfiles/ ~/.dotfiles/
   ```

3. Create symlinks:
   ```bash
   ln -s ~/.dotfiles/bash/.bashrc ~/.bashrc
   ln -s ~/.dotfiles/bash/.bash_aliases ~/.bash_aliases
   ln -s ~/.dotfiles/bash/.bash_path ~/.bash_path
   ln -s ~/.dotfiles/bash/.shell_scripts ~/.shell_scripts
   ln -s ~/.dotfiles/.gitconfig ~/.gitconfig
   ln -s ~/.dotfiles/.nvmrc ~/.nvmrc
   ```

## Customization

Feel free to modify these dotfiles to suit your preferences. After making changes, you can re-run the deployment script to update the symlinks.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
