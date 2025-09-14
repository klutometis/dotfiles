#!/bin/bash

#############################################################################
# Docker Manual Installation Script for Ubuntu
# Created: September 13, 2025
# Purpose: Install Docker CE without adding repositories
# Target: Ubuntu 25.04 (Plucky) - amd64 architecture
#############################################################################

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DOCKER_VERSION="28.4.0"
CONTAINERD_VERSION="1.7.27-1"
BUILDX_VERSION="0.27.0"
COMPOSE_VERSION="2.39.2"
UBUNTU_CODENAME="plucky"
UBUNTU_VERSION="25.04"
ARCH="amd64"

# Base URL for Docker packages
BASE_URL="https://download.docker.com/linux/ubuntu/dists/${UBUNTU_CODENAME}/pool/stable/${ARCH}"

# Directory to store downloaded packages
DOWNLOAD_DIR="${HOME}/docker-debs"

# Function to print colored messages
print_message() {
    echo -e "${2}${1}${NC}"
}

# Function to check if running as root
check_root() {
    if [[ $EUID -eq 0 ]]; then
        print_message "This script should not be run as root. It will use sudo when needed." "$RED"
        exit 1
    fi
}

# Function to detect Ubuntu version
detect_ubuntu() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        print_message "Detected: $NAME $VERSION" "$GREEN"
        
        if [[ "$VERSION_ID" != "25.04" ]]; then
            print_message "Warning: This script is configured for Ubuntu 25.04 (Plucky)" "$YELLOW"
            print_message "You are running Ubuntu $VERSION_ID" "$YELLOW"
            read -p "Do you want to continue anyway? (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                exit 1
            fi
        fi
    else
        print_message "Cannot detect Ubuntu version. Proceeding with caution..." "$YELLOW"
    fi
}

# Function to check prerequisites
check_prerequisites() {
    print_message "Checking prerequisites..." "$GREEN"
    
    # Check for wget
    if ! command -v wget &> /dev/null; then
        print_message "wget is not installed. Installing..." "$YELLOW"
        sudo apt-get update && sudo apt-get install -y wget
    fi
    
    # Check for systemctl
    if ! command -v systemctl &> /dev/null; then
        print_message "systemd is not available. Docker service management may not work." "$YELLOW"
    fi
}

# Function to create download directory
setup_directory() {
    print_message "Setting up download directory: $DOWNLOAD_DIR" "$GREEN"
    mkdir -p "$DOWNLOAD_DIR"
    cd "$DOWNLOAD_DIR"
}

# Function to download Docker packages
download_packages() {
    print_message "Downloading Docker packages..." "$GREEN"
    
    local packages=(
        "containerd.io_${CONTAINERD_VERSION}_${ARCH}.deb"
        "docker-ce_${DOCKER_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb"
        "docker-ce-cli_${DOCKER_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb"
        "docker-buildx-plugin_${BUILDX_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb"
        "docker-compose-plugin_${COMPOSE_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb"
    )
    
    for package in "${packages[@]}"; do
        if [ -f "$package" ]; then
            print_message "Package already exists: $package" "$YELLOW"
        else
            print_message "Downloading: $package" "$GREEN"
            wget -q --show-progress "${BASE_URL}/${package}"
        fi
    done
    
    print_message "All packages downloaded successfully!" "$GREEN"
}

# Function to install Docker packages
install_packages() {
    print_message "Installing Docker packages..." "$GREEN"
    
    # Install all packages at once as recommended by Docker docs
    sudo dpkg -i \
        ./containerd.io_${CONTAINERD_VERSION}_${ARCH}.deb \
        ./docker-ce_${DOCKER_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb \
        ./docker-ce-cli_${DOCKER_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb \
        ./docker-buildx-plugin_${BUILDX_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb \
        ./docker-compose-plugin_${COMPOSE_VERSION}-1~ubuntu.${UBUNTU_VERSION}~${UBUNTU_CODENAME}_${ARCH}.deb
    
    # Fix any dependency issues
    if [ $? -ne 0 ]; then
        print_message "Fixing dependencies..." "$YELLOW"
        sudo apt-get install -f -y
    fi
    
    print_message "Docker packages installed successfully!" "$GREEN"
}

# Function to configure Docker service
configure_docker() {
    print_message "Configuring Docker service..." "$GREEN"
    
    # Start Docker service
    sudo systemctl start docker || print_message "Could not start Docker service" "$YELLOW"
    
    # Enable Docker to start on boot
    sudo systemctl enable docker || print_message "Could not enable Docker service" "$YELLOW"
    
    # Add current user to docker group
    print_message "Adding $USER to docker group..." "$GREEN"
    sudo usermod -aG docker "$USER"
    
    print_message "Docker service configured!" "$GREEN"
}

# Function to verify installation
verify_installation() {
    print_message "\nVerifying Docker installation..." "$GREEN"
    
    # Check Docker version
    if command -v docker &> /dev/null; then
        docker --version
    else
        print_message "Docker command not found!" "$RED"
        return 1
    fi
    
    # Check Docker Compose version
    if docker compose version &> /dev/null; then
        docker compose version
    else
        print_message "Docker Compose plugin not found!" "$YELLOW"
    fi
    
    # Check Docker service status
    if systemctl is-active --quiet docker; then
        print_message "Docker service is running" "$GREEN"
    else
        print_message "Docker service is not running" "$YELLOW"
    fi
}

# Function to show post-installation instructions
post_install_instructions() {
    print_message "\n========================================" "$GREEN"
    print_message "Docker Installation Complete!" "$GREEN"
    print_message "========================================" "$GREEN"
    
    print_message "\nPost-installation steps:" "$YELLOW"
    echo "1. Log out and log back in for group changes to take effect"
    echo "   OR run: newgrp docker"
    echo ""
    echo "2. Test your installation:"
    echo "   docker run hello-world"
    echo ""
    echo "3. Downloaded packages are saved in: $DOWNLOAD_DIR"
    echo "   You can safely delete this directory if installation was successful"
    echo ""
    
    print_message "Documentation saved at: ~/var/build/docker.md" "$GREEN"
}

# Function to handle cleanup on error
cleanup_on_error() {
    print_message "\nInstallation failed. Check the error messages above." "$RED"
    print_message "Downloaded packages are preserved in: $DOWNLOAD_DIR" "$YELLOW"
    exit 1
}

# Main installation flow
main() {
    print_message "========================================" "$GREEN"
    print_message "Docker Manual Installation Script" "$GREEN"
    print_message "========================================" "$GREEN"
    echo ""
    
    # Set error trap
    trap cleanup_on_error ERR
    
    # Run installation steps
    check_root
    detect_ubuntu
    check_prerequisites
    setup_directory
    download_packages
    install_packages
    configure_docker
    verify_installation
    post_install_instructions
}

# Run main function
main "$@"