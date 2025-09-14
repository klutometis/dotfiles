# Docker Manual Installation Documentation

**Date:** September 13, 2025  
**Purpose:** Manual Docker installation on Ubuntu work laptop without repository access

## Background

Due to workplace restrictions preventing the addition of external repositories, Docker needs to be installed manually by downloading the `.deb` packages directly from Docker's download server.

## Package Source

Docker packages for Ubuntu are available at:
- Base URL: `https://download.docker.com/linux/ubuntu/dists/`
- For Ubuntu 25.04 (Plucky): `https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/`

## Required Packages

According to [Docker's official documentation](https://docs.docker.com/engine/install/ubuntu/), the following packages are required for a complete Docker installation:

1. **containerd.io** - Container runtime
2. **docker-ce** - Docker Community Edition engine
3. **docker-ce-cli** - Docker command-line interface
4. **docker-buildx-plugin** - BuildKit-based build plugin
5. **docker-compose-plugin** - Docker Compose v2 plugin

## Latest Package Versions (as of September 2025)

| Package | Version | Filename |
|---------|---------|----------|
| containerd.io | 1.7.27-1 | `containerd.io_1.7.27-1_amd64.deb` |
| docker-ce | 28.4.0 | `docker-ce_28.4.0-1~ubuntu.25.04~plucky_amd64.deb` |
| docker-ce-cli | 28.4.0 | `docker-ce-cli_28.4.0-1~ubuntu.25.04~plucky_amd64.deb` |
| docker-buildx-plugin | 0.27.0 | `docker-buildx-plugin_0.27.0-1~ubuntu.25.04~plucky_amd64.deb` |
| docker-compose-plugin | 2.39.2 | `docker-compose-plugin_2.39.2-1~ubuntu.25.04~plucky_amd64.deb` |

## Installation Methods Explored

### Method 1: Recursive wget (Not Recommended)
```bash
wget -r -np -nH --cut-dirs=7 -A "*.deb" https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/
```
**Issue:** Downloads ALL versions of all packages, which is wasteful and unnecessary.

### Method 2: Targeted wget (Recommended)
Download only the specific latest versions needed:
```bash
wget -P docker-debs/ \
  https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/containerd.io_1.7.27-1_amd64.deb \
  https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/docker-ce_28.4.0-1~ubuntu.25.04~plucky_amd64.deb \
  https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/docker-ce-cli_28.4.0-1~ubuntu.25.04~plucky_amd64.deb \
  https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/docker-buildx-plugin_0.27.0-1~ubuntu.25.04~plucky_amd64.deb \
  https://download.docker.com/linux/ubuntu/dists/plucky/pool/stable/amd64/docker-compose-plugin_2.39.2-1~ubuntu.25.04~plucky_amd64.deb
```

## Installation Process

1. **Download packages** using wget
2. **Install packages** using dpkg in the correct order
3. **Resolve dependencies** if needed using `apt-get install -f`
4. **Start Docker service** using systemctl
5. **Configure user permissions** by adding user to docker group

## Complete Installation Script

A script has been created at `~/bin/install-docker.sh` that automates the entire process:
- Downloads all required packages
- Installs them in the correct order
- Configures the Docker service
- Sets up user permissions

## Post-Installation Steps

1. **Verify installation:**
   ```bash
   docker --version
   docker compose version
   ```

2. **Test Docker:**
   ```bash
   docker run hello-world
   ```

3. **If permission denied:**
   - Log out and log back in for group changes to take effect
   - Or run: `newgrp docker`

## Troubleshooting

### Dependency Issues
If you encounter dependency errors during installation:
```bash
sudo apt-get install -f
```

### Service Not Starting
Check Docker service status:
```bash
sudo systemctl status docker
```

Check logs:
```bash
sudo journalctl -u docker
```

### Different Ubuntu Version
If you're not on Ubuntu 25.04 (Plucky), navigate to the appropriate distribution:
- Ubuntu 24.04 (Noble): `/noble/`
- Ubuntu 22.04 (Jammy): `/jammy/`
- Ubuntu 20.04 (Focal): `/focal/`

## Optional Packages

- **docker-ce-rootless-extras**: For running Docker in rootless mode
- **docker-model-plugin**: For AI/ML model management (experimental)

## References

- [Official Docker Installation Guide for Ubuntu](https://docs.docker.com/engine/install/ubuntu/)
- [Docker Download Index](https://download.docker.com/linux/ubuntu/dists/)
- [Docker Post-Installation Steps](https://docs.docker.com/engine/install/linux-postinstall/)