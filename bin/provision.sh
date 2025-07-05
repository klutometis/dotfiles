#!/bin/bash
# Deploy system configuration files

set -e

echo "Deploying system configuration files..."

# X11 configs
if [ -d ~/etc/X11/xorg.conf.d ]; then
    echo "Copying X11 configuration files..."
    sudo cp ~/etc/X11/xorg.conf.d/* /etc/X11/xorg.conf.d/
    sudo chown root:root /etc/X11/xorg.conf.d/*
    sudo chmod 644 /etc/X11/xorg.conf.d/*
    echo "X11 configs deployed successfully"
else
    echo "No X11 configs found in ~/etc/X11/xorg.conf.d/"
fi

# Other system configs as needed
# sudo cp ~/etc/dictd.conf /etc/dictd.conf

echo "System configuration deployment complete!"
echo "You may need to restart X11 for changes to take effect."
