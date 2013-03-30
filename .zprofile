if [ -z "$SSH_CLIENT" ]; then
    exec screen -RD
fi
