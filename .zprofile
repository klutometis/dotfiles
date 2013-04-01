if [ -z "$SSH_CLIENT" ]; then
    exec screen -DRR
fi
