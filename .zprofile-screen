if [ -z "$SSH_CLIENT" ]; then
    screen -qr -ls

    # If there is one screen to unambiguously attach to or no screens,
    # attach!
    if [ $? -eq 11 ] || [ $? -eq 8 ]; then
        exec screen -R
    else
        # Otherwise, create a new screen.
        exec screen
    fi
fi
