#!/bin/bash

# Ensure we have two arguments
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 host:/dir [diff|pull|push]"
    exit 1
fi

# Parse the input
REMOTE="$1"
ACTION="$2"
REMOTE_HOST=$(echo "$REMOTE" | cut -d: -f1)
REMOTE_DIR=$(echo "$REMOTE" | cut -d: -f2)

# Check that both components are non-empty
if [ -z "$REMOTE_HOST" ] || [ -z "$REMOTE_DIR" ]; then
    echo "Invalid input format. Expected format: host:/dir"
    exit 1
fi

# Check if the action is valid
if [[ "$ACTION" != "diff" && "$ACTION" != "pull" && "$ACTION" != "push" ]]; then
    echo "Invalid action. Expected one of: diff, pull, push"
    exit 1
fi

# Define a temporary file for storing the list of modified files
TEMP_FILE=$(mktemp)

# SSH to the remote host and get the list of modified but uncommitted files
ssh "$REMOTE_HOST" "cd $REMOTE_DIR && git status --porcelain=v1 | grep '^ M' | awk '{print \$2}'" > "$TEMP_FILE"

# Check if there are any modified files
if [ ! -s "$TEMP_FILE" ]; then
    echo "No modified but uncommitted files found on the remote repository."
    rm "$TEMP_FILE"
    exit 0
fi

echo "Found modified files:"
cat "$TEMP_FILE"

# Perform the specified action
case "$ACTION" in
    diff)
        echo "Showing differences between local and remote files..."
        # Create a temporary directory for remote files
        TEMP_REMOTE_DIR=$(mktemp -d)

        while read -r FILE; do
            # Copy the remote file to the temporary directory
            rsync -avz --relative "$REMOTE_HOST:$REMOTE_DIR/./$FILE" "$TEMP_REMOTE_DIR/" > /dev/null 2>&1

            # Construct the full path to the remote file in the temporary directory
            REMOTE_FILE="$TEMP_REMOTE_DIR/$FILE"

            # Perform the diff between the local file and the remote file
            if [ -f "$FILE" ]; then
                echo "Diff for $FILE:"
                diff -u "$FILE" "$REMOTE_FILE" || true
            else
                echo "Local file $FILE does not exist. Skipping diff."
            fi
        done < "$TEMP_FILE"

        # Clean up the temporary directory
        rm -rf "$TEMP_REMOTE_DIR"
        ;;
    pull)
        echo "Pulling modified files from remote to local..."
        while read -r FILE; do
            rsync -avz --relative "$REMOTE_HOST:$REMOTE_DIR/./$FILE" .
        done < "$TEMP_FILE"
        ;;
    push)
        echo "Pushing modified files from local to remote..."
        while read -r FILE; do
            rsync -avz --relative "./$FILE" "$REMOTE_HOST:$REMOTE_DIR/"
        done < "$TEMP_FILE"
        ;;
esac

# Clean up
rm "$TEMP_FILE"

echo "Done!"
