#!/bin/bash

# Response to: Write a bash script that receives as an input host:/dir
# and does an ssh to that host and dir, does a git status, creates a
# list of all modified but not commit files, and then rsync's these
# locally. Conceptually it is like stashing the remote site, "copying"
# the stash, and applying it locally.


# Ensure we have one argument
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 host:/dir"
    exit 1
fi

# Parse the input
REMOTE="$1"
REMOTE_HOST=$(echo "$REMOTE" | cut -d: -f1)
REMOTE_DIR=$(echo "$REMOTE" | cut -d: -f2)

# Check that both components are non-empty
if [ -z "$REMOTE_HOST" ] || [ -z "$REMOTE_DIR" ]; then
    echo "Invalid input format. Expected format: host:/dir"
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

# Create the rsync command to copy the modified files locally
echo "Syncing modified files locally..."
while read -r FILE; do
    # Ensure the file is only rsynced if it exists on the remote host.
    rsync -avz --relative "$REMOTE_HOST:$REMOTE_DIR/./$FILE" .
done < "$TEMP_FILE"

# Clean up
rm "$TEMP_FILE"

echo "Done! All modified files have been copied locally."
