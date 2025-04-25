#!/bin/bash

# This script copies user data from a specified "Users" directory to a destination path
# using rsync, while excluding unnecessary cache and temporary files for all users.
# 
# Excluded directories include:
# - Google Chrome cache, code cache, and GPU cache
# - Firefox cache for all profiles
# - Temporary files (AppData/Local/Temp)
# - Internet Explorer/Edge cache and temporary internet files
#
# Usage:
#   ./backup_users.sh <path_to_users_directory> <destination_path>
#
# Example:
#   ./backup_users.sh /cygdrive/c/Users /backup/users
#
# Requirements:
# - rsync must be installed and accessible in the system's PATH.
# - The source directory must exist and be accessible.

# Check if the user provided the path to the Users directory
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <path_to_users_directory> <destination_path>"
    exit 1
fi

# Input arguments
USERS_DIR="$1"
DESTINATION="$2"

# Validate that the source directory exists
if [ ! -d "$USERS_DIR" ]; then
    echo "Error: The directory '$USERS_DIR' does not exist."
    exit 1
fi

# Rsync command with excludes for all users
rsync -av \
    --exclude="*/AppData/Local/Google/Chrome/User Data/Default/Cache/" \
    --exclude="*/AppData/Local/Google/Chrome/User Data/Default/Code Cache/" \
    --exclude="*/AppData/Local/Google/Chrome/User Data/Default/GPUCache/" \
    --exclude="*/AppData/Local/Mozilla/Firefox/Profiles/*/cache2/" \
    --exclude="*/AppData/Local/Temp/" \
    --exclude="*/AppData/Local/Packages/" \
    --exclude="*/AppData/Local/Comms/UnistoreDB/" \
    --exclude="*/AppData/Local/Skype/" \
    --exclude="*/AppData/Local/Microsoft/Edge/" \
    --exclude="*/AppData/Local/Coms/" \
    --exclude="*/AppData/Local/Spotify/" \
    --exclude="*/AppData/Local/Skype/" \
    --exclude="*/AppData/Local/SquirrelTemp/" \
    --exclude="*/AppData/Local/VirtualStore/" \
    --exclude="*/AppData/Local/Windows Live/" \
    --exclude="*/AppData/Roaming/Microsoft/" \
    --exclude="*/AppData/Roaming/Mozilla/Firefox/Profiles/" \
    --exclude="*/AppData/Roaming/Skype/" \
    --exclude="*/AppData/Roaming/Spotify/" \
    --exclude="*/AppData/Roaming/Zoom/" \
    --exclude="*/AppData/Local/Microsoft/Windows/INetCache/" \
    --exclude="*/AppData/Local/Microsoft/Windows/Temporary Internet Files/" \
    --exclude="*/Desktop/Gammal Firefox-data/" \
    --exclude="*.exe" \
    "$USERS_DIR/" "$DESTINATION/"

# Check if rsync succeeded
if [ $? -eq 0 ]; then
    echo "Data successfully copied from '$USERS_DIR' to '$DESTINATION'."
else
    echo "Error: rsync encountered an issue."
    exit 1
fi
