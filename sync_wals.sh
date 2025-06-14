#!/bin/bash

# Configuration
LOCAL_WAL_ARCHIVE_DIR="/home/jmis/wal_archive" # This should match the volume mount on your host
REMOTE_USER="jgsadmin"
REMOTE_HOST="20.242.125.65"
REMOTE_WAL_ARCHIVE_PATH="/home/jmis/wal_archive_location"

echo "Starting WAL archive synchronization to remote server: $REMOTE_HOST:$REMOTE_WAL_ARCHIVE_PATH"

# Ensure the local WAL archive directory exists (though docker should create it)
mkdir -p "$LOCAL_WAL_ARCHIVE_DIR"

# Use rsync to synchronize WAL files
# -a: archive mode (preserves permissions, timestamps, recursive)
# -z: compress file data during transfer
# -h: human-readable output
# --remove-source-files: (Optional) Remove WAL files locally after successful transfer
#                        Use with caution and only if you are absolutely sure of remote success!
# --progress: show progress during transfer (useful for logs)
rsync -azh --remove-source-files "$LOCAL_WAL_ARCHIVE_DIR"/ "$REMOTE_USER@$REMOTE_HOST:$REMOTE_WAL_ARCHIVE_PATH"

if [ $? -eq 0 ]; then
    echo "WAL archives synchronized successfully to remote server."
else
    echo "ERROR: Failed to synchronize WAL archives to remote server."
    exit 1
fi

echo "WAL archive synchronization process finished."