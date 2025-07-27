#!/bin/bash

# --- Configuration ---
# PostgreSQL Database Details (running on THIS LOCAL machine)
DB_NAME="jgp_app_db"               # Name of the PostgreSQL database to backup
PG_USER="root"               # PostgreSQL username for the database
PG_PORT="5433"               # PostgreSQL port for the database
#PG_PASSWORD="your_postgres_password"      # Uncomment and set if not using .pgpass or trust authentication

# Local Backup Directory Details (on this machine)
LOCAL_BACKUP_DIR="/home/simiyu/kazi/omboi/jmis/jmis_backups" # Local directory to store backups

# Remote Backup Server Details
REMOTE_USER="jgsadmin"         # SSH username on the remote backup server
REMOTE_HOST="20.242.125.65" # IP address or hostname of the remote backup server
REMOTE_BACKUP_DIR="/home/jmis/jmis_db_backups" # Directory on the remote backup server

RETENTION_COUNT=3                          # Number of latest backups to keep (e.g., last 3)

# --- Ensure local backup directory exists ---
mkdir -p "$LOCAL_BACKUP_DIR" || { echo "Error: Failed to create local backup directory: $LOCAL_BACKUP_DIR"; exit 1; }

# --- Generate timestamp for the backup file ---
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${DB_NAME}_${TIMESTAMP}.sqlc.gz" # .sqlc for custom format, .gz for gzip
LOCAL_BACKUP_PATH="${LOCAL_BACKUP_DIR}/${BACKUP_FILE}"

echo "--- Starting PostgreSQL Backup Process ---"
echo "Database: $DB_NAME (Local)"
echo "Timestamp: $TIMESTAMP"
echo "Local Backup Path: $LOCAL_BACKUP_PATH"
echo "Remote Backup Destination: ${REMOTE_HOST}:${REMOTE_BACKUP_DIR}"

# --- 1. Create the compressed backup file locally ---
# If PG_PASSWORD is set, use PGPASSWORD environment variable.
# Otherwise, rely on .pgpass file or trust authentication for local pg_dump.
echo "Creating compressed backup locally: $LOCAL_BACKUP_PATH"
if [ -n "$PG_PASSWORD" ]; then
    PGPASSWORD="$PG_PASSWORD" pg_dump -Fc -p "$PG_PORT" -U "$PG_USER" "$DB_NAME" | gzip > "$LOCAL_BACKUP_PATH"
else
    pg_dump -Fc -p "$PG_PORT" -U "$PG_USER" "$DB_NAME" | gzip > "$LOCAL_BACKUP_PATH"
fi

# Check if pg_dump and gzip were successful and the file is not empty
if [ $? -eq 0 ] && [ -s "$LOCAL_BACKUP_PATH" ]; then
    echo "Local backup created successfully."
else
    echo "Error: Failed to create local backup. Check PostgreSQL credentials or database name."
    rm -f "$LOCAL_BACKUP_PATH" # Clean up potentially corrupted file
    exit 1
fi

# --- 2. Copy the backup file from local to the remote backup server ---
echo "Copying backup file from local to remote server..."
scp "$LOCAL_BACKUP_PATH" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_BACKUP_DIR/"

if [ $? -eq 0 ]; then
    echo "Backup transferred successfully to remote server."
else
    echo "Error: Failed to transfer backup file to remote server."
    # Decide if you want to exit here or proceed with local retention
    exit 1
fi

# --- 3. Implement Retention Policy on Local Machine ---
echo "Applying retention policy on local machine (keeping last $RETENTION_COUNT backups)..."
# Find all backup files, sort by modification time (newest first), skip the first N (RETENTION_COUNT), and delete the rest.
# Using 'sort -r' on filenames assumes timestamps in filenames are sortable (e.g., YYYYMMDD_HHMMSS).
find "$LOCAL_BACKUP_DIR" -maxdepth 1 -name "${DB_NAME}_*.sqlc.gz" -type f | sort -r | tail -n +$((RETENTION_COUNT + 1)) | xargs -r rm -f

if [ $? -eq 0 ]; then
    echo "Local old backups deleted successfully."
else
    echo "Error: Failed to delete old local backups."
fi

# --- 4. Implement Retention Policy on Remote Backup Server ---
echo "Applying retention policy on remote backup server (keeping last $RETENTION_COUNT backups)..."
# Using a single ssh command to execute multiple commands on the remote server for efficiency.
# `find . -maxdepth 1 -name "*.sqlc.gz" -type f -print0` finds files and prints them null-separated.
# `sort -rz` sorts them in reverse (newest first) handling nulls.
# `tail -zn +$((RETENTION_COUNT + 1))` skips the newest N files and outputs the rest.
# `xargs -0r rm -f` deletes the older files, handling potential spaces in filenames.
SSH_RETENTION_COMMAND="cd $REMOTE_BACKUP_DIR && \
    find . -maxdepth 1 -name \"${DB_NAME}_*.sqlc.gz\" -type f -print0 | sort -rz | tail -zn +$((RETENTION_COUNT + 1)) | xargs -0r rm -f"
    
ssh "$REMOTE_USER@$REMOTE_HOST" "$SSH_RETENTION_COMMAND"

if [ $? -eq 0 ]; then
    echo "Remote old backups deleted successfully."
else
    echo "Error: Failed to delete old remote backups."
fi

echo "--- PostgreSQL Backup Process Completed ---"