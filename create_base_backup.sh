#!/bin/bash

# Configuration
POSTGRES_CONTAINER_NAME="postgres_jgp_db" # Service name of your Postgres container in docker-compose.yml
POSTGRES_USER="jgp_user"     # *** CHANGE THIS to your actual PostgreSQL superuser ***
# If you used 'POSTGRES_USER: your_postgres_user' in docker-compose.yml, use that here.
# For pg_basebackup, you generally need a superuser or a user with REPLICATION privilege.
# 'postgres' is the default superuser.
POSTGRES_HOST="localhost"    # Since you're running inside the same container (via docker exec)
POSTGRES_PORT="5432"         # Default PostgreSQL port
LOCAL_BASE_BACKUP_DIR="/home/jmis/backup" # Directory on your host for base backups
REMOTE_USER="jgsadmin"
REMOTE_HOST="20.242.125.65"
REMOTE_BASE_BACKUP_PATH="/home/jmis/anotherbackup"
RETENTION_HOURS=2

# Create local backup directory if it doesn't exist
mkdir -p "$LOCAL_BASE_BACKUP_DIR"

TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_LABEL="base_backup_${TIMESTAMP}"
BACKUP_TARGET_DIR="${LOCAL_BASE_BACKUP_DIR}/${BACKUP_LABEL}"

echo "Starting PostgreSQL base backup for container: $POSTGRES_CONTAINER_NAME"

# Perform pg_basebackup inside the Docker container
# -h: specify host (localhost, as it's the same container)
# -p: specify port
# -U: specify PostgreSQL user
# -D: destination directory
# -Ft: tar format
# -z: gzip compression
# -X stream: stream WAL files as part of the backup (highly recommended)
# -c fast: Use 'fast' checksumming if desired
# -l: label for the backup
# -P: show progress
docker exec "$POSTGRES_CONTAINER_NAME" pg_basebackup \
    -h "$POSTGRES_HOST" \
    -p "$POSTGRES_PORT" \
    -U "$POSTGRES_USER" \
    -D /tmp/base_backup \
    -Ft -z -X stream -c fast -l "$BACKUP_LABEL" -P

if [ $? -eq 0 ]; then
    echo "PostgreSQL base backup created successfully in container's /tmp/base_backup."

    # Copy the tarball from inside the container to the host
    docker cp "$POSTGRES_CONTAINER_NAME":/tmp/base_backup/base.tar.gz "$BACKUP_TARGET_DIR.tar.gz"
    docker cp "$POSTGRES_CONTAINER_NAME":/tmp/base_backup/pg_wal.tar.gz "$BACKUP_TARGET_DIR_wal.tar.gz" # WALs streamed during backup

    # Clean up temporary backup in container
    docker exec "$POSTGRES_CONTAINER_NAME" rm -rf /tmp/base_backup

    if [ $? -eq 0 ]; then
        echo "Base backup tarballs copied to host: ${BACKUP_TARGET_DIR}.tar.gz and ${BACKUP_TARGET_DIR}_wal.tar.gz"

        # Transfer base backup to remote server
        echo "Transferring base backup to remote server: $REMOTE_HOST:$REMOTE_BASE_BACKUP_PATH"
        rsync -azhv "${BACKUP_TARGET_DIR}.tar.gz" "${BACKUP_TARGET_DIR}.tar.gz" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_BASE_BACKUP_PATH"

        if [ $? -eq 0 ]; then
            echo "Base backup transferred successfully to remote server."
            # Optional: Clean up old local base backups (e.g., keep only the latest 2-3)
            find "$LOCAL_BASE_BACKUP_DIR" -type f -name "*.tar.gz" -mtime +"$((RETENTION_HOURS * 60))" -delete
        else
            echo "ERROR: Failed to transfer base backup to remote server."
            exit 1
        fi
    else
        echo "ERROR: Failed to copy base backup from container to host."
        exit 1
    fi
else
    echo "ERROR: PostgreSQL base backup failed."
    exit 1
fi

echo "PostgreSQL base backup process finished."
