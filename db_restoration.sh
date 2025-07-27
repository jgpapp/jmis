# Define your connection details for the target PostgreSQL instance
PG_USER="root"       # The user that owns the database and will perform the restore
PG_PORT="5433"       # The port of the PostgreSQL instance you're restoring to
DB_NAME="jgp_restored"       # The name of the database you're restoring into

# Path to your backup file (e.g., from local backups or copied to /tmp)
BACKUP_FILE_PATH="/tmp/jgp_app_db_20250726_131815.sqlc.gz"

echo "Restoring database '$DB_NAME' from '$BACKUP_FILE_PATH'..."

# Restore command:
# gunzip -c : decompresses the file and pipes its content to stdout
# pg_restore: reads from stdin (-f -), specifies database, user, port
gunzip -c "$BACKUP_FILE_PATH" | pg_restore -h localhost -p "$PG_PORT" -U "$PG_USER" -d "$DB_NAME" --clean --if-exists

if [ $? -eq 0 ]; then
    echo "Database restoration completed successfully."
else
    echo "Error: Database restoration failed. Check the error messages above."
fi