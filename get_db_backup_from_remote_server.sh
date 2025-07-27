# On your local machine:
REMOTE_USER="jgsadmin"
REMOTE_HOST="20.242.125.65"
REMOTE_BACKUP_DIR="/home/jmis/jmis_db_backups"
BACKUP_FILE_TO_RESTORE="jgp_app_db_20250726_131815.sqlc.gz" # The specific file you want

scp "$REMOTE_USER@$REMOTE_HOST:$REMOTE_BACKUP_DIR/$BACKUP_FILE_TO_RESTORE" /tmp/