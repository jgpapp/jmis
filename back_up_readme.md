backup_postgres backs up everything while sync_wals backs up incremental wal files

- Make the script executable by running chmod +x /path/to/your/script/create_base_backup.sh

- Generate ssh keys on the the server hosting the db by running `ssh-keygen -t rsa -b 4096 -C "juliandan7@gmail.com"` and leave passphrase empty and everything default.
- Copy the key to backup server by running `ssh-copy-id your_remote_user@your_remote_host_ip_or_hostname`

- To create cron job for base backup 0 4 * * 0 /path/to/your/script/create_base_backup.sh >> /var/log/postgres_base_backup.log 2>&1 (Every Sunday at 4 AM)
- To set wal cron job 30 0 * * * /path/to/your/script/sync_wals.sh >> /var/log/postgres_wal_sync.log 2>&1 (Every day at 00:30 AM)
