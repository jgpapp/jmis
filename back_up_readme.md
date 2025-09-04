#### Database Backup And Restoration

- This set up is meant to backup postgres once daily to a remote server.
- Make sure we only keep 3 copies of latest backups
- Restore data to one selected backup file most probably latest file

## Files used
- `backup_postgres.sh`
- `get_db_backup_from_remote_server.sh`
- `db_restoration.sh`

### Set up passwordless access to the remote server
- Generate ssh keys on the the server hosting the db by running `ssh-keygen -t rsa -b 4096 -C "juliandan7@gmail.com"` and leave passphrase empty and everything default.
- Copy the key to backup server by running `ssh-copy-id your_remote_user@your_remote_host_ip_or_hostname`

### Change the Authentication Method to scram-sha-256
- Locate the `pg_hba.conf` file which should be at `/etc/postgresql/<db-version>/main/pg_hba.conf` in our case db-version is 17.
- Look for a line that controls local connections, which often looks like this:`local all all peer`
- Change the `peer` method to `scram-sha-256` for all local users. It should then look like this:`local all all scram-sha-256`

### Set up the remote server backup directory
- Run the following command `ssh your_remote_ssh_user@your_remote_server_ip "mkdir -p /path/to/remote/backups"` replacing remote ssh user, remote server ip and path to the backup directory

### Make the Script Executable
- Make backup script executable by running `chmod +x backup_postgres.sh`

### Add postgres user password as an environment variable
- Run `export PG_PASSWORD=your_postgres_user_password`

### Schedule backups with Cron
- Run `crontab -e`
- Add the following line to run the script every day at, for example, 01:00 AM EAT (03:00 UTC):
`0 1 * * * /path/to/your/backup_postgres.sh >> /var/log/postgres_local_backup.log 2>&1`

### Setup For Restoration

### Make the Script Executable
- Make backup script executable by running `chmod +x get_db_backup_from_remote_server.sh`

## Get backup file from remote server
- Update `get_db_backup_from_remote_server.sh` with remote server details and exact file you want to restore
- Then run `sh get_db_backup_from_remote_server.sh`

## Create the database
- Drop and recreate the databse you want to restore or create a new database

### Add postgres user password as an environment variable
- Run `export PG_PASSWORD=your_postgres_user_password`

### Make the Script Executable
- Make backup script executable by running `chmod +x db_restoration.sh`

## Restore the database
- Update `db_restoration.sh` with db details
- Then run `sh db_restoration.sh`
