### Setup For Backup
- `create_base_backup.sh` backs up everything while `sync_wals.sh` backs up incremental wal files
- Make these 2 files accessible from docker by runing 
`sudo chown -R $USER:$USER ./wal_archive`
`sudo chown -R $USER:$USER ./pg_hba.conf`
`sudo chmod -R u+rwx,go+rwx ./wal_archive`
`sudo chmod -R u+rwx,go+rwx ./pg_hba.conf`

- Generate ssh keys on the the server hosting the db by running `ssh-keygen -t rsa -b 4096 -C "juliandan7@gmail.com"` and leave passphrase empty and everything default.
- Copy the key to backup server by running `ssh-copy-id your_remote_user@your_remote_host_ip_or_hostname`

### Setup For Restoration

