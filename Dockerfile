# Dockerfile for backup service
FROM alpine:latest

RUN apk add --no-cache postgresql-client rsync cronie openssh-client

# Copy your crontab configuration for the backup user (e.g., root)
# This assumes your backup_script.sh is generic enough or sources env vars
COPY crontab_backup /etc/crontabs/root
COPY create_base_backup.sh /create_base_backup.sh
COPY sync_wals.sh /sync_wals.sh

# Set appropriate permissions for the crontab file
RUN chmod 600 /etc/crontabs/root
RUN chmod +x /create_base_backup.sh /sync_wals.sh

# Expose the log directory if you want to mount it
VOLUME /var/log

# The command that starts the cron daemon in the foreground
CMD ["crond", "-f"]
