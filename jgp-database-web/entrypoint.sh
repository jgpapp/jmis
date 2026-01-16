#!/bin/sh
set -e # Exit immediately if a command exits with a non-zero status.

echo "Running entrypoint script..."

# Define the template file path and the output file path within the container
TEMPLATE_FILE="/usr/share/nginx/html/env.template.js"
OUTPUT_FILE="/usr/share/nginx/html/env.js"

# Export the environment variables required by envsubst
# Docker Compose will set these variables
# export JGP_APP_BASE_URL
# export JGP_APP_BASE_WEBSOCKET_URL
# Add other exports here
# export ANOTHER_VAR

#echo "Substituting environment variables into ${OUTPUT_FILE}..."

echo "Copying env.js..."
cp "$TEMPLATE_FILE" "$OUTPUT_FILE"

# Use envsubst to replace placeholders in the template and create the final env.js
# The single quotes around the variables list prevent shell expansion before envsubst runs
# envsubst '${JGP_APP_BASE_URL} ${JGP_APP_BASE_WEBSOCKET_URL}' < "$TEMPLATE_FILE" > "$OUTPUT_FILE"
# Add other variables like '${ANOTHER_VAR}' inside the single quotes if needed

echo "Substitution complete. env.js content:"
cat "$OUTPUT_FILE" # Optional: Log the generated file content for debugging


# --- Start: Nginx config generation ---
echo "Generating Nginx configuration..."
TEMPLATE_NGINX_CONF="/etc/nginx/nginx.template.conf"
OUTPUT_NGINX_CONF="/etc/nginx/conf.d/default.conf" # The final Nginx config file

# Export Nginx variables needed by envsubst
# Provide defaults using shell parameter expansion if needed
export SERVER_NAME=${SERVER_NAME:-localhost}
export SSL_CERT_PATH=${SSL_CERT_PATH:-/etc/ssl/certs/nginx-default.crt} # Example default
export SSL_KEY_PATH=${SSL_KEY_PATH:-/etc/ssl/private/nginx-default.key} # Example default
# Add other Nginx related env vars here
# export ACCESS_LOG_FILENAME=${ACCESS_LOG_FILENAME:-access.log}

# Define the list of variables for envsubst for Nginx config
NGINX_VARS='${SERVER_NAME} ${SSL_CERT_PATH} ${SSL_KEY_PATH}' # Add others like ${ACCESS_LOG_FILENAME}

echo "Substituting Nginx configuration variables..."
envsubst "$NGINX_VARS" < "$TEMPLATE_NGINX_CONF" > "$OUTPUT_NGINX_CONF"

echo "Nginx configuration generated:"
cat "$OUTPUT_NGINX_CONF" # Optional: Log generated config
# --- End: Nginx config generation ---

echo "Starting Nginx..."
# Execute the CMD passed to the docker run command (which is nginx -g 'daemon off;')
exec "$@"