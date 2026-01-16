# Stage 1: Build the Angular app
FROM node:22.12-alpine AS build

# Set working directory
WORKDIR /app

# Copy package files for better caching
COPY package*.json ./

# Install dependencies
RUN npm ci --legacy-peer-deps

# Copy source files
COPY . .

# Build the Angular app
RUN npm run build -- --configuration production

# Stage 2: Serve the app with Nginx
FROM nginx:1.27-alpine

# Install gettext for envsubst
RUN apk add --no-cache gettext

# Define where Nginx serves files from
WORKDIR /usr/share/nginx/html

# Remove default Nginx welcome page
RUN rm -rf ./*

# Copy the build output to Nginx's HTML directory
COPY --from=build --chown=nginx:nginx /app/dist/jgp-database-web/browser /usr/share/nginx/html

# Copy env.template.js to the HTML directory
COPY --chown=nginx:nginx public/env.template.js /usr/share/nginx/html/env.template.js

# Copy the Nginx TEMPLATE config
COPY nginx.template.conf /etc/nginx/nginx.template.conf

# Copy the Nginx TEMPLATE config
COPY nginx.template.conf /etc/nginx/nginx.template.conf

# Create NGINX cache directory
RUN mkdir -p /var/cache/nginx/client_temp

# Set ownership of NGINX directories
RUN chown -R nginx:nginx /var/cache/nginx /usr/share/nginx/html

# Copy the entrypoint script and make it executable
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Expose ports
EXPOSE 80 443

# Add health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --quiet --tries=1 --spider http://localhost/ || exit 1

# Set the entrypoint script to run on container start
ENTRYPOINT ["/entrypoint.sh"]

# Start Nginx
CMD ["nginx", "-g", "daemon off;"]