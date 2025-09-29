# Stage 1: Build the Angular app
FROM node:20-alpine AS build

# Set working directory
WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm install --force

# Copy the rest of the application files
COPY . .

# Build the Angular app
RUN npm cache clean --force && npm run build -- --configuration production

# Stage 2: Serve the app with Nginx
FROM nginx:alpine

# Install gettext for envsubst
RUN apk add --no-cache gettext

# Define where Nginx serves files from
WORKDIR /usr/share/nginx/html

# Remove default Nginx welcome page
RUN rm -rf ./*

# Copy the build output to Nginx's HTML directory
COPY --from=build /app/dist/jgp-database-web/browser /usr/share/nginx/html

# Copy the Nginx TEMPLATE config
# Note the change in filename and destination path (temporary location)
COPY nginx.template.conf /etc/nginx/nginx.template.conf

#COPY nginx-http.conf /etc/nginx/conf.d/default.conf
# Create NGINX cache directory
RUN mkdir -p /var/cache/nginx/client_temp
# Set ownership of NGINX cache directory
RUN chown -R nginx:nginx /var/cache/nginx

# Copy the entrypoint script and make it executable
COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

# Expose port 80
EXPOSE 80 443

# Set the entrypoint script to run on container start
ENTRYPOINT ["/entrypoint.sh"]

# Start Nginx
CMD ["nginx", "-g", "daemon off;"]