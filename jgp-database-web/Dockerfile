# Stage 1: Build the Angular app
FROM node:18-alpine AS build

# Set working directory
WORKDIR /app

# Install dependencies
COPY package*.json ./
RUN npm install --force

# Copy the rest of the application files
COPY . .

# Build the Angular app
RUN npm run build -- --configuration production

# Stage 2: Serve the app with Nginx
FROM nginx:alpine

# Remove the default Nginx welcome page
RUN rm /usr/share/nginx/html/*

# Copy the build output to Nginx's HTML directory
COPY --from=build /app/dist/jgp-database-web/browser /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf
# Create NGINX cache directory
RUN mkdir -p /var/cache/nginx/client_temp
# Set ownership of NGINX cache directory
RUN chown -R nginx:nginx /var/cache/nginx

# Expose port 80
EXPOSE 80

# Start Nginx
CMD ["nginx", "-g", "daemon off;"]