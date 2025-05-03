// This file will be processed by envsubst at container startup
(function(window) {
    window.__env = window.__env || {};
  
    // Environment variables will be populated here by the entrypoint script
    window.__env.baseApiUrl = '${JGP_APP_BASE_URL}';
    window.__env.baseWebSocketUrl = '${JGP_APP_BASE_WEBSOCKET_URL}';
  
    // Add more variables as needed
    // window.__env.anotherVar = '${ANOTHER_VAR}';
  
  })(this);