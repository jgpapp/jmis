// This file will be processed by envsubst at container startup
(function(window) {
    window.__env = window.__env || {};
  
    // Environment variables will be populated here by the entrypoint script
    window.__env.baseApiUrl = '/api';
    window.__env.baseWebSocketUrl = '/ws';
  
    // Add more variables as needed
    // window.__env.anotherVar = '${ANOTHER_VAR}';
  
  })(this);