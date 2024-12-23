(function(window) {
    window.env = window.env || {};
  
    // Environment variables
    window["env"].BASE_API_URL = '${BASE_API_URL}';
    window["env"].BASE_API_URL_PORT = '${BASE_API_URL_PORT}';
  })(this);