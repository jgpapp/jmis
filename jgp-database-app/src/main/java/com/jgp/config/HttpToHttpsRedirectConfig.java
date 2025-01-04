package com.jgp.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;
import org.springframework.boot.web.server.WebServerFactoryCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.apache.catalina.connector.Connector;

@Configuration
public class HttpToHttpsRedirectConfig {

    @Value("${server.port:8082}")
    private Integer springHttpPort;
    @Value("${spring.https.port:8082}")
    private Integer springHttpsPort;

   /* @Bean
    public WebServerFactoryCustomizer<TomcatServletWebServerFactory> containerCustomizer() {
        return factory -> factory.addAdditionalTomcatConnectors(httpToHttpsRedirectConnector());
    }*/

    private Connector httpToHttpsRedirectConnector() {
        Connector connector = new Connector(TomcatServletWebServerFactory.DEFAULT_PROTOCOL);
        connector.setScheme("http");
        connector.setPort(springHttpPort);  // HTTP port
        connector.setSecure(false);
        connector.setRedirectPort(springHttpsPort);  // HTTPS port
        return connector;
    }
}
