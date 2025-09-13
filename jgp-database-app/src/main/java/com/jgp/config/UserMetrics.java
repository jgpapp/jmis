package com.jgp.config;

import com.jgp.authentication.service.ActiveUserService;
import io.micrometer.core.instrument.MeterRegistry;
import jakarta.annotation.PostConstruct;
import org.springframework.stereotype.Component;

@Component
public class UserMetrics {

    private final MeterRegistry meterRegistry;
    private final ActiveUserService activeUserService;

    public UserMetrics(MeterRegistry meterRegistry, ActiveUserService activeUserService) {
        this.meterRegistry = meterRegistry;
        this.activeUserService = activeUserService;
    }

    @PostConstruct
    public void registerMetrics() {
        // Register a gauge that reports the active user count
        meterRegistry.gauge("app.active.users", activeUserService, ActiveUserService::getActiveUserCount);
    }
}
