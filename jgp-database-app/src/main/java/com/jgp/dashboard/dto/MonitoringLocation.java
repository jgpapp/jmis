package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record MonitoringLocation(
        Long surveyId,
        String businessCountyName,
        BigDecimal lat,
        BigDecimal lng
) {
}
