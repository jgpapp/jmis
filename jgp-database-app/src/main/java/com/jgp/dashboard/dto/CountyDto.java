package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record CountyDto(
        String countyCode,
        String countyName,
        BigDecimal approximateCenterLatitude,
        BigDecimal approximateCenterLongitude
) {
}
