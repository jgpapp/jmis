package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record CountyDataSummaryResponseDto(
        String countyCode,
        String countyName,
        BigDecimal approximateCenterLatitude,
        BigDecimal approximateCenterLongitude,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        Integer businessesMentored
) {
}
