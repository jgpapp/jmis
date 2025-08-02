package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record CountyDataSummaryResponseDto(
        String countyCode,
        String countyName,
        BigDecimal approximateCenterLatitude,
        BigDecimal approximateCenterLongitude,
        String businessesTrained,
        String businessesLoaned,
        String amountDisbursed,
        String businessesMentored
) {
}
