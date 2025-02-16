package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record CountySummaryDto(
        String countyCode,
        String countyName,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        Integer dataYear,
        Integer dataMonth
) {
}
