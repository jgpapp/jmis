package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record DataSummaryDto(
        String genderCategory,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        Integer dataYear,
        Integer dataMonth
) {
}
