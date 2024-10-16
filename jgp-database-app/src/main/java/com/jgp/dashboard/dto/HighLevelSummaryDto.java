package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record HighLevelSummaryDto(
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount
) {}
