package com.jgp.dashboard.dto;

import java.math.BigDecimal;

public record HighLevelSummaryDto(
        String businessesTrained,
        String businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount
) {}
