package com.jgp.dashboard.dto;

import java.math.BigDecimal;
import java.util.List;

public record PerformanceSummaryDto(
        Integer year,
        Integer month,
        String genderCategory,
        String partner,
        String quarter,
        String category,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        List<PerformanceSummaryDto> children
) {
}
