package com.jgp.dashboard.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.util.List;

@Builder
public record PerformanceSummaryDto(
        String summaryYear,
        Integer yearNumber,
        String summaryMonth,
        Integer monthNumber,
        String summaryQuarter,
        String genderCategory,
        String partner,
        String category,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        List<PerformanceSummaryDto> children
) {
}
