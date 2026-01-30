package com.jgp.dashboard.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public record DataSummaryDto(
        String genderCategory,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        BigDecimal amountRepaid,
        LocalDate summaryDate,
        String summaryWeek,
        String summaryMonth,
        String summaryYear
) {

}
