package com.jgp.dashboard.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public record CountySummaryDto(
        String countyCode,
        String countyName,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        LocalDate dataDate
) {
}
