package com.jgp.dashboard.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record DataSummaryDto(
        Long partnerId,
        String genderCategory,
        Integer businessesTrained,
        Integer businessesLoaned,
        BigDecimal amountDisbursed,
        BigDecimal outStandingAmount,
        BigDecimal amountRepaid,
        LocalDate summaryDate,
        String summaryWeek,
        int weekNumber,
        String summaryMonth,
        int monthNumber,
        String summaryQuarter,
        int quarterNumber,
        String summaryYear,
        int yearNumber) {

}
