package com.jgp.dashboard.dto;

import lombok.Builder;

@Builder
public record SummaryWeekMonthAndYearDto(
        String summaryWeek, int weekNumber, String summaryMonth, int monthNumber,
        String summaryQuarter, int quarterNumber, String summaryYear, int yearNumber
) {
}
