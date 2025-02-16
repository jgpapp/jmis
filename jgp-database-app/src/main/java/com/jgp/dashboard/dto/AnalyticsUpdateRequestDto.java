package com.jgp.dashboard.dto;

import jakarta.validation.constraints.NotNull;

import java.time.LocalDate;

public record AnalyticsUpdateRequestDto(Long partnerId, @NotNull LocalDate fromDate, @NotNull LocalDate toDate) {
}
