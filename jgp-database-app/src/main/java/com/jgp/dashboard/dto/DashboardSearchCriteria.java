package com.jgp.dashboard.dto;

import lombok.Builder;

import java.time.LocalDate;

@Builder
public record DashboardSearchCriteria(
        LocalDate fromDate, LocalDate toDate, Long partnerId, String countyCode, String trainingPartner, String timeScale
) {
}
