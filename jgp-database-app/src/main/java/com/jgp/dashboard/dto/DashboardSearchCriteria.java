package com.jgp.dashboard.dto;

import java.time.LocalDate;

public record DashboardSearchCriteria(
        LocalDate fromDate, LocalDate toDate, Long partnerId, String countyCode, String trainingPartner
) {
}
