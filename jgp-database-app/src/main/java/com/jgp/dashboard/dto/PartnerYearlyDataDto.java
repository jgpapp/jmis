package com.jgp.dashboard.dto;

public record PartnerYearlyDataDto(
        String partnerName,
        Integer year,
        String value
) {
}
