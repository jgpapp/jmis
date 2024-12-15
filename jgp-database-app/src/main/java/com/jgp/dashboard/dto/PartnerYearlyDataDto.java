package com.jgp.dashboard.dto;

public record PartnerYearlyDataDto(
        String partnerName,
        String genderName,
        Integer year,
        String value
) {
}
