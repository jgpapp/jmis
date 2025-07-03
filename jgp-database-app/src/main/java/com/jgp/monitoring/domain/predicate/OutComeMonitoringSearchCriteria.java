package com.jgp.monitoring.domain.predicate;

import lombok.Builder;

import java.time.LocalDate;

@Builder
public record OutComeMonitoringSearchCriteria(
        LocalDate fromDate,
        LocalDate toDate,
        String participantAgeGroup,
        String gender,
        String genderCategory,
        String jgpIntervention,
        String countyCode,
        String region,
        Boolean approved,
        String partner,
        Long participantId,
        String summarizingColumn) {
}
