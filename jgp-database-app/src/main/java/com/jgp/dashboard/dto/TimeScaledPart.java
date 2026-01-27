package com.jgp.dashboard.dto;

import lombok.Builder;

@Builder
public record TimeScaledPart(
        String dateField,
        String timeScale, String aggregationFunction,
        String aggregatedColumn, String entity, String whereClause) {
}
