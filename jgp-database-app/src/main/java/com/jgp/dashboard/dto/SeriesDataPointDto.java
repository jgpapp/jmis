package com.jgp.dashboard.dto;

import java.util.Set;

public record SeriesDataPointDto(
        String name,
        Set<DataPointDto> series
) {
}
