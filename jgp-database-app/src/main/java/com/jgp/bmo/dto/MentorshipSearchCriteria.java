package com.jgp.bmo.dto;

import lombok.Builder;

@Builder
public record MentorshipSearchCriteria(
        Long partnerId,
        Long participantId,
        String dataStatus
) {
}
