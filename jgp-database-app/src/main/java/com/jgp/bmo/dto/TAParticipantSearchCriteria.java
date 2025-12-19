package com.jgp.bmo.dto;

import lombok.Builder;

@Builder
public record TAParticipantSearchCriteria(
        Long partnerId,
        Long participantId,
        Boolean approvedByPartner
) {
}
