package com.jgp.bmo.dto;

import lombok.Builder;

@Builder
public record BMOParticipantSearchCriteria(
        Long partnerId,
        Long participantId,
        Boolean approvedByPartner
) {
}
