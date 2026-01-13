package com.jgp.bmo.dto;

import com.jgp.authentication.domain.AppUser;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.patner.domain.Partner;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record MentorshipRequestDto(
        ParticipantRequestDto participantRequestDto,

        LocalDate mentorShipDate,

        String mentorShipOrganization,

        String bmoMemberShip,

        String mentorShipDeliveryMode,

        String businessSituation,

        int newHiresBecauseOfLoan,

        BigDecimal revenueIncreaseDueToTraining,

        String usefulTrainingTopics,

        String supportNeededAreas,

        String msmeSessionsCovered,

        String smeSessionsCovered,

        String identifiedBusinessGaps,

        String agreedActionForGapOne,

        String additionalNeededSupport,

        Partner partner,

        Document document,

        AppUser createdBy,

        Integer rowIndex

) {
}
