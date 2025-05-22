package com.jgp.bmo.dto;

import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record MentorshipRequestDto(
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

         String additionalNeededSupport
) {
}
