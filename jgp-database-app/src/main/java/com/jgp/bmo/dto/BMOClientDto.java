package com.jgp.bmo.dto;

import java.time.LocalDate;

public record BMOClientDto(
         Long id,

         Long partnerId,

         String partnerName,

         Long participantId,

         String participantName,

         LocalDate dateFormSubmitted,

         Boolean isApplicantEligible,

         Integer tasAttended,

         Integer taSessionsAttended,

         Boolean isRecommendedForFinance,

         LocalDate decisionDate,

         String fiBusinessReferred,

         LocalDate dateRecordedByPartner,

         LocalDate dateRecordedToJGPDB,

         String taNeeds
) {}
