package com.jgp.bmo.dto;

import java.io.Serializable;
import java.time.LocalDate;

public record TAResponseDto(
         Long id,

         Long partnerId,

         String participantJGPID,

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

         String dateRecordedToJGPDB,

         String uploadedBy,

         String dateUploaded,

         String approvedBy,

         LocalDate dateApproved,

         String taNeeds
) implements Serializable {}
