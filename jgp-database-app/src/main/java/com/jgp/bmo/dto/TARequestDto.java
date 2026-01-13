package com.jgp.bmo.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jgp.authentication.domain.AppUser;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.dto.ParticipantRequestDto;
import com.jgp.patner.domain.Partner;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.time.LocalDate;
import java.util.Map;

@Builder
public record TARequestDto(
        LocalDate dateFormSubmitted,
        Boolean isApplicantEligible,
        Integer tasAttended,
        Integer taSessionsAttended,
        Boolean isRecommendedForFinance,
        LocalDate decisionDate,
        @NotBlank(message = "Business referred is required !!")
        String fiBusinessReferred,
        @NotNull(message = "Date partner recorded is required !!")
        LocalDate dateRecordedByPartner,
        LocalDate dateRecordedToJGPDB,
        @NotBlank(message = "TA needs is required !!")
        String taNeeds,
        @NotNull(message = "Training Partner is required !!")
        String trainingPartner,
        @NotNull(message = "Delivery mode is required !!")
        String taDeliveryMode,
        String otherTaNeeds,
        @NotNull(message = "TA Type is required !!")
        String taType,
        AppUser createdBy,
        Document document,
        Integer rowIndex,
        String rowErrorMessage,
        ParticipantRequestDto participantRequestDto,
        Partner partner
) {
}
