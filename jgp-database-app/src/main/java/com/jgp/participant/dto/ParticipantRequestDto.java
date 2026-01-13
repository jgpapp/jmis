package com.jgp.participant.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.jgp.bmo.dto.TAResponseDto;
import com.jgp.finance.dto.LoanResponseDto;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.math.BigDecimal;
import java.util.List;

@Builder
public record ParticipantRequestDto(
        String businessName,

        @NotNull(message = "JGP Id is required !!")
        String jgpId,

        String phoneNumber,

        String alternativePhoneNumber,

        @NotNull(message = "Gender is required !!")
        String ownerGender,

        @NotNull(message = "Owner age is required !!")
        Integer ownerAge,

        @NotBlank(message = "business location is required !!")
        String businessLocation,

        String industrySector,

        String businessSegment,

        String businessRegNumber,

        BigDecimal bestMonthlyRevenue,

        BigDecimal worstMonthlyRevenue,

        @NotNull(message = "Total regular employees is required !!")
        @Min(value = 1, message = "Total regular employees must be greater than 0 !!")
        Integer totalRegularEmployees,

        @NotNull(message = "Youth regular employees is required !!")
        Integer youthRegularEmployees,

        @NotNull(message = "Total casual employees is required !!")
        Integer totalCasualEmployees,

        @NotNull(message = "Youth casual employees is required !!")
        Integer youthCasualEmployees,

        @NotNull(message = "Participant Name is required !!")
        String participantName,

        List<String> sampleRecords,

        String personWithDisability,

        String disabilityType,

        String refugeeStatus,

        List<TAResponseDto> bmoClientDtos,

        List<LoanResponseDto> loanResponseDtos,

        String locationCountyCode,

        String locationSubCounty,

        BigDecimal locationLatitude,

        BigDecimal locationLongitude,

        Boolean isEligible,

        String businessFinancier,

        @JsonIgnore
        Integer rowIndex

        ) {
}
