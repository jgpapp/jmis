package com.jgp.participant.dto;

import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.finance.dto.LoanDto;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.math.BigDecimal;
import java.util.List;

@Builder
public record ParticipantDto(
        @NotNull(message = "Business Name is required !!")
        String businessName,

        @NotNull(message = "JGP Id is required !!")
        @Min(value = 5, message = "JGP Id must be at least 5 characters !!")
        @Max(value = 10, message = "JGP Id must be at most 10 characters !!")
        String jgpId,

        @Min(value = 9, message = "JGP Id must be at least 9 characters !!")
        @Max(value = 12, message = "JGP Id must be at most 12 characters !!")
        String phoneNumber,

        @NotNull(message = "Gender is required !!")
        String ownerGender,

        @NotNull(message = "Owner age is required !!")
        Integer ownerAge,

        @NotBlank(message = "business location is required !!")
        String businessLocation,

        String industrySector,

        @NotBlank(message = "business segment is required !!")
        String businessSegment,

        Boolean isBusinessRegistered,

        String registrationNumber,

        Boolean hasBMOMembership,

        String bmoMembership,

        BigDecimal bestMonthlyRevenue,

        BigDecimal worstMonthlyRevenue,

        @NotNull(message = "Total regular employees is required !!")
        Integer totalRegularEmployees,

        @NotNull(message = "Youth regular employees is required !!")
        Integer youthRegularEmployees,

        @NotNull(message = "Total casual employees is required !!")
        Integer totalCasualEmployees,

        @NotNull(message = "Youth casual employees is required !!")
        Integer youthCasualEmployees,

        @NotBlank(message = "Sample records is required !!")
        String sampleRecords,

        @NotBlank(message = "Disability status is required !!")
        String personWithDisability,

        @NotBlank(message = "Refugee status is required !!")
        String refugeeStatus,

        List<BMOClientDto> bmoClientDtos,

        List<LoanDto> loanDtos,

        String locationCountyCode) {
}
