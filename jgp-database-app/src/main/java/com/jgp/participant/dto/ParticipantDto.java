package com.jgp.participant.dto;

import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.finance.dto.LoanDto;
import lombok.Builder;
import lombok.Setter;

import java.math.BigDecimal;
import java.util.List;

@Builder
public record ParticipantDto(
        String businessName,

        String jgpId,

        String phoneNumber,

        String ownerGender,

        Integer ownerAge,

        String businessLocation,

        String industrySector,

        String businessSegment,

        Boolean isBusinessRegistered,

        String registrationNumber,

        Boolean hasBMOMembership,

        String bmoMembership,

        BigDecimal bestMonthlyRevenue,

        BigDecimal worstMonthlyRevenue,

        Integer totalRegularEmployees,

        Integer youthRegularEmployees,

        Integer totalCasualEmployees,

        Integer youthCasualEmployees,

        String sampleRecords,

        String personWithDisability,

        String refugeeStatus,

        List<BMOClientDto> bmoClientDtos,

        List<LoanDto> loanDtos,

        String locationCountyCode) {
}
