package com.jgp.participant.dto;

import com.jgp.bmo.dto.BMOClientDto;
import com.jgp.finance.dto.LoanDto;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

@Getter
@Setter
public class ParticipantResponseDto implements Serializable {
    private String businessName;

    private String jgpId;

    private String phoneNumber;

    private String ownerGender;

    private Integer ownerAge;

    private String businessLocation;

    private String industrySector;

    private String businessSegment;

    private Boolean isBusinessRegistered;

    private String registrationNumber;

    private Boolean hasBMOMembership;

    private String bmoMembership;

    private BigDecimal bestMonthlyRevenue;

    private BigDecimal worstMonthlyRevenue;

    private Integer totalRegularEmployees;

    private Integer youthRegularEmployees;

    private Integer totalCasualEmployees;

    private Integer youthCasualEmployees;

    private String sampleRecords;

    private String personWithDisability;

    private String refugeeStatus;

    private List<BMOClientDto> bmoClientDtos;

    private List<LoanDto> loanDtos;
}
