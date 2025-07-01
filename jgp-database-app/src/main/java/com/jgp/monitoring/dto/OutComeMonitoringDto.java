package com.jgp.monitoring.dto;


import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Pattern;
import lombok.Builder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Builder
public record OutComeMonitoringDto(
        Long id,
        LocalDate surveyDate,
        String surveyLanguage,
        Boolean consented,
        BigDecimal locationLatitude,
        BigDecimal locationLongitude,
        Long participantId,
        @Min(value = 18, message = "Age must be at least 18")
        @Max(value = 150, message = "Age must not exceed 150")
        Integer age,
        @Pattern(regexp = "Men 36+|Young Women|Young Men|Women 36+", message = "Gender category must be Men 36+, Young Women,  Young Men, or Women 36+")
        String genderCategory,
        @Pattern(regexp = "Micro|SME|MSME", message = "Segment must be Micro, SME or MSME")
        String segment,
        @Pattern(regexp = "KEPSA|PBP|KNCCI|4G|GROOTS|GBF|Don't Know", message = "Partner must be KEPSA, PBP, KNCCI, 4G, GROOTS, GBF or Don't Know")
        String partner,
        @Pattern(regexp = "MALE|FEMALE|DONT KNOW|REFUSED", message = "Gender must be MALE, FEMALE, DONT KNOW or REFUSED")
        String gender,
        @Pattern(regexp = "Central|Coast|Lower Eastern|Nairobi|North Eastern|North Rift|South Rift|Nyanza|Upper Eastern|Western", message = "Region must be Central, Coast, Lower Eastern, Nairobi, North Eastern, North Rift, South Rift, Nyanza, Upper Eastern or Western")
        String region,
        String countyCode,
        String countyName,
        @Pattern(regexp = "Urban|Rural|DONT KNOW|REFUSED", message = "Business setting must be Urban, Rural, DONT KNOW or REFUSED")
        String businessSetting,
        @Pattern(regexp = "10+ Years|1-5 Years|6-10 Years", message = "Business age category must be 10+ Years, 1-5 Years or 6-10 Years")
        String businessAgeCategory,
        @Pattern(regexp = "Persons with disability|Refugee|None of the above|DONT KNOW|REFUSED", message = "Group must be Persons with disability|Refugee|None of the above|DONT KNOW|REFUSED")
        String groupMembership,
        @Pattern(regexp = "No formal education|Primary school|Secondary school|Tertiary education|College|certificate/diploma|Graduate degree|ost graduate degree|DONT KNOW|REFUSED", message = "Education level must be No formal education|Primary school|Secondary school|Tertiary education|College|certificate/diploma|Graduate degree|ost graduate degree|DONT KNOW|REFUSED")
        String educationLevel,
        @Min(value = 0, message = "Business age must be at least 0")
        Integer businessAge,
        @Min(value = 0, message = "Regular employees must be at least 0")
        Integer regularEmployees,
        @Min(value = 0, message = "Casual employees must be at least 0")
        Integer casualEmployees,
        @Pattern(regexp = "Increased a lot|Increased|Stayed the same|Decreased|Decreased a lot|DONT KNOW|REFUSED", message = "Income change must be Increased a lot|Increased|Stayed the same|Decreased|Decreased a lot|DONT KNOW|REFUSED")
        String householdIncomeChange,
        String financialStability,
        String qualityOfLife,
        String empowerment,
        String voiceInCommunity,
        String respectInCommunity,
        String reliableIncome,
        String reputableWork,
        String senseOfPurpose,
        String businessSectorGrowth,
        String communityGrowth,
        String workOpportunities,
        String incomeRegularity,
        String incomeSufficiency,
        String incomePredictability,
        String financialSecurity,
        String communityGroups,
        String leadershipRole,
        String decisionMakingConfidence,
        String communityChange,
        String communityIssues,
        String satisfactionEducation,
        String satisfactionRelationships,
        String satisfactionBusinessType,
        String satisfactionIncome,
        String satisfactionHousing,
        String satisfactionHealthcare,
        String satisfactionWater,
        String satisfactionFood,
        String satisfactionNutrition,
        String satisfactionLife,
        String satisfactionInformation,
        String satisfactionLeisure,
        String jgpInterventions,
        String technicalTraining,
        String newPractices,
        String improvedPractices,
        String trainingImprovements,
        String businessChanges,
        String profitabilityGrowth,
        BigDecimal revenueChange,
        String loanApplication,
        Integer numberOfLoans,
        String loanPlatform,
        String externalFinancing,
        String financingSources,
        String jgpImpact,
        String changesWithoutJgp,
        String marketAccess,
        String businessOpportunities,
        String marketChallenges,
        String uploadedBy,
        LocalDate dateUploaded,
        String approvedBy,
        LocalDate dateApproved
) {}
