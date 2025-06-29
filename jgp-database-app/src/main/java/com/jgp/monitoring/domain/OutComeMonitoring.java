package com.jgp.monitoring.domain;

import com.jgp.authentication.domain.AppUser;
import com.jgp.monitoring.dto.OutComeMonitoringDto;
import com.jgp.participant.domain.Participant;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.stream.Collectors;

@Setter
@Getter
@Entity
@Table(name = "outcome_monitoring")
public class OutComeMonitoring extends BaseEntity {

    @Column(name = "survey_date")
    private LocalDate surveyDate;

    @Column(name = "survey_language")
    private String surveyLanguage;

    @Column(name = "consented")
    private Boolean consented;

    @Column(name = "location_latitude")
    private BigDecimal locationLatitude;

    @Column(name = "location_longitude")
    private BigDecimal locationLongitude;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "participant_id")
    private Participant participant;

    @Column(name = "age")
    private Integer age;

    @Column(name = "gender_category")
    private String genderCategory;

    @Column(name = "segment")
    private String segment;

    @Column(name = "partner")
    private String partner;

    @Column(name = "gender")
    private String gender;

    @Column(name = "region")
    private String region;

    @Column(name = "county_code")
    private String countyCode;

    @Column(name = "county_name")
    private String countyName;

    @Column(name = "business_setting")
    private String businessSetting;

    @Column(name = "business_age_category")
    private String businessAgeCategory;

    @Column(name = "group_membership")
    private String groupMembership;

    @Column(name = "education_level")
    private String educationLevel;

    @Column(name = "business_age")
    private Integer businessAge;

    @Column(name = "regular_employees")
    private Integer regularEmployees;

    @Column(name = "casual_employees")
    private Integer casualEmployees;

    @Column(name = "household_income_change")
    private String householdIncomeChange;

    @Column(name = "financial_stability")
    private String financialStability;

    @Column(name = "quality_of_life")
    private String qualityOfLife;

    @Column(name = "empowerment")
    private String empowerment;

    @Column(name = "voice_in_community")
    private String voiceInCommunity;

    @Column(name = "respect_in_community")
    private String respectInCommunity;

    @Column(name = "reliable_income")
    private String reliableIncome;

    @Column(name = "reputable_work")
    private String reputableWork;

    @Column(name = "sense_of_purpose")
    private String senseOfPurpose;

    @Column(name = "business_sector_growth")
    private String businessSectorGrowth;

    @Column(name = "community_growth")
    private String communityGrowth;

    @Column(name = "work_opportunities")
    private String workOpportunities;

    @Column(name = "income_regularity")
    private String incomeRegularity;

    @Column(name = "income_sufficiency")
    private String incomeSufficiency;

    @Column(name = "income_predictability")
    private String incomePredictability;

    @Column(name = "financial_security")
    private String financialSecurity;

    @Column(name = "community_groups")
    private String communityGroups;

    @Column(name = "leadership_role")
    private String leadershipRole;

    @Column(name = "decision_making_confidence")
    private String decisionMakingConfidence;

    @Column(name = "community_change")
    private String communityChange;

    @Column(name = "community_issues")
    private String communityIssues;

    @Column(name = "satisfaction_education")
    private String satisfactionEducation;

    @Column(name = "satisfaction_relationships")
    private String satisfactionRelationships;

    @Column(name = "satisfaction_business_type")
    private String satisfactionBusinessType;

    @Column(name = "satisfaction_income")
    private String satisfactionIncome;

    @Column(name = "satisfaction_housing")
    private String satisfactionHousing;

    @Column(name = "satisfaction_healthcare")
    private String satisfactionHealthcare;

    @Column(name = "satisfaction_water")
    private String satisfactionWater;

    @Column(name = "satisfaction_food")
    private String satisfactionFood;

    @Column(name = "satisfaction_nutrition")
    private String satisfactionNutrition;

    @Column(name = "satisfaction_life")
    private String satisfactionLife;

    @Column(name = "satisfaction_information")
    private String satisfactionInformation;

    @Column(name = "satisfaction_leisure")
    private String satisfactionLeisure;

    @Column(name = "jgp_interventions")
    private String jgpInterventions;

    @Column(name = "technical_training")
    private String technicalTraining;

    @Column(name = "new_practices")
    private String newPractices;

    @Column(name = "improved_practices")
    private String improvedPractices;

    @Column(name = "training_improvements")
    private String trainingImprovements;

    @Column(name = "business_changes")
    private String businessChanges;

    @Column(name = "profitability_growth")
    private String profitabilityGrowth;

    @Column(name = "revenue_change")
    private BigDecimal revenueChange;

    @Column(name = "loan_application")
    private String loanApplication;

    @Column(name = "number_of_loans")
    private Integer numberOfLoans;

    @Column(name = "loan_platform")
    private String loanPlatform;

    @Column(name = "external_financing")
    private String externalFinancing;

    @Column(name = "financing_sources")
    private String financingSources;

    @Column(name = "jgp_impact")
    private String jgpImpact;

    @Column(name = "changes_without_jgp")
    private String changesWithoutJgp;

    @Column(name = "market_access")
    private String marketAccess;

    @Column(name = "business_opportunities")
    private String businessOpportunities;

    @Column(name = "market_challenges")
    private String marketChallenges;

    @Column(name = "is_approved")
    private boolean isDataApproved;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "approval_by_id")
    private AppUser approvalBy;

    @Column(name = "date_approved")
    private LocalDate dateApproved;

    private transient Integer rowIndex;

    public OutComeMonitoring() {
        // Default constructor
    }
    public OutComeMonitoring(OutComeMonitoringDto outComeMonitoringResponseDto, Participant participant, Integer rowIndex) {
        this.surveyDate = outComeMonitoringResponseDto.surveyDate();
        this.surveyLanguage = outComeMonitoringResponseDto.surveyLanguage();
        this.consented = outComeMonitoringResponseDto.consented();
        this.locationLatitude = outComeMonitoringResponseDto.locationLatitude();
        this.locationLongitude = outComeMonitoringResponseDto.locationLongitude();
        this.participant = participant;
        this.age = outComeMonitoringResponseDto.age();
        this.genderCategory = outComeMonitoringResponseDto.genderCategory();
        this.segment = outComeMonitoringResponseDto.segment();
        this.partner = outComeMonitoringResponseDto.partner();
        this.gender = outComeMonitoringResponseDto.gender();
        this.region = outComeMonitoringResponseDto.region();
        this.countyCode = outComeMonitoringResponseDto.countyCode();
        this.countyName = outComeMonitoringResponseDto.countyName();
        this.businessSetting = outComeMonitoringResponseDto.businessSetting();
        this.businessAgeCategory = outComeMonitoringResponseDto.businessAgeCategory();
        this.groupMembership = outComeMonitoringResponseDto.groupMembership();
        this.educationLevel = outComeMonitoringResponseDto.educationLevel();
        this.businessAge = outComeMonitoringResponseDto.businessAge();
        this.regularEmployees = outComeMonitoringResponseDto.regularEmployees();
        this.casualEmployees = outComeMonitoringResponseDto.casualEmployees();
        this.householdIncomeChange = outComeMonitoringResponseDto.householdIncomeChange();
        this.financialStability = outComeMonitoringResponseDto.financialStability();
        this.qualityOfLife = outComeMonitoringResponseDto.qualityOfLife();
        this.empowerment = outComeMonitoringResponseDto.empowerment();
        this.voiceInCommunity = outComeMonitoringResponseDto.voiceInCommunity();
        this.respectInCommunity = outComeMonitoringResponseDto.respectInCommunity();
        this.reliableIncome = outComeMonitoringResponseDto.reliableIncome();
        this.reputableWork = outComeMonitoringResponseDto.reputableWork();
        this.senseOfPurpose = outComeMonitoringResponseDto.senseOfPurpose();
        this.businessSectorGrowth = outComeMonitoringResponseDto.businessSectorGrowth();
        this.communityGrowth = outComeMonitoringResponseDto.communityGrowth();
        this.workOpportunities = outComeMonitoringResponseDto.workOpportunities();
        this.incomeRegularity = outComeMonitoringResponseDto.incomeRegularity();
        this.incomeSufficiency = outComeMonitoringResponseDto.incomeSufficiency();
        this.incomePredictability = outComeMonitoringResponseDto.incomePredictability();
        this.financialSecurity = outComeMonitoringResponseDto.financialSecurity();
        this.communityGroups = outComeMonitoringResponseDto.communityGroups() != null ? Arrays.stream(outComeMonitoringResponseDto.communityGroups().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.leadershipRole = outComeMonitoringResponseDto.leadershipRole();
        this.decisionMakingConfidence = outComeMonitoringResponseDto.decisionMakingConfidence();
        this.communityChange = outComeMonitoringResponseDto.communityChange();
        this.communityIssues = outComeMonitoringResponseDto.communityIssues();
        this.satisfactionEducation = outComeMonitoringResponseDto.satisfactionEducation();
        this.satisfactionRelationships = outComeMonitoringResponseDto.satisfactionRelationships();
        this.satisfactionBusinessType = outComeMonitoringResponseDto.satisfactionBusinessType();
        this.satisfactionIncome = outComeMonitoringResponseDto.satisfactionIncome();
        this.satisfactionHousing = outComeMonitoringResponseDto.satisfactionHousing();
        this.satisfactionHealthcare = outComeMonitoringResponseDto.satisfactionHealthcare();
        this.satisfactionWater = outComeMonitoringResponseDto.satisfactionWater();
        this.satisfactionFood = outComeMonitoringResponseDto.satisfactionFood();
        this.satisfactionNutrition = outComeMonitoringResponseDto.satisfactionNutrition();
        this.satisfactionLife = outComeMonitoringResponseDto.satisfactionLife();
        this.satisfactionInformation = outComeMonitoringResponseDto.satisfactionInformation();
        this.satisfactionLeisure = outComeMonitoringResponseDto.satisfactionLeisure();
        this.jgpInterventions = outComeMonitoringResponseDto.jgpInterventions() != null ? Arrays.stream(outComeMonitoringResponseDto.jgpInterventions().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.technicalTraining = outComeMonitoringResponseDto.technicalTraining() != null ? Arrays.stream(outComeMonitoringResponseDto.technicalTraining().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.newPractices = outComeMonitoringResponseDto.newPractices() != null ? Arrays.stream(outComeMonitoringResponseDto.newPractices().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.improvedPractices = outComeMonitoringResponseDto.improvedPractices() != null ? Arrays.stream(outComeMonitoringResponseDto.improvedPractices().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.trainingImprovements = outComeMonitoringResponseDto.trainingImprovements();
        this.businessChanges = outComeMonitoringResponseDto.businessChanges();
        this.profitabilityGrowth = outComeMonitoringResponseDto.profitabilityGrowth();
        this.revenueChange = outComeMonitoringResponseDto.revenueChange();
        this.loanApplication = outComeMonitoringResponseDto.loanApplication();
        this.numberOfLoans = outComeMonitoringResponseDto.numberOfLoans();
        this.loanPlatform = outComeMonitoringResponseDto.loanPlatform();
        this.externalFinancing = outComeMonitoringResponseDto.externalFinancing();
        this.financingSources = outComeMonitoringResponseDto.financingSources() != null ? Arrays.stream(outComeMonitoringResponseDto.financingSources().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.jgpImpact = outComeMonitoringResponseDto.jgpImpact();
        this.changesWithoutJgp = outComeMonitoringResponseDto.changesWithoutJgp();
        this.marketAccess = outComeMonitoringResponseDto.marketAccess() != null ? Arrays.stream(outComeMonitoringResponseDto.marketAccess().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.businessOpportunities = outComeMonitoringResponseDto.businessOpportunities();
        this.marketChallenges = outComeMonitoringResponseDto.marketChallenges();
        this.rowIndex = rowIndex;
    }

    public void approveData(Boolean approval, AppUser user){
        this.isDataApproved = approval;
        this.approvalBy = user;
        this.dateApproved = LocalDate.now(ZoneId.systemDefault());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        OutComeMonitoring outComeMonitoring = (OutComeMonitoring) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), outComeMonitoring.getId())
                .append(getParticipant(), outComeMonitoring.getParticipant())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getParticipant()).toHashCode();
    }
}