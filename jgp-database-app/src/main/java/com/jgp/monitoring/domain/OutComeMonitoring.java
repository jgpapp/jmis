package com.jgp.monitoring.domain;

import com.jgp.authentication.domain.AppUser;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.monitoring.dto.OutComeMonitoringRequestDto;
import com.jgp.participant.domain.Participant;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.SequenceGenerator;
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
@SequenceGenerator(name = "outcome_monitoring_seq", sequenceName = "outcome_monitoring_seq", allocationSize = 50)
public class OutComeMonitoring extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "outcome_monitoring_seq")
    public Long getId() {
        return super.getId();
    }

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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "upload_doc_id")
    private Document document;

    private transient Integer rowIndex;

    public OutComeMonitoring() {
        // Default constructor
    }
    public OutComeMonitoring(OutComeMonitoringRequestDto dto) {
        this.surveyDate = dto.surveyDate();
        this.surveyLanguage = dto.surveyLanguage();
        this.consented = "Yes".equalsIgnoreCase(dto.consented());
        this.locationLatitude = dto.locationLatitude();
        this.locationLongitude = dto.locationLongitude();
        this.age = dto.age();
        this.genderCategory = dto.genderCategory();
        this.segment = dto.segment();
        this.partner = dto.partner();
        this.gender = dto.gender();
        this.region = dto.region();
        this.countyCode = dto.countyCode();
        this.countyName = dto.countyName();
        this.businessSetting = dto.businessSetting();
        this.businessAgeCategory = dto.businessAgeCategory();
        this.groupMembership = dto.groupMembership();
        this.educationLevel = dto.educationLevel();
        this.businessAge = dto.businessAge();
        this.regularEmployees = dto.regularEmployees();
        this.casualEmployees = dto.casualEmployees();
        this.householdIncomeChange = dto.householdIncomeChange();
        this.financialStability = dto.financialStability();
        this.qualityOfLife = dto.qualityOfLife();
        this.empowerment = dto.empowerment();
        this.voiceInCommunity = dto.voiceInCommunity();
        this.respectInCommunity = dto.respectInCommunity();
        this.reliableIncome = dto.reliableIncome();
        this.reputableWork = dto.reputableWork();
        this.senseOfPurpose = dto.senseOfPurpose();
        this.businessSectorGrowth = dto.businessSectorGrowth();
        this.communityGrowth = dto.communityGrowth();
        this.workOpportunities = dto.workOpportunities();
        this.incomeRegularity = dto.incomeRegularity();
        this.incomeSufficiency = dto.incomeSufficiency();
        this.incomePredictability = dto.incomePredictability();
        this.financialSecurity = dto.financialSecurity();
        this.communityGroups = dto.communityGroups() != null ? Arrays.stream(dto.communityGroups().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.leadershipRole = dto.leadershipRole();
        this.decisionMakingConfidence = dto.decisionMakingConfidence();
        this.communityChange = dto.communityChange();
        this.communityIssues = dto.communityIssues();
        this.satisfactionEducation = dto.satisfactionEducation();
        this.satisfactionRelationships = dto.satisfactionRelationships();
        this.satisfactionBusinessType = dto.satisfactionBusinessType();
        this.satisfactionIncome = dto.satisfactionIncome();
        this.satisfactionHousing = dto.satisfactionHousing();
        this.satisfactionHealthcare = dto.satisfactionHealthcare();
        this.satisfactionWater = dto.satisfactionWater();
        this.satisfactionFood = dto.satisfactionFood();
        this.satisfactionNutrition = dto.satisfactionNutrition();
        this.satisfactionLife = dto.satisfactionLife();
        this.satisfactionInformation = dto.satisfactionInformation();
        this.satisfactionLeisure = dto.satisfactionLeisure();
        this.jgpInterventions = dto.jgpInterventions() != null ? Arrays.stream(dto.jgpInterventions().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.technicalTraining = dto.technicalTraining() != null ? Arrays.stream(dto.technicalTraining().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.newPractices = dto.newPractices() != null ? Arrays.stream(dto.newPractices().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.improvedPractices = dto.improvedPractices() != null ? Arrays.stream(dto.improvedPractices().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.trainingImprovements = dto.trainingImprovements();
        this.businessChanges = dto.businessChanges();
        this.profitabilityGrowth = dto.profitabilityGrowth();
        this.revenueChange = dto.revenueChange();
        this.loanApplication = dto.loanApplication();
        this.numberOfLoans = dto.numberOfLoans();
        this.loanPlatform = dto.loanPlatform();
        this.externalFinancing = dto.externalFinancing();
        this.financingSources = dto.financingSources() != null ? Arrays.stream(dto.financingSources().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.jgpImpact = dto.jgpImpact();
        this.changesWithoutJgp = dto.changesWithoutJgp();
        this.marketAccess = dto.marketAccess() != null ? Arrays.stream(dto.marketAccess().split(",")).map(String::trim).collect(Collectors.joining(",")) : null;
        this.businessOpportunities = dto.businessOpportunities();
        this.marketChallenges = dto.marketChallenges();
        this.rowIndex = dto.rowIndex();
        this.document = dto.document();
        this.setCreatedBy(dto.createdBy());
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