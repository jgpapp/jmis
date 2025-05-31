package com.jgp.participant.domain;

import com.jgp.participant.dto.ParticipantDto;
import com.jgp.shared.domain.BaseEntity;
import com.jgp.util.CommonUtil;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.util.Objects;

@Getter
@Entity
@Table(name = "participants")
public class Participant extends BaseEntity {

    @Column(name = "participant_name")
    private String participantName;

    @Column(name = "business_name")
    private String businessName;

    @Column(name = "jgp_id")
    private String jgpId;

    @Column(name = "phone_number")
    private String phoneNumber;

    @Column(name = "owner_gender")
    @Enumerated(EnumType.STRING)
    private Gender ownerGender;

    @Column(name = "gender_category")
    private String genderCategory;

    @Column(name = "owner_age")
    private Integer ownerAge;

    @Column(name = "business_location")
    private String businessLocation;

    @Column(name = "location_county_code")
    private String locationCountyCode;

    @Column(name = "location_sub_county")
    private String locationSubCounty;

    @Column(name = "location_latitude")
    private BigDecimal locationLatitude;

    @Column(name = "location_longitude")
    private BigDecimal locationLongitude;

    @Column(name = "industry_sector")
    private String industrySector;

    @Column(name = "business_segment")
    private String businessSegment;

    @Column(name = "registration_number")
    private String registrationNumber;

    @Column(name = "best_monthly_revenue")
    private BigDecimal bestMonthlyRevenue;

    @Column(name = "worst_monthly_revenue")
    private BigDecimal worstMonthlyRevenue;

    @Column(name = "total_regular_employees")
    private Integer totalRegularEmployees;

    @Column(name = "youth_regular_employees")
    private Integer youthRegularEmployees;

    @Column(name = "total_casual_employees")
    private Integer totalCasualEmployees;

    @Column(name = "youth_casual_employees")
    private Integer youthCasualEmployees;

    @Column(name = "sample_records")
    private String sampleRecords;

    @Column(name = "person_with_disability")
    private String personWithDisability;

    @Column(name = "refugee_status")
    private String refugeeStatus;

    @Column(name = "is_active")
    private Boolean isActive;

    @Column(name = "is_eligible")
    private Boolean isEligible;

    @Column(name = "pre_payment", scale = 4, precision = 19, nullable = false)
    private BigDecimal prePayment;

    public Participant() {
    }

    public Participant(ParticipantDto dto) {
        this.businessName = dto.businessName();
        this.jgpId = dto.jgpId();
        this.phoneNumber = dto.phoneNumber();
        this.ownerGender = translateGender(dto.ownerGender());
        this.ownerAge = dto.ownerAge();
        this.businessLocation = dto.businessLocation();
        this.industrySector = dto.industrySector();
        this.businessSegment = dto.businessSegment();
        this.registrationNumber = dto.businessRegNumber();
        this.bestMonthlyRevenue = dto.bestMonthlyRevenue();
        this.worstMonthlyRevenue = dto.worstMonthlyRevenue();
        this.totalRegularEmployees = dto.totalRegularEmployees();
        this.youthRegularEmployees = dto.youthRegularEmployees();
        this.totalCasualEmployees = dto.totalCasualEmployees();
        this.youthCasualEmployees = dto.youthCasualEmployees();
        this.sampleRecords = (null == dto.sampleRecords() ? null : String.join(",", dto.sampleRecords()));
        this.personWithDisability = dto.personWithDisability();
        this.refugeeStatus = dto.refugeeStatus();
        this.isActive = Boolean.FALSE;
        this.genderCategory = GenderCategory.getGenderCategory(this.ownerGender, ownerAge).getName();
        this.locationCountyCode = dto.locationCountyCode();
        this.isEligible = dto.isEligible();
        this.participantName = dto.participantName();
    }

    public void updateParticipant(ParticipantDto dto){
        if (StringUtils.isNotBlank(dto.businessName())){
            this.businessName = dto.businessName().trim();
        }
        if (StringUtils.isNotBlank(dto.participantName())){
            this.participantName = dto.participantName().trim();
        }
        if (StringUtils.isNotBlank(dto.jgpId())){
            this.jgpId = dto.jgpId().trim();
        }
        if (StringUtils.isNotBlank(dto.phoneNumber())){
            this.phoneNumber = dto.phoneNumber().trim();
        }
        if (StringUtils.isNotBlank(dto.ownerGender())){
            this.ownerGender = translateGender(dto.ownerGender().trim());
        }
        if (Objects.nonNull(dto.ownerAge())){
            this.ownerAge = dto.ownerAge();
        }
        if (StringUtils.isNotBlank(dto.industrySector())){
            this.industrySector = dto.industrySector();
        }
        if (StringUtils.isNotBlank(dto.businessSegment())){
            this.businessSegment = dto.businessSegment();
        }
        if (Objects.nonNull(dto.bestMonthlyRevenue())){
            this.bestMonthlyRevenue = dto.bestMonthlyRevenue();
        }
        if (Objects.nonNull(dto.worstMonthlyRevenue())){
            this.worstMonthlyRevenue = dto.worstMonthlyRevenue();
        }
        if (Objects.nonNull(dto.totalRegularEmployees())){
            this.totalRegularEmployees = dto.totalRegularEmployees();
        }
        if (Objects.nonNull(dto.youthRegularEmployees())){
            this.youthRegularEmployees = dto.youthRegularEmployees();
        }
        if (Objects.nonNull(dto.totalCasualEmployees())){
            this.totalCasualEmployees = dto.totalCasualEmployees();
        }
        if (Objects.nonNull(dto.youthCasualEmployees())){
            this.youthCasualEmployees = dto.youthCasualEmployees();
        }
        if (Objects.nonNull(dto.sampleRecords())){
            this.sampleRecords = String.join(",", dto.sampleRecords());
        }
        if (StringUtils.isNotBlank(dto.personWithDisability())){
            this.personWithDisability = dto.personWithDisability();
        }
        if (StringUtils.isNotBlank(dto.refugeeStatus())){
            this.refugeeStatus = dto.refugeeStatus();
        }
        if (StringUtils.isNotBlank(dto.locationCountyCode())){
            var countyCode = dto.locationCountyCode().trim();
            if (countyCode.length() == 1){
                countyCode = String.format("00%s", countyCode);
            }
            if (countyCode.length() == 2){
                countyCode = String.format("0%s", countyCode);
            }
            final var county = CommonUtil.getCountyByCode(countyCode);
            if (null != county){
                this.locationCountyCode = countyCode;
                this.businessLocation = county;
            }

        }
        if (StringUtils.isNotBlank(dto.locationSubCounty())){
            this.locationSubCounty = dto.locationSubCounty();
        }
        if (Objects.nonNull(dto.locationLatitude())){
            this.locationLatitude = dto.locationLatitude();
        }
        if (Objects.nonNull(dto.locationLongitude())){
            this.locationLongitude = dto.locationLongitude();
        }
    }

    public void updateBusinessLocation(ParticipantDto dto){
        if (StringUtils.isNotBlank(dto.locationCountyCode())){
            var countyCode = dto.locationCountyCode().trim();
            if (countyCode.length() == 1){
                countyCode = String.format("00%s", countyCode);
            }
            if (countyCode.length() == 2){
                countyCode = String.format("0%s", countyCode);
            }
            final var county = CommonUtil.getCountyByCode(countyCode);
            if (null != county){
                this.locationCountyCode = countyCode;
                this.businessLocation = county;
            }

        }
        if (StringUtils.isNotBlank(dto.locationSubCounty())){
            this.locationSubCounty = dto.locationSubCounty();
        }
        if (Objects.nonNull(dto.locationLatitude())){
            this.locationLatitude = dto.locationLatitude();
        }
        if (Objects.nonNull(dto.locationLongitude())){
            this.locationLongitude = dto.locationLongitude();
        }
    }

    public void activateParticipant(){
        this.isActive = Boolean.TRUE;
    }

    public void updatePrePaidAmount(BigDecimal newAmount){
        this.prePayment = newAmount;
    }

    public void incrementPrePaidAmount(BigDecimal additionalAmount){
        this.prePayment = Objects.nonNull(this.prePayment) ? this.prePayment.add(additionalAmount) : additionalAmount;
    }

    private static Gender translateGender(String genderString){
        Participant.Gender genderEnum = null;
        try {
            genderEnum = StringUtils.isBlank(genderString) ? Participant.Gender.OTHER : Participant.Gender.valueOf(genderString.toUpperCase());
        }catch (Exception e){
            genderEnum = Participant.Gender.OTHER;
        }
        return genderEnum;
    }

    @Getter
    @RequiredArgsConstructor
    public enum Gender {

        MALE("Male"),
        FEMALE("Female"),
        INTERSEX("Intersex"),
        OTHER("Other");

        private final String name;
    }

    @Getter
    @RequiredArgsConstructor
    public enum GenderCategory {

        YOUNG_MALE("Young Men", 18, 35),
        YOUNG_FEMALE("Young Women", 18, 35),
        ADULT_MALE("Men", 36, 200),
        ADULT_FEMALE("Women", 36, 200),
        OTHER("Other", 0, 200);

        private final String name;
        private final Integer ageFrom;
        private final Integer ageTo;

        public static GenderCategory getGenderCategory(Gender gender, Integer age){
            if (Gender.MALE.equals(gender)){
                if (age > 35){
                    return ADULT_MALE;
                } else {
                    return YOUNG_MALE;
                }
            } else if (Gender.FEMALE.equals(gender)) {
                if (age > 35){
                    return ADULT_FEMALE;
                } else {
                    return YOUNG_FEMALE;
                }
            }else {
                return OTHER;
            }
        }

    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Participant client = (Participant) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), client.getId())
                .append(getJgpId(), client.getJgpId())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getJgpId()).toHashCode();
    }
}
