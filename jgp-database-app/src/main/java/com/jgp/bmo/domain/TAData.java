package com.jgp.bmo.domain;


import com.jgp.authentication.domain.AppUser;
import com.jgp.bmo.dto.TARequestDto;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.domain.Participant;
import com.jgp.patner.domain.Partner;
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

import java.time.LocalDate;
import java.time.ZoneId;

@Setter
@Getter
@Entity
@Table(name = "ta_participants_data")
@SequenceGenerator(name = "ta_participants_data_seq", sequenceName = "ta_participants_data_seq", allocationSize = 50)
public class TAData extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ta_participants_data_seq")
    public Long getId() {
        return super.getId();
    }

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "participant_id")
    private Participant participant;

    @Column(name = "form_submitted_on")
    private LocalDate dateFormSubmitted;

    @Column(name = "is_applicant_eligible")
    private Boolean isApplicantEligible;

    @Column(name = "tas_attended")
    private Integer tasAttended;

    @Column(name = "ta_sessions_attended")
    private Integer taSessionsAttended;

    @Column(name = "recommended_for_finance")
    private Boolean isRecommendedForFinance;

    @Column(name = "decision_date")
    private LocalDate decisionDate;

    @Column(name = "fi_business_referred")
    private String fiBusinessReferred;

    @Column(name = "date_partner_recorded")
    private LocalDate dateRecordedByPartner;

    @Column(name = "date_recorded_to_jgp")
    private LocalDate dateRecordedToJGPDB;

    @Column(name = "data_is_approved")
    private Boolean isDataApprovedByPartner;

    @Column(name = "ta_needs")
    private String taNeeds;

    @Column(name = "training_partner")
    private String trainingPartner;

    @Column(name = "ta_delivery_mode")
    private String taDeliveryMode;

    @Column(name = "other_ta_needs")
    private String otherTaNeeds;

    @Column(name = "ta_type")
    private String taType;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "approval_by_id")
    private AppUser approvalBy;

    @Column(name = "date_approved")
    private LocalDate dateApproved;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "upload_doc_id")
    private Document document;

    private transient Integer rowIndex;

    private transient String rowErrorMessage;

    public TAData() {
    }

    public TAData(TARequestDto dto) {
        this.dateFormSubmitted = dto.dateFormSubmitted();
        this.isApplicantEligible = dto.isApplicantEligible();
        this.tasAttended = dto.tasAttended();
        this.taSessionsAttended = dto.taSessionsAttended();
        this.isRecommendedForFinance = dto.isRecommendedForFinance();
        this.decisionDate = dto.decisionDate();
        this.fiBusinessReferred = dto.fiBusinessReferred();
        this.dateRecordedByPartner = dto.dateRecordedByPartner();
        this.dateRecordedToJGPDB = dto.dateRecordedToJGPDB();
        this.isDataApprovedByPartner = false;
        this.taNeeds = dto.taNeeds();
        this.trainingPartner = dto.trainingPartner();
        this.taDeliveryMode = dto.taDeliveryMode();
        this.otherTaNeeds = dto.otherTaNeeds();
        this.taType = dto.taType();
        this.setCreatedBy(dto.createdBy());
        this.document = dto.document();
        this.rowIndex = dto.rowIndex();
        this.rowErrorMessage = dto.rowErrorMessage();
    }

    public void approveData(Boolean approval, AppUser user){
        this.isDataApprovedByPartner = approval;
        this.approvalBy = user;
        this.dateApproved = LocalDate.now(ZoneId.systemDefault());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        TAData bmoData = (TAData) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), bmoData.getId())
                .append(getPartner(), bmoData.getPartner())
                .append(getParticipant(), bmoData.getParticipant())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getPartner()).append(getParticipant()).toHashCode();
    }
}
