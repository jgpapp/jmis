package com.jgp.bmo.domain;

import com.jgp.authentication.domain.AppUser;
import com.jgp.bmo.dto.MentorshipRequestDto;
import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.participant.domain.Participant;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import com.jgp.shared.domain.DataStatus;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
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

@Setter
@Getter
@Entity
@Table(name = "mentor_ships")
@SequenceGenerator(name = "mentor_ships_seq", sequenceName = "mentor_ships_seq", allocationSize = 50)
public class Mentorship extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "mentor_ships_seq")
    public Long getId() {
        return super.getId();
    }

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @Setter
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "participant_id")
    private Participant participant;

    @Column(name = "mentor_ship_date")
    private LocalDate mentorShipDate;

    @Column(name = "mentor_ship_org")
    private String mentorShipOrganization;

    @Column(name = "bmo_member_ship")
    private String bmoMemberShip;

    @Column(name = "mentor_ship_delivery_mode")
    private String mentorShipDeliveryMode;

    @Column(name = "business_situation")
    private String businessSituation;

    @Column(name = "new_hires_due_to_loan")
    private int newHiresBecauseOfLoan;

    @Column(name = "revenue_due_to_train")
    private BigDecimal revenueIncreaseDueToTraining;

    @Column(name = "useful_training_topics")
    private String usefulTrainingTopics;

    @Column(name = "support_needed_areas")
    private String supportNeededAreas;

    @Column(name = "msme_sessions_covered")
    private String msmeSessionsCovered;

    @Column(name = "sme_sessions_covered")
    private String smeSessionsCovered;

    @Column(name = "identified_business_gaps")
    private String identifiedBusinessGaps;

    @Column(name = "agreed_action_for_gap_one")
    private String agreedActionForGapOne;

    @Column(name = "additional_needed_support")
    private String additionalNeededSupport;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "approval_by_id")
    private AppUser approvalBy;

    @Column(name = "date_approved")
    private LocalDate dateApproved;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "upload_doc_id")
    private Document document;

    @Enumerated(EnumType.STRING)
    @Column(name = "data_status")
    private DataStatus dataStatus;

    private transient Integer rowIndex;

    public Mentorship() {
    }

    public Mentorship(MentorshipRequestDto dto) {
        this.rowIndex = dto.rowIndex();
        this.setCreatedBy(dto.createdBy());
        this.mentorShipDate = dto.mentorShipDate();
        this.mentorShipOrganization = dto.mentorShipOrganization();
        this.bmoMemberShip = dto.bmoMemberShip();
        this.mentorShipDeliveryMode = dto.mentorShipDeliveryMode();
        this.businessSituation = dto.businessSituation();
        this.newHiresBecauseOfLoan = dto.newHiresBecauseOfLoan();
        this.revenueIncreaseDueToTraining = dto.revenueIncreaseDueToTraining();
        this.usefulTrainingTopics = dto.usefulTrainingTopics();
        this.supportNeededAreas = dto.supportNeededAreas();
        this.msmeSessionsCovered = dto.msmeSessionsCovered();
        this.smeSessionsCovered = dto.smeSessionsCovered();
        this.identifiedBusinessGaps = dto.identifiedBusinessGaps();
        this.agreedActionForGapOne = dto.agreedActionForGapOne();
        this.additionalNeededSupport = dto.additionalNeededSupport();
        this.document = dto.document();
        this.dataStatus = DataStatus.PENDING_APPROVAL;
    }

    public void approveData(boolean approval, AppUser user){
        this.approvalBy = user;
        this.dateApproved = LocalDate.now(ZoneId.systemDefault());
        this.dataStatus = approval ? DataStatus.APPROVED : DataStatus.REJECTED;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Mentorship mentorship = (Mentorship) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), mentorship.getId())
                .append(getParticipant(), mentorship.getParticipant())
                .append(getPartner(), mentorship.getPartner())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getParticipant()).append(getPartner()).toHashCode();
    }
}
