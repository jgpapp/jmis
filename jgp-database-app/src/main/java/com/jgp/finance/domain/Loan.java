package com.jgp.finance.domain;

import com.jgp.participant.domain.Participant;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Entity
@Table(name = "loans")
public class Loan extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "participant_id")
    private Participant participant;

    @Column(name = "loan_number")
    private String loanNumber;

    @Column(name = "pipeline_source")
    private String pipeLineSource;

    @Column(name = "loan_amount_applied")
    private BigDecimal loanAmountApplied;

    @Column(name = "loan_amount_approved")
    private BigDecimal loanAmountApproved;

    @Column(name = "loan_amount_accessed")
    private BigDecimal loanAmountAccessed;

    @Column(name = "loan_outstanding_amount")
    private BigDecimal loanOutStandingAmount;

    @Column(name = "loan_duration")
    private Integer loanDuration;

    @Column(name = "date_applied")
    private LocalDate dateApplied;

    @Column(name = "is_repeat_customer")
    private boolean isRepeatCustomer;

    @Column(name = "date_recorded_by_partner")
    private LocalDate dateRecordedByPartner;

    @Column(name = "date_added_to_db")
    private LocalDate dateAddedToDB;

    @Column(name = "date_disbursed")
    private LocalDate dateDisbursed;

    @Column(name = "loan_status")
    @Enumerated(EnumType.STRING)
    private LoanStatus loanStatus;

    @Column(name = "loan_quality")
    @Enumerated(EnumType.STRING)
    private LoanQuality loanQuality;

    @Column(name = "unique_values")
    private String uniqueValues;

    @Column(name = "data_is_approved")
    private boolean isDataApprovedByPartner;

    private transient Integer rowIndex;

    public Loan() {
    }

    public Loan(Partner partner, Participant participant, String loanNumber,
                String pipeLineSource, LoanQuality loanQuality,
                LoanStatus loanStatus, LocalDate dateApplied,
                LocalDate dateDisbursed, BigDecimal loanAmountAccessed,
                Integer loanDuration, BigDecimal loanOutStandingAmount,
                LocalDate dateRecordedByPartner, String uniqueValues,
                LocalDate dateAddedToDB, Integer rowIndex) {
        this.partner = partner;
        this.participant = participant;
        this.loanNumber = loanNumber;
        this.pipeLineSource = pipeLineSource;
        this.loanQuality = loanQuality;
        this.loanStatus = loanStatus;
        this.dateApplied = dateApplied;
        this.dateDisbursed = dateDisbursed;
        this.loanAmountAccessed = loanAmountAccessed;
        this.loanDuration = loanDuration;
        this.loanOutStandingAmount = loanOutStandingAmount;
        this.dateRecordedByPartner = dateRecordedByPartner;
        this.uniqueValues = uniqueValues;
        this.dateAddedToDB = dateAddedToDB;
        this.rowIndex = rowIndex;
        this.isDataApprovedByPartner = false;
    }

    public void approveData(Boolean approval){
        this.isDataApprovedByPartner = approval;
    }

    @Getter
    public enum LoanStatus {

        NEW("Pending Approval"),
        APPROVED("Approved"),
        REJECTED("Rejected");

        private final String name;

        LoanStatus(String name) {
            this.name = name;
        }

    }

    @Getter
    public enum LoanQuality {

        NORMAL("Normal"),
        WATCH("Watch"),
        SUBSTANDARD("Substandard"),
        DOUBTFUL("Doubtful"),
        LOSS("Loss");

        private final String name;

        LoanQuality(String name) {
            this.name = name;
        }

    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Loan loan = (Loan) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), loan.getId())
                .append(getParticipant(), loan.getParticipant())
                .append(getLoanNumber(), loan.getLoanNumber())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getParticipant()).append(getLoanNumber()).toHashCode();
    }

}
