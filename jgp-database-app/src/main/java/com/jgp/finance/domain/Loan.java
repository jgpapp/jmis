package com.jgp.finance.domain;

import com.jgp.authentication.domain.AppUser;
import com.jgp.participant.domain.Participant;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Getter
@Entity
@Table(name = "loans")
public class Loan extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @Setter
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "participant_id")
    private Participant participant;

    @Column(name = "loan_number")
    private String loanNumber;

    @NotBlank(message = "Pipeline source is required !!")
    @Column(name = "pipeline_source")
    private String pipeLineSource;

    @NotNull(message = "Loan Amount is required !!")
    @Min(value = 0, message = "Loan Amount is required !!")
    @Column(name = "loan_amount")
    private BigDecimal loanAmount;

    @NotNull(message = "Outstanding Loan Amount is required !!")
    @Column(name = "loan_outstanding_amount")
    private BigDecimal loanOutStandingAmount;

    @Column(name = "loan_amount_repaid")
    private BigDecimal loanAmountRepaid;

    @Column(name = "loaner_type")
    private String loanerType;

    @Column(name = "loan_product")
    private String loanProduct;

    @NotNull(message = "Loan duration is required !!")
    @Column(name = "loan_duration")
    private Integer loanDuration;

    @NotNull(message = "Application Date is required !!")
    @Column(name = "date_applied")
    private LocalDate dateApplied;

    @Column(name = "date_recorded_by_partner")
    private LocalDate dateRecordedByPartner;

    @NotNull(message = "Date of record added to JGP database is required !!")
    @Column(name = "date_added_to_db")
    private LocalDate dateAddedToDB;

    @NotNull(message = "Disbursement Date is required !!")
    @Column(name = "date_disbursed")
    private LocalDate dateDisbursed;

    @Column(name = "loan_status")
    @Enumerated(EnumType.STRING)
    private LoanStatus loanStatus;

    @NotNull(message = "Loan Quality is required !!")
    @Column(name = "loan_quality")
    @Enumerated(EnumType.STRING)
    private LoanQuality loanQuality;

    @Column(name = "unique_values")
    private String uniqueValues;

    @Column(name = "data_is_approved")
    private boolean isDataApprovedByPartner;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "approval_by_id")
    private AppUser approvalBy;

    @Column(name = "date_approved")
    private LocalDate dateApproved;

    @OneToMany(cascade = CascadeType.ALL, mappedBy = "loan", orphanRemoval = true, fetch = FetchType.LAZY)
    private Set<LoanTransaction> loanTransactions = new HashSet<>();

    private transient Integer rowIndex;

    public Loan() {
    }

    public Loan(Partner partner, Participant participant, String loanNumber,
                String pipeLineSource, LoanQuality loanQuality,
                LoanStatus loanStatus, LocalDate dateApplied,
                LocalDate dateDisbursed, BigDecimal loanAmount,
                Integer loanDuration, BigDecimal loanOutStandingAmount,
                LocalDate dateRecordedByPartner, String uniqueValues,
                LocalDate dateAddedToDB, BigDecimal loanAmountRepaid,
                String loanerType, String loanProduct, AppUser createdBy, Integer rowIndex) {
        this.partner = partner;
        this.participant = participant;
        this.loanNumber = loanNumber;
        this.pipeLineSource = pipeLineSource;
        this.loanQuality = loanQuality;
        this.loanStatus = loanStatus;
        this.dateApplied = dateApplied;
        this.dateDisbursed = dateDisbursed;
        this.loanAmount = loanAmount;
        this.loanDuration = loanDuration;
        this.loanOutStandingAmount = loanOutStandingAmount;
        this.dateRecordedByPartner = dateRecordedByPartner;
        this.uniqueValues = uniqueValues;
        this.dateAddedToDB = dateAddedToDB;
        this.rowIndex = rowIndex;
        this.loanAmountRepaid = loanAmountRepaid;
        this.loanerType = loanerType;
        this.loanProduct = loanProduct;
        this.isDataApprovedByPartner = false;
        this.setCreatedBy(createdBy);
    }

    public void addLoanTransaction(LoanTransaction loanTransaction){
        if (Objects.nonNull(loanTransaction)) {
            loanTransaction.setLoan(this);
            this.loanTransactions.add(loanTransaction);
        }
    }

    public void removeLoanTransaction(final LoanTransaction loanTransaction){
        if (Objects.nonNull(loanTransaction)) {
            this.loanTransactions.removeIf(lt -> lt.getId().equals(loanTransaction.getId()));
        }
    }

    public void approveData(Boolean approval, AppUser user){
        this.isDataApprovedByPartner = approval;
        this.approvalBy = user;
        this.dateApproved = LocalDate.now(ZoneId.systemDefault());
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
