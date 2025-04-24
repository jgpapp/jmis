package com.jgp.finance.domain;

import com.jgp.authentication.domain.AppUser;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.ZoneId;

@Getter
@Entity
@Table(name = "loan_transactions")
public class LoanTransaction extends BaseEntity implements Comparable<LoanTransaction> {

    @Setter
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "loan_id")
    private Loan loan;

    @Column(name = "transaction_type", nullable = false)
    @Enumerated(EnumType.STRING)
    private TransactionType transactionType;

    @Column(name = "tranch")
    private String tranchName;

    @Column(name = "transaction_date", nullable = false)
    private LocalDate transactionDate;

    @Column(name = "amount", scale = 4, precision = 19, nullable = false)
    private BigDecimal amount;

    @Column(name = "out_standing_amount", scale = 4, precision = 19, nullable = false)
    private BigDecimal outStandingAmount;

    @Column(name = "is_approved")
    private boolean isApproved;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "approval_by_id")
    private AppUser approvalBy;

    @Column(name = "date_approved")
    private LocalDate dateApproved;

    public LoanTransaction() {
    }

    public LoanTransaction(Loan loan, TransactionType transactionType, String tranchName, LocalDate transactionDate, BigDecimal amount, BigDecimal outStandingAmount, AppUser createdBy) {
        this.loan = loan;
        this.transactionType = transactionType;
        this.transactionDate = transactionDate;
        this.amount = amount;
        this.outStandingAmount = outStandingAmount;
        this.tranchName = tranchName;
        this.isApproved = false;
        this.setCreatedBy(createdBy);
    }

    public void approveData(Boolean approval, AppUser user){
        this.isApproved = approval;
        this.approvalBy = user;
        this.dateApproved = LocalDate.now(ZoneId.systemDefault());
    }

    @Override
    public int compareTo(@NotNull LoanTransaction loanTransaction) {
        return new CompareToBuilder() //
                .append(this.getId(), loanTransaction.getId()) //
                .append(this.transactionDate, loanTransaction.transactionDate) //
                .toComparison();
    }

    @Getter
    @RequiredArgsConstructor
    public enum TransactionType {
        DISBURSEMENT("Disbursement"), REPAYMENT("Repayment");
        private final String name;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        LoanTransaction loanTransaction = (LoanTransaction) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), loanTransaction.getId())
                .append(getLoan(), loanTransaction.getLoan())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getLoan()).toHashCode();
    }
}
