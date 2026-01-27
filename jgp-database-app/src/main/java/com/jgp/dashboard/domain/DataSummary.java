package com.jgp.dashboard.domain;

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
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Entity
@Table(name = "data_summary", uniqueConstraints = @UniqueConstraint(columnNames = {"partner_id", "gender_category", "data_year", "data_month"}))
@SequenceGenerator(name = "data_summary_seq", sequenceName = "data_summary_seq", allocationSize = 50)
public class DataSummary extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "data_summary_seq")
    public Long getId() {
        return super.getId();
    }

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @Column(name = "gender_category")
    private String genderCategory;

    @Column(name = "businesses_trained")
    private Integer businessesTrained;

    @Column(name = "businesses_loaned")
    private Integer businessesLoaned;

    @Column(name = "amount_disbursed")
    private BigDecimal amountDisbursed;

    @Column(name = "out_standing_amount")
    private BigDecimal outStandingAmount;

    @Column(name = "amount_repaid")
    private BigDecimal amountRepaid;

    @Column(name = "summary_date")
    private LocalDate summaryDate;

    public DataSummary() {
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        DataSummary countySummary = (DataSummary) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), countySummary.getId())
                .append(getGenderCategory(), countySummary.getGenderCategory())
                .append(getSummaryDate(), countySummary.getSummaryDate())
                .append(getPartner(), countySummary.getPartner())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getGenderCategory()).append(getSummaryDate()).append(getPartner()).toHashCode();
    }
}
