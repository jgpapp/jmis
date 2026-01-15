package com.jgp.dashboard.domain;

import com.jgp.dashboard.dto.DataSummaryDto;
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

    @Column(name = "data_year")
    private Integer dataYear;

    @Column(name = "data_month")
    private Integer dataMonth;

    public DataSummary() {
    }

    private DataSummary(String genderCategory, Integer businessesTrained, Integer businessesLoaned, BigDecimal amountDisbursed, BigDecimal outStandingAmount, Integer dataYear, Integer dataMonth, Partner partner) {
        this.genderCategory = genderCategory;
        this.businessesTrained = businessesTrained;
        this.businessesLoaned = businessesLoaned;
        this.amountDisbursed = amountDisbursed;
        this.outStandingAmount = outStandingAmount;
        this.dataYear = dataYear;
        this.dataMonth = dataMonth;
        this.partner = partner;
    }

    public static DataSummary createDataSummary(DataSummaryDto dto, Partner partner){
        return new DataSummary(dto.genderCategory(), dto.businessesTrained(), dto.businessesLoaned(), dto.amountDisbursed(), dto.outStandingAmount(), dto.dataYear(), dto.dataMonth(), partner);
    }

    public void updateDataSummary(DataSummaryDto dto){
        this.businessesTrained = dto.businessesTrained();
        this.businessesLoaned = dto.businessesLoaned();
        this.amountDisbursed = dto.amountDisbursed();
        this.outStandingAmount = dto.outStandingAmount();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        DataSummary countySummary = (DataSummary) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), countySummary.getId())
                .append(getGenderCategory(), countySummary.getGenderCategory())
                .append(getDataYear(), countySummary.getDataYear())
                .append(getDataMonth(), countySummary.getDataMonth())
                .append(getPartner(), countySummary.getPartner())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getGenderCategory()).append(getDataYear()).append(getDataMonth()).append(getPartner()).toHashCode();
    }
}
