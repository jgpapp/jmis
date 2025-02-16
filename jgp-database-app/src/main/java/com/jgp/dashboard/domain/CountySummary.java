package com.jgp.dashboard.domain;

import com.jgp.dashboard.dto.CountySummaryDto;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;

@Getter
@Entity
@Table(name = "county_summary", uniqueConstraints = @UniqueConstraint(columnNames = {"partner_id", "county_code", "data_year", "data_month"}))
public class CountySummary extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @Column(name = "county_code")
    private String countyCode;

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

    public CountySummary() {
    }

    private CountySummary(String countyCode, Integer businessesTrained, Integer businessesLoaned, BigDecimal amountDisbursed, BigDecimal outStandingAmount, Integer dataYear, Integer dataMonth, Partner partner) {
        this.countyCode = countyCode;
        this.businessesTrained = businessesTrained;
        this.businessesLoaned = businessesLoaned;
        this.amountDisbursed = amountDisbursed;
        this.outStandingAmount = outStandingAmount;
        this.dataYear = dataYear;
        this.dataMonth = dataMonth;
        this.partner = partner;
    }

    public static CountySummary createCountySummary(CountySummaryDto dto, Partner partner){
        return new CountySummary(dto.countyCode(), dto.businessesTrained(), dto.businessesLoaned(), dto.amountDisbursed(), dto.outStandingAmount(), dto.dataYear(), dto.dataMonth(), partner);
    }

    public void updateCountySummary(CountySummaryDto dto){
        this.businessesTrained = dto.businessesTrained();
        this.businessesLoaned = dto.businessesLoaned();
        this.amountDisbursed = dto.amountDisbursed();
        this.outStandingAmount = dto.outStandingAmount();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        CountySummary countySummary = (CountySummary) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), countySummary.getId())
                .append(getCountyCode(), countySummary.getCountyCode())
                .append(getDataYear(), countySummary.getDataYear())
                .append(getDataMonth(), countySummary.getDataMonth())
                .append(getPartner(), countySummary.getPartner())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getCountyCode()).append(getDataYear()).append(getDataMonth()).append(getPartner()).toHashCode();
    }
}
