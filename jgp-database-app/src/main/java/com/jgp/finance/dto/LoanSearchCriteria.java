package com.jgp.finance.dto;

import com.jgp.finance.domain.Loan;
import lombok.Builder;

import java.time.LocalDate;

@Builder
public record LoanSearchCriteria(
        Long partnerId,
        Long participantId,
        Loan.LoanStatus status,
        Loan.LoanQuality quality,
        Boolean approvedByPartner,
        LocalDate disbursedFromDate,
        LocalDate disbursedToDate
) {
}
