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
        String dataStatus,
        LocalDate disbursedFromDate,
        LocalDate disbursedToDate
) {
}
