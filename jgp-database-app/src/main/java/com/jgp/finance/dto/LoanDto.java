package com.jgp.finance.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public record LoanDto(
         Long id,

         String participantJGPID,

         String participantName,

         Long partnerId,

         String partnerName,

         String loanNumber,

         String pipeLineSource,

         BigDecimal loanAmountApplied,

         BigDecimal loanAmountApproved,

         BigDecimal loanAmountAccessed,

         BigDecimal loanOutStandingAmount,

         Integer loanDuration,

         LocalDate dateApplied,

         LocalDate dateRecordedByPartner,

         LocalDate dateAddedToDB,

         LocalDate dateDisbursed,

         String loanStatus,

         String loanQuality
) {
}
