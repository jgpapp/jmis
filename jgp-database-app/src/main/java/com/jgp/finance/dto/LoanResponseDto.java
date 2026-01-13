package com.jgp.finance.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDate;

public record LoanResponseDto(
         Long id,

         String participantJGPID,

         String participantName,

         String businessName,

         Long partnerId,

         String partnerName,

         String loanNumber,

         String pipeLineSource,

         BigDecimal loanAmountApplied,

         BigDecimal loanAmountApproved,

         BigDecimal loanAmountAccessed,

         Integer loanDuration,

         LocalDate dateApplied,

         LocalDate dateRecordedByPartner,

         LocalDate dateAddedToDB,

         LocalDate dateDisbursed,

         String loanStatus,

         String loanQuality,

         String uploadedBy,

         LocalDate dateUploaded,

         String approvedBy,

         LocalDate dateApproved
) implements Serializable {
}
