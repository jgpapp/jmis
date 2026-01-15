package com.jgp.finance.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public record LoanTransactionResponseDto(
        Long id,
        String tranch,
        String transactionType,
        LocalDate transactionDate,
        BigDecimal amount,
        String participantJGPID,
        String participantName,

        String businessName,
        String pipeLineSource,
        Integer loanDuration,
        String uploadedBy,

        String dateUploaded,

        String approvedBy,

        LocalDate dateApproved) {
}
