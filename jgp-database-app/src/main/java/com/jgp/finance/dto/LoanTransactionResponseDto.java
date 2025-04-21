package com.jgp.finance.dto;

import java.math.BigDecimal;
import java.time.LocalDate;

public record LoanTransactionResponseDto(
        String transactionType,
        LocalDate transactionDate,
        BigDecimal amount) {
}
