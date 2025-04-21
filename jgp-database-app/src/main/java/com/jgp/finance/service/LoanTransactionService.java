package com.jgp.finance.service;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.dto.LoanTransactionResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface LoanTransactionService {

    void createLoanTransaction(LoanTransaction loanTransaction);

    Page<LoanTransactionResponseDto> getLoanTransactions(Loan loan, Pageable pageable);
}
