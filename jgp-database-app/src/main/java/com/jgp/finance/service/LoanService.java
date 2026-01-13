package com.jgp.finance.service;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.dto.LoanResponseDto;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.finance.dto.LoanTransactionResponseDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface LoanService {

    void createOrUpdateLoan(Loan loan);

    void approvedTransactionsLoans(List<Long> transactionsIds, Boolean approval);

    Page<LoanResponseDto> getLoans(LoanSearchCriteria searchCriteria, Pageable pageable);

    LoanResponseDto findLoanById(Long loanId);

    Page<LoanTransactionResponseDto> getAvailableLoanTransactions(Long partnerId, Long loanId, Boolean isApproved, Pageable pageable);
}
