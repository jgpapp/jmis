package com.jgp.finance.service;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.dto.LoanDto;
import com.jgp.finance.dto.LoanSearchCriteria;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface LoanService {

    void createOrUpdateLoan(Loan loan);

    void approvedParticipantsLoansData(List<Long> dataIds, Boolean approval);

    Page<LoanDto> getLoans(LoanSearchCriteria searchCriteria, Pageable pageable);

    LoanDto findLoanById(Long loanId);
}
