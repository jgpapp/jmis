package com.jgp.finance.service;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.dto.LoanDto;
import com.jgp.finance.dto.LoanSearchCriteria;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface LoanService {

    void createLoans(List<Loan> loans);

    void approvedParticipantsLoansData(List<Long> dataIds, Boolean approval);

    List<LoanDto> getLoans(LoanSearchCriteria searchCriteria, Pageable pageable);

    LoanDto findLoanById(Long loanId);
}
