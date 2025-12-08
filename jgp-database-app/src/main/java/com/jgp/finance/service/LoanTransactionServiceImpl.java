package com.jgp.finance.service;

import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.domain.LoanTransactionRepository;
import com.jgp.finance.mapper.LoanTransactionMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class LoanTransactionServiceImpl implements LoanTransactionService {
    private final LoanTransactionRepository loanTransactionRepository;
    private final LoanTransactionMapper loanTransactionMapper;

    @Override
    public List<LoanTransaction> getLoanTransactions(Long loanImportDocId) {
        return loanTransactionRepository.findByLoanDocumentId(loanImportDocId);
    }
}
