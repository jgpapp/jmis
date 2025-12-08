package com.jgp.finance.service;

import com.jgp.finance.domain.LoanTransaction;

import java.util.List;

public interface LoanTransactionService {

    List<LoanTransaction> getLoanTransactions(Long loanImportDocId);
}
