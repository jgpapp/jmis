package com.jgp.finance.service;

import com.jgp.finance.domain.Loan;
import com.jgp.finance.domain.LoanTransaction;
import com.jgp.finance.domain.LoanTransactionRepository;
import com.jgp.finance.dto.LoanTransactionResponseDto;
import com.jgp.finance.mapper.LoanTransactionMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class LoanTransactionServiceImpl implements LoanTransactionService {
    private final LoanTransactionRepository loanTransactionRepository;
    private final LoanTransactionMapper loanTransactionMapper;

    @Override
    public void createLoanTransaction(LoanTransaction loanTransaction) {
        this.loanTransactionRepository.save(loanTransaction);
    }

    @Override
    public Page<LoanTransactionResponseDto> getLoanTransactions(Loan loan, Pageable pageable) {
        final var transactions = this.loanTransactionRepository.findByLoan(loan, pageable);
        return new PageImpl<>(this.loanTransactionMapper.toDto(transactions.stream().toList()), pageable, transactions.getTotalElements());
    }
}
