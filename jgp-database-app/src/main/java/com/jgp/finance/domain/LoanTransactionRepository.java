package com.jgp.finance.domain;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

@Repository
public interface LoanTransactionRepository extends JpaRepository<LoanTransaction, Long>, JpaSpecificationExecutor<LoanTransaction>, QuerydslPredicateExecutor<LoanTransaction> {

    @Query("select l from LoanTransaction l where l.loan.id = ?1 and l.isApproved = ?2")
    Page<LoanTransaction> findTransactionsByLoan(@NonNull Long loanId, @NonNull boolean isApproved, Pageable pageable);

    Page<LoanTransaction> findByLoan_IdAndIsApproved(@NonNull Long id, @NonNull boolean isApproved, Pageable pageable);


    @Query("select l from LoanTransaction l where l.loan.partner.id = ?1 and l.isApproved = ?2")
    Page<LoanTransaction> getLoanTransactions(@NonNull Long id, @NonNull boolean isApproved, Pageable pageable);


}
