package com.jgp.finance.domain;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface LoanTransactionRepository extends JpaRepository<LoanTransaction, Long>, JpaSpecificationExecutor<LoanTransaction>, QuerydslPredicateExecutor<LoanTransaction> {

    @Query("select l from LoanTransaction l where l.loan.id = ?1 and l.isApproved = ?2 and l.isDeleted = false")
    Page<LoanTransaction> findTransactionsByLoan(@NonNull Long loanId, @NonNull boolean isApproved, Pageable pageable);


    @Query("select l from LoanTransaction l where l.loan.partner.id = ?1 and l.isApproved = ?2 and l.isDeleted = false")
    Page<LoanTransaction> getLoanTransactions(@NonNull Long id, @NonNull boolean isApproved, Pageable pageable);

    @Transactional
    @Modifying
    @Query(value = "update loan_transactions lt set is_deleted = true where lt.id in ?1", nativeQuery = true)
    void deleteLoanTransactionsByIds(@NonNull List<Long> ids);

    List<LoanTransaction> findByLoanDocumentIdAndIsDeleted(@NonNull Long loanImportDocId, @NonNull Boolean isDeleted);

    List<LoanTransaction> findByLoanDocumentId(@NonNull Long loanImportDocId);


}
