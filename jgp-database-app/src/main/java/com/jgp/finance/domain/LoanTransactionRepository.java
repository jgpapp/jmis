package com.jgp.finance.domain;

import com.jgp.shared.domain.DataStatus;
import org.jspecify.annotations.NonNull;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface LoanTransactionRepository extends JpaRepository<LoanTransaction, Long>, JpaSpecificationExecutor<LoanTransaction>, QuerydslPredicateExecutor<LoanTransaction> {


    @Query("select l from LoanTransaction l where l.loan.partner.id = ?1 and l.dataStatus = ?2")
    Page<LoanTransaction> getLoanTransactions(@NonNull Long partnerId, DataStatus dataStatus, Pageable pageable);

    @Transactional
    @Modifying
    @Query(value = "delete from loan_transactions lt where lt.id in ?1", nativeQuery = true)
    void deleteLoanTransactionsByIds(@NonNull List<Long> transactionIds);

    List<LoanTransaction> findByLoanDocumentId(@NonNull Long loanImportDocId);
}
