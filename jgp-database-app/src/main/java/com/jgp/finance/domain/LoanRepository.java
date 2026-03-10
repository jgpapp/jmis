package com.jgp.finance.domain;

import org.jspecify.annotations.NonNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
public interface LoanRepository extends JpaRepository<Loan, Long> , JpaSpecificationExecutor<Loan>, QuerydslPredicateExecutor<Loan> {

    @Query("select l from Loan l where l.participant.id = ?1 and l.loanNumber = ?2 and l.dataStatus = 'APPROVED'")
    Optional<Loan> findByParticipantAndLoanNumber(Long participantId, String loanNumber);


    @Transactional
    @Modifying
    @Query(value = """
            delete from loans l where l.id in ?1 and \
            not exists (select 1 from loan_transactions lt where lt.loan_id = l.id) \
            """, nativeQuery = true)
    void deleteLoansByIds(@NonNull List<Long> loanIds);


}
