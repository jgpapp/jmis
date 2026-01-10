package com.jgp.finance.domain;

import com.jgp.participant.domain.Participant;
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

    Optional<Loan> findByParticipantAndLoanNumber(Participant participant, String loanNumber);

    @Transactional
    @Modifying
    @Query(value = """
            update loans l set is_deleted = true, loan_number = CONCAT(l.loan_number, '_', l.id, '_', 'DELETED') where l.id in ?1 and\s
            not exists (select 1 from loan_transactions lt where lt.loan_id = l.id and lt.is_deleted = false) \s
            """, nativeQuery = true)
    void deleteLoansByIds(@NonNull List<Long> loanIds);


}
