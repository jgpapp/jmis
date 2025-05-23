package com.jgp.finance.domain;

import com.jgp.participant.domain.Participant;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Optional;

@Repository
public interface LoanRepository extends JpaRepository<Loan, Long> , JpaSpecificationExecutor<Loan>, QuerydslPredicateExecutor<Loan> {

    Optional<Loan> findByParticipantAndLoanNumber(@NonNull Participant participant, @NonNull String loanNumber);

    @Transactional
    @Modifying
    @Query(value = """
            delete from loans l where \s
            not exists (select 1 from loan_transactions lt where lt.loan_id = l.id) \s
            and l.id in ?1""", nativeQuery = true)
    void deleteLoansByIds(@NonNull List<Long> ids);


}
