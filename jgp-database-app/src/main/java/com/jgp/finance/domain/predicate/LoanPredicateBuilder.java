package com.jgp.finance.domain.predicate;

import com.jgp.finance.domain.QLoan;
import com.jgp.finance.dto.LoanSearchCriteria;
import com.jgp.shared.domain.DataStatus;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class LoanPredicateBuilder {

    /**
     * Build predicate for search users
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchLoans(LoanSearchCriteria searchCriteria) {

        QLoan qLoan = QLoan.loan;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        if (null != searchCriteria.status()) {
            predicateList.add(qLoan.loanStatus.eq(searchCriteria.status()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qLoan.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != searchCriteria.participantId()) {
            predicateList.add(qLoan.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != DataStatus.getDataStatus(searchCriteria.dataStatus())) {
            predicateList.add(qLoan.dataStatus.eq(DataStatus.getDataStatus(searchCriteria.dataStatus())));
        }else {
            predicateList.add(qLoan.dataStatus.eq(DataStatus.APPROVED));
        }

        builder.orAllOf(predicateList.toArray(new Predicate[0]));

        return builder;
    }
}
