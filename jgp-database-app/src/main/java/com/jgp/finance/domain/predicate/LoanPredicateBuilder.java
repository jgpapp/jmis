package com.jgp.finance.domain.predicate;

import com.jgp.finance.domain.QLoan;
import com.jgp.finance.dto.LoanSearchCriteria;
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

        predicateList.add(qLoan.isDeleted.isFalse());

        if (null != searchCriteria.status()) {
            predicateList.add(qLoan.loanStatus.eq(searchCriteria.status()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qLoan.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != searchCriteria.participantId()) {
            predicateList.add(qLoan.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != searchCriteria.approvedByPartner()) {
            predicateList.add(qLoan.isDataApprovedByPartner.eq(searchCriteria.approvedByPartner()));
        }

        builder.orAllOf(predicateList.toArray(new Predicate[0]));

        return builder;
    }
}
