package com.jgp.bmo.domain.predicate;

import com.jgp.bmo.domain.QMentorship;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class MentorshipPredicateBuilder {

    /**
     * Build predicate for search ta data
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchMentorshipData(MentorshipSearchCriteria searchCriteria) {

        QMentorship qMentorship = QMentorship.mentorship;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        predicateList.add(qMentorship.isDeleted.isFalse());

        if (null != searchCriteria.participantId()) {
            predicateList.add(qMentorship.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qMentorship.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != searchCriteria.approvedByPartner()) {
            predicateList.add(qMentorship.isDataApproved.eq(searchCriteria.approvedByPartner()));
        }

        if (!predicateList.isEmpty()) {
            builder.orAllOf(predicateList.toArray(new Predicate[0]));
        }

        return builder;
    }
}
