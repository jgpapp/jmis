package com.jgp.bmo.domain.predicate;

import com.jgp.bmo.domain.QMentorship;
import com.jgp.bmo.dto.MentorshipSearchCriteria;
import com.jgp.shared.domain.DataStatus;
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

        if (null != searchCriteria.participantId()) {
            predicateList.add(qMentorship.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qMentorship.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != DataStatus.getDataStatus(searchCriteria.dataStatus())) {
            predicateList.add(qMentorship.dataStatus.eq(DataStatus.getDataStatus(searchCriteria.dataStatus())));
        }else {
            predicateList.add(qMentorship.dataStatus.eq(DataStatus.APPROVED));
        }

        builder.orAllOf(predicateList.toArray(new Predicate[0]));

        return builder;
    }
}
