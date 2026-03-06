package com.jgp.bmo.domain.predicate;

import com.jgp.bmo.domain.QTAData;
import com.jgp.bmo.dto.TAParticipantSearchCriteria;
import com.jgp.shared.domain.DataStatus;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class TAPredicateBuilder {

    /**
     * Build predicate for search ta data
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchTAData(TAParticipantSearchCriteria searchCriteria) {

        QTAData qbmoParticipantData = QTAData.tAData;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        if (null != searchCriteria.participantId()) {
            predicateList.add(qbmoParticipantData.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qbmoParticipantData.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != DataStatus.getDataStatus(searchCriteria.dataStatus())) {
            predicateList.add(qbmoParticipantData.dataStatus.eq(DataStatus.getDataStatus(searchCriteria.dataStatus())));
        }else {
            predicateList.add(qbmoParticipantData.dataStatus.eq(DataStatus.APPROVED));
        }

        builder.orAllOf(predicateList.toArray(new Predicate[0]));

        return builder;
    }
}
