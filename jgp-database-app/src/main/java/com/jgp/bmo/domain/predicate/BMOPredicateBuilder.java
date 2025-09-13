package com.jgp.bmo.domain.predicate;

import com.jgp.bmo.domain.QBMOParticipantData;
import com.jgp.bmo.dto.BMOParticipantSearchCriteria;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class BMOPredicateBuilder {

    /**
     * Build predicate for search ta data
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchTAData(BMOParticipantSearchCriteria searchCriteria) {

        QBMOParticipantData qbmoParticipantData = QBMOParticipantData.bMOParticipantData;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        predicateList.add(qbmoParticipantData.isDeleted.isFalse());

        if (null != searchCriteria.participantId()) {
            predicateList.add(qbmoParticipantData.participant.id.eq(searchCriteria.participantId()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qbmoParticipantData.partner.id.eq(searchCriteria.partnerId()));
        }

        if (null != searchCriteria.approvedByPartner()) {
            predicateList.add(qbmoParticipantData.isDataApprovedByPartner.eq(searchCriteria.approvedByPartner()));
        }

        if (!predicateList.isEmpty()) {
            builder.orAllOf(predicateList.toArray(new Predicate[0]));
        }

        return builder;
    }
}
