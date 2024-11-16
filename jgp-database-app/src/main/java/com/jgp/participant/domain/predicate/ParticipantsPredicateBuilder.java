package com.jgp.participant.domain.predicate;

import com.jgp.participant.domain.QParticipant;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ParticipantsPredicateBuilder {

    /**
     * Build predicate for search participants
     *
     * @param searchText searchText
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchParticipants(String searchText) {

        QParticipant qParticipant = QParticipant.participant;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();
        if (null != searchText) {
            var businessNamePredicate = qParticipant.businessName.likeIgnoreCase("%"+searchText+"%s");
            var jgpPredicate = qParticipant.jgpId.likeIgnoreCase("%"+searchText+"%s");
            var phoneNumberPredicate = qParticipant.phoneNumber.likeIgnoreCase("%"+searchText+"%s");
            predicateList.add(businessNamePredicate.or(jgpPredicate).or(phoneNumberPredicate));
        }

        if (!predicateList.isEmpty()) {
            builder.orAllOf(predicateList.toArray(new Predicate[0]));
        }

        return builder;
    }
}
