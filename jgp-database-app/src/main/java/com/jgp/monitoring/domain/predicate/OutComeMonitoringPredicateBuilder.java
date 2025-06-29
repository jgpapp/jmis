package com.jgp.monitoring.domain.predicate;

import com.jgp.monitoring.domain.QOutComeMonitoring;
import com.jgp.util.CommonUtil;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class OutComeMonitoringPredicateBuilder {

    /**
     * Build predicate for search users
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchOutComeMonitorings(OutComeMonitoringSearchCriteria searchCriteria) {

        QOutComeMonitoring qOutComeMonitoring = QOutComeMonitoring.outComeMonitoring;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        if (null != searchCriteria.fromDate() && null != searchCriteria.toDate()) {
            predicateList.add(qOutComeMonitoring.surveyDate.between(searchCriteria.fromDate(), searchCriteria.toDate()));
        }

        if (null != searchCriteria.participantAgeGroup()) {
            final var ageRange = CommonUtil.getAgeRangeFromAgeGroup(searchCriteria.participantAgeGroup());
            predicateList.add(qOutComeMonitoring.age.between(ageRange.getLeft(), ageRange.getRight()));
        }

        if (null != searchCriteria.gender()) {
            predicateList.add(qOutComeMonitoring.gender.eq(searchCriteria.gender()));
        }

        if (null != searchCriteria.jgpIntervention()) {
            predicateList.add(qOutComeMonitoring.jgpInterventions.contains(searchCriteria.jgpIntervention()));
        }

        if (null != searchCriteria.countyCode()) {
            predicateList.add(qOutComeMonitoring.countyCode.eq(searchCriteria.countyCode()));
        }
        if (null != searchCriteria.region()) {
            predicateList.add(qOutComeMonitoring.region.eq(searchCriteria.region()));
        }
        if (null != searchCriteria.approved()) {
            predicateList.add(qOutComeMonitoring.isDataApproved.eq(searchCriteria.approved()));
        }

        if (!predicateList.isEmpty()) {
            builder.orAllOf(predicateList.toArray(new Predicate[0]));
        }

        return builder;
    }
}
