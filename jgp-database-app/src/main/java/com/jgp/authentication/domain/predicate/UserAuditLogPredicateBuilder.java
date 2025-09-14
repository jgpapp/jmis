package com.jgp.authentication.domain.predicate;

import com.jgp.authentication.domain.QUserAuditLog;
import com.jgp.authentication.dto.UserAuditLogSearchCriteria;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * Predicate builder related to application users logs
 *
 * @author simiyu
 */
@Component
public class UserAuditLogPredicateBuilder {

    /**
     * Build predicate for search users
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchUserLogs(UserAuditLogSearchCriteria searchCriteria) {

        QUserAuditLog qUserAuditLog = QUserAuditLog.userAuditLog;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();
        predicateList.add(qUserAuditLog.isDeleted.isFalse());

        if (null != searchCriteria.userName()) {
            predicateList.add(qUserAuditLog.username.eq(searchCriteria.userName()));
        }

        if (null != searchCriteria.action()) {
            predicateList.add(qUserAuditLog.action.eq(searchCriteria.action()));
        }

        if (null != searchCriteria.logTimeFrom() && null != searchCriteria.logTimeTo()) {
            predicateList.add(qUserAuditLog.logTime.between(searchCriteria.logTimeFrom(), searchCriteria.logTimeTo()));
        } else if (null != searchCriteria.logTimeFrom()) {
            predicateList.add(qUserAuditLog.logTime.goe(searchCriteria.logTimeFrom()));
        } else if (null != searchCriteria.logTimeTo()) {
            predicateList.add(qUserAuditLog.logTime.loe(searchCriteria.logTimeTo()));
        }

        builder.orAllOf(predicateList.toArray(new Predicate[0]));

        return builder;
    }
}
