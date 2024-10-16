package com.jgp.authentication.domain.predicate;

import com.jgp.authentication.domain.QAppUser;
import com.jgp.authentication.dto.AppUserSearchCriteria;
import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Predicate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Predicate builder related to application users operations
 *
 * @author amy.muhimpundu
 */
@Component
public class AppUserPredicateBuilder {

    /**
     * Build predicate for search users
     *
     * @param searchCriteria searchCriteria
     * @return {@link BooleanBuilder}
     */
    public BooleanBuilder buildPredicateForSearchUsers(AppUserSearchCriteria searchCriteria) {

        QAppUser qAppUser = QAppUser.appUser;
        BooleanBuilder builder = new BooleanBuilder();

        List<Predicate> predicateList = new ArrayList<>();

        predicateList.add(qAppUser.isActive.eq(true));

        if (null != searchCriteria.userId()) {
            predicateList.add(qAppUser.id.eq(searchCriteria.userId()));
        }

        if (!CollectionUtils.isEmpty(searchCriteria.userIds())) {
            predicateList.add(qAppUser.id.in(searchCriteria.userIds()));
        }

        if (null != searchCriteria.partnerId()) {
            predicateList.add(qAppUser.partner.id.eq(searchCriteria.partnerId()));
        }

        if (!predicateList.isEmpty()) {
            builder.orAllOf(predicateList.toArray(new Predicate[0]));
        }

        return builder;
    }
}
