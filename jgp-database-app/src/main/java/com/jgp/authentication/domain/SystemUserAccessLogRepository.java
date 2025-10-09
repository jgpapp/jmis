package com.jgp.authentication.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.Optional;

@Repository
public interface SystemUserAccessLogRepository extends JpaRepository<SystemUserAccessLog, Long>, JpaSpecificationExecutor<SystemUserAccessLog>,
        QuerydslPredicateExecutor<SystemUserAccessLog> {

    Optional<SystemUserAccessLog> findByUsernameAndLoginDateAndLoginHourAndIsDeletedFalse(@NonNull String username, @NonNull LocalDate loginDate, @NonNull Integer loginHour);


}
