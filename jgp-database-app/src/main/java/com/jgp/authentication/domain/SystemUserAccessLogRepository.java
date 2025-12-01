package com.jgp.authentication.domain;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.querydsl.QuerydslPredicateExecutor;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalTime;

@Repository
public interface SystemUserAccessLogRepository extends JpaRepository<SystemUserAccessLog, Long>, JpaSpecificationExecutor<SystemUserAccessLog>,
        QuerydslPredicateExecutor<SystemUserAccessLog> {

    @Modifying
    @Transactional
    @Query(value = """
            INSERT INTO system_user_access_logs (username, ip_address, login_time, login_date, login_hour) 
            VALUES (?1, ?2, ?3, ?4, ?5) 
            ON CONFLICT (username, login_date, login_hour) DO UPDATE set last_modified = current_timestamp 
            """, nativeQuery = true)
    void saveUserLogin(String username, String ipAddress, LocalTime loginTime, LocalDate loginDate, Integer loginHour);


}
