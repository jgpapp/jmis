package com.jgp.authentication.domain;

import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;

@Getter
@Entity
@Table(name = "system_user_access_logs", uniqueConstraints = {
        @UniqueConstraint(name = "uk_username_login_date_login_hour", columnNames = {"username", "login_date", "login_hour"})
})
@SequenceGenerator(name = "system_user_access_logs_seq", sequenceName = "system_user_access_logs_seq", allocationSize = 1)
public class SystemUserAccessLog extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "system_user_access_logs_seq")
    public Long getId() {
        return super.getId();
    }

    @Column(name = "username", nullable = false)
    private String username;
    @Column(name = "ip_address")
    private String ipAddress;
    @Column(name = "login_time", nullable = false)
    private LocalTime loginTime;
    @Column(name = "login_date", nullable = false)
    private LocalDate loginDate;
    @Column(name = "login_hour", nullable = false)
    private Integer loginHour;

    public SystemUserAccessLog() {
        // Default constructor for JPA
    }

    public SystemUserAccessLog(String username, String ipAddress) {
        this.username = username;
        this.ipAddress = ipAddress;
        this.loginTime = LocalTime.now(ZoneId.systemDefault());
        this.loginDate = LocalDate.now(ZoneId.systemDefault());
        this.loginHour = this.loginTime.getHour();
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        SystemUserAccessLog auditLog = (SystemUserAccessLog) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), auditLog.getId())
                .append(getUsername(), auditLog.getUsername())
                .append(getLoginDate(), auditLog.getLoginDate())
                .append(getLoginHour(), auditLog.getLoginHour())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getUsername()).append(getLoginDate()).append(getLoginHour()).toHashCode();
    }
}
