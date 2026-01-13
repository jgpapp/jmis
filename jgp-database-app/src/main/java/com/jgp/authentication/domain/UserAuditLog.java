package com.jgp.authentication.domain;

import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.SequenceGenerator;
import jakarta.persistence.Table;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Getter
@Entity
@Table(name = "user_audit_logs")
@SequenceGenerator(name = "user_audit_logs_seq", sequenceName = "user_audit_logs_seq", allocationSize = 1)
public class UserAuditLog extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "user_audit_logs_seq")
    public Long getId() {
        return super.getId();
    }

    @Column(name = "username", nullable = false)
    private String username;

    @Column(name = "action", nullable = false)
    private String action;

    @Column(name = "resource_id", nullable = false)
    private Long resourceId;

    @Column(name = "details", columnDefinition = "TEXT")
    private String details;

    @Column(name = "log_time", nullable = false, updatable = false)
    private LocalDateTime logTime;

    public UserAuditLog() {
        // Default constructor for JPA
    }

    public UserAuditLog(String username, String action, Long resourceId, String details) {
        this.username = username;
        this.action = action;
        this.resourceId = resourceId;
        this.details = details;
        this.logTime = LocalDateTime.now(ZoneId.systemDefault());
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        UserAuditLog auditLog = (UserAuditLog) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), auditLog.getId())
                .append(getUsername(), auditLog.getUsername())
                .append(getLogTime(), auditLog.getLogTime())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getUsername()).append(getLogTime()).toHashCode();
    }
}
