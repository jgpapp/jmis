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

@Getter
@Entity
@Table(name = "permission", uniqueConstraints = {
        @UniqueConstraint(columnNames = { "code" }, name = "UNIQUE_PERMISSION")
})
@SequenceGenerator(name = "permission_seq", sequenceName = "permission_seq", allocationSize = 50)
public class Permission extends BaseEntity {

    @Override
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "permission_seq")
    public Long getId() {
        return super.getId();
    }

    @Column(name = "code", nullable = false, length = 100)
    private String code;

    @Column(name = "entity_name", length = 100)
    private String entityName;

    @Column(name = "action_name", length = 100)
    private String actionName;

    public Permission() {
    }

    public Permission(String code, String actionName, String entityName) {
        this.code = code;
        this.actionName = actionName;
        this.entityName = entityName;
    }

    public boolean hasCode(final String checkCode) {
        return this.code.equalsIgnoreCase(checkCode);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Permission permission = (Permission) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), permission.getId())
                .append(getCode(), permission.getCode())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getCode()).toHashCode();
    }
}
