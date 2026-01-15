package com.jgp.shared.domain;

import com.jgp.authentication.domain.AppUser;
import jakarta.persistence.Column;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.PrePersist;
import jakarta.persistence.PreUpdate;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.time.Instant;
import java.util.Objects;

@Setter
@Getter
@MappedSuperclass
public class BaseEntity implements Serializable {

	@Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE)
    private Long id;

    private Instant dateCreated = Instant.now();

    private Instant lastModified;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private AppUser createdBy;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "last_modified_by_id")
    private AppUser lastModifiedBy;

    @Column(name = "is_deleted")
    private Boolean isDeleted = false;

    @PrePersist
    public void setDateCreated() {
        this.dateCreated = Instant.now();
    }

    @PreUpdate
    public void setLastModified() {
        this.lastModified = Instant.now();
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (this.getId() != null ? this.getId().hashCode() : 0);

        return hash;
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (object == null) {
            return false;
        }
        if (getClass() != object.getClass()) {
            return false;
        }

        BaseEntity other = (BaseEntity) object;
        return Objects.equals(this.getId(), other.getId()) || (this.getId() != null && this.id.equals(other.id));
    }

    @Override
    public String toString() {
        return this.getClass().getName() + " [ID=" + id + "]";
    }

}
