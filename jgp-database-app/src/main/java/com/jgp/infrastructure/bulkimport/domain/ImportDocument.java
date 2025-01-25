
package com.jgp.infrastructure.bulkimport.domain;

import com.jgp.infrastructure.documentmanagement.domain.Document;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Getter
@Entity
@Table(name = "import_document")
public class ImportDocument extends BaseEntity {

    @OneToOne
    @JoinColumn(name = "document_id")
    private Document document;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @Column(name = "import_time")
    private LocalDateTime importTime;

    @Column(name = "end_time")
    private LocalDateTime endTime;

    @Column(name = "completed", nullable = false)
    private Boolean completed;

    @Column(name = "entity_type")
    private Integer entityType;

    @Column(name = "total_records")
    private Integer totalRecords;

    @Column(name = "success_count")
    private Integer successCount;

    @Column(name = "failure_count")
    private Integer failureCount;

    protected ImportDocument() {

    }

    public static ImportDocument instance(final Document document, final LocalDateTime importTime, final Integer entityType,
            final Integer totalRecords, Partner partner) {
        final Integer successCount = 0;
        final Integer failureCount = 0;
        final LocalDateTime endTime = LocalDateTime.now(ZoneId.systemDefault());

        return new ImportDocument(document, importTime, endTime, false, entityType, totalRecords, successCount,
                failureCount, partner);
    }

    private ImportDocument(final Document document, final LocalDateTime importTime, final LocalDateTime endTime, Boolean completed,
            final Integer entityType, final Integer totalRecords, final Integer successCount,
            final Integer failureCount, Partner partner) {
        this.document = document;
        this.importTime = importTime;
        this.endTime = endTime;
        this.completed = completed;
        this.entityType = entityType;
        this.totalRecords = totalRecords;
        this.successCount = successCount;
        this.failureCount = failureCount;
        this.partner = partner;
    }

    public void update(final LocalDateTime endTime, final Integer totalCount, final Integer successCount, final Integer errorCount) {
        this.endTime = endTime;
        this.completed = Boolean.TRUE;
        this.successCount = successCount;
        this.failureCount = errorCount;
        this.totalRecords = totalCount;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        ImportDocument impDocument = (ImportDocument) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), impDocument.getId())
                .append(getDocument(), impDocument.getDocument())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getDocument()).toHashCode();
    }

}
