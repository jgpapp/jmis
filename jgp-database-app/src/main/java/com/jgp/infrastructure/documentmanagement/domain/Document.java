
package com.jgp.infrastructure.documentmanagement.domain;

import com.jgp.infrastructure.documentmanagement.command.DocumentCommand;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

@Getter
@Entity
@Table(name = "jgp_document")
public class Document extends BaseEntity {

    @Column(name = "parent_entity_type", length = 50)
    private String parentEntityType;

    @Column(name = "parent_entity_id", length = 1000)
    private Long parentEntityId;

    @Column(name = "doc_name", length = 250)
    private String name;

    @Column(name = "file_name", length = 250)
    private String fileName;

    @Column(name = "doc_size")
    private Long size;

    @Column(name = "type", length = 50)
    private String type;

    @Column(name = "description", length = 1000)
    private String description;

    @Column(name = "location", length = 500)
    private String location;

    public Document() {}

    public static Document createNew(final String parentEntityType, final Long parentEntityId, final String name, final String fileName,
            final Long size, final String type, final String description, final String location) {
        return new Document(parentEntityType, parentEntityId, name, fileName, size, type, description, location);
    }

    private Document(final String parentEntityType, final Long parentEntityId, final String name, final String fileName, final Long size,
            final String type, final String description, final String location) {
        this.parentEntityType = StringUtils.defaultIfEmpty(parentEntityType, null);
        this.parentEntityId = parentEntityId;
        this.name = StringUtils.defaultIfEmpty(name, null);
        this.fileName = StringUtils.defaultIfEmpty(fileName, null);
        this.size = size;
        this.type = StringUtils.defaultIfEmpty(type, null);
        this.description = StringUtils.defaultIfEmpty(description, null);
        this.location = StringUtils.defaultIfEmpty(location, null);
    }

    public void update(final DocumentCommand command) {
        if (command.isDescriptionChanged()) {
            this.description = command.getDescription();
        }
        if (command.isFileNameChanged()) {
            this.fileName = command.getFileName();
        }
        if (command.isFileTypeChanged()) {
            this.type = command.getType();
        }
        if (command.isLocationChanged()) {
            this.location = command.getLocation();
        }
        if (command.isNameChanged()) {
            this.name = command.getName();
        }
        if (command.isSizeChanged()) {
            this.size = command.getSize();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        Document document = (Document) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), document.getId())
                .append(getParentEntityType(), document.getParentEntityType())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getParentEntityType()).toHashCode();
    }
}
