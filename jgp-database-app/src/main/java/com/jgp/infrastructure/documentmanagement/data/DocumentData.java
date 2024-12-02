
package com.jgp.infrastructure.documentmanagement.data;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

/**
 * Immutable data object representing a user document being managed on the platform.
 */
@Getter
public class DocumentData implements Serializable {

    private final Long id;
    private final String parentEntityType;
    private final Long parentEntityId;
    private final String name;
    private final String fileName;
    private final Long size;
    private final String type;
    private final String location;
    private final String description;
    @Setter
    private String url;

    public DocumentData(final Long id, final String parentEntityType, final Long parentEntityId, final String name, final String fileName,
            final Long size, final String type, final String description, final String location) {
        this.id = id;
        this.parentEntityType = parentEntityType;
        this.parentEntityId = parentEntityId;
        this.name = name;
        this.fileName = fileName;
        this.size = size;
        this.type = type;
        this.description = description;
        this.location = location;
    }

    public String contentType() {
        return this.type;
    }

    public String fileName() {
        return this.fileName;
    }

    public String fileLocation() {
        return this.location;
    }


}
