
package com.jgp.infrastructure.documentmanagement.command;

import lombok.Getter;
import lombok.Setter;

import java.util.Set;

@Setter
@Getter
public class DocumentCommand {

    private final Long id;
    private final String parentEntityType;
    private final Long parentEntityId;
    private final String name;
    private final String description;

    private String fileName;
    private Long size;
    private String type;
    private String location;

    private final Set<String> modifiedParameters;

    public DocumentCommand(final Set<String> modifiedParameters, final Long id, final String parentEntityType, final Long parentEntityId,
            final String name, final String fileName, final Long size, final String type, final String description, final String location) {
        this.modifiedParameters = modifiedParameters;
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

    public boolean isNameChanged() {
        return this.modifiedParameters.contains("name");
    }

    public boolean isFileNameChanged() {
        return this.modifiedParameters.contains("fileName");
    }

    public boolean isSizeChanged() {
        return this.modifiedParameters.contains("size");
    }

    public boolean isFileTypeChanged() {
        return this.modifiedParameters.contains("type");
    }

    public boolean isDescriptionChanged() {
        return this.modifiedParameters.contains("description");
    }

    public boolean isLocationChanged() {
        return this.modifiedParameters.contains("location");
    }

}
