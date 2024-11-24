
package com.jgp.infrastructure.documentmanagement.data;

import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepositoryUtils;
import com.jgp.infrastructure.documentmanagement.domain.StorageType;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

public class ImageData implements Serializable {

    private final String location;
    private final StorageType storageType;
    @Getter
    private final String entityDisplayName;
    private final ContentRepositoryUtils.ImageMIMEtype contentType;
    @Getter
    @Setter
    private String url;

    public ImageData(final String location, final StorageType storageType, final String entityDisplayName) {
        this.location = location;
        this.storageType = storageType;
        this.entityDisplayName = entityDisplayName;
        this.contentType = ContentRepositoryUtils.ImageMIMEtype
                .fromFileExtension(ContentRepositoryUtils.imageExtensionFromFileName(location));
    }

    public ContentRepositoryUtils.ImageMIMEtype contentType() {
        return this.contentType;
    }

    public StorageType storageType() {
        return this.storageType;
    }

    public String location() {
        return this.location;
    }

}
