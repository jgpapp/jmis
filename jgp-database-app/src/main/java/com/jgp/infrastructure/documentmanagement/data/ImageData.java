
package com.jgp.infrastructure.documentmanagement.data;

import com.jgp.infrastructure.documentmanagement.contentrepository.ContentRepositoryUtils;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;

public class ImageData implements Serializable {

    private final String location;
    @Getter
    private final String entityDisplayName;
    private final ContentRepositoryUtils.ImageMIMEtype contentType;
    @Getter
    @Setter
    private String url;

    public ImageData(final String location, final String entityDisplayName) {
        this.location = location;
        this.entityDisplayName = entityDisplayName;
        this.contentType = ContentRepositoryUtils.ImageMIMEtype
                .fromFileExtension(ContentRepositoryUtils.imageExtensionFromFileName(location));
    }

    public ContentRepositoryUtils.ImageMIMEtype contentType() {
        return this.contentType;
    }

    public String location() {
        return this.location;
    }

}
