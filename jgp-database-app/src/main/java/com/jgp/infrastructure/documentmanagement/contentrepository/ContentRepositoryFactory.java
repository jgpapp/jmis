
package com.jgp.infrastructure.documentmanagement.contentrepository;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ContentRepositoryFactory {

    private final FileSystemContentPathSanitizer contentPathSanitizer;

    public ContentRepository getRepository() {
        return new FileSystemContentRepository(contentPathSanitizer);
    }

}
