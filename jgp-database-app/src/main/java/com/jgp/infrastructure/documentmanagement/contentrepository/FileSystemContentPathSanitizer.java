
package com.jgp.infrastructure.documentmanagement.contentrepository;

import com.jgp.infrastructure.documentmanagement.exception.ContentManagementException;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.tika.Tika;
import org.apache.tika.io.TikaInputStream;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.sax.BodyContentHandler;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.BufferedInputStream;
import java.nio.file.Path;
import java.util.List;
import java.util.regex.Pattern;

@Slf4j
@RequiredArgsConstructor
@Component
public class FileSystemContentPathSanitizer implements ContentPathSanitizer {

    private static Pattern OVERWRITE_SIBLING_IMAGE = Pattern.compile(".*\\.\\./+[0-9]+/+.*");

    @Value("${jgp.content.regex-whitelist-enabled}")
    private boolean isRegexWhitelistEnabled;
    @Value("${jgp.content.regex-whitelist}")
    private List<String> regexWhitelist;
    @Value("${jgp.content.mime-whitelist-enabled}")
    private boolean isMimeWhitelistEnabled;
    @Value("${jgp.content.mime-whitelist}")
    private List<String> mimeWhitelist;
    private List<Pattern> regexWhitelistPatterns;

    @PostConstruct
    public void init() {
        regexWhitelistPatterns = regexWhitelist.stream().map(Pattern::compile).toList();
    }

    @Override
    public String sanitize(String path) {
        return sanitize(path, null);
    }

    @Override
    public String sanitize(String path, BufferedInputStream is) {
        try {
            if (OVERWRITE_SIBLING_IMAGE.matcher(path).matches()) {
                throw new RuntimeException(String.format("Trying to overwrite another resource's image: %s", path));
            }

            String sanitizedPath = Path.of(path).normalize().toString();

            String fileName = FilenameUtils.getName(sanitizedPath).toLowerCase();

            if (log.isDebugEnabled()) {
                log.debug("Path: {} -> {} ({})", path, sanitizedPath, fileName);
            }

            if (isRegexWhitelistEnabled) {
                boolean matches = regexWhitelistPatterns.stream().anyMatch(p -> p.matcher(fileName).matches());

                if (!matches) {
                    throw new RuntimeException(String.format("File name not allowed: %s", fileName));
                }
            }

            if (is != null && isMimeWhitelistEnabled) {
                Tika tika = new Tika();
                String extensionMimeType = tika.detect(fileName);

                if (StringUtils.isEmpty(extensionMimeType)) {
                    throw new RuntimeException(String.format("Could not detect mime type for filename %s!", fileName));
                }

                if (!mimeWhitelist.contains(extensionMimeType)) {
                    throw new RuntimeException(
                            String.format("Detected mime type %s for filename %s not allowed!", extensionMimeType, fileName));
                }

                String contentMimeType = detectContentMimeType(is);

                if (StringUtils.isEmpty(contentMimeType)) {
                    throw new RuntimeException(String.format("Could not detect content mime type for %s!", fileName));
                }
            }

            Path target = Path.of(sanitizedPath);
            Path rootFolder = Path.of(FileSystemContentRepository.JGP_BASE_DIR);

            if (!target.startsWith(rootFolder)) {
                throw new RuntimeException(String.format("Path traversal attempt: %s (%s)", target, rootFolder));
            }

            return sanitizedPath;
        } catch (Exception e) {
            throw new ContentManagementException(path, e.getMessage(), e);
        }
    }

    private String detectContentMimeType(BufferedInputStream bis) throws Exception {
        TikaInputStream tis = TikaInputStream.get(bis);
        AutoDetectParser parser = new AutoDetectParser();
        BodyContentHandler handler = new BodyContentHandler(-1);
        Metadata metadata = new Metadata();
        parser.parse(tis, handler, metadata);

        return metadata.get("Content-Type");
    }
}
