
package com.jgp.infrastructure.documentmanagement.data;

import com.google.common.io.ByteSource;

public class FileData {

    private final String fileName;
    private final String contentType;
    private final ByteSource byteSource;

    public FileData(final ByteSource byteSource, final String fileName, final String contentType) {
        this.fileName = fileName;
        this.contentType = contentType;
        this.byteSource = byteSource;
    }

    public String contentType() {
        return this.contentType;
    }

    public String name() {
        return this.fileName;
    }

    public ByteSource getByteSource() {
        return this.byteSource;
    }
}
