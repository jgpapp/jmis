
package com.jgp.infrastructure.documentmanagement.contentrepository;

import com.jgp.infrastructure.documentmanagement.exception.ContentManagementException;
import lombok.Getter;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;

import java.security.SecureRandom;

public final class ContentRepositoryUtils {

    private static final SecureRandom random = new SecureRandom();

    private ContentRepositoryUtils() {}

    @Getter
    public enum ImageMIMEtype {

        GIF("image/gif"), JPEG("image/jpeg"), PNG("image/png");

        private final String value;

        ImageMIMEtype(final String value) {
            this.value = value;
        }

        @SuppressWarnings("UnnecessaryDefaultInEnumSwitch")
        public static ImageMIMEtype fromFileExtension(ImageFileExtension fileExtension) {
            return switch (fileExtension) {
                case GIF -> ImageMIMEtype.GIF;
                case JPG, JPEG -> ImageMIMEtype.JPEG;
                case PNG -> ImageMIMEtype.PNG;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @Getter
    public enum ImageFileExtension {

        GIF(".gif"), JPEG(".jpeg"), JPG(".jpg"), PNG(".png");

        private final String value;

        ImageFileExtension(final String value) {
            this.value = value;
        }

    }

    public static ImageFileExtension imageExtensionFromFileName(String fileName) {
        if (fileName == null) {
            return ImageFileExtension.JPEG;
        }
        String lowerFileName = fileName.toLowerCase();
        if (lowerFileName.endsWith(ImageFileExtension.GIF.getValue())) {
            return ImageFileExtension.GIF;
        } else if (lowerFileName.endsWith(ImageFileExtension.PNG.getValue())) {
            return ImageFileExtension.PNG;
        } else {
            return ImageFileExtension.JPEG;
        }
    }

    /**
     * Using Content-Length gives me size of the entire request, which is good enough for now for a fast fail as the
     * length of the rest of the content i.e name and description while compared to the uploaded file size is
     * negligible
     **/
    public static void validateFileSizeWithinPermissibleRange(final Long fileSize, final String name) {

        if (fileSize != null && ((fileSize / (1024 * 1024)) > ContentRepository.MAX_FILE_UPLOAD_SIZE_IN_MB)) {
            throw new ContentManagementException(name, fileSize, ContentRepository.MAX_FILE_UPLOAD_SIZE_IN_MB);
        }
    }

    /**
     * Generate a random String.
     */

    @SuppressFBWarnings(value = {
            "DMI_RANDOM_USED_ONLY_ONCE" }, justification = "False positive for random object created and used only once")
    public static String generateRandomString() {
        final String characters = "abcdefghijklmnopqrstuvwxyz123456789";
        // length is a random number between 5 to 16
        final int length = random.nextInt(11) + 5;
        final char[] text = new char[length];
        for (int i = 0; i < length; i++) {
            text[i] = characters.charAt(random.nextInt(characters.length()));
        }
        return new String(text);
    }

}
