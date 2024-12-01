
package com.jgp.infrastructure.documentmanagement.contentrepository;

import com.jgp.infrastructure.core.domain.Base64EncodedFile;
import com.jgp.infrastructure.core.domain.Base64EncodedImage;
import com.jgp.infrastructure.documentmanagement.exception.ContentManagementException;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;
import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import org.apache.tomcat.util.http.fileupload.FileUploadException;

import java.security.SecureRandom;

public final class ContentRepositoryUtils {

    private static final SecureRandom random = new SecureRandom();
    private static final String DATA_AND_BASE_64_PARAM = "data:%s;base64,";

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

        public String getValueWithoutDot() {
            return this.value.substring(1);
        }

        public ImageFileExtension getFileExtension() {
            return switch (this) {
                case GIF -> ImageFileExtension.GIF;
                case JPEG -> ImageFileExtension.JPEG;
                case PNG -> ImageFileExtension.PNG;
                default -> throw new IllegalArgumentException();
            };
        }
    }

    @Getter
    public enum ImageDataURIsuffix {
        GIF(String.format(DATA_AND_BASE_64_PARAM, ImageMIMEtype.GIF.getValue())),
        JPEG(String.format(DATA_AND_BASE_64_PARAM, ImageMIMEtype.JPEG.getValue())),
        PNG(String.format(DATA_AND_BASE_64_PARAM, ImageMIMEtype.PNG.getValue()));

        private final String value;

        ImageDataURIsuffix(final String value) {
            this.value = value;
        }
    }

    public static ImageFileExtension imageExtensionFromFileName(String fileName) {
        if (StringUtils.endsWith(fileName.toLowerCase(), ImageFileExtension.GIF.getValue())) {
            return ImageFileExtension.GIF;
        } else if (StringUtils.endsWith(fileName, ImageFileExtension.PNG.getValue())) {
            return ImageFileExtension.PNG;
        } else {
            return ImageFileExtension.JPEG;
        }
    }

    /**
     * Validates that passed in Mime type maps to known image mime types
     *
     */
    public static void validateImageMimeType(final String mimeType) {
        if ((!mimeType.equalsIgnoreCase(ImageMIMEtype.GIF.getValue()) && !mimeType.equalsIgnoreCase(ImageMIMEtype.JPEG.getValue())
                && !mimeType.equalsIgnoreCase(ImageMIMEtype.PNG.getValue()))) {
            throw new ContentManagementException(mimeType);
        }
    }

    /**
     * Extracts Image from a Data URL
     *
     * @param dataURL
     *            mimeType
     */
    public static Base64EncodedImage extractImageFromDataURL(final String dataURL) {
        String fileExtension = "";
        String base64EncodedString = null;
        if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.GIF.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.GIF.getValue(), "");
            fileExtension = ImageFileExtension.GIF.getValue();
        } else if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.PNG.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.PNG.getValue(), "");
            fileExtension = ImageFileExtension.PNG.getValue();
        } else if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.JPEG.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.JPEG.getValue(), "");
            fileExtension = ImageFileExtension.JPEG.getValue();
        } else {
            throw new ContentManagementException("Invalid file !!!");
        }

        return new Base64EncodedImage(base64EncodedString, fileExtension);
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

    /**
     * Extracts File from a Data URL
     *
     * @param dataURL
     *            mimeType
     */
    public static Base64EncodedFile extractFileFromDataURL(final String dataURL) throws FileUploadException {
        String fileExtension = "";
        String fileType = "";
        String base64EncodedString = null;
        if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.PNG.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.PNG.getValue(), "");
            fileExtension = ImageFileExtension.PNG.getValue();
            fileType = ImageMIMEtype.PNG.getValue();
        } else if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.JPEG.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.JPEG.getValue(), "");
            fileExtension = ImageFileExtension.JPEG.getValue();
            fileType = ImageMIMEtype.JPEG.getValue();
        } else if (StringUtils.startsWith(dataURL, ImageDataURIsuffix.GIF.getValue())) {
            base64EncodedString = dataURL.replaceAll(ImageDataURIsuffix.GIF.getValue(), "");
            fileExtension = ImageFileExtension.GIF.getValue();
            fileType = ImageMIMEtype.GIF.getValue();
        } else {
            throw new FileUploadException();
        }

        return new Base64EncodedFile(base64EncodedString, fileExtension, base64EncodedString.length(), fileType);
    }
}
