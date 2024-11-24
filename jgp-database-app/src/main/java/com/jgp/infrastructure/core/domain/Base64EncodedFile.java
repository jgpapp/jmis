
package com.jgp.infrastructure.core.domain;

/**
 * A class to represent a file that has been base64-encoded.
 **/
public record Base64EncodedFile(String base64EncodedString, String fileExtension, long size, String fileType) {

}
