package com.jgp.authentication.exception;

import java.io.Serializable;

public class NoAuthorizationException extends RuntimeException implements Serializable {

    public NoAuthorizationException(String message) {
        super(message);
    }

    public NoAuthorizationException(String message, Throwable cause) {
        super(message, cause);
    }
}
