package com.jgp.authentication.exception;

import java.io.Serializable;

public class PasswordChangeException extends RuntimeException implements Serializable {
    public PasswordChangeException(String message) {
        super(message);
    }

    public PasswordChangeException(String message, Throwable cause) {
        super(message, cause);
    }
}
