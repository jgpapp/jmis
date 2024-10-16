package com.jgp.authentication.exception;

import java.io.Serializable;

public class ResetPasswordException extends RuntimeException implements Serializable {

    public ResetPasswordException(String message) {
        super(message);
    }

    public ResetPasswordException(Long userId) {
        super(String.format("User with Id: %d must reset password", userId));
    }

    public ResetPasswordException(String message, Throwable cause) {
        super(message, cause);
    }
}
