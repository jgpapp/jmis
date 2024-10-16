package com.jgp.authentication.exception;

import java.io.Serializable;

public class UserNotAuthenticatedException extends RuntimeException implements Serializable {

    public UserNotAuthenticatedException(String message) {
        super(message);
    }

    public UserNotAuthenticatedException(String message, Throwable cause) {
        super(message, cause);
    }

}
