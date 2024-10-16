package com.jgp.authentication.exception;

import java.io.Serializable;

public class UserNotFoundException extends RuntimeException implements Serializable {

    public UserNotFoundException(String message) {
        super(message);
    }

    public UserNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

}
