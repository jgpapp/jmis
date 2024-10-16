package com.jgp.patner.exception;

import java.io.Serializable;

public class PartnerNotFoundException extends RuntimeException implements Serializable {

    public PartnerNotFoundException(String message) {
        super(message);
    }

    public PartnerNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

}
