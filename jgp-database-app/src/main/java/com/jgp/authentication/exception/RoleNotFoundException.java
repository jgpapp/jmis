package com.jgp.authentication.exception;

public class RoleNotFoundException extends RuntimeException{

    public RoleNotFoundException(String message) {
        super(message);
    }

    public RoleNotFoundException(Long roleId) {
        super(String.format("Role with Id: %d not found", roleId));
    }

    public RoleNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
