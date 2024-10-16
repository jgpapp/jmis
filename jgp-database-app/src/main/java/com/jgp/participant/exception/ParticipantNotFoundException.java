package com.jgp.participant.exception;

public class ParticipantNotFoundException extends RuntimeException{

    public ParticipantNotFoundException(String message) {
        super(message);
    }

    public ParticipantNotFoundException(Long roleId) {
        super(String.format("Participant with Id: %d not found", roleId));
    }

    public ParticipantNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
