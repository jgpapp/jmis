package com.jgp.shared.exception;

import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ProblemDetail;
import org.springframework.web.ErrorResponseException;

import java.time.Instant;

public class ResourceNotFound extends ErrorResponseException {
    public ResourceNotFound(HttpStatusCode status) {
        super(status);
    }

    public ResourceNotFound(HttpStatusCode status, Throwable cause) {
        super(status, cause);
    }

    public ResourceNotFound(HttpStatusCode status, ProblemDetail body, Throwable cause) {
        super(status, body, cause);
    }

    public ResourceNotFound(HttpStatusCode status, ProblemDetail body, Throwable cause, String messageDetailCode, Object[] messageDetailArguments) {
        super(status, body, cause, messageDetailCode, messageDetailArguments);
    }

    protected static ProblemDetail asProblemDetail(String title, String message) {
        ProblemDetail problemDetail = ProblemDetail.forStatusAndDetail(HttpStatus.NOT_FOUND, message);
        problemDetail.setTitle(title);
        problemDetail.setProperty("timestamp", Instant.now());
        return problemDetail;
    }
}