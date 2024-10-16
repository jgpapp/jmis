package com.jgp.shared.exception;

import com.jgp.authentication.exception.UserNotFoundException;
import io.jsonwebtoken.JwtException;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.web.ErrorResponse;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

import java.time.Instant;

@RestControllerAdvice
public class GlobalExceptionHandler extends ResponseEntityExceptionHandler {

    private static final String TIME_STAMP = "timestamp";


    @ExceptionHandler({ UsernameNotFoundException.class })
    public ErrorResponse handleAuthenticationUserNotFoundException(UsernameNotFoundException ex) {
        return ErrorResponse.builder(ex, HttpStatus.NOT_FOUND, ex.getMessage())
                .title("User Not Found")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler({ AuthenticationException.class })
    public ErrorResponse handleAuthenticationException(AuthenticationException ex) {
        return ErrorResponse.builder(ex, HttpStatus.NOT_FOUND, ex.getMessage())
                .title("User Not Authenticated")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler(value = { BadCredentialsException.class})
    @ResponseStatus(value = HttpStatus.UNAUTHORIZED)
    public ErrorResponse unAuthenticatedException(BadCredentialsException ex) {
        return ErrorResponse.builder(ex, HttpStatus.UNAUTHORIZED, ex.getMessage())
                .title("User Not Authenticated")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler(value = { UserNotFoundException.class})
    @ResponseStatus(value = HttpStatus.UNAUTHORIZED)
    public ErrorResponse unAuthenticatedException(UserNotFoundException ex) {
        return ErrorResponse.builder(ex, HttpStatus.UNAUTHORIZED, ex.getMessage())
                .title("User Not Found")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler(value = { JwtException.class})
    @ResponseStatus(value = HttpStatus.UNAUTHORIZED)
    public ErrorResponse unAuthorizedException(JwtException ex) {
        return ErrorResponse.builder(ex, HttpStatus.UNAUTHORIZED, ex.getMessage())
                .title("Invalid Auth Token")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler({DataRulesViolationException.class})
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public ErrorResponse dataIntegrityViolationException(DataRulesViolationException ex){
        return ErrorResponse.builder(ex, HttpStatus.BAD_REQUEST, ex.getMessage())
                .title("Bad Request")
                .property(TIME_STAMP, Instant.now())
                .build();
    }

    @ExceptionHandler({RuntimeException.class})
    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    public ErrorResponse unCaughtRuntimeException(RuntimeException ex){
        return ErrorResponse.builder(ex, HttpStatus.INTERNAL_SERVER_ERROR, ex.getMessage())
                .title("Error Occurred")
                .property(TIME_STAMP, Instant.now())
                .build();
    }
}
