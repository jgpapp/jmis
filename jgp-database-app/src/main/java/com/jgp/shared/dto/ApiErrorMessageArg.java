package com.jgp.shared.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ApiErrorMessageArg {

    /**
     * The actual value of the parameter (if any) as passed to API.
     */
    private Object value;

    public static ApiErrorMessageArg from(final Object object) {
        return new ApiErrorMessageArg(object);
    }

    public ApiErrorMessageArg(final Object object) {
        this.value = object;
    }
}
