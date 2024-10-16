package com.jgp.authentication.api;


import com.jgp.authentication.dto.AuthRequestDto;
import com.jgp.authentication.dto.AuthResponseDto;
import com.jgp.authentication.service.UserService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
@Validated
public class AuthenticationController {

    private final UserService userService;

    @PostMapping("api/v1/users/authenticate")
    public ResponseEntity<AuthResponseDto> authenticateUser(@Valid @RequestBody AuthRequestDto jwtRequest){
        return new ResponseEntity<>(userService.authenticateUser(jwtRequest), HttpStatus.OK);
    }
}
