package com.jgp.authentication.api;

import com.jgp.authentication.dto.UserDtoV2;
import com.jgp.authentication.dto.UserPassChangeDto;
import com.jgp.authentication.service.RoleService;
import com.jgp.authentication.service.UserService;
import com.jgp.shared.dto.ApiResponseDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
@Validated
@RequestMapping("api/v1/users")
public class UserController {

    private final UserService userService;
    private final RoleService roleService;

    @PutMapping("change-password")
    public ResponseEntity<ApiResponseDto> changePassword(@Valid @RequestBody UserPassChangeDto changePasswordDto){
        this.userService.updateUserPassword(changePasswordDto);
        return new ResponseEntity<>(new ApiResponseDto(true, "Password successfully updated !!"), HttpStatus.OK);
    }

    @GetMapping
    public ResponseEntity<List<UserDtoV2>> getAvailableUsers(){
        return new ResponseEntity<>(this.userService.getAllUsers(), HttpStatus.OK);
    }

    @PostMapping
    public ResponseEntity<ApiResponseDto> createUser(@Valid @RequestBody UserDtoV2 newUser){
        this.userService.createUser(newUser);
        return new ResponseEntity<>(new ApiResponseDto(true, "User created !!"), HttpStatus.CREATED);
    }

    @PutMapping("{userId}")
    public ResponseEntity<ApiResponseDto> updateUser(@PathVariable("userId") Long userId, @Valid @RequestBody UserDtoV2 userDto){
        this.userService.updateUser(userId, userDto);
        return new ResponseEntity<>(new ApiResponseDto(true, "User updated !!"), HttpStatus.OK);
    }

    @PutMapping("{userId}/update-roles")
    public ResponseEntity<ApiResponseDto> updateUserRoles(@PathVariable("userId") Long userId, @RequestBody List<String> roleNames){
        this.userService.updateUserRoles(userId, roleNames);
        return new ResponseEntity<>(new ApiResponseDto(true, "User Roles updated !!"), HttpStatus.OK);
    }

    @GetMapping("{userId}")
    public ResponseEntity<UserDtoV2> getUser(@PathVariable("userId") Long userId){
        return new ResponseEntity<>(this.userService.findUserById(userId), HttpStatus.OK);
    }

    @PutMapping("change-user-status/{user-id}/{new-status}")
    public ResponseEntity<ApiResponseDto> changeUserStatus(@PathVariable("user-id") Long userId, @PathVariable("new-status") String newStatus){
        this.userService.changeUserStatus(userId, "ACTIVE".equals(newStatus));
        return new ResponseEntity<>(new ApiResponseDto(true, "User status updated !!"), HttpStatus.OK);
    }
}
