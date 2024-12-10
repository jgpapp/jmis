package com.jgp.authentication.service;

import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.dto.AuthRequestDto;
import com.jgp.authentication.dto.AuthResponseDto;
import com.jgp.authentication.dto.UserDtoV2;
import com.jgp.authentication.dto.UserPassChangeDto;

import java.util.List;

public interface UserService {

    void createUser(UserDtoV2 userDto);

    void updateUser(Long userId, UserDtoV2 userDto);

    void updateUserPassword(UserPassChangeDto userPassChangeDto);

    void resetUserPassword(Long userId);

    UserDtoV2 findUserById(Long userId);

    AppUser findUserByUsername(String userName);

    AuthResponseDto authenticateUser(AuthRequestDto authRequestDto);

    List<UserDtoV2> getAllUsers();

    AppUser currentUser();

    void updateUserRoles(Long userId, List<String> roleNames);

    void changeUserStatus(Long userId, boolean status);
}
