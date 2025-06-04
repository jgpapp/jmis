package com.jgp.authentication.service;

import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.dto.*;

import java.util.List;

public interface UserService {

    void createUser(UserDtoV2 userDto);

    void updateUser(Long userId, UserDtoV2 userDto);

    void updateUserPassword(UserPassChangeDto userPassChangeDto);

    void resetUserPassword(Long userId);

    UserDtoV2 findUserById(Long userId);

    AppUser findUserByUsername(String userName);

    AuthResponseDto authenticateUser(AuthRequestDto authRequestDto);

    AuthResponseDto refreshAccessToken(RefreshTokenRequest refreshTokenRequest);

    List<UserDtoV2> getAllUsers();

    AppUser currentUser();

    void updateUserRoles(Long userId, List<String> roleNames);

    void changeUserStatus(Long userId, boolean status);

    List<AppUser> findUsersByPartnerId(Long partnerId);
}
