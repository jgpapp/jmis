package com.jgp.authentication.service;


import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.domain.AppUserRepository;
import com.jgp.authentication.dto.*;
import com.jgp.authentication.exception.UserNotFoundException;
import com.jgp.authentication.filter.JwtTokenProvider;
import com.jgp.patner.domain.Partner;
import com.jgp.patner.domain.PartnerRepository;
import com.jgp.patner.exception.PartnerNotFoundException;
import com.jgp.shared.exception.DataRulesViolationException;
import com.jgp.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
@Slf4j
public class UserServiceImpl implements UserService{

    private final AppUserRepository userRepository;
    private final UserDetailsService userDetailsService;
    private final AuthenticationManager authenticationManager;
    private final PasswordEncoder passwordEncoder;
    private final PartnerRepository partnerRepository;
    private final RoleService roleService;
    private final JwtTokenProvider jwtTokenProvider;
    private static final String NO_USER_FOUND_WITH_ID = "No user found with Id";


    @Transactional
    @Override
    public void createUser(UserDtoV2 userDto) {
        try {
            Partner partner = null;
            if (Objects.nonNull(userDto.partnerId()) && !Objects.equals(0L, userDto.partnerId())) {
                partner = this.partnerRepository.findById(userDto.partnerId())
                        .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                        .orElseThrow(() -> new PartnerNotFoundException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
            }
            var user = this.userRepository.save(AppUser.createUser(partner, userDto, passwordEncoder));
            if (!userDto.userRoles().isEmpty()){
                this.updateUserRoles(user.getId(), new ArrayList<>(userDto.userRoles()));
            }
        }catch (Exception e){
            throw new IllegalArgumentException(e);
        }

    }

    @Transactional
    @Override
    public void updateUser(Long userId, UserDtoV2 userDto) {
        var currentUser = this.userRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new UserNotFoundException(NO_USER_FOUND_WITH_ID));
        Partner partner = null;
        if (Objects.nonNull(userDto.partnerId()) && !Objects.equals(0L, userDto.partnerId())) {
            partner = this.partnerRepository.findById(userDto.partnerId())
                    .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                    .orElseThrow(() -> new PartnerNotFoundException(CommonUtil.NO_RESOURCE_FOUND_WITH_ID));
        }
        try {
            currentUser.updateUser(userDto, partner);
            this.userRepository.save(currentUser);
            if (!userDto.userRoles().isEmpty()){
                this.updateUserRoles(userId, new ArrayList<>(userDto.userRoles()));
            }
        }catch (Exception e){
            throw new IllegalArgumentException(e);
        }

    }

    @Transactional
    @Override
    public void updateUserPassword(UserPassChangeDto userPassChangeDto) {
        if (!StringUtils.equals(userPassChangeDto.newPass(), userPassChangeDto.passConfirm())){
            throw new DataRulesViolationException("New password must match with confirmation password!!");
        }
        final var passWordStrengthRegex = "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%^&+=])(?=\\S+$).{8,}$";
        boolean isValidPassword = Pattern.compile(passWordStrengthRegex)
                .matcher(userPassChangeDto.newPass())
                .find();
        if (!isValidPassword){
            throw new DataRulesViolationException("""
                    Invalid new password.  A password must:\s
                    1. Has minimum 8 characters in length. \s
                    2. Has at least one uppercase English letter (A-Z).
                    3. Has at least one lowercase English letter (a-z).
                    4. Has at least one digit (0-9) and\s
                    5. Has at least one special character (@#$%^&+=)
                   \s"""
            );
        }
        final var currentUser = currentUser();
        if (this.passwordEncoder.matches(userPassChangeDto.newPass(), currentUser.getPassword())){
            throw new DataRulesViolationException("New Password must not much the previous password!!");
        }else if (!this.passwordEncoder.matches(userPassChangeDto.password(), currentUser.getPassword())){
            throw new DataRulesViolationException("Invalid current password!!");
        }
        currentUser.setPassword(this.passwordEncoder.encode(userPassChangeDto.newPass()));
        currentUser.setForceChangePass(false);
        currentUser.setLastModified(LocalDate.now(ZoneId.systemDefault()));
        currentUser.setDatePasswordChanged(LocalDate.now(ZoneId.systemDefault()));
        this.userRepository.save(currentUser);
    }

    @Override
    public void resetUserPassword(Long userId) {
        var user = this.userRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new UserNotFoundException(NO_USER_FOUND_WITH_ID));
        user.setPassword(this.passwordEncoder.encode(user.getUsername()));
        user.setForceChangePass(true);
        this.userRepository.save(user);
    }

    @Override
    public UserDtoV2 findUserById(Long userId) {
        return this.userRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .map(AppUser::toDto)
                .orElseThrow(() -> new UserNotFoundException(NO_USER_FOUND_WITH_ID));
    }

    @Override
    public AppUser findUserByUsername(String userName) {
        return this.userRepository.findByUsernameAndIsDeletedFalse(userName).orElse(null);
    }


    @Override
    public AuthResponseDto authenticateUser(AuthRequestDto authRequestDto) {
        log.info("Obtaining JWT Token !!!");
        final var userDetails = userDetailsService.loadUserByUsername(authRequestDto.username());
        if (Objects.isNull(userDetails)) {
            throw  new UserNotFoundException("Bad User Credentials !!");
        }else {
            final var user = findUserByUsername(userDetails.getUsername());
            if (!user.isActive()){
                throw  new UserNotFoundException("User Locked !!");
            }
            this.authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(userDetails, authRequestDto.password(), userDetails.getAuthorities()));
            final var accessToken = this.jwtTokenProvider.createToken(user);
            final var refreshToken = this.jwtTokenProvider.createRefreshToken(user);
            return new AuthResponseDto(true, "User Authenticated!!", accessToken, refreshToken);
        }
    }

    @Override
    public AuthResponseDto refreshAccessToken(RefreshTokenRequest refreshTokenRequest) {
        log.info("Refreshing JWT Token !!!");
        String refreshToken = refreshTokenRequest.refreshToken();
        if (jwtTokenProvider.isValidToken(refreshToken)) {

            String username = jwtTokenProvider.extractUsername(refreshToken);
            final var userDetails = userDetailsService.loadUserByUsername(username);
            final var user = findUserByUsername(userDetails.getUsername());

            // Re-generate both access and refresh token for security/simplicity
            final var newAccessToken = this.jwtTokenProvider.createToken(user);
            final var newRefreshToken = this.jwtTokenProvider.createRefreshToken(user);

            return new AuthResponseDto(true, "Token Refreshed!!", newAccessToken, newRefreshToken);
        }
        return null;
    }

    @Override
    public List<UserDtoV2> getAllUsers() {
        return this.userRepository.findByIsDeletedFalse().stream().map(AppUser::toDto).toList();
    }

    @Override
    public AppUser currentUser() {
        AppUser currentUser = null;
        final var context = SecurityContextHolder.getContext();
        if (context != null) {
            final var authentication = context.getAuthentication();
            if (authentication != null && !(authentication instanceof AnonymousAuthenticationToken)) {
                    String currentUserName = authentication.getName();
                    currentUser = this.findUserByUsername(currentUserName);
            }
        }
        return currentUser;
    }

    @Transactional
    @Override
    public void updateUserRoles(Long userId, List<String> roleNames) {
        final var user =  this.userRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new UserNotFoundException(NO_USER_FOUND_WITH_ID));
        user.updateRoles(new HashSet<>(this.roleService.retrieveRolesByNames(roleNames)));
        this.userRepository.save(user);
    }

    @Transactional
    @Override
    public void changeUserStatus(Long userId, boolean status) {
        final var user =  this.userRepository.findById(userId)
                .filter(t -> Boolean.FALSE.equals(t.getIsDeleted()))
                .orElseThrow(() -> new UserNotFoundException(NO_USER_FOUND_WITH_ID));
        user.changeUserStatus(status);
        this.userRepository.save(user);
    }

    @Override
    public List<AppUser> findUsersByPartnerId(Long partnerId) {
        return this.userRepository.findUsersByPartnerId(partnerId);
    }


}
