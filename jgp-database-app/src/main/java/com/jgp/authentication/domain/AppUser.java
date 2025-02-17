package com.jgp.authentication.domain;


import com.jgp.authentication.dto.UserDtoV2;
import com.jgp.authentication.exception.NoAuthorizationException;
import com.jgp.patner.domain.Partner;
import com.jgp.shared.domain.BaseEntity;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.JoinTable;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.UniqueConstraint;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.crypto.password.PasswordEncoder;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Getter
@Entity
@Table(name = "users", uniqueConstraints = { @UniqueConstraint(columnNames = { "email_address" }, name = "EMAIL_UNIQUE")})
public class AppUser extends BaseEntity implements PlatformUser {

	private String firstName;

    private String lastName;
    @Column(name = "email_address", nullable = false)
    private String username;

    @Setter
    @Column(nullable = false)
    private String password;

    private String designation;

    private String town;

    private String gender;

    private String image;

    private String cellPhone;


    private boolean isActive;

    @Setter
    private boolean forceChangePass;

    @Transient
    private String resetPass;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "partner_id")
    private Partner partner;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(name = "appuser_role", joinColumns = @JoinColumn(name = "appuser_id"), inverseJoinColumns = @JoinColumn(name = "role_id"))
    private Set<Role> roles = new HashSet<>();

    public AppUser() {
    }

    private AppUser(Partner partner, String firstName, String lastName, String username,
                    String designation, String town, String gender, String image, String cellPhone, boolean isActive, boolean forceChangePass, PasswordEncoder encoder) {
        this.partner = partner;
        this.firstName = firstName;
        this.lastName = lastName;
        this.username = username;
        this.password = encoder.encode(username);
        this.designation = designation;
        this.cellPhone = cellPhone;
        this.isActive = isActive;
        this.forceChangePass = forceChangePass;
        this.town = town;
        this.gender = gender;
        this.image = image;
    }

    public String getUserFullName(){
        return String.format("%s, %s", this.firstName, this.lastName);
    }

    public static AppUser createUser(Partner partner, UserDtoV2 userDto, PasswordEncoder encoder){
        return new AppUser(partner, userDto.firstName(), userDto.lastName(),
                userDto.username(), userDto.designation(), userDto.town(),
                userDto.gender(), userDto.image(), userDto.cellPhone(), true, true, encoder);
    }

    public void updateUser(UserDtoV2 userDto, Partner partner){
        if(!StringUtils.equals(userDto.firstName(), this.firstName)){
            this.firstName = userDto.firstName();
        }
        if(!StringUtils.equals(userDto.lastName(), this.lastName)){
            this.lastName = userDto.lastName();
        }
        if(!StringUtils.equals(userDto.gender(), this.gender)){
            this.gender = userDto.gender();
        }
        if(!Objects.equals(partner, this.partner)){
            this.partner = partner;
        }
        if(!StringUtils.equals(userDto.designation(), this.designation)){
            this.designation = userDto.designation();
        }
        if(!StringUtils.equals(userDto.town(), this.town)){
            this.town = userDto.town();
        }
        if(!StringUtils.equals(userDto.cellPhone(), this.cellPhone)){
            this.cellPhone = userDto.cellPhone();
        }
    }

    public void changeUserStatus(boolean status){
        this.isActive = status;
    }

    public UserDtoV2 toDto(){
        return new UserDtoV2(getId(),
                this.firstName, this.lastName, this.gender, this.image, this.roles.stream().map(Role::getRoleName).collect(Collectors.toSet()),
                Objects.nonNull(this.partner) ? this.partner.getPartnerName() : "GLOBAL PARTNER", Objects.nonNull(this.partner) ? this.partner.getId() : 0L, this.getDesignation(),
                this.username, this.cellPhone, this.town, this.isActive);
    }

    public void updateRoles(final Set<Role> allRoles) {
            this.roles = allRoles;
    }

    @Override
    public Collection<GrantedAuthority> getAuthorities() {
        return populateGrantedAuthorities();
    }

    private List<GrantedAuthority> populateGrantedAuthorities() {
        final List<GrantedAuthority> grantedAuthorities = new ArrayList<>();
        for (final Role role : this.roles) {
            final Collection<Permission> permissions = role.getPermissions();
            for (final Permission permission : permissions) {
                grantedAuthorities.add(new SimpleGrantedAuthority(permission.getCode()));
            }
        }
        return grantedAuthorities;
    }

    public boolean hasNotPermissionForAnyOf(final String... permissionCodes) {
        boolean hasNotPermission = true;
        for (final String permissionCode : permissionCodes) {
            final boolean checkPermission = hasPermissionTo(permissionCode);
            if (checkPermission) {
                hasNotPermission = false;
                break;
            }
        }
        return hasNotPermission;
    }

    /**
     * Checks whether the user has a given permission explicitly.
     *
     * @param permissionCode
     *            the permission code to check for.
     * @return whether the user has the specified permission
     */
    public boolean hasSpecificPermissionTo(final String permissionCode) {
        boolean hasPermission = false;
        for (final Role role : this.roles) {
            if (role.hasPermissionTo(permissionCode)) {
                hasPermission = true;
                break;
            }
        }
        return hasPermission;
    }

    public void validateHasReadPermission(final String resourceType) {
        validateHasPermission("READ", resourceType);
    }

    public void validateHasCreatePermission(final String resourceType) {
        validateHasPermission("CREATE", resourceType);
    }

    public void validateHasUpdatePermission(final String resourceType) {
        validateHasPermission("UPDATE", resourceType);
    }

    private void validateHasPermission(final String prefix, final String resourceType) {
        final String authorizationMessage = "User has no authority to " + prefix + " " + resourceType.toLowerCase() + "s";
        final String matchPermission = prefix + "_" + resourceType.toUpperCase();

        if (!hasNotPermissionForAnyOf("ALL_FUNCTIONS", "ALL_FUNCTIONS_READ", matchPermission)) {
            return;
        }

        throw new NoAuthorizationException(authorizationMessage);
    }

    private boolean hasNotPermissionTo(final String permissionCode) {
        return !hasPermissionTo(permissionCode);
    }

    private boolean hasPermissionTo(final String permissionCode) {
        boolean hasPermission = hasAllFunctionsPermission();
        if (!hasPermission) {
            for (final Role role : this.roles) {
                if (role.hasPermissionTo(permissionCode)) {
                    hasPermission = true;
                    break;
                }
            }
        }
        return hasPermission;
    }

    private boolean hasAllFunctionsPermission() {
        boolean match = false;
        for (final Role role : this.roles) {
            if (role.hasPermissionTo("ALL_FUNCTIONS")) {
                match = true;
                break;
            }
        }
        return match;
    }

    private boolean hasNotAnyPermission(final List<String> permissions) {
        return !hasAnyPermission(permissions);
    }

    public boolean hasAnyPermission(String... permissions) {
        return hasAnyPermission(Arrays.asList(permissions));
    }

    public boolean hasAnyPermission(final List<String> permissions) {
        boolean hasAtLeastOneOf = false;

        for (final String permissionCode : permissions) {
            if (hasPermissionTo(permissionCode)) {
                hasAtLeastOneOf = true;
                break;
            }
        }

        return hasAtLeastOneOf;
    }

    public void validateHasPermissionTo(final String function) {
        if (hasNotPermissionTo(function)) {
            final String authorizationMessage = "User has no authority to: " + function;
            throw new NoAuthorizationException(authorizationMessage);
        }
    }

    public void validateHasReadPermission(final String function, final Long userId) {
        if (!("USER".equalsIgnoreCase(function) && userId.equals(getId()))) {
            validateHasReadPermission(function);
        }
    }

    public void validateHasCheckerPermissionTo(final String function) {
        final String checkerPermissionName = function.toUpperCase() + "_CHECKER";
        if (hasNotPermissionTo("CHECKER_SUPER_USER") && hasNotPermissionTo(checkerPermissionName)) {
            final String authorizationMessage = "User has no authority to be a checker for: " + function;
            throw new NoAuthorizationException(authorizationMessage);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || getClass() != o.getClass()) return false;

        AppUser user = (AppUser) o;

        return new EqualsBuilder()
                .appendSuper(super.equals(o)).append(getId(), user.getId())
                .append(getUsername(), user.getUsername())
                .isEquals();
    }

    @Override
    public int hashCode() {
        return new HashCodeBuilder(17, 37)
                .appendSuper(super.hashCode()).append(getId()).append(getUsername()).toHashCode();
    }
}
