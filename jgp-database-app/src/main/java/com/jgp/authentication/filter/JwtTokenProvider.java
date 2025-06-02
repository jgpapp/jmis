package com.jgp.authentication.filter;

import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.domain.Role;
import com.jgp.authentication.exception.UserNotAuthenticatedException;
import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;

import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.Objects;
import java.util.function.Function;

@Component
@Slf4j
public class JwtTokenProvider {

    @Value("${jwt.secret}")
    private String jwtSecret;

    @Value("${jwt.expiration}")
    private long jwtExpirationInMs;

    @Value("${jwt.refresh.expiration}")
    private long jwtRefreshExpirationInMs;

    private JwtTokenProvider(){}

    private SecretKey getSigningKey() {
        byte[] keyBytes = jwtSecret.getBytes(StandardCharsets.UTF_8);
        return Keys.hmacShaKeyFor(keyBytes);
    }

    public  <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    private Claims extractAllClaims(String jwtToken) {
        return Jwts.parser()
                .verifyWith(getSigningKey())
                .build() // <-- THIS IS ESSENTIAL!
                .parseSignedClaims(jwtToken)
                .getPayload();
    }

    public boolean isTokenExpired(String token) {
        try {
            Jwts.parser()
                    .verifyWith(getSigningKey())
                    .build()
                    .parseSignedClaims(token);
            return false; // Token is valid and not expired
        } catch (Exception _) {
            return true; // Token is expired
        }
    }

    public boolean isValidToken(String authToken) {
        try {
            Jwts.parser()
                    .verifyWith(getSigningKey())
                    .build()
                    .parseSignedClaims(authToken);
            return true;
        } catch (MalformedJwtException ex) {
            // Invalid JWT token
            log.error("Invalid JWT token", ex);
            throw new UserNotAuthenticatedException("Invalid Credentials !!");
        } catch (ExpiredJwtException ex) {
            // Expired JWT token
            log.error("Expired JWT token", ex);
            throw new UserNotAuthenticatedException("Your Session Has Expired !!");
        } catch (UnsupportedJwtException ex) {
            // Unsupported JWT token
            log.error("Unsupported JWT token", ex);
            throw new UserNotAuthenticatedException("Invalid Credentials !!");
        } catch (IllegalArgumentException ex) {
            // JWT claims string is empty.
            log.error("JWT claims string is empty", ex);
            throw new UserNotAuthenticatedException("Invalid Credentials !!");
        }
    }

    public String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public boolean isTokenValid(String token, UserDetails userDetails) {
        final String username = extractUsername(token);
        return (username.equals(userDetails.getUsername())) && !isTokenExpired(token);
    }

    public String createToken(AppUser user) {

        return Jwts.builder()
                .subject(user.getUsername())
                .claim("user_id", user.getId())
                .claim("user_full_name", user.getUserFullName())
                .claim("user_email", user.getUsername())
                .claim("user_partner_name", Objects.isNull(user.getPartner()) ? null : user.getPartner().getPartnerName())
                .claim("user_partner_type", Objects.isNull(user.getPartner()) ? null : user.getPartner().getType())
                .claim("user_partner_id", Objects.isNull(user.getPartner()) ? null : user.getPartner().getId())
                .claim("user_position", user.getDesignation())
                .claim("user_registration", user.getDateCreated().format(DateTimeFormatter.ofPattern("MMM, yyyy")))
                .claim("user_roles", user.getRoles().stream().map(Role::getRoleName).collect(Collectors.toSet()))
                .claim("user_permissions", user.getAuthorities().stream().map(GrantedAuthority::getAuthority).collect(Collectors.toSet()))
                .claim("force_change_password", user.isForceChangePass())
                .issuedAt(new Date(System.currentTimeMillis()))
                .expiration(new Date(System.currentTimeMillis() + jwtExpirationInMs))
                .signWith(getSigningKey())
                .compact();
    }

    public String createRefreshToken(AppUser user) {

        return Jwts.builder()
                .subject(user.getUsername())
                .claim("user_id", user.getId())
                .claim("user_email", user.getUsername())
                .issuedAt(new Date(System.currentTimeMillis()))
                .expiration(new Date(System.currentTimeMillis() + jwtRefreshExpirationInMs))
                .signWith(getSigningKey())
                .compact();
    }
}
