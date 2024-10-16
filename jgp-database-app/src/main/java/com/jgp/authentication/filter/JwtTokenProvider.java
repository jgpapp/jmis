package com.jgp.authentication.filter;

import com.jgp.authentication.domain.AppUser;
import com.jgp.authentication.domain.Role;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;

import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.security.Key;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.function.Function;

@Component
@Slf4j
public class JwtTokenProvider {

    private JwtTokenProvider(){}

    public static  <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = extractAllClaims(token);
        return claimsResolver.apply(claims);
    }

    private static Claims extractAllClaims(String jwtToken) {
        SecretKey secretKey = Keys.hmacShaKeyFor(SecurityConstants.SECRET.getBytes()); // key must be at least 256 bits

    return Jwts.parser().setSigningKey(secretKey)
              .build().parseClaimsJws(jwtToken).getBody();
    }

    private static boolean isTokenExpired(String token) {
        return extractExpiration(token).before(new Date());
    }

    private static Date extractExpiration(String token) {
        return extractClaim(token, Claims::getExpiration);
    }

    public static String extractUsername(String token) {
        return extractClaim(token, Claims::getSubject);
    }

    public static boolean isTokenValid(String token, UserDetails userDetails) {
        final String username = extractUsername(token);
        return (username.equals(userDetails.getUsername())) && !isTokenExpired(token);
    }

    public static String createToken(AppUser user) {

        SecretKey secretKey = Keys.hmacShaKeyFor(SecurityConstants.SECRET.getBytes()); // key must be at least 256 bits


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
                .expiration(new Date(System.currentTimeMillis() + SecurityConstants.EXPIRATION_TIME))
                .signWith(secretKey)
                .compact();
    }
}
