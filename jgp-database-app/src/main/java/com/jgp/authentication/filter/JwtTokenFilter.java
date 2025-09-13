package com.jgp.authentication.filter;

import com.jgp.authentication.service.ActiveUserService;
import io.jsonwebtoken.JwtException;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class JwtTokenFilter extends OncePerRequestFilter {

    private final UserDetailsService currentUserDetails;
    private final JwtTokenProvider jwtTokenProvider;
    private final ActiveUserService activeUserService;
    private static final long TWO_MINUTES = 120_000L; //2 * 60 * 1000; // 120_000



    @Override
    protected void doFilterInternal(HttpServletRequest request,
                                    HttpServletResponse response,
                                    FilterChain chain) throws IOException, ServletException {
        final var header = request.getHeader(SecurityConstants.HEADER_AUTHORIZATION);
        if (header == null || !header.startsWith(SecurityConstants.TOKEN_PREFIX)) {
            chain.doFilter(request, response);
            return;
        }
        final var token = header.replaceFirst(SecurityConstants.TOKEN_PREFIX, "").trim();
        UserDetails userDetails = null;
        try {
            userDetails = currentUserDetails
                    .loadUserByUsername(this.jwtTokenProvider.extractUsername(token));
        }catch (JwtException e){
            log.error("Auth Error: {}", e.getMessage());
        }
        if (Objects.isNull(userDetails) || !this.jwtTokenProvider.isTokenValid(token, userDetails)) {
            chain.doFilter(request, response);
            return;
        }

        final var authentication = new UsernamePasswordAuthenticationToken(
                userDetails, null, userDetails.getAuthorities());

        if (authentication.isAuthenticated() && !("anonymousUser").equals(authentication.getPrincipal())) {
            String username = authentication.getName();
            activeUserService.userActivity(username);
            activeUserService.removeExpiredUsers(TWO_MINUTES);
        }

        authentication.setDetails(
                new WebAuthenticationDetailsSource().buildDetails(request)
        );
        SecurityContextHolder.getContext().setAuthentication(authentication);
        chain.doFilter(request, response);
    }
}
