export interface CurrentUserCredentials {
    accessToken?: string;
    userFullName?: string;
    email: string;
    desgnation?: string;
    partnerId?: number;
    partnerName?: string;
    partnerType?: string;
    permissions?: any;
    roles?: any;
    username: string;
    registration: string;
    rememberMe: boolean;
    forceChangePassword: boolean;
}