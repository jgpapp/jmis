import { inject } from "@angular/core";
import { ResolveFn } from "@angular/router";
import { UserService } from "@services/users/user.service";

export const UserResolver: ResolveFn<Object> = (route, state) => {
    const userId = route.paramMap.get('id');
    return inject(UserService).getUserById(userId)
}