import { inject } from "@angular/core";
import { ResolveFn } from "@angular/router";
import { UserRoleService } from "@services/users/userroles.service";

export const userRoleResolver: ResolveFn<Object> = (route, state) => {
    const roleId = route.paramMap.get('id');
    return inject(UserRoleService).getUserRoleById(roleId)
}