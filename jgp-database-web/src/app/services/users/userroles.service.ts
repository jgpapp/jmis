import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { UserRoleDto } from '../../dto/UserRoleDto';

@Injectable({
  providedIn: 'root'
})
export class UserRoleService {

  constructor(private httpClient: HttpClient) { }

  getAvailableUserRoles(): Observable<any> {
    return this.httpClient.get(`/roles`);
  }

  createUserRole(userRole: UserRoleDto): Observable<any> {
    return this.httpClient.post(`/roles`, JSON.stringify(userRole));
  }

  updateUserRole(roleId: number, userRole: UserRoleDto): Observable<any> {
    return this.httpClient.put(`/roles/${roleId}`, JSON.stringify(userRole));
  }

  updateRolePermissions(roleId: number, permissions: string[]): Observable<any> {
    return this.httpClient.put(`/roles/${roleId}/update-permissions`, JSON.stringify(permissions));
  }

  getUserRoleById(roleId: number | string | null): Observable<any> {
    return this.httpClient.get(`/roles/${roleId}`);
  }

  deleteUserRoleById(roleId: number): Observable<any> {
    return this.httpClient.delete(`/roles/${roleId}`);
  }
}