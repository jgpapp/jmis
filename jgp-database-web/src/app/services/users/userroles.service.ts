import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Observable } from 'rxjs';
import { UserDto } from '../../dto/UserDto';
import { UserRoleDto } from '../../dto/UserRoleDto';

@Injectable({
  providedIn: 'root'
})
export class UserRoleService {

  constructor(private httpClient: HttpClient, private globalService: GlobalService) { }

  getAvailableUserRoles(): Observable<any> {
    return this.httpClient.get(`${this.globalService.BASE_API_URL}/roles`);
  }

  createUserRole(userRole: UserRoleDto): Observable<any> {
    return this.httpClient.post(`${this.globalService.BASE_API_URL}/roles`, JSON.stringify(userRole));
  }

  updateUserRole(roleId: number, userRole: UserRoleDto): Observable<any> {
    return this.httpClient.put(`${this.globalService.BASE_API_URL}/roles/${roleId}`, JSON.stringify(userRole));
  }

  updateRolePermissions(roleId: number, permissions: string[]): Observable<any> {
    return this.httpClient.put(`${this.globalService.BASE_API_URL}/roles/${roleId}/update-permissions`, JSON.stringify(permissions));
  }

  getUserRoleById(roleId: number | string | null): Observable<any> {
    return this.httpClient.get(`${this.globalService.BASE_API_URL}/roles/${roleId}`);
  }

  deleteUserRoleById(roleId: number): Observable<any> {
    return this.httpClient.delete(`${this.globalService.BASE_API_URL}/roles/${roleId}`);
  }
}