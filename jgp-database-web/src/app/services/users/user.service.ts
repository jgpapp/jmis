import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { User } from '../../common/models/user.model';

@Injectable({
  providedIn: 'root'
})
export class UserService {

  constructor(private httpClient: HttpClient) { }

  getAvailableUsers(): Observable<any> {
    return this.httpClient.get(`/users`);
  }

  createUser(user: User): Observable<any> {
    return this.httpClient.post(`/users`, JSON.stringify(user));
  }

  updateUser(userId: number, user: User): Observable<any> {
    return this.httpClient.put(`/users/${userId}`, JSON.stringify(user));
  }

  lockOrUnlockUser(selectedUser: any): Observable<any> {
    const newUserStatus = selectedUser.isActive ? 'INACTIVE' : 'ACTIVE';
    return this.httpClient.put(`/users/${selectedUser.id}/change-user-status/${newUserStatus}`, '');
  }

  updateUserPassword(userPassDto: {password: string, newPass: string, passConfirm: string}): Observable<any> {
    return this.httpClient.put(`/users/change-password`, JSON.stringify(userPassDto));
  }

  resetUserPassword(userId: number): Observable<any> {
    return this.httpClient.put(`/users/reset-user-password/${userId}`, JSON.stringify({}));
  }

  getUserById(userId: number | string | null): Observable<any> {
    return this.httpClient.get(`/users/${userId}`);
  }
}
