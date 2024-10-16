import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { GlobalService } from '../shared/global.service';
import { Observable } from 'rxjs';
import { User } from '../../common/models/user.model';

@Injectable({
  providedIn: 'root'
})
export class UserService {

  constructor(private httpClient: HttpClient, private globalService: GlobalService) { }

  getAvailableUsers(): Observable<any> {
    return this.httpClient.get(`${this.globalService.BASE_API_URL}/users`);
  }

  createUser(user: User): Observable<any> {
    return this.httpClient.post(`${this.globalService.BASE_API_URL}/users`, JSON.stringify(user));
  }

  updateUser(userId: number, user: User): Observable<any> {
    return this.httpClient.put(`${this.globalService.BASE_API_URL}/users/${userId}`, JSON.stringify(user));
  }

  updateUserPassword(userPassDto: {password: string, newPass: string, passConfirm: string}): Observable<any> {
    return this.httpClient.put(`${this.globalService.BASE_API_URL}/users/change-password`, JSON.stringify(userPassDto));
  }

  getUserById(userId: number | string | null): Observable<any> {
    return this.httpClient.get(`${this.globalService.BASE_API_URL}/users/${userId}`);
  }
}
