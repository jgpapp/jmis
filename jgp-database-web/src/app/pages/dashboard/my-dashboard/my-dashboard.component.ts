import { Component } from '@angular/core';
import { AuthService } from '@services/users/auth.service';

@Component({
  selector: 'app-my-dashboard',
  standalone: true,
  templateUrl: './my-dashboard.component.html',
  styleUrl: './my-dashboard.component.scss'
})
export class MyDashboardComponent {

  partnerType: string | undefined = 'NONE';
  constructor(public authService: AuthService){}

  ngOnInit(): void {
    this.partnerType = this.authService.currentUser()?.partnerType === '-' ? 'NONE' : this.authService.currentUser()?.partnerType;
  }
}
