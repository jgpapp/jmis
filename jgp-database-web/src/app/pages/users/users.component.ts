import { Component, OnDestroy, OnInit, ViewEncapsulation } from '@angular/core';
import { User } from '../../common/models/user.model';
import { MatDialog } from '@angular/material/dialog';
import { UserDialogComponent } from './user-dialog/user-dialog.component'; 
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatButtonToggleModule } from '@angular/material/button-toggle';
import { MatIconModule } from '@angular/material/icon';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatProgressSpinnerModule } from '@angular/material/progress-spinner';
import { FormsModule } from '@angular/forms';
import { NgxPaginationModule } from 'ngx-pagination';
import { MatMenuModule } from '@angular/material/menu';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { PipesModule } from '../../theme/pipes/pipes.module';
import { MatCardModule } from '@angular/material/card';
import { NgClass } from '@angular/common';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatDividerModule } from '@angular/material/divider';
import { UserService } from '@services/users/user.service';
import { RouterModule } from '@angular/router';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { HasPermissionDirective } from '../../directives/has-permission.directive';
import { SubscriptionsContainer } from '../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-users',
    imports: [
        FormsModule,
        FlexLayoutModule,
        MatButtonModule,
        MatButtonToggleModule,
        MatIconModule,
        MatFormFieldModule,
        MatInputModule,
        MatProgressSpinnerModule,
        MatMenuModule,
        MatSlideToggleModule,
        MatCardModule,
        MatDividerModule,
        NgxPaginationModule,
        PipesModule,
        ContentHeaderComponent,
        NgClass,
        RouterModule,
        NoPermissionComponent,
        HasPermissionDirective
    ],
    templateUrl: './users.component.html',
    styleUrl: './users.component.scss',
    encapsulation: ViewEncapsulation.None
})
export class UsersComponent implements OnInit, OnDestroy {
  public users: User[] | null;
  public searchText: string;
  public page: any;
  public showSearch: boolean = false;
  public viewType: string = 'grid';
  public userImage = "img/users/default-user.jpg";

  subs = new SubscriptionsContainer();
  constructor(public dialog: MatDialog, public userService: UserService, public authService: AuthService) { }

  ngOnInit() {
    this.getUsers();
  }

  public getUsers(): void {
    this.users = null; //for show spinner each time
    this.subs.add = this.userService.getAvailableUsers()
    .subscribe(users => this.users = users);
  }
  public addUser(user: User) {
    //this.userService.createUser(user).subscribe(user => this.getUsers());
  }
  public updateUser(user: User) {
    //this.userService.updateUser(user).subscribe(user => this.getUsers());
  }
  public deleteUser(user: User) {
    
  }

  public changeView(viewType: string) {
    this.viewType = viewType;
    this.showSearch = false;
  }

  public onPageChanged(event: any) {
    this.page = event;
    this.getUsers();
    const mainContent = document.getElementById('main');
    if (mainContent) {
      mainContent.scrollTop = 0;
    }
  }

  public openUserDialog(user: User | null) {
    let dialogRef = this.dialog.open(UserDialogComponent, {
      data: user
    });
    dialogRef.afterClosed().subscribe(() => {
      this.getUsers();
    });
    this.showSearch = false;
  }


  ngOnDestroy(): void {
    this.subs.dispose();
  }
}