import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { ClientService } from '@services/data-management/clients.service';
import { MatSort } from '@angular/material/sort';
import { RouterModule } from '@angular/router';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { FormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { GlobalService } from '@services/shared/global.service';
import { SubscriptionsContainer } from '../../theme/utils/subscriptions-container';

@Component({
    selector: 'app-clients',
    standalone: true,
    imports: [
        MatTableModule,
        MatPaginatorModule,
        ContentHeaderComponent,
        RouterModule,
        NoPermissionComponent,
        MatFormFieldModule,
        MatInputModule,
        FormsModule,
        MatButtonModule,
        MatIconModule
    ],
    templateUrl: './clients.component.html',
    styleUrl: './clients.component.scss'
})
export class ClientsComponent implements OnInit, OnDestroy{

  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['participantName', 'jgpId', 'phoneNumber', 'ownerGender', 'businessLocation'];
  public dataSource: any;

  participants: any
  public searchTextInput: string | null;
  searchText: string | null;
  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;
  subs = new SubscriptionsContainer();
  constructor(private clientService: ClientService, public authService: AuthService, public gs: GlobalService) { }

  getAvailableClients() {
    this.subs.add = this.clientService.getAvailableClients(this.searchText, this.pageIndex, this.pageSize)
      .subscribe({
        next: (response) => {
          this.participants = response.content;
          this.dataSource = new MatTableDataSource(this.participants);
          this.totalItems = response.totalElements;
        },
        error: (error) => { }
      });
  }

  onChange(event: Event) {
    if(this.searchTextInput && this.searchTextInput.length > 2){
      this.searchText = this.searchTextInput;
      this.getAvailableClients();
    }else {
      this.searchText = null;
      this.getAvailableClients();
    }
  }

  onPageChange(event: PageEvent) {
    this.pageIndex = event.pageIndex;
    this.pageSize = event.pageSize;
    this.getAvailableClients();
  }

  ngOnInit(): void {
    this.getAvailableClients();
  }

  ngOnDestroy(): void {
    this.subs.dispose();
  }
}
