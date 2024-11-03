import { AfterViewInit, Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { ClientService } from '@services/data-management/clients.service';
import { MatSort } from '@angular/material/sort';
import { RouterModule } from '@angular/router';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-clients',
  standalone: true,
  imports: [
    MatTableModule,
    MatPaginatorModule,
    ContentHeaderComponent,
    RouterModule,
    NoPermissionComponent
  ],
  templateUrl: './clients.component.html',
  styleUrl: './clients.component.scss'
 
})
export class ClientsComponent implements OnInit, AfterViewInit, OnDestroy{

  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['businessName', 'jgpId', 'phoneNumber', 'ownerGender', 'businessLocation'];
  public dataSource: any;

  participants: any
  totalItems = 0;
  private unsubscribe$ = new Subject<void>();
  constructor(private clientService: ClientService, public authService: AuthService) { }

  getAvailableClients(page: number, size: number) {
    this.clientService.getAvailableClients(page, size)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          console.log(response)
          this.participants = response.content;
          this.dataSource = new MatTableDataSource(this.participants);
          this.totalItems = response.totalElements;
        },
        error: (error) => { }
      });
  }

  
  ngAfterViewInit() {
    if(this.dataSource){
      this.dataSource.paginator = this.paginator;
      this.dataSource.sort = this.sort;
      this.paginator.page.subscribe(() => {
        this.getAvailableClients(this.paginator.pageIndex, this.paginator.pageSize);
      });
    }
    
  }

  ngOnInit(): void {
    this.getAvailableClients(0, 10);
  }

  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
