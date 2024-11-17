import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorModule, PageEvent } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatSort } from '@angular/material/sort';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { PartnerDto } from '../../dto/Partner';
import { EditPartnerComponent } from './edit-partner/edit-partner.component';
import { PartnerService } from '@services/data-management/partners.service';
import { MatDialog } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';
import { NoPermissionComponent } from '../errors/no-permission/no-permission.component';
import { AuthService } from '@services/users/auth.service';
import { HasPermissionDirective } from '../../directives/has-permission.directive';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-partners',
  standalone: true,
  imports: [
    MatTableModule,
    MatPaginatorModule,
    ContentHeaderComponent,
    FlexLayoutModule,
    MatButtonModule,
    MatIconModule,
    MatFormFieldModule,
    RouterModule,
    CommonModule,
    NoPermissionComponent,
    HasPermissionDirective
  ],
  templateUrl: './partners.component.html',
  styleUrl: './partners.component.scss'
})
export class PartnersComponent implements OnInit, OnDestroy{
  public searchText: string;
  public page: any;
  pageSize = 10;
  pageIndex = 0;
  totalItems = 0;
  public showSearch: boolean = false;
  public viewType: string = 'grid';
  
  @ViewChild(MatPaginator) paginator: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;
  public displayedColumns = ['partnerName', 'type'];
  public dataSource: any;

  partners: any
  private unsubscribe$ = new Subject<void>();
  constructor(public dialog: MatDialog, private partnerService: PartnerService, public authService: AuthService) { }

  getAvailablePartners() {
    this.partnerService.getAvailablePartners(this.pageIndex, this.pageSize)
    .pipe(takeUntil(this.unsubscribe$))
      .subscribe({
        next: (response) => {
          this.partners = response.content;
          this.dataSource = new MatTableDataSource(this.partners);
          this.totalItems = response.page.totalElements;
        },
        error: (error) => { }
      });
  }

  onPageChange(event: PageEvent) {
    this.pageIndex = event.pageIndex;
    this.pageSize = event.pageSize;
    this.getAvailablePartners();
  }

  
  ngAfterViewInit() {
    if(this.dataSource){
      this.dataSource.paginator = this.paginator;
      this.dataSource.sort = this.sort;
    }
    
  }

  public changeView(viewType: string) {
    this.viewType = viewType;
    this.showSearch = false;
  }



  public openPartnerDialog(partner: PartnerDto | null) {
    let dialogRef = this.dialog.open(EditPartnerComponent, {
      data: partner
    });
    dialogRef.afterClosed().subscribe(() => {
      this.getAvailablePartners();
    });
  }

  ngOnInit(): void {
    this.getAvailablePartners();
  }


  ngOnDestroy(): void {
    this.unsubscribe$.next();
    this.unsubscribe$.complete();
  }
}
