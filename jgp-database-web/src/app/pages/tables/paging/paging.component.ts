import { Component, ViewChild } from '@angular/core';
import { MatPaginator, MatPaginatorModule } from '@angular/material/paginator';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { TablesService, Element } from '@services/tables.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-paging',
    imports: [
        MatTableModule,
        MatPaginatorModule,
        ContentHeaderComponent
    ],
    templateUrl: './paging.component.html',
    providers: [
        TablesService
    ]
})
export class PagingComponent {
  @ViewChild(MatPaginator) paginator: MatPaginator;
  public displayedColumns = ['position', 'name', 'weight', 'symbol'];
  public dataSource: any;
  constructor(private tablesService: TablesService) {
    this.dataSource = new MatTableDataSource<Element>(this.tablesService.getData());
  }
  
  ngAfterViewInit() {
    this.dataSource.paginator = this.paginator;
  }
}
