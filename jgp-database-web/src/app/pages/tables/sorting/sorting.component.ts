import { Component, ViewChild } from '@angular/core';
import { MatSort, MatSortModule } from '@angular/material/sort';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { TablesService, Element } from '@services/tables.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-sorting',
    imports: [
        MatTableModule,
        MatSortModule,
        ContentHeaderComponent
    ],
    templateUrl: './sorting.component.html',
    providers: [
        TablesService
    ]
})
export class SortingComponent {
  @ViewChild(MatSort) sort: MatSort;
  public displayedColumns = ['position', 'name', 'weight', 'symbol'];
  public dataSource: any;
  constructor(private tablesService: TablesService) { 
    this.dataSource = new MatTableDataSource<Element>(this.tablesService.getData());
  }
  
  ngAfterViewInit() {
    this.dataSource.sort = this.sort;
  }
}
