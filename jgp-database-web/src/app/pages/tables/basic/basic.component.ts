import { Component } from '@angular/core';
import { MatTableModule, MatTableDataSource } from '@angular/material/table';
import { TablesService, Element } from '@services/tables.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-basic',
    imports: [
        MatTableModule,
        ContentHeaderComponent
    ],
    templateUrl: './basic.component.html',
    providers: [
        TablesService
    ]
})
export class BasicComponent {
  public displayedColumns = ['position', 'name', 'weight', 'symbol'];
  public dataSource: any; 
  constructor(private tablesService: TablesService) { 
    this.dataSource = new MatTableDataSource<Element>(this.tablesService.getData());
  }
}
