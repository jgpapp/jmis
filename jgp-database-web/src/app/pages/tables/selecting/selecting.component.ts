import { SelectionModel } from '@angular/cdk/collections';
import { Component, OnInit } from '@angular/core';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { TablesService, Element } from '@services/tables.service';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-selecting',
  standalone: true,
  imports: [
    MatTableModule,
    MatCheckboxModule,
    ContentHeaderComponent
  ],
  templateUrl: './selecting.component.html',
  providers: [
    TablesService
  ]
})
export class SelectingComponent implements OnInit {
  public displayedColumns: string[] = ['select', 'position', 'name', 'weight', 'symbol'];
  public dataSource: any;
  public selection = new SelectionModel<Element>(true, []);
  constructor(private tablesService: TablesService) { }

  ngOnInit() {
    this.dataSource = new MatTableDataSource<Element>(this.tablesService.getData());
  }

  isAllSelected() {
    const numSelected = this.selection.selected.length;
    const numRows = this.dataSource.data.length;
    return numSelected === numRows;
  }

  masterToggle() {
    this.isAllSelected() ?
      this.selection.clear() :
      this.dataSource.data.forEach((row: any) => this.selection.select(row));
  }
}
