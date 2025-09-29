import { Component, ViewChild } from '@angular/core';
import { MatInputModule } from '@angular/material/input';
import { NgxDatatableModule, DatatableComponent, SelectionType } from '@swimlane/ngx-datatable';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-ngx-table',
    imports: [
        NgxDatatableModule,
        MatInputModule,
        ContentHeaderComponent
    ],
    templateUrl: './ngx-table.component.html'
})
export class NgxTableComponent {
  @ViewChild(DatatableComponent) table: DatatableComponent;
  editing: any = {};
  rows: any[] = [];
  temp: any[] = [];
  selected: any[] = [];
  loadingIndicator: boolean = true;
  reorderable: boolean = true;
  columns = [
    { prop: 'name' },
    { name: 'Gender' },
    { name: 'Company' }
  ];
  selection: SelectionType;

  constructor() {
    this.selection = SelectionType.checkbox;
    this.fetch((data: any) => {
      this.temp = [...data];
      this.rows = data;
      setTimeout(() => { this.loadingIndicator = false; }, 1500);
    });
  }

  fetch(data: any) {
    const req = new XMLHttpRequest();
    req.open('GET', 'data/company.json');
    req.onload = () => {
      data(JSON.parse(req.response));
    };
    req.send();
  }

  updateFilter(event: any) {
    const val = event.target.value.toLowerCase();
    const temp = this.temp.filter(function (d) {
      return d.name.toLowerCase().indexOf(val) !== -1 || !val;
    });
    this.rows = temp;
    this.table.offset = 0;
  }

  updateValue(event: any, cell: any, rowIndex: any) {
    this.editing[rowIndex + '-' + cell] = false;
    this.rows[rowIndex][cell] = event.target.value;
    this.rows = [...this.rows];
  }

  onSelect({ selected }: any) {
    console.log('Select Event', selected, this.selected);
    this.selected.splice(0, this.selected.length);
    this.selected.push(...selected);
  }

  onActivate(event: any) {
    console.log('Activate Event', event);
  }

}