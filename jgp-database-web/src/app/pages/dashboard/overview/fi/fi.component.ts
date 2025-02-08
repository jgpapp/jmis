import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-fi',
  standalone: true,
  imports: [],
  templateUrl: './fi.component.html',
  styleUrl: './fi.component.scss'
})
export class FiComponent {

  @Input('dashBoardFilters') dashBoardFilters: any;
}
