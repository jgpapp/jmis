import { Component, Input } from '@angular/core';

@Component({
  selector: 'app-ta',
  standalone: true,
  imports: [],
  templateUrl: './ta.component.html',
  styleUrl: './ta.component.scss'
})
export class TaComponent {

  @Input('dashBoardFilters') dashBoardFilters: any;
}
