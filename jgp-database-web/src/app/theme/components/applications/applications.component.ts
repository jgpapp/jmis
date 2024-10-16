import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatGridListModule } from '@angular/material/grid-list';
import { MatCardModule } from '@angular/material/card';
import { MatMenuModule } from '@angular/material/menu';

@Component({
  selector: 'app-applications',
  standalone: true,
  imports: [
    MatButtonModule,
    MatIconModule,
    MatGridListModule,
    MatCardModule,
    MatMenuModule
  ],
  templateUrl: './applications.component.html',
  styleUrls: ['./applications.component.scss'],
  encapsulation: ViewEncapsulation.None
})
export class ApplicationsComponent {
  
}