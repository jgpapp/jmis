import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDividerModule } from '@angular/material/divider';
import { MatRadioModule } from '@angular/material/radio';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-slide-toggle',
  standalone: true,
  imports: [
    FormsModule,
    MatCardModule,
    MatRadioModule,
    MatDividerModule,
    MatSlideToggleModule,
    MatCheckboxModule,
    ContentHeaderComponent
  ],
  templateUrl: './slide-toggle.component.html'
})
export class SlideToggleComponent {
  color = 'accent';
  checked = false;
  disabled = false;
}