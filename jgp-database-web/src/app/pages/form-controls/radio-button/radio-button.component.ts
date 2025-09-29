import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { MatCardModule } from '@angular/material/card';
import { MatRadioModule } from '@angular/material/radio';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../../theme/components/content-header/content-header.component';

@Component({
    selector: 'app-radio-button',
    imports: [
        FlexLayoutModule,
        MatCardModule,
        MatRadioModule,
        FormsModule,
        ContentHeaderComponent
    ],
    templateUrl: './radio-button.component.html'
})
export class RadioButtonComponent {
  favoriteSeason: string;
  seasons = [
    'Winter',
    'Spring',
    'Summer',
    'Autumn',
  ];
}
