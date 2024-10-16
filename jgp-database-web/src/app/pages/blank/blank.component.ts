import { Component } from '@angular/core';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';

@Component({
  selector: 'app-blank',
  standalone: true,
  imports: [
    FlexLayoutModule,
    ContentHeaderComponent
  ],
  templateUrl: './blank.component.html',
  styleUrl: './blank.component.scss'
})
export class BlankComponent {

}
