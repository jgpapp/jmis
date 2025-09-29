import { Component, inject } from '@angular/core';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { IconsService } from '@services/icons.service';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';
import { MatButtonModule } from '@angular/material/button';

@Component({
    selector: 'app-icons',
    imports: [
        FlexLayoutModule,
        MatCardModule,
        MatIconModule,
        MatButtonModule,
        ContentHeaderComponent
    ],
    templateUrl: './icons.component.html',
    providers: [
        IconsService
    ]
})
export class IconsComponent {
  icons: any; 
  iconsService = inject(IconsService); 

  ngOnInit() {
    this.icons = this.iconsService.getIcons();
  }
}
