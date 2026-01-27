import { NgClass } from '@angular/common';
import { Component, Input } from '@angular/core';
import { MatIconModule } from '@angular/material/icon';
import { BreadcrumbComponent } from '../breadcrumb/breadcrumb.component';

@Component({
    selector: 'app-content-header',
    standalone: true,
    imports: [
        NgClass,
        MatIconModule,
        BreadcrumbComponent
    ],
    templateUrl: './content-header.component.html',
    styleUrl: './content-header.component.scss'
})
export class ContentHeaderComponent {
  @Input('icon') icon: any;
  @Input('title') title: any;
  @Input('desc') desc: any;
  @Input('hideBreadcrumb') hideBreadcrumb: boolean = false;
  @Input('hasBgImage') hasBgImage: boolean = false;
  @Input('class') class: any;
}
