import { Component, OnInit, ViewEncapsulation } from '@angular/core';
import { MAT_SELECT_CONFIG, MatSelectModule } from '@angular/material/select';
import { Menu } from '../../../common/models/menu.model';
import { MenuService } from '@services/menu.service';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatTooltipModule } from '@angular/material/tooltip';
import { RouterModule } from '@angular/router';

@Component({
    selector: 'app-favorites',
    standalone: true,
    imports: [
        RouterModule,
        FlexLayoutModule,
        MatIconModule,
        MatButtonModule,
        MatSelectModule,
        MatTooltipModule
    ],
    templateUrl: './favorites.component.html',
    styleUrls: ['./favorites.component.scss'],
    encapsulation: ViewEncapsulation.None,
    providers: [
        {
            provide: MAT_SELECT_CONFIG,
            useValue: { overlayPanelClass: 'w-auto' }
        }
    ]
})
export class FavoritesComponent implements OnInit {
  toppings: any;
  toppingList: string[] = ['Extra cheese', 'Mushroom', 'Onion', 'Pepperoni', 'Sausage', 'Tomato'];
  
  public menuItems: Array<Menu>;
  public favorites: any; 
  constructor(public menuService: MenuService) { }

  ngOnInit() {
    this.menuItems = this.menuService.getVerticalMenuItems().filter(menu => menu.routerLink != null || menu.href !=null);
  }

}
