import { ChangeDetectionStrategy, Component, OnInit } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import {
  NguCarousel,
  NguCarouselConfig,
  NguCarouselDefDirective,
  NguCarouselNextDirective,
  NguCarouselPrevDirective,
  NguTileComponent
} from '@ngu/carousel';
import { LandingService } from '@services/landing.service';
import { Settings, SettingsService } from '@services/settings.service';

@Component({
    selector: 'app-our-services',
    imports: [
        NguCarousel,
        NguTileComponent,
        NguCarousel,
        NguCarouselDefDirective,
        NguCarouselNextDirective,
        NguCarouselPrevDirective,
        FlexLayoutModule,
        MatCardModule,
        MatIconModule,
        MatButtonModule
    ],
    templateUrl: './our-services.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
    providers: [LandingService]
})
export class OurServicesComponent implements OnInit {
  public services: any[];
  public servicesCarouselConfig: NguCarouselConfig;
  public settings: Settings;
  constructor(public settingsService: SettingsService, private landingService: LandingService) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    this.services = this.landingService.getServices();
    this.servicesCarouselConfig = {
      grid: { xs: 1, sm: 2, md: 3, lg: 4, all: 0 },
      slide: 1,
      speed: 500,
      interval: {
        timing: 4000
      },
      point: {
        visible: true
      },
      loop: true,
      touch: true,
      custom: 'banner',
      RTL: this.settings.rtl
    };

  }

}