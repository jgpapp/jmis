import { Component, OnInit } from '@angular/core';
import { MatButtonModule } from '@angular/material/button';
import { MatCardModule } from '@angular/material/card';
import { MatDividerModule } from '@angular/material/divider';
import { MatIconModule } from '@angular/material/icon';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import {
  NguCarousel,
  NguCarouselConfig,
  NguCarouselDefDirective,
  NguCarouselNextDirective,
  NguCarouselPrevDirective,
  NguItemComponent,
  NguTileComponent
} from '@ngu/carousel';
import { LandingService } from '@services/landing.service';
import { Settings, SettingsService } from '@services/settings.service';

@Component({
    selector: 'app-our-works',
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
        MatButtonModule,
        MatDividerModule
    ],
    templateUrl: './our-works.component.html',
    providers: [LandingService]
})
export class OurWorksComponent implements OnInit {
  public works: any;
  public worksCarouselConfig: NguCarouselConfig;
  public settings: Settings;
  constructor(public settingsService: SettingsService, private landingService: LandingService) {
    this.settings = this.settingsService.settings;
  }

  ngOnInit() {
    this.works = this.landingService.getWorks();
    this.worksCarouselConfig = {
      grid: { xs: 1, sm: 2, md: 3, lg: 4, all: 0 },
      slide: 1,
      speed: 400,
      interval: {
        timing: 4000
      },
      point: {
        visible: false
      },
      loop: true,
      touch: true,
      custom: 'banner',
      RTL: this.settings.rtl
    };
  }

}