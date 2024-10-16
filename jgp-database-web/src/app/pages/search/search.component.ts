import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, RouterModule } from '@angular/router';
import { FlexLayoutModule } from '@ngbracket/ngx-layout';
import { ContentHeaderComponent } from '../../theme/components/content-header/content-header.component';

@Component({
  selector: 'app-search',
  standalone: true,
  imports: [
    FlexLayoutModule,
    ContentHeaderComponent
  ],
  templateUrl: './search.component.html'
})
export class SearchComponent implements OnInit {
  param: any;
  title: string = 'Nothing Found';
  description: string = 'Sorry, but nothing matched your search terms. Please try again with some different keywords.';
  constructor(private activatedRoute: ActivatedRoute) { }

  ngOnInit() {
    this.activatedRoute.params.subscribe(params => {
      if (params['name']) {
        this.param = params['name'];
        this.title = 'Search results';
        this.description = 'The following results were found for ' + params['name'];
      }
    });
  }

}