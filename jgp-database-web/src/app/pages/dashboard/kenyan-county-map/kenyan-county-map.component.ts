import { Component, ElementRef, Input, ViewChild } from '@angular/core';
import * as d3 from 'd3';
import * as topojson from 'topojson-client'; // For TopoJSON parsing
import { FeatureCollection, Geometry, GeoJsonProperties, Feature } from 'geojson';
import { DashboardService } from '@services/dashboard/dashboard.service';


@Component({
  selector: 'app-kenyan-county-map',
  standalone: true,
  imports: [],
  templateUrl: './kenyan-county-map.component.html',
  styleUrl: './kenyan-county-map.component.scss'
})
export class KenyanCountyMapComponent {

  @ViewChild('choropleth', { static: true }) private chartContainer!: ElementRef;
  @Input('countyData') countyData: Map<number, any>;
  @Input('countyDataToBePicked') countyDataToBePicked: any;

  private svg: any;
  private width = 975;
  private height = 610;

  constructor(private dashboardService: DashboardService) {}

  ngOnInit(): void {
    this.createChoropleth();
  }


  private createChoropleth(): void {
    const element = this.chartContainer.nativeElement;

    // Set up the SVG canvas
    this.svg = d3.select(element)
      .append('svg')
      .attr('width', this.width)
      .attr('height', this.height)
      .attr('viewBox', [0, 0, this.width, this.height].toString())
      .style('border', '1px solid black');

    // Load the data (both the US map and the unemployment data)
   d3.json('data/kenya-counties.json').then((kenya: any) => {

      

      // Color scale
      const color = d3.scaleQuantize([1, 10], d3.schemeBlues[9]);

      // Projection and path generator
      const path = d3.geoPath();

      // Draw the map
      this.svg.append('g')
        .selectAll('path')
        .data(kenya.features)
        .join('path')
        .attr('fill', 'black')
        .attr('d', path)
        .append('title')
        .text((d: any) => {
          const countyCode = d.properties.COUNTY_COD;
          const dataMap = new Map(Object.entries(this.countyData))
          const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : '';
          return `County: ${d.properties?.COUNTY_NAM}\nTrained: ${dataToDisplay || 0}`
        }
          
        );

      // Draw the state borders
      this.svg.append('path')
        .datum(topojson.mesh(kenya, kenya.objects.states, (a, b) => a !== b))
        .attr('fill', 'none')
        .attr('stroke', 'white')
        .attr('stroke-linejoin', 'round')
        .attr('d', path);
    });
  }

}
