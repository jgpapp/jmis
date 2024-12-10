import { Component, ElementRef, Input, OnInit } from '@angular/core';
import * as d3 from 'd3';

@Component({
  selector: 'app-kenyan-map',
  standalone: true,
  imports: [],
  templateUrl: './kenyan-map.component.html',
  styleUrl: './kenyan-map.component.scss'
})
export class KenyanMapComponent implements OnInit {

  @Input('mapWidth') mapWidth: number;
  @Input('mapHeight') mapHeight: number;
  @Input('countyData') countyData: Map<number, any>;
  @Input('countyDataToBePicked') countyDataToBePicked: any;
  @Input('kenyanMapChartContainer') kenyanMapChartContainer: ElementRef;
  private margin: { top: number, bottom: number, left: number; right: number} =  {top: 20, bottom: 30, left: 30, right: 20};
  private svg: any;
  private projection: any;
  private path: any; 

  constructor() { }

  ngOnInit(): void {
    this.createSvg();
    this.drawMap();
  }

  private createSvg(): void {

    //const element = this.kenyanMapChartContainer.nativeElement;
    //const contentWidth = element.offsetWidth - this.margin.left - this.margin.right;
    //const contentHeight = element.offsetHeight - this.margin.top - this.margin.bottom;
    

    this.svg = d3.select("figure#map")
      .append("svg")
      .attr("width", this.mapWidth)
      .attr("height", this.mapHeight);

    this.projection = d3.geoMercator()
      .scale(1980)
      .center([37.9062, 0.0236])  // Kenya's center
      .translate([this.mapWidth / 2, this.mapHeight / 2]);

    this.path = d3.geoPath().projection(this.projection);
  }

  private drawMap(): void {
    // Color scale
    const color = d3.scaleQuantize([1, 10], d3.schemeBlues[9]);
    // Load the GeoJSON data for Kenya counties
    d3.json('data/kenya-counties.json').then((data: any) => {

      // Draw counties
      const counties = this.svg.append("g")
        .selectAll("path")
        .data(data.features)
        .enter()
        .append("path")
        .attr("d", this.path)
        .attr("fill", "#69b3a2")
        .attr("stroke", "#000")
        .attr("stroke-width", 0.5)
        // Use arrow functions to preserve the 'this' context
        .on("mouseover", (event: any, d: any) => {
          d3.select(event.currentTarget).attr("fill", "#2ca25f");
          const [x, y] = this.path.centroid(d);  // 'this' refers to the class
          const countyCode = d.properties.COUNTY_COD;
          const dataMap = new Map(Object.entries(this.countyData))
          const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : '';
          this.showCountyName(x, y, dataToDisplay);  // Call a method on the component
        })
        .on("mouseout", (event: any, d: any) => {
          d3.select(event.currentTarget).attr("fill", "#69b3a2");
          this.hideCountyName();  // Hide the county name when mouseout
        });

      // Add county names inside the county
      this.svg.append("g")
        .selectAll("text")
        .data(data.features)
        .enter()
        .append("text")
        .attr("x", (d: any) => this.path.centroid(d)[0])  // Position based on the centroid of the county
        .attr("y", (d: any) => this.path.centroid(d)[1])
        .attr("text-anchor", "middle")
        .attr("font-size", "10px")
        .attr("fill", "black")
        .text((d: any) => d.properties.COUNTY_NAM);  // Set the county name

    }).catch((error: any) => {
      console.error('Error loading Kenya counties GeoJSON:', error);
    });
  }

  private showCountyName(x: number, y: number, name: string): void {
    // Custom method to show the county name
    this.svg.append("text")
      .attr("x", x)
      .attr("y", y - 10)
      .attr("class", "county-name")
      .attr("text-anchor", "middle")
      .attr("font-size", "12px")
      .attr("fill", "black")
      .text(name);
  }

  private hideCountyName(): void {
    // Custom method to hide the county name
    this.svg.selectAll(".county-name").remove();
  }
}
