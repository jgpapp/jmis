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
  @Input('kenyanMapChartContainer') kenyanMapChartContainer!: ElementRef;
  private margin: { top: number, bottom: number, left: number; right: number} =  {top: 20, bottom: 30, left: 30, right: 20};
  private svg: any;
  private projection: any;
  private path: any; 
  private kenyanMapJson = 'data/kenya-counties.json';
  private kenyanGeoJson: any;

  public chartSColorScheme: any = [
      '#FF671B',
      '#8DB92E',
      '#4FCDB0',
      '#DE3C95',
      '#F38B00',
      '#2F7B6B',
      '#D22A2F',
      '#FFC81F',
      '#2F3E9E', 
      '#D22E2E', 
      '#378D3B', 
      '#7f7f7f', 
      '#c4a678', 
      '#6a7b6a', 
      '#191919', 
      '#3d144c', 
      '#f0e1dc', 
      '#a04324', 
      '#00ffff', 
      '#0e5600', 
      '#0e9697'
    ];

  constructor() { }

  ngOnInit(): void {
    this.createSvg();
    this.drawMap();
  }

  private createSvg(): void {

    const element = this.kenyanMapChartContainer.nativeElement;
    const containerWidth = element.offsetWidth - this.margin.left - this.margin.right;
    const containerHeight = element.offsetHeight - this.margin.top - this.margin.bottom;
    console.log('Width: '+containerWidth+" Height: "+containerHeight)

    d3.json(this.kenyanMapJson).then(res => {
      this.kenyanGeoJson = res;
    });

    const bounds = d3.geoBounds(this.kenyanGeoJson); // Returns [[minLng, minLat], [maxLng, maxLat]]
    const dx = bounds[1][0] - bounds[0][0]; // Longitude range
    const dy = bounds[1][1] - bounds[0][1]; // Latitude range
    const scale = this.calculateScale(dx, dy, containerWidth, containerHeight);

    this.svg = d3.select("figure#map")
      .append("svg")
      .attr("width", containerWidth)
      .attr("height", containerHeight);

    this.projection = d3.geoMercator()
      .scale(4200)
      .center([37.9062, 0.0236])  // Kenya's center
      .translate([containerWidth / 2, containerHeight / 2]);

    this.path = d3.geoPath().projection(this.projection);
  }

  private drawMap(): void {
    const dataMap = new Map(Object.entries(this.countyData))
    const countyDataValues = Array.from(dataMap.values());
    let values = countyDataValues.map(val => val[this.countyDataToBePicked]);
    values = values.concat(0);
    const result = this.categorizeAndColor(values, this.chartSColorScheme);
    let colorMap = new Map();
    result.forEach(res => colorMap.set(res.value, res.color))
    
    // Load the GeoJSON data for Kenya counties
    d3.json(this.kenyanMapJson).then((data: any) => {

      // Draw counties
      const counties = this.svg.append("g")
        .selectAll("path")
        .data(data.features)
        .enter()
        .append("path")
        .attr("d", this.path)
        .attr("fill", (d: any)  => {
          const countyCode = d.properties.COUNTY_COD;
          const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : 0;
          return colorMap.get(dataToDisplay)
        })
        .attr("stroke", "#000")
        .attr("stroke-width", 0.5)
        // Use arrow functions to preserve the 'this' context
        .on("mouseover", (event: any, d: any) => {
          //d3.select(event.currentTarget).attr("fill", "#2ca25f");
        })
        .on("mouseout", (event: any, d: any) => {
          //d3.select(event.currentTarget).attr("fill", "#69b3a2");
        })
        ;

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
        .text((d: any) => {
          const countyCode = d.properties.COUNTY_COD;
          const dataToDisplay = dataMap.get(`${countyCode}`) ? dataMap.get(`${countyCode}`)[this.countyDataToBePicked] : 0;
          return `${d.properties.COUNTY_NAM}: ${dataToDisplay | 0}`;
        });  // Set the county name

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

  calculateScale(dx: number, dy: number, containerWidth: number, containerHeight: number) {
    // The scale factor will depend on the width and height of the container,
    // as well as the geographical bounds.
    const xScale = containerWidth / dx;
    const yScale = containerHeight / dy;
  
    // Return the smaller of the two scale factors to ensure the map fits both horizontally and vertically
    return Math.max(xScale, yScale);
  }

  categorizeAndColor(values: number[], colorArray: string[]): { value: number, category: number, color: string }[] {
    // Step 1: Sort the values in ascending order
    const sortedValues = [...values].sort((a, b) => a - b);

    // Step 2: Determine the number of categories (max 10)
    const numCategories = Math.min(10, sortedValues.length);

    // Step 3: Divide the values into categories
    const categoryRanges = [];
    const valuesPerCategory = Math.ceil(sortedValues.length / numCategories);

    for (let i = 0; i < numCategories; i++) {
        const startIndex = i * valuesPerCategory;
        const endIndex = Math.min((i + 1) * valuesPerCategory, sortedValues.length);
        categoryRanges.push(sortedValues.slice(startIndex, endIndex));
    }

    // Step 4: Map categories to colors
    const categorizedValues: { value: number, category: number }[] = [];
    for (let i = 0; i < sortedValues.length; i++) {
        const value = sortedValues[i];
        // Find the category that the value belongs to
        const category = categoryRanges.findIndex(range => range.includes(value));
        categorizedValues.push({ value, category });
    }

    // Step 5: Assign color from colorArray based on category
    return categorizedValues.map(({ value, category }) => {
        return {
            value,
            category,
            color: colorArray[category] || colorArray[colorArray.length - 1] // Use the last color if there are more categories than colors
        };
    });
}
}
