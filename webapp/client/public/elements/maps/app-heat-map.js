import {PolymerElement, html} from "@polymer/polymer"
import template from "./app-heat-map.html"

import "leaflet"
import leafletCss from "leaflet/dist/leaflet.css"
import "leaflet.heat"

export default class AppHeatMap extends PolymerElement {

  static get template() {
    return html([`
      <style>${leafletCss}</style>
      ${template}
    `]);
  }

  static get properties() {
    return {
      rotated : {
        type : Boolean,
        value : false
      },
      image : {
        type : String,
        value : false,
        observer : '_imageObserver'
      },
      data : {
        type : Array,
        value : () => [],
        observer : '_onDataChange'
      }
    }
  }

  ready() {
    super.ready();
    
    this.viewer = L.map(this.$.map, {
      crs: L.CRS.Simple,
      minZoom: -4,
      zoomControl : false,
      attributionControl: false
    });

    this.viewer.on('move', e => this._handleMove(e));
    this.viewer.on('zoomend', e => {
      e.originalEvent = true;
      this._handleMove(e)
    });

    this._resize();
    window.addEventListener('resize', () => this._resize());
  }

  _handleMove(e) {
    if( !e.originalEvent ) return;
    if( this.ignoreEvents ) return;
    let detail = {
      ele: this.ele,
      zoom: this.viewer.getZoom(), 
      center: this.viewer.getCenter()
    }
    this.dispatchEvent(new CustomEvent('map-move', {detail}));
  }

  _resize() {
    this.$.map.style.height = this.offsetHeight+'px';
    this.viewer.invalidateSize();
  }

  async _onDataChange() {
    await this.loading;

    if( this.heat ) this.viewer.removeLayer(this.heat);

    let arr = [];
    this.data.forEach(f => {
      if( this.rotated ) {
        if( f.rotation === 0 ) return;
      } else {
        if( f.rotation !== 0 ) return;
      }

      let factor = f.x_wconf/100;

     
      

      let rowstart = f.geojson.coordinates[0][1][0];
      let rowend = f.geojson.coordinates[0][2][0];
      let colend = f.geojson.coordinates[0][1][1]+this.imageHeight;
      let colstart = f.geojson.coordinates[0][0][1]+this.imageHeight;
  
      if( colend - colstart > 1000 ) return;
      if( rowend - rowstart > 1000 ) return;

      // debugger;
      for( let lat = colstart; lat < colend; lat += 10 ) {
        for( let lng = rowstart; lng < rowend; lng += 10 ) {
          arr.push([lat, lng, factor]);
        }
      }

      // end = f.geojson.coordinates[0][3][0];
      // start = f.geojson.coordinates[0][4][0];
      // for( let i = start; i < end; i += 4 ) {
      //   let lat = f.geojson.coordinates[0][3][1]+this.imageHeight;
      //   arr.push([lat, i, factor]);
      // }

      // f.geojson.coordinates[0].forEach(p => {
        
      // })
      
    });

    console.log(this.rotated, arr.length);

    setTimeout(() => {
      this.heat = L.heatLayer(arr, {
        radius: 10,
        blur: 1,
        gradient: {0: 'red', 0.6: 'green'}
      }).addTo(this.viewer);
    }, 500);
    
  }

  async _imageObserver() {
    if( !this.image ) return;

    await this._loadImage();

    L.imageOverlay(this.image, this.bounds).addTo(this.viewer);
    this.viewer.fitBounds(this.bounds);
  }

  _loadImage() {
    this.loading = new Promise((resolve, reject) => {
      var img = new Image();
      img.onload = () => {
        this.imageHeight = img.naturalHeight;
        let res = [img.naturalHeight, img.naturalWidth];
        this.bounds = [[0,0], res];
        resolve();
      };
      img.src = this.image;
    });

    return this.loading;
  }

  setPosition(e) {
    this.ignoreEvents = true;
    setTimeout(() => {
      this.ignoreEvents = false;
    }, 100);
    this.viewer.setView(e.center, e.zoom);
  }


}

customElements.define('app-heat-map', AppHeatMap);