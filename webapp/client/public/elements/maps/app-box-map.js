import {PolymerElement, html} from "@polymer/polymer"
import template from "./app-box-map.html"

import "leaflet"
import clone from "clone"
import leafletCss from "leaflet/dist/leaflet.css"

export default class AppBoxMap extends PolymerElement {

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
    this.geojson = [];
    
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
    if( !this.geojson ) return;
    await this.loading;

    this.geojson.forEach(f => {
      this.viewer.removeLayer(f);
    });
    this.geojson = [];

    this.data.forEach(f => {
      if( this.rotated ) {
        if( f.rotation === 0 ) return;
      } else {
        if( f.rotation !== 0 ) return;
      }

      let rowstart = f.geojson.coordinates[0][1][0];
      let rowend = f.geojson.coordinates[0][2][0];
      let colend = f.geojson.coordinates[0][1][1]+this.imageHeight;
      let colstart = f.geojson.coordinates[0][0][1]+this.imageHeight;
  
      if( colend - colstart > 1000 ) return;
      if( rowend - rowstart > 1000 ) return;

      f = clone(f);

      let factor = f.x_wconf/100;

      let ofactor = factor;
      if( factor < 0.3) ofactor = 0.3;
      if( factor > 0.7) ofactor = 0.7;

      let style = {
        "color": `rgb(${Math.floor(255-(255*factor))},${Math.floor(255*factor)},0)`,
        "weight": 2,
        "opacity": ofactor,
        fillOpacity : ofactor,
      };

      if( f.word_id === '1040545' ) {
        style.color = 'blue';
      }
 
      f.geojson.coordinates[0] = f.geojson.coordinates[0].map(c => {
        return [c[0], this.imageHeight + c[1]];
      })

      let feature = {
        type : 'Feature',
        properties : f,
        geometry : f.geojson
      }

      f = L.geoJSON(feature, {style}).addTo(this.viewer);
      f.on('click', (e) => {
        let detail = clone(e.layer.feature.properties);
        
        // console.log(detail.geojson);
        let c = detail.geojson.coordinates[0];
        let x = c[0][0]-10;
        let y = this.imageHeight-c[0][1]-60;
        let w = c[2][0]-c[1][0]+10;
        let h = c[1][1]-c[0][1]+10;
        console.log(`${x},${y},${w},${h}`);

        delete detail.geojson;
        this.dispatchEvent(new CustomEvent('feature-clicked', {detail}));
      });
      
      this.geojson.push(f);
    })
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

customElements.define('app-box-map', AppBoxMap);