import {PolymerElement, html} from "@polymer/polymer"
import template from "./datafest-app.html"

import "./maps/app-box-map"
import "./maps/app-heat-map"

const ROOT_URL = 'https://digital.ucdavis.edu/fcrepo/rest/collection/sherry-lehmann/catalogs/';

export default class DatafestApp extends PolymerElement {

  static get template() {
    return html([template]);
  }

  static get properties() {
    return {
      hideInfo : {
        type : Boolean,
        value : false
      }
    }
  }

  ready() {
    super.ready();

    setTimeout(() => this.init(), 200);

    this.maps = this.shadowRoot.querySelectorAll('.map');
  }

  async init() {
    let ark = window.location.pathname.replace(/\/ark\//, '');
    let base = ark.split('-')[0];

    this.image = `${ROOT_URL}${base}/media/images/${ark}`;
    console.log(this.image);

    let response = await fetch(`/api/words/${ark}`);
    
    this.data = await response.json();
  }

  _onMapMove(e) {
    e = e.detail;
    for( var i = 0; i < this.maps.length; i++ ) {
      let ele = this.maps[i];
      if( e.ele === ele ) continue;
      ele.setPosition(e);
    }
  }

  _onFeatureClicked(e) {
    e = e.detail;
    let html = '<li>';
    for( let key in e ) {
      html += `<li><b>${key}:</b> ${e[key]}</li>`;
    }
    this.$.info.innerHTML = html + '</ul>';
    setTimeout(() => {
      this.maps.forEach(map => map._resize());
    }, 100);
  }

  hideInfoPanel() {
    this.hideInfo = true;
  }

}

customElements.define('datafest-app', DatafestApp);