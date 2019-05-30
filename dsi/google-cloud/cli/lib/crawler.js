const api = require('@ucd-lib/fin-node-api');

const HAS_PART = 'http://schema.org/hasPart';
const ASSOCIATED_MEDIA = 'http://schema.org/associatedMedia';
const MEDIA_OBJECT = 'http://schema.org/MediaObject';
const HAS_MIME_TYPE = 'http://www.ebu.ch/metadata/ontologies/ebucore/ebucore#hasMimeType'
const FILENAME = 'http://www.ebu.ch/metadata/ontologies/ebucore/ebucore#filename';

class Crawler {
  
  constructor(collection) {
    this.host = 'https://digital.ucdavis.edu';
    this.collection = collection;
  }

  /**
   * @method getImageUrls
   * @description crawl fedora collection looking for images to download
   * 
   */
  async getImageUrls(options={}) {
    options.images = [];

    api.setConfig({
      host : this.host
    });

    let roots = options.root || '/';
    if( !Array.isArray(roots) ) {
      roots = [roots];
    }

    for( let root of roots ) {
      root = '/collection/' + this.collection + root;
      console.log('crawling '+root);
      await this._crawl(root, options);
    }

    return options.images;
  }

  async _crawl(path, options) {
    if( options.max && options.max <= options.images.length ) {
      return;
    }

    let node = await this.get(path);

    if( node[HAS_PART] ) {
      for( let p of node[HAS_PART] ) {
        await this._crawl(this._stripHostAndBase(p['@id']), options);
      }
    }

    if( node[ASSOCIATED_MEDIA] ) {
      for( let p of node[ASSOCIATED_MEDIA] ) {
        await this._crawl(this._stripHostAndBase(p['@id']), options);
      }
    }

    let mimeType = this._getPropertyString(node, HAS_MIME_TYPE);
    if( mimeType && mimeType.startsWith('image') && node['@type'].indexOf(MEDIA_OBJECT) > -1 ) {
      options.images.push({
        imageUrl: node['@id'],
        filename : this._getPropertyString(node, FILENAME),
        pathId : node['@id'].replace(new RegExp('.*collection/'+this.collection), '')
                            .replace(/\.[a-z]*$/i, '')
      });
    }
  }

  _getPropertyString(node, property) {
    if( !node[property] ) return null;
    let val = node[property];
    if( Array.isArray(val) ) val = val[0];
    if( val['@id'] ) return val['@id'];
    return val['@value'];
  }

  _stripHostAndBase(path) {
    return path.replace(new RegExp(this.host+'/fcrepo/rest'), '');
  }

  async get(path) {
    path = path.replace(/\/$/, '');

    let isBinary = false;
    let response = await api.head({path});
    if( !api.isRdfContainer(response.last) ) {
      isBinary = true;
    }

    response = await api.get({
      path : path + (isBinary ? '/fcr:metadata' : ''),
      headers : {
        accept : api.RDF_FORMATS.JSON_LD
      }
    });

    if( response.last.statusCode !== 200 ) {
      throw new Error(`Error fetching ${path} (${response.last.statusCode}):`+response.last.body);
    }

    let graph = JSON.parse(response.last.body);
    for( let item of graph ) {
      if( item['@id'] == this.host+'/fcrepo/rest'+path ) {
        return item;
      }
    }

    return null;
  }

}

module.exports = Crawler;