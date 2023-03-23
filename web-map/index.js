var mykey = config_varmb;

mapboxgl.accessToken = String(mykey);
var map = new mapboxgl.Map({
  container: 'map',
  style: 'mapbox://styles/jankomag/clfbsebky002o01mx8evj3jmc', // replace this with your style URL
  center: [174.7645, -36.8509],
  zoom: 10,
  pitch: 0,
  bearing: 0,
  antialias: true
});
map.on('style.load', () => {
  map.setFog({}); // Set the default atmosphere style
});

map.on('load', function() {
  map.addSource('data', {
    type: 'geojson',
    data: 'https://jan.magnuszewski.com/wp-content/uploads/2023/03/sa1-4326.geojson'
  })
  const layers = map.getStyle().layers;
// Find the index of the first symbol layer in the map style.
let firstSymbolId;
for (const layer of layers) {
if (layer.type === 'symbol') {
firstSymbolId = layer.id;
break;
}
};

  map.addLayer({
    'id': 'KULI',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'kuli_geomAgg',
        stops: [
          [0, '#1588ff'],
          [.2, '#69ccfa'],
          [.5, '#ffffb9'],
          [.9, '#ff5959'],
          [1, '#ff0101']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Train Station',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'station1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Frequent Bus Stop',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'freqbusstop1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Car Infrastructure',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'carInfrastructure1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Bikeability',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'bikeability1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Supermarkets',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'supermarket1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Convenience Store',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'convstor1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Street Connectivity',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'strconnectivity1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Housing Density',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'housedens1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  //MEDICAL
  map.addLayer({
    'id': 'Chemist',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'chemist1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Dentist',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'dentist1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Hospital',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'hospital1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  map.addLayer({
    'id': 'Health Centre',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'healthcr1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  }, firstSymbolId);
  // EDUCATION
  map.addLayer({
    'id': 'Secondary School',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'secondary1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Primary School',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'primary1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Childcare',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'childcare1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  //SAFETY

  map.addLayer({
    'id': 'Crime',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'crime1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Road Safety',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'crashes1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Flood Proneness',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'flood1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Alcohol Environment',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'alcohol1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Emergency Services',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'emergency1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  //CULTURE
  map.addLayer({
    'id': 'Diversity',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'diversity1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Marae',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'marae1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  //
  map.addLayer({
    'id': 'Cinema',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'cinema1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Gallery',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'gallery1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Library',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'library1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Museum',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'museum1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Theatre',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'theatre1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Sport Facilities',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'sport1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Gym',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'gym1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  // FOOD
  map.addLayer({
    'id': 'Cafe',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'cafe1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Restaurant',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'restaurant1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Pub',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'pub1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'BBQ',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'bbq1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  // GREEN SPACE
  map.addLayer({
    'id': 'Park',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'bigpark1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Beach',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'beach1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  //HOUSING
  map.addLayer({
    'id': 'Affordability',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'affordability1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  map.addLayer({
    'id': 'Dampness',
    'type': 'fill',
    'source': 'data',
    'layout': {
      'visibility': 'none'
    },
    'paint': {
      'fill-color': {
        property: 'damp1',
        stops: [
          [0, '#ffffff'],
          [4, '#ffacac'],
          [6, '#ff2b2b'],
          [8, '#ea0000'],
          [10, '#7f0000']
        ]
      }
    }
  },firstSymbolId);
  
  map.addSource('mapbox-style', {
    type: 'vector',
    url: 'mapbox://styles/jankomag/clfbsebky002o01mx8evj3jmc'
    });
    map.addLayer(
    {
    'id': 'lavelss',
    'type': 'symbol',
    'source': 'mapbox-style',
    'source-layer': 'place_label',
    'class': 'settlement_subdivision',
    'paint': {
    'text-color': '#ff69b4'}
    }
    );

    var alllayers = ['KULI','Train Station','Frequent Bus Stop','Bikeability','Car Infrastructure','Supermarkets',
'Convenience Store','Street Connectivity','Housing Density',
'Chemist','Dentist','Hospital','Health Centre',
"Park", "Beach",
'Cinema','Gym','Theatre','Cinema','Museum','Gallery','Sport Facilities',
'Affordability','Dampness',"Alcohol Environment","Crime","Road Safety","Flood Proneness","Emergency Services",
"Diversity","Marae",
"Childcare","Primary School","Secondary School","Cafe","Restaurant","Pub","BBQ"]
alllayers.name = 'alllay'

map.on('click', alllayers, (e) => {
    new mapboxgl.Popup()
        .setLngLat(e.lngLat)
        .setHTML("<strong>KULI score: </strong>"+e.features[0].properties.kuli_geomAgg.toFixed(2))
        .addTo(map);
});
 
// Change the cursor to a pointer when
// the mouse is over the states layer.
map.on('mouseenter', layers, () => {
    map.getCanvas().style.cursor = 'pointer';
});
 
// Change the cursor back to a pointer
// when it leaves the states layer.
map.on('mouseleave', layers, () => {
    map.getCanvas().style.cursor = '';
});
});

var alllayersdict = {
'KULI':'KULI',
'Train Station':'transport',
'Frequent Bus Stop':'transport',
'Bikeability':'transport',
'Car Infrastructure':'transport',
'Supermarkets':'walkability',
'Convenience Store':'walkability',
'Street Connectivity':'walkability',
'Housing Density':'walkability',
'Chemist':'medical',
'Dentist':'medical',
'Hospital':'medical',
'Health Centre':'medical',
"Park":'green infrastructure',
"Beach":'green infrastructure',
'Cinema':'leisure',
'Gym':'leisure',
'Theatre':'leisure',
'Cinema':'leisure',
'Museum':'leisure',
'Gallery':'leisure',
'Sport Facilities':'leisure',
'Affordability':'housing',
'Dampness':'housing',
"Alcohol Environment":'safety',
"Crime":'safety',
"Road Safety":'safety',
"Flood Proneness":'safety',
"Emergency Services":'safety',
"Diversity":'culture',
"Marae":'culture',
"Childcare":'education',
"Primary School":'education',
"Secondary School":'education',
"Cafe":'food',
"Restaurant":'food',
"Pub":'food',
"BBQ":'food',
};

// create a dictionary to store the links for each category
var menuLinks = {};

// create a link for each layer and group them by category
for (var key in alllayersdict) {
  var theme = alllayersdict[key];
  var link = document.createElement('a');
  link.href = '#';
  link.id = key;
  link.textContent = String(key);

  link.onclick = function(e) {
    var clickedLayer = this.textContent;
    e.preventDefault();
    e.stopPropagation();
    for (var key2 in alllayersdict) {
      const anchElem = document.querySelector(`#menu a[id="${key2}"]`);
      if (clickedLayer === key2) {
          //class 'active' should be added to the clicked "a" element
          if (anchElem) { anchElem.classList.add("active"); }
          map.setLayoutProperty(key2, 'visibility', 'visible');
      } else {
          //class 'active' should be removed from the "a" element
          if (anchElem) { anchElem.classList.remove("active"); }
          map.setLayoutProperty(key2, 'visibility', 'none');
      }
  }
  };

  var layers = document.getElementById('menu');
  layers.appendChild(link); 
  
  // create a new HTML menu element for each category
  if (!menuLinks.hasOwnProperty(theme)) {
    var menu = document.createElement('div');
    menu.className = 'menu-category';
    var title = document.createElement('h3');
    title.textContent = theme;
    menu.appendChild(title);
    var layers = document.createElement('div');
    layers.className = 'menu-layers';
    menu.appendChild(layers);
    menuLinks[theme] = layers;
    document.getElementById('menu').appendChild(menu);
  }

  // add the link to the corresponding HTML menu element
  menuLinks[theme].appendChild(link);
}