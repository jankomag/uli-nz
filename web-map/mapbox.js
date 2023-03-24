var mykey = config_varmb;

mapboxgl.accessToken = String(mykey);
var map = new mapboxgl.Map({
  container: 'map',
  style: 'mapbox://styles/jankomag/clfbsebky002o01mx8evj3jmc', // replace this with your style URL
  center: [174.7645, -36.8509],
  zoom: 10,
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

map.addLayer(
  {
    id: "KULI",
    type: "fill",
    source: "data",
    layout: {
      visibility: "none",
    },
    paint: {
      "fill-color": {
        property: "kuli_geomAgg",
        stops: [
          [0, "#1588ff"],
          [0.2, "#69ccfa"],
          [0.5, "#ffffb9"],
          [0.9, "#ff5959"],
          [1, "#ff0101"],
        ],
      },
    },
  },
  firstSymbolId
);

const layerDefinitions = [
  {
    id: "Train Station",
    property: "station1",
  },
  {
    id: "Frequent Bus Stop",
    property: "freqbusstop1",
  },
  {
    id: "Dampness",
    property: "damp1",
  },
  {
    id: "Diversity",
    property: "diversity1",
  },
  {
    id: "Crime",
    property: "crime1",
  },
  {
    id: "Road Safety",
    property: "crashes1",
  },
  {
    id: "Flood Proneness",
    property: "flood1",
  },
  {
    id: "Alcohol Environment",
    property: "alcohol1",
  },
  {
    id: "Marae",
    property: "marae1",
  },
  {
    id: "Cinema",
    property: "cinema1",
  },
  {
    id: "Gallery",
    property: "gallery1",
  },
  {
    id: "Library",
    property: "library1",
  },
  {
    id: "Museum",
    property: "museum1",
  },
  {
    id: "Housing Density",
    property: "housedens1",
  },
  {
    id: "Theatre",
    property: "theatre1",
  },
  {
    id: "Chemist",
    property: "chemist1",
  },
  {
    id: "Dentist",
    property: "dentist1",
  },
  {
    id: "Health Centre",
    property: "healthcr1",
  },
  {
    id: "Hospital",
    property: "hospital1",
  },
  {
    id: "Childcare",
    property: "childcare1",
  },
  {
    id: "Sport Facilities",
    property: "sport1",
  },
  {
    id: "Convenience Store",
    property: "convstor1",
  },
  {
    id: "Supermarkets",
    property: "supermarket1",
  },
  {
    id: "Secondary School",
    property: "secondary1",
  },
  {
    id: "Primary School",
    property: "primary1",
  },
  {
    id: "Street Connectivity",
    property: "strconnectivity1",
  },
  {
    id: "Park",
    property: "bigpark1",
  },
  {
    id: "Cafe",
    property: "cafe1",
  },
  {
    id: "Restaurant",
    property: "restaurant1",
  },
  {
    id: "Pub",
    property: "pub1",
  },
  {
    id: "BBQ",
    property: "bbq1",
  },
  {
    id: "Bikeability",
    property: "bikeability1",
  },
  {
    id: "Gym",
    property: "gym1",
  },
  {
    id: "Beach",
    property: "beach1",
  },
  {
    id: "Affordability",
    property: "affordability1",
  },
  {
    id: "Car Infrastructure",
    property: "carInfrastructure1",
  },
  {
    id: "Emergency Services",
    property: "emergency1",
  },
];

layerDefinitions.forEach((layerDefinition) => {
  map.addLayer(
    {
      id: layerDefinition.id,
      type: "fill",
      source: "data",
      layout: {
        visibility: "none",
      },
      paint: {
        "fill-color": {
          property: layerDefinition.property,
          stops: [
            [0, "#ffffff"],
            [4, "#ffacac"],
            [6, "#ff2b2b"],
            [8, "#ea0000"],
            [10, "#7f0000"],
          ],
        },
      },
    },
    firstSymbolId
  );
});

var alllayers = [
  "KULI",
  "Train Station",
  "Frequent Bus Stop",
  "Bikeability",
  "Car Infrastructure",
  "Supermarkets",
  "Convenience Store",
  "Street Connectivity",
  "Housing Density",
  "Chemist",
  "Dentist",
  "Hospital",
  "Health Centre",
  "Park",
  "Beach",
  "Cinema",
  "Gym",
  "Theatre",
  "Cinema",
  "Museum",
  "Gallery",
  "Sport Facilities",
  "Affordability",
  "Dampness",
  "Alcohol Environment",
  "Crime",
  "Road Safety",
  "Flood Proneness",
  "Emergency Services",
  "Diversity",
  "Marae",
  "Childcare",
  "Primary School",
  "Secondary School",
  "Cafe",
  "Restaurant",
  "Pub",
  "BBQ",
];
alllayers.name = "alllay";

map.on("click", alllayers, (e) => {
  new mapboxgl.Popup()
    .setLngLat(e.lngLat)
    .setHTML(
      "<strong>KULI score: </strong>" +
        e.features[0].properties.kuli_geomAgg.toFixed(2)
    )
    .addTo(map);
});

// Change the cursor to a pointer when
// the mouse is over the states layer.
map.on("mouseenter", layers, () => {
  map.getCanvas().style.cursor = "pointer";
});

// Change the cursor back to a pointer
// when it leaves the states layer.
map.on("mouseleave", layers, () => {
  map.getCanvas().style.cursor = "";
});
});

var alllayersdict = {
KULI: "KULI",
"Train Station": "transport",
"Frequent Bus Stop": "transport",
Bikeability: "transport",
"Car Infrastructure": "transport",
Supermarkets: "walkability",
"Convenience Store": "walkability",
"Street Connectivity": "walkability",
"Housing Density": "walkability",
Chemist: "medical",
Dentist: "medical",
Hospital: "medical",
"Health Centre": "medical",
Park: "green infrastructure",
Beach: "green infrastructure",
Cinema: "leisure",
Gym: "leisure",
Theatre: "leisure",
Cinema: "leisure",
Museum: "leisure",
Gallery: "leisure",
"Sport Facilities": "leisure",
Affordability: "housing",
Dampness: "housing",
"Alcohol Environment": "safety",
Crime: "safety",
"Road Safety": "safety",
"Flood Proneness": "safety",
"Emergency Services": "safety",
Diversity: "culture",
Marae: "culture",
Childcare: "education",
"Primary School": "education",
"Secondary School": "education",
Cafe: "food",
Restaurant: "food",
Pub: "food",
BBQ: "food",
};

// create a dictionary to store the links for each category
var menuLinks = {};

// create a link for each layer and group them by category
for (var key in alllayersdict) {
var theme = alllayersdict[key];
var link = document.createElement("a");
link.href = "#";
link.id = key;
link.textContent = String(key);

link.onclick = function (e) {
  var clickedLayer = this.textContent;
  e.preventDefault();
  e.stopPropagation();
  for (var key2 in alllayersdict) {
    const anchElem = document.querySelector(`#menu a[id="${key2}"]`);
    if (clickedLayer === key2) {
      //class 'active' should be added to the clicked "a" element
      if (anchElem) {
        anchElem.classList.add("active");
      }
      map.setLayoutProperty(key2, "visibility", "visible");
    } else {
      //class 'active' should be removed from the "a" element
      if (anchElem) {
        anchElem.classList.remove("active");
      }
      map.setLayoutProperty(key2, "visibility", "none");
    }
  }
};

var layers = document.getElementById("menu");
layers.appendChild(link);

if (!menuLinks.hasOwnProperty(theme)) {
  var menu = document.createElement("div");
  menu.className = "menu-category " + theme;
  var titleContainer = document.createElement("div"); // add a separate container element
  titleContainer.className = "title-container";
  var title = document.createElement("h3");
  title.textContent = theme;
  titleContainer.appendChild(title);
  menu.appendChild(titleContainer);

  // add a border break line after the h3 element
  var border = document.createElement("hr");
  menu.appendChild(border);

  var layers = document.createElement("div");
  layers.className = "menu-layers";
  menu.appendChild(layers);
  menuLinks[theme] = layers;
  document.getElementById("menu").appendChild(menu);
}
menuLinks[theme].appendChild(link);
}