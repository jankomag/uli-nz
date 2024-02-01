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
        data: 'https://jan.magnuszewski.com/wp-content/uploads/2024/02/sa1_auli.geojson'
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
            id: "AULI",
            type: "fill",
            source: "data",
            layout: {
                visibility: "none",
            },
            paint: {
                "fill-color": {
                    property: "AULI",
                    stops: [
                        [1, "#fff5eb"],
                            [2.9, "#fee6ce"],
                            [4.5, "#fdd0a2"],
                            [6.5, "#f16913"],
                            [7.2, "#d94801"],
                            [8.5, "#8c2d04"]
                    ],
                },
            },
        },
        firstSymbolId
    );

    const layerDefinitions = [{
            id: "Train Station",
            property: "TrainStation",
        },
        {
            id: "Bus Stop",
            property: "BusStop",
        },
        {
            id: "Dampness",
            property: "Dampness",
        },
        {
            id: "Diversity",
            property: "Diversity",
        },
        {
            id: "Crime",
            property: "Crime",
        },
        {
            id: "Road Safety",
            property: "RoadSafety",
        },
        {
            id: "Alcohol Environment",
            property: "AlcoholEnvs",
        },
        {
            id: "Marae",
            property: "Marae",
        },
        {
            id: "Leisure - Art",
            property: "LeisureArt",
        },
        {
            id: "Chemist",
            property: "Chemist",
        },
        {
            id: "Dentist",
            property: "Dentist",
        },
        {
            id: "Health Centre",
            property: "HealthCentre",
        },
        {
            id: "Hospital",
            property: "Hospital",
        },
        {
            id: "Childcare",
            property: "Childcare",
        },
        {
            id: "Leisure - Sport",
            property: "LeisureSport",
        },
        {
            id: "Convenience Store",
            property: "ConvenienceStore",
        },
        {
            id: "Supermarket",
            property: "Supermarket",
        },
        {
            id: "Secondary School",
            property: "SecondarySchool",
        },
        {
            id: "Primary School",
            property: "PrimarySchool",
        },
        {
            id: "Street Connectivity",
            property: "StreetConnectivity",
        },
        {
            id: "Park",
            property: "Park",
        },
        {
            id: "Cafe",
            property: "Cafe",
        },
        {
            id: "Restaurant",
            property: "Restaurant",
        },
        {
            id: "Pub",
            property: "Pub",
        },
        {
            id: "BBQ",
            property: "BBQ",
        },
        {
            id: "Bikeability",
            property: "Bikeability",
        },
        {
            id: "Beach",
            property: "Beach",
        },
        {
            id: "Affordability",
            property: "Affordability",
        },
        {
            id: "EV Charger",
            property: "EVcharger",
        },
    ];

    layerDefinitions.forEach((layerDefinition) => {
        map.addLayer({
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
                            [1, "#fff5eb"],
                            [2.9, "#fee6ce"],
                            [4.5, "#fdd0a2"],
                            [6.5, "#f16913"],
                            [7.2, "#d94801"],
                            [8.5, "#8c2d04"]
                        ],
                    },
                },
            },
            firstSymbolId
        );
    });

    var alllayers = [
        "AULI",
        "Train Station",
        "Bus Stop",
        "Bikeability",
        "EV Charger",
        "Supermarket",
        "Convenience Store",
        "Street Connectivity",
        "Chemist",
        "Dentist",
        "Hospital",
        "Health Centre",
        "Park",
        "Beach",
        "Leisure - Art",
        "Leisure - Sport",
        "Affordability",
        "Dampness",
        "Alcohol Environment",
        "Crime",
        "Road Safety",
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
    // alllayers.name = "alllay";


    alllayers.forEach((layer) => {
        map.on("click", layer, (e) => {
            new mapboxgl.Popup()
                .setLngLat(e.lngLat)
                .setHTML(
                    "<strong>" + layer + " score: </strong>" +
                    e.features[0].properties[layer].toFixed(2)
                )
                .addTo(map);
        });
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
    "AULI": "AULI",
    "Train Station": "transport",
    "Bus Stop": "transport",
    Bikeability: "transport",
    "EV Charger": "transport",
    Supermarket: "Essential Amenities",
    "Convenience Store": "Essential Amenities",
    "Street Connectivity": "transport",
    Chemist: "medical",
    Dentist: "medical",
    Hospital: "medical",
    "Health Centre": "medical",
    Park: "green infrastructure",
    Beach: "green infrastructure",
    "Leisure - Art": "leisure",
    "Leisure - Sport": "leisure",
    Affordability: "housing",
    Dampness: "housing",
    "Alcohol Environment": "safety",
    Crime: "safety",
    "Road Safety": "safety",
    Diversity: "Ethnic Diversity",
    Marae: "Ethnic Diversity",
    Childcare: "education",
    "Primary School": "education",
    "Secondary School": "education",
    Cafe: "food",
    Restaurant: "food",
    Pub: "food",
    BBQ: "food"
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

    link.onclick = function(e) {
        var clickedLayer = this.textContent;
        e.preventDefault();
        e.stopPropagation();
        for (var key2 in alllayersdict) {
            const anchElem = document.querySelector(`#menu a[id="${key2}"]`);
            if (clickedLayer === key2) {
                // class 'active' should be added to the clicked "a" element
                if (anchElem) {
                    anchElem.classList.add('active');
                }
                map.setLayoutProperty(key2, 'visibility', 'visible');
            } else {
                // class 'active' should be removed from the "a" element
                if (anchElem) {
                    anchElem.classList.remove('active');
                }
                map.setLayoutProperty(key2, 'visibility', 'none');
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