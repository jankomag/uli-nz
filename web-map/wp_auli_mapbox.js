// Replace this with your Mapbox access token
var mapboxgl_key = config_varmb;

mapboxgl.accessToken = mapboxgl_key;
const map = new mapboxgl.Map({
    container: 'map',
    style: 'mapbox://styles/mapbox/light-v11',
    center: [174.7645, -36.8509],
    zoom: 10,
    antialias: true
});

// Helper function to wait for source loading
function waitForSourceLoad(callback) {
    if (!map.isSourceLoaded('data')) {
        setTimeout(() => waitForSourceLoad(callback), 200);
    } else {
        callback();
    }
}

// Calculate quantile breaks for color scale
function calculateQuantileBreaks(data, numClasses) {
    const validData = data.filter(d => d !== null && !isNaN(d) && d !== undefined);
    if (validData.length === 0) return Array(numClasses + 1).fill(0);
    
    const sorted = validData.sort((a, b) => a - b);
    const breakpoints = [];
    
    for (let i = 0; i <= numClasses; i++) {
        const index = Math.floor((i / numClasses) * (sorted.length - 1));
        breakpoints.push(Number(sorted[index]));
    }
    
    return breakpoints;
}

// Generate color scale based on data values
function getColorScale(values) {
    try {
        const breaks = calculateQuantileBreaks(values, 5);
        return [
            [breaks[0], '#fff5eb'],
            [breaks[1], '#fee6ce'],
            [breaks[2], '#fdd0a2'],
            [breaks[3], '#f16913'],
            [breaks[4], '#d94801'],
            [breaks[5], '#8c2d04']
        ];
    } catch (error) {
        console.error('Error generating color scale:', error);
        return defaultColorScale();
    }
}

// Default color scale if data processing fails
function defaultColorScale() {
    return [
        [0, '#fff5eb'],
        [2, '#fee6ce'],
        [4, '#fdd0a2'],
        [6, '#f16913'],
        [8, '#d94801'],
        [10, '#8c2d04']
    ];
}

// Create and update popup
function createPopup(e) {
    const features = map.queryRenderedFeatures(e.point);
    if (!features.length) return;

    const feature = features[0];
    const activeLayer = document.querySelector('#menu a.active').id;
    const popupContent = `
        <h3>Area Details</h3>
        <p><strong>${activeLayer}:</strong> ${feature.properties[layerDefinitions.find(l => l.id === activeLayer).property].toFixed(2)}</p>
        <p><strong>Area Code:</strong> ${feature.properties.SA1_CODE}</p>
    `;

    new mapboxgl.Popup()
        .setLngLat(e.lngLat)
        .setHTML(popupContent)
        .addTo(map);
}

// Update map and legend for selected layer
function updateMapAndLegend(layer, property) {
    try {
        const features = map.querySourceFeatures('data');
        const values = features.map(f => Number(f.properties[property]));
        const colorScale = getColorScale(values);
        
        // Update map layer
        map.setPaintProperty(layer, 'fill-color', {
            property: property,
            type: 'interval',
            stops: colorScale
        });
        
        // Update legend
        updateLegend(layer, colorScale);
        
    } catch (error) {
        console.error('Error updating map and legend:', error);
    }
}

// Update legend content
function updateLegend(title, colorScale) {
    const legend = document.getElementById('legend');
    legend.innerHTML = `
        <div class="legend-title">${title}</div>
        ${colorScale.map((item, i) => `
            <div class="legend-item">
                <div class="legend-color" style="background-color: ${item[1]}"></div>
                <div>${i === 0 ? '≤ ' : ''}${item[0].toFixed(1)}${
                    i === colorScale.length - 1 ? '+' : ` - ${colorScale[i + 1][0].toFixed(1)}`
                }</div>
            </div>
        `).join('')}
    `;
}


const layerDefinitions = [
    { id: "AULI", property: "AULI" },
    { id: "Train Station", property: "TrainStation" },
    { id: "Bus Stop", property: "BusStop" },
    { id: "Dampness", property: "Dampness" },
    { id: "Diversity", property: "Diversity" },
    { id: "Crime", property: "Crime" },
    { id: "Road Safety", property: "RoadSafety" },
    { id: "Alcohol Environment", property: "AlcoholEnvs" },
    { id: "Marae", property: "Marae" },
    { id: "Leisure - Art", property: "LeisureArt" },
    { id: "Chemist", property: "Chemist" },
    { id: "Dentist", property: "Dentist" },
    { id: "Health Centre", property: "HealthCentre" },
    { id: "Hospital", property: "Hospital" },
    { id: "Childcare", property: "Childcare" },
    { id: "Leisure - Sport", property: "LeisureSport" },
    { id: "Convenience Store", property: "ConvenienceStore" },
    { id: "Supermarket", property: "Supermarket" },
    { id: "Secondary School", property: "SecondarySchool" },
    { id: "Primary School", property: "PrimarySchool" },
    { id: "Street Connectivity", property: "StreetConnectivity" },
    { id: "Park", property: "Park" },
    { id: "Cafe", property: "Cafe" },
    { id: "Restaurant", property: "Restaurant" },
    { id: "Pub", property: "Pub" },
    { id: "BBQ", property: "BBQ" },
    { id: "Bikeability", property: "Bikeability" },
    { id: "Beach", property: "Beach" },
    { id: "Affordability", property: "Affordability" },
    { id: "EV Charger", property: "EVcharger" }
];

// Create menu categories
const categories = {
    "transport": ["Train Station", "Bus Stop", "Bikeability", "EV Charger", "Street Connectivity"],
    "Essential Amenities": ["Supermarket", "Convenience Store"],
    "medical": ["Chemist", "Dentist", "Hospital", "Health Centre"],
    "green infrastructure": ["Park", "Beach"],
    "leisure": ["Leisure - Art", "Leisure - Sport"],
    "housing": ["Affordability", "Dampness"],
    "safety": ["Alcohol Environment", "Crime", "Road Safety"],
    "Ethnic Diversity": ["Diversity", "Marae"],
    "education": ["Childcare", "Primary School", "Secondary School"],
    "food": ["Cafe", "Restaurant", "Pub", "BBQ"]
};


// Initialize map when style is loaded
map.on('style.load', () => {
    map.setFog({});
});

// Main map initialization
map.on('load', function() {
    // Add data source
    map.addSource('data', {
        type: 'geojson',
        data: 'https://jan.magnuszewski.com/wp-content/uploads/2024/11/sa1_auli.geojson'
    });

    // Find first symbol layer for proper layer ordering
    const firstSymbolId = map.getStyle().layers.find(layer => layer.type === 'symbol')?.id;

    // Initialize layers
    initializeLayers(firstSymbolId);
    
    // Create menu
    createMenu();

    // Add interaction handlers
    addMapInteractions();

    // Initialize with AULI layer
    waitForSourceLoad(() => {
        updateMapAndLegend("AULI", "AULI");
    });
});

// Initialize map layers
function initializeLayers(firstSymbolId) {
    // Add AULI layer
    map.addLayer({
        id: "AULI",
        type: "fill",
        source: "data",
        layout: { visibility: 'visible' },
        paint: {
            "fill-color": {
                property: "AULI",
                type: 'interval',
                stops: defaultColorScale()
            },
            "fill-opacity": 0.8
        }
    }, firstSymbolId);

    // Add other layers
    layerDefinitions.slice(1).forEach((layerDef) => {
        map.addLayer({
            id: layerDef.id,
            type: "fill",
            source: "data",
            layout: { visibility: "none" },
            paint: {
                "fill-color": {
                    property: layerDef.property,
                    type: 'interval',
                    stops: defaultColorScale()
                },
                "fill-opacity": 0.8
            }
        }, firstSymbolId);
    });
}

// Add map interactions
function addMapInteractions() {
    // Change cursor on hover
    map.on('mousemove', (e) => {
        const features = map.queryRenderedFeatures(e.point);
        map.getCanvas().style.cursor = features.length ? 'pointer' : '';
    });

    // Show popup on click
    map.on('click', createPopup);
}

// Create menu structure
function createMenu() {
    const menu = document.getElementById("menu");
    
    // Add AULI link
    addAULILink(menu);
    
    // Add categories
    Object.entries(categories).forEach(([category, layers]) => {
        addCategorySection(menu, category, layers);
    });
}

// Add AULI link to menu
function addAULILink(menu) {
    const auliLink = document.createElement("a");
    auliLink.href = "#";
    auliLink.id = "AULI";
    auliLink.textContent = "AULI";
    auliLink.className = "active";
    
    auliLink.onclick = (e) => {
        handleLayerClick(e, "AULI", "AULI");
    };
    
    menu.appendChild(auliLink);
}

// Add category section to menu
function addCategorySection(menu, category, layers) {
    const section = document.createElement("div");
    section.className = `menu-category ${category.toLowerCase().replace(/\s+/g, '')}`;
    
    const title = document.createElement("h3");
    title.textContent = category;
    section.appendChild(title);
    
    layers.forEach(layerId => {
        const link = document.createElement("a");
        link.href = "#";
        link.id = layerId;
        link.textContent = layerId;
        
        link.onclick = (e) => {
            const layerDef = layerDefinitions.find(l => l.id === layerId);
            handleLayerClick(e, layerId, layerDef.property);
        };
        
        section.appendChild(link);
    });
    
    menu.appendChild(section);
}

// Handle layer click events
function handleLayerClick(e, layerId, property) {
    e.preventDefault();
    e.stopPropagation();

    // Hide all layers
    layerDefinitions.forEach(layer => {
        map.setLayoutProperty(layer.id, 'visibility', 'none');
    });

    // Show selected layer
    map.setLayoutProperty(layerId, 'visibility', 'visible');

    // Update active class
    document.querySelectorAll('#menu a').forEach(link => {
        link.className = link.id === layerId ? 'active' : '';
    });

    // Update legend
    updateMapAndLegend(layerId, property);
}