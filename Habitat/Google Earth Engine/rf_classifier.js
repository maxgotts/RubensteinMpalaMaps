// Manually selected ROIs
// var TreeCover: FeatureCollection
// var Grassland: FeatureCollection

// Thank you to Joao Siqueira, and the MapBiomas team for letting me adapt and use their code

// Image asset id
var imageId = "users/maxgotts/mpala-2021-summer"; //"users/joaovsiqueira1/mapbiomas-course/san-francisco-2020"; // Needs to change

// Load as an image
var mosaic = ee.Image(imageId);

// Mpala shapefile import
var MpalaImport = ee.FeatureCollection("users/maxgotts/MpalaROI");
var coords = MpalaImport.first().geometry().coordinates().get(0);
var mpala = ee.Geometry.Polygon(coords);


// prints the collection structure
print('Mosaic:', mosaic);

// Set the visualization parameters
var visParams = {
    bands: ['B6_median','B5_median','B4_median'],
    gain: [0.08,0.06,0.2]
};

// Add image & ROI to map
Map.addLayer(mosaic, visParams, 'Mosaic')
Map.addLayer(mpala, {}, 'Mpala Research Area');

// Zoom into the image
Map.centerObject(mpala, 11); // Change amount zoomed in


// Create a function to collect random point inside the polygons
var generatePoints = function(polygons, nPoints) {
    
    // Generate N random points inside the polygons
    var points = ee.FeatureCollection.randomPoints(polygons, nPoints);
    
    // Get the class value propertie
    var classValue = polygons.first().get('class');
    
    // Iterate over points and assign the class value
    points = points.map(
        function(point){
            return point.set('class', classValue);
        }
    );
    
    return points;
};

// Collect random points inside your polygons
var TreeCoverPoints = generatePoints(TreeCover, 100);
var GrasslandPoints = generatePoints(Grassland, 100);
// var HydrologyPoints = generatePoints(Hydrology, 50);

// Merge all samples into a featureCollection
var samples = TreeCoverPoints.merge(GrasslandPoints); //.merge(HydrologyPoints);

print("Samples",samples);

Map.addLayer(samples, {color: 'red'}, 'samples');


// Collect the spectral information to get the trained samples
var trainedSamples = mosaic.reduceRegions({
    'collection': samples, 
    'reducer': ee.Reducer.first(), 
    'scale': 30
});
  
print("Trained samples",trainedSamples); // Not really trained samples:
  // this is the set of points with class and mosaic data
 
  
// Set up the Random Forest classifier
var classifier = ee.Classifier.smileRandomForest({
    'numberOfTrees': 500
});

// Training the classifier
classifier = classifier.train({
    'features': trainedSamples, 
    'classProperty': 'class', 
    'inputProperties': [
        'B2_max',
        'B2_median',
        'B2_min',
        'B3_max',
        'B3_median',
        'B3_min',
        'B4_max',
        'B4_median',
        'B4_min',
        'B5_max',
        'B5_median',
        'B5_min',
        'B6_max',
        'B6_median',
        'B6_min',
        'B7_max',
        'B7_median',
        'B7_min',
        'evi_max',
        'evi_median',
        'evi_min',
        'ndvi_max',
        'ndvi_median',
        'ndvi_min',
        'ndwi_max',
        'ndwi_median',
        'ndwi_min',
    ]
  });
    
  print("Classifier",classifier)
    
    
// Run the Random Forest classifier
var classification = mosaic.classify(classifier);

// Add classification to map
Map.addLayer(classification, {
        'min': 0,
        'max': 3,
        'palette': 'ffffff,ff0000,00aa00,0000ff',
        'format': 'png'
    },
    'classification'
);


// Export the mosaic to your asset
Export.image.toAsset({
    image: classification, 
    description: 'classification-2020', 
    assetId: 'classification-2020', 
    pyramidingPolicy: {'.default': 'mode'}, // use mode for classification data
    region: classification.geometry(), 
    scale: 30, 
    maxPixels: 1e13
});

//*/