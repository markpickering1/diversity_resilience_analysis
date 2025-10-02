var SOCC = ee.Image("OpenLandMap/SOL/SOL_ORGANIC-CARBON_USDA-6A1C_M/v02");

// Define bounding box
var europe = ee.Algorithms.GeometryConstructors.BBox(-10.661639298049197, 34.56368725504253, 44.820364499806, 71.18416372752647);
Map.addLayer(europe);

// Explore the dataset
print(SOCC); 

// Retrieve SOCC at 30cm depth and rescale by scale factor (5)
var SOCC30cm = SOCC.select(['b30']).clip(europe);
var SOCC30cmRescaled = SOCC.select(['b30']).divide(5).clip(europe);

// Add and inspect layers
Map.addLayer(SOCC30cm);
Map.addLayer(SOCC30cmRescaled);

// Inspect SOCC crs and projection
print(SOCC30cmRescaled.projection());
print(SOCC30cmRescaled.projection().nominalScale());

// Get the SOCC at MODIS scale and projection
var SOCC30cmRescaledAtModis = SOCC30cmRescaled
    // Force the next reprojection to aggregate instead of resampling
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      maxPixels: 1024
    })
    // Request the data at the scale and projection of the MODIS image
    .reproject({ 
      crs: 'EPSG:4326',
      crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295]
    }); 

// Add layer to map
Map.addLayer(SOCC30cmRescaledAtModis); 

// Export the SOCC at MODIS 0.005 resolution with mean aggregation, WGS84 and transform
Export.image.toDrive({
  image: SOCC30cmRescaledAtModis, 
  description: 'SOCC30cmRescaledAtModis', 
  folder: 'modis',
  region: europe,
  crs: 'EPSG:4326',
  crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
  scale: 556.597453966,
  maxPixels: 1e13});