var europe = ee.FeatureCollection("users/agataelia1991/aoi/Europe_BB"),
    srtm = ee.Image("USGS/SRTMGL1_003"),
    socc005 = ee.Image("users/agataelia1991/Hansen/socc_005"),
    modisSample = ee.Image("users/agataelia1991/Hansen/modisSample");
    

// Set vis par
var elevationVis = {
  min: 0.0,
  max: 8000.0,
  palette: ['0000ff','00ffff','ffff00','ff0000','ffffff'],
};

var slopeVis = {
  min: 0.0,
  max: 90.0,
  palette: ['0000ff','00ffff','ffff00','ff0000','ffffff'],
};

// Explore the dataset
print(srtm);

// Retrieve elevation at 30m
var srtmElevation = srtm.select(['elevation']).clip(europe).float();
print(srtmElevation);

// Add and inspect layers
Map.addLayer(srtmElevation, elevationVis, 'srtm_30');

// Generate slope
var srtmSlope = ee.Terrain.slope(srtmElevation).clip(europe);
print(srtmSlope);

// Add and inspect layers
Map.addLayer(srtmSlope, slopeVis, 'slope_30');

// Inspect srtm crs and projection
print(srtmElevation.projection());
print(srtmElevation.projection().nominalScale()); 
print(socc005.projection());
print(socc005.projection().nominalScale());
print(modisSample.projection());
print(modisSample.projection().nominalScale()); 

// Export to drive
Export.image.toDrive({
  image: srtmElevation.clip(europe),
  description: 'srtmElevation',  
  folder: 'elevation',
  region: europe,
  crs: 'EPSG:4326',
  crsTransform: [0.00025, 0, -10.66499999999295, 0, -0.00025, 71.18499999995295], // as modis export
  maxPixels: 1e13});
  
Export.image.toDrive({
  image: srtmSlope.clip(europe),
  description: 'srtmSlope',  
  folder: 'elevation',
  region: europe,
  crs: 'EPSG:4326',
  crsTransform: [0.00025, 0, -10.66499999999295, 0, -0.00025, 71.18499999995295], // as modis export
  maxPixels: 1e13});
  