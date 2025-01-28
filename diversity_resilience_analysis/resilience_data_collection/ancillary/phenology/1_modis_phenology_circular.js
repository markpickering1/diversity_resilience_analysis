var lcd = ee.ImageCollection("MODIS/061/MCD12Q2"),
    europe = ee.FeatureCollection("users/agataelia1991/aoi/Europe_BB");

// Create a function to add the length of the growing season
var addLength = function(img){
  var sos = img.select(['Greenup_1']);
  var eos = img.select(['Dormancy_1']);
  var length = eos.subtract(sos).round().rename(['Length_1']);
  return img.addBands(length);  
}
;

// Create a function to convert days sice epoch (1 Jan 1970) to DOY
var dse2doy = function(img) {
  // get days since epoch for first day of year from properties of the image
  var firstDayOfYear = ee.Number(img.get('system:time_start')).divide(24*60*60*1000);
  // convert band values to day of year
  var doy = img.subtract(firstDayOfYear).add(1).toInt16();
  // return
  return img.addBands(doy);
};

// Create a function to adjust for doy <0 and >365 
var doy365 = function(img) {
  var imgOk = img.updateMask(img.gte(0).and(img.lte(365))).toInt16();
  var img0fix = (img.updateMask(img.lt(0))).add(365).toInt16();
  var img365fix = (img.updateMask(img.gt(365))).subtract(365).toInt16();
  var imgFixCollection = ee.ImageCollection.fromImages([imgOk, img0fix, img365fix]);
  var imgFix = imgFixCollection.sum().round();
  return imgFix;
};

// Create a function to convert doy into circular units
var doy2circ = function(img) {
  // get days of the year into circular units
  var circular = img.subtract(1).divide(365).multiply(2).multiply(Math.PI).float().rename(['Greenup_1_1_circular', 'Dormancy_1_1_circular']);
  // return
  return img.addBands(circular);
};

// length
var lcdLength = lcd
                .map(addLength)
                .select(['Length_1']);

// lcd in DOY and adjusted for doy <0 and >365 
var lcdDOY = lcd
             .select(['Greenup_1', 'Dormancy_1']) 
             .map(dse2doy)
             .select(['Greenup_1_1', 'Dormancy_1_1'])
             .map(doy365);

// lcd in DOY, adjusted and in circular unit
var lcdDOYcircular = lcdDOY
                     .map(doy2circ);
                     //.select(['Greenup_1_1_circular', 'Dormancy_1_1_circular']);

// Inspect collections
print(lcdLength);
print(lcdDOY);
print(lcdDOYcircular);
Map.addLayer(lcdLength.first());
Map.addLayer(lcdDOY.first());
Map.addLayer(lcdDOYcircular.first());

// Reduce length collection with mean
var lcdLengthMean = lcdLength.mean().round().rename(['Length_1_mean']);
Map.addLayer(lcdLengthMean);

// Reduce greenup collection with mean
var lcdGreenupNormalMean = lcdDOY.select(['Greenup_1_1']).reduce(ee.Reducer.mean()).rename(['Greenup_1_mean']);
Map.addLayer(lcdGreenupNormalMean);

// Reduce dormancy collection with mean
var lcdDormancyNormalMean = lcdDOY.select(['Dormancy_1_1']).reduce(ee.Reducer.mean()).rename(['Dormancy_1_mean']);
Map.addLayer(lcdDormancyNormalMean);

// Reduce greenup collection with circular mean
var lcdGreenupCircularMean = lcdDOYcircular.select(['Greenup_1_1_circular']).reduce(ee.Reducer.circularMean()).rename(['Greenup_1_circular_mean']);
Map.addLayer(lcdGreenupCircularMean);

// Reduce dormancy collection with circular mean
var lcdDormancyCircularMean = lcdDOYcircular.select(['Dormancy_1_1_circular']).reduce(ee.Reducer.circularMean()).rename(['Dormancy_1_circular_mean']);
Map.addLayer(lcdDormancyCircularMean);

// Convert greenup back to doy
var lcdGreenupMean = lcdGreenupCircularMean.multiply(365).divide(2).divide(Math.PI).add(1);
Map.addLayer(lcdGreenupMean);

// Convert dormancy back to doy
var lcdDormancyMean = lcdDormancyCircularMean.multiply(365).divide(2).divide(Math.PI).add(1);
Map.addLayer(lcdDormancyMean);

// Adjust for 365
var lcdGreenupMeanFix = doy365(lcdGreenupMean);
var lcdDormancyMeanFix = doy365(lcdDormancyMean);
Map.addLayer(lcdGreenupMeanFix);
Map.addLayer(lcdDormancyMeanFix);

// Export at 0.005 resolution
Export.image.toDrive({
image: lcdLengthMean.clip(europe).toInt16().unmask(-32767), // Export as Int16 to save space
description: 'modis_lcd_length_1_mean',
folder: 'phenology',
scale: 556.597453966, // Export at 0.005 resolution
crs: 'EPSG:4326',
crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
region: europe,
fileFormat: 'GeoTIFF',
maxPixels: 1e13});

// Export at 0.005 resolution
Export.image.toDrive({
image: lcdGreenupMeanFix.clip(europe).toInt16().unmask(-32767), // Export as Int16 to save space
description: 'modis_lcd_greenup_1_circular_mean',
folder: 'phenology',
scale: 556.597453966, // Export at 0.005 resolution
crs: 'EPSG:4326',
crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
region: europe,
fileFormat: 'GeoTIFF',
maxPixels: 1e13});

// Export at 0.005 resolution
Export.image.toDrive({
image: lcdDormancyMeanFix.clip(europe).toInt16().unmask(-32767), // Export as Int16 to save space
description: 'modis_lcd_dormancy_1_circular_mean',
folder: 'phenology',
scale: 556.597453966, // Export at 0.005 resolution
crs: 'EPSG:4326',
crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
region: europe,
fileFormat: 'GeoTIFF',
maxPixels: 1e13});