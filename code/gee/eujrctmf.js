var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));

var tmf = ee.ImageCollection('projects/JRC/TMF/v1_2025/AnnualChanges').mosaic();

var tmf1990 = tmf.select('Dec1990').clip(kenya);
var tmf2025 = tmf.select('Dec2025').clip(kenya);

var forest1990 = tmf1990.eq(1).or(tmf1990.eq(2));
var forest2025 = tmf2025.eq(1).or(tmf2025.eq(2));

var degradedFrequency = tmf.eq(2).reduce(ee.Reducer.sum()).toByte().clip(kenya);
var degraded = forest1990.multiply(degradedFrequency).toByte();

var deforested = forest1990.and(tmf2025.eq(3));

var forest1990Masked = forest1990.updateMask(forest1990.gt(0));
var degradedMasked = degraded.updateMask(degraded.gt(0));
var deforestedMasked = deforested.updateMask(deforested.gt(0));

Map.centerObject(kenya, 6);
Map.addLayer(forest1990Masked, {palette: ['darkgreen']}, 'ForestCover 1990');
Map.addLayer(degradedMasked, {min: 1, max: 15, palette: ['yellow', 'orange', 'red']}, 'Degraded Frequency');
Map.addLayer(deforestedMasked, {palette: ['red']}, 'Deforested 2025');

Export.image.toDrive({
  image: forest1990,
  description: 'ForestCover_Kenya_1990_EUJRCTMF',
  folder: 'UnsettledGround', 
  scale: 30,             
  region: kenya.geometry(),
  maxPixels: 1e13
});

Export.image.toDrive({
  image: forest2025,
  description: 'ForestCover_Kenya_2025_EUJRCTMF',
  folder: 'UnsettledGround', 
  scale: 30,             
  region: kenya.geometry(),
  maxPixels: 1e13
});

Export.image.toDrive({
  image: degraded,
  description: 'Degraded_Frequency_Kenya_1990_2025_EUJRCTMF',
  folder: 'UnsettledGround',
  scale: 30,
  region: kenya.geometry(),
  maxPixels: 1e13,
  fileDimensions: 65536
});

Export.image.toDrive({
  image: deforested,
  description: 'Deforested_Kenya_2025_EUJRCTMF',
  folder: 'UnsettledGround',
  scale: 30,
  region: kenya.geometry(),
  maxPixels: 1e13
});