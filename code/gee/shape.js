var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));
var kenyaCounties = ee.FeatureCollection('FAO/GAUL/2015/level2')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));
  
Map.addLayer(kenya, {palette: ['red']}, 'Kenya');
Map.addLayer(kenyaCounties, {palette: ['red']}, 'Counties');

Export.table.toDrive({
  collection: kenya,
  description: 'KenyaCounties_Shape',
  folder: 'UnsettledGround',
  fileFormat: 'GeoJSON'
});
Export.table.toDrive({
  collection: kenya,
  description: 'Kenya_Shape',
  folder: 'UnsettledGround',
  fileFormat: 'GeoJSON'
});