var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));

var srtm = ee.Image("USGS/SRTMGL1_003");

var kenya_dem = srtm.clip(kenya);

Map.centerObject(kenya, 6);
Map.addLayer(kenya_dem, {min: 0, max: 3000, palette: ['blue', 'green', 'yellow', 'red']}, 'Kenya DEM');

Export.image.toDrive({
  image: kenya_dem,
  description: 'SRTM_Kenya_30m',
  folder: 'UnsettledGround',
  scale: 30,
  region: kenya.geometry(),
  maxPixels: 1e10,
  fileFormat: 'GeoTIFF',
  crs: 'EPSG:4326',
});