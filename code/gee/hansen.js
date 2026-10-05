var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));

var hansen = ee.Image('UMD/hansen/global_forest_change_2025_v1_13').clip(kenya);

var treeCover2000 = hansen.select('treecover2000').gt(10);

var allLoss = hansen.select('lossyear').gt(0);

var validLoss = allLoss.and(treeCover2000);

var treeCoverMasked = treeCover2000.updateMask(treeCover2000.gt(0));
var lossMasked = validLoss.updateMask(validLoss.gt(0));

Map.centerObject(kenya, 6);
Map.addLayer(treeCoverMasked, {palette: ['darkgreen']}, 'Tree Cover 2000 (>20%)');
Map.addLayer(lossMasked, {palette: ['red']}, 'Forest cover loss 2000-2025');


Export.image.toDrive({
  image: treeCover2000, 
  description: 'Hansen_Kenya_TreeCover_2000_Threshold10',
  folder: 'UnsettledGround', 
  scale: 30,             
  region: kenya.geometry(),
  maxPixels: 1e13
});

Export.image.toDrive({
  image: validLoss,
  description: 'Hansen_Kenya_TreeLoss_2025_Filtered',
  folder: 'UnsettledGround',
  scale: 30,
  region: kenya.geometry(),
  maxPixels: 1e13
});