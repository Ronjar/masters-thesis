var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));

function prepLandsat89(image) {
    var qa = image.select('QA_PIXEL');
    var mask = qa.bitwiseAnd(1 << 3).eq(0).and(qa.bitwiseAnd(1 << 4).eq(0));
    var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    return image.addBands(opticalBands, null, true).updateMask(mask);
}

var start = ee.Date('2023-01-01');
var end = ee.Date('2025-12-31');

var l8Col = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2");
var l9Col = ee.ImageCollection("LANDSAT/LC09/C02/T1_L2");

var combinedCol = l8Col.merge(l9Col)
    .filterBounds(kenya)
    .filterDate(start, end)
    .map(prepLandsat89)
    .map(function (img) {
        return img.normalizedDifference(['SR_B5', 'SR_B4']).rename('NDVI');
    });

var combinedNdvi = combinedCol.median()
    .clip(kenya);

var forestVis = {
    min: 0.2,
    max: 0.75,
    palette: [
        'e5e5e5',
        'f2ebaa',
        '99cc66',
        '339933',
        '006600',
        '002b00'
    ]
};

Map.centerObject(kenya, 6);

var blackBackground = ee.Image(0).clip(kenya);
Map.addLayer(blackBackground, {palette: ['red']}, 'Leak detector');

Map.addLayer(combinedNdvi, forestVis, 'NDVI 2023-2025');

Export.image.toDrive({
  image: combinedNdvi,
  description: 'NDVI_Kenya_2023_2025_Median',
  folder: 'UnsettledGround',
  region: kenya.geometry(),
  scale: 30,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});