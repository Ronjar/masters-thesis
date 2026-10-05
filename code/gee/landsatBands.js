var roi = ee.Geometry.Rectangle([34.9, -1, 36, 0.5]);

function prepLandsat8(image) {
    var qa = image.select('QA_PIXEL');
    var mask = qa.bitwiseAnd(1 << 3).eq(0).and(qa.bitwiseAnd(1 << 4).eq(0));
    var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    return image.addBands(opticalBands, null, true).updateMask(mask);
}

var start = ee.Date('2022-01-01');
var end = ee.Date('2022-12-31');

var l8Col = ee.ImageCollection("LANDSAT/LC08/C02/T1_L2");

var processedL8 = l8Col
    .filterBounds(roi)
    .filterDate(start, end)
    .map(prepLandsat8)
    .median()
    .clip(roi);

var lsatExport = processedL8.select(['SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6', 'SR_B7']);

var landcover = ee.ImageCollection('ESA/WorldCover/v200')
    .filterBounds(roi)
    .first()
    .clip(roi);

Map.centerObject(roi, 7);

var blackBackground = ee.Image(0).clip(roi);
Map.addLayer(blackBackground, {palette: ['red']}, 'Leak detector', false);

var rgbVis = {
    bands: ['SR_B4', 'SR_B3', 'SR_B2'],
    min: 0,
    max: 0.15
};
Map.addLayer(lsatExport, rgbVis, 'Landsat 8 RGB 2023', false);
Map.addLayer(landcover, {}, 'ESA WorldCover 2021', false);

Export.image.toDrive({
    image: lsatExport,
    description: 'Landsat8_AllBands',
    folder: 'UnsettledGround',
    region: roi,
    scale: 30,
    crs: 'EPSG:4326',
    maxPixels: 1e13
});

Export.image.toDrive({
    image: landcover,
    description: 'ESA_WorldCover',
    folder: 'UnsettledGround',
    region: roi,
    scale: 30,
    crs: 'EPSG:4326',
    maxPixels: 1e13
});