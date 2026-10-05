var kenya = ee.FeatureCollection('FAO/GAUL/2015/level0')
  .filter(ee.Filter.eq('ADM0_NAME', 'Kenya'));

function prepLandsat(image) {
    var qa = image.select('QA_PIXEL');
    var mask = qa.bitwiseAnd(1 << 3).eq(0).and(qa.bitwiseAnd(1 << 4).eq(0));
    var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    return image.addBands(opticalBands, null, true).updateMask(mask);
}

function prepLandsatNoFilter(image) {
    var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    return image.addBands(opticalBands, null, true);
}

var l5Col = ee.ImageCollection("LANDSAT/LT05/C02/T1_L2");

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
Map.addLayer(blackBackground, {palette: ['222222']}, 'Background');

function exportNdviPeriod(startDate, endDate, label) {
    var start = ee.Date(startDate);
    var end = ee.Date(endDate);

    var ndviAll = l5Col
        .filterBounds(kenya)
        .filterDate(start, end)
        .map(prepLandsat)
        .map(function (img) {
            return img.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
        });

    var combinedNdvi = ndviAll.median();

    var ndviNoFilterAll = l5Col
        .filterBounds(kenya)
        .filterDate(start, end)
        .map(prepLandsatNoFilter)
        .map(function (img) {
            return img.normalizedDifference(['SR_B4', 'SR_B3']).rename('NDVI');
        });

    var combinedNdviNoFilter = ndviNoFilterAll.median();

    var finalNdvi = ee.Image.constant(-3)
        .where(combinedNdviNoFilter.mask(), -2)
        .where(combinedNdvi.mask(), combinedNdvi)
        .clip(kenya);

    var cloudPixels = finalNdvi.updateMask(finalNdvi.eq(-2));
    var noDataPixels = finalNdvi.updateMask(finalNdvi.eq(-3));

    Map.addLayer(combinedNdvi.clip(kenya), forestVis, label + ' NDVI');
    Map.addLayer(cloudPixels, {palette: ['00ffff']}, label + ' Clouded pixels (-2)');
    Map.addLayer(noDataPixels, {palette: ['ff00ff']}, label + ' Missing data (-3)');

    Export.image.toDrive({
        image: finalNdvi,
        description: 'NDVI_Kenya_' + label + '_Median_with_Flags',
        folder: 'UnsettledGround',
        region: kenya.geometry(),
        scale: 30,
        crs: 'EPSG:4326',
        maxPixels: 1e13
    });
}

exportNdviPeriod('1985-03-01', '1985-03-31', '1985_03');
exportNdviPeriod('1985-01-01', '1985-12-31', '1985');
exportNdviPeriod('1984-01-01', '1986-12-31', '1984-1986');