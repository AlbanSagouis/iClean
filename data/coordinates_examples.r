coords <- c('52.473892, 13.40444',
            '52,473892; 13,40444', '52,473892 13,40444',
            '52°28’26”, 13°24’16”', '52°28’26”N, 13°24’16”E','N52°28’26”, E13°24’16”',
            '52°28’26’’, 13°24’16’’','52°28’26"’, 13°24’16"', "52°28'26'', 13°24'16''",
            '52 28 26, 13 24 16', '52 28 26.13 24 16', '52°28’26” 13°24’16”',
            'S52°28’26”, W13°24’16”', '52°28’26”S, 13°24’16”W', 'S52°28’26” E13°24’16”',
            '13.40444E, 52.473892N', '13°24’16”E, 52°28’26”N',
            '13.40444, 52.473892', '13°24’16”, 52°28’26”',
            '52°28’260”, 13°64’16”', '92°28’26”, 190°24’16”'
)

id <- rep("Site 1", length(coords))

load('./data/coordinates_test_dataset_1')
