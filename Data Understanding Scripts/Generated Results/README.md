# Available Datasets Description

This folder contains datasets that have been generated when running the R scripts with the datasets obtained from heterogeneous sensors, in the context of my PhD research. The datasets available were the following:
* Arduino: Data obtained from an Arduino board with a set of sensors (temperature, humidity and moisture) taking measure in a room with a small plant.
* Injected Errors: Several datasets in which bias, drift, malfunction and random errors were injected, from the work published by de Bruijn et al [1].
* Puertos del Estado: Data provided by Ports of Spain, from different buoys, weather stations and tide gauges that measured air temperature, atmospheric pressure, water salinity, waves altitude, wind speed, water temperature and currents speed, among others .
* Toolbox: Data obtained from the Physics Toolbox Suite application, in this case focused on light intensity, noise level and device movement.
* Bosch/Gyor: Data provided by SZE, about air quality in the city of Gyor, measuring aspects like NO2, O3, PM10, PM2P5, air pressure and temperature.

Some of these datasets have been shared in this repository, under [Sensor-Trust-Tools/Data](../../Data). The rest of datasets have not been shared because they are private and there is no permission to do so. Still the results of their processing is available here, since the resulting datasets represent basic statistical results, results from statistical tests and other additional processing, that do not disclose the original time series.

[1] de Bruijn, B., Nguyen, T.A., Bucur, D., Tei, K., 2016. Benchmark Datasets for Fault Detection and Classification in Sensor Data, in: Proceedings of the 5th International Confererence on Sensor Networks, SENSORNETS 2016. SCITEPRESS - Science and Technology Publications, Lda, Setubal, PRT, pp. 185–195. https://doi.org/10.5220/0005637901850195

## Structure

The folder is structured in the following way (according to the data produced for the paper published and to the data resulting from the research done for the PhD):

    .
    ├── PhD Results                   # Results from PhD research analysis and validation
    │   ├──Arduino                  
    │   ├──Injected Errors
    │   ├──Puertos del Estado
    │   ├──Toolbox
    │   └──Validation
    │      ├──Gyor
    │      └──SensorScope
    ├── Sensors Paper Results         # Results obtained when preparing the paper published in the Sensors journal    
    ├── LICENSE
    └── README.md

## Author

This work has been done by:

Francisco Javier Nieto ([Mail](mailto:fjavier.nieto@opendeusto.es) | [fjaviernieto](https://github.com/fjaviernieto) | [@BrkfstResearch](https://twitter.com/BrkfstResearch))

## License

These datasets are licensed under the Attribution NonCommercial-NoDerivs CC BY-NC-ND License (CC BY-NC-ND 4.0) - see the LICENSE.md file for details

## Reference this Work

If the provided scripts and data are useful for you and you want to reference this work, please, do so by referencing to the following paper:

Nieto FJ, Aguilera U, López-de-Ipiña D. Analyzing Particularities of Sensor Datasets for Supporting Data Understanding and Preparation. Sensors. 2021; 21(18):6063. https://doi.org/10.3390/s21186063

## Acknowledgments

I want to thank to all those colleagues and entities that provided me with the datasets that I needed to do the analysis and the evaluation of the solutions proposed. This includes Ports of Spain, Kunak, Meteoblue and the people from SZE.
