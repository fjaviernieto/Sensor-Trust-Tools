# Available Datasets Description

This folder contains some of the datasets that have been generated to carry out a deep analysis of heterogeneous sensors' datasets (in CSV format), in the context of my PhD research. The datasets available were the following:
* Arduino: Data obtained from an Arduino board with a set of sensors (temperature, humidity and moisture) taking measure in a room with a small plant.
* FaultInjectionDatasets: Several datasets in which bias, drift, malfunction and random errors were injected, from the work published by de Bruijn et al [1].
* PhysicsToolboxSuite: Data obtained from the Physics Toolbox Suite application, in this case focused on light intensity, noise level and device movement.

These are some of the original datasets used that include data from sensors measuring temperature, light intensity, moisture, humidity and movement/vibrations of a device. Although the research done includes additional types of sensors, such datasets are not available due to licensing restrictions. In the case of the published datasets in this folder, they contain the licensing information in their own folder, as different licensing schemes are used.

[1] de Bruijn, B., Nguyen, T.A., Bucur, D., Tei, K., 2016. Benchmark Datasets for Fault Detection and Classification in Sensor Data, in: Proceedings of the 5th International Confererence on Sensor Networks, SENSORNETS 2016. SCITEPRESS - Science and Technology Publications, Lda, Setubal, PRT, pp. 185–195. https://doi.org/10.5220/0005637901850195

## Structure

The folder is structured in the following way:

    .
    ├── Arduino                                    # Data obtained through a custom installation with an Arduino and a few sensors
    ├── FaultInjectionDatasets                     # Data from the work published by de Bruijn et al [1]
    │   ├──intel                  
    │   ├──santander
    │   └──sensorscope
    ├── PhysicsToolboxSuite                        # Data obtained using the Physics Toolbox Suite application with a smart phone    
    └── README.md

## Author

This work has been done by:

Francisco Javier Nieto ([Mail](mailto:fjavier.nieto@opendeusto.es) | [fjaviernieto](https://github.com/fjaviernieto) | [@BrkfstResearch](https://twitter.com/BrkfstResearch))

## License

These datasets are licensed under the Creative Commons Attribution-ShareAlike 4.0 International License and the Creative Commons Attribution 4.0 International license (CC BY 4.0) - see the LICENSE file in each folder for additional details.

## Reference this Work

If the provided scripts and data are useful for you and you want to reference this work, please, do so by referencing to the following paper:

Nieto FJ, Aguilera U, López-de-Ipiña D. Analyzing Particularities of Sensor Datasets for Supporting Data Understanding and Preparation. Sensors. 2021; 21(18):6063. https://doi.org/10.3390/s21186063

## Acknowledgments

I want to thank to all those colleagues and entities that provided me with the datasets that I needed to do the analysis and the evaluation of the solutions proposed. This includes Ports of Spain, Kunak, Meteoblue and the people from SZE.
