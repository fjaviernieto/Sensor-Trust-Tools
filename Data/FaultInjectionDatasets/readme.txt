This file describes the main aspects of the benchmark dataset.

1. Directory structure
The top directory consists of three folders, one for each of the three base datasets that we build upon. Each of the three top directories contain three sub-directories: one for the non-interpolated subsets (i.e. the original timestamps are used), one for the interpolated subsets and one folder containing the raw, original data. The non-interpolated and interpolated folders then each contain seven subfolders, one containing clean data, and six containing the following injected fault types: random, malfunction, bias, drift, polynomial drift and mixed faults. The figure below provides an overview of the folders. If a dataset offers multiple phenomenena, for example both light and temperature, there will be a file for each phenomenon within each folder for the fault types. For example, the intel folder /intel/interpolated/injected_drift contains files for both light and temperature, for all used sensor nodes.


                                + interpolated +---+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                                |
                                |
                                |
                 ++--+Intel+----+ non+interpolated +--+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                  |             |
                  |             |
                  |             |
                  |             + raw
                  |
                  |
                  |
                  |
                  |             + interpolated+---+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                  |             |
                  |             |
                  |             |
Root  +-------------Santander+--+ non+interpolated +---+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                  |             |
                  |             |
                  |             |
                  |             + raw
                  |
                  |
                  |
                  |
                  |                + interpolated +-----+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                  |                |
                  |                |
                  |                |
                 ++--+SensorScope+-+ non+interpolated +---+ clean, random, malfunction, bias, drift, polynomial drift, mixed
                                   |
                                   |
                                   |
                                   + raw

2. Data description
The dataset contains the following data types:

Intel: 
light and temperature (indoors)

Santander: 
temperature (outdoors)

SensorScope: 
temperature (outdoors)


The fault types in the datasets are encoded as such:
No fault = 0
Random = 1
Malfunction = 2
Bias = 4
Drift = 8
Polydrift = 16
Polydriftnoise = 32
