# PsycoRF-PsycoDNet
----
PsycoRF and PsycoDNet are models assisting in diagnosis of psychosis/First Episode Psychosis(FEP) on basis of 49 subjects of blood metabolite.

* To assess the risk probability of psychosis on basis of you own blood metabolite data, refer to [example_data.xlsx](https://github.com/LiMuxiBADD/PsycoRF-PsycoDNet/blob/main/PsycoRF/example_data.xlsx) and [example_data.csv](https://github.com/LiMuxiBADD/PsycoRF-PsycoDNet/blob/main/PsycoDNet/example_data.csv)
  * Each row represents a record, columns from 'A/G' to 'WBC' are 49 indicators required for prediction
  * column 'dig' denotes the diagnosis label, 'N' for non-psychosis and 'P' for psychosis (not necessary for model prediction) 
  * Note that do not change the order of indicators, as they are organized in a fixed way

PsycoRF
----
* To utilize PsycoRF, first download and unzip the packed model [PsycoRF.zip](https://github.com/LiMuxiBADD/PsycoRF-PsycoDNet/blob/main/PsycoRF/PsycoRF.zip)
* Then follow the steps described in [PsycoRF.R](https://github.com/LiMuxiBADD/PsycoRF-PsycoDNet/blob/main/PsycoRF/PsycoRF.R)
