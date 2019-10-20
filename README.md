# DATA_paper_MSTLengthRatios
R codes to develop paper of MST Lengths of financial indices.

As input to the code we need csv file with finantial indices along the time (could be in weeks or days). As output we get the time series of MST length among all indices.



Let me explain some files here:

## Scripts

* `trying_v2.R`: get the correlations between every pair of ratios and computes the mst length for time windows of 40 periods with slice of 10 periods. This is done for daily and weekly data.

* ``trying_v3.R``: se calculan los MST length para los indices financieros.

* `trying.R`: first attempts to elaborate the mst along the time from a sample of ratios.

* `orientations.R`: computation of orientations from weekly and daily data.

* `get_orientation_function.R`: functions get_simple_orientation and get_windows_oriantations. The first one computes the net oriantation of a single state, and the second one computes the mean of net orientations for a time windows. 

* ``entropy_serieV1.R``: Se desarrollan las funciones para calcular la series de tiempo entropicas de los indices utilizando indices simulados de white noise, random walk.

* ``entropy_serieV2.R``: Se desarrollan las funciones para calcular la series de tiempo entropicas de los indices utilizando indices financieros reales.

* 

  

## Datasets and outputs

- `data150419.csv`: csv file indicating daily returns of different finantial indices. 
- `data170419.csv`: csv file indicating weekly returns of different finantial indices. 
- `data070319.csv`: csv file just for the first testing. Includes some finatial indices measures (shares and bonds). These data are used for testing purposes in trying.R
- `mst_lengths_daily_170419.csv` and `mst_lengths_weekly_170419.csv`: csv files outputs from trying_v2.R

* `orientations_daily_250419.csv` and `orientations_weekly_250419.csv`:  csv files outputs from orientations.R 
* ``entropies_from_entropy_serieV2_191019.csv``: Series de tiempo entropicas de todos los indices.
* ``mstlength_from_trying_v3_181019.csv`` : Se obtiene los largos de MST para cada mes de todos los contienentes.



## Results on the fly

* ``101019aresults_from_entropy_serieV1.doc``: Simulaciones de a) random walk, b) white noise, c) AR(1), para obtener la serie de tiempo entropica con distintas segmentos de tiempo T y con dtau=2, y 3.  Vemos las diferencias en la media y distribucion de la entropia para estos distintos casos.