# FluxDataKit

### v3.4.3

- Added spurious (=repeated) values identification for determining good-quality sequences.

### v3.4.2

- Corrected root zone water storage capacity (whc in site meta info). Rooting depth was erronneously read instead of rooting zone water storage capacity.
- Changed from using energy-balance corrected latent heat flux (`LE_CORR`) to the non-corrected values (`LE_F_MDS`) in rsofun driver data. This was changed back to non-energy balance corrected values because daily energy balance corrected latent energy flux was larger than net radiation for a substantial number of data points. This makes it impossible to meaningfully model the latent energy flux and prohibits robust model calibration.

### v3.4.1

- Corrected missing mat and p/pet in site meta info file.

## v3.4

- Complemented site info file with vegetation height, measurement height, mean annual temperature (`mat`) and mean annual precipitation over potential evapotranspiration (`p_over_pet`).
- Revised site info file: classification into vegetation zones.

## Previous versions

- No descriptions available.
