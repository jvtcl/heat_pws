# heat_pws

The primary purpose of this work is to gather heat hazard data for social vulnerability assessment. The [Weather Underground API](https://www.wunderground.com/weather/api/) and R library [`rwunderground`](https://rdrr.io/cran/rwunderground/) are used to generate a coverage of current temperature conditions from personal weather stations and airports that will be compared to vulnerability metrics developed from tract and neighborhood-level socioeconomic data.

A "rough draft" of the code can be found in `maricopa_example.R`. An accompanying illustration is available in `example_phoenix_08_09.png`. Raw data obtained from Wunderground is not available through this repository. However, the code provided here, along with a valid API key, can be used to gather your own.
