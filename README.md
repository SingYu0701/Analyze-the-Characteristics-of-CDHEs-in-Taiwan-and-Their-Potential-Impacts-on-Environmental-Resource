# Analyze the Characteristics of Compound Drought and Heatwave Events in Taiwan and Their Potential Impacts on Environmental Resources
**Master Thesis , Dec 2025@ NCKU Resource Engineering**

**Bao, S.-Y. (2026). *Analysis of compound drought and heatwave events in Taiwan and their potential impacts on environmental resources* (Master’s thesis, National Cheng Kung University, supervised by Kuo-Chin Hsu).**

![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)

## Overview

**This repository contains all scripts, analyses, figures, and supporting materials for the research thesis “Analysis of Compound Drought and Heatwave Events (CDHEs) in Taiwan and Their Potential Impacts on Environmental Resources.”**

The central scientific premise of this study is that compound drought–heatwave events (CDHEs) should not be conceptualized as mere temporal overlaps of two extremes. Instead, CDHEs are treated as nonlinear, asymmetric, and lagged climate processes, emerging from interactions among precipitation deficits, land–atmosphere energy accumulation, and large-scale climate variability (e.g., ENSO).


## 1.Research Motivation

Recent climate-impact studies increasingly recognize that the most severe societal and environmental damages arise from compound climate extremes, rather than from isolated hazards. In Taiwan, droughts and heatwaves frequently co-occur or occur in close succession, amplifying stress on water resources, energy systems, agriculture, and public health.

Despite this recognition, three major limitations persist in the existing literature:

- **Univariate bias**: Many drought and heatwave assessments rely on single-index thresholds and implicitly assume independence.

- **Stationarity assumptions**: Traditional trend analyses often fail to detect abrupt regime shifts or nonlinear transitions under climate change.
  
- **Limited regional specificity**: Large-scale studies frequently obscure local hydroclimatic heterogeneity and ENSO-modulated responses.
  
This study addresses these gaps by integrating **Bayesian change-point detection**, **lagged dependency analysis**, and **copula-based joint probability modeling** to explicitly characterize the evolving dependence structure between droughts and heatwaves in Taiwan.

## 2.Data Description

### 2.1 Meteorological Observations

The core meteorological datasets consist of **daily precipitation** and **daily maximum temperature** spanning 1995–2024, obtained from the Central Weather Administration (CWA) of Taiwan.

Two representative stations are selected as diagnostic case studies:

- Keelung Station (Northern Taiwan)
Characterized by strong monsoonal precipitation, dense population, and complex water-supply infrastructure.

- Hengchun Station (Southern Taiwan)
Influenced by tropical climatic conditions, high baseline temperatures, and agriculture- and aquaculture-dominated land use.

These stations are not intended to represent the entirety of Taiwan; rather, they are chosen to **highlight regional heterogeneity** in compound climate risk.

![area_fig](https://github.com/user-attachments/assets/922f6f74-78f5-4d85-979d-cc8ead092fc0)


### 2.2 Climate Extreme Indices

Two standardized climate indices are employed:

<div align="center">
  
|Index	|Definition|Notes|
|------|---|---|
|SPI1	|Standardized Precipitation Index (1‑month)|Gamma distribution fitted to monthly precipitation; transformed to standard normal|
|HWDI	|Heatwave Duration Index|Counts consecutive days exceeding the day‑specific 95th percentile of daily Tmax|

</div>

The SPI follows WMO‑recommended procedures and enables interannual comparability across climatic regimes. HWDI emphasizes event persistence, which is critical for assessing impacts on energy demand, health outcomes, and agricultural stress.

Drought conditions are defined as **SPI ≤ −1**, while heatwave occurrence is indicated by **HWDI > 0**.

### 2.3 Large‑Scale Climate Forcing

#### 2.3.1 ENSO Forcing (Oceanic Niño Index)

The **Oceanic Niño Index (ONI)** is employed to classify El Niño–Southern Oscillation (ENSO) phases and evaluate their modulation effects on drought–heatwave dependence. ENSO is a dominant mode of interannual climate variability affecting East Asian precipitation patterns, temperature anomalies, and seasonal circulation.

Monthly ONI values are used to stratify the analysis into El Niño, La Niña, and neutral phases. This stratification enables examination of:

- Changes in CDHE frequency under different ENSO regimes

- Variations in lagged SPI–HWDI dependence strength

- Shifts in joint exceedance probabilities during specific climate backgrounds

Rather than treating ENSO as a direct causal driver, it is interpreted as a **conditioning factor** that alters background hydroclimatic states and the likelihood of compound extremes.

#### 2.3.2 Environmental and Socioeconomic System Indicators (EFW‑H Framework)

To evaluate the broader impacts of CDHEs, annual or monthly indicators are compiled across four interconnected systems:

- **Energy**: Household electricity consumption, reflecting cooling demand during prolonged heat stress

- **Water**: Reservoir storage levels, capturing drought-induced supply constraints and delayed depletion effects

- **Food**: Rice production and inland aquaculture yield, representing climate sensitivity in both rain-fed and water-dependent systems

- **Health**: Heat-related illness statistics (2012–2024), indicating human vulnerability to prolonged thermal stress

Together, these indicators form an integrated **Energy–Food–Water–Health (EFW‑H)** assessment framework. The objective is not prediction, but **interpretation** of how compound climate extremes propagate across systems through both concurrent and lagged pathways.

## 3.Methodology Framework

The analytical strategy follows a five‑stage inference pipeline, explicitly formalized in the thesis and reproduced here for transparency:

<div align="center"> 
  
|Raw meteorological observations|
  |---|
|↓|
|SPI / HWDI construction|
|↓|
|Bayesian change‑point detection (BEAST)|
|↓|
|Lagged dependency analysis (ONI → SPI → HWDI)|
|↓|
|Copula‑based joint probability modeling|
|↓|
|EFW‑H system impact interpretation|
</div>

### 3.1 Index Construction

Monthly SPI values are derived by **fitting a gamma distribution to accumulated precipitation and transforming cumulative probabilities into standard normal space**. This approach accommodates precipitation non‑negativity while ensuring statistical comparability.

$$SPI = \Phi^{-1}(G(x))$$

HWDI is defined as the number of days within a month that belong to heatwave events, where a heatwave **consists of three or more consecutive days exceeding the climatological 95th percentile** of daily maximum temperature (baseline: 1995–2014).

$$HWDI = ∑_{i=1}^{n} d_i (T_{max,i} > T_{95}),  \text{for }  d ≥ 3$$

<div align="center"> 
  
![fig4-1](https://github.com/user-attachments/assets/0f89b4ab-fbe0-4a66-a882-4b15e3edf77f)

</div>

### 3.2 Bayesian Change‑Point Detection (BEAST)

The Bayesian Estimator of Abrupt Change, Seasonal Change, and Trend (BEAST) is a fully Bayesian change-point detection framework designed to analyze non-stationary time series with complex temporal structures. BEAST models each climate index time series as the additive combination of multiple components:

- Piecewise linear trend components
- Seasonal harmonic signals
- Random errors

Under the Bayesian paradigm, both the number and locations of change points are treated as unknown random variables. Prior distributions are assigned to model parameters, including trend slopes, seasonal amplitudes, and change-point configurations, allowing structural complexity to be inferred directly from the data rather than specified a priori. Posterior distributions are then estimated via Markov Chain Monte Carlo (MCMC) sampling, providing probabilistic inference on the timing and significance of structural changes.

Change points are identified based on posterior probabilities, which quantify the likelihood of regime shifts occurring at specific time steps. This probabilistic formulation enables BEAST to detect both abrupt transitions and gradual trend modifications while accounting for uncertainty in parameter estimation. As a result, BEAST is particularly well suited for analyzing climate indices influenced by long-term forcing, interannual variability, and episodic extremes without requiring pre-defined breakpoints or stationarity assumptions.


### 3.3 Lagged Dependency Analysis

**Spearman rank correlations** are computed across multiple lag windows to assess delayed relationships between drought and heatwave processes. Results consistently indicate a **lag of approximately five months**, supporting a physical interpretation involving soil‑moisture depletion and land–atmosphere energy feedbacks.

<div align="center"> 

![fig7-1](https://github.com/user-attachments/assets/7e1fd884-a832-470a-a74c-f2af1ae0e779)

</div>

### 3.4 Copula‑Based Dependence Modeling

To quantify asymmetric joint extremes, marginal SPI and HWDI distributions are transformed into pseudo‑observations and modeled using a **270° rotated Clayton copula**. 

$$C_\theta^{(270)}(u,v) = u - C_\theta(u, 1-v)$$

This family is selected for its ability to represent:

- Lower‑tail dependence in SPI (**drought severity**)

- Upper‑tail dependence in HWDI (**heatwave intensity**)

<div align="center"> 
  
![fig8-1](https://github.com/user-attachments/assets/05ffe103-d36b-4f80-b478-041caee3fcfe)

</div>

Model parameters are estimated via maximum likelihood, and comparative diagnostics confirm superior performance relative to symmetric copula families.

Joint probabilities and return periods are subsequently derived to characterize CDHE risk.

### 3.5 EFW‑H Impact Assessment

Finally, CDHE occurrence is linked to energy, food, water, and health indicators to evaluate both concurrent and lagged system responses, **emphasizing interpretability and cross‑sectoral vulnerability** rather than prediction.

## 4.Major Findings

### 4.1 Intensification and Regime Shifts

BEAST results reveal statistically robust intensification of droughts, heatwaves, and CDHEs after approximately 2010. These changes manifest as increases in event frequency, persistence, and joint probability, **consistent with enhanced background warming and altered precipitation variability**.

![beast_fig_wxt](https://github.com/user-attachments/assets/ab5459bf-b1e9-4ed9-920e-349f0decda5e)

<div align="center"> 
 
  **Keelung**

| Variable | Driver | Trend_ρ | Trend_p | Season_ρ | Season_p | Error_ρ | Error_p |
|----------|--------|---------|---------|----------|----------|---------|---------|
| SPI      | CO₂    | -0.917  | **0.001**   | 0.534    | **0.002**    | -0.12   | 0.5     |
| SPI      | ONI    | -0.199  | **0.001**  | -0.042   | 0.427    | -0.112  | **0.03**    |
| HWDI     | CO₂    | 0.856   | **0.001**   | 0.917    | **0.001**    | 0.115   | 0.54    |
| HWDI     | ONI    | 0.109   | **0.038**  | 0.045    | 0.395    | -0.2    | **0.001**   |

**Hengchun**

| Variable | Driver | Trend_ρ | Trend_p | Season_ρ | Season_p | Error_ρ | Error_p |
|----------|--------|---------|---------|----------|----------|---------|---------|
| SPI      | CO₂    | -0.874  | **0.001**   | 0.925    | **0.001**    | -0.01   | 0.5     |
| SPI      | ONI    | -0.106  | **0.044**   | -0.052   | 0.325    | -0.08   | 0.09    |
| HWDI     | CO₂    | 0.657   | **0.001**   | 0.47     | **0.009**    | 0.106   | 0.573   |
| HWDI     | ONI    | 0.138   | **0.009**   | 0.002    | 0.969    | 0.108   | **0.04**    |
</div>

### 4.2 Asymmetric Dependence Structure

The Copula analysis quantifies the joint occurrence of drought (SPI ≤ −1) and heatwave (HWDI > 0) events. In Keelung, compound events are rarer but more extreme, whereas in Hengchun they occur more frequently but with moderate intensity.

<div align="center"> 
  
![fig9](https://github.com/user-attachments/assets/807efdd2-0523-43be-bb3c-9e53deec71c9)

</div>

Joint return periods are computed with an upper limit of 200 months to avoid extreme values. SPI is bounded between −3 and 0, HWDI between 0 and 15, with 100 discretization intervals for probability grids. Example results:

- Keelung: heatwave 7.69 months, drought 26.21 months, CDHE 28.02 months

- Hengchun: heatwave 4.67 months, drought 26.47 months, CDHE 26.47 months

<div align="center"> 
  
![fig10-1](https://github.com/user-attachments/assets/66d21139-3f8c-4490-ba2f-7552e12edd4c)

</div>

Copula analysis confirms pronounced nonlinear dependence between drought and heatwave processes:

- **Northern Taiwan (Keelung)** exhibits stronger drought‑to‑heatwave coupling during **El Niño** phases.

- **Southern Taiwan (Hengchun)** shows higher baseline CDHE frequency, particularly under **La Niña** phases.

Monte Carlo simulations indicate that approximately **23% of drought months in Keelung** and **29% in Hengchun** are followed by heatwave occurrence within the identified lag window.

<div align="center"> 
  
![fig11-1](https://github.com/user-attachments/assets/e00c4035-1950-4abe-8f08-4ecbe73ed8bf)

</div>

### 4.3 Regional System Vulnerability

EFW‑H analysis highlights distinct regional vulnerability pathways:

- Northern Taiwan experiences delayed but cascading impacts on electricity demand, reservoir storage, agricultural output, and heat‑related illness.

- Southern Taiwan exhibits more immediate impacts on aquaculture and public health, with comparatively weaker lagged responses.

These contrasts underscore the importance of **region‑specific adaptation strategies** and challenge the applicability of uniform climate‑risk metrics.

<div align="center"> 
  
![fig12-1](https://github.com/user-attachments/assets/622c9987-84b8-43f6-8bce-17b565aa46c1)

</div>

## 5.Conclusions

This study systematically quantified CDHEs in Taiwan (1995–2024) using BEAST, rotated Clayton copulas, and lagged response analysis. Key findings:

- Droughts and heatwaves are jointly influenced by long-term warming and ENSO variability.

- BEAST reveals decreasing SPI and increasing HWDI trends; structural changes around 2015.

- Copula-based analyses show distinct dependence patterns; return periods are 28 months (Keelung) and 26 months (Hengchun).

- CDHEs have measurable cross-sectoral impacts on energy, food, water, and health, with regional contrasts.

These results **provide actionable insights for climate adaptation**, emphasizing the need for region-specific, time-sensitive risk management strategies in response to compound climate extremes.

## 6.Limitations and Scope

Several limitations should be acknowledged:

- Copula models perform best for SPI < 0 and HWDI > 0 (tail behavior).

- Return periods reflect theoretical expectations, not direct observations.

- Anthropogenic water management and urban heat-island effects not included.

- Future work could explore regime-dependent copulas, additional conditioning variables, or process-based model coupling

## Reference

Alam, A. T. M., Rahman, M. S., & Saadat, A. H. M. (2013). Monitoring meteorological and agricultural drought dynamics in Barind region Bangladesh using standard precipitation index and Markov chain model. International Journal of Geomatics and Geosciences, 3, 511–524.

Awasthi, A., Vishwakarma, K., & Pattnayak, K. C. (2022). Retrospection of heatwave and heat index. Theoretical and Applied Climatology, 147(1–2), 589–604. https://doi.org/10.1007/s00704-021-03854-z
Baddoo, T. D., Guan, Y., Zhang, D., & Andam-Akorful, S. A. (2015). Rainfall Variability in the Huangfuchuang Watershed and Its Relationship with ENSO. Water, 7(7), 3243-3262. https://doi.org/10.3390/w7073243

Brás, T. A., Seixas, J., Carvalhais, N., & Jägermeyr, J. (2021). Severity of drought and heatwave crop losses tripled over the last five decades in Europe. Environmental Research Letters, 16(6), 065012. https://doi.org/10.1088/1748-9326/abf004

Centers for Disease Control, Ministry of Health and Welfare. (2025). Real-time epidemic surveillance and warning system: Heat-related emergency visits. https://data.gov.tw/dataset/157637

Central Weather Administration. (2025). CODiS Climate Observation Data Inquiry Service. Central Weather Administration. https://codis.cwa.gov.tw/

Chand, S., & Dhaliwal, L. K. (2024). Relationship of ENSO and standardized precipitation index (SPI) to characterize drought at different locations of Punjab, India. International Journal of Environment and Climate Change, 14(3), 95–105. https://doi.org/10.9734/ijecc/2024/v14i34022

Chen, J. M., & Lu, M. M. (2000). Interannual variation of the Asian-Pacific atmospheric system in association with the northern summer SST changes. Terrestrial, Atmospheric and Oceanic Sciences, 11, 833–860.

Chen, J. M., Hsieh, C. M., & Liu, J. S. (2012). Possible influences of ENSO on winter shipping in the North Pacific. Terrestrial, Atmospheric and Oceanic Sciences, 23(4), 397–411. https://doi.org/10.3319/TAO.2012.03.02.01(A)

Cowan, T., Purich, A., Perkins, S., Pezza, A., Boschat, G., & Sadler, K. (2014). More frequent, longer, and hotter heat waves for Australia in the Twenty-First Century. Journal of Climate, 27(15), 5851-5871. https://doi.org/10.1175/JCLI-D-14-00092.1

Dai, A. (2011). Drought under global warming: A review. Wiley Interdisciplinary Reviews: Climate Change, 2(1), 45–65. https://doi.org/10.1002/wcc.81

Dar, J. A., & Apurv, T. (2024). Spatiotemporal characteristics and physical drivers of heatwaves in India. Geophysical Research Letters, 51, e2024GL109785. https://doi.org/10.1029/2024GL109785

Deng, X., Hwang, C., Coleman, R., & Featherstone, W. E. (2008). Seasonal and interannual variations of the Leeuwin Current off Western Australia from TOPEX/Poseidon satellite altimetry. Terrestrial, Atmospheric and Oceanic Sciences, 19(1–2), 135–149. https://doi.org/10.3319/TAO.2008.19.1-2.135(SA)

Dosio, A., Mentaschi, L., Fischer, E. M., & Wyser, K. (2018). Extreme heat waves under 1.5 °C and 2 °C global warming. Environmental Research Letters, 13(5), 054006. https://doi.org/10.1088/1748-9326/aab827

Fan, J., Wei, S., Liu, D., Qin, T., Xu, F., Wu, C., Liu, G., & Cheng, Y. (2023). Impact of ENSO events on meteorological drought in the Weihe River basin, China. Frontiers in Earth Science, 11, 1093632. https://doi.org/10.3389/feart.2023.1093632

Fang, S.-L., Tsai, B.-Y., Wu, C.-Y., Chang, S.-C., Chang, Y.-L., & Kuo, B.-J. (2025). The Effect of Climate Change on Important Climate Variables in Taiwan and Its Potential Impact on Crop Production. Agriculture, 15(7), 766. https://doi.org/10.3390/agriculture15070766

Glass, G. V., & Hopkins, K. D. (1996). Statistical methods in education and psychology (3rd ed.). Allyn & Bacon

Guo, Y., Huang, S., Huang, Q., Wang, H., Fang, W., Yang, Y., & Wang, L. (2019). Assessing socioeconomic drought based on an improved Multivariate Standardized Reliability and Resilience Index. Journal of Hydrology, 568, 904–918.https://doi.org/10.1016/j.jhydrol.2018.11.055

Hao, Z., Hao, F., Xia, Y., Feng, S., Sun, C., Zhang, X., Fu, Y., Hao, Y., Zhang, Y., & Meng, Y. (2022). Compound droughts and hot extremes: Characteristics, drivers, changes, and impacts. Earth-Science Reviews, 235, 104241. https://doi.org/10.1016/j.earscirev.2022.104241

Ho, C. H., Lur, H. S., Yao, M. H., & et al. (2018). The impact on food security and future adaptation under climate variation: A case study of Taiwan’s agriculture and fisheries. Mitigation and Adaptation Strategies for Global Change, 23, 311–347. https://doi.org/10.1007/s11027-017-9742-3

Hsu, K.-C., Wang, C.-H., Chen, K.-C., Chen, C.-T., & Ma, K.-W. (2007). Climate-induced hydrological impacts on the groundwater system of the Pingtung Plain, Taiwan. Hydrogeology Journal, 15(5), 903–913. https://doi.org/10.1007/s10040-006-0137-x

Huang, S., Huang, Q., Chang, J., & Leng, G. (2016). Linkages between hydrological drought, climate indices and human activities: A case study in the Columbia River Basin. International Journal of Climatology, 37(5), 2446–2461. https://doi.org/10.1002/joc.4344 

Hung, C. W., & Shih, M. F. (2019). Analysis of severe droughts in taiwan and its related atmospheric and oceanic environments. Atmosphere, 10(3), Article 159. https://doi.org/10.3390/atmos10030159

Intergovernmental Panel on Climate Change (IPCC). (2023). Climate Change 2021 – The Physical Science Basis: Working Group I Contribution to the Sixth Assessment Report of the Intergovernmental Panel on Climate Change. Cambridge: Cambridge University Press. https://doi.org/10.1017/9781009157896

Ionita, M., Caldarescu, D. E., & Nagavciuc, V. (2021).Compound hot and dry events in Europe: Variability and large-scale drivers. Frontiers in Climate, 3. https://doi.org/10.3389/fclim.2021.688991

Kchouk, S., Melsen, L. A., Walker, D. W., & van Oel, P. R. (2022). A geography of drought indices: Mismatch between indicators of drought and its impacts on water and food securities. Natural Hazards and Earth System Sciences, 22(1), 323–344. https://doi.org/10.5194/nhess-22-323-2022

Kendall, M. G. (1938). A new measure of rank correlation. Biometrika, 30(1–2), 81–93. https://doi.org/10.2307/2332226

Kenyon, J., & Hegerl, G. C. (2008). Influence of modes of climate variability on global temperature extremes. Journal of Climate, 21(15), 3872–3889. https://doi.org/10.1175/2008JCLI2125.1

Lan, X., Tans, P., & Thoning, K. W. (2025). Trends in globally-averaged CO₂ determined from NOAA Global Monitoring Laboratory measurements (Version 2025-09). NOAA Global Monitoring Laboratory. https://doi.org/10.15138/9N0H-ZH07

Laurin, G. V., Cotrina-Sanchez, A., Belelli-Marchesini, L., Tomelleri, E., Battipaglia, G., Cocozza, C., Niccoli, F., Kabala, J. P., Gianelle, D., Vescovo, L., Da Ros, L., & Valentini, R. (2024). 

Comparing ground below-canopy and satellite spectral data for an improved and integrated forest phenology monitoring system. Ecological Indicators, 158, 111328. https://doi.org/10.1016/j.ecolind.2023.111328

Legates, D., Yang, D., Quiring, S., Freeman, K., & Bogart, T. (2005). Bias adjustments to Arctic precipitation: A comparison of daily versus monthly bias adjustments. 85th AMS Annual Meeting, American Meteorological Society – Combined Preprints.

Li, J., Wu, J., Yang, Y., & Tang, R. (2025). Economic consequences of compound drought-heatwave induced power deficits: Evidence from Sichuan's 2022 power crisis. Sustainable Cities and Society, 130, 106639. https://doi.org/10.1016/j.scs.2025.106639

Li, M.-Y., Hung, H.-C., Hsu, H.-H., & Wang, P.-S. (2023). Causes of the record-breaking drought in Taiwan in 2020–2021. Meteorological Bulletin, 51(1), 30–57. https://doi.org/10.53106/025400022023015101002

Li, T., & Wang, B. (2005). A review on the western North Pacific monsoon: Synoptic-to-interannual variabilities. Terrestrial, Atmospheric and Oceanic Sciences, 16, 285–314.

Liu, Q. (2016). Interlinking climate change with water-energy-food nexus and related ecosystem processes in California case studies. Ecological Processes, 5, Article 14. https://doi.org/10.1186/s13717-016-0058-0

Liu, X., Zhu, X., Pan, Y., Li, S., Liu, Y., & Ma, Z. (2018). Performance of different drought indices for agriculture drought in the North China Plain. Journal of Arid Land, 8(2), 274–286. https://doi.org/10.1007/s40333-018-0005-2

Lin, C.-Y., Chien, Y.-Y., Su, C.-J., Kuo, C.-H., & Chen, Y.-C. (2017). Climate variability of heat wave and projection of warming scenario in Taiwan. Climatic Change, 145, 305–320. https://doi.org/10.1007/s10584-017-2091-0

Lv, A., Fan, L., & Zhang, W. (2022). Impact of ENSO events on droughts in China. Atmosphere, 13(11), 1764. https://doi.org/10.3390/atmos13111764

Lyon, B., & Barnston, A. G. (2005). ENSO and the Spatial Extent of Interannual Precipitation Extremes in Tropical Land Areas. Journal of Climate, 18(23), 5095-5109. https://doi.org/10.1175/JCLI3598.1

Marengo, J. A., Costa, M. C., Cunha, A. P., Espinoza, J.-C., Jimenez, J. C., Libonati, R., Miranda, V., Trigo, I. F., Sierra, J. P., Geirinhas, J. L., Ramos, A. M., Skansi, M., Molina-Carpio, J., & 

Salinas, R. (2025).Climatological patterns of heatwaves during winter and spring 2023 and trends for the period 1979–2023 in central South America. Frontiers in Climate, 7. https://doi.org/10.3389/fclim.2025.1529082

Mazdiyasni, O., & AghaKouchak, A. (2015). Substantial increase in concurrent droughts and heatwaves in the United States. Proceedings of the National Academy of Sciences of the United States of America, 112(37), 11484–11489. https://doi.org/10.1073/pnas.1422945112

McBride, J. L. , & Nicholls, N. . (1983). Seasonal Relationships between Australian Rainfall and the Southern Oscillation. Monthly Weather Review, 111(10), 1998-2004. https://doi.org/10.1175/1520-0493(1983)111<1998:SRBARA>2.0.CO;2

McCreary, J., Zebiak, S.E. & Glantz, M.H. (2006). ENSO as an Integrating Concept in Earth Science. Science, 314, 1740-1745. https://doi.org/10.1126/science.1132588

McKee, T. B., Doesken, N. J., & Kleist, J. (1993). The relationship of drought frequency and duration to time scales. In Proceedings of the Eighth Conference on Applied Climatology (Vol. 17, pp. 179–183). American Meteorological Society.

Meehl, G. A., & Washington, W. M. (1996). El Niño-like climate change in a model with increased atmospheric CO₂ concentrations. Nature, 382(6589), 56–60. https://doi.org/10.1038/382056a0

Menzo, Z. M., Karamperidou, C., Kong, Q., & Huber, M. (2025). El Niño enhances exposure to humid heat extremes with regionally varying impacts during Eastern versus Central Pacific events. Geophysical Research Letters, 52, e2024GL112387. https://doi.org/10.1029/2024GL112387

Miara, A., Macknick, J. E., & Fekete, B. (2017). Climate and water resource change impacts and adaptation potential for US power supply. Nature Climate Change, 7(11), 793–798. https://doi.org/10.1038/nclimate3417

Ministry of Agriculture. (2025). Agricultural production statistics. https://agrstat.moa.gov.tw/sdweb/public/inquiry/InquireAdvance.aspx

Mishra, A. K., & Singh, V. P. (2010). A review of drought concepts. Journal of Hydrology, 391, 202–216.https://doi.org/10.1016/j.jhydrol.2010.07.012 

Nairn, J. R., & Fawcett, R. J. B. (2015). The Excess Heat Factor: A Metric for Heatwave Intensity and Its Use in Classifying Heatwave Severity. International Journal of Environmental Research and Public Health, 12(1), 227-253. https://doi.org/10.3390/ijerph120100227

Nelsen, R.B. (2006). An Introduction to Copulas. 2nd Edition, Springer Science Business Media, New York.

Nicholson, S. E., & Kim, J. (1997). The relationship of the El Niño–Southern Oscillation to African rainfall. International Journal of Climatology, 17(2), 117–135.https://doi.org/10.1002/(SICI)1097-0088(199702)17:2<117::AID-JOC84>3.0.CO;2-O

NOAA National Centers for Environmental Information. (2025, December). Climate at a glance: Global time series. Retrieved December 12, 2025, from https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/global/time-series

NOAA Climate Prediction Center. (2025). Cold & warm episodes by season. Retrieved July 11, 2025, from https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_v5.php

Nordling, K., Fahrenbach, N. L. S., & Samset, B. H. (2025). Climate variability can outweigh the influence of climate mean changes for extreme precipitation under global warming. Atmospheric Chemistry and Physics, 25, 1659–1684. https://doi.org/10.5194/acp-25-1659-2025

Pan, R., Nieto-Barajas, L. E., & Craiu, R. (2025). Multivariate temporal dependence via mixtures of rotated copulas. arXiv. https://doi.org/10.48550/arXiv.2403.12789

Panda, D. K., AghaKouchak, A., & Ambast, S. K. (2017). Increasing heat waves and warm spells in India, observed from a multiaspect framework. Journal of Geophysical Research: Atmospheres, 122(7), 3837–3858. https://doi.org/10.1002/2016JD026292

Pearson, K. (1895). Note on regression and inheritance in the case of two parents. Proceedings of the Royal Society of London, 58, 240–242. https://doi.org/10.1098/rspl.1895.0041

Perkins, S. E., & Alexander, L. V. (2013). On the measurement of heat waves. Journal of Climate, 26(13), 4500–4517. https://doi.org/10.1175/JCLI-D-12-00383.1

Perkins-Kirkpatrick, S. E. (2015). A review on the scientific understanding of heatwaves: Their measurement, driving mechanisms, and changes at the global scale. Atmospheric Research, 164–165, 242–267. https://doi.org/10.1016/j.atmosres.2015.05.014

Perkins-Kirkpatrick, S. E., & Gibson, P. B. (2017). Changes in regional heatwave characteristics as a function of increasing global temperature. Scientific Reports, 7(1), 12256. DOI: 10.1038/s41598-017-12520-2

Philander, S. G. H. (1983). El Niño Southern Oscillation phenomena. Nature, 302(5906), 295-301. https://doi.org/10.1038/302295a0

Purwanto, A., Sušnik, J., Suryadi, F. X., & de Fraiture, C. (2021). Water-Energy-Food Nexus: Critical Review, Practical Applications, and Prospects for Future Research. Sustainability, 13(4), 1919. https://doi.org/10.3390/su13041919

Redmond, K. T., & Koch, R. W. (1991). Surface climate and streamflow variability in the western United States and their relationship to large-scale circulation indices. Water Resources Research, 27(9), 2381–2399. https://doi.org/10.1029/91WR00690

Ribeiro, A. F. S., Russo, A., Gouveia, C. M., Páscoa, P., & Zscheischler, J. (2020). Risk of crop failure due to compound dry and hot extremes estimated with nested copulas. Biogeosciences, 17, 4815–4830. https://doi.org/10.5194/bg-17-4815-2020

Robine, J. M., Cheung, S. L., Le Roy, S., Van Oyen, H., Griffiths, C., Michel, J. P., & Herrmann, F. R. (2008). Death toll exceeded 70,000 in Europe during the summer of 2003. Comptes rendus biologies, 331(2), 171–178. https://doi.org/10.1016/j.crvi.2007.12.001

Robinson, P. J. (2001). On the Definition of a Heat Wave. Journal of Applied Meteorology, 40(4), 762-775. https://doi.org/10.1175/1520-0450(2001)040<0762:OTDOAH>2.0.CO;2

Sakizadeh, M., Milewski, A., & Sattari, M. T. (2023). Analysis of Long-Term Trend of Stream Flow and Interaction Effect of Land Use and Land Cover on Water Yield by SWAT Model and Statistical Learning in Part of Urmia Lake Basin, Northwest of Iran. Water, 15(4), 690. https://doi.org/10.3390/w15040690

Salvadori, G., De Michele, C., Kottegoda, N. T., & Rosso, R. (2007). Extremes in nature: An approach using copulas. Dordrecht, The Netherlands: Springer. https://doi.org/10.1007/1-4020-4415-1

Šebenik, U., Brilly, M., & Šraj, M. (2017). Drought analysis using the standardized precipitation index (SPI). Acta Geographica Slovenica, 57(1). https://doi.org/10.3986/AGS.729

Seneviratne, S. I., Corti, T., Davin, E. L., Hirschi, M., Jaeger, E. B., Lehner, I., Orlowsky, B., & Teuling, A. J. (2010). Investigating soil moisture–climate interactions in a changing climate: A review. Earth-Science Reviews, 99(3–4), 125–161. https://doi.org/10.1016/j.earscirev.2010.02.004

Sheffield, J., & Wood, E. F. (2008). Global trends and variability in soil moisture and drought characteristics, 1950-2000, from observation-driven simulations of the terrestrial hydrologic cycle. Journal of Climate, 21(3), 432-458. https://doi.org/10.1175/2007JCLI1822.1

Shiu, C., Liu, S. C., & Chen, J. (2009). Diurnally asymmetric trends of temperature, humidity, and precipitation in Taiwan. Journal of Climate, 22(21), 5635–5649. https://doi.org/10.1175/2009JCLI2514.1

Shovon, M. A. C., Mostafiz, R. B., Al Assi, A., & et al. (2025). Hydroclimatic extremes and aquaculture: A review of impact and response strategies. Aquaculture International, 33, 439. https://doi.org/10.1007/s10499-025-02108-3

Shu, J., Qu, J. J., Motha, R., Xu, J. C., & Dong, D. F. (2018). Impacts of climate change on hydropower development and sustainability: A review. IOP Conference Series: Earth and Environmental Science, 163(1), 012126. https://doi.org/10.1088/1755-1315/163/1/012126

Sklar, A. (1959). Fonctions de Répartition à n Dimensions et Leurs Marges. Publications de l’Institut Statistique de l’Université de Paris, 8, 229-231.

Spearman, C. (1904). The proof and measurement of association between two things. The American Journal of Psychology, 15(1), 72–101. https://doi.org/10.2307/1412159

Stagge, J.H., Tallaksen, L.M., Gudmundsson, L., Van Loon, A.F. and Stahl, K. (2015). Candidate Distributions for Climatological Drought Indices (SPI and SPEI). Int. J. Climatol., 35: 4027-4040. https://doi.org/10.1002/joc.4267

Sun, Y., Hu, T., & Zhang, X. (2018). Substantial increase in heat wave risks in China in a future warmer world. Earth's Future, 6, 1528–1538. https://doi.org/10.1029/2018EF000963

Svoboda, M., Hayes, M., & Wood, D. A. (2012). Standardized precipitation index user guide (WMO-No. 1090). World Meteorological Organization. https://library.wmo.int/idurl/4/39629

Taiwan Climate Change Projection and Information Platform Project. (2023). Climate extreme indices [Web page]. Taiwan Climate Change Projection and Information Platform (TCCIP). https://tccip.ncdr.nat.gov.tw/ds_05_03_indices.aspx

Taiwan Power Company. (2025). Household electricity consumption data. https://www.taipower.com.tw/2289/2406/2437/49978/49988/normalPost

Tavakol, A., Rahmani, V., & Harrington, J. Jr. (2020). Probability of compound climate extremes in a changing climate: A copula-based study of hot, dry, and windy events in the central United States. Environmental Research Letters, 15(10), 104058. https://doi.org/10.1088/1748-9326/abb1ef

Thom, H. (1966). Some methods of climatological analysis. WMO Technical Note Number 81, Secretariat of the World Meteorological Organization, Geneva.

Thornthwaite, C. W. (1948). An approach toward a rational classification of climate. Geographical Review, 38, 55–94. https://doi.org/10.2307/210739

Torrence, C., & Compo, G. P. (1998). A Practical Guide to Wavelet Analysis. Bulletin of the American Meteorological Society, 79(1), 61-78. https://doi.org/10.1175/1520-0477(1998)079<0061:APGTWA>2.0.CO;2

Tran, A. P., Tran, B. C., Campbell, S. B., & et al. (2024). Spatio-temporal characterization of drought variability in data-scarce regions using global precipitation data: A case study in Cauto river basin, Cuba. Scientific Reports, 14, 11659. https://doi.org/10.1038/s41598-024-61709-9

Trenberth, K. E. (1997). The Definition of El Niño. Bulletin of the American Meteorological Society, 78(12), 2771-2778. https://doi.org/10.1175/1520-0477(1997)078<2771:TDOENO>2.0.CO;2

Trenberth, K. E., & Stepaniak, D. P. (2001). Indices of El Niño Evolution. Journal of Climate, 14(8), 1697-1701. https://doi.org/10.1175/1520-0442(2001)014<1697:LIOENO>2.0.CO;2

Tsai, C., Wang, Y., Tseng, W., & Chiang, L. (2025). Pacific Meridional Mode Implicated as a Prime Driver of Decadal Summer Temperature Variation over Taiwan. Journal of Climate, 38(8), 1881-1894. https://doi.org/10.1175/JCLI-D-23-0734.1

United Nations Office for the Coordination of Humanitarian Affairs. (2022, May 30). Technical report: Taiwan drought – Interconnected disaster risks 2021/2022. ReliefWeb. https://reliefweb.int/report/china-taiwan-province/technical-report-taiwan-drought-interconnected-disaster-risks-20212022

Van Lanen, H. A. J. (2006). Drought propagation through the hydrological cycle. In Proceedings of the Fifth FRIEND World Conference (IAHS Publication No. 308, pp. 122–127).International Association of Hydrological Sciences.

Van Loon, A. F. (2015). Hydrological drought explained. Wiley Interdisciplinary Reviews: Water, 2(4), 359-392. https://doi.org/10.1002/wat2.1085

Vicente-Serrano, S. M., Beguería, S., & López-Moreno, J. I. (2010). A multiscalar drought index sensitive to global warming: The standardized precipitation evapotranspiration index. Journal of Climate, 23(7), 1696–1718. https://doi.org/10.1175/2009JCLI2909.1

Wang, C., Li, Z., Chen, Y., Ouyang, L., Li, Y., Sun, F., Liu, Y., & Zhu, J. (2023). Drought–heatwave compound events are stronger in drylands. Weather and Climate Extremes, 42, 100632. https://doi.org/10.1016/j.wace.2023.100632

Wang, C., Li, Z., Chen, Y., Ouyang, L., Zhao, H., Zhu, J., Wang, J., & Zhao, Y. (2024). Characteristic changes in compound drought and heatwave events under climate change. Atmospheric Research, 305, 107440. https://doi.org/10.1016/j.atmosres.2024.107440

Wang, Y., Zhang, X., Zhao, K., & Singh, D. (2024). Streamflow in the United States: Characteristics, trends, regime shifts, and extremes. Scientific Data, 11, 788. https://doi.org/10.1038/s41597-024-03618-0

Water Resources Agency, Ministry of Economic Affairs. (2021). Basic plan for water resources management in every region of Taiwan. Water Resources Agency, Ministry of Economic Affairs. Retrieved September 1, 2025, from https://www.wra.gov.tw/en/cp.aspx?n=39478

Water Resources Agency, Ministry of Economic Affairs. (2023). Water resources utilization. Retrieved September 1, 2025, from https://www.wra.gov.tw/en/cp.aspx?n=5154&dn=5155

Water Resources Agency, Ministry of Economic Affairs. (2025). Taiwan major reservoir storage report. https://fhy.wra.gov.tw/ReservoirPage_2011/StorageCapacity.aspx

WeatherRisk Explore Inc. (2021). Causes of the drought in the first half of 2021 [in Chinese]. WeatherRisk. https://www.weatherrisk.com/post/2021年上半年乾旱的原因

Wilhite, D. A., & Glantz, M. H. (1985). Understanding the drought phenomenon: The role of definitions. Water International, 10(3), 111–120. https://doi.org/10.1080/02508068508686328 

World Meteorological Organization. (2009). Guidelines on analysis of extremes in a changing climate in support of informed decisions for adaptation (WCDMP-No. 72, WMO/TD-No. 1500). World Meteorological Organization. https://library.wmo.int/idurl/4/48826

World Meteorological Organization. (2023). Guidelines on the definition and characterization of extreme weather and climate events (WMO-No. 1310). World Meteorological Organization. https://library.wmo.int/idurl/4/58396

Wu, H., Hayes, M., Weiss, A., & Hu, Q. (2001). An evaluation of the Standardized Precipitation Index, the China-Z Index and the statistical Z-Score. International Journal of Climatology, 21, 745–758. https://doi.org/10.1002/joc.658

Wu, T.-T., Hsu, Y.-C., & Lee, C.-P. (2015). The effects of El Niño–Southern Oscillation on temperature and precipitation in Taiwan. Journal of Ocean Engineering, 15(1), 57–65. https://doi.org/10.6266/JCOE.2015.1501.04

Wu, X., Hao, Z., Hao, F., Zhang, X., Singh, V. P., & Sun, C. (2021). Influence of large-scale circulation patterns on compound dry and hot events in China. Journal of Geophysical Research: Atmospheres, 126, e2020JD033918. https://doi.org/10.1029/2020JD033918

Wu, Y.-c., Chu, J.-L., & Yu, Y.-C. (2020). Climatology and the Interannual Variability of the High-Temperature Extremes in Taiwan. Journal of Geophysical Research: Atmospheres, 125, e2019JD030992. https://doi.org/10.1029/2019JD030992

Xu, Z., FitzGerald, G., Guo, Y., Jalaludin, B., & Tong, S. (2016). Impact of heatwave on mortality under different heatwave definitions: A systematic review and meta-analysis. Environment International, 89–90, 193–203. https://doi.org/10.1016/j.envint.2016.02.007

Yang, C., Zhang, H., Ta, Z., Huang, G., & Liu, Y. (2024). Assessment of vegetation response to compound dry-hot events in Central Asia based on the Vine-Copula conditional probability model. Ecological Indicators, 169, 112910. https://doi.org/10.1016/j.ecolind.2024.112910

Yeh, H.-F., & Hsu, H.-L. (2019). Using the Markov Chain to analyze precipitation and groundwater drought characteristics and linkage with atmospheric circulation. Sustainability, 11(6), 1817. https://doi.org/10.3390/su11061817

Yu, R., & Zhai, P. (2020). Changes in compound drought and hot extreme events in summer over populated eastern China. Weather and Climate Extremes, 30, 100295. https://doi.org/10.1016/j.wace.2020.100295

Zhang, Q., Li, J., Singh, V. P., & et al. (2012). SPI-based evaluation of drought events in Xinjiang, China. Natural Hazards, 64, 481–492. https://doi.org/10.1007/s11069-012-0251-0

Zhang, Y., Li, W., Chen, Q., Pu, X., & Xiang, L. (2020). Multi-sensor remote sensing for drought monitoring: A review. Remote Sensing, 12(18), 3014. https://doi.org/10.1016/j.rse.2021.112313

Zhao, K., Wulder, M. A., Hu, T., Bright, R., Wu, Q., Qin, H., et al. (2019). Detecting change‐point, trend, and seasonality in satellite time series data to track abrupt changes and nonlinear dynamics: A Bayesian ensemble algorithm. Remote Sensing of Environment, 232, 111181. https://doi.org/10.1016/j.rse.2019.04.034

Zhao, X., Yu, Y., Cheng, J., Ding, K., Luo, Y., Zheng, K., et al. (2024). A novel framework for heterogeneity decomposition and mechanism inference in spatiotemporal evolution of groundwater storage: Case study in the North China Plain. Water Resources Research, 60, e2023WR036102. https://doi.org/10.1029/2023WR036102

Zscheischler, J., Westra, S., Van Den Hurk, B. J. J. M., Seneviratne, S. I., Ward, P. J., Pitman, A., Aghakouchak, A., Bresch, D. N., Leonard, M., Wahl, T., & Zhang, X. (2018). Future climate risk from compound events. Nature Climate Change, 8(6), 469-477. https://doi.org/10.1038/s41558-018-0156-3
