# Analysis-of-CDHEs-in-Taiwan-and-Their-Potential-Impacts-on-Environmental-Resources.
**Master Thesis , Dec 2025@ NCKU Resource Engineering**

**Bao, S.-Y. (2025). Analysis of Compound Drought and Heatwave Events in Taiwan and Their Potential Impacts on Environmental Resources. MSc Thesis, National Cheng Kung University.**

![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)

## Overview

**This repository contains all scripts, analyses, figures, and supporting materials for the research thesis “Analysis of Compound Drought and Heatwave Events (CDHEs) in Taiwan and Their Potential Impacts on Environmental Resources.”**

The analysis covers 1995–2024, focusing on:

- Identifying drought and heatwave events

- Constructing compound drought–heatwave events (CDHEs)

- Applying change-point detection via BEAST

- Modeling nonlinear dependence with Rotated Clayton Copula

- Investigating ENSO-driven teleconnections

- Evaluating impacts on the Energy–Food–Water–Health (EFW-H) sectors

## Research Motivation

Compound climate extremes—especially interactions between drought and heatwaves—have become increasingly critical under global warming. Taiwan, with its steep terrain, monsoonal climate, and ENSO sensitivity, is highly vulnerable to such events.

This project aims to:

- Quantify long-term changes (1995–2024)

- Identify compound events and their statistical characteristics

- Explain physical mechanisms through ENSO teleconnections

- Evaluate impacts on resource systems (energy/water/agriculture/health)

## Methods Overview
**1. Drought Assessment — SPI**

SPI-1 based on gamma distribution fitting

Drought threshold: SPI ≤ –1.0

<div align="center">
  
<img width="1686" height="272" alt="圖片" src="https://github.com/user-attachments/assets/03e9a753-d105-47da-be10-fe52c44ed92d" />

</div>

**2. Heatwave Identification — HWDI**

Daily Tmax > 95th percentile

Duration ≥ 3 days

Calculated HWDI as cumulative heatwave days

<div align="center">
  
<img width="759" height="171" alt="圖片" src="https://github.com/user-attachments/assets/08d57466-a962-4bed-8252-253c6e46b240" />

</div>

**3. BEAST (Bayesian Change-Point Detection)**

Used to detect:

- long-term monotonic trends

- sudden shifts in extremes

- low-frequency variability

**5. Lag Correlation (ENSO → SPI/HWDI)**

Pearson/Spearman

Lags: 0–6 months

**6. Copula Modeling (Rotated Clayton)**

Captures tail dependence between:

drought intensity (SPI)

heatwave severity (HWDI)

Outputs include:

- joint PDF

- conditional probability

- return periods

**7. EFW-H Impact Evaluation**

Indicators used:

- electricity consumption

- reservoir storage

- rice yield

- aquaculture production

- heat-related illnesses

## How to Run the Code

either north(keelung) or south(hengchun): 

coding_data -> coding_spi&hwdi&beast -> coding_corr -> lag with copula -> coding_cor_north(/south)_resource

## Key Results
**1. Heatwaves Intensifying More Rapidly Than Droughts**

HWDI shows a strong positive trend after ~2010

BEAST detects significant turning points in both stations
<div align="center">
  
![beast_fig](https://github.com/user-attachments/assets/2a7982b7-678f-4b21-80df-a884e9dc0e42)
![fig7](https://github.com/user-attachments/assets/1426ca6f-fcf4-4531-b512-0013cd4f9a8b)

</div>
**2. Compound Events Becoming More Frequent**

CDHE occurrences increase especially after 2010

South (Hengchun) sees more frequent but less intense events

North (Keelung) shows stronger tail dependence, rotated Clayton reveals upper-tail dependence

<div align="center">
  
![fig9](https://github.com/user-attachments/assets/c3a2df3c-87a4-4706-8d7e-c5bc2c81bfd7)

</div>
- Return period
<div align="center">
  
![fig10](https://github.com/user-attachments/assets/03486b7d-158c-4559-bd0b-d50e1d35b7ca)

</div>

**3. ENSO Influence**

El Niño → keelung

La Niña -> hengchun

<div align="center">
  
![fig11](https://github.com/user-attachments/assets/60a9cabd-15e7-440c-b11d-533cd0a42148)

</div>

**4. Impacts on EFW-H Systems**

Reservoir levels and Rice yieldin north are highly sensitive

Heat-related illness cases rise sharply during CDHE years in south

<div align="center">
  
![fig12](https://github.com/user-attachments/assets/a2d6587d-7e7e-4111-8420-eb2547c78871)

</div>
