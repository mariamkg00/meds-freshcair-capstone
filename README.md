## freshcair-meds-capstone

## Assessing the Impacts of Supply-side Oil Policies in California

Group members: Haejin Kim (haejin_kim@ucsb.edu), Maxwell Patterson (maxwellpatterson@ucsb.edu), and Mariam Garcia (mkgarcia@ucsb.edu)

Clients: The 2035 Initiative (lucasboyd@iee.ucsb.edu), and emLab ( tmangin@ucsb.edu)

Corresponding authors: Ranjit Deshmukh (rdeshmukh@ucsb.edu); Paige Weber, (paigeweber@unc.edu); Kyle Meng (kmeng@bren.ucsb.edu)

GitHub repository author and manager: Tracey Mangin (tmangin@ucsb.edu) Zenodo repository manager: Tracey Mangin (tmangin@ucsb.edu)


## Intro
California's ambitious goal to slash GHG emissions by 90% by 2045 marks a significant shift towards sustainability. Supply-side policies, such as Senate Bill 1137, which bans new oil and gas wells within 3,200 feet of sensitive areas, signal a commitment to environmental and public health protection. To gauge SB 1137's impact accurately, the existing model must be adapted to incorporate this setback distance. This capstone project aims to bridge this gap by updating the model and creating accessible educational materials for Californians. Objectives include updating the model, predicting well locations and oil extraction using machine learning, and developing a public online app with R Shiny. The MEDS capstone group will investigate the effects of the 3,200-foot setback distance on emissions, employment, and health, contributing to the evidence supporting SB 1137.

The purpose of this Github repository is to maintain a clear and effective history of working progress in the capstone project. This repository contains parts of the data and scripts used to update the well setback distance reflected by the upcoming Senate Bill 1137. 

## Purpose

........ 




#### repository file structure :dog:

This is the fundamental structure of our repository:
We have two primary structures: "model" and "data". The existing model comprises three different sections (extraction, health, labor), which will be simplified to sections, and we will select the necessary ones to remain in this repository in the future. The Shinydashboard will be updated in the Spring quarter.
We create processed and final data. At the end of the project, we will select only the necessary files and structure the processed and final data accordingly.
​​
```
├── model/
│   ├── energy/
│   │   ├── data-processing-prep/     
│   │   └── extraction/
│   │       ├── extraction/
│   │       └── cost/
│   │       └── map/ 
│   ├── health/
│   │   ├── health/
│   │   └── map/ 
│   ├── labor/
│   ├── mechanism/                             # will be removed 
│   ├── pred-model/                            # will be removed 
│   └── scripts/STAT_tool box                  # will be removed 
├── .gitignore
├── README.md
└── data                                       # not share in public
```



