
........ 




####### repository file structure

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
└── data                                       # not to share in public
```
```
├── shiny_dashboard/ # will update in the next quarter
```




