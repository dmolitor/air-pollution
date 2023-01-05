# air-pollution
 NSF air pollution project.

## Data

- [x] [EPA Interactive Air Pollution monitors map](https://epa.maps.arcgis.com/apps/webappviewer/index.html?id=5f239fd3e72f424f98ef3d5def547eb5&extent=-146.2334,13.1913,-46.3896,56.5319).
- [x] [Monthly, surface-level PM2.5 levels](https://wustl.app.box.com/v/ACAG-V5GL03-GWRPM25).

## Data Exploration
To explore monthly PM2.5 monitor locations and surface-level PM2.5 levels, run the following Shiny app:
```bash
Rscript -e "shiny::runApp('./src')"
```
