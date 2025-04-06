# Farm-Dashboard
Quick Dashboard with R Shiny for internal team members analyzing GeoJSON farm-data
(Automated Preprocessing and Reporting)
---

## 🐳 Docker Instructions

The app runs in a container built from the official Rocker image with preinstalled tidyverse packages and system dependencies for geospatial processing.

### 🔧 Dockerfile Information for future deployment

- Base image: `rocker/tidyverse:4.4.1`
- Installs GDAL, PROJ, GEOS, and other spatial libraries
- Copies app files into the container
- Executes the app using `main.R`

### 🏗️ Build the Docker Image

From the project root directory, run:

build the container:
```bash
docker build -t geojson-viewer .
```

run the container:
```bash
docker run --rm -it -p 3838:80 geojson-viewer
```
