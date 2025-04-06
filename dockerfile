# Use the official Rocker image with the tidyverse preinstalled (based on R 4.4.1)
FROM rocker/tidyverse:4.4.1

# Copy all contents from the local /src directory to the root of the container
COPY /src /

# Install system-level dependencies required for geospatial R packages
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libjq-dev \
    && rm -rf /var/lib/apt/lists/*

# Run setup.R to install R packages  
RUN Rscript setup.R

# Set the command to run main R script
ENTRYPOINT ["Rscript","main.R"]