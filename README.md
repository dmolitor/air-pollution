# Geography of Economic Mobility and Air Pollution

This project visualizes the geographic relationship between intergenerational
economic mobility and average measures of fine particulate matter (PM 2.5) 
across the contiguous United States. Check out the
[full report here](https://dmolitor.com/air-pollution).

## Dependencies

The replication pipeline requires NextFlow and Docker to be installed:
- [x] Install [Nextflow](https://www.nextflow.io/docs/latest/getstarted.html).
- [x] Install [Docker](https://docs.docker.com/get-docker/).

## Replicate

To replicate the visualization and all corresponding text and supplementary materials, make
`air-pollution/` your working directory and execute the following at the command line:
```
nextflow run main.nf
```
The resulting write-up will be rendered in the `docs` directory, and
[Figure 1](https://www.dmolitor.com/air-pollution/src/#fig-1) will be rendered in the `src`
directory.
