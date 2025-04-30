#!/usr/bin/bash

/usr/bin/R -e 'library(devtools); devtools::document("UCBGrading"); install_local("UCBGrading", force=TRUE)'