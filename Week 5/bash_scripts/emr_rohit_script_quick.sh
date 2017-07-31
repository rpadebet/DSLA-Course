#!/bin/bash
set -x -e

RSTUDIO_URL="https://download2.rstudio.org/rstudio-server-rhel-1.0.153-x86_64.rpm"
SHINY_URL="https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.5.3.838-rh5-x86_64.rpm"

# check for master node
IS_MASTER=false
if grep isMaster /mnt/var/lib/info/instance.json | grep true;
then
    IS_MASTER=true
fi

# Update Packages on all nodes
sudo yum -y update
sudo yum -y install libcurl-devel openssl-devel libssh2-devel # used for devtools
echo "Required libraries installed"

if [ "$IS_MASTER" = true ];
then
    echo "is Master"

    # Install RStudio Server
    RSTUDIO_FILE=$(basename $RSTUDIO_URL)
    wget -P /tmp $RSTUDIO_URL
    sudo yum install --nogpgcheck -y /tmp/$RSTUDIO_FILE
    echo "RStudio server installed"

    # Install Shiny Server
    SHINY_FILE=$(basename $SHINY_URL)
    wget -P /tmp $SHINY_URL
    sudo yum install --nogpgcheck -y /tmp/$SHINY_FILE
    echo "Shiny server installed"

    echo "Install shiny package"
    sudo R --no-save << EOF
install.packages(c('shiny'),repos="http://cran.rstudio.com")
EOF
    echo "Shiny package installed"
#   sudo rstudio-server restart || true
fi # Master check

# Make User rstudio-user with same password on all nodes
sudo useradd -m rstudio-user
echo -e "rstudio-user\nrstudio-user"|sudo passwd rstudio-user

echo "rstudio-user created"

# Install R packages on all nodes
sudo R --no-save <<EOF
install.packages(c('RJSONIO', 'itertools','digest','Rcpp', 'functional','httr', 'plyr', 'stringr','reshape2','caTools','rJava', 'data.table','devtools', 'DBI','ggplot2','dplyr','data.table','rsparkling','tidyr','readr','ggthemes','R.methodsS3','Hmisc', 'memoise','rjson','sparklyr','tidyr','lubridate','tidyquant','tidytext'),repos="http://cran.rstudio.com")
EOF

echo "R packages installed"

echo "Bootstrap completed"
