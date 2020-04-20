R CMD build RIlias
R CMD check RIlias_0.3.tar.gz
R CMD INSTALL RIlias_0.3.tar.gz -l ./lib
cd ./lib
zip -r ../RIlias_0.3.zip . -i RIlias/*
