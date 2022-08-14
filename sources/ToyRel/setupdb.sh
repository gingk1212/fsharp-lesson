rm -rf database/master
mkdir -p database/master
cp -ipr ../data/シラバス.csv database/master
cp -ipr ../data/wikipedia/*.csv database/master
cp -ipr ../data/tandp/*.csv database/master
