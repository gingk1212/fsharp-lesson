rm -rf database
mkdir -p database/master
mkdir -p database/wikipedia
mkdir -p database/library
mkdir -p database/stock
cp -ipr ../data/シラバス.csv database/master
cp -ipr ../data/wikipedia/*.csv database/wikipedia
cp -ipr ../data/tandp/book.csv \
        ../data/tandp/index.csv \
        ../data/tandp/subject.csv \
        ../data/tandp/auction.csv \
        database/library
cp -ipr ../data/tandp/goods.csv \
        ../data/tandp/delivery.csv \
        ../data/tandp/stock.csv \
        database/stock
