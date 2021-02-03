if [ ! -d "./tzdir" ]; then
wget -P src --retr-symlinks 'ftp://ftp.iana.org/tz/tz*-latest.tar.gz'
cd src
gzip -dc tzcode-latest.tar.gz | tar -xf -
gzip -dc tzdata-latest.tar.gz | tar -xf -
make TOPDIR=$(dirname $PWD)/tzdir install
cd ..
fi

echo Generating time zone data and code...

./tz_list | ./tz_sort | ./tz_gen > timezones.txt
cat timezones.txt

