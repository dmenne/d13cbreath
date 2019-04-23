rscript -e "rhub::check(platform='debian-gcc-devel')"
ECHO "Window will be closed after 5 seconds"
@PING -n 5 127.0.0.1>nul 
