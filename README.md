=========
#R-script and Data for Korean Statiscal Society Newsletter on October 2014


### Preliminary Steps for Windows user
This R-script has been thoroughly tested under Mac OS X 10.9 and Ubuntu 14.04.
Some codes in the following need to download files with HTTP Secure (not http).
The `download.file` function in R does not support HTTP Secure with default 'method'.
Therefore we should change the method option of the 'download.file' function in to `curl` or `wget`.
But `curl` and `wget` have not been installed basically on the Windows since those are command line tools.


#####You should install programs with following steps 
 0. Install Rtools (optional)
 
     http://cran.r-project.org/bin/windows/Rtools/

   On the install Rtool, You should check the "Edit the System Path" and 
     add the directory which has `R.exe` file something like 
     C:\Program Files\R\R-3.1.0\bin\i386;
     for 32-bit OS (if you use 64-bit os then the last folder may be `x64`).
   After install Rtool, please check the status of system path on the CMD.
   On the CMD, type `R` (without quotation mark) then R will be excuted
     with non-gui mode on the CMD.

 1. Downlaod  nuget Command-Line Utility at
 
   	http://nuget.org/nuget.exe

 2. On the CMD, change directory to download folder of `nuget.exe` and install `chocolatey`. 
   Type the following on the CMD.

		nuget.exe install chocolatey

 3. On the CMD (not one the powershell) type the following
 
 	@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin


 4. Install curl and wget with following commands
 
		choco install curl

		choco install Wget

  Then we can use `download.file` with HTTP Secure.
