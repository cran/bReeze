.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type bReezeNews() to see changes/bug fixes")
    packageStartupMessage("or citation(\"bReeze\") for how to cite bReeze.")
}

bReezeNews <- 
function() {
    file.show(file.path(system.file(package="bReeze"), "NEWS"))
}