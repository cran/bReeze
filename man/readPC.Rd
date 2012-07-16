\encoding{UTF-8}
\name{readPC}
\alias{readPC}
\title{Import power curve from file}
\description{Imports a power curve from a WAsP 'wgt' file or a WindPower program 'pow' file.}
\usage{
readPC(file)
}
\arguments{
  \item{file}{The name of, or the path to a 'wgt' or 'pow' file containing power curve data. It can also be a URL.}
}
\details{
\code{bReeze} provides several power curves of common manufacturers, that can be read from the package directory. See examples for usage.

\emph{Available 'wtg' files}
\tabular{llll}{
 \tab Nordex_N80_2.5MW \tab Nordex_N90_2.5MW_HS \tab Nordex_N90_2.5MW_LS \cr
 \tab Nordex_N100_2.5MW \tab PowerWind_56_900kW \tab PowerWind_90_2.5MW \cr
 \tab Vestas_V52_850kW \tab Vestas_V60_850kW \tab Vestas_V80_2.0MW_os \cr
 \tab Vestas_V80_2.0MW \tab Vestas_V82_1650kW \tab Vestas_V90_1.8MW \cr
 \tab Vestas_V90_2.0MW \tab Vestas_V90_3.0MW \tab Vestas_V100_1.8MW_50Hz \cr
 \tab Vestas_V100_1.8MW_60Hz \tab Vestas_V112_3.0MW \tab
}

\emph{Available 'pow' files}
\tabular{llll}{
 \tab Bonus_82.4m_2.3MW \tab Bonus_MKIV_600kW \tab Clipper_LibertyC89_2.5MW \cr
 \tab Clipper_LibertyC93_2.5MW \tab Clipper_LibertyC96_2.5MW \tab Clipper_LibertyC100_2.5MW \cr
 \tab Enercon_E33_330kW \tab Enercon_E44_900kW \tab Enercon_E48_800kW \cr
 \tab Enercon_E53_800kW \tab Enercon_E70_2.3MW \tab Enercon_E82_2.0MW \cr
 \tab Enercon_E82_2.3MW \tab Enercon_E82_3.0MW \tab Enercon_E101_3.0MW \cr
 \tab Enercon_E126_7.5MW \tab EWT_DW52_500kW \tab EWT_DW54_500kW \cr
 \tab Gamesa_G52_850kW \tab Gamesa_G58_850kW \tab Gamesa_G80_2.0MW \cr
 \tab Gamesa_G83_2.0MW \tab Gamesa_G87_2.0MW \tab Gamesa_G90_2.0MW \cr
 \tab GE_1.5sl_1.5MW \tab GE_1.5sle_1.5MW \tab GE_1.5xle_1.5MW \cr
 \tab GE_1.6MW \tab GE_2.5xl_2.5MW \tab GE_3.6sl_3.6MW \cr
 \tab Leitwind_LTW70_1.7MW \tab Leitwind_LTW70_2.0MW \tab Leitwind_LTW77_1.5MW \cr
 \tab Leitwind_LTW80_1.5MW \tab Leitwind_LTW80_1.8MW \tab Leitwind_LTW101_3.0MW \cr
 \tab Nordex_N60_1.3MW \tab Nordex_N70_1.5MW \tab Nordex_N90_2.5MW \cr
 \tab Nordex_N100_2.5MW \tab Nordex_S70_1.5MW \tab Nordex_S77_1.5MW \cr
 \tab Nordic_1000_1.0MW \tab PowerWind_56_500kW \tab Repower_5M_5.0MW \cr
 \tab Repower_MM82_2.0MW \tab Repower_MM92_2.0MW \tab Siemens_SWT-2.3MW-93m \cr
 \tab Siemens_SWT-2.3MW-101m \tab Siemens_SWT-3.6MW-107m \tab Suzlon_S64_1.25MW \cr
 \tab Suzlon_S64_950kW \tab Suzlon_S66_1.25MW \tab Suzlon_S88_2.1MW \cr
 \tab VensysEnergy_77_1.5MW \tab Vestas_V27_225kW \tab Vestas_V39_500kW \cr
 \tab Vestas_V52_850kW \tab Vestas_V80_2.0MW_os \tab Vestas_V80_2.0MW \cr
 \tab Vestas_V82_1.65MW \tab Vestas_V90_2.0MW \tab Vestas_V90_3.0MW \cr
 \tab Vestas_V112_3MW \tab Vestas_V164_7.0MW_os \tab WindEnergyLebanon_1.0MW
}
For details about power curves in general see \code{\link{createPC}}.
}
\value{Returns a data frame binding the given data}
\source{
Wind turbine generator files (*.wtg) are available from the WAsP website:

http://www.wasp.dk/Download/PowerCurves.aspx

Power curve files (*.pow) are available from the WindPower Program website:

http://www.wind-power-program.com/download.htm
}
\author{Christian Graul}
\note{
All power curves are provided without any warranty of accuracy and timeliness. Reliable data can only be received from the respective manufacturer directly. 
}
\seealso{\code{\link{createPC}}, for creation of your own power curve object}
\examples{
# import power curve from bReeze wtg file
vestas.v90 <- readPC(system.file(package="bReeze", "powercurves", 
	"Vestas_V90_2.0MW.wtg"))

# import power curve from bReeze pow file
repower.mm92 <- readPC(system.file(package="bReeze", "powercurves", 
	"Repower_MM92_2.0MW.pow"))

# import power curve from user file
\dontrun{my.pc <- readPC("~/Projects/bReeze/Sandbox/myPowerCurve.wtg")}
}
\keyword{methods}
