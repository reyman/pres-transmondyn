
# RNetLogo Exmple
# Extracted and adapted from code in
# R Marries NetLogo: Introduction to the RNetLogo Package
# by Jan C. Thiele
# JSS June 2014, Vol 58, Issue 2
#---------------------------------------------------------------------------------------
# Launch RNetLogo and control an initial run of the
# NetLogo Fire Model
library(RNetLogo)
nlDir <- "C:/Program Files (x86)/NetLogo 5.0.5"
setwd(nlDir)

nl.path <- getwd()
NLStart(nl.path)

model.path <- file.path("models", "Sample Models", "Earth Science","Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))

NLCommand("set density 70")    # set density value
NLCommand("setup")             # call the setup routine 
NLCommand("go")                # launch the model from R

#-------------------------------------------------------------------------------------------
# Investigate percentage of forest burned as simulation proceeds and plot
library(ggplot2)
NLCommand("set density 60")
NLCommand("setup")
burned <- NLDoReportWhile("any? turtles", "go",
                c("ticks", "(burned-trees / initial-trees) * 100"),
                as.data.frame = TRUE, df.col.names = c("tick", "percent.burned"))
# Plot with ggplot2
p <- ggplot(burned,aes(x=tick,y=percent.burned))
p + geom_line() + ggtitle("Non-linear forest fire progression with density = 60")

#-------------------------------------------------------------------------------------
# Investigate percent burned as a function of density
# Function sim manages simulation
sim <- function(density) {
  NLCommand("set density ", density, "setup")
  NLDoCommandWhile("any? turtles", "go");
  ret <- NLReport("(burned-trees / initial-trees) * 100")
  return(ret)
}
# Run the simulation
d <- seq(1, 100, 1)
pb <- sapply(d, function(dens) sim(dens))

# Prepare data for ggplot and plot
df <- data.frame(d,pb)
names(df) <- c("density","percent.burned")
PBvsD <- ggplot(df,aes(x=density,y=percent.burned))
PBvsD + geom_line() + ggtitle("NetLogo Forest Fire Simulation")
PBvsD + geom_point() + ggtitle("NetLogo Forest Fire Simulation")
# Plot shows phase transition
NLQuit()           # Quit NetLogo
#-------------------------------------------------------------------------------------
# Investigate Phase Transition Region
# Run this multiple simulation "headless" i.e. without the  NetLogo GUI to save time.
library(RNetLogo)
library(ggplot2)

nlDir <- "C:/Program Files (x86)/NetLogo 5.0.5"
setwd(nlDir)

nl.path <- getwd()
NLStart(nl.path, gui = FALSE)         # start NetLogo "headless"

model.path <- file.path("models", "Sample Models", "Earth Science","Fire.nlogo")
NLLoadModel(file.path(nl.path, model.path))

# functions to control the simulation
sim <- function(density) {
  NLCommand("set density ", density, "setup")
  NLDoCommandWhile("any? turtles", "go");
  ret <- NLReport("(burned-trees / initial-trees) * 100")
  return(ret)
}


rep.sim <- function(density, rep)         # run the simulation
  lapply(density, function(dens) replicate(rep, sim(dens)))


d <- seq(55, 65, 1)                  # vector of densities to examine
res <- rep.sim(d, 20)                # Run the simulation

# Prepare data for ggplot
dd <-rep(55:65, each=20)
df <- data.frame(as.factor(dd),unlist(res))
names(df) <- c("density","percent.burned")
#
bp <- ggplot(df, aes(x=density, y=percent.burned)) + geom_boxplot()
bp + ggtitle("Forest Fire Simulation: variation near phase transition")





