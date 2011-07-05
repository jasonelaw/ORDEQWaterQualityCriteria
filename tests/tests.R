library('RUnit')
library(ORDEQWaterQualityCriteria)

path <- file.path(.path.package(package="ORDEQWaterQualityCriteria"), 
                  "unitTests")
test.suite <- defineTestSuite("ORDEQWaterQualityCriteria",
                              dirs = path,
                              testFileRegexp = "^test.+")
 
test.result <- runTestSuite(test.suite)
setwd('S:/Jlaw/ORDEQWaterQualityCriteria')
sink('.Rout.save')
printTextProtocol(test.result)
