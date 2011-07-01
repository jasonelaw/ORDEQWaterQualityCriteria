library('RUnit')
library(ORDEQWaterQualityCriteria)

path <- file.path(.path.package(package="ORDEQWaterQualityCriteria"), 
                  "inst", "unitTests")
test.suite <- defineTestSuite("ORDEQWaterQualityCriteria",
                              dirs = path,
                              testFileRegexp = "^test.+")
 
test.result <- runTestSuite(test.suite)
sink('.Rout.save')
printTextProtocol(test.result)
