library('RUnit')
library(ORDEQWaterQualityCriteria)

path <- file.path(.path.package(package="ORDEQWaterQualityCriteria"), "unitTests")
test.suite <- defineTestSuite("ORDEQWaterQualityCriteria",
                              dirs = path,
                              testFileRegexp = "^test.+")
 
test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
