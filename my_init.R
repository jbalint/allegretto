# My R init stuff. Load the quantstrat modules locally so I can view/edit the source code more easier within emacs
require(devtools)

# not sure what this really does (R is a pain in the ass)
Sys.setenv(R_KEEP_PKG_SOURCE="yes")

load_all("/home/jbalint/sw/quantmod")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/FinancialInstrument")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/blotter")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/quantstrat")

# my version of mysql import from my db
source("mysql_import.R")
myGetSymbol('VYM', 'ab076987-d139-4fd9-b02e-e060c3e49da7')
myGetSymbol('AAPL', 'f7e9c54e-8d7f-45bc-97d5-35984f94706b')
myGetSymbol('MSFT', 'b62c7bb0-e543-4ec7-8b35-30bff56d6a6b')
myGetSymbol('CRM', '1d49f19a-2513-47f4-b51c-5ef0aec6c378')
myGetSymbol('SCCO', 'd17c268d-4a01-4df5-a856-d4f797cfc208')
