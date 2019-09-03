context("as.quitte units")


  
  test_that("Test if dimensionless variables are transformed correctly", {
  
#    skip_if_not(!file.exists("/p"),"blabla")
a <- magclass::new.magpie(cells_and_regions = c("AFR","CHN"),years = c(2010,2020),fill = 1,names = c("REMIND.VARIABLE ()","REMIND.VARIABLE2 (Unit2)"),sets = c("region","year","model","variable"))

    expect_length(unique(as.quitte(a)[["period"]]),length(magclass::getYears(a)))
    expect_length(unique(as.quitte(a)[["region"]]),length(magclass::getRegions(a)))
    expect_length(unique(as.quitte(a)[["variable"]]),length(magclass::getNames(a)))
    expect_length(unique(as.quitte(a)[["unit"]]),length(magclass::getNames(a)))
    

  
})
