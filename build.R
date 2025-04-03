# https://mdneuzerling.com/post/what-ive-learnt-about-making-an-r-package/
# usethis::create_package("path/to/yourpackage")
rm(list = c("nemenyi_test", "nemenyi_test_and_plot", "nemenyi_test_and_plot_as_rows", "plot_nemenyi"))
# devtools::test()
# devtools::check()
devtools::document()
devtools::install()
# devtools::build(path="build", manual = T)
# install.packages("build/NemenyiPlot_0.1.0.tar.gz")
