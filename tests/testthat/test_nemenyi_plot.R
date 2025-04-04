library(testthat)

all_plots_types = function(...){
  
  # Prevents file Rplots.pdf from being created
  pdf(NULL)

  horizontal_plots(...)
  horizontal_reversed_plots(...)
  vertical_plots(...)
}

horizontal_plots = function(...){
  plot_nemenyi(... , color_ties=F, debug=T)
  plot_nemenyi(... , color_ties=T, debug=T)
}

horizontal_reversed_plots = function(...){
  plot_nemenyi(... , color_ties=F, reversed_ruler=T, debug=T)
  plot_nemenyi(... , color_ties=T, reversed_ruler=T, debug=T)
}

vertical_plots = function(...){
  plot_nemenyi(... , vertical = T, color_ties=F, debug=T)
  plot_nemenyi(... , vertical = T, color_ties=T, debug=T)
  plot_nemenyi(... , vertical = T, color_ties=F, reversed_ruler=T, debug=T)
  plot_nemenyi(... , vertical = T, color_ties=T, reversed_ruler=T, debug=T)
}


# Visualization ----
test_that("plot_nemenyi long text",{
  means = c(1.01, 1, 1.2, 3, 3.2, 3.5, 3.5, 3.5, 3.5, 6, 7)
  labels = c(
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:10], collapse = ""),
    paste(letters[1:20], collapse = ""),
    paste(rep(letters, length.out = 50), collapse = "")
  )
  cd = 1
  
  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
  
})


test_that("plot_nemenyi 1 no ties",{
  means = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 8, 11, 11.5, 12, 12.5, 13, 13.5, 14, 17, 19)
  labels = as.character(seq_along(means))
  cd = 2

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi long text 2",{
  means = c(1, 1, 2, 2.1, 3, 3.1, 3.5, 3.5, 3.5, 3.5, 6, 7, 7)
  labels = c(
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:5], collapse = ""),
    paste(letters[1:10], collapse = ""),
    paste(letters[1:20], collapse = ""),
    paste(rep(letters, length.out = 50), collapse = ""),
    paste(letters[1:25], collapse = ""),
    paste(letters[1:2], collapse = "")
  )
  cd = 1

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi many together",{
  means = c(1.01, 1.02, 1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.2)
  labels = as.character(seq_along(means))
  cd = 0.1

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi equidistants",{
  means = seq(1,3,0.1)
  labels = as.character(seq_along(means))
  cd = 0.3

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi equidistants no ties",{
  means = seq(1,3,0.1)
  labels = as.character(seq_along(means))
  cd = 0.09

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi min rule shrunken",{
  means = c(3, 3.2, 3.3, 4,5)
  labels = as.character(seq_along(means))
  cd = 1

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi min and max rule shrunken",{
  means = c(3, 3, 3.1, 3.5,4,4.5,5)
  labels = as.character(seq_along(means))
  cd = 1

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


test_that("plot_nemenyi min and max rule shrunken 2",{
  means = c(2.1,2.2,3,3.7)
  labels = as.character(seq_along(means))
  cd = 0.1

  testthat::expect_no_error({
    all_plots_types(means, labels, cd)
  })
})


# Nemenyi + plot ----

test_that("plot_nemenyi save pdf",{
  means = c(1, 1.1, 1.2, 1.3, 1.4, 5, 5.1)
  labels = as.character(seq_along(means))
  cd = 0.2
  
  temp_file = tempfile(fileext = ".pdf")
  
  testthat::expect_no_error({
    plot_nemenyi(means, labels, cd, filepath=temp_file, width=6, height=3)
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)
  
})

test_that("nemenyi_test matrix",{
  set.seed(1)

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  testthat::expect_no_error({
    nemenyi_test(x, conf.level=0.95)
  })
})

test_that("nemenyi_test df",{
  set.seed(1)

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  x = as.data.frame(x)

  testthat::expect_no_error({
    nemenyi_test(x, conf.level=0.95)
  })
})


test_that("nemenyi_test_and_plot",{
  set.seed(1)

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  testthat::expect_no_error({
    nemenyi_test_and_plot(x, conf.level=0.95)
  })

})


test_that("nemenyi_test_and_plot save pdf",{

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  temp_file = tempfile(fileext = ".pdf")
  
  testthat::expect_no_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)
  
})

test_that("nemenyi_test_and_plot save png",{

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  temp_file = tempfile(fileext = ".png")
  
  testthat::expect_no_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)
  
})

test_that("nemenyi_test_and_plot save jpg",{

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  temp_file = tempfile(fileext = ".jpg")
  
  testthat::expect_no_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)
  
})

test_that("nemenyi_test_and_plot save jpeg",{

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  temp_file = tempfile(fileext = ".jpeg")
  
  testthat::expect_no_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)
  
})

test_that("nemenyi_test_and_plot save wrong extension",{

  x = matrix( rnorm(50*6,mean=0,sd=1), 50, 6)
  x[,1] = x[,1]+1
  x[,2] = x[,2]+2
  x[,3] = x[,3]+3
  x[,4] = x[,4]+4
  x[,5] = x[,5]+100
  x[,6] = x[,5]+100
  colnames(x) = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  temp_file = tempfile(fileext = ".error")
  
  testthat::expect_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })
  
  # No extension
  temp_file = tempfile()
  
  testthat::expect_error({
    nemenyi_test_and_plot(x, conf.level=0.95, filepath=temp_file, width=6, height=3)
  })
  
  unlink(temp_file)
  
})


test_that("nemenyi_test_and_plot_as_rows",{

  methods = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  df = expand.grid(
    fold = 1:10,
    method = methods
  )

  df$acc = c(
    rnorm(10,mean=0,sd=1),
    rnorm(10,mean=1,sd=1),
    rnorm(10,mean=2,sd=1),
    rnorm(10,mean=3,sd=1),
    rnorm(10,mean=100,sd=1),
    rnorm(10,mean=200,sd=1)
  )

  testthat::expect_no_error({
    nemenyi_test_and_plot_as_rows(df, "acc", "method", "fold")
  })
})

test_that("nemenyi_test_and_plot_as_rows save pdf",{

  methods = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  df = expand.grid(
    fold = 1:10,
    method = methods
  )

  df$acc = c(
    rnorm(10,mean=0,sd=1),
    rnorm(10,mean=1,sd=1),
    rnorm(10,mean=2,sd=1),
    rnorm(10,mean=3,sd=1),
    rnorm(10,mean=100,sd=1),
    rnorm(10,mean=200,sd=1)
  )

  temp_file = tempfile(fileext = ".pdf")
  
  testthat::expect_no_error({
    nemenyi_test_and_plot_as_rows(
      df, "acc", "method", "fold", filepath=temp_file, width=6, height=3
    )
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)

})

test_that("nemenyi_test_and_plot_as_rows save pdf",{

  methods = c("Method A","Method B","Method C - long name","Method D", "Method E", "Method F")

  df = expand.grid(
    fold = 1:10,
    method = methods
  )

  df$acc = c(
    rnorm(10,mean=0,sd=1),
    rnorm(10,mean=1,sd=1),
    rnorm(10,mean=2,sd=1),
    rnorm(10,mean=3,sd=1),
    rnorm(10,mean=100,sd=1),
    rnorm(10,mean=200,sd=1)
  )

  temp_file = tempfile(fileext = ".pdf")
    
  testthat::expect_no_error({
    nemenyi_test_and_plot_as_rows(
      df, "acc", "method", "fold", filepath=temp_file, width=6, height=3
    )
  })

  expect_true(
    expect_true(file.exists(temp_file))
  )
  unlink(temp_file)

})

