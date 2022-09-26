result_path<-'18.14c1f613181ed0043d567ae/1663009000443/valresultat-riksdagen-preliminar-jamforande-statistik.xlsx'
               
result<-Valmyndigheten_api(result_path)

test_that("testing Valmyndigheten_api function can return a given value for a specific set of inputs.", {
  expect_equal(result[[1]]$Parti...1[2], "Centerpartiet")
})

# test_that("with more than one input Valmyndigheten_api does not work.",{
#     length(result_path)==2
#     expect_error(Valmyndigheten_api(result_path))
# 
# })

test_that("with the wrong input Valmyndigheten_api does not work.",{
  result_path<-'8'
  expect_error(Valmyndigheten_api(result_path))
})

result_path_assembly<-'18.14c1f613181ed0043d56f51/1663745020932/preliminar-riksdagsval-jamforande-statistik-2018-2022-med-uppsamlingsdistrikt-ny.xlsx'
result_a<-Valmyndigheten_api(result_path_assembly)

test_that("testing Valmyndigheten_api function can return a given value for a specific set of inputs.", {
  expect_equal(result_a[[1]]$Parti...1[2], "Centerpartiet")
})

test_that("with the wrong input Valmyndigheten_api does not work.",{
  result_path_assembly<-''
  expect_error(Valmyndigheten_api(result_path_assembly))
})


get_p_test<-get_p()

test_that("testing get_p returns 9 rows", {
  expect_equal(nrow(get_p_test), 9)
})


test_that("testing get_p returns data.frame", {
  expect_equal(class(get_p_test), "data.frame")
})

test_that("testing get_p fails when list is returned", {
  class(get_p_test)<-c('list','vector')
  expect_error(get_p_test())
})


get_p_a_test<-get_p_a()

test_that("testing get_p_a returns 9 rows", {
  expect_equal(nrow(get_p_a_test), 9)
})

test_that("testing get_p_a returns data.frame", {
  expect_equal(class(get_p_a_test), "data.frame")
})

test_that("testing get_p_a fails when vector is returned", {
  class(get_p_a_test)<-c('vector','list')
  expect_error(get_p_a_test())
})


get_combined_test<-get_combined_data()

test_that("testing get_comined_data returns data.frame", {
  expect_equal(class(get_combined_test), "data.frame")
})

test_that("testing get_comined_data fails when matrix is returned", {
  class(get_combined_test)<-c('vector','list')
  expect_error(get_combined_test())
})

