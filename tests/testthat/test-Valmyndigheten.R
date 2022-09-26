result_path<-'18.14c1f613181ed0043d567ae/1663009000443/valresultat-riksdagen-preliminar-jamforande-statistik.xlsx'
result<-Valmyndigheten_api(result_path)

test_that("testing Valmyndigheten_api function can return a given value for a specific set of inputs.", {
  expect_equal(result[[1]]$Parti...1[2], "Centerpartiet")
})

test_that("with the wrong input Valmyndigheten_api does not work.",{
  result_path<-'8'
  expect_error(Valmyndigheten_api(result_path))
})