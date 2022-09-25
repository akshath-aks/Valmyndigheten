result_path<-'18.14c1f613181ed0043d567ae/1663009000443/valresultat-riksdagen-preliminar-jamforande-statistik.xlsx'
result<-Valmyndigheten_api(result_path)

test_that("testing Valmyndigheten_api function output is correct.", {
  expect_equal(result[[1]]$Parti...1[2], "Centerpartiet")
})
