test_that("get_parameter_info works", {
  expect_equal(
    get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="description"),
    "Minimum temperature for growth"
  )
  expect_equal(
    get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="unit"),
    "°C"
  )
  expect_equal(
    get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="group"),
    "physiological"
  )
  expect_equal(
    get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="expert.value"),
    "yes"
  )
  expect_equal(
    get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="default"),
    "-"
  )
  
  expect_error(get_parameter_info(model_ = "Lemna_S", parameter_ = "Tmin", type_ ="description"))
  expect_warning(get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "T", type_ ="description"))
  expect_warning(get_parameter_info(model_ = "Lemna_Schmitt", parameter_ = "Tmin", type_ ="descr"))
})
