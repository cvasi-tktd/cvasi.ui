
test_that("lookup_name", {
  
  for(nm in unname(model_choices))
  {
    sc <- construct_model(nm)
    expect_true(cvasi::is_scenario(sc))
    
    val <- lookup_name(nm, lookup_table=model_lookup, from="model_f", to="model_f")
    expect_equal(val, nm)
  }
})

test_that("lookup_name invalid input", {
  
  expect_error(
    lookup_name("LemnaSchmitt", lookup_table=model_lookup, from="foo", to="model_f")
  )
  
  expect_error(
    lookup_name("LemnaSchmitt", lookup_table=model_lookup, from="scenario", to="foo")
  )
  
})
