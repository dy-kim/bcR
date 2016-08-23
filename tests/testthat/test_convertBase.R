context('Converting number bases')

# 2 -> X
test_that("Convert base: 2 -> 2", {
  expect_equal(object = convertBase(11, 2, 2),
               expected =  c('11' = '11'))
})

test_that("Convert base: 2 -> 10", {
  expect_equal(object = convertBase(11, 2, 10),
               expected =  c('11' = 3))
})

test_that("Convert base: 2 -> 16", {
  expect_equal(object = convertBase(11, 2, 16),
               expected =  c('11' = '3'))
})

# 10 -> X
test_that("Convert base: 10 -> 2", {
  expect_equal(object = convertBase(11, 10, 2),
               expected =  c('11' = '1011'))
})

test_that("Convert base: 10 -> 10", {
  expect_equal(object = convertBase(11, 10, 10),
               expected =  c('11' = 11))
})

test_that("Convert base: 10 -> 16", {
  expect_equal(object = convertBase(11, 10, 16),
               expected =  c('11' = 'B'))
})

# 16 -> X
test_that("Convert base: 16 -> 2", {
  expect_equal(object = convertBase(11, 16, 2),
               expected =  c('11' = '10001'))
})
test_that("Convert base: 16 -> 2", {
  expect_equal(object = convertBase('A', 16, 2),
               expected =  c('A' = '1010'))
})

test_that("Convert base: 16 -> 10", {
  expect_equal(object = convertBase(11, 16, 10),
               expected =  c('11' = 17))
})
test_that("Convert base: 16 -> 10", {
  expect_equal(object = convertBase('A', 16, 10),
               expected =  c('A' = 10))
})

test_that("Convert base: 16 -> 16", {
  expect_equal(object = convertBase(11, 16, 16),
               expected =  c('11' = '11'))
})
test_that("Convert base: 16 -> 16", {
  expect_equal(object = convertBase('A', 16, 16),
               expected =  c('A' = 'A'))
})

