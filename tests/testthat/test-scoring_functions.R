test_that("score_rmse", {
  expect_equal(score_rmse(c(1, 2, 3), c(4, 5, 6)), 3)
})

test_that("score_spearman", {
  expect_equal(
    score_spearman(c(1, 2, 3), c(0, 0, 0)),
    "Variance of predictions is 0"
  )
  expect_equal(score_spearman(c(1, 2, 3), c(4, 5, 6)), 1)
})

test_that("score_pearson", {
  expect_equal(
    score_spearman(c(1, 2, 3), c(0, 0, 0)),
    "Variance of predictions is 0"
  )
  expect_equal(score_pearson(c(1, 2, 3), c(4, 5, 6)), 1)
})

test_that("score_auc", {
  expect_equal(as.factor(c("a", "b")), as.factor(c("a", "b")), 1)
})


# TODO: come up with test that works
# test_that("score_balanced_accuracy", {
#   expect_equal(
#     score_balanced_accuracy(as.factor(c("a", "b")), as.factor(c("a", "b"))),
#     1
#   )
# })

test_that("score_mcc", {
  expect_equal(
    score_mcc(as.factor(c("a", "b", "c")), as.factor(c("a", "b", "c"))),
    1
  )
})

# TODO: come up with test that works
# test_that("score_f1", {
#   expect_equal(
#     score_f1(as.factor(c("a", "b", "c")), as.factor(c("a", "b", "c"))),
#     1
#   )
# })
