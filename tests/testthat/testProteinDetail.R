library(prideR)
context("Protein Detail")

test.protein <- ProteinDetail(
                    protein.accession = "P12345",
                    project.accession = "PXT000001",
                    assay.accession = "123456",
                    protein.synonyms = c("S1", "S2"),
                    protein.description = "Test description",
                    protein.sequence = "Test sequence"
                )

test_that("A ProteinDetail is created properly", {
  expect_equal(protein.accession(test.protein), "P12345")
  expect_equal(project.accession(test.protein), "PXT000001")
  expect_equal(assay.accession(test.protein), "123456")
  expect_equal(protein.synonyms(test.protein)[1], "S1")
  expect_equal(protein.synonyms(test.protein)[2], "S2")
  expect_equal(protein.description(test.protein), "Test description")
  expect_equal(protein.sequence(test.protein), "Test sequence")
})

test_that("ProteinDetail's setters are working properly", {
  protein.accession(test.protein) <- "PT"
  expect_equal(protein.accession(test.protein), "PT")
  
  project.accession(test.protein) <- "PXT"
  expect_equal(project.accession(test.protein), "PXT")

  assay.accession(test.protein) <- "234567"
  expect_equal(assay.accession(test.protein), "234567")
  
  protein.synonyms(test.protein) <- c("S3", "S4")
  expect_equal(protein.synonyms(test.protein)[1], "S3")
  expect_equal(protein.synonyms(test.protein)[2], "S4")
  
  protein.sequence(test.protein) <- "ASDASD"
  expect_equal(protein.sequence(test.protein), "ASDASD")
})

test_that("Protein synonyms validation is working", {
  expect_error(protein.synonyms(test.protein)<- c("", "test"), "invalid class “ProteinDetail” object")
})

test_that("ProteinDetails as.data.frame is working", {
  test.protein.data.frame <- as.data.frame.ProteinDetail(test.protein)
  expect_equal(test.protein.data.frame$protein.accession, "P12345")
  expect_equal(test.protein.data.frame$project.accession, "PXT000001")
  expect_equal(test.protein.data.frame$assay.accession, "123456")
  expect_equal(test.protein.data.frame$protein.synonyms, "S1 || S2")
  expect_equal(test.protein.data.frame$protein.description, "Test description")
  expect_equal(test.protein.data.frame$protein.sequence, "Test sequence")
})

