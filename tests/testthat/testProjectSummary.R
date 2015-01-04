library(prideR)
context("Project Summary")

test.project <- new("ProjectSummary",
                    accession = "Test.Accession",
                    project.title = "Test.Title",
                    project.description = "Test.Project.Description",
                    publication.date = as.POSIXct("6-6-2006"),
                    num.assays = 10,
                    species = c("Test.Species.A", "Test.Species.B"),
                    tissues = c("Test.Tissue.A", "Test.Tissue.B"),
                    ptm.names = c("Test.PTM.A", "Test.PTM.B"),
                    instrument.names = c("Test.Instrument.A", "Test.Instrument.B"),
                    project.tags = c("Test.Tag.A", "Test.Tag.B"),
                    submission.type = "Test.Submission.Type"
)

test_that("A Project is created properly", {
    expect_equal(accession(test.project), "Test.Accession")
    expect_equal(project.title(test.project), "Test.Title")
    expect_equal(project.description(test.project), "Test.Project.Description")
    expect_equal(publication.date(test.project), as.POSIXct("6-6-2006"))
    expect_equal(num.assays(test.project), 10)
    expect_equal(species(test.project), c("Test.Species.A", "Test.Species.B"))
    expect_equal(tissues(test.project), c("Test.Tissue.A", "Test.Tissue.B"))
    expect_equal(ptm.names(test.project), c("Test.PTM.A", "Test.PTM.B"))
    expect_equal(instrument.names(test.project), c("Test.Instrument.A", "Test.Instrument.B"))
    expect_equal(project.tags(test.project), c("Test.Tag.A", "Test.Tag.B"))
    expect_equal(submission.type(test.project), "Test.Submission.Type")
})