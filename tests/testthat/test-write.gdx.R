# helpers ----
make_quitte_for_write <- function(param_data, var_name, dim_names) {
    param_data %>%
        setNames(c(dim_names, "value")) %>%
        dplyr::mutate(
            model    = factor("REMIND"),
            scenario = factor("SSP2"),
            unit     = factor("degC"),
            variable = factor(var_name),
            .before  = "value"
        )
}

parameter_d1 <- tibble::tibble(set_d1_UPPER = LETTERS[1:3], value = 1:3)
parameter_d2 <- tidyr::expand_grid(
    set_d1_UPPER = LETTERS[1:3],
    set_d1_lower = letters[23:26]
) %>%
    dplyr::mutate(
        value = as.integer((Vectorize(charToRaw))(set_d1_UPPER)) * 10000L
            + as.integer((Vectorize(charToRaw))(set_d1_lower))
    )

# round-trip: write then read back ----
test_that('write.gdx writes a 2d parameter and round-trips correctly', {
    x      <- make_quitte_for_write(parameter_d2, 'myVar',
                                    c('region', 'period'))
    varmap <- c('myVar' = 'parameter_d2')
    gdxFn  <- withr::local_tempfile(fileext = '.gdx')

    write.gdx(x, gdxFn, varmap, dimCols = c('region', 'period'))

    result <- gamstransfer::readGDX(gdxFn)[['parameter_d2']][['records']]
    expect_equal(nrow(result), nrow(parameter_d2))
    expect_equal(sort(result$value), sort(parameter_d2$value))
})

# varmap validation ----
test_that('write.gdx aborts on unnamed varmap', {
    x <- make_quitte_for_write(parameter_d1, 'myVar', 'region')
    expect_error(
        write.gdx(x, tempfile(), varmap = c('parameter_d1')),
        regexp = 'named character vector'
    )
})

# dimCols validation ----
test_that('write.gdx aborts on missing dimCols', {
    x <- make_quitte_for_write(parameter_d1, 'myVar', 'region')
    expect_error(
        write.gdx(x, tempfile(), varmap = c('myVar' = 'p'),
                  dimCols = c('region', 'nonexistent')),
        regexp = 'dimCols'
    )
})

# unmapped variables (verbose) ----
test_that('write.gdx warns on unmapped variables only if verbose = TRUE', {
    x <- dplyr::bind_rows(
        make_quitte_for_write(parameter_d1, 'mappedVar',   'region'),
        make_quitte_for_write(parameter_d1, 'unmappedVar', 'region')
    )
    gdxFn <- withr::local_tempfile(fileext = '.gdx')

    # no warning without verbose
    expect_no_warning(
        write.gdx(x, gdxFn, varmap = c('mappedVar' = 'p_mapped'),
                  dimCols = 'region')
    )

    # warns with verbose = TRUE
    expect_warning(
        write.gdx(x, gdxFn, varmap = c('mappedVar' = 'p_mapped'),
                  dimCols = 'region', verbose = TRUE),
        regexp = 'will be dropped'
    )

    result <- gamstransfer::readGDX(gdxFn)
    expect_true('p_mapped' %in% names(result))
    expect_false('unmappedVar' %in% names(result))
})

# missing varmap entries always warn ----
test_that("write.gdx always warns on varmap entries missing from df", {
    x <- make_quitte_for_write(parameter_d1, "myVar", "region")
    fp <- withr::local_tempfile(fileext = ".gdx")

    expect_warning(
        write.gdx(x, fp,
            varmap = c("myVar" = "p", "missingVar" = "q"),
            dimCols = "region"
        ),
        regexp = "not found in `qf`"
    )
})

# ...existing code...
# no matches at all ----
test_that("write.gdx aborts when no variables match varmap", {
    x <- make_quitte_for_write(parameter_d1, "myVar", "region")
    expect_error(
        suppressWarnings(
            write.gdx(x, tempfile(),
                varmap = c("otherVar" = "p"),
                dimCols = "region"
            )
        ),
        regexp = "No variables"
    )
})


# return value ----
test_that("write.gdx invisibly returns qf", {
    x <- make_quitte_for_write(parameter_d1, "myVar", "region")
    fp <- withr::local_tempfile(fileext = ".gdx")
    ret <- write.gdx(x, fp,
        varmap = c("myVar" = "p"),
        dimCols = "region"
    )
    expect_equal(ret, x)
})
