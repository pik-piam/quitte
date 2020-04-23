context("removeColNa()")

# create some test data
df <- data.frame(
    character = letters[1:5],
    factor = as.factor(LETTERS[1:5]),
    value = 1:5,
    unit = NA,
    unit2 = forcats::fct_explicit_na(factor(NA)),
    stringsAsFactors = FALSE)

df_removed <- removeColNa(df)

test_that(
    'Test if all NA columns disappeared',
    {
        # identical number of columns
        expect_equal(
            object   = setdiff(colnames(df), colnames(df_removed)),
            expected = c("unit","unit2"))
        
       
    }
)
