source(test_path('test_data/make_test_data.R'))

library(gamstransfer)

m <- Container$new()

invisible(m$addSet(name = 'foo',
                   records = unique(c(set_d1_lower, set_d1_UPPER))))

# sets ----
## one-dimensional sets ----
invisible(m$addSet(name = 'set_d1_UPPER',
                   domain = m['foo'],
                   records = set_d1_UPPER,
                   description = 'a one-dimensional set of upper-case letters'))

invisible(m$addSet(name = 'set_d1_lower',
                   domain = m['foo'],
                   records = set_d1_lower,
                   description = 'a one-dimensional set of lower-case letters'))

## two-dimensional set ----
invisible(m$addSet(name = 'set_d2',
                   domain = list('set_d1_UPPER', 'set_d1_lower'),
                   records = set_d2,
                   description = 'a two-dimensional set'))

## alias of two-dimensional set ----
invisible(m$addAlias(name = 'set_d2_alias', aliasWith = m['set_d2']))

# parameters ----
## scalar/parameter over no set ----
invisible(m$addParameter(name = 'parameter_d0',
                         records = parameter_d0,
                         description = 'a scalar'))

## parameter over one set ----
invisible(m$addParameter(name = 'parameter_d1',
                         domain = m['set_d1_UPPER'],
                         records = parameter_d1,
                         description = 'a parameter over one set'))

## parameter over two sets ----
invisible(m$addParameter(name = 'parameter_d2',
                         domain = c(m['set_d1_UPPER'], m['set_d1_lower']),
                         records = parameter_d2,
                         description = 'a parameter over two sets'))

# variables ----
## variable over no set ----
invisible(m$addVariable(name = 'variable_d0',
                        records = variable_d0,
                        description = 'a scalar variable'))

## variable over one set ----
invisible(m$addVariable(name = 'variable_d1',
                        domain = m['set_d1_UPPER'],
                        records = variable_d1,
                        description = 'a variable over one set'))

## variable over two sets ----
invisible(m$addVariable(name = 'variable_d2',
                        domain = c(m['set_d1_UPPER'], m['set_d1_lower']),
                        records = variable_d2,
                        description = 'a variable over two sets'))

# equations ----
## equation over no set ----
invisible(m$addEquation(name = 'equation_d0',
                        type = 'eq',
                        records = equation_d0,
                        description = 'a scalar equation'))


## equation over one-dimensional set ----
invisible(m$addEquation(name = 'equation_d1',
                        type = 'eq',
                        domain = m['set_d1_UPPER'],
                        records = equation_d1,
                        description = 'an equation over one set'))

## equation over two-dimensional set ----
invisible(m$addEquation(name = 'equation_d2',
                        type = 'eq',
                        domain = c(m['set_d1_UPPER'], m['set_d1_lower']),
                        records = equation_d2,
                        description = 'an equation over two sets'))

m$write(test_path('test_data/test.gdx'))
