
# Simple Examples -----------------------------------------------------------------------------

source_keys <- letters
choice_keys <- c(letters, paste0(letters, 1), paste0(letters, 2))
ham(source = source_keys, choices = choice_keys)
ham(source = source_keys, choices = choice_keys, key = 101:178)



context_table <- data.frame(match = choice_keys, key = 101:178, data = runif(26*3))
ham(source = source_keys, choices = choice_keys, key = 101:178, context = context_table)
# Test duplicates
source_keys <- letters
choice_keys <- c(letters, letters, LETTERS)
context_table <- data.frame(match = choice_keys, key = 101:178,
                            data = runif(26*3), stringsAsFactors=FALSE)
ham(source = source_keys, choices = context_table$match, key = 101:178,
    context = context_table)

