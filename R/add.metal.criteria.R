add.metal.criteria <-
function(sample, analyte, result, hardness.name = 'hardness, total', 
         toxicity = c('acute', 'chronic'), table = c('table33a', 'table20'))
{
    toxicity <- match.arg(toxicity)
    table <- match.arg(table)
    i <- analyte == hardness.name
    vec.hardness <- result[i]
    names(vec.hardness) <- sample[i]
    CalculateMetalCriteria(vec.hardness[as.character(sample)], analyte, toxicity, table)
}

