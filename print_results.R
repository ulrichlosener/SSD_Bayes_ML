################# PRINT RESULTS #############################

# object_result: List with results from sample size determination.
# When the evaluation is for only inequalities hypotheses, the list have the following 
# elements: n1 (numeric), n2 (numeric), eta (numeric), data (matrix),
# hypotheses (string), BF.threshold (numeric), evaluation (string).
# When one of the hypotheses has only equality the list have lists nested. Each 
# nested list is the result of the SSD with a different value of fraction b. 
# The nested lists have the following: n1, n2, Proportion.BF01, Proportion.BF10,
# b.frac, data.H0, data.H1, marker.H0, and marker.H1.
# Outside the nested lists, the main list have hypotheses and BF.threshold. 


print_results <- function(object_result) {
    title <- "Final sample size"
    cat(paste("\n", title, "\n", sep = ""))
    row <- paste(rep("=", nchar(title)), collapse = "")
    cat(row, "\n")
    if (object_result[[length(object_result)]] == "Inequalities") {   # Print for informative hypotheses
        cat("Hypotheses:", "\n")
        cat("    H1:", object_result[[5]][[1]], "\n")
        cat("    H2:", object_result[[5]][[2]], "\n")
        cat("Using cluster size = ", object_result$n1, " and number of clusters = ", object_result$n2, "\n")
        cat("P (BF.12 > ", object_result[[6]], " | H1) = ", object_result$Eta, "\n")
    } else { # Print for null vs informative
        n_object <- length(object_result)
        b_number <- length(object_result) - 2
        results_matrix <- matrix(NA, nrow = b_number, ncol = 5)
        results_matrix[, 1] <- seq(b_number)
        object_result_b <- object_result[1:b_number]
        results_matrix[, 2] <- round(as.numeric(head(unlist(lapply(object_result_b, `[[`, 1)), b_number)))
        results_matrix[, 3] <- round(as.numeric(head(unlist(lapply(object_result_b, `[[`, 2)), b_number)))
        results_matrix[, 4] <- round(as.numeric(head(unlist(lapply(object_result_b, `[[`, 3)), b_number)), 3)
        results_matrix[, 5] <- round(as.numeric(head(unlist(lapply(object_result_b, `[[`, 4)), b_number)), 3)
        colnames(results_matrix) <- c("b", "n1", "n2", paste("P(BF.01 >", object_result[[n_object]], "| H0)", sep = " "), 
                                      paste("P(BF.10 >", object_result[[n_object]], "| H1)", sep = " "))
        
        cat("Hypotheses:", "\n")
        cat("    H0:", object_result[[b_number + 1]][[1]], "\n")
        cat("    H1:", object_result[[b_number + 1]][[2]], "\n")
        
        cat("***********************************************************************", "\n")
        print(format(results_matrix, justify = "centre"))
        cat("***********************************************************************", "\n")
        cat("n1: Cluster sizes", "\n")
        cat("n2: Number of clusters", "\n")
    }
}

# Test
# print_results(ssd_results_null)
