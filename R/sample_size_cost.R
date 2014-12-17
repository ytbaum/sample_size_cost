source("R/helpers.R")

# the theoretical distribution we're sampling from
theoretical <- c(5, 10, 15, 30, 50)

# normalized version of the "theoretical" vector, for use as probabilities
theoretical.probs <- theoretical / sum(theoretical)

# names of the categories represented by the theoretical distribution
num.categories <- length(theoretical)
categories <- 1:num.categories

# a place-holder vector of all 0's, used in constructing the vector of observed frequencies in the sample
zeroes <- rep(0, num.categories)

sample.sizes <- 10 * (1:100)
num.samples <- 1000

similarities <- vector()
for (sample.size in sample.sizes) {

    print(paste("Now testing at sample size ", sample.size, ". Taking ", num.samples, " samples...", sep=""))

    for (i in 1:num.samples) {
    
        # sample from the distribution
        obs.freqs <- table(sample(categories, sample.size, TRUE, theoretical.probs))
    
        # if any categories didn't appear, make sure to record that with a "0" as the frequency instead of omitting that category altogether
        zeroes.df <- data.frame(categories, zeroes)
        obs.freqs.df <- data.frame(obs.freqs)
        obs.freqs <- merge(zeroes.df, obs.freqs.df, by.x = "categories", by.y = "Var1", all = TRUE, incomparables = 0)$Freq
        obs.freqs[which(is.na(obs.freqs))] <- 0
    
        # compute the similarity of the sample to the theoretical distribution using the Jaccard Index
        obs.normalized <- normalize(obs.freqs)
        similarity <- jaccard(obs.normalized, theoretical.probs)
        similarities <- append(similarities, similarity)
    }
    
    filename <- paste("plots/", "sample_size_", sample.size, ".jpg", sep="")
    jpeg(filename)
    hist(similarities, breaks <- seq(0, 1, 0.01), freq=FALSE, main=paste("Histogram of Similarity Scores, Sample Size = ", sample.size, sep="" ), xlab="Similarity", ylab="Probability Density of Scores")
    dev.off()
}

print("Done.")
