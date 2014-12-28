library("psych")

source("R/helpers.R")

# the theoretical distribution we're sampling from
num.categories <- 20
theoretical <- rep(1, num.categories)

# normalized version of the "theoretical" vector, for use as probabilities
theoretical.probs <- theoretical / sum(theoretical)

# names of the categories represented by the theoretical distribution
categories <- 1:num.categories

# a place-holder vector of all 0's, used in constructing the vector of observed frequencies in the sample
zeroes <- rep(0, num.categories)

sample.sizes <- 10 * (1:10)
num.samples <- 1000

# data frame for recording descriptive statistics about each set of samples
descr.stats.df <- data.frame()

for (sample.size in sample.sizes) {
    similarities <- vector()

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
    hist(similarities, breaks <- seq(0, 1, 0.01), freq=FALSE, main=paste("Histogram of Similarity Scores, Sample Size = ", sample.size, sep="" ), xlab="Similarity", ylab="Probability Density of Scores", ylim=c(0,100))
    dev.off()

    # record the descriptive statistics on the sample similarities for this sample size
    descr.stats <- describe(similarities)
    descr.stats.df <- rbind(descr.stats.df, descr.stats)
}

filename <- "plots/errorbars.jpg"
jpeg(filename)
# TODO: figure out why the se values are all 0, fix that if I can, and then use se values instead
error.bars(stats=descr.stats.df, ylab = "Similarities Between Sample and Theoretical Distribution", xlab="Sample Size", sd=TRUE, ylim=c(0, 1), xaxt = 'n')
axis(1, at = 1:length(sample.sizes), labels = err.bars.labels(sample.sizes))
dev.off()

print("Done.")
