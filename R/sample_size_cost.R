library("psych")

source("R/helpers.R")

bucket.counts <- c(20, 40, 60, 80, 100)

# sample size at which to take a cross section (across bucket counts) of similarity means and standard deviations
cross.section.sample.size <- 15 

# a data frame for storing descriptive statistics for cross section values
cross.section.df <- data.frame()

for (bucket.count in bucket.counts) {

    print(paste("Now running trial for ", bucket.count, " buckets.", sep=""))

    plots.dir <- paste("plots/", bucket.count, "/", sep="")
    if (!file.exists(plots.dir)) {
        dir.create(plots.dir)
    }

    # the theoretical distribution we're sampling from
    theoretical <- rep(1, bucket.count)
    
    # normalized version of the "theoretical" vector, for use as probabilities
    theoretical.probs <- theoretical / sum(theoretical)
    
    # names of the categories represented by the theoretical distribution
    categories <- 1:bucket.count
    
    # a place-holder vector of all 0's, used in constructing the vector of observed frequencies in the sample
    zeroes <- rep(0, bucket.count)
    
    sample.sizes <- seq(5, 25, by = 5)
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
        
        filename <- paste(plots.dir, "sample_size_", sample.size, ".jpg", sep="")
        jpeg(filename)
        hist(similarities, breaks <- seq(0, 1, 0.01), freq=FALSE, main=paste("Histogram of Similarity Scores, Sample Size = ", sample.size, sep="" ), xlab="Similarity", ylab="Probability Density of Scores", ylim=c(0,100))
        dev.off()
    
        # record the descriptive statistics on the sample similarities for this sample size
        descr.stats <- describe(similarities)
        descr.stats.df <- rbind(descr.stats.df, descr.stats)

        if (sample.size == cross.section.sample.size) {
            cross.section.df <- rbind(cross.section.df, descr.stats)
        }
    }
    
    print(paste("Plotting means and standard deviations for bucket count ", bucket.count, "...", sep=""))

    # make errorbars chart of means and standard deviations for this bucket count
    filename <- paste(plots.dir, "errorbars.jpg", sep="")
    jpeg(filename)
    # TODO: figure out why the se values are all 0, fix that if I can, and then use se values instead
    error.bars(stats=descr.stats.df, ylab = "Similarities Between Sample and Theoretical Distribution", xlab="Sample Size", sd=TRUE, ylim=c(0, 1), xaxt = 'n')
    axis(1, at = 1:length(sample.sizes), labels = err.bars.labels(sample.sizes))
    dev.off()

    
    print(paste("Done with bucket count ", bucket.count, ".", sep=""))
}

print("Making cross-section plot...")

# make "cross-section" plot
filename <- "plots/cross_section.jpg"
jpeg(filename)
main.title <- paste("Cross-section Plot At Sample Size = ", cross.section.sample.size, sep="")
error.bars(stats=cross.section.df, ylab = "Similarities Between Sample and Theoretical Distribution", xlab="Bucket Count", sd=TRUE, ylim=c(0, 1), xaxt = 'n', main = main.title)
axis(1, at = 1:length(sample.sizes), labels = err.bars.labels(nc))
dev.off()

print("Done.")
