library(shiny)
library(hash)
library(xtable)
library(Rlab)
library(stringr)

outputDir <- "distribution_data"

saveData <- function(data) {
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

# matches "u<=X" or "X<=u"
isSingleInequality <- function(str) {
  str_detect(str, "^X[<>]?=-?[0-9]+.?[0-9]*$|^X[<>]-?[0-9]+.?[0-9]*$")
}

# "u<=X<=v"
isDoubleInequality <- function(str) {
  str_detect(str, "^-?[0-9]+.?[0-9]*<=?X<=?-?[0-9]+.?[0-9]*$")
}

parseProbabilityInput <- function(rawInput) {
  # matches "u<=X" or "X<=u"
  if(isSingleInequality(rawInput)) {
    hash(
      "is_conditional" = FALSE,
      "is_interval" = FALSE,
      "operator" = str_extract(rawInput, "[<>]?=|[<>]"),
      "value" = as.numeric(str_extract(rawInput, "-?[0-9]+.?[0-9]*"))
    )
  }
  # matching "u<=X<=v"
  else if(isDoubleInequality(rawInput)){
    hash(
      "is_conditional" = FALSE,
      "is_interval" = TRUE,
      "operators" = str_extract_all(rawInput, "<=?"),
      "values" = as.numeric(unlist(str_extract_all(rawInput, "-?[0-9]+.?[0-9]*")))
    )
  }
  else if(str_detect(rawInput,"\\|")) {
    if(all(sapply(str_split(rawInput, "\\|"), function(s) isSimpleProbability(s)))) {
      print("OK")
    }
    else {
      print("FAIL")
    }
  }
  else {
    print("FAIL")
  }
}

probabilityWrapper <- function(x, distribution_data, input) {
  print(distribution_data$name)
  if(distribution_data$name == "Normal") {
    print('intra')
    mean <- isolate(input$var_mean)
    std_dev <- isolate(input$var_std_dev)
    
    pnorm(x, mean, std_dev)
  }
  else if(distribution_data$name == 'Log-normal') {
    mean <- isolate(input$var_mean)
    std_dev <- isolate(input$var_std_dev)
    plnorm(x, mean, std_dev)
  }
  else if(distribution_data$name == 'Uniform') {
    a <- isolate(input$var_a)
    b <- isolate(input$var_b)
    punif(x, input$var_a, input$var_b)
  }
  else if(distribution_data$name == 'Geometric') {
    p <- isolate(input$var_p)
    pgeom(x, p)
  }
  else if(distribution_data$name == 'Gamma') {
    k <- isolate(input$var_k)
    th <- isolate(input$var_theta)
    pgamma(x, shape=k, scale=th)
  }
  else if(distribution_data$name == 'Beta') {
    alpha <- isolate(input$var_alpha)
    beta <- isolate(input$var_beta)
    pbeta(x, shape1=alpha, shape2=beta)
  }
  else if(distribution_data$name == 'Cauchy') {
    x0 <- isolate(input$var_x0)
    gamma <- isolate(input$var_gamma)
    pcauchy(x, location=x0, scale=gamma)
  }
  else if(distribution_data$name == 'Chi-square') {
    n <- floor(isolate(input$var_n))
    pchisq(x, df=n)
  }
  else if(distribution_data$name == 'Bernoulli'){
    if (isolate(input$var_x) ==0) {
      p <- 1-(isolate(input$var_p))
    }
    else {
      p <- isolate(input$var_p)
    }
    
    pbern(x, prob = p)
  }
  else if(distribution_data$name == 'Binomial'){
    p<- isolate(input$var_p)
    n<- isolate(input$var_n)
    
    pbinom(x, size=n, prob=p)
  }
  else if(distribution_data$name == 'Poisson') {
    lambda <- isolate(input$var_lambda)
    
    ppois(x, lambda)
  }
  else if(distribution_data$name == 'Exponential') {
    lambda <- isolate(input$var_lambda)
    
    pexp(x, lambda)
  }
  else if(distribution_data$name == 'Hyper-Geometric'){
    n <- isolate(input$var_n)
    m <- isolate(input$var_m)
    k <- isolate(input$var_k)
    
    phyper(x, m, n, k)
  }
  else if(distribution_data$name == 'Negative-Binomial'){
    n <- isolate(input$var_n)
    mu <- isolate(input$var_mu)
    
    pnbinom(x, size=n, mu=mu)
  }
  else if(distribution_data$name == 'Students-T'){
    df <- isolate(input$var_df)
    p <- isolate(input$var_p)
    
    pt(points, df)
  }
}

calculateProbability <- function(data, input) {
  if(!data$probability_data$is_conditional)
  {
    if(!data$probability_data$is_interval){
      # input ~ "X<=u"
      if(data$probability_data$operator %in% c("<", "<=")) {
        probabilityWrapper(data$probability_data$value, data$distribution_data, input)
        #probability_func(data$probability_data$value, isolate(input$var_mean), isolate(input$var_std_dev))
      }
      # input ~ "X>=u"
      else{
        1 - probabilityWrapper(data$probability_data$value, data$distribution_data, input)
      }
    }
    # input ~ "u<=X<=v"
    else{
      probabilityWrapper(data$probability_data$values[2], data$distribution_data, input) - probabilityWrapper(data$probability_data$values[1], data$distribution_data, input)
    }
  }
}

{
  data_distributions <- hash(
    "Bernoulli" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
        <p>A <b>Bernoulli distribution</b> is a <span class="text-success">discrete probability distribution</span> for a Bernoulli trial - a random experiment that has only two outcomes (usually called a "Success" or a "Failure"). For example, the probability of getting a heads (a "success") while flipping a coin is 0.5. The probability of "failure" is 1 - P (1 minus the probability of success, which also equals 0.5 for a coin toss). It is a special case of the <span class="text-success">binomial distribution</span> for n = 1. In other words, it is a binomial distribution with a single trial (e.g. a single coin toss).
        </p>
      </div>
      ',
      "variables" = data.frame(
        "name" = c("p","x"),
        "input_id" = c("p","x"),
        "default_value" = c(0.5,1),
        "min" = c(0,0),
        "max" = c(1,1),
        "step" = c(0.05,1)
      )
    )},
    "Binomial" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
       <p>In probability theory and statistics, the <b>binomial distribution</b> with parameters n and p is the <span class="text-success">discrete probability distribution</span> of the number of successes in a sequence of n <b>independent experiments</b>, each asking a yes-no question, and each with its own Boolean-valued outcome: <span class="text-success">success (with probability p) or failure (with probability q = 1 ??? p)</span>. The <b>binomial distribution</b> is a <b>Bernoulli distribution</b>. The binomial distribution is the basis for the popular binomial test of statistical significance.

The <b>binomial distribution</b> is frequently used to model <span class="text-success">the number of successes</span> in a sample of size n drawn with replacement from a population of size N. If the sampling is carried out without replacement, the draws are not independent and so the resulting distribution is a hypergeometric distribution, not a binomial one. However, for N much larger than n, the binomial distribution remains a good approximation, and is widely used. </p>
      </div>'
      ,
      "variables" = data.frame(
        "name" = c("p","n"),
        "input_id" = c("p","n"),
        "default_value" = c(0.5,1),
        "min" = c(0,1),
        "max" = c(1,50),
        "step" = c(0.05,1)
      )
    )},
    "Geometric" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p>The geometric distribution represents the number of failures before you get a success in a series of 
      <a href="https://www.statisticshowto.com/bernoulli-distribution/#trial">Bernoulli trials</a>. 
      This <a href="https://www.statisticshowto.com/discrete-probability-distribution/">discrete probability distribution</a> 
      is represented by the 
      <a href="https://www.statisticshowto.com/probability-density-function/">probability density function</a>:</p>
      <br />
      <p>
        f(x) = (1 − p)<sup><strong>x</strong></sup> *p
      </p>
      <footer>
        <p>Origin of this description:
          <a href="https://www.statisticshowto.com/geometric-distribution/"> Statistics how to</a>
        </p>
      </footer>
    </div>',
      "variables" = data.frame(
        "name" = c("p", "a", "b"),
        "input_id" = c("p", "a", "b"),
        "default_value" = c(0.5, 0, 10),
        "min" = c(0.1, 0, 0),
        "max" = c(1, 99, 100),
        "step" = c(0.05, 1, 1)
      )
    )},
    "Gamma" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p>The gamma distribution is another widely used distribution. Its importance is largely due to its relation to exponential and normal distributions.
         The gamma function, shown by Γ(x), is an extension of the factorial function to real (and complex) numbers.
      </p>
      Gamma function:
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/8c6d5fccab5ff7d1a34aa6cb2d7e6cc495ea6c9c" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;"/>
      PDF:
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/206171e583bb2aa2e14a65530b7cd9e5596d922e" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
     
    </div>',
      "variables" = data.frame(
        "name" = c("k", "&theta;", "a", "b"),
        "input_id" = c("k", "theta", "a", "b"),
        "default_value" = c(9, 0.5, 0, 20),
        "min" = c(0.00001, 0.00001, 0, 0),
        "max" = c(Inf, Inf, Inf, Inf),
        "step" = c(1, 0.05, 1, 1)
      )
    )},
    "Beta" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p>A Beta distribution is a type of probability distribution. This distribution represents a family of probabilities and is a versatile way to represent outcomes for percentages or proportions. For example, how likely is it that Kanye West will win the next Presidential election? You might think the probability is 0.2. Your friend might think it’s 0.15. The beta distribution gives you a way to describe this.</p>
      Gamma function:
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/b32815268d9d70b0b9fbb8fd5a25be7bc640aa50" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;"/>
      PDF:
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/125fdaa41844a8703d1a8610ac00fbf3edacc8e7" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
    </div>',
      "variables" = data.frame(
        "name" = c("&alpha;", "&beta;"),
        "input_id" = c("alpha", "beta"),
        "default_value" = c(5, 1),
        "min" = c(0.00001, 0.00001),
        "max" = c(Inf, Inf),
        "step" = c(0.5, 0.5)
      )
    )},
    "Cauchy" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p> The Cauchy distribution, named after Augustin Cauchy, is a continuous probability distribution. It is also known, especially among physicists, as the Lorentz distribution </p>
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/2fa7448ba911130c1e33621f1859393d3f00af5c" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
    </div>',
      "variables" = data.frame(
        "name" = c("x0", "&gamma;", "a", "b"),
        "input_id" = c("x0", "gamma", "a", "b"),
        "default_value" = c(0, 0.5, -4, 4),
        "min" = c(-Inf, 0.00001, -Inf, -Inf),
        "max" = c(Inf, Inf, Inf, Inf),
        "step" = c(0.5, 0.5, 1, 1)
      )
    )},
    "Chi-square" = {hash(
      "desc_HTML" = '<div class="panel panel-body">
      <p> The Cauchy distribution, named after Augustin Cauchy, is a continuous probability distribution. It is also known, especially among physicists, as the Lorentz distribution </p>
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/2fa7448ba911130c1e33621f1859393d3f00af5c" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
    </div>',
      "variables" = data.frame(
        "name" = c("n", "a", "b"),
        "input_id" = c("n", "a", "b"),
        "default_value" = c(4, 0, 10),
        "min" = c(-Inf, 0, -0),
        "max" = c(Inf, Inf, Inf),
        "step" = c(1, 0.5, 0.5)
      )
    )},
    "Exponential" = {hash(
      "desc_HTML" = '
      <div class="panel panel-body">
        <p>
          The <span class="text-success">exponential distribution</span> (also called the negative exponential distribution) is a probability distribution that describes time between events in a <span class="text-success">Poisson process</span>.
        </p>
        
        <p>
          There is a strong relationship between the Poisson distribution and the Exponential distribution. For example, let’s say a Poisson distribution models the number of births in a given time period. The time in between each birth can be modeled with an exponential distribution (Young & Young, 1998).
        </p>
        
        <p>
          It is a particular case of the <span class="text-success">gamma distribution</span>. It is the continuous analogue of the geometric distribution, and it has the key property of being <span class="text-success">memoryless</span>.
        </p>
      </div>',
      "variables" = data.frame(
        "name" = c("&lambda;", "a", "b"),
        "input_id" = c("lambda", "a", "b"),
        "default_value" = c(1, 0, 5),
        "min" = c(1, 0, 0),
        "max" = c(100, 100, 100),
        "step" = c(0.1, 1, 1)
      )
    )},
    "Normal" = {hash(
      "desc_HTML" = '
    <div class="panel panel-body">
      <p>
        In probability theory, a <b>normal</b> (or <b>Gaussian</b> or <b>Gauss</b> or <b>Laplace–Gauss</b>) <b>distribution</b> is a type of <span class="text-success">continuous probability distribution</span> for a real-valued random variable. The general form of its probability density function is
      </p>
      <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/00cb9b2c9b866378626bcfa45c86a6de2f2b2e40" class="center-block" aria-hidden="true" style="vertical-align: -2.838ex; width:24.446ex; height:6.676ex;" />
      <br />
      <p>
        The parameter &mu; is the <span class="text-success">mean</span> or <span class="text-success">expectation</span> of the distribution (and also its <b>median</b> and <b>mode</b>), while the parameter &sigma; is its <span class="text-success">standard deviation</span>. The <span class="text-success">variance</span> of the distribution is &sigma;<sup>2</sup>.
      </p>
      
      <p>
        Normal distributions are important in statistics and are often used in the natural and social sciences to <b>represent real-valued random variables whose distributions are not known</b>. Their importance is partly due to the <span class="text-success">central limit theorem</span>. It states that, under some conditions, the average of many samples (observations) of a random variable with finite mean and variance is itself a random variable—whose distribution converges to a normal distribution as the number of samples increases. 
      </p>
    </div>
    
    ',
      "variables" = data.frame(
        "name" = c("&mu;", "&sigma;", "a", "b"),
        "input_id" = c("mean", "std_dev", "a", "b"),
        "default_value" = c(0, 1, -5, 5)
      ))},
    "Log-normal" = {hash(
      "desc_HTML" = '
    <div class="panel panel-body">
      <p>
        A log-normal (lognormal or Galton) distribution is a probability distribution with a normally distributed logarithm. A random variable is <span class="text-success">lognormally distributed</span> if its logarithm is normally distributed. Thus, if the random variable X is log-normally distributed, then <i>Y = ln(X)</i> has a normal distribution. Equivalently, if Y has a normal distribution, then the exponential function of <i>Y</i>, <i>X = exp(Y)</i>, has a log-normal distribution. A random variable which is log-normally distributed takes only positive real values.
      </p>
      
      <p>
        The probability density function is defined by the mean &mu; and standard deviation, &sigma;:
        <img src="https://www.statisticshowto.com/wp-content/uploads/2015/08/lognormal-distribution-pdf3.png" alt="lognormal distribution pdf3" class="alignnone size-full wp-image-19146 lazyloaded" sizes="(max-width: 423px) 100vw, 423px" srcset="https://www.statisticshowto.com/wp-content/uploads/2015/08/lognormal-distribution-pdf3.png 423w, https://www.statisticshowto.com/wp-content/uploads/2015/08/lognormal-distribution-pdf3-300x35.png 300w" data-ll-status="loaded" width="423" height="49">
      </p>
      
    </div>
    
    ',
      "variables" = data.frame(
        "name" = c("&mu;", "&sigma;", "a", "b"),
        "input_id" = c("mean", "std_dev", "a", "b"),
        "default_value" = c(0, 1, 0, 2.5)
      ))},
    "Uniform" = {hash(
      "desc_HTML" = '
      <div class="panel panel-body">
        <p>
          A <span class="text-success">uniform</span> distribution, also called a <span class="text-success">rectangular distribution</span>, is a probability distribution that has <span class="text-success">constant probability</span>.
        </p>
        
        <p>
          This distribution is defined by two parameters, a and b:
        </p>
        
        <ul>
          <li>a is the minimum.</li>
          <li>b is the maximum.</li>
        </ul>
  
        <p>
          The distribution is written as <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/7db59725558a835be6944d93345f46a987a46b1e" class="mwe-math-fallback-image-inline" aria-hidden="true" style="vertical-align: -0.838ex; margin-left: -0.038ex; width:6.706ex; height:2.843ex;">.
        </p>
      </div>
      ',
      "variables" = data.frame(
        "name" = c("a", "b"),
        "input_id" = c("a", "b"),
        "default_value" = c(0, 1)
      ))},
    "Poisson" = {hash(
      "desc_HTML" = '
      <div class="panel panel-body">
        <p>
          A <span class="text-success">Poisson distribution</span> is a tool that helps to predict the probability of certain events from happening when you know how often the event has occurred. It gives us the <span class="text-success">probability of a given number of events happening in a fixed interval of time</span>.
        </p>
        <img src="https://wikimedia.org/api/rest_v1/media/math/render/svg/c22cb4461e100a6db5f815de1f44b1747f160048" class="mwe-math-fallback-image-inline" aria-hidden="true" style="vertical-align: -2.005ex; margin-left: -0.387ex; width:30.958ex; height:5.843ex;">
        <p>
          where
          <ul>
            <li><i>e</i> is <span class="text-success">Euler\'s number</span> (2.71828..)</li>
            <li><i>k</i> is the number of occurences</li>
          </ul>
        </p>
      </div>',
      "variables" = data.frame(
        "name" = c("&lambda;", "a", "b"),
        "input_id" = c("lambda", "a", "b"),
        "default_value" = c(1, 0, 20),
        "min" = c(1, 0, 0),
        "max" = c(100, 100, 100),
        "step" = c(1, 1, 1)
      )
    )},
    "Hyper-Geometric" = {hash(
      "desc_HTML" = '
      <div class="panel panel-body">
        <p>
          In probability theory and statistics, the <b>hypergeometric distribution</b> is a <span class="text-success">discrete probability distribution</span> that describes the probability of <b>k</b> successes (random draws for which the object drawn has a specified feature) in <b>n</b> draws, without replacement, from a finite population of size <b>N</b> that contains exactly <b>K</b> objects with that feature, wherein each draw is <span class="text-success">either a success or a failure.</span>
        </p>
      </div>'
      ,
      "variables" = data.frame(
        "name" = c("p","n","m","k","x"),
        "input_id" = c("p","n","m","k","x"),
        "default_value" = c(0.5,5,5,5,5),
        "min" = c(0,0,0,0,0),
        "max" = c(1,30,30,60,30),
        "step" =c (0.05,1,1,1,1)
      )
    )},
    "Negative-Binomial"={hash(
      "desc_HTML" = '
      <div class="panel panel-body">
      In probability theory and statistics, the <b>negative binomial distribution</b> is a <span class="text-success">discrete probability distribution</span> that models the <span class="text-success">number of successes</span> in a sequence of <b>independent and identically</b> distributed <b>Bernoulli trials</b> before a specified (non-random) number of failures (denoted mu) occurs. For example, we can define rolling a 6 on a die as a failure, and rolling any other number as a success, and ask how many successful rolls will occur before we see the third failure (mu = 3). In such a case, the probability distribution of the number of non-6s that appear will be a negative binomial distribution. We could similarly use the negative binomial distribution to model the number of days a certain machine works before it breaks down (mu = 1).
      </div>'
      ,
      "variables" = data.frame(
        "name" = c("n","mu"),
        "input_id" = c("n","mu"),
        "default_value" = c(10,5),
        "min" = c(1,1),
        "max" = c(50,50),
        "step" = c(1,1)
      )
    )},
    "Students-T"={hash(
      "desc_HTML" = '
      <div class="panel panel-body">
      In probability and statistics, <b>Students t-distribution</b> (or simply the t-distribution) is any member of a <span class="text-success">family of continuous probability distributions</span> that arise when estimating the <b>mean</b> of a normally-distributed population in situations where the sample size is <b>small</b> and the population s standard deviation is unknown. It was developed by English statistician <span class="text-success">William Sealy Gosset</span> under the pseudonym <b>Student</b>.
      </div>'
      ,
      "variables" = data.frame(
        "name" = c("df","p","a","b"),
        "input_id" = c("df","p","a","b"), 
        "default_value" = c(5,0.05,-5,5),
        "min" = c(1,0,-100,-100),
        "max" = c(100,1,100,100),
        "step" = c(1,0.05,1,1)
      )
    )}
  )
}

function(input, output) {
  distribution_plot_data <- eventReactive(c(input$update_plot, input$n_points), {
    distribution_name = paste(input$distribution) # Concatenate vectors after converting to character.
    
    # if the data didn't load yet (update button hasn't been clicked)
    # fill in the data for N(0,1)
    if(length(distribution_name) == 0){
      distribution_name <- 'Normal'
      points <- seq(-5, 5, length.out = 20)
      
      hash(
        "name" = distribution_name,
        "desc" = data_distributions[[distribution_name]]$desc_HTML,
        "dens_mass_plot_values" = dnorm(points, 0, 1),
        "distribution_plot_values" = pnorm(points, 0, 1),
        "points" = points,
        "statistics" = hash(
          "Mean" = 0,
          "Median" = 0,
          "Variance" = 1,
          "Standard deviation" = 1
        ),
        "probability_func" = pnorm)
    }
    else {
      if(distribution_name == 'Normal') {
        points <- seq(input$var_a, input$var_b, length.out = input$n_points)
        std_dev <- input$var_std_dev
        mean <- input$var_mean
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dnorm(points, mean, std_dev),
          "distribution_plot_values" = pnorm(points, mean, std_dev),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = mean,
            "Variance" = std_dev,
            "Standard deviation" = sqrt(std_dev)
          ),
          "probability_func" = pnorm)
      }
      else if(distribution_name == 'Log-normal') {
        points <- seq(input$var_a, input$var_b, length.out = input$n_points)
        std_dev <- input$var_std_dev
        mean <- input$var_mean
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dlnorm(points, mean, std_dev),
          "distribution_plot_values" = plnorm(points, mean, std_dev),
          "points" = points,
          "statistics" = hash(
            "Mean" = exp(mean + std_dev^2 / 2),
            "Median" = exp(mean),
            "Variance" = floor(exp(std_dev^2) - 1) * exp(2 * mean + std_dev^2),
            "Standard deviation" = sqrt(floor(exp(std_dev^2) - 1) * exp(2 * mean + std_dev^2))
          ),
          "probability_func" = plnorm)
      }
      else if(distribution_name == 'Uniform') {
        a <- input$var_a
        b <- input$var_b
        points <- seq(a, b, length.out = input$n_points)
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dunif(points, a, b),
          "distribution_plot_values" = punif(points, a, b),
          "interval_points" = hash(
            "begin" = a,
            "end" = b
          ),
          "points" = points,
          "statistics" = hash(
            "Mean" = (a+b) / 2,
            "Median" = (a+b) / 2,
            "Variance" = (b - a)^2 / 12,
            "Standard deviation" = sqrt((b - a)^2 / 12)
          ),
          "probability_func" = punif)
      }
      else if(distribution_name == 'Geometric') {
        p <- input$var_p
        if(p != 0) {
          points <- seq(input$var_a, input$var_b)
          
          var <- (1 - p)/ p ** 2
          mean <- 1 / p
          median <- -1 / log2(1 - p)
          
          hash(
            "name" = distribution_name,
            "desc" = data_distributions[[distribution_name]]$desc_HTML,
            "dens_mass_plot_values" = dgeom(points, prob = p),
            "distribution_plot_values" = pgeom(points, prob = p),
            "points" = points,
            "statistics" = hash(
              "Mean" = mean,
              "Median" = median,
              "Variance" = var,
              "Standard deviation" = sqrt(var)),
            "probability_func" = pgeom
            )
        }
        else {
          # idk yet
        }
      }
      else if(distribution_name == 'Gamma') {
        a <- input$var_a
        b <- input$var_b
        k <- input$var_k
        th <- input$var_theta
        
        points <- seq(a, b, length.out = input$n_points)
        
        mean <- k * th
        var <-  k * (th ** 2)
        
        hash (
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dgamma(points, shape=k, scale=th),
          "distribution_plot_values" = pgamma(points, shape=k, scale=th),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Variance" = var,
            "Standard deviation" = sqrt(var)),
          "probability_func" = pgamma
        )
      }
      else if(distribution_name == 'Beta') {
        alpha <- input$var_alpha
        beta <- input$var_beta
        
        points <- seq(0, 1, length.out = input$n_points)
        
        mean <- alpha / (alpha + beta)
        median <- (alpha - 1/3) / (alpha + beta - 2/3)
        var <-  alpha * beta / ((alpha + beta) ** 2 * (alpha + beta + 1))
        
        hash (
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dbeta(points, shape1=alpha, shape2=beta),
          "distribution_plot_values" = pbeta(points, shape1=alpha, shape2=beta),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)),
          "probability_func" = pbeta
        )
      }
      else if(distribution_name == 'Cauchy') {
        x0 <- input$var_x0
        gamma <- input$var_gamma
        a <- input$var_a
        b <- input$var_b
        
        points <- seq(a, b, length.out = input$n_points)
        
        median <- x0
        
        hash (
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dcauchy(points, location=x0, scale=gamma),
          "distribution_plot_values" = pcauchy(points, location=x0, scale=gamma),
          "points" = points,
          "statistics" = hash("Median" = median),
          "probability_func" = pcauchy
        )
      }
      else if(distribution_name == 'Chi-square') {
        n <- floor(input$var_n)
        a <- input$var_a
        b <- input$var_b
        
        points <- seq(a, b, length.out = input$n_points)
        
        mean <- n
        median <- n *(1-2/(9*n))**3
        var <- 2*n
        
        hash (
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dchisq(points, df=n),
          "distribution_plot_values" = pchisq(points, df=n),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)),
          "probability_func" = pchisq
        )
      }
      else if(distribution_name == 'Bernoulli'){
        if ( input$var_x ==0){
          p <- 1-(input$var_p)
        }
        else{
          p <- input$var_p
        }
          
        points <- seq(0,1)
        
        var <- p*(1-p)
        mean <- p
        if ( p < 1/2 ){
          median <- 0
        }
        else if ( p > 1/2){
          median <- 1
        }
        else{
          median <- "[0,1]"
        }
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dbern(points,prob = p),
          "distribution_plot_values" = pbern(points,prob = p),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ),
          "probability_func" = pbern)
          
      }
      else if(distribution_name == 'Binomial'){
        p<- (input$var_p)
        n<- (input$var_n)
        points<- seq(0,n)
        
        var <- n * p* (1-p)
        mean <- n* p
        median <- n *p
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dbinom( points,size=n,prob=p),
          "distribution_plot_values" = pbinom( points,size=n,prob=p),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ),
          "probability_func" = pbinom)
      }
      else if(distribution_name == 'Poisson') {
        points <- seq(input$var_a, input$var_b, by=1)
        lambda <- input$var_lambda
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dpois(points, lambda),
          "distribution_plot_values" = ppois(points, lambda),
          "points" = points,
          "statistics" = hash(
            "Mean" = lambda,
            "Median" = floor(lambda + 1/3 - 0.02 / lambda),
            "Variance" = lambda,
            "Standard deviation" = sqrt(lambda)
          ),
          "probability_func" = ppois)
      }
      else if(distribution_name == 'Exponential') {
        points <- seq(input$var_a, input$var_b, length.out = input$n_points)
        lambda <- input$var_lambda
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dexp(points, lambda),
          "distribution_plot_values" = pexp(points, lambda),
          "points" = points,
          "statistics" = hash(
            "Mean" = 1 / lambda,
            "Median" = log(2) / lambda,
            "Variance" = 1 / lambda^2,
            "Standard deviation" = sqrt(1 / lambda^2)
          ),
          "probability_func" = pexp)
      }
      else if(distribution_name == 'Hyper-Geometric'){
        p<- (input$var_p)
        n<- (input$var_n)
        m<- (input$var_m)
        k<- (input$var_k)
        x<- (input$var_x)
        points <- seq(0,x)
        
        var <- k*p*(1-p)*(m+n-k)/(m+n-1)
        mean = k*p
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dhyper( points,m,n,k),
          "distribution_plot_values" = phyper(points,m,n,k),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = "-",
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ),
          "probability_func" = phyper)
      }
      else if(distribution_name == 'Negative-Binomial'){
        n<-(input$var_n)
        mu<-(input$var_mu)
        points <- seq(0,n)
        
        var <- (mu+mu^2)/n
        mean <- mu
        median <- mu
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dnbinom( points,size=n,mu=mu),
          "distribution_plot_values" = pnbinom( points,size=n,mu=mu),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = var,
            "Standard deviation" = sqrt(var)
          ),
          "probability_func" = pnbinom)
      }
      else if(distribution_name == 'Students-T'){
        points <- seq(input$var_a, input$var_b, length.out = input$n_points)
        df <- input$var_df
        p <- input$var_p
        if ( df <= 1)
        {
          mean <-0
        }
        else
        {
          mean <- 'undefined'
        }
        median <- 0
        if ( df > 2)
        {
          variance <- df/(df-2)
        }else if (df<=2 && df>1){
          variance <- Inf
        }else{
          variance <- 'undefined'
        }
        
        
        hash(
          "name" = distribution_name,
          "desc" = data_distributions[[distribution_name]]$desc_HTML,
          "dens_mass_plot_values" = dt(points, df),
          "distribution_plot_values" = pt(points, df),
          "points" = points,
          "statistics" = hash(
            "Mean" = mean,
            "Median" = median,
            "Variance" = variance,
            "Standard deviation" = 'undefined'
          ),
          "probability_func" = pt)
      }
    } 
  })
  
  # Drop-down selection box for which data set
  output$choose_distribution <- renderUI({
    selectInput(inputId = "distribution", "Distribution", keys(data_distributions), selected = "Normal")
  })
  
  # Inputs for the variables of the selected distribution
  output$choose_values <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$distribution))
      return()
    
    # Get the data set with the appropriate name
    selected_distribution <- data_distributions[[paste(input$distribution)]]
    
    variable_inputs <- apply(selected_distribution$variables, 1, function(row){
      name <- row[1]
      input_id <- row[2]
      default_value <- row[3]
      
      min <- NA
      max <- NA
      step <- NA
      if(length(row) > 3) {
        min <- row[4]
        max <- row[5]
        step <- row[6]
      }
      
      numericInput(inputId = paste("var_", input_id, sep = ""), label = HTML(name), value = as.numeric(default_value), min = min, max = max, step = step)
    })
    
    # Create the checkboxes and select them all by default
  })
  
  output$desc <- renderText({distribution_plot_data()$desc})
  
  output$density_mass_plot <- renderPlot({
    data <- distribution_plot_data()
    plot_color <- '#428bca'
    if(data$name == 'Uniform')
    {
      a <- data$interval_points$begin
      b <- data$interval_points$end
      # we center the interval and make it take 70% of the whole x axis
      offset_x <-3 * (b - a) / 14
      plot(
        data$points, data$dens_mass_plot_values,
        xlim = c(a - offset_x, b + offset_x),
        ylim = c(0, 10 / (8 * (b - a))),
        type = 'l',
        col=plot_color,
        lwd = 2)
      
      abline(
        h = 1 / (b-a),
        col = '#555555',
        lty = 'dashed'
      )
      
      text(
        x = a - offset_x,
        y = 1 / (b-a),
        expression(frac(1, b-a)),
        pos = 3,
        cex = 0.8,
        col = '#555555'
      )
      
      segments(
        x0 = c(a - offset_x - 100, b),
        y0 = 0,
        x1 = c(a, b + offset_x + 100),
        y1 = 0,
        col = plot_color,
        lwd = 2)
      
      segments(
        x0 = c(a, b),
        y0 = 0,
        x1 = c(a, b),
        y1 = 1 / (b-a),
        col = plot_color,
        lty = 'dashed'
      )
      
      points(
        x = c(a, b),
        y = c(1 / (b-a), 1 / (b-a)),
        col = plot_color,
        pch = 19
      )
      
      rect(
        xleft = a,
        ybottom = 0,
        xright = b,
        ytop = 1 / (b-a),
        border = 'transparent',
        density = 5,
        angle = 30,
        col = '#555555'
      )
    }
    else if(data$name == 'Poisson')
    {
      plot(
        data$points, data$dens_mass_plot_values,
        type = 'o',
        pch = 19,
        col=plot_color
        )
    }
    else if(data$name == 'Exponential')
    {
      plot(
        data$points, data$dens_mass_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else if(data$name == 'Log-normal')
    {
      plot(
        data$points, data$dens_mass_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else if(data$name %in% c('Gamma', 'Beta', 'Cauchy', 'Chi-square'))
    {
      plot(
        data$points, data$dens_mass_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else
    {
      plot(
        data$points, data$dens_mass_plot_values,
        col=plot_color
        )
    }
  })
  
  output$distribution_plot <- renderPlot({
    data <- distribution_plot_data()
    plot_color <- '#428bca'
    if(data$name == 'Uniform')
    {
      plot_color <- '#428bca'
      a <- data$interval_points$begin
      b <- data$interval_points$end
      # we center the interval and make it take 70% of the whole x axis
      offset_x <-3 * (b - a) / 14
      plot(
        data$points, data$distribution_plot_values,
        xlim = c(a - offset_x, b + offset_x),
        type = 'l',
        col=plot_color,
        lwd = 2)
      
      segments(
        x0 = c(a - offset_x - 100, b),
        y0 = c(0, 1),
        x1 = c(a, b + offset_x + 100),
        y1 = c(0, 1),
        col = plot_color,
        lwd = 2)
      
      segments(
        x0 = b,
        y0 = -100,
        x1 = b,
        y1 = 1,
        col = plot_color,
        lty = 'dashed'
      )
      
      text(
        x = b,
        y = 0,
        "b",
        pos = 2,
        cex = 0.8,
        col = plot_color
      )
    }
    else if(data$name == 'Poisson')
    {
      plot(
        data$points, data$distribution_plot_values,
        pch = 19,
        col = plot_color
      )
      
      segments(
        x0 = data$points,
        y0 = data$distribution_plot_values,
        x1 = data$points + 1,
        y1 = data$distribution_plot_values,
        col = plot_color
      )
    }
    else if(data$name == 'Exponential')
    {
      plot(
        data$points, data$distribution_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else if(data$name == 'Log-normal')
    {
      plot(
        data$points, data$distribution_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else if(data$name %in% c('Gamma', 'Beta', 'Cauchy', 'Chi-square'))
    {
      plot(
        data$points, data$distribution_plot_values,
        type = 'l',
        lwd = 2,
        col=plot_color
      )
    }
    else
    {
      plot(
        data$points, data$distribution_plot_values,
        col=plot_color
        )
    }
  })
  
  output$statistics_table <- renderTable({
    data <- distribution_plot_data()
    
    # we have to convert our 'statistics' hash to a data frame so that it can be rendered to the UI
    table <- data.frame(matrix(ncol = 2, nrow = 0))
    col_names <- c("Stat", "Value")
    colnames(table) <- col_names
    
    for(key in keys(data$statistics))
    {
      table[nrow(table) + 1,] <- list(key, data$statistics[[key]])
    }
    
    table
  })
  #exercitiul 5
  output$rv_hist <- renderPlot({})
  
  probability_calculator_data <- eventReactive(input$calculate_probability, {
    distribution_data <- distribution_plot_data()
    raw_probability_input <- input$probability_calc_input
    
    hash(
      "distribution_data" = distribution_data,
      "probability_data" = parseProbabilityInput(input$probability_calc_input)
    )
  })
  
  #exercitiul 8
  apply_function <- eventReactive(input$function_apply, {
    
    distribution_data <- distribution_plot_data()
    raw_input <- input$function_input
    
    primary_func <- function(x){eval(parse(text=raw_input))}
    
    values<- c(-1000:1000)
    probs<- pnorm(values)
    # facut o matrice din acesetea doua si apoi aplicat getmean si getvariance din discreterv a lui george
    
  })
  
  output$var_output <- renderText({
    data <- apply_function()
    var(data)
  })

  
  output$probability_calc_output <- renderText({
    data <- probability_calculator_data()
    calculateProbability(data, input)
  })
}