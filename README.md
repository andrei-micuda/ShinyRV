# ShinyRV - Proiect Probabilități și Statistică

## Introducere

Proiectul de față urmărește crearea unei aplicații web (folosind pachetul Shiny) care permite lucrul cu variabile aleatoare discrete și continue. Acesta a fost realizat de Anghel Alin, Blănar George și Micudă Andrei.

## Dependențele Proiectului

Aplicația folosește multiple pachete oferite de R prin CRAN, utilitatea fiecăruia fiind detaliată mai jos:

* [`shiny`](https://cran.r-project.org/web/packages/shiny/index.html) - framework-ul de bază cu ajutorul căruia a fost realizată aplicația
* [`hash`](https://cran.r-project.org/web/packages/hash/index.html) - permite definirea unor structuri similare cu obiectele de tip JSON, utilizat pentru stocarea mai usoara a datelor
* [`Rlab`](https://cran.r-project.org/web/packages/Rlab/index.html) -
* [`stringr`](https://cran.r-project.org/web/packages/stringr/index.html) - folosit pentru parsarea input-ului dat de utilizator, precum și realizarea diferitor operații pe string-uri
* [`discreteRV`](https://cran.r-project.org/web/packages/discreteRV/index.html) - lucru cu variabile aleatoare discrete, folosit la cerinta 12

De asemenea, am utilizat diverse surse de inspirație care includ, dar nu se limitează la:

* [Wikipedia](https://en.wikipedia.org/wiki/List_of_probability_distributions) și [Statistics How To](https://www.statisticshowto.com/) pentru rezumatul distribuțiilor din Galerie
* [Documentația](https://www.rdocumentation.org/packages/shiny/versions/1.6.0) și [tutorialul introductiv](https://shiny.rstudio.com/tutorial/) oferite de Shiny
* [Documentația R](https://www.rdocumentation.org)
* [Stack Overflow](https://stackoverflow.com/) pentru diferitele probleme întâmpinate pe parcursul realizării cerințelor

## Comentarea codului

### Cerinte Abordate:
    1, 2, 3, 5, 6, 7, 8, 12



### Cerința 1.
Aplicația conține 15 distribuții predefinite care sunt ținute într-un obiect de tip `hash` de forma:

```r=
data_distributions <- hash(
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
  "Bernoulli" = {hash(...)},
  ...
)
```

Astfel, pentru fiecare distribuție avem definite o descriere sub formă de HTML, precum si un `data.frame` cu variabilele care o parametrizează. `name` reprezintă valoarea care va fi dată `label`-ului pentru input-ul respectiv, `input_id` -- id-ul prin care va fi identificat input-ul variabilei, iar celelalte valori reprezintă valoarea implicită și limitele între care se va încadra fiecare variabilă.

Pe baza cheilor din `hash`-ul anterior, se generează un `selectInput` care permite selecția unei distribuții pe baza numelui.

```r=
# Drop-down selection box for which data set
  output$choose_distribution <- renderUI({
    selectInput(inputId = "distribution", "Distribution", keys(data_distributions), selected = "Normal")
  })
```

Apoi, pe baza distribuției selectate, sunt generate `input`-uri pentru variabilele ce parametrizează acea distribuție.

```r=
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
```

Pentru ca informațiile din stânga să fie actualizate doar la apăsarea butonului **Update**, informațiile legate de distribuția aleasă și valorile parametrilor sunt salvate într-un `hash` în cadrul unei funcții `eventReactive`.

```r=
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
      else if(distribution_name == 'Log-normal') {...}
      ...
    } 
  })
```

Astfel, pentru output-urile din partea stângă, ne folosim de obiectul `distribution_plot_data` pentru generare, astfel încât acestea vor fi actualizate doar când acesta își schimbă structura.

```r=
output$desc <- renderText({distribution_plot_data()$desc})
  
output$density_mass_plot <- renderPlot({
  data <- distribution_plot_data()
  plot_color <- '#428bca'
  
  # we customize some of the plots
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
  else if(data$name == 'Poisson') { ... }
  else
  {
    plot(
      data$points, data$dens_mass_plot_values,
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

...
```

---

### Cerința 2.
Apăsând pe butonul "New Distribution"

```r=
observeEvent(input$newdist, {
showModal(newRv())
})
``` 

utilizatorului îi apare un modal unde trebuie introduși doi vectori <br/>
    
```r=
textInput("values", label="Values"),
textInput("probabilities", label="Probabilities"),
```

(outcomes și probabilities). <br/>
La apăsarea butonului Ok cele doua câmpuri sunt verificate,

```r=
  v <- try(eval(parse(text=input$values)), TRUE)
  p <- try(eval(parse(text=input$probabilities)), TRUE)

  # verfica sa fie bun inputul
  if((typeof(v) != "double" && typeof(v) != "integer") || typeof(p) != "double" && typeof(p) != "integer") {
    # in cazul in care nu este bun afiseaza pe ecran acest lucru
    showModal(newRv(failed = TRUE))
  } 
```
    
iar în cazul în care sunt valide sunt introduse în lista de V.A. 
  
```r=
matr <- validate_probability(v, p)
...
userDist[[length(userDist) + 1]] <<- matr
```

Ulterior lista este folosită pentru a afișa distribuțiile create de utilizator dând click pe butonul "Show user created distribution".

```r=
ShowGraphs <- function(rv) {
modalDialog(
  title = "Distributii adaugate de utilizator",
  renderPlot(plot(x = rv[1,], y = rv[2,]))
)
}

observeEvent(input$showdist,{
for(i in userDist) {
  showModal(ShowGraphs(i))
}
})
```

---

### Cerința 3.
Aceasta cerința se gasește in tab-ul "Working with events", ca input sunt introduse probabilitățile `P(a)` și `P(b)` (În cazul în care Not Known este selectat sunt și alte input-uri)

```r=
    output$events_calculator <- renderUI({
        if(is.null(input$relation))
              return()
        # pentru fiecare optiune din lista Idependente, Incompatibile, Not Known, generam inputuri si outputuri pentru calcularea diferitelor     probabilitati
        if(input$relation == "Independent"){
             list(numericInput(inputId ="Pa", label = "P(a)",value = 0.5, min=0,max=1,step =0.05 ),
            numericInput(inputId ="Pb", label = "P(b)",value = 0.5, min=0,max=1,step =0.05 ))
        }else if( input$relation == "Incompatible") {
           list(numericInput(inputId ="Pa", label = "P(a)",value = 0.5, min=0,max=1,step =0.05 ),
           numericInput(inputId ="Pb", label = "P(b)",value = 0.5, min=0,max=1,step =0.05 ))
        }else if(input$relation == "Not known") {
           list(numericInput(inputId ="Pa", label = "P(a)",value = 0.5, min=0,max=1,step =0.05 ),
           numericInput(inputId ="Pb", label = "P(b)",value = 0.5, min=0,max=1,step =0.05 ),
           numericInput(inputId ="PaUPb", label = "P(a U b)", value =0.5,min = 0 , max =1,step = 0.05),
           numericInput(inputId ="PacPb",label ="P(a|b)", value=0.5,min=0,max=1,step=0.05))
    }
})
```

Când utilizatorul apasă pe calculate, folosind formulele de la curs (precum Poincare și formula probabilității condiționate etc.) sunt calculate celelalte probabilități afișate mai jos.

```r=
pa <- input$Pa
pb <- input$Pb
if(input$relation == "Incompatible"){

  output$Intersectie <-renderText(0) 
  output$Reuniune <-renderText(pa+pb)
  output$Restrictie <-renderText("-") 

}else if(input$relation == "Independent"){

  output$Intersectie <-renderText(pa*pb) 
  output$Reuniune <-renderText(pa+pb- pa*pb)
  output$Restrictie <-renderText(pb) 

}else if(input$relation == "Not known"){

  reun <- input$PaUPb
  restr<- input$PacPb

  output$Intersectie <-renderText(pa+pb-reun) 
  output$Reuniune <-renderText(reun)
  output$Restrictie <-renderText((restr*pb)/pa) 

}
```
---
### Cerința 5.
Tab-ul "Show this Discrete R.V." al galeriei de distribuții este responsabil pentru afișarea unei variabile discrete a cărei repartiție e aleasă de utilizator din galerie cu posibilitatea de a alege valoare de început. Am ales aceasta abordare de rezolvare pentru a ne îndepărtă puțin de spiritul [cerinței 2](#Cerința-2.) deoarece cerința menționată cuprinde și afișarea unei variabile discrete.

Pentru a prelua input-ul (valoarea de început) am folosit `eventReactive` prin `show_drv` care așteaptă ca butonul "="(cu id-ul `svalapply`) să fie apăsat după ce a fost introdus un input. După apăsare valoarea introdusă este preluată și stocată în variabila `stârvalue`.

```r=
show_drv <- eventReactive(input$svalapply,{
    starvalue <- input$svalinp
  })
    
```
`show_drv` va fi apelat ulterior de `startv` pentru a genera plot-ul care servește drept reprezentare grafică a variabilei discrete. În primul rând aflăm prin variabila `data` care este distribuția curentă din galerie. Dacă valoare este 0 (cel puțin în cazul geometrice) datele plot-ului nu se schimbă iar dacă valoarea este diferită de 0 vom modifica corespunzător axele x și y ale plot-ului.

```r=
output$drv_hist <- renderPlot({
    data<- distribution_plot_data()
    startv <- show_drv()
    plot_color <- '#428bca'
    if (startv == 0){
      a <- data$points
      b <-data$dens_mass_plot_values
    }else
    {
      a <- startv:tail(data$points,n=1)
      b <- data$dens_mass_plot_values[-(1: (length(data$dens_mass_plot_values) -length(a)))]
    }
    plot(
      a,b
    )
  })
```
In final plot-ul este afișat in UI prin `plotOutput`

```r=
{
    plotOutput(
      outputId = "drv_hist"
    )
}
```
**Observație** : Această funcționalitate a fost lăsată valabilă pentru toate distribuțiile însă este adecvată pentru cele discrete.

---
### Cerința 6.
Tab-ul "Probability calculator" al galeriei de distribuții permite introducerea unui eveniment pe baza căruia se va calcula probabilitatea asociată acestuia în contexul distribuției selectate.

La fel ca în cadrul [cerinței 1](#Cerința-1.), vom folosi funcția `eventReactive` pentru a actualiza valorile doar la apăsarea butonului "=". Astfel, vom trimite mai departe informațiile distribuției stocate în `distribution_plot_data` și vom parsa input-ul oferit de utilizator.

```r=
probability_calculator_data <- eventReactive(input$calculate_probability, {
    distribution_data <- distribution_plot_data()
    raw_probability_input <- input$probability_calc_input
    
    hash(
      "distribution_data" = hash("name" = distribution_data$name),
      "probability_data" = parseProbabilityInput(input$probability_calc_input)
    )
  })
```

Parsarea efectivă se realizează în cadrul mai multor funcții auxiliare care utilizează funcții predefinite în pachetul `stringr`, precum `str_detect` pentru validarea input-ului folosind expresii regulate sau funcții utilitare cum ar fi `str_extract_all` sau `str_split`

```r=
# matches "u<=X" or "X<=u"
isSingleInequality <- function(str) {
  str_detect(str, "^X[<>]?=-?[0-9]+.?[0-9]*$|^X[<>]-?[0-9]+.?[0-9]*$")
}

# matches "u<=X<=v"
isDoubleInequality <- function(str) {
  str_detect(str, "^-?[0-9]+.?[0-9]*<=?X<=?-?[0-9]+.?[0-9]*$")
}

parseNonConditionalProbability <- function(str) {
  # matches "u<=X" or "X<=u"
  if(isSingleInequality(str)) {
    hash(
      "has_error" = FALSE,
      "is_conditional" = FALSE,
      "is_interval" = FALSE,
      "operator" = str_extract(str, "[<>]?=|[<>]"),
      "value" = as.numeric(str_extract(str, "-?[0-9]+.?[0-9]*"))
    )
  }
  # matching "u<=X<=v"
  else if(isDoubleInequality(str)) {
    hash(
      "has_error" = FALSE,
      "is_conditional" = FALSE,
      "is_interval" = TRUE,
      "operators" = str_extract_all(str, "<=?"),
      "values" = as.numeric(unlist(str_extract_all(str, "-?[0-9]+.?[0-9]*")))
    )
  }
  else {
    hash("has_error" = TRUE)
  }
}

parseProbabilityInput <- function(rawInput) {
  # if string doesn't contain "|" we check if it's a simple probability
  if(!str_detect(rawInput,"\\|")) {
    parseNonConditionalProbability(rawInput)
  }
  else {
    probs <- unlist(str_split(rawInput, "\\|"))
    left_prob <- parseNonConditionalProbability(probs[1])
    right_prob <- parseNonConditionalProbability(probs[2])
    
    # if we successfully parsed both parts of the conditional probability
    if(left_prob$has_error == FALSE & right_prob$has_error == FALSE) {
      hash(
        "has_error" = FALSE,
        "is_conditional" = TRUE,
        "left_prob" = left_prob,
        "right_prob" = right_prob
      )
    }
    else {
      hash("has_error" = TRUE)
    }
  }
}
```

Sunt acceptate input-uri de forma `X<3`, `X<=-2`, `X>5`, `X>=12`, `-3<X<=12`, precum și probabilități condiționate precum `X>5|X>0`. Astfel, pentru un input de forma `X<5|X>=0` în cadrul distribuției normale, obiectul `probability_calculator_data` va conține următoarele informații:

```bash=
<hash> containing 2 key-value pair(s).
  distribution_data : <hash> containing 1 key-value pair(s).
      name : Normal
  probability_data : <hash> containing 4 key-value pair(s).
      has_error : FALSE
      is_conditional : TRUE
      left_prob : <hash> containing 5 key-value pair(s).
          has_error : FALSE
          is_conditional : FALSE
          is_interval : FALSE
          operator : <
          value : 5
      right_prob : <hash> containing 5 key-value pair(s).
          has_error : FALSE
          is_conditional : FALSE
          is_interval : FALSE
          operator : >=
          value : 0
```

Ulterior, în cadrul output-ului, ne vom folosi de funcția `calculateProbability` pentru a transforma `hash`-ul anterior într-o probabilitate.

```r=
output$probability_calc_output <- renderText({
  data <- probability_calculator_data()
  calculateProbability(data, input)
})

probabilityWrapper <- function(x, distribution_data, input) {
  
  distribution_data$name <- unlist(distribution_data$name)

  if(distribution_data$name == "Normal") {
    mean <- isolate(input$var_mean)
    std_dev <- isolate(input$var_std_dev)
    
    pnorm(x, mean, std_dev)
  }
  else if(distribution_data$name == 'Log-normal') { ... }
  ...
}

calculateNonConditionalProbability <- function(data, input) {
  if(data$probability_data$is_interval == FALSE){
    # input ~ "X<=u"
    if(data$probability_data$operator %in% c("<", "<=")) {
      probabilityWrapper(data$probability_data$value, data$distribution_data, input)
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

intersectProbabilities <- function(lprob, rprob) {
  if(!lprob$is_conditional & !rprob$is_conditional) {
    if(lprob$operator %in% c("<", "<=") & rprob$operator %in% c("<", "<=")) {
      hash(
        "has_error" = FALSE,
        "is_conditional" = FALSE,
        "is_interval" = FALSE,
        "operator" = ifelse(lprob$value<rprob$value, lprob$operator, rprob$operator),
        "value" = ifelse(lprob$value<rprob$value, lprob$value, rprob$value)
      )
    }
    else if(lprob$operator %in% c(">", ">=") & rprob$operator %in% c(">", ">=")) {
      hash(
        "has_error" = FALSE,
        "is_conditional" = FALSE,
        "is_interval" = FALSE,
        "operator" = ifelse(lprob$value<rprob$value, rprob$operator, lprob$operator),
        "value" = ifelse(lprob$value<rprob$value, rprob$value, lprob$value)
      )
    }
    else if(lprob$operator %in% c("<", "<=") & rprob$operator %in% c(">", ">=")) {
      hash(
        "has_error" = FALSE,
        "is_conditional" = FALSE,
        "is_interval" = TRUE,
        "operators" = c(rprob$operator, lprob$operator),
        "values" = c(rprob$value, lprob$value)
      )
    }
    else {
      hash(
        "has_error" = FALSE,
        "is_conditional" = FALSE,
        "is_interval" = TRUE,
        "operators" = c(lprob$operator, rprob$operator),
        "values" = c(lprob$value, rprob$value)
      )
    }
  }
  else {
    hash( "has_error" = TRUE )
  }
}

calculateProbability <- function(data, input) {
  if(!data$probability_data$is_conditional)
  {
    calculateNonConditionalProbability(data, input)
  }
  else
  {
    intersectionProbability <- intersectProbabilities(data$probability_data$left_prob, data$probability_data$right_prob)
    
    intersection_data = hash(
      "distribution_data" = hash("name" = data$distribution_data$name),
      "probability_data" = intersectionProbability
    )
    
    r_data = hash(
      "distribution_data" = hash("name" = data$distribution_data$name),
      "probability_data" = data$probability_data$right_prob
    )
    
    #x1 <- calculateNonConditionalProbability(intersection_data, input)
    #x2 <- calculateNonConditionalProbability(r_data, input)
    calculateNonConditionalProbability(intersection_data, input) / calculateNonConditionalProbability(r_data, input)
  }
}
```

Pentru `hash`-ul dar ca exemplu mai devreme, funcția întoarce valoarea `0.9999994`.

**Observație**: Funcția `calculateProbability` permite calculul doar pentru variabile **continue**.

---
### Cerința 7.
Tab-ul "Discrete RV - g(X)" din meniul principal al aplicației permite introducerea valorilor a doi vectori -- pentru valorile și probabilitățile variabilei discrete -- separate prin spațiu și a unei funcții de transformare `g(x)` pentru calcularea noii variabile discrete g(X).

În cadrul unei funcții `eventReactive` vom reține cei doi vectori -- de valori și de probabilități, parsând inputul si transformându-l în numere:

```r=
rv_transform_data <- eventReactive(input$rv_transform_update, {
  x <- unlist(lapply(str_split(input$var_x_transform, " "), function(x) as.numeric(x)))
  p <- unlist(lapply(str_split(input$prob_x_transform, " "), function(x) as.numeric(x)))

  hash(
    "x" = x,
    "p" = p,
    "g" = input$g_x_transform
  )
})
```

Astfel, pentru input-ul `X=-1 0 1`, `p=0.2 0.6 0.2` și `g(x)=x^2`, obiectul `rv_transform_data` va conține următoarele informații:

```bash=
<hash> containing 3 key-value pair(s).
  g : x^2
  p : 0.2 0.6 0.2
  x : -1  0  1
```

Apoi, pentru afișarea output-ului, din moment ce avem de-a face cu v.a. discrete, probabilitățile vor rămâne neschimbate atunci când aplicăm `g(x)`, deci trebuie doar să transformăm valorile lui `x` și să avem grijă să adunăm probabilitățile valorilor care au devenit egale în urma transformării.

```r=
output$rv_transform_output <- renderText({
  data <- rv_transform_data()
  if(length(data$x) != length(data$p)) {
    "Input must have the same length"
  }
  else {
    func <- function(x) { eval(parse(text=data$g)) }
    raw_new_x <- unlist(lapply(data$x, func))
    new_x <- c()
    new_p <- c()
    for(i in seq(from = 1, to = length(raw_new_x), by = 1)) {
      val_curr <- raw_new_x[i]
      if(val_curr %in% new_x) {
        pos <- match(val_curr, new_x)
        new_p[pos] <- new_p[pos] + data$p[i]
      }
      else {
        new_x <- append(new_x, val_curr)
        new_p <- append(new_p, data$p[i])
      }
    }
    new_data <- data.frame(new_x, new_p)
    new_data <- new_data[order(new_data$new_x),]
    output_text <- paste(str_c(new_data[, 1], collapse=" "))
    output_text <- paste(output_text, "\n", str_c(new_data[, 2], collapse=" "))
    output_text
  }
})
```

Astfel, pentru input-ul dat ca exemplu mai devreme, output-ul va fi:

```bash=
0 1 
0.6 0.4
```

---

### Cerința 8.
Tab-ul "Applying Function" al galeriei de distribuții este cel responsabil pentru aplicarea funcțiilor unei variabile aleatoare continue a cărei repartiție este aleasă de utilizator din Galerie. 

Pornim de la un `textInput` care va fi funcția primită de la tastatură (text care va fi preluat de variabilă `raw_input`). Pentru verificarea corectitudinii funcției preluate de la tastatură vom lua spre utilizare o funcție inline anonimă care va parsa input-ul și apoi îl va evalua.

```r=
  apply_function <- eventReactive(input$function_apply, {
    
    distribution_data <- distribution_plot_data()
    
    raw_input <- input$function_input
    
    primary_func <- function(x){eval(parse(text=raw_input))}
    #pentru fiecare distributie continua discretitez intervalul si aplic functia primita
    if(distribution_data$name =="Normal"){
      values<- c(-1000:1000)
      probs<- dnorm(values)
    }else if (distribution_data$name == "Chi-square"){
      values<- c(0:1000)
      probs<- dchisq(values,distribution_data$statistics$Mean)
    }else if (distribution_data$name == "Exponential"){
      values<-c(0:1000)
      probs <- dexp(values,distribution_data$statistics$Mean^(-1))
    }
   
    sample<- random_variable <- matrix(data=c(values,probs), nrow=2, byrow=TRUE)
    result<-applyFunction(sample,primary_func)

  
```
Implementarea este realizată doar pentru 3 repartiții continue, mai sus menționate. Pentru aplicarea unei funcții pe o distribuție a trebuit să discretizăm acea distribuție. Această discretizare este realizată prin atribuirea unui interval mare de valori. După transformarea distribuției într-o matrice vom folosi funcția `applyFunction` (disponibilă în fișierul *discreteRV.r* )

```r=
applyFunction <- function(m, f) {
  m[1,] = f(m[1,])
  m
}

```
După aplicarea funcției asupra distribuției ne vom folosi de `getVariance` și de `getMean` (disponibile în fișierul *discreteRV.r* ) pentru a ajunge la rezultatele dorite. 

```r=
getMean <- function(m) {
  sum(m[2,] * m[1,])
}

getVariance <- function(m) {
  getMean(applyFunction(m, function(x) x**2)) - getMean(m) ** 2
}

```

În final prin `renderText` vom afișa rezultatele.

```r=
  
  output$var_output <- renderText({
    data <- apply_function()
    
    round(getVariance(data),digits = 0)
    
  })
  output$mean_output <- renderText({
    data <- apply_function()
    
    round(getMean(data),digits = 0)
    
  })
  
```


---

### Cerința 12.
Într-o manieră similară cu cerința 2, sunt introduse câmpurile outcomes şi probabilities pentru variabilele X si Y.

```r=
var_x <- try(eval(parse(text=input$var_x)), TRUE)
    prob_x <- try(eval(parse(text=input$prob_x)), TRUE)

    var_y <- try(eval(parse(text=input$var_y)), TRUE)
    prob_y <- try(eval(parse(text=input$prob_y)), TRUE)

    rv_x <- try(RV(outcomes=var_x, probs=prob_x))
    rv_y <- try(RV(outcomes=var_y, probs=prob_y))
```

La apăsarea butonului "Update" sunt calculate suma, diferența produsul şi raportul dintre acestea doua,

```r=
    generateTable <- function(x, y) {
        # tabel folosit pentru frontend-ul aplicatiei
        table <- data.frame(matrix(ncol = 3, nrow = 0))
        col_names <- c("Operation", "Value", "Probabilities")
        colnames(table) <- col_names

        # calculam diferite operatii intre cele 2 V.A.
        sum <- x + y
        dif <- x - y
        prod <- x * y
        ratio <- x / y

        # adaugam in tabel V,A. noi obtinute
        table[1,] <- list("Sumation",toString(outcomes(sum)), toString(round(probs(sum), 3)))
        table[2,] <- list("Difference",toString(outcomes(dif)), toString(round(probs(dif), 3)))
        table[3,] <- list("Product",toString(outcomes(prod)), toString(round(probs(prod), 3)))
        table[4,] <- list("Ratio",toString(outcomes(ratio)), toString(round(probs(ratio), 3)))

        table
  }
```
    
și afișate sub forma unui tabel
    
```r=
    table <- generateTable(rv_x, rv_y)
    output$rv_operations <- renderTable(table)
```

Limite: În cazul în care dăm submit fără să avem câmpurile valide, programul va da eroare


## Dificultăți în realizarea cerințelor
1. O problema întampinată de toți trei a fost cea de parsare a inputurilor complexe (exemple ar fi inputurile în care ceream vectori, funcții care trebuiau aplicate pe distribuții sau input-uri care implicau lucruri cu evenimente). În cazul input-urilor care trebuiau evaluate, precum erau vectorii funcțiile folosite au fost `exec` și `eval`, cu ajutorul cărora puteam executa cod R dintr-un string. Pentru validarea input-urilor complexe (calculul unor probabilități condiționate), am utilizat expresii regulate împreună cu funcțiile oferite de `stringr`.
2. Acomodarea cu workflow-ul impus de Shiny, întrucât crearea unei aplicații web în R este destul de diferită și restrictivă comparativ cu alte framework-uri precum React sau Angular
3. Găsirea unei soluții pentru rezolvarea cerinței 8 a fost destul de dificilă. După câteva ore de încercat variante de răspuns unul dintre noi a venit cu ideea de discretizare iar pentru aceasta a fost nevoie de implementarea unui fișier seapărat .R (*discreteRV.r*) pentru gestionarea aplicării de funcții.
4. Rezolvarea diverselor bug-uri care țin de sintaxă. R nu oferă descrieri explicite pentru problemele de rulare iar atunci când, de exemplu, o virgulă era în plus toată aplicația crăpa.