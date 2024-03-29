# Motion picture editing as a Hawkes process
<br>

![](images/anato.png)

This is the repository for my article 'Motion picture editing as a Hawkes process', which is has been published in the *Journal of Data Science*.

Redfern, N. (2023) Motion Picture Editing as a Hawkes Process, Journal of Data Science 21 (1): 43-56. [https://doi.org/10.6339/22-JDS1055](https://doi.org/10.6339/22-JDS1055).

## Abstract
In this article I analyse editing as a point process to look at the temporal structure in the timings of cuts in motion pictures modelling the editing in 134 Hollywood films released between 1935 and 2005 as a Hawkes process with an exponential kernel. The results show that the editing in Hollywood films can be modelled as a Hawkes process and that the conditional intensity function provides a direct description of the instantaneous cutting rate of a film, revealing the structure of a film’s editing at a range of scales. The parameters of the exponential kernel show a clear trend over time to a more rapid editing with an increase in the rate of exogenous events and small increase in the rate of endogenous events. This is consistent with the shift from a classical to an intensified continuity editing style. There are, however, few differences between genres indicating the consistency of editing practices in Hollywood cinema over time and different types of films.

The repository includes a the R scripts used to fit a Hawkes model with an exponential kernel to editing data for Hollywood films and to produce the plots for publication.

The `data` folder contains the raw data used in the project.

Included in the `results` are the outputs of fitting the model, including

-  The data for plotting the counting process
-  The data for plotting the KS plot to assess goodness of fit
-  The data for plotting the conidtional intensity function
-  The plots for each film of all of the above

This comprises a large number of files and so they are collected together in zip files.

The results of fitting the model to each film are also stored in the `results` folder.

You can explore the genreated by this project as a shiny app at [https://tinyurl.com/2p8c86u3](https://tinyurl.com/2p8c86u3)
