# Motion picture editing as a Hawkes process
This is the repository for my article Motion picture editing as a Hawkes process (under review).

## Abstract
In this article I analyse editing as a point process to look at the temporal structure in the timings of cuts in motion pictures modelling the editing in 134 Hollywood films released between 1935 and 2005 as a Hawkes process with an exponential kernel. The results show that the editing in Hollywood films can be modelled as a Hawkes process and that the conditional intensity function provides a direct description of the instantaneous cutting rate of a film, revealing the structure of a film’s editing at a range of scales. The parameters of the exponential kernel show a clear trend over time to a more rapid editing with an increase in the rate of exogenous events and small increase in the rate of endogenous events. This is consistent with the shift from a classical to an intensified continuity editing style. There are, however, few differences between genres indicating the consistency of editing practices in Hollywood cinema over time and different types of films.

The repository includes a the R scripts used to fit a Hawkes model with an exponential kernel to editing data for Hollywood films and to produce the plots for publication.

The `data` folder contains the raw data used in the project.

Included in the `results` are the outputs of fitting the model, including

-  The data for plotting the counting process
-  The data for plotting the KS plot to assess goodness of fit
-  The data for plotting the conidtional intensity function
-  The plots for each film of all of the above

The results of fitting the model to each film are also stored in the `results` folder.
