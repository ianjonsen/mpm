# mpm
(animal) Move Persistence Model

[![Project Status: Abandoned – Initial development has started, but there has not yet been a stable, usable release; the project has been abandoned and the author(s) do not intend on continuing development.](https://www.repostatus.org/badges/latest/abandoned.svg)](https://www.repostatus.org/#abandoned)

# On 27/09/2019, this package was merged into [`foieGras`](https://github.com/ianjonsen/foieGras) all future development will happen there


This was a development package that fitted a random walk with time-varying move persistence (autocorrelation) to single or multiple individual animal tracks. Move persistence (gamma_t) is a latent variable that is estimated as a simple random walk through time. The models are fitted using maximum likelihood estimation via the `TMB` ([Template Model Builder](https://github.com/kaskr/adcomp)) package and using C++. Whether the input data are for a single or multiple individuals, the model is fit to each individual track separately.


