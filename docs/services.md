# Alternatives to running the NorESM-LSP on your local machine

Containerisation of software like the NorESM-LSP and models (NorESM, CLM, FATES) makes it possible to publish it on other services, e.g. **cloud computing services** where users can sign up and run model experiments remotely. That way, users don't have to install anything on their own computer. For long or multiple experiments, using remote computing resources may be necessary. An early version of the NorESM-LSP exists on Galaxy, a free and open cloud computing service. We also have plans for publishing the NorESM-LSP as a tool (app) on NIRD. 

It is also possible to run the NorESM-LSP on a remote computer, like a research-infrastructure computer cluster (e.g. NREC, Saga, Fram for University of Oslo users), via an **[ssh tunnel](https://en.wikipedia.org/wiki/Tunneling_protocol#Secure_Shell_tunneling)**. If you need to run many or long simulations, a remote computer may be beneficial because it doesn't take up your local computer's resources and doesn't require your local computer to be turned on throughout the simulation time. Instructions below guide you through setting it up.

## Galaxy Tool for FATES
Galaxy is free to use and only requires registering as a user. Note that this tool is to be considered a pilot version, and does not include all the functionalities of the NorESM-LSP. It does, on the other hand, include metadata to make it FAIR (Findable, Accessible, Interoperable, Reusable), and has Research Objects on [ROHub](https://reliance.rohub.org/).

- [Tutorial with the CLM-FATES Galaxy tool]https://training.galaxyproject.org/training-material/topics/climate/tutorials/fates/tutorial.html

