# How to contribute to the NorESM land sites platform

The NorESM land sites platform is developed by a group of students, postdocs, and software engineers when we have the time or need certain functionalities. We welcome anyone to join us in developing new functionalities, using the platform, add to the documentation, and contribute to making this platform a dynamic and useful research and teaching tool. If you have questions, comments, or suggestions for improvements, please open an [issue](https://github.com/NorESMhub/noresm-land-sites-platform/issues) in the repository. Any contributions and involvement in our community must be in line with our [Code of conduct](https://noresmhub.github.io/noresm-land-sites-platform/contributing/#code-of-conduct).

### How to contribute to the code

We use GitHub for developing code. If you are new to working with Git and GitHub, you might like [this](https://kbroman.org/github_tutorial/ "a minimalist intro") or [this](https://docs.github.com/en/get-started/quickstart/hello-world "GitHub's own tutorial") tutorial. 

#### Quick links to central [GitHub repositories](https://en.wikipedia.org/wiki/Git "a place to store code with version control")

- [NorESMhub/noresm-land-sites-platform](https://github.com/NorESMhub/noresm-land-sites-platform)
- [NorESMhub/noresm-lsp-ui](https://github.com/NorESMhub/noresm-lsp-ui)
- [NorESMhub/noresm-lsp-input](https://github.com/NorESMhub/noresm-lsp-input)
- [NorESMhub/ctsm-api](https://github.com/NorESMhub/ctsm-api)
- [NorESMhub/NorESM](https://github.com/NorESMhub/NorESM)

The current version of the platform is kept in the `main` branch of the noresm-land-sites-platform repository. Out first release tag of a functioning version (without GUI and API) is stored in the `archive` branch. Further development happens on individual forks and the `develop` branch, and are merged into main with a pull request when the changes are functioning and tested. If you are developing code, please fork the repository and make your changes there before creating a pull request to the `develop` branch (or to `main` if you are confident the changes are complete and don't break anything).

If you have questions or comments about the platform, please open an issue [issue](https://github.com/NorESMhub/noresm-land-sites-platform/issues) in the repository.

### How to contribute to the documentation

This documentation page is built using GitHub pages and [MkDocs](https://www.mkdocs.org/). To update it, change markdown files in /docs and commit the changes (or open a pull request). When the changes are saved, the page must be built again to fetch the changes. This is done automatically when changes are pushed/committed to `main` by a github Action. Alternatively, you can build the pages by using the `mkdocs gh-deploy` command in the top folder of the repository where mkdocs.yml tells github pages how to build the markdown files into a static webpage. See the [Mkdocs docs](mkdocs gh-deploy) for further info.

### How to contribute new sites

New sites can be added by copying and modifying some existing code and creating new input data for the site. Currently (v1.0.0), you need to carefully hardcode new sites into different files within the noresm-lsp structure. In other words, you need to copy and modify code in several files in the noresm-land-sites-platform repository, and use a high-performance computer (e.g. [Saga](https://documentation.sigma2.no/hpc_machines/saga.html)) to make new input with the input repository. Detailed instructions for doing this are found in the [NorESMhub/noresm-lsp-input](https://github.com/NorESMhub/noresm-lsp-input) repository and code examples are in the [Pull Request where the Iškuras (ISK) site was added](https://github.com/NorESMhub/noresm-land-sites-platform/pull/116). 


--------------------------

## Code of conduct

### Our Pledge

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone, regardless of age, body size, visible or invisible disability, ethnicity, sex characteristics, gender identity and expression, level of experience, education, socio-economic status, nationality, personal appearance, race, caste, color, religion, or sexual identity and orientation. We pledge to act and interact in ways that contribute to an open, welcoming, diverse, inclusive, and healthy community.

### Our Standards

Examples of behavior that contributes to a positive environment for our community include:

- Demonstrating empathy and kindness toward other people
- Being respectful of differing opinions, viewpoints, and experiences
- Giving and gracefully accepting constructive feedback
- Accepting responsibility and apologizing to those affected by our mistakes, and learning from the experience
- Focusing on what is best not just for us as individuals, but for the overall community

Examples of unacceptable behavior include:

- The use of sexualized language or imagery, and sexual attention or advances of any kind
- Trolling, insulting or derogatory comments, and personal or political attacks
- Public or private harassment
- Publishing others’ private information, such as a physical or email address, without their explicit permission
- Other conduct which could reasonably be considered inappropriate in a professional setting

### Scientific Use and Publication Ethics

We aim to create an open development environment where scientists can be confident that all members of the community are conducting research in an ethical manner. In particular, writing scientific code is a form of intellectual contribution, and one should expect that all such intellectual contributions are respected and given credit in any resulting published scientific work. To support the community and avoid issues of scientific misconduct related to the above principle, please respect the following rules:

- Document the version of the platform and models used in any publication, preferably by using a release tag (existing or newly created) if possible, or a commit hash if not.
- Do not use code without checking the license or discussing with the author(s) your intentions for using the code and receiving their permission to do so.
- When using model features that have recently been integrated into this or the central NorESM/CLM/FATES development repositories, be mindful of the contributions of others and, where the novel features qualitatively affect the results of a given simulation, involve the author(s) of these features in any resulting manuscripts. Be particularly aware of the concerns of early career researchers, and ensure they have sufficient opportunities to lead publications using their developments.
- When discussing results arising from older model features that have been described in the literature, accurately cite the publications describing those features or releases. 

### Enforcement Responsibilities

Community leaders should clarify and enforce our standards of acceptable behavior and take appropriate and fair corrective action in response to any behavior that they deem inappropriate, threatening, offensive, or harmful. Community leaders have the right to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct, and will communicate reasons for moderation decisions when appropriate.

### Scope

This Code of Conduct applies within all community spaces, and also applies when an individual is officially representing the community in public spaces. 

### Enforcement

All community leaders are obligated to respect the privacy and security of the reporter of any incident.

Consequences for any action deemed to violate this Code of Conduct, enforced by community leaders, may be:

1. Correction 

Community Impact: Use of inappropriate language or other behavior deemed unprofessional or unwelcome in the community.
Consequence: A private, written warning from community leaders, providing clarity around the nature of the violation and an explanation of why the behavior was inappropriate. A public apology may be requested.

2. Warning

Community Impact: A violation through a single incident or series of actions.
Consequence: A warning with consequences for continued behavior. No interaction with the people involved, including unsolicited interaction with those enforcing the Code of Conduct, for a specified period of time. This includes avoiding interactions in community spaces as well as external channels like social media. Violating these terms may lead to a temporary or permanent ban.

3. Temporary Ban

Community Impact: A serious violation of community standards, including sustained inappropriate behavior.
Consequence: A temporary ban from any sort of interaction or public communication with the community for a specified period of time. No public or private interaction with the people involved, including unsolicited interaction with those enforcing the Code of Conduct, is allowed during this period. Violating these terms may lead to a permanent ban.

4. Permanent Ban

Community Impact: Demonstrating a pattern of violation of community standards, including sustained inappropriate behavior, harassment of an individual, or aggression toward or disparagement of classes of individuals.
Consequence: A permanent ban from any sort of public interaction within the community.

### Attribution

This Code of Conduct is adapted from the [Contributor Covenant, version 2.1](https://www.contributor-covenant.org/version/2/1/code_of_conduct.html), and the [FATES Code of Conduct](https://github.com/NGEET/fates/blob/master/CODE_OF_CONDUCT.md).
