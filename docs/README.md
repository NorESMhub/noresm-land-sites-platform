# /docs contains markdown files used to build the technical documentation

See the documentation at <https://noresmhub.github.io/noresm-land-sites-platform>.

Which markdown files to use, with what headers, and in which order, is specified in [mkdocs.yml](https://github.com/NorESMhub/noresm-land-sites-platform/blob/main/mkdocs.yml) in the repository root.

## Editing the documentation pages

The documentation webpage is built using [GitHub pages](https://pages.github.com/) and [MkDocs](https://www.mkdocs.org/). To update it, change markdown files under `/docs` and commit the changes (or open a pull request). When the changes are saved, the page must be built again to fetch the changes. This is done automatically when changes are pushed/committed to `main` by a github Action. Alternatively, you can build the pages by using the `mkdocs gh-deploy` command in the top folder of the repository where mkdocs.yml tells github pages how to build the markdown files into a static webpage. See [Mkdocs docs deployment](https://www.mkdocs.org/user-guide/deploying-your-docs/) for further info. To add a new page, remember to add it in mkdocs.yml as well.
