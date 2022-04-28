# Building the sphinx-based documentation

## Quick cheat sheet

Assuming you have done the setup as suggested below: To build the
documentation, run:

```
build_docs -r ../../cism-docs/cism-in-cesm
```

## Overview

The CISM documentation is written using reStructuredText markup
language.  ReStructuredText is a markup language, like HTML, markdown or
latex.

Sphinx is a python tool for publishing reStructuredText documents in
other formats such as HTML and PDF.

The built CISM documentation is hosted in the [cism-docs
repository](https://github.com/escomp/cism-docs).  The sphinx-generated
HTML pages are accessible from URL
<https://escomp.github.io/cism-docs/>. This repository contains builds
of the documentation for both CISM itself and for CISM within CESM. The
latter contains general documentation about using CESM for land ice
applications.

There is a separate subdirectory for the build of each release version
of the documentation, as well as the current development version. This
is needed because GitHub pages only builds a single branch. (See the
thoughts [here](https://github.com/ESCOMP/ctsm/issues/239) for more
details.)

## One-time setup on a given machine

1. Make sure sphinx is installed. See
   http://www.sphinx-doc.org/en/stable/install.html for details.
   
   - Then make sure your PATH includes the path to the sphinx-build script.
   
2. Clone the helper tool, https://github.com/ESMCI/versioned-doc-builder
   
   - For convenience, make sure that the `build_docs` program is
     somewhere in your path.
     
   - Note that this tool is not required, but it makes the generation of
     versioned documentation less error-prone.
     
3. Clone the doc build repo, https://github.com/escomp/cism-docs

   - You can put this anywhere. The below instructions will assume that
     you have cloned this side-by-side with the cism-wrapper repository,
     so you have `/path/to/cism-wrapper` and `/path/to/cism-docs`.
     
## Building the documentation

1. Edit the *.rst pages of interest in the source directory

2. Generate the html pages using sphinx by running the `build_docs`
   tool (which wraps a `make` command, using the `Makefile` here):
   
   ```
   build_docs -r ../../cism-docs/cism-in-cesm
   ```
   
   By default, this will generate documentation in a subdirectory of
   `cism-in-cesm/versions` whose name matches the name of the current
   branch. There are various other options to this tool. For more
   details, run:
   
   ```
   build_docs -h
   ```
   
3. View your changes in a local browser using the following (plugging in
   the name of the current branch for VERSION):

   ```
   open ../../cism-docs/cism-in-cesm/versions/VERSION/html/index.html
   ```

4. Commit and push the changes from the `cism-docs` repository.

5. Confirm that the documentation appears correctly at the GitHub Pages
   site: <https://escomp.github.io/cism-docs/>.
   
## Avoiding conflicts in the build

To avoid conflicts in the build, if multiple people are working on the
documentation at once, you should do the following:

* Feel free to generate the html locally as often as you want, but do
  not commit the regenerated html yet
  
* When you're finished with your changes, from the **cism-docs repo
  (i.e., the repo with the build)**:

  ```
  git checkout -- .
  ```
  
  This resets the generated html to whatever it was on the remote.

* Do the following in rapid succession to avoid conflicts (here, `...`
  should not be typed literally, but indicates extra text that you'll
  want to fill in):

  **From cism-docs**:
  
  ```
  git pull
  ```
  
  **From cism-wrapper/doc**:
  
  ```
  build_docs ...
  ```
  
  **From cism-docs**:
  
  ```
  git add .
  git commit -m "Update html"
  git push
  ```

## Adding a new version of the documentation

When adding a new version of the documentation, you need to do two
things:

1. In `source/conf.py`,change the `version` and `release` variables.

2. In the documentation build repository, add a line to
   `versions/versions.json` giving a mapping between the new version's
   directory (which will typically match the version's branch name) and
   the version name you want to see in the drop-down menu. This will
   typically be the same as the `version` in `conf.py`, but it does not
   have to be.

## Resources for learning markup with reStructuredText and using Sphinx

* [reStructuredText Primer](http://www.sphinx-doc.org/en/stable/rest.html)
* [ReST Syntax](https://wiki.typo3.org/ReST_Syntax)
* [Sphinx (including how to get Sphinx)](http://www.sphinx-doc.org/en/stable/)
* [reStructured syntax](http://thomas-cokelaer.info/tutorials/sphinx/rest_syntax.html#tables)

For Mac users with Macports (CGD folks, IS will install it for you if
you don't already have it), you can do:

```
sudo port install py27-sphinx
```
