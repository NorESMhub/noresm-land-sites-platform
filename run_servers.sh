#! /usr/bin/bash
python3 -m bokeh serve --show make_settings &
jupyter notebook --port=8888 --no-browser --ip=0.0.0.0 --NotebookApp.token=abcd