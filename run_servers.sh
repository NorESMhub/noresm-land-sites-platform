#! /usr/bin/bash
cd bokeh_app
python3 -m bokeh serve --show make_settings &
cd ..
jupyter notebook --port=8888 --no-browser --ip=0.0.0.0 --NotebookApp.token=abcd