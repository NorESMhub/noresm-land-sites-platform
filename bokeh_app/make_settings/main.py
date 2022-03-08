#!/usr/bin/python3
"""
Application to create a settings file and build/run cases for the NorESM land
sites platform via interactive widgets.
"""
# General Python imports
import json
from pathlib import Path
from datetime import datetime
import geopandas as gpd
import subprocess
from subprocess import PIPE, STDOUT
import glob

# Local imports
import helpers
from landsites_tools.utils import paths as pth

# Bokeh imports
from bokeh.io import curdoc
from bokeh.layouts import column, row, grid
from bokeh.models import Div, CheckboxGroup, DatePicker, RadioButtonGroup
from bokeh.models import MultiChoice, Panel, Tabs, MultiSelect, CustomJS
from bokeh.models.widgets import TextInput, Button, Paragraph
from bokeh.events import ButtonClick

###############################################################################
"""
File-specific helper functions
"""
###############################################################################

# Handling input changes and validation
default_text = "You need to (re-)validate the input before creating the file."


def _input_valid():
    create_button.disabled = False
    output.css_classes = ['valid']


def _input_invalid():
    create_button.disabled = True
    output.css_classes = ['invalid']


def _input_changed(attr, new, old):
    output.text = default_text
    _input_invalid()


### Paths to roots
app_root_dir_path = Path(__file__).parent
platform_dir_path = Path(__file__).parents[2]

###############################################################################
###############################################################################
###############################################################################
"""
Make settings tab
"""
###############################################################################
###############################################################################
###############################################################################


### Load constraint dictionary
with open(platform_dir_path / 'data/.nlp/settings_constraints.json', 'r') \
        as constraints_file:
    constraints_dict = json.load(constraints_file)

"""
Set settings header as defined in html file.
"""
with open(app_root_dir_path / 'static/html/settings_header.html') as html_file:
    html_text = html_file.read()

header_div = Div(text=html_text, sizing_mode="stretch_width")

###############################################################################
"""
Site selection
"""
###############################################################################
# Read site geojson
nlp_sites_gdf = gpd.read_file(
    platform_dir_path / 'data/.nlp/site_info.geojson')

site_description_div = Div(
    text="<h2>Pick sites</h2>Select the sites you want to simulate "
    + "(multi-selection possible):",
    sizing_mode="stretch_width")
# Site names
site_labels = [row["name"] for _, row in nlp_sites_gdf.iterrows()]
sites_checkbox_group = CheckboxGroup(labels=site_labels, active=[])

# Reset checks if anything changes
sites_checkbox_group.on_change('active', _input_changed)


def _check_sites():
    return bool(sites_checkbox_group.active)


sites_section = column(site_description_div, sites_checkbox_group)

###############################################################################
"""
Directory paths
"""
###############################################################################
input_dir_paths_div = Div(
    text="<h2>Set directory paths</h2>Skip this section to use the default paths "
    + "as indicated. Paths can be absolute or relative to the repository root.<br>",
    sizing_mode="stretch_width")

clm_in_div = Div(
    text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">CLM input</a>: ''',
    css_classes=['item-description'], sizing_mode='stretch_width')
clm_input_dir_text_input = TextInput(value="data/input/clm")
clm_input_dir_text_input.on_change('value', _input_changed)

cases_div = Div(
    text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">case folders</a>: ''',
    css_classes=['item-description'], sizing_mode='stretch_width')
cases_dir_text_input = TextInput(value="data/cases")
cases_dir_text_input.on_change('value', _input_changed)

output_div = Div(
    text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">model output</a>: ''',
    css_classes=['item-description'], sizing_mode='stretch_width')
output_dir_text_input = TextInput(value="data/output")
output_dir_text_input.on_change('value', _input_changed)


def _check_paths():
    for input in [clm_input_dir_text_input, cases_dir_text_input,
                  output_dir_text_input]:
        try:
            if pth.is_valid_path(Path(input.value), type="dir"):
                continue
            else:
                print(f"{input.value} does not point to an existing directory!")
                return False
        except:
            return False
    return True


# Layout
path_input_section = column(input_dir_paths_div,
                            row(clm_in_div, clm_input_dir_text_input),
                            row(cases_div, cases_dir_text_input),
                            row(output_div, output_dir_text_input)
                            )

###############################################################################
"""
Simulation period
"""
###############################################################################
period_div = Div(
    text="<h2>Simulation period</h2>",
    sizing_mode="stretch_width")
start_date_picker = DatePicker(title='Select simulation start date:',
                               value="2000-01-01", min_date="1900-01-01", max_date="2020-12-30")
start_date_picker.on_change('value', _input_changed)

end_date_picker = DatePicker(title='Select simulation end date:',
                             value="2001-01-01", min_date="1900-01-02", max_date="2020-12-31")
end_date_picker.on_change('value', _input_changed)

dates_section = column(period_div, row(start_date_picker, end_date_picker))


def _check_dates():
    global start_date, end_date
    start_date = datetime.strptime(start_date_picker.value, '%Y-%m-%d')
    end_date = datetime.strptime(end_date_picker.value, '%Y-%m-%d')

    # Is end date after start date?
    return ((end_date-start_date).days > 0)


###############################################################################
"""
CLM settings
"""
###############################################################################
clm_div = Div(text="<h2>CLM settings</h2>",
              sizing_mode="stretch_width")

### Radio buttons - run type
type_run_div = Div(text='''Select the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">model run type</a>.''',
                   css_classes=['item-description'], sizing_mode='stretch_width')
type_run_labels = constraints_dict['type_run']['valid_values']
type_run_radio = RadioButtonGroup(labels=type_run_labels, active=0)
#show(runtype_radio_buttons)

### Radio buttons - run mode
type_model_div = Div(
    text='''Select the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">model run mode</a>.''',
    css_classes=['item-description'], sizing_mode='stretch_width')
type_model_labels = constraints_dict['type_model']['valid_values']
type_model_radio = RadioButtonGroup(labels=type_model_labels, active=0)

### Column for display
clm_settings_section = column(clm_div,
                              type_run_div, type_run_radio,
                              type_model_div, type_model_radio)

###############################################################################
"""
FATES settings
"""
###############################################################################
fates_div = Div(
    text="<h2>FATES settings</h2>OBS! Only used if model run mode includes FATES.",
    sizing_mode="stretch_width")

### PFTs to include
pft_div = Div(text='''Select all the '''
              + '''<a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">'''
              + '''PFTs you want to include</a>.''',
              css_classes=['item-description'], sizing_mode='stretch_width')
pft_options = constraints_dict['pft_indices']['long_names']

pft_multi_choice = MultiChoice(value=pft_options, options=pft_options)

fates_settings_section = column(fates_div, pft_div, pft_multi_choice)

###############################################################################
"""
Button to create settings file
"""
###############################################################################
create_button_div = Div(
    text="<h2>Create file</h2>Enter a name for the new settings file:<br>",
    sizing_mode="stretch_width")
input = TextInput(value="file-name")
check_input_button = Button(label="Check input", button_type='primary',
                            disabled=False)
create_button = Button(label="Create file!", button_type='primary',
                       disabled=True)
output = Paragraph(text=default_text, css_classes=["invalid"])

create_file_section = column(create_button_div, input,
                             row(check_input_button, create_button), output)

"""
Updates when buttons are clicked
"""


def check_input():
    """Button handler for checking the user input."""

    if not _check_sites():
        output.text = "You need to select at least one site."
        _input_invalid()
        pass
    elif not _check_dates():
        output.text = "Simulation end date needs to be after the start date."
        _input_invalid()
        pass
    elif not _check_paths():
        output.text = "One of the specified paths does not exist. " \
            + "See console for details."
        _input_invalid()
        pass
    else:
        output.text = "Input checks passed. You can now create the file."
        _input_valid()


def update():
    """Button handler for creating a new file."""

    # Add .txt ending to file name if not provided
    if not input.value.endswith(".txt"):
        file_name = input.value + ".txt"
    else:
        file_name = input.value

    # Check if file already exists
    save_path = platform_dir_path / 'landsites_tools/custom_settings/'
    if (save_path / f"{file_name}").is_file():
        output.text = f"'{file_name}' already exists! Pick a different name."
    else:
        # Try to write new file
        try:
            output.text = f"Creating '{file_name}', please stand by..."

            # Convert site indices to site names
            site_names = []
            for idx in sites_checkbox_group.active:
                site_names.append(site_labels[idx])

            # Convert pft names to indices
            pft_indices = []
            for idx, pft in enumerate(pft_options):
                if pft in pft_multi_choice.value:
                    pft_indices.append(idx)

            # Write settings file
            helpers.write_settings_file(
                file_name=str(file_name),
                sites2run=",".join(site_names),
                dir_cases=str(clm_input_dir_text_input.value),
                dir_clm_input=str(cases_dir_text_input.value),
                dir_output=str(output_dir_text_input.value),
                start_time=str(start_date),
                end_time=str(end_date),
                type_run=str(type_run_labels[type_run_radio.active]),
                type_model=str(type_model_labels[type_model_radio.active]),
                pft_indices=",".join([str(x) for x in pft_indices])
            )
            output.text = f"Created '{file_name}'!"

        except:
            output.text = f"Error when creating {file_name}."
            raise


check_input_button.on_click(check_input)
create_button.on_click(update)

###############################################################################
###############################################################################
###############################################################################
"""
Build and run cases tab
"""
###############################################################################
###############################################################################
###############################################################################

"""
Set settings header as defined in html file.
"""
with open(app_root_dir_path / 'static/html/run_header.html') as html_file:
    run_html_text = html_file.read()

run_header_div = Div(text=run_html_text, sizing_mode="stretch_width")

###############################################################################
"""
Make cases
"""
###############################################################################


root_path = Path(__file__).parents[2]
settings_path = root_path / 'landsites_tools/custom_settings/'
script_path = root_path / 'landsites_tools/simulation/'


def get_settings_files():
    """Get all settings files in 'landsites_tools/custom_settings' in a list"""

    global settings_file_paths
    global settings_file_names

    file_path_str_list = glob.glob(str(settings_path) + "/*.txt")

    settings_file_paths = [Path(fname) for fname in file_path_str_list]
    settings_file_names = [p.name for p in settings_file_paths]


def retrieve_select_options(settings_file_names):

    settings_files_select_options = \
        [(str(idx), f_name) for idx, f_name in enumerate(settings_file_names)]

    return(settings_files_select_options)


make_cases_div = Div(
    text="<h2>Make case(s)</h2>",
    sizing_mode="stretch_width")

# Settings file picker
make_cases_select_div = Div(text="Select one settings file.",
                            css_classes=['item-description'],
                            sizing_mode='stretch_width')

# List for picking a file
get_settings_files()
settings_files_select_options = retrieve_select_options(settings_file_names)

make_cases_select = MultiSelect(value=[],
                                options=settings_files_select_options)

# Make cases button
make_cases_button = Button(label="Make case(s)", button_type='primary',
                           disabled=False)
refresh_button = Button(label="Refresh files", button_type='default',
                        disabled=False)

make_cases_output = Div(text="Click the button above to create and build "
                        + "the cases specified in the chosen settings file.",
                        css_classes=["item-description"],
                        style={'overflow': 'auto',
                               'width': '100%',
                               'height': '200px'}
                        )

"""
JS_executing_feedback = CustomJS(args={'bokeh_div': make_cases_output,
                                       'make_button': make_cases_button},
                                 code='''
bokeh_div.text="Making cases...<br>This step will take approx. 10 minutes " +
"per case.<br>Logging information will be printed here when the process " +
"is finished.";
make_button.disabled=true;
''')
"""
#make_cases_button.js_on_event(ButtonClick, JS_executing_feedback)


def make_cases():

    if len(make_cases_select.value) != 1:
        make_cases_output.text = \
            "You must select one single settings file!"
        return

    file_idx = int(make_cases_select.value[0])

    make_cases_output.text = \
        f"Making cases for {settings_file_names[file_idx]}" \
        + "...<br> This step will take approx. 10 minutes per case. Check " \
        + "your local terminal for progress information."

    make_cases_button.disabled = True


def make_cases_subprocess():

    file_idx = int(make_cases_select.value[0])

    make_cases_settings_file = settings_file_paths[file_idx]

    proc = subprocess.run(
        f"python3 {script_path}/make_cases.py -f {make_cases_settings_file}",
        shell=True, check=True)  # , stdout=PIPE, stderr=STDOUT)

    # new_line = '\n'
    make_cases_output.text = \
        f"Finished for {settings_file_names[file_idx]}!<br>" \
        + "You can find logging information in the terminal."
    # + f"{(proc.stdout).decode('utf-8').replace(new_line, '<br>')}"

    make_cases_button.disabled = False


def refresh_file_list():

    get_settings_files()
    settings_files_select_options = retrieve_select_options(
        settings_file_names)

    make_cases_select.options = settings_files_select_options
    make_cases_select.value = []

    run_cases_select.options = settings_files_select_options
    run_cases_select.value = []


make_cases_button.on_click(make_cases)
make_cases_button.on_click(make_cases_subprocess)
refresh_button.on_click(refresh_file_list)

make_cases_section = column(make_cases_div,
                            make_cases_select_div,
                            make_cases_select,
                            row(make_cases_button, refresh_button),
                            make_cases_output
                            )


###############################################################################
"""
Run cases
"""
###############################################################################

run_cases_div = Div(
    text="<h2>Run case(s)</h2>",
    sizing_mode="stretch_width")

# Settings file picker
run_cases_select_div = Div(text="Select one settings file.",
                           css_classes=['item-description'],
                           sizing_mode='stretch_width')

# List for picking a file
run_cases_select = MultiSelect(value=[],
                               options=settings_files_select_options)

# Make cases button
run_cases_button = Button(label="Run case(s)", button_type='primary',
                          disabled=False)
refresh_button_2 = Button(label="Refresh files", button_type='default',
                          disabled=False)

run_cases_output = Div(text="Click the button above to run the cases "
                       + "specified in the chosen settings file.",
                       css_classes=["item-description"],
                       style={'overflow': 'auto',
                              'width': '100%',
                              'height': '200px'}
                       )


def run_cases():

    file_idx = int(run_cases_select.value[0])

    run_cases_output.text = \
        f"Running cases in {settings_file_names[file_idx]}" \
        + "...<br><br> This step will take a while. The run time depends on " \
        + "the model settings, e.g., the simulation period or the computer " \
        + "resource allocation in docker-compose.yml. Expect roughly 15 min " \
        + "for each simulation year. Check your local terminal for " \
        + "additional progress information."

    run_cases_button.disabled = True


def run_cases_subprocess():

    if len(run_cases_select.value) != 1:
        run_cases_output.text = \
            "You must select one single settings file!"
        return

    file_idx = int(run_cases_select.value[0])

    run_cases_settings_file = settings_file_paths[file_idx]

    proc = subprocess.run(
        f"python3 {script_path}/run_cases.py -f {run_cases_settings_file} -q",
        shell=True, check=True)  # , stdout=PIPE, stderr=STDOUT)

    # new_line = '\n'
    run_cases_output.text = \
        f"Finished for {settings_file_names[file_idx]}!<br>" \
        + "You can find logging information in the terminal."
    # + f"{(proc.stdout).decode('utf-8').replace(new_line, '<br>')}"

    run_cases_button.disabled = False


run_cases_button.on_click(run_cases)
run_cases_button.on_click(run_cases_subprocess)
refresh_button_2.on_click(refresh_file_list)

run_cases_section = column(run_cases_div,
                           run_cases_select_div,
                           run_cases_select,
                           row(run_cases_button, refresh_button_2),
                           run_cases_output
                           )

###############################################################################
"""
Specify site layout
"""
###############################################################################

# Define layouts
layout_settings = grid([
    [header_div],
    [sites_section],
    [path_input_section],
    [dates_section],
    [clm_settings_section],
    [fates_settings_section],
    [create_file_section]
],
 sizing_mode=None)

layout_running = grid([
 [run_header_div],
 [make_cases_section],
 [run_cases_section]
 ],
  sizing_mode=None)

# Define tabs
tab_1 = Panel(child=layout_settings, title="Create settings")
tab_2 = Panel(child=layout_running, title="Build/run cases")

page_tabs = Tabs(tabs=[tab_1, tab_2])

# add the layout to curdoc
curdoc().add_root(page_tabs)
