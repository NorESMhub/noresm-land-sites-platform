#!/usr/bin/python3
"""
Application to create a settings file for the NorESM land sites
platform via interactive widgets.
"""
# General Python imports
import json
from pathlib import Path
from datetime import datetime
import geopandas as gpd

# Local helper functions
import helpers

# Bokeh imports
from bokeh.io import curdoc
from bokeh.layouts import column, row, grid
from bokeh.models import Div, CheckboxGroup, DatePicker, RadioButtonGroup, MultiChoice
from bokeh.models.widgets import TextInput, Button, Paragraph

################################################################################
"""
File-specific helper functions
"""
################################################################################

#### Handling input changes and validation
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

### Load constraint dictionary
with open(platform_dir_path / '/data/.nlp/settings_constraints.json', 'r') \
as constraints_file:
    constraints_dict = json.load(constraints_file)

"""
Define site header as html file.
"""
with open(app_root_dir_path / 'static/html/header.html') as html_file:
    html_text = html_file.read()

header_div = Div(text=html_text, sizing_mode="stretch_width")

################################################################################
"""
Site selection
"""
################################################################################
# Read site geojson
nlp_sites_gdf = gpd.read_file(
platform_dir_path / '/data/.nlp/site_info.geojson')

site_description_div = Div(
text="<h2>Pick sites</h2>Select the sites you want to simulate "\
+"(multi-selection possible):",
sizing_mode="stretch_width")
# Site names
site_labels = [row["name"] for _, row in nlp_sites_gdf.iterrows()]
checkbox_group = CheckboxGroup(labels=site_labels, active=[])

# Reset checks if anything changes
checkbox_group.on_change('active', _input_changed)

def _check_sites():
    return bool(checkbox_group.active)

sites_section = column(site_description_div, checkbox_group)

################################################################################
"""
Directory paths
"""
################################################################################
input_dir_paths_div = Div(
text="<h2>Set directory paths</h2>Skip this section to use the default paths "\
+ "as indicated. Paths can be absolute or relative to the repository root.<br>",
sizing_mode="stretch_width")

clm_in_div = Div(
text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">CLM input</a>: ''',
css_classes=['item-description'], sizing_mode='stretch_width')
clm_input_dir_text_input = TextInput(value="data/input/clm")

cases_div = Div(
text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">case folders</a>: ''',
css_classes=['item-description'], sizing_mode='stretch_width')
cases_dir_text_input = TextInput(value="data/cases")

output_div = Div(
text='''Enter the path for the <a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">model output</a>: ''',
css_classes=['item-description'], sizing_mode='stretch_width')
output_dir_text_input = TextInput(value="data/output")

# Layout
path_input_section = column(input_dir_paths_div,
row(clm_in_div, clm_input_dir_text_input),
row(cases_div, cases_dir_text_input),
row(output_div, output_dir_text_input)
)

################################################################################
"""
Simulation period
"""
################################################################################
period_div = Div(
text="<h2>Simulation period</h2>",
sizing_mode="stretch_width")
start_date_picker = DatePicker(title='Select simulation start date:',
value="2000-01-01", min_date="1900-01-01", max_date="2020-12-30")

end_date_picker = DatePicker(title='Select simulation end date:',
value="2001-01-01", min_date="1900-01-02", max_date="2020-12-31")

dates_section = column(period_div, row(start_date_picker, end_date_picker))

def _check_dates():
    global start_date, end_date
    start_date = datetime.strptime(start_date_picker.value, '%Y-%m-%d')
    end_date = datetime.strptime(end_date_picker.value, '%Y-%m-%d')

    # Is end date after start date?
    return ((end_date-start_date).days > 0)

################################################################################
"""
CLM settings
"""
################################################################################
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

################################################################################
"""
FATES settings
"""
################################################################################
fates_div = Div(
text="<h2>FATES settings</h2>OBS! Only used if model run mode includes FATES.",
sizing_mode="stretch_width")

### PFTs to include
pft_div = Div(text='''Select all the ''' \
+ '''<a href="https://noresmhub.github.io/NorESM_LandSites_Platform/" target="_blank">''' \
+ '''PFTs you want to include</a>.''',
css_classes=['item-description'], sizing_mode='stretch_width')
pft_options = constraints_dict['pft_indices']['long_names']

pft_multi_choice = MultiChoice(value=pft_options, options=pft_options)

fates_settings_section = column(fates_div,
pft_div, pft_multi_choice)

################################################################################
"""
Button to create settings file
"""
################################################################################
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
        output.text = "Simulation end date needs to after the start date."
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

    # Try to write new file
    try:
        helpers.write_settings_file()
        output.text = f"Created '{file_name}'!"
    except:
        output.text = f"Error when creating {file_name}."
        raise

check_input_button.on_click(check_input)
create_button.on_click(update)

################################################################################
"""
Specify site layout
"""
################################################################################
layout = grid([
    [header_div],
    [sites_section],
    [path_input_section],
    [dates_section],
    [clm_settings_section],
    [fates_settings_section],
    [create_file_section]
],
 sizing_mode=None)

# add the layout to curdoc
curdoc().add_root(layout)
