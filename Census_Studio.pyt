"""
ACS Census Data Download Toolbox (R-based)
Uses R's tidycensus library to download American Community Survey data.

Tools:
- Download ACS Data with tidycensus (R)
- Generate Variable Lookup File
- Set Census API Key
"""

import arcpy
import os
import json
import subprocess
import tempfile
import shutil


class Toolbox(object):
    def __init__(self):
        self.label = "Census Studio"
        self.alias = "censusstudio"
        self.tools = [DownloadACSDataR, CompareTimePeriods, JoinACSData, NormalizeData, MOEReliabilityFilter, DemographicProfileReport, HotSpotAnalysis, GenerateVariableLookup, SetCensusAPIKey]


class DownloadACSDataR(object):
    def __init__(self):
        self.label = "Download ACS Census Data"
        self.description = "Download American Community Survey data and create a feature class"
        self.canRunInBackground = False
        self.category = "Census Data"

    def _get_lookup_path(self, year=None, survey=None):
        if year and survey:
            filename = f"acs_variable_lookup_{year}_{survey}.json"
        else:
            filename = "acs_variable_lookup.json"  # fallback
        return os.path.join(os.path.dirname(__file__), "lookups", filename)

    def _load_lookup(self, year=None, survey=None):
        lookup_path = self._get_lookup_path(year, survey)
        try:
            if os.path.exists(lookup_path):
                with open(lookup_path, 'r', encoding='utf-8') as f:
                    return json.load(f)
        except Exception:
            pass
        return None

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Year",
            name="year",
            datatype="GPLong",
            parameterType="Required",
            direction="Input",
            category="Connection")
        p0.value = 2023
        p0.filter.type = "Range"
        p0.filter.list = [2005, 2040]  # Allow wide range for future years
        p0.description = "The ACS data year to download. ACS 5-Year available from 2009, ACS 1-Year from 2005."

        p1 = arcpy.Parameter(
            displayName="Survey",
            name="survey",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Connection")
        p1.filter.type = "ValueList"
        p1.filter.list = ["acs5", "acs1"]
        p1.value = "acs5"
        p1.description = "ACS 5-Year provides data for all geographies but is less current. ACS 1-Year is more current but only covers areas with 65,000+ population."

        p2 = arcpy.Parameter(
            displayName="Category",
            name="category",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable Browser")
        p2.filter.type = "ValueList"
        p2.filter.list = []
        p2.description = "Select a category to filter the available tables."

        p3 = arcpy.Parameter(
            displayName="Table",
            name="table_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable Browser")
        p3.filter.type = "ValueList"
        p3.filter.list = []
        p3.description = "Select a table to see its available variables."

        p4 = arcpy.Parameter(
            displayName="Variables",
            name="variables_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable Browser",
            multiValue=True)
        p4.filter.type = "ValueList"
        p4.filter.list = []
        p4.description = "Select one or more variables to download."

        p5 = arcpy.Parameter(
            displayName="Or Enter Variable Codes Manually (comma-separated)",
            name="variables_manual",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable Browser")
        p5.description = "Enter variable codes directly (e.g., B19013_001, B25077_001). Useful when you know the codes or want variables from multiple tables."

        p6 = arcpy.Parameter(
            displayName="Download Entire Table",
            name="download_table",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input",
            category="Variable Browser")
        p6.value = False
        p6.description = "Download all variables from the selected table instead of individual variables."

        p7 = arcpy.Parameter(
            displayName="Geography Level",
            name="geography",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Geography")
        p7.filter.type = "ValueList"
        p7.filter.list = [
            "state", "county", "tract", "block group", "place", "zcta",
            "congressional district", "state legislative district (upper chamber)",
            "state legislative district (lower chamber)", "county subdivision",
            "school district (unified)"]
        p7.value = "tract"
        p7.description = "The geographic level for the data. Smaller geographies (tract, block group) provide more detail but larger datasets."

        p8 = arcpy.Parameter(
            displayName="State",
            name="state",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Geography",
            multiValue=True)
        p8.filter.type = "ValueList"
        p8.filter.list = [
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
            "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
            "WI", "WY"]
        p8.description = "Select one or more states. County filtering is disabled when multiple states are selected."

        p9 = arcpy.Parameter(
            displayName="County (optional - comma separated for multiple counties)",
            name="county",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p9.description = "Filter to specific counties (comma-separated for multiple, e.g., 'Travis, Williamson, Hays'). Enter county names without 'County' suffix. Leave blank for all counties in the state."

        p10 = arcpy.Parameter(
            displayName="ZCTA (comma separated for multiple ZCTAs)",
            name="zcta",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p10.description = "ZIP Code Tabulation Area codes (comma-separated, e.g., '78701, 78702, 78704'). Only used when Geography Level is 'zcta'. ZCTAs approximate ZIP code boundaries."

        p11 = arcpy.Parameter(
            displayName="MOE Confidence Level",
            name="moe_level",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p11.filter.type = "ValueList"
        p11.filter.list = [90, 95, 99]
        p11.value = 90
        p11.description = "Confidence level for margin of error calculations. Higher values give wider margins. 90% is the Census Bureau default."

        p12 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Output",
            category="Geography")
        p12.description = "Output location for the feature class. Use a geodatabase for full field names, or shapefile (10-character field name limit)."

        p13 = arcpy.Parameter(
            displayName="Keep Geographic Variables",
            name="keep_geo_vars",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p13.value = False
        p13.description = "Include additional geographic identifiers (NAME, STATEFP, COUNTYFP, ALAND, AWATER, etc.) beyond the standard GEOID."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        # Parameter references
        year_param = parameters[0]
        survey_param = parameters[1]
        cat_param = parameters[2]
        tbl_param = parameters[3]
        var_param = parameters[4]
        geo_level_param = parameters[7]
        state_param = parameters[8]
        county_param = parameters[9]
        zcta_param = parameters[10]

        # Get current year and survey
        year = year_param.value
        survey = survey_param.valueAsText

        # Load lookup based on year/survey
        lookup = self._load_lookup(year, survey)

        # --- Variable Browser Logic ---
        # Refresh categories when year or survey changes
        if (year_param.altered and not year_param.hasBeenValidated) or \
           (survey_param.altered and not survey_param.hasBeenValidated):
            if lookup:
                cat_param.filter.list = lookup.get("categories", [])
            else:
                cat_param.filter.list = []
            cat_param.value = None
            tbl_param.filter.list = []
            tbl_param.value = None
            var_param.filter.list = []
            var_param.value = None

        if lookup:
            if cat_param.altered and not cat_param.hasBeenValidated:
                cat = cat_param.valueAsText
                if cat and cat in lookup.get("tables", {}):
                    tables = lookup["tables"][cat]
                    tbl_param.filter.list = sorted([f"{t['code']} - {t['label'][:80]}" for t in tables])
                else:
                    tbl_param.filter.list = []
                tbl_param.value = None
                var_param.value = None
                var_param.filter.list = []

            if tbl_param.altered and not tbl_param.hasBeenValidated:
                tbl_sel = tbl_param.valueAsText
                if tbl_sel:
                    tbl_code = tbl_sel.split(" - ")[0].strip()
                    if tbl_code in lookup.get("variables", {}):
                        vars_list = lookup["variables"][tbl_code]
                        var_param.filter.list = [f"{v['code']} - {v['label'][:70]}" for v in vars_list]
                    else:
                        var_param.filter.list = []
                else:
                    var_param.filter.list = []
                var_param.value = None

        # --- Geography Logic ---
        geo_level = geo_level_param.valueAsText
        state_values = state_param.values or []
        multiple_states = len(state_values) > 1

        if geo_level == "zcta":
            # ZCTA selected: disable State and County, enable ZCTA
            state_param.enabled = False
            county_param.enabled = False
            zcta_param.enabled = True

            # Clear state/county values to prevent stale data being passed to R
            if geo_level_param.altered and not geo_level_param.hasBeenValidated:
                state_param.value = None
                county_param.value = None
        else:
            # Hierarchical geography: enable State, disable ZCTA
            state_param.enabled = True
            zcta_param.enabled = False

            # Disable county if multiple states selected
            if multiple_states:
                county_param.enabled = False
                if state_param.altered and not state_param.hasBeenValidated:
                    county_param.value = None
            else:
                county_param.enabled = True

            # Clear ZCTA value to prevent stale data being passed to R
            if geo_level_param.altered and not geo_level_param.hasBeenValidated:
                zcta_param.value = None

    def updateMessages(self, parameters):
        year_param = parameters[0]
        survey_param = parameters[1]
        geo_level_param = parameters[7]
        state_param = parameters[8]
        county_param = parameters[9]
        zcta_param = parameters[10]
        var_param = parameters[4]
        manual_var_param = parameters[5]
        download_table_param = parameters[6]
        tbl_param = parameters[3]

        year = year_param.value
        survey = survey_param.valueAsText
        geo_level = geo_level_param.valueAsText

        # --- Variable Selection Validation ---
        if not var_param.value and not manual_var_param.value and not download_table_param.value:
            var_param.setErrorMessage(
                "Select variables, enter codes manually, or check Download Entire Table")

        if download_table_param.value and not tbl_param.value:
            download_table_param.setErrorMessage("Select a table first")

        # Check if lookup file exists for selected year/survey
        if not self._load_lookup(year, survey):
            if not manual_var_param.value:
                parameters[2].setErrorMessage(
                    f"No variable lookup file for {year} {survey}. Run 'Generate Variable Lookup File' for this year/survey, or enter variable codes manually.")
            else:
                parameters[2].setWarningMessage(
                    f"No variable lookup file for {year} {survey}. Variable browser unavailable, but manual entry will work.")

        # --- Geography Validation ---
        state_values = state_param.values or []
        multiple_states = len(state_values) > 1

        if geo_level == "zcta":
            # ZCTA mode: ZCTA is required, State/County should be empty
            if not zcta_param.value:
                zcta_param.setErrorMessage("Enter one or more ZCTA codes (comma-separated)")
            if state_param.value:
                state_param.setWarningMessage("State is ignored for ZCTA geography")
            if county_param.value:
                county_param.setWarningMessage("County is ignored for ZCTA geography")
        else:
            # Hierarchical mode: State is required, ZCTA should be empty
            if not state_param.value:
                state_param.setErrorMessage("State is required for this geography level")
            if zcta_param.value:
                zcta_param.setWarningMessage("ZCTA is ignored for non-ZCTA geography levels")
            if multiple_states and county_param.value:
                county_param.setWarningMessage("County filtering disabled when multiple states selected")

    def execute(self, parameters, messages):
        script_path = os.path.join(os.path.dirname(__file__), "acs_download.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        def to_r_bool(val, default="FALSE"):
            if val is None:
                return default
            return "TRUE" if val else "FALSE"

        def to_r_str(val, default=""):
            return str(val) if val else default

        vars_sel = parameters[4].values or []
        vars_man = parameters[5].valueAsText or ""
        dl_table = parameters[6].value
        tbl_sel = parameters[3].valueAsText

        var_codes = [v.split(" - ")[0].strip() for v in vars_sel if v]
        if vars_man:
            var_codes.extend([c.strip() for c in vars_man.split(",")])
        vars_str = ",".join(var_codes)

        tbl_code = ""
        if dl_table and tbl_sel:
            tbl_code = tbl_sel.split(" - ")[0].strip()

        # Handle multi-value state parameter
        state_values = parameters[8].values or []
        if state_values:
            state_str = ",".join([str(s) for s in state_values])
        else:
            state_str = ""

        in_params = [
            to_r_str(parameters[7].valueAsText),   # geography
            vars_str,                               # variables
            tbl_code,                               # table code
            state_str,                              # state (comma-separated)
            to_r_str(parameters[9].valueAsText),   # county
            to_r_str(parameters[10].valueAsText),  # zcta
            to_r_str(parameters[0].value),         # year
            to_r_str(parameters[1].valueAsText),   # survey
            "TRUE",                                 # geometry (hardcoded)
            to_r_bool(parameters[13].value, "FALSE"), # keep_geo_vars
            "",                                     # summary_var (hardcoded empty)
            "TRUE",                                 # output_wide (hardcoded)
            "FALSE",                                # cache_table (hardcoded)
            "FALSE",                                # show_call (hardcoded)
            to_r_str(parameters[11].value, "90"),  # moe_level
        ]

        arcpy.AddMessage("=" * 50)
        arcpy.AddMessage("ACS Download Parameters:")
        arcpy.AddMessage(f"  Year/Survey: {in_params[6]} {in_params[7]}")
        if tbl_code:
            arcpy.AddMessage(f"  Table: {tbl_code} (all variables)")
        else:
            arcpy.AddMessage(f"  Variables: {vars_str or '(none specified)'}")
        arcpy.AddMessage(f"  Geography: {in_params[0]}")
        arcpy.AddMessage(f"  State: {in_params[3]}")
        arcpy.AddMessage(f"  County: {in_params[4] or '(all)'}")
        arcpy.AddMessage(f"  Output: {parameters[12].valueAsText}")
        arcpy.AddMessage("=" * 50)

        # Check if output is a geodatabase feature class
        output_path = parameters[12].valueAsText
        self._temp_shp = None
        truncate_fields = "TRUE"  # Default: truncate for shapefiles

        if '.gdb' in output_path.lower() or '.sde' in output_path.lower():
            # Output is geodatabase - use GeoPackage as intermediate (supports long field names)
            temp_dir = tempfile.mkdtemp(prefix="acs_download_")
            fc_name = os.path.basename(output_path)
            self._temp_gpkg = os.path.join(temp_dir, f"{fc_name}.gpkg")
            truncate_fields = "FALSE"  # Don't truncate - geodatabase supports long names
            arcpy.AddMessage(f"Geodatabase output detected.")
            arcpy.AddMessage(f"Writing to temporary GeoPackage first: {self._temp_gpkg}")
            out_params = [self._temp_gpkg, truncate_fields]
        else:
            self._temp_gpkg = None
            out_params = [output_path, truncate_fields]

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        cmd = [rscript, script_path] + in_params + out_params

        r_success = False

        # Run R script
        arcpy.AddMessage("Connecting to Census API...")
        try:
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                cwd=os.path.dirname(script_path))

            while True:
                line = process.stdout.readline()
                if line == '' and process.poll() is not None:
                    break
                if line:
                    line_stripped = line.strip()
                    arcpy.AddMessage(line_stripped)
                    # Add user-friendly progress messages
                    if "Calling Census API" in line_stripped:
                        arcpy.AddMessage("Downloading data (this may take a moment for large areas)...")
                    elif "Retrieved" in line_stripped and "records" in line_stripped:
                        arcpy.AddMessage("Processing downloaded data...")
                    elif "Writing output" in line_stripped:
                        arcpy.AddMessage("Creating feature class...")

            stderr = process.stderr.read()
            if stderr:
                for line in stderr.split('\n'):
                    if line.strip():
                        arcpy.AddWarning(line)

            if process.returncode == 0:
                r_success = True
            else:
                arcpy.AddError(f"R script failed with code {process.returncode}")

        except Exception as e:
            arcpy.AddError(f"Error running R script: {str(e)}")

        # If R succeeded and we need to convert to geodatabase, do it now
        if r_success and hasattr(self, '_temp_gpkg') and self._temp_gpkg:
            try:
                final_output = parameters[12].valueAsText
                arcpy.AddMessage(f"Converting to geodatabase feature class...")
                arcpy.AddMessage(f"  From: {self._temp_gpkg}")
                arcpy.AddMessage(f"  To: {final_output}")

                # Copy GeoPackage to geodatabase
                gpkg_layer_name = os.path.splitext(os.path.basename(self._temp_gpkg))[0]
                gpkg_layer_path = f"{self._temp_gpkg}\\{gpkg_layer_name}"
                arcpy.management.CopyFeatures(gpkg_layer_path, final_output)
                arcpy.AddMessage("Conversion successful!")

                # Clean up temp files AFTER successful conversion
                try:
                    if os.path.exists(self._temp_gpkg):
                        os.remove(self._temp_gpkg)
                    temp_dir = os.path.dirname(self._temp_gpkg)
                    if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                        shutil.rmtree(temp_dir, ignore_errors=True)
                    arcpy.AddMessage("Temporary files cleaned up.")
                except Exception as cleanup_error:
                    arcpy.AddWarning(f"Could not clean up temp files: {cleanup_error}")

                arcpy.AddMessage("Download completed successfully!")

            except Exception as e:
                arcpy.AddError(f"Failed to convert to geodatabase: {str(e)}")

                # Clean up temp files after failed conversion
                try:
                    if os.path.exists(self._temp_gpkg):
                        os.remove(self._temp_gpkg)
                    temp_dir = os.path.dirname(self._temp_gpkg)
                    if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                        shutil.rmtree(temp_dir, ignore_errors=True)
                    arcpy.AddMessage("Temporary files cleaned up.")
                except Exception as cleanup_error:
                    arcpy.AddWarning(f"Could not clean up temp files: {cleanup_error}")

        elif r_success:
            # Direct output (shapefile), no conversion needed
            arcpy.AddMessage("Download completed successfully!")

        # If R failed, still try to clean up any temp files
        if not r_success and hasattr(self, '_temp_gpkg') and self._temp_gpkg:
            try:
                temp_dir = os.path.dirname(self._temp_gpkg)
                if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                    shutil.rmtree(temp_dir, ignore_errors=True)
            except Exception:
                pass


class JoinACSData(object):
    def __init__(self):
        self.label = "Join ACS Data to Features"
        self.description = "Join ACS Census data to existing polygon features using spatial interpolation"
        self.canRunInBackground = False
        self.category = "Census Data"

    def _get_lookup_path(self, year=None, survey=None):
        if year and survey:
            filename = f"acs_variable_lookup_{year}_{survey}.json"
        else:
            filename = "acs_variable_lookup.json"
        return os.path.join(os.path.dirname(__file__), "lookups", filename)

    def _load_lookup(self, year=None, survey=None):
        lookup_path = self._get_lookup_path(year, survey)
        try:
            if os.path.exists(lookup_path):
                with open(lookup_path, 'r', encoding='utf-8') as f:
                    return json.load(f)
        except Exception:
            pass
        return None

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Input Feature Class",
            name="input_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Input",
            category="Input")
        p0.description = "The polygon feature class to join Census data to."

        p1 = arcpy.Parameter(
            displayName="Year",
            name="year",
            datatype="GPLong",
            parameterType="Required",
            direction="Input",
            category="Census Data")
        p1.value = 2023
        p1.filter.type = "Range"
        p1.filter.list = [2005, 2040]
        p1.description = "The ACS data year."

        p2 = arcpy.Parameter(
            displayName="Survey",
            name="survey",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Census Data")
        p2.filter.type = "ValueList"
        p2.filter.list = ["acs5", "acs1"]
        p2.value = "acs5"
        p2.description = "ACS 5-Year or 1-Year estimates."

        p3 = arcpy.Parameter(
            displayName="Category",
            name="category",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Census Data")
        p3.filter.type = "ValueList"
        p3.filter.list = []
        p3.description = "Select a category to filter tables."

        p4 = arcpy.Parameter(
            displayName="Table",
            name="table_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Census Data")
        p4.filter.type = "ValueList"
        p4.filter.list = []
        p4.description = "Select a table to see variables."

        p5 = arcpy.Parameter(
            displayName="Variables",
            name="variables_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Census Data",
            multiValue=True)
        p5.filter.type = "ValueList"
        p5.filter.list = []
        p5.description = "Select one or more variables to join."

        p6 = arcpy.Parameter(
            displayName="Or Enter Variable Codes Manually (comma-separated)",
            name="variables_manual",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Census Data")
        p6.description = "Enter variable codes directly (e.g., B19013_001, B01003_001)."

        p7 = arcpy.Parameter(
            displayName="Variable Type",
            name="var_type",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Census Data")
        p7.filter.type = "ValueList"
        p7.filter.list = ["Count (e.g., population, households)", "Rate/Median (e.g., median income, percentages)"]
        p7.value = "Count (e.g., population, households)"
        p7.description = "Counts are summed proportionally. Rates/medians are averaged by area or population weight."

        p8 = arcpy.Parameter(
            displayName="Census Geography Level",
            name="geography",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Census Data")
        p8.filter.type = "ValueList"
        p8.filter.list = ["tract", "block group", "county", "zcta"]
        p8.value = "tract"
        p8.description = "The Census geography level to use as source data. Smaller geographies give more precise results."

        p9 = arcpy.Parameter(
            displayName="State",
            name="state",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Census Data",
            multiValue=True)
        p9.filter.type = "ValueList"
        p9.filter.list = [
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
            "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
            "WI", "WY"]
        p9.description = "Select the state(s) that cover your input features."

        p10 = arcpy.Parameter(
            displayName="Join Method",
            name="join_method",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Join Options")
        p10.filter.type = "ValueList"
        p10.filter.list = ["Area Weighted", "Population Weighted"]
        p10.value = "Area Weighted"
        p10.description = "Area weighted uses geometric overlap. Population weighted is more accurate but slower."

        p11 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Output",
            category="Output")
        p11.description = "Output feature class with Census data joined."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        year_param = parameters[1]
        survey_param = parameters[2]
        cat_param = parameters[3]
        tbl_param = parameters[4]
        var_param = parameters[5]

        year = year_param.value
        survey = survey_param.valueAsText
        lookup = self._load_lookup(year, survey)

        if (year_param.altered and not year_param.hasBeenValidated) or \
           (survey_param.altered and not survey_param.hasBeenValidated):
            if lookup:
                cat_param.filter.list = lookup.get("categories", [])
            else:
                cat_param.filter.list = []
            cat_param.value = None
            tbl_param.filter.list = []
            tbl_param.value = None
            var_param.filter.list = []
            var_param.value = None

        if lookup:
            if cat_param.altered and not cat_param.hasBeenValidated:
                cat = cat_param.valueAsText
                if cat and cat in lookup.get("tables", {}):
                    tables = lookup["tables"][cat]
                    tbl_param.filter.list = sorted([f"{t['code']} - {t['label'][:80]}" for t in tables])
                else:
                    tbl_param.filter.list = []
                tbl_param.value = None
                var_param.value = None
                var_param.filter.list = []

            if tbl_param.altered and not tbl_param.hasBeenValidated:
                tbl_sel = tbl_param.valueAsText
                if tbl_sel:
                    tbl_code = tbl_sel.split(" - ")[0].strip()
                    if tbl_code in lookup.get("variables", {}):
                        vars_list = lookup["variables"][tbl_code]
                        var_param.filter.list = [f"{v['code']} - {v['label'][:70]}" for v in vars_list]
                    else:
                        var_param.filter.list = []
                else:
                    var_param.filter.list = []
                var_param.value = None

    def updateMessages(self, parameters):
        input_param = parameters[0]
        year_param = parameters[1]
        survey_param = parameters[2]
        var_param = parameters[5]
        manual_var_param = parameters[6]
        state_param = parameters[9]

        year = year_param.value
        survey = survey_param.valueAsText

        # Check input is polygon
        if input_param.value:
            desc = arcpy.Describe(input_param.valueAsText)
            if desc.shapeType != "Polygon":
                input_param.setErrorMessage("Input must be a polygon feature class")

        # Check variable selection
        if not var_param.value and not manual_var_param.value:
            var_param.setErrorMessage("Select variables or enter variable codes manually")

        # Check lookup file
        if not self._load_lookup(year, survey):
            if not manual_var_param.value:
                parameters[3].setErrorMessage(
                    f"No variable lookup file for {year} {survey}. Run 'Generate Variable Lookup File' or enter codes manually.")

        # Check state selection
        if not state_param.value:
            state_param.setErrorMessage("Select at least one state that covers your input features")

    def execute(self, parameters, messages):
        
        import time
        
        script_path = os.path.join(os.path.dirname(__file__), "acs_join_data.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        input_fc = parameters[0].valueAsText
        year = parameters[1].value
        survey = parameters[2].valueAsText

        # Get variables
        vars_sel = parameters[5].values or []
        vars_man = parameters[6].valueAsText or ""
        var_codes = [v.split(" - ")[0].strip() for v in vars_sel if v]
        if vars_man:
            var_codes.extend([c.strip() for c in vars_man.split(",")])
        vars_str = ",".join(var_codes)

        var_type_full = parameters[7].valueAsText
        var_type = "count" if "Count" in var_type_full else "rate"

        geography = parameters[8].valueAsText

        state_values = parameters[9].values or []
        state_str = ",".join([str(s) for s in state_values])

        join_method_full = parameters[10].valueAsText
        join_method = "area" if "Area" in join_method_full else "population"

        output_path = parameters[11].valueAsText

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Join ACS Data Parameters:")
        arcpy.AddMessage(f"  Input: {input_fc}")
        arcpy.AddMessage(f"  Year/Survey: {year} {survey}")
        arcpy.AddMessage(f"  Variables: {vars_str}")
        arcpy.AddMessage(f"  Variable Type: {var_type}")
        arcpy.AddMessage(f"  Census Geography: {geography}")
        arcpy.AddMessage(f"  State(s): {state_str}")
        arcpy.AddMessage(f"  Join Method: {join_method}")
        arcpy.AddMessage(f"  Output: {output_path}")
        arcpy.AddMessage("=" * 60)

        # Create temp directory
        temp_dir = tempfile.mkdtemp(prefix="acs_join_")
        
        # Create a temporary file geodatabase for input
        temp_gdb = os.path.join(temp_dir, "temp.gdb")
        arcpy.AddMessage("Creating temporary file geodatabase...")
        arcpy.management.CreateFileGDB(temp_dir, "temp.gdb")
        
        # Export input to temp file geodatabase
        temp_input_fc = os.path.join(temp_gdb, "input_features")
        
        arcpy.AddMessage("Exporting input to temporary geodatabase...")
        
        try:
            arcpy.management.CopyFeatures(input_fc, temp_input_fc)
            arcpy.AddMessage("  Export complete")
        except arcpy.ExecuteError as e:
            arcpy.AddError(f"ArcPy error exporting input features: {arcpy.GetMessages(2)}")
            shutil.rmtree(temp_dir, ignore_errors=True)
            return
        except Exception as e:
            arcpy.AddError(f"Failed to export input features: {str(e)}")
            shutil.rmtree(temp_dir, ignore_errors=True)
            return

        # Verify the feature class was created
        if not arcpy.Exists(temp_input_fc):
            arcpy.AddError(f"Failed to create temporary feature class: {temp_input_fc}")
            shutil.rmtree(temp_dir, ignore_errors=True)
            return

        count = int(arcpy.management.GetCount(temp_input_fc)[0])
        arcpy.AddMessage(f"  Exported {count:,} features")
        
        # Brief delay to ensure file handles are released
        time.sleep(1)

        # Set up output - R will write to GeoPackage, we convert to GDB after if needed
        temp_output_gpkg = os.path.join(temp_dir, "output.gpkg")
        self._temp_output_gpkg = None
        truncate_fields = "FALSE"  # GeoPackage supports long field names

        if '.gdb' in output_path.lower() or '.sde' in output_path.lower():
            self._temp_output_gpkg = temp_output_gpkg
            arcpy.AddMessage("Geodatabase output detected - R will write to GeoPackage, then convert.")
            out_file = temp_output_gpkg
        else:
            # Direct output to shapefile or gpkg
            out_file = output_path
            if output_path.lower().endswith('.shp'):
                truncate_fields = "TRUE"

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            shutil.rmtree(temp_dir, ignore_errors=True)
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        # Use forward slashes for R compatibility on Windows
        # For file geodatabase, pass the full path to the feature class
        temp_input_r = temp_input_fc.replace("\\", "/")
        out_file_r = out_file.replace("\\", "/")

        cmd = [
            rscript, script_path,
            temp_input_r,
            str(year), survey, vars_str, var_type,
            geography, state_str, join_method,
            out_file_r, truncate_fields
        ]

        arcpy.AddMessage(f"Input path for R: {temp_input_r}")
        arcpy.AddMessage(f"Output path for R: {out_file_r}")

        r_success = False

        try:
            process = subprocess.Popen(
                cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                text=True, cwd=os.path.dirname(script_path))

            while True:
                line = process.stdout.readline()
                if line == '' and process.poll() is not None:
                    break
                if line:
                    arcpy.AddMessage(line.strip())

            stderr = process.stderr.read()
            if stderr:
                for line in stderr.split('\n'):
                    if line.strip():
                        arcpy.AddWarning(line)

            if process.returncode == 0:
                r_success = True
            else:
                arcpy.AddError(f"R script failed with code {process.returncode}")

        except Exception as e:
            arcpy.AddError(f"Error running R script: {str(e)}")

        # Convert GeoPackage output to geodatabase if needed
        if r_success and self._temp_output_gpkg:
            try:
                if not os.path.exists(self._temp_output_gpkg):
                    arcpy.AddError(f"R did not create output file: {self._temp_output_gpkg}")
                else:
                    arcpy.AddMessage("Converting to geodatabase feature class...")
                    # GeoPackage layer name is the filename without extension
                    gpkg_layer_name = "output"
                    gpkg_layer_path = f"{self._temp_output_gpkg}\\{gpkg_layer_name}"
                    arcpy.management.CopyFeatures(gpkg_layer_path, output_path)
                    arcpy.AddMessage("Conversion successful!")
                    arcpy.AddMessage("Join completed successfully!")
            except Exception as e:
                arcpy.AddError(f"Failed to convert to geodatabase: {str(e)}")

        elif r_success:
            arcpy.AddMessage("Join completed successfully!")

        # Cleanup temp files
        try:
            # Need to release any locks on the file geodatabase
            arcpy.management.ClearWorkspaceCache()
            time.sleep(1)
            if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                shutil.rmtree(temp_dir, ignore_errors=True)
        except Exception:
            pass

class CompareTimePeriods(object):
    def __init__(self):
        self.label = "Compare Time Periods"
        self.description = "Compare a single ACS variable across two time periods with statistical significance testing"
        self.canRunInBackground = False
        self.category = "Census Data"

    def _get_lookup_path(self, year=None, survey=None):
        if year and survey:
            filename = f"acs_variable_lookup_{year}_{survey}.json"
        else:
            filename = "acs_variable_lookup.json"
        return os.path.join(os.path.dirname(__file__), "lookups", filename)

    def _load_lookup(self, year=None, survey=None):
        lookup_path = self._get_lookup_path(year, survey)
        try:
            if os.path.exists(lookup_path):
                with open(lookup_path, 'r', encoding='utf-8') as f:
                    return json.load(f)
        except Exception:
            pass
        return None

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Year 1 (Earlier)",
            name="year1",
            datatype="GPLong",
            parameterType="Required",
            direction="Input",
            category="Connection")
        p0.value = 2018
        p0.filter.type = "Range"
        p0.filter.list = [2005, 2040]
        p0.description = "The earlier year for comparison."

        p1 = arcpy.Parameter(
            displayName="Year 2 (Later)",
            name="year2",
            datatype="GPLong",
            parameterType="Required",
            direction="Input",
            category="Connection")
        p1.value = 2023
        p1.filter.type = "Range"
        p1.filter.list = [2005, 2040]
        p1.description = "The later year for comparison."

        p2 = arcpy.Parameter(
            displayName="Survey",
            name="survey",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Connection")
        p2.filter.type = "ValueList"
        p2.filter.list = ["acs5", "acs1"]
        p2.value = "acs5"
        p2.description = "ACS 5-Year requires at least 5 years between comparisons to avoid data overlap."

        p3 = arcpy.Parameter(
            displayName="Category",
            name="category",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable")
        p3.filter.type = "ValueList"
        p3.filter.list = []
        p3.description = "Select a category to filter the available tables."

        p4 = arcpy.Parameter(
            displayName="Table",
            name="table_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable")
        p4.filter.type = "ValueList"
        p4.filter.list = []
        p4.description = "Select a table to see its available variables."

        p5 = arcpy.Parameter(
            displayName="Variable",
            name="variable_select",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable")
        p5.filter.type = "ValueList"
        p5.filter.list = []
        p5.description = "Select a single variable to compare across time periods."

        p6 = arcpy.Parameter(
            displayName="Or Enter Variable Code Manually",
            name="variable_manual",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Variable")
        p6.description = "Enter a single variable code directly (e.g., B19013_001)."

        p7 = arcpy.Parameter(
            displayName="Variable Type",
            name="var_type",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Variable")
        p7.filter.type = "ValueList"
        p7.filter.list = ["Count (e.g., population, households)", "Rate/Median (e.g., median income, percentages)"]
        p7.value = "Count (e.g., population, households)"
        p7.description = "Specify whether the variable is a count (summed during interpolation) or a rate/median (averaged during interpolation). This affects how cross-decade boundary changes are handled."

        p8 = arcpy.Parameter(
            displayName="Geography Level",
            name="geography",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            category="Geography")
        p8.filter.type = "ValueList"
        p8.filter.list = [
            "state", "county", "tract", "block group", "place", "zcta",
            "congressional district", "state legislative district (upper chamber)",
            "state legislative district (lower chamber)", "county subdivision",
            "school district (unified)"]
        p8.value = "tract"
        p8.description = "The geographic level for comparison. Cross-decade comparisons for tract and block group will use population-weighted interpolation."

        p9 = arcpy.Parameter(
            displayName="State",
            name="state",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Geography",
            multiValue=True)
        p9.filter.type = "ValueList"
        p9.filter.list = [
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
            "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
            "WI", "WY"]
        p9.description = "Select one or more states."

        p10 = arcpy.Parameter(
            displayName="County (optional - comma separated for multiple counties)",
            name="county",
            datatype="GPString",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p10.description = "Filter to specific counties (comma-separated). Leave blank for all counties."

        p11 = arcpy.Parameter(
            displayName="MOE Confidence Level",
            name="moe_level",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input",
            category="Geography")
        p11.filter.type = "ValueList"
        p11.filter.list = [90, 95, 99]
        p11.value = 90
        p11.description = "Confidence level for statistical significance testing of change."

        p12 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Output",
            category="Geography")
        p12.description = "Output location for the comparison feature class."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        year2_param = parameters[1]
        survey_param = parameters[2]
        cat_param = parameters[3]
        tbl_param = parameters[4]
        var_param = parameters[5]
        state_param = parameters[9]
        county_param = parameters[10]

        year = year2_param.value
        survey = survey_param.valueAsText
        lookup = self._load_lookup(year, survey)

        if (year2_param.altered and not year2_param.hasBeenValidated) or \
           (survey_param.altered and not survey_param.hasBeenValidated):
            if lookup:
                cat_param.filter.list = lookup.get("categories", [])
            else:
                cat_param.filter.list = []
            cat_param.value = None
            tbl_param.filter.list = []
            tbl_param.value = None
            var_param.filter.list = []
            var_param.value = None

        if lookup:
            if cat_param.altered and not cat_param.hasBeenValidated:
                cat = cat_param.valueAsText
                if cat and cat in lookup.get("tables", {}):
                    tables = lookup["tables"][cat]
                    tbl_param.filter.list = sorted([f"{t['code']} - {t['label'][:80]}" for t in tables])
                else:
                    tbl_param.filter.list = []
                tbl_param.value = None
                var_param.value = None
                var_param.filter.list = []

            if tbl_param.altered and not tbl_param.hasBeenValidated:
                tbl_sel = tbl_param.valueAsText
                if tbl_sel:
                    tbl_code = tbl_sel.split(" - ")[0].strip()
                    if tbl_code in lookup.get("variables", {}):
                        vars_list = lookup["variables"][tbl_code]
                        var_param.filter.list = [f"{v['code']} - {v['label'][:70]}" for v in vars_list]
                    else:
                        var_param.filter.list = []
                else:
                    var_param.filter.list = []
                var_param.value = None

        state_values = state_param.values or []
        if len(state_values) > 1:
            county_param.enabled = False
            if state_param.altered and not state_param.hasBeenValidated:
                county_param.value = None
        else:
            county_param.enabled = True

    def updateMessages(self, parameters):
        year1_param = parameters[0]
        year2_param = parameters[1]
        survey_param = parameters[2]
        var_param = parameters[5]
        manual_var_param = parameters[6]
        state_param = parameters[9]
        geo_param = parameters[8]

        year1 = year1_param.value
        year2 = year2_param.value
        survey = survey_param.valueAsText

        if year1 and year2 and year1 >= year2:
            year1_param.setErrorMessage("Year 1 must be earlier than Year 2")

        if survey == "acs5" and year1 and year2:
            if year2 - year1 < 5:
                year1_param.setErrorMessage(
                    f"ACS 5-Year estimates require at least 5 years between comparisons to avoid data overlap. "
                    f"Selected years are only {year2 - year1} years apart.")

        if not var_param.value and not manual_var_param.value:
            var_param.setErrorMessage("Select a variable or enter a variable code manually")

        if not self._load_lookup(year2, survey):
            parameters[3].setErrorMessage(
                f"No variable lookup file for {year2} {survey}. Run 'Generate Variable Lookup File'.")

        if not state_param.value and geo_param.valueAsText not in ["state", "zcta"]:
            state_param.setErrorMessage("State is required for this geography level")

        if year1 and year2:
            decade1 = (year1 // 10) * 10
            decade2 = (year2 // 10) * 10
            if decade1 != decade2 and geo_param.valueAsText in ["tract", "block group"]:
                geo_param.setWarningMessage(
                    f"Cross-decade comparison ({decade1}s to {decade2}s) detected. "
                    f"Population-weighted interpolation will be used to align boundaries.")

    def execute(self, parameters, messages):
        script_path = os.path.join(os.path.dirname(__file__), "acs_compare_time.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        year1 = parameters[0].value
        year2 = parameters[1].value
        survey = parameters[2].valueAsText

        var_sel = parameters[5].valueAsText
        var_man = parameters[6].valueAsText
        if var_sel:
            variable = var_sel.split(" - ")[0].strip()
        elif var_man:
            variable = var_man.strip()
        else:
            arcpy.AddError("No variable specified")
            return

        var_type_full = parameters[7].valueAsText
        var_type = "count" if "Count" in var_type_full else "rate"

        geography = parameters[8].valueAsText
        
        state_values = parameters[9].values or []
        state_str = ",".join([str(s) for s in state_values]) if state_values else ""
        
        county = parameters[10].valueAsText or ""
        moe_level = parameters[11].value or 90
        output_path = parameters[12].valueAsText

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Compare Time Periods Parameters:")
        arcpy.AddMessage(f"  Year 1: {year1}")
        arcpy.AddMessage(f"  Year 2: {year2}")
        arcpy.AddMessage(f"  Survey: {survey}")
        arcpy.AddMessage(f"  Variable: {variable}")
        arcpy.AddMessage(f"  Variable Type: {var_type}")
        arcpy.AddMessage(f"  Geography: {geography}")
        arcpy.AddMessage(f"  State: {state_str or '(all)'}")
        arcpy.AddMessage(f"  County: {county or '(all)'}")
        arcpy.AddMessage(f"  Output: {output_path}")
        arcpy.AddMessage("=" * 60)

        self._temp_gpkg = None
        truncate_fields = "TRUE"

        if '.gdb' in output_path.lower() or '.sde' in output_path.lower():
            temp_dir = tempfile.mkdtemp(prefix="acs_compare_")
            fc_name = os.path.basename(output_path)
            self._temp_gpkg = os.path.join(temp_dir, f"{fc_name}.gpkg")
            truncate_fields = "FALSE"
            arcpy.AddMessage("Geodatabase output detected.")
            arcpy.AddMessage(f"Writing to temporary GeoPackage first: {self._temp_gpkg}")
            out_file = self._temp_gpkg
        else:
            out_file = output_path

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        cmd = [
            rscript, script_path,
            str(year1), str(year2), survey, variable, var_type,
            geography, state_str, county, str(moe_level),
            out_file, truncate_fields
        ]

        r_success = False

        try:
            process = subprocess.Popen(
                cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                text=True, cwd=os.path.dirname(script_path))

            while True:
                line = process.stdout.readline()
                if line == '' and process.poll() is not None:
                    break
                if line:
                    arcpy.AddMessage(line.strip())

            stderr = process.stderr.read()
            if stderr:
                for line in stderr.split('\n'):
                    if line.strip():
                        arcpy.AddWarning(line)

            if process.returncode == 0:
                r_success = True
            else:
                arcpy.AddError(f"R script failed with code {process.returncode}")

        except Exception as e:
            arcpy.AddError(f"Error running R script: {str(e)}")

        if r_success and self._temp_gpkg:
            try:
                arcpy.AddMessage("Converting to geodatabase feature class...")
                gpkg_layer_name = os.path.splitext(os.path.basename(self._temp_gpkg))[0]
                gpkg_layer_path = f"{self._temp_gpkg}\\{gpkg_layer_name}"
                arcpy.management.CopyFeatures(gpkg_layer_path, output_path)
                arcpy.AddMessage("Conversion successful!")

                try:
                    if os.path.exists(self._temp_gpkg):
                        os.remove(self._temp_gpkg)
                    temp_dir = os.path.dirname(self._temp_gpkg)
                    if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                        shutil.rmtree(temp_dir, ignore_errors=True)
                except Exception as cleanup_error:
                    arcpy.AddWarning(f"Could not clean up temp files: {cleanup_error}")

                arcpy.AddMessage("Comparison completed successfully!")

            except Exception as e:
                arcpy.AddError(f"Failed to convert to geodatabase: {str(e)}")
                try:
                    temp_dir = os.path.dirname(self._temp_gpkg)
                    if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                        shutil.rmtree(temp_dir, ignore_errors=True)
                except Exception:
                    pass

        elif r_success:
            arcpy.AddMessage("Comparison completed successfully!")

        if not r_success and self._temp_gpkg:
            try:
                temp_dir = os.path.dirname(self._temp_gpkg)
                if os.path.exists(temp_dir) and temp_dir.startswith(tempfile.gettempdir()):
                    shutil.rmtree(temp_dir, ignore_errors=True)
            except Exception:
                pass

class NormalizeData(object):
    def __init__(self):
        self.label = "Normalize Data"
        self.description = "Calculate per capita, percentage, or density fields from raw ACS counts with proper MOE propagation"
        self.canRunInBackground = False
        self.category = "Data Enrichment"

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Input Feature Class",
            name="input_fc",
            datatype="GPFeatureLayer",
            parameterType="Required",
            direction="Input")
        p0.description = "Feature class containing ACS data with estimate and MOE fields."

        p1 = arcpy.Parameter(
            displayName="Numerator Field (Count to Normalize)",
            name="numerator_field",
            datatype="Field",
            parameterType="Required",
            direction="Input")
        p1.parameterDependencies = [p0.name]
        p1.filter.list = ["Short", "Long", "Float", "Double"]
        p1.description = "The estimate field to normalize (e.g., population with bachelor's degree)."

        p2 = arcpy.Parameter(
            displayName="Denominator Field (Base/Universe)",
            name="denominator_field",
            datatype="Field",
            parameterType="Required",
            direction="Input")
        p2.parameterDependencies = [p0.name]
        p2.filter.list = ["Short", "Long", "Float", "Double"]
        p2.description = "The base field to normalize by (e.g., total population 25+, total households)."

        p3 = arcpy.Parameter(
            displayName="Normalization Type",
            name="norm_type",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p3.filter.type = "ValueList"
        p3.filter.list = [
            "Percentage (x 100)",
            "Per 1,000",
            "Per 10,000",
            "Per 100,000",
            "Ratio (no multiplier)"
        ]
        p3.value = "Percentage (x 100)"
        p3.description = "How to express the normalized value."

        p4 = arcpy.Parameter(
            displayName="Output Field Name",
            name="output_field",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p4.description = "Name for the new normalized field. MOE field will be named with '_MOE' suffix."

        p5 = arcpy.Parameter(
            displayName="Numerator is Subset of Denominator",
            name="is_proportion",
            datatype="GPBoolean",
            parameterType="Required",
            direction="Input")
        p5.value = True
        p5.description = "Check if numerator is a subset of denominator (e.g., 'pop with degree' is subset of 'total pop'). This affects MOE calculation."

        p6 = arcpy.Parameter(
            displayName="Calculate Margin of Error",
            name="calc_moe",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input")
        p6.value = True
        p6.description = "Calculate the margin of error for the normalized value using Census Bureau formulas."

        p7 = arcpy.Parameter(
            displayName="Numerator MOE Field",
            name="numerator_moe",
            datatype="Field",
            parameterType="Optional",
            direction="Input")
        p7.parameterDependencies = [p0.name]
        p7.filter.list = ["Short", "Long", "Float", "Double"]
        p7.description = "MOE field for the numerator. If not specified, will look for field with 'M' suffix."

        p8 = arcpy.Parameter(
            displayName="Denominator MOE Field",
            name="denominator_moe",
            datatype="Field",
            parameterType="Optional",
            direction="Input")
        p8.parameterDependencies = [p0.name]
        p8.filter.list = ["Short", "Long", "Float", "Double"]
        p8.description = "MOE field for the denominator. If not specified, will look for field with 'M' suffix."

        p9 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Optional",
            direction="Output")
        p9.description = "Optional: Create a new feature class. If not specified, fields are added to input."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        input_fc = parameters[0]
        numerator_field = parameters[1]
        calc_moe = parameters[6].value
        num_moe = parameters[7]
        denom_moe = parameters[8]

        # Enable/disable MOE fields based on checkbox
        num_moe.enabled = calc_moe
        denom_moe.enabled = calc_moe

        # Auto-detect MOE fields if numerator is selected
        if numerator_field.altered and not numerator_field.hasBeenValidated:
            if numerator_field.valueAsText and input_fc.value:
                num_field = numerator_field.valueAsText
                try:
                    fields = [f.name for f in arcpy.ListFields(input_fc.valueAsText)]
                    moe_field = None
                    
                    # Strategy 1: Field ends in 'E', look for matching 'M' field (raw Census names)
                    if num_field.endswith('E'):
                        candidate = num_field[:-1] + 'M'
                        if candidate in fields:
                            moe_field = candidate
                    
                    # Strategy 2: Look for field + '_MOE' (cleaned names)
                    if not moe_field:
                        candidate = num_field + '_MOE'
                        if candidate in fields:
                            moe_field = candidate
                    
                    # Strategy 3: If field doesn't end in _MOE, check if removing _MOE suffix exists
                    # (in case user selected the MOE field by accident)
                    
                    if moe_field:
                        num_moe.value = moe_field
                except:
                    pass

        # Auto-detect denominator MOE field
        denom_field = parameters[2]
        if denom_field.altered and not denom_field.hasBeenValidated:
            if denom_field.valueAsText and input_fc.value:
                d_field = denom_field.valueAsText
                try:
                    fields = [f.name for f in arcpy.ListFields(input_fc.valueAsText)]
                    moe_field = None
                    
                    # Strategy 1: Field ends in 'E', look for matching 'M' field (raw Census names)
                    if d_field.endswith('E'):
                        candidate = d_field[:-1] + 'M'
                        if candidate in fields:
                            moe_field = candidate
                    
                    # Strategy 2: Look for field + '_MOE' (cleaned names)
                    if not moe_field:
                        candidate = d_field + '_MOE'
                        if candidate in fields:
                            moe_field = candidate
                    
                    if moe_field:
                        denom_moe.value = moe_field
                except:
                    pass

    def updateMessages(self, parameters):
        output_field = parameters[4]
        input_fc = parameters[0]
        calc_moe = parameters[6].value
        num_moe = parameters[7]
        denom_moe = parameters[8]

        # Check output field name validity
        if output_field.value:
            field_name = output_field.valueAsText
            if len(field_name) > 64:
                output_field.setErrorMessage("Field name cannot exceed 64 characters")
            elif not field_name[0].isalpha():
                output_field.setErrorMessage("Field name must start with a letter")
            elif not all(c.isalnum() or c == '_' for c in field_name):
                output_field.setErrorMessage("Field name can only contain letters, numbers, and underscores")

        # Check MOE fields if MOE calculation requested
        if calc_moe:
            if not num_moe.value:
                num_moe.setWarningMessage("No numerator MOE field specified. MOE will not be calculated.")
            if not denom_moe.value:
                denom_moe.setWarningMessage("No denominator MOE field specified. MOE will not be calculated.")

    def execute(self, parameters, messages):
        import math

        input_fc = parameters[0].valueAsText
        numerator_field = parameters[1].valueAsText
        denominator_field = parameters[2].valueAsText
        norm_type = parameters[3].valueAsText
        output_field = parameters[4].valueAsText
        is_proportion = parameters[5].value
        calc_moe = parameters[6].value
        numerator_moe = parameters[7].valueAsText
        denominator_moe = parameters[8].valueAsText
        output_fc = parameters[9].valueAsText

        # Determine multiplier
        multipliers = {
            "Percentage (x 100)": 100,
            "Per 1,000": 1000,
            "Per 10,000": 10000,
            "Per 100,000": 100000,
            "Ratio (no multiplier)": 1
        }
        multiplier = multipliers.get(norm_type, 100)

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Normalize Data")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage(f"  Numerator: {numerator_field}")
        arcpy.AddMessage(f"  Denominator: {denominator_field}")
        arcpy.AddMessage(f"  Type: {norm_type} (multiplier: {multiplier})")
        arcpy.AddMessage(f"  Output field: {output_field}")
        arcpy.AddMessage(f"  Proportion (subset): {is_proportion}")
        arcpy.AddMessage(f"  Calculate MOE: {calc_moe}")
        arcpy.AddMessage("=" * 60)

        # If output FC specified, copy first
        if output_fc:
            arcpy.AddMessage("Creating output feature class...")
            arcpy.management.CopyFeatures(input_fc, output_fc)
            work_fc = output_fc
        else:
            work_fc = input_fc

        # Add output field(s)
        arcpy.AddMessage(f"Adding field: {output_field}")
        
        # Check if field exists
        existing_fields = [f.name for f in arcpy.ListFields(work_fc)]
        if output_field in existing_fields:
            arcpy.AddWarning(f"Field {output_field} already exists - values will be overwritten")
        else:
            arcpy.management.AddField(work_fc, output_field, "DOUBLE")

        moe_output_field = None
        if calc_moe and numerator_moe and denominator_moe:
            moe_output_field = output_field + "_MOE"
            if moe_output_field in existing_fields:
                arcpy.AddWarning(f"Field {moe_output_field} already exists - values will be overwritten")
            else:
                arcpy.management.AddField(work_fc, moe_output_field, "DOUBLE")
            arcpy.AddMessage(f"Adding field: {moe_output_field}")

        # Build field list for cursor
        fields = [numerator_field, denominator_field, output_field]
        if moe_output_field:
            fields.extend([numerator_moe, denominator_moe, moe_output_field])

        # Process records
        arcpy.AddMessage("Calculating normalized values...")
        
        record_count = 0
        null_count = 0
        zero_denom_count = 0
        moe_calc_errors = 0

        with arcpy.da.UpdateCursor(work_fc, fields) as cursor:
            for row in cursor:
                record_count += 1
                
                numerator = row[0]
                denominator = row[1]

                # Handle nulls and zero denominators
                if numerator is None or denominator is None:
                    row[2] = None
                    if moe_output_field:
                        row[5] = None
                    null_count += 1
                    cursor.updateRow(row)
                    continue

                if denominator == 0:
                    row[2] = None
                    if moe_output_field:
                        row[5] = None
                    zero_denom_count += 1
                    cursor.updateRow(row)
                    continue

                # Calculate normalized value
                ratio = numerator / denominator
                normalized_value = ratio * multiplier
                row[2] = normalized_value

                # Calculate MOE if requested
                if moe_output_field:
                    num_moe_val = row[3]
                    denom_moe_val = row[4]

                    if num_moe_val is None or denom_moe_val is None:
                        row[5] = None
                    else:
                        try:
                            if is_proportion:
                                # Proportion formula: sqrt(MOE_num^2 - (proportion^2 * MOE_denom^2)) / denominator
                                # If the value under sqrt is negative, use ratio formula instead
                                under_sqrt = (num_moe_val ** 2) - ((ratio ** 2) * (denom_moe_val ** 2))
                                
                                if under_sqrt >= 0:
                                    moe_ratio = math.sqrt(under_sqrt) / denominator
                                else:
                                    # Fall back to ratio formula
                                    moe_ratio = math.sqrt((num_moe_val ** 2) + ((ratio ** 2) * (denom_moe_val ** 2))) / denominator
                            else:
                                # Ratio formula: sqrt(MOE_num^2 + (ratio^2 * MOE_denom^2)) / denominator
                                moe_ratio = math.sqrt((num_moe_val ** 2) + ((ratio ** 2) * (denom_moe_val ** 2))) / denominator

                            row[5] = moe_ratio * multiplier
                        except Exception as e:
                            row[5] = None
                            moe_calc_errors += 1

                cursor.updateRow(row)

                # Progress reporting
                if record_count % 50000 == 0:
                    arcpy.AddMessage(f"  Processed {record_count:,} records...")

        # Summary
        arcpy.AddMessage("")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Summary:")
        arcpy.AddMessage(f"  Total records: {record_count:,}")
        arcpy.AddMessage(f"  Calculated values: {record_count - null_count - zero_denom_count:,}")
        if null_count > 0:
            arcpy.AddMessage(f"  Null values skipped: {null_count:,}")
        if zero_denom_count > 0:
            arcpy.AddMessage(f"  Zero denominators skipped: {zero_denom_count:,}")
        if moe_calc_errors > 0:
            arcpy.AddWarning(f"  MOE calculation errors: {moe_calc_errors:,}")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Normalization complete!")

class MOEReliabilityFilter(object):
    def __init__(self):
        self.label = "MOE Reliability Filter"
        self.description = "Calculate Coefficient of Variation and flag or filter records based on statistical reliability"
        self.canRunInBackground = False
        self.category = "Data Enrichment"

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Input Feature Class",
            name="input_fc",
            datatype="GPFeatureLayer",
            parameterType="Required",
            direction="Input")
        p0.description = "Feature class containing ACS data with estimate and MOE fields."

        p1 = arcpy.Parameter(
            displayName="Estimate Field",
            name="estimate_field",
            datatype="Field",
            parameterType="Required",
            direction="Input")
        p1.parameterDependencies = [p0.name]
        p1.filter.list = ["Short", "Long", "Float", "Double"]
        p1.description = "The estimate field to evaluate reliability for."

        p2 = arcpy.Parameter(
            displayName="MOE Field",
            name="moe_field",
            datatype="Field",
            parameterType="Optional",
            direction="Input")
        p2.parameterDependencies = [p0.name]
        p2.filter.list = ["Short", "Long", "Float", "Double"]
        p2.description = "The margin of error field. If not specified, will auto-detect based on estimate field name."

        p3 = arcpy.Parameter(
            displayName="Confidence Level of MOE",
            name="confidence_level",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p3.filter.type = "ValueList"
        p3.filter.list = ["90%", "95%", "99%"]
        p3.value = "90%"
        p3.description = "The confidence level used when downloading the data. Default ACS MOE is 90%."

        p4 = arcpy.Parameter(
            displayName="High Reliability Threshold (CV %)",
            name="high_threshold",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input")
        p4.value = 12.0
        p4.description = "CV below this value is considered high reliability. Census Bureau recommends 12%."

        p5 = arcpy.Parameter(
            displayName="Low Reliability Threshold (CV %)",
            name="low_threshold",
            datatype="GPDouble",
            parameterType="Required",
            direction="Input")
        p5.value = 40.0
        p5.description = "CV above this value is considered low reliability. Census Bureau recommends 40%."

        p6 = arcpy.Parameter(
            displayName="Action for Unreliable Records",
            name="action",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p6.filter.type = "ValueList"
        p6.filter.list = [
            "Flag only (add reliability fields)",
            "Set estimate to NULL if Low reliability",
            "Remove records with Low reliability"
        ]
        p6.value = "Flag only (add reliability fields)"
        p6.description = "How to handle records that fall below reliability thresholds."

        p7 = arcpy.Parameter(
            displayName="CV Output Field Name",
            name="cv_field",
            datatype="GPString",
            parameterType="Optional",
            direction="Input")
        p7.value = "CV"
        p7.description = "Name for the Coefficient of Variation field. Leave blank to skip."

        p8 = arcpy.Parameter(
            displayName="Reliability Output Field Name",
            name="reliability_field",
            datatype="GPString",
            parameterType="Optional",
            direction="Input")
        p8.value = "Reliability"
        p8.description = "Name for the reliability category field (High/Medium/Low). Leave blank to skip."

        p9 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Optional",
            direction="Output")
        p9.description = "Optional: Create a new feature class. If not specified, fields are added to input."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        input_fc = parameters[0]
        estimate_field = parameters[1]
        moe_field = parameters[2]

        # Auto-detect MOE field when estimate field is selected
        if estimate_field.altered and not estimate_field.hasBeenValidated:
            if estimate_field.valueAsText and input_fc.value:
                est_field = estimate_field.valueAsText
                try:
                    fields = [f.name for f in arcpy.ListFields(input_fc.valueAsText)]
                    detected_moe = None

                    # Strategy 1: Field ends in 'E', look for matching 'M' field (raw Census names)
                    if est_field.endswith('E'):
                        candidate = est_field[:-1] + 'M'
                        if candidate in fields:
                            detected_moe = candidate

                    # Strategy 2: Look for field + '_MOE' (cleaned names)
                    if not detected_moe:
                        candidate = est_field + '_MOE'
                        if candidate in fields:
                            detected_moe = candidate

                    if detected_moe:
                        moe_field.value = detected_moe
                except:
                    pass

    def updateMessages(self, parameters):
        high_thresh = parameters[4]
        low_thresh = parameters[5]
        cv_field = parameters[7]
        rel_field = parameters[8]

        # Validate thresholds
        if high_thresh.value and low_thresh.value:
            if high_thresh.value >= low_thresh.value:
                high_thresh.setErrorMessage("High threshold must be less than low threshold")

        # Validate field names
        for param in [cv_field, rel_field]:
            if param.value:
                field_name = param.valueAsText
                if len(field_name) > 64:
                    param.setErrorMessage("Field name cannot exceed 64 characters")
                elif not field_name[0].isalpha():
                    param.setErrorMessage("Field name must start with a letter")
                elif not all(c.isalnum() or c == '_' for c in field_name):
                    param.setErrorMessage("Field name can only contain letters, numbers, and underscores")

        # Warn if no output fields specified
        if not cv_field.value and not rel_field.value:
            cv_field.setWarningMessage("No output fields specified - no reliability information will be added")

    def execute(self, parameters, messages):
        input_fc = parameters[0].valueAsText
        estimate_field = parameters[1].valueAsText
        moe_field = parameters[2].valueAsText
        confidence_level = parameters[3].valueAsText
        high_threshold = parameters[4].value
        low_threshold = parameters[5].value
        action = parameters[6].valueAsText
        cv_field = parameters[7].valueAsText
        reliability_field = parameters[8].valueAsText
        output_fc = parameters[9].valueAsText

        # Z-scores for different confidence levels
        z_scores = {
            "90%": 1.645,
            "95%": 1.96,
            "99%": 2.576
        }
        z_score = z_scores.get(confidence_level, 1.645)

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("MOE Reliability Filter")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage(f"  Estimate field: {estimate_field}")
        arcpy.AddMessage(f"  MOE field: {moe_field}")
        arcpy.AddMessage(f"  Confidence level: {confidence_level} (z = {z_score})")
        arcpy.AddMessage(f"  High reliability: CV < {high_threshold}%")
        arcpy.AddMessage(f"  Low reliability: CV > {low_threshold}%")
        arcpy.AddMessage(f"  Action: {action}")
        arcpy.AddMessage("=" * 60)

        # Validate MOE field
        if not moe_field:
            arcpy.AddError("MOE field is required. Could not auto-detect from estimate field name.")
            return

        # If output FC specified, copy first
        if output_fc:
            arcpy.AddMessage("Creating output feature class...")
            arcpy.management.CopyFeatures(input_fc, output_fc)
            work_fc = output_fc
        else:
            work_fc = input_fc

        # Get existing fields
        existing_fields = [f.name for f in arcpy.ListFields(work_fc)]

        # Add CV field if requested
        if cv_field:
            if cv_field in existing_fields:
                arcpy.AddWarning(f"Field {cv_field} already exists - values will be overwritten")
            else:
                arcpy.management.AddField(work_fc, cv_field, "DOUBLE")
                arcpy.AddMessage(f"Added field: {cv_field}")

        # Add reliability field if requested
        if reliability_field:
            if reliability_field in existing_fields:
                arcpy.AddWarning(f"Field {reliability_field} already exists - values will be overwritten")
            else:
                arcpy.management.AddField(work_fc, reliability_field, "TEXT", field_length=10)
                arcpy.AddMessage(f"Added field: {reliability_field}")

        # Build field list for cursor
        fields = [estimate_field, moe_field]
        cv_idx = None
        rel_idx = None

        if cv_field:
            fields.append(cv_field)
            cv_idx = len(fields) - 1
        if reliability_field:
            fields.append(reliability_field)
            rel_idx = len(fields) - 1

        # Track statistics
        total_count = 0
        high_count = 0
        medium_count = 0
        low_count = 0
        null_count = 0
        zero_est_count = 0
        records_to_delete = []

        # Determine if we need to track OIDs for deletion
        delete_unreliable = "Remove records" in action
        null_unreliable = "Set estimate to NULL" in action

        if delete_unreliable:
            fields.append("OID@")
            oid_idx = len(fields) - 1

        # Process records
        arcpy.AddMessage("Calculating reliability metrics...")

        with arcpy.da.UpdateCursor(work_fc, fields) as cursor:
            for row in cursor:
                total_count += 1
                estimate = row[0]
                moe = row[1]

                # Handle nulls
                if estimate is None or moe is None:
                    if cv_idx is not None:
                        row[cv_idx] = None
                    if rel_idx is not None:
                        row[rel_idx] = None
                    null_count += 1
                    cursor.updateRow(row)
                    continue

                # Handle zero estimates
                if estimate == 0:
                    if cv_idx is not None:
                        row[cv_idx] = None
                    if rel_idx is not None:
                        # Zero estimate with zero MOE is actually reliable (true zero)
                        # Zero estimate with non-zero MOE is unreliable
                        if moe == 0:
                            row[rel_idx] = "High"
                            high_count += 1
                        else:
                            row[rel_idx] = "Low"
                            low_count += 1
                            if delete_unreliable:
                                records_to_delete.append(row[oid_idx])
                    else:
                        zero_est_count += 1
                    cursor.updateRow(row)
                    continue

                # Calculate CV
                # CV = (MOE / z-score) / Estimate * 100
                standard_error = moe / z_score
                cv = (standard_error / abs(estimate)) * 100

                # Determine reliability category
                if cv < high_threshold:
                    reliability = "High"
                    high_count += 1
                elif cv <= low_threshold:
                    reliability = "Medium"
                    medium_count += 1
                else:
                    reliability = "Low"
                    low_count += 1

                # Update fields
                if cv_idx is not None:
                    row[cv_idx] = round(cv, 2)
                if rel_idx is not None:
                    row[rel_idx] = reliability

                # Handle unreliable records based on action
                if reliability == "Low":
                    if null_unreliable:
                        row[0] = None  # Set estimate to NULL
                    elif delete_unreliable:
                        records_to_delete.append(row[oid_idx])

                cursor.updateRow(row)

                # Progress reporting
                if total_count % 50000 == 0:
                    arcpy.AddMessage(f"  Processed {total_count:,} records...")

        # Delete unreliable records if requested
        if delete_unreliable and records_to_delete:
            arcpy.AddMessage(f"Removing {len(records_to_delete):,} low reliability records...")
            oid_field = arcpy.Describe(work_fc).OIDFieldName
            # Process in chunks to avoid SQL length limits
            chunk_size = 1000
            for i in range(0, len(records_to_delete), chunk_size):
                chunk = records_to_delete[i:i + chunk_size]
                where_clause = f"{oid_field} IN ({','.join(map(str, chunk))})"
                with arcpy.da.UpdateCursor(work_fc, ["OID@"], where_clause) as del_cursor:
                    for del_row in del_cursor:
                        del_cursor.deleteRow()

        # Calculate percentages
        valid_count = total_count - null_count - zero_est_count
        if valid_count > 0:
            high_pct = high_count / valid_count * 100
            medium_pct = medium_count / valid_count * 100
            low_pct = low_count / valid_count * 100
        else:
            high_pct = medium_pct = low_pct = 0

        # Summary
        arcpy.AddMessage("")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Reliability Summary")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage(f"  Total records: {total_count:,}")
        if null_count > 0:
            arcpy.AddMessage(f"  Null values: {null_count:,}")
        if zero_est_count > 0:
            arcpy.AddMessage(f"  Zero estimates: {zero_est_count:,}")
        arcpy.AddMessage("")
        arcpy.AddMessage(f"  High reliability (CV < {high_threshold}%):   {high_count:,} ({high_pct:.1f}%)")
        arcpy.AddMessage(f"  Medium reliability (CV {high_threshold}-{low_threshold}%): {medium_count:,} ({medium_pct:.1f}%)")
        arcpy.AddMessage(f"  Low reliability (CV > {low_threshold}%):  {low_count:,} ({low_pct:.1f}%)")
        arcpy.AddMessage("")

        if delete_unreliable and records_to_delete:
            arcpy.AddMessage(f"  Records removed: {len(records_to_delete):,}")
            arcpy.AddMessage(f"  Records remaining: {total_count - len(records_to_delete):,}")
        elif null_unreliable:
            arcpy.AddMessage(f"  Estimates set to NULL: {low_count:,}")

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Reliability analysis complete!")

class GenerateVariableLookup(object):
    def __init__(self):
        self.label = "Generate Variable Lookup File"
        self.description = "Generate the JSON file for the variable browser"
        self.canRunInBackground = False
        self.category = "Configuration"

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="ACS Year",
            name="year",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")
        p0.value = 2022
        p0.filter.type = "Range"
        p0.filter.list = [2010, 2023]

        p1 = arcpy.Parameter(
            displayName="Survey",
            name="survey",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p1.filter.type = "ValueList"
        p1.filter.list = ["acs5", "acs1"]
        p1.value = "acs5"

        p2 = arcpy.Parameter(
            displayName="Output JSON File (optional)",
            name="output_file",
            datatype="DEFile",
            parameterType="Optional",
            direction="Output")
        p2.filter.list = ["json"]

        return [p0, p1, p2]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        pass

    def updateMessages(self, parameters):
        pass

    def execute(self, parameters, messages):
        script_path = os.path.join(os.path.dirname(__file__), "generate_acs_lookup.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        year = parameters[0].value
        survey = parameters[1].valueAsText
        output_file = parameters[2].valueAsText
        if not output_file:
            lookups_dir = os.path.join(os.path.dirname(__file__), "lookups")
            if not os.path.exists(lookups_dir):
                os.makedirs(lookups_dir)
            output_file = os.path.join(
                lookups_dir, 
                f"acs_variable_lookup_{year}_{survey}.json"
            )

        arcpy.AddMessage("=" * 50)
        arcpy.AddMessage("Generating ACS Variable Lookup")
        arcpy.AddMessage(f"  Year: {year}")
        arcpy.AddMessage(f"  Survey: {survey}")
        arcpy.AddMessage(f"  Output: {output_file}")
        arcpy.AddMessage("=" * 50)
        arcpy.AddMessage("")
        arcpy.AddMessage("This may take 1-2 minutes...")

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        cmd = [rscript, script_path, str(year), survey, output_file]
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd=os.path.dirname(script_path))

        while True:
            line = process.stdout.readline()
            if line == '' and process.poll() is not None:
                break
            if line:
                arcpy.AddMessage(line.strip())

        stderr = process.stderr.read()
        if stderr:
            for line in stderr.split('\n'):
                if line.strip():
                    arcpy.AddWarning(line)

        if process.returncode == 0:
            arcpy.AddMessage("")
            arcpy.AddMessage("=" * 50)
            arcpy.AddMessage("Lookup file generated successfully!")
            arcpy.AddMessage(f"File: {output_file}")
            arcpy.AddMessage("=" * 50)
        else:
            arcpy.AddError(f"R script failed with code {process.returncode}")

class DemographicProfileReport(object):
    def __init__(self):
        self.label = "Demographic Profile Report"
        self.description = "Generate a demographic profile comparing a study area to county, state, and national averages"
        self.canRunInBackground = False
        self.category = "Analysis"
        
        # Predefined indicators with variable codes and aggregation methods
        # Format: (Display Name, Variable Code, Aggregation Type, Format Type)
        # Aggregation: "sum" for counts, "median" for medians, "percent" for percentages
        # For percentages, we need (numerator_var, denominator_var)
        self.indicators = {
            "Total Population": {
                "variable": "B01003_001",
                "aggregation": "sum",
                "format": "number"
            },
            "Median Age": {
                "variable": "B01002_001",
                "aggregation": "weighted_median",
                "weight_var": "B01003_001",
                "format": "decimal1"
            },
            "Median Household Income": {
                "variable": "B19013_001",
                "aggregation": "weighted_median",
                "weight_var": "B19001_001",
                "format": "currency"
            },
            "Per Capita Income": {
                "variable": "B19301_001",
                "aggregation": "weighted_median",
                "weight_var": "B01003_001",
                "format": "currency"
            },
            "% Below Poverty Level": {
                "numerator": "B17001_002",
                "denominator": "B17001_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Bachelor's Degree or Higher": {
                "numerator": ["B15003_022", "B15003_023", "B15003_024", "B15003_025"],
                "denominator": "B15003_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Unemployed": {
                "numerator": "B23025_005",
                "denominator": "B23025_003",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Owner-Occupied Housing": {
                "numerator": "B25003_002",
                "denominator": "B25003_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Renter-Occupied Housing": {
                "numerator": "B25003_003",
                "denominator": "B25003_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "Median Home Value": {
                "variable": "B25077_001",
                "aggregation": "weighted_median",
                "weight_var": "B25003_002",
                "format": "currency"
            },
            "Median Gross Rent": {
                "variable": "B25064_001",
                "aggregation": "weighted_median",
                "weight_var": "B25003_003",
                "format": "currency"
            },
            "% No Health Insurance": {
                "numerator": "B27010_017",
                "denominator": "B27010_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Foreign Born": {
                "numerator": "B05002_013",
                "denominator": "B05002_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Age 65 and Over": {
                "numerator": ["B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
                             "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049"],
                "denominator": "B01001_001",
                "aggregation": "percent",
                "format": "percent"
            },
            "% Under Age 18": {
                "numerator": ["B01001_003", "B01001_004", "B01001_005", "B01001_006",
                             "B01001_027", "B01001_028", "B01001_029", "B01001_030"],
                "denominator": "B01001_001",
                "aggregation": "percent",
                "format": "percent"
            }
        }

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def _get_all_variables(self, selected_indicators):
        """Extract all variable codes needed for selected indicators"""
        variables = set()
        for indicator_name in selected_indicators:
            if indicator_name in self.indicators:
                ind = self.indicators[indicator_name]
                if "variable" in ind:
                    variables.add(ind["variable"])
                if "weight_var" in ind:
                    variables.add(ind["weight_var"])
                if "numerator" in ind:
                    if isinstance(ind["numerator"], list):
                        variables.update(ind["numerator"])
                    else:
                        variables.add(ind["numerator"])
                if "denominator" in ind:
                    variables.add(ind["denominator"])
        return list(variables)

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Study Area Features",
            name="study_area",
            datatype="GPFeatureLayer",
            parameterType="Required",
            direction="Input")
        p0.filter.list = ["Polygon"]
        p0.description = "Polygon layer defining the study area. Uses selected features if present, otherwise all features."

        p1 = arcpy.Parameter(
            displayName="Indicators",
            name="indicators",
            datatype="GPString",
            parameterType="Required",
            direction="Input",
            multiValue=True)
        p1.filter.type = "ValueList"
        p1.filter.list = list(self.indicators.keys())
        p1.value = ["Total Population", "Median Household Income", "% Bachelor's Degree or Higher", 
                    "% Below Poverty Level", "% Owner-Occupied Housing"]
        p1.description = "Select demographic indicators to include in the report."

        p2 = arcpy.Parameter(
            displayName="Census Geography Level",
            name="geography",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p2.filter.type = "ValueList"
        p2.filter.list = ["tract", "block group"]
        p2.value = "tract"
        p2.description = "Census geography level to aggregate from. Block groups are smaller but may have higher margins of error."

        p3 = arcpy.Parameter(
            displayName="Year",
            name="year",
            datatype="GPLong",
            parameterType="Required",
            direction="Input")
        p3.value = 2023
        p3.filter.type = "Range"
        p3.filter.list = [2009, 2040]
        p3.description = "ACS data year."

        p4 = arcpy.Parameter(
            displayName="Survey",
            name="survey",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p4.filter.type = "ValueList"
        p4.filter.list = ["acs5", "acs1"]
        p4.value = "acs5"
        p4.description = "ACS 5-Year (all geographies) or 1-Year (areas 65,000+ population only)."

        p5 = arcpy.Parameter(
            displayName="State",
            name="state",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p5.filter.type = "ValueList"
        p5.filter.list = [
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
            "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
            "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR",
            "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
            "WI", "WY"]
        p5.description = "State containing the study area."

        p6 = arcpy.Parameter(
            displayName="County (optional)",
            name="county",
            datatype="GPString",
            parameterType="Optional",
            direction="Input")
        p6.description = "County name to limit data download and include in comparison. Leave blank to use entire state."

        p7 = arcpy.Parameter(
            displayName="Compare to County",
            name="compare_county",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input",
            category="Comparison Geographies")
        p7.value = True
        p7.description = "Include county-level comparison in report."

        p8 = arcpy.Parameter(
            displayName="Compare to State",
            name="compare_state",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input",
            category="Comparison Geographies")
        p8.value = True
        p8.description = "Include state-level comparison in report."

        p9 = arcpy.Parameter(
            displayName="Compare to Nation",
            name="compare_nation",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input",
            category="Comparison Geographies")
        p9.value = True
        p9.description = "Include national-level comparison in report."

        p10 = arcpy.Parameter(
            displayName="Study Area Name",
            name="study_area_name",
            datatype="GPString",
            parameterType="Optional",
            direction="Input")
        p10.value = "Study Area"
        p10.description = "Name to use for the study area in the report."

        p11 = arcpy.Parameter(
            displayName="Output Report",
            name="output_file",
            datatype="DEFile",
            parameterType="Required",
            direction="Output")
        p11.filter.list = ["xlsx", "csv", "html"]
        p11.description = "Output report file. Supports Excel (.xlsx), CSV (.csv), or HTML (.html)."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        # Enable/disable county comparison based on whether county is specified
        county_param = parameters[6]
        compare_county = parameters[7]
        
        if not county_param.valueAsText:
            compare_county.value = False
            compare_county.enabled = False
        else:
            compare_county.enabled = True

    def updateMessages(self, parameters):
        study_area = parameters[0]
        indicators = parameters[1]
        compare_county = parameters[7]
        compare_state = parameters[8]
        compare_nation = parameters[9]
        county_param = parameters[6]

        # Check that at least one comparison geography is selected
        if not compare_county.value and not compare_state.value and not compare_nation.value:
            compare_state.setWarningMessage("Select at least one comparison geography for a meaningful report.")

        # Warn about county requirement
        if compare_county.value and not county_param.valueAsText:
            compare_county.setErrorMessage("Specify a county name to enable county comparison.")

    def execute(self, parameters, messages):
        import time
        
        study_area_layer = parameters[0].valueAsText
        selected_indicators = parameters[1].values
        geography = parameters[2].valueAsText
        year = parameters[3].value
        survey = parameters[4].valueAsText
        state = parameters[5].valueAsText
        county = parameters[6].valueAsText
        compare_county = parameters[7].value
        compare_state = parameters[8].value
        compare_nation = parameters[9].value
        study_area_name = parameters[10].valueAsText or "Study Area"
        output_file = parameters[11].valueAsText

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Demographic Profile Report")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage(f"  Study Area: {study_area_name}")
        arcpy.AddMessage(f"  Indicators: {len(selected_indicators)} selected")
        arcpy.AddMessage(f"  Census Geography: {geography}")
        arcpy.AddMessage(f"  Year/Survey: {year} {survey}")
        arcpy.AddMessage(f"  State: {state}")
        if county:
            arcpy.AddMessage(f"  County: {county}")
        arcpy.AddMessage(f"  Output: {output_file}")
        arcpy.AddMessage("=" * 60)

        # Check for selection
        desc = arcpy.Describe(study_area_layer)
        if hasattr(desc, "FIDSet") and desc.FIDSet:
            selected_count = len(desc.FIDSet.split(";"))
            arcpy.AddMessage(f"Using {selected_count} selected features")
        else:
            result = arcpy.management.GetCount(study_area_layer)
            arcpy.AddMessage(f"No selection - using all {result[0]} features")

        # Create temp directory
        temp_dir = tempfile.mkdtemp(prefix="acs_profile_")

        # Dissolve study area to single polygon
        arcpy.AddMessage("Dissolving study area...")
        dissolved_fc = os.path.join(temp_dir, "dissolved.shp")
        arcpy.management.Dissolve(study_area_layer, dissolved_fc)

        # Export dissolved area for R
        study_area_shp = os.path.join(temp_dir, "study_area.shp")
        arcpy.management.CopyFeatures(dissolved_fc, study_area_shp)

        # Get all variables needed
        all_variables = self._get_all_variables(selected_indicators)
        arcpy.AddMessage(f"Fetching {len(all_variables)} Census variables...")

        # Build indicator config JSON for R
        indicator_config = {}
        for ind_name in selected_indicators:
            if ind_name in self.indicators:
                indicator_config[ind_name] = self.indicators[ind_name]
        
        config_file = os.path.join(temp_dir, "indicator_config.json")
        with open(config_file, 'w') as f:
            json.dump(indicator_config, f)

        # Find R script
        script_path = os.path.join(os.path.dirname(__file__), "acs_demographic_profile.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        # Build command
        variables_str = ",".join(all_variables)
        
        cmd = [
            rscript, script_path,
            study_area_shp.replace("\\", "/"),
            str(year),
            survey,
            variables_str,
            geography,
            state,
            county or "",
            str(compare_county).upper(),
            str(compare_state).upper(),
            str(compare_nation).upper(),
            study_area_name,
            config_file.replace("\\", "/"),
            output_file.replace("\\", "/")
        ]

        # Run R script
        try:
            process = subprocess.Popen(
                cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                text=True, cwd=os.path.dirname(script_path))

            while True:
                line = process.stdout.readline()
                if line == '' and process.poll() is not None:
                    break
                if line:
                    arcpy.AddMessage(line.strip())

            stderr = process.stderr.read()
            if stderr:
                for line in stderr.split('\n'):
                    if line.strip():
                        arcpy.AddWarning(line)

            if process.returncode == 0:
                arcpy.AddMessage("=" * 60)
                arcpy.AddMessage(f"Report created: {output_file}")
                arcpy.AddMessage("=" * 60)
            else:
                arcpy.AddError(f"R script failed with code {process.returncode}")

        except Exception as e:
            arcpy.AddError(f"Error running R script: {str(e)}")

        # Cleanup
        try:
            time.sleep(1)
            shutil.rmtree(temp_dir, ignore_errors=True)
        except Exception:
            pass

class HotSpotAnalysis(object):
    def __init__(self):
        self.label = "Hot Spot Analysis"
        self.description = "Identify statistically significant hot spots and cold spots using Getis-Ord Gi* with optional MOE adjustment"
        self.canRunInBackground = False
        self.category = "Analysis"

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Input Feature Class",
            name="input_fc",
            datatype="GPFeatureLayer",
            parameterType="Required",
            direction="Input")
        p0.filter.list = ["Polygon", "Point"]
        p0.description = "Feature class containing the values to analyze."

        p1 = arcpy.Parameter(
            displayName="Analysis Field",
            name="analysis_field",
            datatype="Field",
            parameterType="Required",
            direction="Input")
        p1.parameterDependencies = [p0.name]
        p1.filter.list = ["Short", "Long", "Float", "Double"]
        p1.description = "The numeric field to analyze for clustering."

        p2 = arcpy.Parameter(
            displayName="MOE Field (optional)",
            name="moe_field",
            datatype="Field",
            parameterType="Optional",
            direction="Input")
        p2.parameterDependencies = [p0.name]
        p2.filter.list = ["Short", "Long", "Float", "Double"]
        p2.description = "Margin of error field for MOE-aware significance adjustment."

        p3 = arcpy.Parameter(
            displayName="Spatial Relationship",
            name="spatial_relationship",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p3.filter.type = "ValueList"
        p3.filter.list = [
            "Contiguity (shared boundary)",
            "Distance band",
            "K nearest neighbors"
        ]
        p3.value = "Contiguity (shared boundary)"
        p3.description = "Method for defining spatial neighbors."

        p4 = arcpy.Parameter(
            displayName="Distance Band (meters)",
            name="distance_band",
            datatype="GPDouble",
            parameterType="Optional",
            direction="Input")
        p4.value = 5000
        p4.description = "Distance threshold for distance band method. Features within this distance are neighbors."

        p5 = arcpy.Parameter(
            displayName="Number of Neighbors (K)",
            name="num_neighbors",
            datatype="GPLong",
            parameterType="Optional",
            direction="Input")
        p5.value = 8
        p5.filter.type = "Range"
        p5.filter.list = [1, 100]
        p5.description = "Number of nearest neighbors for KNN method."

        p6 = arcpy.Parameter(
            displayName="Significance Level",
            name="significance_level",
            datatype="GPString",
            parameterType="Required",
            direction="Input")
        p6.filter.type = "ValueList"
        p6.filter.list = ["0.10", "0.05", "0.01"]
        p6.value = "0.05"
        p6.description = "P-value threshold for significance. 0.05 = 95% confidence."

        p7 = arcpy.Parameter(
            displayName="Apply MOE Reliability Adjustment",
            name="apply_moe_adjustment",
            datatype="GPBoolean",
            parameterType="Optional",
            direction="Input")
        p7.value = True
        p7.description = "Adjust significance based on estimate reliability. Requires MOE field."

        p8 = arcpy.Parameter(
            displayName="Output Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Output")
        p8.description = "Output feature class with hot spot analysis results."

        return [p0, p1, p2, p3, p4, p5, p6, p7, p8]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        spatial_rel = parameters[3]
        distance_band = parameters[4]
        num_neighbors = parameters[5]
        moe_field = parameters[2]
        apply_moe = parameters[7]

        # Enable/disable distance band and K based on spatial relationship
        if spatial_rel.valueAsText == "Distance band":
            distance_band.enabled = True
            num_neighbors.enabled = False
        elif spatial_rel.valueAsText == "K nearest neighbors":
            distance_band.enabled = False
            num_neighbors.enabled = True
        else:
            distance_band.enabled = False
            num_neighbors.enabled = False

        # Enable MOE adjustment only if MOE field is provided
        if not moe_field.value:
            apply_moe.enabled = False
            apply_moe.value = False
        else:
            apply_moe.enabled = True

        # Auto-detect MOE field
        input_fc = parameters[0]
        analysis_field = parameters[1]
        
        if analysis_field.altered and not analysis_field.hasBeenValidated:
            if analysis_field.valueAsText and input_fc.value:
                field_name = analysis_field.valueAsText
                try:
                    fields = [f.name for f in arcpy.ListFields(input_fc.valueAsText)]
                    detected_moe = None

                    if field_name.endswith('E'):
                        candidate = field_name[:-1] + 'M'
                        if candidate in fields:
                            detected_moe = candidate

                    if not detected_moe:
                        candidate = field_name + '_MOE'
                        if candidate in fields:
                            detected_moe = candidate

                    if detected_moe:
                        moe_field.value = detected_moe
                except:
                    pass

    def updateMessages(self, parameters):
        spatial_rel = parameters[3]
        distance_band = parameters[4]
        num_neighbors = parameters[5]
        apply_moe = parameters[7]
        moe_field = parameters[2]

        if spatial_rel.valueAsText == "Distance band" and not distance_band.value:
            distance_band.setErrorMessage("Distance band is required for this spatial relationship.")

        if spatial_rel.valueAsText == "K nearest neighbors" and not num_neighbors.value:
            num_neighbors.setErrorMessage("Number of neighbors is required for this spatial relationship.")

        if apply_moe.value and not moe_field.value:
            apply_moe.setErrorMessage("MOE field is required for MOE adjustment.")

    def execute(self, parameters, messages):
        import time

        input_fc = parameters[0].valueAsText
        analysis_field = parameters[1].valueAsText
        moe_field = parameters[2].valueAsText
        spatial_relationship = parameters[3].valueAsText
        distance_band = parameters[4].value
        num_neighbors = parameters[5].value
        significance_level = parameters[6].valueAsText
        apply_moe_adjustment = parameters[7].value
        output_fc = parameters[8].valueAsText

        # Map spatial relationship to R parameter
        if "Contiguity" in spatial_relationship:
            spatial_method = "contiguity"
        elif "Distance" in spatial_relationship:
            spatial_method = "distance"
        else:
            spatial_method = "knn"

        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage("Hot Spot Analysis (Getis-Ord Gi*)")
        arcpy.AddMessage("=" * 60)
        arcpy.AddMessage(f"  Input: {input_fc}")
        arcpy.AddMessage(f"  Analysis Field: {analysis_field}")
        if moe_field:
            arcpy.AddMessage(f"  MOE Field: {moe_field}")
        arcpy.AddMessage(f"  Spatial Relationship: {spatial_relationship}")
        if spatial_method == "distance":
            arcpy.AddMessage(f"  Distance Band: {distance_band} meters")
        elif spatial_method == "knn":
            arcpy.AddMessage(f"  Number of Neighbors: {num_neighbors}")
        arcpy.AddMessage(f"  Significance Level: {significance_level}")
        arcpy.AddMessage(f"  MOE Adjustment: {apply_moe_adjustment}")
        arcpy.AddMessage(f"  Output: {output_fc}")
        arcpy.AddMessage("=" * 60)

        # Create temp directory
        temp_dir = tempfile.mkdtemp(prefix="acs_hotspot_")

        # Export input to temp file geodatabase for R
        temp_gdb = os.path.join(temp_dir, "temp.gdb")
        arcpy.management.CreateFileGDB(temp_dir, "temp.gdb")
        temp_input_fc = os.path.join(temp_gdb, "input_features")

        arcpy.AddMessage("Exporting input features...")
        arcpy.management.CopyFeatures(input_fc, temp_input_fc)

        count = int(arcpy.management.GetCount(temp_input_fc)[0])
        arcpy.AddMessage(f"  Exported {count:,} features")

        time.sleep(1)

        # Set up output
        temp_output_gpkg = os.path.join(temp_dir, "output.gpkg")

        # Find R script
        script_path = os.path.join(os.path.dirname(__file__), "acs_hotspot_analysis.R")
        if not os.path.exists(script_path):
            arcpy.AddError(f"R script not found: {script_path}")
            return

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")

        # Build command
        cmd = [
            rscript, script_path,
            temp_input_fc.replace("\\", "/"),
            analysis_field,
            moe_field or "",
            spatial_method,
            str(distance_band or 0),
            str(num_neighbors or 8),
            significance_level,
            str(apply_moe_adjustment).upper(),
            temp_output_gpkg.replace("\\", "/")
        ]

        # Run R script
        r_success = False
        try:
            process = subprocess.Popen(
                cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                text=True, cwd=os.path.dirname(script_path))

            while True:
                line = process.stdout.readline()
                if line == '' and process.poll() is not None:
                    break
                if line:
                    arcpy.AddMessage(line.strip())

            stderr = process.stderr.read()
            if stderr:
                for line in stderr.split('\n'):
                    if line.strip():
                        arcpy.AddWarning(line)

            if process.returncode == 0:
                r_success = True
            else:
                arcpy.AddError(f"R script failed with code {process.returncode}")

        except Exception as e:
            arcpy.AddError(f"Error running R script: {str(e)}")

        # Convert output to final location
        if r_success:
            try:
                arcpy.AddMessage("Converting output...")
                gpkg_layer_path = f"{temp_output_gpkg}\\output"
                arcpy.management.CopyFeatures(gpkg_layer_path, output_fc)
                
                # Add layer to map and apply symbology
                try:
                    aprx = arcpy.mp.ArcGISProject("CURRENT")
                    active_map = aprx.activeMap
                    
                    if active_map:
                        arcpy.AddMessage("Adding layer to map...")
                        added_layer = active_map.addDataFromPath(output_fc)
                        layer_name = added_layer.name
                        
                        # Check if layer file exists
                        lyrx_path = os.path.join(os.path.dirname(__file__), "HotSpotAnalysis.lyrx")
                        if os.path.exists(lyrx_path):
                            arcpy.AddMessage("Applying symbology...")
                            
                            # Run as separate GP tool call using MakeFeatureLayer first
                            temp_layer = "HotSpot_TempLayer"
                            arcpy.management.MakeFeatureLayer(output_fc, temp_layer)
                            
                            # Now apply symbology to the temp layer
                            arcpy.ApplySymbologyFromLayer_management(
                                temp_layer,
                                lyrx_path,
                                [["VALUE_FIELD", "Gi_Bin_Adjusted", "Gi_Bin_Adjusted"]],
                                "MAINTAIN"
                            )
                            
                            # Save to layer file and re-add
                            temp_lyrx = os.path.join(temp_dir, "output_symbolized.lyrx")
                            arcpy.management.SaveToLayerFile(temp_layer, temp_lyrx, "RELATIVE")
                            
                            # Remove the unsymbolized layer and add the symbolized one
                            active_map.removeLayer(added_layer)
                            active_map.addDataFromPath(temp_lyrx)
                            
                            arcpy.AddMessage("Symbology applied successfully.")
                        else:
                            arcpy.AddMessage("Layer file not found - layer added without symbology.")
                    else:
                        arcpy.AddWarning("No active map found.")
                except Exception as map_error:
                    arcpy.AddWarning(f"Could not add layer to map: {map_error}")
                    lyrx_path = os.path.join(os.path.dirname(__file__), "HotSpotAnalysis.lyrx")
                    if os.path.exists(lyrx_path):
                        arcpy.AddMessage("")
                        arcpy.AddMessage("To apply hot spot symbology manually:")
                        arcpy.AddMessage("  1. Add the output layer to your map")
                        arcpy.AddMessage("  2. Right-click the layer → Symbology")
                        arcpy.AddMessage("  3. Click the menu icon → Import symbology")
                        arcpy.AddMessage(f"  4. Select: {lyrx_path}")
                        arcpy.AddMessage("")
                
                arcpy.AddMessage("=" * 60)
                arcpy.AddMessage(f"Hot spot analysis complete: {output_fc}")
                arcpy.AddMessage("=" * 60)
            except Exception as e:
                arcpy.AddError(f"Failed to convert output: {str(e)}")

        # Cleanup
        try:
            arcpy.management.ClearWorkspaceCache()
            time.sleep(1)
            shutil.rmtree(temp_dir, ignore_errors=True)
        except Exception:
            pass

class SetCensusAPIKey(object):
    def __init__(self):
        self.label = "Set Census API Key"
        self.description = "Store your Census API key permanently"
        self.canRunInBackground = False
        self.category = "Configuration"

    def _find_rscript(self):
        for base_path in [r"C:\Program Files\R", r"C:\Program Files (x86)\R"]:
            if os.path.exists(base_path):
                try:
                    r_versions = [d for d in os.listdir(base_path) if d.startswith("R-")]
                    if r_versions:
                        r_versions.sort(reverse=True)
                        rscript = os.path.join(base_path, r_versions[0], "bin", "Rscript.exe")
                        if os.path.exists(rscript):
                            return rscript
                except Exception:
                    continue
        return None

    def getParameterInfo(self):
        p0 = arcpy.Parameter(
            displayName="Census API Key",
            name="api_key",
            datatype="GPStringHidden",
            parameterType="Required",
            direction="Input")
        return [p0]

    def isLicensed(self):
        return True

    def updateParameters(self, parameters):
        pass

    def updateMessages(self, parameters):
        if parameters[0].value:
            key = parameters[0].valueAsText
            if len(key) != 40:
                parameters[0].setWarningMessage("Census API keys are typically 40 characters")

    def execute(self, parameters, messages):
        api_key = parameters[0].valueAsText

        arcpy.AddMessage("=" * 50)
        arcpy.AddMessage("Storing Census API Key")
        arcpy.AddMessage("=" * 50)

        rscript = self._find_rscript()
        if not rscript:
            arcpy.AddError("Could not find Rscript.exe")
            return

        arcpy.AddMessage(f"Using R: {rscript}")
        arcpy.AddMessage("Saving key to .Renviron file...")

        r_code = f'library(tidycensus); census_api_key("{api_key}", install = TRUE, overwrite = TRUE)'
        cmd = [rscript, "-e", r_code]

        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True)

        stdout, stderr = process.communicate()

        if stdout:
            for line in stdout.split('\n'):
                if line.strip():
                    arcpy.AddMessage(line)

        if stderr:
            for line in stderr.split('\n'):
                if line.strip() and "Attaching" not in line and "masked" not in line:
                    arcpy.AddWarning(line)

        if process.returncode == 0:
            arcpy.AddMessage("")
            arcpy.AddMessage("=" * 50)
            arcpy.AddMessage("Census API key stored successfully!")
            arcpy.AddMessage("")
            arcpy.AddMessage("The key is saved in your .Renviron file and will be")
            arcpy.AddMessage("automatically loaded each time R starts.")
            arcpy.AddMessage("")
            arcpy.AddMessage("You can now use the Download tool without entering")
            arcpy.AddMessage("the API key each time.")
            arcpy.AddMessage("=" * 50)
        else:
            arcpy.AddError("Failed to store API key")
            arcpy.AddError(f"Return code: {process.returncode}")
