## sleap_batch.py

## > cd /d working_directory_with_videos   # RUN IN FOLDER WITH MODELS AND PREDICTIONS FOLDER ALREADY. 
## > conda activate sleap
## > batch.py

# https://sleap.ai/notebooks/Training_and_inference_using_Google_Drive.html 
# add in the tracker information for single animal.... need to connect predictions. 

import os
import subprocess

subprocess.call('(for %i in (*.mp4) do sleap-track ''%i'' -m "models/210226_161753.single_instance.2819")', shell=True)   ### FOR SINGLE ANIMAL MODEL, CHANGE 


