## batch_sleap_inference_v1.py

import os
import subprocess

## type below line  
## sleap-track --help
### FOR MULTI-ANIMAL TOP DOWN MODEL, #1

### FOR MULTI-ANIMAL TOP DOWN MODEL, #4. Does not open gui after tracking. 
subprocess.call('(for %i in (*.mp4) do sleap-track ''%i'' -m "SLEAP_v2/models/210420_212809.multi_instance.n=207" --no-empty-frames --batch_size 16 --tracking.tracker simple --tracking.target_instance_count 2 --tracking.pre_cull_to_target 2 --tracking.pre_cull_iou_threshold 0.9 --tracking.post_connect_single_breaks 2 --tracking.clean_instance_count 2 --tracking.similarity centroid --tracking.match greedy --tracking.track_window 60)', shell=True)   


