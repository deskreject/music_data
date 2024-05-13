"""
the path specification for the project

"""

# path specification - using os
import os

# Path to the directory where path_specification.py is located
dir_of_this_script = os.path.dirname(os.path.abspath(__file__))

# Navigate up to the root directory
root_dir = os.path.abspath(os.path.join(dir_of_this_script, '..', '..'))

print(root_dir)
