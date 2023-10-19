"""
the path specification for the project

"""

# path specification - using os
import os

# Get the current working directory
current_dir = os.getcwd()

# Navigate up two levels to the root directory
root_dir = os.path.abspath(os.path.join(current_dir, '..', '..'))

print(root_dir)
