#!/usr/bin/env python

######################################################################
#  Output disk info in a org-like text table
#
#  Created by github copilot.
#
#  2025-04-19 Sat
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################

import subprocess

def xec(command):
    """
    Executes an external command and returns its stdout as a string.
    Raises an exception if the command fails.
    """
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=True)
        return result.stdout.strip()
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Command '{' '.join(command)}' failed with error: {e.stderr.strip()}")

def get_disk_info():
    """
    Loops over all physical disks and retrieves their names, speeds, physical sizes, models, and read speeds.
    Outputs the data in an Org-mode style table.
    """
    try:
        # Get a list of all block devices
        lsblk_output = xec(["lsblk", "-d", "-o", "NAME,ROTA,SIZE", "--noheadings"])
        disks = lsblk_output.splitlines()

        # Print Org-mode table header
        print("| Disk Name | Type   | Size   | Model               | Read Speed (MB/s) |")
        print("|-----------+--------+--------+---------------------+-------------------|")

        for disk in disks:
            parts = disk.split()
            if len(parts) == 3:
                name, rota, size = parts
                speed = "HDD" if rota == "1" else "SSD"

                # Get model using udevadm
                try:
                    udevadm_output = xec(["udevadm", "info", f"/dev/{name}", "-q", "property"])
                    model = next((line.split("=")[1] for line in udevadm_output.splitlines() if "ID_MODEL" in line), "Unknown")
                except RuntimeError:
                    model = "Unknown"

                # Print Org-mode table row
                print(f"| {name:<9} | {speed:<6} | {size:<6} | {model:<19} |")
    except RuntimeError as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    get_disk_info()
