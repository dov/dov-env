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
import json

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

GRAY = "\033[90m"
RESET = "\033[0m"
NOT_MOUNTED = f"{GRAY}Not mounted{RESET}"

import re
_ANSI_RE = re.compile(r"\033\[[0-9;]*m")

def visible_len(s):
    return len(_ANSI_RE.sub("", s))

def ellipsize(s, width):
    """Truncate string to width, adding ellipsis if needed."""
    if visible_len(s) <= width:
        return s
    return s[:width - 1] + "…"

def partition_mount_label(part):
    """Return mount points for a partition, falling back to fstype if unmounted."""
    mps = [m for m in (part.get("mountpoints") or []) if m and m != "[SWAP]"]
    if mps:
        return mps
    fstype = part.get("fstype")
    if fstype:
        return [f"[{fstype}]"]
    return []

def collect_partition_info(children):
    """Recursively collect (fsuse%, mountpoints) from all partitions under a disk."""
    use_percents = []
    mountpoints = []
    for child in children:
        mountpoints.extend(partition_mount_label(child))
        pct = child.get("fsuse%")
        if pct:
            use_percents.append(int(pct.rstrip("%")))
        if child.get("children"):
            sub_pcts, sub_mps = collect_partition_info(child["children"])
            use_percents.extend(sub_pcts)
            mountpoints.extend(sub_mps)
    return use_percents, mountpoints

def pad(s, width):
    """Left-pad s to width, ignoring invisible ANSI escape bytes."""
    return s + " " * (width - visible_len(s))

def print_row(name, speed, size, use_str, model, mounts_str, nw):
    model = ellipsize(model, 24)
    mounts_str = ellipsize(mounts_str, 27)
    print(f"| {pad(name, nw)} | {speed:<6} | {size:<6} | {use_str:<5} | {model:<24} | {pad(mounts_str, 27)} |")

def get_disk_info():
    """
    Loops over all physical disks and retrieves their names, speeds, sizes, use%, models, and mount points.
    Outputs the data in an Org-mode style table. Partitions are shown as sub-rows below each disk.
    """
    try:
        lsblk_data = json.loads(xec(["lsblk", "-J", "-o", "NAME,ROTA,SIZE,FSUSE%,MOUNTPOINTS,FSTYPE"]))
        disks = lsblk_data["blockdevices"]

        # Compute column width from all disk and partition names
        all_names = ["Disk Name"]
        for disk in disks:
            all_names.append(disk["name"])
            for part in (disk.get("children") or []):
                all_names.append(" " + part["name"])
        nw = max(len(n) for n in all_names)

        sep = "-" * (nw + 2)
        print(f"| {'Disk Name':<{nw}} | Type   | Size   | Use%  | Model                    | Mount Points                |")
        print(f"|{sep}+--------+--------+-------+--------------------------+-----------------------------|")

        for disk in disks:
            name = disk["name"]
            speed = "HDD" if disk["rota"] else "SSD"
            size = disk["size"]
            children = disk.get("children") or []

            use_percents, mountpoints = collect_partition_info(children)
            use_str = f"{max(use_percents)}%" if use_percents else ""
            mounts_str = ", ".join(dict.fromkeys(mountpoints)) if mountpoints else NOT_MOUNTED

            try:
                udevadm_output = xec(["udevadm", "info", f"/dev/{name}", "-q", "property"])
                model = next((line.split("=")[1] for line in udevadm_output.splitlines() if line.startswith("ID_MODEL=")), "Unknown")
            except RuntimeError:
                model = "Unknown"

            print_row(name, speed, size, use_str, model, mounts_str, nw)

            if len(children) > 1:
                for part in children:
                    pname = " " + part["name"]
                    psize = part["size"]
                    ppct = part.get("fsuse%") or ""
                    pmps = partition_mount_label(part)
                    pmounts_str = ", ".join(pmps) if pmps else NOT_MOUNTED
                    print_row(pname, "", psize, ppct, "", pmounts_str, nw)
    except RuntimeError as e:
        print(f"Error: {e}")

if __name__ == "__main__":
    get_disk_info()
