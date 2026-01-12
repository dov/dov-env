#!/usr/bin/python
"""
Docx Script Anonymizer

This module provides a command-line utility to identify and rename specific 
authors within a Microsoft Word (.docx) file. It targets metadata stored in 
both comments and tracked changes (revisions).

Unlike generic metadata removal tools, this script allows for the targeted 
replacement of specific names, making it ideal for double-blind peer reviews 
or sharing documents while preserving the history of other contributors.

Usage:
    List authors:
        python anonymize.py --list-authors input.docx 

    Anonymize a specific author:
        python anonymize.py -o output.docx -t "Old Name" -r "New Name" input.docx 

Author: Dov Grobgeld 
Date: 2026-01-12 <dov.grobgeld@gmail.com>
"""

import zipfile
import argparse
import os
from lxml import etree

def get_all_authors(input_path):
    """Returns a unique set of all authors found in the docx."""
    ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
    target_xmls = ['word/document.xml', 'word/comments.xml']
    authors = set()

    with zipfile.ZipFile(input_path, 'r') as zin:
        for xml_file in target_xmls:
            if xml_file in zin.namelist():
                data = zin.read(xml_file)
                root = etree.fromstring(data)
                # Find all elements with a w:author attribute
                for element in root.xpath('//*[@w:author]', namespaces=ns):
                    name = element.get('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}author')
                    if name:
                        authors.add(name)
    return authors

def anonymize_docx(input_path, output_path, target_name, replacement):
    ns = {'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'}
    target_xmls = ['word/document.xml', 'word/comments.xml']

    with zipfile.ZipFile(input_path, 'r') as zin:
        with zipfile.ZipFile(output_path, 'w') as zout:
            for item in zin.infolist():
                data = zin.read(item.filename)
                
                if item.filename in target_xmls:
                    root = etree.fromstring(data)
                    modified = False
                    for element in root.xpath('//*[@w:author]', namespaces=ns):
                        if element.get('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}author') == target_name:
                            element.set('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}author', replacement)
                            if '{http://schemas.openxmlformats.org/wordprocessingml/2006/main}initials' in element.attrib:
                                element.set('{http://schemas.openxmlformats.org/wordprocessingml/2006/main}initials', replacement[0])
                            modified = True
                    
                    if modified:
                        data = etree.tostring(root)
                
                zout.writestr(item, data)

def main():
    parser = argparse.ArgumentParser(description="Anonymize or list authors in a .docx file.")
    
    # Mode switch
    parser.add_argument("--list-authors", action="store_true", help="List all authors and exit")
    
    # Only required if not listing
    parser.add_argument("-o", "--output", help="Path for the anonymized .docx file")
    parser.add_argument("-t", "--target", help="The author name you want to replace")
    parser.add_argument("-r", "--replace", default="Author", help="The replacement name")

    parser.add_argument('input', nargs=1, help='Input. Default is stdin')

    args = parser.parse_args()

    if not os.path.exists(args.input[0]):
        print(f"Error: File '{args.input[0]}' not found.")
        return

    if args.list_authors:
        authors = get_all_authors(args.input[0])
        if authors:
            print("Authors found in document:")
            for a in sorted(authors):
                print(f" - {a}")
        else:
            print("No authors found in comments or tracked changes.")
    else:
        # Manually enforce requirements if not listing
        if not args.output or not args.target:
            parser.error("The following arguments are required when not using --list-authors: -o/--output, -t/--target")
        
        try:
            anonymize_docx(args.input[0], args.output, args.target, args.replace)
            print(f"Success! '{args.target}' replaced with '{args.replace}' in '{args.output}'.")
        except Exception as e:
            print(f"An error occurred: {e}")

if __name__ == "__main__":
    main()
    
