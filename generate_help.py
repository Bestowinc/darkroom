# -*- coding: utf-8 -*-
"""
Autogenerate the helpdocs for darkroom
run from command line:
    $ python generate_help.py
will run <arg> --help for every argument present in the main function
and replace the present set of arguments
"""
import os
import subprocess
import argparse


def replace_index(filename, doc_arg, doc_str):
    """ finds the old index in filename and replaces it with the lines in new_index
    if no existing index places new index at end of file
    if file doesn't exist creates it and adds new index
    will only replace the first index block in file  (why would you have more?)
    """

    pre_doc = []
    post_doc = []
    pre = True
    post = False
    try:
        with open(filename, "r") as md_in:
            for line in md_in:
                if f"<!-- {doc_arg} start" in line:
                    pre = False
                if f"<!-- {doc_arg} stop" in line:
                    post = True
                if pre:
                    pre_doc.append(line)
                    pre_doc.append("```")
                if post:
                    post_doc.append(line)
                    post_doc.append("```")
    except FileNotFoundError:
        pass

    with open(filename, "w") as md_out:
        md_out.writelines(pre_doc)
        md_out.writelines(doc_str)
        md_out.writelines(post_doc[1:])


def main():
    """generate index optional cmd line arguments"""
    parser = argparse.ArgumentParser(
        description=("auto update darkroom helpdoc using doc_arg anchors")
    )

    parser.add_argument(
        "filename", nargs="?", default="README.md", help="markdown output file"
    )
    current_slugs = ["dark", "dark record", "dark take"]

    args = parser.parse_args()

    cwd = os.getcwd()
    md_out_fn = os.path.join(cwd, args.filename)
    for doc_arg in current_slugs:
        doc_str = subprocess.run(doc_arg.split(" ") + ["--help"]).stdout()
        # do a naive pass for now
        replace_index(md_out_fn, doc_arg, doc_str)


if __name__ == "__main__":
    main()
