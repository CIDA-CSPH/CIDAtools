# =============================================================================
# Name: Documentation Generator
# Author: Andrew Hill
# Last Updated: 08/13/2026
# Description:
#   The purpose of this script is to generate combined R, Python, and CLI
#   documentation blocks, which can be referenced throughout the Zensical
#   documentation.
#
#   The current plan for this script is as follows:
#   1. Match Python/R functions by NAME
#     1a. For R functions, we can parse the documentation files (R/man/*.Rd)
#     1b. For Python functions, we use Griffe to parse the docstrings
#         directly from source.
#   2. If a function is available for R and Python (i.e. the function name
#      exists in both the Python and R packages), the generated block will
#      contain R and Python tabs.
#   3. CLI processing is not complete yet. My currently plan is to refactor the
#      CLI handler into separate functions and use a custom
#      :meta cli_wraps: <func name> to allow us to join CLI blocks to their
#      associated library function.
# ==============================================================================
import argparse
import glob
import itertools
import logging
import pathlib
from collections import Counter, defaultdict
from typing import Any

import griffe
import jinja2
from TexSoup import TexSoup, TexNode
from TexSoup.data import TexText
from pydantic import BaseModel

log = logging.getLogger(__name__)


class RFunctionParam(BaseModel):
    name: str
    description: str | None = None


class RFunction(BaseModel):
    name: str
    arguments: list[RFunctionParam] | None = None
    value: str | None = None
    seealso: str | None = None
    deprecated: bool = False
    description: str | None = None
    example: str | None = None
    usage: str | None = None


def _is_item(elem):
    return isinstance(elem, TexNode) and elem.name == "item"


def _is_text(elem):
    return isinstance(elem, TexText)


def process_r_argumnents(arg_section: TexNode):
    """ Processes an \arguments{} block of a .Rd file.
    :param arg_section: The section to process
    :return:
    """
    tmp_l = []
    i = 0
    descendants = list(arg_section.descendants)
    while i < len(descendants):
        # Current element we are processing
        elem = descendants[i]
        # We expect that each \item should have one or two arguments (name, and description).
        # Ignore newlines or spaces.
        if _is_item(elem):
            if i == len(descendants) - 1:
                log.warning("Found a \\item tag at the end of the arguments list!")
                break
            else:
                pot_param_l = []
                # Get index of next \\item (or end of params)
                offset = 0
                for param_elem in elem.contents:
                    if _is_text(param_elem):
                        pot_param_l.append(str(param_elem))
                # If we got one param, populate the title
                if len(pot_param_l) == 1:
                    tmp_l.append(RFunctionParam(name=pot_param_l[0]))
                # If we got two params, populate title and description
                elif len(pot_param_l) == 2:
                    tmp_l.append(RFunctionParam(name=pot_param_l[0], description=pot_param_l[1]))
                # Otherwise, set a warning
                else:
                    log.warning(f"Parsed an \\item tag with {len(pot_param_l)} elements, but expected 1 or 2.")
        i += 1
    return tmp_l


def get_tag_param(tag: TexNode):
    """ Gets the first param of a  block of a .Rd file.
    :param tag: The section to process
    :return:
    """
    return str(next(tag.descendants))


def process_rd(rd_path: str):
    """ Process a .Rd file, returning a dictionary representation.
    :param rd_path:
    :return:
    """
    log.info(f"Processing {rd_path}")
    # Read the file.
    with open(rd_path, "r") as f:
        rd_contents = f.read()

    # Parse the .Rd file as TeX
    rd = TexSoup(rd_contents)

    # Dictionary to hold state
    rfunc_d = {}

    # Hack to check for deprecation in CIDAtools, since we don't use the deprecated lifecycle tag
    lives_in_temp_refactored = any(desc == "% Please edit documentation in R/temp_refactored_message.R" for desc in rd.descendants)
    mentions_deprecated = any("To be deprecated" in desc for desc in rd.descendants)
    if lives_in_temp_refactored or mentions_deprecated:
        rfunc_d["deprecated"] = True

    # Iterate the tags and process
    for tag in rd.children:
        match tag.name:
            # For many tags we can just extract the first parameter and call it a day
            case "name" | "usage" | "value" | "description" | "seealso":
                rfunc_d[tag.name] = get_tag_param(tag)
            # For arguments, we need to process each \item
            case "arguments":
                rfunc_d[tag.name] = process_r_argumnents(tag)
            # We parse the lifecycle to get a deprecated value.
            case "lifecycle":
                lifecycle_val = get_tag_param(tag)
                if lifecycle_val == "deprecated":
                    rfunc_d["deprecated"] = True
            # Print a log message if we don't know what the tag is.
            case _:
                log.info(f"Skipping tag '{tag.name}'.")

    log.info(f"Done processing {rd_path}")

    return RFunction.model_validate(rfunc_d)


def load_r_functions(r_doc_dir: pathlib.Path):
    # Parse all Rd files.
    rd_files = glob.glob(str(r_doc_dir.joinpath("*.Rd")))
    # Iterate and read each file.
    rd_out = {}
    for rd_file in rd_files:
        rd_name = pathlib.Path(rd_file).name.removesuffix(".Rd")
        rd_out[rd_name] = process_rd(rd_file)
    # Return a dictionary.
    return rd_out


class PyFuncParam(BaseModel):
    name: str
    description: str | None = None
    type_hint: str | None = None
    default_value: str | None = None


class PyReturnVal(BaseModel):
    description: str | None = None
    type_hint: str | None = None


class PyFunction(BaseModel):
    name: str
    module: str
    description: str | None = None
    arguments: list[PyFuncParam] | None = None
    return_value: PyReturnVal | None = None


class DocumentationSnippet(BaseModel):
    name: str
    py: PyFunction | None = None
    r: RFunction | None = None
    cli: None = None

    @property
    def category(self) -> str:
        """ This function tries to determine a category for the function,
        which is currently just the python module name.
        Functions with no Python implementation are grouped under an Uncategorized category.
        :return:
        """
        if self.py is not None:
            return self.py.module
        return "uncategorized"

    def render(self, template_dir: pathlib.Path) -> str:
        loader = jinja2.FileSystemLoader(template_dir)
        env = jinja2.Environment(loader=loader)
        return env.get_template("snippet.md.jinja").render(func=self)


NOT_SPECIFIED = "<Unspecified>"


def load_py_functions():
    # Use Griffe for Python
    cidatools_griffe = griffe.load("cidatools", docstring_parser="sphinx")
    # Get all functions by module.
    # TODO: This almost certainly doesn't work for nested modules (i.e. cidatools.level1.level2.level3).
    #       Fix by using a recursive search through modules.
    all_py_funcs = [func for module in cidatools_griffe.modules.keys() for func in cidatools_griffe.modules[module].functions.values() if not func.is_alias and not func.is_private]
    # Identify duplicates
    func_dups = {e: c for e, c in Counter([func.name for func in all_py_funcs]).items() if c > 1}
    if len(func_dups) > 0:
        log.fatal("The following function names are duplicated "
                  "(same function name in multiple packages), so matching is ambiguous!\n"
                  "\n".join(f"{f}: appears {c} times" for f, c in func_dups.items()))
    # Parsed CIDAtools module functions
    out_d = {}
    for func in all_py_funcs:
        pfunc_d: dict[str, Any] = {"name": func.name, "module": func.module.name}

        # Create a temp return value object.
        tmp_return_d = {"type_hint": str(func.returns) if func.returns is not None else NOT_SPECIFIED}
        # Pull some info from the parsed docstring if available
        if func.docstring is not None:
            for parsed_docstring_elem in func.docstring.parsed:
                match parsed_docstring_elem.kind:
                    case "text":
                        pfunc_d["description"] = parsed_docstring_elem.value
                    case "parameters":
                        pfunc_d["arguments"] = [{
                            "name": p.name,
                            "description": p.description,
                            "type_hint": p.annotation.canonical_name if p.annotation is not None else NOT_SPECIFIED,
                            "default_value": p.default
                        } for p in parsed_docstring_elem.value]
                    case "returns":
                        # We only handle the first element.
                        return_val = parsed_docstring_elem.value[0]
                        # In cases where we don't have a specified type hint, but we do say 'None' in the return value of the docstring,
                        # update the type_hint to be None
                        if tmp_return_d["type_hint"] == NOT_SPECIFIED and return_val.description == "None":
                            tmp_return_d["type_hint"] = "None"
                            tmp_return_d["description"] = None
                        else:
                            tmp_return_d["description"] = return_val.description
        else:
            # Populate a temp dictionary of which are parsed outside of the docstring.
            pfunc_d["arguments"] = [{
                "name": param.name,
                "type_hint": param.annotation.canonical_name if param.annotation is not None else NOT_SPECIFIED,
                "default_value": param.default
            } for param in func.parameters]

        pfunc_d["return_value"] = tmp_return_d
        out_d[func.name] = PyFunction.model_validate(pfunc_d)
    return out_d


if __name__ == "__main__":
    # Set logging
    logging.basicConfig(
        level=logging.DEBUG,
        format="[%(levelname)s] %(message)s",
    )
    # Parse arguments
    parser = argparse.ArgumentParser()
    parser.add_argument("--template-dir", default="../docs/templates", type=pathlib.Path, help="Path where templates are stored (template paths in this script are relative to this directory).")
    parser.add_argument("--output-dir", default="../docs/generated", type=pathlib.Path, help="Path where generated documentation should be stored.")
    parser.add_argument("--rd-dir", default="../R/man", type=pathlib.Path, help="Path where Roxygen R documentation files are stored.")
    args = parser.parse_args()
    # Parse the Rd documents for R
    cidatools_r = load_r_functions(r_doc_dir=args.rd_dir)
    # Parse the Python code using Griffe
    cidatools_py = load_py_functions()
    # TODO: Find a way to integrate CLI functions here
    cidatools_cli = {}
    # Create output directory for snippets
    snippet_output_dir = args.output_dir.joinpath("snippets")
    snippet_output_dir.mkdir(parents=True, exist_ok=True)
    # Create an iterable (by function name) which contains a tuple of the Python, R, and CLI functions
    all_keys = sorted(cidatools_py.keys() | cidatools_r.keys() | cidatools_cli.keys())
    func_iter = {key: (cidatools_py.get(key), cidatools_r.get(key), cidatools_cli.get(key)) for key in all_keys}
    # Iterate and create the snippet
    snippet_d = defaultdict(list)
    for i, (name, (py_f, r_f, cli_f)) in enumerate(func_iter.items()):
        # Validate the snippet model
        snippet = DocumentationSnippet.model_validate({
            "name": name,
            "py": py_f,
            "r": r_f,
            "cli": cli_f
        })
        # Render the output with the Jinja template (save rendered output for later)
        rendered_output = snippet.render(template_dir=args.template_dir)
        # Write the rendered output to a file.
        snippet_path = snippet_output_dir.joinpath(f"{name}.md")
        with open(snippet_path, "w") as f:
            f.write(rendered_output)
        log.info(f"Wrote {snippet_path}.")
        # Save the rendered output
        snippet_d[snippet.category].append(rendered_output)
    log.info(f"Finished generating {len(snippet_d)} snippets.")
    # Make output directory for categorized function lists.
    category_output_dir = args.output_dir.joinpath("categories")
    category_output_dir.mkdir(parents=True, exist_ok=True)
    # Create grouped output files.
    for snippet_group, snippet_outputs in snippet_d.items():
        category_file_path = category_output_dir.joinpath(f"{snippet_group}.md")
        with open(category_file_path, "w") as f:
            f.write("\n".join(snippet_outputs))
        log.info(f"Wrote {category_file_path}.")
    log.info(f"Finished generating {len(snippet_d.items())} categories.")