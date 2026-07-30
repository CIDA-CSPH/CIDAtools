import abc
import functools
import json
import os
import pathlib
import sys
from abc import abstractmethod
from typing import Callable, Any, Generic, TypeVar

from pydantic import BaseModel

GREEN_CHECK = "\x1b[1m\x1b[32m\u2713\x1b[0m"
RED_XMARK = "\x1b[1m\x1b[31m\u2717\x1b[0m"
YELLOW_TRIANGLE = "\x1b[1m\x1b[33m\u26a0\x1b[0m"
BLUE_REFRESH = "\x1b[1m\x1b[34m\u27f3\x1b[0m"


def print_success(message: str):
    """Prints a success message.
    :param message: Message to print.
    :return:
    """
    print(f"{GREEN_CHECK} {message}", file=sys.stderr)


def print_warning(message: str):
    """Prints a warning message.
    :param message: Message to print.
    """
    print(f"{YELLOW_TRIANGLE} {message}", file=sys.stderr)


def print_failure(message: str):
    """Prints a failure message.
    :param message: Message to print.
    :return:
    """
    print(f"{RED_XMARK} {message}", file=sys.stderr)


def print_refresh(message: str):
    """
    Prints a refresh message.
    :param message:
    :return:
    """
    print(f"{BLUE_REFRESH} {message}", file=sys.stderr)


# def serial_wrapper(
#     model_cls: type[BaseModel],
#     reader: Callable[[pathlib.Path], BaseModel | None],
#     writer: Callable[[pathlib.Path, BaseModel], None],
# ):
#
#     def wrapper(cls: type[BaseModel]):
#
#         def _get_config_prop(self, name: str) -> Any:
#             _config = reader(self.path)
#             return getattr(_config, name) if _config is not None else None
#
#         def _write_config_prop(self, value: Any, name: str) -> None:
#             _config = reader(self.path)
#             if _config is None:
#                 print_failure(f"Unable to read config file.")
#                 return
#             # Create a copy with the updated property.
#             tmp_new_config = _config.model_copy(update={name: value})
#             # Validate the new model.
#             val_new_config = model_cls.model_validate(tmp_new_config.model_dump())
#             # Write the model to disk.
#             writer(self.path, val_new_config)
#
#         # Create a model property which loads the whole model.
#         setattr(cls, "model", property(fget=lambda self: reader(self.path)))
#
#         for field, _ in model_cls.model_fields.items():
#             # Create properties in cls which are serialized wrappers around model_cls
#             setattr(
#                 cls,
#                 field,
#                 property(
#                     fget=functools.partial(_get_config_prop, name=field),
#                     fset=functools.partial(_write_config_prop, name=field),
#                 ),
#             )
#
#         return cls
#
#     return wrapper


# def filesize_mtime_cache(f):
#     """Wrap a single argument function which reads a file, and execute f only if the file's size or mtime have changed.
#     :param f:
#     :return:
#     """
#
#     file_size = None
#     file_mtime = None
#     ret_val = None
#
#     @functools.wraps(f)
#     def inner(path: pathlib.Path):
#         nonlocal file_size, file_mtime, ret_val
#
#         # If the file doesn't exist, nothing we can do.
#         if not path.is_file():
#             print_failure(f"File not found: {path}")
#             return None
#
#         # Retrieve the current file stats.
#         cur_stat = path.stat()
#         cur_size = cur_stat.st_size
#         cur_mtime = cur_stat.st_mtime
#
#         # Check if file size or mtime have changed.
#         if cur_size != file_size or cur_mtime != file_mtime:
#             # Print load message
#             if cur_size is None or cur_mtime is None:
#                 # Message on first time load.
#                 print_refresh(f"Loading {path} from disk...")
#             else:
#                 # Notify that we have refreshed.
#                 print_refresh(f"Reloading, contents of file {path} changed on disk...")
#
#             # Update stats.
#             file_size = cur_size
#             file_mtime = cur_mtime
#
#             # Execute f(path) and store the result.
#             ret_val = f(path)
#
#             # Print success message.
#             print_success("File reloaded.")
#
#         # Return the stored value
#         return ret_val
#
#     # Return the wrapped function.
#     return inner
