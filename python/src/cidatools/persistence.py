import abc
import json
import os
import pathlib
from typing import TypeVar

from pydantic import BaseModel

from cidatools.utils import print_info

T = TypeVar("T", bound=BaseModel)


class PersistentWrapper(abc.ABC):
    __model__: type[T] = None
    __parent__: "PersistentWrapper" = None

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)

        if "__model__" not in cls.__dict__ or cls.__dict__["__model__"] is None:
            raise TypeError("Subclass of PersistentWrapper must set '__model__' to a non-None value.")

        if "__parent__" not in cls.__dict__:
            raise TypeError("Subclass of PersistentWrapper must define '__parent__'.")

    @property
    def path(self):
        return self._path

    def needs_reload(self) -> bool:
        # If we have never loaded from file, load the defaults now.
        if self._key_mtime is None or self._key_size is None:
            return True

        # Check file stats to see if anything has changed since last reload.
        file_stats = os.stat(self.path)
        return self._key_mtime != file_stats.st_mtime or self._key_size != file_stats.st_size

    def reload(self, force: bool = False):
        if force or self.needs_reload():
            with open(self.path, "r") as f:
                # Load the model from file.
                self._model = self.__model__.model_validate(json.load(f))
                # Update file stats
                file_stats = os.fstat(f.fileno())
                self._key_mtime = file_stats.st_mtime
                self._key_size = file_stats.st_size
                print_info(f"{self.path} reloaded.")

    def __init__(self, path: pathlib.Path):
        self._path = path
        self._model = None
        self._key_mtime = None
        self._key_size = None
        self._internal_cache = None

    def ensure_loaded(self):
        # Load model on first access.
        if self._model is None:
            self.reload(force=True)

    def _get_model_attr(self, item):
        if item not in self.__model__.model_fields:
            raise AttributeError(f"Field {item} does not exist.")
        # Make sure some model is loaded
        self.reload()
        # Get the attribute from the underlying model.
        return getattr(self._model, item)

    def _set_model_attr(self, key, value):
        # For other keys, check if it is a valid model key.
        if key not in self.__model__.model_fields:
            raise AttributeError(f"Field {key} does not exist.")
        # Reload if required.
        self.reload()
        # Update the existing model
        _updated_model = self._model.model_copy(update={key: value})
        # Validate the updated model
        _valid_model = _updated_model.model_validate(_updated_model.model_dump())
        # Write new model to disk
        tmp_path = self.path.with_suffix(".tmp")
        with open(tmp_path, "w") as f:
            f.write(_valid_model.model_dump_json(indent=4))
        # Once written, replace the tmp
        tmp_path.replace(self.path)
        # Note, we do not update the cached model here
        # so the next attribute access should pull from disk


def _check_attrs(obj):
    for attr in ["__parent__", "__model__"]:
        if not hasattr(obj, attr):
            raise ValueError(f"{obj} has no attribute '{attr}'.")


class PersistentField:
    def __init__(self) -> None:
        pass

    def __get__(self, obj, objtype=None):
        _check_attrs(obj)
        return self._getter(obj)

    def __set__(self, obj, value):
        _check_attrs(obj)
        self._setter(obj, value)

    def __set_name__(self, owner, name):
        # Check that the name we are assigning is actually a field of the model class
        if name not in owner.__model__.model_fields:
            raise ValueError(f"Field {name} is not a field of {owner.model_type}.")
        # Create a getter for this name
        self._getter = self._build_getter(name)
        # Create a setter for this name
        self._setter = self._build_setter(name)

    @staticmethod
    def _build_getter(name: str):

        def getter(instance):
            # Return the loaded value.
            tmp_get = instance._get_model_attr(name)
            if tmp_get is None and instance.__parent__ is not None and hasattr(instance.__parent__, name):
                tmp_get = instance.__parent__._get_model_attr(name)
            return tmp_get

        # Return the constructed function
        return getter

    @staticmethod
    def _build_setter(name: str):
        def setter(instance, value):
            instance._set_model_attr(name, value)

        # Return the constructed function
        return setter
