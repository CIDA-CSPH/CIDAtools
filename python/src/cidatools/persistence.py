import abc
import json
import os
import pathlib
from typing import TypeVar

from pydantic import BaseModel

T = TypeVar("T", bound=BaseModel)


class PersistentWrapper(abc.ABC):

    @property
    @abc.abstractmethod
    def path(self) -> pathlib.Path:
        """Returns the path where the persistent model is stored on disk.
        :return:
        """
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def parent(self) -> "PersistentWrapper | None":
        """Returns the parent PersistentWrapper or None.

        If a parent is defined, it will be searched in the case where the PersistentField is None
        at the current level.
        :return: A PersistentWrapper class or None.
        """
        raise NotImplementedError

    @property
    @abc.abstractmethod
    def model_type(self) -> type[T]:
        """Returns the type of the model which is being wrapped. Should extend BaseModel.
        :return: A type derived from BaseModel
        """
        raise NotImplementedError

    @property
    def model(self) -> T:
        """Returns an instance of the wrapped model, which is used if can_persist=False
        :return:
        """
        return self._model

    @model.setter
    def model(self, value: T):
        self._model = value

    @property
    def can_persist(self):
        return os.access(self.path, os.R_OK | os.W_OK)

    def __init__(self):
        self.model = self.model_type()


def _check_attrs(obj):
    for attr in ["path", "parent", "model_type"]:
        if not hasattr(obj, attr):
            raise ValueError(f"{obj.__name__} has no attribute '{attr}'.")


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
        if name not in owner.model_type.model_fields:
            raise ValueError(f"Field {name} is not a field of {owner.model_type}.")
        # Create a getter for this name
        self._getter = self._build_getter(name)
        # Create a setter for this name
        self._setter = self._build_setter(name)

    @staticmethod
    def _load_model(instance):
        with open(instance.path, "r") as f:
            # Load the model from file.
            _model = instance.model_type.model_validate(json.load(f))
        return _model

    @staticmethod
    def _build_getter(name: str):

        def getter(instance):
            # Load model
            _model = PersistentField._load_model(instance) if instance.can_persist else instance.model
            # Return the loaded value.
            tmp_get = getattr(_model, name)
            if tmp_get is None and instance.parent is not None and hasattr(instance.parent, name):
                tmp_get = getattr(instance.parent, name)
            return tmp_get

        # Return the constructed function
        return getter

    @staticmethod
    def _build_setter(name: str):
        def setter(instance, value):
            can_persist = instance.can_persist
            # Load model
            _existing_model = PersistentField._load_model(instance) if can_persist else instance.model
            # Update the existing model
            _updated_model = _existing_model.model_copy(update={name: value})
            # Validate the updated model
            _valid_model = _updated_model.model_validate(_updated_model.model_dump())
            # If we can persist, we write to the file.
            if can_persist:
                # Write new model to disk
                tmp_path = instance.path.with_suffix(".tmp")
                with open(tmp_path, "w") as f:
                    f.write(_valid_model.model_dump_json(indent=4))
                # Once written, replace the tmp
                tmp_path.replace(instance.path)
            # Otherwise, we save to the internal model attribute.
            else:
                instance.model = _valid_model

        # Return the constructed function
        return setter
