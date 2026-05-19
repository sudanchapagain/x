from typing import Any


class KdlParseError(Exception):
    """Error raised when KDL parsing fails."""


try:
    import ckdl

    def parse(source: str) -> Any:
        """Parse KDL source using ckdl.

        Args:
            source: KDL markup as string

        Returns:
            Parsed KDL document
        """
        return ckdl.parse(source)

    Document = ckdl.Document
    Node = ckdl.Node

except ImportError:

    def parse(source: str) -> Any:
        """Parse KDL source: requires ckdl to be installed.

        Args:
            source: KDL markup as string

        Raises:
            NotImplementedError: When ckdl is not installed
        """
        raise NotImplementedError("ckdl not installed")

    class Document:
        """Document class: requires ckdl to be installed."""

        def __init__(self) -> None:
            raise NotImplementedError("ckdl not installed")

    class Node:
        """Node class: requires ckdl to be installed."""

        pass


__all__ = ["parse", "Document", "Node", "KdlParseError"]
