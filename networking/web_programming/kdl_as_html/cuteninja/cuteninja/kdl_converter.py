from typing import Any, Dict, List, Set


class KdlToHtmlConverter:
    """Convert KDL document nodes to HTML."""

    VOID_ELEMENTS: Set[str] = {
        # https://developer.mozilla.org/en-US/docs/Glossary/Void_element
        "area",
        "base",
        "br",
        "col",
        "embed",
        "hr",
        "img",
        "input",
        "link",
        "meta",
        "param",
        "source",
        "track",
        "wbr",
    }

    SPECIAL_NODES: Set[str] = {"!doctype", "-"}

    indent: str
    format_output: bool

    def __init__(self, indent: str = "    ", format_output: bool = True) -> None:
        self.indent = indent
        self.format_output = format_output

    def convert_node(self, node: Any, level: int = 0) -> str:
        """Convert a KDL node to HTML.

        Args:
            node: KDL node object
            level: Current indentation level

        Returns:
            HTML string representation of the node
        """
        current_indent = self.indent * level if self.format_output else ""
        newline = "\n" if self.format_output else ""

        node_name = node.name

        # Handle special nodes
        if node_name == "!doctype":
            doctype_value = node.args[0] if node.args else "html"
            return f"{current_indent}<!DOCTYPE {doctype_value}>{newline}"

        if node_name == "-":
            text = " ".join(str(arg) for arg in node.args)
            return text

        tag_name = node_name

        opening = f"{current_indent}<{tag_name}"

        props: Dict[str, Any] = node.properties
        for key, value in props.items():
            if isinstance(value, bool):
                if value:
                    opening += f" {key}"
            else:
                escaped_value = str(value).replace('"', "&quot;")
                opening += f' {key}="{escaped_value}"'

        if (
            tag_name.lower() in self.VOID_ELEMENTS
            and not node.args
            and not node.children
        ):
            return f"{opening} />{newline}"

        opening += ">"

        content_parts: List[str] = []

        if node.args:
            text_content = " ".join(str(arg) for arg in node.args)
            content_parts.append(text_content)

        if node.children:
            children = node.children
            has_element_children = any(
                child.name not in self.SPECIAL_NODES or child.name == "-"
                for child in children
            )

            if has_element_children and self.format_output:
                for child in children:
                    child_html = self.convert_node(child, level + 1)
                    if child.name == "-":
                        content_parts.append(child_html)
                    else:
                        content_parts.append(newline + child_html)
            else:
                for child in children:
                    content_parts.append(self.convert_node(child, level + 1))

        if content_parts:
            content = "".join(content_parts)
            if node.children and self.format_output:
                closing = f"{newline}{current_indent}</{tag_name}>{newline}"
            else:
                closing = f"</{tag_name}>{newline}"
            return f"{opening}{content}{closing}"
        else:
            return f"{opening}</{tag_name}>{newline}"

    def convert_document(self, nodes: List[Any]) -> str:
        """Convert a list of KDL nodes to HTML.

        Args:
            nodes: List of KDL node objects

        Returns:
            Complete HTML string
        """
        parts: List[str] = []
        for node in nodes:
            parts.append(self.convert_node(node, level=0))
        return "".join(parts)


def parse_and_convert(kdl_source: str, format_output: bool = True) -> str:
    """Parse KDL source and convert to HTML.

    Args:
        kdl_source: KDL markup as string
        format_output: Whether to format the output

    Returns:
        HTML string
    """
    from .kdl_bindings import parse

    doc = parse(kdl_source)
    converter = KdlToHtmlConverter(format_output=format_output)
    return converter.convert_document(doc.nodes)
