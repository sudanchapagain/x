from typing import Dict

from .jinja_processor import JinjaProcessor
from .kdl_converter import KdlToHtmlConverter


class KdlTemplate:
    original_source: str
    format_output: bool
    jinja_processor: JinjaProcessor
    converter: KdlToHtmlConverter
    token_map: Dict[str, str]
    output: str

    def __init__(self, source: str, format_output: bool = True) -> None:
        self.original_source = source
        self.format_output = format_output
        self.jinja_processor = JinjaProcessor()
        self.converter = KdlToHtmlConverter(format_output=format_output)

        self._process()

    def _process(self) -> None:
        from .kdl_bindings import parse

        cleaned_kdl, self.token_map = self.jinja_processor.extract_jinja(
            self.original_source
        )

        doc = parse(cleaned_kdl)

        html_with_placeholders = self.converter.convert_document(doc.nodes)

        self.output = self.jinja_processor.restore_jinja(
            html_with_placeholders, self.token_map
        )

    def render(self) -> str:
        return self.output

    def get_jinja_tokens(self) -> Dict[str, str]:
        return self.jinja_processor.get_source_map()


def render_kdl(source: str, format_output: bool = True) -> str:
    """Render KDL source to HTML with Jinja2 support.

    Args:
        source: KDL markup as string
        format_output: Whether to format the output with indentation

    Returns:
        HTML string with Jinja2 syntax preserved
    """
    template = KdlTemplate(source, format_output=format_output)
    return template.render()
