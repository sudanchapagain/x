import re
from typing import Dict, Pattern, Tuple


class JinjaProcessor:
    """Processor for extracting Jinja from KDL and subsequently restoring it in HTML."""

    token_map: Dict[str, str]
    token_counter: int

    def __init__(self) -> None:
        self.token_map = {}
        self.token_counter = 0

    def extract_jinja(self, source: str) -> Tuple[str, Dict[str, str]]:
        """Extract Jinja blocks from source and replace with placeholders.

        Args:
            source: KDL source with Jinja syntax

        Returns:
            Tuple of cleaned KDL and mapping of placeholders to tokens
        """
        self.token_map = {}
        self.token_counter = 0

        # pattern to match Jinja syntax: {{ }}, {% %}, {# #}
        pattern = r"(\{\{.*?\}\}|\{%.*?%\}|\{#.*?#\})"

        def replace_jinja(match: re.Match[str]) -> str:
            token = match.group(0)
            placeholder = f"__JINJA_{self.token_counter}__"
            self.token_map[placeholder] = token
            self.token_counter += 1

            # If the token contains newlines then wrap it as a KDL text node
            if "\n" in token:
                return f'- "{placeholder}"'
            return placeholder

        # replace all Jinja blocks with placeholders
        cleaned = re.sub(pattern, replace_jinja, source, flags=re.DOTALL)

        # handle standalone single-line Jinja blocks
        lines = cleaned.split("\n")
        final_lines: list[str] = []

        for line in lines:
            stripped = line.strip()
            # check if line is only a placeholder (i.e. standalone block)
            if re.fullmatch(r"__JINJA_\d+__", stripped):
                # to KDL text node
                indent = len(line) - len(line.lstrip())
                final_lines.append(" " * indent + f'- "{stripped}"')
            else:
                final_lines.append(line)

        return "\n".join(final_lines), self.token_map

    def restore_jinja(self, html: str, token_map: Dict[str, str]) -> str:
        """Restore Jinja blocks from placeholders in HTML.

        Args:
            html: HTML with placeholders
            token_map: Mapping of placeholders to Jinja tokens

        Returns:
            HTML with restored Jinja syntax
        """
        result = html
        for placeholder, token in token_map.items():
            result = result.replace(placeholder, token)
        return result

    def get_source_map(self) -> Dict[str, str]:
        """Get the current token mapping.

        Returns:
            Mapping of placeholders to Jinja tokens
        """
        return self.token_map
