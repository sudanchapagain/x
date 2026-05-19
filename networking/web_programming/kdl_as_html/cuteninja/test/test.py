from cuteninja import render_kdl, KdlTemplate


def test_empty_input():
    kdl = ""
    html = render_kdl(kdl)

    assert html == ""


def test_only_whitespace():
    kdl = "   \n  \n  "
    html = render_kdl(kdl)

    assert isinstance(html, str)


def test_only_jinja():
    kdl = "{% if true %}{% endif %}"
    html = render_kdl(kdl)

    assert "{% if true %}" in html
    assert "{% endif %}" in html


def test_unicode_content():
    kdl = 'p "Hello 世界 🌍"'
    html = render_kdl(kdl, format_output=False)

    assert "Hello 世界 🌍" in html
    assert "<p>" in html


def test_long_content():
    long_text = "A" * 10000
    kdl = f'p "{long_text}"'
    html = render_kdl(kdl, format_output=False)

    assert long_text in html


def test_simple_node():
    kdl = "div"
    html = render_kdl(kdl, format_output=False)
    assert "<div></div>" in html


def test_node_with_args():
    kdl = 'p "Hello World"'
    html = render_kdl(kdl, format_output=False)
    assert "<p>Hello World</p>" in html


def test_node_with_props():
    kdl = 'button label="Click me"'
    html = render_kdl(kdl, format_output=False)
    assert 'label="Click me"' in html
    assert "<button" in html and "</button>" in html


def test_nested_nodes():
    kdl = """
    div {
        p "Hello"
    }
    """
    html = render_kdl(kdl, format_output=False)
    assert "<div>" in html
    assert "<p>Hello</p>" in html
    assert "</div>" in html


def test_doctype():
    kdl = "!doctype html"
    html = render_kdl(kdl, format_output=False)
    assert "<!DOCTYPE html>" in html


def test_jinja_syntax():
    kdl = 'p "{{ message }}"'
    html = render_kdl(kdl, format_output=False)
    assert "{{ message }}" in html
    assert "<p>" in html


def test_jinja_control_flow():
    kdl = """
    div {
        {% if show %}
        p "Visible"
        {% endif %}
    }
    """
    html = render_kdl(kdl, format_output=False)
    assert "{% if show %}" in html
    assert "{% endif %}" in html
    assert "<div>" in html
    assert "<p>Visible</p>" in html


def test_deeply_nested_nodes():
    kdl = """
    div {
        div {
            div {
                div {
                    div {
                        p "Deep"
                    }
                }
            }
        }
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert html.count("<div>") == 5
    assert html.count("</div>") == 5
    assert "Deep" in html


def test_many_siblings():
    kdl = "\n".join([f'p "Item {i}"' for i in range(100)])
    html = render_kdl(kdl, format_output=False)

    assert html.count("<p>") == 100
    assert "Item 0" in html
    assert "Item 99" in html


def test_jinja_with_curly_braces():
    kdl = "p \"{{ {'key': 'value'} }}\""
    html = render_kdl(kdl, format_output=False)

    assert "{{" in html and "}}" in html


def test_multiple_spaces_in_args():
    kdl = 'p "Hello"   "World"'
    html = render_kdl(kdl, format_output=False)

    assert "Hello" in html
    assert "World" in html


def test_numeric_args():
    kdl = "data-value value=42 price=99.99"
    html = render_kdl(kdl, format_output=False)

    assert 'value="42"' in html
    assert 'price="99.99"' in html


def test_null_values():
    kdl = 'div value="null"'
    html = render_kdl(kdl, format_output=False)

    assert "<div" in html


def test_mixed_quote_types():
    kdl = """p "double quotes" """
    html = render_kdl(kdl, format_output=False)

    assert "double quotes" in html


def test_jinja_raw_blocks():
    kdl = """
    div {
        {% raw %}
        p "{{ this_is_literal }}"
        {% endraw %}
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "{% raw %}" in html
    assert "{% endraw %}" in html
    assert "{{ this_is_literal }}" in html


def test_jinja_in_node_names():
    kdl = 'div id="test" "{{ content }}"'
    html = render_kdl(kdl, format_output=False)

    assert "<div" in html
    assert 'id="test"' in html
    assert "{{ content }}" in html


def test_class_template():
    kdl = 'p "{{ message }}"'
    template = KdlTemplate(kdl, format_output=False)
    html = template.render()

    assert "{{ message }}" in html

    tokens = template.get_jinja_tokens()

    assert isinstance(tokens, dict)


def test_special_html_entities():
    kdl = 'p "< > &"'
    html = render_kdl(kdl, format_output=False)

    assert "< > &" in html


def test_multiline_jinja():
    kdl = """
    div {
        {%
            if very_long_condition
            and another_condition
        %}
        p "Content"
        {% endif %}
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "very_long_condition" in html
    assert "another_condition" in html
    assert "{% endif %}" in html


def test_jinja_set_statements():
    kdl = """
    div {
        {% set name = "Alice" %}
        p "{{ name }}"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert '{% set name = "Alice" %}' in html
    assert "{{ name }}" in html


def test_comment_nodes():
    kdl = """
    div {
        - "regular text"
        p "paragraph"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "regular text" in html
    assert "<p>paragraph</p>" in html


def test_complex_nested_jinja():
    kdl = """
    div {
        {% for item in items %}
        ul {
            {% if item.visible %}
            li "{{ item.name }}"
            {% for tag in item.tags %}
            span "{{ tag }}"
            {% endfor %}
            {% endif %}
        }
        {% endfor %}
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "{% for item in items %}" in html
    assert "{% if item.visible %}" in html
    assert "{{ item.name }}" in html
    assert "{% endfor %}" in html
    assert "{% endif %}" in html
    assert "<div>" in html and "</div>" in html
    assert "<ul>" in html and "</ul>" in html


def test_jinja_in_attributes():
    kdl = 'a href="{{ url }}" class="{{ css_class }}" "Click me"'
    html = render_kdl(kdl, format_output=False)

    assert 'href="{{ url }}"' in html
    assert 'class="{{ css_class }}"' in html
    assert ">Click me</a>" in html


def test_jinja_comments():
    kdl = """
    div {
        {# This is a comment #}
        p "Content"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "{# This is a comment #}" in html
    assert "<p>Content</p>" in html


def test_multiple_args():
    kdl = 'p "Hello" "World" "!"'
    html = render_kdl(kdl, format_output=False)

    assert "<p>Hello World !</p>" in html


def test_void_elements():
    kdl = """
    div {
        br
        hr
        img src="test.jpg"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "<br />" in html or "<br>" in html
    assert "<hr />" in html or "<hr>" in html
    assert 'src="test.jpg"' in html


def test_special_characters_in_content():
    kdl = 'p "Hello & goodbye"'
    html = render_kdl(kdl, format_output=False)

    assert "<p>Hello & goodbye</p>" in html


def test_quotes_in_attributes():
    kdl = 'p title="Say \\"hello\\""'
    html = render_kdl(kdl, format_output=False)

    assert "title=" in html


def test_text_only_node():
    kdl = """
    p {
        - "This is plain text"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "<p>This is plain text</p>" in html


def test_full_html_document():
    kdl = """
    !doctype html
    html lang=en {
        head {
            meta charset=utf-8
            title "{{ page_title }}"
        }
        body {
            header {
                h1 "{{ site_name }}"
            }
            main {
                {% for post in posts %}
                article {
                    h2 "{{ post.title }}"
                    p "{{ post.content }}"
                }
                {% endfor %}
            }
            footer {
                p "© {{ year }}"
            }
        }
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "<!DOCTYPE html>" in html
    assert '<html lang="en">' in html
    assert '<meta charset="utf-8"' in html
    assert "{{ page_title }}" in html
    assert "{{ site_name }}" in html
    assert "{% for post in posts %}" in html
    assert "{{ post.title }}" in html
    assert "{% endfor %}" in html
    assert "</html>" in html


def test_jinja_macros():
    kdl = """
    div {
        {% macro render_item(item) %}
        span "{{ item }}"
        {% endmacro %}
        p "{{ render_item('test') }}"
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "{% macro render_item(item) %}" in html
    assert "{% endmacro %}" in html
    assert "{{ render_item('test') }}" in html


def test_jinja_extends_and_blocks():
    kdl = """
    {% extends "base.html" %}
    {% block content %}
    div {
        p "{{ message }}"
    }
    {% endblock %}
    """
    html = render_kdl(kdl, format_output=False)

    assert '{% extends "base.html" %}' in html
    assert "{% block content %}" in html
    assert "{% endblock %}" in html
    assert "{{ message }}" in html


def test_multiple_doctype_values():
    kdl = '!doctype "html PUBLIC"'
    html = render_kdl(kdl, format_output=False)

    assert "<!DOCTYPE html PUBLIC>" in html


def test_formatting_preserved():
    kdl = """
    div {
        p "Hello"
    }
    """
    html_formatted = render_kdl(kdl, format_output=True)
    html_compact = render_kdl(kdl, format_output=False)

    assert "\n" in html_formatted
    assert "    " in html_formatted

    assert len(html_compact) < len(html_formatted)


def test_mixed_jinja_and_content():
    kdl = 'p "Hello {{ name }}, welcome to {{ site }}!"'
    html = render_kdl(kdl, format_output=False)

    assert "Hello {{ name }}, welcome to {{ site }}!" in html
    assert "<p>" in html and "</p>" in html


def test_jinja_filters():
    kdl = 'p "{{ name|upper }}"'
    html = render_kdl(kdl, format_output=False)

    assert "{{ name|upper }}" in html


def test_jinja_tests():
    kdl = """
    div {
        {% if user is defined %}
        p "User exists"
        {% endif %}
    }
    """
    html = render_kdl(kdl, format_output=False)

    assert "{% if user is defined %}" in html
    assert "{% endif %}" in html
