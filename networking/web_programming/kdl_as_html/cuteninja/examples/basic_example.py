from cuteninja import render_kdl
from jinja2 import Template

# KDL template with Jinja syntax
kdl_template = """
!doctype html
html lang=en {
    head {
        title "{{ page_title }}"
        meta charset=utf-8
    }
    body {
        header {
            h1 "Welcome to {{ site_name }}!"
        }
        main {
            {% if user %}
            div class=greeting {
                p "Hello, {{ user.name }}!"
                {% if user.is_admin %}
                p class=admin-notice "You have admin privileges"
                {% endif %}
            }
            {% else %}
            div class=login {
                p "Please log in to continue"
                a href="/login" "Login"
            }
            {% endif %}
            
            section class=content {
                h2 "Items"
                ul {
                    {% for item in items %}
                    li "{{ item }}"
                    {% endfor %}
                }
            }
        }
        footer {
            p "© {{ year }} {{ site_name }}"
        }
    }
}
"""

html_template = render_kdl(kdl_template)
print(html_template)

jinja = Template(html_template)

result_authenticated = jinja.render(
    page_title="Index",
    site_name="DemoSite",
    user={"name": "Sudan", "is_admin": True},
    items=["item 1", "item 2", "item 3"],
    year=2025,
)

print(result_authenticated)

result_guest = jinja.render(
    page_title="Index",
    site_name="DemoSite",
    user=None,
    items=["item 1", "item 2"],
    year=2025,
)

print(result_guest)
