Jinja is the only templating engine that ticks all the boxes. It has full
logic while being safe. It is not XML or indent-based, which generally causes
more friction. Jinja is supported outside of Python, as derivatives inspired or
cloned from Jinja exist in other languages. Also, Jinja feels natural,
primarily due to familiarity but also because of how much it has inspired
and influenced other templating systems. It exists everywhere.

```jinja
<ul>
  {% for user in users %}
    <li>{{ user.name }}</li>
  {% endfor %}
</ul>
```

However, Jinja is not the end-all and be-all. If using Laravel, then `blade` is
the way to go. Similarly, for Django, use `DTL`; for Spring Boot, use `thymeleaf`;
and so on. But, if you are going bare-bones, then picking something closer to
Jinja makes sense. For JavaScript, use `Nunjucks`; for PHP, use `twig` or
`smarty`; for Java, use `pebble`.

Popular alternatives exist, such as `handlebars` and `mustache`, but they are
logic-less. They fit a particular use case. You can choose not to use logic in
full-logic template systems, but you cannot add logic to logic-less ones. If
you primarily handle user data and require maximum safety, then `liquid` is a
good choice. My idea is that the alternatives fit particular cases, but for
general-purpose use, Jinja is the happy path.

## basic jinja

### Basic usage

- variable x has content: `{{ x }}`
- expression: `{{ x + 1 }}`
- escaped for HTML: `{{ x | e }}`

### Control structures

```jinja
{% for x in range(5) %}
    {% if x % 2 == 0 %}
        {{ x }} is even!
    {% else %}
        {{ x }} is odd!
    {% endif %}
{% endfor %}
```

### whitespace trimming

```jinja
these are
{{ "three" }}
lines.

this is conc
{{- "at" -}}
enated.
```

### special blocks

```jinja
{% filter e %}
{% raw %}
    This is a raw block where {{nothing is evaluated}}
    {% not even this %}
    and <html is escaped> too with "e" filter
{% endraw %}
{% endfilter %}

{% macro myfunc(x) %}
    this is a reusable macro, with arguments: {{x}}
{% endmacro %}

{{ myfunc(42) }}

{#
this is a comment
#}
```

### Inheritance

`shared.html`

```jinja
<html>
  <head>
    <title>{%block title %}{% endblock %}</title>
  </head>
  <body>
    <header><h1>{% block title %}{% endblock %}</h1></header>
    <main>{% block content %}{% endblock %}</main>
  </body>
</html>
```

`home.html`

```jinja
{% extends "shared.html" %}
{% block title %}Welcome to my site{% endblock %}
{% block content %}
This is the body
{% endblock %}
```

### Basic usage

```py
from jinja2 import Template
template = Template('Hello {{ name }}!')
template.render(name='John Doe') == u'Hello John Doe!'
```
