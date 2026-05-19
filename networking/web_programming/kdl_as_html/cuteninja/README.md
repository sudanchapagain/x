# cuteninja

A python package that allows you to use [KDL](https://kdl.dev) as the markup
with Jinja2 syntax support.

Why? Because KDL is much more readable than HTML. While looking through
the KDL repository I found a example file that used KDL as an alternative
of HTML. That was what give me the initial idea of using KDL as the markup
with Jinja in Python for succint and maintainable template.

This packages doesn't do much, it extract the Jinja syntax from source files
and then parses the KDL via kdl-rs which generates the HTML. The previously
extracted Jinja syntax is restored and returned as valid HTML with Jinja
syntax. i.e.

```kdl
!DOCTYPE html
html lang=en {
    head {
        title "{{ page_title }}"
    }
    body {
        h1 "Hello, {{ user.name }}!"
        {% if user.is_authenticated %}
        div class=content {
            p "You are logged in."
        }
        {% else %}
        div class=login {
            a href="/login" "Please log in"
        }
        {% endif %}
    }
}
```

is turned to the following:

```jinja
<!DOCTYPE html>
<html lang="en">
    <head>
        <title>{{ page_title }}</title>
    </head>
    <body>
        <h1>Hello, {{ user.name }}!</h1>
        {% if user.is_authenticated %}
        <div class="content">
            <p>You are logged in.</p>
        </div>
        {% else %}
        <div class="login">
            <a href="/login">Please log in</a>
        </div>
        {% endif %}
    </body>
</html>
```

## License

MIT

## Credits

- [KDL Document Language](https://kdl.dev)
- [kdl-rs](https://github.com/kdl-org/kdl-rs)
