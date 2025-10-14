---
name: access-tokens
internalName: accessTokens
type: StringMap
default: []
---
Access tokens used to access protected GitHub, GitLab, or
other locations requiring token-based authentication.

Access tokens are specified as a string made up of
space-separated `host=token` values.  The specific token
used is selected by matching the `host` portion against the
"host" specification of the input. The actual use of the
`token` value is determined by the type of resource being
accessed:

* Github: the token value is the OAUTH-TOKEN string obtained
  as the Personal Access Token from the Github server (see
  https://docs.github.com/en/developers/apps/building-oauth-apps/authorizing-oauth-apps).

* Gitlab: the token value is either the OAuth2 token or the
  Personal Access Token (these are different types tokens
  for gitlab, see
  https://docs.gitlab.com/12.10/ee/api/README.html#authentication).
  The `token` value should be `type:tokenstring` where
  `type` is either `OAuth2` or `PAT` to indicate which type
  of token is being specified.

Example `~/.config/nix/nix.conf`:

```
access-tokens = github.com=23ac...b289 gitlab.mycompany.com=PAT:A123Bp_Cd..EfG gitlab.com=OAuth2:1jklw3jk
```

Example `~/code/flake.nix`:

```nix
input.foo = {
  type = "gitlab";
  host = "gitlab.mycompany.com";
  owner = "mycompany";
  repo = "pro";
};
```

This example specifies three tokens, one each for accessing
github.com, gitlab.mycompany.com, and gitlab.com.

The `input.foo` uses the "gitlab" fetcher, which might
requires specifying the token type along with the token
value.
