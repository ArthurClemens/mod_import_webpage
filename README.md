# Import Webpage Module for Zotonic

Facilitates creating a resource from another web page.

## Dependencies

[mochiweb_xpath](https://github.com/retnuh/mochiweb_xpath)

Add to your `zotonic.config` file:

```
{mochiweb_xpath, ".*", {git, "git://github.com/retnuh/mochiweb_xpath.git", {branch, "master"}}}
```