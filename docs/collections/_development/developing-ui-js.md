---
title: UI - Java script
order: 93
---

This doc covers java script specifics in abapGit UI. See also the [UI - HTML pages](./developing-ui.html).

## General

abapGit UI contains JS code. Some of dynamic features relate on it e.g. keyboard navigation. The JS code is located in `ui/zabapgit_js_common.w3mi.data.js` - the recommended way to modify it described in "Recommended asset development flow" section of [UI - CSS and assets](./developing-ui-css.html).

As SAP GUI uses Internet Explorer component to render HTML the JS code must be optimized for IE11 (and use the features available in it). Although some polyfills are available (and more can be added) at the beginning of the code (like `String.includes`).

The pull request CI check includes a run of eslint, so new code should confirm to the rules defined for the abapGit repository.

## Components

The JS library contains several components which can be reused in different places.

### Command Palette

To add a command palette add the following code in the `script` method of the page.

```abap
ro_html->add( 'var gCommandPalette = new CommandPalette(enumerateFn, {' ).
ro_html->add( '  toggleKey: "F1",' ).
ro_html->add( '  hotkeyDescription: "Command ..."' ).
ro_html->add( '});' ).
```

where:
- `enumerateFn` is a function that returns list of commands in the form of array of
```js
{
    action:    "go_home",        // sapevent action or js function
    iconClass: "icon icon-star", // class for item icon, OPTIONAL !!!
    title:     "Go home"         // title of the command
}
```
- `toggleKey` is a key to toggle the palette. `"^"` at the beginning requires `Ctrl` (`"^g" = Ctrl+g` )
- `hotkeyDescription` is the description which is a) added to the shortkey help popup b) used as placeholder in the command palette

See example of enumerators - `enumerateToolbarActions` and `enumerateTocAllRepos`.

### to do

- `debugOutput`
- `submitSapeventForm`
- `setInitialFocus`
- `setInitialFocusWithQuerySelector`
- `submitFormById`
- `findStyleSheetByName`
- `getIndocStyleSheet`
- `toggleDisplay`
- `Hotkeys.addHotkeyToHelpSheet`
- ...