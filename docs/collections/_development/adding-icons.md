---
title: Adding icons
order: 90
---

abapGit uses SIL and MIT licensed fonts, they are included in the distribution as a MIME object. This includes MIME objects:

- ZABAPGIT_ICON_FONT - font in WOFF format containing the custom icon set. WOFF is used due to compatibility, SAP GUI for Windows reuse IE (11)
- ZABAPGIT_ICON_FONT_CSS - CSS icon map

To add an icon to the icon set follow the next procedure:

1. Add the icon to repo [abapGit/icon-font](https://github.com/abapGit/icon-font) as documented [here](https://github.com/abapGit/icon-font/blob/master/README.md)
2. Get the new abapGit font and icon files from the icon-font GitHub action 
3. Go to transaction `SMW0` and display the abapGit MIME objects (Binary > Package $abapgit)
4. Upload the new font file to ZABAPGIT_ICON_FONT
5. Upload the new CSS file to ZABAPGIT_ICON_FONT_CSS
6. Start abapGit and commit the changes the [abapGit/abapGit](https://github.com/abapGit/abapGit) repo (preferably in a separate commit)

Please **mind the licensing** when adding icons not from Font Awesome.

P.S. Alternatively you can use [w3mipoller](https://github.com/sbcgua/abap_w3mi_poller) for upload step. See also [UI - CSS and assets](./developing-ui-css.html).
