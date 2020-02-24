---
title: Adding icons
order: 90
---

abapGit uses SIL and MIT licensed fonts, they are included in the distribution as a MIME object. This includes MIME objects:

- ZABAPGIT_ICON_FONT - font in WOFF format containing the custom icon set. WOFF is used due to compatibility, SAP GUI for Windows reuse IE (11)
- ZABAPGIT_ICON_FONT_CSS - CSS icon map

To add an icon to the icon set follow the next procedure:

1. You need `nodejs` installed (version 10 at the time of this doc creation)
2. Fork/clone this repo https://github.com/abapGit/icon-font
3. Run `npm install` inside
4. Download [Font Awesome free version](https://fontawesome.com/download)
5. Copy new SVGs to `svg` folder of the repo
6. Run `npm run build`, this will generate `woff` and `css` files in the `build` dir
7. Upload artifacts to the system via SMW0 to the respective objects
8. Preferable commit the change in a separate commit
9. Commit changes to `icon-font` repo too (newly included SVGs)

Please **mind the licensing** when adding icons not from Font Awesome

P.S. Alternatively you can use [w3mipoller](https://github.com/sbcgua/abap_w3mi_poller) for upload step. See also [UI - CSS and assets](./developing-ui-css.html).
