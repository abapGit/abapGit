---
title: Adding icons
order: 90
---

abapGit uses SIL and MIT licensed fonts, they are included in the distribution as a MIME object. This includes 3 MIME objects:

- ZABAPGIT_ICON_FONT - font in WOFF format containing the custom icon set. WOFF is used due to compatibility, SAP GUI for Windows reuse IE (11)
- ZABAPGIT_ICON_FONT_CONFIG - font config for fontello (see details below)
- ZABAPGIT_ICON_FONT_CSS - CSS icon map

To add an icon to the icon set follow the next procedure:

1. Download font config with SMW0
2. Go to http://fontello.com/ and import the config file (see the screen below) - the icons currently used will be automatically selected
3. Select additional icons to include, use only SIL and MIT licensed fonts
4. Press `Download webfont` button - this will download zip file
5. Extract `config.json`, `css\ag-icons.css` and `font\ag-icons.woff`
6. Adjust `ag-icons.css`: replace `src` parameter in font-face tag at the beginning with `src: url('../font/ag-icons.woff') format('woff');` (in-sap browser seems to wrongly interpret `?` param in path of fonts, also we use just woff font so don't need others). Also remove `Chrome hack...` comment
7. Upload all 3 files to the system via SMW0 to the respective objects
8. Double-check the `LICENSE.txt` from the zip, potentially update AG documentation if you used a font, that was not used before
9. Preferable commit the change in a separate commit

![Import config at fontello](../img/fontello-import.png)

P.S. Alternatively you can use [w3mipoller](https://github.com/sbcgua/abap_w3mi_poller) for download/upload
