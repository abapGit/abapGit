---
title: Personal settings
category: setup
order: 200
---

Personal settings in abapGit are valid only for the logged on user. You can maintain the
settings from the repository list or repository view by selecting "Settings > Personal".

![](img/settings-personal-0.png)

Note: Personal settings are relevant all of your repositories.

![](img/settings-personal-1.png)
![](img/settings-personal-2.png)

## Startup

If you turn on "Show Last Opened Repository", abapGit will jump directly to the repository that you worked on last.

## User Interface

You can switch the theme for abapGit between several choices: Default, Dark, and Belize. Alternatively, you can select "Synced with SAP GUI" to use the same theme
you are using for SAP GUI (availability depends on SAP GUI release).

The size of icons can be changed using "icon scaling".

"List Size" defines the maximum number of items shown in a list. If there are more items, then abapGit start paging.

## Interaction

Typically, when objects are changed, the system will display a popup to confirm the activation of objects. This gives the change to address syntax errors, for example.
If you want objects to be activated automatically, select "Activate Objects Without Popup". If there are syntax errors, abapGit will indicate this with an icon next to
the objects in the repository view.

If you are using ABAP Development Tools (ADT) in Eclipse, we recommend to turn on "Enable Jump to ABAP Development Tools". Clicking on objects names in abapGit will then
open the object in ADT (if available).

"Enable Vimium-like Link Hints" and "Key to Activate Link Hints" provide a feature similarly to the [Vimium browser](https://vimium.github.io/). After pressing the
activation key (default "f"), abapGit will uses a clever highlighting method to navigate using links.

![](img/settings-personal-3.png)

## System Resources

By default and if sufficient system resources are available, abapGit will use parallel processing to serialize objects of a repository. You can disable parallel processing
with this setting. Note: Use this setting in case you need to debug a serializer class.



