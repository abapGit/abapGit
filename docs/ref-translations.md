---
title: Translations and i18n
category: reference
order: 11
---

There are currently two options available for including translations in a repository: 

1. Object-specific logic provided by the object serializer class
2. LXE framework (experimental, #4415) 

If no translation languages are maintained in this setting, the first approach is used.

## LXE framework (Highly experimental, use with care)

### Concept

You develop code which you want to deploy to other systems. You maintain certain set of languages. You want these languages are deployed to that another system.

### Problem with the "old" approach

The old approach was to find all translations available for the object and serialize them.
- Translation list for each object was searched in different way
- You don't have a possibility to limit the set of languages to "promised"
- Finally, not all translations are serialized e.g. screens and gui statuses of a program were not

## New approach

... uses LXE framework which supposedly identifies all translations relevant to the object. And also allow requesting specific languages to be serialized in an easy way.

## Current limitations

The LXE approach is currently limited to PROG and FUGR objects.

## Usage

- Go to repo settings, fill "Serialize Translations (experimental LXE approach)" with comma separated list of languages to serialize. E.g. "DE,ES"
- Don't fill the master language - it is serialized by default (and will be automatically removed from the list - this is normal)
- Push code as usual - non-empty list of languages will automatically activate the new logic
- Fill `*` to serialize all installed languages 

## Deployment to another system

Another system may have another set of installed languages. If this is so, only installed will be deserialized (so, intersection between installed and set for the repo). The repo may have non-installed languages in the list (supposedly for packages coming from system). Be aware that it will constantly produce translation-related diffs for repo objects
