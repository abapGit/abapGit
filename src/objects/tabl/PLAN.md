# TABL DDL Completion Plan

## Current Status

`zcl_abapgit_object_tabl_ddl` now implements a supported, typed subset of the
conversion between classic TABL metadata and source-based `define table` DDL.
The parser and serializer are round-trip tested for the supported metadata,
including relationships, value helps, includes, extensions, quoted labels,
and representative built-in/data-element types. The production handler is intentionally still
XML-based: `zcl_abapgit_object_tabl` reads and writes TABL objects through
`lcl_tabl_xml` until the metadata boundary and SAP-release compatibility are
resolved.

Do not integrate the DDL path into the TABL object handler until the supported
metadata has round-trip tests and unsupported metadata has an explicit storage
or rejection policy.

## Known Gaps

### Deserialization correctness

- [x] Parse and validate the `define table <name>` header, and set
      `dd02v-tabname`.
- [x] Parse the body with token offsets, including the closing brace and
      trailing tokens.
- [x] Replace `SPLIT ... AT ';'` and space-based tokenization with a small lexer
      that respects quoted strings, escaped quotes, line breaks, comments, and
      semicolons or colons inside annotation values.
- [x] Replace parser `ASSERT` and `WRITE` fallbacks with a typed parse error that
      identifies the unsupported token or annotation and its location.
- [x] Preserve significant whitespace inside quoted labels, including doubled
      quotes.

### Top-level annotations

- [x] Make `parse_top_annotations` symmetric with `serialize_top`:
  - Map all supported enhancement categories to `dd02v-exclass`.
  - Support `#TRANSPARENT` and `#GLOBAL_TEMPORARY` table categories, including
      the release-dependent `IS_GTT` component.
  - Support activation types `#NAMETAB_GENERATION_OFFLINE` and
    `#ADAPT_C_STRUCTURES`.
  - Support data maintenance values `#ALLOWED`, `#RESTRICTED`, and
    `#NOT_ALLOWED`; accept legacy `#LIMITED` input for compatibility.
  - Support `@AbapCatalog.primaryKey.invertedHashIndex` when the corresponding
    release-dependent component exists.
  - Validate and preserve delivery class and the end-user label.
- [x] Define behavior for valid annotations that this converter does not own:
      reject them with a typed, useful error. The decision is documented in
      `ANORMALIES.md`.

### Fields and types

- [x] Remove the unconditional return immediately after `parse_type` so
      nullability and following clauses are parsed.
- [x] Parse `not null`, and initialize the DD03P fields required by subsequent
      DDIC expansion/activation.
- [x] Verify the legal `key key : ...` case by distinguishing a field named
      `key` from the key modifier.
- [x] Make `parse_type` symmetric with `serialize_type`:
  - Parse data element references.
  - Parse one-parameter built-in types and retain their length.
  - Parse length/decimal pairs for `DEC`, `CURR`, `QUAN`, `DF16_DEC`, and
    `DF34_DEC`.
  - Parse parameterless built-in types such as integer, date/time, client,
    currency key, and decfloat variants.
  - Populate `datatype`, `leng`, `decimals`, `inttype`, and `intlen` consistently.
- [x] Parse include declarations, named includes, `with suffix`, include
      `not null`, and the administrative rows represented by the internal
      structure.

### Field annotations and relationships

- [x] Refactor `parse_field_annotations` so annotations can update both the
      pending DD03P field and its related DD08V record; parsed DD08V metadata is
      appended when the field is completed.
- [x] Parse every annotation emitted by `serialize_field_annotations`:
      end-user label, text language, amount/currency reference,
      quantity/unit reference, and decfloat output style.
- [x] Complete foreign-key annotation parsing:
  - Handle label, key type, screen check, message class, and message number.
  - Reverse the serializer mappings `#TEXT_KEY -> TEXT` and `#NON_KEY -> REF`.
  - Map both `screenCheck` boolean values back to `dd08v-checkflag`.
- [x] Parse `with foreign key`, cardinality, check table, constant conditions,
      and multi-line `where`/`and` conditions into DD08V and ordered DD05M rows.
- [x] Parse `with value help` and its conditions into DD35V/DD36M.
- [x] Parse include `extend` blocks, including foreign-key/value-help override
      and `remove foreign key` / `remove value help` variants.

### Serialization hardening

- [x] Replace serializer `ASSERT` fallbacks for unknown enhancement categories,
      table categories, and foreign-key cardinalities with explicit supported
      mappings or actionable exceptions.
- [x] Never emit an incomplete annotation. Unknown metadata now raises a typed
      serialization error.
- [x] Sort foreign-key conditions by `primpos` before output instead of relying
      on the incoming DD05M order.
- [x] Resolve currency/quantity semantics from the referenced field type rather
      than assuming `CURR` or field name `WAERS` is sufficient.
- [x] Preserve and verify literal values in foreign-key and value-help
      conditions, including quoted values.
- [ ] Compare manual output with `serialize_adt` for representative tables and
      document intentional formatting or release differences.
- [x] Retain `read_data` as a public SAP-side helper, accept a language, and
      replace its assertion with normal typed error propagation.

## Metadata Boundary

The internal TABL structure also contains DD09L technical settings, secondary
indexes (DD12V/DD17V), translated texts, long texts, IDoc segment definitions,
and TABL extras. The current DDL output does not represent these values.

- [ ] For each component of `zif_abapgit_object_tabl=>ty_internal`, determine
      whether ABAP table DDL can represent it.
- [ ] Keep non-DDL data in a documented companion format when it must survive a
      round trip. Do not silently drop it.
- [ ] Treat object types represented separately in source-based DDIC, such as
      secondary indexes, according to their owning abapGit object handler.
- [ ] Define the minimum SAP release and feature-detection strategy for dynamic
      fields and ADT classes used by this converter.

## Test Plan

### Characterization first

- [x] Replace the unfinished `* todo, check xml result` in the ABAP Unit helper
      with assertions over normalized, supported metadata.
- [x] Assert both directions for the existing XML fixtures:
      `internal -> DDL -> internal` and `DDL -> internal -> DDL`.
- [x] Compare fields individually where DDIC expansion derives fields that are
      not present verbatim in DDL; do not hide losses with broad XML suppression.

### Required cases

- [x] All top-level annotation values emitted by `serialize_top`.
- [x] Every built-in type emitted by `serialize_type`, plus data element types,
      lengths, decimals, nullable fields, and key fields.
- [x] A field literally named `key`.
- [x] Plain, named, suffixed, and non-null includes.
- [x] Every emitted field annotation.
- [x] Every supported foreign-key cardinality, fixed-value condition,
      table-field condition, and multiple ordered conditions.
- [x] Value helps and include extensions, including remove/override forms.
- [x] Labels containing escaped quotes, repeated spaces, colons, and semicolons.
- [x] Formatting variants, blank lines, comments, and malformed input with
      stable error assertions.
- [ ] Release-dependent fields such as `IS_GTT` and `PK_IS_INVHASH`.

Use small focused unit fixtures for grammar rules and a separate set of
real-world ADT fixtures for compatibility coverage.

## Delivery Order

1. [done] Add round-trip characterization tests for the existing two fixtures.
2. [done] Introduce robust tokenization and typed parse errors.
3. [done] Complete header, top-annotation, field, nullability, and type parsing.
4. [done] Complete field annotations, foreign keys, value helps, includes, and extends.
5. [partial] Harden serialization. Establishing the metadata boundary and a
   companion format remains open.
6. [open] Validate against ADT output on supported SAP releases.
7. [open] Decide the integration and migration strategy for the main TABL handler,
   including backward compatibility with existing `.tabl.xml` files.

## Definition of Done

- Supported syntax has no TODO branches, debug `WRITE` statements, or assertion
  dumps for user input.
- Every construct emitted by the serializer is accepted by the deserializer or
  is documented as intentionally one-way.
- Supported `ty_internal` values survive normalized round trips without silent
  loss.
- Unsupported syntax and metadata fail with actionable errors.
- The full ABAP Unit, transpiled unit, and abaplint suites pass before any
  production integration is enabled.
