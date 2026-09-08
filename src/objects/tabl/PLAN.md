# TABL DDL Completion Plan

## Current Status

`zcl_abapgit_object_tabl_ddl` is an experimental converter between classic
TABL metadata and source-based `define table` DDL. Serialization covers much
more of the syntax than deserialization, so the class is not round-trip safe.
It also has no production caller: `zcl_abapgit_object_tabl` still reads and
writes TABL objects through `lcl_tabl_xml`.

Do not integrate the DDL path into the TABL object handler until the supported
metadata has round-trip tests and unsupported metadata has an explicit storage
or rejection policy.

## Known Gaps

### Deserialization correctness

- [ ] Parse and validate the `define table <name>` header, and set
      `dd02v-tabname`. The current implementation only finds the braces.
- [ ] Fix body extraction. The current length calculation drops the character
      immediately before `}` and only works when that character is disposable
      whitespace.
- [ ] Replace `SPLIT ... AT ';'` and space-based tokenization with a small lexer
      that respects quoted strings, escaped quotes, line breaks, comments, and
      semicolons or colons inside annotation values.
- [ ] Replace parser `ASSERT` and `WRITE` fallbacks with a typed parse error that
      identifies the unsupported token or annotation and its location.
- [ ] Preserve significant whitespace inside quoted labels. `CONDENSE` currently
      changes text such as `'Two  spaces'`.

### Top-level annotations

- [ ] Make `parse_top_annotations` symmetric with `serialize_top`:
  - Map all enhancement categories to `dd02v-exclass`. The existing
    `#NOT_EXTENSIBLE` branch incorrectly writes `dd02v-contflag`.
  - Support `#TRANSPARENT` and `#GLOBAL_TEMPORARY` table categories, including
    the release-dependent `IS_GTT` component.
  - Support activation types `#NAMETAB_GENERATION_OFFLINE` and
    `#ADAPT_C_STRUCTURES`.
  - Support data maintenance values `#ALLOWED`, `#LIMITED`, and `#NOT_ALLOWED`.
  - Support `@AbapCatalog.primaryKey.invertedHashIndex` when the corresponding
    release-dependent component exists.
  - Validate and preserve delivery class and the end-user label.
- [ ] Define behavior for valid annotations that this converter does not own:
      preserve them, reject them with a useful error, or explicitly ignore them.

### Fields and types

- [ ] Remove the unconditional return immediately after `parse_type`. It makes
      all states for `not null` and following clauses unreachable.
- [ ] Parse `not null`, and initialize the DD03P fields required by subsequent
      DDIC expansion/activation.
- [ ] Verify the legal `key key : ...` case with a test instead of treating the
      second `key` token ambiguously.
- [ ] Make `parse_type` symmetric with `serialize_type`:
  - Parse data element references.
  - Parse one-parameter built-in types and retain their length.
  - Parse length/decimal pairs for `DEC`, `CURR`, `QUAN`, `DF16_DEC`, and
    `DF34_DEC`.
  - Parse parameterless built-in types such as integer, date/time, client,
    currency key, and decfloat variants.
  - Populate `datatype`, `leng`, `decimals`, `inttype`, and `intlen` consistently.
- [ ] Parse include declarations, named includes, `with suffix`, include
      `not null`, and the generated administrative rows needed by DDIC.

### Field annotations and relationships

- [ ] Refactor `parse_field_annotations` so annotations can update both the
      pending DD03P field and its related DD08V record. It currently returns a
      DD08V value that `parse_field` never appends, so even recognized foreign
      key metadata is discarded.
- [ ] Parse every annotation emitted by `serialize_field_annotations`:
      end-user label, text language, amount/currency reference,
      quantity/unit reference, and decfloat output style.
- [ ] Complete foreign-key annotation parsing:
  - Handle label, key type, screen check, message class, and message number.
  - Reverse the serializer mappings `#TEXT_KEY -> TEXT` and `#NON_KEY -> REF`.
  - Map both `screenCheck` boolean values back to `dd08v-checkflag`.
- [ ] Parse `with foreign key`, cardinality, check table, constant conditions,
      and multi-line `where`/`and` conditions into DD08V and ordered DD05M rows.
- [ ] Parse `with value help` and its conditions into DD35V/DD36M.
- [ ] Parse include `extend` blocks, including foreign-key/value-help override
      and `remove foreign key` / `remove value help` variants.

### Serialization hardening

- [ ] Replace serializer `ASSERT` fallbacks for unknown enhancement categories,
      table categories, and foreign-key cardinalities with explicit supported
      mappings or actionable exceptions.
- [ ] Never emit an incomplete annotation. The unknown `mainflag` branch
      currently emits `@AbapCatalog.dataMaintenance :` without a value.
- [ ] Sort foreign-key conditions by `primpos` before output instead of relying
      on the incoming DD05M order.
- [ ] Resolve currency/quantity semantics from the referenced field type rather
      than assuming `CURR` or field name `WAERS` is sufficient.
- [ ] Verify literal escaping in foreign-key and value-help conditions.
- [ ] Compare manual output with `serialize_adt` for representative tables and
      document intentional formatting or release differences.
- [ ] Decide whether the temporary `read_data` helper belongs in the public API;
      if retained, accept a language and replace its `ASSERT sy-subrc = 0` with
      normal error propagation.

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

- [ ] Replace the unfinished `* todo, check xml result` in the ABAP Unit helper
      with assertions over normalized, supported metadata.
- [ ] Assert both directions for every fixture:
      `internal -> DDL -> internal` and `DDL -> internal -> DDL`.
- [ ] Compare fields individually where DDIC expansion derives fields that are
      not present verbatim in DDL; do not hide losses with broad XML suppression.

### Required cases

- [ ] All top-level annotation values emitted by `serialize_top`.
- [ ] Every built-in type emitted by `serialize_type`, plus data element types,
      lengths, decimals, nullable fields, and key fields.
- [ ] A field literally named `key`.
- [ ] Plain, named, suffixed, and non-null includes.
- [ ] Every emitted field annotation.
- [ ] Every supported foreign-key cardinality, fixed-value condition,
      table-field condition, and multiple ordered conditions.
- [ ] Value helps and include extensions, including remove/override forms.
- [ ] Labels containing escaped quotes, repeated spaces, colons, and semicolons.
- [ ] Formatting variants, blank lines, comments, and malformed input with
      stable error assertions.
- [ ] Release-dependent fields such as `IS_GTT` and `PK_IS_INVHASH`.

Use small focused unit fixtures for grammar rules and a separate set of
real-world ADT fixtures for compatibility coverage.

## Delivery Order

1. Add failing round-trip characterization tests for the existing two fixtures.
2. Introduce robust tokenization and typed parse errors.
3. Complete header, top-annotation, field, nullability, and type parsing.
4. Complete field annotations, foreign keys, value helps, includes, and extends.
5. Harden serialization and establish the metadata boundary/companion format.
6. Validate against ADT output on supported SAP releases.
7. Decide the integration and migration strategy for the main TABL handler,
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