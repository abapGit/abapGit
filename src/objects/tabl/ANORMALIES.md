# TABL DDL anomalies

This file records limitations found while implementing and running the TABL
DDL converter.

## open-abap and local harness

- `2026-09-08`: `npm run build` is not PowerShell-compatible in this checkout;
  its package script invokes POSIX `rm -rf output`. The reproducible local
  workaround is `abap_transpile.cmd test/abap_transpile.json`, followed by
  `node output/index.mjs --skip-critical`.
- `2026-09-08`: the pure converter runs in open-abap, but
  `serialize_adt` and `read_data` depend on SAP-only ADT/DDIC runtime objects
  (`CL_WB_OBJECT_OPERATOR` and `DDIF_TABL_GET`). They require validation on a
  SAP system and are not part of the open-abap fixture run.
- Release-dependent DD02V components such as `IS_GTT` and
  `PK_IS_INVHASH` are accessed with `ASSIGN COMPONENT`. When a target release
  or open-abap structure does not contain them, the related optional value is
  ignored rather than causing a syntax or runtime dump.
- open-abap's fixed-character blank comparison does not behave like the
  corresponding string comparison in the lexer. The lexer therefore uses a
  string-template blank for whitespace detection.
- `2026-09-08`: Some SAP DDIC foreign-key records contain legacy or incomplete
       `DD08V` cardinality values that have no direct table-DDL spelling. Since DDL
       cardinality is optional, the converter preserves the foreign-key target and
       conditions while omitting only that cardinality.

- `2026-09-08`: Older table DDL may use `#LIMITED` for an initial DDIC
  `MAINFLAG`, while current table DDL uses `#RESTRICTED`. The parser accepts both;
  serialization emits `#RESTRICTED` canonically.

- `2026-09-08`: Current table DDL spells the enhancement annotation as
  `@AbapCatalog.enhancement.category`; the parser also accepts the older
  camel-case `@AbapCatalog.enhancementCategory`, while serialization uses the
  dotted spelling.

- `2026-09-08`: ADT table DDL can omit semicolons between an include and its
  component extensions, and between intermediate extension blocks. The
  serializer preserves this layout while retaining the semicolon on the final
  extension; the parser accepts the omitted intermediate terminators.

## Conversion boundary

Classic TABL metadata that has no representation in `define table` DDL is not
silently treated as round-trip-safe. This includes DD09L technical settings,
secondary indexes (`DD12V`/`DD17V`), translated texts, long texts, IDoc segment
definitions, and TABL extras. The converter leaves these values outside the
DDL result; a companion format or an explicit integration policy is still
required before the production TABL handler can switch from XML.

Unknown annotations and unsupported metadata are rejected with typed errors.
The existing skip configuration in `test/abap_transpile.json` remains the
source of truth for unrelated open-abap limitations.
