INTERFACE zif_abapgit_aff_docu_v1 PUBLIC.

  TYPES:
    BEGIN OF ty_line,
      format TYPE c LENGTH 2,
      line   TYPE c LENGTH 72,
    END OF ty_line,
    ty_lines TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY.

  TYPES:
    BEGIN OF ty_main,
      format_version TYPE zif_abapgit_aff_types_v1=>ty_format_version,
      lines          TYPE ty_lines,
    END OF ty_main.

ENDINTERFACE.
