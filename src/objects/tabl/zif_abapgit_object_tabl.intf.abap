INTERFACE zif_abapgit_object_tabl PUBLIC.
  CONSTANTS: BEGIN OF c_s_dataname,
               segment_definition TYPE string VALUE 'SEGMENT_DEFINITION',
               tabl_extras        TYPE string VALUE 'TABL_EXTRAS',
             END OF c_s_dataname.

  TYPES: BEGIN OF ty_dd02_text,
           ddlanguage TYPE dd02t-ddlanguage,
           ddtext     TYPE dd02t-ddtext,
         END OF ty_dd02_text.

  TYPES ty_dd02_texts TYPE STANDARD TABLE OF ty_dd02_text WITH DEFAULT KEY.

  TYPES ty_dd03p_tt TYPE STANDARD TABLE OF dd03p WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_segment_definition,
           segmentheader     TYPE edisegmhd,
           segmentdefinition TYPE edisegmdef,
           segmentstructures TYPE STANDARD TABLE OF edisegstru WITH DEFAULT KEY,
         END OF ty_segment_definition.

  TYPES: ty_segment_definitions TYPE STANDARD TABLE OF ty_segment_definition WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_tabl_extras,
           tddat TYPE tddat,
         END OF ty_tabl_extras.

  TYPES: BEGIN OF ty_internal,
           dd02v               TYPE dd02v,
           dd09l               TYPE dd09l,
           dd03p               TYPE ty_dd03p_tt,
           dd05m               TYPE STANDARD TABLE OF dd05m WITH DEFAULT KEY,
           dd08v               TYPE STANDARD TABLE OF dd08v WITH DEFAULT KEY,
           dd12v               TYPE STANDARD TABLE OF dd12v WITH DEFAULT KEY,
           dd17v               TYPE STANDARD TABLE OF dd17v WITH DEFAULT KEY,
           dd35v               TYPE STANDARD TABLE OF dd35v WITH DEFAULT KEY,
           dd36m               TYPE STANDARD TABLE OF dd36m WITH DEFAULT KEY,
           dd02_texts          TYPE ty_dd02_texts,
           i18n_langs          TYPE STANDARD TABLE OF langu WITH DEFAULT KEY,
           longtexts           TYPE zif_abapgit_longtexts=>ty_longtexts,
           segment_definitions TYPE ty_segment_definitions,
           extras              TYPE ty_tabl_extras,
         END OF ty_internal.

ENDINTERFACE.
