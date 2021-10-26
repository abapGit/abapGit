CLASS zcl_abapgit_serializer_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    CLASS-METHODS get_instance
      IMPORTING
        !it_tadir       TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
      RETURNING
        VALUE(ri_cache) TYPE REF TO zcl_abapgit_serializer_cache .
    METHODS reset .
    METHODS get_files
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt .
    METHODS set_files
      IMPORTING
        !is_item  TYPE zif_abapgit_definitions=>ty_item
        !it_files TYPE zif_abapgit_definitions=>ty_files_tt .
    METHODS toggle_on .
    METHODS toggle_off .
    METHODS init_changed_today
      IMPORTING
        !it_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_cache,
        item  TYPE zif_abapgit_definitions=>ty_item,
        files TYPE zif_abapgit_definitions=>ty_files_tt,
      END OF ty_cache .

    CLASS-DATA gi_serializer_cache TYPE REF TO zcl_abapgit_serializer_cache .
    DATA mt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt .
    DATA:
      mt_cache TYPE HASHED TABLE OF ty_cache WITH UNIQUE KEY item .
    DATA:
      mt_today TYPE HASHED TABLE OF zif_abapgit_definitions=>ty_item WITH UNIQUE DEFAULT KEY .
    CLASS-DATA:
      gr_supported_objects TYPE RANGE OF tadir-object .
    DATA mv_use_cache TYPE abap_bool .

    METHODS check_changed_today
      IMPORTING
        !is_item          TYPE zif_abapgit_definitions=>ty_item
      RETURNING
        VALUE(rv_changed) TYPE abap_bool .
    METHODS get_changed_clif
      RETURNING
        VALUE(rt_today) TYPE zif_abapgit_definitions=>ty_items_tt .
    METHODS get_changed_prog
      RETURNING
        VALUE(rt_today) TYPE zif_abapgit_definitions=>ty_items_tt .
ENDCLASS.



CLASS zcl_abapgit_serializer_cache IMPLEMENTATION.


  METHOD check_changed_today.
    READ TABLE mt_today TRANSPORTING NO FIELDS
      WITH KEY obj_type = is_item-obj_type obj_name = is_item-obj_name.
    rv_changed = boolc( sy-subrc = 0 ).
  ENDMETHOD.


  METHOD class_constructor.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF gr_supported_objects.

    " List of supported object types
    " Note: It only makes sense to add types that take significant time to serialize
    APPEND INITIAL LINE TO gr_supported_objects ASSIGNING <ls_object>.
    <ls_object>-sign   = 'I'.
    <ls_object>-option = 'EQ'.
    <ls_object>-low    = 'CLAS'.
    APPEND INITIAL LINE TO gr_supported_objects ASSIGNING <ls_object>.
    <ls_object>-sign   = 'I'.
    <ls_object>-option = 'EQ'.
    <ls_object>-low    = 'INTF'.
    APPEND INITIAL LINE TO gr_supported_objects ASSIGNING <ls_object>.
    <ls_object>-sign   = 'I'.
    <ls_object>-option = 'EQ'.
    <ls_object>-low    = 'PROG'.

  ENDMETHOD.


  METHOD get_changed_clif.

    DATA:
      lt_today    TYPE TABLE OF seoclassdf-clsname,
      lt_clif     TYPE TABLE OF seoclassdf-clsname,
      lt_doks     TYPE TABLE OF dokhl-object,
      lv_clif     TYPE seoclassdf-clsname,
      lv_progname TYPE progname,
      lv_pattern  TYPE string,
      lv_name     TYPE progname.

    FIELD-SYMBOLS:
      <ls_tadir> LIKE LINE OF mt_tadir,
      <ls_today> LIKE LINE OF rt_today.

    " Get all classes and interfaces
    LOOP AT mt_tadir ASSIGNING <ls_tadir> WHERE object = 'CLAS' OR object = 'INTF'.
      lv_clif = <ls_tadir>-obj_name.
      INSERT lv_clif INTO TABLE lt_clif.
    ENDLOOP.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Check which classes and interfaces were touched today
    " Note: These selects are based on primary key and quite fast
    SELECT clsname APPENDING TABLE lt_today FROM seoclassdf
      FOR ALL ENTRIES IN lt_clif
      WHERE clsname = lt_clif-table_line AND ( createdon = sy-datum OR changedon = sy-datum ).

    SELECT clsname APPENDING TABLE lt_today FROM seocompodf
      FOR ALL ENTRIES IN lt_clif
      WHERE clsname = lt_clif-table_line AND ( createdon = sy-datum OR changedon = sy-datum ).

    SELECT clsname APPENDING TABLE lt_today FROM seosubcodf
      FOR ALL ENTRIES IN lt_clif
      WHERE clsname = lt_clif-table_line AND ( createdon = sy-datum OR changedon = sy-datum ).

    SELECT clsname APPENDING TABLE lt_today FROM seometarel
      FOR ALL ENTRIES IN lt_clif
      WHERE clsname = lt_clif-table_line AND ( createdon = sy-datum OR changedon = sy-datum ).

    lt_doks = lt_clif.

    SELECT object APPENDING TABLE lt_today FROM dokhl
      FOR ALL ENTRIES IN lt_doks
      WHERE ( id = 'CA' OR id = 'CE' OR id = 'CL' OR id = 'CO'
        OR    id = 'IA' OR id = 'IE' OR id = 'IF' OR id = 'IO' )
        AND object = lt_doks-table_line
        AND ( dokfdate = sy-datum OR dokldate = sy-datum ).

    SORT lt_today.
    DELETE ADJACENT DUPLICATES FROM lt_today.

    " For each class/interface, check if any include was touched today
    LOOP AT lt_clif INTO lv_clif.
      READ TABLE lt_today TRANSPORTING NO FIELDS WITH TABLE KEY table_line = lv_clif.
      IF sy-subrc <> 0.
        lv_progname = cl_oo_classname_service=>get_cs_name( clsname = lv_clif ).
        lv_pattern = lv_progname(30).
        CONCATENATE lv_pattern '%' INTO lv_pattern.
        IF lv_pattern CA '_'.
          REPLACE ALL OCCURRENCES OF '_' IN lv_pattern WITH '#_' .
        ENDIF.

        SELECT SINGLE progname INTO lv_name FROM reposrc
          WHERE progname <> lv_progname AND progname LIKE lv_pattern ESCAPE '#'
            AND ( cdat = sy-datum OR udat = sy-datum ).
        IF sy-subrc = 0.
          INSERT lv_clif INTO TABLE lt_today.
        ENDIF.

        SELECT progname APPENDING TABLE lt_today FROM repotext
          WHERE progname <> lv_progname AND progname LIKE lv_pattern ESCAPE '#'
            AND udat = sy-datum.
        IF sy-subrc = 0.
          INSERT lv_clif INTO TABLE lt_today.
        ENDIF.
      ENDIF.
    ENDLOOP.

    SORT lt_today.
    DELETE ADJACENT DUPLICATES FROM lt_today.

    " Return touched class and interface items
    LOOP AT mt_tadir ASSIGNING <ls_tadir> WHERE object = 'CLAS' OR object = 'INTF'.
      READ TABLE lt_today TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <ls_tadir>-obj_name.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_today ASSIGNING <ls_today>.
        <ls_today>-obj_type = <ls_tadir>-object.
        <ls_today>-obj_name = <ls_tadir>-obj_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_changed_prog.

    DATA:
      lt_today TYPE TABLE OF progname,
      lv_prog  TYPE progname,
      lt_prog  TYPE TABLE OF progname,
      lt_doks  TYPE TABLE OF dokhl-object.

    FIELD-SYMBOLS:
      <ls_tadir> LIKE LINE OF mt_tadir,
      <ls_today> LIKE LINE OF rt_today.

    " Get all programs
    LOOP AT mt_tadir ASSIGNING <ls_tadir> WHERE object = 'PROG'.
      lv_prog = <ls_tadir>-obj_name.
      INSERT lv_prog INTO TABLE lt_prog.
    ENDLOOP.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Check which programs were touched today (source, texts, screens, menus, longtexts)
    " Note: These selects are based on primary key and quite fast
    SELECT progname APPENDING TABLE lt_today FROM reposrc
      FOR ALL ENTRIES IN lt_prog
      WHERE progname = lt_prog-table_line AND ( cdat = sy-datum OR udat = sy-datum ).

    SELECT progname APPENDING TABLE lt_today FROM repotext
      FOR ALL ENTRIES IN lt_prog
      WHERE progname = lt_prog-table_line AND udat = sy-datum.

    SELECT prog APPENDING TABLE lt_today FROM d020s
      FOR ALL ENTRIES IN lt_prog
      WHERE prog = lt_prog-table_line AND dgen = sy-datum.

    SELECT name APPENDING TABLE lt_today FROM eudb
      FOR ALL ENTRIES IN lt_prog
      WHERE relid = 'CU' AND name = lt_prog-table_line AND ( datum = sy-datum OR vdatum = sy-datum ).

    lt_doks = lt_prog.

    SELECT object APPENDING TABLE lt_today FROM dokhl
      FOR ALL ENTRIES IN lt_doks
      WHERE id = 'RE' AND object = lt_doks-table_line AND ( dokfdate = sy-datum OR dokldate = sy-datum ).

    SORT lt_today.
    DELETE ADJACENT DUPLICATES FROM lt_today.

    " Return touched program items
    LOOP AT mt_tadir ASSIGNING <ls_tadir> WHERE object = 'PROG'.
      READ TABLE lt_today TRANSPORTING NO FIELDS WITH TABLE KEY table_line = <ls_tadir>-obj_name.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_today ASSIGNING <ls_today>.
        <ls_today>-obj_type = <ls_tadir>-object.
        <ls_today>-obj_name = <ls_tadir>-obj_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_files.

    FIELD-SYMBOLS <ls_cache> TYPE ty_cache.

    IF mv_use_cache IS INITIAL.
      RETURN.
    ENDIF.

    IF is_item-obj_type IN gr_supported_objects AND check_changed_today( is_item ) = abap_false.
      READ TABLE mt_cache ASSIGNING <ls_cache> WITH TABLE KEY item = is_item.
      IF sy-subrc = 0.
        rt_files = <ls_cache>-files.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF gi_serializer_cache IS INITIAL.
      CREATE OBJECT gi_serializer_cache.
      gi_serializer_cache->toggle_on( ).
    ENDIF.

    ri_cache = gi_serializer_cache.

  ENDMETHOD.


  METHOD init_changed_today.

    IF mv_use_cache IS INITIAL.
      RETURN.
    ENDIF.

    mt_tadir = it_tadir.

    CLEAR mt_today.

    " Get all classes and interfaces that were touched today
    INSERT LINES OF get_changed_clif( ) INTO TABLE mt_today.

    " Get all programs that were touched today
    INSERT LINES OF get_changed_prog( ) INTO TABLE mt_today.

  ENDMETHOD.


  METHOD reset.
    CLEAR: mt_tadir, mt_cache, mt_today.
  ENDMETHOD.


  METHOD set_files.

    DATA ls_cache TYPE ty_cache.

    IF mv_use_cache IS INITIAL.
      RETURN.
    ENDIF.

    IF is_item-obj_type IN gr_supported_objects AND it_files IS NOT INITIAL.
      DELETE mt_cache WHERE item = is_item.
      ls_cache-item  = is_item.
      ls_cache-files = it_files.
      INSERT ls_cache INTO TABLE mt_cache.
    ENDIF.

  ENDMETHOD.


  METHOD toggle_off.
    mv_use_cache = abap_false.
  ENDMETHOD.


  METHOD toggle_on.
    mv_use_cache = abap_true.
  ENDMETHOD.
ENDCLASS.
