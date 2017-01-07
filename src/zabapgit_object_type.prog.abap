*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_OBJECT_TYPE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_object_type DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_type DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    METHODS read
      EXPORTING ev_ddtext TYPE ddtypet-ddtext
                et_source TYPE abaptxt255_tab
      RAISING   lcx_exception
                lcx_not_found.

    METHODS create
      IMPORTING iv_ddtext   TYPE ddtypet-ddtext
                it_source   TYPE abaptxt255_tab
                iv_devclass TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_type DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_type IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_type IMPLEMENTATION.

  METHOD lif_object~has_changed_since.
    rv_changed = abap_true.
  ENDMETHOD.  "lif_object~has_changed_since

  METHOD lif_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.

  METHOD lif_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.                    "lif_object~get_metadata

  METHOD lif_object~exists.

    TRY.
        read( ).
        rv_bool = abap_true.
      CATCH lcx_not_found lcx_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "lif_object~exists

  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup = ms_item-obj_name
      AND ddlanguage = mv_language.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

    lv_typdname = ms_item-obj_name.
    CALL FUNCTION 'TYPD_GET_OBJECT'
      EXPORTING
        typdname          = lv_typdname
      TABLES
        psmodisrc         = lt_psmodisrc
        psmodilog         = lt_psmodilog
        psource           = et_source
        ptrdir            = lt_ptrdir
      EXCEPTIONS
        version_not_found = 1
        reps_not_exist    = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from TYPD_GET_OBJECT' ).
    ENDIF.

  ENDMETHOD.                    "read

  METHOD lif_object~serialize.

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    TRY.
        read( IMPORTING
                ev_ddtext = lv_ddtext
                et_source = lt_source ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    io_xml->add( iv_name = 'DDTEXT'
                 ig_data = lv_ddtext ).

    mo_files->add_abap( lt_source ).

  ENDMETHOD.                    "serialize

  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

    IF lif_object~exists( ) = abap_true.
      lif_object~delete( ).
    ENDIF.

    CALL FUNCTION 'RS_DD_TYGR_INSERT_SOURCES'
      EXPORTING
        typegroupname        = lv_typegroup
        ddtext               = iv_ddtext
        corrnum              = ''
        devclass             = iv_devclass
      TABLES
        source               = it_source
      EXCEPTIONS
        already_exists       = 1
        not_executed         = 2
        permission_failure   = 3
        object_not_specified = 4
        illegal_name         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from RS_DD_TYGR_INSERT_SOURCES' ).
    ENDIF.

    CONCATENATE '%_C' lv_typegroup INTO lv_progname.
    UPDATE progdir SET uccheck = abap_true
      WHERE name = lv_progname.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error setting uccheck' ).
    ENDIF.

  ENDMETHOD.                    "create

  METHOD lif_object~deserialize.

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    io_xml->read( EXPORTING iv_name = 'DDTEXT'
                  CHANGING cg_data = lv_ddtext ).

    lt_source = mo_files->read_abap( ).

    create( iv_ddtext   = lv_ddtext
            it_source   = lt_source
            iv_devclass = iv_package ).

    lcl_objects_activation=>add_item( ms_item ).

  ENDMETHOD.                    "deserialize

  METHOD lif_object~delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = ms_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'G'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4
        dialog_needed        = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error deleting TYPE' ).
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD lif_object~jump.

    jump_se11( iv_radio = 'RSRD1-TYMA'
               iv_field = 'RSRD1-TYMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD lif_object~compare_to_remote_version.
    CREATE OBJECT ro_comparison_result TYPE lcl_null_comparison_result.
  ENDMETHOD.

ENDCLASS.                    "lcl_object_type IMPLEMENTATION