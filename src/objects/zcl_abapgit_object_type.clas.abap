CLASS zcl_abapgit_object_type DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_prefix TYPE c LENGTH 3 VALUE '%_C'.

    METHODS read
      EXPORTING ev_ddtext TYPE ddtypet-ddtext
                et_source TYPE abaptxt255_tab
      RAISING   zcx_abapgit_exception.

    METHODS create
      IMPORTING iv_ddtext   TYPE ddtypet-ddtext
                it_source   TYPE abaptxt255_tab
                iv_devclass TYPE devclass
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_type IMPLEMENTATION.


  METHOD create.

    DATA: lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.

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
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CONCATENATE c_prefix lv_typegroup INTO lv_progname.
    UPDATE progdir SET uccheck = abap_true
      WHERE name = lv_progname.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error setting uccheck' ).
    ENDIF.

  ENDMETHOD.


  METHOD read.

    DATA: lv_typdname  TYPE rsedd0-typegroup,
          lt_psmodisrc TYPE TABLE OF smodisrc,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_ptrdir    TYPE TABLE OF trdir.


    SELECT SINGLE ddtext FROM ddtypet
      INTO ev_ddtext
      WHERE typegroup  = ms_item-obj_name
        AND ddlanguage = mv_language.

    lv_typdname = ms_item-obj_name.

    " Get active version, ignore errors if not found
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
        OTHERS            = 3 ##FM_SUBRC_OK.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    DATA lv_prog TYPE progname.

    CONCATENATE '%_C' ms_item-obj_name INTO lv_prog.

    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = lv_prog AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'G' ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lv_ddtext    TYPE ddtypet-ddtext,
          lt_source    TYPE abaptxt255_tab,
          lv_progname  TYPE reposrc-progname,
          lv_typegroup TYPE rsedd0-typegroup.


    lv_typegroup = ms_item-obj_name.


    io_xml->read( EXPORTING iv_name = 'DDTEXT'
                  CHANGING cg_data = lv_ddtext ).

    lt_source = zif_abapgit_object~mo_files->read_abap( ).

    IF zif_abapgit_object~exists( ) = abap_false.
      create( iv_ddtext   = lv_ddtext
              it_source   = lt_source
              iv_devclass = iv_package ).
    ELSE.
      CONCATENATE c_prefix lv_typegroup INTO lv_progname.
      INSERT REPORT lv_progname FROM lt_source STATE 'I'.
    ENDIF.

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA: lv_progname TYPE progname,
          lv_state    TYPE r3state.

    lv_progname = |%_C{ ms_item-obj_name }|.
    SELECT SINGLE state
      FROM progdir
      INTO lv_state
      WHERE name = lv_progname.
    IF lv_state IS NOT INITIAL.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_ddtext TYPE ddtypet-ddtext,
          lt_source TYPE abaptxt255_tab.


    read( IMPORTING ev_ddtext = lv_ddtext
                    et_source = lt_source ).

    IF lt_source IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'DDTEXT'
                 ig_data = lv_ddtext ).

    zif_abapgit_object~mo_files->add_abap( lt_source ).

  ENDMETHOD.
ENDCLASS.
