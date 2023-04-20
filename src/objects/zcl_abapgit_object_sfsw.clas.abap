CLASS zcl_abapgit_object_sfsw DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_longtext_id_sfsw TYPE dokil-id VALUE 'SW'.

    DATA mv_switch TYPE sfw_switch_id.

    METHODS:
      activate
        RAISING zcx_abapgit_exception,
      create
        RETURNING VALUE(ro_switch) TYPE REF TO cl_sfw_sw
        RAISING   zcx_abapgit_exception,
      get
        RETURNING VALUE(ro_switch) TYPE REF TO cl_sfw_sw
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_object_sfsw IMPLEMENTATION.


  METHOD activate.

    DATA: lt_switches TYPE sfw_switchtab,
          lt_msgtab   TYPE sprot_u_tab.

    IF zif_abapgit_object~is_active( ) = abap_true.
      RETURN.
    ENDIF.

    APPEND mv_switch TO lt_switches.

    cl_sfw_activate=>activate_sfsw(
      EXPORTING
        p_switches = lt_switches
        p_version  = 'I'
      IMPORTING
        p_msgtab   = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'Error activating SFBS' ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_switch = is_item-obj_name.

  ENDMETHOD.


  METHOD create.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>create_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        zcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

  ENDMETHOD.


  METHOD get.

    TRY.
        " make sure to clear cache
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
        ro_switch->free( ).
        ro_switch = cl_sfw_sw=>get_switch( mv_switch ).
      CATCH cx_pak_invalid_data cx_pak_invalid_state cx_pak_not_authorized.
        zcx_abapgit_exception=>raise( 'Error from CL_SFW_SW=>GET_SWITCH' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.

    DATA: ls_data TYPE sfw_switch.


    ls_data = get( )->get_header_data( ).

    rv_user = ls_data-changedby.
    IF rv_user IS INITIAL.
      rv_user = ls_data-author.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA: lt_delete TYPE sfw_switchtab,
          lt_msgtab TYPE sprot_u_tab.

    APPEND mv_switch TO lt_delete.

    cl_sfw_activate=>delete_sfsw( EXPORTING p_switches = lt_delete
                                  IMPORTING p_msgtab = lt_msgtab ).

    READ TABLE lt_msgtab WITH KEY severity = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      zcx_abapgit_exception=>raise( 'Error deleting SFSW' ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.

    IF iv_step = zif_abapgit_object=>gc_step_id-late.
      activate( ).
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'HEADER'
                  CHANGING cg_data = ls_header ).
    io_xml->read( EXPORTING iv_name = 'NAME32'
                  CHANGING cg_data = lv_name_32 ).
    io_xml->read( EXPORTING iv_name = 'NAME80'
                  CHANGING cg_data = lv_name_80 ).

    io_xml->read( EXPORTING iv_name = 'PARENT_BF'
                  CHANGING cg_data = lt_parent_bf ).
    io_xml->read( EXPORTING iv_name = 'CONFLICTS'
                  CHANGING cg_data = lt_conflicts ).

    TRY.
        IF zif_abapgit_object~exists( ) = abap_true.
          lo_switch = get( ).
        ELSE.
          lo_switch = create( ).
        ENDIF.
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        zcx_abapgit_exception=>raise( 'error in CL_SFW_SW=>CREATE_SWITCH' ).
    ENDTRY.

    ls_header-author = sy-uname.
    ls_header-createdon = sy-datum.
    lo_switch->set_header_data( ls_header ).

    lo_switch->set_texts( p_32 = lv_name_32
                          p_80 = lv_name_80 ).

    lo_switch->set_parent_bf( lt_parent_bf ).
    lo_switch->set_conflicts( lt_conflicts ).

    set_default_package( iv_package ).
    tadir_insert( iv_package ).

    lo_switch->save_all(
      EXCEPTIONS
        not_saved = 1
        OTHERS    = 2 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'error in CL_SFW_SW->SAVE_ALL' ).
    ENDIF.

    deserialize_longtexts( ii_xml         = io_xml
                           iv_longtext_id = c_longtext_id_sfsw ).

    zcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA ls_tadir TYPE tadir.

    IF cl_sfw_sw=>check_existence( mv_switch ) = abap_false.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO ls_tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-late TO rt_steps.
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
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lo_switch    TYPE REF TO cl_sfw_sw,
          ls_header    TYPE sfw_switch,
          lv_name_32   TYPE sfw_name32,
          lv_name_80   TYPE sfw_name80,
          lt_parent_bf TYPE sfw_bf_sw_outtab,
          lt_conflicts TYPE sfw_confl_outtab.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_switch = get( ).

    ls_header = lo_switch->get_header_data( ).
    CLEAR: ls_header-author,
           ls_header-version,
           ls_header-createdon,
           ls_header-changedby,
           ls_header-changedon,
           ls_header-timestamp.

    lo_switch->get_texts(
      IMPORTING
        p_32 = lv_name_32
        p_80 = lv_name_80 ).

    lt_parent_bf = lo_switch->get_parent_bf( ).
    lt_conflicts = lo_switch->get_conflicts( ).

    io_xml->add( ig_data = ls_header
                 iv_name = 'HEADER' ).
    io_xml->add( ig_data = lv_name_32
                 iv_name = 'NAME32' ).
    io_xml->add( ig_data = lv_name_80
                 iv_name = 'NAME80' ).

    io_xml->add( ig_data = lt_parent_bf
                 iv_name = 'PARENT_BF' ).
    io_xml->add( ig_data = lt_conflicts
                 iv_name = 'CONFLICTS' ).

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_sfsw ).

  ENDMETHOD.
ENDCLASS.
