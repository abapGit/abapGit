CLASS zcl_abapgit_object_avar DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: create_object
      RETURNING VALUE(ro_aab_var) TYPE REF TO cl_aab_variant
      RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_OBJECT_AVAR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_ABAPGIT_OBJECT_AVAR->CREATE_OBJECT
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_AAB_VAR                     TYPE REF TO CL_AAB_VARIANT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD create_object.

    DATA: lv_name TYPE aab_var_name.

    lv_name = ms_item-obj_name.

    CREATE OBJECT ro_aab_var
      EXPORTING
        im_name          = lv_name
        im_local         = ''
      EXCEPTIONS
        name_not_allowed = 1
        user_not_valid   = 2
        no_authorization = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~CHANGED_BY
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_USER                        TYPE        XUBNAME
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~changed_by.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->get_author( IMPORTING ex_author = rv_user ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~DELETE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~delete.

    DATA: lo_aab TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->delete(
      EXCEPTIONS
        var_not_found    = 1
        prop_error       = 2
        propt_error      = 3
        var_id_error     = 4
        no_authorization = 5
        cts_error        = 6
        cts_devclass     = 7
        OTHERS           = 8 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting AVAR { ms_item-obj_name }| ).
    ENDIF.
    lo_aab->dequeue( ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~DESERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_PACKAGE                     TYPE        DEVCLASS
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_INPUT
* | [--->] IV_STEP                        TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP
* | [--->] II_LOG                         TYPE REF TO ZIF_ABAPGIT_LOG
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~deserialize.

    DATA: lv_possible    TYPE abap_bool,
          lv_description TYPE aab_var_descript,
          ls_is          TYPE aab_var_obj_act,
          lt_ids         TYPE aab_var_obj_act_tab,
          lo_aab         TYPE REF TO cl_aab_variant.

    " AVAR can only be created in transportable packages
    lv_possible = zcl_abapgit_factory=>get_sap_package( iv_package )->are_changes_recorded_in_tr_req( ).
    IF lv_possible = abap_false.
      zcx_abapgit_exception=>raise( |Global activation variants require a transportable package| ).
    ENDIF.

    " Create AVAR with description and object (id) list
    io_xml->read( EXPORTING iv_name = 'DESCRIPTION'
                  CHANGING  cg_data = lv_description ).

    io_xml->read( EXPORTING iv_name = 'IDS'
                  CHANGING  cg_data = lt_ids ).

    lo_aab = create_object( ).
    lo_aab->enqueue( ).
    lo_aab->set_descript(
      EXPORTING
        im_descript      = lv_description
      EXCEPTIONS
        no_authorization = 1 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |No authorization for { ls_is-object } { ls_is-name }| ).
    ENDIF.

    LOOP AT lt_ids INTO ls_is.
      lo_aab->set_id(
        EXPORTING
          im_name              = ls_is-name
          im_object            = ls_is-object
          im_actmode           = ls_is-actmode
        EXCEPTIONS
          no_authorization     = 1
          id_not_exists        = 2
          id_not_transportable = 3 ).
      CASE sy-subrc.
        WHEN 1.
          zcx_abapgit_exception=>raise( |No authorization for { ls_is-object } { ls_is-name }| ).
        WHEN 2.
          zcx_abapgit_exception=>raise( |{ ls_is-object } { ls_is-name } does not exist| ).
        WHEN 3.
          zcx_abapgit_exception=>raise( |{ ls_is-object } { ls_is-name } must be transportable| ).
      ENDCASE.
    ENDLOOP.

    tadir_insert( iv_package ).

    lo_aab->save(
      EXCEPTIONS
        no_descript_specified = 1
        prop_error            = 2
        propt_error           = 3
        var_id_error          = 4
        no_changes_found      = 5
        cts_error             = 6 ).
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error saving AVAR { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~EXISTS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_BOOL                        TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~exists.

    DATA: lv_state TYPE abap_bool,
          lo_aab   TYPE REF TO cl_aab_variant.

    lo_aab = create_object( ).

    lo_aab->get_state( IMPORTING ex_state = lv_state ).
    rv_bool = boolc( lv_state = abap_true ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~GET_COMPARATOR
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RI_COMPARATOR                  TYPE REF TO ZIF_ABAPGIT_COMPARATOR
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~GET_DESERIALIZE_STEPS
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RT_STEPS                       TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_DESERIALIZATION_STEP_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~GET_METADATA
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RS_METADATA                    TYPE        ZIF_ABAPGIT_DEFINITIONS=>TY_METADATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~IS_ACTIVE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_ACTIVE                      TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~IS_LOCKED
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_IS_LOCKED                   TYPE        ABAP_BOOL
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~JUMP
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~jump.
    zcx_abapgit_exception=>raise( |Jump to AVAR is not supported| ).
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ABAPGIT_OBJECT_AVAR->ZIF_ABAPGIT_OBJECT~SERIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_XML                         TYPE REF TO ZIF_ABAPGIT_XML_OUTPUT
* | [!CX!] ZCX_ABAPGIT_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD zif_abapgit_object~serialize.

    DATA: lo_aab         TYPE REF TO cl_aab_variant,
          lt_ids         TYPE aab_var_obj_act_tab,
          lv_description TYPE aab_var_descript.

    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lo_aab = create_object( ).

    lo_aab->get_descript(
      IMPORTING
        ex_descript = lv_description
      EXCEPTIONS
        no_descript_found = 1 ).
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'DESCRIPTION'
                   ig_data = lv_description ).
    ENDIF.

    lo_aab->get_ids( IMPORTING ex_ids = lt_ids ).

    io_xml->add( iv_name = 'IDS'
                 ig_data = lt_ids ).

  ENDMETHOD.
ENDCLASS.
