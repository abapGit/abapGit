CLASS zcl_abapgit_objects_super DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS c_user_unknown TYPE xubname VALUE 'UNKNOWN'.

    METHODS constructor
      IMPORTING
        !is_item     TYPE zif_abapgit_definitions=>ty_item
        !iv_language TYPE spras .
    CLASS-METHODS jump_adt
      IMPORTING
        !iv_obj_name     TYPE zif_abapgit_definitions=>ty_item-obj_name
        !iv_obj_type     TYPE zif_abapgit_definitions=>ty_item-obj_type
        !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name OPTIONAL
        !iv_sub_obj_type TYPE zif_abapgit_definitions=>ty_item-obj_type OPTIONAL
        !iv_line_number  TYPE i OPTIONAL
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    DATA ms_item TYPE zif_abapgit_definitions=>ty_item .
    DATA mv_language TYPE spras .

    METHODS get_metadata
      RETURNING
        VALUE(rs_metadata) TYPE zif_abapgit_definitions=>ty_metadata .
    METHODS corr_insert
      IMPORTING
        !iv_package      TYPE devclass
        !ig_object_class TYPE any OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS tadir_insert
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    METHODS jump_se11
      RAISING
        zcx_abapgit_exception .
    METHODS exists_a_lock_entry_for
      IMPORTING
        !iv_lock_object               TYPE string
        !iv_argument                  TYPE seqg3-garg OPTIONAL
      RETURNING
        VALUE(rv_exists_a_lock_entry) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS set_default_package
      IMPORTING
        !iv_package TYPE devclass .
    METHODS serialize_longtexts
      IMPORTING
        !ii_xml         TYPE REF TO zif_abapgit_xml_output
        !iv_longtext_id TYPE dokil-id OPTIONAL
        !it_dokil       TYPE zif_abapgit_definitions=>ty_dokil_tt OPTIONAL
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_longtexts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
    METHODS delete_longtexts
      IMPORTING
        !iv_longtext_id TYPE dokil-id
      RAISING
        zcx_abapgit_exception .
    METHODS is_active
      RETURNING
        VALUE(rv_active) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS delete_ddic
      IMPORTING
        VALUE(iv_objtype)              TYPE string
        VALUE(iv_no_ask)               TYPE abap_bool DEFAULT abap_true
        VALUE(iv_no_ask_delete_append) TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS serialize_lxe_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_lxe_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_input
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_objects_super IMPLEMENTATION.


  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.


  METHOD corr_insert.

    DATA: lv_object       TYPE string,
          lv_object_class TYPE string.

    IF ig_object_class IS NOT INITIAL.
      lv_object_class = ig_object_class.
      IF ig_object_class = 'DICT'.
        CONCATENATE ms_item-obj_type ms_item-obj_name INTO lv_object.
      ELSE.
        lv_object = ms_item-obj_name.
      ENDIF.
    ELSE.
      lv_object_class = ms_item-obj_type.
      lv_object       = ms_item-obj_name.
    ENDIF.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = lv_object
        object_class        = lv_object_class
        devclass            = iv_package
        master_language     = mv_language
        global_lock         = abap_true
        mode                = 'I'
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_ddic.

    DATA: lv_objname TYPE rsedd0-ddobjname,
          lv_objtype TYPE rsedd0-ddobjtype.

    lv_objname = ms_item-obj_name.
    lv_objtype = iv_objtype.

    TRY.
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
            no_ask_delete_append = iv_no_ask_delete_append
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
      CATCH cx_sy_dyn_call_param_not_found.
        " no_ask_delete_append not available in lower releases
        CALL FUNCTION 'RS_DD_DELETE_OBJ'
          EXPORTING
            no_ask               = iv_no_ask
            objname              = lv_objname
            objtype              = lv_objtype
          EXCEPTIONS
            not_executed         = 1
            object_not_found     = 2
            object_not_specified = 3
            permission_failure   = 4
            dialog_needed        = 5
            OTHERS               = 6.
    ENDTRY.

    IF sy-subrc = 5.
      zcx_abapgit_exception=>raise( |Object { ms_item-obj_type } { ms_item-obj_name
                                    } has dependencies and must be deleted manually| ).
    ELSEIF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error deleting { ms_item-obj_type } { ms_item-obj_name }| ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->delete(
        iv_longtext_id = iv_longtext_id
        iv_object_name = ms_item-obj_name  ).

  ENDMETHOD.


  METHOD deserialize_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->deserialize(
      ii_xml           = ii_xml
      iv_main_language = mv_language ).

  ENDMETHOD.


  METHOD deserialize_lxe_texts.

    zcl_abapgit_factory=>get_lxe_texts( )->deserialize(
      iv_object_type = ms_item-obj_type
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).

  ENDMETHOD.


  METHOD exists_a_lock_entry_for.

    DATA: lt_lock_entries TYPE STANDARD TABLE OF seqg3.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname                = '*'
        garg                  = iv_argument
      TABLES
        enq                   = lt_lock_entries
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        OTHERS                = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    READ TABLE lt_lock_entries TRANSPORTING NO FIELDS
                               WITH KEY gobj = iv_lock_object.
    IF sy-subrc = 0.
      rv_exists_a_lock_entry = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_metadata.

    DATA: lv_class TYPE string.

    lv_class = cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).

    REPLACE FIRST OCCURRENCE OF 'ZCL_ABAPGIT' IN lv_class WITH 'LCL'.

    rs_metadata-class = lv_class.
    rs_metadata-version = 'v1.0.0'.

  ENDMETHOD.


  METHOD is_active.

    DATA: lt_messages    TYPE STANDARD TABLE OF sprot_u WITH DEFAULT KEY,
          lt_e071_tadirs TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY,
          ls_e071_tadir  LIKE LINE OF lt_e071_tadirs.

    ms_item-inactive = abap_false.

    ls_e071_tadir-object   = ms_item-obj_type.
    ls_e071_tadir-obj_name = ms_item-obj_name.
    INSERT ls_e071_tadir INTO TABLE lt_e071_tadirs.

    CALL FUNCTION 'RS_INACTIVE_OBJECTS_WARNING'
      EXPORTING
        suppress_protocol         = abap_false
        with_program_includes     = abap_false
        suppress_dictionary_check = abap_false
      TABLES
        p_e071                    = lt_e071_tadirs
        p_xmsg                    = lt_messages.

    IF lt_messages IS NOT INITIAL.
      ms_item-inactive = abap_true.
    ENDIF.

    rv_active = boolc( ms_item-inactive = abap_false ).
  ENDMETHOD.


  METHOD jump_adt.

    DATA: lv_adt_link TYPE string,
          lx_error    TYPE REF TO cx_root.

    TRY.

        lv_adt_link = zcl_abapgit_adt_link=>generate(
          iv_obj_name     = iv_obj_name
          iv_obj_type     = iv_obj_type
          iv_sub_obj_name = iv_sub_obj_name
          iv_line_number  = iv_line_number ).

        cl_gui_frontend_services=>execute(
          EXPORTING  document = lv_adt_link
          EXCEPTIONS OTHERS   = 1 ).

        IF sy-subrc <> 0.
          zcx_abapgit_exception=>raise( |ADT Jump Error - failed to open link { lv_adt_link }. Subrc={ sy-subrc }| ).
        ENDIF.

      CATCH cx_root INTO lx_error.
        zcx_abapgit_exception=>raise( iv_text = 'ADT Jump Error'
                                      ix_previous = lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD jump_se11.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = ms_item-obj_name
        object_type         = ms_item-obj_type
        devclass            = ms_item-devclass
        in_new_window       = abap_true
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Jump to SE11 failed (subrc={ sy-subrc } ).| ).
    ENDIF.

  ENDMETHOD.


  METHOD serialize_longtexts.

    zcl_abapgit_factory=>get_longtexts( )->serialize(
        iv_object_name = ms_item-obj_name
        iv_longtext_id = iv_longtext_id
        it_dokil       = it_dokil
        ii_xml         = ii_xml  ).

  ENDMETHOD.


  METHOD serialize_lxe_texts.

    IF ii_xml->i18n_params( )-main_language_only = abap_true OR
       ii_xml->i18n_params( )-translation_languages IS INITIAL.
      RETURN.
    ENDIF.

    zcl_abapgit_factory=>get_lxe_texts( )->serialize(
      iv_object_type = ms_item-obj_type
      iv_object_name = ms_item-obj_name
      ii_xml         = ii_xml ).

  ENDMETHOD.


  METHOD set_default_package.

    " In certain cases we need to set the package package via ABAP memory
    " because we can't supply it via the APIs.
    "
    " Set default package, see function module RS_CORR_INSERT FORM get_current_devclass.
    "
    " We use ABAP memory instead the SET parameter because it is
    " more reliable. SET parameter doesn't work when multiple objects
    " are deserialized which uses the ABAP memory mechanism.
    " We don't need to reset the memory as it is done in above mentioned form routine.

    EXPORT current_devclass FROM iv_package TO MEMORY ID 'EUK'.

  ENDMETHOD.


  METHOD tadir_insert.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = ms_item-obj_type
        wi_tadir_obj_name   = ms_item-obj_name
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = mv_language
        iv_delflag          = abap_false
      EXCEPTIONS
        OTHERS              = 1.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
