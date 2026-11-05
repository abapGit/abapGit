CLASS zcl_abapgit_object_vcls DEFINITION PUBLIC INHERITING FROM zcl_abapgit_objects_super FINAL.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_object.

  PROTECTED SECTION.
  PRIVATE SECTION.

* See include MTOBJCON:
    CONSTANTS c_cluster_type TYPE c VALUE 'C' ##NO_TEXT.
    CONSTANTS c_mode_insert TYPE obj_para-maint_mode VALUE 'I' ##NO_TEXT.

    TYPES ty_vclstruc_tt TYPE STANDARD TABLE OF v_vclstruc WITH DEFAULT KEY.

    METHODS serialize_texts
      IMPORTING
        !ii_xml TYPE REF TO zif_abapgit_xml_output
      RAISING
        zcx_abapgit_exception .
    METHODS deserialize_texts
      IMPORTING
        !ii_xml      TYPE REF TO zif_abapgit_xml_input
        !it_vclstruc TYPE ty_vclstruc_tt
      RAISING
        zcx_abapgit_exception .
    METHODS is_locked
      IMPORTING
        !iv_tabname         TYPE tabname
        !iv_argument        TYPE seqg3-garg
      RETURNING
        VALUE(rv_is_locked) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_object_vcls IMPLEMENTATION.


  METHOD deserialize_texts.

    DATA lt_vcldirt   TYPE STANDARD TABLE OF vcldirt WITH DEFAULT KEY.
    DATA lt_vclstruct TYPE STANDARD TABLE OF vclstruct WITH DEFAULT KEY.
    DATA lv_index     TYPE i.

    FIELD-SYMBOLS <ls_vcldirt>   LIKE LINE OF lt_vcldirt.
    FIELD-SYMBOLS <ls_vclstruct> LIKE LINE OF lt_vclstruct.

    ii_xml->read( EXPORTING iv_name = 'I18N_VCLDIRT'
                  CHANGING  cg_data = lt_vcldirt ).
    ii_xml->read( EXPORTING iv_name = 'I18N_VCLSTRUCT'
                  CHANGING  cg_data = lt_vclstruct ).

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab             = lt_vcldirt ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab             = lt_vclstruct ).

    " Force the view cluster name, and keep only texts of its nodes
    LOOP AT lt_vcldirt ASSIGNING <ls_vcldirt>.
      <ls_vcldirt>-vclname = ms_item-obj_name.
    ENDLOOP.
    LOOP AT lt_vclstruct ASSIGNING <ls_vclstruct>.
      lv_index = sy-tabix.
      <ls_vclstruct>-vclname = ms_item-obj_name.
      READ TABLE it_vclstruc TRANSPORTING NO FIELDS WITH KEY object = <ls_vclstruct>-object.
      IF sy-subrc <> 0.
        DELETE lt_vclstruct INDEX lv_index.
      ENDIF.
    ENDLOOP.

    IF lines( lt_vcldirt ) > 0.
      MODIFY vcldirt FROM TABLE lt_vcldirt.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Update of view cluster title translations failed' ).
      ENDIF.
    ENDIF.
    IF lines( lt_vclstruct ) > 0.
      MODIFY vclstruct FROM TABLE lt_vclstruct.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Update of view cluster node translations failed' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_locked.

    DATA:
      ls_rstable_key TYPE rstable, " Lock argument for table RSTABLE
      lv_argument    TYPE eqegraarg.

    " Set Values for generic table lock
    ls_rstable_key-tabname = iv_tabname.
    ls_rstable_key-varkey  = iv_argument.

    " include all sub keys
    lv_argument = ls_rstable_key.
    lv_argument = lv_argument && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object         = 'E_TABLEE'
                                            iv_argument            = lv_argument ).

  ENDMETHOD.


  METHOD serialize_texts.

    DATA lt_vcldirt   TYPE STANDARD TABLE OF vcldirt WITH DEFAULT KEY.
    DATA lt_vclstruct TYPE STANDARD TABLE OF vclstruct WITH DEFAULT KEY.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Skip the main language, it is part of the definition already.
    " Without the view cluster name, it is the object's name anyway.
    SELECT spras text
      INTO CORRESPONDING FIELDS OF TABLE lt_vcldirt
      FROM vcldirt
      WHERE vclname = ms_item-obj_name
      AND spras <> mv_language
      ORDER BY spras.                                   "#EC CI_SUBRC
    SELECT spras object objecttext
      INTO CORRESPONDING FIELDS OF TABLE lt_vclstruct
      FROM vclstruct
      WHERE vclname = ms_item-obj_name
      AND spras <> mv_language
      ORDER BY spras object.                            "#EC CI_SUBRC

    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab             = lt_vcldirt ).
    mo_i18n_params->trim_saplang_keyed_table(
      EXPORTING
        iv_lang_field_name = 'SPRAS'
      CHANGING
        ct_tab             = lt_vclstruct ).

    IF lines( lt_vcldirt ) > 0.
      ii_xml->add( iv_name = 'I18N_VCLDIRT'
                   ig_data = lt_vcldirt ).
    ENDIF.
    IF lines( lt_vclstruct ) > 0.
      ii_xml->add( iv_name = 'I18N_VCLSTRUCT'
                   ig_data = lt_vclstruct ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~changed_by.
    SELECT SINGLE author FROM vcldir INTO rv_user
      WHERE vclname = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.


  METHOD zif_abapgit_object~delete.
* Do the same as in VIEWCLUSTER_SAVE_DEFINITION
    DATA: lv_vclname TYPE vcl_name.


    lv_vclname = ms_item-obj_name.

    DELETE FROM vcldir WHERE vclname = lv_vclname.        "#EC CI_SUBRC
    DELETE FROM vcldirt WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstruc WHERE vclname = lv_vclname.      "#EC CI_SUBRC
    DELETE FROM vclstruct WHERE vclname = lv_vclname. "#EC CI_NOFIRST "#EC CI_SUBRC
    DELETE FROM vclstrudep WHERE vclname = lv_vclname.    "#EC CI_SUBRC
    DELETE FROM vclmf WHERE vclname = lv_vclname.         "#EC CI_SUBRC

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA: ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE ty_vclstruc_tt,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf,
          lv_objectname   TYPE ob_object.


    io_xml->read( EXPORTING iv_name = 'VCLDIR'
                  CHANGING cg_data = ls_vcldir_entry ).
    io_xml->read( EXPORTING iv_name = 'VLCSTRUC_TAB'
                  CHANGING cg_data = lt_vclstruc ).
    io_xml->read( EXPORTING iv_name = 'VCLSTRUDEP_TAB'
                  CHANGING cg_data = lt_vclstrudep ).
    io_xml->read( EXPORTING iv_name = 'VCLMF_TAB'
                  CHANGING cg_data = lt_vclmf ).

    ls_vcldir_entry-author = sy-uname.
    ls_vcldir_entry-changedate = sy-datum.

    CALL FUNCTION 'VIEWCLUSTER_SAVE_DEFINITION'
      EXPORTING
        vcldir_entry   = ls_vcldir_entry
      TABLES
        vclstruc_tab   = lt_vclstruc
        vclstrudep_tab = lt_vclstrudep
        vclmf_tab      = lt_vclmf.

    " The function module writes the texts in the logon language only and
    " deletes all others, so the translations are written afterwards
    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( ii_xml      = io_xml
                         it_vclstruc = lt_vclstruc ).
    ENDIF.

    corr_insert( iv_package ).

    lv_objectname = ls_vcldir_entry-vclname.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = lv_objectname
        iv_objecttype         = c_cluster_type
        iv_maint_mode         = c_mode_insert
        iv_devclass           = iv_package
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.


  METHOD zif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_deserialize_steps.
    APPEND zif_abapgit_object=>gc_step_id-abap TO rt_steps.
    APPEND zif_abapgit_object=>gc_step_id-lxe TO rt_steps.
  ENDMETHOD.


  METHOD zif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.


  METHOD zif_abapgit_object~is_active.

    DATA lv_changedate TYPE vcldir-changedate.

    SELECT SINGLE changedate INTO lv_changedate FROM vcldir
      WHERE vclname = ms_item-obj_name.

* see logic in function module VIEWCLUSTER_GET_DEFINITION
    rv_active = boolc( lv_changedate IS NOT INITIAL ).

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.

    DATA:
      lv_argument       TYPE seqg3-garg,
      lv_argument_langu TYPE seqg3-garg.

    lv_argument       = ms_item-obj_name.
    lv_argument_langu = |@{ ms_item-obj_name }|.

    "Check all relevant maintain tables for view clusters
    IF is_locked( iv_tabname = 'VCLDIR'
                  iv_argument = lv_argument ) = abap_true
        OR is_locked( iv_tabname = 'VCLDIRT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUC'
                      iv_argument = lv_argument )       = abap_true
        OR is_locked( iv_tabname = 'VCLSTRUCT'
                      iv_argument = lv_argument_langu ) = abap_true
        OR is_locked( iv_tabname = 'VCLMF'
                      iv_argument = lv_argument )       = abap_true.

      rv_is_locked = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0050'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VIEWNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-STRUCT_MNT'.
    ls_bcdata-fval     = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLUS'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-program  = 'SAPMSVIM'.
    ls_bcdata-dynpro   = '0052 '.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'VIMDYNFLDS-VCLNAME'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=CLSH'.
    APPEND ls_bcdata TO lt_bcdata.

    zcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SE54'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA: lv_vclname      TYPE vcl_name,
          ls_vcldir_entry TYPE v_vcldir,
          lt_vclstruc     TYPE TABLE OF v_vclstruc,
          lt_vclstrudep   TYPE TABLE OF v_vclstdep,
          lt_vclmf        TYPE TABLE OF v_vclmf.


    IF zif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    lv_vclname = ms_item-obj_name.

    CALL FUNCTION 'VIEWCLUSTER_GET_DEFINITION'
      EXPORTING
        vclname                = lv_vclname
      IMPORTING
        vcldir_entry           = ls_vcldir_entry
      TABLES
        vclstruc_tab           = lt_vclstruc
        vclstrudep_tab         = lt_vclstrudep
        vclmf_tab              = lt_vclmf
      EXCEPTIONS
        viewcluster_not_found  = 1
        incomplete_viewcluster = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SORT lt_vclstrudep BY vclname object objfield.

    CLEAR ls_vcldir_entry-author.
    CLEAR ls_vcldir_entry-changedate.

    io_xml->add( iv_name = 'VCLDIR'
                 ig_data = ls_vcldir_entry ).
    io_xml->add( iv_name = 'VLCSTRUC_TAB'
                 ig_data = lt_vclstruc ).
    io_xml->add( iv_name = 'VCLSTRUDEP_TAB'
                 ig_data = lt_vclstrudep ).
    io_xml->add( iv_name = 'VCLMF_TAB'
                 ig_data = lt_vclmf ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
