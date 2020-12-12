CLASS zcl_abapgit_ecatt_script_downl DEFINITION
  PUBLIC
  INHERITING FROM cl_apl_ecatt_script_download
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_ecatt_download.

    METHODS:
      download REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      download_data REDEFINITION.

  PRIVATE SECTION.
    DATA:
      mv_xml_stream  TYPE xstring,
      mi_script_node TYPE REF TO if_ixml_element.

    METHODS:
      set_script_to_template
        RAISING
          cx_ecatt_apl,

      set_control_data_for_tcd
        IMPORTING
          is_param  TYPE etpar_gui
          io_params TYPE REF TO cl_apl_ecatt_params
        RAISING
          cx_ecatt_apl,

      escape_control_data
        IMPORTING
          ii_element TYPE REF TO if_ixml_element
          iv_tabname TYPE string
          iv_node    TYPE string
        RAISING
          cx_ecatt_apl,

      set_blob_to_template
        RAISING
          cx_ecatt_apl,

      set_artmp_to_template
        RAISING
          cx_ecatt_apl.

ENDCLASS.



CLASS zcl_abapgit_ecatt_script_downl IMPLEMENTATION.


  METHOD download.

    " Downport

    load_help = im_load_help.
    typ = im_object_type.

    TRY.
        cl_apl_ecatt_object=>show_object(
          EXPORTING
            im_obj_type = im_object_type
            im_name     = im_object_name
            im_version  = im_object_version
          IMPORTING
            re_object   = ecatt_object ).
      CATCH cx_ecatt INTO ex_ecatt.
        RETURN.
    ENDTRY.

    toolname = ecatt_object->attrib->get_tool_name( ).
    set_attributes_to_template( ).

    IF toolname = cl_apl_ecatt_const=>toolname_ecatt.

      ecatt_script ?= ecatt_object.

      set_script_to_template( ).

      TRY.
          get_general_params_data( ecatt_script->params ).
        CATCH cx_ecatt_apl.                              "#EC NO_HANDLER
*         proceed with download and report errors later
      ENDTRY.

      LOOP AT parm INTO wa_parm.
        TRY.
            IF wa_parm-value = '<INITIAL>'.
              CLEAR wa_parm-value.
            ENDIF.
            set_general_params_data_to_dom( ).
            IF NOT wa_parm-pstruc_typ IS INITIAL.
              set_deep_stru_to_dom( ecatt_script->params ).
              set_deep_data_to_dom( ecatt_script->params ).
              IF wa_parm-xmlref_typ = cl_apl_ecatt_const=>ref_type_c_tcd.
                set_control_data_for_tcd( is_param  = wa_parm
                                          io_params = ecatt_script->params ).

              ENDIF.
            ENDIF.
          CATCH cx_ecatt_apl.                            "#EC NO_HANDLER
*         proceed with download and report errors later
        ENDTRY.
      ENDLOOP.

    ELSE.

      set_blob_to_template( ).
      set_artmp_to_template( ).

    ENDIF.

    download_data( ).

  ENDMETHOD.


  METHOD download_data.

    " Downport

    mv_xml_stream = zcl_abapgit_ecatt_helper=>download_data( template_over_all ).

  ENDMETHOD.


  METHOD escape_control_data.

    " Downport

    DATA: li_iter     TYPE REF TO if_ixml_node_iterator,
          li_textit   TYPE REF TO if_ixml_node_iterator,
          li_abapctrl TYPE REF TO if_ixml_node_collection,
          li_text     TYPE REF TO if_ixml_text,
          li_filter   TYPE REF TO if_ixml_node_filter,
          li_list     TYPE REF TO if_ixml_node_list,
          lv_value    TYPE etdom_name,
          li_vars     TYPE REF TO if_ixml_element,
          li_elem     TYPE REF TO if_ixml_element.

    li_vars = ii_element->find_from_name_ns( iv_tabname ).
    li_filter = ii_element->create_filter_node_type( if_ixml_node=>co_node_text ).
    IF li_vars IS NOT INITIAL.
      li_abapctrl = ii_element->get_elements_by_tag_name_ns( iv_node ).

* just for debugging
      li_iter = li_abapctrl->create_iterator( ).
      li_elem ?= li_iter->get_next( ).
      WHILE li_elem IS NOT INITIAL.
        li_list = li_elem->get_children( ).

        li_textit = li_list->create_rev_iterator_filtered( li_filter ).
        li_text ?= li_textit->get_next( ).
        IF li_text IS NOT INITIAL.
          lv_value = li_text->get_data( ).
          IF lv_value(1) = cl_abap_char_utilities=>minchar.
            REPLACE SECTION OFFSET 0 LENGTH 1 OF lv_value WITH space.
            li_text->set_value( value = lv_value ).
          ENDIF.
        ENDIF.
        CLEAR: li_textit, li_list, li_elem, lv_value.
        li_elem ?= li_iter->get_next( ).
      ENDWHILE.
      CLEAR: li_abapctrl, li_elem, li_iter.

    ENDIF.

  ENDMETHOD.


  METHOD set_artmp_to_template.

    " Downport

    DATA: li_artmp_node   TYPE REF TO if_ixml_element,
          lv_rc           TYPE sy-subrc,
          lv_text         TYPE string,
          lv_rc_args_tmpl TYPE i,
          lv_errmsg       TYPE string.

    li_artmp_node = template_over_all->create_simple_element(
                      name   = 'ECET_ARTMP'
                      parent = root_node ).

    ecatt_extprog->get_args_tmpl(
      IMPORTING
        ex_xml_arg_tmpl = lv_text
        ex_rc           = lv_rc_args_tmpl
        ex_errmsg       = lv_errmsg ).

    IF li_artmp_node IS INITIAL OR lv_rc_args_tmpl > 0.
      raise_download_exception(
          textid        = cx_ecatt_apl_util=>download_processing
          previous      = ex_ecatt
          called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE'
          free_text     = lv_errmsg ).
    ENDIF.

    lv_rc = li_artmp_node->set_value( value = lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_ARTMP_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.


  METHOD set_blob_to_template.

    " Downport

    DATA: li_blob_node TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_text      TYPE string.

    li_blob_node = template_over_all->create_simple_element(
                  name   = 'ECET_BLOBS'
                  parent = root_node ).

    IF li_blob_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

    ecatt_extprog->get_blob(
      EXPORTING
        im_whole_data = 1
      IMPORTING
        ex_xml_blob   = lv_text ).

    lv_rc = li_blob_node->set_value( value = lv_text ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_BLOB_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.


  METHOD set_control_data_for_tcd.

    " Downport

    DATA: lt_params TYPE ettcd_params_tabtype,
          lt_verbs  TYPE ettcd_verbs_tabtype,
          lt_vars   TYPE ettcd_vars_tabtype,
          lt_dp_tab TYPE ettcd_dp_tab_tabtype,
          lt_dp_for TYPE ettcd_dp_for_tabtype,
          lt_dp_pro TYPE ettcd_dp_pro_tabtype,
          lt_dp_fld TYPE ettcd_dp_fld_tabtype,
          lt_svars  TYPE ettcd_svars_tabtype.

    DATA: li_element   TYPE REF TO if_ixml_element,
          li_deep_tcd  TYPE REF TO if_ixml_element,
          lv_rc        TYPE sy-subrc,
          lv_name      TYPE string,
          lv_parname   TYPE string,
          lo_pval_xml  TYPE REF TO cl_apl_ecatt_xml_data,
          lo_ctrl_tabs TYPE REF TO cl_apl_ecatt_control_tables.

    FIELD-SYMBOLS: <lt_tab> TYPE STANDARD TABLE.

    IF is_param-xmlref_typ <> cl_apl_ecatt_const=>ref_type_c_tcd OR io_params IS INITIAL.
      RETURN.
    ENDIF.

    lv_parname = is_param-pname.

    io_params->get_param_value(     "TCD command interface
      EXPORTING
        im_var_id   = cl_apl_ecatt_const=>varid_default_val
        im_pname    = lv_parname
        im_pindex   = is_param-pindex
      IMPORTING
        ex_pval_xml = lo_pval_xml ).

    lo_ctrl_tabs = lo_pval_xml->get_control_tables_ref( ).
    IF lo_ctrl_tabs IS INITIAL.
      RETURN.
    ENDIF.

    lo_ctrl_tabs->get_control_tables(          "Read 8 control tables
      IMPORTING
        ex_params = lt_params
        ex_verbs  = lt_verbs
        ex_vars   = lt_vars
        ex_dp_tab = lt_dp_tab
        ex_dp_for = lt_dp_for
        ex_dp_pro = lt_dp_pro
        ex_dp_fld = lt_dp_fld
        ex_svars  = lt_svars ).

    IF lt_params IS INITIAL OR
       lt_verbs  IS INITIAL OR
       lt_vars   IS INITIAL OR
       lt_dp_tab IS INITIAL OR
       lt_dp_for IS INITIAL OR
       lt_dp_pro IS INITIAL OR
       lt_dp_fld IS INITIAL OR
       lt_svars  IS INITIAL.

      RETURN.
    ENDIF.

    li_deep_tcd = template_over_all->create_simple_element_ns(
                    name   = cl_apl_xml_const=>upl_tcd_node
                    parent = ap_current_param ).

    IF li_deep_tcd IS INITIAL.
      raise_download_exception(
            textid   = cx_ecatt_apl_util=>download_processing
            previous = ex_ecatt ).
    ENDIF.

    DO 8 TIMES.                                "Loop at 8 control tables
      CASE sy-index.
        WHEN 1.
          lv_name = 'ETTCD_PARAMS_TABTYPE'.
          ASSIGN lt_params TO <lt_tab>.
        WHEN 2.
          lv_name = 'ETTCD_VERBS_TABTYPE'.
          ASSIGN lt_verbs TO <lt_tab>.
        WHEN 3.
          lv_name = 'ETTCD_VARS_TABTYPE'.
          ASSIGN lt_vars TO <lt_tab>.
        WHEN 4.
          lv_name = 'ETTCD_DP_TAB_TABTYPE'.
          ASSIGN lt_dp_tab TO <lt_tab>.
        WHEN 5.
          lv_name = 'ETTCD_DP_FOR_TABTYPE'.
          ASSIGN lt_dp_for TO <lt_tab>.
        WHEN 6.
          lv_name = 'ETTCD_DP_PRO_TABTYPE'.
          ASSIGN lt_dp_pro TO <lt_tab>.
        WHEN 7.
          lv_name = 'ETTCD_DP_FLD_TABTYPE'.
          ASSIGN lt_dp_fld TO <lt_tab>.
        WHEN 8.
          lv_name = 'ETTCD_SVARS_TABTYPE'.
          ASSIGN lt_svars TO <lt_tab>.
      ENDCASE.

      CALL FUNCTION 'SDIXML_DATA_TO_DOM'       "Ast generieren lassen
        EXPORTING
          name         = lv_name
          dataobject   = <lt_tab>
        IMPORTING
          data_as_dom  = li_element
        EXCEPTIONS
          illegal_name = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.

* Ast in Hauptbaum haengen
      lv_rc = li_deep_tcd->append_child( new_child = li_element ).

      IF lv_rc <> 0.
        raise_download_exception(
              textid   = cx_ecatt_apl_util=>download_processing
              previous = ex_ecatt ).
      ENDIF.
      FREE li_element.
      UNASSIGN <lt_tab>.
    ENDDO.

    escape_control_data( ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VARS_TABTYPE'
      iv_node    = 'CB_INDEX' ).

    escape_control_data(
      ii_element = li_deep_tcd
      iv_tabname = 'ETTCD_VERBS_TABTYPE'
      iv_node    = 'NAME' ).

    FREE: lt_dp_tab, lt_dp_for, lt_dp_fld, lt_svars,
          lt_params, lt_vars,   lt_dp_pro, lt_verbs.

  ENDMETHOD.


  METHOD set_script_to_template.

    " Downport

    DATA:
      lt_text    TYPE etxml_line_tabtype,
      li_element TYPE REF TO if_ixml_element,
      lv_rc      TYPE sy-subrc.

    ecatt_script->get_script_text( CHANGING scripttext = lt_text ).

    mi_script_node = template_over_all->create_simple_element(
                        name = 'SCRIPT'
                        parent = root_node ).

    IF mi_script_node IS INITIAL.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

    CALL FUNCTION 'SDIXML_DATA_TO_DOM'
      EXPORTING
        name         = 'ETXML_LINE_TABTYPE'
        dataobject   = lt_text
      IMPORTING
        data_as_dom  = li_element
      CHANGING
        document     = template_over_all
      EXCEPTIONS
        illegal_name = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).

    ENDIF.

    lv_rc = mi_script_node->append_child( li_element ).
    IF lv_rc <> 0.
      raise_download_exception(
            textid        = cx_ecatt_apl_util=>download_processing
            previous      = ex_ecatt
            called_method = 'CL_APL_ECATT_SCRIPT_DOWNLOAD->SET_SCRIPT_TO_TEMPLATE' ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_ecatt_download~get_xml_stream.

    rv_xml_stream = mv_xml_stream.

  ENDMETHOD.
ENDCLASS.
