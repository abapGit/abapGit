CLASS zcl_abapgit_object_enho_clif DEFINITION PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      deserialize
        IMPORTING io_xml  TYPE REF TO zcl_abapgit_xml_input
                  io_clif TYPE REF TO cl_enh_tool_clif
        RAISING   zcx_abapgit_exception
                  cx_enh_root,
      serialize
        IMPORTING io_xml   TYPE REF TO zcl_abapgit_xml_output
                  io_files TYPE REF TO zcl_abapgit_objects_files
                  io_clif  TYPE REF TO cl_enh_tool_clif
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS: serialize_includes
      IMPORTING io_files TYPE REF TO zcl_abapgit_objects_files
                io_clif  TYPE REF TO cl_enh_tool_clif
      RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_object_enho_clif IMPLEMENTATION.

  METHOD serialize_includes.

    DATA: lt_includes TYPE enhnewmeth_tabincl_plus_enha,
          lt_source   TYPE TABLE OF abaptxt255,
          lv_include  TYPE programm.

    FIELD-SYMBOLS: <ls_include> LIKE LINE OF lt_includes.


    lt_includes = io_clif->get_enh_method_includes( ).
    LOOP AT lt_includes ASSIGNING <ls_include>.
      lv_include = io_clif->if_enh_tool~get_name( ).
      TRANSLATE lv_include USING ' ='.
      lv_include+30 = 'EM'.
      lv_include+32(8) = <ls_include>-includenr.

      CALL FUNCTION 'RPY_PROGRAM_READ'
        EXPORTING
          program_name     = lv_include
          with_lowercase   = abap_true
        TABLES
          source_extended  = lt_source
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.
      IF sy-subrc = 0.
        io_files->add_abap( iv_extra = |EM{ <ls_include>-includenr }|
                            it_abap  = lt_source ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_methods    TYPE enhnewmeth_tab.

    FIELD-SYMBOLS: <ls_attr> LIKE LINE OF lt_tab_attributes,
                   <ls_meth> LIKE LINE OF lt_tab_methods.


    io_clif->get_enhattributes(
      IMPORTING
        tab_attributes = lt_tab_attributes ).

    io_clif->get_enh_new_methodes(
      IMPORTING
        tab_methodes = lt_tab_methods ).

    serialize_includes( io_clif  = io_clif
                        io_files = io_files ).

    LOOP AT lt_tab_attributes ASSIGNING <ls_attr>.
      CLEAR: <ls_attr>-author,
             <ls_attr>-createdon,
             <ls_attr>-changedby,
             <ls_attr>-changedon.
    ENDLOOP.

    LOOP AT lt_tab_methods ASSIGNING <ls_meth>.
      CLEAR: <ls_meth>-meth_header-author,
             <ls_meth>-meth_header-createdon,
             <ls_meth>-meth_header-changedby,
             <ls_meth>-meth_header-changedon,
             <ls_meth>-meth_header-descript_id.
    ENDLOOP.

    io_xml->add( iv_name = 'TAB_ATTRIBUTES'
                 ig_data = lt_tab_attributes ).
    io_xml->add( iv_name = 'TAB_METHODS'
                 ig_data = lt_tab_methods ).

  ENDMETHOD.

  METHOD deserialize.

    DATA: lt_tab_attributes TYPE enhclasstabattrib,
          lt_tab_methods    TYPE enhnewmeth_tab,
          ls_header         TYPE vseomethod,
          ls_param          TYPE vseomepara,
          ls_exc            TYPE vseoexcep.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_tab_methods,
                   <ls_param>  LIKE LINE OF <ls_method>-meth_param,
                   <ls_exc>    LIKE LINE OF <ls_method>-meth_exc.


    io_xml->read( EXPORTING iv_name = 'TAB_ATTRIBUTES'
                  CHANGING cg_data = lt_tab_attributes ).
    io_xml->read( EXPORTING iv_name = 'TAB_METHODS'
                  CHANGING cg_data = lt_tab_methods ).

    io_clif->set_enhattributes( lt_tab_attributes ).

* todo: deserialize includes

* SAP standard SET_ENH_NEW_METHOS does not work

    LOOP AT lt_tab_methods ASSIGNING <ls_method>.

      MOVE-CORRESPONDING <ls_method>-meth_header TO ls_header.

      io_clif->add_change_new_enh_method(
        methkey       = <ls_method>-methkey
        method_header = ls_header ).

* parameters
      LOOP AT <ls_method>-meth_param ASSIGNING <ls_param>.
        MOVE-CORRESPONDING <ls_param> TO ls_param.
        io_clif->add_change_enh_methparam(
          methname   = <ls_method>-methkey-cmpname
          param_line = ls_param ).
      ENDLOOP.

* exceptions
      LOOP AT <ls_method>-meth_exc ASSIGNING <ls_exc>.
        MOVE-CORRESPONDING <ls_exc> TO ls_exc.
        io_clif->add_change_enh_methexc(
          methname    = <ls_method>-methkey-cmpname
          except_line = ls_exc ).
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
