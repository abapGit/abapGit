"! BMFR - Application Component
CLASS zcl_abapgit_object_bmfr DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_object_bm_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    ALIASES:
      mo_files FOR zif_abapgit_object~mo_files.
    CLASS-METHODS:
      read_component_id IMPORTING iv_object_name         TYPE sobj_name
                        RETURNING VALUE(rv_component_id) TYPE ufps_posid
                        RAISING   zcx_abapgit_exception,
      read_object_name_by_comp_id IMPORTING iv_component_id       TYPE ufps_posid
                                  RETURNING VALUE(rv_object_name) TYPE sobj_name
                                  RAISING   zcx_abapgit_exception.
  PROTECTED SECTION.
    METHODS:
      serialize_specific_data REDEFINITION,
      deserialize_specific_data REDEFINITION,
      get_specific_data_descr REDEFINITION,
      map_remote_to_local_tadir REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      c_bmfr_struct_name TYPE strukname VALUE 'BMT_FUNCTION_RANGE'.
ENDCLASS.



CLASS zcl_abapgit_object_bmfr IMPLEMENTATION.
  METHOD read_component_id.
    SELECT SINGLE ps_posid
      FROM df14l
      INTO rv_component_id
      WHERE fctr_id = iv_object_name.
    IF sy-subrc <> 0 OR rv_component_id IS INITIAL.
      zcx_abapgit_exception=>raise( |pos_id for application component { iv_object_name } not found.| ).
    ENDIF.
  ENDMETHOD.

  METHOD read_object_name_by_comp_id.
    DATA: lt_object_names TYPE STANDARD TABLE OF uffctr.

    SELECT fctr_id
      FROM df14l
      INTO TABLE lt_object_names
      WHERE ps_posid = iv_component_id.

    CASE lines( lt_object_names ).
      WHEN 0.
        rv_object_name = space.
      WHEN 1.
        READ TABLE lt_object_names INDEX 1 INTO rv_object_name.
      WHEN OTHERS.
        zcx_abapgit_exception=>raise( |Component ID { iv_component_id } is not unique in system.| ).
    ENDCASE.
  ENDMETHOD.

  METHOD serialize_specific_data.
    FIELD-SYMBOLS: <ls_specific_data> TYPE bmt_function_range.

    ASSIGN ig_specific_data TO <ls_specific_data>.
    ASSERT sy-subrc = 0.

    CLEAR: <ls_specific_data>-fstuser,
           <ls_specific_data>-fsttime,
           <ls_specific_data>-fstdate,
           <ls_specific_data>-lstuser,
           <ls_specific_data>-lsttime,
           <ls_specific_data>-lstdate,
           <ls_specific_data>-fctr_id,
           <ls_specific_data>-as4local,
           <ls_specific_data>-tstamp,
           <ls_specific_data>-langu,
           <ls_specific_data>-tstamp,
           <ls_specific_data>-uname1,
           <ls_specific_data>-uname2,
           <ls_specific_data>-srcsystem,
           <ls_specific_data>-ariid.

    io_xml->add( iv_name = c_bmfr_struct_name
                 ig_data = <ls_specific_data> ).
  ENDMETHOD.

  METHOD deserialize_specific_data.
    DATA: lt_components TYPE abap_compdescr_tab.
    FIELD-SYMBOLS: <ls_specific_data> TYPE bmt_function_range,
                   <ls_component>     TYPE abap_compdescr.

    ASSIGN cg_specific_data TO <ls_specific_data>.
    ASSERT sy-subrc = 0.

    io_xml->read( EXPORTING iv_name = c_bmfr_struct_name
                  CHANGING  cg_data = <ls_specific_data> ).

    <ls_specific_data>-fctr_id = ms_item-obj_name.
    <ls_specific_data>-as4local = 'A'.
    <ls_specific_data>-langu = mv_language.

    lt_components = get_specific_data_descr( )->components.

    LOOP AT lt_components ASSIGNING <ls_component>.
      APPEND <ls_component>-name TO et_specific_fields.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_specific_data_descr.
    ro_descriptor ?= cl_abap_typedescr=>describe_by_name( c_bmfr_struct_name ).
  ENDMETHOD.

  METHOD map_remote_to_local_tadir.
    DATA: lv_component_id TYPE ufps_posid.

    lv_component_id = is_item-obj_name.
    rv_object_name = read_object_name_by_comp_id( lv_component_id ).
  ENDMETHOD.
ENDCLASS.
