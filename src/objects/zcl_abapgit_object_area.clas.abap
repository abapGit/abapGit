CLASS zcl_abapgit_object_area DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_objects_super
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_object .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_object_area IMPLEMENTATION.


  METHOD zif_abapgit_object~changed_by.

    DATA: lv_user TYPE string.

    SELECT SINGLE tstpnm FROM ('RSDAREA') INTO lv_user.

    rv_user = lv_user.

  ENDMETHOD.


  METHOD zif_abapgit_object~delete.

    DATA:
      lr_area         TYPE REF TO object.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~DELETE_NODE')
      EXPORTING
        i_nodename    = ms_item-obj_name
        i_with_dialog = ''.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error while deleting AREA: { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~deserialize.

    DATA:
      lv_nodename   TYPE c LENGTH 40,
      lv_parentname TYPE c LENGTH 40,
      lv_txtsh      TYPE c LENGTH 20,
      lv_txtlg      TYPE c LENGTH 60,
      lr_area       TYPE REF TO object.

    io_xml->read( EXPORTING iv_name = 'NODENAME'
                  CHANGING cg_data = lv_nodename  ).

    io_xml->read( EXPORTING iv_name = 'PARENTNAME'
                  CHANGING  cg_data = lv_parentname ).

    io_xml->read( EXPORTING iv_name = 'TXTSH'
                  CHANGING  cg_data = lv_txtsh ).

    io_xml->read( EXPORTING iv_name = 'TXTLG'
                  CHANGING  cg_data = lv_txtlg ).

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~CREATE_NODE')
      EXPORTING
        i_parentname = lv_parentname
        i_nodename   = lv_nodename
        i_txtsh      = lv_txtsh
        i_txtlg      = lv_txtlg.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error while creating AREA: { ms_item-obj_name }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.


  METHOD zif_abapgit_object~exists.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = ''
        i_langu   = ''
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
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

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_tree> TYPE STANDARD TABLE,
      <ls_tree> TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    IF sy-subrc = 0.
      rv_active = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ERSDAREA' ).
  ENDMETHOD.


  METHOD zif_abapgit_object~jump.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.


  METHOD zif_abapgit_object~serialize.

    DATA:
      lr_area     TYPE REF TO object,
      lr_tab_tree TYPE REF TO data,
      lr_str_tee  TYPE REF TO data,
      lr_rsdareat TYPE REF TO data,
      lv_select   TYPE string.

    FIELD-SYMBOLS:
      <lt_tree>       TYPE STANDARD TABLE,
      <ls_tree>       TYPE any,
      <lv_parentname> TYPE any,
      <ls_rsdareat>   TYPE any,
      <lv_txtlg>      TYPE any,
      <lv_txtsh>      TYPE any.

    CREATE OBJECT lr_area TYPE ('CL_NEW_AWB_AREA').

    CREATE DATA lr_tab_tree TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_tab_tree->* TO <lt_tree>.

    CREATE DATA lr_str_tee TYPE STANDARD TABLE OF ('RSAWBN_S_TREEORG').
    ASSIGN lr_str_tee->* TO <ls_tree>.

    CREATE DATA lr_rsdareat TYPE ('RSDAREAT').
    ASSIGN lr_rsdareat->* TO <ls_rsdareat>.

    CALL METHOD lr_area->('IF_RSAWBN_FOLDER_TREE~GET_TREE')
      EXPORTING
        i_objvers = 'A'
        i_langu   = mv_language
      IMPORTING
        e_t_tree  = <lt_tree>.

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Error while read AREA tree| ).
    ENDIF.

    READ TABLE <lt_tree> WITH KEY ('NODENAME') = ms_item-obj_name ASSIGNING <ls_tree>.

    lv_select = |INFOAREA = '{ ms_item-obj_name }'|.

    SELECT SINGLE * FROM ('RSDAREAT')
    INTO <ls_rsdareat>
    WHERE infoarea = ms_item-obj_name.

    ASSIGN COMPONENT 'TXTSH' OF STRUCTURE <ls_rsdareat> TO <lv_txtsh>.
    ASSIGN COMPONENT 'TXTLG' OF STRUCTURE <ls_rsdareat> TO <lv_txtlg>.


    ASSIGN COMPONENT 'PARENTNAME' OF STRUCTURE <ls_tree> TO <lv_parentname>.

    io_xml->add( iv_name = 'NODENAME'
                 ig_data =  ms_item-obj_name ).

    io_xml->add( iv_name = 'PARENTNAME'
                 ig_data = <lv_parentname> ).

    io_xml->add( iv_name = 'TXTSH'
                 ig_data = <lv_txtsh> ).

    io_xml->add( iv_name = 'TXTLG'
                 ig_data = <lv_txtlg> ).

  ENDMETHOD.
ENDCLASS.
