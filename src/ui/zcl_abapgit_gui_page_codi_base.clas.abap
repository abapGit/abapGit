CLASS zcl_abapgit_gui_page_codi_base DEFINITION PUBLIC ABSTRACT INHERITING FROM zcl_abapgit_gui_page.
  PUBLIC SECTION.
    METHODS:
      zif_abapgit_gui_page~on_event
        REDEFINITION.

  PROTECTED SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mt_result TYPE scit_alvlist .

    METHODS render_result
      IMPORTING
        !io_html   TYPE REF TO zcl_abapgit_html
        !it_result TYPE scit_alvlist .
    METHODS jump
      IMPORTING
        !is_item        TYPE zif_abapgit_definitions=>ty_item
        !is_sub_item    TYPE zif_abapgit_definitions=>ty_item
        !iv_line_number TYPE i
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
    CONSTANTS: c_object_separator TYPE char1 VALUE '|'.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_CODI_BASE IMPLEMENTATION.


  METHOD jump.
    DATA: lo_test               TYPE REF TO cl_ci_test_root,
          ls_info               TYPE scir_rest,
          lo_result             TYPE REF TO cl_ci_result_root,
          lv_check_variant_name TYPE sci_chkv,
          lv_package            TYPE devclass.
    DATA: lv_adt_jump_enabled   TYPE abap_bool.
    DATA: lv_line_number        TYPE i.
    DATA: ls_item               TYPE zif_abapgit_definitions=>ty_item.
    DATA: ls_sub_item           TYPE zif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.

    IF is_sub_item IS NOT INITIAL.
      READ TABLE mt_result WITH KEY objtype  = is_item-obj_type
                                    objname  = is_item-obj_name
                                    sobjtype = is_sub_item-obj_type
                                    sobjname = is_sub_item-obj_name
                                    line     = iv_line_number
                           ASSIGNING <ls_result>.
    ELSE.
      READ TABLE mt_result WITH KEY objtype = is_item-obj_type
                                    objname = is_item-obj_name
                                    line    = iv_line_number
                           ASSIGNING <ls_result>.
    ENDIF.
    ASSERT <ls_result> IS ASSIGNED.
    ls_item-obj_name = <ls_result>-objname.
    ls_item-obj_type = <ls_result>-objtype.

    ls_sub_item-obj_name = <ls_result>-sobjname.
    ls_sub_item-obj_type = <ls_result>-sobjtype.

    lv_package = mo_repo->get_package( ).
    lv_check_variant_name = mo_repo->get_local_settings( )-code_inspector_check_variant.

    " see SCI_LCL_DYNP_530 / HANDLE_DOUBLE_CLICK

    lv_adt_jump_enabled = zcl_abapgit_persist_settings=>get_instance( )->read( )->get_adt_jump_enabled( ).

    TRY.
        IF lv_adt_jump_enabled = abap_true.

          lv_line_number = <ls_result>-line.

          zcl_abapgit_objects_super=>jump_adt( iv_obj_name     = ls_item-obj_name
                                               iv_obj_type     = ls_item-obj_type
                                               iv_sub_obj_name = ls_sub_item-obj_name
                                               iv_sub_obj_type = ls_sub_item-obj_type
                                               iv_line_number  = lv_line_number ).
          RETURN.

        ENDIF.
      CATCH zcx_abapgit_exception.
    ENDTRY.

    TRY.
        lo_test ?= cl_ci_tests=>get_test_ref( <ls_result>-test ).

      CATCH cx_root.
        zcx_abapgit_exception=>raise( |Jump to object not supported in your NW release|  ).
    ENDTRY.

    lo_result = lo_test->get_result_node( <ls_result>-kind ).


    MOVE-CORRESPONDING <ls_result> TO ls_info.

    lo_result->set_info( ls_info ).
    lo_result->if_ci_test~navigate( ).

  ENDMETHOD.


  METHOD render_result.

    CONSTANTS: lc_limit TYPE i VALUE 500.

    DATA: lv_class TYPE string,
          lv_line  TYPE string.

    FIELD-SYMBOLS: <ls_result> TYPE scir_alvlist.


    LOOP AT it_result ASSIGNING <ls_result> TO lc_limit.

      io_html->add( '<div>' ).
      IF <ls_result>-sobjname IS INITIAL OR
         ( <ls_result>-sobjname = <ls_result>-objname AND
           <ls_result>-sobjtype = <ls_result>-sobjtype ).
        io_html->add_a( iv_txt = |{ <ls_result>-objtype } { <ls_result>-objname }|
                        iv_act = |{ <ls_result>-objtype }{ <ls_result>-objname }| &&
                                 |{ c_object_separator }{ c_object_separator }{ <ls_result>-line }|
                        iv_typ = zif_abapgit_definitions=>c_action_type-sapevent ).

      ELSE.
        io_html->add_a( iv_txt = |{ <ls_result>-objtype } { <ls_result>-objname }| &&
                                 | < { <ls_result>-sobjtype } { <ls_result>-sobjname }|
                        iv_act = |{ <ls_result>-objtype }{ <ls_result>-objname }| &&
                                 |{ c_object_separator }{ <ls_result>-sobjtype }{ <ls_result>-sobjname }| &&
                                 |{ c_object_separator }{ <ls_result>-line }|
                        iv_typ = zif_abapgit_definitions=>c_action_type-sapevent ).

      ENDIF.
      io_html->add( '</div>' ).

      CASE <ls_result>-kind.
        WHEN 'E'.
          lv_class = 'error'.
        WHEN 'W'.
          lv_class = 'warning'.
        WHEN OTHERS.
          lv_class = 'grey'.
      ENDCASE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <ls_result>-line
        IMPORTING
          output = lv_line.

      io_html->add( |<div class="{ lv_class }">Line { lv_line }: { <ls_result>-text }</div><br>| ).

    ENDLOOP.

    IF lines( it_result ) > lc_limit.
      io_html->add( |Only first { lc_limit } findings shown in list!| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.
    DATA: ls_item          TYPE zif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE zif_abapgit_definitions=>ty_item,
          lv_main_object   TYPE string,
          lv_sub_object    TYPE string,
          lv_line_number_s TYPE string,
          lv_line_number   TYPE i.


    CASE iv_action.

      WHEN zif_abapgit_definitions=>c_action-abapgit_home.
        RETURN.

      WHEN OTHERS.
        SPLIT iv_action AT c_object_separator INTO lv_main_object lv_sub_object lv_line_number_s.
        ls_item-obj_type = lv_main_object(4).
        ls_item-obj_name = lv_main_object+4(*).

        IF lv_sub_object IS NOT INITIAL.
          ls_sub_item-obj_type = lv_sub_object(4).
          ls_sub_item-obj_name = lv_sub_object+4(*).
        ENDIF.

        lv_line_number = lv_line_number_s.

        jump( is_item        = ls_item
              is_sub_item    = ls_sub_item
              iv_line_number = lv_line_number ).

        ev_state = zif_abapgit_definitions=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
