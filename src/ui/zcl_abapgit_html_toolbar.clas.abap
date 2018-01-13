CLASS zcl_abapgit_html_toolbar DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_id TYPE string OPTIONAL,
      add
        IMPORTING
          iv_txt TYPE string
          io_sub TYPE REF TO zcl_abapgit_html_toolbar OPTIONAL
          iv_typ TYPE c         DEFAULT zif_abapgit_definitions=>gc_action_type-sapevent
          iv_act TYPE string    OPTIONAL
          iv_ico TYPE string    OPTIONAL
          iv_cur TYPE abap_bool OPTIONAL
          iv_opt TYPE c         OPTIONAL
          iv_chk TYPE abap_bool DEFAULT abap_undefined
          iv_aux TYPE string    OPTIONAL
          iv_id  TYPE string    OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      render
        IMPORTING
          iv_right       TYPE abap_bool OPTIONAL
          iv_sort        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      render_as_droplist
        IMPORTING
          iv_label       TYPE string
          iv_right       TYPE abap_bool OPTIONAL
          iv_sort        TYPE abap_bool OPTIONAL
          iv_corner      TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_item,
        txt TYPE string,
        act TYPE string,
        ico TYPE string,
        sub TYPE REF TO zcl_abapgit_html_toolbar,
        opt TYPE char1,
        typ TYPE char1,
        cur TYPE abap_bool,
        chk TYPE abap_bool,
        aux TYPE string,
        id  TYPE string,
      END OF ty_item.

    TYPES tt_items TYPE STANDARD TABLE OF ty_item.

    DATA: mt_items TYPE tt_items,
          mv_id    TYPE string.

    METHODS:
      render_items
        IMPORTING
          iv_sort        TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTML_TOOLBAR IMPLEMENTATION.


  METHOD add.
    DATA ls_item TYPE ty_item.

    ASSERT iv_typ = zif_abapgit_definitions=>gc_action_type-separator  " sep doesn't have action
      OR iv_typ = zif_abapgit_definitions=>gc_action_type-onclick      " click may have no action (assigned in JS)
      OR iv_typ = zif_abapgit_definitions=>gc_action_type-dummy        " dummy may have no action
      OR iv_act IS INITIAL AND io_sub IS NOT INITIAL
      OR iv_act IS NOT INITIAL AND io_sub IS INITIAL. " Only one supplied

    ASSERT NOT ( iv_chk <> abap_undefined AND io_sub IS NOT INITIAL ).

    ls_item-txt = iv_txt.
    ls_item-act = iv_act.
    ls_item-ico = iv_ico.
    ls_item-sub = io_sub.
    ls_item-opt = iv_opt.
    ls_item-typ = iv_typ.
    ls_item-cur = iv_cur.
    ls_item-chk = iv_chk.
    ls_item-aux = iv_aux.
    ls_item-id  = iv_id.

    APPEND ls_item TO mt_items.

  ENDMETHOD.  "add


  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD. "constructor


  METHOD count.
    rv_count = lines( mt_items ).
  ENDMETHOD.


  METHOD render.

    DATA: lv_class TYPE string.

    CREATE OBJECT ro_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).
    ro_html->add( render_items( iv_sort = iv_sort ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render


  METHOD render_as_droplist.

    DATA: lv_class TYPE string.

    CREATE OBJECT ro_html.

    lv_class = 'nav-container' ##NO_TEXT.
    IF iv_right = abap_true.
      lv_class = lv_class && ' float-right'.
    ENDIF.
    IF iv_corner = abap_true.
      lv_class = lv_class && ' corner'.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).
    ro_html->add( '<ul><li>' ).
    ro_html->add_a( iv_txt = iv_label
                    iv_typ = zif_abapgit_definitions=>gc_action_type-dummy
                    iv_act = '' ).
    ro_html->add( '<div class="minizone"></div>' ).
    ro_html->add( render_items( iv_sort = iv_sort ) ).
    ro_html->add( '</li></ul>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD. "render_as_droplist


  METHOD render_items.

    DATA: lv_class     TYPE string,
          lv_icon      TYPE string,
          lv_id        TYPE string,
          lv_check     TYPE string,
          lv_aux       TYPE string,
          lv_has_icons TYPE abap_bool.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF mt_items.

    CREATE OBJECT ro_html.

    IF iv_sort = abap_true.
      SORT mt_items BY txt ASCENDING AS TEXT.
    ENDIF.

    " Check has icons or check boxes
    LOOP AT mt_items ASSIGNING <ls_item> WHERE ico IS NOT INITIAL OR chk <> abap_undefined.
      lv_has_icons = abap_true.
      lv_class     = ' class="with-icons"'.
      EXIT.
    ENDLOOP.

    IF mv_id IS NOT INITIAL.
      lv_id = | id="{ mv_id }"|.
    ENDIF.

    ro_html->add( |<ul{ lv_id }{ lv_class }>| ).

    " Render items
    LOOP AT mt_items ASSIGNING <ls_item>.
      CLEAR: lv_class, lv_icon.

      IF <ls_item>-typ = zif_abapgit_definitions=>gc_action_type-separator.
        ro_html->add( |<li class="separator">{ <ls_item>-txt }</li>| ).
        CONTINUE.
      ENDIF.

      IF lv_has_icons = abap_true.
        IF <ls_item>-chk = abap_true.
          lv_icon  = zcl_abapgit_html=>icon( 'check/blue' ).
          lv_check = ' data-check="X"'.
        ELSEIF <ls_item>-chk = abap_false.
          lv_icon = zcl_abapgit_html=>icon( 'check/grey' ).
          lv_check = ' data-check=""'.
        ELSE. " abap_undefined -> not a check box
          lv_icon = zcl_abapgit_html=>icon( <ls_item>-ico ).
        ENDIF.
      ENDIF.

      IF <ls_item>-cur = abap_true.
        lv_class = ' class="current-menu-item"'.
      ENDIF.

      IF <ls_item>-aux IS NOT INITIAL.
        lv_aux = | data-aux="{ <ls_item>-aux }"|.
      ENDIF.

      ro_html->add( |<li{ lv_class }{ lv_check }{ lv_aux }>| ).
      IF <ls_item>-sub IS INITIAL.
        ro_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = <ls_item>-typ
                        iv_act   = <ls_item>-act
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt ).
      ELSE.
        ro_html->add_a( iv_txt   = lv_icon && <ls_item>-txt
                        iv_typ   = zif_abapgit_definitions=>gc_action_type-dummy
                        iv_act   = ''
                        iv_id    = <ls_item>-id
                        iv_opt   = <ls_item>-opt ).
        ro_html->add( <ls_item>-sub->render_items( iv_sort ) ).
      ENDIF.
      ro_html->add( '</li>' ).

    ENDLOOP.

    ro_html->add( '</ul>' ).

  ENDMETHOD.  "render_items
ENDCLASS.
