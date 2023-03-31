CLASS zcl_abapgit_data_supporter DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_data_factory .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_data_supporter.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_supported_objects TYPE zif_abapgit_data_supporter=>ty_objects.

    METHODS get_supported_objects.

ENDCLASS.



CLASS zcl_abapgit_data_supporter IMPLEMENTATION.


  METHOD get_supported_objects.

    DATA:
      lt_tables  TYPE STANDARD TABLE OF tabname,
      lv_tabname TYPE tabname,
      ls_object  LIKE LINE OF mt_supported_objects,
      li_exit    TYPE REF TO zif_abapgit_exit.

    " For safety reasons, by default only customer-defined customizing tables are supported
    SELECT dd02l~tabname
      FROM dd09l JOIN dd02l
        ON dd09l~tabname = dd02l~tabname
        AND dd09l~as4local = dd02l~as4local
        AND dd09l~as4vers = dd02l~as4vers
      INTO TABLE lt_tables
      WHERE dd02l~tabclass = 'TRANSP'
        AND dd09l~tabart = 'APPL2'
        AND dd09l~as4user <> 'SAP'
        AND dd09l~as4local = 'A' "Only active tables
        AND dd02l~contflag = 'C'. "Only customizing tables

    LOOP AT lt_tables INTO lv_tabname.
      ls_object-type = zif_abapgit_data_config=>c_data_type-tabu.
      ls_object-name = lv_tabname.
      INSERT ls_object INTO TABLE mt_supported_objects.
    ENDLOOP.

    " The list of supported objects can be enhanced using an exit
    " Name patterns are allowed. For example, TABU T009*
    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_supported_data_objects( CHANGING ct_objects =  mt_supported_objects ).

  ENDMETHOD.


  METHOD zif_abapgit_data_supporter~is_object_supported.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF mt_supported_objects.

    IF mt_supported_objects IS INITIAL.
      get_supported_objects( ).
    ENDIF.

    READ TABLE mt_supported_objects TRANSPORTING NO FIELDS
      WITH TABLE KEY type = iv_type name = iv_name.
    IF sy-subrc = 0.
      rv_supported = abap_true.
    ELSE.
      " Check if object name matches pattern
      LOOP AT mt_supported_objects ASSIGNING <ls_object> WHERE type = iv_type.
        IF iv_name CP <ls_object>-name.
          rv_supported = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
