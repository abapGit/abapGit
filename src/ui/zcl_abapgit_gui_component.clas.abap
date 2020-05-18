CLASS zcl_abapgit_gui_component DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_html_parts,
        scripts TYPE string VALUE 'scripts',
      END OF c_html_parts.

    METHODS constructor RAISING zcx_abapgit_exception.
  PROTECTED SECTION.
    DATA mi_gui_services TYPE REF TO zif_abapgit_gui_services.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_COMPONENT IMPLEMENTATION.


  METHOD constructor.
    mi_gui_services = zcl_abapgit_ui_factory=>get_gui_services( ).
  ENDMETHOD.
ENDCLASS.
