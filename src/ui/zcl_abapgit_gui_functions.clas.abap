CLASS zcl_abapgit_gui_functions DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_functions.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_FUNCTIONS IMPLEMENTATION.


  METHOD zif_abapgit_gui_functions~gui_is_available.

    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_gui_is_available.

  ENDMETHOD.


  METHOD zif_abapgit_gui_functions~is_sapgui_for_java.

    CALL FUNCTION 'GUI_HAS_JAVABEANS'
      IMPORTING
        return = rv_result.

  ENDMETHOD.

  METHOD zif_abapgit_gui_functions~is_sapgui_for_windows.
    DATA: lv_platform TYPE i.

    cl_gui_frontend_services=>get_platform(
      RECEIVING
        platform             = lv_platform
      EXCEPTIONS
        error_no_gui         = 1
        cntl_error           = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).
    IF sy-subrc <> 0.
      rv_result = abap_false.
    ENDIF.

    CASE lv_platform.
      WHEN cl_gui_frontend_services=>platform_nt351 OR
           cl_gui_frontend_services=>platform_nt40 OR
           cl_gui_frontend_services=>platform_nt50 OR
           cl_gui_frontend_services=>platform_windows95 OR
           cl_gui_frontend_services=>platform_windows98 OR
           cl_gui_frontend_services=>platform_windowsxp.
        " Everything after Windows XP is reported as Windows XP
        rv_result = abap_true.
      WHEN OTHERS.
        rv_result = abap_false.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
