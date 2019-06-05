CLASS zcl_abapgit_transport_mass DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_transport
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS run .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_TRANSPORT_MASS IMPLEMENTATION.


  METHOD run.

    DATA:
      lt_trkorr          TYPE trwbo_request_headers.

    DATA:
      lo_transport_zipper TYPE REF TO lcl_transport_zipper,
      lo_except           TYPE REF TO cx_root,
      lv_folder           TYPE string.

    TRY.

        lcl_gui=>select_tr_requests( IMPORTING et_trkorr = lt_trkorr ).

        IF lt_trkorr[] IS NOT INITIAL.

* Call destination folder popup
          lcl_gui=>f4_folder( CHANGING cv_folder = lv_folder ).

          IF lv_folder IS INITIAL.
* Empty folder
            zcx_abapgit_exception=>raise( 'Empty destination folder'(006) ).
          ENDIF.

* Instantiate transport zipper object that will also create the timestamped output folder
          CREATE OBJECT lo_transport_zipper TYPE lcl_transport_zipper
            EXPORTING
              iv_folder = lv_folder.

* Generate the local zip files from the given list of transport requests
          lo_transport_zipper->generate_files(
            it_trkorr = lt_trkorr
            iv_logic  = zcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).

* Open output folder if user asked it
          lcl_gui=>open_folder_frontend( iv_folder = lo_transport_zipper->gv_full_folder  ).

        ELSE.
* No data found for the provided selection criterias
          zcx_abapgit_exception=>raise( 'No transport requests selected'(007) ).
        ENDIF.

      CATCH cx_wrong_data
            zcx_abapgit_exception INTO lo_except.

        MESSAGE lo_except->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
