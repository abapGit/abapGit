CLASS zcl_abapgit_git_delta DEFINITION PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS decode_deltas
      IMPORTING
        iv_show_progress TYPE abap_bool DEFAULT abap_true
      CHANGING
        !ct_objects      TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .

  PRIVATE SECTION.
    CLASS-METHODS delta
      IMPORTING
        !is_object  TYPE zif_abapgit_definitions=>ty_object
      CHANGING
        !ct_objects TYPE zif_abapgit_definitions=>ty_objects_tt
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS delta_header
      IMPORTING
        !io_stream TYPE REF TO lcl_stream.

ENDCLASS.

CLASS zcl_abapgit_git_delta IMPLEMENTATION.

  METHOD decode_deltas.

    DATA: ls_object   LIKE LINE OF ct_objects,
          li_progress TYPE REF TO zif_abapgit_progress,
          lt_deltas   LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object
        USING KEY type
        WHERE type = zif_abapgit_git_definitions=>c_type-ref_d.
      INSERT ls_object INTO TABLE lt_deltas.
    ENDLOOP.

    DELETE ct_objects
      USING KEY type
      WHERE type = zif_abapgit_git_definitions=>c_type-ref_d.

    "Restore correct Delta Order
    SORT lt_deltas BY index.

    IF iv_show_progress = abap_true.
      li_progress = zcl_abapgit_progress=>get_instance( lines( lt_deltas ) ).
    ENDIF.

    LOOP AT lt_deltas INTO ls_object.
      IF li_progress IS NOT INITIAL.
        li_progress->show( iv_current = sy-tabix
                           iv_text    = 'Decode deltas' ).
      ENDIF.

      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.

  METHOD delta.

    DATA: lv_base   TYPE xstring,
          lv_result TYPE xstring,
          lv_offset TYPE i,
          lo_stream TYPE REF TO lcl_stream,
          lv_sha1   TYPE zif_abapgit_git_definitions=>ty_sha1,
          ls_object LIKE LINE OF ct_objects,
          lv_len    TYPE i,
          lv_tmp    TYPE xstring,
          lv_org    TYPE i.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object>
      WITH KEY sha COMPONENTS sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Base not found, { is_object-sha1 }| ).
    ELSEIF <ls_object>-type = zif_abapgit_git_definitions=>c_type-ref_d.
* sanity check
      zcx_abapgit_exception=>raise( |Delta, base eq delta| ).
    ENDIF.

    lv_base = <ls_object>-data.

* skip the 2 headers
    delta_header( lo_stream ).
    delta_header( lo_stream ).

    WHILE lo_stream->has_data( ) = abap_true.

      lv_org = lo_stream->eat_byte( ).

      IF lv_org >= 128. " MSB = 1

        lv_offset = 0.
        IF lv_org >= 1.
          lv_offset = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org >= 2.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org >= 4.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 65536.
        ENDIF.
        IF lv_org >= 8.
          lv_offset = lv_offset + lo_stream->eat_byte( ) * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_org >= 16.
          lv_len = lo_stream->eat_byte( ).
        ENDIF.
        IF lv_org >= 32.
          lv_len = lv_len + lo_stream->eat_byte( ) * 256.
        ENDIF.
        IF lv_org >= 64.
          lv_len = lv_len + lo_stream->eat_byte( ) * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len) INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_tmp = lo_stream->eat_bytes( lv_org ).
        CONCATENATE lv_result lv_tmp INTO lv_result IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    lv_sha1 = zcl_abapgit_hash=>sha1( iv_type = <ls_object>-type
                                      iv_data = lv_result ).

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    ls_object-index = <ls_object>-index. "Retain sort index
    APPEND ls_object TO ct_objects.

  ENDMETHOD.


  METHOD delta_header.

    DATA lv_x   TYPE x.
    DATA lv_bit TYPE c LENGTH 1.

* header is skipped for performance reasons
    DO.
      lv_x = io_stream->eat_byte( ).
      GET BIT 1 OF lv_x INTO lv_bit.
      IF lv_bit = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
