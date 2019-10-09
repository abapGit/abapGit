
CLASS ltcl_zlib DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      fixed FOR TESTING RAISING cx_dynamic_check,
      dynamic FOR TESTING RAISING cx_dynamic_check,
      not_compressed FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.


CLASS ltcl_zlib IMPLEMENTATION.

  METHOD fixed.

    DATA: ls_data TYPE zcl_abapgit_zlib=>ty_decompress.

    CONSTANTS:
      lc_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      lc_compressed TYPE xstring VALUE 'F348CDC9C95708CF2FCA4951E4E5020024E90455'.


    ls_data = zcl_abapgit_zlib=>decompress( lc_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lc_raw ).

  ENDMETHOD.

  METHOD not_compressed.

    DATA: ls_data TYPE zcl_abapgit_zlib=>ty_decompress.

    CONSTANTS:
      lc_raw        TYPE xstring VALUE '4142434445464748494A4B4C',
      lc_compressed TYPE xstring VALUE '010C00F3FF4142434445464748494A4B4C'.


    ls_data = zcl_abapgit_zlib=>decompress( lc_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lc_raw ).

  ENDMETHOD.

  METHOD dynamic.

    DATA: ls_data        TYPE zcl_abapgit_zlib=>ty_decompress,
          lv_compressed  TYPE xstring,
          lv_decoded     TYPE xstring,
          lv_hex         TYPE xstring,
          lv_str         TYPE string.

    DEFINE _hex.
      lv_hex = &1.
      CONCATENATE lv_compressed lv_hex INTO lv_compressed IN BYTE MODE.
    END-OF-DEFINITION.

    _hex '75555D6FE246147DF7AFB8521F1610A0AA8F911A'.
    _hex 'C9C1A6B5969034711A352FCE605FF074CD8C3B33'.
    _hex '86657FFD9E191302DBEC8B857DBFCFB9F7F00BED'.
    _hex '2BD1EC8A52ABB5DC448FF13D3DF38A92836A8DA6'.
    _hex 'F806EFF1E26FEACD9D114E9B288A46A34C59279A'.
    _hex '66348A46946D5B6D1CB99A49AFFEE5D2595A1BBD'.
    _hex '25D56D36EC682D1BA64E556CA8118EADA38D7464'.
    _hex 'B8616161B0526D087517D9F23392C5A5933BB811'.
    _hex 'B29FA7846966D81B9E13126DDBC85238A9156A69'.
    _hex '432F68B298DDDDDEE3B19C677FF81613DEEAD0DF'.
    _hex '8771EF61497A7B87ECE9572E3BA4F7639CE53FB3'.
    _hex '9C8370EE42831FCB0F7DB7B5501B26ABB77C848F'.
    _hex '76A2E9D892501559B163383D70233A55D6FD00BE'.
    _hex '938BC44E93E5BEA532A4B301FC27CB186C427155'.
    _hex '013F812A7A1D32A010B850ACC086469DC3F9875A'.
    _hex '3878A3BE2719ECA9CA37D2B3FB16358111483E27'.
    _hex '451EDF2C52D408442B273DF27ECCB38C3EBC15D6'.
    _hex '861CC73E02FFBE1D6DE4462AD104239A09B12B34'.
    _hex '045E3DE35239366B51326DD9D51A88B02BFC2686'.
    _hex '44538AA2D7D757B1126DD4E8A2DCB6FD77FA9D08'.
    _hex '1B5BB85ADAC9357E942D462ABE217040C36994CD'.
    _hex 'E9DC7D725D0B5B08BF528C153F4E094FCA1E295B'.
    _hex '6679162FA61152FE1055864DBB889846E932C9E6'.
    _hex 'D3284AE23CF6FEA709B017CEE8A6C182E7FFDCA7'.
    _hex 'F490CE29BFA36F725F4A3F124DFD0C1FB9FF7F1A'.
    _hex 'B92EDFA7F949D8E4FA02AB4118E0049D8708E54E'.
    _hex 'EFB00218801981CD396E05875D7580442BB2252B'.
    _hex '61A4B657E0F98D88A77B4C9816C94D317FB8BB3D'.
    _hex '1E94A7F404207E9D26BEF81E489E505973F985E4'.
    _hex '3ADC52D919E317D21EACE32D494B07DD19AA78C7'.
    _hex '8D6EB7EFA629E5609558D9CE604B9DDFD7177262'.
    _hex 'D5F89B31908AB6C21656B497AEF62D40B07CCF82'.
    _hex 'F0599056CD015B4509EF8EB5CE978868F6673AFB'.
    _hex '0CCB048DC88ACA963E25A34F3D3213CA8D5036C8'.
    _hex 'D8DEE0FA41E52C14B80A906988823966B5BE8817'.
    _hex '09770A6985AB077FC5E32C1F3FE5E3FB6438262B'.
    _hex 'B72DFA79F709117E160CE88CC44841235F72DCD4'.
    _hex 'E86DCA3DCABCA945E10E2D83CC5F7FA3C1F3454F'.
    _hex '43AF6E4BED7A11B8F5870C593EE0A6B9BDFA998E'.
    _hex 'D1E081FFEBA4017E5E300396FE0E4F087F8CEA90'.
    _hex '560C7F26D329E5FD7B09F0D8F7FF0614B7AD9789'.
    _hex '75B0AC21664755F6FF003D4947C74123BF304DA0'.
    _hex 'FCA536204077CA8D69DD41E72D0E8ED598F86BC9'.
    _hex '0D9E9E8861E01CCAB1626ABB55236D8DDE012316'.
    _hex '558082338D26C5DEA4A986AA92D215440566DFAE'.
    _hex '9F75FEB49CE5D9DDB24817E96DBACC1F7B176C2E'.
    _hex '86F14BF21DD2B86583A502789C33343030333151'.
    _hex '284A4D4CC94DD5CB4D61E8F9CF526533B9F99E89'.
    _hex 'F5DD0E11137B1563BF8D9300E5B40D39B801789C'.
    _hex '0BCBCF4CD15308CFCCC951484A554849CD492D49'.
    _hex '050A700100650107AFB643789C5D524B8FDA3010'.
    _hex 'BEFB578C38ED4AD1F621F5D29B49CC6235C49163'.
    _hex '96720C8921AE428C62A768FF7D6702BBDB564242'.
    _hex 'F3F85EE398CEC2461AC85D638760E1018B47C652'.
    _hex '7F791DDDA98BF0D03CC2D7CF5FBE81AEBBA9077E'.
    _hex 'AD47CB5869C7B30BC1F9015C80CE8EF6F00AA7B1'.
    _hex '1EA26D13388ED6823F42D3D5E3C926103DD4C32B'.
    _hex '5CEC1810E00FB176831B4E5043833A0C37638734'.
    _hex 'C11F23D1E3720B7508BE7135F241EB9BE96C8758'.
    _hex '47D23BBADE067888687C51DD118BC759A4B575CF'.
    _hex 'DC00347B1BC1D5C5CE4F11461BE2E81AE248C00D'.
    _hex '4D3FB5E4E16DDCBBB3BB2B107C0E1F18924E0113'.
    _hex '90CF04CEBE7547FAB773ACCB74E85DE812681D51'.
    _hex '1FA688CD40CDF99609E5F8E44708B6EF193238F4'.
    _hex '3D67FD7037EF90F50B1D34DE4F14A873EDFCF9DF'.
    _hex '242EB0E3340E2869674CEBF164B3E22FDB44EAD0'.

    lv_decoded = zcl_abapgit_convert=>string_to_xstring_utf8(
         |# wdalv_config\nSAP Web Dynpro ABAP ALV |
      && |configurator\n\n\n**Install**\n* Import |
      && |the objects from nugget file under lates|
      && |t git release using SAPLINK\n* Activate |
      && |all the objects\n* Create WD application|
      && |s for ZALV_COMP_CONFIG\n\n**Demo**\n* Cr|
      && |eate WD application for ZALV_DEMO\n* Exe|
      && |cute the application\n* Execute configur|
      && |ator application (ZALV_COMP_CONFIG)\n* C|
      && |hange some config values and save\n* Rel|
      && |aunch ZALV_DEMO application to see the c|
      && |hanges\n\n\n**Use**\n- Add usage of ZALV|
      && | component to any component that uses SA|
      && |P standand ALV component - SALV_WD_TABLE|
      && |\n- Instantiate ZALV component and pass |
      && |ALV usage object of original ALV to ZALV|
      && | by calling interface method set_alv_usa|
      && |ge. \n\n```abap\nlo_cmp_usage =   wd_thi|
      && |s->wd_cpuse_zalv( ).\nIF lo_cmp_usage->h|
      && |as_active_component( ) IS INITIAL.\n    |
      && |lo_cmp_usage->create_component( ).\nENDI|
      && |F.\n\nDATA lo_interfacecontroller TYPE R|
      && |EF TO ziwci_alv .\nlo_interfacecontrolle|
      && |r =   wd_this->wd_cpifc_zalv( ).\n\nlo_i|
      && |nterfacecontroller->set_alv_usage(\n    |
      && |alv_usage =  lo_alv_usage\n  ).\n```\n\n|
      && |- For production scenarios: In method UP|
      && |DATE_DB_FROM_CONFIG of component control|
      && |ler of component ZALV - check if the cur|
      && |rent system is your development system. |
      && |This ensures that Z tables are updated w|
      && |ith config meta data only in Dev system.|
      && | \n\n```abap\n  CHECK sy-sysid cp 'D*'.\n|
      && |```\n- Transport wrapper Config: For ot|
      && |her systems in the transport path(QA,IT,|
      && |UT,PD), simply transport the table entri|
      && |es from ZTALV* tables where config_type |
      && |= 02 (Wrapper Config)\n\n**Note**\n- Man|
      && |datory step: Execute the application (Re|
      && |quired for updating Z tables with config|
      && | meta data ) before running ALV configur|
      && |ator App\n- If ALV functions from config|
      && |urator (like - record count, full screen|
      && |, excel export) are to be published then|
      && | main application need to have node mapp|
      && |ing for FUNCTION_ELEMENTS node of ALV. \n\n| ).

    ls_data = zcl_abapgit_zlib=>decompress( lv_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lv_decoded ).

  ENDMETHOD.

ENDCLASS.
