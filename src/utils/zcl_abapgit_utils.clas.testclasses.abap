CLASS ltcl_is_binary DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor.

  PRIVATE SECTION.
    DATA mv_given_file TYPE xstring.
    DATA mv_act_is_binary TYPE abap_bool.
    CLASS-DATA:
      gv_nl    TYPE c LENGTH 1,
      gv_cr_lf TYPE c LENGTH 2.

    METHODS:
      cds_metadata_is_text FOR TESTING RAISING cx_static_check,
      cds_with_umlaut_is_text FOR TESTING RAISING cx_static_check,
      image_is_binary FOR TESTING RAISING cx_static_check,

      given_file
        IMPORTING
          iv_file TYPE string
        RAISING zcx_abapgit_exception,
      given_image,
      given_cds_metadata
        RAISING zcx_abapgit_exception,
      given_cds_view_with_umlaut
        RAISING zcx_abapgit_exception,

      when_is_binary_determined
        RAISING zcx_abapgit_exception,

      then_is_not_binary,
      then_is_binary.

ENDCLASS.


CLASS ltcl_is_binary IMPLEMENTATION.

  METHOD class_constructor.

    gv_nl    = cl_abap_char_utilities=>newline.
    gv_cr_lf = cl_abap_char_utilities=>cr_lf.

  ENDMETHOD.

  METHOD cds_metadata_is_text.

    given_cds_metadata( ).
    when_is_binary_determined( ).
    then_is_not_binary( ).

  ENDMETHOD.


  METHOD cds_with_umlaut_is_text.

    given_cds_view_with_umlaut( ).
    when_is_binary_determined( ).
    then_is_not_binary( ).

  ENDMETHOD.


  METHOD image_is_binary.

    given_image( ).
    when_is_binary_determined( ).
    then_is_binary( ).

  ENDMETHOD.


  METHOD given_file.

    mv_given_file = zcl_abapgit_convert=>string_to_xstring_utf8( iv_file ).

  ENDMETHOD.


  METHOD given_image.

    mv_given_file =
        zcl_abapgit_convert=>base64_to_xstring(
              `iVBORw0KGgoAAAANSUhEUgAAALEAAAA1CAYAAAAOCAoLAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8Y`
           && `QUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAecSURBVHhe7ZvLilxVFIb1BfQFdOxAcaAgJIgX8B4HOkkPHB`
           && `kxOjFiFBRaEIzowIEYBVGQBMULeIsjFeNI0YGT6EzsB+jETuIblP0V7mbVqn/vs06dc6rqdPbgg+4+a1/`
           && `O3t++VvVV29vbk0plzFSJK6OnSlwZPVXiyuipEldGT5W4MnoGk3jn848nl44/Pdn58D35vFLpi0Ekvvjy`
           && `85N/b795D35XcZVKH/QusRe4ilwZml4lzgmcqCJXhqBXiXc2j0t5LVXkSt/0IvGFn37Y+7mKXFk2nSVOW`
           && `4h/Pvto729V5Moy6SRxEjjhRb549LGZ554q8v7j15/PTs5+/80U9XwIFpbYC5ywIidKM/OqRf7j3O+Tt9`
           && `54QfLjd1/LNBUNbXbbDVfvsXHopsnW33/J2D5ZSOKcwInzv/0yl+bSI/fIWFilyGe+ODXT8BY6RaXZbzC`
           && `QP3j3xOTYEw9OjmwcnGmDu2+9dvo32oJZVqUHZLXpEu+/86qM75PWEjcJDDLdmm4t/Oxh2e8SI6WXtomH`
           && `77x+8unpk3N5sWqp+GW0YSuJIwKD3FK8/oqMtaxCZGaQT069Pdl46MaVdMCq4J39+7ZB5aniSjMxz1Lc4`
           && `4cPyJgIYYmjAk+5/+DMtVtinW8t1LZiv0pcWn0AoRLqOah8rZTAxFDaE5/YfHIvdnCJWwn8P2wfSIvMY7`
           && `h+U8vhfpQ4t+wfuuO66eys0qgBruKA/TVlRA7FlJnyG1TiRQQGKzG/r7vIV4rEVpxE04wJPo2KaYMfGIN`
           && `JHNnH5vASw9xeeWur9+u3dE/JafvMl6enP5dO1YkmiZlh0v0ntLk6Ijalo17pZ/JU8W3w9VIxidw+ONI+`
           && `Xn4VE8VvO2AQibkmU2JFURJD2+s3dUj0IMlLz25MT86+cRJcFSFlTr6cxPw9d4J/bfNoUWbScm2l0ia4S`
           && `yXOp6VsFZ8OSgip3je9p88P1B73xWOHZayH/SvpUx72mWq7hK2L3QNHyL2HJyvxhW+/klJFyUnM774sYm`
           && `2MZefkm3PxFiSi41QjKBBS5aM6ojQoEkio8mt7+md5telzElN/ylTPLAxqmx+ouMgs3ET0rl0NohKdJWa`
           && `pv3zfASlWhJzEama9/Oi9MzEWJb2FmUQ1AA121y3XyGdq5ivNJk34xs4NLJZkdZUHDBibB3nm6h/FDozc`
           && `+9kyF4Vy1F4bVivxLiz9i4qck3jm+m13oJQOjpGthO9oGtMu8Wr/xbJm84BcJ5M/A4UGzQ0YhG3KK20Dg`
           && `Popmf0embhnjjwwFwe2XrkYu0qoOiGVLa8rqq2tiMz61EPF0R48s0TPDEWJYVGRsxIHiQgMdCCdkWZeNX`
           && `p9g6nOo9F8HHnbAQG5bQLpbV6pTsBzv2xTz1IepTg1CFUcpPqr56oduqDaUPWHiutSl0aJYRGRu0gcFTh`
           && `KpMGiHQBq6czF5lBSRSVWceDjbKzKpyQOg440Tdi6RNtQxQ0uMbQRmbiZLcNzT8k4Rd8CQ6TBoh0A6pSd`
           && `i81BvM/DCtE2DngvH5v2xWoFKYmjylXY9157iSEq8pyIwUNiF4HZP/HFFBqNkzmn+ESkwdpIrDqYrYeKJ`
           && `V/iIX1LDNTNh5JTlaXiQEmc3kG9H/g8Ern9vyflD9E2VHFLkxgiIp//89xcutI1GiwqMHs+pPWNUmIIie`
           && `l0G8MMGLmisyg5VVkqDkoSM8j9M8hdsTFzkxaU0OkgZtNE21DFLVViaBKZO+aZNMzEhWu0LgI33ZmqW4A`
           && `hJLaxpXtT4ACq9tVKTlWWigMlsY31NzmgDomeqHTRNozmF2UhiaEkMsLyfBrbwzVajtxVjZ9dfExXidWV`
           && `lo1VA4fn9qYjKmc0DtTAsG2h9vJcD/obGE9UumgbRvOLsrDE0DQjl2Zf6CIwKJlUB/uYLh1Ah/s4sOX6Z`
           && `8hl84C+JUZWHwc2JrelYDUriazaZt9IDJE9sqKrwMCL+8awn1KB6lzVYHJW3+1cP6vnDjxJAiW5/zAkl0`
           && `9UYg6I9oMAylTbKnXYVLMxkF6VD31LrAZTZEXI0VliaCtyHwJDbnnkm2KQO/DRYT4vJUuC+NyNAvhDnYp`
           && `BPOpEObl8/ACEUr3STQfvrJ6r/KbCi+1OgrqRJ+XyBafceyuJ1Vkgt+f2cUA7Uy7QXr5dc/QiMURF7ktg`
           && `aPslG4+dqUqylOCwZGdFUCtEG3ivrvXKXflBk8gRrMSRfvBCRtpIrWCK3iSGJpH7FDjR1Bl0ptoqgO0It`
           && `b9u6hwEVkuwWlYtpCPvXEcibspLSdz0zjxvWpp5Tt7qxiJCUx09tq2hqY0Sqn09vUoMOZGHEBjoDEa57w`
           && `ykTMsp+1oa0WOXOn5Wz0jrBacsyvQzsIXG97JxwLPpGFy2zETTTEze4OtF/sQ3CWwhlvKol7rdSFAvYoj`
           && `1+acBWUJtK9Q7AO1GvD+P5OhdYkDk9EV3hB5K4GXTRo6+yEmsYvtkFe+6aJmDSDxla2v6/Qn16V0lzqok`
           && `HhPDSVzphSpxM1XiNYa9s/ofvfTPpqtY8teRKvGaombgHCr9lUSVeE2pEsepEq8p7HsROYJKfyVRJa6Mn`
           && `ipxZfRUiSujp0pcGT1V4sroqRJXRk+VuDJytif/Aa8ZlmVpfNVQAAAAAElFTkSuQmCC` ).

  ENDMETHOD.


  METHOD given_cds_metadata.

    given_file( `{`
    && gv_nl && `"BASEINFO":`
    && gv_nl && `{`
    && gv_nl && `"FROM":`
    && gv_nl && `[`
    && gv_nl && `"T100"`
    && gv_nl && `],`
    && gv_nl && `"ASSOCIATED":`
    && gv_nl && `[],`
    && gv_nl && `"BASE":`
    && gv_nl && `[],`
    && gv_nl && `"ANNO_REF":`
    && gv_nl && `[],`
    && gv_nl && `"VERSION":0`
    && gv_nl && `}`
    && gv_nl && `}` ).

  ENDMETHOD.


  METHOD given_cds_view_with_umlaut.

    CONSTANTS lc_umlaut_ue TYPE xstring VALUE `C3BC`.

    given_file(     `@EndUserText.label: `
                 && zcl_abapgit_convert=>xstring_to_string_utf8( lc_umlaut_ue )
                 && `bernahmekandidat'`
     && gv_cr_lf && `@AbapCatalog.sqlViewName: 'ZTESTDDLSBUG2'`
     && gv_cr_lf && `@AbapCatalog.compiler.compareFilter: true`
     && gv_cr_lf && `@AbapCatalog.preserveKey: true`
     && gv_cr_lf && `@AccessControl.authorizationCheck: #CHECK`
     && gv_cr_lf && `define view ztest_ddls_bug2`
     && gv_cr_lf && `  as select from t100`
     && gv_cr_lf && `{`
     && gv_cr_lf && `  key sprsl as Sprsl,`
     && gv_cr_lf && `  key arbgb as Arbgb,`
     && gv_cr_lf && `  key msgnr as Msgnr,`
     && gv_cr_lf && `      text  as Text`
     && gv_cr_lf && `}` ).

  ENDMETHOD.

  METHOD when_is_binary_determined.

    mv_act_is_binary = zcl_abapgit_utils=>is_binary( mv_given_file ).

  ENDMETHOD.


  METHOD then_is_not_binary.

    cl_abap_unit_assert=>assert_equals(
      act = mv_act_is_binary
      exp = abap_false ).

  ENDMETHOD.


  METHOD then_is_binary.

    cl_abap_unit_assert=>assert_equals(
      act = mv_act_is_binary
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
