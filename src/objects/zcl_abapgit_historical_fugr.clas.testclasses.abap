CLASS ltcl_fugr DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_fugr DEFINITION LOCAL FRIENDS ltcl_fugr.

CLASS ltcl_fugr DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_fugr.

    METHODS setup.
    METHODS names_files FOR TESTING.
    METHODS names_namespaced_files FOR TESTING.
    METHODS serializes_group FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_include FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_main_program FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.

    METHODS assert_contains
      IMPORTING
        iv_xml TYPE string
        iv_sub TYPE string.

    METHODS assert_not_contains
      IMPORTING
        iv_xml TYPE string
        iv_sub TYPE string.
ENDCLASS.


CLASS ltcl_fugr IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'FUGR'
      obj_name = 'ZTEST_FUGR' ) ).

  ENDMETHOD.


  METHOD assert_contains.

    cl_abap_unit_assert=>assert_differs(
      act = find( val = iv_xml
                  sub = iv_sub )
      exp = -1
      msg = |{ iv_sub } is missing| ).

  ENDMETHOD.


  METHOD assert_not_contains.

    cl_abap_unit_assert=>assert_equals(
      act = find( val = iv_xml
                  sub = iv_sub )
      exp = -1
      msg = |{ iv_sub } is not expected| ).

  ENDMETHOD.


  METHOD names_files.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( 'xml' )
      exp = 'ztest_fugr.fugr.xml' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_extra = 'LZTEST_FUGRTOP'
                                   iv_ext   = 'abap' )
      exp = 'ztest_fugr.fugr.lztest_fugrtop.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_extra = 'Z_TEST_FUNCTION'
                                   iv_ext   = 'abap' )
      exp = 'ztest_fugr.fugr.z_test_function.abap' ).

  ENDMETHOD.


  METHOD names_namespaced_files.

    mo_cut = NEW #( VALUE #(
      object   = 'FUGR'
      obj_name = '/NSP/TEST' ) ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_extra = '/NSP/SAPLTEST'
                                   iv_ext   = 'abap' )
      exp = '#nsp#test.fugr.#nsp#sapltest.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_extra = '/NSP/FUNCTION'
                                   iv_ext   = 'abap' )
      exp = '#nsp#test.fugr.#nsp#function.abap' ).

  ENDMETHOD.


  METHOD serializes_group.

    DATA lt_includes TYPE zcl_abapgit_historical_fugr=>ty_includes.
    DATA lt_modules  TYPE zcl_abapgit_historical_fugr=>ty_modules.
    DATA ls_module   TYPE zcl_abapgit_historical_fugr=>ty_module.

    INSERT VALUE #( name = 'LZTEST_FUGRTOP' ) INTO TABLE lt_includes.
    INSERT VALUE #( name = 'SAPLZTEST_FUGR' ) INTO TABLE lt_includes.

    ls_module-function = VALUE #(
      funcname      = 'Z_TEST_FUNCTION'
      global_flag   = 'X'
      remote_call   = 'R'
      short_text    = 'Test function'
      import        = VALUE #( ( parameter = 'IV_NAME' typ = 'STRING' ) )
      exception     = VALUE #( ( exception = 'NOT_FOUND' ) )
      documentation = VALUE #(
        ( parameter = 'IV_NAME' kind = 'P' stext = 'Name' )
        ( parameter = 'NOT_FOUND' kind = 'X' ) ) ).
    INSERT ls_module INTO TABLE lt_modules.

    DATA(lv_xml) = mo_cut->serialize_xml(
      iv_areat    = 'Test group'
      it_includes = lt_includes
      it_modules  = lt_modules ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<?xml version="1.0" encoding="utf-8"?>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = 'serializer="LCL_OBJECT_FUGR" serializer_version="v1.0.0"' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<AREAT>Test group</AREAT>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SOBJ_NAME>LZTEST_FUGRTOP</SOBJ_NAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SOBJ_NAME>SAPLZTEST_FUGR</SOBJ_NAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<FUNCNAME>Z_TEST_FUNCTION</FUNCNAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<REMOTE_CALL>R</REMOTE_CALL>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SHORT_TEXT>Test function</SHORT_TEXT>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<RSIMP>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<TYP>STRING</TYP>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<RSEXC>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<RSFDO>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<STEXT>Name</STEXT>' ).
* nothing beyond the supported scope
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<DYNPROS>' ).
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<I18N_TPOOL>' ).

  ENDMETHOD.


  METHOD serializes_include.

    DATA ls_include TYPE zcl_abapgit_historical_fugr=>ty_include.

    ls_include-name = 'LZTEST_FUGRTOP'.
    ls_include-program-trdir = VALUE #(
      subc    = 'I'
      fixpt   = 'X'
      uccheck = 'X'
      unam    = 'DEVELOPER' ).

    DATA(lv_xml) = mo_cut->serialize_include_xml( ls_include ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<?xml version="1.0" encoding="utf-8"?>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<abapGit version="v1.0.0">' ).
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = 'serializer=' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<NAME>LZTEST_FUGRTOP</NAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SUBC>I</SUBC>' ).
* an include has no original language and no text pool of its own
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<RLOAD>' ).
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<TPOOL>' ).
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<UNAM>' ).

  ENDMETHOD.


  METHOD serializes_main_program.

    DATA ls_include TYPE zcl_abapgit_historical_fugr=>ty_include.

    ls_include-name = 'SAPLZTEST_FUGR'.
    ls_include-program-trdir = VALUE #(
      name  = 'SAPLZTEST_FUGR'
      subc  = 'F'
      rload = 'E' ).
    INSERT VALUE #( id = 'I' key = '002' entry = 'Second' length = 6 ) INTO TABLE ls_include-tpool.
    INSERT VALUE #( id = 'I' key = '001' entry = 'First' length = 5 ) INTO TABLE ls_include-tpool.

    DATA(lv_xml) = mo_cut->serialize_include_xml( ls_include ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SUBC>F</SUBC>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<RLOAD>E</RLOAD>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<TPOOL>' ).
    cl_abap_unit_assert=>assert_true( xsdbool(
      find( val = lv_xml
            sub = '<KEY>001</KEY>' ) < find( val = lv_xml
                                             sub = '<KEY>002</KEY>' ) ) ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'ztest_fugr.fugr.' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-prefix ).

  ENDMETHOD.
ENDCLASS.
