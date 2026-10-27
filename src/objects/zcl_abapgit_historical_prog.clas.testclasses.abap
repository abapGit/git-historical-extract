CLASS ltcl_prog DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_prog DEFINITION LOCAL FRIENDS ltcl_prog.

CLASS ltcl_prog DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_prog.

    METHODS setup.
    METHODS clears_volatile_fields FOR TESTING.
    METHODS defaults_program_name FOR TESTING.
    METHODS sorts_text_pool FOR TESTING.
    METHODS drops_empty_title FOR TESTING.
    METHODS defaults_original_language FOR TESTING.
    METHODS serializes_attributes FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_selection_texts FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_include FOR TESTING RAISING zcx_abapgit_exception.
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


CLASS ltcl_prog IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'PROG'
      obj_name = 'ZTEST_PROG' ) ).

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


  METHOD clears_volatile_fields.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name    = 'ZTEST_PROG'
      subc    = '1'
      rload   = 'E'
      fixpt   = 'X'
      uccheck = 'X'
      cnam    = 'DEVELOPER'
      cdat    = '20100101'
      unam    = 'DEVELOPER'
      udat    = '20100102'
      vern    = '000010'
      sdate   = '20100102'
      stime   = '120000'
      idate   = '20100102'
      itime   = '120000' ).

    ls_prog = mo_cut->normalize( ls_prog ).

    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-cnam ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-cdat ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-unam ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-udat ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-vern ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-sdate ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-stime ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-idate ).
    cl_abap_unit_assert=>assert_initial( ls_prog-progdir-itime ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-progdir-subc
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-progdir-uccheck
      exp = 'X' ).

  ENDMETHOD.


  METHOD defaults_program_name.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir-rload = 'E'.

    ls_prog = mo_cut->normalize( ls_prog ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-progdir-name
      exp = 'ZTEST_PROG' ).

  ENDMETHOD.


  METHOD sorts_text_pool.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name  = 'ZTEST_PROG'
      rload = 'E' ).
    INSERT VALUE #( id = 'S' key = 'P_NAME' entry = '        Name' length = 12 ) INTO TABLE ls_prog-tpool.
    INSERT VALUE #( id = 'R' entry = 'Test program' length = 12 ) INTO TABLE ls_prog-tpool.
    INSERT VALUE #( id = 'I' key = '002' entry = 'Second' length = 6 ) INTO TABLE ls_prog-tpool.
    INSERT VALUE #( id = 'I' key = '001' entry = 'First' length = 5 ) INTO TABLE ls_prog-tpool.

    ls_prog = mo_cut->normalize( ls_prog ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-tpool[ 1 ]-key
      exp = '001' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-tpool[ 2 ]-key
      exp = '002' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-tpool[ 3 ]-id
      exp = 'R' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-tpool[ 4 ]-key
      exp = 'P_NAME' ).

  ENDMETHOD.


  METHOD drops_empty_title.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name  = 'ZTEST_PROG'
      rload = 'E' ).
    INSERT VALUE #( id = 'R' ) INTO TABLE ls_prog-tpool.
    INSERT VALUE #( id = 'I' key = '001' entry = 'First' length = 5 ) INTO TABLE ls_prog-tpool.

    ls_prog = mo_cut->normalize( ls_prog ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_prog-tpool )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-tpool[ 1 ]-id
      exp = 'I' ).

  ENDMETHOD.


  METHOD defaults_original_language.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir-name = 'ZTEST_PROG'.

    ls_prog = mo_cut->normalize( ls_prog ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_prog-progdir-rload
      exp = 'E' ).

  ENDMETHOD.


  METHOD serializes_attributes.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name    = 'ZTEST_PROG'
      subc    = '1'
      rload   = 'E'
      fixpt   = 'X'
      uccheck = 'X'
      unam    = 'DEVELOPER' ).
    INSERT VALUE #( id = 'R' entry = 'Test program' length = 12 ) INTO TABLE ls_prog-tpool.

    DATA(lv_xml) = mo_cut->serialize_xml( mo_cut->normalize( ls_prog ) ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<?xml version="1.0" encoding="utf-8"?>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = 'serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0"' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<NAME>ZTEST_PROG</NAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SUBC>1</SUBC>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<RLOAD>E</RLOAD>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<UCCHECK>X</UCCHECK>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<ENTRY>Test program</ENTRY>' ).
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<UNAM>' ).

  ENDMETHOD.


  METHOD serializes_selection_texts.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name  = 'ZTEST_PROG'
      subc  = '1'
      rload = 'E' ).
* a plain selection text and one taken from the dictionary, as READ TEXTPOOL returns them
    INSERT VALUE #( id = 'S' key = 'P_BRANCH' entry = '        Branch' length = 14 ) INTO TABLE ls_prog-tpool.
    INSERT VALUE #( id = 'S' key = 'S_TRKORR' entry = 'D       .' length = 9 ) INTO TABLE ls_prog-tpool.

    DATA(lv_xml) = mo_cut->serialize_xml( mo_cut->normalize( ls_prog ) ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<KEY>P_BRANCH</KEY>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<ENTRY>Branch</ENTRY>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<ENTRY>.</ENTRY>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SPLIT>D</SPLIT>' ).

  ENDMETHOD.


  METHOD serializes_include.

    DATA ls_prog TYPE zcl_abapgit_historical_prog=>ty_prog.

    ls_prog-progdir = VALUE #(
      name  = 'ZTEST_PROG'
      subc  = 'I'
      rload = 'E' ).

    DATA(lv_xml) = mo_cut->serialize_xml( mo_cut->normalize( ls_prog ) ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SUBC>I</SUBC>' ).
* an empty text pool is left out as abapGit does
    assert_not_contains( iv_xml = lv_xml
                         iv_sub = '<TPOOL>' ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'ztest_prog.prog.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 2 ]-filename
      exp = 'ztest_prog.prog.xml' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).
    cl_abap_unit_assert=>assert_true( lt_files[ 2 ]-deleted ).

  ENDMETHOD.
ENDCLASS.
