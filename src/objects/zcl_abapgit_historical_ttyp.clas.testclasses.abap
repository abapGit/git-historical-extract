CLASS ltcl_ttyp DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_ttyp DEFINITION LOCAL FRIENDS ltcl_ttyp.

CLASS ltcl_ttyp DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_ttyp.

    METHODS setup.
    METHODS clears_volatile_fields FOR TESTING RAISING zcx_abapgit_exception.
    METHODS keeps_typelen_without_rowkind FOR TESTING RAISING zcx_abapgit_exception.
    METHODS sorts_key_fields FOR TESTING RAISING zcx_abapgit_exception.
    METHODS requires_original_language FOR TESTING.
    METHODS serializes_default_key FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_secondary_key FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.

    METHODS assert_contains
      IMPORTING
        iv_xml TYPE string
        iv_sub TYPE string.
ENDCLASS.


CLASS ltcl_ttyp IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'TTYD'
      obj_name = 'ZTEST_TTYP' ) ).

  ENDMETHOD.


  METHOD assert_contains.

    cl_abap_unit_assert=>assert_differs(
      act = find( val = iv_xml
                  sub = iv_sub )
      exp = -1
      msg = |{ iv_sub } is missing| ).

  ENDMETHOD.


  METHOD clears_volatile_fields.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

    ls_ttyp-dd40v = VALUE #(
      typename   = 'ZTEST_TTYP'
      ddlanguage = 'E'
      rowtype    = 'ZTEST_STRUCTURE'
      rowkind    = 'S'
      typelen    = 120
      as4user    = 'DEVELOPER'
      as4date    = '20100101'
      as4time    = '120000' ).

    ls_ttyp = mo_cut->normalize( ls_ttyp ).

    cl_abap_unit_assert=>assert_initial( ls_ttyp-dd40v-as4user ).
    cl_abap_unit_assert=>assert_initial( ls_ttyp-dd40v-as4date ).
    cl_abap_unit_assert=>assert_initial( ls_ttyp-dd40v-as4time ).
    cl_abap_unit_assert=>assert_initial( ls_ttyp-dd40v-typelen ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd40v-rowtype
      exp = 'ZTEST_STRUCTURE' ).

  ENDMETHOD.


  METHOD keeps_typelen_without_rowkind.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

* a line type given by a built-in type has no row kind, its length stays
    ls_ttyp-dd40v = VALUE #(
      typename   = 'ZTEST_TTYP'
      ddlanguage = 'E'
      datatype   = 'CHAR'
      leng       = 10
      typelen    = 20 ).

    ls_ttyp = mo_cut->normalize( ls_ttyp ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd40v-typelen
      exp = 20 ).

  ENDMETHOD.


  METHOD sorts_key_fields.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

    ls_ttyp-dd40v = VALUE #(
      typename   = 'ZTEST_TTYP'
      ddlanguage = 'E' ).
    ls_ttyp-dd42v = VALUE #(
      ( typename = 'ZTEST_TTYP' seckeyname = 'BY_NAME' keyfdpos = 1 keyfield = 'NAME' )
      ( typename = 'ZTEST_TTYP' keyfdpos = 2 keyfield = 'ITEM' )
      ( typename = 'ZTEST_TTYP' keyfdpos = 1 keyfield = 'ID' ) ).
    ls_ttyp-dd43v = VALUE #(
      ( typename = 'ZTEST_TTYP' seckeyname = 'BY_NAME' )
      ( typename = 'ZTEST_TTYP' seckeyname = 'BY_DATE' ) ).

    ls_ttyp = mo_cut->normalize( ls_ttyp ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd42v[ 1 ]-keyfield
      exp = 'ID' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd42v[ 2 ]-keyfield
      exp = 'ITEM' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd42v[ 3 ]-keyfield
      exp = 'NAME' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_ttyp-dd43v[ 1 ]-seckeyname
      exp = 'BY_DATE' ).

  ENDMETHOD.


  METHOD requires_original_language.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

    ls_ttyp-dd40v-typename = 'ZTEST_TTYP'.

    TRY.
        mo_cut->normalize( ls_ttyp ).
        cl_abap_unit_assert=>fail( 'a table type without original language must be rejected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD serializes_default_key.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

    ls_ttyp-dd40v = VALUE #(
      typename   = 'ZTEST_TTYP'
      ddlanguage = 'E'
      rowtype    = 'ZTEST_STRUCTURE'
      rowkind    = 'S'
      datatype   = 'STRU'
      accessmode = 'T'
      keydef     = 'D'
      keykind    = 'N'
      ddtext     = 'Test table type' ).

    DATA(lv_xml) = mo_cut->serialize_xml( mo_cut->normalize( ls_ttyp ) ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<?xml version="1.0" encoding="utf-8"?>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = 'serializer="LCL_OBJECT_TTYP" serializer_version="v1.0.0"' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<TYPENAME>ZTEST_TTYP</TYPENAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<KEYDEF>D</KEYDEF>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<DDTEXT>Test table type</DDTEXT>' ).

* empty key tables are left out as abapGit does
    cl_abap_unit_assert=>assert_equals(
      act = find( val = lv_xml
                  sub = '<DD42V>' )
      exp = -1 ).
    cl_abap_unit_assert=>assert_equals(
      act = find( val = lv_xml
                  sub = '<DD43V>' )
      exp = -1 ).

  ENDMETHOD.


  METHOD serializes_secondary_key.

    DATA ls_ttyp TYPE zcl_abapgit_historical_ttyp=>ty_ttyp.

    ls_ttyp-dd40v = VALUE #(
      typename   = 'ZTEST_TTYP'
      ddlanguage = 'E'
      rowtype    = 'ZTEST_STRUCTURE'
      rowkind    = 'S'
      datatype   = 'STRU'
      accessmode = 'S'
      keydef     = 'K'
      keykind    = 'U'
      keyfdcount = 1 ).
    ls_ttyp-dd42v = VALUE #(
      ( typename = 'ZTEST_TTYP' keyfdpos = 1 rowtypepos = 1 keyfield = 'ID' )
      ( typename = 'ZTEST_TTYP' seckeyname = 'BY_NAME' keyfdpos = 1 rowtypepos = 2 keyfield = 'NAME' ) ).
    ls_ttyp-dd43v = VALUE #(
      ( typename = 'ZTEST_TTYP' seckeyname = 'BY_NAME' ddlanguage = 'E' accessmode = 'S' kind = 'K' ) ).

    DATA(lv_xml) = mo_cut->serialize_xml( mo_cut->normalize( ls_ttyp ) ).

    assert_contains( iv_xml = lv_xml
                     iv_sub = '<KEYFIELD>ID</KEYFIELD>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<SECKEYNAME>BY_NAME</SECKEYNAME>' ).
    assert_contains( iv_xml = lv_xml
                     iv_sub = '<KIND>K</KIND>' ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'ztest_ttyp.ttyp.xml' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).

  ENDMETHOD.
ENDCLASS.
