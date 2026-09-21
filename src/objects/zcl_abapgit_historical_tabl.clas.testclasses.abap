CLASS ltcl_tabl DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_tabl DEFINITION LOCAL FRIENDS ltcl_tabl.

CLASS ltcl_tabl DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_tabl.

    METHODS setup.
    METHODS transparent_table
      RETURNING
        VALUE(rs_internal) TYPE zif_abapgit_object_tabl=>ty_internal.
    METHODS table_with_references
      RETURNING
        VALUE(rs_internal) TYPE zif_abapgit_object_tabl=>ty_internal.

    METHODS maps_header FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_header FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_ddic FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_references FOR TESTING RAISING zcx_abapgit_exception.
    METHODS accepts_transparent FOR TESTING RAISING zcx_abapgit_exception.
    METHODS skips_structure FOR TESTING RAISING zcx_abapgit_exception.
    METHODS skips_append FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_delivery_class FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_enhancement_category FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_activation_type FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_data_maintenance FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_tabl IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'TABD'
      obj_name = 'ZTEST_TABL' ) ).

  ENDMETHOD.


  METHOD transparent_table.

* one field with a built-in type and a label, one field typed by a data element
    rs_internal-dd02v-tabname    = 'ZTEST_TABL'.
    rs_internal-dd02v-ddlanguage = 'E'.
    rs_internal-dd02v-masterlang = 'E'.
    rs_internal-dd02v-tabclass   = 'TRANSP'.
    rs_internal-dd02v-ddtext     = 'Test table'.
    rs_internal-dd02v-contflag   = 'A'.
    rs_internal-dd02v-exclass    = '1'.

    APPEND VALUE #(
      fieldname  = 'KEY_FIELD'
      keyflag    = abap_true
      adminfield = '0'
      notnull    = abap_true
      datatype   = 'CHAR'
      leng       = 10
      ddtext     = 'Key field' ) TO rs_internal-dd03p.
    APPEND VALUE #(
      fieldname  = 'VALUE'
      adminfield = '0'
      rollname   = 'ZTEST_DTEL'
      datatype   = 'CHAR'
      leng       = 10 ) TO rs_internal-dd03p.

  ENDMETHOD.


  METHOD table_with_references.

* the foreign key and the value help live in their own structures, this guards
* that the whole internal structure reaches the abapGit serializer
    rs_internal = transparent_table( ).

    APPEND VALUE #(
      fieldname  = 'VALUE'
      checktable = 'ZCHECK'
      cardleft   = '1'
      card       = 'N' ) TO rs_internal-dd08v.
    APPEND VALUE #(
      fieldname  = 'VALUE'
      checktable = 'ZCHECK'
      checkfield = 'CHECK_KEY'
      fortable   = 'ZTEST_TABL'
      forkey     = 'VALUE'
      primpos    = 1 ) TO rs_internal-dd05m.
    APPEND VALUE #(
      fieldname = 'VALUE'
      shlpname  = 'ZSHLP' ) TO rs_internal-dd35v.
    APPEND VALUE #(
      fieldname  = 'VALUE'
      shlpname   = 'ZSHLP'
      shlpfield  = 'SHLP_FIELD'
      shtable    = 'ZTEST_TABL'
      shfield    = 'VALUE'
      flposition = 1 ) TO rs_internal-dd36m.

  ENDMETHOD.


  METHOD maps_header.

    DATA(ls_aff) = mo_cut->map_to_aff( transparent_table( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-format_version
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-description
      exp = 'Test table' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-original_language
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-abap_language_version
      exp = zif_abapgit_aff_types_v1=>co_abap_language_version-standard ).

  ENDMETHOD.


  METHOD serializes_header.

    DATA(lv_json) = mo_cut->serialize_aff( transparent_table( ) ).

    cl_abap_unit_assert=>assert_differs(
      act = find( val = lv_json
                  sub = '"formatVersion": "1"' )
      exp = -1
      msg = 'the format version is missing' ).
    cl_abap_unit_assert=>assert_differs(
      act = find( val = lv_json
                  sub = '"description": "Test table"' )
      exp = -1
      msg = 'the description is missing' ).
    cl_abap_unit_assert=>assert_differs(
      act = find( val = lv_json
                  sub = '"originalLanguage": "en"' )
      exp = -1
      msg = 'the original language is missing' ).

* the standard language version is the default and is not written
    cl_abap_unit_assert=>assert_equals(
      act = find( val = lv_json
                  sub = '"abapLanguageVersion"' )
      exp = -1
      msg = 'the standard ABAP language version must be omitted' ).

  ENDMETHOD.


  METHOD serializes_ddic.

    DATA lt_expected TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    APPEND `@EndUserText.label : 'Test table'` TO lt_expected.
    APPEND `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE` TO lt_expected.
    APPEND `@AbapCatalog.tableCategory : #TRANSPARENT` TO lt_expected.
    APPEND `@AbapCatalog.deliveryClass : #A` TO lt_expected.
    APPEND `@AbapCatalog.dataMaintenance : #RESTRICTED` TO lt_expected.
    APPEND `define table ztest_tabl {` TO lt_expected.
    APPEND `` TO lt_expected.
    APPEND `  @EndUserText.label : 'Key field'` TO lt_expected.
    APPEND `  key key_field : abap.char(10) not null;` TO lt_expected.
    APPEND `  value         : ztest_dtel;` TO lt_expected.
    APPEND `` TO lt_expected.
    APPEND `}` TO lt_expected.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->serialize_ddic( transparent_table( ) )
      exp = concat_lines_of( table = lt_expected
                             sep   = |\n| ) ).

  ENDMETHOD.


  METHOD serializes_references.

    DATA lt_expected TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    APPEND `@EndUserText.label : 'Test table'` TO lt_expected.
    APPEND `@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE` TO lt_expected.
    APPEND `@AbapCatalog.tableCategory : #TRANSPARENT` TO lt_expected.
    APPEND `@AbapCatalog.deliveryClass : #A` TO lt_expected.
    APPEND `@AbapCatalog.dataMaintenance : #RESTRICTED` TO lt_expected.
    APPEND `define table ztest_tabl {` TO lt_expected.
    APPEND `` TO lt_expected.
    APPEND `  @EndUserText.label : 'Key field'` TO lt_expected.
    APPEND `  key key_field : abap.char(10) not null;` TO lt_expected.
    APPEND `  @AbapCatalog.foreignKey.screenCheck : true` TO lt_expected.
    APPEND `  value         : ztest_dtel` TO lt_expected.
    APPEND `    with foreign key [1..*,1] zcheck` TO lt_expected.
    APPEND `      where check_key = ztest_tabl.value` TO lt_expected.
    APPEND `    with value help zshlp` TO lt_expected.
    APPEND `      where shlp_field = ztest_tabl.value;` TO lt_expected.
    APPEND `` TO lt_expected.
    APPEND `}` TO lt_expected.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->serialize_ddic( table_with_references( ) )
      exp = concat_lines_of( table = lt_expected
                             sep   = |\n| ) ).

  ENDMETHOD.


  METHOD accepts_transparent.

    cl_abap_unit_assert=>assert_true( mo_cut->is_supported(
      is_internal = transparent_table( )
      iv_versno   = 7 ) ).

  ENDMETHOD.


  METHOD skips_structure.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    ls_internal-dd02v-tabclass = 'INTTAB'.

    cl_abap_unit_assert=>assert_false( mo_cut->is_supported(
      is_internal = ls_internal
      iv_versno   = 7 ) ).

  ENDMETHOD.


  METHOD skips_append.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    ls_internal-dd02v-tabclass = 'APPEND'.

    cl_abap_unit_assert=>assert_false( mo_cut->is_supported(
      is_internal = ls_internal
      iv_versno   = 7 ) ).

  ENDMETHOD.


  METHOD rejects_delivery_class.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    CLEAR ls_internal-dd02v-contflag.

    TRY.
        mo_cut->is_supported( is_internal = ls_internal
                              iv_versno   = 7 ).
        cl_abap_unit_assert=>fail( 'the missing delivery class must be reported' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp( act = lx_error->get_text( )
                                             exp = '*Delivery class is missing in table ZTEST_TABL version 7*' ).
    ENDTRY.

  ENDMETHOD.


  METHOD rejects_enhancement_category.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    CLEAR ls_internal-dd02v-exclass.

    TRY.
        mo_cut->is_supported( is_internal = ls_internal
                              iv_versno   = 7 ).
        cl_abap_unit_assert=>fail( 'the missing enhancement category must be reported' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp( act = lx_error->get_text( )
                                             exp = '*Unsupported enhancement category*' ).
    ENDTRY.

  ENDMETHOD.


  METHOD rejects_activation_type.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    ls_internal-dd02v-authclass = '99'.

    TRY.
        mo_cut->is_supported( is_internal = ls_internal
                              iv_versno   = 7 ).
        cl_abap_unit_assert=>fail( 'the unknown activation type must be reported' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp( act = lx_error->get_text( )
                                             exp = '*Unsupported activation type 99*' ).
    ENDTRY.

  ENDMETHOD.


  METHOD rejects_data_maintenance.

    DATA ls_internal TYPE zif_abapgit_object_tabl=>ty_internal.

    ls_internal = transparent_table( ).
    ls_internal-dd02v-mainflag = 'Z'.

    TRY.
        mo_cut->is_supported( is_internal = ls_internal
                              iv_versno   = 7 ).
        cl_abap_unit_assert=>fail( 'the unknown data maintenance value must be reported' ).
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_char_cp( act = lx_error->get_text( )
                                             exp = '*Unsupported data maintenance Z*' ).
    ENDTRY.

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'ztest_tabl.tabl.json' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 2 ]-filename
      exp = 'ztest_tabl.tabl.ddic' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 2 ]-deleted ).

  ENDMETHOD.
ENDCLASS.
