CLASS ltcl_clas DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_clas DEFINITION LOCAL FRIENDS ltcl_clas.

CLASS ltcl_clas DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_clas.

    METHODS setup.
    METHODS maps_header FOR TESTING RAISING zcx_abapgit_exception.
    METHODS maps_descriptions FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_unknown_category FOR TESTING.
    METHODS serializes_minimal FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_exception_class FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_file_names FOR TESTING.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_clas IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'CLAS'
      obj_name = 'ZCL_TEST' ) ).

  ENDMETHOD.


  METHOD maps_header.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseoclass = VALUE #(
        clsname  = 'ZCL_TEST'
        langu    = 'E'
        descript = 'Test class'
        category = '40'
        fixpt    = abap_true
        msg_id   = 'ZTEST'
        unicode  = '5' ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-format_version
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-description
      exp = 'Test class' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-original_language
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-abap_language_version
      exp = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-category
      exp = zif_abapgit_aff_clas_v1=>co_category-exception_class ).
    cl_abap_unit_assert=>assert_true( ls_aff-fix_point_arithmetic ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-message_class
      exp = 'ZTEST' ).

  ENDMETHOD.


  METHOD maps_descriptions.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseoclass  = VALUE #(
        langu    = 'E'
        category = '00' )
      attributes = VALUE #(
        ( cmpname = 'MV_NAME' langu = 'E' descript = 'Name' ) )
      methods    = VALUE #(
        ( cmpname = 'RUN' langu = 'E' descript = 'Run it' ) )
      parameters = VALUE #(
        ( cmpname = 'RUN' sconame = 'IV_INPUT' langu = 'E' descript = 'Input' ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-attributes
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions(
        ( name = 'MV_NAME' description = 'Name' ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-methods
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_methods(
        ( name        = 'RUN'
          description = 'Run it'
          parameters  = VALUE #( ( name = 'IV_INPUT' description = 'Input' ) ) ) ) ).

  ENDMETHOD.


  METHOD rejects_unknown_category.

    TRY.
        mo_cut->map_to_aff( VALUE #( vseoclass = VALUE #( langu = 'E' category = '99' ) ) ).
        cl_abap_unit_assert=>fail( 'a category without AFF value must be rejected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD serializes_minimal.

    DATA(lv_json) = mo_cut->serialize_aff( VALUE #(
      vseoclass = VALUE #(
        langu    = 'E'
        descript = 'Test class'
        category = '00'
        unicode  = abap_true ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = |\{\n| &&
            |  "formatVersion": "1",\n| &&
            |  "header": \{\n| &&
            |    "description": "Test class",\n| &&
            |    "originalLanguage": "en"\n| &&
            |  \}\n| &&
            |\}\n| ).

  ENDMETHOD.


  METHOD serializes_exception_class.

    DATA(lv_json) = mo_cut->serialize_aff( VALUE #(
      vseoclass = VALUE #(
        langu    = 'E'
        descript = 'Test exception'
        category = '40'
        fixpt    = abap_true
        msg_id   = 'ZTEST'
        unicode  = abap_true ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = |\{\n| &&
            |  "formatVersion": "1",\n| &&
            |  "header": \{\n| &&
            |    "description": "Test exception",\n| &&
            |    "originalLanguage": "en"\n| &&
            |  \},\n| &&
            |  "category": "exceptionClass",\n| &&
            |  "fixPointArithmetic": true,\n| &&
            |  "messageClass": "ZTEST"\n| &&
            |\}\n| ).

  ENDMETHOD.


  METHOD builds_file_names.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( )
      exp = 'zcl_test.clas.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_ext = 'json' )
      exp = 'zcl_test.clas.json' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_file_name( iv_extra = 'testclasses' )
      exp = 'zcl_test.clas.testclasses.abap' ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 6 ).
    LOOP AT lt_files INTO DATA(ls_file).
      cl_abap_unit_assert=>assert_true( ls_file-deleted ).
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
      act = VALUE string_table( FOR ls_line IN lt_files ( ls_line-filename ) )
      exp = VALUE string_table(
        ( `zcl_test.clas.abap` )
        ( `zcl_test.clas.json` )
        ( `zcl_test.clas.definitions.abap` )
        ( `zcl_test.clas.implementations.abap` )
        ( `zcl_test.clas.macros.abap` )
        ( `zcl_test.clas.testclasses.abap` ) ) ).

  ENDMETHOD.
ENDCLASS.
