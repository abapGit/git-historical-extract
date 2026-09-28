CLASS ltcl_intf DEFINITION DEFERRED.
CLASS zcl_abapgit_historical_intf DEFINITION LOCAL FRIENDS ltcl_intf.

CLASS ltcl_intf DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO zcl_abapgit_historical_intf.

    METHODS setup.
    METHODS maps_header FOR TESTING RAISING zcx_abapgit_exception.
    METHODS maps_language_version FOR TESTING RAISING zcx_abapgit_exception.
    METHODS maps_descriptions FOR TESTING RAISING zcx_abapgit_exception.
    METHODS skips_other_languages FOR TESTING RAISING zcx_abapgit_exception.
    METHODS rejects_unknown_category FOR TESTING.
    METHODS serializes_minimal FOR TESTING RAISING zcx_abapgit_exception.
    METHODS serializes_category FOR TESTING RAISING zcx_abapgit_exception.
    METHODS builds_deletion FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_intf IMPLEMENTATION.

  METHOD setup.

    mo_cut = NEW #( VALUE #(
      object   = 'INTF'
      obj_name = 'ZIF_TEST' ) ).

  ENDMETHOD.


  METHOD maps_header.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseointerf = VALUE #(
        clsname  = 'ZIF_TEST'
        langu    = 'E'
        descript = 'Test interface'
        category = '00'
        clsproxy = abap_true
        unicode  = abap_true ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-format_version
      exp = '1' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-description
      exp = 'Test interface' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-original_language
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-category
      exp = zif_abapgit_aff_intf_v1=>co_category-general ).
    cl_abap_unit_assert=>assert_true( ls_aff-proxy ).

  ENDMETHOD.


  METHOD maps_language_version.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseointerf = VALUE #(
        langu    = 'E'
        category = '00'
        unicode  = space ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-abap_language_version
      exp = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard ).

    ls_aff = mo_cut->map_to_aff( VALUE #( vseointerf = VALUE #( langu = 'E' category = '00' unicode = '5' ) ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-header-abap_language_version
      exp = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development ).

  ENDMETHOD.


  METHOD maps_descriptions.

    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseointerf = VALUE #(
        langu    = 'E'
        category = '00' )
      attributes = VALUE #(
        ( cmpname = 'GV_B' langu = 'E' descript = 'Attribute B' )
        ( cmpname = 'GV_A' langu = 'E' descript = 'Attribute A' )
        ( cmpname = 'GV_NO_TEXT' langu = 'E' ) )
      methods    = VALUE #(
        ( cmpname = 'RUN' langu = 'E' descript = 'Run it' )
        ( cmpname = 'ONLY_PARAMETERS' langu = 'E' )
        ( cmpname = 'NO_TEXT' langu = 'E' ) )
      events     = VALUE #(
        ( cmpname = 'CHANGED' langu = 'E' descript = 'Changed' ) )
      parameters = VALUE #(
        ( cmpname = 'RUN' sconame = 'IV_INPUT' langu = 'E' descript = 'Input' )
        ( cmpname = 'RUN' sconame = 'IV_NO_TEXT' langu = 'E' )
        ( cmpname = 'ONLY_PARAMETERS' sconame = 'IV_VALUE' langu = 'E' descript = 'Value' )
        ( cmpname = 'CHANGED' sconame = 'IV_NEW' langu = 'E' descript = 'New value' ) )
      exceptions = VALUE #(
        ( cmpname = 'RUN' sconame = 'ZCX_ERROR' langu = 'E' descript = 'Failure' ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-attributes
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_component_descriptions(
        ( name = 'GV_A' description = 'Attribute A' )
        ( name = 'GV_B' description = 'Attribute B' ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-methods
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_methods(
        ( name       = 'ONLY_PARAMETERS'
          parameters = VALUE #( ( name = 'IV_VALUE' description = 'Value' ) ) )
        ( name        = 'RUN'
          description = 'Run it'
          parameters  = VALUE #( ( name = 'IV_INPUT' description = 'Input' ) )
          exceptions  = VALUE #( ( name = 'ZCX_ERROR' description = 'Failure' ) ) ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-events
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_events(
        ( name        = 'CHANGED'
          description = 'Changed'
          parameters  = VALUE #( ( name = 'IV_NEW' description = 'New value' ) ) ) ) ).

  ENDMETHOD.


  METHOD skips_other_languages.

* texts in a language other than the original are translations, which are out of scope
    DATA(ls_aff) = mo_cut->map_to_aff( VALUE #(
      vseointerf = VALUE #(
        langu    = 'E'
        category = '00' )
      attributes = VALUE #(
        ( cmpname = 'GV_A' langu = 'D' descript = 'Attribut A' ) )
      methods    = VALUE #(
        ( cmpname = 'RUN' langu = 'D' descript = 'Ausfuehren' )
        ( cmpname = 'RUN' langu = 'E' descript = 'Run it' ) ) ) ).

    cl_abap_unit_assert=>assert_initial( ls_aff-descriptions-attributes ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-descriptions-methods
      exp = VALUE zif_abapgit_aff_oo_types_v1=>ty_methods(
        ( name = 'RUN' description = 'Run it' ) ) ).

  ENDMETHOD.


  METHOD rejects_unknown_category.

    TRY.
        mo_cut->map_to_aff( VALUE #(
          vseointerf = VALUE #(
            langu    = 'E'
            category = zif_abapgit_aff_intf_v1=>co_category-business_instance_components ) ) ).
        cl_abap_unit_assert=>fail( 'a category without AFF value must be rejected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD serializes_minimal.

    DATA(lv_json) = mo_cut->serialize_aff( VALUE #(
      vseointerf = VALUE #(
        langu    = 'E'
        descript = 'Test interface'
        category = '00'
        unicode  = abap_true ) ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_json
      exp = |\{\n| &&
            |  "formatVersion": "1",\n| &&
            |  "header": \{\n| &&
            |    "description": "Test interface",\n| &&
            |    "originalLanguage": "en"\n| &&
            |  \}\n| &&
            |\}\n| ).

  ENDMETHOD.


  METHOD serializes_category.

    DATA(lv_json) = mo_cut->serialize_aff( VALUE #(
      vseointerf = VALUE #(
        langu    = 'E'
        descript = 'Test interface'
        category = '01'
        clsproxy = abap_true ) ) ).

    cl_abap_unit_assert=>assert_differs(
      act = find( val = lv_json
                  sub = '"category": "classicBadi"' )
      exp = -1
      msg = 'the category is missing' ).
    cl_abap_unit_assert=>assert_differs(
      act = find( val = lv_json
                  sub = '"proxy": true' )
      exp = -1
      msg = 'the proxy flag is missing' ).

  ENDMETHOD.


  METHOD builds_deletion.

    DATA(lt_files) = mo_cut->zif_abapgit_historical_object~build_deleted_files( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 1 ]-filename
      exp = 'zif_test.intf.abap' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_files[ 2 ]-filename
      exp = 'zif_test.intf.json' ).
    cl_abap_unit_assert=>assert_true( lt_files[ 1 ]-deleted ).
    cl_abap_unit_assert=>assert_true( lt_files[ 2 ]-deleted ).

  ENDMETHOD.
ENDCLASS.
