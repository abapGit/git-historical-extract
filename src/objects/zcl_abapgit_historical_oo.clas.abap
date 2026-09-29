CLASS zcl_abapgit_historical_oo DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ty_vseoattrib_tt TYPE STANDARD TABLE OF vseoattrib WITH DEFAULT KEY .
    TYPES ty_vseomethod_tt TYPE STANDARD TABLE OF vseomethod WITH DEFAULT KEY .
    TYPES ty_vseoevent_tt TYPE STANDARD TABLE OF vseoevent WITH DEFAULT KEY .
    TYPES ty_vseoparam_tt TYPE STANDARD TABLE OF vseoparam WITH DEFAULT KEY .
    TYPES ty_vseoexcep_tt TYPE STANDARD TABLE OF vseoexcep WITH DEFAULT KEY .
* the component definitions as the version readers return them
    TYPES:
      BEGIN OF ty_components,
        attributes TYPE ty_vseoattrib_tt,
        methods    TYPE ty_vseomethod_tt,
        events     TYPE ty_vseoevent_tt,
        parameters TYPE ty_vseoparam_tt,
        exceptions TYPE ty_vseoexcep_tt,
      END OF ty_components .

    CLASS-METHODS map_descriptions
      IMPORTING
        is_components          TYPE ty_components
        iv_language            TYPE sy-langu
      RETURNING
        VALUE(rs_descriptions) TYPE zif_abapgit_aff_oo_types_v1=>ty_descriptions .

    CLASS-METHODS map_abap_language_version
      IMPORTING
        iv_unicode        TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version_src
      RETURNING
        VALUE(rv_version) TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version_src .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_HISTORICAL_OO IMPLEMENTATION.


  METHOD map_abap_language_version.

    CASE iv_unicode.
      WHEN zif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user
          OR zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development.
        rv_version = iv_unicode.
      WHEN OTHERS.
* space is a non-Unicode class or interface on old releases, which is standard ABAP as well
        rv_version = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
    ENDCASE.

  ENDMETHOD.


  METHOD map_descriptions.

* follows abapGit's INTF serializer: components without any text are left out. The version
* readers can return a row per language, so texts are read in the original language only
    DATA ls_method LIKE LINE OF rs_descriptions-methods.
    DATA ls_event  LIKE LINE OF rs_descriptions-events.

    LOOP AT is_components-attributes INTO DATA(ls_attribute)
        WHERE langu = iv_language AND descript IS NOT INITIAL.
      INSERT VALUE #(
        name        = ls_attribute-cmpname
        description = ls_attribute-descript ) INTO TABLE rs_descriptions-attributes.
    ENDLOOP.

    LOOP AT is_components-methods INTO DATA(ls_vseomethod).
      CLEAR ls_method.
      ls_method-name = ls_vseomethod-cmpname.
      READ TABLE is_components-methods INTO DATA(ls_method_text)
        WITH KEY cmpname = ls_vseomethod-cmpname langu = iv_language.
      IF sy-subrc = 0.
        ls_method-description = ls_method_text-descript.
      ENDIF.
      LOOP AT is_components-parameters INTO DATA(ls_parameter)
          WHERE cmpname = ls_vseomethod-cmpname AND langu = iv_language AND descript IS NOT INITIAL.
        INSERT VALUE #(
          name        = ls_parameter-sconame
          description = ls_parameter-descript ) INTO TABLE ls_method-parameters.
      ENDLOOP.
      LOOP AT is_components-exceptions INTO DATA(ls_exception)
          WHERE cmpname = ls_vseomethod-cmpname AND langu = iv_language AND descript IS NOT INITIAL.
        INSERT VALUE #(
          name        = ls_exception-sconame
          description = ls_exception-descript ) INTO TABLE ls_method-exceptions.
      ENDLOOP.
      IF ls_method-description IS NOT INITIAL
          OR ls_method-parameters IS NOT INITIAL
          OR ls_method-exceptions IS NOT INITIAL.
* a method with rows in several languages yields the same entry again, which the unique key drops
        INSERT ls_method INTO TABLE rs_descriptions-methods.
      ENDIF.
    ENDLOOP.

    LOOP AT is_components-events INTO DATA(ls_vseoevent).
      CLEAR ls_event.
      ls_event-name = ls_vseoevent-cmpname.
      READ TABLE is_components-events INTO DATA(ls_event_text)
        WITH KEY cmpname = ls_vseoevent-cmpname langu = iv_language.
      IF sy-subrc = 0.
        ls_event-description = ls_event_text-descript.
      ENDIF.
      LOOP AT is_components-parameters INTO ls_parameter
          WHERE cmpname = ls_vseoevent-cmpname AND langu = iv_language AND descript IS NOT INITIAL.
        INSERT VALUE #(
          name        = ls_parameter-sconame
          description = ls_parameter-descript ) INTO TABLE ls_event-parameters.
      ENDLOOP.
      IF ls_event-description IS NOT INITIAL OR ls_event-parameters IS NOT INITIAL.
        INSERT ls_event INTO TABLE rs_descriptions-events.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
