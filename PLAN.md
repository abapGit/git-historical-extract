# Historical object support plan

Each object handler should implement `zif_abapgit_historical_object`, select the version that belongs to the processed transport, emit files compatible with the abapGit serializer used by this project, and describe every file that must be removed when the R3TR object is deleted. Existing `INTF`, `CLAS`, and `PROG` handlers are treated as baselines to harden rather than finished implementations.

Translations are out of scope for all object types. Extract language-dependent texts only in the object's original language. All file sets, deletion requirements, and import/serialize-back checks below apply to this original-language-only scope.

## DTEL

- **Required output:** AFF JSON (`<object-name>.dtel.json`). Classic abapGit DTEL XML is out of scope.
- [x] Confirm the VRSD component type and released-system function module used to read a historical data element version (including the `DTEL` to version-object translation).
- [x] Add `zcl_abapgit_historical_dtel` and its abapGit class XML, following the constructor, `determine_parts`, `build_files`, and `build_deleted_files` pattern used by `DOMD`.
- [x] Read the historical definition and original-language texts into a stable internal model, including domain references, built-in types, search-help/parameter references, headings, and field labels.
- [x] Map the historical model to the DTEL AFF schema supported by the project's abapGit dependency, including the AFF header, format version, original language, description, type information, references, and field labels.
- [x] Serialize the AFF model as deterministic UTF-8 JSON in `<object-name>.dtel.json`, matching the schema's defaults, enum mappings, property names, and omission rules so that a re-serialization produces no diff.
- [x] Return no file when the requested historical version does not exist, and raise a contextual exception for other version-reader failures.
- [x] Add `DTEL` routing to `zcl_abapgit_historical_objects`, including the R3TR/version-object name translation in both normal and deleted-object paths.
- [x] Emit `<object-name>.dtel.json` as the DTEL deletion file set and ensure no `.dtel.xml` file is generated.
- [ ] Test domain-based and built-in data elements, original-language labels, optional references, missing versions, and deletion.
- [ ] Run abaplint and verify an extracted AFF DTEL can be imported by abapGit and serialized back without a content diff.

## TABL

- **Required output:** the AFF file set `<object-name>.tabl.json` and `<object-name>.tabl.ddic`, where the DDL source is produced by abapGit's `zcl_abapgit_object_tabl_ddl->serialize( )`, merged to abapGit `main` as #7864. Classic abapGit TABL XML is out of scope. Both `zcl_abapgit_object_tabl_ddl` and `zif_abapgit_aff_tabl_v1` are already reachable through the existing abapGit dependency, so nothing new has to be pulled in.
- [x] Confirm the VRSD component type and the released-system version reader, including the `TABL` to version-object translation: the component type is `TABD` and the reader is `SVRS_GET_VERSION_TABD_40`, which returns `DD02V`, `DD03V`, `DD05V`, `DD08V`, `DD35V`, and `DD36V` plus the text tables `DD02TV`, `DD03TV`, and `DD08TV`.
- [x] Confirm the version reader returns the whole table in a single record, so no per-component merge across VRSD rows is needed as for `CLAS` and `PROG`.
- [x] Add `zcl_abapgit_historical_tabl` and its abapGit class XML, following the constructor, `determine_parts`, `build_files`, and `build_deleted_files` pattern used by `DOMD` and `DTEL`.
- [x] Read the historical version into `zif_abapgit_object_tabl=>ty_internal`. The reader hands back the dictionary view structures while the DDL serializer works on the prepared ones, so `DD03V`, `DD05V`, and `DD36V` are moved component-wise into `DD03P`, `DD05M`, and `DD36M`; `DD02V`, `DD08V`, and `DD35V` are taken over as they are.
- [ ] Confirm on a system that `DD03V`, `DD05V`, and `DD36V` name every component the serializer reads the same way their prepared counterparts do, because a renamed component would silently drop a field attribute, a foreign key condition, or a value help parameter rather than fail.
- [x] Merge the original-language texts returned by the version reader into the structures the serializer reads them from: `DD02TV` into `DD02V-DDTEXT` for the table description, `DD03TV` into `DD03P-DDTEXT` for the field labels, and `DD08TV` into `DD08V-DDTEXT` for the foreign key labels.
- [x] Call `zcl_abapgit_object_tabl_ddl->serialize( )` instead of reimplementing DDL syntax: abapGit owns the annotation set, field order, colon alignment, type mapping, and the serialize/deserialize round-trip contract, and formatting defects are fixed there rather than forked here.
- [x] Preserve the field, foreign-key, and value-help order delivered by the version reader, because the serializer emits in table order and any resorting changes the output.
- [x] Map the historical model to `zif_abapgit_aff_tabl_v1=>ty_main` for `<object-name>.tabl.json` — format version, original-language description, original language, ABAP language version — and serialize it through `zcl_abapgit_json_handler` exactly as `DOMD` and `DTEL` already do.
- [x] Reject up front, naming object and version in the message, each input that `zcl_abapgit_object_tabl_ddl` refuses: an `EXCLASS` outside `0` to `4`, an empty `CONTFLAG`, an unknown `AUTHCLASS`, and a `MAINFLAG` that is neither `X`, `N`, nor empty. A `TABCLASS` other than `TRANSP` is skipped rather than raised, so one unsupported table cannot fail the extraction of its whole transport.
- [x] Cover transparent tables including global temporary tables, `.INCLU` includes with name suffixes, and include extensions for foreign keys and value helps. Skip structures (`INTTAB`), append structures (`APPEND`), pooled and cluster tables, and IDoc segment tables, none of which the DDL format describes.
- [x] Document what is deliberately not extracted: the version reader returns neither technical settings (`DD09L`) nor indexes (`DD12V`/`DD17V`), and writing the settings would additionally need an AFF `TABT` type that the abapGit dependency does not provide, so no `<object-name>.tabl.settings.json` is produced; long texts and IDoc segment definitions are dropped as well.
- [x] Record as a known fidelity limit that currency and quantity fields whose reference points at another table resolve through `DDIF_FIELDINFO_GET` against the active dictionary, so the emitted type reflects today's DDIC rather than its state at that transport; raise it upstream in abapGit if it turns out to matter.
- [x] Return no file when the requested historical version does not exist, and raise a contextual exception for other version-reader failures.
- [x] Add `TABL` routing to `zcl_abapgit_historical_objects`, including the R3TR/version-object name translation in both the normal and the deleted-object paths.
- [x] Emit `<object-name>.tabl.json` and `<object-name>.tabl.ddic` as the `TABL` deletion file set, and ensure no `.tabl.xml` file is generated.
- [x] Test fields typed by `DTEL` and by built-in types, key and `not null` flags, foreign keys with cardinalities, value helps, rejected table classes and attribute values, and deletion.
- [ ] Extend the tests to the cases that need either a system or a stubbed version reader: includes with and without suffix, `remove foreign key`, currency and quantity reference fields, and a missing version.
- [x] Run abaplint, and check round-trip stability by feeding an extracted `.tabl.ddic` back through `zcl_abapgit_object_tabl_ddl->deserialize( )` and serializing again to identical text, driving both from the transpiled abapGit build.
- [ ] Revisit end-to-end abapGit import and serialize-back equivalence once `zcl_abapgit_object_tabl` itself reads and writes the DDL file set; today it still serializes `TABL` as XML and never calls `zcl_abapgit_object_tabl_ddl`, so an extracted DDL table cannot round-trip through abapGit yet.

## TTYP

- [ ] Confirm the VRSD component type and historical version reader for R3TR `TTYP` objects.
- [ ] Add `zcl_abapgit_historical_ttyp` and its abapGit class XML, then register `TTYP` in the object factory.
- [ ] Read the historical table-type header, line-type definition, access/table kind, primary key, secondary keys, and original-language description into an internal model.
- [ ] Support elementary, structured, reference, and DDIC-object line types where the installed abapGit serializer supports them; fail clearly for unsupported variants.
- [ ] Map and normalize the historical structures to the current abapGit TTYP representation, preserving explicit versus default key semantics.
- [ ] Produce deterministic canonical output and skip the file when no historical version is available.
- [ ] Emit the complete TTYP deletion file set.
- [ ] Test standard/sorted/hashed types, unique and non-unique keys, default and explicit keys, reference line types, missing versions, and deletion.
- [ ] Run abaplint and verify abapGit import plus serialize-back equivalence, after the referenced DOMA/DTEL/TABL objects are available.

## INTF

- [x] Baseline handler, factory routing, source extraction, and main-file deletion exist.
- [ ] Verify which VRSD records represent the interface source and metadata on every supported SAP release, and select the correct version deterministically if more than one record matches a transport.
- [ ] Do not emit an empty `.intf.abap` file when the historical source cannot be read; distinguish a missing version from a reader error.
- [ ] Compare the generated source and companion metadata with the current abapGit INTF serializer, adding the canonical metadata file if source-only output is not round-trip safe.
- [ ] Preserve interface annotations, aliases, events, types, constants, method signatures, and ABAP Doc contained in the historical source.
- [ ] Normalize line endings, trailing whitespace, and final newline consistently with abapGit.
- [ ] Expand deletion handling to every emitted INTF file.
- [ ] Test interfaces with inheritance, aliases, events, typed parameters, exceptions, ABAP Doc, empty optional sections, missing versions, and deletion.
- [ ] Run abaplint and verify abapGit import plus serialize-back equivalence.

## CLAS

- [x] Baseline handler and factory routing exist for `CPUB`, `CPRO`, `CPRI`, `METH`, and the four `CINC` names.
- [ ] Define the historical class snapshot algorithm: for each transport, combine changed components with the newest preceding versions of unchanged components instead of rebuilding the class from only that transport's VRSD rows.
- [ ] Restrict `METH` discovery to the exact class and determine historical method membership so removed or unrelated methods cannot leak into the output.
- [ ] Assemble public, protected, and private sections plus method implementations in deterministic order, with exactly one class implementation wrapper.
- [ ] Read and emit local definitions, local implementations, macros, and test classes from `CINC` as their canonical abapGit auxiliary files; omit only components that are genuinely empty at that point in history.
- [ ] Compare class metadata with the current abapGit CLAS serializer and add the canonical metadata file when required for descriptions, language version, final/abstract state, or other non-source attributes.
- [ ] Treat missing individual components as absent, but suppress the object or raise a contextual error when its essential definition cannot be reconstructed.
- [ ] Expand deletion handling to the main source, metadata, and all fixed auxiliary class filenames.
- [ ] Test multi-method classes, method additions/deletions, visibility changes, empty and populated local includes, test classes, inheritance/interfaces, missing versions, and deletion.
- [ ] Run abaplint and verify that several incremental class transports import and serialize back without losing unchanged methods or producing diffs.

## PROG

- [x] Baseline handler, factory routing, `REPS` source extraction, and main-file deletion exist.
- [ ] Inventory the VRSD/LIMU components that belong to a full R3TR `PROG` on supported releases, including program attributes, text elements, selection texts, screens, GUI status, documentation, and variants where abapGit treats them as part of the program.
- [ ] Reconstruct the program as of the transport by combining changed components with their latest preceding versions.
- [ ] Harden `REPS` reading so a missing source does not create an empty `.prog.abap`, while genuine reader failures include object/version context.
- [ ] Generate the canonical abapGit program metadata and original-language texts in addition to source, with volatile fields removed and entries sorted deterministically.
- [ ] Add canonical auxiliary files for each supported screen, GUI status, documentation, or variant component; explicitly document any components deferred from the first implementation.
- [ ] Extend deletion/staging so a deleted program removes dynamically named auxiliary files already present in the repository, not only `.prog.abap`.
- [ ] Test executable reports and include programs, original-language text elements, screen/status changes, component deletion, missing versions, and whole-object deletion.
- [ ] Run abaplint and verify a multi-transport PROG history through abapGit import and serialize-back comparison.

## FUGR

- [ ] Define how a historical R3TR `FUGR` maps to its main program, generated includes, customer includes, function modules, screens, GUI status, documentation, and function-group metadata in VRSD.
- [ ] Add `zcl_abapgit_historical_fugr` and its abapGit class XML, then register `FUGR` in the object factory.
- [ ] Reconstruct group membership at the point of each transport from version history; do not rely only on current `D010INC`/function-directory state, because members may have been renamed or deleted later.
- [ ] Read all historical source components and function-module metadata, preserving stable include/function ordering and excluding generated code that the current abapGit FUGR serializer excludes.
- [ ] Combine changed parts with the newest preceding versions of unchanged parts so a single-function transport cannot erase the rest of the group.
- [ ] Emit the canonical abapGit FUGR metadata, include, function-module, screen, GUI-status, and documentation files for the supported scope, with language-dependent texts in the original language only.
- [ ] Handle function-module and include additions, renames, and removals by marking obsolete member files for deletion in the same transport.
- [ ] Extend whole-object deletion to remove every file belonging to the function group, including dynamically named member files.
- [ ] Test a group with multiple function modules, TOP/UXX and custom includes, screens/statuses, documentation, member add/delete/rename history, missing versions, and full deletion.
- [ ] Run abaplint and verify the extracted group imports, activates, and serializes back through abapGit without missing members or spurious diffs.
