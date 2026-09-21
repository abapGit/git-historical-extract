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

- [ ] Confirm the VRSD component types and historical reader for `TABL` definitions on the minimum supported SAP release.
- [ ] Add `zcl_abapgit_historical_tabl` and its abapGit class XML, then register `TABL` in the object factory.
- [ ] Read the complete historical DDIC model needed by abapGit: table/structure header, fields and includes, technical settings, foreign keys, search-help assignments, indexes, and enhancement metadata supported by the target serializer.
- [ ] Reconstruct a transport-consistent snapshot when table subcomponents have independent version records; unchanged components must retain their latest version as of that transport.
- [ ] Normalize volatile activation, user, date/time, position, and generated fields in the same way as the current abapGit TABL serializer.
- [ ] Serialize deterministically to the canonical TABL file set, with stable ordering for fields, keys, indexes, and secondary metadata.
- [ ] Cover transparent tables, structures, include structures, append structures, and explicitly reject or document unsupported pooled/cluster/IDoc variants.
- [ ] Emit every canonical TABL filename on deletion and ensure a deleted table does not leave auxiliary files behind.
- [ ] Test fields based on DOMA/DTEL, built-in fields, includes/appends, foreign keys, search helps, technical settings, indexes, missing versions, and deletion.
- [ ] Run abaplint and verify abapGit import plus serialize-back equivalence for each supported TABL variant.

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
