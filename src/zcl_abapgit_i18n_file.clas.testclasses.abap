class ltcl_i18n_file definition
  for testing
  risk level harmless
  duration short
  final.

  private section.
    methods get_happy_pass_data
      importing
        iv_case type i
      exporting
        et_tobjs type zif_abapgit_i18n=>tt_text_object
        ev_rendered type string.

    methods render for testing raising zcx_abapgit_exception.
    methods parse for testing raising zcx_abapgit_exception.
    methods parse_fails for testing raising zcx_abapgit_exception.
    methods get for testing raising zcx_abapgit_exception.
    methods put_duplicates for testing raising zcx_abapgit_exception.

    " internals
    methods detect_line_type for testing raising zcx_abapgit_exception.
    methods parse_line for testing raising zcx_abapgit_exception.
endclass.

class zcl_abapgit_i18n_file definition local friends ltcl_i18n_file.

class ltcl_i18n_file implementation.

  method get_happy_pass_data.

    field-symbols <ls_tobj> like line of et_tobjs.
    field-symbols <ls_lang> like line of <ls_tobj>-texts.

    clear et_tobjs.

    if iv_case = 1.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-sub_type = 'DYNP'.
      <ls_tobj>-sub_name = '2001'.
      <ls_tobj>-dev_type = 'SRH4'.
      <ls_tobj>-id       = '1'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'ABC'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'ABC2'.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-sub_type = 'DYNP'.
      <ls_tobj>-sub_name = '2001'.
      <ls_tobj>-dev_type = 'SRH4'.
      <ls_tobj>-id       = '2'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'EFG'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'EFG2'.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-sub_type = 'DYNP'.
      <ls_tobj>-sub_name = '2001'.
      <ls_tobj>-dev_type = 'SRT4'.
      <ls_tobj>-id       = '1'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'XYZ'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'XYZ2'.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-sub_type = 'DYNP'.
      <ls_tobj>-sub_name = '2002'.
      <ls_tobj>-dev_type = 'SRH4'.
      <ls_tobj>-id       = '1'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'WTF'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'WTF2'.

      ev_rendered =
        '@@@DYNP:2001\n' &&
        '@@SRH4\n' &&
        '@1\n' &&
        'EN:ABC\n' &&
        'DE:ABC2\n' &&
        '@2\n' &&
        'EN:EFG\n' &&
        'DE:EFG2\n' &&
        '@@SRT4\n' &&
        '@1\n' &&
        'EN:XYZ\n' &&
        'DE:XYZ2\n' &&
        '@@@DYNP:2002\n' &&
        '@@SRH4\n' &&
        '@1\n' &&
        'EN:WTF\n' &&
        'DE:WTF2'.

    elseif iv_case = 2.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-dev_type = 'SRH4'.
      <ls_tobj>-id       = '1'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'ABC'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'ABC2'.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-dev_type = 'SRH4'.
      <ls_tobj>-id       = '2'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'EFG'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'EFG2'.

      append initial line to et_tobjs assigning <ls_tobj>.
      <ls_tobj>-dev_type = 'SRT4'.
      <ls_tobj>-id       = '1'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'EN'.
      <ls_lang>-text = 'XYZ'.
      append initial line to <ls_tobj>-texts assigning <ls_lang>.
      <ls_lang>-lang = 'DE'.
      <ls_lang>-text = 'XYZ2'.

      ev_rendered =
        '@@SRH4\n' &&
        '@1\n' &&
        'EN:ABC\n' &&
        'DE:ABC2\n' &&
        '@2\n' &&
        'EN:EFG\n' &&
        'DE:EFG2\n' &&
        '@@SRT4\n' &&
        '@1\n' &&
        'EN:XYZ\n' &&
        'DE:XYZ2'.

    endif.

    replace all occurrences of '\n' in ev_rendered with cl_abap_char_utilities=>newline.

  endmethod.

  method render.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    data lt_tobjs type zif_abapgit_i18n=>tt_text_object.
    data lv_act type string.
    data lv_exp type string.

    do 2 times.

      get_happy_pass_data(
        exporting
          iv_case  = sy-index
        importing
          et_tobjs = lt_tobjs
          ev_rendered = lv_exp ).

      create object lo_cut.
      lo_cut->put( lt_tobjs ).
      lv_act = lo_cut->render( ).

      cl_abap_unit_assert=>assert_equals( act = lv_act exp = lv_exp ).

    enddo.

  endmethod.

  method put_duplicates.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    data lt_tobjs type zif_abapgit_i18n=>tt_text_object.

    field-symbols <ls_tobj> like line of lt_tobjs.

    create object lo_cut.

    append initial line to lt_tobjs assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    <ls_tobj>-id       = '1'.

    lo_cut->put( lt_tobjs ).

    try .
      lo_cut->put( lt_tobjs ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.

    endtry.

  endmethod.

  method get.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    data lt_tobjs type zif_abapgit_i18n=>tt_text_object.
    data lt_tobjs_exp type zif_abapgit_i18n=>tt_text_object.

    field-symbols <ls_tobj> like line of lt_tobjs.

    create object lo_cut.

    append initial line to lt_tobjs assigning <ls_tobj>.
    <ls_tobj>-sub_type = 'DYNP'.
    <ls_tobj>-sub_name = '2001'.
    <ls_tobj>-dev_type = 'SRH4'.
    <ls_tobj>-id       = '1'.
    append <ls_tobj> to lt_tobjs_exp.

    lo_cut->put( lt_tobjs ).

    <ls_tobj>-id       = '2'.
    append <ls_tobj> to lt_tobjs_exp.

    lo_cut->put( lt_tobjs ).

    <ls_tobj>-sub_name = '2002'.
    lo_cut->put( lt_tobjs ).

    lt_tobjs = lo_cut->get( iv_sub_type = 'DYNP' iv_sub_name = '2001' ).
    cl_abap_unit_assert=>assert_equals( act = lt_tobjs exp = lt_tobjs_exp ).

  endmethod.

  method parse.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    data lt_tobjs_exp type zif_abapgit_i18n=>tt_text_object.
    data lv_data type string.

    do 2 times.

      get_happy_pass_data(
        exporting
          iv_case = sy-index
        importing
          et_tobjs    = lt_tobjs_exp
          ev_rendered = lv_data ).

      create object lo_cut.
      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>assert_equals( act = lo_cut->mt_text_objects exp = lt_tobjs_exp ).

    enddo.

  endmethod.

  method parse_fails.

    data lv_data type string.
    data lo_cut type ref to zcl_abapgit_i18n_file.
    create object lo_cut.

    try .
      lv_data =
        'EN:translation'.
      replace all occurrences of '\n' in lv_data with cl_abap_char_utilities=>newline.

      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lv_data =
        '@DEV1\n' &&
        'EN:translation'.
      replace all occurrences of '\n' in lv_data with cl_abap_char_utilities=>newline.

      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lv_data =
        '@@@DEV1:DEV2\n' &&
        '@@@DEV1:DEV3\n' &&
        '@@DEV1\n' &&
        '@123\n' &&
        'EN:translation'.
      replace all occurrences of '\n' in lv_data with cl_abap_char_utilities=>newline.

      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lv_data =
        '@@@DEV1:DEV2\n' &&
        '@@DEV1\n' &&
        '@@DEV2\n' &&
        '@123\n' &&
        'EN:translation'.
      replace all occurrences of '\n' in lv_data with cl_abap_char_utilities=>newline.

      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lv_data =
        '@@@DEV1:DEV2\n' &&
        '@@DEV1\n' &&
        '@123\n' &&
        '@124\n' &&
        'EN:translation'.
      replace all occurrences of '\n' in lv_data with cl_abap_char_utilities=>newline.

      lo_cut->parse( lv_data ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

  endmethod.

  method detect_line_type.

    data lv_type type c.
    data lv_data type string.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    create object lo_cut.

    lo_cut->detect_line_type(
      exporting
        iv_line = '@@@ABC:EFG'
      importing
        ev_content = lv_data
        ev_type    = lv_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_type exp = zcl_abapgit_i18n_file=>c_line_type-sub_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'ABC:EFG' ).

    lo_cut->detect_line_type(
      exporting
        iv_line = '@@XYZ1'
      importing
        ev_content = lv_data
        ev_type    = lv_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_type exp = zcl_abapgit_i18n_file=>c_line_type-dev_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'XYZ1' ).

    lo_cut->detect_line_type(
      exporting
        iv_line = '@text_key'
      importing
        ev_content = lv_data
        ev_type    = lv_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_type exp = zcl_abapgit_i18n_file=>c_line_type-text_key ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'text_key' ).

    lo_cut->detect_line_type(
      exporting
        iv_line = 'EN:translation'
      importing
        ev_content = lv_data
        ev_type    = lv_type ).
    cl_abap_unit_assert=>assert_equals( act = lv_type exp = zcl_abapgit_i18n_file=>c_line_type-text ).
    cl_abap_unit_assert=>assert_equals( act = lv_data exp = 'EN:translation' ).

    try .
      lo_cut->detect_line_type(
        exporting
          iv_line = 'unexpected text' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

  endmethod.

  method parse_line.

    data ls_tobj_key type zif_abapgit_i18n=>ty_text_object_key.
    data ls_tobj_key_exp type zif_abapgit_i18n=>ty_text_object_key.
    data ls_trans type zif_abapgit_i18n=>ty_translation.
    data ls_trans_exp type zif_abapgit_i18n=>ty_translation.

    data lo_cut type ref to zcl_abapgit_i18n_file.
    create object lo_cut.

    lo_cut->parse_line(
      exporting
        iv_type    = zcl_abapgit_i18n_file=>c_line_type-sub_type
        iv_content = 'ABC:EFG'
      importing
        ev_text_object_key = ls_tobj_key
        ev_translation     = ls_trans ).

    clear: ls_tobj_key_exp, ls_trans_exp.
    ls_tobj_key_exp-sub_type = 'ABC'.
    ls_tobj_key_exp-sub_name = 'EFG'.
    cl_abap_unit_assert=>assert_equals( act = ls_tobj_key exp = ls_tobj_key_exp ).
    cl_abap_unit_assert=>assert_equals( act = ls_trans exp = ls_trans_exp ).

    lo_cut->parse_line(
      exporting
        iv_type    = zcl_abapgit_i18n_file=>c_line_type-dev_type
        iv_content = 'ABC'
      importing
        ev_text_object_key = ls_tobj_key
        ev_translation     = ls_trans ).

    clear: ls_tobj_key_exp, ls_trans_exp.
    ls_tobj_key_exp-dev_type = 'ABC'.
    cl_abap_unit_assert=>assert_equals( act = ls_tobj_key exp = ls_tobj_key_exp ).
    cl_abap_unit_assert=>assert_equals( act = ls_trans exp = ls_trans_exp ).

    lo_cut->parse_line(
      exporting
        iv_type    = zcl_abapgit_i18n_file=>c_line_type-text_key
        iv_content = 'ABC'
      importing
        ev_text_object_key = ls_tobj_key
        ev_translation     = ls_trans ).

    clear: ls_tobj_key_exp, ls_trans_exp.
    ls_tobj_key_exp-id = 'ABC'.
    cl_abap_unit_assert=>assert_equals( act = ls_tobj_key exp = ls_tobj_key_exp ).
    cl_abap_unit_assert=>assert_equals( act = ls_trans exp = ls_trans_exp ).

    lo_cut->parse_line(
      exporting
        iv_type    = zcl_abapgit_i18n_file=>c_line_type-text
        iv_content = 'EN:translation'
      importing
        ev_text_object_key = ls_tobj_key
        ev_translation     = ls_trans ).

    clear: ls_tobj_key_exp, ls_trans_exp.
    ls_trans_exp-lang = 'EN'.
    ls_trans_exp-text = 'translation'.
    cl_abap_unit_assert=>assert_equals( act = ls_tobj_key exp = ls_tobj_key_exp ).
    cl_abap_unit_assert=>assert_equals( act = ls_trans exp = ls_trans_exp ).

    lo_cut->parse_line(
      exporting
        iv_type    = zcl_abapgit_i18n_file=>c_line_type-text
        iv_content = 'EN:'
      importing
        ev_text_object_key = ls_tobj_key
        ev_translation     = ls_trans ).

    clear: ls_tobj_key_exp, ls_trans_exp.
    ls_trans_exp-lang = 'EN'.
    cl_abap_unit_assert=>assert_equals( act = ls_tobj_key exp = ls_tobj_key_exp ).
    cl_abap_unit_assert=>assert_equals( act = ls_trans exp = ls_trans_exp ).

    try .
      lo_cut->parse_line(
        exporting
          iv_type    = zcl_abapgit_i18n_file=>c_line_type-sub_type
          iv_content = 'ABC123:EFG' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lo_cut->parse_line(
        exporting
          iv_type    = zcl_abapgit_i18n_file=>c_line_type-dev_type
          iv_content = 'ABC123' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lo_cut->parse_line(
        exporting
          iv_type    = zcl_abapgit_i18n_file=>c_line_type-text
          iv_content = 'ABC:EFG' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

    try .
      lo_cut->parse_line(
        exporting
          iv_type    = zcl_abapgit_i18n_file=>c_line_type-text
          iv_content = '12:EFG' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_abapgit_exception ##NO_HANDLER.
    endtry.

  endmethod.

endclass.
