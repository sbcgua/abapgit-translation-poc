class zcl_abapgit_i18n_file definition
  public
  final
  create public .

  public section.

    constants:
      begin of c_line_type,
        sub_type type c value '1',
        dev_type type c value '2',
        text_key type c value '3',
        text type c value '4',
      end of c_line_type.

    methods render
      returning
        value(rv_data) type string .
    methods parse
      importing
        !iv_data type string
      raising
        zcx_abapgit_exception .
    methods get
      importing
        !iv_sub_type type zif_abapgit_i18n=>ty_text_object-sub_type
        !iv_sub_name type zif_abapgit_i18n=>ty_text_object-sub_name
      returning
        value(rt_tab) type zif_abapgit_i18n=>tt_text_object .
    methods put
      importing
        !it_tobjs type zif_abapgit_i18n=>tt_text_object
      raising
        zcx_abapgit_exception .
  protected section.
  private section.
    data mt_text_objects type zif_abapgit_i18n=>tt_text_object.

    class-methods detect_line_type
      importing
        iv_line type string
      exporting
        ev_content type string
        ev_type type c
      raising
        zcx_abapgit_exception .

    class-methods parse_line
      importing
        iv_content type string
        iv_type    type c
      exporting
        ev_text_object_key type zif_abapgit_i18n=>ty_text_object_key
        ev_translation     type zif_abapgit_i18n=>ty_translation
      raising
        zcx_abapgit_exception .

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_FILE IMPLEMENTATION.


  method detect_line_type.

      data lv_line_len type i.

      lv_line_len = strlen( iv_line ).

      if lv_line_len > 3 and substring( val = iv_line len = 3 ) = '@@@'.
        ev_type = c_line_type-sub_type.
        ev_content = substring( val = iv_line off = 3 ).
      elseif lv_line_len > 2 and substring( val = iv_line len = 2 ) = '@@'.
        ev_type = c_line_type-dev_type.
        ev_content = substring( val = iv_line off = 2 ).
      elseif lv_line_len > 1 and substring( val = iv_line len = 1 ) = '@'.
        ev_type = c_line_type-text_key.
        ev_content = substring( val = iv_line off = 1 ).
      elseif lv_line_len >= 3
        and substring( val = iv_line off = 2 len = 1 ) = ':'
        and substring( val = iv_line len = 2 ) co 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' .
        ev_type = c_line_type-text.
        ev_content = iv_line.
      else.
        zcx_abapgit_exception=>raise( 'i18n parsing: unexpected line structure' ).
      endif.

  endmethod.


  method get.
    " Get all texts related to sub_type+sub_name -> to be saved in one slug
    field-symbols <ls_i> like line of mt_text_objects.

    loop at mt_text_objects assigning <ls_i> where sub_type = iv_sub_type and sub_name = iv_sub_name.
      append <ls_i> to rt_tab.
    endloop.

  endmethod.


  method parse.

    data lt_lines type string_table.
    data lv_line_type type c.
    data lv_prev_type type c.
    data lv_temp type string.

    data lt_text_objects like mt_text_objects.
    data ls_tobj_key type zif_abapgit_i18n=>ty_text_object_key.
    data ls_tobj_stencil type zif_abapgit_i18n=>ty_text_object_key.
    data ls_trans type zif_abapgit_i18n=>ty_translation.

    field-symbols <lv_i> like line of lt_lines.
    field-symbols <ls_tobj> like line of lt_text_objects.

    split iv_data at cl_abap_char_utilities=>newline into table lt_lines.

    loop at lt_lines assigning <lv_i>.

      if <lv_i> is initial. " TODO refactor when <<<EOT is implemented
        continue.
      endif.

      detect_line_type(
        exporting
          iv_line = <lv_i>
        importing
          ev_type    = lv_line_type
          ev_content = lv_temp ).

      parse_line(
        exporting
          iv_type    = lv_line_type
          iv_content = lv_temp
        importing
          ev_text_object_key = ls_tobj_key
          ev_translation     = ls_trans ).

      case lv_line_type.
        when c_line_type-sub_type.
          if not ( lv_prev_type is initial or lv_prev_type = c_line_type-text ).
            zcx_abapgit_exception=>raise( 'i18n parsing: unexpected line order (sub_type)' ).
          endif.
          clear ls_tobj_stencil.
          ls_tobj_stencil-sub_type = ls_tobj_key-sub_type.
          ls_tobj_stencil-sub_name = ls_tobj_key-sub_name.

        when c_line_type-dev_type.
          if not ( lv_prev_type is initial or lv_prev_type = c_line_type-text or lv_prev_type = c_line_type-sub_type ).
            zcx_abapgit_exception=>raise( 'i18n parsing: unexpected line order (dev_type)' ).
          endif.
          ls_tobj_stencil-dev_type = ls_tobj_key-dev_type.

        when c_line_type-text_key.
          if not ( lv_prev_type = c_line_type-text or lv_prev_type = c_line_type-dev_type ).
            zcx_abapgit_exception=>raise( 'i18n parsing: unexpected line order (text_key)' ).
          endif.
          append initial line to lt_text_objects assigning <ls_tobj>.
          move-corresponding ls_tobj_stencil to <ls_tobj>.
          <ls_tobj>-id = ls_tobj_key-id.

        when c_line_type-text.
          if not ( lv_prev_type = c_line_type-text or lv_prev_type = c_line_type-text_key ).
            zcx_abapgit_exception=>raise( 'i18n parsing: unexpected line order (text)' ).
          endif.
          append ls_trans to <ls_tobj>-texts.

        when others.
          assert 1 = 0.
      endcase.

      lv_prev_type = lv_line_type.

    endloop.

    " TODO duplicate detection ?

    mt_text_objects = lt_text_objects.
    sort mt_text_objects by sub_type sub_name dev_type id.

  endmethod.


  method parse_line.

    data lv_temp1 type string.
    data lv_temp2 type string.

    clear: ev_text_object_key, ev_translation.

    case iv_type.
      when c_line_type-sub_type.
        split iv_content at ':' into lv_temp1 lv_temp2.
        if strlen( lv_temp1 ) > 4.
          zcx_abapgit_exception=>raise( |i18n parsing: subtype length > 4 "{ lv_temp1 }"| ).
        elseif strlen( lv_temp2 ) > 40.
          zcx_abapgit_exception=>raise( |i18n parsing: subname length > 40 "{ lv_temp2 }"| ).
        endif.
        ev_text_object_key-sub_type = lv_temp1.
        ev_text_object_key-sub_name = lv_temp2.

      when c_line_type-dev_type.
        if strlen( iv_content ) > 4.
          zcx_abapgit_exception=>raise( |i18n parsing: devtype length > 4 "{ iv_content }"| ).
        endif.
        if iv_content is initial.
          zcx_abapgit_exception=>raise( |i18n parsing: devtype cannot be empty| ).
        endif.
        ev_text_object_key-dev_type = iv_content.

      when c_line_type-text_key.
        if iv_content is initial.
          zcx_abapgit_exception=>raise( |i18n parsing: textkey cannot be empty| ).
        endif.
        ev_text_object_key-id = iv_content.

      when c_line_type-text.
        split iv_content at ':' into lv_temp1 lv_temp2.
        if strlen( lv_temp1 ) <> 2 or not lv_temp1 co 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
          zcx_abapgit_exception=>raise( |i18n parsing: wrong language "{ lv_temp1 }"| ).
        endif.
        ev_translation-lang = lv_temp1.
        ev_translation-text = lv_temp2.

      when others.
        assert 1 = 0.
    endcase.

  endmethod.


  method put.
    append lines of it_tobjs to mt_text_objects.

    " no perfect, maybe refactor ?
    " also message is not descriptive at all - improve ! add object
    sort mt_text_objects by sub_type sub_name dev_type id.
    delete adjacent duplicates from mt_text_objects comparing sub_type sub_name dev_type id.
    if sy-subrc = 0. " something deleted
      zcx_abapgit_exception=>raise( 'zcl_abapgit_i18n_file->put: duplicate detected' ).
    endif.

  endmethod.


  method render.

    data lt_lines type string_table.
    data lv_tmp   type string.
    field-symbols <i>    like line of mt_text_objects.
    field-symbols <text> like line of <i>-texts.

    data:
      begin of ls_cursor1,
        sub_type like <i>-sub_type,
        sub_name like <i>-sub_name,
      end of ls_cursor1,
      begin of ls_cursor2,
        dev_type like <i>-dev_type,
      end of ls_cursor2,
      ls_cursor1_prev like ls_cursor1,
      ls_cursor2_prev like ls_cursor2.

    sort mt_text_objects by sub_type sub_name dev_type id.

    " Expected file structure example
    " @@@DYNP:2001
    " @@SRH4
    " @DTXT,1:60
    " EN:english label
    " DE:german label
    " @DTXT,2:60
    " EN:english label
    " DE:german label
    " @@SRT4 ...

    " for empty sub_type there will be no @@@
    " but this is ok since parsing will also detect empty sub_type
    " and there should be jusy one empty sub type withing one object

    loop at mt_text_objects assigning <i>.
      move-corresponding <i> to ls_cursor1.
      move-corresponding <i> to ls_cursor2.

      if ls_cursor1 <> ls_cursor1_prev.
        clear ls_cursor2_prev.
        ls_cursor1_prev = ls_cursor1.
        if <i>-sub_type is not initial.
          lv_tmp = |@@@{ <i>-sub_type }:{ <i>-sub_name }|.
        else.
          lv_tmp = |@@@-|.
        endif.
        append lv_tmp to lt_lines.
      endif.
      if ls_cursor2 <> ls_cursor2_prev.
        ls_cursor2_prev = ls_cursor2.
        lv_tmp = |@@{ <i>-dev_type }|.
        append lv_tmp to lt_lines.
      endif.

      lv_tmp = |@{ <i>-id }|.
      append lv_tmp to lt_lines.

      loop at <i>-texts assigning <text>.
        lv_tmp = |{ <text>-lang }:{ <text>-text }|.
        append lv_tmp to lt_lines.
      endloop.
    endloop.

    rv_data = concat_lines_of( table = lt_lines sep = cl_abap_char_utilities=>newline ).

  endmethod.
ENDCLASS.
