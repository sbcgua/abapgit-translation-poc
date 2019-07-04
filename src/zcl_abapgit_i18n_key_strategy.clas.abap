class zcl_abapgit_i18n_key_strategy definition
  public
  final
  create private .

  public section.

    constants c_id_splitter type c value ':'.
    constants c_key_splitter type c value ','.

    types:
      begin of ty_segment,
          len          type i,
          optional_int type abap_bool,
          zeropad_int  type abap_bool,
        end of ty_segment .
    types:
      tt_segments type standard table of ty_segment with default key .
    types:
      begin of ty_destructured_key,
        sub_type type zif_abapgit_i18n=>ty_obj_type,
        sub_name type zif_abapgit_i18n=>ty_sub_name,
        dev_type type zif_abapgit_i18n=>ty_obj_type,
        textkey  type lxetextkey,
        max_size type i,
      end of ty_destructured_key.

    class-methods create
      importing
        iv_use_sub_type type abap_bool optional
        iv_use_sub_name type abap_bool optional
        iv_textkey_config type string optional
      returning
        value(ro_instance) type ref to zcl_abapgit_i18n_key_strategy .
    methods build_key
      importing
        !iv_sub_type type zif_abapgit_i18n=>ty_obj_type
        !iv_sub_name type zif_abapgit_i18n=>ty_sub_name
        !iv_dev_type type zif_abapgit_i18n=>ty_obj_type
        !iv_textkey  type lxetextkey
        !iv_max_size type i
      returning
        value(rv_key) type string .
    methods parse_key
      importing
        !iv_key type string
      returning
        value(rs_key) type ty_destructured_key
      raising
        zcx_abapgit_exception .
    protected section.
    private section.
      data mv_use_sub_type type abap_bool.
      data mv_use_sub_name type abap_bool.
      data mt_segments type tt_segments.

      methods configure_textkey
        importing
          iv_config type string.

      class-methods shift_tab
        changing
          ct_tab type string_table
          cv_val type string
        raising
          zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_KEY_STRATEGY IMPLEMENTATION.


  method build_key.

    data lv_off type i.
    data lv_buf type c length 32. " like lxetextkey
    data lv_int type i.

    data lt_comps1 type string_table.
    data lt_comps2 type string_table.
    data lv_temp type string.
    data lv_lines type i.

    field-symbols <ls_seg> like line of mt_segments.
    field-symbols <lv_tmp> like line of lt_comps2.

*    if mv_use_sub_type = abap_true.
*      append iv_sub_type to lt_comps1.
*    endif.
*
*    if mv_use_sub_name = abap_true.
*      append iv_sub_name to lt_comps1.
*    endif.
*
*    append iv_dev_type to lt_comps1.

    loop at mt_segments assigning <ls_seg>.
      lv_buf = iv_textkey+lv_off(<ls_seg>-len).
      lv_off = lv_off + <ls_seg>-len.
      if <ls_seg>-optional_int = abap_true.
        lv_int = lv_buf.
        if lv_int > 0.
          lv_temp = |{ lv_int }|.
          append lv_temp to lt_comps2.
        endif.
      elseif <ls_seg>-zeropad_int = abap_true.
        lv_int = lv_buf.
        lv_temp = |{ lv_int }|.
        append lv_temp to lt_comps2.
      else.
        append lv_buf to lt_comps2.
      endif.
    endloop.

    " remove last empty rows
    lv_lines = lines( lt_comps2 ).
    do lv_lines times.
      read table lt_comps2 index lv_lines - sy-index + 1 into lv_temp.
      if lv_temp is initial.
        delete lt_comps2 index sy-tabix.
      else.
        exit.
      endif.
    enddo.

    lv_temp = concat_lines_of( table = lt_comps2 sep = c_key_splitter ).
    append lv_temp to lt_comps1.
    lv_temp = |{ iv_max_size }|.
    append lv_temp to lt_comps1.
    rv_key = concat_lines_of( table = lt_comps1 sep = c_id_splitter ).

  endmethod.


  method configure_textkey.

    data lt_segments type string_table.
    field-symbols <iv_s> like line of lt_segments.

    data lv_sum_len type i.
    data lv_len type i.
    data lv_suffix type c.
    data lv_segment_len type i.
    data lv_optional_int type abap_bool.
    data lv_zeropad_int type abap_bool.
    data lv_has_zeropad type abap_bool.
    data lv_has_optional type abap_bool.

    " list of segments is separated by "," e.g. "10,5,10i"
    " segment format: [i|z]D+
    " i - optional int
    " z - zeropad int
    " D+ - 1 or more digits
    " e.g.: 10, 10i, 10z

    split iv_config at ',' into table lt_segments.

    loop at lt_segments assigning <iv_s>.
      clear: lv_segment_len, lv_optional_int, lv_zeropad_int.
      lv_len = strlen( <iv_s> ).
      assert lv_len > 0.
      lv_suffix = substring( val = <iv_s> off = lv_len - 1 ).

      if lv_suffix = 'i'.
        lv_has_optional = abap_true.
        lv_optional_int = abap_true.
        <iv_s> = substring( val = <iv_s> len = lv_len - 1 ).
      elseif lv_suffix = 'z'.
        lv_has_zeropad = abap_true.
        lv_zeropad_int = abap_true.
        <iv_s> = substring( val = <iv_s> len = lv_len - 1 ).
      endif.
      assert <iv_s> co '0123456789'.

      lv_segment_len = <iv_s>.
      lv_sum_len = lv_sum_len + lv_segment_len.

      field-symbols <ls_i> like line of mt_segments.
      append initial line to mt_segments assigning <ls_i>.
      <ls_i>-len          = lv_segment_len.
      <ls_i>-optional_int = lv_optional_int.
      <ls_i>-zeropad_int  = lv_zeropad_int.
    endloop.

    assert lv_sum_len <= 32. " max size of textkey
    assert not ( lv_has_zeropad = abap_true and lv_has_optional = abap_true ).
    " For the parsing protection ... but improve the check later

  endmethod.


  method create.
    create object ro_instance.
    ro_instance->mv_use_sub_type = iv_use_sub_type.
    ro_instance->mv_use_sub_name = iv_use_sub_name.
    if iv_textkey_config is not initial.
      ro_instance->configure_textkey( iv_textkey_config ).
    else.
      ro_instance->configure_textkey( '32' ). " Use whole string, length of textkey
    endif.
  endmethod.


  method parse_key.

    data lt_comps1 type string_table.
    data lt_comps2 type string_table.
    data lv_temp type string.
    data lv_int type i.
    data lv_off type i.
    field-symbols <ls_seg> like line of mt_segments.

    split iv_key at c_id_splitter into table lt_comps1.

*    " sub type
*    if mv_use_sub_type = abap_true.
*      shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
*      if strlen( lv_temp ) > 4.
*        zcx_abapgit_exception=>raise( 'text key parsing: wrong length of sub type' ).
*      endif.
*      rs_key-sub_type = lv_temp.
*    endif.
*
*    " Sub name
*    if mv_use_sub_name = abap_true.
*      shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
*      if strlen( lv_temp ) > 80.
*        zcx_abapgit_exception=>raise( 'text key parsing: wrong length of sub name' ).
*      endif.
*      rs_key-sub_name = lv_temp.
*    endif.
*
*    " Dev type
*    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
*    if strlen( lv_temp ) > 4.
*      zcx_abapgit_exception=>raise( 'text key parsing: wrong length of dev type' ).
*    endif.
*    rs_key-dev_type = lv_temp.

    " text key
    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
    split lv_temp at c_key_splitter into table lt_comps2.
    loop at mt_segments assigning <ls_seg>.
      read table lt_comps2 index sy-tabix into lv_temp.
      if sy-subrc > 0.
        exit. " remaining segments are empty
        " this leaves one potential bug if there is a zeropadded segment with zero value
        " this would mean thatthe component will be empty but the probably had "0000"
        " hopefully zero index does not make sense and will not meet in reality
      endif.

      if <ls_seg>-optional_int = abap_true.
        if not lv_temp co '0123456789'.
          zcx_abapgit_exception=>raise( |text key parsing: segment must be int ({ lv_temp })| ).
        endif.
        rs_key-textkey+lv_off(<ls_seg>-len) = lv_temp.
      elseif <ls_seg>-zeropad_int = abap_true.
        if not lv_temp co '0123456789'.
          zcx_abapgit_exception=>raise( |text key parsing: segment must be int ({ lv_temp })| ).
        endif.
        unpack lv_temp to rs_key-textkey+lv_off(<ls_seg>-len).
      else.
        rs_key-textkey+lv_off(<ls_seg>-len) = lv_temp.
      endif.
      lv_off = lv_off + <ls_seg>-len.

    endloop.


    " Max length
    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
    if not lv_temp co '0123456789'.
      zcx_abapgit_exception=>raise( 'text key parsing: max size must be a number' ).
    endif.
    rs_key-max_size = lv_temp.

  endmethod.


  method shift_tab.

    if lines( ct_tab ) = 0.
      zcx_abapgit_exception=>raise( 'unexpected (small) number of text key segments' ).
    endif.
    read table ct_tab index 1 into cv_val.
    delete ct_tab index 1.

  endmethod.
ENDCLASS.
