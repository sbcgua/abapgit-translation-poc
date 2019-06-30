class ZCL_ABAPGIT_I18N_KEY_STRATEGY definition
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

  class-methods CREATE
    importing
      iv_use_sub_type type abap_bool optional
      iv_use_sub_name type abap_bool optional
      iv_textkey_config type string optional
    returning
      value(RO_INSTANCE) type ref to ZCL_ABAPGIT_I18N_KEY_STRATEGY .
  methods USE_SUB_TYPE
    returning
      value(RO_INSTANCE) type ref to ZCL_ABAPGIT_I18N_KEY_STRATEGY .
  methods USE_SUB_NAME
    returning
      value(RO_INSTANCE) type ref to ZCL_ABAPGIT_I18N_KEY_STRATEGY .
  methods CONFIGURE_TEXTKEY
    importing
      iv_config type string
    returning
      value(RO_INSTANCE) type ref to ZCL_ABAPGIT_I18N_KEY_STRATEGY .
  methods ADD_TEXTKEY_SEGMENT
    importing
      !IV_LEN type I
      !IV_OPTIONAL_INT type ABAP_BOOL optional
      !IV_ZEROPAD_INT  type ABAP_BOOL optional
    returning
      value(RO_INSTANCE) type ref to ZCL_ABAPGIT_I18N_KEY_STRATEGY .
  methods BUILD_KEY
    importing
      !IV_SUB_TYPE type ZIF_ABAPGIT_I18N=>TY_OBJ_TYPE
      !IV_SUB_NAME type ZIF_ABAPGIT_I18N=>TY_SUB_NAME
      !IV_DEV_TYPE type ZIF_ABAPGIT_I18N=>TY_OBJ_TYPE
      !IV_TEXTKEY  type LXETEXTKEY
      !IV_MAX_SIZE type I
    returning
      value(RV_KEY) type STRING .
  methods PARSE_KEY
    importing
      !IV_KEY type STRING
    exporting
      !EV_SUB_TYPE type ZIF_ABAPGIT_I18N=>TY_OBJ_TYPE
      !EV_SUB_NAME type ZIF_ABAPGIT_I18N=>TY_SUB_NAME
      !EV_DEV_TYPE type ZIF_ABAPGIT_I18N=>TY_OBJ_TYPE
      !EV_TEXTKEY  type LXETEXTKEY
      !EV_MAX_SIZE type I
    raising
      zcx_abapgit_exception .
  protected section.
  private section.
    data mv_use_sub_type type abap_bool.
    data mv_use_sub_name type abap_bool.
    data mt_segments type tt_segments.

    class-methods shift_tab
      changing
        ct_tab type string_table
        cv_val type string
      raising
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_I18N_KEY_STRATEGY IMPLEMENTATION.


  method add_textkey_segment.
    field-symbols <ls_i> like line of mt_segments.
    append initial line to mt_segments assigning <ls_i>.
    <ls_i>-len          = iv_len.
    <ls_i>-optional_int = iv_optional_int.
    <ls_i>-zeropad_int  = iv_zeropad_int.
    ro_instance = me.
  endmethod.


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

    if mv_use_sub_type = abap_true.
      append iv_sub_type to lt_comps1.
    endif.

    if mv_use_sub_name = abap_true.
      append iv_sub_name to lt_comps1.
    endif.

    append iv_dev_type to lt_comps1.

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

    " list of segments is separated by "," e.g. "10,5,i10"
    " segment format: [i|z]D+
    " i - optional int
    " z - zeropad int
    " D+ - 1 or more digits
    " e.g.: 10, i10, z10

    split iv_config at ',' into table lt_segments.

    loop at lt_segments assigning <iv_s>.
      clear: lv_segment_len, lv_optional_int, lv_zeropad_int.
      lv_len = strlen( <iv_s> ).
      assert lv_len > 0.
      lv_suffix = substring( val = <iv_s> off = lv_len - 1 ).

      if lv_suffix = 'i'.
        lv_optional_int = abap_true.
        <iv_s> = substring( val = <iv_s> len = lv_len - 1 ).
      elseif lv_suffix = 'z'.
        lv_zeropad_int = abap_true.
        <iv_s> = substring( val = <iv_s> len = lv_len - 1 ).
      endif.
      assert <iv_s> co '0123456789'.

      lv_segment_len = <iv_s>.
      lv_sum_len = lv_sum_len + lv_segment_len.
      add_textkey_segment(
        iv_len          = lv_segment_len
        iv_optional_int = lv_optional_int
        iv_zeropad_int  = lv_zeropad_int ).
    endloop.

    assert lv_sum_len <= 32. " max size of textkey
    ro_instance = me.

  endmethod.


  method create.
    create object ro_instance.
    ro_instance->mv_use_sub_type = iv_use_sub_type.
    ro_instance->mv_use_sub_name = iv_use_sub_name.
    if iv_textkey_config is not initial.
      ro_instance->configure_textkey( iv_textkey_config ).
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

    " sub type
    if mv_use_sub_type = abap_true.
      shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
      if strlen( lv_temp ) > 4.
        zcx_abapgit_exception=>raise( 'text key parsing: wrong length of sub type' ).
      endif.
      ev_sub_type = lv_temp.
    endif.

    " Sub name
    if mv_use_sub_name = abap_true.
      shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
      if strlen( lv_temp ) > 80.
        zcx_abapgit_exception=>raise( 'text key parsing: wrong length of sub name' ).
      endif.
      ev_sub_name = lv_temp.
    endif.

    " Dev type
    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
    if strlen( lv_temp ) > 4.
      zcx_abapgit_exception=>raise( 'text key parsing: wrong length of dev type' ).
    endif.
    ev_dev_type = lv_temp.

    " text key
    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
    split lv_temp at c_key_splitter into table lt_comps2.
    loop at mt_segments assigning <ls_seg>.
      read table lt_comps2 index sy-tabix into lv_temp.
      if sy-subrc > 0.
        exit. " remaining segments are empty
      endif.

      if <ls_seg>-optional_int = abap_true.
        if not lv_temp co '0123456789'.
          zcx_abapgit_exception=>raise( |text key parsing: segment must be int ({ lv_temp })| ).
        endif.
        ev_textkey+lv_off(<ls_seg>-len) = lv_temp.
      elseif <ls_seg>-zeropad_int = abap_true.
        if not lv_temp co '0123456789'.
          zcx_abapgit_exception=>raise( |text key parsing: segment must be int ({ lv_temp })| ).
        endif.
        unpack lv_temp to ev_textkey+lv_off(<ls_seg>-len).
      else.
        ev_textkey+lv_off(<ls_seg>-len) = lv_temp.
      endif.
      lv_off = lv_off + <ls_seg>-len.

    endloop.


    " Max length
    shift_tab( changing ct_tab = lt_comps1 cv_val = lv_temp ).
    if not lv_temp co '0123456789'.
      zcx_abapgit_exception=>raise( 'text key parsing: max size must be a number' ).
    endif.
    ev_max_size = lv_temp.


  endmethod.


  method shift_tab.

    if lines( ct_tab ) = 0.
      zcx_abapgit_exception=>raise( 'unexpected (small) number of text key segments' ).
    endif.
    read table ct_tab index 1 into cv_val.
    delete ct_tab index 1.

  endmethod.


  method use_sub_name.
    mv_use_sub_name = abap_true.
    ro_instance = me.
  endmethod.


  method use_sub_type.
    mv_use_sub_type = abap_true.
    ro_instance = me.
  endmethod.
ENDCLASS.
