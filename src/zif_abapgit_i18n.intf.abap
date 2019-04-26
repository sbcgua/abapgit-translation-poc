interface ZIF_ABAPGIT_I18N
  public .

  types:
    ty_obj_type type c length 10, " Technically should be TROBJTYPE, but potentially customly defined translations may exist too, so leave some space for them
    ty_sub_name type c length 80, " length(SOBJ_SNAME) - length(SOBJ_NAME)
    ty_textid   type string,      " ???

    ty_langid   type laiso,
    tt_langid   type standard table of ty_langid with default key,

    begin of ty_translation,
      lang type ty_langid,
      text type string,
    end of ty_translation,
    tt_translation type standard table of ty_translation with key lang,

    begin of ty_text_object,
      type      type ty_obj_type, " cumulative type, like DYNP
      sub_name  type ty_sub_name, " optional subname e.g 2000 (dynp)
      id        type ty_textid,   " textid, specific to object, e.g. SRH4:DTXT      00001:50, where SRH4 is specific type, DTXT... text key, 50 max len
      texts     type tt_translation,
    end of ty_text_object,
    tt_text_object type standard table of ty_text_object with key type sub_name id.

endinterface.
