interface ZIF_ABAPGIT_I18N
  public .

  types:
    ty_obj_type type c length 4,  " length(TROBJTYPE)
    ty_sub_name type c length 80, " length(SOBJ_SNAME) - length(SOBJ_NAME)
    ty_textid   type string,      " ???

    ty_langid   type laiso,
    tt_langid   type standard table of ty_langid with default key,

    begin of ty_translation,
      lang type ty_langid,
      text type string,
    end of ty_translation,
    tt_translation type standard table of ty_translation with key lang,

    begin of ty_text_object_key,
      sub_type  type ty_obj_type, " cumulative type, like DYNP
      sub_name  type ty_sub_name, " optional subname e.g 2000 (dynp)
      dev_type  type ty_obj_type, " e.g. SRH4
      id        type ty_textid,   " textid, specific to object, e.g. SRH4:DTXT      00001:50, where SRH4 is specific type, DTXT... text key, 50 max len
    end of ty_text_object_key,

    begin of ty_text_object.
      include type ty_text_object_key.
      types:
      texts     type tt_translation,
    end of ty_text_object,
    tt_text_object type standard table of ty_text_object with key sub_type sub_name dev_type id.

endinterface.
