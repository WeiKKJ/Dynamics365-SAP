*----------------------------------------------------------------------*
***INCLUDE LZFG_CRMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form checkdom
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> ACTION
*&      <-- RTMSG
*&---------------------------------------------------------------------*
*检查值域
FORM checkdom USING indomain invalue CHANGING dommsg.
  DATA:it_dd07v TYPE TABLE OF dd07v WITH HEADER LINE,
       wa_dd01t TYPE dd01t.
  CHECK invalue IS NOT INITIAL.
  CLEAR dommsg.
  PERFORM getdomain(zpubform) TABLES it_dd07v USING indomain.
  READ TABLE it_dd07v WITH KEY domvalue_l = invalue.
  IF sy-subrc NE 0.
    SELECT SINGLE *
    INTO wa_dd01t
    FROM dd01t
    WHERE ddlanguage = sy-langu
    AND   as4local = 'A'
    AND   domname = indomain.
    CONCATENATE wa_dd01t-ddtext invalue '不在可输入范围内' INTO dommsg.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form i_longtext
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_HEAD_NEW_CONTRACTI
*&      --> <LT_TTXIT>-TDID
*&      --> <ZLONGTEXT>
*&---------------------------------------------------------------------*
FORM i_longtext  USING    p_sapno p_tdid
                          p_zlongtext.
  DATA:text_string TYPE string,
       text_table  TYPE TABLE OF zslongtext.
  CHECK p_zlongtext IS NOT INITIAL.
  text_string = p_zlongtext.
  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = p_zlongtext
      i_tabline_length = 132
*     I_UNICODE        =
    TABLES
      et_table         = text_table.
  CALL FUNCTION 'ZFM_DEALLONGTEXT'
    EXPORTING
      intype = 'I'
      tdid   = p_tdid
      sapno  = p_sapno
      sapmk  = 'SD'
    TABLES
      t_text = text_table.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ezsdr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> DATA_NEW_CONTRACTID
*&      <-- RTMSG
*&---------------------------------------------------------------------*
FORM ezsdr  USING    VALUE(p_unlock)
                     p_new_contractid
            CHANGING p_rtmsg.
  IF p_unlock NE 'X'."加锁.
    CALL FUNCTION 'ENQUEUE_EZNEW_CONTRACTID'
      EXPORTING
*       MODE_ZTCRM_SO_HEAD       = 'E'
*       MANDT          = SY-MANDT
        new_contractid = p_new_contractid
*       X_NEW_CONTRACTID         = ' '
        _scope         = '1'
*       _WAIT          = ' '
*       _COLLECT       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
*查找别名
      SELECT SINGLE *
      INTO @DATA(l_usrefus)
      FROM usrefus
      WHERE bname = @sy-msgv1.
      p_rtmsg = |用户{ sy-msgv1 }({ l_usrefus-useralias })正在处理{ p_new_contractid }|.
    ENDIF.
  ELSE.
    CALL FUNCTION 'DEQUEUE_EZNEW_CONTRACTID'
      EXPORTING
*       MODE_ZTCRM_SO_HEAD       = 'E'
*       MANDT          = SY-MANDT
        new_contractid = p_new_contractid
*       X_NEW_CONTRACTID         = ' '
        _scope         = '1'
*       _SYNCHRON      = ' '
*       _COLLECT       = ' '
      .
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form domain_value_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> DATA
*&      <-- RTMSG
*&---------------------------------------------------------------------*
FORM domain_value_check  USING    p_data
                         CHANGING p_rtmsg.
  DATA:it_dd07v TYPE TABLE OF dd07v WITH HEADER LINE,
       wa_dd01t TYPE dd01t.
  CLEAR p_rtmsg.
  CHECK p_data IS NOT INITIAL.
  TRY.
      DATA(realname) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( p_data ) )->get_relative_name( ).
    CATCH cx_root INTO DATA(exc).
      DATA(exct) = exc->get_text( ).
      RETURN.
  ENDTRY.
  SELECT SINGLE
    dd04l~domname,
    dd01t~ddtext
    FROM dd04l
    JOIN dd01t ON dd04l~domname = dd01t~domname AND dd01t~ddlanguage = @sy-langu AND dd01t~as4local = 'A'
    WHERE rollname = @realname
    AND dd04l~as4local = 'A'
    INTO @DATA(wa_dom).
  CHECK wa_dom IS NOT INITIAL.

  PERFORM getdomain(zpubform) TABLES it_dd07v USING wa_dom-domname.
  CHECK it_dd07v[] IS NOT INITIAL.
  READ TABLE it_dd07v WITH KEY domvalue_l = p_data.
  IF sy-subrc NE 0.
    p_rtmsg = |{ wa_dom-ddtext }[{ p_data }]不在可输入范围内|.
  ENDIF.
ENDFORM.

FORM fillcond USING p_posnr
                    p_kschl
*                    P_UNT
                    p_kbetr
                    p_vbak TYPE vbak
              CHANGING o_cond STRUCTURE bapicond
                       o_condx STRUCTURE bapicondx.
  TABLES:t683s.

  DATA:kalsm TYPE t683v-kalsm.
  CLEAR:o_cond,o_condx,t683s.
*  CHECK P_KSCHL IS NOT INITIAL
*  AND P_KBETR IS NOT INITIAL.

  PERFORM getkalsm(zpubform) USING p_vbak p_kschl
        CHANGING kalsm.

  SELECT SINGLE * FROM t683s
   WHERE kvewe = 'A'
     AND kappl = 'V'
     AND kalsm = kalsm
     AND kschl = p_kschl.
  o_cond-itm_number  = p_posnr.
  o_condx-itm_number = p_posnr.
  o_cond-cond_st_no  = t683s-stunr.
  o_condx-cond_st_no = t683s-stunr.
  o_cond-cond_count  = '01'.
  o_condx-cond_count = '01'.
  o_cond-cond_type   = p_kschl.
  o_condx-cond_type  = p_kschl.
*  o_cond-cond_p_unt = '1'.
*  o_condx-cond_p_unt = 'X'.
  o_cond-cond_value  = p_kbetr.
  o_condx-cond_value = 'X'.
  o_cond-currency    = p_vbak-waerk.
  IF t683s IS NOT INITIAL.
    o_condx-updateflag = 'U'.
  ELSE.
    o_condx-updateflag = 'I'.
  ENDIF.
ENDFORM.

FORM addunit USING matnr wsmei CHANGING message.

  DATA: headdata        TYPE bapimathead,
        return          TYPE bapiret2,
        clientdata      TYPE bapi_mara,
        clientdatax     TYPE bapi_marax,
        returnmessages  TYPE TABLE OF bapi_matreturn2 WITH HEADER LINE,
        unitsofmeasure  TYPE TABLE OF bapi_marm WITH HEADER LINE,
        unitsofmeasurex TYPE TABLE OF bapi_marmx WITH HEADER LINE.

  headdata-material = matnr.
  clientdata-uomusage = 'B'.
  clientdatax-uomusage = 'X'.
*维护比例产量单位
  IF wsmei IS NOT INITIAL.
    CLEAR:unitsofmeasure,unitsofmeasurex.
    unitsofmeasure-alt_unit = wsmei.
    unitsofmeasure-numerator = 1.
    unitsofmeasure-denominatr = 1.
    unitsofmeasurex-alt_unit = wsmei.
    unitsofmeasurex-numerator = 'X'.
    unitsofmeasurex-denominatr = 'X'.
    APPEND:unitsofmeasure,unitsofmeasurex.
  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata        = headdata
      clientdata      = clientdata
      clientdatax     = clientdatax
    IMPORTING
      return          = return
    TABLES
      returnmessages  = returnmessages
      unitsofmeasure  = unitsofmeasure
      unitsofmeasurex = unitsofmeasurex.

  LOOP AT returnmessages WHERE type CA 'AEX'.
    EXIT.
  ENDLOOP.
  IF sy-subrc EQ 0 OR return-type CA 'AEX'.
    message = 'E'.
    LOOP AT returnmessages WHERE type CA 'AEX'.
      message = message && returnmessages-message && '/' .
    ENDLOOP.
    message = message && return-message && '/' .
    PERFORM bapirun(zpubform) USING ''.
  ELSE.
    SET UPDATE TASK LOCAL.
    PERFORM bapirun(zpubform) USING 'X'.
    message = 'S:成功'.
  ENDIF.

ENDFORM.

FORM setbapix USING fs CHANGING fsx.
  FIELD-SYMBOLS:<fs>,<fsx>.
  DATA: outfieldcat TYPE slis_t_fieldcat_alv,
        wa_field    TYPE slis_fieldcat_alv.
  CLEAR:outfieldcat,wa_field,fsx.

  PERFORM gettabstru_se11(zpubform) USING fsx CHANGING outfieldcat.

  LOOP AT outfieldcat INTO wa_field.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fs TO <fs>.
    IF sy-subrc <> 0 .
      CONTINUE.
    ENDIF.
    ASSIGN COMPONENT wa_field-fieldname OF STRUCTURE fsx TO <fsx>.
    IF sy-subrc <> 0 .
      EXIT.
    ENDIF.
*对于长度为1，但是不是赋值X的字段特殊处理
    IF <fs> IS NOT INITIAL.
      IF wa_field-rollname = 'BAPIUPDATE' OR wa_field-rollname = 'CHAR1' OR wa_field-rollname = 'CHAR01'.
        <fsx> = 'X'.
      ELSE.
        <fsx> = <fs>.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
