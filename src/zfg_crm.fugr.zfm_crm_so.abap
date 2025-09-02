FUNCTION zfm_crm_so.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(ACTION) TYPE  ZE_SO_ACTION DEFAULT 'S'
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(VBELN) TYPE  VBELN
*"  CHANGING
*"     VALUE(DATA) TYPE  ZSCRM_SO
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_SO'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  DATA:kunnrx     TYPE abap_bool,
       messtab    LIKE TABLE OF bdcmsgcoll,
       ext_return TYPE TABLE OF bapiret2,
       lv_msg     TYPE string.
  DATA:w_ztsd001a TYPE ztsd001a.
  CLEAR:kunnrx,messtab,ext_return,lv_msg.
  IF action NE 'S'.
    IF data IS INITIAL.
      fillmsg 'E' '请传入抬头数据后再调用本接口'.
    ENDIF.
  ELSE.
    IF data IS INITIAL OR data-items IS INITIAL.
      fillmsg 'E' '请传入抬头和明细数据后再调用本接口'.
    ENDIF.
  ENDIF.

  CLEAR:rtype,rtmsg,vbeln.

  TYPES:BEGIN OF zsvbap,
          vbeln  TYPE vbak-vbeln,
          posnr  TYPE vbap-posnr,
          matnr  TYPE vbap-matnr,
          werks  TYPE vbap-werks,
          pstyv  TYPE vbap-pstyv,
          znodel TYPE char1,
          zmatnr TYPE char1,
        END OF zsvbap.
  DATA:it_vbap TYPE TABLE OF zsvbap WITH HEADER LINE.

  DATA:sales_header_in      TYPE bapisdhd1,
       sales_header_inx     TYPE bapisdhd1x,
       order_header_in      TYPE bapisdh1,
       order_header_inx     TYPE bapisdh1x,
       salesdocument_ex     TYPE bapivbeln-vbeln,
       return               TYPE STANDARD TABLE OF bapiret2 WITH HEADER LINE,
       sales_items_in       TYPE STANDARD TABLE OF bapisditm WITH HEADER LINE,
       sales_items_inx      TYPE STANDARD TABLE OF bapisditmx WITH HEADER LINE,
       sales_partners       TYPE STANDARD TABLE OF bapiparnr WITH HEADER LINE,
       sales_partnersc      TYPE TABLE OF bapiparnrc WITH HEADER LINE,
       sales_schedules_in   TYPE STANDARD TABLE OF bapischdl WITH HEADER LINE,
       sales_schedules_inx  TYPE STANDARD TABLE OF bapischdlx WITH HEADER LINE,
       sales_conditions_in  TYPE STANDARD TABLE OF bapicond WITH HEADER LINE,
       sales_conditions_inx TYPE STANDARD TABLE OF bapicondx WITH HEADER LINE,
       extensionin          TYPE TABLE OF bapiparex WITH HEADER LINE,
       extensionex          TYPE TABLE OF bapiparex WITH HEADER LINE.
  DATA:order_header    TYPE bapisdh1,
       order_headerx   TYPE bapisdh1x,
       salesdocument   TYPE bapivbeln-vbeln,
       order_item_in   TYPE TABLE OF bapisditm  WITH HEADER LINE,
       order_item_inx  TYPE TABLE OF bapisditmx WITH HEADER LINE,
       schedule_lines  TYPE TABLE OF bapischdl  WITH HEADER LINE,
       schedule_linesx TYPE TABLE OF bapischdlx WITH HEADER LINE,
       order_text      TYPE TABLE OF bapisdtext WITH HEADER LINE,
       conditions_in   TYPE TABLE OF bapicond   WITH HEADER LINE,
       conditions_inx  TYPE TABLE OF bapicondx  WITH HEADER LINE,
       logic_switch    LIKE  bapisdls.
  DATA:updateflag TYPE updkz_d.

  DATA:BEGIN OF text_stream OCCURS 0,
         text TYPE char2048,
       END OF text_stream,
       lines TYPE TABLE OF tline WITH HEADER LINE.
  DATA:wa_extk  TYPE bape_vbak,
       wa_extkx TYPE bape_vbakx,
       wa_extp  TYPE bape_vbap,
       wa_extpx TYPE bape_vbapx,
       subrc    TYPE sy-subrc,
       mes      TYPE string.
  DATA:posnr TYPE vbap-posnr.
**********************************
  DATA:wa_head TYPE ztcrm_so_head,
       lt_item TYPE TABLE OF ztcrm_so_item.
  TYPES:BEGIN OF ty_ak,
          ak       TYPE vbak,
          bstkd    TYPE vbkd-bstkd,
          kunnr_ag TYPE kunnr,
          kunnr_we TYPE kunnr,
        END OF ty_ak.
  TYPES:BEGIN OF ty_so.
          INCLUDE TYPE ty_ak.
  TYPES:  ap TYPE TABLE OF vbap WITH DEFAULT KEY,
        END OF ty_so.
  DATA:BEGIN OF wa_check,
         head TYPE ztcrm_so_head,
         item TYPE TABLE OF ztcrm_so_item,
         so   TYPE TABLE OF ty_so,
       END OF wa_check.

  PERFORM ezsdr USING '' data-new_contractid CHANGING rtmsg.
  IF rtmsg IS NOT INITIAL.
    fillmsg 'E' rtmsg.
  ENDIF.

  PERFORM domain_value_check USING action CHANGING rtmsg.
  IF rtmsg IS NOT INITIAL.
    fillmsg 'E' rtmsg.
  ENDIF.

  "补0
  PERFORM addzero(zpubform) CHANGING data-vbeln.
  IF NOT action EQ 'D'.
    PERFORM addzero(zpubform) CHANGING data-kunnr_we.
    PERFORM addzero(zpubform) CHANGING data-kunnr_ag.

    " 数据校验  24.09.2024 15:18:51 by kkw
    checkinitial data-new_contractid       'CRM合同ID'            .
    checkinitial data-bstkd       '客户参考'            .
    checkinitial data-auart       '销售凭证类型'          .
    checkinitial data-kunnr_we    '送达方'             .
    checkinitial data-kunnr_ag    '售达方'             .
    checkinitial data-vkorg       '销售组织'            .
    checkinitial data-vtweg       '分销渠道'            .
    checkinitial data-prsdt       '凭证日期'            .
    checkinitial data-guebg       '有效期自'            .
    checkinitial data-gueen       '有效期至'            .
    checkinitial data-zhtyf       '合同月份'            .
    checkinitial data-spart       '产品组'             .
    checkinitial data-zhwlx       '货物类型'            .
    checkinitial data-vkbur       '销售办事处'           .
    checkinitial data-vkgrp       '销售组'             .
    checkinitial data-waerk       '凭证货币'            .
    checkinitial data-kursk       '汇率'              .
    checkinitial data-zterm       '付款条件'            .
    checkinitial data-zhtjgfs     '合同加工方式'          .
    checkinitial data-zdjbl       '定金比例'            .
    checkinitial data-zisdxs      '销售类型'            .
    checkinitial data-zcpyt       '产品用途'            .

    DATA(comp) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( data ) )->components.
    LOOP AT comp ASSIGNING FIELD-SYMBOL(<comp>).
      ASSIGN COMPONENT <comp>-name OF STRUCTURE data TO FIELD-SYMBOL(<data_value>).
      IF sy-subrc EQ 0.
        PERFORM domain_value_check USING <data_value> CHANGING rtmsg.
        UNASSIGN <data_value>.
        IF rtmsg IS NOT INITIAL.
          fillmsg 'E' rtmsg.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT data-items ASSIGNING FIELD-SYMBOL(<item>) GROUP BY ( new_contractdetailid = <item>-new_contractdetailid
      index = GROUP INDEX size = GROUP SIZE
       ) ASSIGNING FIELD-SYMBOL(<group>).
      IF <group>-size NE 1.
        rtmsg = |CRM合同明细ID[{ <group>-new_contractdetailid }]重复[{ <group>-size }]次，请核实明细数据|.
        fillmsg 'E' rtmsg.
      ENDIF.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<mem>).
        DATA(tabix) = sy-tabix.
        mes = '第' && tabix && '行,' && '品名'           .
        checkinitial <mem>-groes mes.
        mes = '第' && tabix && '行,' && 'CRM合同明细ID'           .
        checkinitial <mem>-new_contractdetailid mes.
*      mes = '第' && tabix && '行,' && '厚度'           .
*      checkinitial <mem>-houdu mes.
        mes = '第' && tabix && '行,' && '宽度'           .
        checkinitial <mem>-width mes.
        mes = '第' && tabix && '行,' && '材质'           .
        checkinitial <mem>-caizhi mes.
        mes = '第' && tabix && '行,' && '工厂'           .
        checkinitial <mem>-werks mes.
        mes = '第' && tabix && '行,' && '销售单位'         .
        checkinitial <mem>-vrkme mes.
        mes = '第' && tabix && '行,' && '订单数量'         .
        checkinitial <mem>-kwmeng mes.
*      mes = '第' && tabix && '行,' && '价格'           .
*      checkinitial <mem>-kbetr mes.
        mes = '第' && tabix && '行,' && '税率'           .
        checkinitial <mem>-mwskz mes.
        mes = '第' && tabix && '行,' && '厚度类型'           .
        checkinitial <mem>-houdulx mes.

        comp = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <mem> ) )->components.
        LOOP AT comp ASSIGNING <comp>.
          ASSIGN COMPONENT <comp>-name OF STRUCTURE <mem> TO <data_value>.
          IF sy-subrc EQ 0.
            PERFORM domain_value_check USING <data_value> CHANGING rtmsg.
            UNASSIGN <data_value>.
            IF rtmsg IS NOT INITIAL.
              rtmsg = |明细第{ tabix }行,{ rtmsg }|.
              fillmsg 'E' rtmsg.
            ENDIF.
          ENDIF.
        ENDLOOP.
*单位转换
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            input          = <mem>-vrkme
            language       = sy-langu
          IMPORTING
            output         = <mem>-vrkme
          EXCEPTIONS
            unit_not_found = 1
            OTHERS         = 2.
      ENDLOOP.
    ENDLOOP.

    SELECT
      ttxit~*
      FROM tvak
      JOIN ttxern ON tvak~txtgr = ttxern~txtgr
      JOIN ttxit ON ttxern~tdid = ttxit~tdid AND ttxern~tdobject = ttxit~tdobject
      WHERE tvak~auart = @data-auart
      AND ttxern~tdobject IN ( 'VBBK','VBBP' )
      AND ttxern~tdid LIKE 'Z%'
      AND ttxit~tdspras = @sy-langu
      ORDER BY ttxit~tdobject,ttxit~tdid
      INTO TABLE @DATA(lt_ttxit).

    " 校验SO的存在性用于确定增删改  21.09.2024 11:38:03 by kkw
    SELECT SINGLE
      *
      FROM ztcrm_so_head
      WHERE new_contractid = @data-new_contractid
      INTO CORRESPONDING FIELDS OF @wa_check-head
      .
    SELECT
      *
      FROM ztcrm_so_item
      WHERE new_contractid = @data-new_contractid
      INTO CORRESPONDING FIELDS OF TABLE @wa_check-item
      .
    SELECT
      vbak~vbeln,
      vbak~new_contractid,
      vbkd~bstkd,
      vbak~kunnr AS kunnr_ag,
      we~kunnr AS kunnr_we
      FROM vbak
      LEFT JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
      LEFT JOIN vbpa AS we ON vbak~vbeln = we~vbeln AND we~parvw = 'WE' AND we~posnr = '000000'
      WHERE new_contractid = @data-new_contractid
      AND vbtyp = 'G'
      INTO TABLE @DATA(lt_vbak)
      .
    LOOP AT lt_vbak ASSIGNING FIELD-SYMBOL(<lt_vbak>).
      INSERT INITIAL LINE INTO TABLE wa_check-so ASSIGNING FIELD-SYMBOL(<so>).
      MOVE-CORRESPONDING <lt_vbak> TO <so>.
      MOVE-CORRESPONDING <lt_vbak> TO <so>-ak.
      SELECT vbeln,posnr,new_contractdetailid FROM vbap WHERE vbeln = @<lt_vbak>-vbeln INTO CORRESPONDING FIELDS OF TABLE @<so>-ap.
    ENDLOOP.

    DESCRIBE TABLE wa_check-so LINES DATA(soline).
    IF soline GT 1.
      rtmsg = |CRM合同ID[{ data-new_contractid }]创建了[{ soline }]个SAP合同了，请核实SAP数据|.
      fillmsg 'E' rtmsg.
    ELSEIF soline EQ 1.
      READ TABLE wa_check-so INTO DATA(wa_so) INDEX 1.
      IF data-vbeln IS NOT INITIAL.
        IF data-vbeln NE wa_so-ak-vbeln.
          rtmsg = |CRM的合同号[{ data-vbeln }]和SAP的合同号[{ wa_so-ak-vbeln }]不一致|.
          fillmsg 'E' rtmsg.
        ENDIF.
      ELSE.
        data-vbeln = wa_so-ak-vbeln.
      ENDIF.
      IF NOT ( data-kunnr_ag = wa_so-kunnr_ag AND data-kunnr_we = wa_so-kunnr_we ).
*        rtmsg = |CRM售达方[{ data-kunnr_ag }]和SAP的售达方[{ wa_so-kunnr_ag }]或者CRM送达方[{ data-kunnr_we }]和SAP的送达方[{ wa_so-kunnr_we }]不一致|.
*        fillmsg 'E' rtmsg.
        kunnrx = abap_true.
        CLEAR:subrc,lv_msg.
        CALL FUNCTION 'ZFM_BDC_VA02_KUNNR'
          EXPORTING
*           CTU       = 'X'
*           MODE      = 'N'
*           UPDATE    = 'L'
*           GROUP     =
*           USER      =
*           KEEP      =
*           HOLDDATE  =
*           NODATA    = '/'
            vbeln_001 = CONV bdcdata-fval( data-vbeln )
*           BSTKD_002 =
            kunnr_003 = CONV bdcdata-fval( data-kunnr_ag )
            kunnr_004 = CONV bdcdata-fval( data-kunnr_we )
*           GUEBG_005 =
*           GUEEN_006 =
*           PRSDT_007 =
*           VSBED_008 =
*           BSTKD_009 =
            kunnr_010 = CONV bdcdata-fval( data-kunnr_ag )
            kunnr_011 = CONV bdcdata-fval( data-kunnr_we )
*           GUEBG_012 =
*           GUEEN_013 =
*           PRSDT_014 =
*           VSBED_015 =
          IMPORTING
            subrc     = subrc
          TABLES
            messtab   = messtab.
        IF subrc NE 0.
          rtmsg = |更改客户失败：|.
          CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
            TABLES
              imt_bdcmsgcoll = messtab
              ext_return     = ext_return.
          LOOP AT ext_return INTO DATA(wa_return) .
            CLEAR lv_msg.
            CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                msgid               = wa_return-id
                msgnr               = wa_return-number
                msgv1               = wa_return-message_v1
                msgv2               = wa_return-message_v2
                msgv3               = wa_return-message_v3
                msgv4               = wa_return-message_v4
              IMPORTING
                message_text_output = lv_msg.
            CONCATENATE rtmsg lv_msg INTO rtmsg SEPARATED BY '/'.
          ENDLOOP.
          fillmsg 'E' rtmsg.
        ENDIF.
      ENDIF.
      SELECT
        COUNT(*)
        FROM vbkd
        WHERE bstkd = @data-bstkd
        AND substring( vbeln,1,5 ) = '00400'
        AND vbeln NE @data-vbeln
        .
      IF sy-subrc EQ 0.
        rtmsg = |外部合同号[{ data-bstkd }]已存在|.
        fillmsg 'E' rtmsg.
      ENDIF.
      IF data-province IS INITIAL.
        data-province = wa_check-head-province.
      ENDIF.
      IF data-city IS INITIAL.
        data-city = wa_check-head-city.
      ENDIF.
      IF data-county IS INITIAL.
        data-county = wa_check-head-county.
      ENDIF.
    ELSE.
      SELECT
        COUNT(*)
        FROM vbkd
        WHERE bstkd = @data-bstkd
        .
      IF sy-subrc EQ 0.
        rtmsg = |外部合同号[{ data-bstkd }]已存在|.
        fillmsg 'E' rtmsg.
      ENDIF.
    ENDIF.
  ENDIF."删除操作无需校验

  CASE action.
    WHEN 'S'.
      MOVE-CORRESPONDING data TO wa_head.
      MOVE-CORRESPONDING data-items TO lt_item.

      LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<lt_item>).
        CLEAR:<lt_item>-posnr.
        <lt_item>-new_contractid = data-new_contractid.
        IF wa_check IS NOT INITIAL.
          READ TABLE wa_check-item ASSIGNING FIELD-SYMBOL(<wa_check>) WITH KEY new_contractdetailid = <lt_item>-new_contractdetailid.
          IF sy-subrc EQ 0.
            <lt_item>-posnr = |{ <wa_check>-posnr ALPHA = IN }|.
*            <lt_item>-matnr = <wa_check>-matnr.
            IF NOT ( <lt_item>-action = 'U' OR <lt_item>-action = 'D' ).
              rtmsg = |明细ID[{ <lt_item>-new_contractdetailid }]已存在，只能做修改或删除操作|.
              fillmsg 'E' rtmsg.
            ENDIF.
          ELSE.
            IF NOT <lt_item>-action = 'A'.
              rtmsg = |明细ID[{ <lt_item>-new_contractdetailid }]未存在，只能做增行操作|.
              fillmsg 'E' rtmsg.
            ENDIF.
          ENDIF.
        ELSE.
          IF NOT <lt_item>-action = 'I'.
            rtmsg = |单ID[{ data-new_contractid }]未存在，只能做创建操作|.
            fillmsg 'E' rtmsg.
          ENDIF.
*          SELECT
*            COUNT(*)
*            FROM vbkd
*            WHERE bstkd = @data-bstkd
*            .
*          IF sy-subrc = 0.
*            rtmsg = |外部合同号：[{ data-bstkd }]已创建销售订单，请确认！|.
*            fillmsg 'E' rtmsg.
*          ENDIF.
        ENDIF.
        LOOP AT lt_ttxit ASSIGNING FIELD-SYMBOL(<lt_ttxit>) WHERE tdobject = 'VBBP'.
          ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <lt_item> TO FIELD-SYMBOL(<zlongtext>).
          IF sy-subrc EQ 0.
            DATA(sapno) = |{ <lt_item>-new_contractid }{ <lt_item>-new_contractdetailid }|.
            PERFORM i_longtext USING sapno <lt_ttxit>-tdid <zlongtext>.
            CLEAR <zlongtext>.
            UNASSIGN <zlongtext>.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE wa_head TO <zlongtext>.
        IF sy-subrc EQ 0.
          PERFORM i_longtext USING wa_head-new_contractid <lt_ttxit>-tdid <zlongtext>.
          CLEAR <zlongtext>.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
      MODIFY ztcrm_so_item FROM TABLE lt_item.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      MODIFY ztcrm_so_head FROM wa_head.
      IF sy-subrc NE 0.
        subrc = 4.
      ENDIF.
      IF subrc EQ 0.
        COMMIT WORK.
        rtmsg = 'S:数据保存成功'.
        rtype = 'S'.
        SELECT SINGLE
          vbak~vbeln,
          vbak~auart
          FROM vbak
          JOIN ztsd001 ON vbak~vbeln = ztsd001~vbeln AND vbak~auart = ztsd001~auart
          WHERE vbak~new_contractid = @data-new_contractid
          AND vbak~vbtyp = 'G'
          AND ztsd001~flag = '1'
          INTO @DATA(w_ak)
          .
        IF sy-subrc EQ 0.
          UPDATE ztsd001 SET
            flag = 'A'
            WHERE vbeln = @w_ak-vbeln
            AND auart = @w_ak-auart
          .
          w_ztsd001a-vbeln = w_ak-vbeln.
          w_ztsd001a-auart = w_ak-auart.
          SELECT SINGLE MAX( item )
            INTO @w_ztsd001a-item
            FROM ztsd001a
            WHERE vbeln = @w_ak-vbeln
            AND   auart = @w_ak-auart.
          ADD 1 TO w_ztsd001a-item.
          w_ztsd001a-zname = sy-uname.
          w_ztsd001a-zdate = sy-datum.
          w_ztsd001a-ztime = sy-uzeit.
          w_ztsd001a-flag = 'A'.
          INSERT ztsd001a FROM w_ztsd001a.
          COMMIT WORK.
          rtmsg = |{ rtmsg }，合同审批状态变为初始状态|.
        ENDIF.
        zfmdatasave2 'R'.
      ELSE.
        ROLLBACK WORK.
        rtmsg = 'E:数据保存失败'.
        fillmsg rtmsg(1) rtmsg.
      ENDIF.

      RETURN.
    WHEN 'I'.
*      SELECT
*        COUNT(*)
*        FROM vbkd
*        WHERE bstkd = @data-bstkd
*        .
*      IF sy-subrc = 0.
*        rtmsg = |外部合同号：[{ data-bstkd }]已创建销售订单，请确认！|.
*        fillmsg 'E' rtmsg.
*      ENDIF.
*      SELECT
*        COUNT( DISTINCT vbeln ) AS countvn
*        FROM vbak
*        WHERE new_contractid = @data-new_contractid
*        INTO @DATA(countvn)
*      .
*      IF countvn NE 0.
*        rtmsg = |CRM合同ID[{ data-new_contractid }]存在于[{ countvn }]个合同！！！|.
*        fillmsg 'E' rtmsg.
*      ENDIF.
      IF data-items IS INITIAL.
        fillmsg 'E' '合同明细不能为空'.
      ENDIF.
      IF data-vbeln IS NOT INITIAL.
        rtmsg = |外部合同号已创建过销售合同[{ data-vbeln }]|.
        fillmsg 'E' rtmsg.
      ENDIF.
      CLEAR:sales_header_in        .
      CLEAR:sales_header_inx       .
      CLEAR:salesdocument_ex       .
      CLEAR:return                 ,    return[]               .
      CLEAR:sales_items_in         ,    sales_items_in[]       .
      CLEAR:sales_items_inx        ,    sales_items_inx[]      .
      CLEAR:sales_partners         ,    sales_partners[]       .
      CLEAR:sales_schedules_in     ,    sales_schedules_in[]   .
      CLEAR:sales_schedules_inx    ,    sales_schedules_inx[]  .
      CLEAR:sales_conditions_in    ,    sales_conditions_in[]  .
      CLEAR:sales_conditions_inx   ,    sales_conditions_inx[] .
      CLEAR:order_text             ,    order_text[]           .
      CLEAR:extensionin            ,    extensionin[]          .
      CLEAR:logic_switch.

      "抬头
*******************************************************20230214
      sales_header_in-doc_type   = data-auart ."订单类型
      sales_header_in-sales_org  = data-vkorg ."销售组织
      sales_header_in-purch_no_c = data-bstkd ."外部合同号
      sales_header_in-division   = data-spart ."产品组
      sales_header_in-sales_grp  = data-vkgrp ."销售组
      sales_header_in-distr_chan = data-vtweg ."分销渠道
      sales_header_in-sales_off  = data-vkbur ."销售办事处
      sales_header_in-currency   = data-waerk.
      sales_header_in-doc_date   = data-prsdt."凭证日期 (接收/发送日期)
      sales_header_in-incoterms1 = data-inco1."*  国际贸易条款（第 1 部分）
      sales_header_in-incoterms2 = data-inco2_l."*  国际贸易条款（第 2 部分）
      sales_header_in-price_date = data-prsdt.
      sales_header_in-ct_valid_f = data-guebg.
      sales_header_in-ct_valid_t = data-gueen.
      sales_header_in-cust_group = data-kdgrp.
      PERFORM setbapix USING sales_header_in CHANGING sales_header_inx.

*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30 = wa_extk.
      APPEND extensionin.
      CLEAR:extensionin.
      PERFORM setbapix USING wa_extk CHANGING wa_extkx.
      extensionin-structure = 'BAPE_VBAKX'.
      extensionin+30 = wa_extkx.
      APPEND extensionin.
**********************************
      "合作伙伴
      CLEAR sales_partners.
      sales_partners-partn_role = 'AG'.
      sales_partners-partn_numb = data-kunnr_ag.
      APPEND sales_partners.
      CLEAR sales_partners.
      sales_partners-partn_role = 'WE'.
      sales_partners-partn_numb = data-kunnr_we.
      APPEND sales_partners.

      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE data TO <zlongtext>.
        IF sy-subrc EQ 0.
          "切割文本
          CLEAR:text_stream,text_stream[],lines[],lines.
          text_stream-text = <zlongtext>.
          APPEND text_stream.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = text_stream
              itf_text    = lines.
          LOOP AT lines.
            CLEAR:order_text.
            order_text-text_id    = <lt_ttxit>-tdid.
            order_text-langu      = sy-langu.
            order_text-format_col = '*' .
            order_text-text_line  = lines-tdline.
            APPEND order_text.
          ENDLOOP.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
**********************************
      "行项目
      posnr = 0 .
      LOOP AT data-items TRANSPORTING NO FIELDS WHERE posnr = ''.
        EXIT.
      ENDLOOP.
      IF sy-subrc EQ 0.
        DATA(posnrx) = 'X'.
      ENDIF.
      LOOP AT data-items ASSIGNING <item> .
        CLEAR:sales_items_in,sales_items_inx,sales_schedules_in,sales_schedules_inx .
        IF posnrx = 'X'.
          ADD 10 TO posnr.
          <item>-posnr = posnr.
        ENDIF.
        sales_items_in-itm_number     = <item>-posnr.
        sales_items_in-material       = <item>-matnr.
        PERFORM addzero(zpubform) CHANGING sales_items_in-material.
        sales_items_in-target_qty     = <item>-kwmeng.
        sales_items_in-target_qu      = <item>-vrkme.
        sales_items_in-sales_unit     = <item>-vrkme.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-target_qu.
        PERFORM transunit_to_inside(zpubform) CHANGING sales_items_in-sales_unit.
        sales_items_in-plant          = <item>-werks.
        sales_items_in-currency       = data-waerk.
        PERFORM setbapix USING sales_items_in CHANGING sales_items_inx.
        APPEND:sales_items_in,sales_items_inx.
        "计划行
        sales_schedules_in-itm_number = <item>-posnr.
        sales_schedules_in-sched_line = '0001'.
        sales_schedules_in-req_qty    = <item>-kwmeng.
        sales_schedules_in-req_date   = <item>-edatu .
        PERFORM setbapix USING sales_schedules_in CHANGING sales_schedules_inx.
        APPEND: sales_schedules_in,sales_schedules_inx.

*增强字段
        CLEAR:extensionin,wa_extp.
        MOVE-CORRESPONDING <item> TO wa_extp.
        PERFORM CONCATENATEzqdhtgg USING <item> data-spart CHANGING wa_extp.
        extensionin-structure = 'BAPE_VBAP'.
        extensionin+30 = wa_extp.
        APPEND extensionin.
        CLEAR:extensionin,wa_extpx .
        PERFORM setbapix USING wa_extp CHANGING wa_extpx.
        extensionin-structure = 'BAPE_VBAPX'.
        extensionin+30 = wa_extpx.
        APPEND extensionin.

*文本
        LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
          ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <item> TO <zlongtext>.
          IF sy-subrc EQ 0.
            "切割文本
            CLEAR:text_stream,text_stream[],lines[],lines.
            text_stream-text = <zlongtext>.
            APPEND text_stream.
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              TABLES
                text_stream = text_stream
                itf_text    = lines.
            LOOP AT lines.
              CLEAR:order_text.
              order_text-itm_number = <item>-posnr.
              order_text-text_id    = <lt_ttxit>-tdid.
              order_text-langu      = sy-langu .
              order_text-format_col = '*' .
              order_text-text_line  = lines-tdline.
              APPEND order_text.
            ENDLOOP.
            UNASSIGN <zlongtext>.
          ENDIF.
        ENDLOOP.
        CASE data-auart.
          WHEN 'ZCQ1' OR 'ZCQ3' OR 'ZCQ4'.
            CLEAR:sales_conditions_in,sales_conditions_inx.
            sales_conditions_in-itm_number = <item>-posnr.
            sales_conditions_in-cond_type  = 'ZPR0'.
            sales_conditions_in-cond_value = <item>-kbetr.
            sales_conditions_in-currency   = data-waerk.
            PERFORM setbapix USING sales_conditions_in CHANGING sales_conditions_inx.
            APPEND:sales_conditions_in,sales_conditions_inx.
          WHEN 'ZCQ2'.
            CLEAR:sales_conditions_in,sales_conditions_inx.
            sales_conditions_in-itm_number = <item>-posnr.
            sales_conditions_in-cond_type  = 'ZP02'.
            sales_conditions_in-currency   = data-waerk.
            SELECT SINGLE * FROM mara WHERE matnr = @<item>-matnr INTO @DATA(wa_mara).
            SELECT SINGLE
              konp~kbetr
              FROM a901
              JOIN konp ON konp~knumh = a901~knumh AND konp~kschl = 'ZP02'
              WHERE a901~vkorg = @data-vkorg
              AND a901~kunwe = @data-kunnr_we
              AND a901~spart = @data-spart
              AND a901~caizhi = @wa_mara-caizhi
              AND a901~houdu = @wa_mara-houdu
              AND a901~width = @wa_mara-width
              AND a901~chandi = @wa_mara-chandi
              AND a901~datab LE @data-prsdt
              AND a901~datbi GE @data-prsdt
              INTO @sales_conditions_in-cond_value
              .
            IF sy-subrc NE 0.
              SELECT SINGLE
                konda
                FROM knvv
                WHERE kunnr = @data-kunnr_ag
                AND vkorg = @data-vkorg
                AND vtweg = @data-vtweg
                AND spart = '00'
                INTO @DATA(konda)
                .
              SELECT SINGLE
                konp~kbetr
                FROM a900
                JOIN konp ON konp~knumh = a900~knumh AND konp~kschl = 'ZP02'
                WHERE a900~vkorg = @data-vkorg
                AND a900~konda = @konda
                AND a900~spart = @data-spart
                AND a900~caizhi = @wa_mara-caizhi
                AND a900~houdu = @wa_mara-houdu
                AND a900~width = @wa_mara-width
                AND a900~datab LE @data-prsdt
                AND a900~datbi GE @data-prsdt
                INTO @sales_conditions_in-cond_value
                .
              IF sy-subrc NE 0.
                SELECT SINGLE
                  konp~kbetr
                  FROM a905
                  JOIN konp ON konp~knumh = a905~knumh AND konp~kschl = 'ZP02'
                  WHERE a905~vkorg = @data-vkorg
                  AND a905~spart = @data-spart
                  AND a905~datab LE @data-prsdt
                  AND a905~datbi GE @data-prsdt
                  INTO @sales_conditions_in-cond_value
                  .
              ENDIF.
            ENDIF.
            PERFORM setbapix USING sales_conditions_in CHANGING sales_conditions_inx.
            sales_conditions_inx-updateflag = 'I'.
            IF sales_conditions_in-cond_value NE 0.
              APPEND:sales_conditions_in,sales_conditions_inx.
            ENDIF.

            CLEAR:sales_conditions_in,sales_conditions_inx.
            sales_conditions_in-itm_number = <item>-posnr.
            sales_conditions_in-cond_type  = 'ZP01'.
            sales_conditions_in-currency   = data-waerk.
            SELECT SINGLE
              konp~kbetr
              FROM a903
              JOIN konp ON konp~knumh = a903~knumh AND konp~kschl = 'ZP01'
              WHERE a903~vkorg = @data-vkorg
              AND a903~spart = @data-spart
              AND a903~caizhi = @wa_mara-caizhi
              AND a903~houdu = @wa_mara-houdu
              AND a903~width = @wa_mara-width
              AND a903~chandi = @wa_mara-chandi
              AND a903~datab LE @data-prsdt
              AND a903~datbi GE @data-prsdt
              INTO @sales_conditions_in-cond_value
              .
            IF sy-subrc NE 0.
              SELECT SINGLE
                konp~kbetr
                FROM a902
                JOIN konp ON konp~knumh = a902~knumh AND konp~kschl = 'ZP01'
                WHERE a902~vkorg = @data-vkorg
                AND a902~spart = @data-spart
                AND a902~width = @wa_mara-width
                AND a902~datab LE @data-prsdt
                AND a902~datbi GE @data-prsdt
                INTO @sales_conditions_in-cond_value
                .
              IF sy-subrc NE 0.
                SELECT SINGLE
                  konp~kbetr
                  FROM a350
                  JOIN konp ON konp~knumh = a350~knumh AND konp~kschl = 'ZP01'
                  WHERE a350~vkorg = @data-vkorg
                  AND a350~datab LE @data-prsdt
                  AND a350~datbi GE @data-prsdt
                  INTO @sales_conditions_in-cond_value
                  .
              ENDIF.
            ENDIF.
            PERFORM setbapix USING sales_conditions_in CHANGING sales_conditions_inx.
            sales_conditions_inx-updateflag = 'I'.
            IF sales_conditions_in-cond_value NE 0.
              APPEND:sales_conditions_in,sales_conditions_inx.
            ENDIF.
        ENDCASE.
      ENDLOOP.
**********************************
      logic_switch-pricing = 'G'.
      SET UPDATE TASK LOCAL.
      CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
        EXPORTING
          sales_header_in      = sales_header_in
          sales_header_inx     = sales_header_inx
*         logic_switch         = logic_switch
        IMPORTING
          salesdocument_ex     = salesdocument_ex
        TABLES
          return               = return
          sales_items_in       = sales_items_in
          sales_items_inx      = sales_items_inx
          sales_partners       = sales_partners
          sales_schedules_in   = sales_schedules_in
          sales_schedules_inx  = sales_schedules_inx
          sales_conditions_in  = sales_conditions_in
          sales_conditions_inx = sales_conditions_inx
          sales_text           = order_text
          extensionin          = extensionin
        EXCEPTIONS
          OTHERS               = 1.

      IF sy-subrc NE 0.
        ADD sy-subrc TO subrc.
      ENDIF.
      CLEAR lv_msg.
      LOOP AT return WHERE type CA 'AEX'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = return-id
            msgnr               = return-number
            msgv1               = return-message_v1
            msgv2               = return-message_v2
            msgv3               = return-message_v3
            msgv4               = return-message_v4
          IMPORTING
            message_text_output = lv_msg.
        CONCATENATE lv_msg rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc EQ 0.
        ADD 4 TO subrc.
      ENDIF.

      IF subrc = 0 AND salesdocument_ex IS NOT INITIAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtype = 'S'.
        rtmsg = |创建销售合同成功，单号：[{ salesdocument_ex }]|.
        vbeln = salesdocument_ex.
        data-vbeln = vbeln.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'U'.
      CLEAR:salesdocument,order_item_in,order_item_inx,schedule_lines,schedule_linesx,updateflag,logic_switch,
      order_header,order_headerx,order_text,conditions_in,conditions_inx,subrc,sales_partners,sales_partnersc.
      REFRESH:
      order_item_in,order_item_inx,schedule_lines,schedule_linesx,
      order_text,conditions_in,conditions_inx,sales_partners,sales_partnersc.
      "补0
      PERFORM addzero(zpubform) CHANGING data-vbeln.
      SELECT
        vbak~vbeln,
        vbap~posnr,
        vbap~matnr,
        vbap~werks,
        vbap~pstyv
        FROM vbak
        JOIN vbap ON vbak~vbeln = vbap~vbeln
        WHERE vbak~vbeln = @data-vbeln
        AND vbak~vbtyp = 'G' "合同
        ORDER BY vbak~vbeln,vbap~posnr
        INTO CORRESPONDING FIELDS OF TABLE @it_vbap
        .
      IF sy-subrc NE 0.
        rtmsg = |合同{ data-vbeln }不存在|.
        fillmsg 'E' rtmsg.
      ENDIF.
      "抬头
      salesdocument = data-vbeln.

      setbapix 'order_header' 'sales_org ' data-vkorg  .
      setbapix 'order_header' 'purch_no_c' data-bstkd  .
      setbapix 'order_header' 'division  ' data-spart  .
      setbapix 'order_header' 'sales_grp ' data-vkgrp  .
      setbapix 'order_header' 'distr_chan' data-vtweg  .
      setbapix 'order_header' 'sales_off ' data-vkbur  .
      setbapix 'order_header' 'currency  ' data-waerk  .
      setbapix 'order_header' 'doc_date  ' data-prsdt  .
      IF data-inco1 IS NOT INITIAL.
        setbapix 'order_header' 'incoterms1' data-inco1  .
      ENDIF.
      IF data-inco2_l IS NOT INITIAL.
        setbapix 'order_header' 'incoterms2' data-inco2_l.
      ENDIF.
      setbapix 'order_header' 'price_date' data-prsdt  .
      setbapix 'order_header' 'ct_valid_f' data-guebg  .
      setbapix 'order_header' 'ct_valid_t' data-gueen  .
      setbapix 'order_header' 'cust_group' data-kdgrp  .
      order_headerx-updateflag = 'U'.

*增强字段
      CLEAR wa_extk.
      MOVE-CORRESPONDING data TO wa_extk.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAK'.
      extensionin+30 = wa_extk.
      APPEND extensionin.
      CLEAR:extensionin.
      extensionin-structure = 'BAPE_VBAKX'.
      DATA(componentsVBAKX) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'BAPE_VBAK' ) )->get_components( ).
      LOOP AT componentsVBAKX INTO DATA(wa_componentsVBAKX) WHERE as_include = 'X'.
        DATA(components) = CAST cl_abap_structdescr( wa_componentsVBAKX-type )->components.
        LOOP AT components ASSIGNING FIELD-SYMBOL(<components>).
          DATA(key) = |WA_EXTKX-{ <components>-name }|.
          ASSIGN (key) TO FIELD-SYMBOL(<valuek>).
          IF sy-subrc EQ 0.
            <valuek> = 'X'.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      wa_extkx-vbeln = data-vbeln.
      extensionin+30 = wa_extkx.
      APPEND extensionin.

      LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBK'.
        ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE data TO <zlongtext>.
        IF sy-subrc EQ 0.
          "切割文本
          CLEAR:text_stream,text_stream[],lines[],lines.
          text_stream-text = <zlongtext>.
          APPEND text_stream.
          CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
            TABLES
              text_stream = text_stream
              itf_text    = lines.
          LOOP AT lines.
            CLEAR:order_text.
            order_text-doc_number = data-vbeln.
            order_text-text_id    = <lt_ttxit>-tdid.
            order_text-langu      = sy-langu.
            order_text-format_col = '*' .
            order_text-text_line  = lines-tdline.
            APPEND order_text.
          ENDLOOP.
          UNASSIGN <zlongtext>.
        ENDIF.
      ENDLOOP.
**********************************
      "合作伙伴
***      CLEAR:sales_partners,sales_partnersc.
***      sales_partners-partn_role = 'AG'.
***      sales_partners-partn_numb = data-kunnr_ag.
***
***      sales_partnersc-document      = data-vbeln.
****      sales_partnersc-itm_number    = '000000'.
***      sales_partnersc-updateflag    = 'U'.
***      sales_partnersc-partn_role    = 'AG'.
****      sales_partnersc-p_numb_old    =  ''.
***      SELECT SINGLE kunnr,posnr FROM vbpa WHERE vbeln = @data-vbeln AND posnr = '000000' AND parvw = 'AG' INTO ( @sales_partnersc-p_numb_old,@sales_partnersc-itm_number ).
***      sales_partnersc-p_numb_new    =  data-kunnr_ag.
***      APPEND:sales_partners,sales_partnersc.
***      CLEAR:sales_partners,sales_partnersc.
***      sales_partners-partn_role = 'WE'.
***      sales_partners-partn_numb = data-kunnr_we.
***
***      sales_partnersc-document      = data-vbeln.
****      sales_partnersc-itm_number    =  '000000'.
***      sales_partnersc-updateflag    =  'U'.
***      sales_partnersc-partn_role    =  'WE'.
****      sales_partnersc-p_numb_old    =  ''.
***      SELECT SINGLE kunnr,posnr FROM vbpa WHERE vbeln = @data-vbeln AND posnr = '000000' AND parvw = 'WE' INTO ( @sales_partnersc-p_numb_old,@sales_partnersc-itm_number ).
***      sales_partnersc-p_numb_new    =  data-kunnr_we.
***      APPEND:sales_partners,sales_partnersc.
**********************************
      "行项目
      "新增行取最大行项目
      SELECT MAX( vbap~posnr )
        INTO posnr
        FROM vbap
        WHERE vbeln = data-vbeln
        .
      SELECT SINGLE
        vbak~*
        INTO @DATA(vbak)
        FROM vbak
        WHERE vbak~vbeln = @data-vbeln.
      LOOP AT data-items ASSIGNING <item> .
        CLEAR:order_item_in, order_item_inx, schedule_lines,schedule_linesx,wa_extp,wa_extpx.
        READ TABLE it_vbap WITH KEY vbeln = data-vbeln posnr = <item>-posnr BINARY SEARCH.
        IF sy-subrc = 0 .
          updateflag = 'U'.
          tabix = sy-tabix.
          "更新未被删除标志
          it_vbap-znodel = 'X'.
          MODIFY it_vbap INDEX tabix TRANSPORTING znodel.
          CLEAR:conditions_in,conditions_inx.
          IF data-auart = 'ZCQ1' OR data-auart = 'ZCQ3' OR data-auart = 'ZCQ4'.
            SELECT *
              FROM prcd_elements AS prcd
              WHERE knumv = @vbak-knumv
              AND kposn = @it_vbap-posnr
              AND kinak = ''
              AND kschl = 'ZPR0'
              INTO TABLE @DATA(lt_prcd)
              .
            LOOP AT lt_prcd ASSIGNING FIELD-SYMBOL(<lt_prcd>).
              CLEAR:conditions_in,conditions_inx.
              setbapix 'conditions_in' 'itm_number' it_vbap-posnr.
              setbapix 'conditions_in' 'cond_st_no' <lt_prcd>-stunr.
              setbapix 'conditions_in' 'cond_count' <lt_prcd>-zaehk.
              setbapix 'conditions_in' 'cond_type ' <lt_prcd>-kschl.
              setbapix 'conditions_in' 'cond_value' <item>-kbetr.
*              setbapix 'conditions_in' 'currency  ' vbak-waerk.
              setbapix 'conditions_in' 'currency  ' data-waerk.
              setbapix 'conditions_in' 'cond_p_unt' <lt_prcd>-kpein.
              setbapix 'conditions_in' 'cond_unit ' <lt_prcd>-kmein.

              conditions_inx-itm_number = it_vbap-posnr.
              conditions_inx-cond_st_no = <lt_prcd>-stunr.
              conditions_inx-cond_count = <lt_prcd>-zaehk.
              conditions_inx-cond_type  = <lt_prcd>-kschl.
              IF <lt_prcd>-kinak = ''.
                conditions_inx-updateflag = 'U'.
              ELSEIF <lt_prcd>-kinak = 'M'.
                CONTINUE.
              ELSE.
                conditions_inx-updateflag = 'D'.
              ENDIF.
              APPEND:conditions_in,conditions_inx.
            ENDLOOP.
          ENDIF.
        ELSE.
          updateflag = 'I'.
          IF <item>-posnr IS INITIAL.
            ADD 10 TO posnr.
            <item>-posnr = posnr.
          ENDIF.

          "价格
          CASE data-auart.
            WHEN 'ZCQ1' OR 'ZCQ3' OR 'ZCQ4'.
              CLEAR:conditions_in,conditions_inx.
              conditions_in-itm_number = <item>-posnr.
              conditions_in-cond_type  = 'ZPR0'.
              conditions_in-cond_value = <item>-kbetr.
              conditions_in-currency   = data-waerk.
              PERFORM setbapix USING conditions_in CHANGING conditions_inx.
              conditions_inx-updateflag = 'I'.
              APPEND:conditions_in,conditions_inx.
            WHEN 'ZCQ2'.
              CLEAR:conditions_in,conditions_inx.
              conditions_in-itm_number = <item>-posnr.
              conditions_in-cond_type  = 'ZP02'.
              conditions_in-currency   = data-waerk.
              SELECT SINGLE * FROM mara WHERE matnr = @<item>-matnr INTO @wa_mara.
              SELECT SINGLE
                konp~kbetr
                FROM a901
                JOIN konp ON konp~knumh = a901~knumh AND konp~kschl = 'ZP02'
                WHERE a901~vkorg = @data-vkorg
                AND a901~kunwe = @data-kunnr_we
                AND a901~spart = @data-spart
                AND a901~caizhi = @wa_mara-caizhi
                AND a901~houdu = @wa_mara-houdu
                AND a901~width = @wa_mara-width
                AND a901~chandi = @wa_mara-chandi
                AND a901~datab LE @data-prsdt
                AND a901~datbi GE @data-prsdt
                INTO @conditions_in-cond_value
                .
              IF sy-subrc NE 0.
                SELECT SINGLE
                  konda
                  FROM knvv
                  WHERE kunnr = @data-kunnr_ag
                  AND vkorg = @data-vkorg
                  AND vtweg = @data-vtweg
                  AND spart = '00'
                  INTO @konda
                  .
                SELECT SINGLE
                  konp~kbetr
                  FROM a900
                  JOIN konp ON konp~knumh = a900~knumh AND konp~kschl = 'ZP02'
                  WHERE a900~vkorg = @data-vkorg
                  AND a900~konda = @konda
                  AND a900~spart = @data-spart
                  AND a900~caizhi = @wa_mara-caizhi
                  AND a900~houdu = @wa_mara-houdu
                  AND a900~width = @wa_mara-width
                  AND a900~datab LE @data-prsdt
                  AND a900~datbi GE @data-prsdt
                  INTO @conditions_in-cond_value
                  .
                IF sy-subrc NE 0.
                  SELECT SINGLE
                    konp~kbetr
                    FROM a905
                    JOIN konp ON konp~knumh = a905~knumh AND konp~kschl = 'ZP02'
                    WHERE a905~vkorg = @data-vkorg
                    AND a905~spart = @data-spart
                    AND a905~datab LE @data-prsdt
                    AND a905~datbi GE @data-prsdt
                    INTO @conditions_in-cond_value
                    .
                ENDIF.
              ENDIF.
              PERFORM setbapix USING conditions_in CHANGING conditions_inx.
              conditions_inx-updateflag = 'I'.
              IF conditions_in-cond_value NE 0.
                APPEND:conditions_in,conditions_inx.
              ENDIF.

              CLEAR:conditions_in,conditions_inx.
              conditions_in-itm_number = <item>-posnr.
              conditions_in-cond_type  = 'ZP01'.
              conditions_in-currency   = data-waerk.
              SELECT SINGLE
                konp~kbetr
                FROM a903
                JOIN konp ON konp~knumh = a903~knumh AND konp~kschl = 'ZP01'
                WHERE a903~vkorg = @data-vkorg
                AND a903~spart = @data-spart
                AND a903~caizhi = @wa_mara-caizhi
                AND a903~houdu = @wa_mara-houdu
                AND a903~width = @wa_mara-width
                AND a903~chandi = @wa_mara-chandi
                AND a903~datab LE @data-prsdt
                AND a903~datbi GE @data-prsdt
                INTO @conditions_in-cond_value
                .
              IF sy-subrc NE 0.
                SELECT SINGLE
                  konp~kbetr
                  FROM a902
                  JOIN konp ON konp~knumh = a902~knumh AND konp~kschl = 'ZP01'
                  WHERE a902~vkorg = @data-vkorg
                  AND a902~spart = @data-spart
                  AND a902~width = @wa_mara-width
                  AND a902~datab LE @data-prsdt
                  AND a902~datbi GE @data-prsdt
                  INTO @conditions_in-cond_value
                  .
                IF sy-subrc NE 0.
                  SELECT SINGLE
                    konp~kbetr
                    FROM a350
                    JOIN konp ON konp~knumh = a350~knumh AND konp~kschl = 'ZP01'
                    WHERE a350~vkorg = @data-vkorg
                    AND a350~datab LE @data-prsdt
                    AND a350~datbi GE @data-prsdt
                    INTO @conditions_in-cond_value
                    .
                ENDIF.
              ENDIF.
              PERFORM setbapix USING conditions_in CHANGING conditions_inx.
              conditions_inx-updateflag = 'I'.
              IF conditions_in-cond_value NE 0.
                APPEND:conditions_in,conditions_inx.
              ENDIF.
          ENDCASE.
        ENDIF.
        setbapix 'order_item_in' 'itm_number' <item>-posnr.
        setbapix 'order_item_in' 'material  ' <item>-matnr.
        setbapix 'order_item_in' 'target_qty' <item>-kwmeng.
        setbapix 'order_item_in' 'target_qu ' <item>-vrkme.
        setbapix 'order_item_in' 'sales_unit' <item>-vrkme.
        setbapix 'order_item_in' 'plant     ' <item>-werks.
        setbapix 'order_item_in' 'currency  ' data-waerk.
        order_item_inx-itm_number = <item>-posnr.
        order_item_inx-updateflag = updateflag.
        APPEND:order_item_in,order_item_inx.
        "计划行
        setbapix 'schedule_lines' 'itm_number' <item>-posnr.
        setbapix 'schedule_lines' 'sched_line' '0001'.
        setbapix 'schedule_lines' 'req_qty   ' <item>-kwmeng.
        setbapix 'schedule_lines' 'req_date  ' <item>-edatu .
        schedule_linesx-itm_number = <item>-posnr.
        schedule_linesx-sched_line = '0001'.
        schedule_linesx-updateflag = updateflag.
        APPEND:schedule_lines,schedule_linesx.
*文本
        LOOP AT lt_ttxit ASSIGNING <lt_ttxit> WHERE tdobject = 'VBBP'.
          ASSIGN COMPONENT <lt_ttxit>-tdid OF STRUCTURE <item> TO <zlongtext>.
          IF sy-subrc EQ 0.
            "切割文本
            CLEAR:text_stream,text_stream[],lines[],lines.
            text_stream-text = <zlongtext>.
            APPEND text_stream.
            CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
              TABLES
                text_stream = text_stream
                itf_text    = lines.
            LOOP AT lines.
              CLEAR:order_text.
              order_text-doc_number = data-vbeln.
              order_text-itm_number = <item>-posnr.
              order_text-text_id    = <lt_ttxit>-tdid.
              order_text-langu      = sy-langu .
              order_text-format_col = '*' .
              order_text-text_line  = lines-tdline.
              APPEND order_text.
            ENDLOOP.
            UNASSIGN <zlongtext>.
          ENDIF.
        ENDLOOP.
*增强字段
        CLEAR:extensionin,wa_extp.
        MOVE-CORRESPONDING <item> TO wa_extp.
        PERFORM CONCATENATEzqdhtgg USING <item> data-spart CHANGING wa_extp.
        extensionin-structure = 'BAPE_VBAP'.
        extensionin+30 = wa_extp.
        APPEND extensionin.
        CLEAR:extensionin,wa_extpx .
        extensionin-structure = 'BAPE_VBAPX'.
        DATA(componentsVBAPX) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_name( 'BAPE_VBAP' ) )->get_components( ).
        LOOP AT componentsVBAPX INTO DATA(wa_componentsVBAPX) WHERE as_include = 'X'.
          components = CAST cl_abap_structdescr( wa_componentsVBAPX-type )->components.
          LOOP AT components ASSIGNING <components>.
            key = |WA_EXTPX-{ <components>-name }|.
            ASSIGN (key) TO FIELD-SYMBOL(<valuep>).
            IF sy-subrc EQ 0.
              <valuep> = 'X'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
        wa_extpx-vbeln = data-vbeln.
        wa_extpx-posnr = <item>-posnr.
        extensionin+30 = wa_extpx.
        APPEND extensionin.
      ENDLOOP.

      LOOP AT it_vbap WHERE znodel = ''.
        CLEAR:order_item_in,order_item_inx.
        order_item_in-itm_number  = it_vbap-posnr.
        PERFORM setbapix USING order_item_in CHANGING order_item_inx.
        order_item_inx-updateflag  = 'D'.
        APPEND:order_item_in,order_item_inx.
      ENDLOOP.
      SET UPDATE TASK LOCAL.
      logic_switch-pricing = 'G'.
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header
          order_header_inx = order_headerx
          logic_switch     = logic_switch
        TABLES
          return           = return
          order_item_in    = order_item_in
          order_item_inx   = order_item_inx
*         partners         = sales_partners
*         partnerchanges   = sales_partnersc
          schedule_lines   = schedule_lines
          schedule_linesx  = schedule_linesx
          order_text       = order_text
          conditions_in    = conditions_in
          conditions_inx   = conditions_inx
          extensionin      = extensionin
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        ADD sy-subrc TO subrc.
      ENDIF.
      CLEAR lv_msg.
      LOOP AT return WHERE type CA 'AEX'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = return-id
            msgnr               = return-number
            msgv1               = return-message_v1
            msgv2               = return-message_v2
            msgv3               = return-message_v3
            msgv4               = return-message_v4
          IMPORTING
            message_text_output = lv_msg.
        CONCATENATE lv_msg rtmsg INTO rtmsg SEPARATED BY '/'.
      ENDLOOP.
      IF sy-subrc NE 0 AND subrc EQ 0.
        SET UPDATE TASK LOCAL.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtmsg = |更改销售合同成功，单号：[{ salesdocument }]|.
        vbeln = salesdocument.
*& kkw BDC更改售、送达方
*        IF kunnrx = abap_true.
*          CLEAR subrc.
*          CALL FUNCTION 'ZFM_BDC_VA02_KUNNR'
*            EXPORTING
**             CTU       = 'X'
**             MODE      = 'N'
**             UPDATE    = 'L'
**             GROUP     =
**             USER      =
**             KEEP      =
**             HOLDDATE  =
**             NODATA    = '/'
*              vbeln_001 = CONV bdcdata-fval( data-vbeln )
**             BSTKD_002 =
*              kunnr_003 = CONV bdcdata-fval( data-kunnr_ag )
*              kunnr_004 = CONV bdcdata-fval( data-kunnr_we )
**             GUEBG_005 =
**             GUEEN_006 =
**             PRSDT_007 =
**             VSBED_008 =
**             BSTKD_009 =
*              kunnr_010 = CONV bdcdata-fval( data-kunnr_ag )
*              kunnr_011 = CONV bdcdata-fval( data-kunnr_we )
**             GUEBG_012 =
**             GUEEN_013 =
**             PRSDT_014 =
**             VSBED_015 =
*            IMPORTING
*              subrc     = subrc
*            TABLES
*              messtab   = messtab.
*          IF subrc NE 0.
*            rtmsg = |{ rtmsg },更改客户失败：|.
*            CALL FUNCTION 'CONVERT_BDCMSGCOLL_TO_BAPIRET2'
*              TABLES
*                imt_bdcmsgcoll = messtab
*                ext_return     = ext_return.
*            LOOP AT ext_return INTO DATA(wa_return) ."WHERE type CA 'AEX' OR ( type = 'S' AND number  = '344' )
*              "OR ( type = 'S' AND id(1)  = 'Z' ).
*              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*                EXPORTING
*                  msgid               = wa_return-id
*                  msgnr               = wa_return-number
*                  msgv1               = wa_return-message_v1
*                  msgv2               = wa_return-message_v2
*                  msgv3               = wa_return-message_v3
*                  msgv4               = wa_return-message_v4
*                IMPORTING
*                  message_text_output = lv_msg.
*              CONCATENATE rtmsg lv_msg INTO rtmsg SEPARATED BY '/'.
*            ENDLOOP.
*            fillmsg 'E' rtmsg.
*        ELSE.
*          rtmsg = |{ rtmsg },更改客户成功|.
*        ENDIF.
*        ENDIF.
*& End  BDC更改售、送达方 18.02.2025 13:32:34
        IF kunnrx = abap_true.
          rtmsg = |{ rtmsg },更改客户成功|.
        ENDIF.
        fillmsg 'S' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform) USING 'E'.
        IF kunnrx = abap_true.
          rtmsg = |{ rtmsg },更改客户成功|.
        ENDIF.
        fillmsg 'E' rtmsg.
      ENDIF.
    WHEN 'D'.
      SELECT SINGLE
        vbak~vbeln
        INTO @vbeln
        FROM vbak
        JOIN vbkd ON vbak~vbeln = vbkd~vbeln AND vbkd~posnr = '000000'
        WHERE vbak~vbeln = @data-vbeln
        AND vbkd~bstkd = @data-bstkd
        .
      IF sy-subrc <> 0.
        fillmsg 'E' '未查到合同！' .
      ENDIF.
      salesdocument = vbeln.
      CLEAR:order_header_in, order_header_inx.
      order_header_in-collect_no = data-vbeln.
      order_header_inx-collect_no = data-vbeln.
      order_header_inx-updateflag  = 'D'.

*调用BAPI_SALESORDER_CHANGE bapi
      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
        EXPORTING
          salesdocument    = salesdocument
          order_header_in  = order_header_in
          order_header_inx = order_header_inx
        TABLES
          return           = return
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc NE 0.
        ADD sy-subrc TO subrc.
      ENDIF.
      LOOP AT return WHERE type CA 'AEX'.
        CLEAR lv_msg.
        LOOP AT return WHERE type CA 'AEX'.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              msgid               = return-id
              msgnr               = return-number
              msgv1               = return-message_v1
              msgv2               = return-message_v2
              msgv3               = return-message_v3
              msgv4               = return-message_v4
            IMPORTING
              message_text_output = lv_msg.
          CONCATENATE lv_msg rtmsg INTO rtmsg SEPARATED BY '/'.
        ENDLOOP.
      ENDLOOP.
      IF sy-subrc = 0 OR subrc NE 0.
        PERFORM bapirun(zpubform) USING 'E'.
        fillmsg 'E' rtmsg.
      ELSE.
        PERFORM bapirun(zpubform)  USING 'S'.
        rtmsg = |删除销售合同成功，单号：[{ salesdocument }]|.
        fillmsg 'S' rtmsg.
      ENDIF.
    WHEN OTHERS.
      rtmsg = '不支持保存、新增、更新、删除以外其他功能'.
      fillmsg 'E' rtmsg.
  ENDCASE.


  zfmdatasave2 'R'.


ENDFUNCTION.
