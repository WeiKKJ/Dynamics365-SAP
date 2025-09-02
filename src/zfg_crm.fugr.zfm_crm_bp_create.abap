FUNCTION zfm_crm_bp_create.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(DATA) TYPE  ZSCRM_BP
*"     VALUE(QCC) TYPE  CHAR1 OPTIONAL
*"  EXPORTING
*"     VALUE(RTYPE) TYPE  BAPI_MTYPE
*"     VALUE(RTMSG) TYPE  BAPI_MSG
*"     VALUE(KUNNR) TYPE  KUNNR
*"     VALUE(MDMBP) TYPE  BU_BPEXT
*"----------------------------------------------------------------------
  zfmdatasave1 'ZFM_CRM_BP_CREATE'.
  zfmdatasave2 'B'.
  COMMIT WORK.
  IF data IS INITIAL.
    fillmsg 'E' '请传入DATA数据后再调用本接口'.
  ENDIF.
  DATA:errcode   TYPE posnr,
       errmsg    TYPE bapi_msg,
       lt_qcctab TYPE TABLE OF zssd_001.
  IF qcc = 'X'.
    CALL FUNCTION 'ZFM_SD_GETQCC'
      EXPORTING
        p_name  = data-name1
      IMPORTING
        errcode = errcode
        rtmsg   = errmsg
      TABLES
        outtab  = lt_qcctab.
    LOOP AT lt_qcctab TRANSPORTING NO FIELDS WHERE creditcode = data-taxnumxl.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      rtmsg = |天眼查校验客户：{ data-name1 }失败|.
      fillmsg 'E' rtmsg.
    ENDIF.
  ENDIF.

  CLEAR:ls_partner          ,
        ls_customer         ,
        lt_addresses        ,
        ls_addresses        ,
        lt_phone            ,
        ls_phone            ,
        lt_remarks_a        ,
        ls_remarks_a        ,
        lt_bank             ,
        ls_bank             ,
        it_taxnumbers       ,
        wa_taxnumbers       ,
        lt_tax_ind          ,
        ls_tax_ind          ,
        ls_header           ,
        ls_central_data     ,
        ls_company_data     ,
        lt_company          ,
        ls_company          ,
        ls_sales_data       ,
        lt_sales            ,
        ls_sales            ,
        ls_data             ,
        lt_data             ,
        lt_return           ,
        ls_return           ,
        ls_msg              ,
        lt_partnerguid_list ,
        ls_partnerguid_list ,
        lt_smtp             ,
        ls_smtp             ,
        lt_roles            ,
        ls_roles            .
  CLEAR:mdm_intab,mdm_intab[],mdm_outtab,mdm_outtab[],mdm_rtype,mdm_rtmsg.

  DATA:lv_msg TYPE string.
  DATA:lv_msg1 TYPE string.
  DATA:lv_bpartnerguid TYPE bu_partner_guid_bapi.
  DATA:lv_bpartnerguid1 TYPE bu_partner_guid_bapi.
  DATA:lv_bpartnerguid2 TYPE bu_partner_guid_bapi.
  DATA:lt_return_frg TYPE TABLE OF bapiret2 WITH HEADER LINE.

  DATA:lt_return_ukmbp TYPE ukm_t_monitor_return .
  CLEAR:rtype,rtmsg,kunnr,mdmbp.
  "校验系统中是否已存在当前税号，如果存在，则报错修改
  SELECT
    COUNT(*)
    FROM dfkkbptaxnum
    WHERE taxnumxl = data-taxnumxl
    OR taxnum = data-taxnumxl.
  IF sy-subrc EQ 0.
    rtmsg = |系统中已存在导入的税号，客户号：{ data-taxnumxl }请修改!|.
    fillmsg 'E' rtmsg.
  ENDIF.

  CLEAR lv_bpartnerguid.
  TRY .
      lv_bpartnerguid = cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ).
    CATCH cx_root INTO DATA(exc).
      rtmsg = |创建UUID发生了异常：{ exc->get_text( ) }请修改!|.
      fillmsg 'E' rtmsg.
  ENDTRY.
  "开始
  CHECK lv_bpartnerguid IS NOT INITIAL.
  CLEAR:ls_partner.
  ls_partner-header-object_task = 'I'.
  ls_partner-header-object_instance-bpartnerguid = lv_bpartnerguid.
*  ls_partner-header-object_instance-bpartner = <fs_data>-partner ."客户号
  ls_partner-central_data-common-data-bp_control-category = 2.                 "业务伙伴类别
  ls_partner-central_data-common-data-bp_control-grouping = data-ktokd.  " 分组


  ls_partner-central_data-common-data-bp_organization-name1 = data-name1. " 名称1
  ls_partner-central_data-common-data-bp_organization-name2 = data-name1.  " 名称2
  ls_partner-central_data-common-data-bp_organization-name4 = data-name_org4.  " 法人
  ls_partner-central_data-taxnumber-common-data-nat_person = data-natpers.

  ls_partner-central_data-common-data-bp_organization-foundationdate = data-found_dat.   "组织成立日期
  ls_partner-central_data-common-data-bp_centraldata-searchterm1 = data-sortl. "业务伙伴的搜索词 1（简称）
  ls_partner-central_data-common-data-bp_centraldata-partnerlanguage = sy-langu.

  ls_partner-central_data-common-datax-bp_organization-name1 = abap_true.
  ls_partner-central_data-common-datax-bp_organization-name2 = abap_true.
  ls_partner-central_data-common-datax-bp_organization-name4 = abap_true.
  ls_partner-central_data-taxnumber-common-datax-nat_person = abap_true.
  ls_partner-central_data-common-datax-bp_organization-foundationdate = abap_true.
  ls_partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
  ls_partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.

  CLEAR:lt_roles.
  CLEAR ls_roles.
  ls_roles-task = 'I'.
  ls_roles-data_key = 'FLCU00'.     "角色
  APPEND ls_roles TO lt_roles.
  CLEAR ls_roles.
  ls_roles-task = 'I'.
  ls_roles-data_key = 'FLCU01'.     "角色
  APPEND ls_roles TO lt_roles.
  CLEAR ls_roles.
  ls_roles-task = 'I'.
  ls_roles-data_key = 'UKM000'.     "角色
  APPEND ls_roles TO lt_roles.

  ls_partner-central_data-role-roles = lt_roles.

  CLEAR:ls_addresses,lt_addresses.
  ls_addresses-task = 'I'.
  ls_addresses-data-postal-data-langu = sy-langu."语言
  ls_addresses-data-postal-data-postl_cod1 = data-pstlz.  " 邮政编码
  ls_addresses-data-postal-data-country = data-land1.     " 国家代码
  ls_addresses-data-postal-data-region = |{ data-regio ALPHA = IN }|.     " 地区代码
  ls_addresses-data-postal-data-city = data-ort01.     " 地区代码
  ls_addresses-data-postal-data-str_suppl3 = data-strsuppl3.     " 街道2 WD
  ls_addresses-data-postal-data-location = data-location.     " 街道3 WD

  ls_addresses-data-postal-datax-langu = abap_true."语言
  ls_addresses-data-postal-datax-postl_cod1 = abap_true.       " 邮政编码
  ls_addresses-data-postal-datax-country = abap_true.     " 国家代码
  ls_addresses-data-postal-datax-region = abap_true.     " 地区代码
  ls_addresses-data-postal-datax-city = abap_true.     " 地区代码
  ls_addresses-data-postal-datax-str_suppl3 = abap_true.     " 街道3 WD
  ls_addresses-data-postal-datax-location = abap_true.     " 街道4

  CLEAR:ls_phone,lt_phone.
  ls_phone-contact-task = 'I'.
  ls_phone-contact-data-telephone = data-eikto.  "电话
  ls_phone-contact-datax-telephone = abap_true.
  APPEND ls_phone TO lt_phone.
  CLEAR:ls_phone.
  ls_phone-contact-task = 'I'.
  ls_phone-contact-data-telephone = data-telf1. "手机
  ls_phone-contact-data-r_3_user = 3.    "3表示移动电话
  ls_phone-contact-datax-telephone = abap_true.
  ls_phone-contact-datax-r_3_user = abap_true.
  APPEND ls_phone TO lt_phone.
  ls_addresses-data-communication-phone-phone = lt_phone.

*--地址注释 WD(联系人)
  REFRESH lt_remarks_a.
  CLEAR:ls_remarks_a.
  ls_remarks_a-task = 'I'.
  ls_remarks_a-data-langu = sy-langu.
  ls_remarks_a-datax-langu = abap_true.
*  ls_remarks_a-data-adr_notes = data-bahne.    "地址注释
*  ls_remarks_a-datax-adr_notes = abap_true.
  APPEND ls_remarks_a TO lt_remarks_a .
  ls_addresses-data-remark-remarks = lt_remarks_a.
*电子邮件存储CRM客户ID
  CLEAR:lt_smtp,ls_smtp.
  ls_smtp-contact-task = 'I'.
  ls_smtp-contact-data-e_mail = |{ data-accountid }@crm.com|.
  ls_smtp-contact-datax-e_mail = abap_true.
  APPEND ls_smtp TO lt_smtp.
  ls_addresses-data-communication-smtp-smtp = lt_smtp.

  APPEND ls_addresses TO lt_addresses.
  ls_partner-central_data-address-addresses = lt_addresses.

*--银行信息
  CLEAR:ls_bank,lt_bank.
  LOOP AT data-banks ASSIGNING FIELD-SYMBOL(<bank>).
    CLEAR:ls_bank.
    ls_bank-task = 'I'.
    ls_bank-data-bank_ctry = <bank>-banks.    "银行国家代码
    ls_bank-data-bank_key  = <bank>-bankl.    "联行号
    ls_bank-data-coll_auth = <bank>-xezer.
    ls_bank-data-bankaccountname = <bank>-accname. "银行名称
    ls_bank-data-accountholder  = <bank>-koinh.    "户主
    ls_bank-data-bank_acct  = <bank>-bankn.    "账号1
    ls_bank-data-externalbankid  = <bank>-bkext.    "账号2

    ls_bank-datax-bank_ctry = abap_true.
    ls_bank-datax-bank_key  = abap_true.
    ls_bank-datax-coll_auth  = abap_true.
    ls_bank-datax-bankaccountname  = abap_true.
    ls_bank-datax-accountholder  = abap_true.
    ls_bank-datax-bank_acct  = abap_true.
    ls_bank-datax-externalbankid  = abap_true.

    APPEND ls_bank TO lt_bank.
  ENDLOOP.

  ls_partner-central_data-bankdetail-bankdetails = lt_bank.
********ADD BY DONGPZ BEGIN AT 21.12.2021 22:09:26
*税号信息
  CLEAR:wa_taxnumbers,it_taxnumbers.
  wa_taxnumbers-data_key-taxtype = 'CN5'.
  wa_taxnumbers-task = 'I'.
  wa_taxnumbers-data_key-taxnumxl = data-taxnumxl.
  APPEND wa_taxnumbers TO it_taxnumbers.
  ls_partner-central_data-taxnumber-taxnumbers = it_taxnumbers."税号
********ADD BY DONGPZ END AT 21.12.2021 22:09:26
****客户数据表头**********************************************
  CLEAR:ls_header,ls_customer.
  ls_header-object_task = 'I'.
  ls_customer-header = ls_header.

  "客户销售的出发票
  CLEAR:ls_tax_ind,lt_tax_ind.
  ls_tax_ind-task = 'I'.
  ls_tax_ind-data_key-aland = 'CN'.
  ls_tax_ind-data_key-tatyp = 'MWST'.
  ls_tax_ind-data-taxkd = data-taxkd.
  ls_tax_ind-datax-taxkd = 'X'.
  APPEND ls_tax_ind TO lt_tax_ind.

  CLEAR:ls_central_data.
  ls_central_data-tax_ind-tax_ind = lt_tax_ind.
  ls_customer-central_data = ls_central_data.

*******************法人及其他信息
*    ls_central_data-central-data-stkzn = data-natpers.   "自然人
*
*    ls_central_data-central-datax-stkzn = abap_true.
  "20211211修改联系人及电话到销售视图
*    ls_central_data-central-data-bahne = data-bahne.   "联系人
*    ls_central_data-central-data-bahns = data-bahns.   "联系电话

  ls_customer-central_data = ls_central_data.


****公司代码数据************************************************
  CLEAR:ls_company,lt_company.
  IF data-bukrs IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   " 统驭科目补零
      EXPORTING
        input  = data-akont
      IMPORTING
        output = data-akont.

    ls_company-task = 'I'.
    ls_company-data_key-bukrs = data-bukrs. " 公司代码
    ls_company-data-akont = data-akont. " 统驭科目
    ls_company-data-zuawa = data-zuawa. " 排序码
    ls_company-data-zterm = data-zterm1. " 付款条款

    ls_company-datax-akont = abap_true.
    ls_company-datax-zuawa = abap_true.
    ls_company-datax-zterm = abap_true.
    APPEND ls_company TO lt_company.
    ls_company_data-company = lt_company.
    ls_customer-company_data = ls_company_data.

  ENDIF.

*****销售数据****************************************************
*    IF <FS_DATA>-KTOKD <> 'ZS03' AND <FS_DATA>-KTOKD <> 'ZS07' AND <FS_DATA>-KTOKD <> 'ZS08'  .
  CLEAR:ls_sales,lt_sales.
  IF  data-vkorg IS NOT INITIAL
      AND data-vtweg IS NOT INITIAL
      AND data-spart IS NOT INITIAL.
    ls_sales-task = 'I'.
    ls_sales-data_key-vkorg = data-vkorg.
    ls_sales-data_key-vtweg = data-vtweg.
    ls_sales-data_key-spart = data-spart.
    ls_sales-data-vkbur = data-vkbur.
    ls_sales-data-vkgrp = data-vkgrp.
    "ls_sales-data-kvgr1 = '01'.  "客户组
    ls_sales-data-waers = data-waers.
    ls_sales-data-kalks = 1.
    ls_sales-data-vsbed = '01'.
    ls_sales-data-vwerk = data-vwerk.
    ls_sales-data-pltyp = '01'.
    ls_sales-data-podkz = data-podkz.
    ls_sales-data-zterm = data-zterm1.
    ls_sales-data-ktgrd = data-ktgrd.
    ls_sales-data-konda = data-konda.
    IF data-vkbur <> '2020'.
      ls_sales-data-inco1 = 'EXW'.
      ls_sales-data-inco2_l = '博兴'.
      ls_sales-datax-inco1 = abap_true.
      ls_sales-datax-inco2_l = abap_true.
    ENDIF.
    ls_sales-data-vsort = data-vsort.   "联系人
    ls_sales-data-eikto = data-eikto.   "联系电话

    ls_sales-datax-vkbur = abap_true.
    ls_sales-datax-vkgrp = abap_true.
    "ls_sales-datax-kvgr1 = abap_true.  "客户组
    ls_sales-datax-waers = abap_true.
    ls_sales-datax-kalks = abap_true.
    ls_sales-datax-vsbed = abap_true.
    ls_sales-datax-vwerk = abap_true.
    ls_sales-datax-pltyp = abap_true.
    ls_sales-datax-podkz = abap_true.
    ls_sales-datax-zterm = abap_true.
    ls_sales-datax-ktgrd = abap_true.
    ls_sales-datax-konda = abap_true.
    ls_sales-datax-vsort = abap_true.
    ls_sales-datax-eikto = abap_true.

    APPEND ls_sales TO lt_sales.
    CLEAR ls_sales_data.
    ls_sales_data-sales = lt_sales.
    ls_customer-sales_data = ls_sales_data.
  ENDIF.

  CLEAR:ls_data,lt_data.
  ls_data-partner = ls_partner.
  ls_data-customer = ls_customer.

  APPEND ls_data TO lt_data.

  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_data
*     I_EXT_DATA       =
    IMPORTING
      e_return = lt_return.
  CLEAR lv_msg.
  LOOP AT lt_return INTO ls_return.
    LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A' OR type = 'X'.
      CONCATENATE lv_msg ls_msg-message INTO lv_msg.
    ENDLOOP.
  ENDLOOP.

  IF lv_msg IS INITIAL.
    CLEAR:lt_partnerguid_list,ls_partnerguid_list.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
*  根据伙伴GUID取出客户编号
    ls_partnerguid_list = ls_partner-header-object_instance-bpartnerguid.
    APPEND ls_partnerguid_list TO lt_partnerguid_list.
    DATA(lt_customer_list) = cvi_mapper=>get_instance( )->get_assigned_customers_for_bps(
                                              i_partner_guids = lt_partnerguid_list ).
    IF lt_customer_list IS NOT INITIAL.
      READ TABLE lt_customer_list INTO DATA(ls_customer_list) INDEX 1 .
      IF sy-subrc EQ 0.
        CONCATENATE '客户' ls_customer_list-customer '创建成功！' INTO rtmsg.
        kunnr = ls_customer_list-customer.
      ENDIF.
    ELSE.
      "可能创建成功了BP，但未创建成功customer
      DATA : ls_cvis_error TYPE cvis_error.
      ls_cvis_error = cvi_mapper=>get_instance( )->undo_assignments(
                                              i_for_partners = lt_partnerguid_list ).
      rtmsg = '客户创建出错，客户部分数据出错，请检查是否是BP伙伴功能问题'.
      fillmsg 'E' rtmsg.
    ENDIF.

*--维护信用段数据
    CLEAR:lv_msg1.
    REFRESH lt_return_ukmbp[].
    PERFORM frm_add_ukmbp USING data kunnr CHANGING lt_return_ukmbp .
    LOOP AT lt_return_frg WHERE type = 'E' OR type = 'A' OR type = 'X'.
      CONCATENATE lv_msg1 lt_return_frg-message INTO lv_msg1.
    ENDLOOP.

    IF lv_msg1 IS INITIAL .
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
        .
      rtmsg = '客户创建成功，但是信用相关数据创建失败'.
      fillmsg 'E' rtmsg.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
      .
    rtmsg = lv_msg.
    fillmsg 'E' rtmsg.
  ENDIF.

  "根据MDM湖区BPCODE
  "输入
  CLEAR :mdm_intab[].
  mdm_intab-kunnr = kunnr.
  mdm_intab-name1 = data-name1.
  mdm_intab-stceg = data-taxnumxl.
  mdm_intab-kdkg1 = ''.
  mdm_intab-kdkg2 = ''.
  mdm_intab-zxtly = ''.
  mdm_intab-bukrs = data-bukrs.  "公司编码
  mdm_intab-butxt = ''.    "公司名称
  mdm_intab-zcontactstatus = ''.
  mdm_intab-bahne = data-vsort.
  mdm_intab-bahns = data-eikto.
  mdm_intab-name4 = data-name_org4.
  mdm_intab-telf1 = ''.
  mdm_intab-str_suppl3 = data-strsuppl3.  "详细地址
  mdm_intab-found_dat = sy-datum.
  mdm_intab-land1 = data-land1.     "国家
  mdm_intab-landx = data-guojia .     "国家名称
  mdm_intab-regio = data-regio.   "省
  mdm_intab-bezei20 = data-province.    "省名称
  mdm_intab-zcitycode = ''.  "市编码
  mdm_intab-ort01 = data-ort01.  "市名称
  mdm_intab-counc = ''.   "县代码
  mdm_intab-zbankstatus = ''.  "银行国家代码
  READ TABLE data-banks ASSIGNING <bank> INDEX 1.
  IF <bank> IS ASSIGNED.
    mdm_intab-banks = <bank>-banks.  "银行国家代码
    mdm_intab-bankl = <bank>-bankl.  "银行编号
    mdm_intab-banka = ''.
    mdm_intab-ebpp_accname = <bank>-accname.
    mdm_intab-bankn = <bank>-bankn.
  ENDIF.
  APPEND mdm_intab.
  CLEAR:mdm_rtype,mdm_rtmsg,mdm_outtab,mdm_outtab[].
  CALL FUNCTION 'ZFM_SD_MDM_CUSTOM'
    IMPORTING
      rtype    = mdm_rtype
      rtmsg    = mdm_rtmsg
    TABLES
      in_tab   = mdm_intab
      t_result = mdm_outtab.
  IF mdm_outtab[] IS NOT INITIAL.
    LOOP AT mdm_outtab.
      mdmbp = mdm_outtab-bpext.
    ENDLOOP.
  ELSE.
    rtype = '创建MDM系统BP编号失败'.
    fillmsg 'E' rtmsg.
  ENDIF.

  "把BP编号更新到bpext中----------------------------------------------------------------------
  CLEAR:lt_return,lt_data,ls_partner,ls_customer,ls_data,ls_header,ls_company,lt_company.
  SELECT *
    FROM but000
    WHERE partner = @kunnr
    INTO TABLE @DATA(it_but000)
    .

  SELECT *
    FROM but020
    WHERE partner = @kunnr
   INTO TABLE @DATA(it_but020)
    .

  READ TABLE it_but000 INTO DATA(wa_but000) WITH KEY partner = kunnr.
  IF sy-subrc = 0.
    lv_bpartnerguid1 = wa_but000-partner_guid.
  ENDIF.

  READ TABLE it_but020 INTO DATA(wa_but020) WITH KEY partner = kunnr.
  IF sy-subrc = 0.
    lv_bpartnerguid2 = wa_but020-address_guid.
  ENDIF.

  CHECK lv_bpartnerguid1 IS NOT INITIAL.

  ls_partner-header-object_task = 'U'.
  ls_partner-header-object_instance-bpartner = kunnr ."客户号
  ls_partner-header-object_instance-bpartnerguid = lv_bpartnerguid1.

  ls_partner-central_data-common-data-bp_centraldata-partnerexternal = mdmbp.
  ls_partner-central_data-common-datax-bp_centraldata-partnerexternal = abap_true.

*  ls_partner-central_data-common-data-bp_control-category = 2.                 "业务伙伴类别
*  ls_partner-central_data-common-data-bp_control-grouping = data-ktokd.  " 分组
*
*
*  ls_partner-central_data-common-data-bp_organization-name1 = data-name1. " 名称1
*  ls_partner-central_data-common-data-bp_organization-name2 = data-name2.  " 名称2
*  ls_partner-central_data-common-data-bp_centraldata-searchterm1 = data-sortl. "业务伙伴的搜索词 1（简称）
*  ls_partner-central_data-common-data-bp_centraldata-partnerlanguage = sy-langu.
*
*  ls_partner-central_data-common-datax-bp_organization-name1 = abap_true.
*  ls_partner-central_data-common-datax-bp_organization-name2 = abap_true.
*  ls_partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
*  ls_partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.
*
*  CLEAR:ls_addresses,lt_addresses.
*  ls_addresses-task = 'M'.
*  ls_addresses-data_key-guid = lv_bpartnerguid2.
*  ls_addresses-data-postal-data-move_addr_guid = lv_bpartnerguid2.
*  ls_addresses-data-postal-data-langu = sy-langu."语言
*  ls_addresses-data-postal-data-postl_cod1 = data-pstlz.  " 邮政编码
*  ls_addresses-data-postal-data-country = data-land1.     " 国家代码
*  ls_addresses-data-postal-data-region = data-regio.     " 地区代码
*  ls_addresses-data-postal-data-city = data-ort01.     " 地区代码
*
*  ls_addresses-data-postal-data-str_suppl3 = data-strsuppl3.     " 街道2 WD
*  ls_addresses-data-postal-data-location = data-location.     " 街道3 WD
*
*  ls_addresses-data-postal-datax-move_addr_guid = abap_true.
*  ls_addresses-data-postal-datax-langu = abap_true."语言
*  ls_addresses-data-postal-datax-postl_cod1 = abap_true.       " 邮政编码
*  ls_addresses-data-postal-datax-country = abap_true.     " 国家代码
*  ls_addresses-data-postal-datax-region = abap_true.     " 地区代码
*  ls_addresses-data-postal-datax-city = abap_true.     " 地区代码
*  ls_addresses-data-postal-datax-str_suppl3 = abap_true.     " 街道3 WD
*  ls_addresses-data-postal-datax-location = abap_true.     " 街道4
*
*  ls_addresses-data-postal-data-location = data-location.
*  ls_addresses-data-postal-datax-location = abap_true.
*
*  ls_addresses-data-postal-data-country = 'CN'.
*  ls_addresses-data-postal-datax-country = abap_true.
*
*  CLEAR:ls_phone,lt_phone.
*  ls_phone-contact-task = 'M'.
*  ls_phone-contact-data-telephone = data-eikto.  "电话
*  ls_phone-contact-datax-telephone = abap_true.
*  APPEND ls_phone TO lt_phone.
*  CLEAR:ls_phone.
*  ls_phone-contact-task = 'M'.
*  ls_phone-contact-data-telephone = data-telf1. "手机
*  ls_phone-contact-data-r_3_user = 3.    "3表示移动电话
*  ls_phone-contact-datax-telephone = abap_true.
*  ls_phone-contact-datax-r_3_user = abap_true.
*  APPEND ls_phone TO lt_phone.
*  ls_addresses-data-communication-phone-phone = lt_phone.
*
**--地址注释 WD(联系人)
*  REFRESH lt_remarks_a.
*  CLEAR:ls_remarks_a.
*  ls_remarks_a-task = 'M'.
*  ls_remarks_a-data-langu = sy-langu.
*  ls_remarks_a-datax-langu = abap_true.
*  ls_remarks_a-data-adr_notes = data-vsort.
*  ls_remarks_a-datax-adr_notes = abap_true.
*  APPEND ls_remarks_a TO lt_remarks_a .
*  ls_addresses-data-remark-remarks = lt_remarks_a.
*
**电子邮件存储CRM客户ID
*  CLEAR:lt_smtp,ls_smtp.
*  ls_smtp-contact-task = 'M'.
*  ls_smtp-contact-data-e_mail = data-accountid.
*  ls_smtp-contact-datax-e_mail = abap_true.
*  APPEND ls_smtp TO lt_smtp.
*  ls_addresses-data-communication-smtp-smtp = lt_smtp.
*
*  APPEND ls_addresses TO lt_addresses.
*  ls_partner-central_data-address-addresses = lt_addresses.
*
*
*****客户数据表头**********************************************
*  ls_header-object_task = 'U'.
*  ls_customer-header = ls_header.
*
*****公司代码数据************************************************
*  IF data-bukrs IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'   " 统驭科目补零
*      EXPORTING
*        input  = data-akont
*      IMPORTING
*        output = data-akont.
*
*    ls_company-task = 'M'.
*    ls_company-data_key-bukrs = data-bukrs. " 公司代码
*    ls_company-data-akont = data-akont. " 统驭科目
*    ls_company-data-zuawa = data-zuawa. " 排序码
*    ls_company-data-zterm = data-zterm1. " 付款条款
*
*    ls_company-datax-akont = abap_true.
*    ls_company-datax-zuawa = abap_true.
*    ls_company-datax-zterm = abap_true.
*    APPEND ls_company TO lt_company.
*    ls_company_data-company = lt_company.
*    ls_customer-company_data = ls_company_data.

*  ENDIF.

  CLEAR:lt_data.
  ls_data-partner = ls_partner.
  ls_data-customer = ls_customer.
  APPEND ls_data TO lt_data.

  SET UPDATE TASK LOCAL.
  CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
    EXPORTING
      i_data   = lt_data
*     I_EXT_DATA       =
    IMPORTING
      e_return = lt_return.

  CLEAR lv_msg.
  LOOP AT lt_return INTO ls_return.
    LOOP AT ls_return-object_msg INTO ls_msg WHERE type = 'E' OR type = 'A' OR type = 'X'.
      CONCATENATE lv_msg ls_msg-message INTO lv_msg.
    ENDLOOP.
  ENDLOOP.
  IF lv_msg IS INITIAL .
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*       IMPORTING
*         RETURN        =
      .
    rtmsg = |更新BP编码出错：{ lv_msg }|.
    fillmsg 'E' rtmsg.
  ENDIF.

  fillmsg 'S' '创建客户成功'.
  zfmdatasave2 'R'.




ENDFUNCTION.

*&---------------------------------------------------------------------*
*& Form FRM_ADD_UKMBP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_add_ukmbp USING VALUE(p_data) TYPE zscrm_bp VALUE(p_kunnr) CHANGING c_return TYPE ukm_t_monitor_return.

  DATA: io_facade        TYPE REF TO cl_ukm_facade,
        io_bupa_factory  TYPE REF TO cl_ukm_bupa_factory,
        io_partner       TYPE REF TO cl_ukm_business_partner,
        io_account       TYPE REF TO cl_ukm_account,
        lw_bp_credit_sgm TYPE ukm_s_bp_cms_sgm.

  DATA: lwa_ukm_s_bp_cms TYPE ukm_s_bp_cms.
  DATA: lv_partner      TYPE bu_partner,
        lv_credit_sgmnt TYPE ukm_credit_sgmnt.


*  创建'MAINTAIN'对象
  io_facade  = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
  io_bupa_factory = io_facade->get_bupa_factory( ).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_kunnr
    IMPORTING
      output = p_kunnr.

  lv_partner      = p_kunnr. "客户代码
  lv_credit_sgmnt = p_data-bukrs. "信用段

  io_partner = io_bupa_factory->get_business_partner( lv_partner ).
  io_partner->get_bp_cms( IMPORTING es_bp_cms =  lwa_ukm_s_bp_cms ).

  lwa_ukm_s_bp_cms-risk_class   = 'A'.  "风险类
  lwa_ukm_s_bp_cms-check_rule   = '01'.  "检查规则
  lwa_ukm_s_bp_cms-limit_rule   = 'STANDARD'.   "规则
  "LWA_UKM_S_BP_CMS-CREDIT_GROUP = <FS_DATA>-CREDIT_GROUP. "客户组


  io_partner->set_bp_cms( lwa_ukm_s_bp_cms ).

  CALL METHOD io_bupa_factory->get_credit_account
    EXPORTING
      i_partner         = lv_partner
      i_credit_sgmnt    = lv_credit_sgmnt
    RECEIVING
      ro_credit_account = io_account.

  io_account->get_bp_cms_sgm( IMPORTING es_bp_cms_sgm = lw_bp_credit_sgm ).

  lw_bp_credit_sgm-credit_limit   = 0."信用额度
  lw_bp_credit_sgm-xcritical = 'X'.
  lw_bp_credit_sgm-limit_chg_date = sy-datum.

  io_account->set_bp_cms_sgm( EXPORTING is_bp_cms_sgm = lw_bp_credit_sgm ).

  io_bupa_factory->save_all( EXPORTING i_upd_task = abap_true
  RECEIVING et_return = c_return   ).

ENDFORM.
