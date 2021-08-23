class ZCX_PUSH definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of APPL_NOT_CONFIGURED,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of APPL_NOT_CONFIGURED .
  constants:
    begin of PROVIDER_NOT_CONFIGURED,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PROVIDER_NOT_CONFIGURED .
  constants:
    begin of FAIL_INSTANCE_CLASS,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FAIL_INSTANCE_CLASS .
  constants:
    begin of ERROR_CREATE_HTTP_CONNECTION,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_CREATE_HTTP_CONNECTION .
  constants:
    begin of ERROR_SEND_DATA,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_SEND_DATA .
  constants:
    begin of ERROR_RECEIVE_DATA,
      msgid type symsgid value 'ZPUSH',
      msgno type symsgno value '011',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_CONTENT_RESPONSE',
      attr4 type scx_attrname value '',
    end of ERROR_RECEIVE_DATA .
  data MV_MSGV1 type STRING .
  data MV_MSGV2 type STRING .
  data MV_MSGV3 type STRING .
  data MV_MSGV4 type STRING .
  data MV_CONTENT_RESPONSE type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_MSGV1 type STRING optional
      !MV_MSGV2 type STRING optional
      !MV_MSGV3 type STRING optional
      !MV_MSGV4 type STRING optional
      !MV_CONTENT_RESPONSE type STRING optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_PUSH IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
me->MV_CONTENT_RESPONSE = MV_CONTENT_RESPONSE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
