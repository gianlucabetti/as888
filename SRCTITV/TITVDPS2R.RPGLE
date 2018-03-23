000100101005      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400170213     Ftabel00f  if   e           k disk
000500000724     Fazorg01l  if   e           k disk
000600990910     Ftivin00r  uF   E             DISK    usropn
000700090911     FEDIVABwr  O    E             DISK    usropn
000800090911     FEDIVATwr  O    E             DISK    usropn
000900000313     D*
001000040113     D*------------
001100110923     D prefix          S             10    inz
001200170213     D shipment        S                   like(vindta) inz
001300170213     D sender          S                   like(vindta) inz
001400170213     D receiver        S                   like(vindta) inz
001500170213     D delivery        S                   like(vindta) inz
001600170213     D parcel          S                   like(vindta) inz
001700170213     D cod             S                   like(vindta) inz
001800170213     D msg             S                   like(vindta) inz
001900010330     D*----------------------------------------------------
002000010330     D* DICHIARAZIOINE VARIABILI DI WRK
002100010330     D*----------------------------------------------------
002200010330     D dscmz         e ds                  inz
002300010330     D psds           sds
002400010330     D  procname         *PROC
002500010330     D tivlrds       e ds                  extname(tivlr00f)
002600040128     D tisi95ds      e ds
002700170213     D ds15          e ds
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D wrkdata         s               d
003300010330     D wrkora          s               t
003400010330     D rrnum           s              6  0 INZ(*zeros)
003500010330     D recko           s            150    INZ(*blanks)
003600011113     D depcmd          s            150    INZ(*blanks)
003700040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003800040510     D parccm          s              8    INZ(*blanks)
003900040119     D parmbr          s             10    INZ(*blanks)
004000040119     D paropz          s              1    INZ(*blanks)
004100040119     D chkcall         s              1    INZ(*blanks)
004200080125     D wFlgCAS         s              1    INZ(*blanks)
004300080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
004400090911     D wCMR            s             35    INZ(*blanks)
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800170213     D SkSplitFSHP     S             32    DIM(200)
004900170213     D SkSplitFSND     S             32    DIM(200)
005000170213     D SkSplitFRCV     S             32    DIM(200)
005100170213     D SkSplitFDLV     S             32    DIM(200)
005200170213     D SkSplitFPCL     S             32    DIM(200)
005300170213     D SkSplitFCOD     S             32    DIM(200)
005400170213     D SkSplitFMSG     S             32    DIM(200)
005500111118     D SkSplitCSV      S            256    DIM(200)
005600030715     D CharCSV         S              1
005700030715     D CharTXT         S              1
005800030715     D CharNUM         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300080125     D wVATNOT_A       s                   LIKE(VATNOT)
006400080125     D wVATNOT_B       s                   LIKE(VATNOT)
006500141125     D wVATNOT_S       s                   LIKE(VATNOT)
006600141125     D wVATNOT_IJ      s             70
006700090904     D wVATNOT_E       s                   LIKE(VATNOT)
006800111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
006900111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
007000111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
007100111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
007200170213
007300170213     D*------------
007400170213     D jNAZ            s              5  0 INZ(*zeros)
007500170213     D skNAZISO        S              3  0 DIM(1000)
007600170213     D skNAZBAR        S              3    DIM(1000)
007700170213
007800030822     D*------------------
007900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
008000030822     D*------------------
008100030822     D posDaDft        S              3  0 INZ(*zeros)
008200030822     D posADft         S              3  0 INZ(*zeros)
008300030822     D j               s              3  0 INZ(1)
008400030822     D wGiroDft        s              1  0 INZ(*zeros)
008500031201     D*------------------
008600031201     D* Costanti
008700031201     D*------------------
008800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
009000080617     D*------------------
009100080617
009200080617     D*------------------
009300080617     D* LINKING A DEFINIZIONI ESTERNE
009400080617     D*------------------
009500080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009600090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009700010330
009800010330
009900010330
010000990915     C                   time                    wrkdata
010100990915     C                   time                    wrkora
010200000913     C                   reset                   rrnum
010300990921     C                   reset                   esito
010400081014     C                   reset                   wrkesito
010500010601     C*
010600010601     C                   exsr      opeini
010700170213     C                   exsr      cartab
010800170213     C                   exsr      rwvab
010900081014     C                   exsr      endela
011000010601     C*
011100010601     C                   seton                                        lr
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010601     C*--------------------------------------------------------
011900010601     C     ENDELA        BEGSR
012000081014     C*
012100081014     C                   if        %open(tivin00r)
012200081014     C                   close     tivin00r
012300081014     C                   endif
012400090911     C                   if        %open(edivabwr)
012500090911     C                   close     edivabwr
012600081014     C                   endif
012700090911     C                   if        %open(edivatwr)
012800090911     C                   close     edivatwr
012900081014     C                   endif
013000040119     C*
013100040119     C* Effettuo la chiamata al CLLE preposto
013200101005     C                   call(e)   'TITVEVTC'
013300040510     C                   parm                    parccm
013400040119     C                   parm                    parmbr
013500040119     C                   parm      '2'           paropz
013600050201     C*
013700050201     C* Effettuo lancio TISI95 solo x chiusura
013800050201     C                   CLEAR                   TISI95DS
013900050201     C                   EVAL      I95TLA = 'C'
014000050201     C                   CALL      'TISI95R'
014100050201     C                   PARM                    TISI95DS
014200000616     C*
014300010601     C                   ENDSR
014400010601     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600090911     C                   if        not %open(edivabwr)
015700090911     C                   open      edivabwr
015800010330     C                   endif
015900040119     C*
016000101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016100101005     C                   exsr      prevat
016200040119     C*
016300040119     C                   if        chkcall = '0'
016400040119     C*
016500090911     C                   if        not %open(edivatwr)
016600090911     C                   open      edivatwr
016700040119     C                   endif
016800090623     C*
016900040119     C                   clear                   §CTROKVB          7 0
017000040119     C                   clear                   §CTROKVT          7 0
017100040119     C                   clear                   §CTRMO            7 0
017200040119     C                   clear                   §CTRNO            7 0
017300090623     C*
017400090623     C* Inizializzazioni
017500170213     C                   exsr      inzvar
017600170213     C                   z-add     *zeros        wNewBolla         7 0
017700010330     C*
017800030822     C                   do        *HIVAL
017900010330     C*
018000030822     C                   read      tivin00r                               70
018100010618     C*
018200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018300010618     C                   if        vinflg = '1'
018400010618     C                   eval      flgOk = '1'
018500010618     C                   endif
018600040510     C*
018700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018800040510     C                   z-add     *zeros        wLenVINDTA        4 0
018900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019300040510     C                   z-add     1             wFlgVINDTA
019400040510     C                   leave
019500040510     C                   endif
019600040510     C                   enddo
019700170213     C*
019800170213     C* Slavo il CMR del file corrente
019900170213     C                   if        %subst(vindta:1:5)  ='#FILE'
020000170213     C                   eval      wCMR = %subst(vindta:22:
020100170213     C                                           %len(%trim(vindta))-1-22)
020200170213     C                   endif
020300110923     C*
020400170213     C* Ignoro i record che iniziano x '#' (tranne quelli che sono di "intestazione")
020500170213     C                   if        %subst(vindta:1:1)  ='#'                  AND
020600170213     C                             %subst(vindta:6:16)<>'GEODATA:SHIPMENT'   AND
020700170213     C                             %subst(vindta:6:14)<>'GEODATA:SENDER'     AND
020800170213     C                             %subst(vindta:6:16)<>'GEODATA:RECEIVER'   AND
020900170213     C                             %subst(vindta:6:16)<>'GEODATA:DELIVERY'   AND
021000170213     C                             %subst(vindta:6:14)<>'GEODATA:PARCEL'     AND
021100170213     C                             %subst(vindta:6:11)<>'GEODATA:COD'        AND
021200170213     C                             %subst(vindta:6:11)<>'GEODATA:MSG'
021300111118     C                   z-add     0             wFlgVINDTA
021400111118     C                   else
021500110923     C*
021600110923     C* Setto i column headers "variabili"
021700170213     C                   if        %subst(vindta:1:21)='#DEF;GEODATA:SHIPMENT'
021800170213     C                   eval      shipment = %trim(%subst(vindta:14))
021900110923     C                   endif
022000170213     C                   if        %subst(vindta:1:19)='#DEF;GEODATA:SENDER'
022100170213     C                   eval      sender   = %trim(%subst(vindta:14))
022200110923     C                   endif
022300170213     C                   if        %subst(vindta:1:21)='#DEF;GEODATA:RECEIVER'
022400170213     C                   eval      receiver = %trim(%subst(vindta:14))
022500161011     C                   endif
022600170213     C                   if        %subst(vindta:1:21)='#DEF;GEODATA:DELIVERY'
022700170213     C                   eval      delivery = %trim(%subst(vindta:14))
022800170213     C                   endif
022900170213     C                   if        %subst(vindta:1:19)='#DEF;GEODATA:PARCEL'
023000170213     C                   eval      parcel   = %trim(%subst(vindta:14))
023100170213     C                   endif
023200170213     C                   if        %subst(vindta:1:16)='#DEF;GEODATA:COD'
023300170213     C                   eval      cod      = %trim(%subst(vindta:14))
023400170213     C                   endif
023500170213     C                   if        %subst(vindta:1:16)='#DEF;GEODATA:MSG'
023600170213     C                   eval      msg      = %trim(%subst(vindta:14))
023700170213     C                   endif
023800170213     C                   endif
023900170213     C*
024000170213     C                   add       1             rrnum
024100010618     C*
024200040510     C                   if        vindta > *blanks AND
024300090420     C                             wFlgVINDTA = 1
024400010330     C*
024500010601     C                   if        *in70 = *off and
024600010330     C                             (vinflg = *blanks
024700010330     C                              or vinflg = '0'
024800010330     C                              or vinflg = '2')
024900010330     C*
025000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025100010711     C                   if        vinflg = *blanks or vinflg = '0'
025200010711     C                   clear                   vinmsg
025300010711     C                   endif
025400010601     C*
025500090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
025600111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
025700111117     C***                eval      vindta  = '*' + %trim(vindta)
025800111117     C***                endif
025900090914     C*
026000090914     C* Elaborazione record corrente
026100090914     C                   exsr      elabCurrRec
026200151120     C*
026300151120     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026400151120     C                   if        vlrpoi = 999
026500151120     C                   else
026600151120     C                   move(P)   vlrpoi        VABFGS
026700151120     C                   endif
026800151120     C                   eval      invfil = VABFGS
026900010604     C*
027000010604     C                   if        *in31 = *off and
027100010604     C                             *in32 = *off
027200010604     C                   eval      vinflg = '1'
027300010604     C                   else
027400010604     C                   eval      recko = vindta
027500010604     C                   eval      vinflg = '2'
027600010604     C                   endif
027700010601     C*
027800010604     C                   endif
027900010604     C*
028000010330     C                   else
028100010330     C                   eval      vinflg = '1'
028200010330     C                   endif
028300010601     C*
028400010601     C  N70              update    tivin000
028500010330     C*
028600030822     C  N70              enddo
028700090623     C*
028800090623     C* Scarico la testata rimasta "in canna"
028900140415     C                   if        not *in33                                    * esludo estero
029000170213     C  N31              exsr      chkVAB
029100090623     C  N31              exsr      wriVAB
029200170217     C  N31              exsr      chkVAT
029300170217     C  N31              exsr      wriVAT_A
029400170217     C  N31              exsr      wriVAT_B
029500170217     C  N31              exsr      wriVAT_IJ
029600170217     C  N31              exsr      wriVAT_S
029700140415     C                   endif
029800040119     C*
029900040119     C                   endif                                                  (endif - chkcall)
030000010601     C*
030100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
030200010601     C                   if        cntNonEl = *zeros or
030300010601     C                             flgMulti = '0'
030400010330     C* Se non ci sono record con errori ...
030500010601     C                   if        §ctrno = 0 and
030600010604     C                             §ctrmo = 0 and
030700010601     C                             flgStato <> '2'
030800010330     C* ... restituisco esito OK.
030900010330     C                   eval      wrkesito = '0'
031000010330     C                   else
031100040119     C                   if        §ctrokvb > 0
031200010330     C                   eval      wrkesito = '1'
031300010330     C                   else
031400010615     C                   if        flgOk = '0'
031500010615     C                   eval      wrkesito = '2'
031600010615     C                   else
031700010615     C                   eval      wrkesito = '6'
031800010615     C                   endif
031900010330     C                   endif
032000010330     C                   endif
032100010601     C                   else
032200010601     C                   eval      wrkesito = '9'
032300010601     C                   endif
032400010330     C*
032500010330     C                   if        %open(tivin00r)
032600010330     C                   close     tivin00r
032700010330     C                   endif
032800090911     C                   if        %open(edivabwr)
032900090911     C                   close     edivabwr
033000010330     C                   endif
033100090911     C                   if        %open(edivatwr)
033200090911     C                   close     edivatwr
033300040512     C                   endif
033400010601     C*
033500010601     C                   if        vlrpoi <> 999
033600010601     C                   eval      invfil = vlrpoi
033700010601     C                   endif
033800010330     C*
033900040119     C                   if        §ctrokvb > 0
034000010601     C                             and invfil > *zeros
034100010330     C                   exsr      invio
034200010330     C                   endif
034300010330     C*
034400010330     C                   ENDSR
034500010330     C***
034600090623
034700090623
034800090623
034900090623     C*----------------------------------------------------*
035000090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
035100090623     C*----------------------------------------------------*
035200090623     C     INZWRK        BEGSR
035300090623     C*
035400090623     C                   eval      i = 1
035500090623     C                   eval      posDa = *zeros
035600090623     C                   eval      posA  = *zeros
035700090623     C*
035800090623     C                   ENDSR
035900090623
036000090623
036100090623
036200090623     C*----------------------------------------------------*
036300090623     C*  ELABORAZIONE RECORD CORRENTE
036400090623     C*----------------------------------------------------*
036500090623     C     ELABCURRREC   BEGSR
036600090623     C*
036700090623     C                   select
036800090623     C*
036900170213     C* Tipo record "TESTATA" - SHIPMENT
037000170213     C                   when      %subst(vindta:1:8) = 'SHIPMENT'
037100090623     C                   if        wNewBolla > *zeros
037200140415     C                   if        not *in33                                    * esludo estero
037300170213     C  N31              exsr      chkVAB
037400170217     C  N31              exsr      wriVAB
037500170217     C  N31              exsr      chkVAT
037600170217     C  N31              exsr      wriVAT_A
037700170217     C  N31              exsr      wriVAT_B
037800170217     C  N31              exsr      wriVAT_IJ
037900170217     C  N31              exsr      wriVAT_S
038000140415     C                   endif
038100161011     C                   exsr      inzvar
038200161011     C                   endif
038300090624     C                   eval      wNewBolla = 1
038400161011     C                   eval      wFlgCAS = *blanks
038500090623     C                   eval      wGiro = *zeros
038600170213     C                   clear                   SkSplitFSHP
038700170213     C                   eval      prefix = 'SHP_'
038800090624     C                   exsr      inzwrk
038900170213     C                   exsr      impvabshp
039000090623     C                   exsr      inzwrk
039100170213     C                   exsr      impvabshp
039200170213     C*
039300170213     C* Tipo record "TESTATA" - SENDER
039400170213     C                   when      %subst(vindta:1:6) = 'SENDER'
039500170213     C                   eval      wGiro = *zeros
039600170213     C                   clear                   SkSplitFSND
039700170213     C                   eval      prefix = 'SND_'
039800170213     C                   exsr      inzwrk
039900170213     C                   exsr      impvabsnd
040000170213     C                   exsr      inzwrk
040100170213     C                   exsr      impvabsnd
040200170213     C*
040300170213     C* Tipo record "TESTATA" - RECEIVER
040400170213     C                   when      %subst(vindta:1:8) = 'RECEIVER'
040500170213     C                   eval      wGiro = *zeros
040600170213     C                   clear                   SkSplitFRCV
040700170213     C                   eval      prefix = 'RCV_'
040800170213     C                   exsr      inzwrk
040900170213     C                   exsr      impvabrcv
041000170213     C                   exsr      inzwrk
041100170213     C                   exsr      impvabrcv
041200090623     C*
041300170213     C* Tipo record "TESTATA" - DELIVERY
041400170213     C                   when      %subst(vindta:1:8) = 'DELIVERY'
041500090623     C                   eval      wGiro = *zeros
041600170213     C                   clear                   SkSplitFDLV
041700170213     C                   eval      prefix = 'DLV_'
041800090623     C                   exsr      inzwrk
041900170213     C                   exsr      impvabdlv
042000090623     C                   exsr      inzwrk
042100170213     C                   exsr      impvabdlv
042200170213     C*
042300170213     C* Tipo record "DETTAGLIO" - PARCEL
042400170213     C                   when      %subst(vindta:1:6) = 'PARCEL'
042500170213     C                   eval      wGiro = *zeros
042600170213     C                   clear                   SkSplitFPCL
042700170213     C                   eval      prefix = 'PCL_'
042800170213     C                   exsr      inzwrk
042900170213     C                   exsr      impvatpcl
043000170213     C                   exsr      inzwrk
043100170213     C                   exsr      impvatpcl
043200161209     C*
043300161209     C                   if        not *in33                                    * esludo estero
043400170213     C  N31              exsr      chkVAT
043500170217     C  N31              exsr      wriVAT_E
043600161209     C                   endif
043700161011     C*
043800161011     C* Tipo record "CONTRASSEGNO" - COD
043900161011     C                   when      %subst(vindta:1:3) = 'COD'
044000161011     C                   eval      wGiro = *zeros
044100170213     C                   clear                   SkSplitFCOD
044200161011     C                   eval      prefix = 'COD_'
044300161011     C                   exsr      inzwrk
044400161011     C                   exsr      impvabcod
044500161011     C                   exsr      inzwrk
044600161011     C                   exsr      impvabcod
044700170213     C*
044800170213     C* Tipo record "ALERT" - MSG
044900170213     C                   when      %subst(vindta:1:3) = 'MSG'
045000170213     C                   eval      wGiro = *zeros
045100170213     C                   clear                   SkSplitFMSG
045200170213     C                   eval      prefix = 'MSG_'
045300170213     C                   exsr      inzwrk
045400170213     C                   exsr      impvatmsg
045500170213     C                   exsr      inzwrk
045600170213     C                   exsr      impvatmsg
045700090623     C*
045800090623     C                   endsl
045900090623     C*
046000090623     C                   ENDSR
046100170213
046200170213
046300170213
046400170213     C*----------------------------------------------------*
046500170213     C*  CONTROLLO PRE-SCARICAMNETO BUFFER VAB
046600170213     C*----------------------------------------------------*
046700170213     C     CHKVAB        BEGSR
046800170213     C*
046900170213     C* Suddivisione nei campi note spedizione
047000170213     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
047100170213     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
047200170213     C*
047300170213     C                   ENDSR
047400040119
047500040119
047600040119
047700040119     C*----------------------------------------------------*
047800170213     C*  SCARICAMENTO BUFFER RECORDS VAB
047900170213     C*----------------------------------------------------*
048000170213     C     WRIVAB        BEGSR
048100090623     C*
048200090623     C  N31              add       1             §CTROKVB
048300090623     C   32              add       1             §CTRMO
048400090623     C   31              add       1             §CTRNO
048500040119     C*
048600060331     C* Gestisco l'eventuale rottura x numero spedizione
048700070823     C                   if        VABNSP <> depspe
048800070301     C                   if        wVATNOT_E <> *blanks
048900080125     C                   if        VABCTM = *blanks
049000080125     C                   eval      VABCTM = '7Q'
049100080125     C                   endif
049200070301     C                   endif
049300090911     C*
049400090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
049500090911     C                   eval      VABCMR = wCMR
049600090911     C                   eval      VABDCM = DATCOR
049700090911     C                   eval      VABDTS = DATCOR
049800090911     C                   eval      VABHMS = ORACOR
049900090911     C                   eval      VABCNT = 1
050000170213     C***
050100090911     C                   write     edivab00                                     => scarico il VAB
050200060331     C                   endif
050300090623     C*
050400090623     C  N31              eval      depspe = vabNSP
050500040119     C*
050600040119     C                   ENDSR
050700170213
050800170213
050900170213
051000170213     C*----------------------------------------------------*
051100170213     C*  CONTROLLO PRE-SCARICAMNETO BUFFER VAT
051200170213     C*----------------------------------------------------*
051300170213     C     CHKVAT        BEGSR
051400170213     C*
051500170213     C*
051600170213     C                   ENDSR
051700040119
051800040119
051900040119
052000040119     C*----------------------------------------------------*
052100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
052200040119     C*----------------------------------------------------*
052300040510     C     WRIVAT        BEGSR
052400060512     C*
052500060512     C* Inizializzo i campi chiave
052600151120     C                   eval      VATFGS = VABFGS
052700060512     C                   eval      VATAAS = VABAAS
052800060512     C                   eval      VATCCM = VABCCM
052900070823     C                   eval      VATNRS = VABNRS
053000060512     C                   eval      VATNSP = VABNSP
053100060512     C                   eval      VATLNP = VABLNP
053200111117     C                   eval      VATCMR = wCMR
053300090914     C                   eval      VATCNT = 1
053400060331     C*
053500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
053600060331     C                   if        wVATNOT_A <> *blanks
053700060331     C                   eval      VATNOT = wVATNOT_A
053800060331     C                   eval      VATTRC = 'A'
053900161212     C                   if        vatNOT <> *blanks
054000090911     C                   write     EDIVAT00
054100060331     C                   add       1             §CTROKVT
054200161212     C                   endif
054300060331     C                   endif
054400060331     C*
054500060331     C                   if        wVATNOT_B <> *blanks
054600060331     C                   eval      VATNOT = wVATNOT_B
054700060331     C                   eval      VATTRC = 'B'
054800161212     C                   if        vatNOT <> *blanks
054900090911     C                   write     EDIVAT00
055000060331     C                   add       1             §CTROKVT
055100161212     C                   endif
055200060331     C                   endif
055300111118     C*
055400111118     C                   if        wVATNOT_E <> *blanks
055500111118     C                   eval      VATNOT = wVATNOT_E
055600111118     C                   eval      VATTRC = 'E'
055700161212     C                   if        vatNOT <> *blanks
055800111118     C                   write     EDIVAT00
055900111118     C                   add       1             §CTROKVT
056000161212     C                   endif
056100111118     C                   endif
056200141125     C*
056300170213     C                   if        wVATNOT_S <> *blanks
056400170213     C                   eval      VATNOT = wVATNOT_S
056500170213     C                   eval      VATTRC = 'S'
056600170213     C                   if        vatNOT <> *blanks
056700170213     C                   write     EDIVAT00
056800170213     C                   add       1             §CTROKVT
056900170213     C                   endif
057000170213     C                   endif
057100141125     C*
057200141125     C                   if        wVATNOT_IJ <> *blanks
057300141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
057400141125     C                   eval      VATTRC = 'I'
057500161212     C                   if        vatNOT <> *blanks
057600141125     C                   write     EDIVAT00
057700141125     C                   add       1             §CTROKVT
057800161212     C                   endif
057900141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
058000141125     C                   eval      VATTRC = 'J'
058100161212     C                   if        vatNOT <> *blanks
058200141125     C                   write     EDIVAT00
058300141125     C                   add       1             §CTROKVT
058400161212     C                   endif
058500141125     C                   endif
058600040119     C*
058700040119     C                   ENDSR
058800170217
058900170217
059000170217
059100170217     C*----------------------------------------------------*
059200170217     C*  SCARICAMENTO BUFFER RECORDS VAT - A
059300170217     C*----------------------------------------------------*
059400170217     C     WRIVAT_A      BEGSR
059500170217     C*
059600170217     C* Inizializzo i campi chiave
059700170217     C                   eval      VATFGS = VABFGS
059800170217     C                   eval      VATAAS = VABAAS
059900170217     C                   eval      VATCCM = VABCCM
060000170217     C                   eval      VATNRS = VABNRS
060100170217     C                   eval      VATNSP = VABNSP
060200170217     C                   eval      VATLNP = VABLNP
060300170217     C                   eval      VATCMR = wCMR
060400170217     C                   eval      VATCNT = 1
060500170217     C*
060600170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
060700170217     C                   if        wVATNOT_A <> *blanks
060800170217     C                   eval      VATNOT = wVATNOT_A
060900170217     C                   eval      VATTRC = 'A'
061000170217     C                   if        vatNOT <> *blanks
061100170217     C                   write     EDIVAT00
061200170217     C                   add       1             §CTROKVT
061300170217     C                   endif
061400170217     C                   endif
061500170217     C*
061600170217     C                   ENDSR
061700170217
061800170217
061900170217
062000170217     C*----------------------------------------------------*
062100170217     C*  SCARICAMENTO BUFFER RECORDS VAT - B
062200170217     C*----------------------------------------------------*
062300170217     C     WRIVAT_B      BEGSR
062400170217     C*
062500170217     C* Inizializzo i campi chiave
062600170217     C                   eval      VATFGS = VABFGS
062700170217     C                   eval      VATAAS = VABAAS
062800170217     C                   eval      VATCCM = VABCCM
062900170217     C                   eval      VATNRS = VABNRS
063000170217     C                   eval      VATNSP = VABNSP
063100170217     C                   eval      VATLNP = VABLNP
063200170217     C                   eval      VATCMR = wCMR
063300170217     C                   eval      VATCNT = 1
063400170217     C*
063500170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
063600170217     C                   if        wVATNOT_B <> *blanks
063700170217     C                   eval      VATNOT = wVATNOT_B
063800170217     C                   eval      VATTRC = 'B'
063900170217     C                   if        vatNOT <> *blanks
064000170217     C                   write     EDIVAT00
064100170217     C                   add       1             §CTROKVT
064200170217     C                   endif
064300170217     C                   endif
064400170217     C*
064500170217     C                   ENDSR
064600170217
064700170217
064800170217
064900170217     C*----------------------------------------------------*
065000170217     C*  SCARICAMENTO BUFFER RECORDS VAT - E
065100170217     C*----------------------------------------------------*
065200170217     C     WRIVAT_E      BEGSR
065300170217     C*
065400170217     C* Inizializzo i campi chiave
065500170217     C                   eval      VATFGS = VABFGS
065600170217     C                   eval      VATAAS = VABAAS
065700170217     C                   eval      VATCCM = VABCCM
065800170217     C                   eval      VATNRS = VABNRS
065900170217     C                   eval      VATNSP = VABNSP
066000170217     C                   eval      VATLNP = VABLNP
066100170217     C                   eval      VATCMR = wCMR
066200170217     C                   eval      VATCNT = 1
066300170217     C*
066400170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
066500170217     C                   if        wVATNOT_E <> *blanks
066600170217     C                   eval      VATNOT = wVATNOT_E
066700170217     C                   eval      VATTRC = 'E'
066800170217     C                   if        vatNOT <> *blanks
066900170217     C                   write     EDIVAT00
067000170217     C                   add       1             §CTROKVT
067100170217     C                   endif
067200170217     C                   endif
067300170217     C*
067400170217     C                   ENDSR
067500170217
067600170217
067700170217
067800170217     C*----------------------------------------------------*
067900170217     C*  SCARICAMENTO BUFFER RECORDS VAT - S
068000170217     C*----------------------------------------------------*
068100170217     C     WRIVAT_S      BEGSR
068200170217     C*
068300170217     C* Inizializzo i campi chiave
068400170217     C                   eval      VATFGS = VABFGS
068500170217     C                   eval      VATAAS = VABAAS
068600170217     C                   eval      VATCCM = VABCCM
068700170217     C                   eval      VATNRS = VABNRS
068800170217     C                   eval      VATNSP = VABNSP
068900170217     C                   eval      VATLNP = VABLNP
069000170217     C                   eval      VATCMR = wCMR
069100170217     C                   eval      VATCNT = 1
069200170217     C*
069300170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
069400170217     C                   if        wVATNOT_S <> *blanks
069500170217     C                   eval      VATNOT = wVATNOT_S
069600170217     C                   eval      VATTRC = 'S'
069700170217     C                   if        vatNOT <> *blanks
069800170217     C                   write     EDIVAT00
069900170217     C                   add       1             §CTROKVT
070000170217     C                   endif
070100170217     C                   endif
070200170217     C*
070300170217     C                   ENDSR
070400170217
070500170217
070600170217
070700170217     C*----------------------------------------------------*
070800170217     C*  SCARICAMENTO BUFFER RECORDS VAT - IJ
070900170217     C*----------------------------------------------------*
071000170217     C     WRIVAT_IJ     BEGSR
071100170217     C*
071200170217     C* Inizializzo i campi chiave
071300170217     C                   eval      VATFGS = VABFGS
071400170217     C                   eval      VATAAS = VABAAS
071500170217     C                   eval      VATCCM = VABCCM
071600170217     C                   eval      VATNRS = VABNRS
071700170217     C                   eval      VATNSP = VABNSP
071800170217     C                   eval      VATLNP = VABLNP
071900170217     C                   eval      VATCMR = wCMR
072000170217     C                   eval      VATCNT = 1
072100170217     C*
072200170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
072300170217     C                   if        wVATNOT_IJ <> *blanks
072400170217     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
072500170217     C                   eval      VATTRC = 'I'
072600170217     C                   if        vatNOT <> *blanks
072700170217     C                   write     EDIVAT00
072800170217     C                   add       1             §CTROKVT
072900170217     C                   endif
073000170217     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
073100170217     C                   eval      VATTRC = 'J'
073200170217     C                   if        vatNOT <> *blanks
073300170217     C                   write     EDIVAT00
073400170217     C                   add       1             §CTROKVT
073500170217     C                   endif
073600170217     C                   endif
073700170217     C*
073800170217     C                   ENDSR
073900040119
074000040119
074100040119
074200040119     C*----------------------------------------------------*
074300101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
074400040119     C*----------------------------------------------------*
074500101005     C     PREVAT        BEGSR
074600040119     C*
074700101005     C* Compongo il nome del membro da dare al EDIVATWR
074800040119     C                   eval      parmbr = vlrhdl
074900040119     C                   movel     'M'           parmbr
075000040510     C                   eval      parccm = vlrksc
075100040119     C                   eval      paropz = '1'
075200040119     C* Effettuo la chiamata al CLLE preposto
075300101005     C                   call(e)   'TITVEVTC'
075400040119     C                   parm                    parccm
075500040119     C                   parm                    parmbr
075600040119     C                   parm                    paropz
075700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
075800040119     C                   if        %error
075900040119     C                   movel     '1'           chkcall
076000040119     C                   else
076100040119     C                   movel     '0'           chkcall
076200040119     C                   endif
076300040119     C*
076400040119     C                   ENDSR
076500010601
076600010601
076700010601
076800010330     C*----------------------------------------------------*
076900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
077000010330     C*----------------------------------------------------*
077100040119     C     INZVAR        BEGSR
077200010330     C*
077300030822     C* Inizializzo variabili di wrk
077400140415     C                   SETOFF                                       313233
077500090916     C                   Z-ADD     *zeros        Num5_0            5 0
077600090916     C                   MOVEL     *blanks       wNOTE            70
077700060331     C                   MOVEL     *blanks       wVATNOT_A
077800060331     C                   MOVEL     *blanks       wVATNOT_B
077900141125     C                   MOVEL     *blanks       wVATNOT_S
078000141125     C                   MOVEL     *blanks       wVATNOT_IJ
078100090904     C                   MOVEL     *blanks       wVATNOT_E
078200111117     C                   MOVEL     *blanks       wVATNOT_E_1
078300111117     C                   MOVEL     *blanks       wVATNOT_E_2
078400111118     C                   MOVEL     *blanks       wVATNOT_E_3
078500111118     C                   MOVEL     *blanks       wVATNOT_E_4
078600170213     C                   Z-ADD     *zeros        savNAZISO         3 0
078700170213     C                   MOVEL     *blanks       savNAZISORCV      3
078800030822     C*
078900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
079000090911     C                   CLEAR                   EDIVAB00
079100090911     C                   CLEAR                   EDIVAT00
079200030822     C                   CLEAR                   SkSplitCSV
079300170213     C                   EXSR      inzwrk
079400030822     C*
079500030822     C* Reimposto i valori di default
079600030822     C                   EXSR      DEFCAM
079700030822     C*
079800010330     C                   ENDSR
079900010330     C*----------------------------------------------------*
080000170213     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
080100010330     C*----------------------------------------------------*
080200010330     C     DEFCAM        BEGSR
080300030822     C*
080400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
080500030715     C* e delimitatore testo.
080600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
080700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
080800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
080900030822     C*
081000150203     C* Se richiesta esclusione Estero
081100150203     C                   SETOFF                                       50
081200150203     C                   IF        %subst(vlrppt:1:1) = 'E'
081300150203     C                   SETON                                        50
081400030715     C                   ENDIF
081500090421     C*
081600170213     C* Di default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
081700090421     C                   EVAL      VABCBO = '1'
081800090421     C                   EVAL      VABLNP = vlrpoi
081900090421     C                   EVAL      VABNSP = rrnum
082000030822     C*
082100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
082200030822     C                   EVAL      posDaDft = 1
082300030822     C                   EVAL      posADft  = 0
082400030822     C                   EVAL      wGiroDft = 0
082500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
082600030822     C                             posDaDft > 0
082700030822     C*
082800030822     C* Gestisco il 1° giro
082900030822     C                   IF        wGiroDft = 0
083000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
083100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
083200030822     C* Incremento il contatore dei "giri"
083300030822     C                   EVAL      wGiroDft = 1
083400030822     C                   ELSE
083500030822     C                   EVAL      posDaDft = posADft
083600030822     C                   ENDIF
083700030822     C* Eseguo lo scan x trovare la fine del campo corrente
083800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
083900030822     C*
084000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
084100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
084200030822     C                   IF        posDaDft > 0 AND
084300030822     C                             posADft  > 0
084400030822     C* NCL
084500030822     C                   IF        %subst(
084600030822     C                             %subst(vlrppt:posDaDft+1:
084700030822     C                             posADft-posDaDft-1):1:3)
084800030822     C                             = 'NCL'
084900030822     C                   EVAL      PiStr=%trim(%subst(
085000030822     C                             %subst(vlrppt:posDaDft+1:
085100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085200030822     C                   EXSR      CHKNUM
085300030822     C                   IF        PiInt=*on
085400030822     C                   Z-ADD     PiVal         VABNCL
085500030822     C                   ENDIF
085600030822     C                   ENDIF
085700030822     C* CCM
085800030822     C                   IF        %subst(
085900030822     C                             %subst(vlrppt:posDaDft+1:
086000030822     C                             posADft-posDaDft-1):1:3)
086100030822     C                             = 'CCM'
086200030822     C                   EVAL      PiStr=%trim(%subst(
086300030822     C                             %subst(vlrppt:posDaDft+1:
086400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
086500030822     C                   EXSR      CHKNUM
086600030822     C                   IF        PiInt=*on
086700030822     C                   Z-ADD     PiVal         VABCCM
086800030822     C                   ENDIF
086900030822     C                   ENDIF
087000030822     C* LNP
087100030822     C                   IF        %subst(
087200030822     C                             %subst(vlrppt:posDaDft+1:
087300030822     C                             posADft-posDaDft-1):1:3)
087400030822     C                             = 'LNP'
087500030822     C                   EVAL      PiStr=%trim(%subst(
087600030822     C                             %subst(vlrppt:posDaDft+1:
087700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087800030822     C                   EXSR      CHKNUM
087900030822     C                   IF        PiInt=*on
088000030822     C                   Z-ADD     PiVal         VABLNP
088100030822     C                   ENDIF
088200030822     C                   ENDIF
088300030822     C* NRS
088400030822     C                   IF        %subst(
088500030822     C                             %subst(vlrppt:posDaDft+1:
088600030822     C                             posADft-posDaDft-1):1:3)
088700030822     C                             = 'NRS'
088800030822     C                   EVAL      PiStr=%trim(%subst(
088900030822     C                             %subst(vlrppt:posDaDft+1:
089000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089100030822     C                   EXSR      CHKNUM
089200030822     C                   IF        PiInt=*on
089300030822     C                   Z-ADD     PiVal         VABNRS
089400030822     C                   ENDIF
089500030822     C                   ENDIF
089600030822     C* CTR
089700030822     C                   IF        %subst(
089800030822     C                             %subst(vlrppt:posDaDft+1:
089900030822     C                             posADft-posDaDft-1):1:3)
090000030822     C                             = 'CTR'
090100030822     C                   EVAL      PiStr=%trim(%subst(
090200030822     C                             %subst(vlrppt:posDaDft+1:
090300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
090400030822     C                   EXSR      CHKNUM
090500030822     C                   IF        PiInt=*on
090600030822     C                   Z-ADD     PiVal         VABCTR
090700030822     C                   ENDIF
090800030822     C                   ENDIF
090900030822     C* PKB
091000030822     C                   IF        %subst(
091100030822     C                             %subst(vlrppt:posDaDft+1:
091200030822     C                             posADft-posDaDft-1):1:3)
091300030822     C                             = 'PKB'
091400030822     C                   EVAL      PiStr=%trim(%subst(
091500030822     C                             %subst(vlrppt:posDaDft+1:
091600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091700030822     C                   EXSR      CHKNUM
091800030822     C                   IF        PiNum=*on
091900030822     C                   Z-ADD     PiVal         VABPKB
092000030822     C                   ENDIF
092100030822     C                   ENDIF
092200140715     C* PKN
092300140715     C                   IF        %subst(
092400140715     C                             %subst(vlrppt:posDaDft+1:
092500140715     C                             posADft-posDaDft-1):1:3)
092600140715     C                             = 'PKN'
092700140715     C                   EVAL      PiStr=%trim(%subst(
092800140715     C                             %subst(vlrppt:posDaDft+1:
092900140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
093000140715     C                   EXSR      CHKNUM
093100140715     C                   IF        PiNum=*on
093200140715     C                   Z-ADD     PiVal         wVABPKN           7 1
093300140715     C                   ENDIF
093400140715     C                   ENDIF
093500030822     C* VLB
093600030822     C                   IF        %subst(
093700030822     C                             %subst(vlrppt:posDaDft+1:
093800030822     C                             posADft-posDaDft-1):1:3)
093900030822     C                             = 'VLB'
094000030822     C                   EVAL      PiStr=%trim(%subst(
094100030822     C                             %subst(vlrppt:posDaDft+1:
094200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
094300030822     C                   EXSR      CHKNUM
094400030822     C                   IF        PiNum=*on
094500030822     C                   Z-ADD     PiVal         VABVLB
094600030822     C                   ENDIF
094700030822     C                   ENDIF
094800030822     C* QFT
094900030822     C                   IF        %subst(
095000030822     C                             %subst(vlrppt:posDaDft+1:
095100030822     C                             posADft-posDaDft-1):1:3)
095200030822     C                             = 'QFT'
095300030822     C                   EVAL      PiStr=%trim(%subst(
095400030822     C                             %subst(vlrppt:posDaDft+1:
095500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
095600030822     C                   EXSR      CHKNUM
095700030822     C                   IF        PiNum=*on
095800030822     C                   Z-ADD     PiVal         VABQFT
095900030822     C                   ENDIF
096000030822     C                   ENDIF
096100030822     C* CBO
096200030822     C                   IF        %subst(
096300030822     C                             %subst(vlrppt:posDaDft+1:
096400030822     C                             posADft-posDaDft-1):1:3)
096500030822     C                             = 'CBO'
096600030822     C                   EVAL      VABCBO=%trim(%subst(
096700030822     C                             %subst(vlrppt:posDaDft+1:
096800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
096900030822     C                   ENDIF
097000030822     C* TSP
097100030822     C                   IF        %subst(
097200030822     C                             %subst(vlrppt:posDaDft+1:
097300030822     C                             posADft-posDaDft-1):1:3)
097400030822     C                             = 'TSP'
097500030822     C                   EVAL      VABTSP=%trim(%subst(
097600030822     C                             %subst(vlrppt:posDaDft+1:
097700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
097800030822     C                   ENDIF
097900030822     C* VAS
098000030822     C                   IF        %subst(
098100030822     C                             %subst(vlrppt:posDaDft+1:
098200030822     C                             posADft-posDaDft-1):1:3)
098300030822     C                             = 'VAS'
098400030822     C                   EVAL      VABVAS=%trim(%subst(
098500030822     C                             %subst(vlrppt:posDaDft+1:
098600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
098700030822     C                   ENDIF
098800030822     C* VCA
098900030822     C                   IF        %subst(
099000030822     C                             %subst(vlrppt:posDaDft+1:
099100030822     C                             posADft-posDaDft-1):1:3)
099200030822     C                             = 'VCA'
099300030822     C                   EVAL      VABVCA=%trim(%subst(
099400030822     C                             %subst(vlrppt:posDaDft+1:
099500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
099600030822     C                   ENDIF
099700070925     C* TIC
099800070925     C                   IF        %subst(
099900070925     C                             %subst(vlrppt:posDaDft+1:
100000070925     C                             posADft-posDaDft-1):1:3)
100100070925     C                             = 'TIC'
100200070925     C                   EVAL      VABTIC=%trim(%subst(
100300070925     C                             %subst(vlrppt:posDaDft+1:
100400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
100500070925     C                   ENDIF
100600030822     C* GCA
100700030822     C                   IF        %subst(
100800030822     C                             %subst(vlrppt:posDaDft+1:
100900030822     C                             posADft-posDaDft-1):1:3)
101000030822     C                             = 'GCA'
101100030822     C                   EVAL      VABGCA=%trim(%subst(
101200030822     C                             %subst(vlrppt:posDaDft+1:
101300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
101400030822     C                   ENDIF
101500030822     C* CTM
101600030822     C                   IF        %subst(
101700030822     C                             %subst(vlrppt:posDaDft+1:
101800030822     C                             posADft-posDaDft-1):1:3)
101900030822     C                             = 'CTM'
102000030822     C                   EVAL      VABCTM=%trim(%subst(
102100030822     C                             %subst(vlrppt:posDaDft+1:
102200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
102300030822     C                   ENDIF
102400030822     C* FFD
102500030822     C                   IF        %subst(
102600030822     C                             %subst(vlrppt:posDaDft+1:
102700030822     C                             posADft-posDaDft-1):1:3)
102800030822     C                             = 'FFD'
102900030822     C                   EVAL      VABFFD=%trim(%subst(
103000030822     C                             %subst(vlrppt:posDaDft+1:
103100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
103200030822     C                   ENDIF
103300030822     C* VAD
103400030822     C                   IF        %subst(
103500030822     C                             %subst(vlrppt:posDaDft+1:
103600030822     C                             posADft-posDaDft-1):1:3)
103700030822     C                             = 'VAD'
103800030822     C                   EVAL      VABVAD=%trim(%subst(
103900030822     C                             %subst(vlrppt:posDaDft+1:
104000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
104100030822     C                   ENDIF
104200030822     C* GMA
104300030822     C                   IF        %subst(
104400030822     C                             %subst(vlrppt:posDaDft+1:
104500030822     C                             posADft-posDaDft-1):1:3)
104600030822     C                             = 'GMA'
104700030822     C                   EVAL      VABGMA=%trim(%subst(
104800030822     C                             %subst(vlrppt:posDaDft+1:
104900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
105000030822     C                   ENDIF
105100030822     C* GGA
105200030822     C                   IF        %subst(
105300030822     C                             %subst(vlrppt:posDaDft+1:
105400030822     C                             posADft-posDaDft-1):1:3)
105500030822     C                             = 'GGA'
105600030822     C                   EVAL      VABGGA=%trim(%subst(
105700030822     C                             %subst(vlrppt:posDaDft+1:
105800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
105900030822     C                   ENDIF
106000030822     C* GVA
106100030822     C                   IF        %subst(
106200030822     C                             %subst(vlrppt:posDaDft+1:
106300030822     C                             posADft-posDaDft-1):1:3)
106400030822     C                             = 'GVA'
106500030822     C                   EVAL      VABGVA=%trim(%subst(
106600030822     C                             %subst(vlrppt:posDaDft+1:
106700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
106800030822     C                   ENDIF
106900030822     C* TC1
107000030822     C                   IF        %subst(
107100030822     C                             %subst(vlrppt:posDaDft+1:
107200030822     C                             posADft-posDaDft-1):1:3)
107300030822     C                             = 'TC1'
107400030822     C                   EVAL      VABTC1=%trim(%subst(
107500030822     C                             %subst(vlrppt:posDaDft+1:
107600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
107700030822     C                   ENDIF
107800030822     C* TC2
107900030822     C                   IF        %subst(
108000030822     C                             %subst(vlrppt:posDaDft+1:
108100030822     C                             posADft-posDaDft-1):1:3)
108200030822     C                             = 'TC2'
108300030822     C                   EVAL      VABTC2=%trim(%subst(
108400030822     C                             %subst(vlrppt:posDaDft+1:
108500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
108600030822     C                   ENDIF
108700040714     C* VATTRC
108800040714     C                   IF        %subst(
108900040714     C                             %subst(vlrppt:posDaDft+1:
109000040714     C                             posADft-posDaDft-1):1:3)
109100040714     C                             = 'TRC'
109200040714     C                   EVAL      VATTRC=%trim(%subst(
109300040714     C                             %subst(vlrppt:posDaDft+1:
109400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
109500040714     C                   ENDIF
109600030822     C* ...
109700030822     C                   ENDIF
109800030822     C                   ENDDO
109900020204     C*
110000010330     C                   ENDSR
110100170213
110200170213
110300170213
110400010607     C*----------------------------------------------------*
110500170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
110600010607     C*----------------------------------------------------*
110700170213     C     IMPVABSHP     BEGSR
110800030515     C*
110900010607     C                   MOVEL     datcor        VABAAS
111000010607     C                   MOVE      datcor        VABMGS
111100010607     C*
111200030715     C**********
111300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
111400030715     C**********
111500030715     C                   IF        wGiro = *zeros
111600080716     C*
111700080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
111800080716     C                   EVAL      depvinDTA = vinDTA
111900040113     C*
112000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
112100170213     C                   IF        shipment <> *blanks
112200170213     C                   EVAL      vindta = shipment
112300070823     C     ';':CharCSV   XLATE     vindta        vindta
112400040510     C                   ENDIF
112500080626     C*
112600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
112700031201     C     minu:maiu     XLATE     vindta        vindta
112800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
112900030715     C                   DOW       posDa <= %len(%trim(vindta))
113000030715     C*
113100030715     C* Gestisco il 1° campo
113200030715     C                   IF        i = 1
113300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
113400030715     C                   EVAL      posDa = 1
113500030715     C* Eseguo lo scan x trovare la fine del primo campo
113600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113700030715     C* A questo punto "estraggo" il campo corrente
113800170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
113900110923     C                                         %subst(vindta:posDa:(posA-posDa))
114000030715     C* X i campi successivi al 1°
114100030715     C                   ELSE
114200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
114300030715     C                   EVAL      posDa = posA + 1
114400030715     C* Eseguo lo scan x trovare la fine del campo corrente
114500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
114600030715     C* Gestisco l'ultimo campo
114700030715     C                   IF        posA = *zeros
114800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
114900030715     C* A questo punto "estraggo" il campo corrente
115000170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
115100110923     C                                         %subst(vindta:posDa:(posA-posDa))
115200030715     C                   LEAVE
115300030715     C                   ELSE
115400030715     C* A questo punto "estraggo" il campo corrente
115500170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
115600110923     C                                         %subst(vindta:posDa:(posA-posDa))
115700030715     C                   ENDIF
115800030715     C                   ENDIF
115900030715     C* Incremento il contatore d campo
116000030715     C                   EVAL      i = i +1
116100030715     C                   ENDDO
116200030715     C*
116300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
116400030715     C* delimitazione testo
116500030715     C                   EVAL      i = 1
116600170213     C                   DOW       i < %elem(SkSplitFSHP)
116700170213     C     CharTXT:' '   XLATE     SkSplitFSHP(i)SkSplitFSHP(i)
116800170213     C                   EVAL      SkSplitFSHP(i) = %trim(SkSplitFSHP(i))
116900030715     C                   EVAL      i = i + 1
117000030715     C                   ENDDO
117100030715     C                   EVAL      wGiro = 1
117200080716     C* Ripristino la 1° riga originale
117300080716     C                   EVAL      vinDTA = depvinDTA
117400030715     C                   ELSE
117500030715     C                   EVAL      wGiro = 2
117600031204     C**********
117700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
117800170213     C* di separatore campo
117900031204     C**********
118000031204     C                   EVAL      vindta = %trim(vindta)
118100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
118200031204     C                   EVAL      vindta = %subst(vindta:2)
118300031204     C                   ENDDO
118400031204     C*
118500031204     C                   Z-ADD     *zeros        lunghInput        4 0
118600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
118700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
118800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
118900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
119000031204     C                   ENDDO
119100030715     C**********
119200030715     C* Effettuo lo split del campo dati d input
119300030715     C**********
119400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
119500030715     C                   DOW       posDa <= %len(%trim(vindta))
119600030715     C*
119700030715     C* Gestisco il 1° campo
119800030715     C                   IF        i = 1
119900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
120000030715     C                   EVAL      posDa = 1
120100030715     C* Eseguo lo scan x trovare la fine del primo campo
120200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
120300030715     C* A questo punto "estraggo" il campo corrente
120400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
120500030715     C                                                    (posA-posDa))
120600030715     C* X i campi successivi al 1°
120700030715     C                   ELSE
120800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
120900030715     C                   EVAL      posDa = posA + 1
121000031204     C* Verifico che nn vi sia il campo nn valorizzato
121100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
121200031204     C* Se campo nn valorizzato skippo al prossimo
121300031204     C                   EVAL      SkSplitCSV(i) = *blanks
121400031204     C                   EVAL      posA  = posA + 1
121500031204     C                   ELSE
121600030715     C* Eseguo lo scan x trovare la fine del campo corrente
121700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
121800030715     C* Gestisco l'ultimo campo
121900030715     C                   IF        posA = *zeros
122000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
122100030715     C* A questo punto "estraggo" il campo corrente
122200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
122300030715     C                                                    (posA-posDa))
122400030715     C                   LEAVE
122500030715     C                   ELSE
122600030715     C* A questo punto "estraggo" il campo corrente
122700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
122800030715     C                                                    (posA-posDa))
122900030715     C                   ENDIF
123000031204     C                   ENDIF
123100030715     C                   ENDIF
123200030715     C* Incremento il contatore d campo
123300030715     C                   EVAL      i = i +1
123400030715     C                   ENDDO
123500030715     C*
123600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
123700030715     C* delimitazione testo
123800030715     C                   EVAL      i = 1
123900030715     C                   DOW       i < %elem(SkSplitCSV)
124000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
124100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
124200030715     C                   EVAL      i = i + 1
124300030715     C                   ENDDO
124400170215     C*
124500170215     C* Inizializzazioni "specifiche"
124600170215     C                   exsr      INZVALSHP
124700030715     C*
124800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
124900030715     C                   EVAL      i = 1
125000170213     C                   DOW       i < %elem(SkSplitFSHP)
125100030715     C*
125200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
125300170213     C                   IF        SkSplitFSHP(i) <> *blanks AND
125400090623     C                             SkSplitCSV(i) <> *blanks
125500090623     C*
125600170213     C                   exsr      CARVALSHP
125700040510     C*
125800030715     C                   ENDIF
125900030715     C                   EVAL      i = i + 1
126000030715     C                   ENDDO
126100070925     C*
126200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
126300080125     C*
126400140715     C*
126500170213     C* Se peso non indicato nei dati cleinte e richiesto un peso forzato per collo
126600140715     C                   IF        VABPKB = *zeros
126700170213     C                   EVAL      VABPKB = wVABPKN * VABNCL
126800140715     C                   ENDIF
126900040510     C*
127000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127100170213     C                   IF        VABNSP = *zeros
127200040510     C                   SETON                                        31
127300040510     C                   ENDIF
127400040715     C*
127500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
127600040715     C                   IF        VABLNP = *zeros
127700040715     C                   SETON                                        31
127800040715     C                   ENDIF
127900040510     C*
128000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
128100040510     C                   IF        VABRMN = *zeros
128200040510     C                   Z-ADD     VABNSP        VABRMN
128300040510     C                   ENDIF
128400040510     C*
128500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
128600040510     C                   IF        VABRMA = *blanks
128700040714     C                   MOVEL     VABRMN        VABRMA
128800040510     C                   ENDIF
128900170213     C*
129000170213     C***  <===  -----------------------------------------
129100030715     C                   ENDIF
129200020204     C*
129300010607     C                   ENDSR
129400010607     C*----------------------------------------------------*
129500170213
129600170213
129700170213
129800170213     C*----------------------------------------------------*
129900170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
130000170213     C*----------------------------------------------------*
130100170213     C     IMPVABSND     BEGSR
130200170213     C*
130300170213     C**********
130400170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
130500170213     C**********
130600170213     C                   IF        wGiro = *zeros
130700170213     C*
130800170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
130900170213     C                   EVAL      depvinDTA = vinDTA
131000170213     C*
131100170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
131200170213     C                   IF        sender <> *blanks
131300170213     C                   EVAL      vindta = sender
131400170213     C     ';':CharCSV   XLATE     vindta        vindta
131500170213     C                   ENDIF
131600170213     C*
131700170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
131800170213     C     minu:maiu     XLATE     vindta        vindta
131900170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
132000170213     C                   DOW       posDa <= %len(%trim(vindta))
132100170213     C*
132200170213     C* Gestisco il 1° campo
132300170213     C                   IF        i = 1
132400170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
132500170213     C                   EVAL      posDa = 1
132600170213     C* Eseguo lo scan x trovare la fine del primo campo
132700170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
132800170213     C* A questo punto "estraggo" il campo corrente
132900170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
133000170213     C                                         %subst(vindta:posDa:(posA-posDa))
133100170213     C* X i campi successivi al 1°
133200170213     C                   ELSE
133300170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
133400170213     C                   EVAL      posDa = posA + 1
133500170213     C* Eseguo lo scan x trovare la fine del campo corrente
133600170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
133700170213     C* Gestisco l'ultimo campo
133800170213     C                   IF        posA = *zeros
133900170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
134000170213     C* A questo punto "estraggo" il campo corrente
134100170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
134200170213     C                                         %subst(vindta:posDa:(posA-posDa))
134300170213     C                   LEAVE
134400170213     C                   ELSE
134500170213     C* A questo punto "estraggo" il campo corrente
134600170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
134700170213     C                                         %subst(vindta:posDa:(posA-posDa))
134800170213     C                   ENDIF
134900170213     C                   ENDIF
135000170213     C* Incremento il contatore d campo
135100170213     C                   EVAL      i = i +1
135200170213     C                   ENDDO
135300170213     C*
135400170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
135500170213     C* delimitazione testo
135600170213     C                   EVAL      i = 1
135700170213     C                   DOW       i < %elem(SkSplitFSND)
135800170213     C     CharTXT:' '   XLATE     SkSplitFSND(i)SkSplitFSND(i)
135900170213     C                   EVAL      SkSplitFSND(i) = %trim(SkSplitFSND(i))
136000170213     C                   EVAL      i = i + 1
136100170213     C                   ENDDO
136200170213     C                   EVAL      wGiro = 1
136300170213     C* Ripristino la 1° riga originale
136400170213     C                   EVAL      vinDTA = depvinDTA
136500170213     C                   ELSE
136600170213     C                   EVAL      wGiro = 2
136700170213     C**********
136800170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
136900170213     C* d separatore campo
137000170213     C**********
137100170213     C                   EVAL      vindta = %trim(vindta)
137200170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
137300170213     C                   EVAL      vindta = %subst(vindta:2)
137400170213     C                   ENDDO
137500170213     C*
137600170213     C                   Z-ADD     *zeros        lunghInput        4 0
137700170213     C                   EVAL      lunghInput = %len(%trim(vindta))
137800170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
137900170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
138000170213     C                   EVAL      lunghInput = %len(%trim(vindta))
138100170213     C                   ENDDO
138200170213     C**********
138300170213     C* Effettuo lo split del campo dati d input
138400170213     C**********
138500170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
138600170213     C                   DOW       posDa <= %len(%trim(vindta))
138700170213     C*
138800170213     C* Gestisco il 1° campo
138900170213     C                   IF        i = 1
139000170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
139100170213     C                   EVAL      posDa = 1
139200170213     C* Eseguo lo scan x trovare la fine del primo campo
139300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
139400170213     C* A questo punto "estraggo" il campo corrente
139500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
139600170213     C                                                    (posA-posDa))
139700170213     C* X i campi successivi al 1°
139800170213     C                   ELSE
139900170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
140000170213     C                   EVAL      posDa = posA + 1
140100170213     C* Verifico che nn vi sia il campo nn valorizzato
140200170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
140300170213     C* Se campo nn valorizzato skippo al prossimo
140400170213     C                   EVAL      SkSplitCSV(i) = *blanks
140500170213     C                   EVAL      posA  = posA + 1
140600170213     C                   ELSE
140700170213     C* Eseguo lo scan x trovare la fine del campo corrente
140800170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
140900170213     C* Gestisco l'ultimo campo
141000170213     C                   IF        posA = *zeros
141100170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
141200170213     C* A questo punto "estraggo" il campo corrente
141300170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
141400170213     C                                                    (posA-posDa))
141500170213     C                   LEAVE
141600170213     C                   ELSE
141700170213     C* A questo punto "estraggo" il campo corrente
141800170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
141900170213     C                                                    (posA-posDa))
142000170213     C                   ENDIF
142100170213     C                   ENDIF
142200170213     C                   ENDIF
142300170213     C* Incremento il contatore d campo
142400170213     C                   EVAL      i = i +1
142500170213     C                   ENDDO
142600170213     C*
142700170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
142800170213     C* delimitazione testo
142900170213     C                   EVAL      i = 1
143000170213     C                   DOW       i < %elem(SkSplitCSV)
143100170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
143200170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
143300170213     C                   EVAL      i = i + 1
143400170213     C                   ENDDO
143500170215     C*
143600170215     C* Inizializzazioni "specifiche"
143700170215     C                   exsr      INZVALSND
143800170213     C*
143900170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
144000170213     C                   EVAL      i = 1
144100170213     C                   DOW       i < %elem(SkSplitFSND)
144200170213     C*
144300170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
144400170213     C                   IF        SkSplitFSND(i) <> *blanks AND
144500170213     C                             SkSplitCSV(i) <> *blanks
144600170213     C*
144700170213     C                   exsr      CARVALSND
144800170213     C*
144900170213     C                   ENDIF
145000170213     C                   EVAL      i = i + 1
145100170213     C                   ENDDO
145200170213     C*
145300170213     C***  ===>  Gestione campi "particolari" fuori ciclo
145400170213     C*
145500170213     C*
145600170213     C***  <===  -----------------------------------------
145700170213     C*
145800170213     C                   ENDIF
145900170213     C*
146000170213     C                   ENDSR
146100170213     C*----------------------------------------------------*
146200170213
146300170213
146400170213
146500170213     C*----------------------------------------------------*
146600170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
146700170213     C*----------------------------------------------------*
146800170213     C     IMPVABRCV     BEGSR
146900170213     C*
147000170213     C**********
147100170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
147200170213     C**********
147300170213     C                   IF        wGiro = *zeros
147400170213     C*
147500170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
147600170213     C                   EVAL      depvinDTA = vinDTA
147700170213     C*
147800170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
147900170213     C                   IF        receiver <> *blanks
148000170213     C                   EVAL      vindta = receiver
148100170213     C     ';':CharCSV   XLATE     vindta        vindta
148200170213     C                   ENDIF
148300170213     C*
148400170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
148500170213     C     minu:maiu     XLATE     vindta        vindta
148600170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
148700170213     C                   DOW       posDa <= %len(%trim(vindta))
148800170213     C*
148900170213     C* Gestisco il 1° campo
149000170213     C                   IF        i = 1
149100170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
149200170213     C                   EVAL      posDa = 1
149300170213     C* Eseguo lo scan x trovare la fine del primo campo
149400170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
149500170213     C* A questo punto "estraggo" il campo corrente
149600170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
149700170213     C                                         %subst(vindta:posDa:(posA-posDa))
149800170213     C* X i campi successivi al 1°
149900170213     C                   ELSE
150000170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
150100170213     C                   EVAL      posDa = posA + 1
150200170213     C* Eseguo lo scan x trovare la fine del campo corrente
150300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
150400170213     C* Gestisco l'ultimo campo
150500170213     C                   IF        posA = *zeros
150600170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
150700170213     C* A questo punto "estraggo" il campo corrente
150800170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
150900170213     C                                         %subst(vindta:posDa:(posA-posDa))
151000170213     C                   LEAVE
151100170213     C                   ELSE
151200170213     C* A questo punto "estraggo" il campo corrente
151300170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
151400170213     C                                         %subst(vindta:posDa:(posA-posDa))
151500170213     C                   ENDIF
151600170213     C                   ENDIF
151700170213     C* Incremento il contatore d campo
151800170213     C                   EVAL      i = i +1
151900170213     C                   ENDDO
152000170213     C*
152100170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
152200170213     C* delimitazione testo
152300170213     C                   EVAL      i = 1
152400170213     C                   DOW       i < %elem(SkSplitFRCV)
152500170213     C     CharTXT:' '   XLATE     SkSplitFRCV(i)SkSplitFRCV(i)
152600170213     C                   EVAL      SkSplitFRCV(i) = %trim(SkSplitFRCV(i))
152700170213     C                   EVAL      i = i + 1
152800170213     C                   ENDDO
152900170213     C                   EVAL      wGiro = 1
153000170213     C* Ripristino la 1° riga originale
153100170213     C                   EVAL      vinDTA = depvinDTA
153200170213     C                   ELSE
153300170213     C                   EVAL      wGiro = 2
153400170213     C**********
153500170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
153600170213     C* d separatore campo
153700170213     C**********
153800170213     C                   EVAL      vindta = %trim(vindta)
153900170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
154000170213     C                   EVAL      vindta = %subst(vindta:2)
154100170213     C                   ENDDO
154200170213     C*
154300170213     C                   Z-ADD     *zeros        lunghInput        4 0
154400170213     C                   EVAL      lunghInput = %len(%trim(vindta))
154500170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
154600170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
154700170213     C                   EVAL      lunghInput = %len(%trim(vindta))
154800170213     C                   ENDDO
154900170213     C**********
155000170213     C* Effettuo lo split del campo dati d input
155100170213     C**********
155200170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
155300170213     C                   DOW       posDa <= %len(%trim(vindta))
155400170213     C*
155500170213     C* Gestisco il 1° campo
155600170213     C                   IF        i = 1
155700170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
155800170213     C                   EVAL      posDa = 1
155900170213     C* Eseguo lo scan x trovare la fine del primo campo
156000170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
156100170213     C* A questo punto "estraggo" il campo corrente
156200170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
156300170213     C                                                    (posA-posDa))
156400170213     C* X i campi successivi al 1°
156500170213     C                   ELSE
156600170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
156700170213     C                   EVAL      posDa = posA + 1
156800170213     C* Verifico che nn vi sia il campo nn valorizzato
156900170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
157000170213     C* Se campo nn valorizzato skippo al prossimo
157100170213     C                   EVAL      SkSplitCSV(i) = *blanks
157200170213     C                   EVAL      posA  = posA + 1
157300170213     C                   ELSE
157400170213     C* Eseguo lo scan x trovare la fine del campo corrente
157500170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
157600170213     C* Gestisco l'ultimo campo
157700170213     C                   IF        posA = *zeros
157800170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
157900170213     C* A questo punto "estraggo" il campo corrente
158000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
158100170213     C                                                    (posA-posDa))
158200170213     C                   LEAVE
158300170213     C                   ELSE
158400170213     C* A questo punto "estraggo" il campo corrente
158500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
158600170213     C                                                    (posA-posDa))
158700170213     C                   ENDIF
158800170213     C                   ENDIF
158900170213     C                   ENDIF
159000170213     C* Incremento il contatore d campo
159100170213     C                   EVAL      i = i +1
159200170213     C                   ENDDO
159300170213     C*
159400170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
159500170213     C* delimitazione testo
159600170213     C                   EVAL      i = 1
159700170213     C                   DOW       i < %elem(SkSplitCSV)
159800170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
159900170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
160000170213     C                   EVAL      i = i + 1
160100170213     C                   ENDDO
160200170215     C*
160300170215     C* Inizializzazioni "specifiche"
160400170215     C                   exsr      INZVALRCV
160500170213     C*
160600170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
160700170213     C                   EVAL      i = 1
160800170213     C                   DOW       i < %elem(SkSplitFRCV)
160900170213     C*
161000170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
161100170213     C                   IF        SkSplitFRCV(i) <> *blanks AND
161200170213     C                             SkSplitCSV(i) <> *blanks
161300170213     C*
161400170213     C                   exsr      CARVALRCV
161500170213     C*
161600170213     C                   ENDIF
161700170213     C                   EVAL      i = i + 1
161800170213     C                   ENDDO
161900170213     C*
162000170213     C***  ===>  Gestione campi "particolari" fuori ciclo
162100170213     C*
162200170213     C* Se provincia non valorizzata la reperisco
162300170213     C* tramite TISI95R a seconda dei dati d instradamento presenti
162400170215     C                   IF        VABNZD = *blanks AND
162500170215     C                             VABPRD = *blanks
162600170213     C                   CLEAR                   TISI95DS
162700170213     C                   EVAL      I95TCN = '3'
162800170213     C                   Z-ADD     datcor        I95DAT
162900170213     C                   EVAL      I95NAR = VABNZD
163000170213     C                   EVAL      I95CAP = VABCAD
163100170213     C                   EVAL      I95LOC = VABLOD
163200170213     C                   CALL      'TISI95R'
163300170213     C                   PARM                    TISI95DS
163400170213     C                   EVAL      VABPRD = O95PRV
163500170213     C                   ENDIF
163600170213     C*
163700170213     C***  <===  -----------------------------------------
163800170213     C*
163900170213     C                   ENDIF
164000170213     C*
164100170213     C                   ENDSR
164200170213     C*----------------------------------------------------*
164300170213
164400170213
164500170213
164600170213     C*----------------------------------------------------*
164700170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
164800170213     C*----------------------------------------------------*
164900170213     C     IMPVABDLV     BEGSR
165000170213     C*
165100170213     C**********
165200170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
165300170213     C**********
165400170213     C                   IF        wGiro = *zeros
165500170213     C*
165600170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
165700170213     C                   EVAL      depvinDTA = vinDTA
165800170213     C*
165900170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
166000170213     C                   IF        delivery <> *blanks
166100170213     C                   EVAL      vindta = delivery
166200170213     C     ';':CharCSV   XLATE     vindta        vindta
166300170213     C                   ENDIF
166400170213     C*
166500170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
166600170213     C     minu:maiu     XLATE     vindta        vindta
166700170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
166800170213     C                   DOW       posDa <= %len(%trim(vindta))
166900170213     C*
167000170213     C* Gestisco il 1° campo
167100170213     C                   IF        i = 1
167200170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
167300170213     C                   EVAL      posDa = 1
167400170213     C* Eseguo lo scan x trovare la fine del primo campo
167500170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
167600170213     C* A questo punto "estraggo" il campo corrente
167700170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
167800170213     C                                         %subst(vindta:posDa:(posA-posDa))
167900170213     C* X i campi successivi al 1°
168000170213     C                   ELSE
168100170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
168200170213     C                   EVAL      posDa = posA + 1
168300170213     C* Eseguo lo scan x trovare la fine del campo corrente
168400170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
168500170213     C* Gestisco l'ultimo campo
168600170213     C                   IF        posA = *zeros
168700170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
168800170213     C* A questo punto "estraggo" il campo corrente
168900170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
169000170213     C                                         %subst(vindta:posDa:(posA-posDa))
169100170213     C                   LEAVE
169200170213     C                   ELSE
169300170213     C* A questo punto "estraggo" il campo corrente
169400170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
169500170213     C                                         %subst(vindta:posDa:(posA-posDa))
169600170213     C                   ENDIF
169700170213     C                   ENDIF
169800170213     C* Incremento il contatore d campo
169900170213     C                   EVAL      i = i +1
170000170213     C                   ENDDO
170100170213     C*
170200170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
170300170213     C* delimitazione testo
170400170213     C                   EVAL      i = 1
170500170213     C                   DOW       i < %elem(SkSplitFDLV)
170600170213     C     CharTXT:' '   XLATE     SkSplitFDLV(i)SkSplitFDLV(i)
170700170213     C                   EVAL      SkSplitFDLV(i) = %trim(SkSplitFDLV(i))
170800170213     C                   EVAL      i = i + 1
170900170213     C                   ENDDO
171000170213     C                   EVAL      wGiro = 1
171100170213     C* Ripristino la 1° riga originale
171200170213     C                   EVAL      vinDTA = depvinDTA
171300170213     C                   ELSE
171400170213     C                   EVAL      wGiro = 2
171500170213     C**********
171600170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
171700170213     C* d separatore campo
171800170213     C**********
171900170213     C                   EVAL      vindta = %trim(vindta)
172000170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
172100170213     C                   EVAL      vindta = %subst(vindta:2)
172200170213     C                   ENDDO
172300170213     C*
172400170213     C                   Z-ADD     *zeros        lunghInput        4 0
172500170213     C                   EVAL      lunghInput = %len(%trim(vindta))
172600170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
172700170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
172800170213     C                   EVAL      lunghInput = %len(%trim(vindta))
172900170213     C                   ENDDO
173000170213     C**********
173100170213     C* Effettuo lo split del campo dati d input
173200170213     C**********
173300170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
173400170213     C                   DOW       posDa <= %len(%trim(vindta))
173500170213     C*
173600170213     C* Gestisco il 1° campo
173700170213     C                   IF        i = 1
173800170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
173900170213     C                   EVAL      posDa = 1
174000170213     C* Eseguo lo scan x trovare la fine del primo campo
174100170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
174200170213     C* A questo punto "estraggo" il campo corrente
174300170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
174400170213     C                                                    (posA-posDa))
174500170213     C* X i campi successivi al 1°
174600170213     C                   ELSE
174700170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
174800170213     C                   EVAL      posDa = posA + 1
174900170213     C* Verifico che nn vi sia il campo nn valorizzato
175000170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
175100170213     C* Se campo nn valorizzato skippo al prossimo
175200170213     C                   EVAL      SkSplitCSV(i) = *blanks
175300170213     C                   EVAL      posA  = posA + 1
175400170213     C                   ELSE
175500170213     C* Eseguo lo scan x trovare la fine del campo corrente
175600170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
175700170213     C* Gestisco l'ultimo campo
175800170213     C                   IF        posA = *zeros
175900170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
176000170213     C* A questo punto "estraggo" il campo corrente
176100170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
176200170213     C                                                    (posA-posDa))
176300170213     C                   LEAVE
176400170213     C                   ELSE
176500170213     C* A questo punto "estraggo" il campo corrente
176600170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
176700170213     C                                                    (posA-posDa))
176800170213     C                   ENDIF
176900170213     C                   ENDIF
177000170213     C                   ENDIF
177100170213     C* Incremento il contatore d campo
177200170213     C                   EVAL      i = i +1
177300170213     C                   ENDDO
177400170213     C*
177500170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
177600170213     C* delimitazione testo
177700170213     C                   EVAL      i = 1
177800170213     C                   DOW       i < %elem(SkSplitCSV)
177900170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
178000170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
178100170213     C                   EVAL      i = i + 1
178200170213     C                   ENDDO
178300170215     C*
178400170215     C* Inizializzazioni "specifiche"
178500170215     C                   exsr      INZVALDLV
178600170213     C*
178700170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
178800170213     C                   EVAL      i = 1
178900170213     C                   DOW       i < %elem(SkSplitFDLV)
179000170213     C*
179100170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
179200170213     C                   IF        SkSplitFDLV(i) <> *blanks AND
179300170213     C                             SkSplitCSV(i) <> *blanks
179400170213     C*
179500170213     C                   exsr      CARVALDLV
179600170213     C*
179700170213     C                   ENDIF
179800170213     C                   EVAL      i = i + 1
179900170213     C                   ENDDO
180000170213     C*
180100170213     C***  ===>  Gestione campi "particolari" fuori ciclo
180200170213     C*
180300170213     C*
180400170213     C***  <===  -----------------------------------------
180500170213     C*
180600170213     C                   ENDIF
180700170213     C*
180800170213     C                   ENDSR
180900170213     C*----------------------------------------------------*
181000170213
181100170213
181200170213
181300170213     C*----------------------------------------------------*
181400170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
181500170213     C*----------------------------------------------------*
181600170213     C     IMPVABCOD     BEGSR
181700170213     C*
181800170213     C**********
181900170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
182000170213     C**********
182100170213     C                   IF        wGiro = *zeros
182200170213     C*
182300170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
182400170213     C                   EVAL      depvinDTA = vinDTA
182500170213     C*
182600170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
182700170213     C                   IF        cod    <> *blanks
182800170213     C                   EVAL      vindta = cod
182900170213     C     ';':CharCSV   XLATE     vindta        vindta
183000170213     C                   ENDIF
183100170213     C*
183200170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
183300170213     C     minu:maiu     XLATE     vindta        vindta
183400170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
183500170213     C                   DOW       posDa <= %len(%trim(vindta))
183600170213     C*
183700170213     C* Gestisco il 1° campo
183800170213     C                   IF        i = 1
183900170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
184000170213     C                   EVAL      posDa = 1
184100170213     C* Eseguo lo scan x trovare la fine del primo campo
184200170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
184300170213     C* A questo punto "estraggo" il campo corrente
184400170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
184500170213     C                                         %subst(vindta:posDa:(posA-posDa))
184600170213     C* X i campi successivi al 1°
184700170213     C                   ELSE
184800170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
184900170213     C                   EVAL      posDa = posA + 1
185000170213     C* Eseguo lo scan x trovare la fine del campo corrente
185100170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
185200170213     C* Gestisco l'ultimo campo
185300170213     C                   IF        posA = *zeros
185400170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
185500170213     C* A questo punto "estraggo" il campo corrente
185600170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
185700170213     C                                         %subst(vindta:posDa:(posA-posDa))
185800170213     C                   LEAVE
185900170213     C                   ELSE
186000170213     C* A questo punto "estraggo" il campo corrente
186100170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
186200170213     C                                         %subst(vindta:posDa:(posA-posDa))
186300170213     C                   ENDIF
186400170213     C                   ENDIF
186500170213     C* Incremento il contatore d campo
186600170213     C                   EVAL      i = i +1
186700170213     C                   ENDDO
186800170213     C*
186900170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
187000170213     C* delimitazione testo
187100170213     C                   EVAL      i = 1
187200170213     C                   DOW       i < %elem(SkSplitFCOD)
187300170213     C     CharTXT:' '   XLATE     SkSplitFCOD(i)SkSplitFCOD(i)
187400170213     C                   EVAL      SkSplitFCOD(i) = %trim(SkSplitFCOD(i))
187500170213     C                   EVAL      i = i + 1
187600170213     C                   ENDDO
187700170213     C                   EVAL      wGiro = 1
187800170213     C* Ripristino la 1° riga originale
187900170213     C                   EVAL      vinDTA = depvinDTA
188000170213     C                   ELSE
188100170213     C                   EVAL      wGiro = 2
188200170213     C**********
188300170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
188400170213     C* d separatore campo
188500170213     C**********
188600170213     C                   EVAL      vindta = %trim(vindta)
188700170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
188800170213     C                   EVAL      vindta = %subst(vindta:2)
188900170213     C                   ENDDO
189000170213     C*
189100170213     C                   Z-ADD     *zeros        lunghInput        4 0
189200170213     C                   EVAL      lunghInput = %len(%trim(vindta))
189300170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
189400170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
189500170213     C                   EVAL      lunghInput = %len(%trim(vindta))
189600170213     C                   ENDDO
189700170213     C**********
189800170213     C* Effettuo lo split del campo dati d input
189900170213     C**********
190000170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
190100170213     C                   DOW       posDa <= %len(%trim(vindta))
190200170213     C*
190300170213     C* Gestisco il 1° campo
190400170213     C                   IF        i = 1
190500170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
190600170213     C                   EVAL      posDa = 1
190700170213     C* Eseguo lo scan x trovare la fine del primo campo
190800170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
190900170213     C* A questo punto "estraggo" il campo corrente
191000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
191100170213     C                                                    (posA-posDa))
191200170213     C* X i campi successivi al 1°
191300170213     C                   ELSE
191400170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
191500170213     C                   EVAL      posDa = posA + 1
191600170213     C* Verifico che nn vi sia il campo nn valorizzato
191700170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
191800170213     C* Se campo nn valorizzato skippo al prossimo
191900170213     C                   EVAL      SkSplitCSV(i) = *blanks
192000170213     C                   EVAL      posA  = posA + 1
192100170213     C                   ELSE
192200170213     C* Eseguo lo scan x trovare la fine del campo corrente
192300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
192400170213     C* Gestisco l'ultimo campo
192500170213     C                   IF        posA = *zeros
192600170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
192700170213     C* A questo punto "estraggo" il campo corrente
192800170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
192900170213     C                                                    (posA-posDa))
193000170213     C                   LEAVE
193100170213     C                   ELSE
193200170213     C* A questo punto "estraggo" il campo corrente
193300170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
193400170213     C                                                    (posA-posDa))
193500170213     C                   ENDIF
193600170213     C                   ENDIF
193700170213     C                   ENDIF
193800170213     C* Incremento il contatore d campo
193900170213     C                   EVAL      i = i +1
194000170213     C                   ENDDO
194100170213     C*
194200170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
194300170213     C* delimitazione testo
194400170213     C                   EVAL      i = 1
194500170213     C                   DOW       i < %elem(SkSplitCSV)
194600170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
194700170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
194800170213     C                   EVAL      i = i + 1
194900170213     C                   ENDDO
195000170215     C*
195100170215     C* Inizializzazioni "specifiche"
195200170215     C                   exsr      INZVALCOD
195300170213     C*
195400170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
195500170213     C                   EVAL      i = 1
195600170213     C                   DOW       i < %elem(SkSplitFCOD)
195700170213     C*
195800170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
195900170213     C                   IF        SkSplitFCOD(i) <> *blanks AND
196000170213     C                             SkSplitCSV(i) <> *blanks
196100170213     C*
196200170213     C                   exsr      CARVALCOD
196300170213     C*
196400170213     C                   ENDIF
196500170213     C                   EVAL      i = i + 1
196600170213     C                   ENDDO
196700170213     C*
196800170213     C***  ===>  Gestione campi "particolari" fuori ciclo
196900170213     C*
197000170213     C* Considerazioni finali su CBO/CAS
197100170213     C                   IF        wFlgCAS = '1' OR
197200170213     C                             VABCAS > *zeros
197300170213     C                   IF        VABCBO = '1'
197400170213     C                   EVAL      VABCBO = '4'
197500170213     C                   ENDIF
197600170213     C                   IF        VABCBO = '2'
197700170213     C                   EVAL      VABCBO = '6'
197800170213     C                   ENDIF
197900170213     C                   ENDIF
198000170213     C*
198100170213     C* Eseguo routine finale x considerazioni specifiche su importi/divise
198200170213     C                   EXSR      CHKIMPDIV
198300170213     C*
198400170213     C***  <===  -----------------------------------------
198500170213     C*
198600170213     C                   ENDIF
198700170213     C*
198800170213     C                   ENDSR
198900170213     C*----------------------------------------------------*
199000090623
199100090623
199200090623
199300090623     C*----------------------------------------------------*
199400101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
199500090623     C*----------------------------------------------------*
199600170213     C     IMPVATPCL     BEGSR
199700090623     C*
199800090623     C**********
199900090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
200000090623     C**********
200100090623     C                   IF        wGiro = *zeros
200200090623     C*
200300090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
200400090623     C                   EVAL      depvinDTA = vinDTA
200500090623     C*
200600090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
200700110923     C                   IF        parcel <> *blanks
200800110923     C                   EVAL      vindta = parcel
200900090623     C     ';':CharCSV   XLATE     vindta        vindta
201000090623     C                   ENDIF
201100090623     C*
201200090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
201300090623     C     minu:maiu     XLATE     vindta        vindta
201400090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
201500090623     C                   DOW       posDa <= %len(%trim(vindta))
201600090623     C*
201700090623     C* Gestisco il 1° campo
201800090623     C                   IF        i = 1
201900090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
202000090623     C                   EVAL      posDa = 1
202100090623     C* Eseguo lo scan x trovare la fine del primo campo
202200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
202300090623     C* A questo punto "estraggo" il campo corrente
202400170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
202500110923     C                                         %subst(vindta:posDa:(posA-posDa))
202600090623     C* X i campi successivi al 1°
202700090623     C                   ELSE
202800090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
202900090623     C                   EVAL      posDa = posA + 1
203000090623     C* Eseguo lo scan x trovare la fine del campo corrente
203100090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
203200090623     C* Gestisco l'ultimo campo
203300090623     C                   IF        posA = *zeros
203400090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
203500090623     C* A questo punto "estraggo" il campo corrente
203600170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
203700110923     C                                         %subst(vindta:posDa:(posA-posDa))
203800090623     C                   LEAVE
203900090623     C                   ELSE
204000090623     C* A questo punto "estraggo" il campo corrente
204100170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
204200110923     C                                         %subst(vindta:posDa:(posA-posDa))
204300090623     C                   ENDIF
204400090623     C                   ENDIF
204500090623     C* Incremento il contatore d campo
204600090623     C                   EVAL      i = i +1
204700090623     C                   ENDDO
204800090623     C*
204900090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
205000090623     C* delimitazione testo
205100090623     C                   EVAL      i = 1
205200170213     C                   DOW       i < %elem(SkSplitFPCL)
205300170213     C     CharTXT:' '   XLATE     SkSplitFPCL(i)SkSplitFPCL(i)
205400170213     C                   EVAL      SkSplitFPCL(i) = %trim(SkSplitFPCL(i))
205500090623     C                   EVAL      i = i + 1
205600090623     C                   ENDDO
205700090623     C                   EVAL      wGiro = 1
205800090623     C* Ripristino la 1° riga originale
205900090623     C                   EVAL      vinDTA = depvinDTA
206000090623     C                   ELSE
206100090623     C                   EVAL      wGiro = 2
206200090623     C**********
206300090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
206400090623     C* d separatore campo
206500090623     C**********
206600090623     C                   EVAL      vindta = %trim(vindta)
206700090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
206800090623     C                   EVAL      vindta = %subst(vindta:2)
206900090623     C                   ENDDO
207000090623     C*
207100090623     C                   Z-ADD     *zeros        lunghInput        4 0
207200090623     C                   EVAL      lunghInput = %len(%trim(vindta))
207300090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
207400090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
207500090623     C                   EVAL      lunghInput = %len(%trim(vindta))
207600090623     C                   ENDDO
207700090623     C**********
207800090623     C* Effettuo lo split del campo dati d input
207900090623     C**********
208000090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
208100090623     C                   DOW       posDa <= %len(%trim(vindta))
208200090623     C*
208300090623     C* Gestisco il 1° campo
208400090623     C                   IF        i = 1
208500090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
208600090623     C                   EVAL      posDa = 1
208700090623     C* Eseguo lo scan x trovare la fine del primo campo
208800090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
208900090623     C* A questo punto "estraggo" il campo corrente
209000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
209100090623     C                                                    (posA-posDa))
209200090623     C* X i campi successivi al 1°
209300090623     C                   ELSE
209400090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
209500090623     C                   EVAL      posDa = posA + 1
209600090623     C* Verifico che nn vi sia il campo nn valorizzato
209700090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
209800090623     C* Se campo nn valorizzato skippo al prossimo
209900090623     C                   EVAL      SkSplitCSV(i) = *blanks
210000090623     C                   EVAL      posA  = posA + 1
210100090623     C                   ELSE
210200090623     C* Eseguo lo scan x trovare la fine del campo corrente
210300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
210400090623     C* Gestisco l'ultimo campo
210500090623     C                   IF        posA = *zeros
210600090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
210700090623     C* A questo punto "estraggo" il campo corrente
210800090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
210900090623     C                                                    (posA-posDa))
211000090623     C                   LEAVE
211100090623     C                   ELSE
211200090623     C* A questo punto "estraggo" il campo corrente
211300090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
211400090623     C                                                    (posA-posDa))
211500090623     C                   ENDIF
211600090623     C                   ENDIF
211700090623     C                   ENDIF
211800090623     C* Incremento il contatore d campo
211900090623     C                   EVAL      i = i +1
212000090623     C                   ENDDO
212100090623     C*
212200090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
212300090623     C* delimitazione testo
212400090623     C                   EVAL      i = 1
212500090623     C                   DOW       i < %elem(SkSplitCSV)
212600090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
212700090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
212800090623     C                   EVAL      i = i + 1
212900090623     C                   ENDDO
213000170215     C*
213100170215     C* Inizializzazioni "specifiche"
213200170215     C                   exsr      INZVALPCL
213300090623     C*
213400090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
213500090623     C                   EVAL      i = 1
213600170213     C                   DOW       i < %elem(SkSplitFPCL)
213700090623     C*
213800090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
213900170213     C                   IF        SkSplitFPCL(i) <> *blanks AND
214000090623     C                             SkSplitCSV(i) <> *blanks
214100090623     C*
214200170213     C                   exsr      CARVALPCL
214300090623     C*
214400090623     C                   ENDIF
214500090623     C                   EVAL      i = i + 1
214600090623     C                   ENDDO
214700090623     C*
214800090623     C***  ===>  Gestione campi "particolari" fuori ciclo
214900111118     C*
215000170213     C*** VATNOT_E_4
215100170213     C                   EVAL      wVATNOT_E_4 = savNAZISORCV
215200170213     C*
215300170213     C* Compongo il "chi sono" completo
215400170213     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
215500111118     C                                         %trim(wVATNOT_E_2) +
215600111118     C                                         %trim(wVATNOT_E_3) +
215700111118     C                                         %trim(wVATNOT_E_4)
215800090623     C*
215900090623     C***  <===  -----------------------------------------
216000090623     C*
216100090623     C                   ENDIF
216200090623     C*
216300090623     C                   ENDSR
216400090623     C*----------------------------------------------------*
216500170213
216600170213
216700170213
216800170213     C*----------------------------------------------------*
216900170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
217000170213     C*----------------------------------------------------*
217100170213     C     IMPVATMSG     BEGSR
217200170213     C*
217300170213     C**********
217400170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
217500170213     C**********
217600170213     C                   IF        wGiro = *zeros
217700170213     C*
217800170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
217900170213     C                   EVAL      depvinDTA = vinDTA
218000170213     C*
218100170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
218200170213     C                   IF        msg    <> *blanks
218300170213     C                   EVAL      vindta = msg
218400170213     C     ';':CharCSV   XLATE     vindta        vindta
218500170213     C                   ENDIF
218600170213     C*
218700170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
218800170213     C     minu:maiu     XLATE     vindta        vindta
218900170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
219000170213     C                   DOW       posDa <= %len(%trim(vindta))
219100170213     C*
219200170213     C* Gestisco il 1° campo
219300170213     C                   IF        i = 1
219400170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
219500170213     C                   EVAL      posDa = 1
219600170213     C* Eseguo lo scan x trovare la fine del primo campo
219700170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
219800170213     C* A questo punto "estraggo" il campo corrente
219900170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
220000170213     C                                         %subst(vindta:posDa:(posA-posDa))
220100170213     C* X i campi successivi al 1°
220200170213     C                   ELSE
220300170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
220400170213     C                   EVAL      posDa = posA + 1
220500170213     C* Eseguo lo scan x trovare la fine del campo corrente
220600170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
220700170213     C* Gestisco l'ultimo campo
220800170213     C                   IF        posA = *zeros
220900170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
221000170213     C* A questo punto "estraggo" il campo corrente
221100170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
221200170213     C                                         %subst(vindta:posDa:(posA-posDa))
221300170213     C                   LEAVE
221400170213     C                   ELSE
221500170213     C* A questo punto "estraggo" il campo corrente
221600170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
221700170213     C                                         %subst(vindta:posDa:(posA-posDa))
221800170213     C                   ENDIF
221900170213     C                   ENDIF
222000170213     C* Incremento il contatore d campo
222100170213     C                   EVAL      i = i +1
222200170213     C                   ENDDO
222300170213     C*
222400170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
222500170213     C* delimitazione testo
222600170213     C                   EVAL      i = 1
222700170213     C                   DOW       i < %elem(SkSplitFMSG)
222800170213     C     CharTXT:' '   XLATE     SkSplitFMSG(i)SkSplitFMSG(i)
222900170213     C                   EVAL      SkSplitFMSG(i) = %trim(SkSplitFMSG(i))
223000170213     C                   EVAL      i = i + 1
223100170213     C                   ENDDO
223200170213     C                   EVAL      wGiro = 1
223300170213     C* Ripristino la 1° riga originale
223400170213     C                   EVAL      vinDTA = depvinDTA
223500170213     C                   ELSE
223600170213     C                   EVAL      wGiro = 2
223700170213     C**********
223800170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
223900170213     C* d separatore campo
224000170213     C**********
224100170213     C                   EVAL      vindta = %trim(vindta)
224200170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
224300170213     C                   EVAL      vindta = %subst(vindta:2)
224400170213     C                   ENDDO
224500170213     C*
224600170213     C                   Z-ADD     *zeros        lunghInput        4 0
224700170213     C                   EVAL      lunghInput = %len(%trim(vindta))
224800170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
224900170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
225000170213     C                   EVAL      lunghInput = %len(%trim(vindta))
225100170213     C                   ENDDO
225200170213     C**********
225300170213     C* Effettuo lo split del campo dati d input
225400170213     C**********
225500170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
225600170213     C                   DOW       posDa <= %len(%trim(vindta))
225700170213     C*
225800170213     C* Gestisco il 1° campo
225900170213     C                   IF        i = 1
226000170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
226100170213     C                   EVAL      posDa = 1
226200170213     C* Eseguo lo scan x trovare la fine del primo campo
226300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
226400170213     C* A questo punto "estraggo" il campo corrente
226500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
226600170213     C                                                    (posA-posDa))
226700170213     C* X i campi successivi al 1°
226800170213     C                   ELSE
226900170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
227000170213     C                   EVAL      posDa = posA + 1
227100170213     C* Verifico che nn vi sia il campo nn valorizzato
227200170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
227300170213     C* Se campo nn valorizzato skippo al prossimo
227400170213     C                   EVAL      SkSplitCSV(i) = *blanks
227500170213     C                   EVAL      posA  = posA + 1
227600170213     C                   ELSE
227700170213     C* Eseguo lo scan x trovare la fine del campo corrente
227800170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
227900170213     C* Gestisco l'ultimo campo
228000170213     C                   IF        posA = *zeros
228100170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
228200170213     C* A questo punto "estraggo" il campo corrente
228300170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
228400170213     C                                                    (posA-posDa))
228500170213     C                   LEAVE
228600170213     C                   ELSE
228700170213     C* A questo punto "estraggo" il campo corrente
228800170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
228900170213     C                                                    (posA-posDa))
229000170213     C                   ENDIF
229100170213     C                   ENDIF
229200170213     C                   ENDIF
229300170213     C* Incremento il contatore d campo
229400170213     C                   EVAL      i = i +1
229500170213     C                   ENDDO
229600170213     C*
229700170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
229800170213     C* delimitazione testo
229900170213     C                   EVAL      i = 1
230000170213     C                   DOW       i < %elem(SkSplitCSV)
230100170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
230200170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
230300170213     C                   EVAL      i = i + 1
230400170213     C                   ENDDO
230500170215     C*
230600170215     C* Inizializzazioni "specifiche"
230700170215     C                   exsr      INZVALMSG
230800170213     C*
230900170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
231000170213     C                   EVAL      i = 1
231100170213     C                   DOW       i < %elem(SkSplitFMSG)
231200170213     C*
231300170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
231400170213     C                   IF        SkSplitFMSG(i) <> *blanks AND
231500170213     C                             SkSplitCSV(i) <> *blanks
231600170213     C*
231700170213     C                   exsr      CARVALMSG
231800170213     C*
231900170213     C                   ENDIF
232000170213     C                   EVAL      i = i + 1
232100170213     C                   ENDDO
232200170213     C*
232300170213     C***  ===>  Gestione campi "particolari" fuori ciclo
232400170213     C*
232500170213     C* Solo se messaggio diretto al destinatario
232600170213     C                   IF        wMsgDest = '0'
232700170213     C                   IF        wMsgType = 'EML'
232800170213     C                   EVAL      wVATNOT_IJ = wMsgValue
232900170213     C                   ENDIF
233000170213     C                   IF        wMsgType = 'SMS'
233100170213     C                   EVAL      wVATNOT_S  = wMsgValue
233200170213     C                   ENDIF
233300170213     C*
233400170213     C                   IF        wPredictSms <> 'S'
233500170213     C                   EVAL      %subst(wVATNOT_S:17:1) = 'N'
233600170213     C                   ENDIF
233700170213     C                   IF        wPredictEml <> 'S'
233800170213     C                   EVAL      %subst(wVATNOT_S:18:1) = 'N'
233900170213     C                   ENDIF
234000170213     C                   ENDIF
234100170213     C*
234200170213     C***  <===  -----------------------------------------
234300170213     C*
234400170213     C                   ENDIF
234500170213     C*
234600170213     C                   ENDSR
234700170213     C*----------------------------------------------------*
234800170215
234900170215
235000170215
235100170215     C*----------------------------------------------------*
235200170215     C*  ROUTINE DI INNIZIALIZZAZIONI
235300170215     C*----------------------------------------------------*
235400170215     C     INZVALSHP     BEGSR
235500170215     C*
235600170215     C*
235700170215     C                   ENDSR
235800170215     C***
235900170215
236000170215
236100170215
236200170215     C*----------------------------------------------------*
236300170215     C*  ROUTINE DI INNIZIALIZZAZIONI
236400170215     C*----------------------------------------------------*
236500170215     C     INZVALSND     BEGSR
236600170215     C*
236700170215     C*
236800170215     C                   ENDSR
236900170215     C***
237000170215
237100170215
237200170215
237300170215     C*----------------------------------------------------*
237400170215     C*  ROUTINE DI INNIZIALIZZAZIONI
237500170215     C*----------------------------------------------------*
237600170215     C     INZVALRCV     BEGSR
237700170215     C*
237800170215     C                   CLEAR                   savNAZISORCV
237900170215     C                   CLEAR                   wVATNOT_A
238000170215     C                   CLEAR                   wVATNOT_B
238100170215     C                   CLEAR                   wVATNOT_E_1
238200170215     C*
238300170215     C                   ENDSR
238400170215     C***
238500170215
238600170215
238700170215
238800170215     C*----------------------------------------------------*
238900170215     C*  ROUTINE DI INNIZIALIZZAZIONI
239000170215     C*----------------------------------------------------*
239100170215     C     INZVALDLV     BEGSR
239200170215     C*
239300170215     C*
239400170215     C                   ENDSR
239500170215     C***
239600170215
239700170215
239800170215
239900170215     C*----------------------------------------------------*
240000170215     C*  ROUTINE DI INNIZIALIZZAZIONI
240100170215     C*----------------------------------------------------*
240200170215     C     INZVALCOD     BEGSR
240300170215     C*
240400170215     C*
240500170215     C                   ENDSR
240600170215     C***
240700170215
240800170215
240900170215
241000170215     C*----------------------------------------------------*
241100170215     C*  ROUTINE DI INNIZIALIZZAZIONI
241200170215     C*----------------------------------------------------*
241300170215     C     INZVALPCL     BEGSR
241400170215     C*
241500170215     C                   CLEAR                   wVATNOT_E_2
241600170215     C                   CLEAR                   wVATNOT_E_3
241700170215     C*
241800170215     C                   ENDSR
241900170215     C***
242000170215
242100170215
242200170215
242300170215     C*----------------------------------------------------*
242400170215     C*  ROUTINE DI INNIZIALIZZAZIONI
242500170215     C*----------------------------------------------------*
242600170215     C     INZVALMSG     BEGSR
242700170215     C*
242800170215     C* Inizializzazioni "specifiche"
242900170215     C                   MOVEL     *blanks       wMsgType          3
243000170215     C                   MOVEL     *blanks       wMsgDest          1
243100170215     C                   MOVEL     *blanks       wPredictEml       1
243200170215     C                   MOVEL     *blanks       wPredictSms       1
243300170215     C                   MOVEL     *blanks       wMsgValue        70
243400170215     C*
243500170215     C                   ENDSR
243600170215     C***
243700170213
243800090623
243900090623
244000090623     C*----------------------------------------------------*
244100090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
244200090623     C*----------------------------------------------------*
244300170213     C     CARVALSHP     BEGSR
244400090623     C*
244500111117     C*** NSP
244600170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSID'
244700170213     C                   EVAL      PiStr=SkSplitCSV(i)
244800090623     C                   EXSR      CHKNUM
244900090623     C                   IF        PiInt=*on
245000090623     C                   Z-ADD     PiVal         VABNSP
245100090623     C                   ELSE
245200090623     C                   SETON                                        31
245300090623     C                   EVAL      VABNSP = *zeros
245400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
245500090623     C                             + ' ' + 'VABNSP'
245600090623     C                   ENDIF
245700090623     C                   ENDIF
245800090623     C*** RMN
245900170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSID'
246000170213     C                   EVAL      PiStr=SkSplitCSV(i)
246100090623     C                   EXSR      CHKNUM
246200090623     C                   IF        PiInt=*on
246300090623     C                   Z-ADD     PiVal         VABRMN
246400090623     C                   ELSE
246500090623     C                   SETON                                        32
246600090623     C                   EVAL      VABRMN = *zeros
246700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
246800090623     C                             + ' ' + 'VABRMN'
246900090623     C                   ENDIF
247000090623     C                   ENDIF
247100170213     C*** RMA
247200170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSCREF1'
247300170213     C                   EVAL      VABRMA = SkSplitCSV(i)
247400170213     C                   ENDIF
247500090623     C*** NCL
247600170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSCOUNT'
247700090623     C                   EVAL      PiStr=SkSplitCSV(i)
247800090623     C                   EXSR      CHKNUM
247900090623     C                   IF        PiInt=*on
248000090623     C                   Z-ADD     PiVal         VABNCL
248100090623     C                   ELSE
248200090623     C                   SETON                                        32
248300090623     C                   EVAL      VABNCL = *zeros
248400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
248500090623     C                             + ' ' + 'VABNCL'
248600090623     C                   ENDIF
248700090623     C                   ENDIF
248800170213     C*** VLB
248900170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSVOLUME'
249000170213     C                   IF        SkSplitCSV(i) <> *blanks AND
249100170213     C                             SkSplitCSV(i)  > *zeros
249200170213     C                   EVAL      PiStr=SkSplitCSV(i)
249300170213     C                   EXSR      CHKNUM
249400170213     C                   IF        PiNum=*on
249500170213     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
249600170213     C                   Z-ADD     PiVal         VABVLB
249700170213     C                   ELSE
249800170213     C                   SETON                                        32
249900170213     C                   EVAL      VABVLB = *zeros
250000170213     C                   EVAL      vinmsg = %trimr(vinmsg)
250100170213     C                             + ' ' + 'VABVLB'
250200170213     C                   ENDIF
250300170213     C                   ENDIF
250400170213     C                   ENDIF
250500090623     C*** PKB
250600170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSWEIGHT'
250700090623     C                   EVAL      PiStr=SkSplitCSV(i)
250800090623     C                   EXSR      CHKNUM
250900090623     C                   IF        PiNum=*on
251000130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
251100090623     C                   Z-ADD     PiVal         VABPKB
251200090623     C                   ELSE
251300090623     C                   SETON                                        32
251400090623     C                   EVAL      VABPKB = *zeros
251500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
251600090623     C                             + ' ' + 'VABPKB'
251700090623     C                   ENDIF
251800170213     C                   ENDIF
251900090623     C*
252000090623     C***  ===>  Gestione campi "particolari" dentro ciclo
252100090623     C*
252200090623     C*
252300090623     C***  <===  -----------------------------------------
252400090908     C*
252500090623     C*
252600090623     C                   ENDSR
252700090623     C***
252800170213
252900170213
253000170213
253100170213     C*----------------------------------------------------*
253200170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
253300170213     C*----------------------------------------------------*
253400170213     C     CARVALSND     BEGSR
253500170213     C*
253600170213     C*** RMO
253700170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOMPNAME'
253800170213     C                   EVAL      VABRMO = SkSplitCSV(i)
253900170213     C                   ENDIF
254000170213     C*** CMO
254100170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SZIPCODE'
254200170213     C                   EVAL      VABCMO = SkSplitCSV(i)
254300170213     C                   ENDIF
254400170213     C*** NMO
254500170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOUNTRYCODE'
254600170213     C                   EVAL      savNAZISO = %dec(%trim(SkSplitCSV(i)):3:0)
254700170213     C                   Z-ADD     1             jNAZ
254800170213     C     savNAZISO     LOOKUP    skNAZISO(jNAZ)                         13
254900170213     C                   IF        %found
255000170213     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
255100170213     C                   ELSE
255200170213     C                   EVAL      VABNMO = %editc(savNAZISO:'X')
255300170213     C                   ENDIF
255400170213     C                   ENDIF
255500170213     C*** NOT
255600170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOMMENT'
255700170213     C                   EVAL      wNOTE = %trim(%trim(wNOTE)+' '+SkSplitCSV(i))
255800170213     C                   ENDIF
255900170213     C*
256000170213     C***  ===>  Gestione campi "particolari" dentro ciclo
256100170213     C*
256200170213     C*
256300170213     C***  <===  -----------------------------------------
256400170213     C*
256500170213     C*
256600170213     C                   ENDSR
256700170213     C***
256800170213
256900170213
257000170213
257100170213     C*----------------------------------------------------*
257200170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
257300170213     C*----------------------------------------------------*
257400170213     C     CARVALRCV     BEGSR
257500170213     C*
257600170213     C*** RSD 1/2
257700170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RNAME1'
257800170213     C                   EVAL      VABRSD = SkSplitCSV(i)
257900170213     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
258000170213     C     '@':'A'       XLATE     VABRSD        VABRSD
258100170213     C* ==
258200170213     C                   ENDIF
258300170213     C*** RD2 1/2
258400170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RNAME2'
258500170213     C                   EVAL      VABRD2 = SkSplitCSV(i)
258600170213     C                   ENDIF
258700170213     C*** RSD 2/2
258800170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMPNAME'
258900170213     C                   EVAL      VABRSD=%trim(%trim(VABRSD)+' '+SkSplitCSV(i))
259000170213     C                   ENDIF
259100170213     C*** RS2 2/2
259200170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMPNAME2'
259300170213     C                   EVAL      VABRD2=%trim(%trim(VABRD2)+' '+SkSplitCSV(i))
259400170213     C                   ENDIF
259500170213     C*** IND 1/4
259600170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RSTREET'
259700170213     C                   EVAL      VABIND = SkSplitCSV(i)
259800170213     C                   ENDIF
259900170213     C*** IND 2/4
260000170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RPROPNUM'
260100170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
260200170213     C                   ENDIF
260300170213     C*** IND 3/4
260400170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RADD2'
260500170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
260600170213     C                   ENDIF
260700170213     C*** IND 4/4
260800170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RADD3'
260900170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
261000170213     C                   ENDIF
261100170213     C*** CAD
261200170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RZIPCODE'
261300170213     C                   EVAL      VABCAD = SkSplitCSV(i)
261400170213     C                   MOVEL     *blanks       wAlfa7            7
261500170213     C                   MOVEL     *zeros        wAlfa7z           7
261600170213     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
261700170213     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
261800170213     C                   ENDIF
261900170213     C*** LOD
262000170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RTOWN'
262100170213     C                   EVAL      VABLOD = SkSplitCSV(i)
262200170213     C                   ENDIF
262300170213     C*** NZD
262400170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOUNTRYCODE'
262500170213     C                   EVAL      savNAZISO = %dec(%trim(SkSplitCSV(i)):3:0)
262600170213     C                   EVAL      savNAZISORCV = %editc(savNAZISO:'X')
262700170213     C                   Z-ADD     1             jNAZ
262800170213     C     savNAZISO     LOOKUP    skNAZISO(jNAZ)                         13
262900170213     C                   IF        %found
263000170213     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
263100170213     C                   ELSE
263200170213     C                   EVAL      VABNZD = savNAZISORCV
263300170213     C                   ENDIF
263400170213     C                   IF        savNAZISO <> 380                             * 380 = Italia
263500170213     C   50              SETON                                        33
263600170213     C                   ENDIF
263700170213     C                   ENDIF
263800170213     C*** NOT_A
263900170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCONTACT'
264000170213     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
264100170213     C                   ENDIF
264200170213     C*** NOT_B
264300170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCONTACTPHO1'
264400170213     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
264500170213     C                   ENDIF
264600170213     C*** NOT
264700170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMMENT'
264800170213     C                   EVAL      wNOTE = %trim(%trim(wNOTE)+' '+SkSplitCSV(i))
264900170213     C                   ENDIF
265000170213     C*
265100170213     C***  ===>  Gestione campi "particolari" dentro ciclo
265200170213     C*
265300170213     C*
265400170213     C***  <===  -----------------------------------------
265500170213     C*
265600170213     C*
265700170213     C                   ENDSR
265800170213     C***
265900170213
266000170213
266100170213
266200170213     C*----------------------------------------------------*
266300170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
266400170213     C*----------------------------------------------------*
266500170213     C     CARVALDLV     BEGSR
266600170213     C*
266700170213     C*** DCR
266800170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_DELIVERYDATE_FROM'
266900170213     C                   IF        SkSplitCSV(i) > *zeros
267000170213     C                   EVAL      PiStr=SkSplitCSV(i)
267100170213     C                   EXSR      CHKNUM
267200170213     C                   IF        PiInt=*on
267300170213     C                   Z-ADD     PiVal         VABDCR
267400170213     C                   ELSE
267500170213     C                   SETON                                        32
267600170213     C                   EVAL      VABDCR = *zeros
267700170213     C                   EVAL      vinmsg = %trimr(vinmsg)
267800170213     C                             + ' ' + 'VABDCR'
267900170213     C                   ENDIF
268000170213     C                   ENDIF
268100170213     C                   ENDIF
268200170213     C*
268300170213     C*** DCR
268400170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_DELIVERYDATE_TO'
268500170213     C                   IF        SkSplitCSV(i) > *zeros
268600170213     C                   EVAL      PiStr=SkSplitCSV(i)
268700170213     C                   EXSR      CHKNUM
268800170213     C                   IF        PiInt=*on
268900170213     C                   Z-ADD     PiVal         VABDCR
269000170213     C                   ELSE
269100170213     C                   SETON                                        32
269200170213     C                   EVAL      VABDCR = *zeros
269300170213     C                   EVAL      vinmsg = %trimr(vinmsg)
269400170213     C                             + ' ' + 'VABDCR'
269500170213     C                   ENDIF
269600170213     C                   ENDIF
269700170213     C                   ENDIF
269800170213     C*
269900170213     C*** HCR
270000170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_TIMEFRAME_FROM'
270100170213     C                   IF        SkSplitCSV(i) > *zeros
270200170213     C                   EVAL      PiStr=SkSplitCSV(i)
270300170213     C                   EXSR      CHKNUM
270400170213     C                   IF        PiInt=*on
270500170213     C                   Z-ADD     PiVal         VABHCR
270600170213     C                   ELSE
270700170213     C                   SETON                                        32
270800170213     C                   EVAL      VABHCR = *zeros
270900170213     C                   EVAL      vinmsg = %trimr(vinmsg)
271000170213     C                             + ' ' + 'VABHCR'
271100170213     C                   ENDIF
271200170213     C                   ENDIF
271300170213     C                   ENDIF
271400170213     C*
271500170213     C*** HCR
271600170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_TIMEFRAME_TO'
271700170213     C                   IF        SkSplitCSV(i) > *zeros
271800170213     C                   EVAL      PiStr=SkSplitCSV(i)
271900170213     C                   EXSR      CHKNUM
272000170213     C                   IF        PiInt=*on
272100170213     C                   Z-ADD     PiVal         VABHCR
272200170213     C                   ELSE
272300170213     C                   SETON                                        32
272400170213     C                   EVAL      VABHCR = *zeros
272500170213     C                   EVAL      vinmsg = %trimr(vinmsg)
272600170213     C                             + ' ' + 'VABHCR'
272700170213     C                   ENDIF
272800170213     C                   ENDIF
272900170213     C                   ENDIF
273000170213     C*
273100170213     C***  ===>  Gestione campi "particolari" dentro ciclo
273200170213     C*
273300170213     C*
273400170213     C***  <===  -----------------------------------------
273500170213     C*
273600170213     C*
273700170213     C                   ENDSR
273800170213     C***
273900170213
274000170213
274100170213
274200170213     C*----------------------------------------------------*
274300170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
274400170213     C*----------------------------------------------------*
274500170213     C     CARVALCOD     BEGSR
274600170213     C*
274700170213     C*
274800170213     C* Reperisco quindi i campi numerici...
274900170213     C*** CAS
275000170213     C                   IF        %trim(SkSplitFCOD(i)) = 'COD_NAMOUNT'
275100170213     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
275200170213     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
275300170213     C                             %trim(SkSplitCSV(i)) <> '0'      AND
275400170213     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
275500170213     C                             %trim(SkSplitCSV(i)) <> '0,00'   AND
275600170213     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
275700170213     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
275800170213     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
275900170213     C                             %trim(SkSplitCSV(i)) <> '0.000'
276000170213     C                   EVAL      wFlgCAS = '1'
276100170213     C                   EVAL      PiStr=SkSplitCSV(i)
276200170213     C                   EXSR      CHKNUM
276300170213     C                   IF        PiNum=*on
276400170213     C                   EVAL      VABCAS = PiVal
276500170213     C                   ELSE
276600170213     C                   SETON                                        32
276700170213     C                   EVAL      VABCAS = *zeros
276800170213     C                   EVAL      vinmsg = %trimr(vinmsg)
276900170213     C                             + ' ' + 'VABCAS'
277000170213     C                   ENDIF
277100170213     C                   ENDIF
277200170213     C                   ENDIF
277300170213     C*** VCA
277400170213     C                   IF        %trim(SkSplitFCOD(i)) = 'COD_NCURRENCY'
277500170213     C                   EVAL      VABVCA = %trim(SkSplitCSV(i))
277600170213     C                   ENDIF
277700170213     C*
277800170213     C***  ===>  Gestione campi "particolari" dentro ciclo
277900170213     C*
278000170213     C*
278100170213     C***  <===  -----------------------------------------
278200170213     C*
278300170213     C*
278400170213     C                   ENDSR
278500170213     C***
278600090623
278700111118
278800111118
278900111118     C*----------------------------------------------------*
279000111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
279100111118     C*----------------------------------------------------*
279200170213     C     CARVALPCL     BEGSR
279300111118     C*
279400111118     C*** VATNOT_E_2
279500170213     C                   IF        %trim(SkSplitFPCL(i)) = 'PCL_PARCELNUMBER'
279600111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
279700111118     C                   ENDIF
279800111118     C*** VATNOT_E_3
279900170213     C                   IF        %trim(SkSplitFPCL(i)) = 'PCL_SERVICECODE'
280000111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
280100111118     C                   ENDIF
280200111118     C*
280300111118     C***  ===>  Gestione campi "particolari" dentro ciclo
280400111118     C*
280500111118     C*
280600111118     C***  <===  -----------------------------------------
280700111118     C*
280800111118     C*
280900111118     C                   ENDSR
281000111118     C***
281100170213
281200170213
281300170213
281400170213     C*----------------------------------------------------*
281500170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
281600170213     C*----------------------------------------------------*
281700170213     C     CARVALMSG     BEGSR
281800170213     C*
281900170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGTYPE'
282000170213     C                   SELECT
282100170213     C                   WHEN      %trim(SkSplitCSV(i)) = '1'
282200170213     C                   EVAL      wMsgType = 'EML'
282300170213     C                   WHEN      %trim(SkSplitCSV(i)) = '3'
282400170213     C                   EVAL      wMsgType = 'SMS'
282500170213     C                   ENDSL
282600170213     C                   ENDIF
282700170213     C*
282800170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGDESTINATION'
282900170213     C                   EVAL      wMsgValue = %trim(SkSplitCSV(i))
283000170213     C                   ENDIF
283100170213     C*
283200170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGDESTTYPE'
283300170213     C                   EVAL      wMsgDest = %trim(SkSplitCSV(i))
283400170213     C                   ENDIF
283500170213     C*
283600170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGTRIGGER'
283700170213     C                   IF        %trim(SkSplitCSV(i)) = '904'
283800170213     C                   SELECT
283900170213     C                   WHEN      wMsgType='EML'
284000170213     C                   EVAL      wPredictEml = 'S'
284100170213     C                   WHEN      wMsgType='SMS'
284200170213     C                   EVAL      wPredictSms = 'S'
284300170213     C                   ENDSL
284400170213     C                   ENDIF
284500170213     C                   ENDIF
284600170213     C*
284700170213     C***  ===>  Gestione campi "particolari" dentro ciclo
284800170213     C*
284900170213     C*
285000170213     C***  <===  -----------------------------------------
285100170213     C*
285200170213     C*
285300170213     C                   ENDSR
285400170213     C***
285500161011
285600010601
285700020204
285800020204     C*----------------------------------------------------*
285900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
286000020204     C*----------------------------------------------------*
286100020204     C     CHKIMPDIV     BEGSR
286200020204     C*
286300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
286400020204     C                   Z-ADD     *zeros        wrkDec            9 9
286500020204     C*
286600020204     C* Come prima cosa effettuo considerazioni sulla divisa
286700020204     C                   IF        vabIAS > *zeros
286800020204     C                   IF        vabVAS <> 'EUR'
286900020204     C                   EVAL      vabVAS =  'ITL'
287000020204     C                   ENDIF
287100020204     C                   ENDIF
287200020204     C*
287300020204     C                   IF        vabCAS > *zeros
287400020204     C                   IF        vabVCA <> 'EUR'
287500020204     C                   EVAL      vabVCA =  'ITL'
287600020204     C                   ENDIF
287700020204     C                   ENDIF
287800020204     C*
287900020204     C                   IF        vabVMD > *zeros
288000020204     C                   IF        vabVAD <> 'EUR'
288100020204     C                   EVAL      vabVAD =  'ITL'
288200020204     C                   ENDIF
288300020204     C                   ENDIF
288400020204     C*
288500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
288600020204     C                   Z-ADD     vabIAS        wrkDec
288700020204     C                   IF        wrkDec > *zeros
288800020204     C                   IF        vabVAS = 'ITL'
288900020204     C                   EVAL      vabIAS = *zeros
289000020204     C                   ENDIF
289100020204     C                   ENDIF
289200020204     C*
289300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
289400020204     C                   Z-ADD     vabCAS        wrkDec
289500020204     C                   IF        wrkDec > *zeros
289600020204     C                   IF        vabVCA = 'ITL'
289700020204     C                   EVAL      vabCAS = *zeros
289800020204     C                   ENDIF
289900020204     C                   ENDIF
290000020204     C*
290100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
290200020204     C                   Z-ADD     vabVMD        wrkDec
290300020204     C                   IF        wrkDec > *zeros
290400020204     C                   IF        vabVAD = 'ITL'
290500020204     C                   EVAL      vabVMD = *zeros
290600020204     C                   ENDIF
290700020204     C                   ENDIF
290800020204     C*
290900020204     C                   ENDSR
291000020204     C***
291100080617
291200080617
291300080617
291400080617     C*----------------------------------------------------*
291500080617     C*  CONTROLLO NUMERICITA' CAMPI
291600080617     C*----------------------------------------------------*
291700080617     C     CHKNUM        BEGSR
291800080617     C*
291900080617     C                   IF        PiDecChr = *blanks
292000080617     C                   EVAL      PiDecChr = CharNUM
292100080617     C                   ENDIF
292200080617     C*
292300080617     C                   callp     UBISNUM_Check(PiStr
292400080617     C                                          :PiDecChr
292500080617     C                                          :PiVal
292600080617     C                                          :PiNum
292700080617     C                                          :PiInt)
292800080617     C*
292900080617     C                   ENDSR
293000080617     C***
293100010601
293200010601
293300010601
293400010330
293500040119      /TITLE Invio dei dati al punto operativo.
293600040119     C     invio         BEGSR
293700040119     C*
293800101005     C* 1° invio EDIVAT
293900040119     C                   reset                   dscmz
294000151120     C                   move      invfil        cmzdst
294100090911     C                   eval      cmzfld = 'EDIVATWR'
294200040119     C                   eval      cmzmbd = vlrhdl
294300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
294400040119     C***                if        prmfir = *blanks
294500090911     C                   eval      cmzfla = 'EDIVAT0F'
294600090911     C                   eval      cmzmba = 'EDIVAT0F'
294700040119     C***                else
294800040119     C***                eval      cmzfla = prmfir
294900040119     C***                eval      cmzmba = prmfir
295000040119     C***                endif
295100040119     C                   eval      cmznrr = *zeros
295200040119     C                   move      §ctrokvt      cmznrr
295300040119     C                   eval      cmzlba = vlrfl1
295400040119     C                   call(e)   'TIS711C'
295500040119     C                   parm                    dscmz
295600040119     C                   parm      *blanks       esito
295700040119     C                   if        %error
295800040119     C                             or cmzerr = '1'
295900040119     C                             or esito  = '1'
296000040119     C                   eval      wrkesito = '3'
296100040119     C                   else
296200040119     C*
296300101005     C* 2° invio EDIVAB
296400040119     C                   reset                   dscmz
296500151120     C                   move      invfil        cmzdst
296600040119     C                   eval      cmzfld = vlrfou
296700040119     C                   eval      cmzmbd = vlrhdl
296800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
296900040119     C***                if        prmfir = *blanks
297000090911     C                   eval      cmzfla = 'EDIVAB0F'
297100090911     C                   eval      cmzmba = 'EDIVAB0F'
297200040119     C***                else
297300040119     C***                eval      cmzfla = prmfir
297400040119     C***                eval      cmzmba = prmfir
297500040119     C***                endif
297600040119     C                   eval      cmznrr = *zeros
297700040119     C                   move      §ctrokvb      cmznrr
297800040119     C                   eval      cmzlba = vlrfl1
297900040119     C                   call(e)   'TIS711C'
298000040119     C                   parm                    dscmz
298100040119     C                   parm      *blanks       esito
298200040119     C                   if        %error
298300040119     C                             or cmzerr = '1'
298400040119     C                             or esito  = '1'
298500040119     C                   eval      wrkesito = '3'
298600040119     C                   endif
298700040119     C                   endif
298800040119     C*
298900040119     C                   ENDSR
299000040119     C***
299100010601
299200010601
299300010601
299400010601
299500010601      /TITLE Invio dei dati al punto operativo.
299600010601     C     opeini        BEGSR
299700010601     C*
299800010601     C* Inizializzo flag e contatori operativi
299900010601     C                   movel     '0'           flgGiro           1
300000010601     C                   movel     '0'           flgMulti          1
300100010601     C                   movel     '1'           flgStato          1
300200010615     C                   movel     '0'           flgOk             1
300300010601     C                   z-add     *zeros        cntNonEl         10 0
300400010601     C                   z-add     *zeros        invfil            3 0
300500010601     C*
300600010601     C                   ENDSR
300700010601     C***
300800070326
300900070326
301000070326
301100070326
301200080916     C     *pssr         BEGSR
301300070329     C*
301400080916     C                   if        %open(tivin00r)
301500080916     C                   close     tivin00r
301600080916     C                   endif
301700090911     C                   if        %open(edivabwr)
301800090911     C                   close     edivabwr
301900080916     C                   endif
302000090911     C                   if        %open(edivatwr)
302100090911     C                   close     edivatwr
302200080916     C                   endif
302300070326     C*
302400070326     C* Effettuo la chiamata al CLLE preposto
302500101005     C                   call(e)   'TITVEVTC'
302600080916     C                   parm                    parccm
302700080916     C                   parm                    parmbr
302800080916     C                   parm      '2'           paropz
302900070326     C*
303000080916     C                   eval      wrkesito = '2'
303100070404     C*
303200080916     C                   seton                                        LR
303300070326     C*
303400080916     C                   ENDSR     '*CANCL'
303500070326     C***
303600170213
303700170213
303800170213
303900170213     C*--------------------------------------------------------
304000170213     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
304100170213     C*--------------------------------------------------------
304200170213     C     CARTAB        BEGSR
304300170213     C*
304400170213     C* TABELLA '15' - NAZIONI
304500170213     C                   clear                   skNAZISO
304600170213     C                   clear                   skNAZBAR
304700170213     C                   eval      tblKUT = 1
304800170213     C                   eval      tblCOD = '15'
304900170213     C     KEYtabP       setll     tabel00f
305000170213     C     KEYtabP       reade     tabel00f
305100170213     C                   dow       not %eof(tabel00f)
305200170213     C                   if        tblFLG = *blanks
305300170213     C                   movel(p)  tblUNI        ds15
305400170213     C                   add       1             jNAZ
305500170213     C                   eval      skNAZBAR(jNAZ) = tblKEY
305600170213     C                   eval      skNAZISO(jNAZ) = §15CIE
305700170213     C                   endif
305800170213     C     KEYtabP       reade     tabel00f
305900170213     C                   enddo
306000170213     C*
306100170213     C                   ENDSR
306200170213     C***
306300070326
306400010330
306500010330
306600000613     C     *inzsr        BEGSR
306700990910     C*
306800990910     C     *entry        plist
306900990920     C                   parm                    tivlrds
307000990921     C                   parm      wrkesito      esito
307100000724     C                   parm                    prmlit
307200000710     C                   parm                    prmfir
307300010330     C*
307400010330     C* CALCOLA LA DATA CORRENTE
307500010330     C                   time                    wn14             14 0
307600090911     C                   movel     wn14          oracor            6 0          *ORA
307700110923     C                   z-add     *zeros        datcor            8 0
307800110923     C                   eval      datcor = %dec(%date() : *ISO)
307900170213     C*
308000170213     C* Chiave su TABEL00F - parziale
308100170213     C     KEYtabP       KLIST
308200170213     C                   KFLD                    tblKUT
308300170213     C                   KFLD                    tblCOD
308400000613     C*
308500000613     C                   ENDSR
308600000613     C***
