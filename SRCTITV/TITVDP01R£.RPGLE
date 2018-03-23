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
046800170802     C*
046900170802     C* Suddivisione nei campi destinatario
047000170802     C                   EVAL      VABRSD = %subst(wDEST:1:35)
047100170802     C                   EVAL      VABRD2 = %subst(wDEST:36:35)
047200170802     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
047300170802     C     '@':'A'       XLATE     VABRSD        VABRSD
047400170213     C*
047500170213     C* Suddivisione nei campi note spedizione
047600170213     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
047700170213     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
047800170213     C*
047900170213     C                   ENDSR
048000040119
048100040119
048200040119
048300040119     C*----------------------------------------------------*
048400170213     C*  SCARICAMENTO BUFFER RECORDS VAB
048500170213     C*----------------------------------------------------*
048600170213     C     WRIVAB        BEGSR
048700090623     C*
048800090623     C  N31              add       1             §CTROKVB
048900090623     C   32              add       1             §CTRMO
049000090623     C   31              add       1             §CTRNO
049100040119     C*
049200060331     C* Gestisco l'eventuale rottura x numero spedizione
049300070823     C                   if        VABNSP <> depspe
049400070301     C                   if        wVATNOT_E <> *blanks
049500080125     C                   if        VABCTM = *blanks
049600080125     C                   eval      VABCTM = '7Q'
049700080125     C                   endif
049800070301     C                   endif
049900090911     C*
050000090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
050100090911     C                   eval      VABCMR = wCMR
050200090911     C                   eval      VABDCM = DATCOR
050300090911     C                   eval      VABDTS = DATCOR
050400090911     C                   eval      VABHMS = ORACOR
050500090911     C                   eval      VABCNT = 1
050600170213     C***
050700090911     C                   write     edivab00                                     => scarico il VAB
050800060331     C                   endif
050900090623     C*
051000090623     C  N31              eval      depspe = vabNSP
051100040119     C*
051200040119     C                   ENDSR
051300170213
051400170213
051500170213
051600170213     C*----------------------------------------------------*
051700170213     C*  CONTROLLO PRE-SCARICAMNETO BUFFER VAT
051800170213     C*----------------------------------------------------*
051900170213     C     CHKVAT        BEGSR
052000170213     C*
052100170213     C*
052200170213     C                   ENDSR
052300040119
052400040119
052500040119
052600040119     C*----------------------------------------------------*
052700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
052800040119     C*----------------------------------------------------*
052900040510     C     WRIVAT        BEGSR
053000060512     C*
053100060512     C* Inizializzo i campi chiave
053200151120     C                   eval      VATFGS = VABFGS
053300060512     C                   eval      VATAAS = VABAAS
053400060512     C                   eval      VATCCM = VABCCM
053500070823     C                   eval      VATNRS = VABNRS
053600060512     C                   eval      VATNSP = VABNSP
053700060512     C                   eval      VATLNP = VABLNP
053800111117     C                   eval      VATCMR = wCMR
053900090914     C                   eval      VATCNT = 1
054000060331     C*
054100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
054200060331     C                   if        wVATNOT_A <> *blanks
054300060331     C                   eval      VATNOT = wVATNOT_A
054400060331     C                   eval      VATTRC = 'A'
054500161212     C                   if        vatNOT <> *blanks
054600090911     C                   write     EDIVAT00
054700060331     C                   add       1             §CTROKVT
054800161212     C                   endif
054900060331     C                   endif
055000060331     C*
055100060331     C                   if        wVATNOT_B <> *blanks
055200060331     C                   eval      VATNOT = wVATNOT_B
055300060331     C                   eval      VATTRC = 'B'
055400161212     C                   if        vatNOT <> *blanks
055500090911     C                   write     EDIVAT00
055600060331     C                   add       1             §CTROKVT
055700161212     C                   endif
055800060331     C                   endif
055900111118     C*
056000111118     C                   if        wVATNOT_E <> *blanks
056100111118     C                   eval      VATNOT = wVATNOT_E
056200111118     C                   eval      VATTRC = 'E'
056300161212     C                   if        vatNOT <> *blanks
056400111118     C                   write     EDIVAT00
056500111118     C                   add       1             §CTROKVT
056600161212     C                   endif
056700111118     C                   endif
056800141125     C*
056900170213     C                   if        wVATNOT_S <> *blanks
057000170213     C                   eval      VATNOT = wVATNOT_S
057100170213     C                   eval      VATTRC = 'S'
057200170213     C                   if        vatNOT <> *blanks
057300170213     C                   write     EDIVAT00
057400170213     C                   add       1             §CTROKVT
057500170213     C                   endif
057600170213     C                   endif
057700141125     C*
057800141125     C                   if        wVATNOT_IJ <> *blanks
057900141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
058000141125     C                   eval      VATTRC = 'I'
058100161212     C                   if        vatNOT <> *blanks
058200141125     C                   write     EDIVAT00
058300141125     C                   add       1             §CTROKVT
058400161212     C                   endif
058500141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
058600141125     C                   eval      VATTRC = 'J'
058700161212     C                   if        vatNOT <> *blanks
058800141125     C                   write     EDIVAT00
058900141125     C                   add       1             §CTROKVT
059000161212     C                   endif
059100141125     C                   endif
059200040119     C*
059300040119     C                   ENDSR
059400170217
059500170217
059600170217
059700170217     C*----------------------------------------------------*
059800170217     C*  SCARICAMENTO BUFFER RECORDS VAT - A
059900170217     C*----------------------------------------------------*
060000170217     C     WRIVAT_A      BEGSR
060100170217     C*
060200170217     C* Inizializzo i campi chiave
060300170217     C                   eval      VATFGS = VABFGS
060400170217     C                   eval      VATAAS = VABAAS
060500170217     C                   eval      VATCCM = VABCCM
060600170217     C                   eval      VATNRS = VABNRS
060700170217     C                   eval      VATNSP = VABNSP
060800170217     C                   eval      VATLNP = VABLNP
060900170217     C                   eval      VATCMR = wCMR
061000170217     C                   eval      VATCNT = 1
061100170217     C*
061200170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
061300170217     C                   if        wVATNOT_A <> *blanks
061400170217     C                   eval      VATNOT = wVATNOT_A
061500170217     C                   eval      VATTRC = 'A'
061600170217     C                   if        vatNOT <> *blanks
061700170217     C                   write     EDIVAT00
061800170217     C                   add       1             §CTROKVT
061900170217     C                   endif
062000170217     C                   endif
062100170217     C*
062200170217     C                   ENDSR
062300170217
062400170217
062500170217
062600170217     C*----------------------------------------------------*
062700170217     C*  SCARICAMENTO BUFFER RECORDS VAT - B
062800170217     C*----------------------------------------------------*
062900170217     C     WRIVAT_B      BEGSR
063000170217     C*
063100170217     C* Inizializzo i campi chiave
063200170217     C                   eval      VATFGS = VABFGS
063300170217     C                   eval      VATAAS = VABAAS
063400170217     C                   eval      VATCCM = VABCCM
063500170217     C                   eval      VATNRS = VABNRS
063600170217     C                   eval      VATNSP = VABNSP
063700170217     C                   eval      VATLNP = VABLNP
063800170217     C                   eval      VATCMR = wCMR
063900170217     C                   eval      VATCNT = 1
064000170217     C*
064100170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
064200170217     C                   if        wVATNOT_B <> *blanks
064300170217     C                   eval      VATNOT = wVATNOT_B
064400170217     C                   eval      VATTRC = 'B'
064500170217     C                   if        vatNOT <> *blanks
064600170217     C                   write     EDIVAT00
064700170217     C                   add       1             §CTROKVT
064800170217     C                   endif
064900170217     C                   endif
065000170217     C*
065100170217     C                   ENDSR
065200170217
065300170217
065400170217
065500170217     C*----------------------------------------------------*
065600170217     C*  SCARICAMENTO BUFFER RECORDS VAT - E
065700170217     C*----------------------------------------------------*
065800170217     C     WRIVAT_E      BEGSR
065900170217     C*
066000170217     C* Inizializzo i campi chiave
066100170217     C                   eval      VATFGS = VABFGS
066200170217     C                   eval      VATAAS = VABAAS
066300170217     C                   eval      VATCCM = VABCCM
066400170217     C                   eval      VATNRS = VABNRS
066500170217     C                   eval      VATNSP = VABNSP
066600170217     C                   eval      VATLNP = VABLNP
066700170217     C                   eval      VATCMR = wCMR
066800170217     C                   eval      VATCNT = 1
066900170217     C*
067000170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
067100170217     C                   if        wVATNOT_E <> *blanks
067200170217     C                   eval      VATNOT = wVATNOT_E
067300170217     C                   eval      VATTRC = 'E'
067400170217     C                   if        vatNOT <> *blanks
067500170217     C                   write     EDIVAT00
067600170217     C                   add       1             §CTROKVT
067700170217     C                   endif
067800170217     C                   endif
067900170217     C*
068000170217     C                   ENDSR
068100170217
068200170217
068300170217
068400170217     C*----------------------------------------------------*
068500170217     C*  SCARICAMENTO BUFFER RECORDS VAT - S
068600170217     C*----------------------------------------------------*
068700170217     C     WRIVAT_S      BEGSR
068800170217     C*
068900170217     C* Inizializzo i campi chiave
069000170217     C                   eval      VATFGS = VABFGS
069100170217     C                   eval      VATAAS = VABAAS
069200170217     C                   eval      VATCCM = VABCCM
069300170217     C                   eval      VATNRS = VABNRS
069400170217     C                   eval      VATNSP = VABNSP
069500170217     C                   eval      VATLNP = VABLNP
069600170217     C                   eval      VATCMR = wCMR
069700170217     C                   eval      VATCNT = 1
069800170217     C*
069900170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
070000170217     C                   if        wVATNOT_S <> *blanks
070100170217     C                   eval      VATNOT = wVATNOT_S
070200170217     C                   eval      VATTRC = 'S'
070300170217     C                   if        vatNOT <> *blanks
070400170217     C                   write     EDIVAT00
070500170217     C                   add       1             §CTROKVT
070600170217     C                   endif
070700170217     C                   endif
070800170217     C*
070900170217     C                   ENDSR
071000170217
071100170217
071200170217
071300170217     C*----------------------------------------------------*
071400170217     C*  SCARICAMENTO BUFFER RECORDS VAT - IJ
071500170217     C*----------------------------------------------------*
071600170217     C     WRIVAT_IJ     BEGSR
071700170217     C*
071800170217     C* Inizializzo i campi chiave
071900170217     C                   eval      VATFGS = VABFGS
072000170217     C                   eval      VATAAS = VABAAS
072100170217     C                   eval      VATCCM = VABCCM
072200170217     C                   eval      VATNRS = VABNRS
072300170217     C                   eval      VATNSP = VABNSP
072400170217     C                   eval      VATLNP = VABLNP
072500170217     C                   eval      VATCMR = wCMR
072600170217     C                   eval      VATCNT = 1
072700170217     C*
072800170217     C* Gestisco estensioni VAT "specificatamente esplicitate"
072900170217     C                   if        wVATNOT_IJ <> *blanks
073000170217     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
073100170217     C                   eval      VATTRC = 'I'
073200170217     C                   if        vatNOT <> *blanks
073300170217     C                   write     EDIVAT00
073400170217     C                   add       1             §CTROKVT
073500170217     C                   endif
073600170217     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
073700170217     C                   eval      VATTRC = 'J'
073800170217     C                   if        vatNOT <> *blanks
073900170217     C                   write     EDIVAT00
074000170217     C                   add       1             §CTROKVT
074100170217     C                   endif
074200170217     C                   endif
074300170217     C*
074400170217     C                   ENDSR
074500040119
074600040119
074700040119
074800040119     C*----------------------------------------------------*
074900101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
075000040119     C*----------------------------------------------------*
075100101005     C     PREVAT        BEGSR
075200040119     C*
075300101005     C* Compongo il nome del membro da dare al EDIVATWR
075400040119     C                   eval      parmbr = vlrhdl
075500040119     C                   movel     'M'           parmbr
075600040510     C                   eval      parccm = vlrksc
075700040119     C                   eval      paropz = '1'
075800040119     C* Effettuo la chiamata al CLLE preposto
075900101005     C                   call(e)   'TITVEVTC'
076000040119     C                   parm                    parccm
076100040119     C                   parm                    parmbr
076200040119     C                   parm                    paropz
076300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076400040119     C                   if        %error
076500040119     C                   movel     '1'           chkcall
076600040119     C                   else
076700040119     C                   movel     '0'           chkcall
076800040119     C                   endif
076900040119     C*
077000040119     C                   ENDSR
077100010601
077200010601
077300010601
077400010330     C*----------------------------------------------------*
077500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
077600010330     C*----------------------------------------------------*
077700040119     C     INZVAR        BEGSR
077800010330     C*
077900030822     C* Inizializzo variabili di wrk
078000140415     C                   SETOFF                                       313233
078100090916     C                   Z-ADD     *zeros        Num5_0            5 0
078200090916     C                   MOVEL     *blanks       wNOTE            70
078300170802     C                   MOVEL     *blanks       wDEST            70
078400060331     C                   MOVEL     *blanks       wVATNOT_A
078500060331     C                   MOVEL     *blanks       wVATNOT_B
078600141125     C                   MOVEL     *blanks       wVATNOT_S
078700141125     C                   MOVEL     *blanks       wVATNOT_IJ
078800090904     C                   MOVEL     *blanks       wVATNOT_E
078900111117     C                   MOVEL     *blanks       wVATNOT_E_1
079000111117     C                   MOVEL     *blanks       wVATNOT_E_2
079100111118     C                   MOVEL     *blanks       wVATNOT_E_3
079200111118     C                   MOVEL     *blanks       wVATNOT_E_4
079300170213     C                   Z-ADD     *zeros        savNAZISO         3 0
079400170213     C                   MOVEL     *blanks       savNAZISORCV      3
079500030822     C*
079600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
079700090911     C                   CLEAR                   EDIVAB00
079800090911     C                   CLEAR                   EDIVAT00
079900030822     C                   CLEAR                   SkSplitCSV
080000170213     C                   EXSR      inzwrk
080100030822     C*
080200030822     C* Reimposto i valori di default
080300030822     C                   EXSR      DEFCAM
080400030822     C*
080500010330     C                   ENDSR
080600010330     C*----------------------------------------------------*
080700170213     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
080800010330     C*----------------------------------------------------*
080900010330     C     DEFCAM        BEGSR
081000030822     C*
081100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
081200030715     C* e delimitatore testo.
081300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
081400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
081500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
081600030822     C*
081700150203     C* Se richiesta esclusione Estero
081800150203     C                   SETOFF                                       50
081900150203     C                   IF        %subst(vlrppt:1:1) = 'E'
082000150203     C                   SETON                                        50
082100030715     C                   ENDIF
082200090421     C*
082300170213     C* Di default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
082400090421     C                   EVAL      VABCBO = '1'
082500090421     C                   EVAL      VABLNP = vlrpoi
082600090421     C                   EVAL      VABNSP = rrnum
082700030822     C*
082800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
082900030822     C                   EVAL      posDaDft = 1
083000030822     C                   EVAL      posADft  = 0
083100030822     C                   EVAL      wGiroDft = 0
083200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
083300030822     C                             posDaDft > 0
083400030822     C*
083500030822     C* Gestisco il 1° giro
083600030822     C                   IF        wGiroDft = 0
083700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
083800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
083900030822     C* Incremento il contatore dei "giri"
084000030822     C                   EVAL      wGiroDft = 1
084100030822     C                   ELSE
084200030822     C                   EVAL      posDaDft = posADft
084300030822     C                   ENDIF
084400030822     C* Eseguo lo scan x trovare la fine del campo corrente
084500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
084600030822     C*
084700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
084800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
084900030822     C                   IF        posDaDft > 0 AND
085000030822     C                             posADft  > 0
085100030822     C* NCL
085200030822     C                   IF        %subst(
085300030822     C                             %subst(vlrppt:posDaDft+1:
085400030822     C                             posADft-posDaDft-1):1:3)
085500030822     C                             = 'NCL'
085600030822     C                   EVAL      PiStr=%trim(%subst(
085700030822     C                             %subst(vlrppt:posDaDft+1:
085800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
085900030822     C                   EXSR      CHKNUM
086000030822     C                   IF        PiInt=*on
086100030822     C                   Z-ADD     PiVal         VABNCL
086200030822     C                   ENDIF
086300030822     C                   ENDIF
086400030822     C* CCM
086500030822     C                   IF        %subst(
086600030822     C                             %subst(vlrppt:posDaDft+1:
086700030822     C                             posADft-posDaDft-1):1:3)
086800030822     C                             = 'CCM'
086900030822     C                   EVAL      PiStr=%trim(%subst(
087000030822     C                             %subst(vlrppt:posDaDft+1:
087100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
087200030822     C                   EXSR      CHKNUM
087300030822     C                   IF        PiInt=*on
087400030822     C                   Z-ADD     PiVal         VABCCM
087500030822     C                   ENDIF
087600030822     C                   ENDIF
087700030822     C* LNP
087800030822     C                   IF        %subst(
087900030822     C                             %subst(vlrppt:posDaDft+1:
088000030822     C                             posADft-posDaDft-1):1:3)
088100030822     C                             = 'LNP'
088200030822     C                   EVAL      PiStr=%trim(%subst(
088300030822     C                             %subst(vlrppt:posDaDft+1:
088400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
088500030822     C                   EXSR      CHKNUM
088600030822     C                   IF        PiInt=*on
088700030822     C                   Z-ADD     PiVal         VABLNP
088800030822     C                   ENDIF
088900030822     C                   ENDIF
089000030822     C* NRS
089100030822     C                   IF        %subst(
089200030822     C                             %subst(vlrppt:posDaDft+1:
089300030822     C                             posADft-posDaDft-1):1:3)
089400030822     C                             = 'NRS'
089500030822     C                   EVAL      PiStr=%trim(%subst(
089600030822     C                             %subst(vlrppt:posDaDft+1:
089700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
089800030822     C                   EXSR      CHKNUM
089900030822     C                   IF        PiInt=*on
090000030822     C                   Z-ADD     PiVal         VABNRS
090100030822     C                   ENDIF
090200030822     C                   ENDIF
090300030822     C* CTR
090400030822     C                   IF        %subst(
090500030822     C                             %subst(vlrppt:posDaDft+1:
090600030822     C                             posADft-posDaDft-1):1:3)
090700030822     C                             = 'CTR'
090800030822     C                   EVAL      PiStr=%trim(%subst(
090900030822     C                             %subst(vlrppt:posDaDft+1:
091000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
091100030822     C                   EXSR      CHKNUM
091200030822     C                   IF        PiInt=*on
091300030822     C                   Z-ADD     PiVal         VABCTR
091400030822     C                   ENDIF
091500030822     C                   ENDIF
091600030822     C* PKB
091700030822     C                   IF        %subst(
091800030822     C                             %subst(vlrppt:posDaDft+1:
091900030822     C                             posADft-posDaDft-1):1:3)
092000030822     C                             = 'PKB'
092100030822     C                   EVAL      PiStr=%trim(%subst(
092200030822     C                             %subst(vlrppt:posDaDft+1:
092300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
092400030822     C                   EXSR      CHKNUM
092500030822     C                   IF        PiNum=*on
092600030822     C                   Z-ADD     PiVal         VABPKB
092700030822     C                   ENDIF
092800030822     C                   ENDIF
092900140715     C* PKN
093000140715     C                   IF        %subst(
093100140715     C                             %subst(vlrppt:posDaDft+1:
093200140715     C                             posADft-posDaDft-1):1:3)
093300140715     C                             = 'PKN'
093400140715     C                   EVAL      PiStr=%trim(%subst(
093500140715     C                             %subst(vlrppt:posDaDft+1:
093600140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
093700140715     C                   EXSR      CHKNUM
093800140715     C                   IF        PiNum=*on
093900140715     C                   Z-ADD     PiVal         wVABPKN           7 1
094000140715     C                   ENDIF
094100140715     C                   ENDIF
094200030822     C* VLB
094300030822     C                   IF        %subst(
094400030822     C                             %subst(vlrppt:posDaDft+1:
094500030822     C                             posADft-posDaDft-1):1:3)
094600030822     C                             = 'VLB'
094700030822     C                   EVAL      PiStr=%trim(%subst(
094800030822     C                             %subst(vlrppt:posDaDft+1:
094900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
095000030822     C                   EXSR      CHKNUM
095100030822     C                   IF        PiNum=*on
095200030822     C                   Z-ADD     PiVal         VABVLB
095300030822     C                   ENDIF
095400030822     C                   ENDIF
095500030822     C* QFT
095600030822     C                   IF        %subst(
095700030822     C                             %subst(vlrppt:posDaDft+1:
095800030822     C                             posADft-posDaDft-1):1:3)
095900030822     C                             = 'QFT'
096000030822     C                   EVAL      PiStr=%trim(%subst(
096100030822     C                             %subst(vlrppt:posDaDft+1:
096200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
096300030822     C                   EXSR      CHKNUM
096400030822     C                   IF        PiNum=*on
096500030822     C                   Z-ADD     PiVal         VABQFT
096600030822     C                   ENDIF
096700030822     C                   ENDIF
096800030822     C* CBO
096900030822     C                   IF        %subst(
097000030822     C                             %subst(vlrppt:posDaDft+1:
097100030822     C                             posADft-posDaDft-1):1:3)
097200030822     C                             = 'CBO'
097300030822     C                   EVAL      VABCBO=%trim(%subst(
097400030822     C                             %subst(vlrppt:posDaDft+1:
097500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
097600030822     C                   ENDIF
097700030822     C* TSP
097800030822     C                   IF        %subst(
097900030822     C                             %subst(vlrppt:posDaDft+1:
098000030822     C                             posADft-posDaDft-1):1:3)
098100030822     C                             = 'TSP'
098200030822     C                   EVAL      VABTSP=%trim(%subst(
098300030822     C                             %subst(vlrppt:posDaDft+1:
098400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
098500030822     C                   ENDIF
098600030822     C* VAS
098700030822     C                   IF        %subst(
098800030822     C                             %subst(vlrppt:posDaDft+1:
098900030822     C                             posADft-posDaDft-1):1:3)
099000030822     C                             = 'VAS'
099100030822     C                   EVAL      VABVAS=%trim(%subst(
099200030822     C                             %subst(vlrppt:posDaDft+1:
099300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
099400030822     C                   ENDIF
099500030822     C* VCA
099600030822     C                   IF        %subst(
099700030822     C                             %subst(vlrppt:posDaDft+1:
099800030822     C                             posADft-posDaDft-1):1:3)
099900030822     C                             = 'VCA'
100000030822     C                   EVAL      VABVCA=%trim(%subst(
100100030822     C                             %subst(vlrppt:posDaDft+1:
100200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
100300030822     C                   ENDIF
100400070925     C* TIC
100500070925     C                   IF        %subst(
100600070925     C                             %subst(vlrppt:posDaDft+1:
100700070925     C                             posADft-posDaDft-1):1:3)
100800070925     C                             = 'TIC'
100900070925     C                   EVAL      VABTIC=%trim(%subst(
101000070925     C                             %subst(vlrppt:posDaDft+1:
101100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
101200070925     C                   ENDIF
101300030822     C* GCA
101400030822     C                   IF        %subst(
101500030822     C                             %subst(vlrppt:posDaDft+1:
101600030822     C                             posADft-posDaDft-1):1:3)
101700030822     C                             = 'GCA'
101800030822     C                   EVAL      VABGCA=%trim(%subst(
101900030822     C                             %subst(vlrppt:posDaDft+1:
102000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
102100030822     C                   ENDIF
102200030822     C* CTM
102300030822     C                   IF        %subst(
102400030822     C                             %subst(vlrppt:posDaDft+1:
102500030822     C                             posADft-posDaDft-1):1:3)
102600030822     C                             = 'CTM'
102700030822     C                   EVAL      VABCTM=%trim(%subst(
102800030822     C                             %subst(vlrppt:posDaDft+1:
102900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
103000030822     C                   ENDIF
103100030822     C* FFD
103200030822     C                   IF        %subst(
103300030822     C                             %subst(vlrppt:posDaDft+1:
103400030822     C                             posADft-posDaDft-1):1:3)
103500030822     C                             = 'FFD'
103600030822     C                   EVAL      VABFFD=%trim(%subst(
103700030822     C                             %subst(vlrppt:posDaDft+1:
103800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
103900030822     C                   ENDIF
104000030822     C* VAD
104100030822     C                   IF        %subst(
104200030822     C                             %subst(vlrppt:posDaDft+1:
104300030822     C                             posADft-posDaDft-1):1:3)
104400030822     C                             = 'VAD'
104500030822     C                   EVAL      VABVAD=%trim(%subst(
104600030822     C                             %subst(vlrppt:posDaDft+1:
104700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
104800030822     C                   ENDIF
104900030822     C* GMA
105000030822     C                   IF        %subst(
105100030822     C                             %subst(vlrppt:posDaDft+1:
105200030822     C                             posADft-posDaDft-1):1:3)
105300030822     C                             = 'GMA'
105400030822     C                   EVAL      VABGMA=%trim(%subst(
105500030822     C                             %subst(vlrppt:posDaDft+1:
105600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
105700030822     C                   ENDIF
105800030822     C* GGA
105900030822     C                   IF        %subst(
106000030822     C                             %subst(vlrppt:posDaDft+1:
106100030822     C                             posADft-posDaDft-1):1:3)
106200030822     C                             = 'GGA'
106300030822     C                   EVAL      VABGGA=%trim(%subst(
106400030822     C                             %subst(vlrppt:posDaDft+1:
106500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
106600030822     C                   ENDIF
106700030822     C* GVA
106800030822     C                   IF        %subst(
106900030822     C                             %subst(vlrppt:posDaDft+1:
107000030822     C                             posADft-posDaDft-1):1:3)
107100030822     C                             = 'GVA'
107200030822     C                   EVAL      VABGVA=%trim(%subst(
107300030822     C                             %subst(vlrppt:posDaDft+1:
107400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
107500030822     C                   ENDIF
107600030822     C* TC1
107700030822     C                   IF        %subst(
107800030822     C                             %subst(vlrppt:posDaDft+1:
107900030822     C                             posADft-posDaDft-1):1:3)
108000030822     C                             = 'TC1'
108100030822     C                   EVAL      VABTC1=%trim(%subst(
108200030822     C                             %subst(vlrppt:posDaDft+1:
108300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
108400030822     C                   ENDIF
108500030822     C* TC2
108600030822     C                   IF        %subst(
108700030822     C                             %subst(vlrppt:posDaDft+1:
108800030822     C                             posADft-posDaDft-1):1:3)
108900030822     C                             = 'TC2'
109000030822     C                   EVAL      VABTC2=%trim(%subst(
109100030822     C                             %subst(vlrppt:posDaDft+1:
109200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
109300030822     C                   ENDIF
109400040714     C* VATTRC
109500040714     C                   IF        %subst(
109600040714     C                             %subst(vlrppt:posDaDft+1:
109700040714     C                             posADft-posDaDft-1):1:3)
109800040714     C                             = 'TRC'
109900040714     C                   EVAL      VATTRC=%trim(%subst(
110000040714     C                             %subst(vlrppt:posDaDft+1:
110100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
110200040714     C                   ENDIF
110300030822     C* ...
110400030822     C                   ENDIF
110500030822     C                   ENDDO
110600020204     C*
110700010330     C                   ENDSR
110800170213
110900170213
111000170213
111100010607     C*----------------------------------------------------*
111200170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
111300010607     C*----------------------------------------------------*
111400170213     C     IMPVABSHP     BEGSR
111500030515     C*
111600010607     C                   MOVEL     datcor        VABAAS
111700010607     C                   MOVE      datcor        VABMGS
111800010607     C*
111900030715     C**********
112000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
112100030715     C**********
112200030715     C                   IF        wGiro = *zeros
112300080716     C*
112400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
112500080716     C                   EVAL      depvinDTA = vinDTA
112600040113     C*
112700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
112800170213     C                   IF        shipment <> *blanks
112900170213     C                   EVAL      vindta = shipment
113000070823     C     ';':CharCSV   XLATE     vindta        vindta
113100040510     C                   ENDIF
113200080626     C*
113300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
113400031201     C     minu:maiu     XLATE     vindta        vindta
113500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
113600030715     C                   DOW       posDa <= %len(%trim(vindta))
113700030715     C*
113800030715     C* Gestisco il 1° campo
113900030715     C                   IF        i = 1
114000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
114100030715     C                   EVAL      posDa = 1
114200030715     C* Eseguo lo scan x trovare la fine del primo campo
114300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
114400030715     C* A questo punto "estraggo" il campo corrente
114500170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
114600110923     C                                         %subst(vindta:posDa:(posA-posDa))
114700030715     C* X i campi successivi al 1°
114800030715     C                   ELSE
114900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
115000030715     C                   EVAL      posDa = posA + 1
115100030715     C* Eseguo lo scan x trovare la fine del campo corrente
115200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
115300030715     C* Gestisco l'ultimo campo
115400030715     C                   IF        posA = *zeros
115500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
115600030715     C* A questo punto "estraggo" il campo corrente
115700170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
115800110923     C                                         %subst(vindta:posDa:(posA-posDa))
115900030715     C                   LEAVE
116000030715     C                   ELSE
116100030715     C* A questo punto "estraggo" il campo corrente
116200170213     C                   EVAL      SkSplitFSHP(i) = %trim(prefix) +
116300110923     C                                         %subst(vindta:posDa:(posA-posDa))
116400030715     C                   ENDIF
116500030715     C                   ENDIF
116600030715     C* Incremento il contatore d campo
116700030715     C                   EVAL      i = i +1
116800030715     C                   ENDDO
116900030715     C*
117000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
117100030715     C* delimitazione testo
117200030715     C                   EVAL      i = 1
117300170213     C                   DOW       i < %elem(SkSplitFSHP)
117400170213     C     CharTXT:' '   XLATE     SkSplitFSHP(i)SkSplitFSHP(i)
117500170213     C                   EVAL      SkSplitFSHP(i) = %trim(SkSplitFSHP(i))
117600030715     C                   EVAL      i = i + 1
117700030715     C                   ENDDO
117800030715     C                   EVAL      wGiro = 1
117900080716     C* Ripristino la 1° riga originale
118000080716     C                   EVAL      vinDTA = depvinDTA
118100030715     C                   ELSE
118200030715     C                   EVAL      wGiro = 2
118300031204     C**********
118400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
118500170213     C* di separatore campo
118600031204     C**********
118700031204     C                   EVAL      vindta = %trim(vindta)
118800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
118900031204     C                   EVAL      vindta = %subst(vindta:2)
119000031204     C                   ENDDO
119100031204     C*
119200031204     C                   Z-ADD     *zeros        lunghInput        4 0
119300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
119400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
119500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
119600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
119700031204     C                   ENDDO
119800030715     C**********
119900030715     C* Effettuo lo split del campo dati d input
120000030715     C**********
120100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
120200030715     C                   DOW       posDa <= %len(%trim(vindta))
120300030715     C*
120400030715     C* Gestisco il 1° campo
120500030715     C                   IF        i = 1
120600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
120700030715     C                   EVAL      posDa = 1
120800030715     C* Eseguo lo scan x trovare la fine del primo campo
120900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
121000030715     C* A questo punto "estraggo" il campo corrente
121100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
121200030715     C                                                    (posA-posDa))
121300030715     C* X i campi successivi al 1°
121400030715     C                   ELSE
121500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
121600030715     C                   EVAL      posDa = posA + 1
121700031204     C* Verifico che nn vi sia il campo nn valorizzato
121800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
121900031204     C* Se campo nn valorizzato skippo al prossimo
122000031204     C                   EVAL      SkSplitCSV(i) = *blanks
122100031204     C                   EVAL      posA  = posA + 1
122200031204     C                   ELSE
122300030715     C* Eseguo lo scan x trovare la fine del campo corrente
122400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
122500030715     C* Gestisco l'ultimo campo
122600030715     C                   IF        posA = *zeros
122700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
122800030715     C* A questo punto "estraggo" il campo corrente
122900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
123000030715     C                                                    (posA-posDa))
123100030715     C                   LEAVE
123200030715     C                   ELSE
123300030715     C* A questo punto "estraggo" il campo corrente
123400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
123500030715     C                                                    (posA-posDa))
123600030715     C                   ENDIF
123700031204     C                   ENDIF
123800030715     C                   ENDIF
123900030715     C* Incremento il contatore d campo
124000030715     C                   EVAL      i = i +1
124100030715     C                   ENDDO
124200030715     C*
124300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
124400030715     C* delimitazione testo
124500030715     C                   EVAL      i = 1
124600030715     C                   DOW       i < %elem(SkSplitCSV)
124700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
124800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
124900030715     C                   EVAL      i = i + 1
125000030715     C                   ENDDO
125100170215     C*
125200170215     C* Inizializzazioni "specifiche"
125300170215     C                   exsr      INZVALSHP
125400030715     C*
125500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
125600030715     C                   EVAL      i = 1
125700170213     C                   DOW       i < %elem(SkSplitFSHP)
125800030715     C*
125900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
126000170213     C                   IF        SkSplitFSHP(i) <> *blanks AND
126100090623     C                             SkSplitCSV(i) <> *blanks
126200090623     C*
126300170213     C                   exsr      CARVALSHP
126400040510     C*
126500030715     C                   ENDIF
126600030715     C                   EVAL      i = i + 1
126700030715     C                   ENDDO
126800070925     C*
126900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
127000080125     C*
127100140715     C*
127200170213     C* Se peso non indicato nei dati cleinte e richiesto un peso forzato per collo
127300140715     C                   IF        VABPKB = *zeros
127400170213     C                   EVAL      VABPKB = wVABPKN * VABNCL
127500140715     C                   ENDIF
127600040510     C*
127700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
127800170213     C                   IF        VABNSP = *zeros
127900040510     C                   SETON                                        31
128000040510     C                   ENDIF
128100040715     C*
128200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
128300040715     C                   IF        VABLNP = *zeros
128400040715     C                   SETON                                        31
128500040715     C                   ENDIF
128600040510     C*
128700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
128800040510     C                   IF        VABRMN = *zeros
128900040510     C                   Z-ADD     VABNSP        VABRMN
129000040510     C                   ENDIF
129100040510     C*
129200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
129300040510     C                   IF        VABRMA = *blanks
129400040714     C                   MOVEL     VABRMN        VABRMA
129500040510     C                   ENDIF
129600170213     C*
129700170213     C***  <===  -----------------------------------------
129800030715     C                   ENDIF
129900020204     C*
130000010607     C                   ENDSR
130100010607     C*----------------------------------------------------*
130200170213
130300170213
130400170213
130500170213     C*----------------------------------------------------*
130600170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
130700170213     C*----------------------------------------------------*
130800170213     C     IMPVABSND     BEGSR
130900170213     C*
131000170213     C**********
131100170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
131200170213     C**********
131300170213     C                   IF        wGiro = *zeros
131400170213     C*
131500170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
131600170213     C                   EVAL      depvinDTA = vinDTA
131700170213     C*
131800170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
131900170213     C                   IF        sender <> *blanks
132000170213     C                   EVAL      vindta = sender
132100170213     C     ';':CharCSV   XLATE     vindta        vindta
132200170213     C                   ENDIF
132300170213     C*
132400170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
132500170213     C     minu:maiu     XLATE     vindta        vindta
132600170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
132700170213     C                   DOW       posDa <= %len(%trim(vindta))
132800170213     C*
132900170213     C* Gestisco il 1° campo
133000170213     C                   IF        i = 1
133100170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
133200170213     C                   EVAL      posDa = 1
133300170213     C* Eseguo lo scan x trovare la fine del primo campo
133400170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
133500170213     C* A questo punto "estraggo" il campo corrente
133600170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
133700170213     C                                         %subst(vindta:posDa:(posA-posDa))
133800170213     C* X i campi successivi al 1°
133900170213     C                   ELSE
134000170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
134100170213     C                   EVAL      posDa = posA + 1
134200170213     C* Eseguo lo scan x trovare la fine del campo corrente
134300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
134400170213     C* Gestisco l'ultimo campo
134500170213     C                   IF        posA = *zeros
134600170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
134700170213     C* A questo punto "estraggo" il campo corrente
134800170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
134900170213     C                                         %subst(vindta:posDa:(posA-posDa))
135000170213     C                   LEAVE
135100170213     C                   ELSE
135200170213     C* A questo punto "estraggo" il campo corrente
135300170213     C                   EVAL      SkSplitFSND(i) = %trim(prefix) +
135400170213     C                                         %subst(vindta:posDa:(posA-posDa))
135500170213     C                   ENDIF
135600170213     C                   ENDIF
135700170213     C* Incremento il contatore d campo
135800170213     C                   EVAL      i = i +1
135900170213     C                   ENDDO
136000170213     C*
136100170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
136200170213     C* delimitazione testo
136300170213     C                   EVAL      i = 1
136400170213     C                   DOW       i < %elem(SkSplitFSND)
136500170213     C     CharTXT:' '   XLATE     SkSplitFSND(i)SkSplitFSND(i)
136600170213     C                   EVAL      SkSplitFSND(i) = %trim(SkSplitFSND(i))
136700170213     C                   EVAL      i = i + 1
136800170213     C                   ENDDO
136900170213     C                   EVAL      wGiro = 1
137000170213     C* Ripristino la 1° riga originale
137100170213     C                   EVAL      vinDTA = depvinDTA
137200170213     C                   ELSE
137300170213     C                   EVAL      wGiro = 2
137400170213     C**********
137500170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
137600170213     C* d separatore campo
137700170213     C**********
137800170213     C                   EVAL      vindta = %trim(vindta)
137900170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
138000170213     C                   EVAL      vindta = %subst(vindta:2)
138100170213     C                   ENDDO
138200170213     C*
138300170213     C                   Z-ADD     *zeros        lunghInput        4 0
138400170213     C                   EVAL      lunghInput = %len(%trim(vindta))
138500170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
138600170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
138700170213     C                   EVAL      lunghInput = %len(%trim(vindta))
138800170213     C                   ENDDO
138900170213     C**********
139000170213     C* Effettuo lo split del campo dati d input
139100170213     C**********
139200170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
139300170213     C                   DOW       posDa <= %len(%trim(vindta))
139400170213     C*
139500170213     C* Gestisco il 1° campo
139600170213     C                   IF        i = 1
139700170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
139800170213     C                   EVAL      posDa = 1
139900170213     C* Eseguo lo scan x trovare la fine del primo campo
140000170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
140100170213     C* A questo punto "estraggo" il campo corrente
140200170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
140300170213     C                                                    (posA-posDa))
140400170213     C* X i campi successivi al 1°
140500170213     C                   ELSE
140600170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
140700170213     C                   EVAL      posDa = posA + 1
140800170213     C* Verifico che nn vi sia il campo nn valorizzato
140900170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
141000170213     C* Se campo nn valorizzato skippo al prossimo
141100170213     C                   EVAL      SkSplitCSV(i) = *blanks
141200170213     C                   EVAL      posA  = posA + 1
141300170213     C                   ELSE
141400170213     C* Eseguo lo scan x trovare la fine del campo corrente
141500170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
141600170213     C* Gestisco l'ultimo campo
141700170213     C                   IF        posA = *zeros
141800170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
141900170213     C* A questo punto "estraggo" il campo corrente
142000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
142100170213     C                                                    (posA-posDa))
142200170213     C                   LEAVE
142300170213     C                   ELSE
142400170213     C* A questo punto "estraggo" il campo corrente
142500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
142600170213     C                                                    (posA-posDa))
142700170213     C                   ENDIF
142800170213     C                   ENDIF
142900170213     C                   ENDIF
143000170213     C* Incremento il contatore d campo
143100170213     C                   EVAL      i = i +1
143200170213     C                   ENDDO
143300170213     C*
143400170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
143500170213     C* delimitazione testo
143600170213     C                   EVAL      i = 1
143700170213     C                   DOW       i < %elem(SkSplitCSV)
143800170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
143900170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
144000170213     C                   EVAL      i = i + 1
144100170213     C                   ENDDO
144200170215     C*
144300170215     C* Inizializzazioni "specifiche"
144400170215     C                   exsr      INZVALSND
144500170213     C*
144600170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
144700170213     C                   EVAL      i = 1
144800170213     C                   DOW       i < %elem(SkSplitFSND)
144900170213     C*
145000170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
145100170213     C                   IF        SkSplitFSND(i) <> *blanks AND
145200170213     C                             SkSplitCSV(i) <> *blanks
145300170213     C*
145400170213     C                   exsr      CARVALSND
145500170213     C*
145600170213     C                   ENDIF
145700170213     C                   EVAL      i = i + 1
145800170213     C                   ENDDO
145900170213     C*
146000170213     C***  ===>  Gestione campi "particolari" fuori ciclo
146100170213     C*
146200170213     C*
146300170213     C***  <===  -----------------------------------------
146400170213     C*
146500170213     C                   ENDIF
146600170213     C*
146700170213     C                   ENDSR
146800170213     C*----------------------------------------------------*
146900170213
147000170213
147100170213
147200170213     C*----------------------------------------------------*
147300170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
147400170213     C*----------------------------------------------------*
147500170213     C     IMPVABRCV     BEGSR
147600170213     C*
147700170213     C**********
147800170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
147900170213     C**********
148000170213     C                   IF        wGiro = *zeros
148100170213     C*
148200170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
148300170213     C                   EVAL      depvinDTA = vinDTA
148400170213     C*
148500170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
148600170213     C                   IF        receiver <> *blanks
148700170213     C                   EVAL      vindta = receiver
148800170213     C     ';':CharCSV   XLATE     vindta        vindta
148900170213     C                   ENDIF
149000170213     C*
149100170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
149200170213     C     minu:maiu     XLATE     vindta        vindta
149300170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
149400170213     C                   DOW       posDa <= %len(%trim(vindta))
149500170213     C*
149600170213     C* Gestisco il 1° campo
149700170213     C                   IF        i = 1
149800170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
149900170213     C                   EVAL      posDa = 1
150000170213     C* Eseguo lo scan x trovare la fine del primo campo
150100170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
150200170213     C* A questo punto "estraggo" il campo corrente
150300170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
150400170213     C                                         %subst(vindta:posDa:(posA-posDa))
150500170213     C* X i campi successivi al 1°
150600170213     C                   ELSE
150700170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
150800170213     C                   EVAL      posDa = posA + 1
150900170213     C* Eseguo lo scan x trovare la fine del campo corrente
151000170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
151100170213     C* Gestisco l'ultimo campo
151200170213     C                   IF        posA = *zeros
151300170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
151400170213     C* A questo punto "estraggo" il campo corrente
151500170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
151600170213     C                                         %subst(vindta:posDa:(posA-posDa))
151700170213     C                   LEAVE
151800170213     C                   ELSE
151900170213     C* A questo punto "estraggo" il campo corrente
152000170213     C                   EVAL      SkSplitFRCV(i) = %trim(prefix) +
152100170213     C                                         %subst(vindta:posDa:(posA-posDa))
152200170213     C                   ENDIF
152300170213     C                   ENDIF
152400170213     C* Incremento il contatore d campo
152500170213     C                   EVAL      i = i +1
152600170213     C                   ENDDO
152700170213     C*
152800170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
152900170213     C* delimitazione testo
153000170213     C                   EVAL      i = 1
153100170213     C                   DOW       i < %elem(SkSplitFRCV)
153200170213     C     CharTXT:' '   XLATE     SkSplitFRCV(i)SkSplitFRCV(i)
153300170213     C                   EVAL      SkSplitFRCV(i) = %trim(SkSplitFRCV(i))
153400170213     C                   EVAL      i = i + 1
153500170213     C                   ENDDO
153600170213     C                   EVAL      wGiro = 1
153700170213     C* Ripristino la 1° riga originale
153800170213     C                   EVAL      vinDTA = depvinDTA
153900170213     C                   ELSE
154000170213     C                   EVAL      wGiro = 2
154100170213     C**********
154200170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
154300170213     C* d separatore campo
154400170213     C**********
154500170213     C                   EVAL      vindta = %trim(vindta)
154600170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
154700170213     C                   EVAL      vindta = %subst(vindta:2)
154800170213     C                   ENDDO
154900170213     C*
155000170213     C                   Z-ADD     *zeros        lunghInput        4 0
155100170213     C                   EVAL      lunghInput = %len(%trim(vindta))
155200170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
155300170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
155400170213     C                   EVAL      lunghInput = %len(%trim(vindta))
155500170213     C                   ENDDO
155600170213     C**********
155700170213     C* Effettuo lo split del campo dati d input
155800170213     C**********
155900170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
156000170213     C                   DOW       posDa <= %len(%trim(vindta))
156100170213     C*
156200170213     C* Gestisco il 1° campo
156300170213     C                   IF        i = 1
156400170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
156500170213     C                   EVAL      posDa = 1
156600170213     C* Eseguo lo scan x trovare la fine del primo campo
156700170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
156800170213     C* A questo punto "estraggo" il campo corrente
156900170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
157000170213     C                                                    (posA-posDa))
157100170213     C* X i campi successivi al 1°
157200170213     C                   ELSE
157300170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
157400170213     C                   EVAL      posDa = posA + 1
157500170213     C* Verifico che nn vi sia il campo nn valorizzato
157600170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
157700170213     C* Se campo nn valorizzato skippo al prossimo
157800170213     C                   EVAL      SkSplitCSV(i) = *blanks
157900170213     C                   EVAL      posA  = posA + 1
158000170213     C                   ELSE
158100170213     C* Eseguo lo scan x trovare la fine del campo corrente
158200170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
158300170213     C* Gestisco l'ultimo campo
158400170213     C                   IF        posA = *zeros
158500170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
158600170213     C* A questo punto "estraggo" il campo corrente
158700170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
158800170213     C                                                    (posA-posDa))
158900170213     C                   LEAVE
159000170213     C                   ELSE
159100170213     C* A questo punto "estraggo" il campo corrente
159200170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
159300170213     C                                                    (posA-posDa))
159400170213     C                   ENDIF
159500170213     C                   ENDIF
159600170213     C                   ENDIF
159700170213     C* Incremento il contatore d campo
159800170213     C                   EVAL      i = i +1
159900170213     C                   ENDDO
160000170213     C*
160100170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
160200170213     C* delimitazione testo
160300170213     C                   EVAL      i = 1
160400170213     C                   DOW       i < %elem(SkSplitCSV)
160500170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
160600170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
160700170213     C                   EVAL      i = i + 1
160800170213     C                   ENDDO
160900170215     C*
161000170215     C* Inizializzazioni "specifiche"
161100170215     C                   exsr      INZVALRCV
161200170213     C*
161300170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
161400170213     C                   EVAL      i = 1
161500170213     C                   DOW       i < %elem(SkSplitFRCV)
161600170213     C*
161700170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
161800170213     C                   IF        SkSplitFRCV(i) <> *blanks AND
161900170213     C                             SkSplitCSV(i) <> *blanks
162000170213     C*
162100170213     C                   exsr      CARVALRCV
162200170213     C*
162300170213     C                   ENDIF
162400170213     C                   EVAL      i = i + 1
162500170213     C                   ENDDO
162600170213     C*
162700170213     C***  ===>  Gestione campi "particolari" fuori ciclo
162800170213     C*
162900170213     C* Se provincia non valorizzata la reperisco
163000170213     C* tramite TISI95R a seconda dei dati d instradamento presenti
163100170215     C                   IF        VABNZD = *blanks AND
163200170215     C                             VABPRD = *blanks
163300170213     C                   CLEAR                   TISI95DS
163400170213     C                   EVAL      I95TCN = '3'
163500170213     C                   Z-ADD     datcor        I95DAT
163600170213     C                   EVAL      I95NAR = VABNZD
163700170213     C                   EVAL      I95CAP = VABCAD
163800170213     C                   EVAL      I95LOC = VABLOD
163900170213     C                   CALL      'TISI95R'
164000170213     C                   PARM                    TISI95DS
164100170213     C                   EVAL      VABPRD = O95PRV
164200170213     C                   ENDIF
164300170213     C*
164400170213     C***  <===  -----------------------------------------
164500170213     C*
164600170213     C                   ENDIF
164700170213     C*
164800170213     C                   ENDSR
164900170213     C*----------------------------------------------------*
165000170213
165100170213
165200170213
165300170213     C*----------------------------------------------------*
165400170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
165500170213     C*----------------------------------------------------*
165600170213     C     IMPVABDLV     BEGSR
165700170213     C*
165800170213     C**********
165900170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
166000170213     C**********
166100170213     C                   IF        wGiro = *zeros
166200170213     C*
166300170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
166400170213     C                   EVAL      depvinDTA = vinDTA
166500170213     C*
166600170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
166700170213     C                   IF        delivery <> *blanks
166800170213     C                   EVAL      vindta = delivery
166900170213     C     ';':CharCSV   XLATE     vindta        vindta
167000170213     C                   ENDIF
167100170213     C*
167200170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
167300170213     C     minu:maiu     XLATE     vindta        vindta
167400170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
167500170213     C                   DOW       posDa <= %len(%trim(vindta))
167600170213     C*
167700170213     C* Gestisco il 1° campo
167800170213     C                   IF        i = 1
167900170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
168000170213     C                   EVAL      posDa = 1
168100170213     C* Eseguo lo scan x trovare la fine del primo campo
168200170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
168300170213     C* A questo punto "estraggo" il campo corrente
168400170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
168500170213     C                                         %subst(vindta:posDa:(posA-posDa))
168600170213     C* X i campi successivi al 1°
168700170213     C                   ELSE
168800170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
168900170213     C                   EVAL      posDa = posA + 1
169000170213     C* Eseguo lo scan x trovare la fine del campo corrente
169100170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
169200170213     C* Gestisco l'ultimo campo
169300170213     C                   IF        posA = *zeros
169400170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
169500170213     C* A questo punto "estraggo" il campo corrente
169600170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
169700170213     C                                         %subst(vindta:posDa:(posA-posDa))
169800170213     C                   LEAVE
169900170213     C                   ELSE
170000170213     C* A questo punto "estraggo" il campo corrente
170100170213     C                   EVAL      SkSplitFDLV(i) = %trim(prefix) +
170200170213     C                                         %subst(vindta:posDa:(posA-posDa))
170300170213     C                   ENDIF
170400170213     C                   ENDIF
170500170213     C* Incremento il contatore d campo
170600170213     C                   EVAL      i = i +1
170700170213     C                   ENDDO
170800170213     C*
170900170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
171000170213     C* delimitazione testo
171100170213     C                   EVAL      i = 1
171200170213     C                   DOW       i < %elem(SkSplitFDLV)
171300170213     C     CharTXT:' '   XLATE     SkSplitFDLV(i)SkSplitFDLV(i)
171400170213     C                   EVAL      SkSplitFDLV(i) = %trim(SkSplitFDLV(i))
171500170213     C                   EVAL      i = i + 1
171600170213     C                   ENDDO
171700170213     C                   EVAL      wGiro = 1
171800170213     C* Ripristino la 1° riga originale
171900170213     C                   EVAL      vinDTA = depvinDTA
172000170213     C                   ELSE
172100170213     C                   EVAL      wGiro = 2
172200170213     C**********
172300170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
172400170213     C* d separatore campo
172500170213     C**********
172600170213     C                   EVAL      vindta = %trim(vindta)
172700170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
172800170213     C                   EVAL      vindta = %subst(vindta:2)
172900170213     C                   ENDDO
173000170213     C*
173100170213     C                   Z-ADD     *zeros        lunghInput        4 0
173200170213     C                   EVAL      lunghInput = %len(%trim(vindta))
173300170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
173400170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
173500170213     C                   EVAL      lunghInput = %len(%trim(vindta))
173600170213     C                   ENDDO
173700170213     C**********
173800170213     C* Effettuo lo split del campo dati d input
173900170213     C**********
174000170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
174100170213     C                   DOW       posDa <= %len(%trim(vindta))
174200170213     C*
174300170213     C* Gestisco il 1° campo
174400170213     C                   IF        i = 1
174500170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
174600170213     C                   EVAL      posDa = 1
174700170213     C* Eseguo lo scan x trovare la fine del primo campo
174800170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
174900170213     C* A questo punto "estraggo" il campo corrente
175000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
175100170213     C                                                    (posA-posDa))
175200170213     C* X i campi successivi al 1°
175300170213     C                   ELSE
175400170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
175500170213     C                   EVAL      posDa = posA + 1
175600170213     C* Verifico che nn vi sia il campo nn valorizzato
175700170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
175800170213     C* Se campo nn valorizzato skippo al prossimo
175900170213     C                   EVAL      SkSplitCSV(i) = *blanks
176000170213     C                   EVAL      posA  = posA + 1
176100170213     C                   ELSE
176200170213     C* Eseguo lo scan x trovare la fine del campo corrente
176300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
176400170213     C* Gestisco l'ultimo campo
176500170213     C                   IF        posA = *zeros
176600170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
176700170213     C* A questo punto "estraggo" il campo corrente
176800170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
176900170213     C                                                    (posA-posDa))
177000170213     C                   LEAVE
177100170213     C                   ELSE
177200170213     C* A questo punto "estraggo" il campo corrente
177300170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
177400170213     C                                                    (posA-posDa))
177500170213     C                   ENDIF
177600170213     C                   ENDIF
177700170213     C                   ENDIF
177800170213     C* Incremento il contatore d campo
177900170213     C                   EVAL      i = i +1
178000170213     C                   ENDDO
178100170213     C*
178200170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
178300170213     C* delimitazione testo
178400170213     C                   EVAL      i = 1
178500170213     C                   DOW       i < %elem(SkSplitCSV)
178600170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
178700170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
178800170213     C                   EVAL      i = i + 1
178900170213     C                   ENDDO
179000170215     C*
179100170215     C* Inizializzazioni "specifiche"
179200170215     C                   exsr      INZVALDLV
179300170213     C*
179400170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
179500170213     C                   EVAL      i = 1
179600170213     C                   DOW       i < %elem(SkSplitFDLV)
179700170213     C*
179800170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
179900170213     C                   IF        SkSplitFDLV(i) <> *blanks AND
180000170213     C                             SkSplitCSV(i) <> *blanks
180100170213     C*
180200170213     C                   exsr      CARVALDLV
180300170213     C*
180400170213     C                   ENDIF
180500170213     C                   EVAL      i = i + 1
180600170213     C                   ENDDO
180700170213     C*
180800170213     C***  ===>  Gestione campi "particolari" fuori ciclo
180900170213     C*
181000170213     C*
181100170213     C***  <===  -----------------------------------------
181200170213     C*
181300170213     C                   ENDIF
181400170213     C*
181500170213     C                   ENDSR
181600170213     C*----------------------------------------------------*
181700170213
181800170213
181900170213
182000170213     C*----------------------------------------------------*
182100170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
182200170213     C*----------------------------------------------------*
182300170213     C     IMPVABCOD     BEGSR
182400170213     C*
182500170213     C**********
182600170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
182700170213     C**********
182800170213     C                   IF        wGiro = *zeros
182900170213     C*
183000170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
183100170213     C                   EVAL      depvinDTA = vinDTA
183200170213     C*
183300170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
183400170213     C                   IF        cod    <> *blanks
183500170213     C                   EVAL      vindta = cod
183600170213     C     ';':CharCSV   XLATE     vindta        vindta
183700170213     C                   ENDIF
183800170213     C*
183900170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
184000170213     C     minu:maiu     XLATE     vindta        vindta
184100170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
184200170213     C                   DOW       posDa <= %len(%trim(vindta))
184300170213     C*
184400170213     C* Gestisco il 1° campo
184500170213     C                   IF        i = 1
184600170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
184700170213     C                   EVAL      posDa = 1
184800170213     C* Eseguo lo scan x trovare la fine del primo campo
184900170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
185000170213     C* A questo punto "estraggo" il campo corrente
185100170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
185200170213     C                                         %subst(vindta:posDa:(posA-posDa))
185300170213     C* X i campi successivi al 1°
185400170213     C                   ELSE
185500170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
185600170213     C                   EVAL      posDa = posA + 1
185700170213     C* Eseguo lo scan x trovare la fine del campo corrente
185800170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
185900170213     C* Gestisco l'ultimo campo
186000170213     C                   IF        posA = *zeros
186100170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
186200170213     C* A questo punto "estraggo" il campo corrente
186300170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
186400170213     C                                         %subst(vindta:posDa:(posA-posDa))
186500170213     C                   LEAVE
186600170213     C                   ELSE
186700170213     C* A questo punto "estraggo" il campo corrente
186800170213     C                   EVAL      SkSplitFCOD(i) = %trim(prefix) +
186900170213     C                                         %subst(vindta:posDa:(posA-posDa))
187000170213     C                   ENDIF
187100170213     C                   ENDIF
187200170213     C* Incremento il contatore d campo
187300170213     C                   EVAL      i = i +1
187400170213     C                   ENDDO
187500170213     C*
187600170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
187700170213     C* delimitazione testo
187800170213     C                   EVAL      i = 1
187900170213     C                   DOW       i < %elem(SkSplitFCOD)
188000170213     C     CharTXT:' '   XLATE     SkSplitFCOD(i)SkSplitFCOD(i)
188100170213     C                   EVAL      SkSplitFCOD(i) = %trim(SkSplitFCOD(i))
188200170213     C                   EVAL      i = i + 1
188300170213     C                   ENDDO
188400170213     C                   EVAL      wGiro = 1
188500170213     C* Ripristino la 1° riga originale
188600170213     C                   EVAL      vinDTA = depvinDTA
188700170213     C                   ELSE
188800170213     C                   EVAL      wGiro = 2
188900170213     C**********
189000170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
189100170213     C* d separatore campo
189200170213     C**********
189300170213     C                   EVAL      vindta = %trim(vindta)
189400170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
189500170213     C                   EVAL      vindta = %subst(vindta:2)
189600170213     C                   ENDDO
189700170213     C*
189800170213     C                   Z-ADD     *zeros        lunghInput        4 0
189900170213     C                   EVAL      lunghInput = %len(%trim(vindta))
190000170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
190100170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
190200170213     C                   EVAL      lunghInput = %len(%trim(vindta))
190300170213     C                   ENDDO
190400170213     C**********
190500170213     C* Effettuo lo split del campo dati d input
190600170213     C**********
190700170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
190800170213     C                   DOW       posDa <= %len(%trim(vindta))
190900170213     C*
191000170213     C* Gestisco il 1° campo
191100170213     C                   IF        i = 1
191200170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
191300170213     C                   EVAL      posDa = 1
191400170213     C* Eseguo lo scan x trovare la fine del primo campo
191500170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
191600170213     C* A questo punto "estraggo" il campo corrente
191700170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
191800170213     C                                                    (posA-posDa))
191900170213     C* X i campi successivi al 1°
192000170213     C                   ELSE
192100170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
192200170213     C                   EVAL      posDa = posA + 1
192300170213     C* Verifico che nn vi sia il campo nn valorizzato
192400170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
192500170213     C* Se campo nn valorizzato skippo al prossimo
192600170213     C                   EVAL      SkSplitCSV(i) = *blanks
192700170213     C                   EVAL      posA  = posA + 1
192800170213     C                   ELSE
192900170213     C* Eseguo lo scan x trovare la fine del campo corrente
193000170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
193100170213     C* Gestisco l'ultimo campo
193200170213     C                   IF        posA = *zeros
193300170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
193400170213     C* A questo punto "estraggo" il campo corrente
193500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
193600170213     C                                                    (posA-posDa))
193700170213     C                   LEAVE
193800170213     C                   ELSE
193900170213     C* A questo punto "estraggo" il campo corrente
194000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
194100170213     C                                                    (posA-posDa))
194200170213     C                   ENDIF
194300170213     C                   ENDIF
194400170213     C                   ENDIF
194500170213     C* Incremento il contatore d campo
194600170213     C                   EVAL      i = i +1
194700170213     C                   ENDDO
194800170213     C*
194900170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
195000170213     C* delimitazione testo
195100170213     C                   EVAL      i = 1
195200170213     C                   DOW       i < %elem(SkSplitCSV)
195300170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
195400170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
195500170213     C                   EVAL      i = i + 1
195600170213     C                   ENDDO
195700170215     C*
195800170215     C* Inizializzazioni "specifiche"
195900170215     C                   exsr      INZVALCOD
196000170213     C*
196100170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
196200170213     C                   EVAL      i = 1
196300170213     C                   DOW       i < %elem(SkSplitFCOD)
196400170213     C*
196500170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
196600170213     C                   IF        SkSplitFCOD(i) <> *blanks AND
196700170213     C                             SkSplitCSV(i) <> *blanks
196800170213     C*
196900170213     C                   exsr      CARVALCOD
197000170213     C*
197100170213     C                   ENDIF
197200170213     C                   EVAL      i = i + 1
197300170213     C                   ENDDO
197400170213     C*
197500170213     C***  ===>  Gestione campi "particolari" fuori ciclo
197600170213     C*
197700170213     C* Considerazioni finali su CBO/CAS
197800170213     C                   IF        wFlgCAS = '1' OR
197900170213     C                             VABCAS > *zeros
198000170213     C                   IF        VABCBO = '1'
198100170213     C                   EVAL      VABCBO = '4'
198200170213     C                   ENDIF
198300170213     C                   IF        VABCBO = '2'
198400170213     C                   EVAL      VABCBO = '6'
198500170213     C                   ENDIF
198600170213     C                   ENDIF
198700170213     C*
198800170213     C* Eseguo routine finale x considerazioni specifiche su importi/divise
198900170213     C                   EXSR      CHKIMPDIV
199000170213     C*
199100170213     C***  <===  -----------------------------------------
199200170213     C*
199300170213     C                   ENDIF
199400170213     C*
199500170213     C                   ENDSR
199600170213     C*----------------------------------------------------*
199700090623
199800090623
199900090623
200000090623     C*----------------------------------------------------*
200100101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
200200090623     C*----------------------------------------------------*
200300170213     C     IMPVATPCL     BEGSR
200400090623     C*
200500090623     C**********
200600090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
200700090623     C**********
200800090623     C                   IF        wGiro = *zeros
200900090623     C*
201000090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
201100090623     C                   EVAL      depvinDTA = vinDTA
201200090623     C*
201300090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
201400110923     C                   IF        parcel <> *blanks
201500110923     C                   EVAL      vindta = parcel
201600090623     C     ';':CharCSV   XLATE     vindta        vindta
201700090623     C                   ENDIF
201800090623     C*
201900090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
202000090623     C     minu:maiu     XLATE     vindta        vindta
202100090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
202200090623     C                   DOW       posDa <= %len(%trim(vindta))
202300090623     C*
202400090623     C* Gestisco il 1° campo
202500090623     C                   IF        i = 1
202600090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
202700090623     C                   EVAL      posDa = 1
202800090623     C* Eseguo lo scan x trovare la fine del primo campo
202900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
203000090623     C* A questo punto "estraggo" il campo corrente
203100170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
203200110923     C                                         %subst(vindta:posDa:(posA-posDa))
203300090623     C* X i campi successivi al 1°
203400090623     C                   ELSE
203500090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
203600090623     C                   EVAL      posDa = posA + 1
203700090623     C* Eseguo lo scan x trovare la fine del campo corrente
203800090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
203900090623     C* Gestisco l'ultimo campo
204000090623     C                   IF        posA = *zeros
204100090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
204200090623     C* A questo punto "estraggo" il campo corrente
204300170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
204400110923     C                                         %subst(vindta:posDa:(posA-posDa))
204500090623     C                   LEAVE
204600090623     C                   ELSE
204700090623     C* A questo punto "estraggo" il campo corrente
204800170213     C                   EVAL      SkSplitFPCL(i) = %trim(prefix) +
204900110923     C                                         %subst(vindta:posDa:(posA-posDa))
205000090623     C                   ENDIF
205100090623     C                   ENDIF
205200090623     C* Incremento il contatore d campo
205300090623     C                   EVAL      i = i +1
205400090623     C                   ENDDO
205500090623     C*
205600090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
205700090623     C* delimitazione testo
205800090623     C                   EVAL      i = 1
205900170213     C                   DOW       i < %elem(SkSplitFPCL)
206000170213     C     CharTXT:' '   XLATE     SkSplitFPCL(i)SkSplitFPCL(i)
206100170213     C                   EVAL      SkSplitFPCL(i) = %trim(SkSplitFPCL(i))
206200090623     C                   EVAL      i = i + 1
206300090623     C                   ENDDO
206400090623     C                   EVAL      wGiro = 1
206500090623     C* Ripristino la 1° riga originale
206600090623     C                   EVAL      vinDTA = depvinDTA
206700090623     C                   ELSE
206800090623     C                   EVAL      wGiro = 2
206900090623     C**********
207000090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
207100090623     C* d separatore campo
207200090623     C**********
207300090623     C                   EVAL      vindta = %trim(vindta)
207400090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
207500090623     C                   EVAL      vindta = %subst(vindta:2)
207600090623     C                   ENDDO
207700090623     C*
207800090623     C                   Z-ADD     *zeros        lunghInput        4 0
207900090623     C                   EVAL      lunghInput = %len(%trim(vindta))
208000090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
208100090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
208200090623     C                   EVAL      lunghInput = %len(%trim(vindta))
208300090623     C                   ENDDO
208400090623     C**********
208500090623     C* Effettuo lo split del campo dati d input
208600090623     C**********
208700090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
208800090623     C                   DOW       posDa <= %len(%trim(vindta))
208900090623     C*
209000090623     C* Gestisco il 1° campo
209100090623     C                   IF        i = 1
209200090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
209300090623     C                   EVAL      posDa = 1
209400090623     C* Eseguo lo scan x trovare la fine del primo campo
209500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
209600090623     C* A questo punto "estraggo" il campo corrente
209700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
209800090623     C                                                    (posA-posDa))
209900090623     C* X i campi successivi al 1°
210000090623     C                   ELSE
210100090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
210200090623     C                   EVAL      posDa = posA + 1
210300090623     C* Verifico che nn vi sia il campo nn valorizzato
210400090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
210500090623     C* Se campo nn valorizzato skippo al prossimo
210600090623     C                   EVAL      SkSplitCSV(i) = *blanks
210700090623     C                   EVAL      posA  = posA + 1
210800090623     C                   ELSE
210900090623     C* Eseguo lo scan x trovare la fine del campo corrente
211000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
211100090623     C* Gestisco l'ultimo campo
211200090623     C                   IF        posA = *zeros
211300090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
211400090623     C* A questo punto "estraggo" il campo corrente
211500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
211600090623     C                                                    (posA-posDa))
211700090623     C                   LEAVE
211800090623     C                   ELSE
211900090623     C* A questo punto "estraggo" il campo corrente
212000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
212100090623     C                                                    (posA-posDa))
212200090623     C                   ENDIF
212300090623     C                   ENDIF
212400090623     C                   ENDIF
212500090623     C* Incremento il contatore d campo
212600090623     C                   EVAL      i = i +1
212700090623     C                   ENDDO
212800090623     C*
212900090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
213000090623     C* delimitazione testo
213100090623     C                   EVAL      i = 1
213200090623     C                   DOW       i < %elem(SkSplitCSV)
213300090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
213400090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
213500090623     C                   EVAL      i = i + 1
213600090623     C                   ENDDO
213700170215     C*
213800170215     C* Inizializzazioni "specifiche"
213900170215     C                   exsr      INZVALPCL
214000090623     C*
214100090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
214200090623     C                   EVAL      i = 1
214300170213     C                   DOW       i < %elem(SkSplitFPCL)
214400090623     C*
214500090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
214600170213     C                   IF        SkSplitFPCL(i) <> *blanks AND
214700090623     C                             SkSplitCSV(i) <> *blanks
214800090623     C*
214900170213     C                   exsr      CARVALPCL
215000090623     C*
215100090623     C                   ENDIF
215200090623     C                   EVAL      i = i + 1
215300090623     C                   ENDDO
215400090623     C*
215500090623     C***  ===>  Gestione campi "particolari" fuori ciclo
215600111118     C*
215700170213     C*** VATNOT_E_4
215800170213     C                   EVAL      wVATNOT_E_4 = savNAZISORCV
215900170213     C*
216000170213     C* Compongo il "chi sono" completo
216100170213     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
216200111118     C                                         %trim(wVATNOT_E_2) +
216300111118     C                                         %trim(wVATNOT_E_3) +
216400111118     C                                         %trim(wVATNOT_E_4)
216500090623     C*
216600090623     C***  <===  -----------------------------------------
216700090623     C*
216800090623     C                   ENDIF
216900090623     C*
217000090623     C                   ENDSR
217100090623     C*----------------------------------------------------*
217200170213
217300170213
217400170213
217500170213     C*----------------------------------------------------*
217600170213     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
217700170213     C*----------------------------------------------------*
217800170213     C     IMPVATMSG     BEGSR
217900170213     C*
218000170213     C**********
218100170213     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
218200170213     C**********
218300170213     C                   IF        wGiro = *zeros
218400170213     C*
218500170213     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
218600170213     C                   EVAL      depvinDTA = vinDTA
218700170213     C*
218800170213     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
218900170213     C                   IF        msg    <> *blanks
219000170213     C                   EVAL      vindta = msg
219100170213     C     ';':CharCSV   XLATE     vindta        vindta
219200170213     C                   ENDIF
219300170213     C*
219400170213     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
219500170213     C     minu:maiu     XLATE     vindta        vindta
219600170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
219700170213     C                   DOW       posDa <= %len(%trim(vindta))
219800170213     C*
219900170213     C* Gestisco il 1° campo
220000170213     C                   IF        i = 1
220100170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
220200170213     C                   EVAL      posDa = 1
220300170213     C* Eseguo lo scan x trovare la fine del primo campo
220400170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
220500170213     C* A questo punto "estraggo" il campo corrente
220600170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
220700170213     C                                         %subst(vindta:posDa:(posA-posDa))
220800170213     C* X i campi successivi al 1°
220900170213     C                   ELSE
221000170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
221100170213     C                   EVAL      posDa = posA + 1
221200170213     C* Eseguo lo scan x trovare la fine del campo corrente
221300170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
221400170213     C* Gestisco l'ultimo campo
221500170213     C                   IF        posA = *zeros
221600170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
221700170213     C* A questo punto "estraggo" il campo corrente
221800170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
221900170213     C                                         %subst(vindta:posDa:(posA-posDa))
222000170213     C                   LEAVE
222100170213     C                   ELSE
222200170213     C* A questo punto "estraggo" il campo corrente
222300170213     C                   EVAL      SkSplitFMSG(i) = %trim(prefix) +
222400170213     C                                         %subst(vindta:posDa:(posA-posDa))
222500170213     C                   ENDIF
222600170213     C                   ENDIF
222700170213     C* Incremento il contatore d campo
222800170213     C                   EVAL      i = i +1
222900170213     C                   ENDDO
223000170213     C*
223100170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
223200170213     C* delimitazione testo
223300170213     C                   EVAL      i = 1
223400170213     C                   DOW       i < %elem(SkSplitFMSG)
223500170213     C     CharTXT:' '   XLATE     SkSplitFMSG(i)SkSplitFMSG(i)
223600170213     C                   EVAL      SkSplitFMSG(i) = %trim(SkSplitFMSG(i))
223700170213     C                   EVAL      i = i + 1
223800170213     C                   ENDDO
223900170213     C                   EVAL      wGiro = 1
224000170213     C* Ripristino la 1° riga originale
224100170213     C                   EVAL      vinDTA = depvinDTA
224200170213     C                   ELSE
224300170213     C                   EVAL      wGiro = 2
224400170213     C**********
224500170213     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
224600170213     C* d separatore campo
224700170213     C**********
224800170213     C                   EVAL      vindta = %trim(vindta)
224900170213     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
225000170213     C                   EVAL      vindta = %subst(vindta:2)
225100170213     C                   ENDDO
225200170213     C*
225300170213     C                   Z-ADD     *zeros        lunghInput        4 0
225400170213     C                   EVAL      lunghInput = %len(%trim(vindta))
225500170213     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
225600170213     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
225700170213     C                   EVAL      lunghInput = %len(%trim(vindta))
225800170213     C                   ENDDO
225900170213     C**********
226000170213     C* Effettuo lo split del campo dati d input
226100170213     C**********
226200170213     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
226300170213     C                   DOW       posDa <= %len(%trim(vindta))
226400170213     C*
226500170213     C* Gestisco il 1° campo
226600170213     C                   IF        i = 1
226700170213     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
226800170213     C                   EVAL      posDa = 1
226900170213     C* Eseguo lo scan x trovare la fine del primo campo
227000170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
227100170213     C* A questo punto "estraggo" il campo corrente
227200170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
227300170213     C                                                    (posA-posDa))
227400170213     C* X i campi successivi al 1°
227500170213     C                   ELSE
227600170213     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
227700170213     C                   EVAL      posDa = posA + 1
227800170213     C* Verifico che nn vi sia il campo nn valorizzato
227900170213     C                   IF        %subst(vindta:posDa:1) = CharCSV
228000170213     C* Se campo nn valorizzato skippo al prossimo
228100170213     C                   EVAL      SkSplitCSV(i) = *blanks
228200170213     C                   EVAL      posA  = posA + 1
228300170213     C                   ELSE
228400170213     C* Eseguo lo scan x trovare la fine del campo corrente
228500170213     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
228600170213     C* Gestisco l'ultimo campo
228700170213     C                   IF        posA = *zeros
228800170213     C                   EVAL      posA = %len(%trim(vindta)) + 1
228900170213     C* A questo punto "estraggo" il campo corrente
229000170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
229100170213     C                                                    (posA-posDa))
229200170213     C                   LEAVE
229300170213     C                   ELSE
229400170213     C* A questo punto "estraggo" il campo corrente
229500170213     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
229600170213     C                                                    (posA-posDa))
229700170213     C                   ENDIF
229800170213     C                   ENDIF
229900170213     C                   ENDIF
230000170213     C* Incremento il contatore d campo
230100170213     C                   EVAL      i = i +1
230200170213     C                   ENDDO
230300170213     C*
230400170213     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
230500170213     C* delimitazione testo
230600170213     C                   EVAL      i = 1
230700170213     C                   DOW       i < %elem(SkSplitCSV)
230800170213     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
230900170213     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
231000170213     C                   EVAL      i = i + 1
231100170213     C                   ENDDO
231200170215     C*
231300170215     C* Inizializzazioni "specifiche"
231400170215     C                   exsr      INZVALMSG
231500170213     C*
231600170213     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
231700170213     C                   EVAL      i = 1
231800170213     C                   DOW       i < %elem(SkSplitFMSG)
231900170213     C*
232000170213     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
232100170213     C                   IF        SkSplitFMSG(i) <> *blanks AND
232200170213     C                             SkSplitCSV(i) <> *blanks
232300170213     C*
232400170213     C                   exsr      CARVALMSG
232500170213     C*
232600170213     C                   ENDIF
232700170213     C                   EVAL      i = i + 1
232800170213     C                   ENDDO
232900170213     C*
233000170213     C***  ===>  Gestione campi "particolari" fuori ciclo
233100170213     C*
233200170213     C* Solo se messaggio diretto al destinatario
233300170213     C                   IF        wMsgDest = '0'
233400170213     C                   IF        wMsgType = 'EML'
233500170213     C                   EVAL      wVATNOT_IJ = wMsgValue
233600170213     C                   ENDIF
233700170213     C                   IF        wMsgType = 'SMS'
233800170213     C                   EVAL      wVATNOT_S  = wMsgValue
233900170213     C                   ENDIF
234000170213     C*
234100170803     C***                IF        wPredictSms <> 'S'
234200170803     C***                EVAL      %subst(wVATNOT_S:17:1) = 'N'
234300170803     C***                ENDIF
234400170803     C***                IF        wPredictEml <> 'S'
234500170803     C***                EVAL      %subst(wVATNOT_S:18:1) = 'N'
234600170803     C***                ENDIF
234700170213     C                   ENDIF
234800170213     C*
234900170213     C***  <===  -----------------------------------------
235000170213     C*
235100170213     C                   ENDIF
235200170213     C*
235300170213     C                   ENDSR
235400170213     C*----------------------------------------------------*
235500170215
235600170215
235700170215
235800170215     C*----------------------------------------------------*
235900170215     C*  ROUTINE DI INNIZIALIZZAZIONI
236000170215     C*----------------------------------------------------*
236100170215     C     INZVALSHP     BEGSR
236200170215     C*
236300170215     C*
236400170215     C                   ENDSR
236500170215     C***
236600170215
236700170215
236800170215
236900170215     C*----------------------------------------------------*
237000170215     C*  ROUTINE DI INNIZIALIZZAZIONI
237100170215     C*----------------------------------------------------*
237200170215     C     INZVALSND     BEGSR
237300170215     C*
237400170215     C*
237500170215     C                   ENDSR
237600170215     C***
237700170215
237800170215
237900170215
238000170215     C*----------------------------------------------------*
238100170215     C*  ROUTINE DI INNIZIALIZZAZIONI
238200170215     C*----------------------------------------------------*
238300170215     C     INZVALRCV     BEGSR
238400170215     C*
238500170215     C                   CLEAR                   savNAZISORCV
238600170215     C                   CLEAR                   wVATNOT_A
238700170215     C                   CLEAR                   wVATNOT_B
238800170215     C                   CLEAR                   wVATNOT_E_1
238900170215     C*
239000170215     C                   ENDSR
239100170215     C***
239200170215
239300170215
239400170215
239500170215     C*----------------------------------------------------*
239600170215     C*  ROUTINE DI INNIZIALIZZAZIONI
239700170215     C*----------------------------------------------------*
239800170215     C     INZVALDLV     BEGSR
239900170215     C*
240000170215     C*
240100170215     C                   ENDSR
240200170215     C***
240300170215
240400170215
240500170215
240600170215     C*----------------------------------------------------*
240700170215     C*  ROUTINE DI INNIZIALIZZAZIONI
240800170215     C*----------------------------------------------------*
240900170215     C     INZVALCOD     BEGSR
241000170215     C*
241100170215     C*
241200170215     C                   ENDSR
241300170215     C***
241400170215
241500170215
241600170215
241700170215     C*----------------------------------------------------*
241800170215     C*  ROUTINE DI INNIZIALIZZAZIONI
241900170215     C*----------------------------------------------------*
242000170215     C     INZVALPCL     BEGSR
242100170215     C*
242200170215     C                   CLEAR                   wVATNOT_E_2
242300170215     C                   CLEAR                   wVATNOT_E_3
242400170215     C*
242500170215     C                   ENDSR
242600170215     C***
242700170215
242800170215
242900170215
243000170215     C*----------------------------------------------------*
243100170215     C*  ROUTINE DI INNIZIALIZZAZIONI
243200170215     C*----------------------------------------------------*
243300170215     C     INZVALMSG     BEGSR
243400170215     C*
243500170215     C* Inizializzazioni "specifiche"
243600170215     C                   MOVEL     *blanks       wMsgType          3
243700170215     C                   MOVEL     *blanks       wMsgDest          1
243800170215     C                   MOVEL     *blanks       wPredictEml       1
243900170215     C                   MOVEL     *blanks       wPredictSms       1
244000170215     C                   MOVEL     *blanks       wMsgValue        70
244100170215     C*
244200170215     C                   ENDSR
244300170215     C***
244400170213
244500090623
244600090623
244700090623     C*----------------------------------------------------*
244800090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
244900090623     C*----------------------------------------------------*
245000170213     C     CARVALSHP     BEGSR
245100090623     C*
245200111117     C*** NSP
245300170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSID'
245400170213     C                   EVAL      PiStr=SkSplitCSV(i)
245500090623     C                   EXSR      CHKNUM
245600090623     C                   IF        PiInt=*on
245700090623     C                   Z-ADD     PiVal         VABNSP
245800090623     C                   ELSE
245900090623     C                   SETON                                        31
246000090623     C                   EVAL      VABNSP = *zeros
246100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
246200090623     C                             + ' ' + 'VABNSP'
246300090623     C                   ENDIF
246400090623     C                   ENDIF
246500090623     C*** RMN
246600170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSID'
246700170213     C                   EVAL      PiStr=SkSplitCSV(i)
246800090623     C                   EXSR      CHKNUM
246900090623     C                   IF        PiInt=*on
247000090623     C                   Z-ADD     PiVal         VABRMN
247100090623     C                   ELSE
247200090623     C                   SETON                                        32
247300090623     C                   EVAL      VABRMN = *zeros
247400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
247500090623     C                             + ' ' + 'VABRMN'
247600090623     C                   ENDIF
247700090623     C                   ENDIF
247800170213     C*** RMA
247900170720     C***                IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSCREF1'
248000170720     C***                EVAL      VABRMA = SkSplitCSV(i)
248100170720     C***                ENDIF
248200090623     C*** NCL
248300170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSCOUNT'
248400090623     C                   EVAL      PiStr=SkSplitCSV(i)
248500090623     C                   EXSR      CHKNUM
248600090623     C                   IF        PiInt=*on
248700090623     C                   Z-ADD     PiVal         VABNCL
248800090623     C                   ELSE
248900090623     C                   SETON                                        32
249000090623     C                   EVAL      VABNCL = *zeros
249100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
249200090623     C                             + ' ' + 'VABNCL'
249300090623     C                   ENDIF
249400090623     C                   ENDIF
249500170213     C*** VLB
249600170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSVOLUME'
249700170213     C                   IF        SkSplitCSV(i) <> *blanks AND
249800170213     C                             SkSplitCSV(i)  > *zeros
249900170213     C                   EVAL      PiStr=SkSplitCSV(i)
250000170213     C                   EXSR      CHKNUM
250100170213     C                   IF        PiNum=*on
250200170213     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
250300170213     C                   Z-ADD     PiVal         VABVLB
250400170213     C                   ELSE
250500170213     C                   SETON                                        32
250600170213     C                   EVAL      VABVLB = *zeros
250700170213     C                   EVAL      vinmsg = %trimr(vinmsg)
250800170213     C                             + ' ' + 'VABVLB'
250900170213     C                   ENDIF
251000170213     C                   ENDIF
251100170213     C                   ENDIF
251200090623     C*** PKB
251300170213     C                   IF        %trim(SkSplitFSHP(i)) = 'SHP_MPSWEIGHT'
251400090623     C                   EVAL      PiStr=SkSplitCSV(i)
251500090623     C                   EXSR      CHKNUM
251600090623     C                   IF        PiNum=*on
251700130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
251800090623     C                   Z-ADD     PiVal         VABPKB
251900090623     C                   ELSE
252000090623     C                   SETON                                        32
252100090623     C                   EVAL      VABPKB = *zeros
252200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
252300090623     C                             + ' ' + 'VABPKB'
252400090623     C                   ENDIF
252500170213     C                   ENDIF
252600090623     C*
252700090623     C***  ===>  Gestione campi "particolari" dentro ciclo
252800090623     C*
252900090623     C*
253000090623     C***  <===  -----------------------------------------
253100090908     C*
253200090623     C*
253300090623     C                   ENDSR
253400090623     C***
253500170213
253600170213
253700170213
253800170213     C*----------------------------------------------------*
253900170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
254000170213     C*----------------------------------------------------*
254100170213     C     CARVALSND     BEGSR
254200170213     C*
254300170213     C*** RMO
254400170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOMPNAME'
254500170213     C                   EVAL      VABRMO = SkSplitCSV(i)
254600170213     C                   ENDIF
254700170213     C*** CMO
254800170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SZIPCODE'
254900170213     C                   EVAL      VABCMO = SkSplitCSV(i)
255000170213     C                   ENDIF
255100170213     C*** NMO
255200170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOUNTRYCODE'
255300170213     C                   EVAL      savNAZISO = %dec(%trim(SkSplitCSV(i)):3:0)
255400170213     C                   Z-ADD     1             jNAZ
255500170213     C     savNAZISO     LOOKUP    skNAZISO(jNAZ)                         13
255600170213     C                   IF        %found
255700170213     C                   EVAL      VABNMO = skNAZBAR(jNAZ)
255800170213     C                   ELSE
255900170213     C                   EVAL      VABNMO = %editc(savNAZISO:'X')
256000170213     C                   ENDIF
256100170213     C                   ENDIF
256200170213     C*** NOT
256300170213     C                   IF        %trim(SkSplitFSND(i)) = 'SND_SCOMMENT'
256400170213     C                   EVAL      wNOTE = %trim(%trim(wNOTE)+' '+SkSplitCSV(i))
256500170213     C                   ENDIF
256600170213     C*
256700170213     C***  ===>  Gestione campi "particolari" dentro ciclo
256800170213     C*
256900170213     C*
257000170213     C***  <===  -----------------------------------------
257100170213     C*
257200170213     C*
257300170213     C                   ENDSR
257400170213     C***
257500170213
257600170213
257700170213
257800170213     C*----------------------------------------------------*
257900170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
258000170213     C*----------------------------------------------------*
258100170213     C     CARVALRCV     BEGSR
258200170213     C*
258300170213     C*** RSD 1/2
258400170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RNAME1'
258500170802     C***                EVAL      VABRSD = SkSplitCSV(i)
258600170802     C                   EVAL      wDEST = %trim(wDEST)+' '+%trim(SkSplitCSV(i))
258700170213     C* ==
258800170213     C                   ENDIF
258900170802     C*** RSD 2/2
259000170802     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMPNAME'
259100170802     C***                EVAL      VABRSD=%trim(%trim(VABRSD)+' '+SkSplitCSV(i))
259200170802     C                   EVAL      wDEST = %trim(wDEST)+' '+%trim(SkSplitCSV(i))
259300170802     C                   ENDIF
259400170213     C*** RD2 1/2
259500170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RNAME2'
259600170802     C***                EVAL      VABRD2 = SkSplitCSV(i)
259700170802     C                   EVAL      wDEST = %trim(wDEST)+' '+%trim(SkSplitCSV(i))
259800170213     C                   ENDIF
259900170213     C*** RS2 2/2
260000170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMPNAME2'
260100170802     C***                EVAL      VABRD2=%trim(%trim(VABRD2)+' '+SkSplitCSV(i))
260200170802     C                   EVAL      wDEST = %trim(wDEST)+' '+%trim(SkSplitCSV(i))
260300170213     C                   ENDIF
260400170213     C*** IND 1/4
260500170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RSTREET'
260600170213     C                   EVAL      VABIND = SkSplitCSV(i)
260700170213     C                   ENDIF
260800170213     C*** IND 2/4
260900170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RPROPNUM'
261000170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
261100170213     C                   ENDIF
261200170213     C*** IND 3/4
261300170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RADD2'
261400170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
261500170213     C                   ENDIF
261600170213     C*** IND 4/4
261700170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RADD3'
261800170213     C                   EVAL      VABIND=%trim(%trim(VABIND)+' '+SkSplitCSV(i))
261900170213     C                   ENDIF
262000170213     C*** CAD
262100170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RZIPCODE'
262200170213     C                   EVAL      VABCAD = SkSplitCSV(i)
262300170213     C                   MOVEL     *blanks       wAlfa7            7
262400170213     C                   MOVEL     *zeros        wAlfa7z           7
262500170213     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
262600170213     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
262700170213     C                   ENDIF
262800170213     C*** LOD
262900170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RTOWN'
263000170213     C                   EVAL      VABLOD = SkSplitCSV(i)
263100170213     C                   ENDIF
263200170213     C*** NZD
263300170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOUNTRYCODE'
263400170213     C                   EVAL      savNAZISO = %dec(%trim(SkSplitCSV(i)):3:0)
263500170213     C                   EVAL      savNAZISORCV = %editc(savNAZISO:'X')
263600170213     C                   Z-ADD     1             jNAZ
263700170213     C     savNAZISO     LOOKUP    skNAZISO(jNAZ)                         13
263800170213     C                   IF        %found
263900170213     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
264000170213     C                   ELSE
264100170213     C                   EVAL      VABNZD = savNAZISORCV
264200170213     C                   ENDIF
264300170213     C                   IF        savNAZISO <> 380                             * 380 = Italia
264400170213     C   50              SETON                                        33
264500170213     C                   ENDIF
264600170213     C                   ENDIF
264700170213     C*** NOT_A
264800170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCONTACT'
264900170213     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
265000170213     C                   ENDIF
265100170213     C*** NOT_B
265200170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCONTACTPHO1'
265300170213     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
265400170213     C                   ENDIF
265500170213     C*** NOT
265600170213     C                   IF        %trim(SkSplitFRCV(i)) = 'RCV_RCOMMENT'
265700170213     C                   EVAL      wNOTE = %trim(%trim(wNOTE)+' '+SkSplitCSV(i))
265800170213     C                   ENDIF
265900170213     C*
266000170213     C***  ===>  Gestione campi "particolari" dentro ciclo
266100170213     C*
266200170213     C*
266300170213     C***  <===  -----------------------------------------
266400170213     C*
266500170213     C*
266600170213     C                   ENDSR
266700170213     C***
266800170213
266900170213
267000170213
267100170213     C*----------------------------------------------------*
267200170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
267300170213     C*----------------------------------------------------*
267400170213     C     CARVALDLV     BEGSR
267500170213     C*
267600170213     C*** DCR
267700170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_DELIVERYDATE_FROM'
267800170213     C                   IF        SkSplitCSV(i) > *zeros
267900170213     C                   EVAL      PiStr=SkSplitCSV(i)
268000170213     C                   EXSR      CHKNUM
268100170213     C                   IF        PiInt=*on
268200170213     C                   Z-ADD     PiVal         VABDCR
268300170213     C                   ELSE
268400170213     C                   SETON                                        32
268500170213     C                   EVAL      VABDCR = *zeros
268600170213     C                   EVAL      vinmsg = %trimr(vinmsg)
268700170213     C                             + ' ' + 'VABDCR'
268800170213     C                   ENDIF
268900170213     C                   ENDIF
269000170213     C                   ENDIF
269100170213     C*
269200170213     C*** DCR
269300170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_DELIVERYDATE_TO'
269400170213     C                   IF        SkSplitCSV(i) > *zeros
269500170213     C                   EVAL      PiStr=SkSplitCSV(i)
269600170213     C                   EXSR      CHKNUM
269700170213     C                   IF        PiInt=*on
269800170213     C                   Z-ADD     PiVal         VABDCR
269900170213     C                   ELSE
270000170213     C                   SETON                                        32
270100170213     C                   EVAL      VABDCR = *zeros
270200170213     C                   EVAL      vinmsg = %trimr(vinmsg)
270300170213     C                             + ' ' + 'VABDCR'
270400170213     C                   ENDIF
270500170213     C                   ENDIF
270600170213     C                   ENDIF
270700170213     C*
270800170213     C*** HCR
270900170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_TIMEFRAME_FROM'
271000170213     C                   IF        SkSplitCSV(i) > *zeros
271100170213     C                   EVAL      PiStr=SkSplitCSV(i)
271200170213     C                   EXSR      CHKNUM
271300170213     C                   IF        PiInt=*on
271400170213     C                   Z-ADD     PiVal         VABHCR
271500170213     C                   ELSE
271600170213     C                   SETON                                        32
271700170213     C                   EVAL      VABHCR = *zeros
271800170213     C                   EVAL      vinmsg = %trimr(vinmsg)
271900170213     C                             + ' ' + 'VABHCR'
272000170213     C                   ENDIF
272100170213     C                   ENDIF
272200170213     C                   ENDIF
272300170213     C*
272400170213     C*** HCR
272500170213     C                   IF        %trim(SkSplitFDLV(i))='DLV_TIMEFRAME_TO'
272600170213     C                   IF        SkSplitCSV(i) > *zeros
272700170213     C                   EVAL      PiStr=SkSplitCSV(i)
272800170213     C                   EXSR      CHKNUM
272900170213     C                   IF        PiInt=*on
273000170213     C                   Z-ADD     PiVal         VABHCR
273100170213     C                   ELSE
273200170213     C                   SETON                                        32
273300170213     C                   EVAL      VABHCR = *zeros
273400170213     C                   EVAL      vinmsg = %trimr(vinmsg)
273500170213     C                             + ' ' + 'VABHCR'
273600170213     C                   ENDIF
273700170213     C                   ENDIF
273800170213     C                   ENDIF
273900170213     C*
274000170213     C***  ===>  Gestione campi "particolari" dentro ciclo
274100170213     C*
274200170213     C*
274300170213     C***  <===  -----------------------------------------
274400170213     C*
274500170213     C*
274600170213     C                   ENDSR
274700170213     C***
274800170213
274900170213
275000170213
275100170213     C*----------------------------------------------------*
275200170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
275300170213     C*----------------------------------------------------*
275400170213     C     CARVALCOD     BEGSR
275500170213     C*
275600170213     C*
275700170213     C* Reperisco quindi i campi numerici...
275800170213     C*** CAS
275900170213     C                   IF        %trim(SkSplitFCOD(i)) = 'COD_NAMOUNT'
276000170213     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
276100170213     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
276200170213     C                             %trim(SkSplitCSV(i)) <> '0'      AND
276300170213     C                             %trim(SkSplitCSV(i)) <> '0,0'    AND
276400170213     C                             %trim(SkSplitCSV(i)) <> '0,00'   AND
276500170213     C                             %trim(SkSplitCSV(i)) <> '0,000'  AND
276600170213     C                             %trim(SkSplitCSV(i)) <> '0.0'    AND
276700170213     C                             %trim(SkSplitCSV(i)) <> '0.00'   AND
276800170213     C                             %trim(SkSplitCSV(i)) <> '0.000'
276900170213     C                   EVAL      wFlgCAS = '1'
277000170213     C                   EVAL      PiStr=SkSplitCSV(i)
277100170213     C                   EXSR      CHKNUM
277200170213     C                   IF        PiNum=*on
277300170213     C                   EVAL      VABCAS = PiVal
277400170213     C                   ELSE
277500170213     C                   SETON                                        32
277600170213     C                   EVAL      VABCAS = *zeros
277700170213     C                   EVAL      vinmsg = %trimr(vinmsg)
277800170213     C                             + ' ' + 'VABCAS'
277900170213     C                   ENDIF
278000170213     C                   ENDIF
278100170213     C                   ENDIF
278200170213     C*** VCA
278300170213     C                   IF        %trim(SkSplitFCOD(i)) = 'COD_NCURRENCY'
278400170213     C                   EVAL      VABVCA = %trim(SkSplitCSV(i))
278500170213     C                   ENDIF
278600170213     C*
278700170213     C***  ===>  Gestione campi "particolari" dentro ciclo
278800170213     C*
278900170213     C*
279000170213     C***  <===  -----------------------------------------
279100170213     C*
279200170213     C*
279300170213     C                   ENDSR
279400170213     C***
279500090623
279600111118
279700111118
279800111118     C*----------------------------------------------------*
279900111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
280000111118     C*----------------------------------------------------*
280100170213     C     CARVALPCL     BEGSR
280200111118     C*
280300111118     C*** VATNOT_E_2
280400170213     C                   IF        %trim(SkSplitFPCL(i)) = 'PCL_PARCELNUMBER'
280500111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
280600111118     C                   ENDIF
280700111118     C*** VATNOT_E_3
280800170213     C                   IF        %trim(SkSplitFPCL(i)) = 'PCL_SERVICECODE'
280900170619     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
281000170619     C                   IF        %trim(SkSplitCSV(i)) = '109'
281100170619     C                   EVAL      wFlgCAS = '1'
281200170619     C                   ENDIF
281300111118     C                   ENDIF
281400170720     C*** RMA
281500170721     C                   IF        %trim(SkSplitFPCL(i)) = 'PCL_SENDPARCELREF1'
281600170720     C                   EVAL      VABRMA = SkSplitCSV(i)
281700170720     C                   ENDIF
281800111118     C*
281900111118     C***  ===>  Gestione campi "particolari" dentro ciclo
282000111118     C*
282100111118     C*
282200111118     C***  <===  -----------------------------------------
282300111118     C*
282400111118     C*
282500111118     C                   ENDSR
282600111118     C***
282700170213
282800170213
282900170213
283000170213     C*----------------------------------------------------*
283100170213     C*  ROUTINE DI VALORIZZAZIONE CAMPI
283200170213     C*----------------------------------------------------*
283300170213     C     CARVALMSG     BEGSR
283400170213     C*
283500170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGTYPE'
283600170213     C                   SELECT
283700170213     C                   WHEN      %trim(SkSplitCSV(i)) = '1'
283800170213     C                   EVAL      wMsgType = 'EML'
283900170213     C                   WHEN      %trim(SkSplitCSV(i)) = '3'
284000170213     C                   EVAL      wMsgType = 'SMS'
284100170213     C                   ENDSL
284200170213     C                   ENDIF
284300170213     C*
284400170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGDESTINATION'
284500170213     C                   EVAL      wMsgValue = %trim(SkSplitCSV(i))
284600170213     C                   ENDIF
284700170213     C*
284800170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGDESTTYPE'
284900170213     C                   EVAL      wMsgDest = %trim(SkSplitCSV(i))
285000170213     C                   ENDIF
285100170213     C*
285200170213     C                   IF        %trim(SkSplitFMSG(i)) = 'MSG_MSGTRIGGER'
285300170607     C***                IF        %trim(SkSplitCSV(i)) = '904'
285400170213     C                   SELECT
285500170213     C                   WHEN      wMsgType='EML'
285600170213     C                   EVAL      wPredictEml = 'S'
285700170213     C                   WHEN      wMsgType='SMS'
285800170213     C                   EVAL      wPredictSms = 'S'
285900170213     C                   ENDSL
286000170607     C***                ENDIF
286100170213     C                   ENDIF
286200170213     C*
286300170213     C***  ===>  Gestione campi "particolari" dentro ciclo
286400170213     C*
286500170213     C*
286600170213     C***  <===  -----------------------------------------
286700170213     C*
286800170213     C*
286900170213     C                   ENDSR
287000170213     C***
287100161011
287200010601
287300020204
287400020204     C*----------------------------------------------------*
287500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
287600020204     C*----------------------------------------------------*
287700020204     C     CHKIMPDIV     BEGSR
287800020204     C*
287900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
288000020204     C                   Z-ADD     *zeros        wrkDec            9 9
288100020204     C*
288200020204     C* Come prima cosa effettuo considerazioni sulla divisa
288300020204     C                   IF        vabIAS > *zeros
288400020204     C                   IF        vabVAS <> 'EUR'
288500020204     C                   EVAL      vabVAS =  'ITL'
288600020204     C                   ENDIF
288700020204     C                   ENDIF
288800020204     C*
288900020204     C                   IF        vabCAS > *zeros
289000020204     C                   IF        vabVCA <> 'EUR'
289100020204     C                   EVAL      vabVCA =  'ITL'
289200020204     C                   ENDIF
289300020204     C                   ENDIF
289400020204     C*
289500020204     C                   IF        vabVMD > *zeros
289600020204     C                   IF        vabVAD <> 'EUR'
289700020204     C                   EVAL      vabVAD =  'ITL'
289800020204     C                   ENDIF
289900020204     C                   ENDIF
290000020204     C*
290100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
290200020204     C                   Z-ADD     vabIAS        wrkDec
290300020204     C                   IF        wrkDec > *zeros
290400020204     C                   IF        vabVAS = 'ITL'
290500020204     C                   EVAL      vabIAS = *zeros
290600020204     C                   ENDIF
290700020204     C                   ENDIF
290800020204     C*
290900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
291000020204     C                   Z-ADD     vabCAS        wrkDec
291100020204     C                   IF        wrkDec > *zeros
291200020204     C                   IF        vabVCA = 'ITL'
291300020204     C                   EVAL      vabCAS = *zeros
291400020204     C                   ENDIF
291500020204     C                   ENDIF
291600020204     C*
291700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
291800020204     C                   Z-ADD     vabVMD        wrkDec
291900020204     C                   IF        wrkDec > *zeros
292000020204     C                   IF        vabVAD = 'ITL'
292100020204     C                   EVAL      vabVMD = *zeros
292200020204     C                   ENDIF
292300020204     C                   ENDIF
292400020204     C*
292500020204     C                   ENDSR
292600020204     C***
292700080617
292800080617
292900080617
293000080617     C*----------------------------------------------------*
293100080617     C*  CONTROLLO NUMERICITA' CAMPI
293200080617     C*----------------------------------------------------*
293300080617     C     CHKNUM        BEGSR
293400080617     C*
293500080617     C                   IF        PiDecChr = *blanks
293600080617     C                   EVAL      PiDecChr = CharNUM
293700080617     C                   ENDIF
293800080617     C*
293900080617     C                   callp     UBISNUM_Check(PiStr
294000080617     C                                          :PiDecChr
294100080617     C                                          :PiVal
294200080617     C                                          :PiNum
294300080617     C                                          :PiInt)
294400080617     C*
294500080617     C                   ENDSR
294600080617     C***
294700010601
294800010601
294900010601
295000010330
295100040119      /TITLE Invio dei dati al punto operativo.
295200040119     C     invio         BEGSR
295300040119     C*
295400101005     C* 1° invio EDIVAT
295500040119     C                   reset                   dscmz
295600151120     C                   move      invfil        cmzdst
295700090911     C                   eval      cmzfld = 'EDIVATWR'
295800040119     C                   eval      cmzmbd = vlrhdl
295900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
296000040119     C***                if        prmfir = *blanks
296100090911     C                   eval      cmzfla = 'EDIVAT0F'
296200090911     C                   eval      cmzmba = 'EDIVAT0F'
296300040119     C***                else
296400040119     C***                eval      cmzfla = prmfir
296500040119     C***                eval      cmzmba = prmfir
296600040119     C***                endif
296700040119     C                   eval      cmznrr = *zeros
296800040119     C                   move      §ctrokvt      cmznrr
296900040119     C                   eval      cmzlba = vlrfl1
297000040119     C                   call(e)   'TIS711C'
297100040119     C                   parm                    dscmz
297200040119     C                   parm      *blanks       esito
297300040119     C                   if        %error
297400040119     C                             or cmzerr = '1'
297500040119     C                             or esito  = '1'
297600040119     C                   eval      wrkesito = '3'
297700040119     C                   else
297800040119     C*
297900101005     C* 2° invio EDIVAB
298000040119     C                   reset                   dscmz
298100151120     C                   move      invfil        cmzdst
298200040119     C                   eval      cmzfld = vlrfou
298300040119     C                   eval      cmzmbd = vlrhdl
298400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
298500040119     C***                if        prmfir = *blanks
298600090911     C                   eval      cmzfla = 'EDIVAB0F'
298700090911     C                   eval      cmzmba = 'EDIVAB0F'
298800040119     C***                else
298900040119     C***                eval      cmzfla = prmfir
299000040119     C***                eval      cmzmba = prmfir
299100040119     C***                endif
299200040119     C                   eval      cmznrr = *zeros
299300040119     C                   move      §ctrokvb      cmznrr
299400040119     C                   eval      cmzlba = vlrfl1
299500040119     C                   call(e)   'TIS711C'
299600040119     C                   parm                    dscmz
299700040119     C                   parm      *blanks       esito
299800040119     C                   if        %error
299900040119     C                             or cmzerr = '1'
300000040119     C                             or esito  = '1'
300100040119     C                   eval      wrkesito = '3'
300200040119     C                   endif
300300040119     C                   endif
300400040119     C*
300500040119     C                   ENDSR
300600040119     C***
300700010601
300800010601
300900010601
301000010601
301100010601      /TITLE Invio dei dati al punto operativo.
301200010601     C     opeini        BEGSR
301300010601     C*
301400010601     C* Inizializzo flag e contatori operativi
301500010601     C                   movel     '0'           flgGiro           1
301600010601     C                   movel     '0'           flgMulti          1
301700010601     C                   movel     '1'           flgStato          1
301800010615     C                   movel     '0'           flgOk             1
301900010601     C                   z-add     *zeros        cntNonEl         10 0
302000010601     C                   z-add     *zeros        invfil            3 0
302100010601     C*
302200010601     C                   ENDSR
302300010601     C***
302400070326
302500070326
302600070326
302700070326
302800080916     C     *pssr         BEGSR
302900070329     C*
303000080916     C                   if        %open(tivin00r)
303100080916     C                   close     tivin00r
303200080916     C                   endif
303300090911     C                   if        %open(edivabwr)
303400090911     C                   close     edivabwr
303500080916     C                   endif
303600090911     C                   if        %open(edivatwr)
303700090911     C                   close     edivatwr
303800080916     C                   endif
303900070326     C*
304000070326     C* Effettuo la chiamata al CLLE preposto
304100101005     C                   call(e)   'TITVEVTC'
304200080916     C                   parm                    parccm
304300080916     C                   parm                    parmbr
304400080916     C                   parm      '2'           paropz
304500070326     C*
304600080916     C                   eval      wrkesito = '2'
304700070404     C*
304800080916     C                   seton                                        LR
304900070326     C*
305000080916     C                   ENDSR     '*CANCL'
305100070326     C***
305200170213
305300170213
305400170213
305500170213     C*--------------------------------------------------------
305600170213     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
305700170213     C*--------------------------------------------------------
305800170213     C     CARTAB        BEGSR
305900170213     C*
306000170213     C* TABELLA '15' - NAZIONI
306100170213     C                   clear                   skNAZISO
306200170213     C                   clear                   skNAZBAR
306300170213     C                   eval      tblKUT = 1
306400170213     C                   eval      tblCOD = '15'
306500170213     C     KEYtabP       setll     tabel00f
306600170213     C     KEYtabP       reade     tabel00f
306700170213     C                   dow       not %eof(tabel00f)
306800170213     C                   if        tblFLG = *blanks
306900170213     C                   movel(p)  tblUNI        ds15
307000170213     C                   add       1             jNAZ
307100170213     C                   eval      skNAZBAR(jNAZ) = tblKEY
307200170213     C                   eval      skNAZISO(jNAZ) = §15CIE
307300170213     C                   endif
307400170213     C     KEYtabP       reade     tabel00f
307500170213     C                   enddo
307600170213     C*
307700170213     C                   ENDSR
307800170213     C***
307900070326
308000010330
308100010330
308200000613     C     *inzsr        BEGSR
308300990910     C*
308400990910     C     *entry        plist
308500990920     C                   parm                    tivlrds
308600990921     C                   parm      wrkesito      esito
308700000724     C                   parm                    prmlit
308800000710     C                   parm                    prmfir
308900010330     C*
309000010330     C* CALCOLA LA DATA CORRENTE
309100010330     C                   time                    wn14             14 0
309200090911     C                   movel     wn14          oracor            6 0          *ORA
309300110923     C                   z-add     *zeros        datcor            8 0
309400110923     C                   eval      datcor = %dec(%date() : *ISO)
309500170213     C*
309600170213     C* Chiave su TABEL00F - parziale
309700170213     C     KEYtabP       KLIST
309800170213     C                   KFLD                    tblKUT
309900170213     C                   KFLD                    tblCOD
310000000613     C*
310100000613     C                   ENDSR
310200000613     C***
