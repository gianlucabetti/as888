000100101005      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600090911     FEDIVABwr  O    E             DISK    usropn
000700090911     FEDIVATwr  O    E             DISK    usropn
000800000313     D*
000900040113     D*------------
001000110923     D prefix          S             10    inz
001100110923     D header          S                   like(vindta) inz
001200110923     D parcel          S                   like(vindta) inz
001300010330     D*----------------------------------------------------
001400010330     D* DICHIARAZIOINE VARIABILI DI WRK
001500010330     D*----------------------------------------------------
001600010330     D dscmz         e ds                  inz
001700010330     D psds           sds
001800010330     D  procname         *PROC
001900010330     D tivlrds       e ds                  extname(tivlr00f)
002000040128     D tisi95ds      e ds
002100010330     D esito           s              1
002200010330     D prmlit          s             10
002300010330     D prmfir          s             10
002400010330     D wrkesito        s                   like(esito)
002500010330     D wrkdata         s               d
002600010330     D wrkora          s               t
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800010330     D recko           s            150    INZ(*blanks)
002900011113     D depcmd          s            150    INZ(*blanks)
003000040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003100040510     D parccm          s              8    INZ(*blanks)
003200040119     D parmbr          s             10    INZ(*blanks)
003300040119     D paropz          s              1    INZ(*blanks)
003400040119     D chkcall         s              1    INZ(*blanks)
003500080125     D wFlgCAS         s              1    INZ(*blanks)
003600080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003700090911     D wCMR            s             35    INZ(*blanks)
003800030715     D*------------------
003900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004000030715     D*------------------
004100111118     D SkSplitFLDH     S             32    DIM(200)
004200111118     D SkSplitFLDP     S             32    DIM(200)
004300111118     D SkSplitCSV      S            256    DIM(200)
004400030715     D CharCSV         S              1
004500030715     D CharTXT         S              1
004600030715     D CharNUM         S              1
004700030715     D posDa           S              3  0 INZ(*zeros)
004800030715     D posA            S              3  0 INZ(*zeros)
004900030715     D i               s              3  0 INZ(1)
005000030715     D wGiro           s              1  0 INZ(*zeros)
005100080125     D wVATNOT_A       s                   LIKE(VATNOT)
005200080125     D wVATNOT_B       s                   LIKE(VATNOT)
005300141125     D wVATNOT_S       s                   LIKE(VATNOT)
005400141125     D wVATNOT_IJ      s             70
005500090904     D wVATNOT_E       s                   LIKE(VATNOT)
005600111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
005700111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
005800111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
005900111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700031201     D*------------------
006800031201     D* Costanti
006900031201     D*------------------
007000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007200080617     D*------------------
007300080617
007400080617     D*------------------
007500080617     D* LINKING A DEFINIZIONI ESTERNE
007600080617     D*------------------
007700080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007800090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007900010330
008000010330
008100010330
008200990915     C                   time                    wrkdata
008300990915     C                   time                    wrkora
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600081014     C                   reset                   wrkesito
008700010601     C*
008800010601     C                   exsr      opeini
008900010605     C                   exsr      rwvab
009000081014     C*
009100081014     C                   exsr      endela
009200010601     C*
009300010601     C                   seton                                        lr
009400010601
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010000010601     C*--------------------------------------------------------
010100010601     C     ENDELA        BEGSR
010200081014     C*
010300081014     C                   if        %open(tivin00r)
010400081014     C                   close     tivin00r
010500081014     C                   endif
010600090911     C                   if        %open(edivabwr)
010700090911     C                   close     edivabwr
010800081014     C                   endif
010900090911     C                   if        %open(edivatwr)
011000090911     C                   close     edivatwr
011100081014     C                   endif
011200040119     C*
011300040119     C* Effettuo la chiamata al CLLE preposto
011400101005     C                   call(e)   'TITVEVTC'
011500040510     C                   parm                    parccm
011600040119     C                   parm                    parmbr
011700040119     C                   parm      '2'           paropz
011800050201     C*
011900050201     C* Effettuo lancio TISI95 solo x chiusura
012000050201     C                   CLEAR                   TISI95DS
012100050201     C                   EVAL      I95TLA = 'C'
012200050201     C                   CALL      'TISI95R'
012300050201     C                   PARM                    TISI95DS
012400000616     C*
012500010601     C                   ENDSR
012600010601     C***
012700000613
012800010601
012900010601
013000010330     C*--------------------------------------------------------
013100090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013200010330     C*--------------------------------------------------------
013300010605     C     RWVAB         BEGSR
013400010330     C*
013500010330     C                   if        not %open(tivin00r)
013600010330     C                   open      tivin00r
013700010330     C                   endif
013800090911     C                   if        not %open(edivabwr)
013900090911     C                   open      edivabwr
014000010330     C                   endif
014100040119     C*
014200101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014300101005     C                   exsr      prevat
014400040119     C*
014500040119     C                   if        chkcall = '0'
014600040119     C*
014700090911     C                   if        not %open(edivatwr)
014800090911     C                   open      edivatwr
014900040119     C                   endif
015000090623     C*
015100040119     C                   clear                   §CTROKVB          7 0
015200040119     C                   clear                   §CTROKVT          7 0
015300040119     C                   clear                   §CTRMO            7 0
015400040119     C                   clear                   §CTRNO            7 0
015500090623     C*
015600090623     C* Inizializzazioni
015700090623     C                   exsr      inzvar
015800090623     C                   z-add     *zeros        wNewBolla         7 0
015900010330     C*
016000030822     C                   do        *HIVAL
016100010330     C*
016200030822     C                   read      tivin00r                               70
016300010618     C*
016400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016500010618     C                   if        vinflg = '1'
016600010618     C                   eval      flgOk = '1'
016700010618     C                   endif
016800040510     C*
016900040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017000040510     C                   z-add     *zeros        wLenVINDTA        4 0
017100040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017200040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017300040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017400040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017500040510     C                   z-add     1             wFlgVINDTA
017600040510     C                   leave
017700040510     C                   endif
017800040510     C                   enddo
017900110923     C*
018000110923     C* Ignoro i record che iniziano x '#'
018100110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
018200110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
018300110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
018400111118     C                   z-add     0             wFlgVINDTA
018500111118     C                   else
018600110923     C*
018700110923     C* Setto i column headers "variabili"
018800111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
018900110923     C                   eval      header = %trim(%subst(vindta:13))
019000110923     C                   endif
019100111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
019200110923     C                   eval      parcel = %trim(%subst(vindta:13))
019300110923     C                   endif
019400111118     C                   endif
019500090420     C*
019600090420     C                   add       1             rrnum
019700010618     C*
019800040510     C                   if        vindta > *blanks AND
019900090420     C                             wFlgVINDTA = 1
020000010330     C*
020100010601     C                   if        *in70 = *off and
020200010330     C                             (vinflg = *blanks
020300010330     C                              or vinflg = '0'
020400010330     C                              or vinflg = '2')
020500010330     C*
020600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020700010711     C                   if        vinflg = *blanks or vinflg = '0'
020800010711     C                   clear                   vinmsg
020900010711     C                   endif
021000010601     C*
021100090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
021200111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
021300111117     C***                eval      vindta  = '*' + %trim(vindta)
021400111117     C***                endif
021500090914     C*
021600090914     C* Elaborazione record corrente
021700090914     C                   exsr      elabCurrRec
021800151120     C*
021900151120     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022000151120     C                   if        vlrpoi = 999
022100151120     C                   else
022200151120     C                   move(P)   vlrpoi        VABFGS
022300151120     C                   endif
022400151120     C                   eval      invfil = VABFGS
022500010604     C*
022600010604     C                   if        *in31 = *off and
022700010604     C                             *in32 = *off
022800010604     C                   eval      vinflg = '1'
022900010604     C                   else
023000010604     C                   eval      recko = vindta
023100010604     C                   eval      vinflg = '2'
023200010604     C                   endif
023300010601     C*
023400010604     C                   endif
023500010604     C*
023600010330     C                   else
023700010330     C                   eval      vinflg = '1'
023800010330     C                   endif
023900010601     C*
024000010601     C  N70              update    tivin000
024100010330     C*
024200030822     C  N70              enddo
024300090623     C*
024400090623     C* Scarico la testata rimasta "in canna"
024500140415     C                   if        not *in33                                    * esludo estero
024600090623     C  N31              exsr      wriVAB
024700140415     C                   endif
024800040119     C*
024900040119     C                   endif                                                  (endif - chkcall)
025000010601     C*
025100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025200010601     C                   if        cntNonEl = *zeros or
025300010601     C                             flgMulti = '0'
025400010330     C* Se non ci sono record con errori ...
025500010601     C                   if        §ctrno = 0 and
025600010604     C                             §ctrmo = 0 and
025700010601     C                             flgStato <> '2'
025800010330     C* ... restituisco esito OK.
025900010330     C                   eval      wrkesito = '0'
026000010330     C                   else
026100040119     C                   if        §ctrokvb > 0
026200010330     C                   eval      wrkesito = '1'
026300010330     C                   else
026400010615     C                   if        flgOk = '0'
026500010615     C                   eval      wrkesito = '2'
026600010615     C                   else
026700010615     C                   eval      wrkesito = '6'
026800010615     C                   endif
026900010330     C                   endif
027000010330     C                   endif
027100010601     C                   else
027200010601     C                   eval      wrkesito = '9'
027300010601     C                   endif
027400010330     C*
027500010330     C                   if        %open(tivin00r)
027600010330     C                   close     tivin00r
027700010330     C                   endif
027800090911     C                   if        %open(edivabwr)
027900090911     C                   close     edivabwr
028000010330     C                   endif
028100090911     C                   if        %open(edivatwr)
028200090911     C                   close     edivatwr
028300040512     C                   endif
028400010601     C*
028500010601     C                   if        vlrpoi <> 999
028600010601     C                   eval      invfil = vlrpoi
028700010601     C                   endif
028800010330     C*
028900040119     C                   if        §ctrokvb > 0
029000010601     C                             and invfil > *zeros
029100010330     C                   exsr      invio
029200010330     C                   endif
029300010330     C*
029400010330     C                   ENDSR
029500010330     C***
029600090623
029700090623
029800090623
029900090623     C*----------------------------------------------------*
030000090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
030100090623     C*----------------------------------------------------*
030200090623     C     INZWRK        BEGSR
030300090623     C*
030400090623     C                   eval      i = 1
030500090623     C                   eval      posDa = *zeros
030600090623     C                   eval      posA  = *zeros
030700090623     C*
030800090623     C                   ENDSR
030900090623
031000090623
031100090623
031200090623     C*----------------------------------------------------*
031300090623     C*  ELABORAZIONE RECORD CORRENTE
031400090623     C*----------------------------------------------------*
031500090623     C     ELABCURRREC   BEGSR
031600090623     C*
031700090623     C                   select
031800090623     C*
031900110923     C* Tipo record "TESTATA" - HEADER
032000111118     C                   when      %subst(vindta:1:6) = 'HEADER'
032100090623     C                   if        wNewBolla > *zeros
032200140415     C                   if        not *in33                                    * esludo estero
032300090623     C  N31              exsr      wriVAB
032400140415     C                   endif
032500090623     C                   exsr      inzvar
032600090623     C                   endif
032700090624     C                   eval      wNewBolla = 1
032800090623     C                   eval      wGiro = *zeros
032900111118     C                   clear                   SkSplitFLDH
033000110923     C                   eval      prefix = 'HDR_'
033100090624     C                   exsr      inzwrk
033200090623     C                   exsr      impvab
033300090623     C                   exsr      inzwrk
033400090623     C                   exsr      impvab
033500090623     C*
033600110923     C* Tipo record "DETTAGLIO" - PARCEL
033700110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
033800090623     C                   eval      wGiro = *zeros
033900111118     C                   clear                   SkSplitFLDP
034000110923     C                   eval      prefix = 'PCL_'
034100090623     C                   exsr      inzwrk
034200101005     C                   exsr      impvat
034300090623     C                   exsr      inzwrk
034400101005     C                   exsr      impvat
034500090624     C*
034600150203     C                   if        not *in33                                    * esludo estero
034700090624     C  N31              exsr      wrivat
034800150203     C                   endif
034900090623     C*
035000090623     C                   endsl
035100090623     C*
035200090623     C                   ENDSR
035300040119
035400040119
035500040119
035600040119     C*----------------------------------------------------*
035700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035800040119     C*----------------------------------------------------*
035900040510     C     WRIVAB        BEGSR
036000090623     C*
036100090623     C  N31              add       1             §CTROKVB
036200090623     C   32              add       1             §CTRMO
036300090623     C   31              add       1             §CTRNO
036400040119     C*
036500060331     C* Gestisco l'eventuale rottura x numero spedizione
036600070823     C                   if        VABNSP <> depspe
036700070301     C                   if        wVATNOT_E <> *blanks
036800080125     C                   if        VABCTM = *blanks
036900080125     C                   eval      VABCTM = '7Q'
037000080125     C                   endif
037100070301     C                   endif
037200090911     C*
037300090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037400090911     C                   eval      VABCMR = wCMR
037500090911     C                   eval      VABDCM = DATCOR
037600090911     C                   eval      VABDTS = DATCOR
037700090911     C                   eval      VABHMS = ORACOR
037800090911     C                   eval      VABCNT = 1
037900090911     C****
038000090911     C                   write     edivab00                                     => scarico il VAB
038100060331     C                   endif
038200090623     C*
038300090623     C  N31              eval      depspe = vabNSP
038400040119     C*
038500040119     C                   ENDSR
038600040119
038700040119
038800040119
038900040119     C*----------------------------------------------------*
039000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039100040119     C*----------------------------------------------------*
039200040510     C     WRIVAT        BEGSR
039300060512     C*
039400060512     C* Inizializzo i campi chiave
039500151120     C                   eval      VATFGS = VABFGS
039600060512     C                   eval      VATAAS = VABAAS
039700060512     C                   eval      VATCCM = VABCCM
039800070823     C                   eval      VATNRS = VABNRS
039900060512     C                   eval      VATNSP = VABNSP
040000060512     C                   eval      VATLNP = VABLNP
040100111117     C                   eval      VATCMR = wCMR
040200090914     C                   eval      VATCNT = 1
040300060331     C*
040400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040500060331     C                   if        wVATNOT_A <> *blanks
040600060331     C                   eval      VATNOT = wVATNOT_A
040700060331     C                   eval      VATTRC = 'A'
040800090911     C                   write     EDIVAT00
040900060331     C                   add       1             §CTROKVT
041000060331     C                   endif
041100060331     C*
041200060331     C                   if        wVATNOT_B <> *blanks
041300060331     C                   eval      VATNOT = wVATNOT_B
041400060331     C                   eval      VATTRC = 'B'
041500090911     C                   write     EDIVAT00
041600060331     C                   add       1             §CTROKVT
041700060331     C                   endif
041800111118     C*
041900111118     C                   if        wVATNOT_E <> *blanks
042000111118     C                   eval      VATNOT = wVATNOT_E
042100111118     C                   eval      VATTRC = 'E'
042200111118     C                   write     EDIVAT00
042300111118     C                   add       1             §CTROKVT
042400111118     C                   endif
042500141125     C*
042600141126 xxx C***                if        wVATNOT_S <> *blanks
042700141126     C***                eval      VATNOT = wVATNOT_S
042800141126     C***                eval      VATTRC = 'S'
042900141126     C***                write     EDIVAT00
043000141126     C***                add       1             §CTROKVT
043100141126     C***                endif
043200141125     C*
043300141125     C                   if        wVATNOT_IJ <> *blanks
043400141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043500141125     C                   eval      VATTRC = 'I'
043600141125     C                   write     EDIVAT00
043700141125     C                   add       1             §CTROKVT
043800141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
043900141125     C                   eval      VATTRC = 'J'
044000141125     C                   write     EDIVAT00
044100141125     C                   add       1             §CTROKVT
044200141125     C                   endif
044300090623     C*
044400111118 xxx C* Considero il VAT solo a rottura del "chi sono"
044500111118     C***                if        wVATNOT_E <> *blanks AND
044600111118     C***                          wVATNOT_E <> sVATNOT_E
044700111118     C***                eval      VATNOT = wVATNOT_E
044800111118     C***                eval      VATTRC = 'E'
044900111118     C***                write     EDIVAT00
045000111118     C***                add       1             §CTROKVT
045100111118     C***                eval      sVATNOT_E = wVATNOT_E
045200111118     C***                add       1             VABNCL
045300111118     C***                endif
045400040119     C*
045500040119     C                   ENDSR
045600040119
045700040119
045800040119
045900040119     C*----------------------------------------------------*
046000101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046100040119     C*----------------------------------------------------*
046200101005     C     PREVAT        BEGSR
046300040119     C*
046400101005     C* Compongo il nome del membro da dare al EDIVATWR
046500040119     C                   eval      parmbr = vlrhdl
046600040119     C                   movel     'M'           parmbr
046700040510     C                   eval      parccm = vlrksc
046800040119     C                   eval      paropz = '1'
046900040119     C* Effettuo la chiamata al CLLE preposto
047000101005     C                   call(e)   'TITVEVTC'
047100040119     C                   parm                    parccm
047200040119     C                   parm                    parmbr
047300040119     C                   parm                    paropz
047400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047500040119     C                   if        %error
047600040119     C                   movel     '1'           chkcall
047700040119     C                   else
047800040119     C                   movel     '0'           chkcall
047900040119     C                   endif
048000040119     C*
048100040119     C                   ENDSR
048200010601
048300010601
048400010601
048500010330     C*----------------------------------------------------*
048600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048700010330     C*----------------------------------------------------*
048800040119     C     INZVAR        BEGSR
048900010330     C*
049000030822     C* Inizializzo variabili di wrk
049100140415     C                   SETOFF                                       313233
049200090916     C                   Z-ADD     *zeros        Num5_0            5 0
049300090916     C                   MOVEL     *blanks       wNOTE            70
049400060331     C                   MOVEL     *blanks       wVATNOT_A
049500060331     C                   MOVEL     *blanks       wVATNOT_B
049600141125     C                   MOVEL     *blanks       wVATNOT_S
049700141125     C                   MOVEL     *blanks       wVATNOT_IJ
049800090904     C                   MOVEL     *blanks       wVATNOT_E
049900111117     C                   MOVEL     *blanks       wVATNOT_E_1
050000111117     C                   MOVEL     *blanks       wVATNOT_E_2
050100111118     C                   MOVEL     *blanks       wVATNOT_E_3
050200111118     C                   MOVEL     *blanks       wVATNOT_E_4
050300090623     C*
050400111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
050500090220     C*
050600080617     C                   EVAL      wFlgCAS = *blanks
050700030822     C*
050800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050900090911     C                   CLEAR                   EDIVAB00
051000090911     C                   CLEAR                   EDIVAT00
051100030822     C                   CLEAR                   SkSplitCSV
051200090623     C                   EXSR      inzwrk
051300030822     C*
051400030822     C* Reimposto i valori di default
051500030822     C                   EXSR      DEFCAM
051600030822     C*
051700010330     C                   ENDSR
051800010330     C*----------------------------------------------------*
051900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052000010330     C*----------------------------------------------------*
052100010330     C     DEFCAM        BEGSR
052200030822     C*
052300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052400030715     C* e delimitatore testo.
052500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052800030822     C*
052900150203     C* Se richiesta esclusione Estero
053000150203     C                   SETOFF                                       50
053100150203     C                   IF        %subst(vlrppt:1:1) = 'E'
053200150203     C                   SETON                                        50
053300030715     C                   ENDIF
053400090421     C*
053500090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
053600090421     C                   EVAL      VABCBO = '1'
053700090421     C                   EVAL      VABLNP = vlrpoi
053800090421     C                   EVAL      VABNSP = rrnum
053900030822     C*
054000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054100030822     C                   EVAL      posDaDft = 1
054200030822     C                   EVAL      posADft  = 0
054300030822     C                   EVAL      wGiroDft = 0
054400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054500030822     C                             posDaDft > 0
054600030822     C*
054700030822     C* Gestisco il 1° giro
054800030822     C                   IF        wGiroDft = 0
054900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055100030822     C* Incremento il contatore dei "giri"
055200030822     C                   EVAL      wGiroDft = 1
055300030822     C                   ELSE
055400030822     C                   EVAL      posDaDft = posADft
055500030822     C                   ENDIF
055600030822     C* Eseguo lo scan x trovare la fine del campo corrente
055700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055800030822     C*
055900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056100030822     C                   IF        posDaDft > 0 AND
056200030822     C                             posADft  > 0
056300030822     C* NCL
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NCL'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNCL
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CCM
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CCM'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCCM
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* LNP
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'LNP'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABLNP
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* NRS
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NRS'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNRS
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CTR
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CTR'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCTR
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* PKB
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'PKB'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800030822     C                   Z-ADD     PiVal         VABPKB
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100140715     C* PKN
064200140715     C                   IF        %subst(
064300140715     C                             %subst(vlrppt:posDaDft+1:
064400140715     C                             posADft-posDaDft-1):1:3)
064500140715     C                             = 'PKN'
064600140715     C                   EVAL      PiStr=%trim(%subst(
064700140715     C                             %subst(vlrppt:posDaDft+1:
064800140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900140715     C                   EXSR      CHKNUM
065000140715     C                   IF        PiNum=*on
065100140715     C                   Z-ADD     PiVal         wVABPKN           7 1
065200140715     C                   ENDIF
065300140715     C                   ENDIF
065400030822     C* VLB
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'VLB'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABVLB
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* QFT
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'QFT'
067200030822     C                   EVAL      PiStr=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   EXSR      CHKNUM
067600030822     C                   IF        PiNum=*on
067700030822     C                   Z-ADD     PiVal         VABQFT
067800030822     C                   ENDIF
067900030822     C                   ENDIF
068000030822     C* CBO
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'CBO'
068500030822     C                   EVAL      VABCBO=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* TSP
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'TSP'
069400030822     C                   EVAL      VABTSP=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* VAS
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'VAS'
070300030822     C                   EVAL      VABVAS=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VCA
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VCA'
071200030822     C                   EVAL      VABVCA=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600070925     C* TIC
071700070925     C                   IF        %subst(
071800070925     C                             %subst(vlrppt:posDaDft+1:
071900070925     C                             posADft-posDaDft-1):1:3)
072000070925     C                             = 'TIC'
072100070925     C                   EVAL      VABTIC=%trim(%subst(
072200070925     C                             %subst(vlrppt:posDaDft+1:
072300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400070925     C                   ENDIF
072500030822     C* GCA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'GCA'
073000030822     C                   EVAL      VABGCA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* CTM
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'CTM'
073900030822     C                   EVAL      VABCTM=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* FFD
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'FFD'
074800030822     C                   EVAL      VABFFD=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* VAD
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'VAD'
075700030822     C                   EVAL      VABVAD=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GMA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GMA'
076600030822     C                   EVAL      VABGMA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GGA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GGA'
077500030822     C                   EVAL      VABGGA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* GVA
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'GVA'
078400030822     C                   EVAL      VABGVA=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* TC1
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'TC1'
079300030822     C                   EVAL      VABTC1=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700030822     C* TC2
079800030822     C                   IF        %subst(
079900030822     C                             %subst(vlrppt:posDaDft+1:
080000030822     C                             posADft-posDaDft-1):1:3)
080100030822     C                             = 'TC2'
080200030822     C                   EVAL      VABTC2=%trim(%subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500030822     C                   ENDIF
080600040714     C* VATTRC
080700040714     C                   IF        %subst(
080800040714     C                             %subst(vlrppt:posDaDft+1:
080900040714     C                             posADft-posDaDft-1):1:3)
081000040714     C                             = 'TRC'
081100040714     C                   EVAL      VATTRC=%trim(%subst(
081200040714     C                             %subst(vlrppt:posDaDft+1:
081300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081400040714     C                   ENDIF
081500030822     C* ...
081600030822     C                   ENDIF
081700030822     C                   ENDDO
081800020204     C*
081900010330     C                   ENDSR
082000010607     C*----------------------------------------------------*
082100090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
082200010607     C*----------------------------------------------------*
082300090623     C     IMPVAB        BEGSR
082400030515     C*
082500010607     C                   MOVEL     datcor        VABAAS
082600010607     C                   MOVE      datcor        VABMGS
082700010607     C*
082800030715     C**********
082900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083000030715     C**********
083100030715     C                   IF        wGiro = *zeros
083200080716     C*
083300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
083400080716     C                   EVAL      depvinDTA = vinDTA
083500040113     C*
083600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083700110923     C                   IF        header <> *blanks
083800110923     C                   EVAL      vindta = header
083900070823     C     ';':CharCSV   XLATE     vindta        vindta
084000040510     C                   ENDIF
084100080626     C*
084200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084300031201     C     minu:maiu     XLATE     vindta        vindta
084400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084500030715     C                   DOW       posDa <= %len(%trim(vindta))
084600030715     C*
084700030715     C* Gestisco il 1° campo
084800030715     C                   IF        i = 1
084900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085000030715     C                   EVAL      posDa = 1
085100030715     C* Eseguo lo scan x trovare la fine del primo campo
085200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085300030715     C* A questo punto "estraggo" il campo corrente
085400111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
085500110923     C                                         %subst(vindta:posDa:(posA-posDa))
085600030715     C* X i campi successivi al 1°
085700030715     C                   ELSE
085800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085900030715     C                   EVAL      posDa = posA + 1
086000030715     C* Eseguo lo scan x trovare la fine del campo corrente
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* Gestisco l'ultimo campo
086300030715     C                   IF        posA = *zeros
086400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086500030715     C* A questo punto "estraggo" il campo corrente
086600111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
086700110923     C                                         %subst(vindta:posDa:(posA-posDa))
086800030715     C                   LEAVE
086900030715     C                   ELSE
087000030715     C* A questo punto "estraggo" il campo corrente
087100111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
087200110923     C                                         %subst(vindta:posDa:(posA-posDa))
087300030715     C                   ENDIF
087400030715     C                   ENDIF
087500030715     C* Incremento il contatore d campo
087600030715     C                   EVAL      i = i +1
087700030715     C                   ENDDO
087800030715     C*
087900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088000030715     C* delimitazione testo
088100030715     C                   EVAL      i = 1
088200111118     C                   DOW       i < %elem(SkSplitFLDH)
088300111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
088400111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
088500030715     C                   EVAL      i = i + 1
088600030715     C                   ENDDO
088700030715     C                   EVAL      wGiro = 1
088800080716     C* Ripristino la 1° riga originale
088900080716     C                   EVAL      vinDTA = depvinDTA
089000030715     C                   ELSE
089100030715     C                   EVAL      wGiro = 2
089200031204     C**********
089300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
089400031204     C* d separatore campo
089500031204     C**********
089600031204     C                   EVAL      vindta = %trim(vindta)
089700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089800031204     C                   EVAL      vindta = %subst(vindta:2)
089900031204     C                   ENDDO
090000031204     C*
090100031204     C                   Z-ADD     *zeros        lunghInput        4 0
090200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
090400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
090500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090600031204     C                   ENDDO
090700030715     C**********
090800030715     C* Effettuo lo split del campo dati d input
090900030715     C**********
091000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091100030715     C                   DOW       posDa <= %len(%trim(vindta))
091200030715     C*
091300030715     C* Gestisco il 1° campo
091400030715     C                   IF        i = 1
091500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091600030715     C                   EVAL      posDa = 1
091700030715     C* Eseguo lo scan x trovare la fine del primo campo
091800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C* X i campi successivi al 1°
092300030715     C                   ELSE
092400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
092500030715     C                   EVAL      posDa = posA + 1
092600031204     C* Verifico che nn vi sia il campo nn valorizzato
092700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092800031204     C* Se campo nn valorizzato skippo al prossimo
092900031204     C                   EVAL      SkSplitCSV(i) = *blanks
093000031204     C                   EVAL      posA  = posA + 1
093100031204     C                   ELSE
093200030715     C* Eseguo lo scan x trovare la fine del campo corrente
093300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093400030715     C* Gestisco l'ultimo campo
093500030715     C                   IF        posA = *zeros
093600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093700030715     C* A questo punto "estraggo" il campo corrente
093800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093900030715     C                                                    (posA-posDa))
094000030715     C                   LEAVE
094100030715     C                   ELSE
094200030715     C* A questo punto "estraggo" il campo corrente
094300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094400030715     C                                                    (posA-posDa))
094500030715     C                   ENDIF
094600031204     C                   ENDIF
094700030715     C                   ENDIF
094800030715     C* Incremento il contatore d campo
094900030715     C                   EVAL      i = i +1
095000030715     C                   ENDDO
095100030715     C*
095200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095300030715     C* delimitazione testo
095400030715     C                   EVAL      i = 1
095500030715     C                   DOW       i < %elem(SkSplitCSV)
095600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095800030715     C                   EVAL      i = i + 1
095900030715     C                   ENDDO
096000030715     C*
096100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096200030715     C                   EVAL      i = 1
096300111118     C                   DOW       i < %elem(SkSplitFLDH)
096400030715     C*
096500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096600111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
096700090623     C                             SkSplitCSV(i) <> *blanks
096800090623     C*
096900111118     C                   exsr      CARVALH
097000040510     C*
097100030715     C                   ENDIF
097200030715     C                   EVAL      i = i + 1
097300030715     C                   ENDDO
097400070925     C*
097500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
097600080125     C*
097700111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
097800111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
097900140715     C*
098000140715     C                   IF        VABPKB = *zeros
098100140715     C                   EVAL      VABPKB = wVABPKN * VABNCL
098200140715     C                   ENDIF
098300070925     C*
098400071029     C***  <===  -----------------------------------------
098500070925     C*
098600070925     C* Considerazioni finali su CBO/CAS
098700080125     C                   IF        wFlgCAS = '1'
098800070925     C                   IF        VABCBO = '1'
098900070925     C                   EVAL      VABCBO = '4'
099000070925     C                   ENDIF
099100070925     C                   IF        VABCBO = '2'
099200070925     C                   EVAL      VABCBO = '6'
099300070925     C                   ENDIF
099400070925     C                   ENDIF
099500070925     C***  <===  ----------------------------
099600040510     C*
099700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
099800040510     C                   IF        VABNSP = *zeros
099900040510     C                   SETON                                        31
100000040510     C                   ENDIF
100100040715     C*
100200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
100300040715     C                   IF        VABLNP = *zeros
100400040715     C                   SETON                                        31
100500040715     C                   ENDIF
100600040510     C*
100700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
100800040510     C                   IF        VABRMN = *zeros
100900040510     C                   Z-ADD     VABNSP        VABRMN
101000040510     C                   ENDIF
101100040510     C*
101200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
101300040510     C                   IF        VABRMA = *blanks
101400040714     C                   MOVEL     VABRMN        VABRMA
101500040510     C                   ENDIF
101600040128     C*
101700040510     C* Se provincia nn valorizzata la reperisco
101800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
101900040510     C                   IF        VABPRD = *blanks
102000040128     C                   CLEAR                   TISI95DS
102100040128     C                   EVAL      I95TCN = '3'
102200040128     C                   Z-ADD     datcor        I95DAT
102300040128     C                   EVAL      I95NAR = VABNZD
102400040128     C                   EVAL      I95CAP = VABCAD
102500040128     C                   EVAL      I95LOC = VABLOD
102600040128     C                   CALL      'TISI95R'
102700040128     C                   PARM                    TISI95DS
102800040128     C                   EVAL      VABPRD = O95PRV
102900040510     C                   ENDIF
103000020204     C*
103100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
103200020204     C                   EXSR      CHKIMPDIV
103300030715     C                   ENDIF
103400020204     C*
103500010607     C                   ENDSR
103600010607     C*----------------------------------------------------*
103700090623
103800090623
103900090623
104000090623     C*----------------------------------------------------*
104100101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
104200090623     C*----------------------------------------------------*
104300101005     C     IMPVAT        BEGSR
104400090623     C*
104500090623     C**********
104600090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
104700090623     C**********
104800090623     C                   IF        wGiro = *zeros
104900090623     C*
105000090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
105100090623     C                   EVAL      depvinDTA = vinDTA
105200090623     C*
105300090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
105400110923     C                   IF        parcel <> *blanks
105500110923     C                   EVAL      vindta = parcel
105600090623     C     ';':CharCSV   XLATE     vindta        vindta
105700090623     C                   ENDIF
105800090623     C*
105900090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
106000090623     C     minu:maiu     XLATE     vindta        vindta
106100090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
106200090623     C                   DOW       posDa <= %len(%trim(vindta))
106300090623     C*
106400090623     C* Gestisco il 1° campo
106500090623     C                   IF        i = 1
106600090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
106700090623     C                   EVAL      posDa = 1
106800090623     C* Eseguo lo scan x trovare la fine del primo campo
106900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
107000090623     C* A questo punto "estraggo" il campo corrente
107100111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
107200110923     C                                         %subst(vindta:posDa:(posA-posDa))
107300090623     C* X i campi successivi al 1°
107400090623     C                   ELSE
107500090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
107600090623     C                   EVAL      posDa = posA + 1
107700090623     C* Eseguo lo scan x trovare la fine del campo corrente
107800090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
107900090623     C* Gestisco l'ultimo campo
108000090623     C                   IF        posA = *zeros
108100090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
108200090623     C* A questo punto "estraggo" il campo corrente
108300111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
108400110923     C                                         %subst(vindta:posDa:(posA-posDa))
108500090623     C                   LEAVE
108600090623     C                   ELSE
108700090623     C* A questo punto "estraggo" il campo corrente
108800111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
108900110923     C                                         %subst(vindta:posDa:(posA-posDa))
109000090623     C                   ENDIF
109100090623     C                   ENDIF
109200090623     C* Incremento il contatore d campo
109300090623     C                   EVAL      i = i +1
109400090623     C                   ENDDO
109500090623     C*
109600090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
109700090623     C* delimitazione testo
109800090623     C                   EVAL      i = 1
109900111118     C                   DOW       i < %elem(SkSplitFLDP)
110000111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
110100111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
110200090623     C                   EVAL      i = i + 1
110300090623     C                   ENDDO
110400090623     C                   EVAL      wGiro = 1
110500090623     C* Ripristino la 1° riga originale
110600090623     C                   EVAL      vinDTA = depvinDTA
110700090623     C                   ELSE
110800090623     C                   EVAL      wGiro = 2
110900090623     C**********
111000090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
111100090623     C* d separatore campo
111200090623     C**********
111300090623     C                   EVAL      vindta = %trim(vindta)
111400090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
111500090623     C                   EVAL      vindta = %subst(vindta:2)
111600090623     C                   ENDDO
111700090623     C*
111800090623     C                   Z-ADD     *zeros        lunghInput        4 0
111900090623     C                   EVAL      lunghInput = %len(%trim(vindta))
112000090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
112100090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
112200090623     C                   EVAL      lunghInput = %len(%trim(vindta))
112300090623     C                   ENDDO
112400090623     C**********
112500090623     C* Effettuo lo split del campo dati d input
112600090623     C**********
112700090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
112800090623     C                   DOW       posDa <= %len(%trim(vindta))
112900090623     C*
113000090623     C* Gestisco il 1° campo
113100090623     C                   IF        i = 1
113200090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
113300090623     C                   EVAL      posDa = 1
113400090623     C* Eseguo lo scan x trovare la fine del primo campo
113500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113600090623     C* A questo punto "estraggo" il campo corrente
113700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
113800090623     C                                                    (posA-posDa))
113900090623     C* X i campi successivi al 1°
114000090623     C                   ELSE
114100090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
114200090623     C                   EVAL      posDa = posA + 1
114300090623     C* Verifico che nn vi sia il campo nn valorizzato
114400090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
114500090623     C* Se campo nn valorizzato skippo al prossimo
114600090623     C                   EVAL      SkSplitCSV(i) = *blanks
114700090623     C                   EVAL      posA  = posA + 1
114800090623     C                   ELSE
114900090623     C* Eseguo lo scan x trovare la fine del campo corrente
115000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
115100090623     C* Gestisco l'ultimo campo
115200090623     C                   IF        posA = *zeros
115300090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
115400090623     C* A questo punto "estraggo" il campo corrente
115500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
115600090623     C                                                    (posA-posDa))
115700090623     C                   LEAVE
115800090623     C                   ELSE
115900090623     C* A questo punto "estraggo" il campo corrente
116000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116100090623     C                                                    (posA-posDa))
116200090623     C                   ENDIF
116300090623     C                   ENDIF
116400090623     C                   ENDIF
116500090623     C* Incremento il contatore d campo
116600090623     C                   EVAL      i = i +1
116700090623     C                   ENDDO
116800090623     C*
116900090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
117000090623     C* delimitazione testo
117100090623     C                   EVAL      i = 1
117200090623     C                   DOW       i < %elem(SkSplitCSV)
117300090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
117400090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
117500090623     C                   EVAL      i = i + 1
117600090623     C                   ENDDO
117700090623     C*
117800090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
117900090623     C                   EVAL      i = 1
118000111118     C                   DOW       i < %elem(SkSplitFLDP)
118100090623     C*
118200090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
118300111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
118400090623     C                             SkSplitCSV(i) <> *blanks
118500090623     C*
118600111118     C                   exsr      CARVALP
118700090623     C*
118800090623     C                   ENDIF
118900090623     C                   EVAL      i = i + 1
119000090623     C                   ENDDO
119100090623     C*
119200090623     C***  ===>  Gestione campi "particolari" fuori ciclo
119300111118     C*
119400111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
119500111118     C                                         %trim(wVATNOT_E_2) +
119600111118     C                                         %trim(wVATNOT_E_3) +
119700111118     C                                         %trim(wVATNOT_E_4)
119800090623     C*
119900090623     C***  <===  -----------------------------------------
120000090623     C*
120100090623     C                   ENDIF
120200090623     C*
120300090623     C                   ENDSR
120400090623     C*----------------------------------------------------*
120500090623
120600090623
120700090623     C*----------------------------------------------------*
120800090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
120900090623     C*----------------------------------------------------*
121000111118     C     CARVALH       BEGSR
121100090623     C*
121200090623     C*** ATB
121300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
121400090623     C                   EVAL      VABATB = SkSplitCSV(i)
121500090623     C                   ENDIF
121600090623     C*** RSD
121700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
121800090623     C                   EVAL      VABRSD = SkSplitCSV(i)
121900090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
122000090623     C     '@':'A'       XLATE     VABRSD        VABRSD
122100090623     C* ==
122200090623     C                   ENDIF
122300090623     C*** RD2
122400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
122500090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
122600090623     C                   ENDIF
122700130530     C*** IND 1 (HDR)
122800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
122900090623     C                   EVAL      VABIND = SkSplitCSV(i)
123000090623     C                   ENDIF
123100130530     C*** IND 2 (HDR)
123200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
123300111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
123400111117     C                   ENDIF
123500130530     C*** IND 1
123600130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
123700130530     C                   EVAL      VABIND = SkSplitCSV(i)
123800130530     C                   ENDIF
123900130530     C*** IND 2
124000130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
124100130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
124200130530     C                   ENDIF
124300111117     C*** CAD
124400111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
124500111117     C                   EVAL      VABCAD = SkSplitCSV(i)
124600111216     C                   MOVEL     *blanks       wAlfa7            7
124700111216     C                   MOVEL     *zeros        wAlfa7z           7
124800111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
124900111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
125000111117     C                   ENDIF
125100090623     C*** LOD
125200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
125300090623     C                   EVAL      VABLOD = SkSplitCSV(i)
125400090623     C                   ENDIF
125500111117     C*** PRD
125600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
125700090623     C                   EVAL      VABPRD = SkSplitCSV(i)
125800090623     C                   ENDIF
125900090623     C*** NZD
126000140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
126100140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
126200150203     C   50              SETON                                        33
126300090623     C                   ENDIF
126400140415     C                   ENDIF
126500090623     C*** RMA
126600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
126700130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
126800130611     C                   EVAL      VABRMA = SkSplitCSV(i)
126900090623     C                   ENDIF
127000111117     C*** NOT
127100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
127200111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
127300090623     C                   ENDIF
127400111117     C*** NT2
127500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
127600111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
127700090623     C                   ENDIF
127800090623     C*** VCA
127900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
128000090623     C                   EVAL      VABVCA = SkSplitCSV(i)
128100090623     C                   ENDIF
128200090623     C*** VAS
128300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
128400090623     C                   EVAL      VABVAS = SkSplitCSV(i)
128500090623     C                   ENDIF
128600090623     C*** FFD
128700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
128800090623     C                   EVAL      VABFFD = SkSplitCSV(i)
128900090623     C                   ENDIF
129000090623     C*** GC1
129100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
129200090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
129300090623     C                   ENDIF
129400090623     C*** GC2
129500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
129600090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
129700090623     C                   ENDIF
129800090623     C*** TSP
129900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
130000090623     C                   EVAL      VABTSP = SkSplitCSV(i)
130100090623     C                   ENDIF
130200090623     C*** CBO
130300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
130400090623     C                   EVAL      VABCBO = SkSplitCSV(i)
130500090623     C                   ENDIF
130600090623     C*** NAS
130700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
130800090623     C                   EVAL      VABNAS = SkSplitCSV(i)
130900090623     C                   ENDIF
131000090623     C*** TIC
131100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
131200090623     C                   EVAL      VABTIC = SkSplitCSV(i)
131300090623     C                   ENDIF
131400090623     C*** GCA
131500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
131600090623     C                   EVAL      VABGCA = SkSplitCSV(i)
131700090623     C                   ENDIF
131800090623     C*** XCO
131900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
132000090623     C                   EVAL      VABXCO = SkSplitCSV(i)
132100090623     C                   ENDIF
132200090623     C*** CTM
132300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
132400090623     C                   EVAL      VABCTM = SkSplitCSV(i)
132500090623     C                   ENDIF
132600090623     C*** TCR
132700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
132800090623     C                   EVAL      VABTCR = SkSplitCSV(i)
132900090623     C                   ENDIF
133000090623     C*** CTS
133100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
133200090623     C                   EVAL      VABCTS = SkSplitCSV(i)
133300090623     C                   ENDIF
133400090623     C*** FTM
133500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
133600090623     C                   EVAL      VABFTM = SkSplitCSV(i)
133700090623     C                   ENDIF
133800090623     C*** VAD
133900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
134000090623     C                   EVAL      VABVAD = SkSplitCSV(i)
134100090623     C                   ENDIF
134200090623     C*** GMA
134300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
134400090623     C                   EVAL      VABGMA = SkSplitCSV(i)
134500090623     C                   ENDIF
134600090623     C*** GGA
134700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
134800090623     C                   EVAL      VABGGA = SkSplitCSV(i)
134900090623     C                   ENDIF
135000090623     C*** GVA
135100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
135200090623     C                   EVAL      VABGVA = SkSplitCSV(i)
135300090623     C                   ENDIF
135400090623     C*** TC1
135500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
135600090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
135700090623     C                   ENDIF
135800090623     C*** TC2
135900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
136000090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
136100090623     C                   ENDIF
136200090623     C*** SCL
136300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
136400090623     C                   EVAL      VABSCL = SkSplitCSV(i)
136500090623     C                   ENDIF
136600090623     C*** RMO
136700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
136800090623     C                   EVAL      VABRMO = SkSplitCSV(i)
136900090623     C                   ENDIF
137000090623     C*** NMO
137100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
137200090623     C                   EVAL      VABNMO = SkSplitCSV(i)
137300090623     C                   ENDIF
137400090623     C*
137500090623     C* Reperisco quindi i campi numerici...
137600111117     C*** NSP
137700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
137800111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
137900090623     C                   EXSR      CHKNUM
138000090623     C                   IF        PiInt=*on
138100090623     C                   Z-ADD     PiVal         VABNSP
138200090623     C                   ELSE
138300090623     C                   SETON                                        31
138400090623     C                   EVAL      VABNSP = *zeros
138500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
138600090623     C                             + ' ' + 'VABNSP'
138700090623     C                   ENDIF
138800090623     C                   ENDIF
138900090623     C*** RMN
139000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
139100111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
139200090623     C                   EXSR      CHKNUM
139300090623     C                   IF        PiInt=*on
139400090623     C                   Z-ADD     PiVal         VABRMN
139500090623     C                   ELSE
139600090623     C                   SETON                                        32
139700090623     C                   EVAL      VABRMN = *zeros
139800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
139900090623     C                             + ' ' + 'VABRMN'
140000090623     C                   ENDIF
140100090623     C                   ENDIF
140200151116     C*** LNP
140300151116     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF2'
140400151120     C* Solo se richiesta elab. multi-filiale in anagrafica
140500151120     C                   IF        vlrpoi = 999
140600151116     C                   EVAL      PiStr=SkSplitCSV(i)
140700151116     C                   EXSR      CHKNUM
140800151116     C                   IF        PiInt=*on
140900151120     C                   Z-ADD     PiVal         VABFGS
141000151120     C                   Z-ADD     PiVal         VATFGS
141100151116     C                   ELSE
141200151116     C                   SETON                                        32
141300151116     C                   EVAL      vinmsg = %trimr(vinmsg)
141400151120     C                             + ' ' + 'VABFGS VATFGS'
141500151120     C                   ENDIF
141600151116     C                   ENDIF
141700151116     C                   ENDIF
141800090623     C*** CAD
141900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
142000090623     C                   EVAL      PiStr=SkSplitCSV(i)
142100090623     C                   EXSR      CHKNUM
142200090623     C                   IF        PiInt=*on
142300090623     C                   Z-ADD     PiVal         Num5_0
142400090623     C                   MOVEL(p)  Num5_0        VABCAD
142500090623     C                   ELSE
142600090623     C                   EVAL      VABCAD = SkSplitCSV(i)
142700090623     C                   SETON                                        32
142800090623     C                   EVAL      VABCAD = *zeros
142900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143000090623     C                             + ' ' + 'VABCAD'
143100090623     C                   ENDIF
143200090623     C                   ENDIF
143300090623     C*** NCL
143400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
143500090623     C                   EVAL      PiStr=SkSplitCSV(i)
143600090623     C                   EXSR      CHKNUM
143700090623     C                   IF        PiInt=*on
143800090623     C                   Z-ADD     PiVal         VABNCL
143900090623     C                   ELSE
144000090623     C                   SETON                                        32
144100090623     C                   EVAL      VABNCL = *zeros
144200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144300090623     C                             + ' ' + 'VABNCL'
144400090623     C                   ENDIF
144500090623     C                   ENDIF
144600090623     C*** PKB
144700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
144800090623     C                   EVAL      PiStr=SkSplitCSV(i)
144900090623     C                   EXSR      CHKNUM
145000090623     C                   IF        PiNum=*on
145100130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
145200090623     C                   Z-ADD     PiVal         VABPKB
145300090623     C                   ELSE
145400090623     C                   SETON                                        32
145500090623     C                   EVAL      VABPKB = *zeros
145600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145700090623     C                             + ' ' + 'VABPKB'
145800090623     C                   ENDIF
145900090623     C                   ENDIF
146000090623     C*** CAS
146100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
146200090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
146300090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
146400090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
146500090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
146600090623     C                   EVAL      wFlgCAS = '1'
146700090623     C                   ENDIF
146800090623     C                   EVAL      PiStr=SkSplitCSV(i)
146900090623     C                   EXSR      CHKNUM
147000090623     C                   IF        PiNum=*on
147100090623     C                   Z-ADD     PiVal         VABCAS
147200090623     C                   ELSE
147300090623     C                   SETON                                        32
147400090623     C                   EVAL      VABCAS = *zeros
147500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147600090623     C                             + ' ' + 'VABCAS'
147700090623     C                   ENDIF
147800090623     C                   ENDIF
147900090623     C*** CCM
148000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
148100090623     C                   EVAL      PiStr=SkSplitCSV(i)
148200090623     C                   EXSR      CHKNUM
148300090623     C                   IF        PiInt=*on      AND
148400090623     C                             PiVal<=9999999 AND
148500090623     C                             PiVal>*zeros
148600090623     C                   Z-ADD     PiVal         VABCCM
148700090623     C                   ELSE
148800090623     C                   SETON                                        32
148900090623     C                   EVAL      VABCCM = *zeros
149000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149100090623     C                             + ' ' + 'VABCCM'
149200090623     C                   ENDIF
149300090623     C                   ENDIF
149400090623     C*** LNP
149500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
149600090623     C                   EVAL      PiStr=SkSplitCSV(i)
149700090623     C                   EXSR      CHKNUM
149800090623     C                   IF        PiInt=*on    AND
149900090623     C                             PiVal<=999   AND
150000090623     C                             PiVal>*zeros
150100090623     C                   Z-ADD     PiVal         VABLNP
150200090623     C                   ELSE
150300090623     C                   SETON                                        32
150400090623     C                   EVAL      VABLNP = *zeros
150500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150600090623     C                             + ' ' + 'VABLNP'
150700090623     C                   ENDIF
150800090623     C                   ENDIF
150900090623     C*** NRS
151000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
151100090623     C                   EVAL      PiStr=SkSplitCSV(i)
151200090623     C                   EXSR      CHKNUM
151300090623     C                   IF        PiInt=*on AND
151400090623     C                             PiVal<=99
151500090623     C                   Z-ADD     PiVal         VABNRS
151600090623     C                   ELSE
151700090623     C                   SETON                                        32
151800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151900090623     C                             + ' ' + 'VABNRS'
152000090623     C                   ENDIF
152100090623     C                   ENDIF
152200090623     C*** LNA
152300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
152400090623     C                   EVAL      PiStr=SkSplitCSV(i)
152500090623     C                   EXSR      CHKNUM
152600090623     C                   IF        PiInt=*on    AND
152700090623     C                             PiVal<=999   AND
152800090623     C                             PiVal>*zeros
152900090623     C                   Z-ADD     PiVal         VABLNA
153000090623     C                   ELSE
153100090623     C                   SETON                                        32
153200090623     C                   EVAL      VABLNA = *zeros
153300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
153400090623     C                             + ' ' + 'VABLNA'
153500090623     C                   ENDIF
153600090623     C                   ENDIF
153700090623     C*** CTR
153800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
153900090623     C                   EVAL      PiStr=SkSplitCSV(i)
154000090623     C                   EXSR      CHKNUM
154100090623     C                   IF        PiInt=*on  AND
154200090623     C                             PiVal<=999
154300090623     C                   Z-ADD     PiVal         VABCTR
154400090623     C                   ELSE
154500090623     C                   SETON                                        32
154600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154700090623     C                             + ' ' + 'VABCTR'
154800090623     C                   ENDIF
154900090623     C                   ENDIF
155000090623     C*** IAS
155100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
155200090623     C                   EVAL      PiStr=SkSplitCSV(i)
155300090623     C                   EXSR      CHKNUM
155400090623     C                   IF        PiNum=*on
155500090623     C                   Z-ADD     PiVal         VABIAS
155600090623     C                   ELSE
155700090623     C                   SETON                                        32
155800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155900090623     C                             + ' ' + 'VABIAS'
156000090623     C                   ENDIF
156100090623     C                   ENDIF
156200090623     C*** VLB
156300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
156400090623     C                   EVAL      PiStr=SkSplitCSV(i)
156500090623     C                   EXSR      CHKNUM
156600090623     C                   IF        PiNum=*on
156700111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
156800090623     C                   Z-ADD     PiVal         VABVLB
156900090623     C                   ELSE
157000090623     C                   SETON                                        32
157100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157200090623     C                             + ' ' + 'VABVLB'
157300090623     C                   ENDIF
157400090623     C                   ENDIF
157500090623     C*** QFT
157600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
157700090623     C                   EVAL      PiStr=SkSplitCSV(i)
157800090623     C                   EXSR      CHKNUM
157900090623     C                   IF        PiNum=*on
158000090623     C                   Z-ADD     PiVal         VABQFT
158100090623     C                   ELSE
158200090623     C                   SETON                                        32
158300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158400090623     C                             + ' ' + 'VABQFT'
158500090623     C                   ENDIF
158600090623     C                   ENDIF
158700090623     C*** NCD
158800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
158900090623     C                   EVAL      PiStr=SkSplitCSV(i)
159000090623     C                   EXSR      CHKNUM
159100090623     C                   IF        PiInt=*on      AND
159200090623     C                             PiVal<=9999999
159300090623     C                   Z-ADD     PiVal         VABNCD
159400090623     C                   ELSE
159500090623     C                   SETON                                        32
159600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159700090623     C                             + ' ' + 'VABNCD'
159800090623     C                   ENDIF
159900090623     C                   ENDIF
160000090623     C*** NCA
160100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
160200090623     C                   EVAL      PiStr=SkSplitCSV(i)
160300090623     C                   EXSR      CHKNUM
160400090623     C                   IF        PiInt=*on      AND
160500090623     C                             PiVal<=9999999
160600090623     C                   Z-ADD     PiVal         VABNCA
160700090623     C                   ELSE
160800090623     C                   SETON                                        32
160900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161000090623     C                             + ' ' + 'VABNCA'
161100090623     C                   ENDIF
161200090623     C                   ENDIF
161300090623     C*** ZNC
161400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
161500090623     C                   EVAL      PiStr=SkSplitCSV(i)
161600090623     C                   EXSR      CHKNUM
161700090623     C                   IF        PiInt=*on AND
161800090623     C                             PiVal<=99
161900090623     C                   Z-ADD     PiVal         VABZNC
162000090623     C                   ELSE
162100090623     C                   SETON                                        32
162200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162300090623     C                             + ' ' + 'VABZNC'
162400090623     C                   ENDIF
162500090623     C                   ENDIF
162600090623     C*** DCR
162700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
162800090623     C                   EVAL      PiStr=SkSplitCSV(i)
162900090623     C                   EXSR      CHKNUM
163000090623     C                   IF        PiInt=*on       AND
163100090623     C                             PiVal<=99999999
163200090623     C                   Z-ADD     PiVal         VABDCR
163300090623     C                   ELSE
163400090623     C                   SETON                                        32
163500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163600090623     C                             + ' ' + 'VABDCR'
163700090623     C                   ENDIF
163800090623     C                   ENDIF
163900090623     C*** HCR
164000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
164100090623     C                   EVAL      PiStr=SkSplitCSV(i)
164200090623     C                   EXSR      CHKNUM
164300090623     C                   IF        PiInt=*on   AND
164400090623     C                             PiVal<=9999
164500090623     C                   Z-ADD     PiVal         VABHCR
164600090623     C                   ELSE
164700090623     C                   SETON                                        32
164800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164900090623     C                             + ' ' + 'VABHCR'
165000090623     C                   ENDIF
165100090623     C                   ENDIF
165200090623     C*** VMD
165300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
165400090623     C                   EVAL      PiStr=SkSplitCSV(i)
165500090623     C                   EXSR      CHKNUM
165600090623     C                   IF        PiNum=*on
165700090623     C                   Z-ADD     PiVal         VABVMD
165800090623     C                   ELSE
165900090623     C                   SETON                                        32
166000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166100090623     C                             + ' ' + 'VABVMD'
166200090623     C                   ENDIF
166300090623     C                   ENDIF
166400090623     C*** ANT
166500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
166600090623     C                   EVAL      PiStr=SkSplitCSV(i)
166700090623     C                   EXSR      CHKNUM
166800090623     C                   IF        PiInt=*on        AND
166900090623     C                             PiVal<=999999999
167000090623     C                   Z-ADD     PiVal         VABANT
167100090623     C                   ELSE
167200090623     C                   SETON                                        32
167300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167400090623     C                             + ' ' + 'VABANT'
167500090623     C                   ENDIF
167600090623     C                   ENDIF
167700090623     C*** CMO
167800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
167900090623     C                   EVAL      PiStr=SkSplitCSV(i)
168000090623     C                   EXSR      CHKNUM
168100090623     C                   IF        PiInt=*on
168200090623     C                   Z-ADD     PiVal         Num5_0
168300090623     C                   MOVEL(p)  Num5_0        VABCMO
168400090623     C                   ELSE
168500090623     C                   SETON                                        32
168600090623     C                   EVAL      VABCMO = *zeros
168700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168800090623     C                             + ' ' + 'VABCMO'
168900090623     C                   ENDIF
169000090623     C                   ENDIF
169100090623     C*
169200090911     C* Reperisco i campi del EDIVAT
169300090623     C*** VATNOT
169400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
169500090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
169600090623     C                   ENDIF
169700090623     C*** VATTRC
169800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
169900090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
170000090623     C                   ENDIF
170100090623     C*** VATNOT_A
170200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
170300090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
170400090623     C                   ENDIF
170500090623     C*** VATNOT_B
170600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
170700090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
170800090623     C                   ENDIF
170900141125     C*** VATNOT_S
171000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
171100141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
171200141125     C                   ENDIF
171300141125     C*** VATNOT_IJ
171400141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
171500141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
171600141125     C                   ENDIF
171700111118     C*** VATNOT_E_4
171800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
171900111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
172000111118     C                   ENDIF
172100090623     C*
172200090623     C***  ===>  Gestione campi "particolari" dentro ciclo
172300090623     C*
172400090623     C*
172500090623     C***  <===  -----------------------------------------
172600090908     C*
172700090623     C*
172800090623     C                   ENDSR
172900090623     C***
173000090623
173100111118
173200111118
173300111118     C*----------------------------------------------------*
173400111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
173500111118     C*----------------------------------------------------*
173600111118     C     CARVALP       BEGSR
173700111118     C*
173800111118     C* Reperisco i campi del EDIVAT
173900111118     C*** VATNOT
174000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
174100111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
174200111118     C                   ENDIF
174300111118     C*** VATTRC
174400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
174500111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
174600111118     C                   ENDIF
174700111118     C*** VATNOT_A
174800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
174900111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
175000111118     C                   ENDIF
175100111118     C*** VATNOT_B
175200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
175300111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
175400111118     C                   ENDIF
175500141125     C*** VATNOT_S
175600141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
175700141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
175800141125     C                   ENDIF
175900141125     C*** VATNOT_IJ
176000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
176100141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
176200141125     C                   ENDIF
176300111216     C*** VATNOT_E_1
176400111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
176500111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
176600111216     C***                ENDIF
176700111118     C*** VATNOT_E_2
176800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
176900111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
177000111118     C                   ENDIF
177100111118     C*** VATNOT_E_3
177200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
177300111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
177400111118     C                   ENDIF
177500111118     C*** VATNOT_E_4
177600111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
177700111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
177800111118     C                   ENDIF
177900111118     C*
178000111118     C***  ===>  Gestione campi "particolari" dentro ciclo
178100111118     C*
178200111118     C*
178300111118     C***  <===  -----------------------------------------
178400111118     C*
178500111118     C*
178600111118     C                   ENDSR
178700111118     C***
178800010601
178900020204
179000020204     C*----------------------------------------------------*
179100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
179200020204     C*----------------------------------------------------*
179300020204     C     CHKIMPDIV     BEGSR
179400020204     C*
179500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
179600020204     C                   Z-ADD     *zeros        wrkDec            9 9
179700020204     C*
179800020204     C* Come prima cosa effettuo considerazioni sulla divisa
179900020204     C                   IF        vabIAS > *zeros
180000020204     C                   IF        vabVAS <> 'EUR'
180100020204     C                   EVAL      vabVAS =  'ITL'
180200020204     C                   ENDIF
180300020204     C                   ENDIF
180400020204     C*
180500020204     C                   IF        vabCAS > *zeros
180600020204     C                   IF        vabVCA <> 'EUR'
180700020204     C                   EVAL      vabVCA =  'ITL'
180800020204     C                   ENDIF
180900020204     C                   ENDIF
181000020204     C*
181100020204     C                   IF        vabVMD > *zeros
181200020204     C                   IF        vabVAD <> 'EUR'
181300020204     C                   EVAL      vabVAD =  'ITL'
181400020204     C                   ENDIF
181500020204     C                   ENDIF
181600020204     C*
181700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
181800020204     C                   Z-ADD     vabIAS        wrkDec
181900020204     C                   IF        wrkDec > *zeros
182000020204     C                   IF        vabVAS = 'ITL'
182100020204     C                   EVAL      vabIAS = *zeros
182200020204     C                   ENDIF
182300020204     C                   ENDIF
182400020204     C*
182500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
182600020204     C                   Z-ADD     vabCAS        wrkDec
182700020204     C                   IF        wrkDec > *zeros
182800020204     C                   IF        vabVCA = 'ITL'
182900020204     C                   EVAL      vabCAS = *zeros
183000020204     C                   ENDIF
183100020204     C                   ENDIF
183200020204     C*
183300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
183400020204     C                   Z-ADD     vabVMD        wrkDec
183500020204     C                   IF        wrkDec > *zeros
183600020204     C                   IF        vabVAD = 'ITL'
183700020204     C                   EVAL      vabVMD = *zeros
183800020204     C                   ENDIF
183900020204     C                   ENDIF
184000020204     C*
184100020204     C                   ENDSR
184200020204     C***
184300080617
184400080617
184500080617
184600080617     C*----------------------------------------------------*
184700080617     C*  CONTROLLO NUMERICITA' CAMPI
184800080617     C*----------------------------------------------------*
184900080617     C     CHKNUM        BEGSR
185000080617     C*
185100080617     C                   IF        PiDecChr = *blanks
185200080617     C                   EVAL      PiDecChr = CharNUM
185300080617     C                   ENDIF
185400080617     C*
185500080617     C                   callp     UBISNUM_Check(PiStr
185600080617     C                                          :PiDecChr
185700080617     C                                          :PiVal
185800080617     C                                          :PiNum
185900080617     C                                          :PiInt)
186000080617     C*
186100080617     C                   ENDSR
186200080617     C***
186300010601
186400010601
186500010601
186600010330
186700040119      /TITLE Invio dei dati al punto operativo.
186800040119     C     invio         BEGSR
186900040119     C*
187000101005     C* 1° invio EDIVAT
187100040119     C                   reset                   dscmz
187200151120     C                   move      invfil        cmzdst
187300090911     C                   eval      cmzfld = 'EDIVATWR'
187400040119     C                   eval      cmzmbd = vlrhdl
187500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
187600040119     C***                if        prmfir = *blanks
187700090911     C                   eval      cmzfla = 'EDIVAT0F'
187800090911     C                   eval      cmzmba = 'EDIVAT0F'
187900040119     C***                else
188000040119     C***                eval      cmzfla = prmfir
188100040119     C***                eval      cmzmba = prmfir
188200040119     C***                endif
188300040119     C                   eval      cmznrr = *zeros
188400040119     C                   move      §ctrokvt      cmznrr
188500040119     C                   eval      cmzlba = vlrfl1
188600040119     C                   call(e)   'TIS711C'
188700040119     C                   parm                    dscmz
188800040119     C                   parm      *blanks       esito
188900040119     C                   if        %error
189000040119     C                             or cmzerr = '1'
189100040119     C                             or esito  = '1'
189200040119     C                   eval      wrkesito = '3'
189300040119     C                   else
189400040119     C*
189500101005     C* 2° invio EDIVAB
189600040119     C                   reset                   dscmz
189700151120     C                   move      invfil        cmzdst
189800040119     C                   eval      cmzfld = vlrfou
189900040119     C                   eval      cmzmbd = vlrhdl
190000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
190100040119     C***                if        prmfir = *blanks
190200090911     C                   eval      cmzfla = 'EDIVAB0F'
190300090911     C                   eval      cmzmba = 'EDIVAB0F'
190400040119     C***                else
190500040119     C***                eval      cmzfla = prmfir
190600040119     C***                eval      cmzmba = prmfir
190700040119     C***                endif
190800040119     C                   eval      cmznrr = *zeros
190900040119     C                   move      §ctrokvb      cmznrr
191000040119     C                   eval      cmzlba = vlrfl1
191100040119     C                   call(e)   'TIS711C'
191200040119     C                   parm                    dscmz
191300040119     C                   parm      *blanks       esito
191400040119     C                   if        %error
191500040119     C                             or cmzerr = '1'
191600040119     C                             or esito  = '1'
191700040119     C                   eval      wrkesito = '3'
191800040119     C                   endif
191900040119     C                   endif
192000040119     C*
192100040119     C                   ENDSR
192200040119     C***
192300010601
192400010601
192500010601
192600010601
192700010601      /TITLE Invio dei dati al punto operativo.
192800010601     C     opeini        BEGSR
192900010601     C*
193000010601     C* Inizializzo flag e contatori operativi
193100010601     C                   movel     '0'           flgGiro           1
193200010601     C                   movel     '0'           flgMulti          1
193300010601     C                   movel     '1'           flgStato          1
193400010615     C                   movel     '0'           flgOk             1
193500010601     C                   z-add     *zeros        cntNonEl         10 0
193600010601     C                   z-add     *zeros        invfil            3 0
193700010601     C*
193800010601     C                   ENDSR
193900010601     C***
194000070326
194100070326
194200070326
194300070326
194400080916     C     *pssr         BEGSR
194500070329     C*
194600080916     C                   if        %open(tivin00r)
194700080916     C                   close     tivin00r
194800080916     C                   endif
194900090911     C                   if        %open(edivabwr)
195000090911     C                   close     edivabwr
195100080916     C                   endif
195200090911     C                   if        %open(edivatwr)
195300090911     C                   close     edivatwr
195400080916     C                   endif
195500070326     C*
195600070326     C* Effettuo la chiamata al CLLE preposto
195700101005     C                   call(e)   'TITVEVTC'
195800080916     C                   parm                    parccm
195900080916     C                   parm                    parmbr
196000080916     C                   parm      '2'           paropz
196100070326     C*
196200080916     C                   eval      wrkesito = '2'
196300070404     C*
196400080916     C                   seton                                        LR
196500070326     C*
196600080916     C                   ENDSR     '*CANCL'
196700070326     C***
196800070326
196900070326
197000010330
197100010330
197200000613     C     *inzsr        BEGSR
197300990910     C*
197400990910     C     *entry        plist
197500990920     C                   parm                    tivlrds
197600990921     C                   parm      wrkesito      esito
197700000724     C                   parm                    prmlit
197800000710     C                   parm                    prmfir
197900010330     C*
198000010330     C* CALCOLA LA DATA CORRENTE
198100010330     C                   time                    wn14             14 0
198200090911     C                   movel     wn14          oracor            6 0          *ORA
198300110923     C                   z-add     *zeros        datcor            8 0
198400110923     C                   eval      datcor = %dec(%date() : *ISO)
198500111118     C*
198600111118     C* Forzo un numero CMR
198700150605     C                   eval      wCMR = 'ARAMEX '+%char(datcor)+%char(oracor)
198800000613     C*
198900000613     C                   ENDSR
199000000613     C***
