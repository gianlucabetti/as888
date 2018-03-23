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
021500010601     C*
021600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021700010605     C                   eval      depfil = VABLNP
021800010601     C                   exsr      repfil
021900010601     C                   if        depfil = invfil
022000021025     C                   if        vlrpoi = 999
022100030822     C                   move(P)   invfil        VABFGS
022200021025     C                   else
022300030822     C                   move(P)   vlrpoi        VABFGS
022400021025     C                   endif
022500090914     C*
022600090914     C                   move      VABFGS        currFGS           3 0
022700090914     C*
022800090914     C* Elaborazione record corrente
022900090914     C                   exsr      elabCurrRec
023000010604     C*
023100010604     C                   if        *in31 = *off and
023200010604     C                             *in32 = *off
023300010604     C                   eval      vinflg = '1'
023400010604     C                   else
023500010604     C                   eval      recko = vindta
023600010604     C                   eval      vinflg = '2'
023700010604     C                   endif
023800010604     C                   endif
023900010601     C*
024000010604     C                   endif
024100010604     C*
024200010330     C                   else
024300010330     C                   eval      vinflg = '1'
024400010330     C                   endif
024500010601     C*
024600010601     C  N70              update    tivin000
024700010330     C*
024800030822     C  N70              enddo
024900090623     C*
025000090623     C* Scarico la testata rimasta "in canna"
025100140415     C                   if        not *in33                                    * esludo estero
025200090623     C  N31              exsr      wriVAB
025300140415     C                   endif
025400040119     C*
025500040119     C                   endif                                                  (endif - chkcall)
025600010601     C*
025700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025800010601     C                   if        cntNonEl = *zeros or
025900010601     C                             flgMulti = '0'
026000010330     C* Se non ci sono record con errori ...
026100010601     C                   if        §ctrno = 0 and
026200010604     C                             §ctrmo = 0 and
026300010601     C                             flgStato <> '2'
026400010330     C* ... restituisco esito OK.
026500010330     C                   eval      wrkesito = '0'
026600010330     C                   else
026700040119     C                   if        §ctrokvb > 0
026800010330     C                   eval      wrkesito = '1'
026900010330     C                   else
027000010615     C                   if        flgOk = '0'
027100010615     C                   eval      wrkesito = '2'
027200010615     C                   else
027300010615     C                   eval      wrkesito = '6'
027400010615     C                   endif
027500010330     C                   endif
027600010330     C                   endif
027700010601     C                   else
027800010601     C                   eval      wrkesito = '9'
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        %open(tivin00r)
028200010330     C                   close     tivin00r
028300010330     C                   endif
028400090911     C                   if        %open(edivabwr)
028500090911     C                   close     edivabwr
028600010330     C                   endif
028700090911     C                   if        %open(edivatwr)
028800090911     C                   close     edivatwr
028900040512     C                   endif
029000010601     C*
029100010601     C                   if        vlrpoi <> 999
029200010601     C                   eval      invfil = vlrpoi
029300010601     C                   endif
029400010330     C*
029500040119     C                   if        §ctrokvb > 0
029600010601     C                             and invfil > *zeros
029700010330     C                   exsr      invio
029800010330     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200090623
030300090623
030400090623
030500090623     C*----------------------------------------------------*
030600090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
030700090623     C*----------------------------------------------------*
030800090623     C     INZWRK        BEGSR
030900090623     C*
031000090623     C                   eval      i = 1
031100090623     C                   eval      posDa = *zeros
031200090623     C                   eval      posA  = *zeros
031300090623     C*
031400090623     C                   ENDSR
031500090623
031600090623
031700090623
031800090623     C*----------------------------------------------------*
031900090623     C*  ELABORAZIONE RECORD CORRENTE
032000090623     C*----------------------------------------------------*
032100090623     C     ELABCURRREC   BEGSR
032200090623     C*
032300090623     C                   select
032400090623     C*
032500110923     C* Tipo record "TESTATA" - HEADER
032600111118     C                   when      %subst(vindta:1:6) = 'HEADER'
032700090623     C                   if        wNewBolla > *zeros
032800140415     C                   if        not *in33                                    * esludo estero
032900090623     C  N31              exsr      wriVAB
033000140415     C                   endif
033100090623     C                   exsr      inzvar
033200090623     C                   endif
033300090624     C                   eval      wNewBolla = 1
033400090623     C                   eval      wGiro = *zeros
033500111118     C                   clear                   SkSplitFLDH
033600110923     C                   eval      prefix = 'HDR_'
033700090624     C                   exsr      inzwrk
033800090623     C                   exsr      impvab
033900090623     C                   exsr      inzwrk
034000090623     C                   exsr      impvab
034100090623     C*
034200110923     C* Tipo record "DETTAGLIO" - PARCEL
034300110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
034400090623     C                   eval      wGiro = *zeros
034500111118     C                   clear                   SkSplitFLDP
034600110923     C                   eval      prefix = 'PCL_'
034700090623     C                   exsr      inzwrk
034800101005     C                   exsr      impvat
034900090623     C                   exsr      inzwrk
035000101005     C                   exsr      impvat
035100090624     C*
035200090624     C  N31              exsr      wrivat
035300090623     C*
035400090623     C*
035500090623     C                   endsl
035600090623     C*
035700090623     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036300040119     C*----------------------------------------------------*
036400040510     C     WRIVAB        BEGSR
036500090623     C*
036600090623     C  N31              add       1             §CTROKVB
036700090623     C   32              add       1             §CTRMO
036800090623     C   31              add       1             §CTRNO
036900040119     C*
037000060331     C* Gestisco l'eventuale rottura x numero spedizione
037100070823     C                   if        VABNSP <> depspe
037200070301     C                   if        wVATNOT_E <> *blanks
037300080125     C                   if        VABCTM = *blanks
037400080125     C                   eval      VABCTM = '7Q'
037500080125     C                   endif
037600070301     C                   endif
037700090911     C*
037800090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037900090911     C                   eval      VABCMR = wCMR
038000090911     C                   eval      VABDCM = DATCOR
038100090911     C                   eval      VABDTS = DATCOR
038200090911     C                   eval      VABHMS = ORACOR
038300090911     C                   eval      VABCNT = 1
038400090911     C****
038500090911     C                   write     edivab00                                     => scarico il VAB
038600060331     C                   endif
038700090623     C*
038800090623     C  N31              eval      depspe = vabNSP
038900040119     C*
039000040119     C                   ENDSR
039100040119
039200040119
039300040119
039400040119     C*----------------------------------------------------*
039500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039600040119     C*----------------------------------------------------*
039700040510     C     WRIVAT        BEGSR
039800060512     C*
039900060512     C* Inizializzo i campi chiave
040000090914     C                   eval      VATFGS = currFGS
040100060512     C                   eval      VATAAS = VABAAS
040200060512     C                   eval      VATCCM = VABCCM
040300070823     C                   eval      VATNRS = VABNRS
040400060512     C                   eval      VATNSP = VABNSP
040500060512     C                   eval      VATLNP = VABLNP
040600111117     C                   eval      VATCMR = wCMR
040700090914     C                   eval      VATCNT = 1
040800060331     C*
040900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041000060331     C                   if        wVATNOT_A <> *blanks
041100060331     C                   eval      VATNOT = wVATNOT_A
041200060331     C                   eval      VATTRC = 'A'
041300090911     C                   write     EDIVAT00
041400060331     C                   add       1             §CTROKVT
041500060331     C                   endif
041600060331     C*
041700060331     C                   if        wVATNOT_B <> *blanks
041800060331     C                   eval      VATNOT = wVATNOT_B
041900060331     C                   eval      VATTRC = 'B'
042000090911     C                   write     EDIVAT00
042100060331     C                   add       1             §CTROKVT
042200060331     C                   endif
042300111118     C*
042400111118     C                   if        wVATNOT_E <> *blanks
042500111118     C                   eval      VATNOT = wVATNOT_E
042600111118     C                   eval      VATTRC = 'E'
042700111118     C                   write     EDIVAT00
042800111118     C                   add       1             §CTROKVT
042900111118     C                   endif
043000141125     C*
043100141125     C                   if        wVATNOT_S <> *blanks
043200141125     C                   eval      VATNOT = wVATNOT_S
043300141125     C                   eval      VATTRC = 'S'
043400141125     C                   write     EDIVAT00
043500141125     C                   add       1             §CTROKVT
043600141125     C                   endif
043700141125     C*
043800141125     C                   if        wVATNOT_IJ <> *blanks
043900141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044000141125     C                   eval      VATTRC = 'I'
044100141125     C                   write     EDIVAT00
044200141125     C                   add       1             §CTROKVT
044300141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
044400141125     C                   eval      VATTRC = 'J'
044500141125     C                   write     EDIVAT00
044600141125     C                   add       1             §CTROKVT
044700141125     C                   endif
044800090623     C*
044900111118 xxx C* Considero il VAT solo a rottura del "chi sono"
045000111118     C***                if        wVATNOT_E <> *blanks AND
045100111118     C***                          wVATNOT_E <> sVATNOT_E
045200111118     C***                eval      VATNOT = wVATNOT_E
045300111118     C***                eval      VATTRC = 'E'
045400111118     C***                write     EDIVAT00
045500111118     C***                add       1             §CTROKVT
045600111118     C***                eval      sVATNOT_E = wVATNOT_E
045700111118     C***                add       1             VABNCL
045800111118     C***                endif
045900040119     C*
046000040119     C                   ENDSR
046100040119
046200040119
046300040119
046400040119     C*----------------------------------------------------*
046500101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
046600040119     C*----------------------------------------------------*
046700101005     C     PREVAT        BEGSR
046800040119     C*
046900101005     C* Compongo il nome del membro da dare al EDIVATWR
047000040119     C                   eval      parmbr = vlrhdl
047100040119     C                   movel     'M'           parmbr
047200040510     C                   eval      parccm = vlrksc
047300040119     C                   eval      paropz = '1'
047400040119     C* Effettuo la chiamata al CLLE preposto
047500101005     C                   call(e)   'TITVEVTC'
047600040119     C                   parm                    parccm
047700040119     C                   parm                    parmbr
047800040119     C                   parm                    paropz
047900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048000040119     C                   if        %error
048100040119     C                   movel     '1'           chkcall
048200040119     C                   else
048300040119     C                   movel     '0'           chkcall
048400040119     C                   endif
048500040119     C*
048600040119     C                   ENDSR
048700010601
048800010601
048900010601
049000010330     C*----------------------------------------------------*
049100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049200010330     C*----------------------------------------------------*
049300040119     C     INZVAR        BEGSR
049400010330     C*
049500030822     C* Inizializzo variabili di wrk
049600140415     C                   SETOFF                                       313233
049700090916     C                   Z-ADD     *zeros        Num5_0            5 0
049800090916     C                   MOVEL     *blanks       wNOTE            70
049900060331     C                   MOVEL     *blanks       wVATNOT_A
050000060331     C                   MOVEL     *blanks       wVATNOT_B
050100141125     C                   MOVEL     *blanks       wVATNOT_S
050200141125     C                   MOVEL     *blanks       wVATNOT_IJ
050300090904     C                   MOVEL     *blanks       wVATNOT_E
050400111117     C                   MOVEL     *blanks       wVATNOT_E_1
050500111117     C                   MOVEL     *blanks       wVATNOT_E_2
050600111118     C                   MOVEL     *blanks       wVATNOT_E_3
050700111118     C                   MOVEL     *blanks       wVATNOT_E_4
050800090623     C*
050900111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
051000090220     C*
051100080617     C                   EVAL      wFlgCAS = *blanks
051200030822     C*
051300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
051400090911     C                   CLEAR                   EDIVAB00
051500090911     C                   CLEAR                   EDIVAT00
051600030822     C                   CLEAR                   SkSplitCSV
051700090623     C                   EXSR      inzwrk
051800030822     C*
051900030822     C* Reimposto i valori di default
052000030822     C                   EXSR      DEFCAM
052100030822     C*
052200010330     C                   ENDSR
052300010330     C*----------------------------------------------------*
052400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052500010330     C*----------------------------------------------------*
052600010330     C     DEFCAM        BEGSR
052700030822     C*
052800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052900030715     C* e delimitatore testo.
053000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053300030822     C*
053400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
053600030715     C                   SETON                                        50
053700030715     C                   ELSE
053800030715     C                   SETOFF                                       50
053900030715     C                   ENDIF
054000090421     C*
054100090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
054200090421     C                   EVAL      VABCBO = '1'
054300090421     C                   EVAL      VABLNP = vlrpoi
054400090421     C                   EVAL      VABNSP = rrnum
054500030822     C*
054600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054700030822     C                   EVAL      posDaDft = 1
054800030822     C                   EVAL      posADft  = 0
054900030822     C                   EVAL      wGiroDft = 0
055000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055100030822     C                             posDaDft > 0
055200030822     C*
055300030822     C* Gestisco il 1° giro
055400030822     C                   IF        wGiroDft = 0
055500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055700030822     C* Incremento il contatore dei "giri"
055800030822     C                   EVAL      wGiroDft = 1
055900030822     C                   ELSE
056000030822     C                   EVAL      posDaDft = posADft
056100030822     C                   ENDIF
056200030822     C* Eseguo lo scan x trovare la fine del campo corrente
056300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056400030822     C*
056500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056700030822     C                   IF        posDaDft > 0 AND
056800030822     C                             posADft  > 0
056900030822     C* NCL
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'NCL'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABNCL
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* CCM
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'CCM'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABCCM
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* LNP
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'LNP'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABLNP
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* NRS
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'NRS'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABNRS
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CTR
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CTR'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABCTR
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* PKB
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'PKB'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABPKB
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700140715     C* PKN
064800140715     C                   IF        %subst(
064900140715     C                             %subst(vlrppt:posDaDft+1:
065000140715     C                             posADft-posDaDft-1):1:3)
065100140715     C                             = 'PKN'
065200140715     C                   EVAL      PiStr=%trim(%subst(
065300140715     C                             %subst(vlrppt:posDaDft+1:
065400140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500140715     C                   EXSR      CHKNUM
065600140715     C                   IF        PiNum=*on
065700140715     C                   Z-ADD     PiVal         wVABPKN           7 1
065800140715     C                   ENDIF
065900140715     C                   ENDIF
066000030822     C* VLB
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VLB'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABVLB
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* QFT
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'QFT'
067800030822     C                   EVAL      PiStr=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   EXSR      CHKNUM
068200030822     C                   IF        PiNum=*on
068300030822     C                   Z-ADD     PiVal         VABQFT
068400030822     C                   ENDIF
068500030822     C                   ENDIF
068600030822     C* CBO
068700030822     C                   IF        %subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):1:3)
069000030822     C                             = 'CBO'
069100030822     C                   EVAL      VABCBO=%trim(%subst(
069200030822     C                             %subst(vlrppt:posDaDft+1:
069300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069400030822     C                   ENDIF
069500030822     C* TSP
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'TSP'
070000030822     C                   EVAL      VABTSP=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* VAS
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'VAS'
070900030822     C                   EVAL      VABVAS=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* VCA
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'VCA'
071800030822     C                   EVAL      VABVCA=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200070925     C* TIC
072300070925     C                   IF        %subst(
072400070925     C                             %subst(vlrppt:posDaDft+1:
072500070925     C                             posADft-posDaDft-1):1:3)
072600070925     C                             = 'TIC'
072700070925     C                   EVAL      VABTIC=%trim(%subst(
072800070925     C                             %subst(vlrppt:posDaDft+1:
072900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000070925     C                   ENDIF
073100030822     C* GCA
073200030822     C                   IF        %subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):1:3)
073500030822     C                             = 'GCA'
073600030822     C                   EVAL      VABGCA=%trim(%subst(
073700030822     C                             %subst(vlrppt:posDaDft+1:
073800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900030822     C                   ENDIF
074000030822     C* CTM
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'CTM'
074500030822     C                   EVAL      VABCTM=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* FFD
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'FFD'
075400030822     C                   EVAL      VABFFD=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* VAD
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'VAD'
076300030822     C                   EVAL      VABVAD=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* GMA
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'GMA'
077200030822     C                   EVAL      VABGMA=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* GGA
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'GGA'
078100030822     C                   EVAL      VABGGA=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* GVA
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'GVA'
079000030822     C                   EVAL      VABGVA=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* TC1
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'TC1'
079900030822     C                   EVAL      VABTC1=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* TC2
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'TC2'
080800030822     C                   EVAL      VABTC2=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200040714     C* VATTRC
081300040714     C                   IF        %subst(
081400040714     C                             %subst(vlrppt:posDaDft+1:
081500040714     C                             posADft-posDaDft-1):1:3)
081600040714     C                             = 'TRC'
081700040714     C                   EVAL      VATTRC=%trim(%subst(
081800040714     C                             %subst(vlrppt:posDaDft+1:
081900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000040714     C                   ENDIF
082100030822     C* ...
082200030822     C                   ENDIF
082300030822     C                   ENDDO
082400020204     C*
082500010330     C                   ENDSR
082600010607     C*----------------------------------------------------*
082700090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
082800010607     C*----------------------------------------------------*
082900090623     C     IMPVAB        BEGSR
083000030515     C*
083100010607     C                   MOVEL     datcor        VABAAS
083200010607     C                   MOVE      datcor        VABMGS
083300010607     C*
083400030715     C**********
083500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
083600030715     C**********
083700030715     C                   IF        wGiro = *zeros
083800080716     C*
083900080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084000080716     C                   EVAL      depvinDTA = vinDTA
084100040113     C*
084200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084300110923     C                   IF        header <> *blanks
084400110923     C                   EVAL      vindta = header
084500070823     C     ';':CharCSV   XLATE     vindta        vindta
084600040510     C                   ENDIF
084700080626     C*
084800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
084900031201     C     minu:maiu     XLATE     vindta        vindta
085000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085100030715     C                   DOW       posDa <= %len(%trim(vindta))
085200030715     C*
085300030715     C* Gestisco il 1° campo
085400030715     C                   IF        i = 1
085500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085600030715     C                   EVAL      posDa = 1
085700030715     C* Eseguo lo scan x trovare la fine del primo campo
085800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085900030715     C* A questo punto "estraggo" il campo corrente
086000111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
086100110923     C                                         %subst(vindta:posDa:(posA-posDa))
086200030715     C* X i campi successivi al 1°
086300030715     C                   ELSE
086400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086500030715     C                   EVAL      posDa = posA + 1
086600030715     C* Eseguo lo scan x trovare la fine del campo corrente
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* Gestisco l'ultimo campo
086900030715     C                   IF        posA = *zeros
087000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087100030715     C* A questo punto "estraggo" il campo corrente
087200111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
087300110923     C                                         %subst(vindta:posDa:(posA-posDa))
087400030715     C                   LEAVE
087500030715     C                   ELSE
087600030715     C* A questo punto "estraggo" il campo corrente
087700111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
087800110923     C                                         %subst(vindta:posDa:(posA-posDa))
087900030715     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800111118     C                   DOW       i < %elem(SkSplitFLDH)
088900111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
089000111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300030715     C                   EVAL      wGiro = 1
089400080716     C* Ripristino la 1° riga originale
089500080716     C                   EVAL      vinDTA = depvinDTA
089600030715     C                   ELSE
089700030715     C                   EVAL      wGiro = 2
089800031204     C**********
089900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090000031204     C* d separatore campo
090100031204     C**********
090200031204     C                   EVAL      vindta = %trim(vindta)
090300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090400031204     C                   EVAL      vindta = %subst(vindta:2)
090500031204     C                   ENDDO
090600031204     C*
090700031204     C                   Z-ADD     *zeros        lunghInput        4 0
090800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091200031204     C                   ENDDO
091300030715     C**********
091400030715     C* Effettuo lo split del campo dati d input
091500030715     C**********
091600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
091700030715     C                   DOW       posDa <= %len(%trim(vindta))
091800030715     C*
091900030715     C* Gestisco il 1° campo
092000030715     C                   IF        i = 1
092100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092200030715     C                   EVAL      posDa = 1
092300030715     C* Eseguo lo scan x trovare la fine del primo campo
092400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C* X i campi successivi al 1°
092900030715     C                   ELSE
093000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093100030715     C                   EVAL      posDa = posA + 1
093200031204     C* Verifico che nn vi sia il campo nn valorizzato
093300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093400031204     C* Se campo nn valorizzato skippo al prossimo
093500031204     C                   EVAL      SkSplitCSV(i) = *blanks
093600031204     C                   EVAL      posA  = posA + 1
093700031204     C                   ELSE
093800030715     C* Eseguo lo scan x trovare la fine del campo corrente
093900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094000030715     C* Gestisco l'ultimo campo
094100030715     C                   IF        posA = *zeros
094200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094300030715     C* A questo punto "estraggo" il campo corrente
094400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094500030715     C                                                    (posA-posDa))
094600030715     C                   LEAVE
094700030715     C                   ELSE
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   ENDIF
095200031204     C                   ENDIF
095300030715     C                   ENDIF
095400030715     C* Incremento il contatore d campo
095500030715     C                   EVAL      i = i +1
095600030715     C                   ENDDO
095700030715     C*
095800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
095900030715     C* delimitazione testo
096000030715     C                   EVAL      i = 1
096100030715     C                   DOW       i < %elem(SkSplitCSV)
096200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096400030715     C                   EVAL      i = i + 1
096500030715     C                   ENDDO
096600030715     C*
096700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
096800030715     C                   EVAL      i = 1
096900111118     C                   DOW       i < %elem(SkSplitFLDH)
097000030715     C*
097100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097200111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
097300090623     C                             SkSplitCSV(i) <> *blanks
097400090623     C*
097500111118     C                   exsr      CARVALH
097600040510     C*
097700030715     C                   ENDIF
097800030715     C                   EVAL      i = i + 1
097900030715     C                   ENDDO
098000070925     C*
098100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
098200080125     C*
098300111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
098400111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
098500140715     C*
098600140715     C                   IF        VABPKB = *zeros
098700140715     C                   EVAL      VABPKB = wVABPKN * VABNCL
098800140715     C                   ENDIF
098900070925     C*
099000071029     C***  <===  -----------------------------------------
099100070925     C*
099200070925     C* Considerazioni finali su CBO/CAS
099300080125     C                   IF        wFlgCAS = '1'
099400070925     C                   IF        VABCBO = '1'
099500070925     C                   EVAL      VABCBO = '4'
099600070925     C                   ENDIF
099700070925     C                   IF        VABCBO = '2'
099800070925     C                   EVAL      VABCBO = '6'
099900070925     C                   ENDIF
100000070925     C                   ENDIF
100100070925     C***  <===  ----------------------------
100200040510     C*
100300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
100400040510     C                   IF        VABNSP = *zeros
100500040510     C                   SETON                                        31
100600040510     C                   ENDIF
100700040715     C*
100800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
100900040715     C                   IF        VABLNP = *zeros
101000040715     C                   SETON                                        31
101100040715     C                   ENDIF
101200040510     C*
101300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
101400040510     C                   IF        VABRMN = *zeros
101500040510     C                   Z-ADD     VABNSP        VABRMN
101600040510     C                   ENDIF
101700040510     C*
101800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
101900040510     C                   IF        VABRMA = *blanks
102000040714     C                   MOVEL     VABRMN        VABRMA
102100040510     C                   ENDIF
102200040128     C*
102300040510     C* Se provincia nn valorizzata la reperisco
102400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
102500040510     C                   IF        VABPRD = *blanks
102600040128     C                   CLEAR                   TISI95DS
102700040128     C                   EVAL      I95TCN = '3'
102800040128     C                   Z-ADD     datcor        I95DAT
102900040128     C                   EVAL      I95NAR = VABNZD
103000040128     C                   EVAL      I95CAP = VABCAD
103100040128     C                   EVAL      I95LOC = VABLOD
103200040128     C                   CALL      'TISI95R'
103300040128     C                   PARM                    TISI95DS
103400040128     C                   EVAL      VABPRD = O95PRV
103500040510     C                   ENDIF
103600020204     C*
103700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
103800020204     C                   EXSR      CHKIMPDIV
103900030715     C                   ENDIF
104000020204     C*
104100010607     C                   ENDSR
104200010607     C*----------------------------------------------------*
104300090623
104400090623
104500090623
104600090623     C*----------------------------------------------------*
104700101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
104800090623     C*----------------------------------------------------*
104900101005     C     IMPVAT        BEGSR
105000090623     C*
105100090623     C**********
105200090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
105300090623     C**********
105400090623     C                   IF        wGiro = *zeros
105500090623     C*
105600090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
105700090623     C                   EVAL      depvinDTA = vinDTA
105800090623     C*
105900090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
106000110923     C                   IF        parcel <> *blanks
106100110923     C                   EVAL      vindta = parcel
106200090623     C     ';':CharCSV   XLATE     vindta        vindta
106300090623     C                   ENDIF
106400090623     C*
106500090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
106600090623     C     minu:maiu     XLATE     vindta        vindta
106700090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
106800090623     C                   DOW       posDa <= %len(%trim(vindta))
106900090623     C*
107000090623     C* Gestisco il 1° campo
107100090623     C                   IF        i = 1
107200090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
107300090623     C                   EVAL      posDa = 1
107400090623     C* Eseguo lo scan x trovare la fine del primo campo
107500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
107600090623     C* A questo punto "estraggo" il campo corrente
107700111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
107800110923     C                                         %subst(vindta:posDa:(posA-posDa))
107900090623     C* X i campi successivi al 1°
108000090623     C                   ELSE
108100090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
108200090623     C                   EVAL      posDa = posA + 1
108300090623     C* Eseguo lo scan x trovare la fine del campo corrente
108400090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
108500090623     C* Gestisco l'ultimo campo
108600090623     C                   IF        posA = *zeros
108700090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
108800090623     C* A questo punto "estraggo" il campo corrente
108900111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
109000110923     C                                         %subst(vindta:posDa:(posA-posDa))
109100090623     C                   LEAVE
109200090623     C                   ELSE
109300090623     C* A questo punto "estraggo" il campo corrente
109400111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
109500110923     C                                         %subst(vindta:posDa:(posA-posDa))
109600090623     C                   ENDIF
109700090623     C                   ENDIF
109800090623     C* Incremento il contatore d campo
109900090623     C                   EVAL      i = i +1
110000090623     C                   ENDDO
110100090623     C*
110200090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
110300090623     C* delimitazione testo
110400090623     C                   EVAL      i = 1
110500111118     C                   DOW       i < %elem(SkSplitFLDP)
110600111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
110700111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
110800090623     C                   EVAL      i = i + 1
110900090623     C                   ENDDO
111000090623     C                   EVAL      wGiro = 1
111100090623     C* Ripristino la 1° riga originale
111200090623     C                   EVAL      vinDTA = depvinDTA
111300090623     C                   ELSE
111400090623     C                   EVAL      wGiro = 2
111500090623     C**********
111600090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
111700090623     C* d separatore campo
111800090623     C**********
111900090623     C                   EVAL      vindta = %trim(vindta)
112000090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
112100090623     C                   EVAL      vindta = %subst(vindta:2)
112200090623     C                   ENDDO
112300090623     C*
112400090623     C                   Z-ADD     *zeros        lunghInput        4 0
112500090623     C                   EVAL      lunghInput = %len(%trim(vindta))
112600090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
112700090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
112800090623     C                   EVAL      lunghInput = %len(%trim(vindta))
112900090623     C                   ENDDO
113000090623     C**********
113100090623     C* Effettuo lo split del campo dati d input
113200090623     C**********
113300090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
113400090623     C                   DOW       posDa <= %len(%trim(vindta))
113500090623     C*
113600090623     C* Gestisco il 1° campo
113700090623     C                   IF        i = 1
113800090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
113900090623     C                   EVAL      posDa = 1
114000090623     C* Eseguo lo scan x trovare la fine del primo campo
114100090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
114200090623     C* A questo punto "estraggo" il campo corrente
114300090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
114400090623     C                                                    (posA-posDa))
114500090623     C* X i campi successivi al 1°
114600090623     C                   ELSE
114700090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
114800090623     C                   EVAL      posDa = posA + 1
114900090623     C* Verifico che nn vi sia il campo nn valorizzato
115000090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
115100090623     C* Se campo nn valorizzato skippo al prossimo
115200090623     C                   EVAL      SkSplitCSV(i) = *blanks
115300090623     C                   EVAL      posA  = posA + 1
115400090623     C                   ELSE
115500090623     C* Eseguo lo scan x trovare la fine del campo corrente
115600090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
115700090623     C* Gestisco l'ultimo campo
115800090623     C                   IF        posA = *zeros
115900090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
116000090623     C* A questo punto "estraggo" il campo corrente
116100090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116200090623     C                                                    (posA-posDa))
116300090623     C                   LEAVE
116400090623     C                   ELSE
116500090623     C* A questo punto "estraggo" il campo corrente
116600090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116700090623     C                                                    (posA-posDa))
116800090623     C                   ENDIF
116900090623     C                   ENDIF
117000090623     C                   ENDIF
117100090623     C* Incremento il contatore d campo
117200090623     C                   EVAL      i = i +1
117300090623     C                   ENDDO
117400090623     C*
117500090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
117600090623     C* delimitazione testo
117700090623     C                   EVAL      i = 1
117800090623     C                   DOW       i < %elem(SkSplitCSV)
117900090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
118000090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
118100090623     C                   EVAL      i = i + 1
118200090623     C                   ENDDO
118300090623     C*
118400090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
118500090623     C                   EVAL      i = 1
118600111118     C                   DOW       i < %elem(SkSplitFLDP)
118700090623     C*
118800090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
118900111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
119000090623     C                             SkSplitCSV(i) <> *blanks
119100090623     C*
119200111118     C                   exsr      CARVALP
119300090623     C*
119400090623     C                   ENDIF
119500090623     C                   EVAL      i = i + 1
119600090623     C                   ENDDO
119700090623     C*
119800090623     C***  ===>  Gestione campi "particolari" fuori ciclo
119900111118     C*
120000111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
120100111118     C                                         %trim(wVATNOT_E_2) +
120200111118     C                                         %trim(wVATNOT_E_3) +
120300111118     C                                         %trim(wVATNOT_E_4)
120400090623     C*
120500090623     C***  <===  -----------------------------------------
120600090623     C*
120700090623     C                   ENDIF
120800090623     C*
120900090623     C                   ENDSR
121000090623     C*----------------------------------------------------*
121100090623
121200090623
121300090623     C*----------------------------------------------------*
121400090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
121500090623     C*----------------------------------------------------*
121600111118     C     CARVALH       BEGSR
121700090623     C*
121800090623     C*** ATB
121900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
122000090623     C                   EVAL      VABATB = SkSplitCSV(i)
122100090623     C                   ENDIF
122200090623     C*** RSD
122300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
122400090623     C                   EVAL      VABRSD = SkSplitCSV(i)
122500090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
122600090623     C     '@':'A'       XLATE     VABRSD        VABRSD
122700090623     C* ==
122800090623     C                   ENDIF
122900090623     C*** RD2
123000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
123100090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
123200090623     C                   ENDIF
123300130530     C*** IND 1 (HDR)
123400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
123500090623     C                   EVAL      VABIND = SkSplitCSV(i)
123600090623     C                   ENDIF
123700130530     C*** IND 2 (HDR)
123800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
123900111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
124000111117     C                   ENDIF
124100130530     C*** IND 1
124200130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
124300130530     C                   EVAL      VABIND = SkSplitCSV(i)
124400130530     C                   ENDIF
124500130530     C*** IND 2
124600130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
124700130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
124800130530     C                   ENDIF
124900111117     C*** CAD
125000111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
125100111117     C                   EVAL      VABCAD = SkSplitCSV(i)
125200111216     C                   MOVEL     *blanks       wAlfa7            7
125300111216     C                   MOVEL     *zeros        wAlfa7z           7
125400111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
125500111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
125600111117     C                   ENDIF
125700090623     C*** LOD
125800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
125900090623     C                   EVAL      VABLOD = SkSplitCSV(i)
126000090623     C                   ENDIF
126100111117     C*** PRD
126200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
126300090623     C                   EVAL      VABPRD = SkSplitCSV(i)
126400090623     C                   ENDIF
126500090623     C*** NZD
126600140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
126700140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
126800140415     C                   SETON                                        33
126900090623     C                   ENDIF
127000140415     C                   ENDIF
127100090623     C*** RMA
127200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
127300130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
127400130611     C                   EVAL      VABRMA = SkSplitCSV(i)
127500090623     C                   ENDIF
127600111117     C*** NOT
127700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
127800111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
127900090623     C                   ENDIF
128000111117     C*** NT2
128100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
128200111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
128300090623     C                   ENDIF
128400090623     C*** VCA
128500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
128600090623     C                   EVAL      VABVCA = SkSplitCSV(i)
128700090623     C                   ENDIF
128800090623     C*** VAS
128900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
129000090623     C                   EVAL      VABVAS = SkSplitCSV(i)
129100090623     C                   ENDIF
129200090623     C*** FFD
129300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
129400090623     C                   EVAL      VABFFD = SkSplitCSV(i)
129500090623     C                   ENDIF
129600090623     C*** GC1
129700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
129800090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
129900090623     C                   ENDIF
130000090623     C*** GC2
130100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
130200090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
130300090623     C                   ENDIF
130400090623     C*** TSP
130500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
130600090623     C                   EVAL      VABTSP = SkSplitCSV(i)
130700090623     C                   ENDIF
130800090623     C*** CBO
130900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
131000090623     C                   EVAL      VABCBO = SkSplitCSV(i)
131100090623     C                   ENDIF
131200090623     C*** NAS
131300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
131400090623     C                   EVAL      VABNAS = SkSplitCSV(i)
131500090623     C                   ENDIF
131600090623     C*** TIC
131700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
131800090623     C                   EVAL      VABTIC = SkSplitCSV(i)
131900090623     C                   ENDIF
132000090623     C*** GCA
132100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
132200090623     C                   EVAL      VABGCA = SkSplitCSV(i)
132300090623     C                   ENDIF
132400090623     C*** XCO
132500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
132600090623     C                   EVAL      VABXCO = SkSplitCSV(i)
132700090623     C                   ENDIF
132800090623     C*** CTM
132900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
133000090623     C                   EVAL      VABCTM = SkSplitCSV(i)
133100090623     C                   ENDIF
133200090623     C*** TCR
133300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
133400090623     C                   EVAL      VABTCR = SkSplitCSV(i)
133500090623     C                   ENDIF
133600090623     C*** CTS
133700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
133800090623     C                   EVAL      VABCTS = SkSplitCSV(i)
133900090623     C                   ENDIF
134000090623     C*** FTM
134100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
134200090623     C                   EVAL      VABFTM = SkSplitCSV(i)
134300090623     C                   ENDIF
134400090623     C*** VAD
134500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
134600090623     C                   EVAL      VABVAD = SkSplitCSV(i)
134700090623     C                   ENDIF
134800090623     C*** GMA
134900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
135000090623     C                   EVAL      VABGMA = SkSplitCSV(i)
135100090623     C                   ENDIF
135200090623     C*** GGA
135300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
135400090623     C                   EVAL      VABGGA = SkSplitCSV(i)
135500090623     C                   ENDIF
135600090623     C*** GVA
135700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
135800090623     C                   EVAL      VABGVA = SkSplitCSV(i)
135900090623     C                   ENDIF
136000090623     C*** TC1
136100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
136200090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
136300090623     C                   ENDIF
136400090623     C*** TC2
136500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
136600090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
136700090623     C                   ENDIF
136800090623     C*** SCL
136900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
137000090623     C                   EVAL      VABSCL = SkSplitCSV(i)
137100090623     C                   ENDIF
137200090623     C*** RMO
137300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
137400090623     C                   EVAL      VABRMO = SkSplitCSV(i)
137500090623     C                   ENDIF
137600090623     C*** NMO
137700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
137800090623     C                   EVAL      VABNMO = SkSplitCSV(i)
137900090623     C                   ENDIF
138000090623     C*
138100090623     C* Reperisco quindi i campi numerici...
138200111117     C*** NSP
138300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
138400111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
138500090623     C                   EXSR      CHKNUM
138600090623     C                   IF        PiInt=*on
138700090623     C                   Z-ADD     PiVal         VABNSP
138800090623     C                   ELSE
138900090623     C                   SETON                                        31
139000090623     C                   EVAL      VABNSP = *zeros
139100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
139200090623     C                             + ' ' + 'VABNSP'
139300090623     C                   ENDIF
139400090623     C                   ENDIF
139500090623     C*** RMN
139600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
139700111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
139800090623     C                   EXSR      CHKNUM
139900090623     C                   IF        PiInt=*on
140000090623     C                   Z-ADD     PiVal         VABRMN
140100090623     C                   ELSE
140200090623     C                   SETON                                        32
140300090623     C                   EVAL      VABRMN = *zeros
140400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
140500090623     C                             + ' ' + 'VABRMN'
140600090623     C                   ENDIF
140700090623     C                   ENDIF
140800090623     C*** CAD
140900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
141000090623     C                   EVAL      PiStr=SkSplitCSV(i)
141100090623     C                   EXSR      CHKNUM
141200090623     C                   IF        PiInt=*on
141300090623     C                   Z-ADD     PiVal         Num5_0
141400090623     C                   MOVEL(p)  Num5_0        VABCAD
141500090623     C                   ELSE
141600090623     C                   EVAL      VABCAD = SkSplitCSV(i)
141700090623     C                   SETON                                        32
141800090623     C                   EVAL      VABCAD = *zeros
141900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142000090623     C                             + ' ' + 'VABCAD'
142100090623     C                   ENDIF
142200090623     C                   ENDIF
142300090623     C*** NCL
142400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
142500090623     C                   EVAL      PiStr=SkSplitCSV(i)
142600090623     C                   EXSR      CHKNUM
142700090623     C                   IF        PiInt=*on
142800090623     C                   Z-ADD     PiVal         VABNCL
142900090623     C                   ELSE
143000090623     C                   SETON                                        32
143100090623     C                   EVAL      VABNCL = *zeros
143200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143300090623     C                             + ' ' + 'VABNCL'
143400090623     C                   ENDIF
143500090623     C                   ENDIF
143600090623     C*** PKB
143700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
143800090623     C                   EVAL      PiStr=SkSplitCSV(i)
143900090623     C                   EXSR      CHKNUM
144000090623     C                   IF        PiNum=*on
144100130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
144200090623     C                   Z-ADD     PiVal         VABPKB
144300090623     C                   ELSE
144400090623     C                   SETON                                        32
144500090623     C                   EVAL      VABPKB = *zeros
144600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144700090623     C                             + ' ' + 'VABPKB'
144800090623     C                   ENDIF
144900090623     C                   ENDIF
145000090623     C*** CAS
145100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
145200090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
145300090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
145400090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
145500090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
145600090623     C                   EVAL      wFlgCAS = '1'
145700090623     C                   ENDIF
145800090623     C                   EVAL      PiStr=SkSplitCSV(i)
145900090623     C                   EXSR      CHKNUM
146000090623     C                   IF        PiNum=*on
146100090623     C                   Z-ADD     PiVal         VABCAS
146200090623     C                   ELSE
146300090623     C                   SETON                                        32
146400090623     C                   EVAL      VABCAS = *zeros
146500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
146600090623     C                             + ' ' + 'VABCAS'
146700090623     C                   ENDIF
146800090623     C                   ENDIF
146900090623     C*** CCM
147000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
147100090623     C                   EVAL      PiStr=SkSplitCSV(i)
147200090623     C                   EXSR      CHKNUM
147300090623     C                   IF        PiInt=*on      AND
147400090623     C                             PiVal<=9999999 AND
147500090623     C                             PiVal>*zeros
147600090623     C                   Z-ADD     PiVal         VABCCM
147700090623     C                   ELSE
147800090623     C                   SETON                                        32
147900090623     C                   EVAL      VABCCM = *zeros
148000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148100090623     C                             + ' ' + 'VABCCM'
148200090623     C                   ENDIF
148300090623     C                   ENDIF
148400090623     C*** LNP
148500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
148600090623     C                   EVAL      PiStr=SkSplitCSV(i)
148700090623     C                   EXSR      CHKNUM
148800090623     C                   IF        PiInt=*on    AND
148900090623     C                             PiVal<=999   AND
149000090623     C                             PiVal>*zeros
149100090623     C                   Z-ADD     PiVal         VABLNP
149200090623     C                   ELSE
149300090623     C                   SETON                                        32
149400090623     C                   EVAL      VABLNP = *zeros
149500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149600090623     C                             + ' ' + 'VABLNP'
149700090623     C                   ENDIF
149800090623     C                   ENDIF
149900090623     C*** NRS
150000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
150100090623     C                   EVAL      PiStr=SkSplitCSV(i)
150200090623     C                   EXSR      CHKNUM
150300090623     C                   IF        PiInt=*on AND
150400090623     C                             PiVal<=99
150500090623     C                   Z-ADD     PiVal         VABNRS
150600090623     C                   ELSE
150700090623     C                   SETON                                        32
150800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150900090623     C                             + ' ' + 'VABNRS'
151000090623     C                   ENDIF
151100090623     C                   ENDIF
151200090623     C*** LNA
151300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
151400090623     C                   EVAL      PiStr=SkSplitCSV(i)
151500090623     C                   EXSR      CHKNUM
151600090623     C                   IF        PiInt=*on    AND
151700090623     C                             PiVal<=999   AND
151800090623     C                             PiVal>*zeros
151900090623     C                   Z-ADD     PiVal         VABLNA
152000090623     C                   ELSE
152100090623     C                   SETON                                        32
152200090623     C                   EVAL      VABLNA = *zeros
152300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152400090623     C                             + ' ' + 'VABLNA'
152500090623     C                   ENDIF
152600090623     C                   ENDIF
152700090623     C*** CTR
152800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
152900090623     C                   EVAL      PiStr=SkSplitCSV(i)
153000090623     C                   EXSR      CHKNUM
153100090623     C                   IF        PiInt=*on  AND
153200090623     C                             PiVal<=999
153300090623     C                   Z-ADD     PiVal         VABCTR
153400090623     C                   ELSE
153500090623     C                   SETON                                        32
153600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
153700090623     C                             + ' ' + 'VABCTR'
153800090623     C                   ENDIF
153900090623     C                   ENDIF
154000090623     C*** IAS
154100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
154200090623     C                   EVAL      PiStr=SkSplitCSV(i)
154300090623     C                   EXSR      CHKNUM
154400090623     C                   IF        PiNum=*on
154500090623     C                   Z-ADD     PiVal         VABIAS
154600090623     C                   ELSE
154700090623     C                   SETON                                        32
154800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154900090623     C                             + ' ' + 'VABIAS'
155000090623     C                   ENDIF
155100090623     C                   ENDIF
155200090623     C*** VLB
155300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
155400090623     C                   EVAL      PiStr=SkSplitCSV(i)
155500090623     C                   EXSR      CHKNUM
155600090623     C                   IF        PiNum=*on
155700111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
155800090623     C                   Z-ADD     PiVal         VABVLB
155900090623     C                   ELSE
156000090623     C                   SETON                                        32
156100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156200090623     C                             + ' ' + 'VABVLB'
156300090623     C                   ENDIF
156400090623     C                   ENDIF
156500090623     C*** QFT
156600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
156700090623     C                   EVAL      PiStr=SkSplitCSV(i)
156800090623     C                   EXSR      CHKNUM
156900090623     C                   IF        PiNum=*on
157000090623     C                   Z-ADD     PiVal         VABQFT
157100090623     C                   ELSE
157200090623     C                   SETON                                        32
157300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157400090623     C                             + ' ' + 'VABQFT'
157500090623     C                   ENDIF
157600090623     C                   ENDIF
157700090623     C*** NCD
157800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
157900090623     C                   EVAL      PiStr=SkSplitCSV(i)
158000090623     C                   EXSR      CHKNUM
158100090623     C                   IF        PiInt=*on      AND
158200090623     C                             PiVal<=9999999
158300090623     C                   Z-ADD     PiVal         VABNCD
158400090623     C                   ELSE
158500090623     C                   SETON                                        32
158600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158700090623     C                             + ' ' + 'VABNCD'
158800090623     C                   ENDIF
158900090623     C                   ENDIF
159000090623     C*** NCA
159100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
159200090623     C                   EVAL      PiStr=SkSplitCSV(i)
159300090623     C                   EXSR      CHKNUM
159400090623     C                   IF        PiInt=*on      AND
159500090623     C                             PiVal<=9999999
159600090623     C                   Z-ADD     PiVal         VABNCA
159700090623     C                   ELSE
159800090623     C                   SETON                                        32
159900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160000090623     C                             + ' ' + 'VABNCA'
160100090623     C                   ENDIF
160200090623     C                   ENDIF
160300090623     C*** ZNC
160400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
160500090623     C                   EVAL      PiStr=SkSplitCSV(i)
160600090623     C                   EXSR      CHKNUM
160700090623     C                   IF        PiInt=*on AND
160800090623     C                             PiVal<=99
160900090623     C                   Z-ADD     PiVal         VABZNC
161000090623     C                   ELSE
161100090623     C                   SETON                                        32
161200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161300090623     C                             + ' ' + 'VABZNC'
161400090623     C                   ENDIF
161500090623     C                   ENDIF
161600090623     C*** DCR
161700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
161800090623     C                   EVAL      PiStr=SkSplitCSV(i)
161900090623     C                   EXSR      CHKNUM
162000090623     C                   IF        PiInt=*on       AND
162100090623     C                             PiVal<=99999999
162200090623     C                   Z-ADD     PiVal         VABDCR
162300090623     C                   ELSE
162400090623     C                   SETON                                        32
162500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162600090623     C                             + ' ' + 'VABDCR'
162700090623     C                   ENDIF
162800090623     C                   ENDIF
162900090623     C*** HCR
163000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
163100090623     C                   EVAL      PiStr=SkSplitCSV(i)
163200090623     C                   EXSR      CHKNUM
163300090623     C                   IF        PiInt=*on   AND
163400090623     C                             PiVal<=9999
163500090623     C                   Z-ADD     PiVal         VABHCR
163600090623     C                   ELSE
163700090623     C                   SETON                                        32
163800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163900090623     C                             + ' ' + 'VABHCR'
164000090623     C                   ENDIF
164100090623     C                   ENDIF
164200090623     C*** VMD
164300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
164400090623     C                   EVAL      PiStr=SkSplitCSV(i)
164500090623     C                   EXSR      CHKNUM
164600090623     C                   IF        PiNum=*on
164700090623     C                   Z-ADD     PiVal         VABVMD
164800090623     C                   ELSE
164900090623     C                   SETON                                        32
165000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165100090623     C                             + ' ' + 'VABVMD'
165200090623     C                   ENDIF
165300090623     C                   ENDIF
165400090623     C*** ANT
165500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
165600090623     C                   EVAL      PiStr=SkSplitCSV(i)
165700090623     C                   EXSR      CHKNUM
165800090623     C                   IF        PiInt=*on        AND
165900090623     C                             PiVal<=999999999
166000090623     C                   Z-ADD     PiVal         VABANT
166100090623     C                   ELSE
166200090623     C                   SETON                                        32
166300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166400090623     C                             + ' ' + 'VABANT'
166500090623     C                   ENDIF
166600090623     C                   ENDIF
166700090623     C*** CMO
166800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
166900090623     C                   EVAL      PiStr=SkSplitCSV(i)
167000090623     C                   EXSR      CHKNUM
167100090623     C                   IF        PiInt=*on
167200090623     C                   Z-ADD     PiVal         Num5_0
167300090623     C                   MOVEL(p)  Num5_0        VABCMO
167400090623     C                   ELSE
167500090623     C                   SETON                                        32
167600090623     C                   EVAL      VABCMO = *zeros
167700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167800090623     C                             + ' ' + 'VABCMO'
167900090623     C                   ENDIF
168000090623     C                   ENDIF
168100090623     C*
168200090911     C* Reperisco i campi del EDIVAT
168300090623     C*** VATNOT
168400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
168500090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
168600090623     C                   ENDIF
168700090623     C*** VATTRC
168800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
168900090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
169000090623     C                   ENDIF
169100090623     C*** VATNOT_A
169200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
169300090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
169400090623     C                   ENDIF
169500090623     C*** VATNOT_B
169600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
169700090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
169800090623     C                   ENDIF
169900141125     C*** VATNOT_S
170000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
170100141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
170200141125     C                   ENDIF
170300141125     C*** VATNOT_IJ
170400141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
170500141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
170600141125     C                   ENDIF
170700111118     C*** VATNOT_E_4
170800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
170900111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
171000111118     C                   ENDIF
171100090623     C*
171200090623     C***  ===>  Gestione campi "particolari" dentro ciclo
171300090623     C*
171400090623     C*
171500090623     C***  <===  -----------------------------------------
171600090908     C*
171700090623     C*
171800090623     C                   ENDSR
171900090623     C***
172000090623
172100111118
172200111118
172300111118     C*----------------------------------------------------*
172400111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
172500111118     C*----------------------------------------------------*
172600111118     C     CARVALP       BEGSR
172700111118     C*
172800111118     C* Reperisco i campi del EDIVAT
172900111118     C*** VATNOT
173000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
173100111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
173200111118     C                   ENDIF
173300111118     C*** VATTRC
173400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
173500111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
173600111118     C                   ENDIF
173700111118     C*** VATNOT_A
173800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
173900111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
174000111118     C                   ENDIF
174100111118     C*** VATNOT_B
174200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
174300111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
174400111118     C                   ENDIF
174500141125     C*** VATNOT_S
174600141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
174700141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
174800141125     C                   ENDIF
174900141125     C*** VATNOT_IJ
175000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
175100141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
175200141125     C                   ENDIF
175300111216     C*** VATNOT_E_1
175400111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
175500111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
175600111216     C***                ENDIF
175700111118     C*** VATNOT_E_2
175800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
175900111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
176000111118     C                   ENDIF
176100111118     C*** VATNOT_E_3
176200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
176300111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
176400111118     C                   ENDIF
176500111118     C*** VATNOT_E_4
176600111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
176700111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
176800111118     C                   ENDIF
176900111118     C*
177000111118     C***  ===>  Gestione campi "particolari" dentro ciclo
177100111118     C*
177200111118     C*
177300111118     C***  <===  -----------------------------------------
177400111118     C*
177500111118     C*
177600111118     C                   ENDSR
177700111118     C***
177800010601
177900020204
178000020204     C*----------------------------------------------------*
178100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
178200020204     C*----------------------------------------------------*
178300020204     C     CHKIMPDIV     BEGSR
178400020204     C*
178500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
178600020204     C                   Z-ADD     *zeros        wrkDec            9 9
178700020204     C*
178800020204     C* Come prima cosa effettuo considerazioni sulla divisa
178900020204     C                   IF        vabIAS > *zeros
179000020204     C                   IF        vabVAS <> 'EUR'
179100020204     C                   EVAL      vabVAS =  'ITL'
179200020204     C                   ENDIF
179300020204     C                   ENDIF
179400020204     C*
179500020204     C                   IF        vabCAS > *zeros
179600020204     C                   IF        vabVCA <> 'EUR'
179700020204     C                   EVAL      vabVCA =  'ITL'
179800020204     C                   ENDIF
179900020204     C                   ENDIF
180000020204     C*
180100020204     C                   IF        vabVMD > *zeros
180200020204     C                   IF        vabVAD <> 'EUR'
180300020204     C                   EVAL      vabVAD =  'ITL'
180400020204     C                   ENDIF
180500020204     C                   ENDIF
180600020204     C*
180700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
180800020204     C                   Z-ADD     vabIAS        wrkDec
180900020204     C                   IF        wrkDec > *zeros
181000020204     C                   IF        vabVAS = 'ITL'
181100020204     C                   EVAL      vabIAS = *zeros
181200020204     C                   ENDIF
181300020204     C                   ENDIF
181400020204     C*
181500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
181600020204     C                   Z-ADD     vabCAS        wrkDec
181700020204     C                   IF        wrkDec > *zeros
181800020204     C                   IF        vabVCA = 'ITL'
181900020204     C                   EVAL      vabCAS = *zeros
182000020204     C                   ENDIF
182100020204     C                   ENDIF
182200020204     C*
182300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
182400020204     C                   Z-ADD     vabVMD        wrkDec
182500020204     C                   IF        wrkDec > *zeros
182600020204     C                   IF        vabVAD = 'ITL'
182700020204     C                   EVAL      vabVMD = *zeros
182800020204     C                   ENDIF
182900020204     C                   ENDIF
183000020204     C*
183100020204     C                   ENDSR
183200020204     C***
183300080617
183400080617
183500080617
183600080617     C*----------------------------------------------------*
183700080617     C*  CONTROLLO NUMERICITA' CAMPI
183800080617     C*----------------------------------------------------*
183900080617     C     CHKNUM        BEGSR
184000080617     C*
184100080617     C                   IF        PiDecChr = *blanks
184200080617     C                   EVAL      PiDecChr = CharNUM
184300080617     C                   ENDIF
184400080617     C*
184500080617     C                   callp     UBISNUM_Check(PiStr
184600080617     C                                          :PiDecChr
184700080617     C                                          :PiVal
184800080617     C                                          :PiNum
184900080617     C                                          :PiInt)
185000080617     C*
185100080617     C                   ENDSR
185200080617     C***
185300010330
185400010601
185500010601
185600010601
185700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
185800010601     C     repfil        BEGSR
185900010601     C*
186000010601     C                   if        invfil = *zeros and
186100010601     C                             depfil > *zeros and
186200010629     C                             (vinflg = *blanks or
186300010629     C                              vinflg = *zeros)
186400010601     C
186500010601     C                   eval      invfil = depfil
186600010601     C                   endif
186700010601     C*
186800010601     C                   if        depfil <> invfil and
186900010601     C                             invfil > *zeros
187000010601     C                   eval      flgMulti = '1'
187100010601     C                   if        vinflg = *blanks
187200010601     C                   add       1             cntNonEl
187300010601     C                   endif
187400010601     C                   endif
187500010601     C*
187600010601     C                   if        vinflg = '2'
187700010601     C                   eval      flgStato = '2'
187800010601     C                   endif
187900010601     C*
188000010601     C                   ENDSR
188100010601     C***
188200010601
188300010601
188400010601
188500010330
188600040119      /TITLE Invio dei dati al punto operativo.
188700040119     C     invio         BEGSR
188800040119     C*
188900101005     C* 1° invio EDIVAT
189000040119     C                   reset                   dscmz
189100040119     C                   move      vlrpoi        cmzdst
189200090911     C                   eval      cmzfld = 'EDIVATWR'
189300040119     C                   eval      cmzmbd = vlrhdl
189400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
189500040119     C***                if        prmfir = *blanks
189600090911     C                   eval      cmzfla = 'EDIVAT0F'
189700090911     C                   eval      cmzmba = 'EDIVAT0F'
189800040119     C***                else
189900040119     C***                eval      cmzfla = prmfir
190000040119     C***                eval      cmzmba = prmfir
190100040119     C***                endif
190200040119     C                   eval      cmznrr = *zeros
190300040119     C                   move      §ctrokvt      cmznrr
190400040119     C                   eval      cmzlba = vlrfl1
190500040119     C                   call(e)   'TIS711C'
190600040119     C                   parm                    dscmz
190700040119     C                   parm      *blanks       esito
190800040119     C                   if        %error
190900040119     C                             or cmzerr = '1'
191000040119     C                             or esito  = '1'
191100040119     C                   eval      wrkesito = '3'
191200040119     C                   else
191300040119     C*
191400101005     C* 2° invio EDIVAB
191500040119     C                   reset                   dscmz
191600040119     C                   move      vlrpoi        cmzdst
191700040119     C                   eval      cmzfld = vlrfou
191800040119     C                   eval      cmzmbd = vlrhdl
191900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
192000040119     C***                if        prmfir = *blanks
192100090911     C                   eval      cmzfla = 'EDIVAB0F'
192200090911     C                   eval      cmzmba = 'EDIVAB0F'
192300040119     C***                else
192400040119     C***                eval      cmzfla = prmfir
192500040119     C***                eval      cmzmba = prmfir
192600040119     C***                endif
192700040119     C                   eval      cmznrr = *zeros
192800040119     C                   move      §ctrokvb      cmznrr
192900040119     C                   eval      cmzlba = vlrfl1
193000040119     C                   call(e)   'TIS711C'
193100040119     C                   parm                    dscmz
193200040119     C                   parm      *blanks       esito
193300040119     C                   if        %error
193400040119     C                             or cmzerr = '1'
193500040119     C                             or esito  = '1'
193600040119     C                   eval      wrkesito = '3'
193700040119     C                   endif
193800040119     C                   endif
193900040119     C*
194000040119     C                   ENDSR
194100040119     C***
194200010601
194300010601
194400010601
194500010601
194600010601      /TITLE Invio dei dati al punto operativo.
194700010601     C     opeini        BEGSR
194800010601     C*
194900010601     C* Inizializzo flag e contatori operativi
195000010601     C                   movel     '0'           flgGiro           1
195100010601     C                   movel     '0'           flgMulti          1
195200010601     C                   movel     '1'           flgStato          1
195300010615     C                   movel     '0'           flgOk             1
195400010601     C                   z-add     *zeros        cntNonEl         10 0
195500010601     C                   z-add     *zeros        depfil            3 0
195600010601     C                   z-add     *zeros        invfil            3 0
195700010601     C*
195800010601     C                   ENDSR
195900010601     C***
196000070326
196100070326
196200070326
196300070326
196400080916     C     *pssr         BEGSR
196500070329     C*
196600080916     C                   if        %open(tivin00r)
196700080916     C                   close     tivin00r
196800080916     C                   endif
196900090911     C                   if        %open(edivabwr)
197000090911     C                   close     edivabwr
197100080916     C                   endif
197200090911     C                   if        %open(edivatwr)
197300090911     C                   close     edivatwr
197400080916     C                   endif
197500070326     C*
197600070326     C* Effettuo la chiamata al CLLE preposto
197700101005     C                   call(e)   'TITVEVTC'
197800080916     C                   parm                    parccm
197900080916     C                   parm                    parmbr
198000080916     C                   parm      '2'           paropz
198100070326     C*
198200080916     C                   eval      wrkesito = '2'
198300070404     C*
198400080916     C                   seton                                        LR
198500070326     C*
198600080916     C                   ENDSR     '*CANCL'
198700070326     C***
198800070326
198900070326
199000010330
199100010330
199200000613     C     *inzsr        BEGSR
199300990910     C*
199400990910     C     *entry        plist
199500990920     C                   parm                    tivlrds
199600990921     C                   parm      wrkesito      esito
199700000724     C                   parm                    prmlit
199800000710     C                   parm                    prmfir
199900010330     C*
200000010330     C* CALCOLA LA DATA CORRENTE
200100010330     C                   time                    wn14             14 0
200200090911     C                   movel     wn14          oracor            6 0          *ORA
200300110923     C                   z-add     *zeros        datcor            8 0
200400110923     C                   eval      datcor = %dec(%date() : *ISO)
200500111118     C*
200600111118     C* Forzo un numero CMR
200700111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
200800000613     C*
200900000613     C                   ENDSR
201000000613     C***
