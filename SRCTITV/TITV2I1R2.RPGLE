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
001300161011     D cod             S                   like(vindta) inz
001400010330     D*----------------------------------------------------
001500010330     D* DICHIARAZIOINE VARIABILI DI WRK
001600010330     D*----------------------------------------------------
001700010330     D dscmz         e ds                  inz
001800010330     D psds           sds
001900010330     D  procname         *PROC
002000010330     D tivlrds       e ds                  extname(tivlr00f)
002100040128     D tisi95ds      e ds
002200010330     D esito           s              1
002300010330     D prmlit          s             10
002400010330     D prmfir          s             10
002500010330     D wrkesito        s                   like(esito)
002600010330     D wrkdata         s               d
002700010330     D wrkora          s               t
002800010330     D rrnum           s              6  0 INZ(*zeros)
002900010330     D recko           s            150    INZ(*blanks)
003000011113     D depcmd          s            150    INZ(*blanks)
003100040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003200040510     D parccm          s              8    INZ(*blanks)
003300040119     D parmbr          s             10    INZ(*blanks)
003400040119     D paropz          s              1    INZ(*blanks)
003500040119     D chkcall         s              1    INZ(*blanks)
003600080125     D wFlgCAS         s              1    INZ(*blanks)
003700080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003800090911     D wCMR            s             35    INZ(*blanks)
003900030715     D*------------------
004000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004100030715     D*------------------
004200111118     D SkSplitFLDH     S             32    DIM(200)
004300111118     D SkSplitFLDP     S             32    DIM(200)
004400161011     D SkSplitFLDC     S             32    DIM(200)
004500111118     D SkSplitCSV      S            256    DIM(200)
004600030715     D CharCSV         S              1
004700030715     D CharTXT         S              1
004800030715     D CharNUM         S              1
004900030715     D posDa           S              3  0 INZ(*zeros)
005000030715     D posA            S              3  0 INZ(*zeros)
005100030715     D i               s              3  0 INZ(1)
005200030715     D wGiro           s              1  0 INZ(*zeros)
005300080125     D wVATNOT_A       s                   LIKE(VATNOT)
005400080125     D wVATNOT_B       s                   LIKE(VATNOT)
005500141125     D wVATNOT_S       s                   LIKE(VATNOT)
005600141125     D wVATNOT_IJ      s             70
005700090904     D wVATNOT_E       s                   LIKE(VATNOT)
005800111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
005900111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
006000111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
006100111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
006200030822     D*------------------
006300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006400030822     D*------------------
006500030822     D posDaDft        S              3  0 INZ(*zeros)
006600030822     D posADft         S              3  0 INZ(*zeros)
006700030822     D j               s              3  0 INZ(1)
006800030822     D wGiroDft        s              1  0 INZ(*zeros)
006900031201     D*------------------
007000031201     D* Costanti
007100031201     D*------------------
007200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007400080617     D*------------------
007500080617
007600080617     D*------------------
007700080617     D* LINKING A DEFINIZIONI ESTERNE
007800080617     D*------------------
007900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008100010330
008200010330
008300010330
008400990915     C                   time                    wrkdata
008500990915     C                   time                    wrkora
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800081014     C                   reset                   wrkesito
008900010601     C*
009000010601     C                   exsr      opeini
009100010605     C                   exsr      rwvab
009200081014     C*
009300081014     C                   exsr      endela
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010601     C*--------------------------------------------------------
010300010601     C     ENDELA        BEGSR
010400081014     C*
010500081014     C                   if        %open(tivin00r)
010600081014     C                   close     tivin00r
010700081014     C                   endif
010800090911     C                   if        %open(edivabwr)
010900090911     C                   close     edivabwr
011000081014     C                   endif
011100090911     C                   if        %open(edivatwr)
011200090911     C                   close     edivatwr
011300081014     C                   endif
011400040119     C*
011500040119     C* Effettuo la chiamata al CLLE preposto
011600101005     C                   call(e)   'TITVEVTC'
011700040510     C                   parm                    parccm
011800040119     C                   parm                    parmbr
011900040119     C                   parm      '2'           paropz
012000050201     C*
012100050201     C* Effettuo lancio TISI95 solo x chiusura
012200050201     C                   CLEAR                   TISI95DS
012300050201     C                   EVAL      I95TLA = 'C'
012400050201     C                   CALL      'TISI95R'
012500050201     C                   PARM                    TISI95DS
012600000616     C*
012700010601     C                   ENDSR
012800010601     C***
012900000613
013000010601
013100010601
013200010330     C*--------------------------------------------------------
013300090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013400010330     C*--------------------------------------------------------
013500010605     C     RWVAB         BEGSR
013600010330     C*
013700010330     C                   if        not %open(tivin00r)
013800010330     C                   open      tivin00r
013900010330     C                   endif
014000090911     C                   if        not %open(edivabwr)
014100090911     C                   open      edivabwr
014200010330     C                   endif
014300040119     C*
014400101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014500101005     C                   exsr      prevat
014600040119     C*
014700040119     C                   if        chkcall = '0'
014800040119     C*
014900090911     C                   if        not %open(edivatwr)
015000090911     C                   open      edivatwr
015100040119     C                   endif
015200090623     C*
015300040119     C                   clear                   §CTROKVB          7 0
015400040119     C                   clear                   §CTROKVT          7 0
015500040119     C                   clear                   §CTRMO            7 0
015600040119     C                   clear                   §CTRNO            7 0
015700090623     C*
015800090623     C* Inizializzazioni
015900090623     C                   exsr      inzvar
016000090623     C                   z-add     *zeros        wNewBolla         7 0
016100010330     C*
016200030822     C                   do        *HIVAL
016300010330     C*
016400030822     C                   read      tivin00r                               70
016500010618     C*
016600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016700010618     C                   if        vinflg = '1'
016800010618     C                   eval      flgOk = '1'
016900010618     C                   endif
017000040510     C*
017100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017200040510     C                   z-add     *zeros        wLenVINDTA        4 0
017300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017700040510     C                   z-add     1             wFlgVINDTA
017800040510     C                   leave
017900040510     C                   endif
018000040510     C                   enddo
018100110923     C*
018200110923     C* Ignoro i record che iniziano x '#'
018300110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
018400110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
018500161011     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL' AND
018600161011     C                             %subst(vindta:1:15)<>'#DEF;MPSEXP:COD'
018700111118     C                   z-add     0             wFlgVINDTA
018800111118     C                   else
018900110923     C*
019000110923     C* Setto i column headers "variabili"
019100111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
019200110923     C                   eval      header = %trim(%subst(vindta:13))
019300110923     C                   endif
019400111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
019500110923     C                   eval      parcel = %trim(%subst(vindta:13))
019600110923     C                   endif
019700161011     C                   if        %subst(vindta:1:15) = '#DEF;MPSEXP:COD'
019800161011     C                   eval      cod    = %trim(%subst(vindta:13))
019900161011     C                   endif
020000111118     C                   endif
020100090420     C*
020200090420     C                   add       1             rrnum
020300010618     C*
020400040510     C                   if        vindta > *blanks AND
020500090420     C                             wFlgVINDTA = 1
020600010330     C*
020700010601     C                   if        *in70 = *off and
020800010330     C                             (vinflg = *blanks
020900010330     C                              or vinflg = '0'
021000010330     C                              or vinflg = '2')
021100010330     C*
021200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021300010711     C                   if        vinflg = *blanks or vinflg = '0'
021400010711     C                   clear                   vinmsg
021500010711     C                   endif
021600010601     C*
021700090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
021800111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
021900111117     C***                eval      vindta  = '*' + %trim(vindta)
022000111117     C***                endif
022100090914     C*
022200090914     C* Elaborazione record corrente
022300090914     C                   exsr      elabCurrRec
022400151120     C*
022500151120     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022600151120     C                   if        vlrpoi = 999
022700151120     C                   else
022800151120     C                   move(P)   vlrpoi        VABFGS
022900151120     C                   endif
023000151120     C                   eval      invfil = VABFGS
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
023600010604     C                   eval      recko = vindta
023700010604     C                   eval      vinflg = '2'
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
033100161011     C                   exsr      inzvar
033200161011     C                   endif
033300090624     C                   eval      wNewBolla = 1
033400161011     C                   eval      wFlgCAS = *blanks
033500090623     C                   eval      wGiro = *zeros
033600111118     C                   clear                   SkSplitFLDH
033700110923     C                   eval      prefix = 'HDR_'
033800090624     C                   exsr      inzwrk
033900090623     C                   exsr      impvab
034000090623     C                   exsr      inzwrk
034100090623     C                   exsr      impvab
034200090623     C*
034300110923     C* Tipo record "DETTAGLIO" - PARCEL
034400110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
034500090623     C                   eval      wGiro = *zeros
034600111118     C                   clear                   SkSplitFLDP
034700110923     C                   eval      prefix = 'PCL_'
034800090623     C                   exsr      inzwrk
034900101005     C                   exsr      impvat
035000090623     C                   exsr      inzwrk
035100101005     C                   exsr      impvat
035200161209     C*
035300161209     C                   if        not *in33                                    * esludo estero
035400161209     C  N31              exsr      wrivat
035500161209     C                   endif
035600161011     C*
035700161011     C* Tipo record "CONTRASSEGNO" - COD
035800161011     C                   when      %subst(vindta:1:3) = 'COD'
035900161011     C                   eval      wGiro = *zeros
036000161011     C                   clear                   SkSplitFLDC
036100161011     C                   eval      prefix = 'COD_'
036200161011     C                   exsr      inzwrk
036300161011     C                   exsr      impvabcod
036400161011     C                   exsr      inzwrk
036500161011     C                   exsr      impvabcod
036600090623     C*
036700090623     C                   endsl
036800090623     C*
036900090623     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037500040119     C*----------------------------------------------------*
037600040510     C     WRIVAB        BEGSR
037700090623     C*
037800090623     C  N31              add       1             §CTROKVB
037900090623     C   32              add       1             §CTRMO
038000090623     C   31              add       1             §CTRNO
038100040119     C*
038200060331     C* Gestisco l'eventuale rottura x numero spedizione
038300070823     C                   if        VABNSP <> depspe
038400070301     C                   if        wVATNOT_E <> *blanks
038500080125     C                   if        VABCTM = *blanks
038600080125     C                   eval      VABCTM = '7Q'
038700080125     C                   endif
038800070301     C                   endif
038900090911     C*
039000090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
039100090911     C                   eval      VABCMR = wCMR
039200090911     C                   eval      VABDCM = DATCOR
039300090911     C                   eval      VABDTS = DATCOR
039400090911     C                   eval      VABHMS = ORACOR
039500090911     C                   eval      VABCNT = 1
039600090911     C****
039700090911     C                   write     edivab00                                     => scarico il VAB
039800060331     C                   endif
039900090623     C*
040000090623     C  N31              eval      depspe = vabNSP
040100040119     C*
040200040119     C                   ENDSR
040300040119
040400040119
040500040119
040600040119     C*----------------------------------------------------*
040700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040800040119     C*----------------------------------------------------*
040900040510     C     WRIVAT        BEGSR
041000060512     C*
041100060512     C* Inizializzo i campi chiave
041200151120     C                   eval      VATFGS = VABFGS
041300060512     C                   eval      VATAAS = VABAAS
041400060512     C                   eval      VATCCM = VABCCM
041500070823     C                   eval      VATNRS = VABNRS
041600060512     C                   eval      VATNSP = VABNSP
041700060512     C                   eval      VATLNP = VABLNP
041800111117     C                   eval      VATCMR = wCMR
041900090914     C                   eval      VATCNT = 1
042000060331     C*
042100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042200060331     C                   if        wVATNOT_A <> *blanks
042300060331     C                   eval      VATNOT = wVATNOT_A
042400060331     C                   eval      VATTRC = 'A'
042500161212     C                   if        vatNOT <> *blanks
042600090911     C                   write     EDIVAT00
042700060331     C                   add       1             §CTROKVT
042800161212     C                   endif
042900060331     C                   endif
043000060331     C*
043100060331     C                   if        wVATNOT_B <> *blanks
043200060331     C                   eval      VATNOT = wVATNOT_B
043300060331     C                   eval      VATTRC = 'B'
043400161212     C                   if        vatNOT <> *blanks
043500090911     C                   write     EDIVAT00
043600060331     C                   add       1             §CTROKVT
043700161212     C                   endif
043800060331     C                   endif
043900111118     C*
044000111118     C                   if        wVATNOT_E <> *blanks
044100111118     C                   eval      VATNOT = wVATNOT_E
044200111118     C                   eval      VATTRC = 'E'
044300161212     C                   if        vatNOT <> *blanks
044400111118     C                   write     EDIVAT00
044500111118     C                   add       1             §CTROKVT
044600161212     C                   endif
044700111118     C                   endif
044800141125     C*
044900141126 xxx C***                if        wVATNOT_S <> *blanks
045000141126     C***                eval      VATNOT = wVATNOT_S
045100141126     C***                eval      VATTRC = 'S'
045200161212     C***                if        vatNOT <> *blanks
045300141126     C***                write     EDIVAT00
045400141126     C***                add       1             §CTROKVT
045500161212     C***                endif
045600141126     C***                endif
045700141125     C*
045800141125     C                   if        wVATNOT_IJ <> *blanks
045900141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
046000141125     C                   eval      VATTRC = 'I'
046100161212     C                   if        vatNOT <> *blanks
046200141125     C                   write     EDIVAT00
046300141125     C                   add       1             §CTROKVT
046400161212     C                   endif
046500141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
046600141125     C                   eval      VATTRC = 'J'
046700161212     C                   if        vatNOT <> *blanks
046800141125     C                   write     EDIVAT00
046900141125     C                   add       1             §CTROKVT
047000161212     C                   endif
047100141125     C                   endif
047200090623     C*
047300111118 xxx C* Considero il VAT solo a rottura del "chi sono"
047400111118     C***                if        wVATNOT_E <> *blanks AND
047500111118     C***                          wVATNOT_E <> sVATNOT_E
047600111118     C***                eval      VATNOT = wVATNOT_E
047700111118     C***                eval      VATTRC = 'E'
047800161212     C***                if        vatNOT <> *blanks
047900111118     C***                write     EDIVAT00
048000111118     C***                add       1             §CTROKVT
048100161212     C***                endif
048200111118     C***                eval      sVATNOT_E = wVATNOT_E
048300111118     C***                add       1             VABNCL
048400111118     C***                endif
048500040119     C*
048600040119     C                   ENDSR
048700040119
048800040119
048900040119
049000040119     C*----------------------------------------------------*
049100101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
049200040119     C*----------------------------------------------------*
049300101005     C     PREVAT        BEGSR
049400040119     C*
049500101005     C* Compongo il nome del membro da dare al EDIVATWR
049600040119     C                   eval      parmbr = vlrhdl
049700040119     C                   movel     'M'           parmbr
049800040510     C                   eval      parccm = vlrksc
049900040119     C                   eval      paropz = '1'
050000040119     C* Effettuo la chiamata al CLLE preposto
050100101005     C                   call(e)   'TITVEVTC'
050200040119     C                   parm                    parccm
050300040119     C                   parm                    parmbr
050400040119     C                   parm                    paropz
050500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
050600040119     C                   if        %error
050700040119     C                   movel     '1'           chkcall
050800040119     C                   else
050900040119     C                   movel     '0'           chkcall
051000040119     C                   endif
051100040119     C*
051200040119     C                   ENDSR
051300010601
051400010601
051500010601
051600010330     C*----------------------------------------------------*
051700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051800010330     C*----------------------------------------------------*
051900040119     C     INZVAR        BEGSR
052000010330     C*
052100030822     C* Inizializzo variabili di wrk
052200140415     C                   SETOFF                                       313233
052300090916     C                   Z-ADD     *zeros        Num5_0            5 0
052400090916     C                   MOVEL     *blanks       wNOTE            70
052500060331     C                   MOVEL     *blanks       wVATNOT_A
052600060331     C                   MOVEL     *blanks       wVATNOT_B
052700141125     C                   MOVEL     *blanks       wVATNOT_S
052800141125     C                   MOVEL     *blanks       wVATNOT_IJ
052900090904     C                   MOVEL     *blanks       wVATNOT_E
053000111117     C                   MOVEL     *blanks       wVATNOT_E_1
053100111117     C                   MOVEL     *blanks       wVATNOT_E_2
053200111118     C                   MOVEL     *blanks       wVATNOT_E_3
053300111118     C                   MOVEL     *blanks       wVATNOT_E_4
053400090623     C*
053500111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
053600030822     C*
053700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053800090911     C                   CLEAR                   EDIVAB00
053900090911     C                   CLEAR                   EDIVAT00
054000030822     C                   CLEAR                   SkSplitCSV
054100090623     C                   EXSR      inzwrk
054200030822     C*
054300030822     C* Reimposto i valori di default
054400030822     C                   EXSR      DEFCAM
054500030822     C*
054600010330     C                   ENDSR
054700010330     C*----------------------------------------------------*
054800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054900010330     C*----------------------------------------------------*
055000010330     C     DEFCAM        BEGSR
055100030822     C*
055200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
055300030715     C* e delimitatore testo.
055400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
055500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
055600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055700030822     C*
055800150203     C* Se richiesta esclusione Estero
055900150203     C                   SETOFF                                       50
056000150203     C                   IF        %subst(vlrppt:1:1) = 'E'
056100150203     C                   SETON                                        50
056200030715     C                   ENDIF
056300090421     C*
056400090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
056500090421     C                   EVAL      VABCBO = '1'
056600090421     C                   EVAL      VABLNP = vlrpoi
056700090421     C                   EVAL      VABNSP = rrnum
056800030822     C*
056900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
057000030822     C                   EVAL      posDaDft = 1
057100030822     C                   EVAL      posADft  = 0
057200030822     C                   EVAL      wGiroDft = 0
057300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057400030822     C                             posDaDft > 0
057500030822     C*
057600030822     C* Gestisco il 1° giro
057700030822     C                   IF        wGiroDft = 0
057800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
058000030822     C* Incremento il contatore dei "giri"
058100030822     C                   EVAL      wGiroDft = 1
058200030822     C                   ELSE
058300030822     C                   EVAL      posDaDft = posADft
058400030822     C                   ENDIF
058500030822     C* Eseguo lo scan x trovare la fine del campo corrente
058600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058700030822     C*
058800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
059000030822     C                   IF        posDaDft > 0 AND
059100030822     C                             posADft  > 0
059200030822     C* NCL
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'NCL'
059700030822     C                   EVAL      PiStr=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   EXSR      CHKNUM
060100030822     C                   IF        PiInt=*on
060200030822     C                   Z-ADD     PiVal         VABNCL
060300030822     C                   ENDIF
060400030822     C                   ENDIF
060500030822     C* CCM
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'CCM'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiInt=*on
061500030822     C                   Z-ADD     PiVal         VABCCM
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800030822     C* LNP
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'LNP'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiInt=*on
062800030822     C                   Z-ADD     PiVal         VABLNP
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* NRS
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'NRS'
063600030822     C                   EVAL      PiStr=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   EXSR      CHKNUM
064000030822     C                   IF        PiInt=*on
064100030822     C                   Z-ADD     PiVal         VABNRS
064200030822     C                   ENDIF
064300030822     C                   ENDIF
064400030822     C* CTR
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'CTR'
064900030822     C                   EVAL      PiStr=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   EXSR      CHKNUM
065300030822     C                   IF        PiInt=*on
065400030822     C                   Z-ADD     PiVal         VABCTR
065500030822     C                   ENDIF
065600030822     C                   ENDIF
065700030822     C* PKB
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'PKB'
066200030822     C                   EVAL      PiStr=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   EXSR      CHKNUM
066600030822     C                   IF        PiNum=*on
066700030822     C                   Z-ADD     PiVal         VABPKB
066800030822     C                   ENDIF
066900030822     C                   ENDIF
067000140715     C* PKN
067100140715     C                   IF        %subst(
067200140715     C                             %subst(vlrppt:posDaDft+1:
067300140715     C                             posADft-posDaDft-1):1:3)
067400140715     C                             = 'PKN'
067500140715     C                   EVAL      PiStr=%trim(%subst(
067600140715     C                             %subst(vlrppt:posDaDft+1:
067700140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800140715     C                   EXSR      CHKNUM
067900140715     C                   IF        PiNum=*on
068000140715     C                   Z-ADD     PiVal         wVABPKN           7 1
068100140715     C                   ENDIF
068200140715     C                   ENDIF
068300030822     C* VLB
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'VLB'
068800030822     C                   EVAL      PiStr=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   EXSR      CHKNUM
069200030822     C                   IF        PiNum=*on
069300030822     C                   Z-ADD     PiVal         VABVLB
069400030822     C                   ENDIF
069500030822     C                   ENDIF
069600030822     C* QFT
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'QFT'
070100030822     C                   EVAL      PiStr=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   EXSR      CHKNUM
070500030822     C                   IF        PiNum=*on
070600030822     C                   Z-ADD     PiVal         VABQFT
070700030822     C                   ENDIF
070800030822     C                   ENDIF
070900030822     C* CBO
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'CBO'
071400030822     C                   EVAL      VABCBO=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* TSP
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'TSP'
072300030822     C                   EVAL      VABTSP=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VAS
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VAS'
073200030822     C                   EVAL      VABVAS=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VCA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VCA'
074100030822     C                   EVAL      VABVCA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500070925     C* TIC
074600070925     C                   IF        %subst(
074700070925     C                             %subst(vlrppt:posDaDft+1:
074800070925     C                             posADft-posDaDft-1):1:3)
074900070925     C                             = 'TIC'
075000070925     C                   EVAL      VABTIC=%trim(%subst(
075100070925     C                             %subst(vlrppt:posDaDft+1:
075200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300070925     C                   ENDIF
075400030822     C* GCA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GCA'
075900030822     C                   EVAL      VABGCA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* CTM
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'CTM'
076800030822     C                   EVAL      VABCTM=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* FFD
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'FFD'
077700030822     C                   EVAL      VABFFD=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* VAD
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'VAD'
078600030822     C                   EVAL      VABVAD=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GMA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GMA'
079500030822     C                   EVAL      VABGMA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* GGA
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'GGA'
080400030822     C                   EVAL      VABGGA=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* GVA
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'GVA'
081300030822     C                   EVAL      VABGVA=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700030822     C* TC1
081800030822     C                   IF        %subst(
081900030822     C                             %subst(vlrppt:posDaDft+1:
082000030822     C                             posADft-posDaDft-1):1:3)
082100030822     C                             = 'TC1'
082200030822     C                   EVAL      VABTC1=%trim(%subst(
082300030822     C                             %subst(vlrppt:posDaDft+1:
082400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500030822     C                   ENDIF
082600030822     C* TC2
082700030822     C                   IF        %subst(
082800030822     C                             %subst(vlrppt:posDaDft+1:
082900030822     C                             posADft-posDaDft-1):1:3)
083000030822     C                             = 'TC2'
083100030822     C                   EVAL      VABTC2=%trim(%subst(
083200030822     C                             %subst(vlrppt:posDaDft+1:
083300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083400030822     C                   ENDIF
083500040714     C* VATTRC
083600040714     C                   IF        %subst(
083700040714     C                             %subst(vlrppt:posDaDft+1:
083800040714     C                             posADft-posDaDft-1):1:3)
083900040714     C                             = 'TRC'
084000040714     C                   EVAL      VATTRC=%trim(%subst(
084100040714     C                             %subst(vlrppt:posDaDft+1:
084200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
084300040714     C                   ENDIF
084400030822     C* ...
084500030822     C                   ENDIF
084600030822     C                   ENDDO
084700020204     C*
084800010330     C                   ENDSR
084900010607     C*----------------------------------------------------*
085000090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
085100010607     C*----------------------------------------------------*
085200090623     C     IMPVAB        BEGSR
085300030515     C*
085400010607     C                   MOVEL     datcor        VABAAS
085500010607     C                   MOVE      datcor        VABMGS
085600010607     C*
085700030715     C**********
085800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085900030715     C**********
086000030715     C                   IF        wGiro = *zeros
086100080716     C*
086200080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
086300080716     C                   EVAL      depvinDTA = vinDTA
086400040113     C*
086500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
086600110923     C                   IF        header <> *blanks
086700110923     C                   EVAL      vindta = header
086800070823     C     ';':CharCSV   XLATE     vindta        vindta
086900040510     C                   ENDIF
087000080626     C*
087100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
087200031201     C     minu:maiu     XLATE     vindta        vindta
087300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087400030715     C                   DOW       posDa <= %len(%trim(vindta))
087500030715     C*
087600030715     C* Gestisco il 1° campo
087700030715     C                   IF        i = 1
087800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087900030715     C                   EVAL      posDa = 1
088000030715     C* Eseguo lo scan x trovare la fine del primo campo
088100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088200030715     C* A questo punto "estraggo" il campo corrente
088300111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
088400110923     C                                         %subst(vindta:posDa:(posA-posDa))
088500030715     C* X i campi successivi al 1°
088600030715     C                   ELSE
088700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088800030715     C                   EVAL      posDa = posA + 1
088900030715     C* Eseguo lo scan x trovare la fine del campo corrente
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* Gestisco l'ultimo campo
089200030715     C                   IF        posA = *zeros
089300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089400030715     C* A questo punto "estraggo" il campo corrente
089500111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
089600110923     C                                         %subst(vindta:posDa:(posA-posDa))
089700030715     C                   LEAVE
089800030715     C                   ELSE
089900030715     C* A questo punto "estraggo" il campo corrente
090000111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
090100110923     C                                         %subst(vindta:posDa:(posA-posDa))
090200030715     C                   ENDIF
090300030715     C                   ENDIF
090400030715     C* Incremento il contatore d campo
090500030715     C                   EVAL      i = i +1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090900030715     C* delimitazione testo
091000030715     C                   EVAL      i = 1
091100111118     C                   DOW       i < %elem(SkSplitFLDH)
091200111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
091300111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
091400030715     C                   EVAL      i = i + 1
091500030715     C                   ENDDO
091600030715     C                   EVAL      wGiro = 1
091700080716     C* Ripristino la 1° riga originale
091800080716     C                   EVAL      vinDTA = depvinDTA
091900030715     C                   ELSE
092000030715     C                   EVAL      wGiro = 2
092100031204     C**********
092200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
092300031204     C* d separatore campo
092400031204     C**********
092500031204     C                   EVAL      vindta = %trim(vindta)
092600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092700031204     C                   EVAL      vindta = %subst(vindta:2)
092800031204     C                   ENDDO
092900031204     C*
093000031204     C                   Z-ADD     *zeros        lunghInput        4 0
093100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
093300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
093400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
093500031204     C                   ENDDO
093600030715     C**********
093700030715     C* Effettuo lo split del campo dati d input
093800030715     C**********
093900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
094000030715     C                   DOW       posDa <= %len(%trim(vindta))
094100030715     C*
094200030715     C* Gestisco il 1° campo
094300030715     C                   IF        i = 1
094400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
094500030715     C                   EVAL      posDa = 1
094600030715     C* Eseguo lo scan x trovare la fine del primo campo
094700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C* X i campi successivi al 1°
095200030715     C                   ELSE
095300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
095400030715     C                   EVAL      posDa = posA + 1
095500031204     C* Verifico che nn vi sia il campo nn valorizzato
095600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095700031204     C* Se campo nn valorizzato skippo al prossimo
095800031204     C                   EVAL      SkSplitCSV(i) = *blanks
095900031204     C                   EVAL      posA  = posA + 1
096000031204     C                   ELSE
096100030715     C* Eseguo lo scan x trovare la fine del campo corrente
096200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
096300030715     C* Gestisco l'ultimo campo
096400030715     C                   IF        posA = *zeros
096500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
096600030715     C* A questo punto "estraggo" il campo corrente
096700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096800030715     C                                                    (posA-posDa))
096900030715     C                   LEAVE
097000030715     C                   ELSE
097100030715     C* A questo punto "estraggo" il campo corrente
097200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
097300030715     C                                                    (posA-posDa))
097400030715     C                   ENDIF
097500031204     C                   ENDIF
097600030715     C                   ENDIF
097700030715     C* Incremento il contatore d campo
097800030715     C                   EVAL      i = i +1
097900030715     C                   ENDDO
098000030715     C*
098100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
098200030715     C* delimitazione testo
098300030715     C                   EVAL      i = 1
098400030715     C                   DOW       i < %elem(SkSplitCSV)
098500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
098600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098700030715     C                   EVAL      i = i + 1
098800030715     C                   ENDDO
098900030715     C*
099000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
099100030715     C                   EVAL      i = 1
099200111118     C                   DOW       i < %elem(SkSplitFLDH)
099300030715     C*
099400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
099500111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
099600090623     C                             SkSplitCSV(i) <> *blanks
099700090623     C*
099800161212 xxx C                   exsr      CARVALH
099900040510     C*
100000030715     C                   ENDIF
100100030715     C                   EVAL      i = i + 1
100200030715     C                   ENDDO
100300070925     C*
100400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
100500080125     C*
100600111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
100700111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
100800140715     C*
100900140715     C                   IF        VABPKB = *zeros
101000140715     C                   EVAL      VABPKB = wVABPKN * VABNCL
101100140715     C                   ENDIF
101200070925     C*
101300071029     C***  <===  -----------------------------------------
101400070925     C*
101500070925     C* Considerazioni finali su CBO/CAS
101600161011     C                   IF        wFlgCAS = '1' OR
101700161011     C                             VABCAS > *zeros
101800070925     C                   IF        VABCBO = '1'
101900070925     C                   EVAL      VABCBO = '4'
102000070925     C                   ENDIF
102100070925     C                   IF        VABCBO = '2'
102200070925     C                   EVAL      VABCBO = '6'
102300070925     C                   ENDIF
102400070925     C                   ENDIF
102500070925     C***  <===  ----------------------------
102600040510     C*
102700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
102800040510     C                   IF        VABNSP = *zeros
102900040510     C                   SETON                                        31
103000040510     C                   ENDIF
103100040715     C*
103200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
103300040715     C                   IF        VABLNP = *zeros
103400040715     C                   SETON                                        31
103500040715     C                   ENDIF
103600040510     C*
103700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
103800040510     C                   IF        VABRMN = *zeros
103900040510     C                   Z-ADD     VABNSP        VABRMN
104000040510     C                   ENDIF
104100040510     C*
104200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
104300040510     C                   IF        VABRMA = *blanks
104400040714     C                   MOVEL     VABRMN        VABRMA
104500040510     C                   ENDIF
104600040128     C*
104700040510     C* Se provincia nn valorizzata la reperisco
104800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
104900040510     C                   IF        VABPRD = *blanks
105000040128     C                   CLEAR                   TISI95DS
105100040128     C                   EVAL      I95TCN = '3'
105200040128     C                   Z-ADD     datcor        I95DAT
105300040128     C                   EVAL      I95NAR = VABNZD
105400040128     C                   EVAL      I95CAP = VABCAD
105500040128     C                   EVAL      I95LOC = VABLOD
105600040128     C                   CALL      'TISI95R'
105700040128     C                   PARM                    TISI95DS
105800040128     C                   EVAL      VABPRD = O95PRV
105900040510     C                   ENDIF
106000020204     C*
106100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
106200020204     C                   EXSR      CHKIMPDIV
106300030715     C                   ENDIF
106400020204     C*
106500010607     C                   ENDSR
106600010607     C*----------------------------------------------------*
106700090623
106800090623
106900090623
107000090623     C*----------------------------------------------------*
107100101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
107200090623     C*----------------------------------------------------*
107300101005     C     IMPVAT        BEGSR
107400090623     C*
107500090623     C**********
107600090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
107700090623     C**********
107800090623     C                   IF        wGiro = *zeros
107900090623     C*
108000090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
108100090623     C                   EVAL      depvinDTA = vinDTA
108200090623     C*
108300090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
108400110923     C                   IF        parcel <> *blanks
108500110923     C                   EVAL      vindta = parcel
108600090623     C     ';':CharCSV   XLATE     vindta        vindta
108700090623     C                   ENDIF
108800090623     C*
108900090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
109000090623     C     minu:maiu     XLATE     vindta        vindta
109100090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
109200090623     C                   DOW       posDa <= %len(%trim(vindta))
109300090623     C*
109400090623     C* Gestisco il 1° campo
109500090623     C                   IF        i = 1
109600090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
109700090623     C                   EVAL      posDa = 1
109800090623     C* Eseguo lo scan x trovare la fine del primo campo
109900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
110000090623     C* A questo punto "estraggo" il campo corrente
110100111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
110200110923     C                                         %subst(vindta:posDa:(posA-posDa))
110300090623     C* X i campi successivi al 1°
110400090623     C                   ELSE
110500090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
110600090623     C                   EVAL      posDa = posA + 1
110700090623     C* Eseguo lo scan x trovare la fine del campo corrente
110800090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
110900090623     C* Gestisco l'ultimo campo
111000090623     C                   IF        posA = *zeros
111100090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
111200090623     C* A questo punto "estraggo" il campo corrente
111300111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
111400110923     C                                         %subst(vindta:posDa:(posA-posDa))
111500090623     C                   LEAVE
111600090623     C                   ELSE
111700090623     C* A questo punto "estraggo" il campo corrente
111800111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
111900110923     C                                         %subst(vindta:posDa:(posA-posDa))
112000090623     C                   ENDIF
112100090623     C                   ENDIF
112200090623     C* Incremento il contatore d campo
112300090623     C                   EVAL      i = i +1
112400090623     C                   ENDDO
112500090623     C*
112600090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
112700090623     C* delimitazione testo
112800090623     C                   EVAL      i = 1
112900111118     C                   DOW       i < %elem(SkSplitFLDP)
113000111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
113100111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
113200090623     C                   EVAL      i = i + 1
113300090623     C                   ENDDO
113400090623     C                   EVAL      wGiro = 1
113500090623     C* Ripristino la 1° riga originale
113600090623     C                   EVAL      vinDTA = depvinDTA
113700090623     C                   ELSE
113800090623     C                   EVAL      wGiro = 2
113900090623     C**********
114000090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
114100090623     C* d separatore campo
114200090623     C**********
114300090623     C                   EVAL      vindta = %trim(vindta)
114400090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
114500090623     C                   EVAL      vindta = %subst(vindta:2)
114600090623     C                   ENDDO
114700090623     C*
114800090623     C                   Z-ADD     *zeros        lunghInput        4 0
114900090623     C                   EVAL      lunghInput = %len(%trim(vindta))
115000090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
115100090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
115200090623     C                   EVAL      lunghInput = %len(%trim(vindta))
115300090623     C                   ENDDO
115400090623     C**********
115500090623     C* Effettuo lo split del campo dati d input
115600090623     C**********
115700090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
115800090623     C                   DOW       posDa <= %len(%trim(vindta))
115900090623     C*
116000090623     C* Gestisco il 1° campo
116100090623     C                   IF        i = 1
116200090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
116300090623     C                   EVAL      posDa = 1
116400090623     C* Eseguo lo scan x trovare la fine del primo campo
116500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
116600090623     C* A questo punto "estraggo" il campo corrente
116700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116800090623     C                                                    (posA-posDa))
116900090623     C* X i campi successivi al 1°
117000090623     C                   ELSE
117100090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
117200090623     C                   EVAL      posDa = posA + 1
117300090623     C* Verifico che nn vi sia il campo nn valorizzato
117400090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
117500090623     C* Se campo nn valorizzato skippo al prossimo
117600090623     C                   EVAL      SkSplitCSV(i) = *blanks
117700090623     C                   EVAL      posA  = posA + 1
117800090623     C                   ELSE
117900090623     C* Eseguo lo scan x trovare la fine del campo corrente
118000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
118100090623     C* Gestisco l'ultimo campo
118200090623     C                   IF        posA = *zeros
118300090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
118400090623     C* A questo punto "estraggo" il campo corrente
118500090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
118600090623     C                                                    (posA-posDa))
118700090623     C                   LEAVE
118800090623     C                   ELSE
118900090623     C* A questo punto "estraggo" il campo corrente
119000090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
119100090623     C                                                    (posA-posDa))
119200090623     C                   ENDIF
119300090623     C                   ENDIF
119400090623     C                   ENDIF
119500090623     C* Incremento il contatore d campo
119600090623     C                   EVAL      i = i +1
119700090623     C                   ENDDO
119800090623     C*
119900090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
120000090623     C* delimitazione testo
120100090623     C                   EVAL      i = 1
120200090623     C                   DOW       i < %elem(SkSplitCSV)
120300090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
120400090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
120500090623     C                   EVAL      i = i + 1
120600090623     C                   ENDDO
120700090623     C*
120800090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
120900090623     C                   EVAL      i = 1
121000111118     C                   DOW       i < %elem(SkSplitFLDP)
121100090623     C*
121200090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
121300111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
121400090623     C                             SkSplitCSV(i) <> *blanks
121500090623     C*
121600161212 xxx C                   exsr      CARVALP
121700090623     C*
121800090623     C                   ENDIF
121900090623     C                   EVAL      i = i + 1
122000090623     C                   ENDDO
122100090623     C*
122200090623     C***  ===>  Gestione campi "particolari" fuori ciclo
122300111118     C*
122400111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
122500111118     C                                         %trim(wVATNOT_E_2) +
122600111118     C                                         %trim(wVATNOT_E_3) +
122700111118     C                                         %trim(wVATNOT_E_4)
122800090623     C*
122900090623     C***  <===  -----------------------------------------
123000090623     C*
123100090623     C                   ENDIF
123200090623     C*
123300090623     C                   ENDSR
123400090623     C*----------------------------------------------------*
123500161011
123600161011
123700161011
123800161011     C*----------------------------------------------------*
123900161011     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
124000161011     C*----------------------------------------------------*
124100161011     C     IMPVABCOD     BEGSR
124200161011     C*
124300161011     C**********
124400161011     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
124500161011     C**********
124600161011     C                   IF        wGiro = *zeros
124700161011     C*
124800161011     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
124900161011     C                   EVAL      depvinDTA = vinDTA
125000161011     C*
125100161011     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
125200161011     C                   IF        cod    <> *blanks
125300161011     C                   EVAL      vindta = cod
125400161011     C     ';':CharCSV   XLATE     vindta        vindta
125500161011     C                   ENDIF
125600161011     C*
125700161011     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
125800161011     C     minu:maiu     XLATE     vindta        vindta
125900161011     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
126000161011     C                   DOW       posDa <= %len(%trim(vindta))
126100161011     C*
126200161011     C* Gestisco il 1° campo
126300161011     C                   IF        i = 1
126400161011     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
126500161011     C                   EVAL      posDa = 1
126600161011     C* Eseguo lo scan x trovare la fine del primo campo
126700161011     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
126800161011     C* A questo punto "estraggo" il campo corrente
126900161011     C                   EVAL      SkSplitFLDC(i) = %trim(prefix) +
127000161011     C                                         %subst(vindta:posDa:(posA-posDa))
127100161011     C* X i campi successivi al 1°
127200161011     C                   ELSE
127300161011     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
127400161011     C                   EVAL      posDa = posA + 1
127500161011     C* Eseguo lo scan x trovare la fine del campo corrente
127600161011     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
127700161011     C* Gestisco l'ultimo campo
127800161011     C                   IF        posA = *zeros
127900161011     C                   EVAL      posA = %len(%trim(vindta)) + 1
128000161011     C* A questo punto "estraggo" il campo corrente
128100161011     C                   EVAL      SkSplitFLDC(i) = %trim(prefix) +
128200161011     C                                         %subst(vindta:posDa:(posA-posDa))
128300161011     C                   LEAVE
128400161011     C                   ELSE
128500161011     C* A questo punto "estraggo" il campo corrente
128600161011     C                   EVAL      SkSplitFLDC(i) = %trim(prefix) +
128700161011     C                                         %subst(vindta:posDa:(posA-posDa))
128800161011     C                   ENDIF
128900161011     C                   ENDIF
129000161011     C* Incremento il contatore d campo
129100161011     C                   EVAL      i = i +1
129200161011     C                   ENDDO
129300161011     C*
129400161011     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
129500161011     C* delimitazione testo
129600161011     C                   EVAL      i = 1
129700161011     C                   DOW       i < %elem(SkSplitFLDC)
129800161011     C     CharTXT:' '   XLATE     SkSplitFLDC(i)SkSplitFLDC(i)
129900161011     C                   EVAL      SkSplitFLDC(i) = %trim(SkSplitFLDC(i))
130000161011     C                   EVAL      i = i + 1
130100161011     C                   ENDDO
130200161011     C                   EVAL      wGiro = 1
130300161011     C* Ripristino la 1° riga originale
130400161011     C                   EVAL      vinDTA = depvinDTA
130500161011     C                   ELSE
130600161011     C                   EVAL      wGiro = 2
130700161011     C**********
130800161011     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
130900161011     C* d separatore campo
131000161011     C**********
131100161011     C                   EVAL      vindta = %trim(vindta)
131200161011     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
131300161011     C                   EVAL      vindta = %subst(vindta:2)
131400161011     C                   ENDDO
131500161011     C*
131600161011     C                   Z-ADD     *zeros        lunghInput        4 0
131700161011     C                   EVAL      lunghInput = %len(%trim(vindta))
131800161011     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
131900161011     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
132000161011     C                   EVAL      lunghInput = %len(%trim(vindta))
132100161011     C                   ENDDO
132200161011     C**********
132300161011     C* Effettuo lo split del campo dati d input
132400161011     C**********
132500161011     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
132600161011     C                   DOW       posDa <= %len(%trim(vindta))
132700161011     C*
132800161011     C* Gestisco il 1° campo
132900161011     C                   IF        i = 1
133000161011     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
133100161011     C                   EVAL      posDa = 1
133200161011     C* Eseguo lo scan x trovare la fine del primo campo
133300161011     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
133400161011     C* A questo punto "estraggo" il campo corrente
133500161011     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
133600161011     C                                                    (posA-posDa))
133700161011     C* X i campi successivi al 1°
133800161011     C                   ELSE
133900161011     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
134000161011     C                   EVAL      posDa = posA + 1
134100161011     C* Verifico che nn vi sia il campo nn valorizzato
134200161011     C                   IF        %subst(vindta:posDa:1) = CharCSV
134300161011     C* Se campo nn valorizzato skippo al prossimo
134400161011     C                   EVAL      SkSplitCSV(i) = *blanks
134500161011     C                   EVAL      posA  = posA + 1
134600161011     C                   ELSE
134700161011     C* Eseguo lo scan x trovare la fine del campo corrente
134800161011     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
134900161011     C* Gestisco l'ultimo campo
135000161011     C                   IF        posA = *zeros
135100161011     C                   EVAL      posA = %len(%trim(vindta)) + 1
135200161011     C* A questo punto "estraggo" il campo corrente
135300161011     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
135400161011     C                                                    (posA-posDa))
135500161011     C                   LEAVE
135600161011     C                   ELSE
135700161011     C* A questo punto "estraggo" il campo corrente
135800161011     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
135900161011     C                                                    (posA-posDa))
136000161011     C                   ENDIF
136100161011     C                   ENDIF
136200161011     C                   ENDIF
136300161011     C* Incremento il contatore d campo
136400161011     C                   EVAL      i = i +1
136500161011     C                   ENDDO
136600161011     C*
136700161011     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
136800161011     C* delimitazione testo
136900161011     C                   EVAL      i = 1
137000161011     C                   DOW       i < %elem(SkSplitCSV)
137100161011     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
137200161011     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
137300161011     C                   EVAL      i = i + 1
137400161011     C                   ENDDO
137500161011     C*
137600161011     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
137700161011     C                   EVAL      i = 1
137800161011     C                   DOW       i < %elem(SkSplitFLDC)
137900161011     C*
138000161011     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
138100161011     C                   IF        SkSplitFLDC(i) <> *blanks AND
138200161011     C                             SkSplitCSV(i) <> *blanks
138300161011     C*
138400161212 xxx C                   exsr      CARVALC
138500161011     C*
138600161011     C                   ENDIF
138700161011     C                   EVAL      i = i + 1
138800161011     C                   ENDDO
138900161011     C*
139000161011     C***  ===>  Gestione campi "particolari" fuori ciclo
139100161011     C*
139200161011     C*
139300161011     C***  <===  -----------------------------------------
139400161011     C*
139500161011     C                   ENDIF
139600161011     C*
139700161011     C                   ENDSR
139800161011     C*----------------------------------------------------*
139900090623
140000090623
140100090623     C*----------------------------------------------------*
140200090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
140300090623     C*----------------------------------------------------*
140400111118     C     CARVALH       BEGSR
140500090623     C*
140600090623     C*** ATB
140700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
140800090623     C                   EVAL      VABATB = SkSplitCSV(i)
140900090623     C                   ENDIF
141000090623     C*** RSD
141100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
141200090623     C                   EVAL      VABRSD = SkSplitCSV(i)
141300090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
141400090623     C     '@':'A'       XLATE     VABRSD        VABRSD
141500090623     C* ==
141600090623     C                   ENDIF
141700090623     C*** RD2
141800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
141900090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
142000090623     C                   ENDIF
142100130530     C*** IND 1 (HDR)
142200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
142300090623     C                   EVAL      VABIND = SkSplitCSV(i)
142400090623     C                   ENDIF
142500130530     C*** IND 2 (HDR)
142600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
142700111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
142800111117     C                   ENDIF
142900111117     C*** CAD
143000111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
143100111117     C                   EVAL      VABCAD = SkSplitCSV(i)
143200111216     C                   MOVEL     *blanks       wAlfa7            7
143300111216     C                   MOVEL     *zeros        wAlfa7z           7
143400111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
143500111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
143600111117     C                   ENDIF
143700090623     C*** LOD
143800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
143900090623     C                   EVAL      VABLOD = SkSplitCSV(i)
144000090623     C                   ENDIF
144100111117     C*** PRD
144200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
144300090623     C                   EVAL      VABPRD = SkSplitCSV(i)
144400090623     C                   ENDIF
144500090623     C*** NZD
144600140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
144700140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
144800150203     C   50              SETON                                        33
144900090623     C                   ENDIF
145000140415     C                   ENDIF
145100090623     C*** RMA
145200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
145300130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
145400130611     C                   EVAL      VABRMA = SkSplitCSV(i)
145500090623     C                   ENDIF
145600111117     C*** NOT
145700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
145800111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
145900090623     C                   ENDIF
146000111117     C*** NT2
146100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
146200111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
146300090623     C                   ENDIF
146400090623     C*** VCA
146500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
146600090623     C                   EVAL      VABVCA = SkSplitCSV(i)
146700090623     C                   ENDIF
146800090623     C*** VAS
146900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
147000090623     C                   EVAL      VABVAS = SkSplitCSV(i)
147100090623     C                   ENDIF
147200090623     C*** FFD
147300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
147400090623     C                   EVAL      VABFFD = SkSplitCSV(i)
147500090623     C                   ENDIF
147600090623     C*** GC1
147700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
147800090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
147900090623     C                   ENDIF
148000090623     C*** GC2
148100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
148200090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
148300090623     C                   ENDIF
148400090623     C*** TSP
148500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
148600090623     C                   EVAL      VABTSP = SkSplitCSV(i)
148700090623     C                   ENDIF
148800090623     C*** CBO
148900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
149000090623     C                   EVAL      VABCBO = SkSplitCSV(i)
149100090623     C                   ENDIF
149200090623     C*** NAS
149300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
149400090623     C                   EVAL      VABNAS = SkSplitCSV(i)
149500090623     C                   ENDIF
149600090623     C*** TIC
149700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
149800090623     C                   EVAL      VABTIC = SkSplitCSV(i)
149900090623     C                   ENDIF
150000090623     C*** GCA
150100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
150200090623     C                   EVAL      VABGCA = SkSplitCSV(i)
150300090623     C                   ENDIF
150400090623     C*** XCO
150500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
150600090623     C                   EVAL      VABXCO = SkSplitCSV(i)
150700090623     C                   ENDIF
150800090623     C*** CTM
150900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
151000090623     C                   EVAL      VABCTM = SkSplitCSV(i)
151100090623     C                   ENDIF
151200090623     C*** TCR
151300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
151400090623     C                   EVAL      VABTCR = SkSplitCSV(i)
151500090623     C                   ENDIF
151600090623     C*** CTS
151700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
151800090623     C                   EVAL      VABCTS = SkSplitCSV(i)
151900090623     C                   ENDIF
152000090623     C*** FTM
152100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
152200090623     C                   EVAL      VABFTM = SkSplitCSV(i)
152300090623     C                   ENDIF
152400090623     C*** VAD
152500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
152600090623     C                   EVAL      VABVAD = SkSplitCSV(i)
152700090623     C                   ENDIF
152800090623     C*** GMA
152900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
153000090623     C                   EVAL      VABGMA = SkSplitCSV(i)
153100090623     C                   ENDIF
153200090623     C*** GGA
153300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
153400090623     C                   EVAL      VABGGA = SkSplitCSV(i)
153500090623     C                   ENDIF
153600090623     C*** GVA
153700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
153800090623     C                   EVAL      VABGVA = SkSplitCSV(i)
153900090623     C                   ENDIF
154000090623     C*** TC1
154100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
154200090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
154300090623     C                   ENDIF
154400090623     C*** TC2
154500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
154600090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
154700090623     C                   ENDIF
154800090623     C*** SCL
154900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
155000090623     C                   EVAL      VABSCL = SkSplitCSV(i)
155100090623     C                   ENDIF
155200090623     C*** RMO
155300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
155400090623     C                   EVAL      VABRMO = SkSplitCSV(i)
155500090623     C                   ENDIF
155600090623     C*** NMO
155700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
155800090623     C                   EVAL      VABNMO = SkSplitCSV(i)
155900090623     C                   ENDIF
156000090623     C*
156100090623     C* Reperisco quindi i campi numerici...
156200111117     C*** NSP
156300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
156400111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
156500090623     C                   EXSR      CHKNUM
156600090623     C                   IF        PiInt=*on
156700090623     C                   Z-ADD     PiVal         VABNSP
156800090623     C                   ELSE
156900090623     C                   SETON                                        31
157000090623     C                   EVAL      VABNSP = *zeros
157100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157200090623     C                             + ' ' + 'VABNSP'
157300090623     C                   ENDIF
157400090623     C                   ENDIF
157500090623     C*** RMN
157600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
157700111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
157800090623     C                   EXSR      CHKNUM
157900090623     C                   IF        PiInt=*on
158000090623     C                   Z-ADD     PiVal         VABRMN
158100090623     C                   ELSE
158200090623     C                   SETON                                        32
158300090623     C                   EVAL      VABRMN = *zeros
158400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158500090623     C                             + ' ' + 'VABRMN'
158600090623     C                   ENDIF
158700090623     C                   ENDIF
158800151116     C*** LNP
158900151116     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF2'
159000151120     C* Solo se richiesta elab. multi-filiale in anagrafica
159100151120     C                   IF        vlrpoi = 999
159200151116     C                   EVAL      PiStr=SkSplitCSV(i)
159300151116     C                   EXSR      CHKNUM
159400151116     C                   IF        PiInt=*on
159500151120     C                   Z-ADD     PiVal         VABFGS
159600151120     C                   Z-ADD     PiVal         VATFGS
159700151116     C                   ELSE
159800151116     C                   SETON                                        32
159900151116     C                   EVAL      vinmsg = %trimr(vinmsg)
160000151120     C                             + ' ' + 'VABFGS VATFGS'
160100151120     C                   ENDIF
160200151116     C                   ENDIF
160300151116     C                   ENDIF
160400090623     C*** CAD
160500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
160600090623     C                   EVAL      PiStr=SkSplitCSV(i)
160700090623     C                   EXSR      CHKNUM
160800090623     C                   IF        PiInt=*on
160900090623     C                   Z-ADD     PiVal         Num5_0
161000090623     C                   MOVEL(p)  Num5_0        VABCAD
161100090623     C                   ELSE
161200090623     C                   EVAL      VABCAD = SkSplitCSV(i)
161300090623     C                   SETON                                        32
161400090623     C                   EVAL      VABCAD = *zeros
161500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161600090623     C                             + ' ' + 'VABCAD'
161700090623     C                   ENDIF
161800090623     C                   ENDIF
161900090623     C*** NCL
162000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
162100090623     C                   EVAL      PiStr=SkSplitCSV(i)
162200090623     C                   EXSR      CHKNUM
162300090623     C                   IF        PiInt=*on
162400090623     C                   Z-ADD     PiVal         VABNCL
162500090623     C                   ELSE
162600090623     C                   SETON                                        32
162700090623     C                   EVAL      VABNCL = *zeros
162800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162900090623     C                             + ' ' + 'VABNCL'
163000090623     C                   ENDIF
163100090623     C                   ENDIF
163200090623     C*** PKB
163300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
163400090623     C                   EVAL      PiStr=SkSplitCSV(i)
163500090623     C                   EXSR      CHKNUM
163600090623     C                   IF        PiNum=*on
163700130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
163800090623     C                   Z-ADD     PiVal         VABPKB
163900090623     C                   ELSE
164000090623     C                   SETON                                        32
164100090623     C                   EVAL      VABPKB = *zeros
164200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164300090623     C                             + ' ' + 'VABPKB'
164400090623     C                   ENDIF
164500090623     C                   ENDIF
164600090623     C*** CAS
164700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
164800090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
164900090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
165000090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
165100090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
165200090623     C                   EVAL      wFlgCAS = '1'
165300090623     C                   ENDIF
165400090623     C                   EVAL      PiStr=SkSplitCSV(i)
165500090623     C                   EXSR      CHKNUM
165600090623     C                   IF        PiNum=*on
165700090623     C                   Z-ADD     PiVal         VABCAS
165800090623     C                   ELSE
165900090623     C                   SETON                                        32
166000090623     C                   EVAL      VABCAS = *zeros
166100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166200090623     C                             + ' ' + 'VABCAS'
166300090623     C                   ENDIF
166400090623     C                   ENDIF
166500090623     C*** CCM
166600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
166700090623     C                   EVAL      PiStr=SkSplitCSV(i)
166800090623     C                   EXSR      CHKNUM
166900090623     C                   IF        PiInt=*on      AND
167000090623     C                             PiVal<=9999999 AND
167100090623     C                             PiVal>*zeros
167200090623     C                   Z-ADD     PiVal         VABCCM
167300090623     C                   ELSE
167400090623     C                   SETON                                        32
167500090623     C                   EVAL      VABCCM = *zeros
167600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
167700090623     C                             + ' ' + 'VABCCM'
167800090623     C                   ENDIF
167900090623     C                   ENDIF
168000090623     C*** LNP
168100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
168200090623     C                   EVAL      PiStr=SkSplitCSV(i)
168300090623     C                   EXSR      CHKNUM
168400090623     C                   IF        PiInt=*on    AND
168500090623     C                             PiVal<=999   AND
168600090623     C                             PiVal>*zeros
168700090623     C                   Z-ADD     PiVal         VABLNP
168800090623     C                   ELSE
168900090623     C                   SETON                                        32
169000090623     C                   EVAL      VABLNP = *zeros
169100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169200090623     C                             + ' ' + 'VABLNP'
169300090623     C                   ENDIF
169400090623     C                   ENDIF
169500090623     C*** NRS
169600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
169700090623     C                   EVAL      PiStr=SkSplitCSV(i)
169800090623     C                   EXSR      CHKNUM
169900090623     C                   IF        PiInt=*on AND
170000090623     C                             PiVal<=99
170100090623     C                   Z-ADD     PiVal         VABNRS
170200090623     C                   ELSE
170300090623     C                   SETON                                        32
170400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
170500090623     C                             + ' ' + 'VABNRS'
170600090623     C                   ENDIF
170700090623     C                   ENDIF
170800090623     C*** LNA
170900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
171000090623     C                   EVAL      PiStr=SkSplitCSV(i)
171100090623     C                   EXSR      CHKNUM
171200090623     C                   IF        PiInt=*on    AND
171300090623     C                             PiVal<=999   AND
171400090623     C                             PiVal>*zeros
171500090623     C                   Z-ADD     PiVal         VABLNA
171600090623     C                   ELSE
171700090623     C                   SETON                                        32
171800090623     C                   EVAL      VABLNA = *zeros
171900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
172000090623     C                             + ' ' + 'VABLNA'
172100090623     C                   ENDIF
172200090623     C                   ENDIF
172300090623     C*** CTR
172400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
172500090623     C                   EVAL      PiStr=SkSplitCSV(i)
172600090623     C                   EXSR      CHKNUM
172700090623     C                   IF        PiInt=*on  AND
172800090623     C                             PiVal<=999
172900090623     C                   Z-ADD     PiVal         VABCTR
173000090623     C                   ELSE
173100090623     C                   SETON                                        32
173200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
173300090623     C                             + ' ' + 'VABCTR'
173400090623     C                   ENDIF
173500090623     C                   ENDIF
173600090623     C*** IAS
173700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
173800090623     C                   EVAL      PiStr=SkSplitCSV(i)
173900090623     C                   EXSR      CHKNUM
174000090623     C                   IF        PiNum=*on
174100090623     C                   Z-ADD     PiVal         VABIAS
174200090623     C                   ELSE
174300090623     C                   SETON                                        32
174400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
174500090623     C                             + ' ' + 'VABIAS'
174600090623     C                   ENDIF
174700090623     C                   ENDIF
174800090623     C*** VLB
174900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
175000090623     C                   EVAL      PiStr=SkSplitCSV(i)
175100090623     C                   EXSR      CHKNUM
175200090623     C                   IF        PiNum=*on
175300111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
175400090623     C                   Z-ADD     PiVal         VABVLB
175500090623     C                   ELSE
175600090623     C                   SETON                                        32
175700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
175800090623     C                             + ' ' + 'VABVLB'
175900090623     C                   ENDIF
176000090623     C                   ENDIF
176100090623     C*** QFT
176200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
176300090623     C                   EVAL      PiStr=SkSplitCSV(i)
176400090623     C                   EXSR      CHKNUM
176500090623     C                   IF        PiNum=*on
176600090623     C                   Z-ADD     PiVal         VABQFT
176700090623     C                   ELSE
176800090623     C                   SETON                                        32
176900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
177000090623     C                             + ' ' + 'VABQFT'
177100090623     C                   ENDIF
177200090623     C                   ENDIF
177300090623     C*** NCD
177400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
177500090623     C                   EVAL      PiStr=SkSplitCSV(i)
177600090623     C                   EXSR      CHKNUM
177700090623     C                   IF        PiInt=*on      AND
177800090623     C                             PiVal<=9999999
177900090623     C                   Z-ADD     PiVal         VABNCD
178000090623     C                   ELSE
178100090623     C                   SETON                                        32
178200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
178300090623     C                             + ' ' + 'VABNCD'
178400090623     C                   ENDIF
178500090623     C                   ENDIF
178600090623     C*** NCA
178700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
178800090623     C                   EVAL      PiStr=SkSplitCSV(i)
178900090623     C                   EXSR      CHKNUM
179000090623     C                   IF        PiInt=*on      AND
179100090623     C                             PiVal<=9999999
179200090623     C                   Z-ADD     PiVal         VABNCA
179300090623     C                   ELSE
179400090623     C                   SETON                                        32
179500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
179600090623     C                             + ' ' + 'VABNCA'
179700090623     C                   ENDIF
179800090623     C                   ENDIF
179900090623     C*** ZNC
180000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
180100090623     C                   EVAL      PiStr=SkSplitCSV(i)
180200090623     C                   EXSR      CHKNUM
180300090623     C                   IF        PiInt=*on AND
180400090623     C                             PiVal<=99
180500090623     C                   Z-ADD     PiVal         VABZNC
180600090623     C                   ELSE
180700090623     C                   SETON                                        32
180800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
180900090623     C                             + ' ' + 'VABZNC'
181000090623     C                   ENDIF
181100090623     C                   ENDIF
181200090623     C*** DCR
181300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
181400090623     C                   EVAL      PiStr=SkSplitCSV(i)
181500090623     C                   EXSR      CHKNUM
181600090623     C                   IF        PiInt=*on       AND
181700090623     C                             PiVal<=99999999
181800090623     C                   Z-ADD     PiVal         VABDCR
181900090623     C                   ELSE
182000090623     C                   SETON                                        32
182100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
182200090623     C                             + ' ' + 'VABDCR'
182300090623     C                   ENDIF
182400090623     C                   ENDIF
182500090623     C*** HCR
182600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
182700090623     C                   EVAL      PiStr=SkSplitCSV(i)
182800090623     C                   EXSR      CHKNUM
182900090623     C                   IF        PiInt=*on   AND
183000090623     C                             PiVal<=9999
183100090623     C                   Z-ADD     PiVal         VABHCR
183200090623     C                   ELSE
183300090623     C                   SETON                                        32
183400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
183500090623     C                             + ' ' + 'VABHCR'
183600090623     C                   ENDIF
183700090623     C                   ENDIF
183800090623     C*** VMD
183900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
184000090623     C                   EVAL      PiStr=SkSplitCSV(i)
184100090623     C                   EXSR      CHKNUM
184200090623     C                   IF        PiNum=*on
184300090623     C                   Z-ADD     PiVal         VABVMD
184400090623     C                   ELSE
184500090623     C                   SETON                                        32
184600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
184700090623     C                             + ' ' + 'VABVMD'
184800090623     C                   ENDIF
184900090623     C                   ENDIF
185000090623     C*** ANT
185100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
185200090623     C                   EVAL      PiStr=SkSplitCSV(i)
185300090623     C                   EXSR      CHKNUM
185400090623     C                   IF        PiInt=*on        AND
185500090623     C                             PiVal<=999999999
185600090623     C                   Z-ADD     PiVal         VABANT
185700090623     C                   ELSE
185800090623     C                   SETON                                        32
185900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
186000090623     C                             + ' ' + 'VABANT'
186100090623     C                   ENDIF
186200090623     C                   ENDIF
186300090623     C*** CMO
186400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
186500090623     C                   EVAL      PiStr=SkSplitCSV(i)
186600090623     C                   EXSR      CHKNUM
186700090623     C                   IF        PiInt=*on
186800090623     C                   Z-ADD     PiVal         Num5_0
186900090623     C                   MOVEL(p)  Num5_0        VABCMO
187000090623     C                   ELSE
187100090623     C                   SETON                                        32
187200090623     C                   EVAL      VABCMO = *zeros
187300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
187400090623     C                             + ' ' + 'VABCMO'
187500090623     C                   ENDIF
187600090623     C                   ENDIF
187700090623     C*
187800090911     C* Reperisco i campi del EDIVAT
187900090623     C*** VATNOT
188000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
188100090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
188200090623     C                   ENDIF
188300090623     C*** VATTRC
188400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
188500090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
188600090623     C                   ENDIF
188700090623     C*** VATNOT_A
188800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
188900090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
189000090623     C                   ENDIF
189100090623     C*** VATNOT_B
189200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
189300090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
189400090623     C                   ENDIF
189500141125     C*** VATNOT_S
189600141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
189700141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
189800141125     C                   ENDIF
189900141125     C*** VATNOT_IJ
190000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
190100141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
190200141125     C                   ENDIF
190300111118     C*** VATNOT_E_4
190400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
190500111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
190600111118     C                   ENDIF
190700090623     C*
190800090623     C***  ===>  Gestione campi "particolari" dentro ciclo
190900090623     C*
191000090623     C*
191100090623     C***  <===  -----------------------------------------
191200090908     C*
191300090623     C*
191400090623     C                   ENDSR
191500090623     C***
191600090623
191700111118
191800111118
191900111118     C*----------------------------------------------------*
192000111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
192100111118     C*----------------------------------------------------*
192200111118     C     CARVALP       BEGSR
192300111118     C*
192400111118     C* Reperisco i campi del EDIVAT
192500111118     C*** VATNOT
192600111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
192700111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
192800111118     C                   ENDIF
192900111118     C*** VATTRC
193000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
193100111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
193200111118     C                   ENDIF
193300111118     C*** VATNOT_A
193400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
193500111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
193600111118     C                   ENDIF
193700111118     C*** VATNOT_B
193800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
193900111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
194000111118     C                   ENDIF
194100111216     C*** VATNOT_E_1
194200111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
194300111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
194400111216     C***                ENDIF
194500111118     C*** VATNOT_E_2
194600111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
194700111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
194800111118     C                   ENDIF
194900111118     C*** VATNOT_E_3
195000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
195100111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
195200161011     C                   IF        %trim(SkSplitCSV(i)) = '329'
195300161011     C                   EVAL      wFlgCAS = '1'
195400161011     C                   ELSE
195500161011     C                   EVAL      wFlgCAS = *blanks
195600161011     C                   ENDIF
195700111118     C                   ENDIF
195800111118     C*** VATNOT_E_4
195900111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
196000111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
196100111118     C                   ENDIF
196200111118     C*
196300111118     C***  ===>  Gestione campi "particolari" dentro ciclo
196400111118     C*
196500111118     C*
196600111118     C***  <===  -----------------------------------------
196700111118     C*
196800111118     C*
196900111118     C                   ENDSR
197000111118     C***
197100161011
197200161011
197300161011
197400161011     C*----------------------------------------------------*
197500161011     C*  ROUTINE DI VALORIZZAZIONE CAMPI
197600161011     C*----------------------------------------------------*
197700161011     C     CARVALC       BEGSR
197800161011     C*
197900161011     C*
198000161011     C* Reperisco quindi i campi numerici...
198100161011     C*** CAS
198200161011     C                   IF        %trim(SkSplitFLDC(i)) = 'COD_NAMOUNT'
198300161011     C                   IF        wFlgCAS = '1'
198400161011     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
198500161011     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
198600161011     C                             %trim(SkSplitCSV(i)) <> '0'      AND
198700161011     C                             %trim(SkSplitCSV(i)) <> '0,00'   AND
198800161011     C                             %trim(SkSplitCSV(i)) <> '0,000'
198900161011     C                   ENDIF
199000161011     C                   EVAL      PiStr=SkSplitCSV(i)
199100161011     C                   EXSR      CHKNUM
199200161011     C                   IF        PiNum=*on
199300161011     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
199400161011     C                   ADD       PiVal         VABCAS
199500161011     C                   ELSE
199600161011     C                   SETON                                        32
199700161011     C                   EVAL      VABCAS = *zeros
199800161011     C                   EVAL      vinmsg = %trimr(vinmsg)
199900161011     C                             + ' ' + 'VABCAS'
200000161011     C                   ENDIF
200100161011     C                   ENDIF
200200161011     C                   ENDIF
200300161011     C*** VCA
200400161212     C                   IF        %trim(SkSplitFLDC(i)) = 'COD_NCURR'
200500161011     C                   EVAL      VABVCA = %trim(SkSplitCSV(i))
200600161011     C                   ENDIF
200700161011     C*
200800161011     C***  ===>  Gestione campi "particolari" dentro ciclo
200900161011     C*
201000161011     C*
201100161011     C***  <===  -----------------------------------------
201200161011     C*
201300161011     C*
201400161011     C                   ENDSR
201500161011     C***
201600161011
201700010601
201800020204
201900020204     C*----------------------------------------------------*
202000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
202100020204     C*----------------------------------------------------*
202200020204     C     CHKIMPDIV     BEGSR
202300020204     C*
202400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
202500020204     C                   Z-ADD     *zeros        wrkDec            9 9
202600020204     C*
202700020204     C* Come prima cosa effettuo considerazioni sulla divisa
202800020204     C                   IF        vabIAS > *zeros
202900020204     C                   IF        vabVAS <> 'EUR'
203000020204     C                   EVAL      vabVAS =  'ITL'
203100020204     C                   ENDIF
203200020204     C                   ENDIF
203300020204     C*
203400020204     C                   IF        vabCAS > *zeros
203500020204     C                   IF        vabVCA <> 'EUR'
203600020204     C                   EVAL      vabVCA =  'ITL'
203700020204     C                   ENDIF
203800020204     C                   ENDIF
203900020204     C*
204000020204     C                   IF        vabVMD > *zeros
204100020204     C                   IF        vabVAD <> 'EUR'
204200020204     C                   EVAL      vabVAD =  'ITL'
204300020204     C                   ENDIF
204400020204     C                   ENDIF
204500020204     C*
204600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
204700020204     C                   Z-ADD     vabIAS        wrkDec
204800020204     C                   IF        wrkDec > *zeros
204900020204     C                   IF        vabVAS = 'ITL'
205000020204     C                   EVAL      vabIAS = *zeros
205100020204     C                   ENDIF
205200020204     C                   ENDIF
205300020204     C*
205400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
205500020204     C                   Z-ADD     vabCAS        wrkDec
205600020204     C                   IF        wrkDec > *zeros
205700020204     C                   IF        vabVCA = 'ITL'
205800020204     C                   EVAL      vabCAS = *zeros
205900020204     C                   ENDIF
206000020204     C                   ENDIF
206100020204     C*
206200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
206300020204     C                   Z-ADD     vabVMD        wrkDec
206400020204     C                   IF        wrkDec > *zeros
206500020204     C                   IF        vabVAD = 'ITL'
206600020204     C                   EVAL      vabVMD = *zeros
206700020204     C                   ENDIF
206800020204     C                   ENDIF
206900020204     C*
207000020204     C                   ENDSR
207100020204     C***
207200080617
207300080617
207400080617
207500080617     C*----------------------------------------------------*
207600080617     C*  CONTROLLO NUMERICITA' CAMPI
207700080617     C*----------------------------------------------------*
207800080617     C     CHKNUM        BEGSR
207900080617     C*
208000080617     C                   IF        PiDecChr = *blanks
208100080617     C                   EVAL      PiDecChr = CharNUM
208200080617     C                   ENDIF
208300080617     C*
208400080617     C                   callp     UBISNUM_Check(PiStr
208500080617     C                                          :PiDecChr
208600080617     C                                          :PiVal
208700080617     C                                          :PiNum
208800080617     C                                          :PiInt)
208900080617     C*
209000080617     C                   ENDSR
209100080617     C***
209200010601
209300010601
209400010601
209500010330
209600040119      /TITLE Invio dei dati al punto operativo.
209700040119     C     invio         BEGSR
209800040119     C*
209900101005     C* 1° invio EDIVAT
210000040119     C                   reset                   dscmz
210100151120     C                   move      invfil        cmzdst
210200090911     C                   eval      cmzfld = 'EDIVATWR'
210300040119     C                   eval      cmzmbd = vlrhdl
210400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
210500040119     C***                if        prmfir = *blanks
210600090911     C                   eval      cmzfla = 'EDIVAT0F'
210700090911     C                   eval      cmzmba = 'EDIVAT0F'
210800040119     C***                else
210900040119     C***                eval      cmzfla = prmfir
211000040119     C***                eval      cmzmba = prmfir
211100040119     C***                endif
211200040119     C                   eval      cmznrr = *zeros
211300040119     C                   move      §ctrokvt      cmznrr
211400040119     C                   eval      cmzlba = vlrfl1
211500040119     C                   call(e)   'TIS711C'
211600040119     C                   parm                    dscmz
211700040119     C                   parm      *blanks       esito
211800040119     C                   if        %error
211900040119     C                             or cmzerr = '1'
212000040119     C                             or esito  = '1'
212100040119     C                   eval      wrkesito = '3'
212200040119     C                   else
212300040119     C*
212400101005     C* 2° invio EDIVAB
212500040119     C                   reset                   dscmz
212600151120     C                   move      invfil        cmzdst
212700040119     C                   eval      cmzfld = vlrfou
212800040119     C                   eval      cmzmbd = vlrhdl
212900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
213000040119     C***                if        prmfir = *blanks
213100090911     C                   eval      cmzfla = 'EDIVAB0F'
213200090911     C                   eval      cmzmba = 'EDIVAB0F'
213300040119     C***                else
213400040119     C***                eval      cmzfla = prmfir
213500040119     C***                eval      cmzmba = prmfir
213600040119     C***                endif
213700040119     C                   eval      cmznrr = *zeros
213800040119     C                   move      §ctrokvb      cmznrr
213900040119     C                   eval      cmzlba = vlrfl1
214000040119     C                   call(e)   'TIS711C'
214100040119     C                   parm                    dscmz
214200040119     C                   parm      *blanks       esito
214300040119     C                   if        %error
214400040119     C                             or cmzerr = '1'
214500040119     C                             or esito  = '1'
214600040119     C                   eval      wrkesito = '3'
214700040119     C                   endif
214800040119     C                   endif
214900040119     C*
215000040119     C                   ENDSR
215100040119     C***
215200010601
215300010601
215400010601
215500010601
215600010601      /TITLE Invio dei dati al punto operativo.
215700010601     C     opeini        BEGSR
215800010601     C*
215900010601     C* Inizializzo flag e contatori operativi
216000010601     C                   movel     '0'           flgGiro           1
216100010601     C                   movel     '0'           flgMulti          1
216200010601     C                   movel     '1'           flgStato          1
216300010615     C                   movel     '0'           flgOk             1
216400010601     C                   z-add     *zeros        cntNonEl         10 0
216500010601     C                   z-add     *zeros        invfil            3 0
216600010601     C*
216700010601     C                   ENDSR
216800010601     C***
216900070326
217000070326
217100070326
217200070326
217300080916     C     *pssr         BEGSR
217400070329     C*
217500080916     C                   if        %open(tivin00r)
217600080916     C                   close     tivin00r
217700080916     C                   endif
217800090911     C                   if        %open(edivabwr)
217900090911     C                   close     edivabwr
218000080916     C                   endif
218100090911     C                   if        %open(edivatwr)
218200090911     C                   close     edivatwr
218300080916     C                   endif
218400070326     C*
218500070326     C* Effettuo la chiamata al CLLE preposto
218600101005     C                   call(e)   'TITVEVTC'
218700080916     C                   parm                    parccm
218800080916     C                   parm                    parmbr
218900080916     C                   parm      '2'           paropz
219000070326     C*
219100080916     C                   eval      wrkesito = '2'
219200070404     C*
219300080916     C                   seton                                        LR
219400070326     C*
219500080916     C                   ENDSR     '*CANCL'
219600070326     C***
219700070326
219800070326
219900010330
220000010330
220100000613     C     *inzsr        BEGSR
220200990910     C*
220300990910     C     *entry        plist
220400990920     C                   parm                    tivlrds
220500990921     C                   parm      wrkesito      esito
220600000724     C                   parm                    prmlit
220700000710     C                   parm                    prmfir
220800010330     C*
220900010330     C* CALCOLA LA DATA CORRENTE
221000010330     C                   time                    wn14             14 0
221100090911     C                   movel     wn14          oracor            6 0          *ORA
221200110923     C                   z-add     *zeros        datcor            8 0
221300110923     C                   eval      datcor = %dec(%date() : *ISO)
221400111118     C*
221500111118     C* Forzo un numero CMR
221600161209     C                   eval      wCMR = 'DPD BELGIUM - BOSE ' +
221700161209     C                                    %char(datcor)+%char(oracor)
221800000613     C*
221900000613     C                   ENDSR
222000000613     C***
