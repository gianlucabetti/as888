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
017900170731     C*
018000170731     C* Se indicato salvo il numero CMR del file corrente
018100170731     C                   if        %subst(vindta:1:6) = '#FILE;'
018200170731     C                   eval      wCMR = %trim(%subst(vindta:17))
018300170731     C                   endif
018400110923     C*
018500110923     C* Ignoro i record che iniziano x '#'
018600110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
018700110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
018800170731     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
018900111118     C                   z-add     0             wFlgVINDTA
019000111118     C                   else
019100110923     C*
019200110923     C* Setto i column headers "variabili"
019300111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
019400110923     C                   eval      header = %trim(%subst(vindta:13))
019500110923     C                   endif
019600111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
019700110923     C                   eval      parcel = %trim(%subst(vindta:13))
019800110923     C                   endif
019900111118     C                   endif
020000090420     C*
020100090420     C                   add       1             rrnum
020200010618     C*
020300040510     C                   if        vindta > *blanks AND
020400090420     C                             wFlgVINDTA = 1
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500010601     C*
021600090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
021700111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
021800111117     C***                eval      vindta  = '*' + %trim(vindta)
021900111117     C***                endif
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600030822     C                   move(P)   invfil        VABFGS
022700021025     C                   else
022800030822     C                   move(P)   vlrpoi        VABFGS
022900021025     C                   endif
023000090914     C*
023100090914     C                   move      VABFGS        currFGS           3 0
023200090914     C*
023300090914     C* Elaborazione record corrente
023400090914     C                   exsr      elabCurrRec
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      recko = vindta
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010604     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300030822     C  N70              enddo
025400090623     C*
025500090623     C* Scarico la testata rimasta "in canna"
025600140415     C                   if        not *in33                                    * esludo estero
025700090623     C  N31              exsr      wriVAB
025800140415     C                   endif
025900040119     C*
026000040119     C                   endif                                                  (endif - chkcall)
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200040119     C                   if        §ctrokvb > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900090911     C                   if        %open(edivabwr)
029000090911     C                   close     edivabwr
029100010330     C                   endif
029200090911     C                   if        %open(edivatwr)
029300090911     C                   close     edivatwr
029400040512     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000040119     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010330     C*
030500010330     C                   ENDSR
030600010330     C***
030700090623
030800090623
030900090623
031000090623     C*----------------------------------------------------*
031100090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
031200090623     C*----------------------------------------------------*
031300090623     C     INZWRK        BEGSR
031400090623     C*
031500090623     C                   eval      i = 1
031600090623     C                   eval      posDa = *zeros
031700090623     C                   eval      posA  = *zeros
031800090623     C*
031900090623     C                   ENDSR
032000090623
032100090623
032200090623
032300090623     C*----------------------------------------------------*
032400090623     C*  ELABORAZIONE RECORD CORRENTE
032500090623     C*----------------------------------------------------*
032600090623     C     ELABCURRREC   BEGSR
032700090623     C*
032800090623     C                   select
032900090623     C*
033000110923     C* Tipo record "TESTATA" - HEADER
033100111118     C                   when      %subst(vindta:1:6) = 'HEADER'
033200090623     C                   if        wNewBolla > *zeros
033300140415     C                   if        not *in33                                    * esludo estero
033400090623     C  N31              exsr      wriVAB
033500140415     C                   endif
033600090623     C                   exsr      inzvar
033700090623     C                   endif
033800090624     C                   eval      wNewBolla = 1
033900090623     C                   eval      wGiro = *zeros
034000111118     C                   clear                   SkSplitFLDH
034100110923     C                   eval      prefix = 'HDR_'
034200090624     C                   exsr      inzwrk
034300090623     C                   exsr      impvab
034400090623     C                   exsr      inzwrk
034500090623     C                   exsr      impvab
034600090623     C*
034700110923     C* Tipo record "DETTAGLIO" - PARCEL
034800110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
034900090623     C                   eval      wGiro = *zeros
035000111118     C                   clear                   SkSplitFLDP
035100110923     C                   eval      prefix = 'PCL_'
035200090623     C                   exsr      inzwrk
035300101005     C                   exsr      impvat
035400090623     C                   exsr      inzwrk
035500101005     C                   exsr      impvat
035600090624     C*
035700150203     C                   if        not *in33                                    * esludo estero
035800090624     C  N31              exsr      wrivat
035900150203     C                   endif
036000090623     C*
036100090623     C                   endsl
036200090623     C*
036300090623     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036900040119     C*----------------------------------------------------*
037000040510     C     WRIVAB        BEGSR
037100090623     C*
037200090623     C  N31              add       1             §CTROKVB
037300090623     C   32              add       1             §CTRMO
037400090623     C   31              add       1             §CTRNO
037500040119     C*
037600060331     C* Gestisco l'eventuale rottura x numero spedizione
037700070823     C                   if        VABNSP <> depspe
037800070301     C                   if        wVATNOT_E <> *blanks
037900080125     C                   if        VABCTM = *blanks
038000080125     C                   eval      VABCTM = '7Q'
038100080125     C                   endif
038200070301     C                   endif
038300090911     C*
038400090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038500090911     C                   eval      VABCMR = wCMR
038600090911     C                   eval      VABDCM = DATCOR
038700090911     C                   eval      VABDTS = DATCOR
038800090911     C                   eval      VABHMS = ORACOR
038900090911     C                   eval      VABCNT = 1
039000090911     C****
039100090911     C                   write     edivab00                                     => scarico il VAB
039200060331     C                   endif
039300090623     C*
039400090623     C  N31              eval      depspe = vabNSP
039500040119     C*
039600040119     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
040200040119     C*----------------------------------------------------*
040300040510     C     WRIVAT        BEGSR
040400060512     C*
040500060512     C* Inizializzo i campi chiave
040600090914     C                   eval      VATFGS = currFGS
040700060512     C                   eval      VATAAS = VABAAS
040800060512     C                   eval      VATCCM = VABCCM
040900070823     C                   eval      VATNRS = VABNRS
041000060512     C                   eval      VATNSP = VABNSP
041100060512     C                   eval      VATLNP = VABLNP
041200111117     C                   eval      VATCMR = wCMR
041300090914     C                   eval      VATCNT = 1
041400060331     C*
041500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041600060331     C                   if        wVATNOT_A <> *blanks
041700060331     C                   eval      VATNOT = wVATNOT_A
041800060331     C                   eval      VATTRC = 'A'
041900090911     C                   write     EDIVAT00
042000060331     C                   add       1             §CTROKVT
042100060331     C                   endif
042200060331     C*
042300060331     C                   if        wVATNOT_B <> *blanks
042400060331     C                   eval      VATNOT = wVATNOT_B
042500060331     C                   eval      VATTRC = 'B'
042600090911     C                   write     EDIVAT00
042700060331     C                   add       1             §CTROKVT
042800060331     C                   endif
042900111118     C*
043000111118     C                   if        wVATNOT_E <> *blanks
043100111118     C                   eval      VATNOT = wVATNOT_E
043200111118     C                   eval      VATTRC = 'E'
043300111118     C                   write     EDIVAT00
043400111118     C                   add       1             §CTROKVT
043500111118     C                   endif
043600141125     C*
043700141126 xxx C***                if        wVATNOT_S <> *blanks
043800141126     C***                eval      VATNOT = wVATNOT_S
043900141126     C***                eval      VATTRC = 'S'
044000141126     C***                write     EDIVAT00
044100141126     C***                add       1             §CTROKVT
044200141126     C***                endif
044300141125     C*
044400141125     C                   if        wVATNOT_IJ <> *blanks
044500141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
044600141125     C                   eval      VATTRC = 'I'
044700141125     C                   write     EDIVAT00
044800141125     C                   add       1             §CTROKVT
044900141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045000141125     C                   eval      VATTRC = 'J'
045100141125     C                   write     EDIVAT00
045200141125     C                   add       1             §CTROKVT
045300141125     C                   endif
045400090623     C*
045500111118 xxx C* Considero il VAT solo a rottura del "chi sono"
045600111118     C***                if        wVATNOT_E <> *blanks AND
045700111118     C***                          wVATNOT_E <> sVATNOT_E
045800111118     C***                eval      VATNOT = wVATNOT_E
045900111118     C***                eval      VATTRC = 'E'
046000111118     C***                write     EDIVAT00
046100111118     C***                add       1             §CTROKVT
046200111118     C***                eval      sVATNOT_E = wVATNOT_E
046300111118     C***                add       1             VABNCL
046400111118     C***                endif
046500040119     C*
046600040119     C                   ENDSR
046700040119
046800040119
046900040119
047000040119     C*----------------------------------------------------*
047100101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
047200040119     C*----------------------------------------------------*
047300101005     C     PREVAT        BEGSR
047400040119     C*
047500101005     C* Compongo il nome del membro da dare al EDIVATWR
047600040119     C                   eval      parmbr = vlrhdl
047700040119     C                   movel     'M'           parmbr
047800040510     C                   eval      parccm = vlrksc
047900040119     C                   eval      paropz = '1'
048000040119     C* Effettuo la chiamata al CLLE preposto
048100101005     C                   call(e)   'TITVEVTC'
048200040119     C                   parm                    parccm
048300040119     C                   parm                    parmbr
048400040119     C                   parm                    paropz
048500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
048600040119     C                   if        %error
048700040119     C                   movel     '1'           chkcall
048800040119     C                   else
048900040119     C                   movel     '0'           chkcall
049000040119     C                   endif
049100040119     C*
049200040119     C                   ENDSR
049300010601
049400010601
049500010601
049600010330     C*----------------------------------------------------*
049700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049800010330     C*----------------------------------------------------*
049900040119     C     INZVAR        BEGSR
050000010330     C*
050100030822     C* Inizializzo variabili di wrk
050200140415     C                   SETOFF                                       313233
050300090916     C                   Z-ADD     *zeros        Num5_0            5 0
050400090916     C                   MOVEL     *blanks       wNOTE            70
050500060331     C                   MOVEL     *blanks       wVATNOT_A
050600060331     C                   MOVEL     *blanks       wVATNOT_B
050700141125     C                   MOVEL     *blanks       wVATNOT_S
050800141125     C                   MOVEL     *blanks       wVATNOT_IJ
050900090904     C                   MOVEL     *blanks       wVATNOT_E
051000111117     C                   MOVEL     *blanks       wVATNOT_E_1
051100111117     C                   MOVEL     *blanks       wVATNOT_E_2
051200111118     C                   MOVEL     *blanks       wVATNOT_E_3
051300111118     C                   MOVEL     *blanks       wVATNOT_E_4
051400090623     C*
051500111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
051600090220     C*
051700080617     C                   EVAL      wFlgCAS = *blanks
051800030822     C*
051900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052000090911     C                   CLEAR                   EDIVAB00
052100090911     C                   CLEAR                   EDIVAT00
052200030822     C                   CLEAR                   SkSplitCSV
052300090623     C                   EXSR      inzwrk
052400030822     C*
052500030822     C* Reimposto i valori di default
052600030822     C                   EXSR      DEFCAM
052700030822     C*
052800010330     C                   ENDSR
052900010330     C*----------------------------------------------------*
053000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053100010330     C*----------------------------------------------------*
053200010330     C     DEFCAM        BEGSR
053300030822     C*
053400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
053500030715     C* e delimitatore testo.
053600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
053700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
053800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
053900030822     C*
054000150203     C* Se richiesta esclusione Estero
054100150203     C                   SETOFF                                       50
054200150203     C                   IF        %subst(vlrppt:1:1) = 'E'
054300150203     C                   SETON                                        50
054400030715     C                   ENDIF
054500090421     C*
054600090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
054700090421     C                   EVAL      VABCBO = '1'
054800090421     C                   EVAL      VABLNP = vlrpoi
054900090421     C                   EVAL      VABNSP = rrnum
055000030822     C*
055100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
055200030822     C                   EVAL      posDaDft = 1
055300030822     C                   EVAL      posADft  = 0
055400030822     C                   EVAL      wGiroDft = 0
055500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055600030822     C                             posDaDft > 0
055700030822     C*
055800030822     C* Gestisco il 1° giro
055900030822     C                   IF        wGiroDft = 0
056000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
056100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
056200030822     C* Incremento il contatore dei "giri"
056300030822     C                   EVAL      wGiroDft = 1
056400030822     C                   ELSE
056500030822     C                   EVAL      posDaDft = posADft
056600030822     C                   ENDIF
056700030822     C* Eseguo lo scan x trovare la fine del campo corrente
056800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056900030822     C*
057000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
057100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
057200030822     C                   IF        posDaDft > 0 AND
057300030822     C                             posADft  > 0
057400030822     C* NCL
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'NCL'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABNCL
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* CCM
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'CCM'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABCCM
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* LNP
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'LNP'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABLNP
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* NRS
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'NRS'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABNRS
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* CTR
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'CTR'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABCTR
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* PKB
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'PKB'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiNum=*on
064900030822     C                   Z-ADD     PiVal         VABPKB
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200140715     C* PKN
065300140715     C                   IF        %subst(
065400140715     C                             %subst(vlrppt:posDaDft+1:
065500140715     C                             posADft-posDaDft-1):1:3)
065600140715     C                             = 'PKN'
065700140715     C                   EVAL      PiStr=%trim(%subst(
065800140715     C                             %subst(vlrppt:posDaDft+1:
065900140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000140715     C                   EXSR      CHKNUM
066100140715     C                   IF        PiNum=*on
066200140715     C                   Z-ADD     PiVal         wVABPKN           7 1
066300140715     C                   ENDIF
066400140715     C                   ENDIF
066500030822     C* VLB
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VLB'
067000030822     C                   EVAL      PiStr=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   EXSR      CHKNUM
067400030822     C                   IF        PiNum=*on
067500030822     C                   Z-ADD     PiVal         VABVLB
067600030822     C                   ENDIF
067700030822     C                   ENDIF
067800030822     C* QFT
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'QFT'
068300030822     C                   EVAL      PiStr=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   EXSR      CHKNUM
068700030822     C                   IF        PiNum=*on
068800030822     C                   Z-ADD     PiVal         VABQFT
068900030822     C                   ENDIF
069000030822     C                   ENDIF
069100030822     C* CBO
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'CBO'
069600030822     C                   EVAL      VABCBO=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* TSP
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'TSP'
070500030822     C                   EVAL      VABTSP=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* VAS
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'VAS'
071400030822     C                   EVAL      VABVAS=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VCA'
072300030822     C                   EVAL      VABVCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700070925     C* TIC
072800070925     C                   IF        %subst(
072900070925     C                             %subst(vlrppt:posDaDft+1:
073000070925     C                             posADft-posDaDft-1):1:3)
073100070925     C                             = 'TIC'
073200070925     C                   EVAL      VABTIC=%trim(%subst(
073300070925     C                             %subst(vlrppt:posDaDft+1:
073400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500070925     C                   ENDIF
073600030822     C* GCA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GCA'
074100030822     C                   EVAL      VABGCA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* CTM
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'CTM'
075000030822     C                   EVAL      VABCTM=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* FFD
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'FFD'
075900030822     C                   EVAL      VABFFD=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* VAD
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'VAD'
076800030822     C                   EVAL      VABVAD=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GMA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GMA'
077700030822     C                   EVAL      VABGMA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* GGA
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'GGA'
078600030822     C                   EVAL      VABGGA=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* GVA
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'GVA'
079500030822     C                   EVAL      VABGVA=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900030822     C* TC1
080000030822     C                   IF        %subst(
080100030822     C                             %subst(vlrppt:posDaDft+1:
080200030822     C                             posADft-posDaDft-1):1:3)
080300030822     C                             = 'TC1'
080400030822     C                   EVAL      VABTC1=%trim(%subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700030822     C                   ENDIF
080800030822     C* TC2
080900030822     C                   IF        %subst(
081000030822     C                             %subst(vlrppt:posDaDft+1:
081100030822     C                             posADft-posDaDft-1):1:3)
081200030822     C                             = 'TC2'
081300030822     C                   EVAL      VABTC2=%trim(%subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081600030822     C                   ENDIF
081700040714     C* VATTRC
081800040714     C                   IF        %subst(
081900040714     C                             %subst(vlrppt:posDaDft+1:
082000040714     C                             posADft-posDaDft-1):1:3)
082100040714     C                             = 'TRC'
082200040714     C                   EVAL      VATTRC=%trim(%subst(
082300040714     C                             %subst(vlrppt:posDaDft+1:
082400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082500040714     C                   ENDIF
082600030822     C* ...
082700030822     C                   ENDIF
082800030822     C                   ENDDO
082900020204     C*
083000010330     C                   ENDSR
083100010607     C*----------------------------------------------------*
083200090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083300010607     C*----------------------------------------------------*
083400090623     C     IMPVAB        BEGSR
083500030515     C*
083600010607     C                   MOVEL     datcor        VABAAS
083700010607     C                   MOVE      datcor        VABMGS
083800010607     C*
083900030715     C**********
084000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084100030715     C**********
084200030715     C                   IF        wGiro = *zeros
084300080716     C*
084400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
084500080716     C                   EVAL      depvinDTA = vinDTA
084600040113     C*
084700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084800110923     C                   IF        header <> *blanks
084900110923     C                   EVAL      vindta = header
085000070823     C     ';':CharCSV   XLATE     vindta        vindta
085100040510     C                   ENDIF
085200080626     C*
085300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085400031201     C     minu:maiu     XLATE     vindta        vindta
085500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085600030715     C                   DOW       posDa <= %len(%trim(vindta))
085700030715     C*
085800030715     C* Gestisco il 1° campo
085900030715     C                   IF        i = 1
086000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086100030715     C                   EVAL      posDa = 1
086200030715     C* Eseguo lo scan x trovare la fine del primo campo
086300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086400030715     C* A questo punto "estraggo" il campo corrente
086500111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
086600110923     C                                         %subst(vindta:posDa:(posA-posDa))
086700030715     C* X i campi successivi al 1°
086800030715     C                   ELSE
086900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087000030715     C                   EVAL      posDa = posA + 1
087100030715     C* Eseguo lo scan x trovare la fine del campo corrente
087200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087300030715     C* Gestisco l'ultimo campo
087400030715     C                   IF        posA = *zeros
087500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087600030715     C* A questo punto "estraggo" il campo corrente
087700111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
087800110923     C                                         %subst(vindta:posDa:(posA-posDa))
087900030715     C                   LEAVE
088000030715     C                   ELSE
088100030715     C* A questo punto "estraggo" il campo corrente
088200111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
088300110923     C                                         %subst(vindta:posDa:(posA-posDa))
088400030715     C                   ENDIF
088500030715     C                   ENDIF
088600030715     C* Incremento il contatore d campo
088700030715     C                   EVAL      i = i +1
088800030715     C                   ENDDO
088900030715     C*
089000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089100030715     C* delimitazione testo
089200030715     C                   EVAL      i = 1
089300111118     C                   DOW       i < %elem(SkSplitFLDH)
089400111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
089500111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
089600030715     C                   EVAL      i = i + 1
089700030715     C                   ENDDO
089800030715     C                   EVAL      wGiro = 1
089900080716     C* Ripristino la 1° riga originale
090000080716     C                   EVAL      vinDTA = depvinDTA
090100030715     C                   ELSE
090200030715     C                   EVAL      wGiro = 2
090300031204     C**********
090400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090500031204     C* d separatore campo
090600031204     C**********
090700031204     C                   EVAL      vindta = %trim(vindta)
090800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
090900031204     C                   EVAL      vindta = %subst(vindta:2)
091000031204     C                   ENDDO
091100031204     C*
091200031204     C                   Z-ADD     *zeros        lunghInput        4 0
091300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091700031204     C                   ENDDO
091800030715     C**********
091900030715     C* Effettuo lo split del campo dati d input
092000030715     C**********
092100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092200030715     C                   DOW       posDa <= %len(%trim(vindta))
092300030715     C*
092400030715     C* Gestisco il 1° campo
092500030715     C                   IF        i = 1
092600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092700030715     C                   EVAL      posDa = 1
092800030715     C* Eseguo lo scan x trovare la fine del primo campo
092900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C* X i campi successivi al 1°
093400030715     C                   ELSE
093500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093600030715     C                   EVAL      posDa = posA + 1
093700031204     C* Verifico che nn vi sia il campo nn valorizzato
093800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
093900031204     C* Se campo nn valorizzato skippo al prossimo
094000031204     C                   EVAL      SkSplitCSV(i) = *blanks
094100031204     C                   EVAL      posA  = posA + 1
094200031204     C                   ELSE
094300030715     C* Eseguo lo scan x trovare la fine del campo corrente
094400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094500030715     C* Gestisco l'ultimo campo
094600030715     C                   IF        posA = *zeros
094700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094800030715     C* A questo punto "estraggo" il campo corrente
094900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095000030715     C                                                    (posA-posDa))
095100030715     C                   LEAVE
095200030715     C                   ELSE
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   ENDIF
095700031204     C                   ENDIF
095800030715     C                   ENDIF
095900030715     C* Incremento il contatore d campo
096000030715     C                   EVAL      i = i +1
096100030715     C                   ENDDO
096200030715     C*
096300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096400030715     C* delimitazione testo
096500030715     C                   EVAL      i = 1
096600030715     C                   DOW       i < %elem(SkSplitCSV)
096700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
096900030715     C                   EVAL      i = i + 1
097000030715     C                   ENDDO
097100030715     C*
097200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097300030715     C                   EVAL      i = 1
097400111118     C                   DOW       i < %elem(SkSplitFLDH)
097500030715     C*
097600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
097700111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
097800090623     C                             SkSplitCSV(i) <> *blanks
097900090623     C*
098000111118     C                   exsr      CARVALH
098100040510     C*
098200030715     C                   ENDIF
098300030715     C                   EVAL      i = i + 1
098400030715     C                   ENDDO
098500070925     C*
098600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
098700080125     C*
098800111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
098900111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
099000140715     C*
099100140715     C                   IF        VABPKB = *zeros
099200140715     C                   EVAL      VABPKB = wVABPKN * VABNCL
099300140715     C                   ENDIF
099400070925     C*
099500071029     C***  <===  -----------------------------------------
099600070925     C*
099700070925     C* Considerazioni finali su CBO/CAS
099800080125     C                   IF        wFlgCAS = '1'
099900070925     C                   IF        VABCBO = '1'
100000070925     C                   EVAL      VABCBO = '4'
100100070925     C                   ENDIF
100200070925     C                   IF        VABCBO = '2'
100300070925     C                   EVAL      VABCBO = '6'
100400070925     C                   ENDIF
100500070925     C                   ENDIF
100600070925     C***  <===  ----------------------------
100700040510     C*
100800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
100900040510     C                   IF        VABNSP = *zeros
101000040510     C                   SETON                                        31
101100040510     C                   ENDIF
101200040715     C*
101300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
101400040715     C                   IF        VABLNP = *zeros
101500040715     C                   SETON                                        31
101600040715     C                   ENDIF
101700040510     C*
101800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
101900040510     C                   IF        VABRMN = *zeros
102000040510     C                   Z-ADD     VABNSP        VABRMN
102100040510     C                   ENDIF
102200040510     C*
102300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
102400040510     C                   IF        VABRMA = *blanks
102500040714     C                   MOVEL     VABRMN        VABRMA
102600040510     C                   ENDIF
102700040128     C*
102800040510     C* Se provincia nn valorizzata la reperisco
102900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
103000040510     C                   IF        VABPRD = *blanks
103100040128     C                   CLEAR                   TISI95DS
103200040128     C                   EVAL      I95TCN = '3'
103300040128     C                   Z-ADD     datcor        I95DAT
103400040128     C                   EVAL      I95NAR = VABNZD
103500040128     C                   EVAL      I95CAP = VABCAD
103600040128     C                   EVAL      I95LOC = VABLOD
103700040128     C                   CALL      'TISI95R'
103800040128     C                   PARM                    TISI95DS
103900040128     C                   EVAL      VABPRD = O95PRV
104000040510     C                   ENDIF
104100020204     C*
104200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
104300020204     C                   EXSR      CHKIMPDIV
104400030715     C                   ENDIF
104500020204     C*
104600010607     C                   ENDSR
104700010607     C*----------------------------------------------------*
104800090623
104900090623
105000090623
105100090623     C*----------------------------------------------------*
105200101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
105300090623     C*----------------------------------------------------*
105400101005     C     IMPVAT        BEGSR
105500090623     C*
105600090623     C**********
105700090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
105800090623     C**********
105900090623     C                   IF        wGiro = *zeros
106000090623     C*
106100090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
106200090623     C                   EVAL      depvinDTA = vinDTA
106300090623     C*
106400090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
106500110923     C                   IF        parcel <> *blanks
106600110923     C                   EVAL      vindta = parcel
106700090623     C     ';':CharCSV   XLATE     vindta        vindta
106800090623     C                   ENDIF
106900090623     C*
107000090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
107100090623     C     minu:maiu     XLATE     vindta        vindta
107200090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
107300090623     C                   DOW       posDa <= %len(%trim(vindta))
107400090623     C*
107500090623     C* Gestisco il 1° campo
107600090623     C                   IF        i = 1
107700090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
107800090623     C                   EVAL      posDa = 1
107900090623     C* Eseguo lo scan x trovare la fine del primo campo
108000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
108100090623     C* A questo punto "estraggo" il campo corrente
108200111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
108300110923     C                                         %subst(vindta:posDa:(posA-posDa))
108400090623     C* X i campi successivi al 1°
108500090623     C                   ELSE
108600090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
108700090623     C                   EVAL      posDa = posA + 1
108800090623     C* Eseguo lo scan x trovare la fine del campo corrente
108900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
109000090623     C* Gestisco l'ultimo campo
109100090623     C                   IF        posA = *zeros
109200090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
109300090623     C* A questo punto "estraggo" il campo corrente
109400111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
109500110923     C                                         %subst(vindta:posDa:(posA-posDa))
109600090623     C                   LEAVE
109700090623     C                   ELSE
109800090623     C* A questo punto "estraggo" il campo corrente
109900111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
110000110923     C                                         %subst(vindta:posDa:(posA-posDa))
110100090623     C                   ENDIF
110200090623     C                   ENDIF
110300090623     C* Incremento il contatore d campo
110400090623     C                   EVAL      i = i +1
110500090623     C                   ENDDO
110600090623     C*
110700090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
110800090623     C* delimitazione testo
110900090623     C                   EVAL      i = 1
111000111118     C                   DOW       i < %elem(SkSplitFLDP)
111100111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
111200111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
111300090623     C                   EVAL      i = i + 1
111400090623     C                   ENDDO
111500090623     C                   EVAL      wGiro = 1
111600090623     C* Ripristino la 1° riga originale
111700090623     C                   EVAL      vinDTA = depvinDTA
111800090623     C                   ELSE
111900090623     C                   EVAL      wGiro = 2
112000090623     C**********
112100090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
112200090623     C* d separatore campo
112300090623     C**********
112400090623     C                   EVAL      vindta = %trim(vindta)
112500090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
112600090623     C                   EVAL      vindta = %subst(vindta:2)
112700090623     C                   ENDDO
112800090623     C*
112900090623     C                   Z-ADD     *zeros        lunghInput        4 0
113000090623     C                   EVAL      lunghInput = %len(%trim(vindta))
113100090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
113200090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
113300090623     C                   EVAL      lunghInput = %len(%trim(vindta))
113400090623     C                   ENDDO
113500090623     C**********
113600090623     C* Effettuo lo split del campo dati d input
113700090623     C**********
113800090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
113900090623     C                   DOW       posDa <= %len(%trim(vindta))
114000090623     C*
114100090623     C* Gestisco il 1° campo
114200090623     C                   IF        i = 1
114300090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
114400090623     C                   EVAL      posDa = 1
114500090623     C* Eseguo lo scan x trovare la fine del primo campo
114600090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
114700090623     C* A questo punto "estraggo" il campo corrente
114800090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
114900090623     C                                                    (posA-posDa))
115000090623     C* X i campi successivi al 1°
115100090623     C                   ELSE
115200090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
115300090623     C                   EVAL      posDa = posA + 1
115400090623     C* Verifico che nn vi sia il campo nn valorizzato
115500090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
115600090623     C* Se campo nn valorizzato skippo al prossimo
115700090623     C                   EVAL      SkSplitCSV(i) = *blanks
115800090623     C                   EVAL      posA  = posA + 1
115900090623     C                   ELSE
116000090623     C* Eseguo lo scan x trovare la fine del campo corrente
116100090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
116200090623     C* Gestisco l'ultimo campo
116300090623     C                   IF        posA = *zeros
116400090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
116500090623     C* A questo punto "estraggo" il campo corrente
116600090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116700090623     C                                                    (posA-posDa))
116800090623     C                   LEAVE
116900090623     C                   ELSE
117000090623     C* A questo punto "estraggo" il campo corrente
117100090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
117200090623     C                                                    (posA-posDa))
117300090623     C                   ENDIF
117400090623     C                   ENDIF
117500090623     C                   ENDIF
117600090623     C* Incremento il contatore d campo
117700090623     C                   EVAL      i = i +1
117800090623     C                   ENDDO
117900090623     C*
118000090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
118100090623     C* delimitazione testo
118200090623     C                   EVAL      i = 1
118300090623     C                   DOW       i < %elem(SkSplitCSV)
118400090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
118500090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
118600090623     C                   EVAL      i = i + 1
118700090623     C                   ENDDO
118800090623     C*
118900090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
119000090623     C                   EVAL      i = 1
119100111118     C                   DOW       i < %elem(SkSplitFLDP)
119200090623     C*
119300090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
119400111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
119500090623     C                             SkSplitCSV(i) <> *blanks
119600090623     C*
119700111118     C                   exsr      CARVALP
119800090623     C*
119900090623     C                   ENDIF
120000090623     C                   EVAL      i = i + 1
120100090623     C                   ENDDO
120200090623     C*
120300090623     C***  ===>  Gestione campi "particolari" fuori ciclo
120400111118     C*
120500111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
120600111118     C                                         %trim(wVATNOT_E_2) +
120700111118     C                                         %trim(wVATNOT_E_3) +
120800111118     C                                         %trim(wVATNOT_E_4)
120900090623     C*
121000090623     C***  <===  -----------------------------------------
121100090623     C*
121200090623     C                   ENDIF
121300090623     C*
121400090623     C                   ENDSR
121500090623     C*----------------------------------------------------*
121600090623
121700090623
121800090623     C*----------------------------------------------------*
121900090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
122000090623     C*----------------------------------------------------*
122100111118     C     CARVALH       BEGSR
122200090623     C*
122300090623     C*** ATB
122400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
122500090623     C                   EVAL      VABATB = SkSplitCSV(i)
122600090623     C                   ENDIF
122700090623     C*** RSD
122800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
122900090623     C                   EVAL      VABRSD = SkSplitCSV(i)
123000090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
123100090623     C     '@':'A'       XLATE     VABRSD        VABRSD
123200090623     C* ==
123300090623     C                   ENDIF
123400090623     C*** RD2
123500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
123600090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
123700090623     C                   ENDIF
123800130530     C*** IND 1 (HDR)
123900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
124000090623     C                   EVAL      VABIND = SkSplitCSV(i)
124100090623     C                   ENDIF
124200130530     C*** IND 2 (HDR)
124300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
124400111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
124500111117     C                   ENDIF
124600130530     C*** IND 1
124700130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
124800130530     C                   EVAL      VABIND = SkSplitCSV(i)
124900130530     C                   ENDIF
125000130530     C*** IND 2
125100130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
125200130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
125300130530     C                   ENDIF
125400111117     C*** CAD
125500111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
125600111117     C                   EVAL      VABCAD = SkSplitCSV(i)
125700111216     C                   MOVEL     *blanks       wAlfa7            7
125800111216     C                   MOVEL     *zeros        wAlfa7z           7
125900111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
126000111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
126100111117     C                   ENDIF
126200090623     C*** LOD
126300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
126400090623     C                   EVAL      VABLOD = SkSplitCSV(i)
126500090623     C                   ENDIF
126600111117     C*** PRD
126700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
126800090623     C                   EVAL      VABPRD = SkSplitCSV(i)
126900090623     C                   ENDIF
127000090623     C*** NZD
127100140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
127200140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
127300150203     C   50              SETON                                        33
127400090623     C                   ENDIF
127500140415     C                   ENDIF
127600090623     C*** RMA
127700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
127800130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
127900130611     C                   EVAL      VABRMA = SkSplitCSV(i)
128000090623     C                   ENDIF
128100111117     C*** NOT
128200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
128300111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
128400090623     C                   ENDIF
128500111117     C*** NT2
128600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
128700111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
128800090623     C                   ENDIF
128900090623     C*** VCA
129000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
129100090623     C                   EVAL      VABVCA = SkSplitCSV(i)
129200090623     C                   ENDIF
129300090623     C*** VAS
129400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
129500090623     C                   EVAL      VABVAS = SkSplitCSV(i)
129600090623     C                   ENDIF
129700090623     C*** FFD
129800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
129900090623     C                   EVAL      VABFFD = SkSplitCSV(i)
130000090623     C                   ENDIF
130100090623     C*** GC1
130200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
130300090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
130400090623     C                   ENDIF
130500090623     C*** GC2
130600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
130700090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
130800090623     C                   ENDIF
130900090623     C*** TSP
131000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
131100090623     C                   EVAL      VABTSP = SkSplitCSV(i)
131200090623     C                   ENDIF
131300090623     C*** CBO
131400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
131500090623     C                   EVAL      VABCBO = SkSplitCSV(i)
131600090623     C                   ENDIF
131700090623     C*** NAS
131800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
131900090623     C                   EVAL      VABNAS = SkSplitCSV(i)
132000090623     C                   ENDIF
132100090623     C*** TIC
132200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
132300090623     C                   EVAL      VABTIC = SkSplitCSV(i)
132400090623     C                   ENDIF
132500090623     C*** GCA
132600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
132700090623     C                   EVAL      VABGCA = SkSplitCSV(i)
132800090623     C                   ENDIF
132900090623     C*** XCO
133000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
133100090623     C                   EVAL      VABXCO = SkSplitCSV(i)
133200090623     C                   ENDIF
133300090623     C*** CTM
133400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
133500090623     C                   EVAL      VABCTM = SkSplitCSV(i)
133600090623     C                   ENDIF
133700090623     C*** TCR
133800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
133900090623     C                   EVAL      VABTCR = SkSplitCSV(i)
134000090623     C                   ENDIF
134100090623     C*** CTS
134200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
134300090623     C                   EVAL      VABCTS = SkSplitCSV(i)
134400090623     C                   ENDIF
134500090623     C*** FTM
134600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
134700090623     C                   EVAL      VABFTM = SkSplitCSV(i)
134800090623     C                   ENDIF
134900090623     C*** VAD
135000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
135100090623     C                   EVAL      VABVAD = SkSplitCSV(i)
135200090623     C                   ENDIF
135300090623     C*** GMA
135400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
135500090623     C                   EVAL      VABGMA = SkSplitCSV(i)
135600090623     C                   ENDIF
135700090623     C*** GGA
135800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
135900090623     C                   EVAL      VABGGA = SkSplitCSV(i)
136000090623     C                   ENDIF
136100090623     C*** GVA
136200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
136300090623     C                   EVAL      VABGVA = SkSplitCSV(i)
136400090623     C                   ENDIF
136500090623     C*** TC1
136600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
136700090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
136800090623     C                   ENDIF
136900090623     C*** TC2
137000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
137100090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
137200090623     C                   ENDIF
137300090623     C*** SCL
137400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
137500090623     C                   EVAL      VABSCL = SkSplitCSV(i)
137600090623     C                   ENDIF
137700090623     C*** RMO
137800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
137900090623     C                   EVAL      VABRMO = SkSplitCSV(i)
138000090623     C                   ENDIF
138100090623     C*** NMO
138200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
138300090623     C                   EVAL      VABNMO = SkSplitCSV(i)
138400090623     C                   ENDIF
138500090623     C*
138600090623     C* Reperisco quindi i campi numerici...
138700111117     C*** NSP
138800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
138900111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
139000090623     C                   EXSR      CHKNUM
139100090623     C                   IF        PiInt=*on
139200090623     C                   Z-ADD     PiVal         VABNSP
139300090623     C                   ELSE
139400090623     C                   SETON                                        31
139500090623     C                   EVAL      VABNSP = *zeros
139600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
139700090623     C                             + ' ' + 'VABNSP'
139800090623     C                   ENDIF
139900090623     C                   ENDIF
140000090623     C*** RMN
140100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
140200111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
140300090623     C                   EXSR      CHKNUM
140400090623     C                   IF        PiInt=*on
140500090623     C                   Z-ADD     PiVal         VABRMN
140600090623     C                   ELSE
140700090623     C                   SETON                                        32
140800090623     C                   EVAL      VABRMN = *zeros
140900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
141000090623     C                             + ' ' + 'VABRMN'
141100090623     C                   ENDIF
141200090623     C                   ENDIF
141300090623     C*** CAD
141400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
141500090623     C                   EVAL      PiStr=SkSplitCSV(i)
141600090623     C                   EXSR      CHKNUM
141700090623     C                   IF        PiInt=*on
141800090623     C                   Z-ADD     PiVal         Num5_0
141900090623     C                   MOVEL(p)  Num5_0        VABCAD
142000090623     C                   ELSE
142100090623     C                   EVAL      VABCAD = SkSplitCSV(i)
142200090623     C                   SETON                                        32
142300090623     C                   EVAL      VABCAD = *zeros
142400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142500090623     C                             + ' ' + 'VABCAD'
142600090623     C                   ENDIF
142700090623     C                   ENDIF
142800090623     C*** NCL
142900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
143000090623     C                   EVAL      PiStr=SkSplitCSV(i)
143100090623     C                   EXSR      CHKNUM
143200090623     C                   IF        PiInt=*on
143300090623     C                   Z-ADD     PiVal         VABNCL
143400090623     C                   ELSE
143500090623     C                   SETON                                        32
143600090623     C                   EVAL      VABNCL = *zeros
143700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143800090623     C                             + ' ' + 'VABNCL'
143900090623     C                   ENDIF
144000090623     C                   ENDIF
144100090623     C*** PKB
144200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
144300090623     C                   EVAL      PiStr=SkSplitCSV(i)
144400090623     C                   EXSR      CHKNUM
144500090623     C                   IF        PiNum=*on
144600130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
144700090623     C                   Z-ADD     PiVal         VABPKB
144800090623     C                   ELSE
144900090623     C                   SETON                                        32
145000090623     C                   EVAL      VABPKB = *zeros
145100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145200090623     C                             + ' ' + 'VABPKB'
145300090623     C                   ENDIF
145400090623     C                   ENDIF
145500090623     C*** CAS
145600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
145700090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
145800090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
145900090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
146000090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
146100090623     C                   EVAL      wFlgCAS = '1'
146200090623     C                   ENDIF
146300090623     C                   EVAL      PiStr=SkSplitCSV(i)
146400090623     C                   EXSR      CHKNUM
146500090623     C                   IF        PiNum=*on
146600090623     C                   Z-ADD     PiVal         VABCAS
146700090623     C                   ELSE
146800090623     C                   SETON                                        32
146900090623     C                   EVAL      VABCAS = *zeros
147000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147100090623     C                             + ' ' + 'VABCAS'
147200090623     C                   ENDIF
147300090623     C                   ENDIF
147400090623     C*** CCM
147500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
147600090623     C                   EVAL      PiStr=SkSplitCSV(i)
147700090623     C                   EXSR      CHKNUM
147800090623     C                   IF        PiInt=*on      AND
147900090623     C                             PiVal<=9999999 AND
148000090623     C                             PiVal>*zeros
148100090623     C                   Z-ADD     PiVal         VABCCM
148200090623     C                   ELSE
148300090623     C                   SETON                                        32
148400090623     C                   EVAL      VABCCM = *zeros
148500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148600090623     C                             + ' ' + 'VABCCM'
148700090623     C                   ENDIF
148800090623     C                   ENDIF
148900090623     C*** LNP
149000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
149100090623     C                   EVAL      PiStr=SkSplitCSV(i)
149200090623     C                   EXSR      CHKNUM
149300090623     C                   IF        PiInt=*on    AND
149400090623     C                             PiVal<=999   AND
149500090623     C                             PiVal>*zeros
149600090623     C                   Z-ADD     PiVal         VABLNP
149700090623     C                   ELSE
149800090623     C                   SETON                                        32
149900090623     C                   EVAL      VABLNP = *zeros
150000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150100090623     C                             + ' ' + 'VABLNP'
150200090623     C                   ENDIF
150300090623     C                   ENDIF
150400090623     C*** NRS
150500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
150600090623     C                   EVAL      PiStr=SkSplitCSV(i)
150700090623     C                   EXSR      CHKNUM
150800090623     C                   IF        PiInt=*on AND
150900090623     C                             PiVal<=99
151000090623     C                   Z-ADD     PiVal         VABNRS
151100090623     C                   ELSE
151200090623     C                   SETON                                        32
151300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151400090623     C                             + ' ' + 'VABNRS'
151500090623     C                   ENDIF
151600090623     C                   ENDIF
151700090623     C*** LNA
151800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
151900090623     C                   EVAL      PiStr=SkSplitCSV(i)
152000090623     C                   EXSR      CHKNUM
152100090623     C                   IF        PiInt=*on    AND
152200090623     C                             PiVal<=999   AND
152300090623     C                             PiVal>*zeros
152400090623     C                   Z-ADD     PiVal         VABLNA
152500090623     C                   ELSE
152600090623     C                   SETON                                        32
152700090623     C                   EVAL      VABLNA = *zeros
152800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152900090623     C                             + ' ' + 'VABLNA'
153000090623     C                   ENDIF
153100090623     C                   ENDIF
153200090623     C*** CTR
153300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
153400090623     C                   EVAL      PiStr=SkSplitCSV(i)
153500090623     C                   EXSR      CHKNUM
153600090623     C                   IF        PiInt=*on  AND
153700090623     C                             PiVal<=999
153800090623     C                   Z-ADD     PiVal         VABCTR
153900090623     C                   ELSE
154000090623     C                   SETON                                        32
154100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154200090623     C                             + ' ' + 'VABCTR'
154300090623     C                   ENDIF
154400090623     C                   ENDIF
154500090623     C*** IAS
154600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
154700090623     C                   EVAL      PiStr=SkSplitCSV(i)
154800090623     C                   EXSR      CHKNUM
154900090623     C                   IF        PiNum=*on
155000090623     C                   Z-ADD     PiVal         VABIAS
155100090623     C                   ELSE
155200090623     C                   SETON                                        32
155300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155400090623     C                             + ' ' + 'VABIAS'
155500090623     C                   ENDIF
155600090623     C                   ENDIF
155700090623     C*** VLB
155800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
155900090623     C                   EVAL      PiStr=SkSplitCSV(i)
156000090623     C                   EXSR      CHKNUM
156100090623     C                   IF        PiNum=*on
156200111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
156300090623     C                   Z-ADD     PiVal         VABVLB
156400090623     C                   ELSE
156500090623     C                   SETON                                        32
156600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156700090623     C                             + ' ' + 'VABVLB'
156800090623     C                   ENDIF
156900090623     C                   ENDIF
157000090623     C*** QFT
157100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
157200090623     C                   EVAL      PiStr=SkSplitCSV(i)
157300090623     C                   EXSR      CHKNUM
157400090623     C                   IF        PiNum=*on
157500090623     C                   Z-ADD     PiVal         VABQFT
157600090623     C                   ELSE
157700090623     C                   SETON                                        32
157800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157900090623     C                             + ' ' + 'VABQFT'
158000090623     C                   ENDIF
158100090623     C                   ENDIF
158200090623     C*** NCD
158300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
158400090623     C                   EVAL      PiStr=SkSplitCSV(i)
158500090623     C                   EXSR      CHKNUM
158600090623     C                   IF        PiInt=*on      AND
158700090623     C                             PiVal<=9999999
158800090623     C                   Z-ADD     PiVal         VABNCD
158900090623     C                   ELSE
159000090623     C                   SETON                                        32
159100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159200090623     C                             + ' ' + 'VABNCD'
159300090623     C                   ENDIF
159400090623     C                   ENDIF
159500090623     C*** NCA
159600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
159700090623     C                   EVAL      PiStr=SkSplitCSV(i)
159800090623     C                   EXSR      CHKNUM
159900090623     C                   IF        PiInt=*on      AND
160000090623     C                             PiVal<=9999999
160100090623     C                   Z-ADD     PiVal         VABNCA
160200090623     C                   ELSE
160300090623     C                   SETON                                        32
160400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160500090623     C                             + ' ' + 'VABNCA'
160600090623     C                   ENDIF
160700090623     C                   ENDIF
160800090623     C*** ZNC
160900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
161000090623     C                   EVAL      PiStr=SkSplitCSV(i)
161100090623     C                   EXSR      CHKNUM
161200090623     C                   IF        PiInt=*on AND
161300090623     C                             PiVal<=99
161400090623     C                   Z-ADD     PiVal         VABZNC
161500090623     C                   ELSE
161600090623     C                   SETON                                        32
161700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161800090623     C                             + ' ' + 'VABZNC'
161900090623     C                   ENDIF
162000090623     C                   ENDIF
162100090623     C*** DCR
162200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
162300090623     C                   EVAL      PiStr=SkSplitCSV(i)
162400090623     C                   EXSR      CHKNUM
162500090623     C                   IF        PiInt=*on       AND
162600090623     C                             PiVal<=99999999
162700090623     C                   Z-ADD     PiVal         VABDCR
162800090623     C                   ELSE
162900090623     C                   SETON                                        32
163000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163100090623     C                             + ' ' + 'VABDCR'
163200090623     C                   ENDIF
163300090623     C                   ENDIF
163400090623     C*** HCR
163500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
163600090623     C                   EVAL      PiStr=SkSplitCSV(i)
163700090623     C                   EXSR      CHKNUM
163800090623     C                   IF        PiInt=*on   AND
163900090623     C                             PiVal<=9999
164000090623     C                   Z-ADD     PiVal         VABHCR
164100090623     C                   ELSE
164200090623     C                   SETON                                        32
164300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164400090623     C                             + ' ' + 'VABHCR'
164500090623     C                   ENDIF
164600090623     C                   ENDIF
164700090623     C*** VMD
164800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
164900090623     C                   EVAL      PiStr=SkSplitCSV(i)
165000090623     C                   EXSR      CHKNUM
165100090623     C                   IF        PiNum=*on
165200090623     C                   Z-ADD     PiVal         VABVMD
165300090623     C                   ELSE
165400090623     C                   SETON                                        32
165500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165600090623     C                             + ' ' + 'VABVMD'
165700090623     C                   ENDIF
165800090623     C                   ENDIF
165900090623     C*** ANT
166000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
166100090623     C                   EVAL      PiStr=SkSplitCSV(i)
166200090623     C                   EXSR      CHKNUM
166300090623     C                   IF        PiInt=*on        AND
166400090623     C                             PiVal<=999999999
166500090623     C                   Z-ADD     PiVal         VABANT
166600090623     C                   ELSE
166700090623     C                   SETON                                        32
166800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166900090623     C                             + ' ' + 'VABANT'
167000090623     C                   ENDIF
167100090623     C                   ENDIF
167200090623     C*** CMO
167300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
167400090623     C                   EVAL      PiStr=SkSplitCSV(i)
167500090623     C                   EXSR      CHKNUM
167600090623     C                   IF        PiInt=*on
167700090623     C                   Z-ADD     PiVal         Num5_0
167800090623     C                   MOVEL(p)  Num5_0        VABCMO
167900090623     C                   ELSE
168000090623     C                   SETON                                        32
168100090623     C                   EVAL      VABCMO = *zeros
168200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168300090623     C                             + ' ' + 'VABCMO'
168400090623     C                   ENDIF
168500090623     C                   ENDIF
168600090623     C*
168700090911     C* Reperisco i campi del EDIVAT
168800090623     C*** VATNOT
168900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
169000090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
169100090623     C                   ENDIF
169200090623     C*** VATTRC
169300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
169400090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
169500090623     C                   ENDIF
169600090623     C*** VATNOT_A
169700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
169800090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
169900090623     C                   ENDIF
170000090623     C*** VATNOT_B
170100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
170200090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
170300090623     C                   ENDIF
170400141125     C*** VATNOT_S
170500141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
170600141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
170700141125     C                   ENDIF
170800141125     C*** VATNOT_IJ
170900141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
171000141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
171100141125     C                   ENDIF
171200111118     C*** VATNOT_E_4
171300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
171400111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
171500111118     C                   ENDIF
171600090623     C*
171700090623     C***  ===>  Gestione campi "particolari" dentro ciclo
171800090623     C*
171900090623     C*
172000090623     C***  <===  -----------------------------------------
172100090908     C*
172200090623     C*
172300090623     C                   ENDSR
172400090623     C***
172500090623
172600111118
172700111118
172800111118     C*----------------------------------------------------*
172900111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
173000111118     C*----------------------------------------------------*
173100111118     C     CARVALP       BEGSR
173200111118     C*
173300111118     C* Reperisco i campi del EDIVAT
173400111118     C*** VATNOT
173500111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
173600111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
173700111118     C                   ENDIF
173800111118     C*** VATTRC
173900111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
174000111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
174100111118     C                   ENDIF
174200111118     C*** VATNOT_A
174300111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
174400111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
174500111118     C                   ENDIF
174600111118     C*** VATNOT_B
174700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
174800111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
174900111118     C                   ENDIF
175000141125     C*** VATNOT_S
175100141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
175200141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
175300141125     C                   ENDIF
175400141125     C*** VATNOT_IJ
175500141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
175600141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
175700141125     C                   ENDIF
175800111216     C*** VATNOT_E_1
175900111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
176000111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
176100111216     C***                ENDIF
176200111118     C*** VATNOT_E_2
176300111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
176400111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
176500111118     C                   ENDIF
176600111118     C*** VATNOT_E_3
176700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
176800111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
176900111118     C                   ENDIF
177000111118     C*** VATNOT_E_4
177100111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
177200111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
177300111118     C                   ENDIF
177400111118     C*
177500111118     C***  ===>  Gestione campi "particolari" dentro ciclo
177600111118     C*
177700111118     C*
177800111118     C***  <===  -----------------------------------------
177900111118     C*
178000111118     C*
178100111118     C                   ENDSR
178200111118     C***
178300010601
178400020204
178500020204     C*----------------------------------------------------*
178600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
178700020204     C*----------------------------------------------------*
178800020204     C     CHKIMPDIV     BEGSR
178900020204     C*
179000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
179100020204     C                   Z-ADD     *zeros        wrkDec            9 9
179200020204     C*
179300020204     C* Come prima cosa effettuo considerazioni sulla divisa
179400020204     C                   IF        vabIAS > *zeros
179500020204     C                   IF        vabVAS <> 'EUR'
179600020204     C                   EVAL      vabVAS =  'ITL'
179700020204     C                   ENDIF
179800020204     C                   ENDIF
179900020204     C*
180000020204     C                   IF        vabCAS > *zeros
180100020204     C                   IF        vabVCA <> 'EUR'
180200020204     C                   EVAL      vabVCA =  'ITL'
180300020204     C                   ENDIF
180400020204     C                   ENDIF
180500020204     C*
180600020204     C                   IF        vabVMD > *zeros
180700020204     C                   IF        vabVAD <> 'EUR'
180800020204     C                   EVAL      vabVAD =  'ITL'
180900020204     C                   ENDIF
181000020204     C                   ENDIF
181100020204     C*
181200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
181300020204     C                   Z-ADD     vabIAS        wrkDec
181400020204     C                   IF        wrkDec > *zeros
181500020204     C                   IF        vabVAS = 'ITL'
181600020204     C                   EVAL      vabIAS = *zeros
181700020204     C                   ENDIF
181800020204     C                   ENDIF
181900020204     C*
182000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
182100020204     C                   Z-ADD     vabCAS        wrkDec
182200020204     C                   IF        wrkDec > *zeros
182300020204     C                   IF        vabVCA = 'ITL'
182400020204     C                   EVAL      vabCAS = *zeros
182500020204     C                   ENDIF
182600020204     C                   ENDIF
182700020204     C*
182800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
182900020204     C                   Z-ADD     vabVMD        wrkDec
183000020204     C                   IF        wrkDec > *zeros
183100020204     C                   IF        vabVAD = 'ITL'
183200020204     C                   EVAL      vabVMD = *zeros
183300020204     C                   ENDIF
183400020204     C                   ENDIF
183500020204     C*
183600020204     C                   ENDSR
183700020204     C***
183800080617
183900080617
184000080617
184100080617     C*----------------------------------------------------*
184200080617     C*  CONTROLLO NUMERICITA' CAMPI
184300080617     C*----------------------------------------------------*
184400080617     C     CHKNUM        BEGSR
184500080617     C*
184600080617     C                   IF        PiDecChr = *blanks
184700080617     C                   EVAL      PiDecChr = CharNUM
184800080617     C                   ENDIF
184900080617     C*
185000080617     C                   callp     UBISNUM_Check(PiStr
185100080617     C                                          :PiDecChr
185200080617     C                                          :PiVal
185300080617     C                                          :PiNum
185400080617     C                                          :PiInt)
185500080617     C*
185600080617     C                   ENDSR
185700080617     C***
185800010330
185900010601
186000010601
186100010601
186200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
186300010601     C     repfil        BEGSR
186400010601     C*
186500010601     C                   if        invfil = *zeros and
186600010601     C                             depfil > *zeros and
186700010629     C                             (vinflg = *blanks or
186800010629     C                              vinflg = *zeros)
186900010601     C
187000010601     C                   eval      invfil = depfil
187100010601     C                   endif
187200010601     C*
187300010601     C                   if        depfil <> invfil and
187400010601     C                             invfil > *zeros
187500010601     C                   eval      flgMulti = '1'
187600010601     C                   if        vinflg = *blanks
187700010601     C                   add       1             cntNonEl
187800010601     C                   endif
187900010601     C                   endif
188000010601     C*
188100010601     C                   if        vinflg = '2'
188200010601     C                   eval      flgStato = '2'
188300010601     C                   endif
188400010601     C*
188500010601     C                   ENDSR
188600010601     C***
188700010601
188800010601
188900010601
189000010330
189100040119      /TITLE Invio dei dati al punto operativo.
189200040119     C     invio         BEGSR
189300040119     C*
189400101005     C* 1° invio EDIVAT
189500040119     C                   reset                   dscmz
189600040119     C                   move      vlrpoi        cmzdst
189700090911     C                   eval      cmzfld = 'EDIVATWR'
189800040119     C                   eval      cmzmbd = vlrhdl
189900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
190000040119     C***                if        prmfir = *blanks
190100090911     C                   eval      cmzfla = 'EDIVAT0F'
190200090911     C                   eval      cmzmba = 'EDIVAT0F'
190300040119     C***                else
190400040119     C***                eval      cmzfla = prmfir
190500040119     C***                eval      cmzmba = prmfir
190600040119     C***                endif
190700040119     C                   eval      cmznrr = *zeros
190800040119     C                   move      §ctrokvt      cmznrr
190900040119     C                   eval      cmzlba = vlrfl1
191000040119     C                   call(e)   'TIS711C'
191100040119     C                   parm                    dscmz
191200040119     C                   parm      *blanks       esito
191300040119     C                   if        %error
191400040119     C                             or cmzerr = '1'
191500040119     C                             or esito  = '1'
191600040119     C                   eval      wrkesito = '3'
191700040119     C                   else
191800040119     C*
191900101005     C* 2° invio EDIVAB
192000040119     C                   reset                   dscmz
192100040119     C                   move      vlrpoi        cmzdst
192200040119     C                   eval      cmzfld = vlrfou
192300040119     C                   eval      cmzmbd = vlrhdl
192400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
192500040119     C***                if        prmfir = *blanks
192600090911     C                   eval      cmzfla = 'EDIVAB0F'
192700090911     C                   eval      cmzmba = 'EDIVAB0F'
192800040119     C***                else
192900040119     C***                eval      cmzfla = prmfir
193000040119     C***                eval      cmzmba = prmfir
193100040119     C***                endif
193200040119     C                   eval      cmznrr = *zeros
193300040119     C                   move      §ctrokvb      cmznrr
193400040119     C                   eval      cmzlba = vlrfl1
193500040119     C                   call(e)   'TIS711C'
193600040119     C                   parm                    dscmz
193700040119     C                   parm      *blanks       esito
193800040119     C                   if        %error
193900040119     C                             or cmzerr = '1'
194000040119     C                             or esito  = '1'
194100040119     C                   eval      wrkesito = '3'
194200040119     C                   endif
194300040119     C                   endif
194400040119     C*
194500040119     C                   ENDSR
194600040119     C***
194700010601
194800010601
194900010601
195000010601
195100010601      /TITLE Invio dei dati al punto operativo.
195200010601     C     opeini        BEGSR
195300010601     C*
195400010601     C* Inizializzo flag e contatori operativi
195500010601     C                   movel     '0'           flgGiro           1
195600010601     C                   movel     '0'           flgMulti          1
195700010601     C                   movel     '1'           flgStato          1
195800010615     C                   movel     '0'           flgOk             1
195900010601     C                   z-add     *zeros        cntNonEl         10 0
196000010601     C                   z-add     *zeros        depfil            3 0
196100010601     C                   z-add     *zeros        invfil            3 0
196200010601     C*
196300010601     C                   ENDSR
196400010601     C***
196500070326
196600070326
196700070326
196800070326
196900080916     C     *pssr         BEGSR
197000070329     C*
197100080916     C                   if        %open(tivin00r)
197200080916     C                   close     tivin00r
197300080916     C                   endif
197400090911     C                   if        %open(edivabwr)
197500090911     C                   close     edivabwr
197600080916     C                   endif
197700090911     C                   if        %open(edivatwr)
197800090911     C                   close     edivatwr
197900080916     C                   endif
198000070326     C*
198100070326     C* Effettuo la chiamata al CLLE preposto
198200101005     C                   call(e)   'TITVEVTC'
198300080916     C                   parm                    parccm
198400080916     C                   parm                    parmbr
198500080916     C                   parm      '2'           paropz
198600070326     C*
198700080916     C                   eval      wrkesito = '2'
198800070404     C*
198900080916     C                   seton                                        LR
199000070326     C*
199100080916     C                   ENDSR     '*CANCL'
199200070326     C***
199300070326
199400070326
199500010330
199600010330
199700000613     C     *inzsr        BEGSR
199800990910     C*
199900990910     C     *entry        plist
200000990920     C                   parm                    tivlrds
200100990921     C                   parm      wrkesito      esito
200200000724     C                   parm                    prmlit
200300000710     C                   parm                    prmfir
200400010330     C*
200500010330     C* CALCOLA LA DATA CORRENTE
200600010330     C                   time                    wn14             14 0
200700090911     C                   movel     wn14          oracor            6 0          *ORA
200800110923     C                   z-add     *zeros        datcor            8 0
200900110923     C                   eval      datcor = %dec(%date() : *ISO)
201000111118     C*
201100111118     C* Forzo un numero CMR
201200111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
201300000613     C*
201400000613     C                   ENDSR
201500000613     C***
