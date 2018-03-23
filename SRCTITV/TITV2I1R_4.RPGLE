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
005300090904     D wVATNOT_E       s                   LIKE(VATNOT)
005400111117     D wVATNOT_E_1     s                   LIKE(VATNOT)
005500111117     D wVATNOT_E_2     s                   LIKE(VATNOT)
005600111118     D wVATNOT_E_3     s                   LIKE(VATNOT)
005700111118     D wVATNOT_E_4     s                   LIKE(VATNOT)
005800030822     D*------------------
005900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006000030822     D*------------------
006100030822     D posDaDft        S              3  0 INZ(*zeros)
006200030822     D posADft         S              3  0 INZ(*zeros)
006300030822     D j               s              3  0 INZ(1)
006400030822     D wGiroDft        s              1  0 INZ(*zeros)
006500031201     D*------------------
006600031201     D* Costanti
006700031201     D*------------------
006800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000080617     D*------------------
007100080617
007200080617     D*------------------
007300080617     D* LINKING A DEFINIZIONI ESTERNE
007400080617     D*------------------
007500080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700010330
007800010330
007900010330
008000990915     C                   time                    wrkdata
008100990915     C                   time                    wrkora
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400081014     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700010605     C                   exsr      rwvab
008800081014     C*
008900081014     C                   exsr      endela
009000010601     C*
009100010601     C                   seton                                        lr
009200010601
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009800010601     C*--------------------------------------------------------
009900010601     C     ENDELA        BEGSR
010000081014     C*
010100081014     C                   if        %open(tivin00r)
010200081014     C                   close     tivin00r
010300081014     C                   endif
010400090911     C                   if        %open(edivabwr)
010500090911     C                   close     edivabwr
010600081014     C                   endif
010700090911     C                   if        %open(edivatwr)
010800090911     C                   close     edivatwr
010900081014     C                   endif
011000040119     C*
011100040119     C* Effettuo la chiamata al CLLE preposto
011200101005     C                   call(e)   'TITVEVTC'
011300040510     C                   parm                    parccm
011400040119     C                   parm                    parmbr
011500040119     C                   parm      '2'           paropz
011600050201     C*
011700050201     C* Effettuo lancio TISI95 solo x chiusura
011800050201     C                   CLEAR                   TISI95DS
011900050201     C                   EVAL      I95TLA = 'C'
012000050201     C                   CALL      'TISI95R'
012100050201     C                   PARM                    TISI95DS
012200000616     C*
012300010601     C                   ENDSR
012400010601     C***
012500000613
012600010601
012700010601
012800010330     C*--------------------------------------------------------
012900090911     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013000010330     C*--------------------------------------------------------
013100010605     C     RWVAB         BEGSR
013200010330     C*
013300010330     C                   if        not %open(tivin00r)
013400010330     C                   open      tivin00r
013500010330     C                   endif
013600090911     C                   if        not %open(edivabwr)
013700090911     C                   open      edivabwr
013800010330     C                   endif
013900040119     C*
014000101005     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014100101005     C                   exsr      prevat
014200040119     C*
014300040119     C                   if        chkcall = '0'
014400040119     C*
014500090911     C                   if        not %open(edivatwr)
014600090911     C                   open      edivatwr
014700040119     C                   endif
014800090623     C*
014900040119     C                   clear                   §CTROKVB          7 0
015000040119     C                   clear                   §CTROKVT          7 0
015100040119     C                   clear                   §CTRMO            7 0
015200040119     C                   clear                   §CTRNO            7 0
015300090623     C*
015400090623     C* Inizializzazioni
015500090623     C                   exsr      inzvar
015600090623     C                   z-add     *zeros        wNewBolla         7 0
015700010330     C*
015800030822     C                   do        *HIVAL
015900010330     C*
016000030822     C                   read      tivin00r                               70
016100010618     C*
016200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016300010618     C                   if        vinflg = '1'
016400010618     C                   eval      flgOk = '1'
016500010618     C                   endif
016600040510     C*
016700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016800040510     C                   z-add     *zeros        wLenVINDTA        4 0
016900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017300040510     C                   z-add     1             wFlgVINDTA
017400040510     C                   leave
017500040510     C                   endif
017600040510     C                   enddo
017700110923     C*
017800110923     C* Ignoro i record che iniziano x '#'
017900110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
018000110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
018100110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
018200111118     C                   z-add     0             wFlgVINDTA
018300111118     C                   else
018400110923     C*
018500110923     C* Setto i column headers "variabili"
018600111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
018700110923     C                   eval      header = %trim(%subst(vindta:13))
018800110923     C                   endif
018900111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
019000110923     C                   eval      parcel = %trim(%subst(vindta:13))
019100110923     C                   endif
019200111118     C                   endif
019300090420     C*
019400090420     C                   add       1             rrnum
019500010618     C*
019600040510     C                   if        vindta > *blanks AND
019700090420     C                             wFlgVINDTA = 1
019800010330     C*
019900010601     C                   if        *in70 = *off and
020000010330     C                             (vinflg = *blanks
020100010330     C                              or vinflg = '0'
020200010330     C                              or vinflg = '2')
020300010330     C*
020400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020500010711     C                   if        vinflg = *blanks or vinflg = '0'
020600010711     C                   clear                   vinmsg
020700010711     C                   endif
020800010601     C*
020900090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
021000111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
021100111117     C***                eval      vindta  = '*' + %trim(vindta)
021200111117     C***                endif
021300010601     C*
021400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021500010605     C                   eval      depfil = VABLNP
021600010601     C                   exsr      repfil
021700010601     C                   if        depfil = invfil
021800021025     C                   if        vlrpoi = 999
021900030822     C                   move(P)   invfil        VABFGS
022000021025     C                   else
022100030822     C                   move(P)   vlrpoi        VABFGS
022200021025     C                   endif
022300090914     C*
022400090914     C                   move      VABFGS        currFGS           3 0
022500090914     C*
022600090914     C* Elaborazione record corrente
022700090914     C                   exsr      elabCurrRec
022800010604     C*
022900010604     C                   if        *in31 = *off and
023000010604     C                             *in32 = *off
023100010604     C                   eval      vinflg = '1'
023200010604     C                   else
023300010604     C                   eval      recko = vindta
023400010604     C                   eval      vinflg = '2'
023500010604     C                   endif
023600010604     C                   endif
023700010601     C*
023800010604     C                   endif
023900010604     C*
024000010330     C                   else
024100010330     C                   eval      vinflg = '1'
024200010330     C                   endif
024300010601     C*
024400010601     C  N70              update    tivin000
024500010330     C*
024600030822     C  N70              enddo
024700090623     C*
024800090623     C* Scarico la testata rimasta "in canna"
024900140415     C                   if        not *in33 = *off                             * esludo estero
025000090623     C  N31              exsr      wriVAB
025100140415     C                   endif
025200040119     C*
025300040119     C                   endif                                                  (endif - chkcall)
025400010601     C*
025500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025600010601     C                   if        cntNonEl = *zeros or
025700010601     C                             flgMulti = '0'
025800010330     C* Se non ci sono record con errori ...
025900010601     C                   if        §ctrno = 0 and
026000010604     C                             §ctrmo = 0 and
026100010601     C                             flgStato <> '2'
026200010330     C* ... restituisco esito OK.
026300010330     C                   eval      wrkesito = '0'
026400010330     C                   else
026500040119     C                   if        §ctrokvb > 0
026600010330     C                   eval      wrkesito = '1'
026700010330     C                   else
026800010615     C                   if        flgOk = '0'
026900010615     C                   eval      wrkesito = '2'
027000010615     C                   else
027100010615     C                   eval      wrkesito = '6'
027200010615     C                   endif
027300010330     C                   endif
027400010330     C                   endif
027500010601     C                   else
027600010601     C                   eval      wrkesito = '9'
027700010601     C                   endif
027800010330     C*
027900010330     C                   if        %open(tivin00r)
028000010330     C                   close     tivin00r
028100010330     C                   endif
028200090911     C                   if        %open(edivabwr)
028300090911     C                   close     edivabwr
028400010330     C                   endif
028500090911     C                   if        %open(edivatwr)
028600090911     C                   close     edivatwr
028700040512     C                   endif
028800010601     C*
028900010601     C                   if        vlrpoi <> 999
029000010601     C                   eval      invfil = vlrpoi
029100010601     C                   endif
029200010330     C*
029300040119     C                   if        §ctrokvb > 0
029400010601     C                             and invfil > *zeros
029500010330     C                   exsr      invio
029600010330     C                   endif
029700010330     C*
029800010330     C                   ENDSR
029900010330     C***
030000090623
030100090623
030200090623
030300090623     C*----------------------------------------------------*
030400090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
030500090623     C*----------------------------------------------------*
030600090623     C     INZWRK        BEGSR
030700090623     C*
030800090623     C                   eval      i = 1
030900090623     C                   eval      posDa = *zeros
031000090623     C                   eval      posA  = *zeros
031100090623     C*
031200090623     C                   ENDSR
031300090623
031400090623
031500090623
031600090623     C*----------------------------------------------------*
031700090623     C*  ELABORAZIONE RECORD CORRENTE
031800090623     C*----------------------------------------------------*
031900090623     C     ELABCURRREC   BEGSR
032000090623     C*
032100090623     C                   select
032200090623     C*
032300110923     C* Tipo record "TESTATA" - HEADER
032400111118     C                   when      %subst(vindta:1:6) = 'HEADER'
032500090623     C                   if        wNewBolla > *zeros
032600140415     C                   if        not *in33 = *off                             * esludo estero
032700090623     C  N31              exsr      wriVAB
032800140415     C                   endif
032900090623     C                   exsr      inzvar
033000090623     C                   endif
033100090624     C                   eval      wNewBolla = 1
033200090623     C                   eval      wGiro = *zeros
033300111118     C                   clear                   SkSplitFLDH
033400110923     C                   eval      prefix = 'HDR_'
033500090624     C                   exsr      inzwrk
033600090623     C                   exsr      impvab
033700090623     C                   exsr      inzwrk
033800090623     C                   exsr      impvab
033900090623     C*
034000110923     C* Tipo record "DETTAGLIO" - PARCEL
034100110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
034200090623     C                   eval      wGiro = *zeros
034300111118     C                   clear                   SkSplitFLDP
034400110923     C                   eval      prefix = 'PCL_'
034500090623     C                   exsr      inzwrk
034600101005     C                   exsr      impvat
034700090623     C                   exsr      inzwrk
034800101005     C                   exsr      impvat
034900090624     C*
035000090624     C  N31              exsr      wrivat
035100090623     C*
035200090623     C*
035300090623     C                   endsl
035400090623     C*
035500090623     C                   ENDSR
035600040119
035700040119
035800040119
035900040119     C*----------------------------------------------------*
036000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036100040119     C*----------------------------------------------------*
036200040510     C     WRIVAB        BEGSR
036300090623     C*
036400090623     C  N31              add       1             §CTROKVB
036500090623     C   32              add       1             §CTRMO
036600090623     C   31              add       1             §CTRNO
036700040119     C*
036800060331     C* Gestisco l'eventuale rottura x numero spedizione
036900070823     C                   if        VABNSP <> depspe
037000070301     C                   if        wVATNOT_E <> *blanks
037100080125     C                   if        VABCTM = *blanks
037200080125     C                   eval      VABCTM = '7Q'
037300080125     C                   endif
037400070301     C                   endif
037500090911     C*
037600090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037700090911     C                   eval      VABCMR = wCMR
037800090911     C                   eval      VABDCM = DATCOR
037900090911     C                   eval      VABDTS = DATCOR
038000090911     C                   eval      VABHMS = ORACOR
038100090911     C                   eval      VABCNT = 1
038200090911     C****
038300090911     C                   write     edivab00                                     => scarico il VAB
038400060331     C                   endif
038500090623     C*
038600090623     C  N31              eval      depspe = vabNSP
038700040119     C*
038800040119     C                   ENDSR
038900040119
039000040119
039100040119
039200040119     C*----------------------------------------------------*
039300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039400040119     C*----------------------------------------------------*
039500040510     C     WRIVAT        BEGSR
039600060512     C*
039700060512     C* Inizializzo i campi chiave
039800090914     C                   eval      VATFGS = currFGS
039900060512     C                   eval      VATAAS = VABAAS
040000060512     C                   eval      VATCCM = VABCCM
040100070823     C                   eval      VATNRS = VABNRS
040200060512     C                   eval      VATNSP = VABNSP
040300060512     C                   eval      VATLNP = VABLNP
040400111117     C                   eval      VATCMR = wCMR
040500090914     C                   eval      VATCNT = 1
040600060331     C*
040700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040800060331     C                   if        wVATNOT_A <> *blanks
040900060331     C                   eval      VATNOT = wVATNOT_A
041000060331     C                   eval      VATTRC = 'A'
041100090911     C                   write     EDIVAT00
041200060331     C                   add       1             §CTROKVT
041300060331     C                   endif
041400060331     C*
041500060331     C                   if        wVATNOT_B <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_B
041700060331     C                   eval      VATTRC = 'B'
041800090911     C                   write     EDIVAT00
041900060331     C                   add       1             §CTROKVT
042000060331     C                   endif
042100111118     C*
042200111118     C                   if        wVATNOT_E <> *blanks
042300111118     C                   eval      VATNOT = wVATNOT_E
042400111118     C                   eval      VATTRC = 'E'
042500111118     C                   write     EDIVAT00
042600111118     C                   add       1             §CTROKVT
042700111118     C                   endif
042800090623     C*
042900111118 xxx C* Considero il VAT solo a rottura del "chi sono"
043000111118     C***                if        wVATNOT_E <> *blanks AND
043100111118     C***                          wVATNOT_E <> sVATNOT_E
043200111118     C***                eval      VATNOT = wVATNOT_E
043300111118     C***                eval      VATTRC = 'E'
043400111118     C***                write     EDIVAT00
043500111118     C***                add       1             §CTROKVT
043600111118     C***                eval      sVATNOT_E = wVATNOT_E
043700111118     C***                add       1             VABNCL
043800111118     C***                endif
043900040119     C*
044000040119     C                   ENDSR
044100040119
044200040119
044300040119
044400040119     C*----------------------------------------------------*
044500101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
044600040119     C*----------------------------------------------------*
044700101005     C     PREVAT        BEGSR
044800040119     C*
044900101005     C* Compongo il nome del membro da dare al EDIVATWR
045000040119     C                   eval      parmbr = vlrhdl
045100040119     C                   movel     'M'           parmbr
045200040510     C                   eval      parccm = vlrksc
045300040119     C                   eval      paropz = '1'
045400040119     C* Effettuo la chiamata al CLLE preposto
045500101005     C                   call(e)   'TITVEVTC'
045600040119     C                   parm                    parccm
045700040119     C                   parm                    parmbr
045800040119     C                   parm                    paropz
045900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046000040119     C                   if        %error
046100040119     C                   movel     '1'           chkcall
046200040119     C                   else
046300040119     C                   movel     '0'           chkcall
046400040119     C                   endif
046500040119     C*
046600040119     C                   ENDSR
046700010601
046800010601
046900010601
047000010330     C*----------------------------------------------------*
047100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047200010330     C*----------------------------------------------------*
047300040119     C     INZVAR        BEGSR
047400010330     C*
047500030822     C* Inizializzo variabili di wrk
047600140415     C                   SETOFF                                       313233
047700090916     C                   Z-ADD     *zeros        Num5_0            5 0
047800090916     C                   MOVEL     *blanks       wNOTE            70
047900060331     C                   MOVEL     *blanks       wVATNOT_A
048000060331     C                   MOVEL     *blanks       wVATNOT_B
048100090904     C                   MOVEL     *blanks       wVATNOT_E
048200111117     C                   MOVEL     *blanks       wVATNOT_E_1
048300111117     C                   MOVEL     *blanks       wVATNOT_E_2
048400111118     C                   MOVEL     *blanks       wVATNOT_E_3
048500111118     C                   MOVEL     *blanks       wVATNOT_E_4
048600090623     C*
048700111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
048800090220     C*
048900080617     C                   EVAL      wFlgCAS = *blanks
049000030822     C*
049100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049200090911     C                   CLEAR                   EDIVAB00
049300090911     C                   CLEAR                   EDIVAT00
049400030822     C                   CLEAR                   SkSplitCSV
049500090623     C                   EXSR      inzwrk
049600030822     C*
049700030822     C* Reimposto i valori di default
049800030822     C                   EXSR      DEFCAM
049900030822     C*
050000010330     C                   ENDSR
050100010330     C*----------------------------------------------------*
050200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050300010330     C*----------------------------------------------------*
050400010330     C     DEFCAM        BEGSR
050500030822     C*
050600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050700030715     C* e delimitatore testo.
050800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051100030822     C*
051200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
051400030715     C                   SETON                                        50
051500030715     C                   ELSE
051600030715     C                   SETOFF                                       50
051700030715     C                   ENDIF
051800090421     C*
051900090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
052000090421     C                   EVAL      VABCBO = '1'
052100090421     C                   EVAL      VABLNP = vlrpoi
052200090421     C                   EVAL      VABNSP = rrnum
052300030822     C*
052400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052500030822     C                   EVAL      posDaDft = 1
052600030822     C                   EVAL      posADft  = 0
052700030822     C                   EVAL      wGiroDft = 0
052800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052900030822     C                             posDaDft > 0
053000030822     C*
053100030822     C* Gestisco il 1° giro
053200030822     C                   IF        wGiroDft = 0
053300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053500030822     C* Incremento il contatore dei "giri"
053600030822     C                   EVAL      wGiroDft = 1
053700030822     C                   ELSE
053800030822     C                   EVAL      posDaDft = posADft
053900030822     C                   ENDIF
054000030822     C* Eseguo lo scan x trovare la fine del campo corrente
054100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054200030822     C*
054300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054500030822     C                   IF        posDaDft > 0 AND
054600030822     C                             posADft  > 0
054700030822     C* NCL
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'NCL'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABNCL
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CCM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CCM'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABCCM
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* LNP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'LNP'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABLNP
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* NRS
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'NRS'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABNRS
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* CTR
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CTR'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABCTR
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* PKB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'PKB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200030822     C                   Z-ADD     PiVal         VABPKB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* VLB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'VLB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABVLB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* QFT
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'QFT'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABQFT
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CBO
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CBO'
065600030822     C                   EVAL      VABCBO=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* TSP
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'TSP'
066500030822     C                   EVAL      VABTSP=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* VAS
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VAS'
067400030822     C                   EVAL      VABVAS=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VCA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VCA'
068300030822     C                   EVAL      VABVCA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700070925     C* TIC
068800070925     C                   IF        %subst(
068900070925     C                             %subst(vlrppt:posDaDft+1:
069000070925     C                             posADft-posDaDft-1):1:3)
069100070925     C                             = 'TIC'
069200070925     C                   EVAL      VABTIC=%trim(%subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500070925     C                   ENDIF
069600030822     C* GCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GCA'
070100030822     C                   EVAL      VABGCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* CTM
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CTM'
071000030822     C                   EVAL      VABCTM=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* FFD
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'FFD'
071900030822     C                   EVAL      VABFFD=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VAD
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VAD'
072800030822     C                   EVAL      VABVAD=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* GMA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'GMA'
073700030822     C                   EVAL      VABGMA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* GGA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GGA'
074600030822     C                   EVAL      VABGGA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GVA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GVA'
075500030822     C                   EVAL      VABGVA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* TC1
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'TC1'
076400030822     C                   EVAL      VABTC1=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* TC2
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'TC2'
077300030822     C                   EVAL      VABTC2=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700040714     C* VATTRC
077800040714     C                   IF        %subst(
077900040714     C                             %subst(vlrppt:posDaDft+1:
078000040714     C                             posADft-posDaDft-1):1:3)
078100040714     C                             = 'TRC'
078200040714     C                   EVAL      VATTRC=%trim(%subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500040714     C                   ENDIF
078600030822     C* ...
078700030822     C                   ENDIF
078800030822     C                   ENDDO
078900020204     C*
079000010330     C                   ENDSR
079100010607     C*----------------------------------------------------*
079200090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
079300010607     C*----------------------------------------------------*
079400090623     C     IMPVAB        BEGSR
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300080716     C*
080400080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
080500080716     C                   EVAL      depvinDTA = vinDTA
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800110923     C                   IF        header <> *blanks
080900110923     C                   EVAL      vindta = header
081000070823     C     ';':CharCSV   XLATE     vindta        vindta
081100040510     C                   ENDIF
081200080626     C*
081300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081400031201     C     minu:maiu     XLATE     vindta        vindta
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
082600110923     C                                         %subst(vindta:posDa:(posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
083800110923     C                                         %subst(vindta:posDa:(posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
084300110923     C                                         %subst(vindta:posDa:(posA-posDa))
084400030715     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300111118     C                   DOW       i < %elem(SkSplitFLDH)
085400111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
085500111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C                   EVAL      wGiro = 1
085900080716     C* Ripristino la 1° riga originale
086000080716     C                   EVAL      vinDTA = depvinDTA
086100030715     C                   ELSE
086200030715     C                   EVAL      wGiro = 2
086300031204     C**********
086400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086500031204     C* d separatore campo
086600031204     C**********
086700031204     C                   EVAL      vindta = %trim(vindta)
086800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086900031204     C                   EVAL      vindta = %subst(vindta:2)
087000031204     C                   ENDDO
087100031204     C*
087200031204     C                   Z-ADD     *zeros        lunghInput        4 0
087300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087700031204     C                   ENDDO
087800030715     C**********
087900030715     C* Effettuo lo split del campo dati d input
088000030715     C**********
088100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088200030715     C                   DOW       posDa <= %len(%trim(vindta))
088300030715     C*
088400030715     C* Gestisco il 1° campo
088500030715     C                   IF        i = 1
088600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088700030715     C                   EVAL      posDa = 1
088800030715     C* Eseguo lo scan x trovare la fine del primo campo
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* A questo punto "estraggo" il campo corrente
089100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089200030715     C                                                    (posA-posDa))
089300030715     C* X i campi successivi al 1°
089400030715     C                   ELSE
089500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089600030715     C                   EVAL      posDa = posA + 1
089700031204     C* Verifico che nn vi sia il campo nn valorizzato
089800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089900031204     C* Se campo nn valorizzato skippo al prossimo
090000031204     C                   EVAL      SkSplitCSV(i) = *blanks
090100031204     C                   EVAL      posA  = posA + 1
090200031204     C                   ELSE
090300030715     C* Eseguo lo scan x trovare la fine del campo corrente
090400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090500030715     C* Gestisco l'ultimo campo
090600030715     C                   IF        posA = *zeros
090700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   LEAVE
091200030715     C                   ELSE
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   ENDIF
091700031204     C                   ENDIF
091800030715     C                   ENDIF
091900030715     C* Incremento il contatore d campo
092000030715     C                   EVAL      i = i +1
092100030715     C                   ENDDO
092200030715     C*
092300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092400030715     C* delimitazione testo
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitCSV)
092700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092900030715     C                   EVAL      i = i + 1
093000030715     C                   ENDDO
093100030715     C*
093200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093300030715     C                   EVAL      i = 1
093400111118     C                   DOW       i < %elem(SkSplitFLDH)
093500030715     C*
093600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093700111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
093800090623     C                             SkSplitCSV(i) <> *blanks
093900090623     C*
094000111118     C                   exsr      CARVALH
094100040510     C*
094200030715     C                   ENDIF
094300030715     C                   EVAL      i = i + 1
094400030715     C                   ENDDO
094500070925     C*
094600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
094700080125     C*
094800111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
094900111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
095000070925     C*
095100071029     C***  <===  -----------------------------------------
095200070925     C*
095300070925     C* Considerazioni finali su CBO/CAS
095400080125     C                   IF        wFlgCAS = '1'
095500070925     C                   IF        VABCBO = '1'
095600070925     C                   EVAL      VABCBO = '4'
095700070925     C                   ENDIF
095800070925     C                   IF        VABCBO = '2'
095900070925     C                   EVAL      VABCBO = '6'
096000070925     C                   ENDIF
096100070925     C                   ENDIF
096200070925     C***  <===  ----------------------------
096300040510     C*
096400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
096500040510     C                   IF        VABNSP = *zeros
096600040510     C                   SETON                                        31
096700040510     C                   ENDIF
096800040715     C*
096900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
097000040715     C                   IF        VABLNP = *zeros
097100040715     C                   SETON                                        31
097200040715     C                   ENDIF
097300040510     C*
097400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
097500040510     C                   IF        VABRMN = *zeros
097600040510     C                   Z-ADD     VABNSP        VABRMN
097700040510     C                   ENDIF
097800040510     C*
097900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
098000040510     C                   IF        VABRMA = *blanks
098100040714     C                   MOVEL     VABRMN        VABRMA
098200040510     C                   ENDIF
098300040128     C*
098400040510     C* Se provincia nn valorizzata la reperisco
098500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
098600040510     C                   IF        VABPRD = *blanks
098700040128     C                   CLEAR                   TISI95DS
098800040128     C                   EVAL      I95TCN = '3'
098900040128     C                   Z-ADD     datcor        I95DAT
099000040128     C                   EVAL      I95NAR = VABNZD
099100040128     C                   EVAL      I95CAP = VABCAD
099200040128     C                   EVAL      I95LOC = VABLOD
099300040128     C                   CALL      'TISI95R'
099400040128     C                   PARM                    TISI95DS
099500040128     C                   EVAL      VABPRD = O95PRV
099600040510     C                   ENDIF
099700020204     C*
099800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
099900020204     C                   EXSR      CHKIMPDIV
100000030715     C                   ENDIF
100100020204     C*
100200010607     C                   ENDSR
100300010607     C*----------------------------------------------------*
100400090623
100500090623
100600090623
100700090623     C*----------------------------------------------------*
100800101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
100900090623     C*----------------------------------------------------*
101000101005     C     IMPVAT        BEGSR
101100090623     C*
101200090623     C**********
101300090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
101400090623     C**********
101500090623     C                   IF        wGiro = *zeros
101600090623     C*
101700090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
101800090623     C                   EVAL      depvinDTA = vinDTA
101900090623     C*
102000090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
102100110923     C                   IF        parcel <> *blanks
102200110923     C                   EVAL      vindta = parcel
102300090623     C     ';':CharCSV   XLATE     vindta        vindta
102400090623     C                   ENDIF
102500090623     C*
102600090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
102700090623     C     minu:maiu     XLATE     vindta        vindta
102800090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
102900090623     C                   DOW       posDa <= %len(%trim(vindta))
103000090623     C*
103100090623     C* Gestisco il 1° campo
103200090623     C                   IF        i = 1
103300090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
103400090623     C                   EVAL      posDa = 1
103500090623     C* Eseguo lo scan x trovare la fine del primo campo
103600090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
103700090623     C* A questo punto "estraggo" il campo corrente
103800111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
103900110923     C                                         %subst(vindta:posDa:(posA-posDa))
104000090623     C* X i campi successivi al 1°
104100090623     C                   ELSE
104200090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
104300090623     C                   EVAL      posDa = posA + 1
104400090623     C* Eseguo lo scan x trovare la fine del campo corrente
104500090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
104600090623     C* Gestisco l'ultimo campo
104700090623     C                   IF        posA = *zeros
104800090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
104900090623     C* A questo punto "estraggo" il campo corrente
105000111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
105100110923     C                                         %subst(vindta:posDa:(posA-posDa))
105200090623     C                   LEAVE
105300090623     C                   ELSE
105400090623     C* A questo punto "estraggo" il campo corrente
105500111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
105600110923     C                                         %subst(vindta:posDa:(posA-posDa))
105700090623     C                   ENDIF
105800090623     C                   ENDIF
105900090623     C* Incremento il contatore d campo
106000090623     C                   EVAL      i = i +1
106100090623     C                   ENDDO
106200090623     C*
106300090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
106400090623     C* delimitazione testo
106500090623     C                   EVAL      i = 1
106600111118     C                   DOW       i < %elem(SkSplitFLDP)
106700111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
106800111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
106900090623     C                   EVAL      i = i + 1
107000090623     C                   ENDDO
107100090623     C                   EVAL      wGiro = 1
107200090623     C* Ripristino la 1° riga originale
107300090623     C                   EVAL      vinDTA = depvinDTA
107400090623     C                   ELSE
107500090623     C                   EVAL      wGiro = 2
107600090623     C**********
107700090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
107800090623     C* d separatore campo
107900090623     C**********
108000090623     C                   EVAL      vindta = %trim(vindta)
108100090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
108200090623     C                   EVAL      vindta = %subst(vindta:2)
108300090623     C                   ENDDO
108400090623     C*
108500090623     C                   Z-ADD     *zeros        lunghInput        4 0
108600090623     C                   EVAL      lunghInput = %len(%trim(vindta))
108700090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
108800090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
108900090623     C                   EVAL      lunghInput = %len(%trim(vindta))
109000090623     C                   ENDDO
109100090623     C**********
109200090623     C* Effettuo lo split del campo dati d input
109300090623     C**********
109400090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
109500090623     C                   DOW       posDa <= %len(%trim(vindta))
109600090623     C*
109700090623     C* Gestisco il 1° campo
109800090623     C                   IF        i = 1
109900090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
110000090623     C                   EVAL      posDa = 1
110100090623     C* Eseguo lo scan x trovare la fine del primo campo
110200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
110300090623     C* A questo punto "estraggo" il campo corrente
110400090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
110500090623     C                                                    (posA-posDa))
110600090623     C* X i campi successivi al 1°
110700090623     C                   ELSE
110800090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
110900090623     C                   EVAL      posDa = posA + 1
111000090623     C* Verifico che nn vi sia il campo nn valorizzato
111100090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
111200090623     C* Se campo nn valorizzato skippo al prossimo
111300090623     C                   EVAL      SkSplitCSV(i) = *blanks
111400090623     C                   EVAL      posA  = posA + 1
111500090623     C                   ELSE
111600090623     C* Eseguo lo scan x trovare la fine del campo corrente
111700090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
111800090623     C* Gestisco l'ultimo campo
111900090623     C                   IF        posA = *zeros
112000090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
112100090623     C* A questo punto "estraggo" il campo corrente
112200090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
112300090623     C                                                    (posA-posDa))
112400090623     C                   LEAVE
112500090623     C                   ELSE
112600090623     C* A questo punto "estraggo" il campo corrente
112700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
112800090623     C                                                    (posA-posDa))
112900090623     C                   ENDIF
113000090623     C                   ENDIF
113100090623     C                   ENDIF
113200090623     C* Incremento il contatore d campo
113300090623     C                   EVAL      i = i +1
113400090623     C                   ENDDO
113500090623     C*
113600090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
113700090623     C* delimitazione testo
113800090623     C                   EVAL      i = 1
113900090623     C                   DOW       i < %elem(SkSplitCSV)
114000090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
114100090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
114200090623     C                   EVAL      i = i + 1
114300090623     C                   ENDDO
114400090623     C*
114500090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
114600090623     C                   EVAL      i = 1
114700111118     C                   DOW       i < %elem(SkSplitFLDP)
114800090623     C*
114900090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
115000111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
115100090623     C                             SkSplitCSV(i) <> *blanks
115200090623     C*
115300111118     C                   exsr      CARVALP
115400090623     C*
115500090623     C                   ENDIF
115600090623     C                   EVAL      i = i + 1
115700090623     C                   ENDDO
115800090623     C*
115900090623     C***  ===>  Gestione campi "particolari" fuori ciclo
116000111118     C*
116100111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
116200111118     C                                         %trim(wVATNOT_E_2) +
116300111118     C                                         %trim(wVATNOT_E_3) +
116400111118     C                                         %trim(wVATNOT_E_4)
116500090623     C*
116600090623     C***  <===  -----------------------------------------
116700090623     C*
116800090623     C                   ENDIF
116900090623     C*
117000090623     C                   ENDSR
117100090623     C*----------------------------------------------------*
117200090623
117300090623
117400090623     C*----------------------------------------------------*
117500090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
117600090623     C*----------------------------------------------------*
117700111118     C     CARVALH       BEGSR
117800090623     C*
117900090623     C*** ATB
118000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
118100090623     C                   EVAL      VABATB = SkSplitCSV(i)
118200090623     C                   ENDIF
118300090623     C*** RSD
118400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
118500090623     C                   EVAL      VABRSD = SkSplitCSV(i)
118600090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
118700090623     C     '@':'A'       XLATE     VABRSD        VABRSD
118800090623     C* ==
118900090623     C                   ENDIF
119000090623     C*** RD2
119100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
119200090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
119300090623     C                   ENDIF
119400130530     C*** IND 1 (HDR)
119500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
119600090623     C                   EVAL      VABIND = SkSplitCSV(i)
119700090623     C                   ENDIF
119800130530     C*** IND 2 (HDR)
119900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
120000111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
120100111117     C                   ENDIF
120200130530     C*** IND 1
120300130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
120400130530     C                   EVAL      VABIND = SkSplitCSV(i)
120500130530     C                   ENDIF
120600130530     C*** IND 2
120700130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
120800130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
120900130530     C                   ENDIF
121000111117     C*** CAD
121100111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
121200111117     C                   EVAL      VABCAD = SkSplitCSV(i)
121300111216     C                   MOVEL     *blanks       wAlfa7            7
121400111216     C                   MOVEL     *zeros        wAlfa7z           7
121500111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
121600111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
121700111117     C                   ENDIF
121800090623     C*** LOD
121900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
122000090623     C                   EVAL      VABLOD = SkSplitCSV(i)
122100090623     C                   ENDIF
122200111117     C*** PRD
122300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
122400090623     C                   EVAL      VABPRD = SkSplitCSV(i)
122500090623     C                   ENDIF
122600090623     C*** NZD
122700140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
122800140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
122900140415     C                   SETON                                        33
123000090623     C                   ENDIF
123100140415     C                   ENDIF
123200090623     C*** RMA
123300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
123400130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
123500130611     C                   EVAL      VABRMA = SkSplitCSV(i)
123600090623     C                   ENDIF
123700111117     C*** NOT
123800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
123900111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
124000090623     C                   ENDIF
124100111117     C*** NT2
124200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
124300111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
124400090623     C                   ENDIF
124500090623     C*** VCA
124600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
124700090623     C                   EVAL      VABVCA = SkSplitCSV(i)
124800090623     C                   ENDIF
124900090623     C*** VAS
125000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
125100090623     C                   EVAL      VABVAS = SkSplitCSV(i)
125200090623     C                   ENDIF
125300090623     C*** FFD
125400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
125500090623     C                   EVAL      VABFFD = SkSplitCSV(i)
125600090623     C                   ENDIF
125700090623     C*** GC1
125800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
125900090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
126000090623     C                   ENDIF
126100090623     C*** GC2
126200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
126300090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
126400090623     C                   ENDIF
126500090623     C*** TSP
126600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
126700090623     C                   EVAL      VABTSP = SkSplitCSV(i)
126800090623     C                   ENDIF
126900090623     C*** CBO
127000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
127100090623     C                   EVAL      VABCBO = SkSplitCSV(i)
127200090623     C                   ENDIF
127300090623     C*** NAS
127400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
127500090623     C                   EVAL      VABNAS = SkSplitCSV(i)
127600090623     C                   ENDIF
127700090623     C*** TIC
127800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
127900090623     C                   EVAL      VABTIC = SkSplitCSV(i)
128000090623     C                   ENDIF
128100090623     C*** GCA
128200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
128300090623     C                   EVAL      VABGCA = SkSplitCSV(i)
128400090623     C                   ENDIF
128500090623     C*** XCO
128600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
128700090623     C                   EVAL      VABXCO = SkSplitCSV(i)
128800090623     C                   ENDIF
128900090623     C*** CTM
129000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
129100090623     C                   EVAL      VABCTM = SkSplitCSV(i)
129200090623     C                   ENDIF
129300090623     C*** TCR
129400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
129500090623     C                   EVAL      VABTCR = SkSplitCSV(i)
129600090623     C                   ENDIF
129700090623     C*** CTS
129800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
129900090623     C                   EVAL      VABCTS = SkSplitCSV(i)
130000090623     C                   ENDIF
130100090623     C*** FTM
130200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
130300090623     C                   EVAL      VABFTM = SkSplitCSV(i)
130400090623     C                   ENDIF
130500090623     C*** VAD
130600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
130700090623     C                   EVAL      VABVAD = SkSplitCSV(i)
130800090623     C                   ENDIF
130900090623     C*** GMA
131000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
131100090623     C                   EVAL      VABGMA = SkSplitCSV(i)
131200090623     C                   ENDIF
131300090623     C*** GGA
131400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
131500090623     C                   EVAL      VABGGA = SkSplitCSV(i)
131600090623     C                   ENDIF
131700090623     C*** GVA
131800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
131900090623     C                   EVAL      VABGVA = SkSplitCSV(i)
132000090623     C                   ENDIF
132100090623     C*** TC1
132200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
132300090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
132400090623     C                   ENDIF
132500090623     C*** TC2
132600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
132700090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
132800090623     C                   ENDIF
132900090623     C*** SCL
133000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
133100090623     C                   EVAL      VABSCL = SkSplitCSV(i)
133200090623     C                   ENDIF
133300090623     C*** RMO
133400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
133500090623     C                   EVAL      VABRMO = SkSplitCSV(i)
133600090623     C                   ENDIF
133700090623     C*** NMO
133800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
133900090623     C                   EVAL      VABNMO = SkSplitCSV(i)
134000090623     C                   ENDIF
134100090623     C*
134200090623     C* Reperisco quindi i campi numerici...
134300111117     C*** NSP
134400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
134500111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
134600090623     C                   EXSR      CHKNUM
134700090623     C                   IF        PiInt=*on
134800090623     C                   Z-ADD     PiVal         VABNSP
134900090623     C                   ELSE
135000090623     C                   SETON                                        31
135100090623     C                   EVAL      VABNSP = *zeros
135200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
135300090623     C                             + ' ' + 'VABNSP'
135400090623     C                   ENDIF
135500090623     C                   ENDIF
135600090623     C*** RMN
135700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
135800111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
135900090623     C                   EXSR      CHKNUM
136000090623     C                   IF        PiInt=*on
136100090623     C                   Z-ADD     PiVal         VABRMN
136200090623     C                   ELSE
136300090623     C                   SETON                                        32
136400090623     C                   EVAL      VABRMN = *zeros
136500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
136600090623     C                             + ' ' + 'VABRMN'
136700090623     C                   ENDIF
136800090623     C                   ENDIF
136900090623     C*** CAD
137000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
137100090623     C                   EVAL      PiStr=SkSplitCSV(i)
137200090623     C                   EXSR      CHKNUM
137300090623     C                   IF        PiInt=*on
137400090623     C                   Z-ADD     PiVal         Num5_0
137500090623     C                   MOVEL(p)  Num5_0        VABCAD
137600090623     C                   ELSE
137700090623     C                   EVAL      VABCAD = SkSplitCSV(i)
137800090623     C                   SETON                                        32
137900090623     C                   EVAL      VABCAD = *zeros
138000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
138100090623     C                             + ' ' + 'VABCAD'
138200090623     C                   ENDIF
138300090623     C                   ENDIF
138400090623     C*** NCL
138500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
138600090623     C                   EVAL      PiStr=SkSplitCSV(i)
138700090623     C                   EXSR      CHKNUM
138800090623     C                   IF        PiInt=*on
138900090623     C                   Z-ADD     PiVal         VABNCL
139000090623     C                   ELSE
139100090623     C                   SETON                                        32
139200090623     C                   EVAL      VABNCL = *zeros
139300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
139400090623     C                             + ' ' + 'VABNCL'
139500090623     C                   ENDIF
139600090623     C                   ENDIF
139700090623     C*** PKB
139800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
139900090623     C                   EVAL      PiStr=SkSplitCSV(i)
140000090623     C                   EXSR      CHKNUM
140100090623     C                   IF        PiNum=*on
140200130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
140300090623     C                   Z-ADD     PiVal         VABPKB
140400090623     C                   ELSE
140500090623     C                   SETON                                        32
140600090623     C                   EVAL      VABPKB = *zeros
140700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
140800090623     C                             + ' ' + 'VABPKB'
140900090623     C                   ENDIF
141000090623     C                   ENDIF
141100090623     C*** CAS
141200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
141300090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
141400090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
141500090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
141600090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
141700090623     C                   EVAL      wFlgCAS = '1'
141800090623     C                   ENDIF
141900090623     C                   EVAL      PiStr=SkSplitCSV(i)
142000090623     C                   EXSR      CHKNUM
142100090623     C                   IF        PiNum=*on
142200090623     C                   Z-ADD     PiVal         VABCAS
142300090623     C                   ELSE
142400090623     C                   SETON                                        32
142500090623     C                   EVAL      VABCAS = *zeros
142600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142700090623     C                             + ' ' + 'VABCAS'
142800090623     C                   ENDIF
142900090623     C                   ENDIF
143000090623     C*** CCM
143100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
143200090623     C                   EVAL      PiStr=SkSplitCSV(i)
143300090623     C                   EXSR      CHKNUM
143400090623     C                   IF        PiInt=*on      AND
143500090623     C                             PiVal<=9999999 AND
143600090623     C                             PiVal>*zeros
143700090623     C                   Z-ADD     PiVal         VABCCM
143800090623     C                   ELSE
143900090623     C                   SETON                                        32
144000090623     C                   EVAL      VABCCM = *zeros
144100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144200090623     C                             + ' ' + 'VABCCM'
144300090623     C                   ENDIF
144400090623     C                   ENDIF
144500090623     C*** LNP
144600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
144700090623     C                   EVAL      PiStr=SkSplitCSV(i)
144800090623     C                   EXSR      CHKNUM
144900090623     C                   IF        PiInt=*on    AND
145000090623     C                             PiVal<=999   AND
145100090623     C                             PiVal>*zeros
145200090623     C                   Z-ADD     PiVal         VABLNP
145300090623     C                   ELSE
145400090623     C                   SETON                                        32
145500090623     C                   EVAL      VABLNP = *zeros
145600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145700090623     C                             + ' ' + 'VABLNP'
145800090623     C                   ENDIF
145900090623     C                   ENDIF
146000090623     C*** NRS
146100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
146200090623     C                   EVAL      PiStr=SkSplitCSV(i)
146300090623     C                   EXSR      CHKNUM
146400090623     C                   IF        PiInt=*on AND
146500090623     C                             PiVal<=99
146600090623     C                   Z-ADD     PiVal         VABNRS
146700090623     C                   ELSE
146800090623     C                   SETON                                        32
146900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147000090623     C                             + ' ' + 'VABNRS'
147100090623     C                   ENDIF
147200090623     C                   ENDIF
147300090623     C*** LNA
147400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
147500090623     C                   EVAL      PiStr=SkSplitCSV(i)
147600090623     C                   EXSR      CHKNUM
147700090623     C                   IF        PiInt=*on    AND
147800090623     C                             PiVal<=999   AND
147900090623     C                             PiVal>*zeros
148000090623     C                   Z-ADD     PiVal         VABLNA
148100090623     C                   ELSE
148200090623     C                   SETON                                        32
148300090623     C                   EVAL      VABLNA = *zeros
148400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148500090623     C                             + ' ' + 'VABLNA'
148600090623     C                   ENDIF
148700090623     C                   ENDIF
148800090623     C*** CTR
148900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
149000090623     C                   EVAL      PiStr=SkSplitCSV(i)
149100090623     C                   EXSR      CHKNUM
149200090623     C                   IF        PiInt=*on  AND
149300090623     C                             PiVal<=999
149400090623     C                   Z-ADD     PiVal         VABCTR
149500090623     C                   ELSE
149600090623     C                   SETON                                        32
149700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149800090623     C                             + ' ' + 'VABCTR'
149900090623     C                   ENDIF
150000090623     C                   ENDIF
150100090623     C*** IAS
150200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
150300090623     C                   EVAL      PiStr=SkSplitCSV(i)
150400090623     C                   EXSR      CHKNUM
150500090623     C                   IF        PiNum=*on
150600090623     C                   Z-ADD     PiVal         VABIAS
150700090623     C                   ELSE
150800090623     C                   SETON                                        32
150900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151000090623     C                             + ' ' + 'VABIAS'
151100090623     C                   ENDIF
151200090623     C                   ENDIF
151300090623     C*** VLB
151400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
151500090623     C                   EVAL      PiStr=SkSplitCSV(i)
151600090623     C                   EXSR      CHKNUM
151700090623     C                   IF        PiNum=*on
151800111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
151900090623     C                   Z-ADD     PiVal         VABVLB
152000090623     C                   ELSE
152100090623     C                   SETON                                        32
152200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152300090623     C                             + ' ' + 'VABVLB'
152400090623     C                   ENDIF
152500090623     C                   ENDIF
152600090623     C*** QFT
152700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
152800090623     C                   EVAL      PiStr=SkSplitCSV(i)
152900090623     C                   EXSR      CHKNUM
153000090623     C                   IF        PiNum=*on
153100090623     C                   Z-ADD     PiVal         VABQFT
153200090623     C                   ELSE
153300090623     C                   SETON                                        32
153400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
153500090623     C                             + ' ' + 'VABQFT'
153600090623     C                   ENDIF
153700090623     C                   ENDIF
153800090623     C*** NCD
153900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
154000090623     C                   EVAL      PiStr=SkSplitCSV(i)
154100090623     C                   EXSR      CHKNUM
154200090623     C                   IF        PiInt=*on      AND
154300090623     C                             PiVal<=9999999
154400090623     C                   Z-ADD     PiVal         VABNCD
154500090623     C                   ELSE
154600090623     C                   SETON                                        32
154700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154800090623     C                             + ' ' + 'VABNCD'
154900090623     C                   ENDIF
155000090623     C                   ENDIF
155100090623     C*** NCA
155200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
155300090623     C                   EVAL      PiStr=SkSplitCSV(i)
155400090623     C                   EXSR      CHKNUM
155500090623     C                   IF        PiInt=*on      AND
155600090623     C                             PiVal<=9999999
155700090623     C                   Z-ADD     PiVal         VABNCA
155800090623     C                   ELSE
155900090623     C                   SETON                                        32
156000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156100090623     C                             + ' ' + 'VABNCA'
156200090623     C                   ENDIF
156300090623     C                   ENDIF
156400090623     C*** ZNC
156500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
156600090623     C                   EVAL      PiStr=SkSplitCSV(i)
156700090623     C                   EXSR      CHKNUM
156800090623     C                   IF        PiInt=*on AND
156900090623     C                             PiVal<=99
157000090623     C                   Z-ADD     PiVal         VABZNC
157100090623     C                   ELSE
157200090623     C                   SETON                                        32
157300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157400090623     C                             + ' ' + 'VABZNC'
157500090623     C                   ENDIF
157600090623     C                   ENDIF
157700090623     C*** DCR
157800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
157900090623     C                   EVAL      PiStr=SkSplitCSV(i)
158000090623     C                   EXSR      CHKNUM
158100090623     C                   IF        PiInt=*on       AND
158200090623     C                             PiVal<=99999999
158300090623     C                   Z-ADD     PiVal         VABDCR
158400090623     C                   ELSE
158500090623     C                   SETON                                        32
158600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
158700090623     C                             + ' ' + 'VABDCR'
158800090623     C                   ENDIF
158900090623     C                   ENDIF
159000090623     C*** HCR
159100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
159200090623     C                   EVAL      PiStr=SkSplitCSV(i)
159300090623     C                   EXSR      CHKNUM
159400090623     C                   IF        PiInt=*on   AND
159500090623     C                             PiVal<=9999
159600090623     C                   Z-ADD     PiVal         VABHCR
159700090623     C                   ELSE
159800090623     C                   SETON                                        32
159900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160000090623     C                             + ' ' + 'VABHCR'
160100090623     C                   ENDIF
160200090623     C                   ENDIF
160300090623     C*** VMD
160400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
160500090623     C                   EVAL      PiStr=SkSplitCSV(i)
160600090623     C                   EXSR      CHKNUM
160700090623     C                   IF        PiNum=*on
160800090623     C                   Z-ADD     PiVal         VABVMD
160900090623     C                   ELSE
161000090623     C                   SETON                                        32
161100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161200090623     C                             + ' ' + 'VABVMD'
161300090623     C                   ENDIF
161400090623     C                   ENDIF
161500090623     C*** ANT
161600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
161700090623     C                   EVAL      PiStr=SkSplitCSV(i)
161800090623     C                   EXSR      CHKNUM
161900090623     C                   IF        PiInt=*on        AND
162000090623     C                             PiVal<=999999999
162100090623     C                   Z-ADD     PiVal         VABANT
162200090623     C                   ELSE
162300090623     C                   SETON                                        32
162400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162500090623     C                             + ' ' + 'VABANT'
162600090623     C                   ENDIF
162700090623     C                   ENDIF
162800090623     C*** CMO
162900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
163000090623     C                   EVAL      PiStr=SkSplitCSV(i)
163100090623     C                   EXSR      CHKNUM
163200090623     C                   IF        PiInt=*on
163300090623     C                   Z-ADD     PiVal         Num5_0
163400090623     C                   MOVEL(p)  Num5_0        VABCMO
163500090623     C                   ELSE
163600090623     C                   SETON                                        32
163700090623     C                   EVAL      VABCMO = *zeros
163800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
163900090623     C                             + ' ' + 'VABCMO'
164000090623     C                   ENDIF
164100090623     C                   ENDIF
164200090623     C*
164300090911     C* Reperisco i campi del EDIVAT
164400090623     C*** VATNOT
164500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
164600090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
164700090623     C                   ENDIF
164800090623     C*** VATTRC
164900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
165000090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
165100090623     C                   ENDIF
165200090623     C*** VATNOT_A
165300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
165400090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
165500090623     C                   ENDIF
165600090623     C*** VATNOT_B
165700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
165800090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
165900090623     C                   ENDIF
166000111118     C*** VATNOT_E_4
166100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
166200111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
166300111118     C                   ENDIF
166400090623     C*
166500090623     C***  ===>  Gestione campi "particolari" dentro ciclo
166600090623     C*
166700090623     C*
166800090623     C***  <===  -----------------------------------------
166900090908     C*
167000090623     C*
167100090623     C                   ENDSR
167200090623     C***
167300090623
167400111118
167500111118
167600111118     C*----------------------------------------------------*
167700111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
167800111118     C*----------------------------------------------------*
167900111118     C     CARVALP       BEGSR
168000111118     C*
168100111118     C* Reperisco i campi del EDIVAT
168200111118     C*** VATNOT
168300111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
168400111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
168500111118     C                   ENDIF
168600111118     C*** VATTRC
168700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
168800111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
168900111118     C                   ENDIF
169000111118     C*** VATNOT_A
169100111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
169200111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
169300111118     C                   ENDIF
169400111118     C*** VATNOT_B
169500111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
169600111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
169700111118     C                   ENDIF
169800111216     C*** VATNOT_E_1
169900111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
170000111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
170100111216     C***                ENDIF
170200111118     C*** VATNOT_E_2
170300111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
170400111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
170500111118     C                   ENDIF
170600111118     C*** VATNOT_E_3
170700111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
170800111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
170900111118     C                   ENDIF
171000111118     C*** VATNOT_E_4
171100111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
171200111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
171300111118     C                   ENDIF
171400111118     C*
171500111118     C***  ===>  Gestione campi "particolari" dentro ciclo
171600111118     C*
171700111118     C*
171800111118     C***  <===  -----------------------------------------
171900111118     C*
172000111118     C*
172100111118     C                   ENDSR
172200111118     C***
172300010601
172400020204
172500020204     C*----------------------------------------------------*
172600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
172700020204     C*----------------------------------------------------*
172800020204     C     CHKIMPDIV     BEGSR
172900020204     C*
173000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
173100020204     C                   Z-ADD     *zeros        wrkDec            9 9
173200020204     C*
173300020204     C* Come prima cosa effettuo considerazioni sulla divisa
173400020204     C                   IF        vabIAS > *zeros
173500020204     C                   IF        vabVAS <> 'EUR'
173600020204     C                   EVAL      vabVAS =  'ITL'
173700020204     C                   ENDIF
173800020204     C                   ENDIF
173900020204     C*
174000020204     C                   IF        vabCAS > *zeros
174100020204     C                   IF        vabVCA <> 'EUR'
174200020204     C                   EVAL      vabVCA =  'ITL'
174300020204     C                   ENDIF
174400020204     C                   ENDIF
174500020204     C*
174600020204     C                   IF        vabVMD > *zeros
174700020204     C                   IF        vabVAD <> 'EUR'
174800020204     C                   EVAL      vabVAD =  'ITL'
174900020204     C                   ENDIF
175000020204     C                   ENDIF
175100020204     C*
175200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
175300020204     C                   Z-ADD     vabIAS        wrkDec
175400020204     C                   IF        wrkDec > *zeros
175500020204     C                   IF        vabVAS = 'ITL'
175600020204     C                   EVAL      vabIAS = *zeros
175700020204     C                   ENDIF
175800020204     C                   ENDIF
175900020204     C*
176000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
176100020204     C                   Z-ADD     vabCAS        wrkDec
176200020204     C                   IF        wrkDec > *zeros
176300020204     C                   IF        vabVCA = 'ITL'
176400020204     C                   EVAL      vabCAS = *zeros
176500020204     C                   ENDIF
176600020204     C                   ENDIF
176700020204     C*
176800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
176900020204     C                   Z-ADD     vabVMD        wrkDec
177000020204     C                   IF        wrkDec > *zeros
177100020204     C                   IF        vabVAD = 'ITL'
177200020204     C                   EVAL      vabVMD = *zeros
177300020204     C                   ENDIF
177400020204     C                   ENDIF
177500020204     C*
177600020204     C                   ENDSR
177700020204     C***
177800080617
177900080617
178000080617
178100080617     C*----------------------------------------------------*
178200080617     C*  CONTROLLO NUMERICITA' CAMPI
178300080617     C*----------------------------------------------------*
178400080617     C     CHKNUM        BEGSR
178500080617     C*
178600080617     C                   IF        PiDecChr = *blanks
178700080617     C                   EVAL      PiDecChr = CharNUM
178800080617     C                   ENDIF
178900080617     C*
179000080617     C                   callp     UBISNUM_Check(PiStr
179100080617     C                                          :PiDecChr
179200080617     C                                          :PiVal
179300080617     C                                          :PiNum
179400080617     C                                          :PiInt)
179500080617     C*
179600080617     C                   ENDSR
179700080617     C***
179800010330
179900010601
180000010601
180100010601
180200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
180300010601     C     repfil        BEGSR
180400010601     C*
180500010601     C                   if        invfil = *zeros and
180600010601     C                             depfil > *zeros and
180700010629     C                             (vinflg = *blanks or
180800010629     C                              vinflg = *zeros)
180900010601     C
181000010601     C                   eval      invfil = depfil
181100010601     C                   endif
181200010601     C*
181300010601     C                   if        depfil <> invfil and
181400010601     C                             invfil > *zeros
181500010601     C                   eval      flgMulti = '1'
181600010601     C                   if        vinflg = *blanks
181700010601     C                   add       1             cntNonEl
181800010601     C                   endif
181900010601     C                   endif
182000010601     C*
182100010601     C                   if        vinflg = '2'
182200010601     C                   eval      flgStato = '2'
182300010601     C                   endif
182400010601     C*
182500010601     C                   ENDSR
182600010601     C***
182700010601
182800010601
182900010601
183000010330
183100040119      /TITLE Invio dei dati al punto operativo.
183200040119     C     invio         BEGSR
183300040119     C*
183400101005     C* 1° invio EDIVAT
183500040119     C                   reset                   dscmz
183600040119     C                   move      vlrpoi        cmzdst
183700090911     C                   eval      cmzfld = 'EDIVATWR'
183800040119     C                   eval      cmzmbd = vlrhdl
183900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
184000040119     C***                if        prmfir = *blanks
184100090911     C                   eval      cmzfla = 'EDIVAT0F'
184200090911     C                   eval      cmzmba = 'EDIVAT0F'
184300040119     C***                else
184400040119     C***                eval      cmzfla = prmfir
184500040119     C***                eval      cmzmba = prmfir
184600040119     C***                endif
184700040119     C                   eval      cmznrr = *zeros
184800040119     C                   move      §ctrokvt      cmznrr
184900040119     C                   eval      cmzlba = vlrfl1
185000040119     C                   call(e)   'TIS711C'
185100040119     C                   parm                    dscmz
185200040119     C                   parm      *blanks       esito
185300040119     C                   if        %error
185400040119     C                             or cmzerr = '1'
185500040119     C                             or esito  = '1'
185600040119     C                   eval      wrkesito = '3'
185700040119     C                   else
185800040119     C*
185900101005     C* 2° invio EDIVAB
186000040119     C                   reset                   dscmz
186100040119     C                   move      vlrpoi        cmzdst
186200040119     C                   eval      cmzfld = vlrfou
186300040119     C                   eval      cmzmbd = vlrhdl
186400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
186500040119     C***                if        prmfir = *blanks
186600090911     C                   eval      cmzfla = 'EDIVAB0F'
186700090911     C                   eval      cmzmba = 'EDIVAB0F'
186800040119     C***                else
186900040119     C***                eval      cmzfla = prmfir
187000040119     C***                eval      cmzmba = prmfir
187100040119     C***                endif
187200040119     C                   eval      cmznrr = *zeros
187300040119     C                   move      §ctrokvb      cmznrr
187400040119     C                   eval      cmzlba = vlrfl1
187500040119     C                   call(e)   'TIS711C'
187600040119     C                   parm                    dscmz
187700040119     C                   parm      *blanks       esito
187800040119     C                   if        %error
187900040119     C                             or cmzerr = '1'
188000040119     C                             or esito  = '1'
188100040119     C                   eval      wrkesito = '3'
188200040119     C                   endif
188300040119     C                   endif
188400040119     C*
188500040119     C                   ENDSR
188600040119     C***
188700010601
188800010601
188900010601
189000010601
189100010601      /TITLE Invio dei dati al punto operativo.
189200010601     C     opeini        BEGSR
189300010601     C*
189400010601     C* Inizializzo flag e contatori operativi
189500010601     C                   movel     '0'           flgGiro           1
189600010601     C                   movel     '0'           flgMulti          1
189700010601     C                   movel     '1'           flgStato          1
189800010615     C                   movel     '0'           flgOk             1
189900010601     C                   z-add     *zeros        cntNonEl         10 0
190000010601     C                   z-add     *zeros        depfil            3 0
190100010601     C                   z-add     *zeros        invfil            3 0
190200010601     C*
190300010601     C                   ENDSR
190400010601     C***
190500070326
190600070326
190700070326
190800070326
190900080916     C     *pssr         BEGSR
191000070329     C*
191100080916     C                   if        %open(tivin00r)
191200080916     C                   close     tivin00r
191300080916     C                   endif
191400090911     C                   if        %open(edivabwr)
191500090911     C                   close     edivabwr
191600080916     C                   endif
191700090911     C                   if        %open(edivatwr)
191800090911     C                   close     edivatwr
191900080916     C                   endif
192000070326     C*
192100070326     C* Effettuo la chiamata al CLLE preposto
192200101005     C                   call(e)   'TITVEVTC'
192300080916     C                   parm                    parccm
192400080916     C                   parm                    parmbr
192500080916     C                   parm      '2'           paropz
192600070326     C*
192700080916     C                   eval      wrkesito = '2'
192800070404     C*
192900080916     C                   seton                                        LR
193000070326     C*
193100080916     C                   ENDSR     '*CANCL'
193200070326     C***
193300070326
193400070326
193500010330
193600010330
193700000613     C     *inzsr        BEGSR
193800990910     C*
193900990910     C     *entry        plist
194000990920     C                   parm                    tivlrds
194100990921     C                   parm      wrkesito      esito
194200000724     C                   parm                    prmlit
194300000710     C                   parm                    prmfir
194400010330     C*
194500010330     C* CALCOLA LA DATA CORRENTE
194600010330     C                   time                    wn14             14 0
194700090911     C                   movel     wn14          oracor            6 0          *ORA
194800110923     C                   z-add     *zeros        datcor            8 0
194900110923     C                   eval      datcor = %dec(%date() : *ISO)
195000111118     C*
195100111118     C* Forzo un numero CMR
195200111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
195300000613     C*
195400000613     C                   ENDSR
195500000613     C***
