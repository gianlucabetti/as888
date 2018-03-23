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
024900140415     C                   if        not *in33                                    * esludo estero
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
032600140415     C                   if        not *in33                                    * esludo estero
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
062500140715     C* PKN
062600140715     C                   IF        %subst(
062700140715     C                             %subst(vlrppt:posDaDft+1:
062800140715     C                             posADft-posDaDft-1):1:3)
062900140715     C                             = 'PKN'
063000140715     C                   EVAL      PiStr=%trim(%subst(
063100140715     C                             %subst(vlrppt:posDaDft+1:
063200140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300140715     C                   EXSR      CHKNUM
063400140715     C                   IF        PiNum=*on
063500140715     C                   Z-ADD     PiVal         wVABPKN           7 1
063600140715     C                   ENDIF
063700140715     C                   ENDIF
063800030822     C* VLB
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'VLB'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABVLB
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* QFT
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'QFT'
065600030822     C                   EVAL      PiStr=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   EXSR      CHKNUM
066000030822     C                   IF        PiNum=*on
066100030822     C                   Z-ADD     PiVal         VABQFT
066200030822     C                   ENDIF
066300030822     C                   ENDIF
066400030822     C* CBO
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'CBO'
066900030822     C                   EVAL      VABCBO=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* TSP
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'TSP'
067800030822     C                   EVAL      VABTSP=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VAS
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VAS'
068700030822     C                   EVAL      VABVAS=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VCA'
069600030822     C                   EVAL      VABVCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000070925     C* TIC
070100070925     C                   IF        %subst(
070200070925     C                             %subst(vlrppt:posDaDft+1:
070300070925     C                             posADft-posDaDft-1):1:3)
070400070925     C                             = 'TIC'
070500070925     C                   EVAL      VABTIC=%trim(%subst(
070600070925     C                             %subst(vlrppt:posDaDft+1:
070700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800070925     C                   ENDIF
070900030822     C* GCA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'GCA'
071400030822     C                   EVAL      VABGCA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* CTM
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'CTM'
072300030822     C                   EVAL      VABCTM=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* FFD
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'FFD'
073200030822     C                   EVAL      VABFFD=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* VAD
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'VAD'
074100030822     C                   EVAL      VABVAD=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GMA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GMA'
075000030822     C                   EVAL      VABGMA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GGA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GGA'
075900030822     C                   EVAL      VABGGA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GVA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GVA'
076800030822     C                   EVAL      VABGVA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* TC1
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'TC1'
077700030822     C                   EVAL      VABTC1=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* TC2
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'TC2'
078600030822     C                   EVAL      VABTC2=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000040714     C* VATTRC
079100040714     C                   IF        %subst(
079200040714     C                             %subst(vlrppt:posDaDft+1:
079300040714     C                             posADft-posDaDft-1):1:3)
079400040714     C                             = 'TRC'
079500040714     C                   EVAL      VATTRC=%trim(%subst(
079600040714     C                             %subst(vlrppt:posDaDft+1:
079700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800040714     C                   ENDIF
079900030822     C* ...
080000030822     C                   ENDIF
080100030822     C                   ENDDO
080200020204     C*
080300010330     C                   ENDSR
080400010607     C*----------------------------------------------------*
080500090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
080600010607     C*----------------------------------------------------*
080700090623     C     IMPVAB        BEGSR
080800030515     C*
080900010607     C                   MOVEL     datcor        VABAAS
081000010607     C                   MOVE      datcor        VABMGS
081100010607     C*
081200030715     C**********
081300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081400030715     C**********
081500030715     C                   IF        wGiro = *zeros
081600080716     C*
081700080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
081800080716     C                   EVAL      depvinDTA = vinDTA
081900040113     C*
082000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082100110923     C                   IF        header <> *blanks
082200110923     C                   EVAL      vindta = header
082300070823     C     ';':CharCSV   XLATE     vindta        vindta
082400040510     C                   ENDIF
082500080626     C*
082600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082700031201     C     minu:maiu     XLATE     vindta        vindta
082800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082900030715     C                   DOW       posDa <= %len(%trim(vindta))
083000030715     C*
083100030715     C* Gestisco il 1° campo
083200030715     C                   IF        i = 1
083300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083400030715     C                   EVAL      posDa = 1
083500030715     C* Eseguo lo scan x trovare la fine del primo campo
083600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083700030715     C* A questo punto "estraggo" il campo corrente
083800111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
083900110923     C                                         %subst(vindta:posDa:(posA-posDa))
084000030715     C* X i campi successivi al 1°
084100030715     C                   ELSE
084200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084300030715     C                   EVAL      posDa = posA + 1
084400030715     C* Eseguo lo scan x trovare la fine del campo corrente
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* Gestisco l'ultimo campo
084700030715     C                   IF        posA = *zeros
084800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084900030715     C* A questo punto "estraggo" il campo corrente
085000111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
085100110923     C                                         %subst(vindta:posDa:(posA-posDa))
085200030715     C                   LEAVE
085300030715     C                   ELSE
085400030715     C* A questo punto "estraggo" il campo corrente
085500111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
085600110923     C                                         %subst(vindta:posDa:(posA-posDa))
085700030715     C                   ENDIF
085800030715     C                   ENDIF
085900030715     C* Incremento il contatore d campo
086000030715     C                   EVAL      i = i +1
086100030715     C                   ENDDO
086200030715     C*
086300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086400030715     C* delimitazione testo
086500030715     C                   EVAL      i = 1
086600111118     C                   DOW       i < %elem(SkSplitFLDH)
086700111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
086800111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
086900030715     C                   EVAL      i = i + 1
087000030715     C                   ENDDO
087100030715     C                   EVAL      wGiro = 1
087200080716     C* Ripristino la 1° riga originale
087300080716     C                   EVAL      vinDTA = depvinDTA
087400030715     C                   ELSE
087500030715     C                   EVAL      wGiro = 2
087600031204     C**********
087700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087800031204     C* d separatore campo
087900031204     C**********
088000031204     C                   EVAL      vindta = %trim(vindta)
088100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088200031204     C                   EVAL      vindta = %subst(vindta:2)
088300031204     C                   ENDDO
088400031204     C*
088500031204     C                   Z-ADD     *zeros        lunghInput        4 0
088600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089000031204     C                   ENDDO
089100030715     C**********
089200030715     C* Effettuo lo split del campo dati d input
089300030715     C**********
089400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089500030715     C                   DOW       posDa <= %len(%trim(vindta))
089600030715     C*
089700030715     C* Gestisco il 1° campo
089800030715     C                   IF        i = 1
089900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090000030715     C                   EVAL      posDa = 1
090100030715     C* Eseguo lo scan x trovare la fine del primo campo
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* A questo punto "estraggo" il campo corrente
090400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090500030715     C                                                    (posA-posDa))
090600030715     C* X i campi successivi al 1°
090700030715     C                   ELSE
090800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090900030715     C                   EVAL      posDa = posA + 1
091000031204     C* Verifico che nn vi sia il campo nn valorizzato
091100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091200031204     C* Se campo nn valorizzato skippo al prossimo
091300031204     C                   EVAL      SkSplitCSV(i) = *blanks
091400031204     C                   EVAL      posA  = posA + 1
091500031204     C                   ELSE
091600030715     C* Eseguo lo scan x trovare la fine del campo corrente
091700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091800030715     C* Gestisco l'ultimo campo
091900030715     C                   IF        posA = *zeros
092000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   LEAVE
092500030715     C                   ELSE
092600030715     C* A questo punto "estraggo" il campo corrente
092700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092800030715     C                                                    (posA-posDa))
092900030715     C                   ENDIF
093000031204     C                   ENDIF
093100030715     C                   ENDIF
093200030715     C* Incremento il contatore d campo
093300030715     C                   EVAL      i = i +1
093400030715     C                   ENDDO
093500030715     C*
093600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093700030715     C* delimitazione testo
093800030715     C                   EVAL      i = 1
093900030715     C                   DOW       i < %elem(SkSplitCSV)
094000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094200030715     C                   EVAL      i = i + 1
094300030715     C                   ENDDO
094400030715     C*
094500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094600030715     C                   EVAL      i = 1
094700111118     C                   DOW       i < %elem(SkSplitFLDH)
094800030715     C*
094900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095000111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
095100090623     C                             SkSplitCSV(i) <> *blanks
095200090623     C*
095300111118     C                   exsr      CARVALH
095400040510     C*
095500030715     C                   ENDIF
095600030715     C                   EVAL      i = i + 1
095700030715     C                   ENDDO
095800070925     C*
095900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
096000080125     C*
096100111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
096200111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
096300140715     C*
096400140715     C                   IF        VABPKB = *zeros
096500140715     C                   EVAL      VABPKB = wVABPKN * VABNCL
096600140715     C                   ENDIF
096700070925     C*
096800071029     C***  <===  -----------------------------------------
096900070925     C*
097000070925     C* Considerazioni finali su CBO/CAS
097100080125     C                   IF        wFlgCAS = '1'
097200070925     C                   IF        VABCBO = '1'
097300070925     C                   EVAL      VABCBO = '4'
097400070925     C                   ENDIF
097500070925     C                   IF        VABCBO = '2'
097600070925     C                   EVAL      VABCBO = '6'
097700070925     C                   ENDIF
097800070925     C                   ENDIF
097900070925     C***  <===  ----------------------------
098000040510     C*
098100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
098200040510     C                   IF        VABNSP = *zeros
098300040510     C                   SETON                                        31
098400040510     C                   ENDIF
098500040715     C*
098600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
098700040715     C                   IF        VABLNP = *zeros
098800040715     C                   SETON                                        31
098900040715     C                   ENDIF
099000040510     C*
099100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
099200040510     C                   IF        VABRMN = *zeros
099300040510     C                   Z-ADD     VABNSP        VABRMN
099400040510     C                   ENDIF
099500040510     C*
099600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
099700040510     C                   IF        VABRMA = *blanks
099800040714     C                   MOVEL     VABRMN        VABRMA
099900040510     C                   ENDIF
100000040128     C*
100100040510     C* Se provincia nn valorizzata la reperisco
100200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
100300040510     C                   IF        VABPRD = *blanks
100400040128     C                   CLEAR                   TISI95DS
100500040128     C                   EVAL      I95TCN = '3'
100600040128     C                   Z-ADD     datcor        I95DAT
100700040128     C                   EVAL      I95NAR = VABNZD
100800040128     C                   EVAL      I95CAP = VABCAD
100900040128     C                   EVAL      I95LOC = VABLOD
101000040128     C                   CALL      'TISI95R'
101100040128     C                   PARM                    TISI95DS
101200040128     C                   EVAL      VABPRD = O95PRV
101300040510     C                   ENDIF
101400020204     C*
101500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
101600020204     C                   EXSR      CHKIMPDIV
101700030715     C                   ENDIF
101800020204     C*
101900010607     C                   ENDSR
102000010607     C*----------------------------------------------------*
102100090623
102200090623
102300090623
102400090623     C*----------------------------------------------------*
102500101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
102600090623     C*----------------------------------------------------*
102700101005     C     IMPVAT        BEGSR
102800090623     C*
102900090623     C**********
103000090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
103100090623     C**********
103200090623     C                   IF        wGiro = *zeros
103300090623     C*
103400090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
103500090623     C                   EVAL      depvinDTA = vinDTA
103600090623     C*
103700090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
103800110923     C                   IF        parcel <> *blanks
103900110923     C                   EVAL      vindta = parcel
104000090623     C     ';':CharCSV   XLATE     vindta        vindta
104100090623     C                   ENDIF
104200090623     C*
104300090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
104400090623     C     minu:maiu     XLATE     vindta        vindta
104500090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
104600090623     C                   DOW       posDa <= %len(%trim(vindta))
104700090623     C*
104800090623     C* Gestisco il 1° campo
104900090623     C                   IF        i = 1
105000090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
105100090623     C                   EVAL      posDa = 1
105200090623     C* Eseguo lo scan x trovare la fine del primo campo
105300090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
105400090623     C* A questo punto "estraggo" il campo corrente
105500111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
105600110923     C                                         %subst(vindta:posDa:(posA-posDa))
105700090623     C* X i campi successivi al 1°
105800090623     C                   ELSE
105900090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
106000090623     C                   EVAL      posDa = posA + 1
106100090623     C* Eseguo lo scan x trovare la fine del campo corrente
106200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
106300090623     C* Gestisco l'ultimo campo
106400090623     C                   IF        posA = *zeros
106500090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
106600090623     C* A questo punto "estraggo" il campo corrente
106700111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
106800110923     C                                         %subst(vindta:posDa:(posA-posDa))
106900090623     C                   LEAVE
107000090623     C                   ELSE
107100090623     C* A questo punto "estraggo" il campo corrente
107200111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
107300110923     C                                         %subst(vindta:posDa:(posA-posDa))
107400090623     C                   ENDIF
107500090623     C                   ENDIF
107600090623     C* Incremento il contatore d campo
107700090623     C                   EVAL      i = i +1
107800090623     C                   ENDDO
107900090623     C*
108000090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
108100090623     C* delimitazione testo
108200090623     C                   EVAL      i = 1
108300111118     C                   DOW       i < %elem(SkSplitFLDP)
108400111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
108500111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
108600090623     C                   EVAL      i = i + 1
108700090623     C                   ENDDO
108800090623     C                   EVAL      wGiro = 1
108900090623     C* Ripristino la 1° riga originale
109000090623     C                   EVAL      vinDTA = depvinDTA
109100090623     C                   ELSE
109200090623     C                   EVAL      wGiro = 2
109300090623     C**********
109400090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
109500090623     C* d separatore campo
109600090623     C**********
109700090623     C                   EVAL      vindta = %trim(vindta)
109800090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
109900090623     C                   EVAL      vindta = %subst(vindta:2)
110000090623     C                   ENDDO
110100090623     C*
110200090623     C                   Z-ADD     *zeros        lunghInput        4 0
110300090623     C                   EVAL      lunghInput = %len(%trim(vindta))
110400090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
110500090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
110600090623     C                   EVAL      lunghInput = %len(%trim(vindta))
110700090623     C                   ENDDO
110800090623     C**********
110900090623     C* Effettuo lo split del campo dati d input
111000090623     C**********
111100090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
111200090623     C                   DOW       posDa <= %len(%trim(vindta))
111300090623     C*
111400090623     C* Gestisco il 1° campo
111500090623     C                   IF        i = 1
111600090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
111700090623     C                   EVAL      posDa = 1
111800090623     C* Eseguo lo scan x trovare la fine del primo campo
111900090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
112000090623     C* A questo punto "estraggo" il campo corrente
112100090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
112200090623     C                                                    (posA-posDa))
112300090623     C* X i campi successivi al 1°
112400090623     C                   ELSE
112500090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
112600090623     C                   EVAL      posDa = posA + 1
112700090623     C* Verifico che nn vi sia il campo nn valorizzato
112800090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
112900090623     C* Se campo nn valorizzato skippo al prossimo
113000090623     C                   EVAL      SkSplitCSV(i) = *blanks
113100090623     C                   EVAL      posA  = posA + 1
113200090623     C                   ELSE
113300090623     C* Eseguo lo scan x trovare la fine del campo corrente
113400090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
113500090623     C* Gestisco l'ultimo campo
113600090623     C                   IF        posA = *zeros
113700090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
113800090623     C* A questo punto "estraggo" il campo corrente
113900090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
114000090623     C                                                    (posA-posDa))
114100090623     C                   LEAVE
114200090623     C                   ELSE
114300090623     C* A questo punto "estraggo" il campo corrente
114400090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
114500090623     C                                                    (posA-posDa))
114600090623     C                   ENDIF
114700090623     C                   ENDIF
114800090623     C                   ENDIF
114900090623     C* Incremento il contatore d campo
115000090623     C                   EVAL      i = i +1
115100090623     C                   ENDDO
115200090623     C*
115300090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
115400090623     C* delimitazione testo
115500090623     C                   EVAL      i = 1
115600090623     C                   DOW       i < %elem(SkSplitCSV)
115700090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
115800090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
115900090623     C                   EVAL      i = i + 1
116000090623     C                   ENDDO
116100090623     C*
116200090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
116300090623     C                   EVAL      i = 1
116400111118     C                   DOW       i < %elem(SkSplitFLDP)
116500090623     C*
116600090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
116700111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
116800090623     C                             SkSplitCSV(i) <> *blanks
116900090623     C*
117000111118     C                   exsr      CARVALP
117100090623     C*
117200090623     C                   ENDIF
117300090623     C                   EVAL      i = i + 1
117400090623     C                   ENDDO
117500090623     C*
117600090623     C***  ===>  Gestione campi "particolari" fuori ciclo
117700111118     C*
117800111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
117900111118     C                                         %trim(wVATNOT_E_2) +
118000111118     C                                         %trim(wVATNOT_E_3) +
118100111118     C                                         %trim(wVATNOT_E_4)
118200090623     C*
118300090623     C***  <===  -----------------------------------------
118400090623     C*
118500090623     C                   ENDIF
118600090623     C*
118700090623     C                   ENDSR
118800090623     C*----------------------------------------------------*
118900090623
119000090623
119100090623     C*----------------------------------------------------*
119200090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
119300090623     C*----------------------------------------------------*
119400111118     C     CARVALH       BEGSR
119500090623     C*
119600090623     C*** ATB
119700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
119800090623     C                   EVAL      VABATB = SkSplitCSV(i)
119900090623     C                   ENDIF
120000090623     C*** RSD
120100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
120200090623     C                   EVAL      VABRSD = SkSplitCSV(i)
120300090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
120400090623     C     '@':'A'       XLATE     VABRSD        VABRSD
120500090623     C* ==
120600090623     C                   ENDIF
120700090623     C*** RD2
120800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
120900090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
121000090623     C                   ENDIF
121100130530     C*** IND 1 (HDR)
121200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
121300090623     C                   EVAL      VABIND = SkSplitCSV(i)
121400090623     C                   ENDIF
121500130530     C*** IND 2 (HDR)
121600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
121700111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
121800111117     C                   ENDIF
121900130530     C*** IND 1
122000130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
122100130530     C                   EVAL      VABIND = SkSplitCSV(i)
122200130530     C                   ENDIF
122300130530     C*** IND 2
122400130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
122500130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
122600130530     C                   ENDIF
122700111117     C*** CAD
122800111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
122900111117     C                   EVAL      VABCAD = SkSplitCSV(i)
123000111216     C                   MOVEL     *blanks       wAlfa7            7
123100111216     C                   MOVEL     *zeros        wAlfa7z           7
123200111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
123300111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
123400111117     C                   ENDIF
123500090623     C*** LOD
123600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
123700090623     C                   EVAL      VABLOD = SkSplitCSV(i)
123800090623     C                   ENDIF
123900111117     C*** PRD
124000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
124100090623     C                   EVAL      VABPRD = SkSplitCSV(i)
124200090623     C                   ENDIF
124300090623     C*** NZD
124400140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
124500140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
124600140415     C                   SETON                                        33
124700090623     C                   ENDIF
124800140415     C                   ENDIF
124900090623     C*** RMA
125000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
125100130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
125200130611     C                   EVAL      VABRMA = SkSplitCSV(i)
125300090623     C                   ENDIF
125400111117     C*** NOT
125500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
125600111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
125700090623     C                   ENDIF
125800111117     C*** NT2
125900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
126000111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
126100090623     C                   ENDIF
126200090623     C*** VCA
126300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
126400090623     C                   EVAL      VABVCA = SkSplitCSV(i)
126500090623     C                   ENDIF
126600090623     C*** VAS
126700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
126800090623     C                   EVAL      VABVAS = SkSplitCSV(i)
126900090623     C                   ENDIF
127000090623     C*** FFD
127100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
127200090623     C                   EVAL      VABFFD = SkSplitCSV(i)
127300090623     C                   ENDIF
127400090623     C*** GC1
127500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
127600090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
127700090623     C                   ENDIF
127800090623     C*** GC2
127900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
128000090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
128100090623     C                   ENDIF
128200090623     C*** TSP
128300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
128400090623     C                   EVAL      VABTSP = SkSplitCSV(i)
128500090623     C                   ENDIF
128600090623     C*** CBO
128700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
128800090623     C                   EVAL      VABCBO = SkSplitCSV(i)
128900090623     C                   ENDIF
129000090623     C*** NAS
129100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
129200090623     C                   EVAL      VABNAS = SkSplitCSV(i)
129300090623     C                   ENDIF
129400090623     C*** TIC
129500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
129600090623     C                   EVAL      VABTIC = SkSplitCSV(i)
129700090623     C                   ENDIF
129800090623     C*** GCA
129900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
130000090623     C                   EVAL      VABGCA = SkSplitCSV(i)
130100090623     C                   ENDIF
130200090623     C*** XCO
130300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
130400090623     C                   EVAL      VABXCO = SkSplitCSV(i)
130500090623     C                   ENDIF
130600090623     C*** CTM
130700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
130800090623     C                   EVAL      VABCTM = SkSplitCSV(i)
130900090623     C                   ENDIF
131000090623     C*** TCR
131100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
131200090623     C                   EVAL      VABTCR = SkSplitCSV(i)
131300090623     C                   ENDIF
131400090623     C*** CTS
131500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
131600090623     C                   EVAL      VABCTS = SkSplitCSV(i)
131700090623     C                   ENDIF
131800090623     C*** FTM
131900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
132000090623     C                   EVAL      VABFTM = SkSplitCSV(i)
132100090623     C                   ENDIF
132200090623     C*** VAD
132300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
132400090623     C                   EVAL      VABVAD = SkSplitCSV(i)
132500090623     C                   ENDIF
132600090623     C*** GMA
132700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
132800090623     C                   EVAL      VABGMA = SkSplitCSV(i)
132900090623     C                   ENDIF
133000090623     C*** GGA
133100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
133200090623     C                   EVAL      VABGGA = SkSplitCSV(i)
133300090623     C                   ENDIF
133400090623     C*** GVA
133500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
133600090623     C                   EVAL      VABGVA = SkSplitCSV(i)
133700090623     C                   ENDIF
133800090623     C*** TC1
133900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
134000090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
134100090623     C                   ENDIF
134200090623     C*** TC2
134300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
134400090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
134500090623     C                   ENDIF
134600090623     C*** SCL
134700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
134800090623     C                   EVAL      VABSCL = SkSplitCSV(i)
134900090623     C                   ENDIF
135000090623     C*** RMO
135100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
135200090623     C                   EVAL      VABRMO = SkSplitCSV(i)
135300090623     C                   ENDIF
135400090623     C*** NMO
135500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
135600090623     C                   EVAL      VABNMO = SkSplitCSV(i)
135700090623     C                   ENDIF
135800090623     C*
135900090623     C* Reperisco quindi i campi numerici...
136000111117     C*** NSP
136100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
136200111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
136300090623     C                   EXSR      CHKNUM
136400090623     C                   IF        PiInt=*on
136500090623     C                   Z-ADD     PiVal         VABNSP
136600090623     C                   ELSE
136700090623     C                   SETON                                        31
136800090623     C                   EVAL      VABNSP = *zeros
136900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
137000090623     C                             + ' ' + 'VABNSP'
137100090623     C                   ENDIF
137200090623     C                   ENDIF
137300090623     C*** RMN
137400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
137500111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
137600090623     C                   EXSR      CHKNUM
137700090623     C                   IF        PiInt=*on
137800090623     C                   Z-ADD     PiVal         VABRMN
137900090623     C                   ELSE
138000090623     C                   SETON                                        32
138100090623     C                   EVAL      VABRMN = *zeros
138200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
138300090623     C                             + ' ' + 'VABRMN'
138400090623     C                   ENDIF
138500090623     C                   ENDIF
138600090623     C*** CAD
138700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
138800090623     C                   EVAL      PiStr=SkSplitCSV(i)
138900090623     C                   EXSR      CHKNUM
139000090623     C                   IF        PiInt=*on
139100090623     C                   Z-ADD     PiVal         Num5_0
139200090623     C                   MOVEL(p)  Num5_0        VABCAD
139300090623     C                   ELSE
139400090623     C                   EVAL      VABCAD = SkSplitCSV(i)
139500090623     C                   SETON                                        32
139600090623     C                   EVAL      VABCAD = *zeros
139700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
139800090623     C                             + ' ' + 'VABCAD'
139900090623     C                   ENDIF
140000090623     C                   ENDIF
140100090623     C*** NCL
140200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
140300090623     C                   EVAL      PiStr=SkSplitCSV(i)
140400090623     C                   EXSR      CHKNUM
140500090623     C                   IF        PiInt=*on
140600090623     C                   Z-ADD     PiVal         VABNCL
140700090623     C                   ELSE
140800090623     C                   SETON                                        32
140900090623     C                   EVAL      VABNCL = *zeros
141000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
141100090623     C                             + ' ' + 'VABNCL'
141200090623     C                   ENDIF
141300090623     C                   ENDIF
141400090623     C*** PKB
141500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
141600090623     C                   EVAL      PiStr=SkSplitCSV(i)
141700090623     C                   EXSR      CHKNUM
141800090623     C                   IF        PiNum=*on
141900130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
142000090623     C                   Z-ADD     PiVal         VABPKB
142100090623     C                   ELSE
142200090623     C                   SETON                                        32
142300090623     C                   EVAL      VABPKB = *zeros
142400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142500090623     C                             + ' ' + 'VABPKB'
142600090623     C                   ENDIF
142700090623     C                   ENDIF
142800090623     C*** CAS
142900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
143000090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
143100090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
143200090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
143300090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
143400090623     C                   EVAL      wFlgCAS = '1'
143500090623     C                   ENDIF
143600090623     C                   EVAL      PiStr=SkSplitCSV(i)
143700090623     C                   EXSR      CHKNUM
143800090623     C                   IF        PiNum=*on
143900090623     C                   Z-ADD     PiVal         VABCAS
144000090623     C                   ELSE
144100090623     C                   SETON                                        32
144200090623     C                   EVAL      VABCAS = *zeros
144300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144400090623     C                             + ' ' + 'VABCAS'
144500090623     C                   ENDIF
144600090623     C                   ENDIF
144700090623     C*** CCM
144800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
144900090623     C                   EVAL      PiStr=SkSplitCSV(i)
145000090623     C                   EXSR      CHKNUM
145100090623     C                   IF        PiInt=*on      AND
145200090623     C                             PiVal<=9999999 AND
145300090623     C                             PiVal>*zeros
145400090623     C                   Z-ADD     PiVal         VABCCM
145500090623     C                   ELSE
145600090623     C                   SETON                                        32
145700090623     C                   EVAL      VABCCM = *zeros
145800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
145900090623     C                             + ' ' + 'VABCCM'
146000090623     C                   ENDIF
146100090623     C                   ENDIF
146200090623     C*** LNP
146300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
146400090623     C                   EVAL      PiStr=SkSplitCSV(i)
146500090623     C                   EXSR      CHKNUM
146600090623     C                   IF        PiInt=*on    AND
146700090623     C                             PiVal<=999   AND
146800090623     C                             PiVal>*zeros
146900090623     C                   Z-ADD     PiVal         VABLNP
147000090623     C                   ELSE
147100090623     C                   SETON                                        32
147200090623     C                   EVAL      VABLNP = *zeros
147300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
147400090623     C                             + ' ' + 'VABLNP'
147500090623     C                   ENDIF
147600090623     C                   ENDIF
147700090623     C*** NRS
147800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
147900090623     C                   EVAL      PiStr=SkSplitCSV(i)
148000090623     C                   EXSR      CHKNUM
148100090623     C                   IF        PiInt=*on AND
148200090623     C                             PiVal<=99
148300090623     C                   Z-ADD     PiVal         VABNRS
148400090623     C                   ELSE
148500090623     C                   SETON                                        32
148600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148700090623     C                             + ' ' + 'VABNRS'
148800090623     C                   ENDIF
148900090623     C                   ENDIF
149000090623     C*** LNA
149100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
149200090623     C                   EVAL      PiStr=SkSplitCSV(i)
149300090623     C                   EXSR      CHKNUM
149400090623     C                   IF        PiInt=*on    AND
149500090623     C                             PiVal<=999   AND
149600090623     C                             PiVal>*zeros
149700090623     C                   Z-ADD     PiVal         VABLNA
149800090623     C                   ELSE
149900090623     C                   SETON                                        32
150000090623     C                   EVAL      VABLNA = *zeros
150100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
150200090623     C                             + ' ' + 'VABLNA'
150300090623     C                   ENDIF
150400090623     C                   ENDIF
150500090623     C*** CTR
150600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
150700090623     C                   EVAL      PiStr=SkSplitCSV(i)
150800090623     C                   EXSR      CHKNUM
150900090623     C                   IF        PiInt=*on  AND
151000090623     C                             PiVal<=999
151100090623     C                   Z-ADD     PiVal         VABCTR
151200090623     C                   ELSE
151300090623     C                   SETON                                        32
151400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151500090623     C                             + ' ' + 'VABCTR'
151600090623     C                   ENDIF
151700090623     C                   ENDIF
151800090623     C*** IAS
151900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
152000090623     C                   EVAL      PiStr=SkSplitCSV(i)
152100090623     C                   EXSR      CHKNUM
152200090623     C                   IF        PiNum=*on
152300090623     C                   Z-ADD     PiVal         VABIAS
152400090623     C                   ELSE
152500090623     C                   SETON                                        32
152600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152700090623     C                             + ' ' + 'VABIAS'
152800090623     C                   ENDIF
152900090623     C                   ENDIF
153000090623     C*** VLB
153100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
153200090623     C                   EVAL      PiStr=SkSplitCSV(i)
153300090623     C                   EXSR      CHKNUM
153400090623     C                   IF        PiNum=*on
153500111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
153600090623     C                   Z-ADD     PiVal         VABVLB
153700090623     C                   ELSE
153800090623     C                   SETON                                        32
153900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154000090623     C                             + ' ' + 'VABVLB'
154100090623     C                   ENDIF
154200090623     C                   ENDIF
154300090623     C*** QFT
154400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
154500090623     C                   EVAL      PiStr=SkSplitCSV(i)
154600090623     C                   EXSR      CHKNUM
154700090623     C                   IF        PiNum=*on
154800090623     C                   Z-ADD     PiVal         VABQFT
154900090623     C                   ELSE
155000090623     C                   SETON                                        32
155100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155200090623     C                             + ' ' + 'VABQFT'
155300090623     C                   ENDIF
155400090623     C                   ENDIF
155500090623     C*** NCD
155600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
155700090623     C                   EVAL      PiStr=SkSplitCSV(i)
155800090623     C                   EXSR      CHKNUM
155900090623     C                   IF        PiInt=*on      AND
156000090623     C                             PiVal<=9999999
156100090623     C                   Z-ADD     PiVal         VABNCD
156200090623     C                   ELSE
156300090623     C                   SETON                                        32
156400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156500090623     C                             + ' ' + 'VABNCD'
156600090623     C                   ENDIF
156700090623     C                   ENDIF
156800090623     C*** NCA
156900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
157000090623     C                   EVAL      PiStr=SkSplitCSV(i)
157100090623     C                   EXSR      CHKNUM
157200090623     C                   IF        PiInt=*on      AND
157300090623     C                             PiVal<=9999999
157400090623     C                   Z-ADD     PiVal         VABNCA
157500090623     C                   ELSE
157600090623     C                   SETON                                        32
157700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157800090623     C                             + ' ' + 'VABNCA'
157900090623     C                   ENDIF
158000090623     C                   ENDIF
158100090623     C*** ZNC
158200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
158300090623     C                   EVAL      PiStr=SkSplitCSV(i)
158400090623     C                   EXSR      CHKNUM
158500090623     C                   IF        PiInt=*on AND
158600090623     C                             PiVal<=99
158700090623     C                   Z-ADD     PiVal         VABZNC
158800090623     C                   ELSE
158900090623     C                   SETON                                        32
159000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159100090623     C                             + ' ' + 'VABZNC'
159200090623     C                   ENDIF
159300090623     C                   ENDIF
159400090623     C*** DCR
159500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
159600090623     C                   EVAL      PiStr=SkSplitCSV(i)
159700090623     C                   EXSR      CHKNUM
159800090623     C                   IF        PiInt=*on       AND
159900090623     C                             PiVal<=99999999
160000090623     C                   Z-ADD     PiVal         VABDCR
160100090623     C                   ELSE
160200090623     C                   SETON                                        32
160300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160400090623     C                             + ' ' + 'VABDCR'
160500090623     C                   ENDIF
160600090623     C                   ENDIF
160700090623     C*** HCR
160800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
160900090623     C                   EVAL      PiStr=SkSplitCSV(i)
161000090623     C                   EXSR      CHKNUM
161100090623     C                   IF        PiInt=*on   AND
161200090623     C                             PiVal<=9999
161300090623     C                   Z-ADD     PiVal         VABHCR
161400090623     C                   ELSE
161500090623     C                   SETON                                        32
161600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161700090623     C                             + ' ' + 'VABHCR'
161800090623     C                   ENDIF
161900090623     C                   ENDIF
162000090623     C*** VMD
162100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
162200090623     C                   EVAL      PiStr=SkSplitCSV(i)
162300090623     C                   EXSR      CHKNUM
162400090623     C                   IF        PiNum=*on
162500090623     C                   Z-ADD     PiVal         VABVMD
162600090623     C                   ELSE
162700090623     C                   SETON                                        32
162800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162900090623     C                             + ' ' + 'VABVMD'
163000090623     C                   ENDIF
163100090623     C                   ENDIF
163200090623     C*** ANT
163300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
163400090623     C                   EVAL      PiStr=SkSplitCSV(i)
163500090623     C                   EXSR      CHKNUM
163600090623     C                   IF        PiInt=*on        AND
163700090623     C                             PiVal<=999999999
163800090623     C                   Z-ADD     PiVal         VABANT
163900090623     C                   ELSE
164000090623     C                   SETON                                        32
164100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164200090623     C                             + ' ' + 'VABANT'
164300090623     C                   ENDIF
164400090623     C                   ENDIF
164500090623     C*** CMO
164600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
164700090623     C                   EVAL      PiStr=SkSplitCSV(i)
164800090623     C                   EXSR      CHKNUM
164900090623     C                   IF        PiInt=*on
165000090623     C                   Z-ADD     PiVal         Num5_0
165100090623     C                   MOVEL(p)  Num5_0        VABCMO
165200090623     C                   ELSE
165300090623     C                   SETON                                        32
165400090623     C                   EVAL      VABCMO = *zeros
165500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165600090623     C                             + ' ' + 'VABCMO'
165700090623     C                   ENDIF
165800090623     C                   ENDIF
165900090623     C*
166000090911     C* Reperisco i campi del EDIVAT
166100090623     C*** VATNOT
166200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
166300090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
166400090623     C                   ENDIF
166500090623     C*** VATTRC
166600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
166700090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
166800090623     C                   ENDIF
166900090623     C*** VATNOT_A
167000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
167100090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
167200090623     C                   ENDIF
167300090623     C*** VATNOT_B
167400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
167500090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
167600090623     C                   ENDIF
167700111118     C*** VATNOT_E_4
167800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
167900111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
168000111118     C                   ENDIF
168100090623     C*
168200090623     C***  ===>  Gestione campi "particolari" dentro ciclo
168300090623     C*
168400090623     C*
168500090623     C***  <===  -----------------------------------------
168600090908     C*
168700090623     C*
168800090623     C                   ENDSR
168900090623     C***
169000090623
169100111118
169200111118
169300111118     C*----------------------------------------------------*
169400111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
169500111118     C*----------------------------------------------------*
169600111118     C     CARVALP       BEGSR
169700111118     C*
169800111118     C* Reperisco i campi del EDIVAT
169900111118     C*** VATNOT
170000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
170100111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
170200111118     C                   ENDIF
170300111118     C*** VATTRC
170400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
170500111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
170600111118     C                   ENDIF
170700111118     C*** VATNOT_A
170800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
170900111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
171000111118     C                   ENDIF
171100111118     C*** VATNOT_B
171200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
171300111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
171400111118     C                   ENDIF
171500111216     C*** VATNOT_E_1
171600111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
171700111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
171800111216     C***                ENDIF
171900111118     C*** VATNOT_E_2
172000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
172100111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
172200111118     C                   ENDIF
172300111118     C*** VATNOT_E_3
172400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
172500111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
172600111118     C                   ENDIF
172700111118     C*** VATNOT_E_4
172800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
172900111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
173000111118     C                   ENDIF
173100111118     C*
173200111118     C***  ===>  Gestione campi "particolari" dentro ciclo
173300111118     C*
173400111118     C*
173500111118     C***  <===  -----------------------------------------
173600111118     C*
173700111118     C*
173800111118     C                   ENDSR
173900111118     C***
174000010601
174100020204
174200020204     C*----------------------------------------------------*
174300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
174400020204     C*----------------------------------------------------*
174500020204     C     CHKIMPDIV     BEGSR
174600020204     C*
174700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
174800020204     C                   Z-ADD     *zeros        wrkDec            9 9
174900020204     C*
175000020204     C* Come prima cosa effettuo considerazioni sulla divisa
175100020204     C                   IF        vabIAS > *zeros
175200020204     C                   IF        vabVAS <> 'EUR'
175300020204     C                   EVAL      vabVAS =  'ITL'
175400020204     C                   ENDIF
175500020204     C                   ENDIF
175600020204     C*
175700020204     C                   IF        vabCAS > *zeros
175800020204     C                   IF        vabVCA <> 'EUR'
175900020204     C                   EVAL      vabVCA =  'ITL'
176000020204     C                   ENDIF
176100020204     C                   ENDIF
176200020204     C*
176300020204     C                   IF        vabVMD > *zeros
176400020204     C                   IF        vabVAD <> 'EUR'
176500020204     C                   EVAL      vabVAD =  'ITL'
176600020204     C                   ENDIF
176700020204     C                   ENDIF
176800020204     C*
176900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
177000020204     C                   Z-ADD     vabIAS        wrkDec
177100020204     C                   IF        wrkDec > *zeros
177200020204     C                   IF        vabVAS = 'ITL'
177300020204     C                   EVAL      vabIAS = *zeros
177400020204     C                   ENDIF
177500020204     C                   ENDIF
177600020204     C*
177700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
177800020204     C                   Z-ADD     vabCAS        wrkDec
177900020204     C                   IF        wrkDec > *zeros
178000020204     C                   IF        vabVCA = 'ITL'
178100020204     C                   EVAL      vabCAS = *zeros
178200020204     C                   ENDIF
178300020204     C                   ENDIF
178400020204     C*
178500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
178600020204     C                   Z-ADD     vabVMD        wrkDec
178700020204     C                   IF        wrkDec > *zeros
178800020204     C                   IF        vabVAD = 'ITL'
178900020204     C                   EVAL      vabVMD = *zeros
179000020204     C                   ENDIF
179100020204     C                   ENDIF
179200020204     C*
179300020204     C                   ENDSR
179400020204     C***
179500080617
179600080617
179700080617
179800080617     C*----------------------------------------------------*
179900080617     C*  CONTROLLO NUMERICITA' CAMPI
180000080617     C*----------------------------------------------------*
180100080617     C     CHKNUM        BEGSR
180200080617     C*
180300080617     C                   IF        PiDecChr = *blanks
180400080617     C                   EVAL      PiDecChr = CharNUM
180500080617     C                   ENDIF
180600080617     C*
180700080617     C                   callp     UBISNUM_Check(PiStr
180800080617     C                                          :PiDecChr
180900080617     C                                          :PiVal
181000080617     C                                          :PiNum
181100080617     C                                          :PiInt)
181200080617     C*
181300080617     C                   ENDSR
181400080617     C***
181500010330
181600010601
181700010601
181800010601
181900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
182000010601     C     repfil        BEGSR
182100010601     C*
182200010601     C                   if        invfil = *zeros and
182300010601     C                             depfil > *zeros and
182400010629     C                             (vinflg = *blanks or
182500010629     C                              vinflg = *zeros)
182600010601     C
182700010601     C                   eval      invfil = depfil
182800010601     C                   endif
182900010601     C*
183000010601     C                   if        depfil <> invfil and
183100010601     C                             invfil > *zeros
183200010601     C                   eval      flgMulti = '1'
183300010601     C                   if        vinflg = *blanks
183400010601     C                   add       1             cntNonEl
183500010601     C                   endif
183600010601     C                   endif
183700010601     C*
183800010601     C                   if        vinflg = '2'
183900010601     C                   eval      flgStato = '2'
184000010601     C                   endif
184100010601     C*
184200010601     C                   ENDSR
184300010601     C***
184400010601
184500010601
184600010601
184700010330
184800040119      /TITLE Invio dei dati al punto operativo.
184900040119     C     invio         BEGSR
185000040119     C*
185100101005     C* 1° invio EDIVAT
185200040119     C                   reset                   dscmz
185300040119     C                   move      vlrpoi        cmzdst
185400090911     C                   eval      cmzfld = 'EDIVATWR'
185500040119     C                   eval      cmzmbd = vlrhdl
185600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
185700040119     C***                if        prmfir = *blanks
185800090911     C                   eval      cmzfla = 'EDIVAT0F'
185900090911     C                   eval      cmzmba = 'EDIVAT0F'
186000040119     C***                else
186100040119     C***                eval      cmzfla = prmfir
186200040119     C***                eval      cmzmba = prmfir
186300040119     C***                endif
186400040119     C                   eval      cmznrr = *zeros
186500040119     C                   move      §ctrokvt      cmznrr
186600040119     C                   eval      cmzlba = vlrfl1
186700040119     C                   call(e)   'TIS711C'
186800040119     C                   parm                    dscmz
186900040119     C                   parm      *blanks       esito
187000040119     C                   if        %error
187100040119     C                             or cmzerr = '1'
187200040119     C                             or esito  = '1'
187300040119     C                   eval      wrkesito = '3'
187400040119     C                   else
187500040119     C*
187600101005     C* 2° invio EDIVAB
187700040119     C                   reset                   dscmz
187800040119     C                   move      vlrpoi        cmzdst
187900040119     C                   eval      cmzfld = vlrfou
188000040119     C                   eval      cmzmbd = vlrhdl
188100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
188200040119     C***                if        prmfir = *blanks
188300090911     C                   eval      cmzfla = 'EDIVAB0F'
188400090911     C                   eval      cmzmba = 'EDIVAB0F'
188500040119     C***                else
188600040119     C***                eval      cmzfla = prmfir
188700040119     C***                eval      cmzmba = prmfir
188800040119     C***                endif
188900040119     C                   eval      cmznrr = *zeros
189000040119     C                   move      §ctrokvb      cmznrr
189100040119     C                   eval      cmzlba = vlrfl1
189200040119     C                   call(e)   'TIS711C'
189300040119     C                   parm                    dscmz
189400040119     C                   parm      *blanks       esito
189500040119     C                   if        %error
189600040119     C                             or cmzerr = '1'
189700040119     C                             or esito  = '1'
189800040119     C                   eval      wrkesito = '3'
189900040119     C                   endif
190000040119     C                   endif
190100040119     C*
190200040119     C                   ENDSR
190300040119     C***
190400010601
190500010601
190600010601
190700010601
190800010601      /TITLE Invio dei dati al punto operativo.
190900010601     C     opeini        BEGSR
191000010601     C*
191100010601     C* Inizializzo flag e contatori operativi
191200010601     C                   movel     '0'           flgGiro           1
191300010601     C                   movel     '0'           flgMulti          1
191400010601     C                   movel     '1'           flgStato          1
191500010615     C                   movel     '0'           flgOk             1
191600010601     C                   z-add     *zeros        cntNonEl         10 0
191700010601     C                   z-add     *zeros        depfil            3 0
191800010601     C                   z-add     *zeros        invfil            3 0
191900010601     C*
192000010601     C                   ENDSR
192100010601     C***
192200070326
192300070326
192400070326
192500070326
192600080916     C     *pssr         BEGSR
192700070329     C*
192800080916     C                   if        %open(tivin00r)
192900080916     C                   close     tivin00r
193000080916     C                   endif
193100090911     C                   if        %open(edivabwr)
193200090911     C                   close     edivabwr
193300080916     C                   endif
193400090911     C                   if        %open(edivatwr)
193500090911     C                   close     edivatwr
193600080916     C                   endif
193700070326     C*
193800070326     C* Effettuo la chiamata al CLLE preposto
193900101005     C                   call(e)   'TITVEVTC'
194000080916     C                   parm                    parccm
194100080916     C                   parm                    parmbr
194200080916     C                   parm      '2'           paropz
194300070326     C*
194400080916     C                   eval      wrkesito = '2'
194500070404     C*
194600080916     C                   seton                                        LR
194700070326     C*
194800080916     C                   ENDSR     '*CANCL'
194900070326     C***
195000070326
195100070326
195200010330
195300010330
195400000613     C     *inzsr        BEGSR
195500990910     C*
195600990910     C     *entry        plist
195700990920     C                   parm                    tivlrds
195800990921     C                   parm      wrkesito      esito
195900000724     C                   parm                    prmlit
196000000710     C                   parm                    prmfir
196100010330     C*
196200010330     C* CALCOLA LA DATA CORRENTE
196300010330     C                   time                    wn14             14 0
196400090911     C                   movel     wn14          oracor            6 0          *ORA
196500110923     C                   z-add     *zeros        datcor            8 0
196600110923     C                   eval      datcor = %dec(%date() : *ISO)
196700111118     C*
196800111118     C* Forzo un numero CMR
196900111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
197000000613     C*
197100000613     C                   ENDSR
197200000613     C***
