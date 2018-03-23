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
006000170823     D wVABPKB_HDR     s                   LIKE(VABPKB)
006100170823     D wVABPKB_PCL     s                   LIKE(VABPKB)
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
018100170731     C*
018200170731     C* Se indicato salvo il numero CMR del file corrente
018300170731     C                   if        %subst(vindta:1:6) = '#FILE;'
018400170731     C                   eval      wCMR = %trim(%subst(vindta:17))
018500170731     C                   endif
018600110923     C*
018700110923     C* Ignoro i record che iniziano x '#'
018800110923     C                   if        %subst(vindta:1:1)  ='#'                  AND
018900110923     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:HEADER' AND
019000170731     C                             %subst(vindta:1:18)<>'#DEF;MPSEXP:PARCEL'
019100111118     C                   z-add     0             wFlgVINDTA
019200111118     C                   else
019300110923     C*
019400110923     C* Setto i column headers "variabili"
019500111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:HEADER'
019600110923     C                   eval      header = %trim(%subst(vindta:13))
019700110923     C                   endif
019800111118     C                   if        %subst(vindta:1:18) = '#DEF;MPSEXP:PARCEL'
019900110923     C                   eval      parcel = %trim(%subst(vindta:13))
020000110923     C                   endif
020100111118     C                   endif
020200090420     C*
020300090420     C                   add       1             rrnum
020400010618     C*
020500040510     C                   if        vindta > *blanks AND
020600090420     C                             wFlgVINDTA = 1
020700010330     C*
020800010601     C                   if        *in70 = *off and
020900010330     C                             (vinflg = *blanks
021000010330     C                              or vinflg = '0'
021100010330     C                              or vinflg = '2')
021200010330     C*
021300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021400010711     C                   if        vinflg = *blanks or vinflg = '0'
021500010711     C                   clear                   vinmsg
021600010711     C                   endif
021700010601     C*
021800090421     C* Aggiungo un byte ad '*' all'inizio d ogni record
021900111117     C***                if        %subst(%trim(vindta):1:1) = CharCSV
022000111117     C***                eval      vindta  = '*' + %trim(vindta)
022100111117     C***                endif
022200010601     C*
022300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022400010605     C                   eval      depfil = VABLNP
022500010601     C                   exsr      repfil
022600010601     C                   if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800030822     C                   move(P)   invfil        VABFGS
022900021025     C                   else
023000030822     C                   move(P)   vlrpoi        VABFGS
023100021025     C                   endif
023200090914     C*
023300090914     C                   move      VABFGS        currFGS           3 0
023400090914     C*
023500090914     C* Elaborazione record corrente
023600090914     C                   exsr      elabCurrRec
023700010604     C*
023800010604     C                   if        *in31 = *off and
023900010604     C                             *in32 = *off
024000010604     C                   eval      vinflg = '1'
024100010604     C                   else
024200010604     C                   eval      recko = vindta
024300010604     C                   eval      vinflg = '2'
024400010604     C                   endif
024500010604     C                   endif
024600010601     C*
024700010604     C                   endif
024800010604     C*
024900010330     C                   else
025000010330     C                   eval      vinflg = '1'
025100010330     C                   endif
025200010601     C*
025300010601     C  N70              update    tivin000
025400010330     C*
025500030822     C  N70              enddo
025600090623     C*
025700090623     C* Scarico la testata rimasta "in canna"
025800140415     C                   if        not *in33                                    * esludo estero
025900090623     C  N31              exsr      wriVAB
026000140415     C                   endif
026100040119     C*
026200040119     C                   endif                                                  (endif - chkcall)
026300010601     C*
026400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026500010601     C                   if        cntNonEl = *zeros or
026600010601     C                             flgMulti = '0'
026700010330     C* Se non ci sono record con errori ...
026800010601     C                   if        §ctrno = 0 and
026900010604     C                             §ctrmo = 0 and
027000010601     C                             flgStato <> '2'
027100010330     C* ... restituisco esito OK.
027200010330     C                   eval      wrkesito = '0'
027300010330     C                   else
027400040119     C                   if        §ctrokvb > 0
027500010330     C                   eval      wrkesito = '1'
027600010330     C                   else
027700010615     C                   if        flgOk = '0'
027800010615     C                   eval      wrkesito = '2'
027900010615     C                   else
028000010615     C                   eval      wrkesito = '6'
028100010615     C                   endif
028200010330     C                   endif
028300010330     C                   endif
028400010601     C                   else
028500010601     C                   eval      wrkesito = '9'
028600010601     C                   endif
028700010330     C*
028800010330     C                   if        %open(tivin00r)
028900010330     C                   close     tivin00r
029000010330     C                   endif
029100090911     C                   if        %open(edivabwr)
029200090911     C                   close     edivabwr
029300010330     C                   endif
029400090911     C                   if        %open(edivatwr)
029500090911     C                   close     edivatwr
029600040512     C                   endif
029700010601     C*
029800010601     C                   if        vlrpoi <> 999
029900010601     C                   eval      invfil = vlrpoi
030000010601     C                   endif
030100010330     C*
030200040119     C                   if        §ctrokvb > 0
030300010601     C                             and invfil > *zeros
030400010330     C                   exsr      invio
030500010330     C                   endif
030600010330     C*
030700010330     C                   ENDSR
030800010330     C***
030900090623
031000090623
031100090623
031200090623     C*----------------------------------------------------*
031300090623     C*  INIZIALIZZAZIONI CAMPI DI WRK
031400090623     C*----------------------------------------------------*
031500090623     C     INZWRK        BEGSR
031600090623     C*
031700090623     C                   eval      i = 1
031800090623     C                   eval      posDa = *zeros
031900090623     C                   eval      posA  = *zeros
032000090623     C*
032100090623     C                   ENDSR
032200090623
032300090623
032400090623
032500090623     C*----------------------------------------------------*
032600090623     C*  ELABORAZIONE RECORD CORRENTE
032700090623     C*----------------------------------------------------*
032800090623     C     ELABCURRREC   BEGSR
032900090623     C*
033000090623     C                   select
033100090623     C*
033200110923     C* Tipo record "TESTATA" - HEADER
033300111118     C                   when      %subst(vindta:1:6) = 'HEADER'
033400090623     C                   if        wNewBolla > *zeros
033500140415     C                   if        not *in33                                    * esludo estero
033600090623     C  N31              exsr      wriVAB
033700140415     C                   endif
033800090623     C                   exsr      inzvar
033900090623     C                   endif
034000090624     C                   eval      wNewBolla = 1
034100090623     C                   eval      wGiro = *zeros
034200111118     C                   clear                   SkSplitFLDH
034300110923     C                   eval      prefix = 'HDR_'
034400090624     C                   exsr      inzwrk
034500090623     C                   exsr      impvab
034600090623     C                   exsr      inzwrk
034700090623     C                   exsr      impvab
034800090623     C*
034900110923     C* Tipo record "DETTAGLIO" - PARCEL
035000110923     C                   when      %subst(vindta:1:6) = 'PARCEL'
035100090623     C                   eval      wGiro = *zeros
035200111118     C                   clear                   SkSplitFLDP
035300110923     C                   eval      prefix = 'PCL_'
035400090623     C                   exsr      inzwrk
035500101005     C                   exsr      impvat
035600090623     C                   exsr      inzwrk
035700101005     C                   exsr      impvat
035800090624     C*
035900150203     C                   if        not *in33                                    * esludo estero
036000090624     C  N31              exsr      wrivat
036100150203     C                   endif
036200090623     C*
036300090623     C                   endsl
036400090623     C*
036500090623     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037100040119     C*----------------------------------------------------*
037200040510     C     WRIVAB        BEGSR
037300090623     C*
037400090623     C  N31              add       1             §CTROKVB
037500090623     C   32              add       1             §CTRMO
037600090623     C   31              add       1             §CTRNO
037700040119     C*
037800060331     C* Gestisco l'eventuale rottura x numero spedizione
037900070823     C                   if        VABNSP <> depspe
038000070301     C                   if        wVATNOT_E <> *blanks
038100080125     C                   if        VABCTM = *blanks
038200080125     C                   eval      VABCTM = '7Q'
038300080125     C                   endif
038400070301     C                   endif
038500090911     C*
038600090911     C* VALORIZZO CAMPI RELATIVI AL "CMR"
038700090911     C                   eval      VABCMR = wCMR
038800090911     C                   eval      VABDCM = DATCOR
038900090911     C                   eval      VABDTS = DATCOR
039000090911     C                   eval      VABHMS = ORACOR
039100090911     C                   eval      VABCNT = 1
039200170823     C*
039300170823     C* Considerazioni rispetto al peso
039400170823     C                   if        wVABPKB_HDR > *zeros
039500170823     C                   eval      vabPKB = wVABPKB_HDR
039600170823     C                   else
039700170823     C                   eval      vabPKB = wVABPKB_PCL
039800170823     C                   endif
039900090911     C****
040000090911     C                   write     edivab00                                     => scarico il VAB
040100060331     C                   endif
040200090623     C*
040300090623     C  N31              eval      depspe = vabNSP
040400040119     C*
040500040119     C                   ENDSR
040600040119
040700040119
040800040119
040900040119     C*----------------------------------------------------*
041000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
041100040119     C*----------------------------------------------------*
041200040510     C     WRIVAT        BEGSR
041300060512     C*
041400060512     C* Inizializzo i campi chiave
041500090914     C                   eval      VATFGS = currFGS
041600060512     C                   eval      VATAAS = VABAAS
041700060512     C                   eval      VATCCM = VABCCM
041800070823     C                   eval      VATNRS = VABNRS
041900060512     C                   eval      VATNSP = VABNSP
042000060512     C                   eval      VATLNP = VABLNP
042100111117     C                   eval      VATCMR = wCMR
042200090914     C                   eval      VATCNT = 1
042300060331     C*
042400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
042500060331     C                   if        wVATNOT_A <> *blanks
042600060331     C                   eval      VATNOT = wVATNOT_A
042700060331     C                   eval      VATTRC = 'A'
042800090911     C                   write     EDIVAT00
042900060331     C                   add       1             §CTROKVT
043000060331     C                   endif
043100060331     C*
043200060331     C                   if        wVATNOT_B <> *blanks
043300060331     C                   eval      VATNOT = wVATNOT_B
043400060331     C                   eval      VATTRC = 'B'
043500090911     C                   write     EDIVAT00
043600060331     C                   add       1             §CTROKVT
043700060331     C                   endif
043800111118     C*
043900111118     C                   if        wVATNOT_E <> *blanks
044000111118     C                   eval      VATNOT = wVATNOT_E
044100111118     C                   eval      VATTRC = 'E'
044200111118     C                   write     EDIVAT00
044300111118     C                   add       1             §CTROKVT
044400111118     C                   endif
044500141125     C*
044600141126 xxx C***                if        wVATNOT_S <> *blanks
044700141126     C***                eval      VATNOT = wVATNOT_S
044800141126     C***                eval      VATTRC = 'S'
044900141126     C***                write     EDIVAT00
045000141126     C***                add       1             §CTROKVT
045100141126     C***                endif
045200141125     C*
045300141125     C                   if        wVATNOT_IJ <> *blanks
045400141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
045500141125     C                   eval      VATTRC = 'I'
045600141125     C                   write     EDIVAT00
045700141125     C                   add       1             §CTROKVT
045800141125     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
045900141125     C                   eval      VATTRC = 'J'
046000141125     C                   write     EDIVAT00
046100141125     C                   add       1             §CTROKVT
046200141125     C                   endif
046300090623     C*
046400111118 xxx C* Considero il VAT solo a rottura del "chi sono"
046500111118     C***                if        wVATNOT_E <> *blanks AND
046600111118     C***                          wVATNOT_E <> sVATNOT_E
046700111118     C***                eval      VATNOT = wVATNOT_E
046800111118     C***                eval      VATTRC = 'E'
046900111118     C***                write     EDIVAT00
047000111118     C***                add       1             §CTROKVT
047100111118     C***                eval      sVATNOT_E = wVATNOT_E
047200111118     C***                add       1             VABNCL
047300111118     C***                endif
047400040119     C*
047500040119     C                   ENDSR
047600040119
047700040119
047800040119
047900040119     C*----------------------------------------------------*
048000101005     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048100040119     C*----------------------------------------------------*
048200101005     C     PREVAT        BEGSR
048300040119     C*
048400101005     C* Compongo il nome del membro da dare al EDIVATWR
048500040119     C                   eval      parmbr = vlrhdl
048600040119     C                   movel     'M'           parmbr
048700040510     C                   eval      parccm = vlrksc
048800040119     C                   eval      paropz = '1'
048900040119     C* Effettuo la chiamata al CLLE preposto
049000101005     C                   call(e)   'TITVEVTC'
049100040119     C                   parm                    parccm
049200040119     C                   parm                    parmbr
049300040119     C                   parm                    paropz
049400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049500040119     C                   if        %error
049600040119     C                   movel     '1'           chkcall
049700040119     C                   else
049800040119     C                   movel     '0'           chkcall
049900040119     C                   endif
050000040119     C*
050100040119     C                   ENDSR
050200010601
050300010601
050400010601
050500010330     C*----------------------------------------------------*
050600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050700010330     C*----------------------------------------------------*
050800040119     C     INZVAR        BEGSR
050900010330     C*
051000030822     C* Inizializzo variabili di wrk
051100140415     C                   SETOFF                                       313233
051200090916     C                   Z-ADD     *zeros        Num5_0            5 0
051300090916     C                   MOVEL     *blanks       wNOTE            70
051400060331     C                   MOVEL     *blanks       wVATNOT_A
051500060331     C                   MOVEL     *blanks       wVATNOT_B
051600141125     C                   MOVEL     *blanks       wVATNOT_S
051700141125     C                   MOVEL     *blanks       wVATNOT_IJ
051800090904     C                   MOVEL     *blanks       wVATNOT_E
051900111117     C                   MOVEL     *blanks       wVATNOT_E_1
052000111117     C                   MOVEL     *blanks       wVATNOT_E_2
052100111118     C                   MOVEL     *blanks       wVATNOT_E_3
052200111118     C                   MOVEL     *blanks       wVATNOT_E_4
052300170823     C                   Z-ADD     *zeros        wVABPKB_HDR
052400170823     C                   Z-ADD     *zeros        wVABPKB_PCL
052500090623     C*
052600111118 xxx C***                MOVEL     *blanks       sVATNOT_E        35
052700090220     C*
052800080617     C                   EVAL      wFlgCAS = *blanks
052900030822     C*
053000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
053100090911     C                   CLEAR                   EDIVAB00
053200090911     C                   CLEAR                   EDIVAT00
053300030822     C                   CLEAR                   SkSplitCSV
053400090623     C                   EXSR      inzwrk
053500030822     C*
053600030822     C* Reimposto i valori di default
053700030822     C                   EXSR      DEFCAM
053800030822     C*
053900010330     C                   ENDSR
054000010330     C*----------------------------------------------------*
054100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
054200010330     C*----------------------------------------------------*
054300010330     C     DEFCAM        BEGSR
054400030822     C*
054500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054600030715     C* e delimitatore testo.
054700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
055000030822     C*
055100150203     C* Se richiesta esclusione Estero
055200150203     C                   SETOFF                                       50
055300150203     C                   IF        %subst(vlrppt:1:1) = 'E'
055400150203     C                   SETON                                        50
055500030715     C                   ENDIF
055600090421     C*
055700090421     C* D default SEMPRE porto franco / SEMPRE lnp = p.o. invio / SEMPRE nsp = rrnum
055800090421     C                   EVAL      VABCBO = '1'
055900090421     C                   EVAL      VABLNP = vlrpoi
056000090421     C                   EVAL      VABNSP = rrnum
056100030822     C*
056200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056300030822     C                   EVAL      posDaDft = 1
056400030822     C                   EVAL      posADft  = 0
056500030822     C                   EVAL      wGiroDft = 0
056600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056700030822     C                             posDaDft > 0
056800030822     C*
056900030822     C* Gestisco il 1° giro
057000030822     C                   IF        wGiroDft = 0
057100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057300030822     C* Incremento il contatore dei "giri"
057400030822     C                   EVAL      wGiroDft = 1
057500030822     C                   ELSE
057600030822     C                   EVAL      posDaDft = posADft
057700030822     C                   ENDIF
057800030822     C* Eseguo lo scan x trovare la fine del campo corrente
057900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058000030822     C*
058100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058300030822     C                   IF        posDaDft > 0 AND
058400030822     C                             posADft  > 0
058500030822     C* NCL
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'NCL'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiInt=*on
059500030822     C                   Z-ADD     PiVal         VABNCL
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* CCM
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'CCM'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiInt=*on
060800030822     C                   Z-ADD     PiVal         VABCCM
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* LNP
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'LNP'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiInt=*on
062100030822     C                   Z-ADD     PiVal         VABLNP
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* NRS
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'NRS'
062900030822     C                   EVAL      PiStr=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   EXSR      CHKNUM
063300030822     C                   IF        PiInt=*on
063400030822     C                   Z-ADD     PiVal         VABNRS
063500030822     C                   ENDIF
063600030822     C                   ENDIF
063700030822     C* CTR
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'CTR'
064200030822     C                   EVAL      PiStr=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   EXSR      CHKNUM
064600030822     C                   IF        PiInt=*on
064700030822     C                   Z-ADD     PiVal         VABCTR
064800030822     C                   ENDIF
064900030822     C                   ENDIF
065000030822     C* PKB
065100030822     C                   IF        %subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):1:3)
065400030822     C                             = 'PKB'
065500030822     C                   EVAL      PiStr=%trim(%subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065800030822     C                   EXSR      CHKNUM
065900030822     C                   IF        PiNum=*on
066000030822     C                   Z-ADD     PiVal         VABPKB
066100030822     C                   ENDIF
066200030822     C                   ENDIF
066300140715     C* PKN
066400140715     C                   IF        %subst(
066500140715     C                             %subst(vlrppt:posDaDft+1:
066600140715     C                             posADft-posDaDft-1):1:3)
066700140715     C                             = 'PKN'
066800140715     C                   EVAL      PiStr=%trim(%subst(
066900140715     C                             %subst(vlrppt:posDaDft+1:
067000140715     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067100140715     C                   EXSR      CHKNUM
067200140715     C                   IF        PiNum=*on
067300140715     C                   Z-ADD     PiVal         wVABPKN           7 1
067400140715     C                   ENDIF
067500140715     C                   ENDIF
067600030822     C* VLB
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VLB'
068100030822     C                   EVAL      PiStr=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   EXSR      CHKNUM
068500030822     C                   IF        PiNum=*on
068600030822     C                   Z-ADD     PiVal         VABVLB
068700030822     C                   ENDIF
068800030822     C                   ENDIF
068900030822     C* QFT
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'QFT'
069400030822     C                   EVAL      PiStr=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   EXSR      CHKNUM
069800030822     C                   IF        PiNum=*on
069900030822     C                   Z-ADD     PiVal         VABQFT
070000030822     C                   ENDIF
070100030822     C                   ENDIF
070200030822     C* CBO
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'CBO'
070700030822     C                   EVAL      VABCBO=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TSP
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TSP'
071600030822     C                   EVAL      VABTSP=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VAS
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VAS'
072500030822     C                   EVAL      VABVAS=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* VCA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'VCA'
073400030822     C                   EVAL      VABVCA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800070925     C* TIC
073900070925     C                   IF        %subst(
074000070925     C                             %subst(vlrppt:posDaDft+1:
074100070925     C                             posADft-posDaDft-1):1:3)
074200070925     C                             = 'TIC'
074300070925     C                   EVAL      VABTIC=%trim(%subst(
074400070925     C                             %subst(vlrppt:posDaDft+1:
074500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600070925     C                   ENDIF
074700030822     C* GCA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GCA'
075200030822     C                   EVAL      VABGCA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* CTM
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'CTM'
076100030822     C                   EVAL      VABCTM=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* FFD
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'FFD'
077000030822     C                   EVAL      VABFFD=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400030822     C* VAD
077500030822     C                   IF        %subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):1:3)
077800030822     C                             = 'VAD'
077900030822     C                   EVAL      VABVAD=%trim(%subst(
078000030822     C                             %subst(vlrppt:posDaDft+1:
078100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200030822     C                   ENDIF
078300030822     C* GMA
078400030822     C                   IF        %subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):1:3)
078700030822     C                             = 'GMA'
078800030822     C                   EVAL      VABGMA=%trim(%subst(
078900030822     C                             %subst(vlrppt:posDaDft+1:
079000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079100030822     C                   ENDIF
079200030822     C* GGA
079300030822     C                   IF        %subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):1:3)
079600030822     C                             = 'GGA'
079700030822     C                   EVAL      VABGGA=%trim(%subst(
079800030822     C                             %subst(vlrppt:posDaDft+1:
079900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080000030822     C                   ENDIF
080100030822     C* GVA
080200030822     C                   IF        %subst(
080300030822     C                             %subst(vlrppt:posDaDft+1:
080400030822     C                             posADft-posDaDft-1):1:3)
080500030822     C                             = 'GVA'
080600030822     C                   EVAL      VABGVA=%trim(%subst(
080700030822     C                             %subst(vlrppt:posDaDft+1:
080800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080900030822     C                   ENDIF
081000030822     C* TC1
081100030822     C                   IF        %subst(
081200030822     C                             %subst(vlrppt:posDaDft+1:
081300030822     C                             posADft-posDaDft-1):1:3)
081400030822     C                             = 'TC1'
081500030822     C                   EVAL      VABTC1=%trim(%subst(
081600030822     C                             %subst(vlrppt:posDaDft+1:
081700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081800030822     C                   ENDIF
081900030822     C* TC2
082000030822     C                   IF        %subst(
082100030822     C                             %subst(vlrppt:posDaDft+1:
082200030822     C                             posADft-posDaDft-1):1:3)
082300030822     C                             = 'TC2'
082400030822     C                   EVAL      VABTC2=%trim(%subst(
082500030822     C                             %subst(vlrppt:posDaDft+1:
082600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082700030822     C                   ENDIF
082800040714     C* VATTRC
082900040714     C                   IF        %subst(
083000040714     C                             %subst(vlrppt:posDaDft+1:
083100040714     C                             posADft-posDaDft-1):1:3)
083200040714     C                             = 'TRC'
083300040714     C                   EVAL      VATTRC=%trim(%subst(
083400040714     C                             %subst(vlrppt:posDaDft+1:
083500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
083600040714     C                   ENDIF
083700030822     C* ...
083800030822     C                   ENDIF
083900030822     C                   ENDDO
084000020204     C*
084100010330     C                   ENDSR
084200010607     C*----------------------------------------------------*
084300090911     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
084400010607     C*----------------------------------------------------*
084500090623     C     IMPVAB        BEGSR
084600030515     C*
084700010607     C                   MOVEL     datcor        VABAAS
084800010607     C                   MOVE      datcor        VABMGS
084900010607     C*
085000030715     C**********
085100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
085200030715     C**********
085300030715     C                   IF        wGiro = *zeros
085400080716     C*
085500080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
085600080716     C                   EVAL      depvinDTA = vinDTA
085700040113     C*
085800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085900110923     C                   IF        header <> *blanks
086000110923     C                   EVAL      vindta = header
086100070823     C     ';':CharCSV   XLATE     vindta        vindta
086200040510     C                   ENDIF
086300080626     C*
086400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086500031201     C     minu:maiu     XLATE     vindta        vindta
086600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086700030715     C                   DOW       posDa <= %len(%trim(vindta))
086800030715     C*
086900030715     C* Gestisco il 1° campo
087000030715     C                   IF        i = 1
087100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087200030715     C                   EVAL      posDa = 1
087300030715     C* Eseguo lo scan x trovare la fine del primo campo
087400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087500030715     C* A questo punto "estraggo" il campo corrente
087600111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
087700110923     C                                         %subst(vindta:posDa:(posA-posDa))
087800030715     C* X i campi successivi al 1°
087900030715     C                   ELSE
088000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088100030715     C                   EVAL      posDa = posA + 1
088200030715     C* Eseguo lo scan x trovare la fine del campo corrente
088300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088400030715     C* Gestisco l'ultimo campo
088500030715     C                   IF        posA = *zeros
088600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088700030715     C* A questo punto "estraggo" il campo corrente
088800111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
088900110923     C                                         %subst(vindta:posDa:(posA-posDa))
089000030715     C                   LEAVE
089100030715     C                   ELSE
089200030715     C* A questo punto "estraggo" il campo corrente
089300111118     C                   EVAL      SkSplitFLDH(i) = %trim(prefix) +
089400110923     C                                         %subst(vindta:posDa:(posA-posDa))
089500030715     C                   ENDIF
089600030715     C                   ENDIF
089700030715     C* Incremento il contatore d campo
089800030715     C                   EVAL      i = i +1
089900030715     C                   ENDDO
090000030715     C*
090100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090200030715     C* delimitazione testo
090300030715     C                   EVAL      i = 1
090400111118     C                   DOW       i < %elem(SkSplitFLDH)
090500111118     C     CharTXT:' '   XLATE     SkSplitFLDH(i)SkSplitFLDH(i)
090600111118     C                   EVAL      SkSplitFLDH(i) = %trim(SkSplitFLDH(i))
090700030715     C                   EVAL      i = i + 1
090800030715     C                   ENDDO
090900030715     C                   EVAL      wGiro = 1
091000080716     C* Ripristino la 1° riga originale
091100080716     C                   EVAL      vinDTA = depvinDTA
091200030715     C                   ELSE
091300030715     C                   EVAL      wGiro = 2
091400031204     C**********
091500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091600031204     C* d separatore campo
091700031204     C**********
091800031204     C                   EVAL      vindta = %trim(vindta)
091900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
092000031204     C                   EVAL      vindta = %subst(vindta:2)
092100031204     C                   ENDDO
092200031204     C*
092300031204     C                   Z-ADD     *zeros        lunghInput        4 0
092400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092800031204     C                   ENDDO
092900030715     C**********
093000030715     C* Effettuo lo split del campo dati d input
093100030715     C**********
093200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
093300030715     C                   DOW       posDa <= %len(%trim(vindta))
093400030715     C*
093500030715     C* Gestisco il 1° campo
093600030715     C                   IF        i = 1
093700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093800030715     C                   EVAL      posDa = 1
093900030715     C* Eseguo lo scan x trovare la fine del primo campo
094000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094100030715     C* A questo punto "estraggo" il campo corrente
094200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
094300030715     C                                                    (posA-posDa))
094400030715     C* X i campi successivi al 1°
094500030715     C                   ELSE
094600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094700030715     C                   EVAL      posDa = posA + 1
094800031204     C* Verifico che nn vi sia il campo nn valorizzato
094900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
095000031204     C* Se campo nn valorizzato skippo al prossimo
095100031204     C                   EVAL      SkSplitCSV(i) = *blanks
095200031204     C                   EVAL      posA  = posA + 1
095300031204     C                   ELSE
095400030715     C* Eseguo lo scan x trovare la fine del campo corrente
095500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095600030715     C* Gestisco l'ultimo campo
095700030715     C                   IF        posA = *zeros
095800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095900030715     C* A questo punto "estraggo" il campo corrente
096000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096100030715     C                                                    (posA-posDa))
096200030715     C                   LEAVE
096300030715     C                   ELSE
096400030715     C* A questo punto "estraggo" il campo corrente
096500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096600030715     C                                                    (posA-posDa))
096700030715     C                   ENDIF
096800031204     C                   ENDIF
096900030715     C                   ENDIF
097000030715     C* Incremento il contatore d campo
097100030715     C                   EVAL      i = i +1
097200030715     C                   ENDDO
097300030715     C*
097400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
097500030715     C* delimitazione testo
097600030715     C                   EVAL      i = 1
097700030715     C                   DOW       i < %elem(SkSplitCSV)
097800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
098000030715     C                   EVAL      i = i + 1
098100030715     C                   ENDDO
098200030715     C*
098300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098400030715     C                   EVAL      i = 1
098500111118     C                   DOW       i < %elem(SkSplitFLDH)
098600030715     C*
098700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098800111118     C                   IF        SkSplitFLDH(i) <> *blanks AND
098900090623     C                             SkSplitCSV(i) <> *blanks
099000090623     C*
099100111118     C                   exsr      CARVALH
099200040510     C*
099300030715     C                   ENDIF
099400030715     C                   EVAL      i = i + 1
099500030715     C                   ENDDO
099600070925     C*
099700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
099800080125     C*
099900111117     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
100000111117     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
100100140715     C*
100200170823     C                   IF        wVABPKB_HDR = *zeros
100300170823     C                   EVAL      wVABPKB_HDR = wVABPKN * VABNCL
100400140715     C                   ENDIF
100500070925     C*
100600071029     C***  <===  -----------------------------------------
100700070925     C*
100800070925     C* Considerazioni finali su CBO/CAS
100900080125     C                   IF        wFlgCAS = '1'
101000070925     C                   IF        VABCBO = '1'
101100070925     C                   EVAL      VABCBO = '4'
101200070925     C                   ENDIF
101300070925     C                   IF        VABCBO = '2'
101400070925     C                   EVAL      VABCBO = '6'
101500070925     C                   ENDIF
101600070925     C                   ENDIF
101700070925     C***  <===  ----------------------------
101800040510     C*
101900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
102000040510     C                   IF        VABNSP = *zeros
102100040510     C                   SETON                                        31
102200040510     C                   ENDIF
102300040715     C*
102400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
102500040715     C                   IF        VABLNP = *zeros
102600040715     C                   SETON                                        31
102700040715     C                   ENDIF
102800040510     C*
102900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
103000040510     C                   IF        VABRMN = *zeros
103100040510     C                   Z-ADD     VABNSP        VABRMN
103200040510     C                   ENDIF
103300040510     C*
103400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
103500040510     C                   IF        VABRMA = *blanks
103600040714     C                   MOVEL     VABRMN        VABRMA
103700040510     C                   ENDIF
103800040128     C*
103900040510     C* Se provincia nn valorizzata la reperisco
104000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
104100040510     C                   IF        VABPRD = *blanks
104200040128     C                   CLEAR                   TISI95DS
104300040128     C                   EVAL      I95TCN = '3'
104400040128     C                   Z-ADD     datcor        I95DAT
104500040128     C                   EVAL      I95NAR = VABNZD
104600040128     C                   EVAL      I95CAP = VABCAD
104700040128     C                   EVAL      I95LOC = VABLOD
104800040128     C                   CALL      'TISI95R'
104900040128     C                   PARM                    TISI95DS
105000040128     C                   EVAL      VABPRD = O95PRV
105100040510     C                   ENDIF
105200020204     C*
105300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
105400020204     C                   EXSR      CHKIMPDIV
105500030715     C                   ENDIF
105600020204     C*
105700010607     C                   ENDSR
105800010607     C*----------------------------------------------------*
105900090623
106000090623
106100090623
106200090623     C*----------------------------------------------------*
106300101005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
106400090623     C*----------------------------------------------------*
106500101005     C     IMPVAT        BEGSR
106600090623     C*
106700090623     C**********
106800090623     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
106900090623     C**********
107000090623     C                   IF        wGiro = *zeros
107100090623     C*
107200090623     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
107300090623     C                   EVAL      depvinDTA = vinDTA
107400090623     C*
107500090623     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
107600110923     C                   IF        parcel <> *blanks
107700110923     C                   EVAL      vindta = parcel
107800090623     C     ';':CharCSV   XLATE     vindta        vindta
107900090623     C                   ENDIF
108000090623     C*
108100090623     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
108200090623     C     minu:maiu     XLATE     vindta        vindta
108300090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
108400090623     C                   DOW       posDa <= %len(%trim(vindta))
108500090623     C*
108600090623     C* Gestisco il 1° campo
108700090623     C                   IF        i = 1
108800090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
108900090623     C                   EVAL      posDa = 1
109000090623     C* Eseguo lo scan x trovare la fine del primo campo
109100090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
109200090623     C* A questo punto "estraggo" il campo corrente
109300111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
109400110923     C                                         %subst(vindta:posDa:(posA-posDa))
109500090623     C* X i campi successivi al 1°
109600090623     C                   ELSE
109700090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
109800090623     C                   EVAL      posDa = posA + 1
109900090623     C* Eseguo lo scan x trovare la fine del campo corrente
110000090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
110100090623     C* Gestisco l'ultimo campo
110200090623     C                   IF        posA = *zeros
110300090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
110400090623     C* A questo punto "estraggo" il campo corrente
110500111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
110600110923     C                                         %subst(vindta:posDa:(posA-posDa))
110700090623     C                   LEAVE
110800090623     C                   ELSE
110900090623     C* A questo punto "estraggo" il campo corrente
111000111118     C                   EVAL      SkSplitFLDP(i) = %trim(prefix) +
111100110923     C                                         %subst(vindta:posDa:(posA-posDa))
111200090623     C                   ENDIF
111300090623     C                   ENDIF
111400090623     C* Incremento il contatore d campo
111500090623     C                   EVAL      i = i +1
111600090623     C                   ENDDO
111700090623     C*
111800090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
111900090623     C* delimitazione testo
112000090623     C                   EVAL      i = 1
112100111118     C                   DOW       i < %elem(SkSplitFLDP)
112200111118     C     CharTXT:' '   XLATE     SkSplitFLDP(i)SkSplitFLDP(i)
112300111118     C                   EVAL      SkSplitFLDP(i) = %trim(SkSplitFLDP(i))
112400090623     C                   EVAL      i = i + 1
112500090623     C                   ENDDO
112600090623     C                   EVAL      wGiro = 1
112700090623     C* Ripristino la 1° riga originale
112800090623     C                   EVAL      vinDTA = depvinDTA
112900090623     C                   ELSE
113000090623     C                   EVAL      wGiro = 2
113100090623     C**********
113200090623     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
113300090623     C* d separatore campo
113400090623     C**********
113500090623     C                   EVAL      vindta = %trim(vindta)
113600090623     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
113700090623     C                   EVAL      vindta = %subst(vindta:2)
113800090623     C                   ENDDO
113900090623     C*
114000090623     C                   Z-ADD     *zeros        lunghInput        4 0
114100090623     C                   EVAL      lunghInput = %len(%trim(vindta))
114200090623     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
114300090623     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
114400090623     C                   EVAL      lunghInput = %len(%trim(vindta))
114500090623     C                   ENDDO
114600090623     C**********
114700090623     C* Effettuo lo split del campo dati d input
114800090623     C**********
114900090623     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
115000090623     C                   DOW       posDa <= %len(%trim(vindta))
115100090623     C*
115200090623     C* Gestisco il 1° campo
115300090623     C                   IF        i = 1
115400090623     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
115500090623     C                   EVAL      posDa = 1
115600090623     C* Eseguo lo scan x trovare la fine del primo campo
115700090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
115800090623     C* A questo punto "estraggo" il campo corrente
115900090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
116000090623     C                                                    (posA-posDa))
116100090623     C* X i campi successivi al 1°
116200090623     C                   ELSE
116300090623     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
116400090623     C                   EVAL      posDa = posA + 1
116500090623     C* Verifico che nn vi sia il campo nn valorizzato
116600090623     C                   IF        %subst(vindta:posDa:1) = CharCSV
116700090623     C* Se campo nn valorizzato skippo al prossimo
116800090623     C                   EVAL      SkSplitCSV(i) = *blanks
116900090623     C                   EVAL      posA  = posA + 1
117000090623     C                   ELSE
117100090623     C* Eseguo lo scan x trovare la fine del campo corrente
117200090623     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
117300090623     C* Gestisco l'ultimo campo
117400090623     C                   IF        posA = *zeros
117500090623     C                   EVAL      posA = %len(%trim(vindta)) + 1
117600090623     C* A questo punto "estraggo" il campo corrente
117700090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
117800090623     C                                                    (posA-posDa))
117900090623     C                   LEAVE
118000090623     C                   ELSE
118100090623     C* A questo punto "estraggo" il campo corrente
118200090623     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
118300090623     C                                                    (posA-posDa))
118400090623     C                   ENDIF
118500090623     C                   ENDIF
118600090623     C                   ENDIF
118700090623     C* Incremento il contatore d campo
118800090623     C                   EVAL      i = i +1
118900090623     C                   ENDDO
119000090623     C*
119100090623     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
119200090623     C* delimitazione testo
119300090623     C                   EVAL      i = 1
119400090623     C                   DOW       i < %elem(SkSplitCSV)
119500090623     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
119600090623     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
119700090623     C                   EVAL      i = i + 1
119800090623     C                   ENDDO
119900090623     C*
120000090623     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
120100090623     C                   EVAL      i = 1
120200111118     C                   DOW       i < %elem(SkSplitFLDP)
120300090623     C*
120400090623     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
120500111118     C                   IF        SkSplitFLDP(i) <> *blanks AND
120600090623     C                             SkSplitCSV(i) <> *blanks
120700090623     C*
120800111118     C                   exsr      CARVALP
120900090623     C*
121000090623     C                   ENDIF
121100090623     C                   EVAL      i = i + 1
121200090623     C                   ENDDO
121300090623     C*
121400090623     C***  ===>  Gestione campi "particolari" fuori ciclo
121500111118     C*
121600111118     C                   EVAL      wVATNOT_E = %trim(wVATNOT_E_1) +
121700111118     C                                         %trim(wVATNOT_E_2) +
121800111118     C                                         %trim(wVATNOT_E_3) +
121900111118     C                                         %trim(wVATNOT_E_4)
122000090623     C*
122100090623     C***  <===  -----------------------------------------
122200090623     C*
122300090623     C                   ENDIF
122400090623     C*
122500090623     C                   ENDSR
122600090623     C*----------------------------------------------------*
122700090623
122800090623
122900090623     C*----------------------------------------------------*
123000090623     C*  ROUTINE DI VALORIZZAZIONE CAMPI
123100090623     C*----------------------------------------------------*
123200111118     C     CARVALH       BEGSR
123300090623     C*
123400090623     C*** ATB
123500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABATB'
123600090623     C                   EVAL      VABATB = SkSplitCSV(i)
123700090623     C                   ENDIF
123800090623     C*** RSD
123900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME1'
124000090623     C                   EVAL      VABRSD = SkSplitCSV(i)
124100090623     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
124200090623     C     '@':'A'       XLATE     VABRSD        VABRSD
124300090623     C* ==
124400090623     C                   ENDIF
124500090623     C*** RD2
124600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RNAME2'
124700090623     C                   EVAL      VABRD2 = SkSplitCSV(i)
124800090623     C                   ENDIF
124900130530     C*** IND 1 (HDR)
125000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RSTREET'
125100090623     C                   EVAL      VABIND = SkSplitCSV(i)
125200090623     C                   ENDIF
125300130530     C*** IND 2 (HDR)
125400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RHOUSENO'
125500111118     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
125600111117     C                   ENDIF
125700130530     C*** IND 1
125800130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RSTREET'
125900130530     C                   EVAL      VABIND = SkSplitCSV(i)
126000130530     C                   ENDIF
126100130530     C*** IND 2
126200130530     C                   IF        %trim(SkSplitFLDH(i)) = 'RHOUSENO'
126300130530     C                   EVAL      VABIND = %trim(VABIND) + ' ' + SkSplitCSV(i)
126400130530     C                   ENDIF
126500111117     C*** CAD
126600111216     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPOSTAL'
126700111117     C                   EVAL      VABCAD = SkSplitCSV(i)
126800111216     C                   MOVEL     *blanks       wAlfa7            7
126900111216     C                   MOVEL     *zeros        wAlfa7z           7
127000111216     C                   EVALR     wAlfa7 = wAlfa7z + %trim(VABCAD)
127100111216     C                   EVAL      wVATNOT_E_1 = '%'+wAlfa7
127200111117     C                   ENDIF
127300090623     C*** LOD
127400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCITY'
127500090623     C                   EVAL      VABLOD = SkSplitCSV(i)
127600090623     C                   ENDIF
127700111117     C*** PRD
127800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABPRD'
127900090623     C                   EVAL      VABPRD = SkSplitCSV(i)
128000090623     C                   ENDIF
128100090623     C*** NZD
128200140415     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
128300140415     C                   IF        %trim(SkSplitCSV(i)) <> '380'                * 380 = Italia
128400150203     C   50              SETON                                        33
128500090623     C                   ENDIF
128600140415     C                   ENDIF
128700090623     C*** RMA
128800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCREF1'
128900130611     C***                EVAL      VABRMA = %subst(SkSplitCSV(i):4:14)
129000130611     C                   EVAL      VABRMA = SkSplitCSV(i)
129100090623     C                   ENDIF
129200111117     C*** NOT
129300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_SCOMMENT'
129400111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
129500090623     C                   ENDIF
129600111117     C*** NT2
129700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOMMENT'
129800111117     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' + SkSplitCSV(i)
129900090623     C                   ENDIF
130000090623     C*** VCA
130100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVCA'
130200090623     C                   EVAL      VABVCA = SkSplitCSV(i)
130300090623     C                   ENDIF
130400090623     C*** VAS
130500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAS'
130600090623     C                   EVAL      VABVAS = SkSplitCSV(i)
130700090623     C                   ENDIF
130800090623     C*** FFD
130900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFFD'
131000090623     C                   EVAL      VABFFD = SkSplitCSV(i)
131100090623     C                   ENDIF
131200090623     C*** GC1
131300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC1'
131400090623     C                   EVAL      VABGC1 = SkSplitCSV(i)
131500090623     C                   ENDIF
131600090623     C*** GC2
131700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGC2'
131800090623     C                   EVAL      VABGC2 = SkSplitCSV(i)
131900090623     C                   ENDIF
132000090623     C*** TSP
132100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTSP'
132200090623     C                   EVAL      VABTSP = SkSplitCSV(i)
132300090623     C                   ENDIF
132400090623     C*** CBO
132500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCBO'
132600090623     C                   EVAL      VABCBO = SkSplitCSV(i)
132700090623     C                   ENDIF
132800090623     C*** NAS
132900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNAS'
133000090623     C                   EVAL      VABNAS = SkSplitCSV(i)
133100090623     C                   ENDIF
133200090623     C*** TIC
133300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTIC'
133400090623     C                   EVAL      VABTIC = SkSplitCSV(i)
133500090623     C                   ENDIF
133600090623     C*** GCA
133700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGCA'
133800090623     C                   EVAL      VABGCA = SkSplitCSV(i)
133900090623     C                   ENDIF
134000090623     C*** XCO
134100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABXCO'
134200090623     C                   EVAL      VABXCO = SkSplitCSV(i)
134300090623     C                   ENDIF
134400090623     C*** CTM
134500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTM'
134600090623     C                   EVAL      VABCTM = SkSplitCSV(i)
134700090623     C                   ENDIF
134800090623     C*** TCR
134900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTCR'
135000090623     C                   EVAL      VABTCR = SkSplitCSV(i)
135100090623     C                   ENDIF
135200090623     C*** CTS
135300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTS'
135400090623     C                   EVAL      VABCTS = SkSplitCSV(i)
135500090623     C                   ENDIF
135600090623     C*** FTM
135700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABFTM'
135800090623     C                   EVAL      VABFTM = SkSplitCSV(i)
135900090623     C                   ENDIF
136000090623     C*** VAD
136100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVAD'
136200090623     C                   EVAL      VABVAD = SkSplitCSV(i)
136300090623     C                   ENDIF
136400090623     C*** GMA
136500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGMA'
136600090623     C                   EVAL      VABGMA = SkSplitCSV(i)
136700090623     C                   ENDIF
136800090623     C*** GGA
136900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGGA'
137000090623     C                   EVAL      VABGGA = SkSplitCSV(i)
137100090623     C                   ENDIF
137200090623     C*** GVA
137300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABGVA'
137400090623     C                   EVAL      VABGVA = SkSplitCSV(i)
137500090623     C                   ENDIF
137600090623     C*** TC1
137700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC1'
137800090623     C                   EVAL      VABTC1 = SkSplitCSV(i)
137900090623     C                   ENDIF
138000090623     C*** TC2
138100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABTC2'
138200090623     C                   EVAL      VABTC2 = SkSplitCSV(i)
138300090623     C                   ENDIF
138400090623     C*** SCL
138500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABSCL'
138600090623     C                   EVAL      VABSCL = SkSplitCSV(i)
138700090623     C                   ENDIF
138800090623     C*** RMO
138900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABRMO'
139000090623     C                   EVAL      VABRMO = SkSplitCSV(i)
139100090623     C                   ENDIF
139200090623     C*** NMO
139300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNMO'
139400090623     C                   EVAL      VABNMO = SkSplitCSV(i)
139500090623     C                   ENDIF
139600090623     C*
139700090623     C* Reperisco quindi i campi numerici...
139800111117     C*** NSP
139900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
140000111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
140100090623     C                   EXSR      CHKNUM
140200090623     C                   IF        PiInt=*on
140300090623     C                   Z-ADD     PiVal         VABNSP
140400090623     C                   ELSE
140500090623     C                   SETON                                        31
140600090623     C                   EVAL      VABNSP = *zeros
140700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
140800090623     C                             + ' ' + 'VABNSP'
140900090623     C                   ENDIF
141000090623     C                   ENDIF
141100090623     C*** RMN
141200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSID'
141300111117     C                   EVAL      PiStr=%subst(SkSplitCSV(i):4:14)
141400090623     C                   EXSR      CHKNUM
141500090623     C                   IF        PiInt=*on
141600090623     C                   Z-ADD     PiVal         VABRMN
141700090623     C                   ELSE
141800090623     C                   SETON                                        32
141900090623     C                   EVAL      VABRMN = *zeros
142000090623     C                   EVAL      vinmsg = %trimr(vinmsg)
142100090623     C                             + ' ' + 'VABRMN'
142200090623     C                   ENDIF
142300090623     C                   ENDIF
142400090623     C*** CAD
142500111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAD'
142600090623     C                   EVAL      PiStr=SkSplitCSV(i)
142700090623     C                   EXSR      CHKNUM
142800090623     C                   IF        PiInt=*on
142900090623     C                   Z-ADD     PiVal         Num5_0
143000090623     C                   MOVEL(p)  Num5_0        VABCAD
143100090623     C                   ELSE
143200090623     C                   EVAL      VABCAD = SkSplitCSV(i)
143300090623     C                   SETON                                        32
143400090623     C                   EVAL      VABCAD = *zeros
143500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
143600090623     C                             + ' ' + 'VABCAD'
143700090623     C                   ENDIF
143800090623     C                   ENDIF
143900090623     C*** NCL
144000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSCOUNT'
144100090623     C                   EVAL      PiStr=SkSplitCSV(i)
144200090623     C                   EXSR      CHKNUM
144300090623     C                   IF        PiInt=*on
144400090623     C                   Z-ADD     PiVal         VABNCL
144500090623     C                   ELSE
144600090623     C                   SETON                                        32
144700090623     C                   EVAL      VABNCL = *zeros
144800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
144900090623     C                             + ' ' + 'VABNCL'
145000090623     C                   ENDIF
145100090623     C                   ENDIF
145200090623     C*** PKB
145300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSWEIGHT'
145400090623     C                   EVAL      PiStr=SkSplitCSV(i)
145500090623     C                   EXSR      CHKNUM
145600090623     C                   IF        PiNum=*on
145700130611     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
145800170823     C                   Z-ADD     PiVal         wVABPKB_HDR
145900090623     C                   ELSE
146000090623     C                   SETON                                        32
146100170823     C                   EVAL      wVABPKB_HDR = *zeros
146200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
146300090623     C                             + ' ' + 'VABPKB'
146400090623     C                   ENDIF
146500090623     C                   ENDIF
146600090623     C*** CAS
146700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCAS'
146800090623     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
146900090623     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
147000090623     C                             %trim(SkSplitCSV(i)) <> '0'      AND
147100090623     C                             %trim(SkSplitCSV(i)) <> '0,00'
147200090623     C                   EVAL      wFlgCAS = '1'
147300090623     C                   ENDIF
147400090623     C                   EVAL      PiStr=SkSplitCSV(i)
147500090623     C                   EXSR      CHKNUM
147600090623     C                   IF        PiNum=*on
147700090623     C                   Z-ADD     PiVal         VABCAS
147800090623     C                   ELSE
147900090623     C                   SETON                                        32
148000090623     C                   EVAL      VABCAS = *zeros
148100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
148200090623     C                             + ' ' + 'VABCAS'
148300090623     C                   ENDIF
148400090623     C                   ENDIF
148500090623     C*** CCM
148600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCCM'
148700090623     C                   EVAL      PiStr=SkSplitCSV(i)
148800090623     C                   EXSR      CHKNUM
148900090623     C                   IF        PiInt=*on      AND
149000090623     C                             PiVal<=9999999 AND
149100090623     C                             PiVal>*zeros
149200090623     C                   Z-ADD     PiVal         VABCCM
149300090623     C                   ELSE
149400090623     C                   SETON                                        32
149500090623     C                   EVAL      VABCCM = *zeros
149600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
149700090623     C                             + ' ' + 'VABCCM'
149800090623     C                   ENDIF
149900090623     C                   ENDIF
150000090623     C*** LNP
150100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNP'
150200090623     C                   EVAL      PiStr=SkSplitCSV(i)
150300090623     C                   EXSR      CHKNUM
150400090623     C                   IF        PiInt=*on    AND
150500090623     C                             PiVal<=999   AND
150600090623     C                             PiVal>*zeros
150700090623     C                   Z-ADD     PiVal         VABLNP
150800090623     C                   ELSE
150900090623     C                   SETON                                        32
151000090623     C                   EVAL      VABLNP = *zeros
151100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
151200090623     C                             + ' ' + 'VABLNP'
151300090623     C                   ENDIF
151400090623     C                   ENDIF
151500090623     C*** NRS
151600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNRS'
151700090623     C                   EVAL      PiStr=SkSplitCSV(i)
151800090623     C                   EXSR      CHKNUM
151900090623     C                   IF        PiInt=*on AND
152000090623     C                             PiVal<=99
152100090623     C                   Z-ADD     PiVal         VABNRS
152200090623     C                   ELSE
152300090623     C                   SETON                                        32
152400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
152500090623     C                             + ' ' + 'VABNRS'
152600090623     C                   ENDIF
152700090623     C                   ENDIF
152800090623     C*** LNA
152900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABLNA'
153000090623     C                   EVAL      PiStr=SkSplitCSV(i)
153100090623     C                   EXSR      CHKNUM
153200090623     C                   IF        PiInt=*on    AND
153300090623     C                             PiVal<=999   AND
153400090623     C                             PiVal>*zeros
153500090623     C                   Z-ADD     PiVal         VABLNA
153600090623     C                   ELSE
153700090623     C                   SETON                                        32
153800090623     C                   EVAL      VABLNA = *zeros
153900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
154000090623     C                             + ' ' + 'VABLNA'
154100090623     C                   ENDIF
154200090623     C                   ENDIF
154300090623     C*** CTR
154400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCTR'
154500090623     C                   EVAL      PiStr=SkSplitCSV(i)
154600090623     C                   EXSR      CHKNUM
154700090623     C                   IF        PiInt=*on  AND
154800090623     C                             PiVal<=999
154900090623     C                   Z-ADD     PiVal         VABCTR
155000090623     C                   ELSE
155100090623     C                   SETON                                        32
155200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
155300090623     C                             + ' ' + 'VABCTR'
155400090623     C                   ENDIF
155500090623     C                   ENDIF
155600090623     C*** IAS
155700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABIAS'
155800090623     C                   EVAL      PiStr=SkSplitCSV(i)
155900090623     C                   EXSR      CHKNUM
156000090623     C                   IF        PiNum=*on
156100090623     C                   Z-ADD     PiVal         VABIAS
156200090623     C                   ELSE
156300090623     C                   SETON                                        32
156400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
156500090623     C                             + ' ' + 'VABIAS'
156600090623     C                   ENDIF
156700090623     C                   ENDIF
156800090623     C*** VLB
156900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_MPSVOLUME'
157000090623     C                   EVAL      PiStr=SkSplitCSV(i)
157100090623     C                   EXSR      CHKNUM
157200090623     C                   IF        PiNum=*on
157300111117     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
157400090623     C                   Z-ADD     PiVal         VABVLB
157500090623     C                   ELSE
157600090623     C                   SETON                                        32
157700090623     C                   EVAL      vinmsg = %trimr(vinmsg)
157800090623     C                             + ' ' + 'VABVLB'
157900090623     C                   ENDIF
158000090623     C                   ENDIF
158100090623     C*** QFT
158200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABQFT'
158300090623     C                   EVAL      PiStr=SkSplitCSV(i)
158400090623     C                   EXSR      CHKNUM
158500090623     C                   IF        PiNum=*on
158600090623     C                   Z-ADD     PiVal         VABQFT
158700090623     C                   ELSE
158800090623     C                   SETON                                        32
158900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
159000090623     C                             + ' ' + 'VABQFT'
159100090623     C                   ENDIF
159200090623     C                   ENDIF
159300090623     C*** NCD
159400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCD'
159500090623     C                   EVAL      PiStr=SkSplitCSV(i)
159600090623     C                   EXSR      CHKNUM
159700090623     C                   IF        PiInt=*on      AND
159800090623     C                             PiVal<=9999999
159900090623     C                   Z-ADD     PiVal         VABNCD
160000090623     C                   ELSE
160100090623     C                   SETON                                        32
160200090623     C                   EVAL      vinmsg = %trimr(vinmsg)
160300090623     C                             + ' ' + 'VABNCD'
160400090623     C                   ENDIF
160500090623     C                   ENDIF
160600090623     C*** NCA
160700111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABNCA'
160800090623     C                   EVAL      PiStr=SkSplitCSV(i)
160900090623     C                   EXSR      CHKNUM
161000090623     C                   IF        PiInt=*on      AND
161100090623     C                             PiVal<=9999999
161200090623     C                   Z-ADD     PiVal         VABNCA
161300090623     C                   ELSE
161400090623     C                   SETON                                        32
161500090623     C                   EVAL      vinmsg = %trimr(vinmsg)
161600090623     C                             + ' ' + 'VABNCA'
161700090623     C                   ENDIF
161800090623     C                   ENDIF
161900090623     C*** ZNC
162000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABZNC'
162100090623     C                   EVAL      PiStr=SkSplitCSV(i)
162200090623     C                   EXSR      CHKNUM
162300090623     C                   IF        PiInt=*on AND
162400090623     C                             PiVal<=99
162500090623     C                   Z-ADD     PiVal         VABZNC
162600090623     C                   ELSE
162700090623     C                   SETON                                        32
162800090623     C                   EVAL      vinmsg = %trimr(vinmsg)
162900090623     C                             + ' ' + 'VABZNC'
163000090623     C                   ENDIF
163100090623     C                   ENDIF
163200090623     C*** DCR
163300111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABDCR'
163400090623     C                   EVAL      PiStr=SkSplitCSV(i)
163500090623     C                   EXSR      CHKNUM
163600090623     C                   IF        PiInt=*on       AND
163700090623     C                             PiVal<=99999999
163800090623     C                   Z-ADD     PiVal         VABDCR
163900090623     C                   ELSE
164000090623     C                   SETON                                        32
164100090623     C                   EVAL      vinmsg = %trimr(vinmsg)
164200090623     C                             + ' ' + 'VABDCR'
164300090623     C                   ENDIF
164400090623     C                   ENDIF
164500090623     C*** HCR
164600111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABHCR'
164700090623     C                   EVAL      PiStr=SkSplitCSV(i)
164800090623     C                   EXSR      CHKNUM
164900090623     C                   IF        PiInt=*on   AND
165000090623     C                             PiVal<=9999
165100090623     C                   Z-ADD     PiVal         VABHCR
165200090623     C                   ELSE
165300090623     C                   SETON                                        32
165400090623     C                   EVAL      vinmsg = %trimr(vinmsg)
165500090623     C                             + ' ' + 'VABHCR'
165600090623     C                   ENDIF
165700090623     C                   ENDIF
165800090623     C*** VMD
165900111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABVMD'
166000090623     C                   EVAL      PiStr=SkSplitCSV(i)
166100090623     C                   EXSR      CHKNUM
166200090623     C                   IF        PiNum=*on
166300090623     C                   Z-ADD     PiVal         VABVMD
166400090623     C                   ELSE
166500090623     C                   SETON                                        32
166600090623     C                   EVAL      vinmsg = %trimr(vinmsg)
166700090623     C                             + ' ' + 'VABVMD'
166800090623     C                   ENDIF
166900090623     C                   ENDIF
167000090623     C*** ANT
167100111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABANT'
167200090623     C                   EVAL      PiStr=SkSplitCSV(i)
167300090623     C                   EXSR      CHKNUM
167400090623     C                   IF        PiInt=*on        AND
167500090623     C                             PiVal<=999999999
167600090623     C                   Z-ADD     PiVal         VABANT
167700090623     C                   ELSE
167800090623     C                   SETON                                        32
167900090623     C                   EVAL      vinmsg = %trimr(vinmsg)
168000090623     C                             + ' ' + 'VABANT'
168100090623     C                   ENDIF
168200090623     C                   ENDIF
168300090623     C*** CMO
168400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VABCMO'
168500090623     C                   EVAL      PiStr=SkSplitCSV(i)
168600090623     C                   EXSR      CHKNUM
168700090623     C                   IF        PiInt=*on
168800090623     C                   Z-ADD     PiVal         Num5_0
168900090623     C                   MOVEL(p)  Num5_0        VABCMO
169000090623     C                   ELSE
169100090623     C                   SETON                                        32
169200090623     C                   EVAL      VABCMO = *zeros
169300090623     C                   EVAL      vinmsg = %trimr(vinmsg)
169400090623     C                             + ' ' + 'VABCMO'
169500090623     C                   ENDIF
169600090623     C                   ENDIF
169700090623     C*
169800090911     C* Reperisco i campi del EDIVAT
169900090623     C*** VATNOT
170000111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATNOT'
170100090623     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
170200090623     C                   ENDIF
170300090623     C*** VATTRC
170400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'VATTRC'
170500090623     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
170600090623     C                   ENDIF
170700090623     C*** VATNOT_A
170800111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCONTACT'
170900090623     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
171000090623     C                   ENDIF
171100090623     C*** VATNOT_B
171200111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
171300090623     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
171400090623     C                   ENDIF
171500141125     C*** VATNOT_S
171600141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
171700141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
171800141125     C                   ENDIF
171900141125     C*** VATNOT_IJ
172000141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
172100141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
172200141125     C                   ENDIF
172300111118     C*** VATNOT_E_4
172400111118     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RCOUNTRYN'
172500111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
172600111118     C                   ENDIF
172700090623     C*
172800090623     C***  ===>  Gestione campi "particolari" dentro ciclo
172900090623     C*
173000090623     C*
173100090623     C***  <===  -----------------------------------------
173200090908     C*
173300090623     C*
173400090623     C                   ENDSR
173500090623     C***
173600090623
173700111118
173800111118
173900111118     C*----------------------------------------------------*
174000111118     C*  ROUTINE DI VALORIZZAZIONE CAMPI
174100111118     C*----------------------------------------------------*
174200111118     C     CARVALP       BEGSR
174300111118     C*
174400111118     C* Reperisco i campi del EDIVAT
174500111118     C*** VATNOT
174600111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATNOT'
174700111118     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
174800111118     C                   ENDIF
174900111118     C*** VATTRC
175000111118     C                   IF        %trim(SkSplitFLDP(i)) = 'VATTRC'
175100111118     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
175200111118     C                   ENDIF
175300111118     C*** VATNOT_A
175400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RCONTACT'
175500111118     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
175600111118     C                   ENDIF
175700111118     C*** VATNOT_B
175800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'HDR_RPHONE'
175900111118     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
176000111118     C                   ENDIF
176100141125     C*** VATNOT_S
176200141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_RPHONE'
176300141125     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
176400141125     C                   ENDIF
176500141125     C*** VATNOT_IJ
176600141125     C                   IF        %trim(SkSplitFLDH(i)) = 'HDR_REMAIL'
176700141125     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
176800141125     C                   ENDIF
176900111216     C*** VATNOT_E_1
177000111216     C***                IF        %trim(SkSplitFLDP(i)) = 'PCL_CREF1'
177100111216     C***                EVAL      wVATNOT_E_1 = '%'+%subst(SkSplitCSV(i):7:4)
177200111216     C***                ENDIF
177300111118     C*** VATNOT_E_2
177400111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_PARCELNO'
177500111215     C                   EVAL      wVATNOT_E_2 = SkSplitCSV(i)
177600111118     C                   ENDIF
177700111118     C*** VATNOT_E_3
177800111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_SERVICE'
177900111118     C                   EVAL      wVATNOT_E_3 = SkSplitCSV(i)
178000111118     C                   ENDIF
178100111118     C*** VATNOT_E_4
178200111118     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_RCOUNTRYN'
178300111118     C                   EVAL      wVATNOT_E_4 = SkSplitCSV(i)
178400111118     C                   ENDIF
178500170823     C*** PKB
178600170825     C                   IF        %trim(SkSplitFLDP(i)) = 'PCL_WEIGHT'
178700170823     C                   EVAL      PiStr=SkSplitCSV(i)
178800170823     C                   EXSR      CHKNUM
178900170823     C                   IF        PiNum=*on
179000170823     C                   EVAL      PiVal = PiVal / 100                          * da Dg. a Kg.
179100170823     C                   ADD       PiVal         wVABPKB_PCL
179200170823     C                   ELSE
179300170823     C                   SETON                                        32
179400170823     C                   EVAL      wVABPKB_PCL = *zeros
179500170823     C                   EVAL      vinmsg = %trimr(vinmsg)
179600170823     C                             + ' ' + 'VABPKB'
179700170823     C                   ENDIF
179800170823     C                   ENDIF
179900111118     C*
180000111118     C***  ===>  Gestione campi "particolari" dentro ciclo
180100111118     C*
180200111118     C*
180300111118     C***  <===  -----------------------------------------
180400111118     C*
180500111118     C*
180600111118     C                   ENDSR
180700111118     C***
180800010601
180900020204
181000020204     C*----------------------------------------------------*
181100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
181200020204     C*----------------------------------------------------*
181300020204     C     CHKIMPDIV     BEGSR
181400020204     C*
181500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
181600020204     C                   Z-ADD     *zeros        wrkDec            9 9
181700020204     C*
181800020204     C* Come prima cosa effettuo considerazioni sulla divisa
181900020204     C                   IF        vabIAS > *zeros
182000020204     C                   IF        vabVAS <> 'EUR'
182100020204     C                   EVAL      vabVAS =  'ITL'
182200020204     C                   ENDIF
182300020204     C                   ENDIF
182400020204     C*
182500020204     C                   IF        vabCAS > *zeros
182600020204     C                   IF        vabVCA <> 'EUR'
182700020204     C                   EVAL      vabVCA =  'ITL'
182800020204     C                   ENDIF
182900020204     C                   ENDIF
183000020204     C*
183100020204     C                   IF        vabVMD > *zeros
183200020204     C                   IF        vabVAD <> 'EUR'
183300020204     C                   EVAL      vabVAD =  'ITL'
183400020204     C                   ENDIF
183500020204     C                   ENDIF
183600020204     C*
183700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
183800020204     C                   Z-ADD     vabIAS        wrkDec
183900020204     C                   IF        wrkDec > *zeros
184000020204     C                   IF        vabVAS = 'ITL'
184100020204     C                   EVAL      vabIAS = *zeros
184200020204     C                   ENDIF
184300020204     C                   ENDIF
184400020204     C*
184500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
184600020204     C                   Z-ADD     vabCAS        wrkDec
184700020204     C                   IF        wrkDec > *zeros
184800020204     C                   IF        vabVCA = 'ITL'
184900020204     C                   EVAL      vabCAS = *zeros
185000020204     C                   ENDIF
185100020204     C                   ENDIF
185200020204     C*
185300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
185400020204     C                   Z-ADD     vabVMD        wrkDec
185500020204     C                   IF        wrkDec > *zeros
185600020204     C                   IF        vabVAD = 'ITL'
185700020204     C                   EVAL      vabVMD = *zeros
185800020204     C                   ENDIF
185900020204     C                   ENDIF
186000020204     C*
186100020204     C                   ENDSR
186200020204     C***
186300080617
186400080617
186500080617
186600080617     C*----------------------------------------------------*
186700080617     C*  CONTROLLO NUMERICITA' CAMPI
186800080617     C*----------------------------------------------------*
186900080617     C     CHKNUM        BEGSR
187000080617     C*
187100080617     C                   IF        PiDecChr = *blanks
187200080617     C                   EVAL      PiDecChr = CharNUM
187300080617     C                   ENDIF
187400080617     C*
187500080617     C                   callp     UBISNUM_Check(PiStr
187600080617     C                                          :PiDecChr
187700080617     C                                          :PiVal
187800080617     C                                          :PiNum
187900080617     C                                          :PiInt)
188000080617     C*
188100080617     C                   ENDSR
188200080617     C***
188300010330
188400010601
188500010601
188600010601
188700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
188800010601     C     repfil        BEGSR
188900010601     C*
189000010601     C                   if        invfil = *zeros and
189100010601     C                             depfil > *zeros and
189200010629     C                             (vinflg = *blanks or
189300010629     C                              vinflg = *zeros)
189400010601     C
189500010601     C                   eval      invfil = depfil
189600010601     C                   endif
189700010601     C*
189800010601     C                   if        depfil <> invfil and
189900010601     C                             invfil > *zeros
190000010601     C                   eval      flgMulti = '1'
190100010601     C                   if        vinflg = *blanks
190200010601     C                   add       1             cntNonEl
190300010601     C                   endif
190400010601     C                   endif
190500010601     C*
190600010601     C                   if        vinflg = '2'
190700010601     C                   eval      flgStato = '2'
190800010601     C                   endif
190900010601     C*
191000010601     C                   ENDSR
191100010601     C***
191200010601
191300010601
191400010601
191500010330
191600040119      /TITLE Invio dei dati al punto operativo.
191700040119     C     invio         BEGSR
191800040119     C*
191900101005     C* 1° invio EDIVAT
192000040119     C                   reset                   dscmz
192100040119     C                   move      vlrpoi        cmzdst
192200090911     C                   eval      cmzfld = 'EDIVATWR'
192300040119     C                   eval      cmzmbd = vlrhdl
192400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
192500040119     C***                if        prmfir = *blanks
192600090911     C                   eval      cmzfla = 'EDIVAT0F'
192700090911     C                   eval      cmzmba = 'EDIVAT0F'
192800040119     C***                else
192900040119     C***                eval      cmzfla = prmfir
193000040119     C***                eval      cmzmba = prmfir
193100040119     C***                endif
193200040119     C                   eval      cmznrr = *zeros
193300040119     C                   move      §ctrokvt      cmznrr
193400040119     C                   eval      cmzlba = vlrfl1
193500040119     C                   call(e)   'TIS711C'
193600040119     C                   parm                    dscmz
193700040119     C                   parm      *blanks       esito
193800040119     C                   if        %error
193900040119     C                             or cmzerr = '1'
194000040119     C                             or esito  = '1'
194100040119     C                   eval      wrkesito = '3'
194200040119     C                   else
194300040119     C*
194400101005     C* 2° invio EDIVAB
194500040119     C                   reset                   dscmz
194600040119     C                   move      vlrpoi        cmzdst
194700040119     C                   eval      cmzfld = vlrfou
194800040119     C                   eval      cmzmbd = vlrhdl
194900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
195000040119     C***                if        prmfir = *blanks
195100090911     C                   eval      cmzfla = 'EDIVAB0F'
195200090911     C                   eval      cmzmba = 'EDIVAB0F'
195300040119     C***                else
195400040119     C***                eval      cmzfla = prmfir
195500040119     C***                eval      cmzmba = prmfir
195600040119     C***                endif
195700040119     C                   eval      cmznrr = *zeros
195800040119     C                   move      §ctrokvb      cmznrr
195900040119     C                   eval      cmzlba = vlrfl1
196000040119     C                   call(e)   'TIS711C'
196100040119     C                   parm                    dscmz
196200040119     C                   parm      *blanks       esito
196300040119     C                   if        %error
196400040119     C                             or cmzerr = '1'
196500040119     C                             or esito  = '1'
196600040119     C                   eval      wrkesito = '3'
196700040119     C                   endif
196800040119     C                   endif
196900040119     C*
197000040119     C                   ENDSR
197100040119     C***
197200010601
197300010601
197400010601
197500010601
197600010601      /TITLE Invio dei dati al punto operativo.
197700010601     C     opeini        BEGSR
197800010601     C*
197900010601     C* Inizializzo flag e contatori operativi
198000010601     C                   movel     '0'           flgGiro           1
198100010601     C                   movel     '0'           flgMulti          1
198200010601     C                   movel     '1'           flgStato          1
198300010615     C                   movel     '0'           flgOk             1
198400010601     C                   z-add     *zeros        cntNonEl         10 0
198500010601     C                   z-add     *zeros        depfil            3 0
198600010601     C                   z-add     *zeros        invfil            3 0
198700010601     C*
198800010601     C                   ENDSR
198900010601     C***
199000070326
199100070326
199200070326
199300070326
199400080916     C     *pssr         BEGSR
199500070329     C*
199600080916     C                   if        %open(tivin00r)
199700080916     C                   close     tivin00r
199800080916     C                   endif
199900090911     C                   if        %open(edivabwr)
200000090911     C                   close     edivabwr
200100080916     C                   endif
200200090911     C                   if        %open(edivatwr)
200300090911     C                   close     edivatwr
200400080916     C                   endif
200500070326     C*
200600070326     C* Effettuo la chiamata al CLLE preposto
200700101005     C                   call(e)   'TITVEVTC'
200800080916     C                   parm                    parccm
200900080916     C                   parm                    parmbr
201000080916     C                   parm      '2'           paropz
201100070326     C*
201200080916     C                   eval      wrkesito = '2'
201300070404     C*
201400080916     C                   seton                                        LR
201500070326     C*
201600080916     C                   ENDSR     '*CANCL'
201700070326     C***
201800070326
201900070326
202000010330
202100010330
202200000613     C     *inzsr        BEGSR
202300990910     C*
202400990910     C     *entry        plist
202500990920     C                   parm                    tivlrds
202600990921     C                   parm      wrkesito      esito
202700000724     C                   parm                    prmlit
202800000710     C                   parm                    prmfir
202900010330     C*
203000010330     C* CALCOLA LA DATA CORRENTE
203100010330     C                   time                    wn14             14 0
203200090911     C                   movel     wn14          oracor            6 0          *ORA
203300110923     C                   z-add     *zeros        datcor            8 0
203400110923     C                   eval      datcor = %dec(%date() : *ISO)
203500111118     C*
203600111118     C* Forzo un numero CMR
203700111118     C                   eval      wCMR = 'DPD NL '+%char(datcor)+%char(oracor)
203800000613     C*
203900000613     C                   ENDSR
204000000613     C***
