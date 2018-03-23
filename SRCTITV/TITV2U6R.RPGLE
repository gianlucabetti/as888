000100110930      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110930     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600110930     FEDIVATwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000110930     D edivabs       e ds                  extname(edivab0f)
002100110930     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002200110930     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
002300010330     D esito           s              1
002400010330     D prmlit          s             10
002500010330     D prmfir          s             10
002600010330     D wrkesito        s                   like(esito)
002700010330     D rrnum           s              6  0 INZ(*zeros)
002800040510     D parccm          s              8    INZ(*blanks)
002900040119     D parmbr          s             10    INZ(*blanks)
003000040119     D paropz          s              1    INZ(*blanks)
003100040119     D chkcall         s              1    INZ(*blanks)
003200080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003300080418     D wFlgCAS         s              1    INZ(*blanks)
003400100326     D*------------------
003500100326     D* DS REPERIMENTO NUMERATORE
003600100326     D*------------------
003700100326     D trul33ds      e ds                  inz
003800100326     D*------------------
003900100326     D* DS ARCHITETTURA
004000100326     D*------------------
004100100326     D kpjba         e ds                  inz
004200160427     D*------------------
004300160427     D* DS CALCOLO CHECK-DIGITS MOD 10 (cod. 103)
004400160427     D*------------------
004500160427     D TRUL28DS      E DS
004600100326
004700030715     D*------------------
004800030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004900030715     D*------------------
005000160503     D SkSplitFLD      S             20    DIM(100)
005100030715     D SkSplitCSV      S            256    DIM(100)
005200030715     D CharCSV         S              1
005300030715     D CharTXT         S              1
005400030715     D CharNUM         S              1
005500100318     D CharSOS         S              1
005600030715     D posDa           S              3  0 INZ(*zeros)
005700030715     D posA            S              3  0 INZ(*zeros)
005800030715     D i               s              3  0 INZ(1)
005900030715     D wGiro           s              1  0 INZ(*zeros)
006000070823     D wVATNOT_A       s                   LIKE(VATNOT)
006100060331     D wVATNOT_B       s                   LIKE(VATNOT)
006200060331     D wVATNOT_E       s                   LIKE(VATNOT)
006300110927     D wVATNOT_I       s                   LIKE(VATNOT)
006400111124     D wVATNOT_J       s                   LIKE(VATNOT)
006500120405     D wVATNOT_4       s                   LIKE(VATNOT)
006600131018     D wVATNOT_S       s                   LIKE(VATNOT)
006700141128     D wVATNOT_Y       s                   LIKE(VATNOT)
006800030822     D*------------------
006900030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007000030822     D*------------------
007100030822     D posDaDft        S              3  0 INZ(*zeros)
007200030822     D posADft         S              3  0 INZ(*zeros)
007300030822     D j               s              3  0 INZ(1)
007400030822     D wGiroDft        s              1  0 INZ(*zeros)
007500080418     D*------------------
007600080418     D* Costanti
007700080418     D*------------------
007800080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007900080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008000100318
008100100318     D*------------------
008200100318     D* LINKING A DEFINIZIONI ESTERNE
008300100318     D*------------------
008400100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008500100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008600010330
008700010330
008800010330
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100010601     C*
011200010601     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400040119     C*
012500040119     C* Effettuo la chiamata al CLLE preposto
012600110930     C                   call(e)   'TITVEVTC'
012700040510     C                   parm                    parccm
012800040119     C                   parm                    parmbr
012900040119     C                   parm      '2'           paropz
013000050201     C*
013100050201     C* Effettuo lancio TISI95 solo x chiusura
013200050201     C                   CLEAR                   TISI95DS
013300050201     C                   EVAL      I95TLA = 'C'
013400050201     C                   CALL      'TISI95R'
013500050201     C                   PARM                    TISI95DS
013600000616     C*
013700010601     C                   ENDSR
013800010601     C***
013900000613
014000010601
014100010601
014200010330     C*--------------------------------------------------------
014300110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
014400010330     C*--------------------------------------------------------
014500010605     C     RWVAB         BEGSR
014600010330     C*
014700010330     C                   if        not %open(tivin00r)
014800010330     C                   open      tivin00r
014900010330     C                   endif
015000110930     C                   if        not %open(edivabwr)
015100110930     C                   open      edivabwr
015200010330     C                   endif
015300040119     C*
015400110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015500040119     C                   exsr      prevat
015600040119     C*
015700040119     C                   if        chkcall = '0'
015800040119     C*
015900110930     C                   if        not %open(edivatwr)
016000110930     C                   open      edivatwr
016100040119     C                   endif
016200010330     C*
016300040119     C                   clear                   §CTROKVB          7 0
016400040119     C                   clear                   §CTROKVT          7 0
016500040119     C                   clear                   §CTRMO            7 0
016600040119     C                   clear                   §CTRNO            7 0
016700070823     C*
016800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017000160421     C                   exsr      inzvar
017100160421     C                   exsr      impvab
017200110131     C*
017300110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017400110912 xxx C***                read      tivin00r                               70
017500010330     C*
017600030822     C                   do        *HIVAL
017700010330     C*
017800030822     C                   read      tivin00r                               70
017900010618     C*
018000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018100010618     C                   if        vinflg = '1'
018200010618     C                   eval      flgOk = '1'
018300010618     C                   endif
018400040510     C*
018500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018600040510     C                   z-add     *zeros        wLenVINDTA        4 0
018700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019100040510     C                   z-add     1             wFlgVINDTA
019200040510     C                   leave
019300040510     C                   endif
019400040510     C                   enddo
019500010618     C*
019600040510     C                   if        vindta > *blanks AND
019700040510     C                             wFlgVINDTA = 1
019800010330     C                   add       1             rrnum
019900010330     C*
020000010601     C                   if        *in70 = *off and
020100010330     C                             (vinflg = *blanks
020200010330     C                              or vinflg = '0'
020300010330     C                              or vinflg = '2')
020400010330     C*
020500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020600010711     C                   if        vinflg = *blanks or vinflg = '0'
020700010711     C                   clear                   vinmsg
020800010711     C                   endif
020900010601     C*
021000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021100070925     C                   eval      vindta  = ' ' + %trim(vindta)
021200070925     C*
021300080527     C                   exsr      inzvar
021400100318     C                   exsr      normCSV
021500010605     C                   exsr      impvab
021600010601     C*
021700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021800010605     C                   eval      depfil = VABLNP
021900010601     C                   exsr      repfil
022000010601     C                   if        depfil = invfil
022100021025     C                   if        vlrpoi = 999
022200030822     C                   move(P)   invfil        VABFGS
022300021025     C                   else
022400030822     C                   move(P)   vlrpoi        VABFGS
022500021025     C                   endif
022600010601     C*
022700010601     C                   exsr      PREELA
022800010601     C*
022900010604     C* Ebbene...
023000010604     C*
023100040119     C  N31              add       1             §CTROKVB
023200040119     C   32              add       1             §CTRMO
023300040119     C   31              add       1             §CTRNO
023400080418     C*
023500030822     C                   if        wGiro = 2
023600080513     C* Scrivo sempre il VAT
023700080513     C  N31              exsr      wrivat
023800110201     C                   exsr      chkvab
023900080513     C                   endif
024000010604     C*
024100010604     C                   if        *in31 = *off and
024200010604     C                             *in32 = *off
024300010604     C                   eval      vinflg = '1'
024400010604     C                   else
024500010604     C                   eval      vinflg = '2'
024600010604     C                   endif
024700010604     C                   endif
024800010601     C*
024900010604     C                   endif
025000010604     C*
025100010330     C                   else
025200010330     C                   eval      vinflg = '1'
025300010330     C                   endif
025400010601     C*
025500010601     C  N70              update    tivin000
025600010330     C*
025700030822     C  N70              enddo
025800040119     C*
025900080527     C* Verifico dati rimasti "in cannna"...
026000080527     C                   seton                                        65
026100110201     C                   exsr      chkvab
026200080527     C*
026300040119     C                   endif                                                  (endif - chkcall)
026400010601     C*
026500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026600010601     C                   if        cntNonEl = *zeros or
026700010601     C                             flgMulti = '0'
026800010330     C* Se non ci sono record con errori ...
026900010601     C                   if        §ctrno = 0 and
027000010604     C                             §ctrmo = 0 and
027100010601     C                             flgStato <> '2'
027200010330     C* ... restituisco esito OK.
027300010330     C                   eval      wrkesito = '0'
027400010330     C                   else
027500040119     C                   if        §ctrokvb > 0
027600010330     C                   eval      wrkesito = '1'
027700010330     C                   else
027800010615     C                   if        flgOk = '0'
027900010615     C                   eval      wrkesito = '2'
028000010615     C                   else
028100010615     C                   eval      wrkesito = '6'
028200010615     C                   endif
028300010330     C                   endif
028400010330     C                   endif
028500010601     C                   else
028600010601     C                   eval      wrkesito = '9'
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        %open(tivin00r)
029000010330     C                   close     tivin00r
029100010330     C                   endif
029200110930     C                   if        %open(edivabwr)
029300110930     C                   close     edivabwr
029400010330     C                   endif
029500110930     C                   if        %open(edivatwr)
029600110930     C                   close     edivatwr
029700040512     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300040119     C                   if        §ctrokvb > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010618     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010618     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400080527
031500080527
031600080527
031700080527     C*----------------------------------------------------*
031800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031900080527     C*----------------------------------------------------*
032000100413     C     CHKVAB        BEGSR
032100110201     C*
032200110201     C* Se richiesti ignorare colli / peso / volume
032300110315 xxx C***                if        *in35
032400110315 xxx C***                z-add     *zeros        VABNCL
032500110315 xxx C***                z-add     *zeros        VABPKB
032600110315 xxx C***                z-add     *zeros        VABVLB
032700110315 xxx C***                endif
032800080527     C*
032900080527     C* Scrivo il VAB solo a rottura d spedizione
033000080527     C                   if        depspe = *zeros
033100110930     C  N31              eval      edivabs_sav = edivabs
033200080527     C                   eval      depspe = vabNSP
033300080527     C                   else
033400080527     C                   if        depspe = vabNSP AND *in65 = *off
033500100413     C   51              add       VABNCL        sav_VABNCL
033600100413     C   51              add       VABPKB        sav_VABPKB
033700100413     C   51              add       VABVLB        sav_VABVLB
033800100720     C   52              z-add     VABNCL        sav_VABNCL
033900100720     C   52              z-add     VABPKB        sav_VABPKB
034000100720     C   52              z-add     VABVLB        sav_VABVLB
034100100720     C   53              add       VABNCL        sav_VABNCL
034200100720     C   53              z-add     VABPKB        sav_VABPKB
034300100720     C   53              z-add     VABVLB        sav_VABVLB
034400080527     C                   else
034500110930     C  N31              eval      edivabs_ok = edivabs_sav
034600080527     C  N31              exsr      wrivab
034700110930     C  N31              eval      edivabs_sav = edivabs
034800080527     C                   eval      depspe = vabNSP
034900080527     C                   endif
035000080527     C                   endif
035100080527     C*
035200080527     C                   ENDSR
035300040119
035400040119
035500040119
035600040119     C*----------------------------------------------------*
035700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035800040119     C*----------------------------------------------------*
035900040510     C     WRIVAB        BEGSR
036000080513     C*
036100080513     C* Gestisco l'eventuale rottura x numero spedizione
036200111124     C                   if        VABNSP <> depspe or *in65 = *on
036300070301     C                   if        wVATNOT_E <> *blanks
036400111124     C                   eval      ok_VABCTM = '7Q'
036500070301     C                   endif
036600110930     C*
036700110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
036800111124     C                   EVAL      ok_VABCMR = %char(datcor)+%char(oracor)
036900111124     C                   EVAL      ok_VABDCM = DATCOR
037000111124     C                   EVAL      ok_VABDTS = DATCOR
037100111124     C                   EVAL      ok_VABHMS = ORACOR
037200111124     C                   EVAL      ok_VABCNT = 1
037300110930     C*
037400110930     C                   write     edivab00
037500080513     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038300040119     C*----------------------------------------------------*
038400040510     C     WRIVAT        BEGSR
038500060512     C*
038600060512     C* Inizializzo i campi chiave
038700060512     C                   eval      VATFGS = VABFGS
038800060512     C                   eval      VATAAS = VABAAS
038900060512     C                   eval      VATCCM = VABCCM
039000070823     C                   eval      VATNRS = VABNRS
039100060512     C                   eval      VATNSP = VABNSP
039200060512     C                   eval      VATLNP = VABLNP
039300040119     C*
039400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039500040510     C                   if        VATTRC = *blanks
039600040510     C                   eval      VATTRC = 'E'
039700040510     C                   endif
039800110318     C                   if        VATNOT <> *blanks
039900110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
040000110930     C                   eval      VATCNT = 1
040100110930     C                   write     EDIVAT00
040200040119     C                   add       1             §CTROKVT
040300110318     C                   endif
040400060331     C*
040500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040600060331     C                   if        wVATNOT_A <> *blanks
040700060331     C                   eval      VATNOT = wVATNOT_A
040800060331     C                   eval      VATTRC = 'A'
040900110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041000110930     C                   eval      VATCNT = 1
041100110930     C                   write     EDIVAT00
041200060331     C                   add       1             §CTROKVT
041300060331     C                   endif
041400060331     C*
041500060331     C                   if        wVATNOT_B <> *blanks
041600060331     C                   eval      VATNOT = wVATNOT_B
041700060331     C                   eval      VATTRC = 'B'
041800110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041900110930     C                   eval      VATCNT = 1
042000110930     C                   write     EDIVAT00
042100060331     C                   add       1             §CTROKVT
042200060331     C                   endif
042300060331     C*
042400060331     C                   if        wVATNOT_E <> *blanks
042500110201     C                   eval      VATNOT = wVATNOT_E
042600060331     C                   eval      VATTRC = 'E'
042700110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
042800110930     C                   eval      VATCNT = 1
042900110930     C                   write     EDIVAT00
043000110201     C                   add       1             §CTROKVT
043100110201     C                   endif
043200110927     C*
043300110927     C                   if        wVATNOT_I <> *blanks
043400110927     C                   eval      VATNOT = wVATNOT_I
043500110927     C                   eval      VATTRC = 'I'
043600110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
043700110930     C                   eval      VATCNT = 1
043800110930     C                   write     EDIVAT00
043900110927     C                   add       1             §CTROKVT
044000110927     C                   endif
044100111124     C*
044200111124     C                   if        wVATNOT_J <> *blanks
044300111124     C                   eval      VATNOT = wVATNOT_J
044400111124     C                   eval      VATTRC = 'J'
044500111124     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044600111124     C                   eval      VATCNT = 1
044700111124     C                   write     EDIVAT00
044800111124     C                   add       1             §CTROKVT
044900111124     C                   endif
045000120405     C*
045100120405     C                   if        wVATNOT_4 <> *blanks
045200120405     C                   eval      VATNOT = wVATNOT_4
045300120405     C                   eval      VATTRC = '4'
045400120405     C                   eval      VATCMR = %char(datcor)+%char(oracor)
045500120405     C                   eval      VATCNT = 1
045600120405     C                   write     EDIVAT00
045700120405     C                   add       1             §CTROKVT
045800120405     C                   endif
045900131018     C*
046000131018     C                   if        wVATNOT_S <> *blanks
046100131018     C                   eval      VATNOT = wVATNOT_S
046200131018     C                   eval      VATTRC = 'S'
046300131018     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046400131018     C                   eval      VATCNT = 1
046500131018     C                   write     EDIVAT00
046600131018     C                   add       1             §CTROKVT
046700131018     C                   endif
046800141128     C*
046900141128     C                   if        wVATNOT_Y <> *blanks
047000141128     C                   eval      VATNOT = wVATNOT_Y
047100141128     C                   eval      VATTRC = 'Y'
047200141128     C                   eval      VATCMR = %char(datcor)+%char(oracor)
047300141128     C                   eval      VATCNT = 1
047400141128     C                   write     EDIVAT00
047500141128     C                   add       1             §CTROKVT
047600141128     C                   endif
047700040119     C*
047800040119     C                   ENDSR
047900040119
048000040119
048100040119
048200040119     C*----------------------------------------------------*
048300110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048400040119     C*----------------------------------------------------*
048500040119     C     PREVAT        BEGSR
048600040119     C*
048700110930     C* Compongo il nome del membro da dare al EDIVATWR
048800040119     C                   eval      parmbr = vlrhdl
048900040119     C                   movel     'M'           parmbr
049000040510     C                   eval      parccm = vlrksc
049100040119     C                   eval      paropz = '1'
049200040119     C* Effettuo la chiamata al CLLE preposto
049300110930     C                   call(e)   'TITVEVTC'
049400040119     C                   parm                    parccm
049500040119     C                   parm                    parmbr
049600040119     C                   parm                    paropz
049700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049800040119     C                   if        %error
049900040119     C                   movel     '1'           chkcall
050000040119     C                   else
050100040119     C                   movel     '0'           chkcall
050200040119     C                   endif
050300040119     C*
050400040119     C                   ENDSR
050500010601
050600010601
050700010601
050800010330     C*----------------------------------------------------*
050900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
051000010330     C*----------------------------------------------------*
051100040119     C     INZVAR        BEGSR
051200080527     C*
051300080527     C                   setoff                                       65
051400010330     C*
051500030822     C* Inizializzo variabili di wrk
051600020204     C                   Z-ADD     *zeros        Num5_0            5 0
051700101018     C                   MOVEL     *blanks       wFlgCAS
051800060331     C                   MOVEL     *blanks       wVATNOT_A
051900060331     C                   MOVEL     *blanks       wVATNOT_B
052000060331     C                   MOVEL     *blanks       wVATNOT_E
052100110927     C                   MOVEL     *blanks       wVATNOT_I
052200111124     C                   MOVEL     *blanks       wVATNOT_J
052300120405     C                   MOVEL     *blanks       wVATNOT_4
052400131018     C                   MOVEL     *blanks       wVATNOT_S
052500141128     C                   MOVEL     *blanks       wVATNOT_Y
052600030822     C*
052700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052800030822     C                   CLEAR                   SkSplitCSV
052900030822     C                   EVAL      i = 1
053000030822     C                   EVAL      posDa = *zeros
053100030822     C                   EVAL      posA  = *zeros
053200080527     C*
053300110930     C                   CLEAR                   EDIVABS
053400110930     C                   CLEAR                   EDIVAT00
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
055000100318     C*
055100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
055200100318     C                   EVAL      CharSOS = CharNUM
055300030822     C*
055400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055500100720     C                   SETOFF                                       505152
055600100720     C                   SETOFF                                       53
055700100413     C                   SELECT
055800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055900100720     C                   SETON                                        50
056000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
056100100413     C                   SETON                                        51
056200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
056300100720     C                   SETON                                        52
056400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056500100720     C                   SETON                                        53
056600100413     C                   ENDSL
056700030822     C*
056800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056900030822     C                   EVAL      posDaDft = 1
057000030822     C                   EVAL      posADft  = 0
057100030822     C                   EVAL      wGiroDft = 0
057200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
057300030822     C                             posDaDft > 0
057400030822     C*
057500030822     C* Gestisco il 1° giro
057600030822     C                   IF        wGiroDft = 0
057700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057900030822     C* Incremento il contatore dei "giri"
058000030822     C                   EVAL      wGiroDft = 1
058100030822     C                   ELSE
058200030822     C                   EVAL      posDaDft = posADft
058300030822     C                   ENDIF
058400030822     C* Eseguo lo scan x trovare la fine del campo corrente
058500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058600030822     C*
058700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058900030822     C                   IF        posDaDft > 0 AND
059000030822     C                             posADft  > 0
059100030822     C* NCL
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'NCL'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABNCL
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* CCM
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'CCM'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABCCM
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* LNP
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'LNP'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiInt=*on
062700030822     C                   Z-ADD     PiVal         VABLNP
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* NRS
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'NRS'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiInt=*on
064000030822     C                   Z-ADD     PiVal         VABNRS
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CTR
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CTR'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiInt=*on
065300030822     C                   Z-ADD     PiVal         VABCTR
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* PKB
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'PKB'
066100030822     C                   EVAL      PiStr=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   EXSR      CHKNUM
066500030822     C                   IF        PiNum=*on
066600030822     C                   Z-ADD     PiVal         VABPKB
066700030822     C                   ENDIF
066800030822     C                   ENDIF
066900030822     C* VLB
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VLB'
067400030822     C                   EVAL      PiStr=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   EXSR      CHKNUM
067800030822     C                   IF        PiNum=*on
067900030822     C                   Z-ADD     PiVal         VABVLB
068000030822     C                   ENDIF
068100030822     C                   ENDIF
068200030822     C* QFT
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'QFT'
068700030822     C                   EVAL      PiStr=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   EXSR      CHKNUM
069100030822     C                   IF        PiNum=*on
069200030822     C                   Z-ADD     PiVal         VABQFT
069300030822     C                   ENDIF
069400030822     C                   ENDIF
069500030822     C* CBO
069600030822     C                   IF        %subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):1:3)
069900030822     C                             = 'CBO'
070000030822     C                   EVAL      VABCBO=%trim(%subst(
070100030822     C                             %subst(vlrppt:posDaDft+1:
070200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070300030822     C                   ENDIF
070400030822     C* TSP
070500030822     C                   IF        %subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):1:3)
070800030822     C                             = 'TSP'
070900030822     C                   EVAL      VABTSP=%trim(%subst(
071000030822     C                             %subst(vlrppt:posDaDft+1:
071100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071200030822     C                   ENDIF
071300030822     C* VAS
071400030822     C                   IF        %subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):1:3)
071700030822     C                             = 'VAS'
071800030822     C                   EVAL      VABVAS=%trim(%subst(
071900030822     C                             %subst(vlrppt:posDaDft+1:
072000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072100030822     C                   ENDIF
072200030822     C* VCA
072300030822     C                   IF        %subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):1:3)
072600030822     C                             = 'VCA'
072700030822     C                   EVAL      VABVCA=%trim(%subst(
072800030822     C                             %subst(vlrppt:posDaDft+1:
072900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073000030822     C                   ENDIF
073100070925     C* TIC
073200070925     C                   IF        %subst(
073300070925     C                             %subst(vlrppt:posDaDft+1:
073400070925     C                             posADft-posDaDft-1):1:3)
073500070925     C                             = 'TIC'
073600070925     C                   EVAL      VABTIC=%trim(%subst(
073700070925     C                             %subst(vlrppt:posDaDft+1:
073800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073900070925     C                   ENDIF
074000030822     C* GCA
074100030822     C                   IF        %subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):1:3)
074400030822     C                             = 'GCA'
074500030822     C                   EVAL      VABGCA=%trim(%subst(
074600030822     C                             %subst(vlrppt:posDaDft+1:
074700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074800030822     C                   ENDIF
074900030822     C* CTM
075000030822     C                   IF        %subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):1:3)
075300030822     C                             = 'CTM'
075400030822     C                   EVAL      VABCTM=%trim(%subst(
075500030822     C                             %subst(vlrppt:posDaDft+1:
075600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075700030822     C                   ENDIF
075800030822     C* FFD
075900030822     C                   IF        %subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):1:3)
076200030822     C                             = 'FFD'
076300030822     C                   EVAL      VABFFD=%trim(%subst(
076400030822     C                             %subst(vlrppt:posDaDft+1:
076500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076600030822     C                   ENDIF
076700030822     C* VAD
076800030822     C                   IF        %subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):1:3)
077100030822     C                             = 'VAD'
077200030822     C                   EVAL      VABVAD=%trim(%subst(
077300030822     C                             %subst(vlrppt:posDaDft+1:
077400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077500030822     C                   ENDIF
077600030822     C* GMA
077700030822     C                   IF        %subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):1:3)
078000030822     C                             = 'GMA'
078100030822     C                   EVAL      VABGMA=%trim(%subst(
078200030822     C                             %subst(vlrppt:posDaDft+1:
078300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078400030822     C                   ENDIF
078500030822     C* GGA
078600030822     C                   IF        %subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):1:3)
078900030822     C                             = 'GGA'
079000030822     C                   EVAL      VABGGA=%trim(%subst(
079100030822     C                             %subst(vlrppt:posDaDft+1:
079200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079300030822     C                   ENDIF
079400030822     C* GVA
079500030822     C                   IF        %subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):1:3)
079800030822     C                             = 'GVA'
079900030822     C                   EVAL      VABGVA=%trim(%subst(
080000030822     C                             %subst(vlrppt:posDaDft+1:
080100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080200030822     C                   ENDIF
080300030822     C* TC1
080400030822     C                   IF        %subst(
080500030822     C                             %subst(vlrppt:posDaDft+1:
080600030822     C                             posADft-posDaDft-1):1:3)
080700030822     C                             = 'TC1'
080800030822     C                   EVAL      VABTC1=%trim(%subst(
080900030822     C                             %subst(vlrppt:posDaDft+1:
081000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
081100030822     C                   ENDIF
081200030822     C* TC2
081300030822     C                   IF        %subst(
081400030822     C                             %subst(vlrppt:posDaDft+1:
081500030822     C                             posADft-posDaDft-1):1:3)
081600030822     C                             = 'TC2'
081700030822     C                   EVAL      VABTC2=%trim(%subst(
081800030822     C                             %subst(vlrppt:posDaDft+1:
081900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082000030822     C                   ENDIF
082100040714     C* VATTRC
082200040714     C                   IF        %subst(
082300040714     C                             %subst(vlrppt:posDaDft+1:
082400040714     C                             posADft-posDaDft-1):1:3)
082500040714     C                             = 'TRC'
082600040714     C                   EVAL      VATTRC=%trim(%subst(
082700040714     C                             %subst(vlrppt:posDaDft+1:
082800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
082900040714     C                   ENDIF
083000030822     C* ...
083100030822     C                   ENDIF
083200030822     C                   ENDDO
083300020204     C*
083400010330     C                   ENDSR
083500010607     C*----------------------------------------------------*
083600110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083700010607     C*----------------------------------------------------*
083800010607     C     IMPVAB        BEGSR
083900010607     C*
084000010607     C                   SETOFF                                       3132
084100110315 xxx C***                SETON                                        35
084200030515     C*
084300010607     C                   MOVEL     datcor        VABAAS
084400010607     C                   MOVE      datcor        VABMGS
084500010607     C*
084600030715     C**********
084700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084800030715     C**********
084900030715     C                   IF        wGiro = *zeros
085000040113     C*
085100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
085200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
085300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085400080429     C                             %trim(intcol(5))<>*blanks
085500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085700080429     C                                      %trim(intcol(5))
085800070823     C     ';':CharCSV   XLATE     vindta        vindta
085900040510     C                   ENDIF
086000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
086100031201     C     minu:maiu     XLATE     vindta        vindta
086200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086300030715     C                   DOW       posDa <= %len(%trim(vindta))
086400030715     C*
086500030715     C* Gestisco il 1° campo
086600030715     C                   IF        i = 1
086700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086800030715     C                   EVAL      posDa = 1
086900030715     C* Eseguo lo scan x trovare la fine del primo campo
087000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087100030715     C* A questo punto "estraggo" il campo corrente
087200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
087300030715     C                                                    (posA-posDa))
087400030715     C* X i campi successivi al 1°
087500030715     C                   ELSE
087600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087700030715     C                   EVAL      posDa = posA + 1
087800030715     C* Eseguo lo scan x trovare la fine del campo corrente
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* Gestisco l'ultimo campo
088100030715     C                   IF        posA = *zeros
088200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   LEAVE
088700030715     C                   ELSE
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   ENDIF
089200030715     C                   ENDIF
089300030715     C* Incremento il contatore d campo
089400030715     C                   EVAL      i = i +1
089500030715     C                   ENDDO
089600030715     C*
089700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089800030715     C* delimitazione testo
089900030715     C                   EVAL      i = 1
090000030715     C                   DOW       i < %elem(SkSplitFLD)
090100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
090200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
090300030715     C                   EVAL      i = i + 1
090400030715     C                   ENDDO
090500030715     C                   EVAL      wGiro = 1
090600030715     C                   ELSE
090700030715     C                   EVAL      wGiro = 2
090800031204     C**********
090900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
091000031204     C* d separatore campo
091100031204     C**********
091200031204     C                   EVAL      vindta = %trim(vindta)
091300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091400031204     C                   EVAL      vindta = %subst(vindta:2)
091500031204     C                   ENDDO
091600031204     C*
091700031204     C                   Z-ADD     *zeros        lunghInput        4 0
091800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
092000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
092100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
092200110912     C                   ENDDO
092300030715     C**********
092400030715     C* Effettuo lo split del campo dati d input
092500030715     C**********
092600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092700030715     C                   DOW       posDa <= %len(%trim(vindta))
092800030715     C*
092900030715     C* Gestisco il 1° campo
093000030715     C                   IF        i = 1
093100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
093200030715     C                   EVAL      posDa = 1
093300030715     C* Eseguo lo scan x trovare la fine del primo campo
093400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093500030715     C* A questo punto "estraggo" il campo corrente
093600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093700030715     C                                                    (posA-posDa))
093800030715     C* X i campi successivi al 1°
093900030715     C                   ELSE
094000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
094100030715     C                   EVAL      posDa = posA + 1
094200031204     C* Verifico che nn vi sia il campo nn valorizzato
094300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094400031204     C* Se campo nn valorizzato skippo al prossimo
094500031204     C                   EVAL      SkSplitCSV(i) = *blanks
094600031204     C                   EVAL      posA  = posA + 1
094700031204     C                   ELSE
094800030715     C* Eseguo lo scan x trovare la fine del campo corrente
094900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
095000030715     C* Gestisco l'ultimo campo
095100030715     C                   IF        posA = *zeros
095200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
095300030715     C* A questo punto "estraggo" il campo corrente
095400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095500030715     C                                                    (posA-posDa))
095600030715     C                   LEAVE
095700030715     C                   ELSE
095800030715     C* A questo punto "estraggo" il campo corrente
095900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
096000030715     C                                                    (posA-posDa))
096100030715     C                   ENDIF
096200031204     C                   ENDIF
096300030715     C                   ENDIF
096400030715     C* Incremento il contatore d campo
096500030715     C                   EVAL      i = i +1
096600030715     C                   ENDDO
096700030715     C*
096800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096900030715     C* delimitazione testo
097000030715     C                   EVAL      i = 1
097100030715     C                   DOW       i < %elem(SkSplitCSV)
097200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
097300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097400030715     C                   EVAL      i = i + 1
097500030715     C                   ENDDO
097600100720     C*
097700100720     C* Stacco un progressivo x numero spedizione
097800110930 xxx C                   EXSR      repNSP
097900100326     C*
098000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
098100030715     C                   EVAL      i = 1
098200030715     C                   DOW       i < %elem(SkSplitFLD)
098300030715     C*
098400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098600030715     C                             SkSplitCSV(i) <> *blanks
098700030721     C*** ATB
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098900030721     C                   EVAL      VABATB = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030715     C*** RSD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
099300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099600030715     C* ==
099700030715     C                   ENDIF
099800030721     C*** RD2
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
100000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030715     C*** IND
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100400030715     C                   EVAL      VABIND = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** LOD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** PRD
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
101200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030721     C*** NZD
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101700160429     C     minu:maiu     XLATE     VABNZD        VABNZD
101800160429     C                   IF        VABNZD = 'IT'  OR
101900160429     C                             VABNZD = 'ITA'
102000160429     C                   EVAL      VABNZD = *blanks
102100160429     C                   ENDIF
102200030721     C                   ENDIF
102300030715     C*** RMA
102400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102600030715     C                   ENDIF
102700030715     C*** NOT
102800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
103000030715     C                   ENDIF
103100030715     C*** NT2
103200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
103300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
103400030715     C                   ENDIF
103500030715     C*** VCA
103600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103800030715     C                   ENDIF
103900030715     C*** VAS
104000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
104100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
104200030715     C                   ENDIF
104300030715     C*** FFD
104400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104600030715     C                   ENDIF
104700030721     C*** GC1
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GC2
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
105300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TSP
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105800030721     C                   ENDIF
105900040510     C*** CBO
106000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
106100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
106200040510     C                   ENDIF
106300030721     C*** NAS
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TIC
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** GCA
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
107300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** XCO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** CTM
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
108100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** TCR
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*** CTS
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
109000030721     C                   ENDIF
109100030721     C*** FTM
109200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
109300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
109400030721     C                   ENDIF
109500030721     C*** VAD
109600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109800030721     C                   ENDIF
109900030721     C*** GMA
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
110100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
110200030721     C                   ENDIF
110300030721     C*** GGA
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110600030721     C                   ENDIF
110700030721     C*** GVA
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
111000030721     C                   ENDIF
111100030721     C*** TC1
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
111300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
111400030721     C                   ENDIF
111500030721     C*** TC2
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111800030721     C                   ENDIF
111900030721     C*** SCL
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
112100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
112200030721     C                   ENDIF
112300030721     C*** RMO
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112600030721     C                   ENDIF
112700030721     C*** NMO
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
113000030721     C                   ENDIF
113100030721     C*
113200030721     C* Reperisco quindi i campi numerici...
113300040510     C*** NSP
113400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113500040510     C                   EVAL      PiStr=SkSplitCSV(i)
113600040510     C                   EXSR      CHKNUM
113700040510     C                   IF        PiInt=*on
113800040510     C                   Z-ADD     PiVal         VABNSP
113900040510     C                   ELSE
114000040510     C                   SETON                                        31
114100040510     C                   EVAL      VABNSP = *zeros
114200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
114300040510     C                             + ' ' + 'VABNSP'
114400040510     C                   ENDIF
114500040510     C                   ENDIF
114600071106     C*** RMN
114700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114800071106     C                   EVAL      PiStr=SkSplitCSV(i)
114900071106     C                   EXSR      CHKNUM
115000071106     C                   IF        PiInt=*on
115100071106     C                   Z-ADD     PiVal         VABRMN
115200071106     C                   ELSE
115300071106     C                   SETON                                        32
115400071106     C                   EVAL      VABRMN = *zeros
115500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115600071106     C                             + ' ' + 'VABRMN'
115700071106     C                   ENDIF
115800071106     C                   ENDIF
115900030721     C*** CAD
116000160421     C***                IF        %trim(SkSplitFLD(i)) = 'VABCAD'
116100160421     C***                EVAL      PiStr=SkSplitCSV(i)
116200160421     C***                EXSR      CHKNUM
116300160421     C***                IF        PiInt=*on
116400160421     C***                Z-ADD     PiVal         Num5_0
116500160421     C***                MOVEL(p)  Num5_0        VABCAD
116600160421     C***                ELSE
116700160421     C***                SETON                                        32
116800160421     C***                EVAL      VABCAD = *zeros
116900160421     C***                EVAL      vinmsg = %trimr(vinmsg)
117000160421     C***                          + ' ' + 'VABCAD'
117100160421     C***                ENDIF
117200160421     C***                ENDIF
117300160421     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
117400160421     C                   EVAL      VABCAD = SkSplitCSV(i)
117500160421     C                   ENDIF
117600030721     C*** NCL
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on
118100030721     C                   Z-ADD     PiVal         VABNCL
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABNCL = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040122     C                             + ' ' + 'VABNCL'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** PKB
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiNum=*on
119400030721     C                   Z-ADD     PiVal         VABPKB
119500030721     C                   ELSE
119600030721     C                   SETON                                        32
119700030721     C                   EVAL      VABPKB = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABPKB'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** CAS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
120400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
120500111124     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
120600140915     C                             %trim(SkSplitCSV(i)) <> '.000'  AND
120700161018     C                             %trim(SkSplitCSV(i)) <> ',000'  AND
120800161018     C                             %trim(SkSplitCSV(i)) <> '0.00'
120900101018     C                   EVAL      wFlgCAS = '1'
121000161018     C                   EVAL      VABVCA = 'EUR'
121100101018     C                   ENDIF
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiNum=*on
121500030721     C                   Z-ADD     PiVal         VABCAS
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      VABCAS = *zeros
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABCAS'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** CCM
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiInt=*on      AND
122800030721     C                             PiVal<=9999999 AND
122900030721     C                             PiVal>*zeros
123000030721     C                   Z-ADD     PiVal         VABCCM
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      VABCCM = *zeros
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500040510     C                             + ' ' + 'VABCCM'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** LNP
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiInt=*on    AND
124300030721     C                             PiVal<=999   AND
124400030721     C                             PiVal>*zeros
124500030721     C                   Z-ADD     PiVal         VABLNP
124600030721     C                   ELSE
124700040715     C                   SETON                                        32
124800030721     C                   EVAL      VABLNP = *zeros
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000040510     C                             + ' ' + 'VABLNP'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NRS
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on AND
125800030721     C                             PiVal<=99
125900030721     C                   Z-ADD     PiVal         VABNRS
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNRS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** LNA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on    AND
127100030721     C                             PiVal<=999   AND
127200030721     C                             PiVal>*zeros
127300030721     C                   Z-ADD     PiVal         VABLNA
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      VABLNA = *zeros
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABLNA'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** CTR
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on  AND
128600030721     C                             PiVal<=999
128700030721     C                   Z-ADD     PiVal         VABCTR
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABCTR'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** IAS
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiNum=*on
129900030721     C                   Z-ADD     PiVal         VABIAS
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABIAS'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** VLB
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiNum=*on
131100030721     C                   Z-ADD     PiVal         VABVLB
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABVLB'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** QFT
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABQFT
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABQFT'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** NCD
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on      AND
133500030721     C                             PiVal<=9999999
133600030721     C                   Z-ADD     PiVal         VABNCD
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABNCD'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** NCA
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on      AND
134800030721     C                             PiVal<=9999999
134900030721     C                   Z-ADD     PiVal         VABNCA
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABNCA'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** ZNC
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on AND
136100030721     C                             PiVal<=99
136200030721     C                   Z-ADD     PiVal         VABZNC
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABZNC'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** DCR
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on       AND
137400030721     C                             PiVal<=99999999
137500030721     C                   Z-ADD     PiVal         VABDCR
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137900030721     C                             + ' ' + 'VABDCR'
138000030721     C                   ENDIF
138100030721     C                   ENDIF
138200030721     C*** HCR
138300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
138400030721     C                   EVAL      PiStr=SkSplitCSV(i)
138500030721     C                   EXSR      CHKNUM
138600030721     C                   IF        PiInt=*on   AND
138700030721     C                             PiVal<=9999
138800030721     C                   Z-ADD     PiVal         VABHCR
138900030721     C                   ELSE
139000030721     C                   SETON                                        32
139100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139200030721     C                             + ' ' + 'VABHCR'
139300030721     C                   ENDIF
139400030721     C                   ENDIF
139500030721     C*** VMD
139600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
139700030721     C                   EVAL      PiStr=SkSplitCSV(i)
139800030721     C                   EXSR      CHKNUM
139900030721     C                   IF        PiNum=*on
140000030721     C                   Z-ADD     PiVal         VABVMD
140100030721     C                   ELSE
140200030721     C                   SETON                                        32
140300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140400030721     C                             + ' ' + 'VABVMD'
140500030721     C                   ENDIF
140600030721     C                   ENDIF
140700030721     C*** ANT
140800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
140900030721     C                   EVAL      PiStr=SkSplitCSV(i)
141000030721     C                   EXSR      CHKNUM
141100030721     C                   IF        PiInt=*on        AND
141200030721     C                             PiVal<=999999999
141300030721     C                   Z-ADD     PiVal         VABANT
141400030721     C                   ELSE
141500030721     C                   SETON                                        32
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABANT'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000030721     C*** CMO
142100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
142200030721     C                   EVAL      PiStr=SkSplitCSV(i)
142300030721     C                   EXSR      CHKNUM
142400030721     C                   IF        PiInt=*on
142500030721     C                   Z-ADD     PiVal         Num5_0
142600030721     C                   MOVEL(p)  Num5_0        VABCMO
142700030721     C                   ELSE
142800030721     C                   SETON                                        32
142900030721     C                   EVAL      VABCMO = *zeros
143000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
143100030721     C                             + ' ' + 'VABCMO'
143200030721     C                   ENDIF
143300030721     C                   ENDIF
143400040510     C*
143500110930     C* Reperisco i campi del EDIVAT
143600040510     C*** VATNOT
143700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
143800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
143900040510     C                   ENDIF
144000040510     C*** VATTRC
144100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
144200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
144300040510     C                   ENDIF
144400060331     C*** VATNOT_A
144500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
144600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
144700060331     C                   ENDIF
144800060331     C*** VATNOT_B
144900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
145000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
145100060331     C                   ENDIF
145200060331     C*** VATNOT_E
145300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
145400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
145500060331     C                   ENDIF
145600110927     C*** VATNOT_I
145700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
145800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
145900110927     C                   ENDIF
146000111124     C*** VATNOT_J
146100111124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
146200111124     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
146300111124     C                   ENDIF
146400120405     C*** VATNOT_4
146500120405     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
146600120405     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
146700120405     C                   ENDIF
146800131018     C*** VATNOT_S
146900131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
147000131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
147100131018     C                   ENDIF
147200141128     C*** VATNOT_Y
147300141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
147400141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
147500141128     C                   ENDIF
147600070823     C*
147700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
147800160427     C*
147900160427     C*** W_VATNOT_E
148000160427     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
148100160427     C                   IF        %len(%trim(SkSplitCSV(i))) = 20
148200160427     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
148300160427     C                   EVAL      VABCTM = '7Q'
148400160427     C                   ENDIF
148500160427     C                   ENDIF
148600160427     C*
148700160427     C*** W_DISK_B
148800160427     C                   IF        %trim(SkSplitFLD(i)) = 'W_DISK_B'
148900160427     C                   SETOFF                                       74
149000160427     C                   IF        %len(%trim(SkSplitCSV(i))) = 18
149100160427     C                   MOVEL     *blanks       wSEGNAC          18
149200160427     C                   EVAL      wSEGNAC = %trim(SkSplitCSV(i))
149300160427     C*
149400160427     C* Controllo numericità valore
149500160427     C                   EVAL      PiStr=wSEGNAC
149600160427     C                   EXSR      CHKNUM
149700160427     C                   IF        PiInt=*on AND PiVal>*zeros
149800160427     C*
149900160427     C* Controllo trattasi di barcode BRT
150000160427     C                   CLEAR                   TRUL28DS
150100160427     C                   EVAL      I28MOD = 'BAR'
150200160427     C                   EVAL      I28COD = %subst(wSEGNAC:1:17)
150300160427     C                   CALL      'TRUL28R1'
150400160427     C                   PARM                    TRUL28DS
150500160427     C                   IF        O28ERR = *blanks AND
150600160427     C                             O28CKD = %subst(wSEGNAC:18:1)
150700160427     C                   SETON                                        74
150800160427     C                   EVAL      VABLNP = %dec(%subst(wSEGNAC:1:3):3:0)
150900160427     C                   EVAL      VABLNA = %dec(%subst(wSEGNAC:4:3):3:0)
151000160427     C                   EVAL      VABNRS = %dec(%subst(wSEGNAC:7:2):2:0)
151100160427     C                   EVAL      VABNCD = %dec(%subst(wSEGNAC:9:7):7:0)
151200160427     C                   EVAL      VABZNC = %dec(%subst(wSEGNAC:16:2):2:0)
151300160427     C                   ENDIF
151400160427     C*
151500160427     C                   ENDIF
151600160427     C*
151700160427     C                   ENDIF
151800160427     C*
151900160427     C* Se Disk B "errato" considero come Disk C
152000160427     C                   IF        not *in74
152100160427     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
152200160427     C                   EVAL      VABCTM = '7Q'
152300160429     C                   ELSE
152400160429     C                   EVAL      wVATNOT_E = *blanks
152500160429     C                   EVAL      VABCTM = '2'
152600160427     C                   ENDIF
152700160427     C*
152800160427     C                   ENDIF
152900110421     C*
153000160421     C*** W_VABRSD_1
153100160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_1'
153200160421     C                   EVAL      VABRSD = %trim(SkSplitCSV(i))
153300160421     C                   ENDIF
153400160421     C*
153500160421     C*** W_VABRSD_2
153600160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_2'
153700160421     C                   EVAL      VABRSD = %trim(VABRSD) + ' ' +
153800160421     C                                      %trim(SkSplitCSV(i))
153900160421     C                   EVAL      VABRSD = %trim(VABRSD)
154000160421     C                   ENDIF
154100160421     C*
154200160421     C*** W_VATNOT_A
154300160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_A'
154400160421     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
154500160503     C                   IF        wVATNOT_A = *blanks OR
154600160503     C                             wVATNOT_A = wVATNOT_B
154700160421     C                   EVAL      wVATNOT_A = VABRSD
154800160421     C                   ENDIF
154900160421     C                   ENDIF
155000160421     C*
155100160421     C*** W_VATNOT_IJ
155200160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_IJ'
155300160421     C                   MOVEL     *blanks       wEMAIL           70
155400160421     C                   EVAL      wEMAIL = %trim(SkSplitCSV(i))
155500160421     C                   EVAL      wVATNOT_I = %subst(wEMAIL:1:35)
155600160421     C                   EVAL      wVATNOT_J = %subst(wEMAIL:36:35)
155700160421     C                   ENDIF
155800160421     C*
155900160421     C*** W_VABNOT
156000160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
156100160421     C                   MOVEL     *blanks       wNOTE            70
156200160421     C                   EVAL      wNOTE  = %trim(SkSplitCSV(i))
156300160421     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
156400160421     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
156500160421     C                   ENDIF
156600160421     C*
156700160421     C*** W_VABNAS
156800160421     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNAS'
156900160421     C                   IF        %trim(SkSplitCSV(i)) = 'true'
157000160421     C                   EVAL      VABNAS = 'MERCE PERICOL.'
157100160421     C                   ENDIF
157200160421     C                   ENDIF
157300070925     C*
157400071029     C***  <===  -----------------------------------------
157500040510     C*
157600030715     C                   ENDIF
157700030715     C                   EVAL      i = i + 1
157800030715     C                   ENDDO
157900070925     C*
158000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
158100100831     C*
158200070925     C*
158300071029     C***  <===  -----------------------------------------
158400070925     C*
158500070925     C* Considerazioni finali su CBO/CAS
158600100318     C                   IF        wFlgCAS = '1'    OR
158700100318     C                             VABCAS  > *zeros
158800070925     C                   IF        VABCBO = '1'
158900070925     C                   EVAL      VABCBO = '4'
159000070925     C                   ENDIF
159100070925     C                   IF        VABCBO = '2'
159200070925     C                   EVAL      VABCBO = '6'
159300070925     C                   ENDIF
159400070925     C                   ENDIF
159500070925     C***  <===  ----------------------------
159600040510     C*
159700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
159800040510     C                   IF        VABNSP = *zeros
159900040510     C                   SETON                                        31
160000040510     C                   ENDIF
160100040715     C*
160200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
160300040715     C                   IF        VABLNP = *zeros
160400040715     C                   SETON                                        31
160500040715     C                   ENDIF
160600040510     C*
160700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
160800040510     C                   IF        VABRMN = *zeros
160900040510     C                   Z-ADD     VABNSP        VABRMN
161000040510     C                   ENDIF
161100040510     C*
161200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
161300040510     C                   IF        VABRMA = *blanks
161400040714     C                   MOVEL     VABRMN        VABRMA
161500040510     C                   ENDIF
161600040128     C*
161700040510     C* Se provincia nn valorizzata la reperisco
161800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
161900110912     C                   IF        VABPRD =  *blanks AND
162000110912     C                             VABNZD =  *blanks AND
162100110912     C                             VABCAD <> *blanks AND
162200110912     C                             VABLOD <> *blanks
162300040128     C                   CLEAR                   TISI95DS
162400040128     C                   EVAL      I95TCN = '3'
162500040128     C                   Z-ADD     datcor        I95DAT
162600040128     C                   EVAL      I95NAR = VABNZD
162700040128     C                   EVAL      I95CAP = VABCAD
162800040128     C                   EVAL      I95LOC = VABLOD
162900040128     C                   CALL      'TISI95R'
163000040128     C                   PARM                    TISI95DS
163100040128     C                   EVAL      VABPRD = O95PRV
163200040510     C                   ENDIF
163300020204     C*
163400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
163500020204     C                   EXSR      CHKIMPDIV
163600030715     C                   ENDIF
163700020204     C*
163800010607     C                   ENDSR
163900010607     C*----------------------------------------------------*
164000010601
164100020204
164200020204     C*----------------------------------------------------*
164300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
164400020204     C*----------------------------------------------------*
164500020204     C     CHKIMPDIV     BEGSR
164600020204     C*
164700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
164800020204     C                   Z-ADD     *zeros        wrkDec            9 9
164900020204     C*
165000020204     C* Come prima cosa effettuo considerazioni sulla divisa
165100020204     C                   IF        vabIAS > *zeros
165200020204     C                   IF        vabVAS <> 'EUR'
165300020204     C                   EVAL      vabVAS =  'ITL'
165400020204     C                   ENDIF
165500020204     C                   ENDIF
165600020204     C*
165700020204     C                   IF        vabCAS > *zeros
165800020204     C                   IF        vabVCA <> 'EUR'
165900020204     C                   EVAL      vabVCA =  'ITL'
166000020204     C                   ENDIF
166100020204     C                   ENDIF
166200020204     C*
166300020204     C                   IF        vabVMD > *zeros
166400020204     C                   IF        vabVAD <> 'EUR'
166500020204     C                   EVAL      vabVAD =  'ITL'
166600020204     C                   ENDIF
166700020204     C                   ENDIF
166800020204     C*
166900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
167000020204     C                   Z-ADD     vabIAS        wrkDec
167100020204     C                   IF        wrkDec > *zeros
167200020204     C                   IF        vabVAS = 'ITL'
167300020204     C                   EVAL      vabIAS = *zeros
167400020204     C                   ENDIF
167500020204     C                   ENDIF
167600020204     C*
167700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
167800020204     C                   Z-ADD     vabCAS        wrkDec
167900020204     C                   IF        wrkDec > *zeros
168000020204     C                   IF        vabVCA = 'ITL'
168100020204     C                   EVAL      vabCAS = *zeros
168200020204     C                   ENDIF
168300020204     C                   ENDIF
168400020204     C*
168500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
168600020204     C                   Z-ADD     vabVMD        wrkDec
168700020204     C                   IF        wrkDec > *zeros
168800020204     C                   IF        vabVAD = 'ITL'
168900020204     C                   EVAL      vabVMD = *zeros
169000020204     C                   ENDIF
169100020204     C                   ENDIF
169200020204     C*
169300020204     C                   ENDSR
169400020204     C***
169500010330
169600100318
169700100318
169800100318     C*----------------------------------------------------*
169900100318     C*  CONTROLLO NUMERICITA' CAMPI
170000100318     C*----------------------------------------------------*
170100100318     C     CHKNUM        BEGSR
170200100318     C*
170300100318     C                   IF        PiDecChr = *blanks
170400100318     C                   EVAL      PiDecChr = CharNUM
170500100318     C                   ENDIF
170600100318     C*
170700100318     C                   callp     UBISNUM_Check(PiStr
170800100318     C                                          :PiDecChr
170900100318     C                                          :PiVal
171000100318     C                                          :PiNum
171100100318     C                                          :PiInt)
171200100318     C*
171300100318     C                   ENDSR
171400100318     C***
171500010601
171600010601
171700010601
171800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
171900010601     C     repfil        BEGSR
172000010601     C*
172100010601     C                   if        invfil = *zeros and
172200010601     C                             depfil > *zeros and
172300010629     C                             (vinflg = *blanks or
172400010629     C                              vinflg = *zeros)
172500010601     C
172600010601     C                   eval      invfil = depfil
172700010601     C                   endif
172800010601     C*
172900010601     C                   if        depfil <> invfil and
173000010601     C                             invfil > *zeros
173100010601     C                   eval      flgMulti = '1'
173200010601     C                   if        vinflg = *blanks
173300010601     C                   add       1             cntNonEl
173400010601     C                   endif
173500010601     C                   endif
173600010601     C*
173700010601     C                   if        vinflg = '2'
173800010601     C                   eval      flgStato = '2'
173900010601     C                   endif
174000010601     C*
174100010601     C                   ENDSR
174200010601     C***
174300010601
174400010601
174500010601
174600010330
174700040119      /TITLE Invio dei dati al punto operativo.
174800040119     C     invio         BEGSR
174900040119     C*
175000110930     C* 1° invio EDIVAT
175100040119     C                   reset                   dscmz
175200040119     C                   move      vlrpoi        cmzdst
175300110930     C                   eval      cmzfld = 'EDIVATWR'
175400040119     C                   eval      cmzmbd = vlrhdl
175500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
175600040119     C***                if        prmfir = *blanks
175700110930     C                   eval      cmzfla = 'EDIVAT0F'
175800110930     C                   eval      cmzmba = 'EDIVAT0F'
175900040119     C***                else
176000040119     C***                eval      cmzfla = prmfir
176100040119     C***                eval      cmzmba = prmfir
176200040119     C***                endif
176300040119     C                   eval      cmznrr = *zeros
176400040119     C                   move      §ctrokvt      cmznrr
176500040119     C                   eval      cmzlba = vlrfl1
176600040119     C                   call(e)   'TIS711C'
176700040119     C                   parm                    dscmz
176800040119     C                   parm      *blanks       esito
176900040119     C                   if        %error
177000040119     C                             or cmzerr = '1'
177100040119     C                             or esito  = '1'
177200040119     C                   eval      wrkesito = '3'
177300040119     C                   else
177400040119     C*
177500110930     C* 2° invio EDIVAB
177600040119     C                   reset                   dscmz
177700040119     C                   move      vlrpoi        cmzdst
177800040119     C                   eval      cmzfld = vlrfou
177900040119     C                   eval      cmzmbd = vlrhdl
178000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
178100040119     C***                if        prmfir = *blanks
178200110930     C                   eval      cmzfla = 'EDIVAB0F'
178300110930     C                   eval      cmzmba = 'EDIVAB0F'
178400040119     C***                else
178500040119     C***                eval      cmzfla = prmfir
178600040119     C***                eval      cmzmba = prmfir
178700040119     C***                endif
178800040119     C                   eval      cmznrr = *zeros
178900040119     C                   move      §ctrokvb      cmznrr
179000040119     C                   eval      cmzlba = vlrfl1
179100040119     C                   call(e)   'TIS711C'
179200040119     C                   parm                    dscmz
179300040119     C                   parm      *blanks       esito
179400040119     C                   if        %error
179500040119     C                             or cmzerr = '1'
179600040119     C                             or esito  = '1'
179700040119     C                   eval      wrkesito = '3'
179800040119     C                   endif
179900040119     C                   endif
180000040119     C*
180100040119     C                   ENDSR
180200040119     C***
180300010601
180400010601
180500010601
180600010601
180700010601      /TITLE Invio dei dati al punto operativo.
180800010601     C     opeini        BEGSR
180900010601     C*
181000010601     C* Inizializzo flag e contatori operativi
181100010601     C                   movel     '0'           flgGiro           1
181200010601     C                   movel     '0'           flgMulti          1
181300010601     C                   movel     '1'           flgStato          1
181400010615     C                   movel     '0'           flgOk             1
181500010601     C                   z-add     *zeros        cntNonEl         10 0
181600010601     C                   z-add     *zeros        depfil            3 0
181700010601     C                   z-add     *zeros        invfil            3 0
181800010601     C*
181900010601     C                   ENDSR
182000010601     C***
182100070326
182200070326
182300070326
182400070326
182500070326     C     *pssr         BEGSR
182600070329     C*
182700070329     C                   if        %open(tivin00r)
182800070329     C                   close     tivin00r
182900070329     C                   endif
183000110930     C                   if        %open(edivabwr)
183100110930     C                   close     edivabwr
183200070329     C                   endif
183300110930     C                   if        %open(edivatwr)
183400110930     C                   close     edivatwr
183500070329     C                   endif
183600070326     C*
183700070326     C* Effettuo la chiamata al CLLE preposto
183800110930     C                   call(e)   'TITVEVTC'
183900070326     C                   parm                    parccm
184000070326     C                   parm                    parmbr
184100070326     C                   parm      '2'           paropz
184200070326     C*
184300070326     C                   eval      wrkesito = '2'
184400070404     C*
184500070404     C                   seton                                        LR
184600070326     C*
184700070326     C                   ENDSR     '*CANCL'
184800070326     C***
184900100318
185000100318
185100100318
185200100318     C     normCSV       BEGSR
185300100318     C*
185400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
185500100318     C* separatori campi
185600100318     C                   if        CharTXT <> *blanks
185700100318     C                   z-add     *zeros        wLenVINDTA        4 0
185800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
185900100318     C                   setoff                                       80
186000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
186100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
186200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
186300100318     C                   if        not *in80
186400100318     C                   seton                                        80
186500100318     C                   else
186600100318     C                   setoff                                       80
186700100318     C                   endif
186800100318     C                   endif
186900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
187000100318     C                             and *in80 = *on
187100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
187200100318     C                   endif
187300100318     C                   enddo
187400100318     C                   endif
187500100318     C*
187600100318     C                   ENDSR
187700100318     C***
187800070326
187900100326
188000100326
188100100326     C*----------------------------------------------------*
188200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
188300100326     C*----------------------------------------------------*
188400100326     C     REPNSP        BEGSR
188500100326     C*
188600100326     C* NSP => Stacco un numeratore da AZNUM
188700100326     C                   clear                   TRUL33DS
188800100326     C                   eval      I33OPE = *zeros
188900100326     C                   eval      I33CNU = 302
189000100326     C                   eval      I33NUM = 1
189100100326     C                   movel     TRUL33DS      KPJBU
189200100326     C                   call      'TRUL33R'
189300100326     C                   parm                    KPJBA
189400100326     C                   movel     KPJBU         TRUL33DS
189500100326     C                   if        O33ERR = *zeros
189600100326     C                   z-add     O33NRF        VABNSP
189700100326     C                   z-add     O33NRF        VATNSP
189800100326     C                   else
189900100326     C                   SETON                                        31
190000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
190100100326     C                             + ' ' + 'VABNSP VATNSP'
190200100326     C                   endif
190300100326     C*
190400100326     C                   ENDSR
190500010330
190600100326
190700010330
190800000613     C     *inzsr        BEGSR
190900990910     C*
191000990910     C     *entry        plist
191100990920     C                   parm                    tivlrds
191200990921     C                   parm      wrkesito      esito
191300000724     C                   parm                    prmlit
191400000710     C                   parm                    prmfir
191500010330     C*
191600010330     C* CALCOLA LA DATA CORRENTE
191700110930     C                   time                    wn14             14 0
191800110930     C                   movel     wn14          oracor            6 0          *ORA
191900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
192000100318     C                   eval      datcor = %dec(%date() : *ISO)
192100000613     C*
192200000613     C                   ENDSR
192300000613     C***
192400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
192500160427VABRMN;W_VATNOT_E;X;X;X;X;W_VABRSD_1;W_VABRSD_2;VABIND;VABRD2;VABLOD;VABCAD;
192600161018X;VABNZD;VATNOT_B;W_VATNOT_A;W_VATNOT_IJ;W_VABNOT;X;X;X;X;VABCAS;X;X;X;X;
192700161018X;X;X;X;W_DISK_B;X;VABTSP;X;VABRMA;X;X;X;W_VABNAS;X;X;X;
192800110315
192900100318
