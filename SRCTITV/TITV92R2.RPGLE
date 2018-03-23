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
004200100326
004300030715     D*------------------
004400030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004500030715     D*------------------
004600030715     D SkSplitFLD      S             10    DIM(100)
004700030715     D SkSplitCSV      S            256    DIM(100)
004800030715     D CharCSV         S              1
004900030715     D CharTXT         S              1
005000030715     D CharNUM         S              1
005100100318     D CharSOS         S              1
005200030715     D posDa           S              3  0 INZ(*zeros)
005300030715     D posA            S              3  0 INZ(*zeros)
005400030715     D i               s              3  0 INZ(1)
005500030715     D wGiro           s              1  0 INZ(*zeros)
005600070823     D wVATNOT_A       s                   LIKE(VATNOT)
005700060331     D wVATNOT_B       s                   LIKE(VATNOT)
005800060331     D wVATNOT_E       s                   LIKE(VATNOT)
005900110927     D wVATNOT_I       s                   LIKE(VATNOT)
006000111124     D wVATNOT_J       s                   LIKE(VATNOT)
006100120405     D wVATNOT_4       s                   LIKE(VATNOT)
006200131018     D wVATNOT_S       s                   LIKE(VATNOT)
006300141128     D wVATNOT_Y       s                   LIKE(VATNOT)
006400030822     D*------------------
006500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006600030822     D*------------------
006700030822     D posDaDft        S              3  0 INZ(*zeros)
006800030822     D posADft         S              3  0 INZ(*zeros)
006900030822     D j               s              3  0 INZ(1)
007000030822     D wGiroDft        s              1  0 INZ(*zeros)
007100080418     D*------------------
007200080418     D* Costanti
007300080418     D*------------------
007400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007600100318
007700100318     D*------------------
007800100318     D* LINKING A DEFINIZIONI ESTERNE
007900100318     D*------------------
008000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008200010330
008300010330
008400010330
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700990921     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700010601     C*
010800010601     C                   endif
010900010601     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010601     C*--------------------------------------------------------
011900010601     C     ENDELA        BEGSR
012000040119     C*
012100040119     C* Effettuo la chiamata al CLLE preposto
012200110930     C                   call(e)   'TITVEVTC'
012300040510     C                   parm                    parccm
012400040119     C                   parm                    parmbr
012500040119     C                   parm      '2'           paropz
012600050201     C*
012700050201     C* Effettuo lancio TISI95 solo x chiusura
012800050201     C                   CLEAR                   TISI95DS
012900050201     C                   EVAL      I95TLA = 'C'
013000050201     C                   CALL      'TISI95R'
013100050201     C                   PARM                    TISI95DS
013200000616     C*
013300010601     C                   ENDSR
013400010601     C***
013500000613
013600010601
013700010601
013800010330     C*--------------------------------------------------------
013900110930     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
014000010330     C*--------------------------------------------------------
014100010605     C     RWVAB         BEGSR
014200010330     C*
014300010330     C                   if        not %open(tivin00r)
014400010330     C                   open      tivin00r
014500010330     C                   endif
014600110930     C                   if        not %open(edivabwr)
014700110930     C                   open      edivabwr
014800010330     C                   endif
014900040119     C*
015000110930     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015100040119     C                   exsr      prevat
015200040119     C*
015300040119     C                   if        chkcall = '0'
015400040119     C*
015500110930     C                   if        not %open(edivatwr)
015600110930     C                   open      edivatwr
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300070823     C*
016400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016600110912     C***                exsr      inzvar
016700110912     C***                exsr      impvab
016800110131     C*
016900110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017000110912 xxx C***                read      tivin00r                               70
017100010330     C*
017200030822     C                   do        *HIVAL
017300010330     C*
017400030822     C                   read      tivin00r                               70
017500010618     C*
017600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017700010618     C                   if        vinflg = '1'
017800010618     C                   eval      flgOk = '1'
017900010618     C                   endif
018000040510     C*
018100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018200040510     C                   z-add     *zeros        wLenVINDTA        4 0
018300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018700040510     C                   z-add     1             wFlgVINDTA
018800040510     C                   leave
018900040510     C                   endif
019000040510     C                   enddo
019100010618     C*
019200040510     C                   if        vindta > *blanks AND
019300040510     C                             wFlgVINDTA = 1
019400010330     C                   add       1             rrnum
019500010330     C*
019600010601     C                   if        *in70 = *off and
019700010330     C                             (vinflg = *blanks
019800010330     C                              or vinflg = '0'
019900010330     C                              or vinflg = '2')
020000010330     C*
020100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020200010711     C                   if        vinflg = *blanks or vinflg = '0'
020300010711     C                   clear                   vinmsg
020400010711     C                   endif
020500010601     C*
020600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020700070925     C                   eval      vindta  = ' ' + %trim(vindta)
020800070925     C*
020900080527     C                   exsr      inzvar
021000100318     C                   exsr      normCSV
021100010605     C                   exsr      impvab
021200010601     C*
021300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400010605     C                   eval      depfil = VABLNP
021500010601     C                   exsr      repfil
021600010601     C                   if        depfil = invfil
021700021025     C                   if        vlrpoi = 999
021800030822     C                   move(P)   invfil        VABFGS
021900021025     C                   else
022000030822     C                   move(P)   vlrpoi        VABFGS
022100021025     C                   endif
022200010601     C*
022300010601     C                   exsr      PREELA
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700040119     C  N31              add       1             §CTROKVB
022800040119     C   32              add       1             §CTRMO
022900040119     C   31              add       1             §CTRNO
023000080418     C*
023100030822     C                   if        wGiro = 2
023200080513     C* Scrivo sempre il VAT
023300080513     C  N31              exsr      wrivat
023400110201     C                   exsr      chkvab
023500080513     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
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
025400040119     C*
025500080527     C* Verifico dati rimasti "in cannna"...
025600080527     C                   seton                                        65
025700110201     C                   exsr      chkvab
025800080527     C*
025900040119     C                   endif                                                  (endif - chkcall)
026000010601     C*
026100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026200010601     C                   if        cntNonEl = *zeros or
026300010601     C                             flgMulti = '0'
026400010330     C* Se non ci sono record con errori ...
026500010601     C                   if        §ctrno = 0 and
026600010604     C                             §ctrmo = 0 and
026700010601     C                             flgStato <> '2'
026800010330     C* ... restituisco esito OK.
026900010330     C                   eval      wrkesito = '0'
027000010330     C                   else
027100040119     C                   if        §ctrokvb > 0
027200010330     C                   eval      wrkesito = '1'
027300010330     C                   else
027400010615     C                   if        flgOk = '0'
027500010615     C                   eval      wrkesito = '2'
027600010615     C                   else
027700010615     C                   eval      wrkesito = '6'
027800010615     C                   endif
027900010330     C                   endif
028000010330     C                   endif
028100010601     C                   else
028200010601     C                   eval      wrkesito = '9'
028300010601     C                   endif
028400010330     C*
028500010330     C                   if        %open(tivin00r)
028600010330     C                   close     tivin00r
028700010330     C                   endif
028800110930     C                   if        %open(edivabwr)
028900110930     C                   close     edivabwr
029000010330     C                   endif
029100110930     C                   if        %open(edivatwr)
029200110930     C                   close     edivatwr
029300040512     C                   endif
029400010601     C*
029500010601     C                   if        vlrpoi <> 999
029600010601     C                   eval      invfil = vlrpoi
029700010601     C                   endif
029800010330     C*
029900040119     C                   if        §ctrokvb > 0
030000010601     C                             and invfil > *zeros
030100010330     C                   exsr      invio
030200010330     C                   endif
030300010601     C*
030400010618     C                   if        flgGiro = '1'
030500010601     C                   exsr      endela
030600010618     C                   endif
030700010330     C*
030800010330     C                   ENDSR
030900010330     C***
031000080527
031100080527
031200080527
031300080527     C*----------------------------------------------------*
031400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031500080527     C*----------------------------------------------------*
031600100413     C     CHKVAB        BEGSR
031700110201     C*
031800110201     C* Se richiesti ignorare colli / peso / volume
031900110315 xxx C***                if        *in35
032000110315 xxx C***                z-add     *zeros        VABNCL
032100110315 xxx C***                z-add     *zeros        VABPKB
032200110315 xxx C***                z-add     *zeros        VABVLB
032300110315 xxx C***                endif
032400080527     C*
032500080527     C* Scrivo il VAB solo a rottura d spedizione
032600080527     C                   if        depspe = *zeros
032700110930     C  N31              eval      edivabs_sav = edivabs
032800080527     C                   eval      depspe = vabNSP
032900080527     C                   else
033000080527     C                   if        depspe = vabNSP AND *in65 = *off
033100100413     C   51              add       VABNCL        sav_VABNCL
033200100413     C   51              add       VABPKB        sav_VABPKB
033300100413     C   51              add       VABVLB        sav_VABVLB
033400100720     C   52              z-add     VABNCL        sav_VABNCL
033500100720     C   52              z-add     VABPKB        sav_VABPKB
033600100720     C   52              z-add     VABVLB        sav_VABVLB
033700100720     C   53              add       VABNCL        sav_VABNCL
033800100720     C   53              z-add     VABPKB        sav_VABPKB
033900100720     C   53              z-add     VABVLB        sav_VABVLB
034000080527     C                   else
034100110930     C  N31              eval      edivabs_ok = edivabs_sav
034200080527     C  N31              exsr      wrivab
034300110930     C  N31              eval      edivabs_sav = edivabs
034400080527     C                   eval      depspe = vabNSP
034500080527     C                   endif
034600080527     C                   endif
034700080527     C*
034800080527     C                   ENDSR
034900040119
035000040119
035100040119
035200040119     C*----------------------------------------------------*
035300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035400040119     C*----------------------------------------------------*
035500040510     C     WRIVAB        BEGSR
035600080513     C*
035700080513     C* Gestisco l'eventuale rottura x numero spedizione
035800111124     C                   if        VABNSP <> depspe or *in65 = *on
035900070301     C                   if        wVATNOT_E <> *blanks
036000111124     C                   eval      ok_VABCTM = '7Q'
036100070301     C                   endif
036200110930     C*
036300110930     C* VALORIZZO CAMPI RELATIVI AL "CMR"
036400111124     C                   EVAL      ok_VABCMR = %char(datcor)+%char(oracor)
036500111124     C                   EVAL      ok_VABDCM = DATCOR
036600111124     C                   EVAL      ok_VABDTS = DATCOR
036700111124     C                   EVAL      ok_VABHMS = ORACOR
036800111124     C                   EVAL      ok_VABCNT = 1
036900110930     C*
037000110930     C                   write     edivab00
037100080513     C                   endif
037200040119     C*
037300040119     C                   ENDSR
037400040119
037500040119
037600040119
037700040119     C*----------------------------------------------------*
037800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037900040119     C*----------------------------------------------------*
038000040510     C     WRIVAT        BEGSR
038100060512     C*
038200060512     C* Inizializzo i campi chiave
038300060512     C                   eval      VATFGS = VABFGS
038400060512     C                   eval      VATAAS = VABAAS
038500060512     C                   eval      VATCCM = VABCCM
038600070823     C                   eval      VATNRS = VABNRS
038700060512     C                   eval      VATNSP = VABNSP
038800060512     C                   eval      VATLNP = VABLNP
038900040119     C*
039000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039100040510     C                   if        VATTRC = *blanks
039200040510     C                   eval      VATTRC = 'E'
039300040510     C                   endif
039400110318     C                   if        VATNOT <> *blanks
039500110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
039600110930     C                   eval      VATCNT = 1
039700110930     C                   write     EDIVAT00
039800040119     C                   add       1             §CTROKVT
039900110318     C                   endif
040000060331     C*
040100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040200060331     C                   if        wVATNOT_A <> *blanks
040300060331     C                   eval      VATNOT = wVATNOT_A
040400060331     C                   eval      VATTRC = 'A'
040500110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
040600110930     C                   eval      VATCNT = 1
040700110930     C                   write     EDIVAT00
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_B <> *blanks
041200060331     C                   eval      VATNOT = wVATNOT_B
041300060331     C                   eval      VATTRC = 'B'
041400110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
041500110930     C                   eval      VATCNT = 1
041600110930     C                   write     EDIVAT00
041700060331     C                   add       1             §CTROKVT
041800060331     C                   endif
041900060331     C*
042000060331     C                   if        wVATNOT_E <> *blanks
042100110201     C                   eval      VATNOT = wVATNOT_E
042200060331     C                   eval      VATTRC = 'E'
042300110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
042400110930     C                   eval      VATCNT = 1
042500110930     C                   write     EDIVAT00
042600110201     C                   add       1             §CTROKVT
042700110201     C                   endif
042800110927     C*
042900110927     C                   if        wVATNOT_I <> *blanks
043000110927     C                   eval      VATNOT = wVATNOT_I
043100110927     C                   eval      VATTRC = 'I'
043200110930     C                   eval      VATCMR = %char(datcor)+%char(oracor)
043300110930     C                   eval      VATCNT = 1
043400110930     C                   write     EDIVAT00
043500110927     C                   add       1             §CTROKVT
043600110927     C                   endif
043700111124     C*
043800111124     C                   if        wVATNOT_J <> *blanks
043900111124     C                   eval      VATNOT = wVATNOT_J
044000111124     C                   eval      VATTRC = 'J'
044100111124     C                   eval      VATCMR = %char(datcor)+%char(oracor)
044200111124     C                   eval      VATCNT = 1
044300111124     C                   write     EDIVAT00
044400111124     C                   add       1             §CTROKVT
044500111124     C                   endif
044600120405     C*
044700120405     C                   if        wVATNOT_4 <> *blanks
044800120405     C                   eval      VATNOT = wVATNOT_4
044900120405     C                   eval      VATTRC = '4'
045000120405     C                   eval      VATCMR = %char(datcor)+%char(oracor)
045100120405     C                   eval      VATCNT = 1
045200120405     C                   write     EDIVAT00
045300120405     C                   add       1             §CTROKVT
045400120405     C                   endif
045500131018     C*
045600131018     C                   if        wVATNOT_S <> *blanks
045700131018     C                   eval      VATNOT = wVATNOT_S
045800131018     C                   eval      VATTRC = 'S'
045900131018     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046000131018     C                   eval      VATCNT = 1
046100131018     C                   write     EDIVAT00
046200131018     C                   add       1             §CTROKVT
046300131018     C                   endif
046400141128     C*
046500141128     C                   if        wVATNOT_Y <> *blanks
046600141128     C                   eval      VATNOT = wVATNOT_Y
046700141128     C                   eval      VATTRC = 'Y'
046800141128     C                   eval      VATCMR = %char(datcor)+%char(oracor)
046900141128     C                   eval      VATCNT = 1
047000141128     C                   write     EDIVAT00
047100141128     C                   add       1             §CTROKVT
047200141128     C                   endif
047300040119     C*
047400040119     C                   ENDSR
047500040119
047600040119
047700040119
047800040119     C*----------------------------------------------------*
047900110930     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
048000040119     C*----------------------------------------------------*
048100040119     C     PREVAT        BEGSR
048200040119     C*
048300110930     C* Compongo il nome del membro da dare al EDIVATWR
048400040119     C                   eval      parmbr = vlrhdl
048500040119     C                   movel     'M'           parmbr
048600040510     C                   eval      parccm = vlrksc
048700040119     C                   eval      paropz = '1'
048800040119     C* Effettuo la chiamata al CLLE preposto
048900110930     C                   call(e)   'TITVEVTC'
049000040119     C                   parm                    parccm
049100040119     C                   parm                    parmbr
049200040119     C                   parm                    paropz
049300040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
049400040119     C                   if        %error
049500040119     C                   movel     '1'           chkcall
049600040119     C                   else
049700040119     C                   movel     '0'           chkcall
049800040119     C                   endif
049900040119     C*
050000040119     C                   ENDSR
050100010601
050200010601
050300010601
050400010330     C*----------------------------------------------------*
050500030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
050600010330     C*----------------------------------------------------*
050700040119     C     INZVAR        BEGSR
050800080527     C*
050900080527     C                   setoff                                       65
051000010330     C*
051100030822     C* Inizializzo variabili di wrk
051200020204     C                   Z-ADD     *zeros        Num5_0            5 0
051300101018     C                   MOVEL     *blanks       wFlgCAS
051400060331     C                   MOVEL     *blanks       wVATNOT_A
051500060331     C                   MOVEL     *blanks       wVATNOT_B
051600060331     C                   MOVEL     *blanks       wVATNOT_E
051700110927     C                   MOVEL     *blanks       wVATNOT_I
051800111124     C                   MOVEL     *blanks       wVATNOT_J
051900120405     C                   MOVEL     *blanks       wVATNOT_4
052000131018     C                   MOVEL     *blanks       wVATNOT_S
052100141128     C                   MOVEL     *blanks       wVATNOT_Y
052200030822     C*
052300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
052400030822     C                   CLEAR                   SkSplitCSV
052500030822     C                   EVAL      i = 1
052600030822     C                   EVAL      posDa = *zeros
052700030822     C                   EVAL      posA  = *zeros
052800080527     C*
052900110930     C                   CLEAR                   EDIVABS
053000110930     C                   CLEAR                   EDIVAT00
053100030822     C*
053200030822     C* Reimposto i valori di default
053300030822     C                   EXSR      DEFCAM
053400030822     C*
053500010330     C                   ENDSR
053600010330     C*----------------------------------------------------*
053700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
053800010330     C*----------------------------------------------------*
053900010330     C     DEFCAM        BEGSR
054000030822     C*
054100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
054200030715     C* e delimitatore testo.
054300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
054400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
054500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
054600100318     C*
054700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
054800100318     C                   EVAL      CharSOS = CharNUM
054900030822     C*
055000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
055100100720     C                   SETOFF                                       505152
055200100720     C                   SETOFF                                       53
055300100413     C                   SELECT
055400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
055500100720     C                   SETON                                        50
055600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
055700100413     C                   SETON                                        51
055800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
055900100720     C                   SETON                                        52
056000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
056100100720     C                   SETON                                        53
056200100413     C                   ENDSL
056300030822     C*
056400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
056500030822     C                   EVAL      posDaDft = 1
056600030822     C                   EVAL      posADft  = 0
056700030822     C                   EVAL      wGiroDft = 0
056800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
056900030822     C                             posDaDft > 0
057000030822     C*
057100030822     C* Gestisco il 1° giro
057200030822     C                   IF        wGiroDft = 0
057300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
057400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
057500030822     C* Incremento il contatore dei "giri"
057600030822     C                   EVAL      wGiroDft = 1
057700030822     C                   ELSE
057800030822     C                   EVAL      posDaDft = posADft
057900030822     C                   ENDIF
058000030822     C* Eseguo lo scan x trovare la fine del campo corrente
058100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
058200030822     C*
058300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
058400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
058500030822     C                   IF        posDaDft > 0 AND
058600030822     C                             posADft  > 0
058700030822     C* NCL
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'NCL'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABNCL
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* CCM
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'CCM'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiInt=*on
061000030822     C                   Z-ADD     PiVal         VABCCM
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* LNP
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'LNP'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiInt=*on
062300030822     C                   Z-ADD     PiVal         VABLNP
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* NRS
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'NRS'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiInt=*on
063600030822     C                   Z-ADD     PiVal         VABNRS
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* CTR
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CTR'
064400030822     C                   EVAL      PiStr=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   EXSR      CHKNUM
064800030822     C                   IF        PiInt=*on
064900030822     C                   Z-ADD     PiVal         VABCTR
065000030822     C                   ENDIF
065100030822     C                   ENDIF
065200030822     C* PKB
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'PKB'
065700030822     C                   EVAL      PiStr=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   EXSR      CHKNUM
066100030822     C                   IF        PiNum=*on
066200030822     C                   Z-ADD     PiVal         VABPKB
066300030822     C                   ENDIF
066400030822     C                   ENDIF
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
083200110930     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
083300010607     C*----------------------------------------------------*
083400010607     C     IMPVAB        BEGSR
083500010607     C*
083600010607     C                   SETOFF                                       3132
083700110315 xxx C***                SETON                                        35
083800030515     C*
083900010607     C                   MOVEL     datcor        VABAAS
084000010607     C                   MOVE      datcor        VABMGS
084100010607     C*
084200030715     C**********
084300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
084400030715     C**********
084500030715     C                   IF        wGiro = *zeros
084600040113     C*
084700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
084800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
084900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
085000080429     C                             %trim(intcol(5))<>*blanks
085100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
085200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
085300080429     C                                      %trim(intcol(5))
085400070823     C     ';':CharCSV   XLATE     vindta        vindta
085500040510     C                   ENDIF
085600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
085700031201     C     minu:maiu     XLATE     vindta        vindta
085800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085900030715     C                   DOW       posDa <= %len(%trim(vindta))
086000030715     C*
086100030715     C* Gestisco il 1° campo
086200030715     C                   IF        i = 1
086300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086400030715     C                   EVAL      posDa = 1
086500030715     C* Eseguo lo scan x trovare la fine del primo campo
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* A questo punto "estraggo" il campo corrente
086800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086900030715     C                                                    (posA-posDa))
087000030715     C* X i campi successivi al 1°
087100030715     C                   ELSE
087200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087300030715     C                   EVAL      posDa = posA + 1
087400030715     C* Eseguo lo scan x trovare la fine del campo corrente
087500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087600030715     C* Gestisco l'ultimo campo
087700030715     C                   IF        posA = *zeros
087800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C                   LEAVE
088300030715     C                   ELSE
088400030715     C* A questo punto "estraggo" il campo corrente
088500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
088600030715     C                                                    (posA-posDa))
088700030715     C                   ENDIF
088800030715     C                   ENDIF
088900030715     C* Incremento il contatore d campo
089000030715     C                   EVAL      i = i +1
089100030715     C                   ENDDO
089200030715     C*
089300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089400030715     C* delimitazione testo
089500030715     C                   EVAL      i = 1
089600030715     C                   DOW       i < %elem(SkSplitFLD)
089700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
089800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
089900030715     C                   EVAL      i = i + 1
090000030715     C                   ENDDO
090100030715     C                   EVAL      wGiro = 1
090200030715     C                   ELSE
090300030715     C                   EVAL      wGiro = 2
090400031204     C**********
090500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
090600031204     C* d separatore campo
090700031204     C**********
090800031204     C                   EVAL      vindta = %trim(vindta)
090900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
091000031204     C                   EVAL      vindta = %subst(vindta:2)
091100031204     C                   ENDDO
091200031204     C*
091300031204     C                   Z-ADD     *zeros        lunghInput        4 0
091400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
091600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
091700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
091800110912     C                   ENDDO
091900030715     C**********
092000030715     C* Effettuo lo split del campo dati d input
092100030715     C**********
092200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
092300030715     C                   DOW       posDa <= %len(%trim(vindta))
092400030715     C*
092500030715     C* Gestisco il 1° campo
092600030715     C                   IF        i = 1
092700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
092800030715     C                   EVAL      posDa = 1
092900030715     C* Eseguo lo scan x trovare la fine del primo campo
093000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C* X i campi successivi al 1°
093500030715     C                   ELSE
093600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
093700030715     C                   EVAL      posDa = posA + 1
093800031204     C* Verifico che nn vi sia il campo nn valorizzato
093900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
094000031204     C* Se campo nn valorizzato skippo al prossimo
094100031204     C                   EVAL      SkSplitCSV(i) = *blanks
094200031204     C                   EVAL      posA  = posA + 1
094300031204     C                   ELSE
094400030715     C* Eseguo lo scan x trovare la fine del campo corrente
094500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
094600030715     C* Gestisco l'ultimo campo
094700030715     C                   IF        posA = *zeros
094800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
094900030715     C* A questo punto "estraggo" il campo corrente
095000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095100030715     C                                                    (posA-posDa))
095200030715     C                   LEAVE
095300030715     C                   ELSE
095400030715     C* A questo punto "estraggo" il campo corrente
095500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
095600030715     C                                                    (posA-posDa))
095700030715     C                   ENDIF
095800031204     C                   ENDIF
095900030715     C                   ENDIF
096000030715     C* Incremento il contatore d campo
096100030715     C                   EVAL      i = i +1
096200030715     C                   ENDDO
096300030715     C*
096400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
096500030715     C* delimitazione testo
096600030715     C                   EVAL      i = 1
096700030715     C                   DOW       i < %elem(SkSplitCSV)
096800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
096900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
097000030715     C                   EVAL      i = i + 1
097100030715     C                   ENDDO
097200100720     C*
097300100720     C* Stacco un progressivo x numero spedizione
097400110930 xxx C                   EXSR      repNSP
097500100326     C*
097600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
097700030715     C                   EVAL      i = 1
097800030715     C                   DOW       i < %elem(SkSplitFLD)
097900030715     C*
098000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
098100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
098200030715     C                             SkSplitCSV(i) <> *blanks
098300030721     C*** ATB
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
098500030721     C                   EVAL      VABATB = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030715     C*** RSD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
098900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
099000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
099100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
099200030715     C* ==
099300030715     C                   ENDIF
099400030721     C*** RD2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
099600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030715     C*** IND
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
100000030715     C                   EVAL      VABIND = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** LOD
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
100400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** PRD
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
100800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030721     C*** NZD
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
101200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030715     C*** RMA
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
101600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030715     C*** NOT
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NT2
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** VCA
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
102800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VAS
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** FFD
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
103600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030721     C*** GC1
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GC2
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TSP
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
104800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
104900030721     C                   ENDIF
105000040510     C*** CBO
105100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105300040510     C                   ENDIF
105400030721     C*** NAS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
105600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TIC
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GCA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** XCO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
106800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** CTM
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** TCR
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
107600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** CTS
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** FTM
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** VAD
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
108800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** GMA
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GGA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
109600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GVA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** TC1
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC2
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
110800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** SCL
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** RMO
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
111600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** NMO
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*
112300030721     C* Reperisco quindi i campi numerici...
112400040510     C*** NSP
112500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
112600040510     C                   EVAL      PiStr=SkSplitCSV(i)
112700040510     C                   EXSR      CHKNUM
112800040510     C                   IF        PiInt=*on
112900040510     C                   Z-ADD     PiVal         VABNSP
113000040510     C                   ELSE
113100040510     C                   SETON                                        31
113200040510     C                   EVAL      VABNSP = *zeros
113300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040510     C                             + ' ' + 'VABNSP'
113500040510     C                   ENDIF
113600040510     C                   ENDIF
113700071106     C*** RMN
113800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
113900071106     C                   EVAL      PiStr=SkSplitCSV(i)
114000071106     C                   EXSR      CHKNUM
114100071106     C                   IF        PiInt=*on
114200071106     C                   Z-ADD     PiVal         VABRMN
114300071106     C                   ELSE
114400071106     C                   SETON                                        32
114500071106     C                   EVAL      VABRMN = *zeros
114600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
114700071106     C                             + ' ' + 'VABRMN'
114800071106     C                   ENDIF
114900071106     C                   ENDIF
115000030721     C*** CAD
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115200030721     C                   EVAL      PiStr=SkSplitCSV(i)
115300030721     C                   EXSR      CHKNUM
115400030721     C                   IF        PiInt=*on
115500030721     C                   Z-ADD     PiVal         Num5_0
115600030721     C                   MOVEL(p)  Num5_0        VABCAD
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAD = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAD'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** NCL
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on
116900030721     C                   Z-ADD     PiVal         VABNCL
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABNCL = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400040122     C                             + ' ' + 'VABNCL'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** PKB
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200030721     C                   Z-ADD     PiVal         VABPKB
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABPKB = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABPKB'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CAS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119300111124     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119400140915     C                             %trim(SkSplitCSV(i)) <> '.000'  AND
119500140915     C                             %trim(SkSplitCSV(i)) <> ',000'
119600101018     C                   EVAL      wFlgCAS = '1'
119700101018     C                   ENDIF
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABCAS
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABCAS = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABCAS'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** CCM
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on      AND
121400030721     C                             PiVal<=9999999 AND
121500030721     C                             PiVal>*zeros
121600030721     C                   Z-ADD     PiVal         VABCCM
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      VABCCM = *zeros
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100040510     C                             + ' ' + 'VABCCM'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNP
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNP
123200030721     C                   ELSE
123300040715     C                   SETON                                        32
123400030721     C                   EVAL      VABLNP = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600040510     C                             + ' ' + 'VABLNP'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** NRS
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on AND
124400030721     C                             PiVal<=99
124500030721     C                   Z-ADD     PiVal         VABNRS
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABNRS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** LNA
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiInt=*on    AND
125700030721     C                             PiVal<=999   AND
125800030721     C                             PiVal>*zeros
125900030721     C                   Z-ADD     PiVal         VABLNA
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      VABLNA = *zeros
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABLNA'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** CTR
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on  AND
127200030721     C                             PiVal<=999
127300030721     C                   Z-ADD     PiVal         VABCTR
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABCTR'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** IAS
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiNum=*on
128500030721     C                   Z-ADD     PiVal         VABIAS
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABIAS'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** VLB
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiNum=*on
129700030721     C                   Z-ADD     PiVal         VABVLB
129800030721     C                   ELSE
129900030721     C                   SETON                                        32
130000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130100030721     C                             + ' ' + 'VABVLB'
130200030721     C                   ENDIF
130300030721     C                   ENDIF
130400030721     C*** QFT
130500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
130600030721     C                   EVAL      PiStr=SkSplitCSV(i)
130700030721     C                   EXSR      CHKNUM
130800030721     C                   IF        PiNum=*on
130900030721     C                   Z-ADD     PiVal         VABQFT
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABQFT'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** NCD
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on      AND
132100030721     C                             PiVal<=9999999
132200030721     C                   Z-ADD     PiVal         VABNCD
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABNCD'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** NCA
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on      AND
133400030721     C                             PiVal<=9999999
133500030721     C                   Z-ADD     PiVal         VABNCA
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABNCA'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** ZNC
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on AND
134700030721     C                             PiVal<=99
134800030721     C                   Z-ADD     PiVal         VABZNC
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABZNC'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** DCR
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiInt=*on       AND
136000030721     C                             PiVal<=99999999
136100030721     C                   Z-ADD     PiVal         VABDCR
136200030721     C                   ELSE
136300030721     C                   SETON                                        32
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABDCR'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800030721     C*** HCR
136900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137000030721     C                   EVAL      PiStr=SkSplitCSV(i)
137100030721     C                   EXSR      CHKNUM
137200030721     C                   IF        PiInt=*on   AND
137300030721     C                             PiVal<=9999
137400030721     C                   Z-ADD     PiVal         VABHCR
137500030721     C                   ELSE
137600030721     C                   SETON                                        32
137700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137800030721     C                             + ' ' + 'VABHCR'
137900030721     C                   ENDIF
138000030721     C                   ENDIF
138100030721     C*** VMD
138200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138300030721     C                   EVAL      PiStr=SkSplitCSV(i)
138400030721     C                   EXSR      CHKNUM
138500030721     C                   IF        PiNum=*on
138600030721     C                   Z-ADD     PiVal         VABVMD
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139000030721     C                             + ' ' + 'VABVMD'
139100030721     C                   ENDIF
139200030721     C                   ENDIF
139300030721     C*** ANT
139400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139500030721     C                   EVAL      PiStr=SkSplitCSV(i)
139600030721     C                   EXSR      CHKNUM
139700030721     C                   IF        PiInt=*on        AND
139800030721     C                             PiVal<=999999999
139900030721     C                   Z-ADD     PiVal         VABANT
140000030721     C                   ELSE
140100030721     C                   SETON                                        32
140200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140300030721     C                             + ' ' + 'VABANT'
140400030721     C                   ENDIF
140500030721     C                   ENDIF
140600030721     C*** CMO
140700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
140800030721     C                   EVAL      PiStr=SkSplitCSV(i)
140900030721     C                   EXSR      CHKNUM
141000030721     C                   IF        PiInt=*on
141100030721     C                   Z-ADD     PiVal         Num5_0
141200030721     C                   MOVEL(p)  Num5_0        VABCMO
141300030721     C                   ELSE
141400030721     C                   SETON                                        32
141500030721     C                   EVAL      VABCMO = *zeros
141600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
141700030721     C                             + ' ' + 'VABCMO'
141800030721     C                   ENDIF
141900030721     C                   ENDIF
142000040510     C*
142100110930     C* Reperisco i campi del EDIVAT
142200040510     C*** VATNOT
142300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142500040510     C                   ENDIF
142600040510     C*** VATTRC
142700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
142800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
142900040510     C                   ENDIF
143000060331     C*** VATNOT_A
143100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143300060331     C                   ENDIF
143400060331     C*** VATNOT_B
143500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
143600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
143700060331     C                   ENDIF
143800060331     C*** VATNOT_E
143900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144100060331     C                   ENDIF
144200110927     C*** VATNOT_I
144300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144500110927     C                   ENDIF
144600111124     C*** VATNOT_J
144700111124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
144800111124     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
144900111124     C                   ENDIF
145000120405     C*** VATNOT_4
145100120405     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
145200120405     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
145300120405     C                   ENDIF
145400131018     C*** VATNOT_S
145500131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
145600131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
145700131018     C                   ENDIF
145800141128     C*** VATNOT_Y
145900141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
146000141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
146100141128     C                   ENDIF
146200070823     C*
146300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146400110421     C*
146500070925     C*
146600071029     C***  <===  -----------------------------------------
146700040510     C*
146800030715     C                   ENDIF
146900030715     C                   EVAL      i = i + 1
147000030715     C                   ENDDO
147100070925     C*
147200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147300100831     C*
147400070925     C*
147500071029     C***  <===  -----------------------------------------
147600070925     C*
147700070925     C* Considerazioni finali su CBO/CAS
147800100318     C                   IF        wFlgCAS = '1'    OR
147900100318     C                             VABCAS  > *zeros
148000070925     C                   IF        VABCBO = '1'
148100070925     C                   EVAL      VABCBO = '4'
148200070925     C                   ENDIF
148300070925     C                   IF        VABCBO = '2'
148400070925     C                   EVAL      VABCBO = '6'
148500070925     C                   ENDIF
148600070925     C                   ENDIF
148700070925     C***  <===  ----------------------------
148800040510     C*
148900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149000040510     C                   IF        VABNSP = *zeros
149100040510     C                   SETON                                        31
149200040510     C                   ENDIF
149300040715     C*
149400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149500040715     C                   IF        VABLNP = *zeros
149600040715     C                   SETON                                        31
149700040715     C                   ENDIF
149800040510     C*
149900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150000040510     C                   IF        VABRMN = *zeros
150100040510     C                   Z-ADD     VABNSP        VABRMN
150200040510     C                   ENDIF
150300040510     C*
150400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150500040510     C                   IF        VABRMA = *blanks
150600040714     C                   MOVEL     VABRMN        VABRMA
150700040510     C                   ENDIF
150800040128     C*
150900040510     C* Se provincia nn valorizzata la reperisco
151000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151100110912     C                   IF        VABPRD =  *blanks AND
151200110912     C                             VABNZD =  *blanks AND
151300110912     C                             VABCAD <> *blanks AND
151400110912     C                             VABLOD <> *blanks
151500040128     C                   CLEAR                   TISI95DS
151600040128     C                   EVAL      I95TCN = '3'
151700040128     C                   Z-ADD     datcor        I95DAT
151800040128     C                   EVAL      I95NAR = VABNZD
151900040128     C                   EVAL      I95CAP = VABCAD
152000040128     C                   EVAL      I95LOC = VABLOD
152100040128     C                   CALL      'TISI95R'
152200040128     C                   PARM                    TISI95DS
152300040128     C                   EVAL      VABPRD = O95PRV
152400040510     C                   ENDIF
152500020204     C*
152600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152700020204     C                   EXSR      CHKIMPDIV
152800030715     C                   ENDIF
152900020204     C*
153000010607     C                   ENDSR
153100010607     C*----------------------------------------------------*
153200010601
153300020204
153400020204     C*----------------------------------------------------*
153500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153600020204     C*----------------------------------------------------*
153700020204     C     CHKIMPDIV     BEGSR
153800020204     C*
153900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154000020204     C                   Z-ADD     *zeros        wrkDec            9 9
154100020204     C*
154200020204     C* Come prima cosa effettuo considerazioni sulla divisa
154300020204     C                   IF        vabIAS > *zeros
154400020204     C                   IF        vabVAS <> 'EUR'
154500020204     C                   EVAL      vabVAS =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   IF        vabCAS > *zeros
155000020204     C                   IF        vabVCA <> 'EUR'
155100020204     C                   EVAL      vabVCA =  'ITL'
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C                   IF        vabVMD > *zeros
155600020204     C                   IF        vabVAD <> 'EUR'
155700020204     C                   EVAL      vabVAD =  'ITL'
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156200020204     C                   Z-ADD     vabIAS        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAS = 'ITL'
156500020204     C                   EVAL      vabIAS = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157000020204     C                   Z-ADD     vabCAS        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVCA = 'ITL'
157300020204     C                   EVAL      vabCAS = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157800020204     C                   Z-ADD     vabVMD        wrkDec
157900020204     C                   IF        wrkDec > *zeros
158000020204     C                   IF        vabVAD = 'ITL'
158100020204     C                   EVAL      vabVMD = *zeros
158200020204     C                   ENDIF
158300020204     C                   ENDIF
158400020204     C*
158500020204     C                   ENDSR
158600020204     C***
158700010330
158800100318
158900100318
159000100318     C*----------------------------------------------------*
159100100318     C*  CONTROLLO NUMERICITA' CAMPI
159200100318     C*----------------------------------------------------*
159300100318     C     CHKNUM        BEGSR
159400100318     C*
159500100318     C                   IF        PiDecChr = *blanks
159600100318     C                   EVAL      PiDecChr = CharNUM
159700100318     C                   ENDIF
159800100318     C*
159900100318     C                   callp     UBISNUM_Check(PiStr
160000100318     C                                          :PiDecChr
160100100318     C                                          :PiVal
160200100318     C                                          :PiNum
160300100318     C                                          :PiInt)
160400100318     C*
160500100318     C                   ENDSR
160600100318     C***
160700010601
160800010601
160900010601
161000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161100010601     C     repfil        BEGSR
161200010601     C*
161300010601     C                   if        invfil = *zeros and
161400010601     C                             depfil > *zeros and
161500010629     C                             (vinflg = *blanks or
161600010629     C                              vinflg = *zeros)
161700010601     C
161800010601     C                   eval      invfil = depfil
161900010601     C                   endif
162000010601     C*
162100010601     C                   if        depfil <> invfil and
162200010601     C                             invfil > *zeros
162300010601     C                   eval      flgMulti = '1'
162400010601     C                   if        vinflg = *blanks
162500010601     C                   add       1             cntNonEl
162600010601     C                   endif
162700010601     C                   endif
162800010601     C*
162900010601     C                   if        vinflg = '2'
163000010601     C                   eval      flgStato = '2'
163100010601     C                   endif
163200010601     C*
163300010601     C                   ENDSR
163400010601     C***
163500010601
163600010601
163700010601
163800010330
163900040119      /TITLE Invio dei dati al punto operativo.
164000040119     C     invio         BEGSR
164100040119     C*
164200110930     C* 1° invio EDIVAT
164300040119     C                   reset                   dscmz
164400040119     C                   move      vlrpoi        cmzdst
164500110930     C                   eval      cmzfld = 'EDIVATWR'
164600040119     C                   eval      cmzmbd = vlrhdl
164700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164800040119     C***                if        prmfir = *blanks
164900110930     C                   eval      cmzfla = 'EDIVAT0F'
165000110930     C                   eval      cmzmba = 'EDIVAT0F'
165100040119     C***                else
165200040119     C***                eval      cmzfla = prmfir
165300040119     C***                eval      cmzmba = prmfir
165400040119     C***                endif
165500040119     C                   eval      cmznrr = *zeros
165600040119     C                   move      §ctrokvt      cmznrr
165700040119     C                   eval      cmzlba = vlrfl1
165800040119     C                   call(e)   'TIS711C'
165900040119     C                   parm                    dscmz
166000040119     C                   parm      *blanks       esito
166100040119     C                   if        %error
166200040119     C                             or cmzerr = '1'
166300040119     C                             or esito  = '1'
166400040119     C                   eval      wrkesito = '3'
166500040119     C                   else
166600040119     C*
166700110930     C* 2° invio EDIVAB
166800040119     C                   reset                   dscmz
166900040119     C                   move      vlrpoi        cmzdst
167000040119     C                   eval      cmzfld = vlrfou
167100040119     C                   eval      cmzmbd = vlrhdl
167200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167300040119     C***                if        prmfir = *blanks
167400110930     C                   eval      cmzfla = 'EDIVAB0F'
167500110930     C                   eval      cmzmba = 'EDIVAB0F'
167600040119     C***                else
167700040119     C***                eval      cmzfla = prmfir
167800040119     C***                eval      cmzmba = prmfir
167900040119     C***                endif
168000040119     C                   eval      cmznrr = *zeros
168100040119     C                   move      §ctrokvb      cmznrr
168200040119     C                   eval      cmzlba = vlrfl1
168300040119     C                   call(e)   'TIS711C'
168400040119     C                   parm                    dscmz
168500040119     C                   parm      *blanks       esito
168600040119     C                   if        %error
168700040119     C                             or cmzerr = '1'
168800040119     C                             or esito  = '1'
168900040119     C                   eval      wrkesito = '3'
169000040119     C                   endif
169100040119     C                   endif
169200040119     C*
169300040119     C                   ENDSR
169400040119     C***
169500010601
169600010601
169700010601
169800010601
169900010601      /TITLE Invio dei dati al punto operativo.
170000010601     C     opeini        BEGSR
170100010601     C*
170200010601     C* Inizializzo flag e contatori operativi
170300010601     C                   movel     '0'           flgGiro           1
170400010601     C                   movel     '0'           flgMulti          1
170500010601     C                   movel     '1'           flgStato          1
170600010615     C                   movel     '0'           flgOk             1
170700010601     C                   z-add     *zeros        cntNonEl         10 0
170800010601     C                   z-add     *zeros        depfil            3 0
170900010601     C                   z-add     *zeros        invfil            3 0
171000010601     C*
171100010601     C                   ENDSR
171200010601     C***
171300070326
171400070326
171500070326
171600070326
171700070326     C     *pssr         BEGSR
171800070329     C*
171900070329     C                   if        %open(tivin00r)
172000070329     C                   close     tivin00r
172100070329     C                   endif
172200110930     C                   if        %open(edivabwr)
172300110930     C                   close     edivabwr
172400070329     C                   endif
172500110930     C                   if        %open(edivatwr)
172600110930     C                   close     edivatwr
172700070329     C                   endif
172800070326     C*
172900070326     C* Effettuo la chiamata al CLLE preposto
173000110930     C                   call(e)   'TITVEVTC'
173100070326     C                   parm                    parccm
173200070326     C                   parm                    parmbr
173300070326     C                   parm      '2'           paropz
173400070326     C*
173500070326     C                   eval      wrkesito = '2'
173600070404     C*
173700070404     C                   seton                                        LR
173800070326     C*
173900070326     C                   ENDSR     '*CANCL'
174000070326     C***
174100100318
174200100318
174300100318
174400100318     C     normCSV       BEGSR
174500100318     C*
174600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174700100318     C* separatori campi
174800100318     C                   if        CharTXT <> *blanks
174900100318     C                   z-add     *zeros        wLenVINDTA        4 0
175000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175100100318     C                   setoff                                       80
175200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175500100318     C                   if        not *in80
175600100318     C                   seton                                        80
175700100318     C                   else
175800100318     C                   setoff                                       80
175900100318     C                   endif
176000100318     C                   endif
176100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176200100318     C                             and *in80 = *on
176300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176400100318     C                   endif
176500100318     C                   enddo
176600100318     C                   endif
176700100318     C*
176800100318     C                   ENDSR
176900100318     C***
177000070326
177100100326
177200100326
177300100326     C*----------------------------------------------------*
177400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177500100326     C*----------------------------------------------------*
177600100326     C     REPNSP        BEGSR
177700100326     C*
177800100326     C* NSP => Stacco un numeratore da AZNUM
177900100326     C                   clear                   TRUL33DS
178000100326     C                   eval      I33OPE = *zeros
178100100326     C                   eval      I33CNU = 302
178200100326     C                   eval      I33NUM = 1
178300100326     C                   movel     TRUL33DS      KPJBU
178400100326     C                   call      'TRUL33R'
178500100326     C                   parm                    KPJBA
178600100326     C                   movel     KPJBU         TRUL33DS
178700100326     C                   if        O33ERR = *zeros
178800100326     C                   z-add     O33NRF        VABNSP
178900100326     C                   z-add     O33NRF        VATNSP
179000100326     C                   else
179100100326     C                   SETON                                        31
179200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179300100326     C                             + ' ' + 'VABNSP VATNSP'
179400100326     C                   endif
179500100326     C*
179600100326     C                   ENDSR
179700010330
179800100326
179900010330
180000000613     C     *inzsr        BEGSR
180100990910     C*
180200990910     C     *entry        plist
180300990920     C                   parm                    tivlrds
180400990921     C                   parm      wrkesito      esito
180500000724     C                   parm                    prmlit
180600000710     C                   parm                    prmfir
180700010330     C*
180800010330     C* CALCOLA LA DATA CORRENTE
180900110930     C                   time                    wn14             14 0
181000110930     C                   movel     wn14          oracor            6 0          *ORA
181100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181200100318     C                   eval      datcor = %dec(%date() : *ISO)
181300000613     C*
181400000613     C                   ENDSR
181500000613     C***
181600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181700111124
181800111124
181900110315
182000110315
182100100318
