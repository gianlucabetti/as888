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
020800160224     C*
020900160224     C* Personalizzazione =>
021000160224     C                   eval      vindta  = %scanrpl(CharCSV+'VATNOT_E':
021100160224     C                                       CharCSV+'VATNOT_E'+CharCSV:vindta)
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
101700030721     C                   ENDIF
101800030715     C*** RMA
101900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
102000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
102100030715     C                   ENDIF
102200030715     C*** NOT
102300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
102400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
102500030715     C                   ENDIF
102600030715     C*** NT2
102700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
102800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
102900030715     C                   ENDIF
103000030715     C*** VCA
103100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
103200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
103300030715     C                   ENDIF
103400030715     C*** VAS
103500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
103600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
103700030715     C                   ENDIF
103800030715     C*** FFD
103900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
104000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
104100030715     C                   ENDIF
104200030721     C*** GC1
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
104400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GC2
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
104800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TSP
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
105200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
105300030721     C                   ENDIF
105400040510     C*** CBO
105500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
105600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
105700040510     C                   ENDIF
105800030721     C*** NAS
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
106000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TIC
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
106400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GCA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
106800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** XCO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
107200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** CTM
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
107600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TCR
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
108000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** CTS
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
108400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** FTM
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
108800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** VAD
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
109200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** GMA
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
109600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** GGA
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
110000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*** GVA
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
110400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
110500030721     C                   ENDIF
110600030721     C*** TC1
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
110800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
110900030721     C                   ENDIF
111000030721     C*** TC2
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
111200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
111300030721     C                   ENDIF
111400030721     C*** SCL
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
111600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
111700030721     C                   ENDIF
111800030721     C*** RMO
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
112000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
112100030721     C                   ENDIF
112200030721     C*** NMO
112300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
112400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
112500030721     C                   ENDIF
112600030721     C*
112700030721     C* Reperisco quindi i campi numerici...
112800040510     C*** NSP
112900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
113000040510     C                   EVAL      PiStr=SkSplitCSV(i)
113100040510     C                   EXSR      CHKNUM
113200040510     C                   IF        PiInt=*on
113300040510     C                   Z-ADD     PiVal         VABNSP
113400040510     C                   ELSE
113500040510     C                   SETON                                        31
113600040510     C                   EVAL      VABNSP = *zeros
113700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
113800040510     C                             + ' ' + 'VABNSP'
113900040510     C                   ENDIF
114000040510     C                   ENDIF
114100071106     C*** RMN
114200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
114300071106     C                   EVAL      PiStr=SkSplitCSV(i)
114400071106     C                   EXSR      CHKNUM
114500071106     C                   IF        PiInt=*on
114600071106     C                   Z-ADD     PiVal         VABRMN
114700071106     C                   ELSE
114800071106     C                   SETON                                        32
114900071106     C                   EVAL      VABRMN = *zeros
115000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
115100071106     C                             + ' ' + 'VABRMN'
115200071106     C                   ENDIF
115300071106     C                   ENDIF
115400030721     C*** CAD
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on
115900030721     C                   Z-ADD     PiVal         Num5_0
116000030721     C                   MOVEL(p)  Num5_0        VABCAD
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABCAD = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABCAD'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** NCL
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on
117300030721     C                   Z-ADD     PiVal         VABNCL
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABNCL = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800040122     C                             + ' ' + 'VABNCL'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** PKB
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABPKB
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      VABPKB = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABPKB'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** CAS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
119600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
119700111124     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
119800140915     C                             %trim(SkSplitCSV(i)) <> '.000'  AND
119900140915     C                             %trim(SkSplitCSV(i)) <> ',000'
120000101018     C                   EVAL      wFlgCAS = '1'
120100101018     C                   ENDIF
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiNum=*on
120500030721     C                   Z-ADD     PiVal         VABCAS
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABCAS = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABCAS'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CCM
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on      AND
121800030721     C                             PiVal<=9999999 AND
121900030721     C                             PiVal>*zeros
122000030721     C                   Z-ADD     PiVal         VABCCM
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      VABCCM = *zeros
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500040510     C                             + ' ' + 'VABCCM'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** LNP
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on    AND
123300030721     C                             PiVal<=999   AND
123400030721     C                             PiVal>*zeros
123500030721     C                   Z-ADD     PiVal         VABLNP
123600030721     C                   ELSE
123700040715     C                   SETON                                        32
123800030721     C                   EVAL      VABLNP = *zeros
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000040510     C                             + ' ' + 'VABLNP'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** NRS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiInt=*on AND
124800030721     C                             PiVal<=99
124900030721     C                   Z-ADD     PiVal         VABNRS
125000030721     C                   ELSE
125100030721     C                   SETON                                        32
125200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125300030721     C                             + ' ' + 'VABNRS'
125400030721     C                   ENDIF
125500030721     C                   ENDIF
125600030721     C*** LNA
125700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
125800030721     C                   EVAL      PiStr=SkSplitCSV(i)
125900030721     C                   EXSR      CHKNUM
126000030721     C                   IF        PiInt=*on    AND
126100030721     C                             PiVal<=999   AND
126200030721     C                             PiVal>*zeros
126300030721     C                   Z-ADD     PiVal         VABLNA
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      VABLNA = *zeros
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABLNA'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** CTR
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on  AND
127600030721     C                             PiVal<=999
127700030721     C                   Z-ADD     PiVal         VABCTR
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABCTR'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** IAS
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABIAS
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABIAS'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** VLB
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABVLB
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABVLB'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** QFT
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiNum=*on
131300030721     C                   Z-ADD     PiVal         VABQFT
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABQFT'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** NCD
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on      AND
132500030721     C                             PiVal<=9999999
132600030721     C                   Z-ADD     PiVal         VABNCD
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABNCD'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** NCA
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on      AND
133800030721     C                             PiVal<=9999999
133900030721     C                   Z-ADD     PiVal         VABNCA
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABNCA'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** ZNC
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiInt=*on AND
135100030721     C                             PiVal<=99
135200030721     C                   Z-ADD     PiVal         VABZNC
135300030721     C                   ELSE
135400030721     C                   SETON                                        32
135500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135600030721     C                             + ' ' + 'VABZNC'
135700030721     C                   ENDIF
135800030721     C                   ENDIF
135900030721     C*** DCR
136000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
136100030721     C                   EVAL      PiStr=SkSplitCSV(i)
136200030721     C                   EXSR      CHKNUM
136300030721     C                   IF        PiInt=*on       AND
136400030721     C                             PiVal<=99999999
136500030721     C                   Z-ADD     PiVal         VABDCR
136600030721     C                   ELSE
136700030721     C                   SETON                                        32
136800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136900030721     C                             + ' ' + 'VABDCR'
137000030721     C                   ENDIF
137100030721     C                   ENDIF
137200030721     C*** HCR
137300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
137400030721     C                   EVAL      PiStr=SkSplitCSV(i)
137500030721     C                   EXSR      CHKNUM
137600030721     C                   IF        PiInt=*on   AND
137700030721     C                             PiVal<=9999
137800030721     C                   Z-ADD     PiVal         VABHCR
137900030721     C                   ELSE
138000030721     C                   SETON                                        32
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABHCR'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500030721     C*** VMD
138600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
138700030721     C                   EVAL      PiStr=SkSplitCSV(i)
138800030721     C                   EXSR      CHKNUM
138900030721     C                   IF        PiNum=*on
139000030721     C                   Z-ADD     PiVal         VABVMD
139100030721     C                   ELSE
139200030721     C                   SETON                                        32
139300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139400030721     C                             + ' ' + 'VABVMD'
139500030721     C                   ENDIF
139600030721     C                   ENDIF
139700030721     C*** ANT
139800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
139900030721     C                   EVAL      PiStr=SkSplitCSV(i)
140000030721     C                   EXSR      CHKNUM
140100030721     C                   IF        PiInt=*on        AND
140200030721     C                             PiVal<=999999999
140300030721     C                   Z-ADD     PiVal         VABANT
140400030721     C                   ELSE
140500030721     C                   SETON                                        32
140600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
140700030721     C                             + ' ' + 'VABANT'
140800030721     C                   ENDIF
140900030721     C                   ENDIF
141000030721     C*** CMO
141100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
141200030721     C                   EVAL      PiStr=SkSplitCSV(i)
141300030721     C                   EXSR      CHKNUM
141400030721     C                   IF        PiInt=*on
141500030721     C                   Z-ADD     PiVal         Num5_0
141600030721     C                   MOVEL(p)  Num5_0        VABCMO
141700030721     C                   ELSE
141800030721     C                   SETON                                        32
141900030721     C                   EVAL      VABCMO = *zeros
142000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
142100030721     C                             + ' ' + 'VABCMO'
142200030721     C                   ENDIF
142300030721     C                   ENDIF
142400040510     C*
142500110930     C* Reperisco i campi del EDIVAT
142600040510     C*** VATNOT
142700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
142800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
142900040510     C                   ENDIF
143000040510     C*** VATTRC
143100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
143200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
143300040510     C                   ENDIF
143400060331     C*** VATNOT_A
143500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
143600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
143700060331     C                   ENDIF
143800060331     C*** VATNOT_B
143900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
144000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
144100060331     C                   ENDIF
144200060331     C*** VATNOT_E
144300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
144400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
144500060331     C                   ENDIF
144600110927     C*** VATNOT_I
144700110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
144800110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
144900110927     C                   ENDIF
145000111124     C*** VATNOT_J
145100111124     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
145200111124     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
145300111124     C                   ENDIF
145400120405     C*** VATNOT_4
145500120405     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
145600120405     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
145700120405     C                   ENDIF
145800131018     C*** VATNOT_S
145900131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
146000131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
146100131018     C                   ENDIF
146200141128     C*** VATNOT_Y
146300141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
146400141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
146500141128     C                   ENDIF
146600070823     C*
146700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
146800110421     C*
146900070925     C*
147000071029     C***  <===  -----------------------------------------
147100040510     C*
147200030715     C                   ENDIF
147300030715     C                   EVAL      i = i + 1
147400030715     C                   ENDDO
147500070925     C*
147600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147700100831     C*
147800070925     C*
147900071029     C***  <===  -----------------------------------------
148000070925     C*
148100070925     C* Considerazioni finali su CBO/CAS
148200100318     C                   IF        wFlgCAS = '1'    OR
148300100318     C                             VABCAS  > *zeros
148400070925     C                   IF        VABCBO = '1'
148500070925     C                   EVAL      VABCBO = '4'
148600070925     C                   ENDIF
148700070925     C                   IF        VABCBO = '2'
148800070925     C                   EVAL      VABCBO = '6'
148900070925     C                   ENDIF
149000070925     C                   ENDIF
149100070925     C***  <===  ----------------------------
149200040510     C*
149300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149400040510     C                   IF        VABNSP = *zeros
149500040510     C                   SETON                                        31
149600040510     C                   ENDIF
149700040715     C*
149800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149900040715     C                   IF        VABLNP = *zeros
150000040715     C                   SETON                                        31
150100040715     C                   ENDIF
150200040510     C*
150300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150400040510     C                   IF        VABRMN = *zeros
150500040510     C                   Z-ADD     VABNSP        VABRMN
150600040510     C                   ENDIF
150700040510     C*
150800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150900040510     C                   IF        VABRMA = *blanks
151000040714     C                   MOVEL     VABRMN        VABRMA
151100040510     C                   ENDIF
151200040128     C*
151300040510     C* Se provincia nn valorizzata la reperisco
151400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151500110912     C                   IF        VABPRD =  *blanks AND
151600110912     C                             VABNZD =  *blanks AND
151700110912     C                             VABCAD <> *blanks AND
151800110912     C                             VABLOD <> *blanks
151900040128     C                   CLEAR                   TISI95DS
152000040128     C                   EVAL      I95TCN = '3'
152100040128     C                   Z-ADD     datcor        I95DAT
152200040128     C                   EVAL      I95NAR = VABNZD
152300040128     C                   EVAL      I95CAP = VABCAD
152400040128     C                   EVAL      I95LOC = VABLOD
152500040128     C                   CALL      'TISI95R'
152600040128     C                   PARM                    TISI95DS
152700040128     C                   EVAL      VABPRD = O95PRV
152800040510     C                   ENDIF
152900020204     C*
153000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153100020204     C                   EXSR      CHKIMPDIV
153200030715     C                   ENDIF
153300020204     C*
153400010607     C                   ENDSR
153500010607     C*----------------------------------------------------*
153600010601
153700020204
153800020204     C*----------------------------------------------------*
153900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
154000020204     C*----------------------------------------------------*
154100020204     C     CHKIMPDIV     BEGSR
154200020204     C*
154300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154400020204     C                   Z-ADD     *zeros        wrkDec            9 9
154500020204     C*
154600020204     C* Come prima cosa effettuo considerazioni sulla divisa
154700020204     C                   IF        vabIAS > *zeros
154800020204     C                   IF        vabVAS <> 'EUR'
154900020204     C                   EVAL      vabVAS =  'ITL'
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C                   IF        vabCAS > *zeros
155400020204     C                   IF        vabVCA <> 'EUR'
155500020204     C                   EVAL      vabVCA =  'ITL'
155600020204     C                   ENDIF
155700020204     C                   ENDIF
155800020204     C*
155900020204     C                   IF        vabVMD > *zeros
156000020204     C                   IF        vabVAD <> 'EUR'
156100020204     C                   EVAL      vabVAD =  'ITL'
156200020204     C                   ENDIF
156300020204     C                   ENDIF
156400020204     C*
156500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156600020204     C                   Z-ADD     vabIAS        wrkDec
156700020204     C                   IF        wrkDec > *zeros
156800020204     C                   IF        vabVAS = 'ITL'
156900020204     C                   EVAL      vabIAS = *zeros
157000020204     C                   ENDIF
157100020204     C                   ENDIF
157200020204     C*
157300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157400020204     C                   Z-ADD     vabCAS        wrkDec
157500020204     C                   IF        wrkDec > *zeros
157600020204     C                   IF        vabVCA = 'ITL'
157700020204     C                   EVAL      vabCAS = *zeros
157800020204     C                   ENDIF
157900020204     C                   ENDIF
158000020204     C*
158100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158200020204     C                   Z-ADD     vabVMD        wrkDec
158300020204     C                   IF        wrkDec > *zeros
158400020204     C                   IF        vabVAD = 'ITL'
158500020204     C                   EVAL      vabVMD = *zeros
158600020204     C                   ENDIF
158700020204     C                   ENDIF
158800020204     C*
158900020204     C                   ENDSR
159000020204     C***
159100010330
159200100318
159300100318
159400100318     C*----------------------------------------------------*
159500100318     C*  CONTROLLO NUMERICITA' CAMPI
159600100318     C*----------------------------------------------------*
159700100318     C     CHKNUM        BEGSR
159800100318     C*
159900100318     C                   IF        PiDecChr = *blanks
160000100318     C                   EVAL      PiDecChr = CharNUM
160100100318     C                   ENDIF
160200100318     C*
160300100318     C                   callp     UBISNUM_Check(PiStr
160400100318     C                                          :PiDecChr
160500100318     C                                          :PiVal
160600100318     C                                          :PiNum
160700100318     C                                          :PiInt)
160800100318     C*
160900100318     C                   ENDSR
161000100318     C***
161100010601
161200010601
161300010601
161400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161500010601     C     repfil        BEGSR
161600010601     C*
161700010601     C                   if        invfil = *zeros and
161800010601     C                             depfil > *zeros and
161900010629     C                             (vinflg = *blanks or
162000010629     C                              vinflg = *zeros)
162100010601     C
162200010601     C                   eval      invfil = depfil
162300010601     C                   endif
162400010601     C*
162500010601     C                   if        depfil <> invfil and
162600010601     C                             invfil > *zeros
162700010601     C                   eval      flgMulti = '1'
162800010601     C                   if        vinflg = *blanks
162900010601     C                   add       1             cntNonEl
163000010601     C                   endif
163100010601     C                   endif
163200010601     C*
163300010601     C                   if        vinflg = '2'
163400010601     C                   eval      flgStato = '2'
163500010601     C                   endif
163600010601     C*
163700010601     C                   ENDSR
163800010601     C***
163900010601
164000010601
164100010601
164200010330
164300040119      /TITLE Invio dei dati al punto operativo.
164400040119     C     invio         BEGSR
164500040119     C*
164600110930     C* 1° invio EDIVAT
164700040119     C                   reset                   dscmz
164800040119     C                   move      vlrpoi        cmzdst
164900110930     C                   eval      cmzfld = 'EDIVATWR'
165000040119     C                   eval      cmzmbd = vlrhdl
165100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165200040119     C***                if        prmfir = *blanks
165300110930     C                   eval      cmzfla = 'EDIVAT0F'
165400110930     C                   eval      cmzmba = 'EDIVAT0F'
165500040119     C***                else
165600040119     C***                eval      cmzfla = prmfir
165700040119     C***                eval      cmzmba = prmfir
165800040119     C***                endif
165900040119     C                   eval      cmznrr = *zeros
166000040119     C                   move      §ctrokvt      cmznrr
166100040119     C                   eval      cmzlba = vlrfl1
166200040119     C                   call(e)   'TIS711C'
166300040119     C                   parm                    dscmz
166400040119     C                   parm      *blanks       esito
166500040119     C                   if        %error
166600040119     C                             or cmzerr = '1'
166700040119     C                             or esito  = '1'
166800040119     C                   eval      wrkesito = '3'
166900040119     C                   else
167000040119     C*
167100110930     C* 2° invio EDIVAB
167200040119     C                   reset                   dscmz
167300040119     C                   move      vlrpoi        cmzdst
167400040119     C                   eval      cmzfld = vlrfou
167500040119     C                   eval      cmzmbd = vlrhdl
167600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167700040119     C***                if        prmfir = *blanks
167800110930     C                   eval      cmzfla = 'EDIVAB0F'
167900110930     C                   eval      cmzmba = 'EDIVAB0F'
168000040119     C***                else
168100040119     C***                eval      cmzfla = prmfir
168200040119     C***                eval      cmzmba = prmfir
168300040119     C***                endif
168400040119     C                   eval      cmznrr = *zeros
168500040119     C                   move      §ctrokvb      cmznrr
168600040119     C                   eval      cmzlba = vlrfl1
168700040119     C                   call(e)   'TIS711C'
168800040119     C                   parm                    dscmz
168900040119     C                   parm      *blanks       esito
169000040119     C                   if        %error
169100040119     C                             or cmzerr = '1'
169200040119     C                             or esito  = '1'
169300040119     C                   eval      wrkesito = '3'
169400040119     C                   endif
169500040119     C                   endif
169600040119     C*
169700040119     C                   ENDSR
169800040119     C***
169900010601
170000010601
170100010601
170200010601
170300010601      /TITLE Invio dei dati al punto operativo.
170400010601     C     opeini        BEGSR
170500010601     C*
170600010601     C* Inizializzo flag e contatori operativi
170700010601     C                   movel     '0'           flgGiro           1
170800010601     C                   movel     '0'           flgMulti          1
170900010601     C                   movel     '1'           flgStato          1
171000010615     C                   movel     '0'           flgOk             1
171100010601     C                   z-add     *zeros        cntNonEl         10 0
171200010601     C                   z-add     *zeros        depfil            3 0
171300010601     C                   z-add     *zeros        invfil            3 0
171400010601     C*
171500010601     C                   ENDSR
171600010601     C***
171700070326
171800070326
171900070326
172000070326
172100070326     C     *pssr         BEGSR
172200070329     C*
172300070329     C                   if        %open(tivin00r)
172400070329     C                   close     tivin00r
172500070329     C                   endif
172600110930     C                   if        %open(edivabwr)
172700110930     C                   close     edivabwr
172800070329     C                   endif
172900110930     C                   if        %open(edivatwr)
173000110930     C                   close     edivatwr
173100070329     C                   endif
173200070326     C*
173300070326     C* Effettuo la chiamata al CLLE preposto
173400110930     C                   call(e)   'TITVEVTC'
173500070326     C                   parm                    parccm
173600070326     C                   parm                    parmbr
173700070326     C                   parm      '2'           paropz
173800070326     C*
173900070326     C                   eval      wrkesito = '2'
174000070404     C*
174100070404     C                   seton                                        LR
174200070326     C*
174300070326     C                   ENDSR     '*CANCL'
174400070326     C***
174500100318
174600100318
174700100318
174800100318     C     normCSV       BEGSR
174900100318     C*
175000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175100100318     C* separatori campi
175200100318     C                   if        CharTXT <> *blanks
175300100318     C                   z-add     *zeros        wLenVINDTA        4 0
175400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175500100318     C                   setoff                                       80
175600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175900100318     C                   if        not *in80
176000100318     C                   seton                                        80
176100100318     C                   else
176200100318     C                   setoff                                       80
176300100318     C                   endif
176400100318     C                   endif
176500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176600100318     C                             and *in80 = *on
176700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176800100318     C                   endif
176900100318     C                   enddo
177000100318     C                   endif
177100100318     C*
177200100318     C                   ENDSR
177300100318     C***
177400070326
177500100326
177600100326
177700100326     C*----------------------------------------------------*
177800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177900100326     C*----------------------------------------------------*
178000100326     C     REPNSP        BEGSR
178100100326     C*
178200100326     C* NSP => Stacco un numeratore da AZNUM
178300100326     C                   clear                   TRUL33DS
178400100326     C                   eval      I33OPE = *zeros
178500100326     C                   eval      I33CNU = 302
178600100326     C                   eval      I33NUM = 1
178700100326     C                   movel     TRUL33DS      KPJBU
178800100326     C                   call      'TRUL33R'
178900100326     C                   parm                    KPJBA
179000100326     C                   movel     KPJBU         TRUL33DS
179100100326     C                   if        O33ERR = *zeros
179200100326     C                   z-add     O33NRF        VABNSP
179300100326     C                   z-add     O33NRF        VATNSP
179400100326     C                   else
179500100326     C                   SETON                                        31
179600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179700100326     C                             + ' ' + 'VABNSP VATNSP'
179800100326     C                   endif
179900100326     C*
180000100326     C                   ENDSR
180100010330
180200100326
180300010330
180400000613     C     *inzsr        BEGSR
180500990910     C*
180600990910     C     *entry        plist
180700990920     C                   parm                    tivlrds
180800990921     C                   parm      wrkesito      esito
180900000724     C                   parm                    prmlit
181000000710     C                   parm                    prmfir
181100010330     C*
181200010330     C* CALCOLA LA DATA CORRENTE
181300110930     C                   time                    wn14             14 0
181400110930     C                   movel     wn14          oracor            6 0          *ORA
181500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181600100318     C                   eval      datcor = %dec(%date() : *ISO)
181700000613     C*
181800000613     C                   ENDSR
181900000613     C***
182000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
182100111124
182200111124
182300110315
182400110315
182500100318
