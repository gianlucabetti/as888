000100121119      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500121119     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600121119     FEDIVATwr  O    E             DISK    usropn
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
002000121119     D fivabds       e ds                  extname(EDIVAB0f)
002100121119     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002200121119     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
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
005900110708     D wVABRSD_1       s                   LIKE(VABRSD)
006000110708     D wVABRSD_2       s                   LIKE(VABRSD)
006100110708     D wVABRSD_3       s                   LIKE(VABRSD)
006200110708     D wVABDCR         s                   LIKE(VABDCR)
006300110708     D wFLGDCR         s              1    INZ
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
008200100318
008300100909     D*------------------
008400100909     D* DS CALCOLO CHECK-DIGITS
008500100909     D*------------------
008600100909     D TRUL28DSX     e ds                  inz
008700010330
008800010330
008900010330
009000000913     C                   reset                   rrnum
009100990921     C                   reset                   esito
009200990921     C                   reset                   wrkesito
009300010601     C*
009400010601     C                   exsr      opeini
009500010605     C                   exsr      rwvab
009600010601     C*
009700010601     C                   seton                                        lr
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010601     C*--------------------------------------------------------
010400010601     C     PREELA        BEGSR
010500010601     C*
010600010601     C* SE OCCORRE SPEDIRE IN FILIALE
010700010601     C                   if        invfil <> *zeros and
010800010601     C                             flgGiro = '0'
010900010601     C*
011000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010601     C                   eval      flgGiro = '1'
011200010601     C*
011300010601     C                   endif
011400010601     C*
011500010601     C                   ENDSR
011600010601     C***
011700010601
011800010601
011900010601
012000010601
012100010601     C*--------------------------------------------------------
012200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012300010601     C*--------------------------------------------------------
012400010601     C     ENDELA        BEGSR
012500040119     C*
012600040119     C* Effettuo la chiamata al CLLE preposto
012700121119     C                   call(e)   'TITVEVTC'
012800040510     C                   parm                    parccm
012900040119     C                   parm                    parmbr
013000040119     C                   parm      '2'           paropz
013100050201     C*
013200050201     C* Effettuo lancio TISI95 solo x chiusura
013300050201     C                   CLEAR                   TISI95DS
013400050201     C                   EVAL      I95TLA = 'C'
013500050201     C                   CALL      'TISI95R'
013600050201     C                   PARM                    TISI95DS
013700000616     C*
013800010601     C                   ENDSR
013900010601     C***
014000000613
014100010601
014200010601
014300010330     C*--------------------------------------------------------
014400121119     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
014500010330     C*--------------------------------------------------------
014600010605     C     RWVAB         BEGSR
014700010330     C*
014800010330     C                   if        not %open(tivin00r)
014900010330     C                   open      tivin00r
015000010330     C                   endif
015100121119     C                   if        not %open(EDIVABwr)
015200121119     C                   open      EDIVABwr
015300010330     C                   endif
015400040119     C*
015500121119     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
015600040119     C                   exsr      prevat
015700040119     C*
015800040119     C                   if        chkcall = '0'
015900040119     C*
016000121119     C                   if        not %open(EDIVATwr)
016100121119     C                   open      EDIVATwr
016200040119     C                   endif
016300010330     C*
016400040119     C                   clear                   §CTROKVB          7 0
016500040119     C                   clear                   §CTROKVT          7 0
016600040119     C                   clear                   §CTRMO            7 0
016700040119     C                   clear                   §CTRNO            7 0
016800070823     C*
016900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017100110415     C                   exsr      inzvar
017200110415     C                   exsr      impvab
017300110131     C*
017400110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017500110315 xxx C***                read      tivin00r                               70
017600010330     C*
017700030822     C                   do        *HIVAL
017800010330     C*
017900030822     C                   read      tivin00r                               70
018000010618     C*
018100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018200010618     C                   if        vinflg = '1'
018300010618     C                   eval      flgOk = '1'
018400010618     C                   endif
018500040510     C*
018600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018700040510     C                   z-add     *zeros        wLenVINDTA        4 0
018800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019200040510     C                   z-add     1             wFlgVINDTA
019300040510     C                   leave
019400040510     C                   endif
019500040510     C                   enddo
019600010618     C*
019700040510     C                   if        vindta > *blanks AND
019800040510     C                             wFlgVINDTA = 1
019900010330     C                   add       1             rrnum
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
021100070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021200070925     C                   eval      vindta  = ' ' + %trim(vindta)
021300070925     C*
021400080527     C                   exsr      inzvar
021500100318     C                   exsr      normCSV
021600010605     C                   exsr      impvab
021700010601     C*
021800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021900010605     C                   eval      depfil = VABLNP
022000010601     C                   exsr      repfil
022100010601     C                   if        depfil = invfil
022200021025     C                   if        vlrpoi = 999
022300030822     C                   move(P)   invfil        VABFGS
022400021025     C                   else
022500030822     C                   move(P)   vlrpoi        VABFGS
022600021025     C                   endif
022700010601     C*
022800010601     C                   exsr      PREELA
022900010601     C*
023000010604     C* Ebbene...
023100010604     C*
023200040119     C  N31              add       1             §CTROKVB
023300040119     C   32              add       1             §CTRMO
023400040119     C   31              add       1             §CTRNO
023500080418     C*
023600030822     C                   if        wGiro = 2
023700080513     C* Scrivo sempre il VAT
023800080513     C  N31              exsr      wrivat
023900110201     C                   exsr      chkvab
024000080513     C                   endif
024100010604     C*
024200010604     C                   if        *in31 = *off and
024300010604     C                             *in32 = *off
024400010604     C                   eval      vinflg = '1'
024500010604     C                   else
024600010604     C                   eval      vinflg = '2'
024700010604     C                   endif
024800010604     C                   endif
024900010601     C*
025000010604     C                   endif
025100010604     C*
025200010330     C                   else
025300010330     C                   eval      vinflg = '1'
025400010330     C                   endif
025500010601     C*
025600010601     C  N70              update    tivin000
025700010330     C*
025800030822     C  N70              enddo
025900040119     C*
026000080527     C* Verifico dati rimasti "in cannna"...
026100080527     C                   seton                                        65
026200110201     C                   exsr      chkvab
026300080527     C*
026400040119     C                   endif                                                  (endif - chkcall)
026500010601     C*
026600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026700010601     C                   if        cntNonEl = *zeros or
026800010601     C                             flgMulti = '0'
026900010330     C* Se non ci sono record con errori ...
027000010601     C                   if        §ctrno = 0 and
027100010604     C                             §ctrmo = 0 and
027200010601     C                             flgStato <> '2'
027300010330     C* ... restituisco esito OK.
027400010330     C                   eval      wrkesito = '0'
027500010330     C                   else
027600040119     C                   if        §ctrokvb > 0
027700010330     C                   eval      wrkesito = '1'
027800010330     C                   else
027900010615     C                   if        flgOk = '0'
028000010615     C                   eval      wrkesito = '2'
028100010615     C                   else
028200010615     C                   eval      wrkesito = '6'
028300010615     C                   endif
028400010330     C                   endif
028500010330     C                   endif
028600010601     C                   else
028700010601     C                   eval      wrkesito = '9'
028800010601     C                   endif
028900010330     C*
029000010330     C                   if        %open(tivin00r)
029100010330     C                   close     tivin00r
029200010330     C                   endif
029300121119     C                   if        %open(EDIVABwr)
029400121119     C                   close     EDIVABwr
029500010330     C                   endif
029600121119     C                   if        %open(EDIVATwr)
029700121119     C                   close     EDIVATwr
029800040512     C                   endif
029900010601     C*
030000010601     C                   if        vlrpoi <> 999
030100010601     C                   eval      invfil = vlrpoi
030200010601     C                   endif
030300010330     C*
030400040119     C                   if        §ctrokvb > 0
030500010601     C                             and invfil > *zeros
030600010330     C                   exsr      invio
030700010330     C                   endif
030800010601     C*
030900010618     C                   if        flgGiro = '1'
031000010601     C                   exsr      endela
031100010618     C                   endif
031200010330     C*
031300010330     C                   ENDSR
031400010330     C***
031500080527
031600080527
031700080527
031800080527     C*----------------------------------------------------*
031900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032000080527     C*----------------------------------------------------*
032100100413     C     CHKVAB        BEGSR
032200110201     C*
032300110201     C* Se richiesti ignorare colli / peso / volume
032400110315 xxx C***                if        *in35
032500110315 xxx C***                z-add     *zeros        VABNCL
032600110315 xxx C***                z-add     *zeros        VABPKB
032700110315 xxx C***                z-add     *zeros        VABVLB
032800110315 xxx C***                endif
032900080527     C*
033000080527     C* Scrivo il VAB solo a rottura d spedizione
033100080527     C                   if        depspe = *zeros
033200080527     C  N31              eval      fivabds_sav = fivabds
033300080527     C                   eval      depspe = vabNSP
033400080527     C                   else
033500080527     C                   if        depspe = vabNSP AND *in65 = *off
033600100413     C   51              add       VABNCL        sav_VABNCL
033700100413     C   51              add       VABPKB        sav_VABPKB
033800100413     C   51              add       VABVLB        sav_VABVLB
033900100720     C   52              z-add     VABNCL        sav_VABNCL
034000100720     C   52              z-add     VABPKB        sav_VABPKB
034100100720     C   52              z-add     VABVLB        sav_VABVLB
034200100720     C   53              add       VABNCL        sav_VABNCL
034300100720     C   53              z-add     VABPKB        sav_VABPKB
034400100720     C   53              z-add     VABVLB        sav_VABVLB
034500080527     C                   else
034600080527     C  N31              eval      fivabds_ok = fivabds_sav
034700080527     C  N31              exsr      wrivab
034800080527     C  N31              eval      fivabds_sav = fivabds
034900080527     C                   eval      depspe = vabNSP
035000080527     C                   endif
035100080527     C                   endif
035200080527     C*
035300080527     C                   ENDSR
035400040119
035500040119
035600040119
035700040119     C*----------------------------------------------------*
035800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035900040119     C*----------------------------------------------------*
036000040510     C     WRIVAB        BEGSR
036100121119     C*
036200121119     C* EDIVABWR ha il prefisso "ok_"
036300080513     C*
036400080513     C* Gestisco l'eventuale rottura x numero spedizione
036500080527     C                   if        VABNSP <> depspe or *in65 = *on
036600070301     C                   if        wVATNOT_E <> *blanks
036700070301     C                   eval      VABCTM = '7Q'
036800070301     C                   endif
036900121119     C*
037000121119     C* VALORIZZO CAMPI RELATIVI AL "CMR"
037100121119     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
037200121119     C                   EVAL      ok_VABDCM = DATCOR
037300121119     C                   EVAL      ok_VABDTS = DATCOR
037400121119     C                   EVAL      ok_VABHMS = ORACOR
037500121119     C                   EVAL      ok_VABCNT = 1
037600121119     C*
037700121119     C                   write     EDIVAB00
037800080513     C                   endif
037900040119     C*
038000040119     C                   ENDSR
038100040119
038200040119
038300040119
038400040119     C*----------------------------------------------------*
038500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038600040119     C*----------------------------------------------------*
038700040510     C     WRIVAT        BEGSR
038800060512     C*
038900060512     C* Inizializzo i campi chiave
039000060512     C                   eval      VATFGS = VABFGS
039100060512     C                   eval      VATAAS = VABAAS
039200060512     C                   eval      VATCCM = VABCCM
039300070823     C                   eval      VATNRS = VABNRS
039400060512     C                   eval      VATNSP = VABNSP
039500060512     C                   eval      VATLNP = VABLNP
039600040119     C*
039700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039800040510     C                   if        VATTRC = *blanks
039900040510     C                   eval      VATTRC = 'E'
040000040510     C                   endif
040100110318     C                   if        VATNOT <> *blanks
040200121119     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
040300121119     C                   eval      VATCNT = 1
040400121119     C                   write     EDIVAT00
040500040119     C                   add       1             §CTROKVT
040600110318     C                   endif
040700060331     C*
040800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040900060331     C                   if        wVATNOT_A <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_A
041100060331     C                   eval      VATTRC = 'A'
041200121119     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
041300121119     C                   eval      VATCNT = 1
041400121119     C                   write     EDIVAT00
041500060331     C                   add       1             §CTROKVT
041600060331     C                   endif
041700060331     C*
041800060331     C                   if        wVATNOT_B <> *blanks
041900060331     C                   eval      VATNOT = wVATNOT_B
042000060331     C                   eval      VATTRC = 'B'
042100121119     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
042200121119     C                   eval      VATCNT = 1
042300121119     C                   write     EDIVAT00
042400060331     C                   add       1             §CTROKVT
042500060331     C                   endif
042600060331     C*
042700060331     C                   if        wVATNOT_E <> *blanks
042800110201     C                   eval      VATNOT = wVATNOT_E
042900060331     C                   eval      VATTRC = 'E'
043000121119     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
043100121119     C                   eval      VATCNT = 1
043200121119     C                   write     EDIVAT00
043300110201     C                   add       1             §CTROKVT
043400110201     C                   endif
043500040119     C*
043600040119     C                   ENDSR
043700040119
043800040119
043900040119
044000040119     C*----------------------------------------------------*
044100121119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
044200040119     C*----------------------------------------------------*
044300040119     C     PREVAT        BEGSR
044400040119     C*
044500121119     C* Compongo il nome del membro da dare al EDIVATWR
044600040119     C                   eval      parmbr = vlrhdl
044700040119     C                   movel     'M'           parmbr
044800040510     C                   eval      parccm = vlrksc
044900040119     C                   eval      paropz = '1'
045000040119     C* Effettuo la chiamata al CLLE preposto
045100121119     C                   call(e)   'TITVEVTC'
045200040119     C                   parm                    parccm
045300040119     C                   parm                    parmbr
045400040119     C                   parm                    paropz
045500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045600040119     C                   if        %error
045700040119     C                   movel     '1'           chkcall
045800040119     C                   else
045900040119     C                   movel     '0'           chkcall
046000040119     C                   endif
046100040119     C*
046200040119     C                   ENDSR
046300010601
046400010601
046500010601
046600010330     C*----------------------------------------------------*
046700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046800010330     C*----------------------------------------------------*
046900040119     C     INZVAR        BEGSR
047000080527     C*
047100080527     C                   setoff                                       65
047200010330     C*
047300030822     C* Inizializzo variabili di wrk
047400020204     C                   Z-ADD     *zeros        Num5_0            5 0
047500101018     C                   MOVEL     *blanks       wFlgCAS
047600060331     C                   MOVEL     *blanks       wVATNOT_A
047700060331     C                   MOVEL     *blanks       wVATNOT_B
047800060331     C                   MOVEL     *blanks       wVATNOT_E
047900110708     C                   MOVEL     *blanks       wVABRSD_1
048000110708     C                   MOVEL     *blanks       wVABRSD_2
048100110708     C                   MOVEL     *blanks       wVABRSD_3
048200110708     C                   Z-ADD     *zeros        wVABDCR
048300110708     C                   MOVEL     *blanks       wFLGDCR
048400030822     C*
048500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048600030822     C                   CLEAR                   SkSplitCSV
048700030822     C                   EVAL      i = 1
048800030822     C                   EVAL      posDa = *zeros
048900030822     C                   EVAL      posA  = *zeros
049000080527     C*
049100080527     C                   CLEAR                   FIVABDS
049200121119     C                   CLEAR                   EDIVAT00
049300030822     C*
049400030822     C* Reimposto i valori di default
049500030822     C                   EXSR      DEFCAM
049600030822     C*
049700010330     C                   ENDSR
049800010330     C*----------------------------------------------------*
049900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050000010330     C*----------------------------------------------------*
050100010330     C     DEFCAM        BEGSR
050200030822     C*
050300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050400030715     C* e delimitatore testo.
050500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050800100318     C*
050900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051000100318     C                   EVAL      CharSOS = CharNUM
051100030822     C*
051200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051300100720     C                   SETOFF                                       505152
051400100720     C                   SETOFF                                       53
051500100413     C                   SELECT
051600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051700100720     C                   SETON                                        50
051800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051900100413     C                   SETON                                        51
052000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052100100720     C                   SETON                                        52
052200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052300100720     C                   SETON                                        53
052400100413     C                   ENDSL
052500030822     C*
052600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052700030822     C                   EVAL      posDaDft = 1
052800030822     C                   EVAL      posADft  = 0
052900030822     C                   EVAL      wGiroDft = 0
053000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053100030822     C                             posDaDft > 0
053200030822     C*
053300030822     C* Gestisco il 1° giro
053400030822     C                   IF        wGiroDft = 0
053500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053700030822     C* Incremento il contatore dei "giri"
053800030822     C                   EVAL      wGiroDft = 1
053900030822     C                   ELSE
054000030822     C                   EVAL      posDaDft = posADft
054100030822     C                   ENDIF
054200030822     C* Eseguo lo scan x trovare la fine del campo corrente
054300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054400030822     C*
054500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054700030822     C                   IF        posDaDft > 0 AND
054800030822     C                             posADft  > 0
054900030822     C* NCL
055000030822     C                   IF        %subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):1:3)
055300030822     C                             = 'NCL'
055400030822     C                   EVAL      PiStr=%trim(%subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055700030822     C                   EXSR      CHKNUM
055800030822     C                   IF        PiInt=*on
055900030822     C                   Z-ADD     PiVal         VABNCL
056000030822     C                   ENDIF
056100030822     C                   ENDIF
056200030822     C* CCM
056300030822     C                   IF        %subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):1:3)
056600030822     C                             = 'CCM'
056700030822     C                   EVAL      PiStr=%trim(%subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057000030822     C                   EXSR      CHKNUM
057100030822     C                   IF        PiInt=*on
057200030822     C                   Z-ADD     PiVal         VABCCM
057300030822     C                   ENDIF
057400030822     C                   ENDIF
057500030822     C* LNP
057600030822     C                   IF        %subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):1:3)
057900030822     C                             = 'LNP'
058000030822     C                   EVAL      PiStr=%trim(%subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058300030822     C                   EXSR      CHKNUM
058400030822     C                   IF        PiInt=*on
058500030822     C                   Z-ADD     PiVal         VABLNP
058600030822     C                   ENDIF
058700030822     C                   ENDIF
058800030822     C* NRS
058900030822     C                   IF        %subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):1:3)
059200030822     C                             = 'NRS'
059300030822     C                   EVAL      PiStr=%trim(%subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059600030822     C                   EXSR      CHKNUM
059700030822     C                   IF        PiInt=*on
059800030822     C                   Z-ADD     PiVal         VABNRS
059900030822     C                   ENDIF
060000030822     C                   ENDIF
060100030822     C* CTR
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'CTR'
060600030822     C                   EVAL      PiStr=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   EXSR      CHKNUM
061000030822     C                   IF        PiInt=*on
061100030822     C                   Z-ADD     PiVal         VABCTR
061200030822     C                   ENDIF
061300030822     C                   ENDIF
061400030822     C* PKB
061500030822     C                   IF        %subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):1:3)
061800030822     C                             = 'PKB'
061900030822     C                   EVAL      PiStr=%trim(%subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062200030822     C                   EXSR      CHKNUM
062300030822     C                   IF        PiNum=*on
062400030822     C                   Z-ADD     PiVal         VABPKB
062500030822     C                   ENDIF
062600030822     C                   ENDIF
062700030822     C* VLB
062800030822     C                   IF        %subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):1:3)
063100030822     C                             = 'VLB'
063200030822     C                   EVAL      PiStr=%trim(%subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063500030822     C                   EXSR      CHKNUM
063600030822     C                   IF        PiNum=*on
063700030822     C                   Z-ADD     PiVal         VABVLB
063800030822     C                   ENDIF
063900030822     C                   ENDIF
064000030822     C* QFT
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'QFT'
064500030822     C                   EVAL      PiStr=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   EXSR      CHKNUM
064900030822     C                   IF        PiNum=*on
065000030822     C                   Z-ADD     PiVal         VABQFT
065100030822     C                   ENDIF
065200030822     C                   ENDIF
065300030822     C* CBO
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'CBO'
065800030822     C                   EVAL      VABCBO=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* TSP
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'TSP'
066700030822     C                   EVAL      VABTSP=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* VAS
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VAS'
067600030822     C                   EVAL      VABVAS=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* VCA
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VCA'
068500030822     C                   EVAL      VABVCA=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900070925     C* TIC
069000070925     C                   IF        %subst(
069100070925     C                             %subst(vlrppt:posDaDft+1:
069200070925     C                             posADft-posDaDft-1):1:3)
069300070925     C                             = 'TIC'
069400070925     C                   EVAL      VABTIC=%trim(%subst(
069500070925     C                             %subst(vlrppt:posDaDft+1:
069600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700070925     C                   ENDIF
069800030822     C* GCA
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'GCA'
070300030822     C                   EVAL      VABGCA=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* CTM
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'CTM'
071200030822     C                   EVAL      VABCTM=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* FFD
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'FFD'
072100030822     C                   EVAL      VABFFD=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* VAD
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'VAD'
073000030822     C                   EVAL      VABVAD=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* GMA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GMA'
073900030822     C                   EVAL      VABGMA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* GGA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'GGA'
074800030822     C                   EVAL      VABGGA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GVA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GVA'
075700030822     C                   EVAL      VABGVA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* TC1
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'TC1'
076600030822     C                   EVAL      VABTC1=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* TC2
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'TC2'
077500030822     C                   EVAL      VABTC2=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900040714     C* VATTRC
078000040714     C                   IF        %subst(
078100040714     C                             %subst(vlrppt:posDaDft+1:
078200040714     C                             posADft-posDaDft-1):1:3)
078300040714     C                             = 'TRC'
078400040714     C                   EVAL      VATTRC=%trim(%subst(
078500040714     C                             %subst(vlrppt:posDaDft+1:
078600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700040714     C                   ENDIF
078800030822     C* ...
078900030822     C                   ENDIF
079000030822     C                   ENDDO
079100020204     C*
079200010330     C                   ENDSR
079300010607     C*----------------------------------------------------*
079400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079500010607     C*----------------------------------------------------*
079600010607     C     IMPVAB        BEGSR
079700010607     C*
079800010607     C                   SETOFF                                       3132
079900110315 xxx C***                SETON                                        35
080000030515     C*
080100010607     C                   MOVEL     datcor        VABAAS
080200010607     C                   MOVE      datcor        VABMGS
080300010607     C*
080400030715     C**********
080500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080600030715     C**********
080700030715     C                   IF        wGiro = *zeros
080800040113     C*
080900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081200080429     C                             %trim(intcol(5))<>*blanks
081300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081500080429     C                                      %trim(intcol(5))
081600070823     C     ';':CharCSV   XLATE     vindta        vindta
081700040510     C                   ENDIF
081800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081900031201     C     minu:maiu     XLATE     vindta        vindta
082000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082100030715     C                   DOW       posDa <= %len(%trim(vindta))
082200030715     C*
082300030715     C* Gestisco il 1° campo
082400030715     C                   IF        i = 1
082500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082600030715     C                   EVAL      posDa = 1
082700030715     C* Eseguo lo scan x trovare la fine del primo campo
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C* X i campi successivi al 1°
083300030715     C                   ELSE
083400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083500030715     C                   EVAL      posDa = posA + 1
083600030715     C* Eseguo lo scan x trovare la fine del campo corrente
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* Gestisco l'ultimo campo
083900030715     C                   IF        posA = *zeros
084000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   LEAVE
084500030715     C                   ELSE
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   ENDIF
085000030715     C                   ENDIF
085100030715     C* Incremento il contatore d campo
085200030715     C                   EVAL      i = i +1
085300030715     C                   ENDDO
085400030715     C*
085500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085600030715     C* delimitazione testo
085700030715     C                   EVAL      i = 1
085800030715     C                   DOW       i < %elem(SkSplitFLD)
085900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086100030715     C                   EVAL      i = i + 1
086200030715     C                   ENDDO
086300030715     C                   EVAL      wGiro = 1
086400030715     C                   ELSE
086500030715     C                   EVAL      wGiro = 2
086600031204     C**********
086700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086800031204     C* d separatore campo
086900031204     C**********
087000031204     C                   EVAL      vindta = %trim(vindta)
087100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087200031204     C                   EVAL      vindta = %subst(vindta:2)
087300031204     C                   ENDDO
087400031204     C*
087500031204     C                   Z-ADD     *zeros        lunghInput        4 0
087600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088000031204     C                   ENDDO
088100030715     C**********
088200030715     C* Effettuo lo split del campo dati d input
088300030715     C**********
088400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088500030715     C                   DOW       posDa <= %len(%trim(vindta))
088600030715     C*
088700030715     C* Gestisco il 1° campo
088800030715     C                   IF        i = 1
088900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089000030715     C                   EVAL      posDa = 1
089100030715     C* Eseguo lo scan x trovare la fine del primo campo
089200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C* X i campi successivi al 1°
089700030715     C                   ELSE
089800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089900030715     C                   EVAL      posDa = posA + 1
090000031204     C* Verifico che nn vi sia il campo nn valorizzato
090100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090200031204     C* Se campo nn valorizzato skippo al prossimo
090300031204     C                   EVAL      SkSplitCSV(i) = *blanks
090400031204     C                   EVAL      posA  = posA + 1
090500031204     C                   ELSE
090600030715     C* Eseguo lo scan x trovare la fine del campo corrente
090700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090800030715     C* Gestisco l'ultimo campo
090900030715     C                   IF        posA = *zeros
091000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   LEAVE
091500030715     C                   ELSE
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C                   ENDIF
092000031204     C                   ENDIF
092100030715     C                   ENDIF
092200030715     C* Incremento il contatore d campo
092300030715     C                   EVAL      i = i +1
092400030715     C                   ENDDO
092500030715     C*
092600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092700030715     C* delimitazione testo
092800030715     C                   EVAL      i = 1
092900030715     C                   DOW       i < %elem(SkSplitCSV)
093000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093200030715     C                   EVAL      i = i + 1
093300030715     C                   ENDDO
093400100720     C*
093500100720     C* Stacco un progressivo x numero spedizione
093600110421 xxx C                   EXSR      repNSP
093700100326     C*
093800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093900030715     C                   EVAL      i = 1
094000030715     C                   DOW       i < %elem(SkSplitFLD)
094100030715     C*
094200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094400030715     C                             SkSplitCSV(i) <> *blanks
094500030721     C*** ATB
094600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094700030721     C                   EVAL      VABATB = SkSplitCSV(i)
094800030721     C                   ENDIF
094900030715     C*** RSD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095400030715     C* ==
095500030715     C                   ENDIF
095600030721     C*** RD2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030715     C*** IND
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096200030715     C                   EVAL      VABIND = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** LOD
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030715     C*** PRD
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030721     C*** NZD
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030715     C*** RMA
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** NOT
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** NT2
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** VCA
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030715     C*** VAS
099300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099500030715     C                   ENDIF
099600030715     C*** FFD
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030721     C*** GC1
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GC2
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** TSP
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101100030721     C                   ENDIF
101200040510     C*** CBO
101300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101500040510     C                   ENDIF
101600030721     C*** NAS
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TIC
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GCA
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** XCO
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** CTM
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** TCR
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** CTS
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** FTM
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** VAD
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GMA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** GGA
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** GVA
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** TC1
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** TC2
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** SCL
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** RMO
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** NMO
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*
108500030721     C* Reperisco quindi i campi numerici...
108600040510     C*** NSP
108700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108800040510     C                   EVAL      PiStr=SkSplitCSV(i)
108900040510     C                   EXSR      CHKNUM
109000040510     C                   IF        PiInt=*on
109100040510     C                   Z-ADD     PiVal         VABNSP
109200040510     C                   ELSE
109300040510     C                   SETON                                        31
109400040510     C                   EVAL      VABNSP = *zeros
109500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109600040510     C                             + ' ' + 'VABNSP'
109700040510     C                   ENDIF
109800040510     C                   ENDIF
109900071106     C*** RMN
110000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110100071106     C                   EVAL      PiStr=SkSplitCSV(i)
110200071106     C                   EXSR      CHKNUM
110300071106     C                   IF        PiInt=*on
110400071106     C                   Z-ADD     PiVal         VABRMN
110500071106     C                   ELSE
110600071106     C                   SETON                                        32
110700071106     C                   EVAL      VABRMN = *zeros
110800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110900071106     C                             + ' ' + 'VABRMN'
111000071106     C                   ENDIF
111100071106     C                   ENDIF
111200030721     C*** CAD
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         Num5_0
111800030721     C                   MOVEL(p)  Num5_0        VABCAD
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      VABCAD = *zeros
112200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112300030721     C                             + ' ' + 'VABCAD'
112400030721     C                   ENDIF
112500030721     C                   ENDIF
112600030721     C*** NCL
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100030721     C                   Z-ADD     PiVal         VABNCL
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABNCL = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040122     C                             + ' ' + 'VABNCL'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** PKB
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiNum=*on
114400030721     C                   Z-ADD     PiVal         VABPKB
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABPKB = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABPKB'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** CAS
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115500110506     C                             %trim(SkSplitCSV(i)) <> *zeros
115600101018     C                   EVAL      wFlgCAS = '1'
115700101018     C                   ENDIF
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiNum=*on
116100030721     C                   Z-ADD     PiVal         VABCAS
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCAS = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCAS'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** CCM
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on      AND
117400030721     C                             PiVal<=9999999 AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABCCM
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      VABCCM = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100040510     C                             + ' ' + 'VABCCM'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** LNP
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on    AND
118900030721     C                             PiVal<=999   AND
119000030721     C                             PiVal>*zeros
119100030721     C                   Z-ADD     PiVal         VABLNP
119200030721     C                   ELSE
119300040715     C                   SETON                                        32
119400030721     C                   EVAL      VABLNP = *zeros
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600040510     C                             + ' ' + 'VABLNP'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** NRS
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on AND
120400030721     C                             PiVal<=99
120500030721     C                   Z-ADD     PiVal         VABNRS
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABNRS'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** LNA
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on    AND
121700030721     C                             PiVal<=999   AND
121800030721     C                             PiVal>*zeros
121900030721     C                   Z-ADD     PiVal         VABLNA
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      VABLNA = *zeros
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABLNA'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** CTR
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiInt=*on  AND
123200030721     C                             PiVal<=999
123300030721     C                   Z-ADD     PiVal         VABCTR
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABCTR'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** IAS
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiNum=*on
124500030721     C                   Z-ADD     PiVal         VABIAS
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABIAS'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** VLB
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABVLB
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABVLB'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** QFT
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABQFT
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABQFT'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NCD
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on      AND
128100030721     C                             PiVal<=9999999
128200030721     C                   Z-ADD     PiVal         VABNCD
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNCD'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** NCA
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on      AND
129400030721     C                             PiVal<=9999999
129500030721     C                   Z-ADD     PiVal         VABNCA
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABNCA'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** ZNC
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on AND
130700030721     C                             PiVal<=99
130800030721     C                   Z-ADD     PiVal         VABZNC
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABZNC'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** DCR
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on       AND
132000030721     C                             PiVal<=99999999
132100030721     C                   Z-ADD     PiVal         VABDCR
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABDCR'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** HCR
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiInt=*on   AND
133300030721     C                             PiVal<=9999
133400030721     C                   Z-ADD     PiVal         VABHCR
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABHCR'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** VMD
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiNum=*on
134600030721     C                   Z-ADD     PiVal         VABVMD
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABVMD'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** ANT
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on        AND
135800030721     C                             PiVal<=999999999
135900030721     C                   Z-ADD     PiVal         VABANT
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABANT'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600030721     C*** CMO
136700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136800030721     C                   EVAL      PiStr=SkSplitCSV(i)
136900030721     C                   EXSR      CHKNUM
137000030721     C                   IF        PiInt=*on
137100030721     C                   Z-ADD     PiVal         Num5_0
137200030721     C                   MOVEL(p)  Num5_0        VABCMO
137300030721     C                   ELSE
137400030721     C                   SETON                                        32
137500030721     C                   EVAL      VABCMO = *zeros
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABCMO'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000040510     C*
138100040510     C* Reperisco i campi del FIVAT
138200040510     C*** VATNOT
138300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138500040510     C                   ENDIF
138600040510     C*** VATTRC
138700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138900040510     C                   ENDIF
139000060331     C*** VATNOT_A
139100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139300060331     C                   ENDIF
139400060331     C*** VATNOT_B
139500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139700060331     C                   ENDIF
139800060331     C*** VATNOT_E
139900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140100060331     C                   ENDIF
140200070823     C*
140300100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140400110421     C*
140500110708     C*** RSD_1
140600110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_1'
140700110708     C                   EVAL      wVABRSD_1 = %trim(SkSplitCSV(i))
140800110415     C                   ENDIF
140900110708     C*** RSD_2
141000110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_2'
141100110708     C                   EVAL      wVABRSD_2 = %trim(SkSplitCSV(i))
141200110708     C                   ENDIF
141300110708     C*** RSD_2
141400110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD_3'
141500110708     C                   EVAL      wVABRSD_3 = %trim(SkSplitCSV(i))
141600110708     C                   ENDIF
141700110708     C*** NSP / RMN
141800110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
141900110708     C                   EVAL      PiStr=SkSplitCSV(i)
142000110708     C                   EXSR      CHKNUM
142100110708     C                   IF        PiInt=*on
142200110708     C                   Z-ADD     PiVal         VABNSP
142300110708     C                   Z-ADD     PiVal         VATNSP
142400110708     C                   Z-ADD     PiVal         VABRMN
142500110708     C                   ELSE
142600110708     C                   SETON                                        31
142700110708     C                   EVAL      VABNSP = *zeros
142800110708     C                   EVAL      VABRMN = *zeros
142900110708     C                   EVAL      vinmsg = %trimr(vinmsg)
143000110708     C                             + ' ' + 'VABNSP VATNSP VABRMN'
143100110708     C                   ENDIF
143200110708     C                   ENDIF
143300110708     C*** DCR_1
143400110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR_1'
143500110708     C                   EVAL      PiStr=%subst(SkSplitCSV(i):7:4)+
143600110708     C                                   %subst(SkSplitCSV(i):4:2)+
143700110708     C                                   %subst(SkSplitCSV(i):1:2)
143800110708     C                   EXSR      CHKNUM
143900110708     C                   IF        PiInt=*on
144000110708     C                   Z-ADD     PiVal         wVABDCR
144100110708     C                   ELSE
144200110708     C                   SETON                                        32
144300110708     C                   EVAL      vinmsg = %trimr(vinmsg)
144400110708     C                             + ' ' + 'VABDCR'
144500110708     C                   ENDIF
144600110708     C                   ENDIF
144700110708     C*** DCR_2
144800110708     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR_2'
144900110708     C                   IF        %trim(SkSplitCSV(i)) = 'FIX'
145000110708     C                   EVAL      wFLGDCR = 'S'
145100110708     C                   ENDIF
145200110708     C                   ENDIF
145300070925     C*
145400071029     C***  <===  -----------------------------------------
145500040510     C*
145600030715     C                   ENDIF
145700030715     C                   EVAL      i = i + 1
145800030715     C                   ENDDO
145900070925     C*
146000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146100110708     C*
146200110708     C                   EVAL      VABRSD = %trim(wVABRSD_1)+' '+
146300110708     C                                      %trim(wVABRSD_2)+' '+
146400110708     C                                      %trim(wVABRSD_3)
146500110708     C*
146600110708     C                   IF        wFLGDCR = 'S'
146700110708     C                   Z-ADD     wVABDCR       VABDCR
146800110708     C                   ENDIF
146900070925     C*
147000071029     C***  <===  -----------------------------------------
147100070925     C*
147200070925     C* Considerazioni finali su CBO/CAS
147300100318     C                   IF        wFlgCAS = '1'    OR
147400100318     C                             VABCAS  > *zeros
147500070925     C                   IF        VABCBO = '1'
147600070925     C                   EVAL      VABCBO = '4'
147700070925     C                   ENDIF
147800070925     C                   IF        VABCBO = '2'
147900070925     C                   EVAL      VABCBO = '6'
148000070925     C                   ENDIF
148100070925     C                   ENDIF
148200070925     C***  <===  ----------------------------
148300040510     C*
148400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148500040510     C                   IF        VABNSP = *zeros
148600040510     C                   SETON                                        31
148700040510     C                   ENDIF
148800040715     C*
148900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149000040715     C                   IF        VABLNP = *zeros
149100040715     C                   SETON                                        31
149200040715     C                   ENDIF
149300040510     C*
149400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149500040510     C                   IF        VABRMN = *zeros
149600040510     C                   Z-ADD     VABNSP        VABRMN
149700040510     C                   ENDIF
149800040510     C*
149900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150000040510     C                   IF        VABRMA = *blanks
150100040714     C                   MOVEL     VABRMN        VABRMA
150200040510     C                   ENDIF
150300040128     C*
150400040510     C* Se provincia nn valorizzata la reperisco
150500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150600040510     C                   IF        VABPRD = *blanks
150700040128     C                   CLEAR                   TISI95DS
150800040128     C                   EVAL      I95TCN = '3'
150900040128     C                   Z-ADD     datcor        I95DAT
151000040128     C                   EVAL      I95NAR = VABNZD
151100040128     C                   EVAL      I95CAP = VABCAD
151200040128     C                   EVAL      I95LOC = VABLOD
151300040128     C                   CALL      'TISI95R'
151400040128     C                   PARM                    TISI95DS
151500040128     C                   EVAL      VABPRD = O95PRV
151600040510     C                   ENDIF
151700020204     C*
151800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151900020204     C                   EXSR      CHKIMPDIV
152000030715     C                   ENDIF
152100020204     C*
152200010607     C                   ENDSR
152300010607     C*----------------------------------------------------*
152400010601
152500020204
152600020204     C*----------------------------------------------------*
152700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152800020204     C*----------------------------------------------------*
152900020204     C     CHKIMPDIV     BEGSR
153000020204     C*
153100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153200020204     C                   Z-ADD     *zeros        wrkDec            9 9
153300020204     C*
153400020204     C* Come prima cosa effettuo considerazioni sulla divisa
153500020204     C                   IF        vabIAS > *zeros
153600020204     C                   IF        vabVAS <> 'EUR'
153700020204     C                   EVAL      vabVAS =  'ITL'
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C                   IF        vabCAS > *zeros
154200020204     C                   IF        vabVCA <> 'EUR'
154300020204     C                   EVAL      vabVCA =  'ITL'
154400020204     C                   ENDIF
154500020204     C                   ENDIF
154600020204     C*
154700020204     C                   IF        vabVMD > *zeros
154800020204     C                   IF        vabVAD <> 'EUR'
154900020204     C                   EVAL      vabVAD =  'ITL'
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155400020204     C                   Z-ADD     vabIAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVAS = 'ITL'
155700020204     C                   EVAL      vabIAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156200020204     C                   Z-ADD     vabCAS        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVCA = 'ITL'
156500020204     C                   EVAL      vabCAS = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157000020204     C                   Z-ADD     vabVMD        wrkDec
157100020204     C                   IF        wrkDec > *zeros
157200020204     C                   IF        vabVAD = 'ITL'
157300020204     C                   EVAL      vabVMD = *zeros
157400020204     C                   ENDIF
157500020204     C                   ENDIF
157600020204     C*
157700020204     C                   ENDSR
157800020204     C***
157900010330
158000100318
158100100318
158200100318     C*----------------------------------------------------*
158300100318     C*  CONTROLLO NUMERICITA' CAMPI
158400100318     C*----------------------------------------------------*
158500100318     C     CHKNUM        BEGSR
158600100318     C*
158700100318     C                   IF        PiDecChr = *blanks
158800100318     C                   EVAL      PiDecChr = CharNUM
158900100318     C                   ENDIF
159000100318     C*
159100100318     C                   callp     UBISNUM_Check(PiStr
159200100318     C                                          :PiDecChr
159300100318     C                                          :PiVal
159400100318     C                                          :PiNum
159500100318     C                                          :PiInt)
159600100318     C*
159700100318     C                   ENDSR
159800100318     C***
159900010601
160000010601
160100010601
160200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160300010601     C     repfil        BEGSR
160400010601     C*
160500010601     C                   if        invfil = *zeros and
160600010601     C                             depfil > *zeros and
160700010629     C                             (vinflg = *blanks or
160800010629     C                              vinflg = *zeros)
160900010601     C
161000010601     C                   eval      invfil = depfil
161100010601     C                   endif
161200010601     C*
161300010601     C                   if        depfil <> invfil and
161400010601     C                             invfil > *zeros
161500010601     C                   eval      flgMulti = '1'
161600010601     C                   if        vinflg = *blanks
161700010601     C                   add       1             cntNonEl
161800010601     C                   endif
161900010601     C                   endif
162000010601     C*
162100010601     C                   if        vinflg = '2'
162200010601     C                   eval      flgStato = '2'
162300010601     C                   endif
162400010601     C*
162500010601     C                   ENDSR
162600010601     C***
162700010601
162800010601
162900010601
163000010330
163100040119      /TITLE Invio dei dati al punto operativo.
163200040119     C     invio         BEGSR
163300040119     C*
163400040119     C* 1° invio FIVAT
163500040119     C                   reset                   dscmz
163600040119     C                   move      vlrpoi        cmzdst
163700121119     C                   eval      cmzfld = 'EDIVATWR'
163800040119     C                   eval      cmzmbd = vlrhdl
163900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164000040119     C***                if        prmfir = *blanks
164100121119     C                   eval      cmzfla = 'EDIVAT0F'
164200121119     C                   eval      cmzmba = 'EDIVAT0F'
164300040119     C***                else
164400040119     C***                eval      cmzfla = prmfir
164500040119     C***                eval      cmzmba = prmfir
164600040119     C***                endif
164700040119     C                   eval      cmznrr = *zeros
164800040119     C                   move      §ctrokvt      cmznrr
164900040119     C                   eval      cmzlba = vlrfl1
165000040119     C                   call(e)   'TIS711C'
165100040119     C                   parm                    dscmz
165200040119     C                   parm      *blanks       esito
165300040119     C                   if        %error
165400040119     C                             or cmzerr = '1'
165500040119     C                             or esito  = '1'
165600040119     C                   eval      wrkesito = '3'
165700040119     C                   else
165800040119     C*
165900040119     C* 2° invio FIVAB
166000040119     C                   reset                   dscmz
166100040119     C                   move      vlrpoi        cmzdst
166200040119     C                   eval      cmzfld = vlrfou
166300040119     C                   eval      cmzmbd = vlrhdl
166400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166500040119     C***                if        prmfir = *blanks
166600121119     C                   eval      cmzfla = 'EDIVAB0F'
166700121119     C                   eval      cmzmba = 'EDIVAB0F'
166800040119     C***                else
166900040119     C***                eval      cmzfla = prmfir
167000040119     C***                eval      cmzmba = prmfir
167100040119     C***                endif
167200040119     C                   eval      cmznrr = *zeros
167300040119     C                   move      §ctrokvb      cmznrr
167400040119     C                   eval      cmzlba = vlrfl1
167500040119     C                   call(e)   'TIS711C'
167600040119     C                   parm                    dscmz
167700040119     C                   parm      *blanks       esito
167800040119     C                   if        %error
167900040119     C                             or cmzerr = '1'
168000040119     C                             or esito  = '1'
168100040119     C                   eval      wrkesito = '3'
168200040119     C                   endif
168300040119     C                   endif
168400040119     C*
168500040119     C                   ENDSR
168600040119     C***
168700010601
168800010601
168900010601
169000010601
169100010601      /TITLE Invio dei dati al punto operativo.
169200010601     C     opeini        BEGSR
169300010601     C*
169400010601     C* Inizializzo flag e contatori operativi
169500010601     C                   movel     '0'           flgGiro           1
169600010601     C                   movel     '0'           flgMulti          1
169700010601     C                   movel     '1'           flgStato          1
169800010615     C                   movel     '0'           flgOk             1
169900010601     C                   z-add     *zeros        cntNonEl         10 0
170000010601     C                   z-add     *zeros        depfil            3 0
170100010601     C                   z-add     *zeros        invfil            3 0
170200010601     C*
170300010601     C                   ENDSR
170400010601     C***
170500070326
170600070326
170700070326
170800070326
170900070326     C     *pssr         BEGSR
171000070329     C*
171100070329     C                   if        %open(tivin00r)
171200070329     C                   close     tivin00r
171300070329     C                   endif
171400121119     C                   if        %open(EDIVABwr)
171500121119     C                   close     EDIVABwr
171600070329     C                   endif
171700121119     C                   if        %open(EDIVATwr)
171800121119     C                   close     EDIVATwr
171900070329     C                   endif
172000070326     C*
172100070326     C* Effettuo la chiamata al CLLE preposto
172200121119     C                   call(e)   'TITVEVTC'
172300070326     C                   parm                    parccm
172400070326     C                   parm                    parmbr
172500070326     C                   parm      '2'           paropz
172600070326     C*
172700070326     C                   eval      wrkesito = '2'
172800070404     C*
172900070404     C                   seton                                        LR
173000070326     C*
173100070326     C                   ENDSR     '*CANCL'
173200070326     C***
173300100318
173400100318
173500100318
173600100318     C     normCSV       BEGSR
173700100318     C*
173800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173900100318     C* separatori campi
174000100318     C                   if        CharTXT <> *blanks
174100100318     C                   z-add     *zeros        wLenVINDTA        4 0
174200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174300100318     C                   setoff                                       80
174400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174700100318     C                   if        not *in80
174800100318     C                   seton                                        80
174900100318     C                   else
175000100318     C                   setoff                                       80
175100100318     C                   endif
175200100318     C                   endif
175300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175400100318     C                             and *in80 = *on
175500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175600100318     C                   endif
175700100318     C                   enddo
175800100318     C                   endif
175900100318     C*
176000100318     C                   ENDSR
176100100318     C***
176200070326
176300100326
176400100326
176500100326     C*----------------------------------------------------*
176600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176700100326     C*----------------------------------------------------*
176800100326     C     REPNSP        BEGSR
176900100326     C*
177000100326     C* NSP => Stacco un numeratore da AZNUM
177100100326     C                   clear                   TRUL33DS
177200100326     C                   eval      I33OPE = *zeros
177300100326     C                   eval      I33CNU = 302
177400100326     C                   eval      I33NUM = 1
177500100326     C                   movel     TRUL33DS      KPJBU
177600100326     C                   call      'TRUL33R'
177700100326     C                   parm                    KPJBA
177800100326     C                   movel     KPJBU         TRUL33DS
177900100326     C                   if        O33ERR = *zeros
178000100326     C                   z-add     O33NRF        VABNSP
178100100326     C                   z-add     O33NRF        VATNSP
178200100326     C                   else
178300100326     C                   SETON                                        31
178400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178500100326     C                             + ' ' + 'VABNSP VATNSP'
178600100326     C                   endif
178700100326     C*
178800100326     C                   ENDSR
178900010330
179000100326
179100010330
179200000613     C     *inzsr        BEGSR
179300990910     C*
179400990910     C     *entry        plist
179500990920     C                   parm                    tivlrds
179600990921     C                   parm      wrkesito      esito
179700000724     C                   parm                    prmlit
179800000710     C                   parm                    prmfir
179900010330     C*
180000010330     C* CALCOLA LA DATA CORRENTE
180100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180200100318     C                   eval      datcor = %dec(%date() : *ISO)
180300121119     C                   time                    wn14             14 0
180400121119     C                   movel     wn14          oracor            6 0          *ORA
180500000613     C*
180600000613     C                   ENDSR
180700000613     C***
180800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180900110708FILXXX;FILXXX;FILXXX;W_VABRSD_1;W_VABRSD_2;W_VABRSD_3;FILXXX;VABCAD;
181000110708VABIND;VABLOD;VABPKB;W_VABRMN;VABRMA;FILXXX;VABVLB;FILXXX;FILXXX;
181100110708FILXXX;VABNOT;W_VABDCR_1;W_VABDCR_2;FILXXX;FILXXX;VATNOT_E;FILXXX;
181200110315
181300100318
