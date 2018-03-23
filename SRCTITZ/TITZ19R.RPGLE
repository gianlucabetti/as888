000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000600040119     FFIVATwwr  O    E             DISK    usropn
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
002000100413     D fivabds       e ds                  extname(fivab00f)
002100100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002200100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
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
006000030822     D*------------------
006100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006200030822     D*------------------
006300030822     D posDaDft        S              3  0 INZ(*zeros)
006400030822     D posADft         S              3  0 INZ(*zeros)
006500030822     D j               s              3  0 INZ(1)
006600030822     D wGiroDft        s              1  0 INZ(*zeros)
006700080418     D*------------------
006800080418     D* Costanti
006900080418     D*------------------
007000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007200100318
007300100318     D*------------------
007400100318     D* LINKING A DEFINIZIONI ESTERNE
007500100318     D*------------------
007600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800100318
007900010330
008000010330
008100010330
008200000913     C                   reset                   rrnum
008300990921     C                   reset                   esito
008400990921     C                   reset                   wrkesito
008500010601     C*
008600010601     C                   exsr      opeini
008700010605     C                   exsr      rwvab
008800010601     C*
008900010601     C                   seton                                        lr
009000010601
009100010601
009200010601
009300010601     C*--------------------------------------------------------
009400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009500010601     C*--------------------------------------------------------
009600010601     C     PREELA        BEGSR
009700010601     C*
009800010601     C* SE OCCORRE SPEDIRE IN FILIALE
009900010601     C                   if        invfil <> *zeros and
010000010601     C                             flgGiro = '0'
010100010601     C*
010200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010300010601     C                   eval      flgGiro = '1'
010400010601     C*
010500010601     C                   endif
010600010601     C*
010700010601     C                   ENDSR
010800010601     C***
010900010601
011000010601
011100010601
011200010601
011300010601     C*--------------------------------------------------------
011400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011500010601     C*--------------------------------------------------------
011600010601     C     ENDELA        BEGSR
011700040119     C*
011800040119     C* Effettuo la chiamata al CLLE preposto
011900040119     C                   call(e)   'TITVVTC'
012000040510     C                   parm                    parccm
012100040119     C                   parm                    parmbr
012200040119     C                   parm      '2'           paropz
012300050201     C*
012400050201     C* Effettuo lancio TISI95 solo x chiusura
012500050201     C                   CLEAR                   TISI95DS
012600050201     C                   EVAL      I95TLA = 'C'
012700050201     C                   CALL      'TISI95R'
012800050201     C                   PARM                    TISI95DS
012900000616     C*
013000010601     C                   ENDSR
013100010601
013200010601
013300010330     C*--------------------------------------------------------
013400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013500010330     C*--------------------------------------------------------
013600010605     C     RWVAB         BEGSR
013700010330     C*
013800010330     C                   if        not %open(tivin00r)
013900010330     C                   open      tivin00r
014000010330     C                   endif
014100021025     C                   if        not %open(fivabwwr)
014200021025     C                   open      fivabwwr
014300010330     C                   endif
014400040119     C*
014500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014600040119     C                   exsr      prevat
014700040119     C*
014800040119     C                   if        chkcall = '0'
014900040119     C*
015000040119     C                   if        not %open(fivatwwr)
015100040119     C                   open      fivatwwr
015200040119     C                   endif
015300010330     C*
015400040119     C                   clear                   §CTROKVB          7 0
015500040119     C                   clear                   §CTROKVT          7 0
015600040119     C                   clear                   §CTRMO            7 0
015700040119     C                   clear                   §CTRNO            7 0
015800070823     C*
015900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016100120227     C                   exsr      inzvar
016200120227     C                   exsr      impvab
016300010330     C*
016400030822     C                   do        *HIVAL
016500010330     C*
016600030822     C                   read      tivin00r                               70
016700010618     C*
016800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016900010618     C                   if        vinflg = '1'
017000010618     C                   eval      flgOk = '1'
017100010618     C                   endif
017200040510     C*
017300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017400040510     C                   z-add     *zeros        wLenVINDTA        4 0
017500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
017900040510     C                   z-add     1             wFlgVINDTA
018000040510     C                   leave
018100040510     C                   endif
018200040510     C                   enddo
018300010618     C*
018400040510     C                   if        vindta > *blanks AND
018500040510     C                             wFlgVINDTA = 1
018600010330     C                   add       1             rrnum
018700010330     C*
018800010601     C                   if        *in70 = *off and
018900010330     C                             (vinflg = *blanks
019000010330     C                              or vinflg = '0'
019100010330     C                              or vinflg = '2')
019200010330     C*
019300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019400010711     C                   if        vinflg = *blanks or vinflg = '0'
019500010711     C                   clear                   vinmsg
019600010711     C                   endif
019700010601     C*
019800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
019900070925     C                   eval      vindta  = ' ' + %trim(vindta)
020000070925     C*
020100080527     C                   exsr      inzvar
020200100318     C                   exsr      normCSV
020300010605     C                   exsr      impvab
020400010601     C*
020500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020600010605     C                   eval      depfil = VABLNP
020700010601     C                   exsr      repfil
020800010601     C                   if        depfil = invfil
020900021025     C                   if        vlrpoi = 999
021000030822     C                   move(P)   invfil        VABFGS
021100021025     C                   else
021200030822     C                   move(P)   vlrpoi        VABFGS
021300021025     C                   endif
021400010601     C*
021500010601     C                   exsr      PREELA
021600010601     C*
021700010604     C* Ebbene...
021800010604     C*
021900040119     C  N31              add       1             §CTROKVB
022000040119     C   32              add       1             §CTRMO
022100040119     C   31              add       1             §CTRNO
022200080418     C*
022300030822     C                   if        wGiro = 2
022400080513     C* Scrivo sempre il VAT
022500080513     C  N31              exsr      wrivat
022600110201     C                   exsr      chkvab
022700080513     C                   endif
022800010604     C*
022900010604     C                   if        *in31 = *off and
023000010604     C                             *in32 = *off
023100010604     C                   eval      vinflg = '1'
023200010604     C                   else
023300010604     C                   eval      vinflg = '2'
023400010604     C                   endif
023500010604     C                   endif
023600010601     C*
023700010604     C                   endif
023800010604     C*
023900010330     C                   else
024000010330     C                   eval      vinflg = '1'
024100010330     C                   endif
024200010601     C*
024300010601     C  N70              update    tivin000
024400010330     C*
024500030822     C  N70              enddo
024600040119     C*
024700080527     C* Verifico dati rimasti "in cannna"...
024800080527     C                   seton                                        65
024900110201     C                   exsr      chkvab
025000080527     C*
025100040119     C                   endif                                                  (endif - chkcall)
025200010601     C*
025300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025400010601     C                   if        cntNonEl = *zeros or
025500010601     C                             flgMulti = '0'
025600010330     C* Se non ci sono record con errori ...
025700010601     C                   if        §ctrno = 0 and
025800010604     C                             §ctrmo = 0 and
025900010601     C                             flgStato <> '2'
026000010330     C* ... restituisco esito OK.
026100010330     C                   eval      wrkesito = '0'
026200010330     C                   else
026300040119     C                   if        §ctrokvb > 0
026400010330     C                   eval      wrkesito = '1'
026500010330     C                   else
026600010615     C                   if        flgOk = '0'
026700010615     C                   eval      wrkesito = '2'
026800010615     C                   else
026900010615     C                   eval      wrkesito = '6'
027000010615     C                   endif
027100010330     C                   endif
027200010330     C                   endif
027300010601     C                   else
027400010601     C                   eval      wrkesito = '9'
027500010601     C                   endif
027600010330     C*
027700010330     C                   if        %open(tivin00r)
027800010330     C                   close     tivin00r
027900010330     C                   endif
028000021025     C                   if        %open(fivabwwr)
028100021025     C                   close     fivabwwr
028200010330     C                   endif
028300040512     C                   if        %open(fivatwwr)
028400040512     C                   close     fivatwwr
028500040512     C                   endif
028600010601     C*
028700010601     C                   if        vlrpoi <> 999
028800010601     C                   eval      invfil = vlrpoi
028900010601     C                   endif
029000010330     C*
029100040119     C                   if        §ctrokvb > 0
029200010601     C                             and invfil > *zeros
029300010330     C                   exsr      invio
029400010330     C                   endif
029500010601     C*
029600010618     C                   if        flgGiro = '1'
029700010601     C                   exsr      endela
029800010618     C                   endif
029900010330     C*
030000010330     C                   ENDSR
030100010330     C***
030200080527
030300080527
030400080527
030500080527     C*----------------------------------------------------*
030600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
030700080527     C*----------------------------------------------------*
030800100413     C     CHKVAB        BEGSR
030900110201     C*
031000110201     C* Se richiesti ignorare colli / peso / volume
031100110315 xxx C***                if        *in35
031200110315 xxx C***                z-add     *zeros        VABNCL
031300110315 xxx C***                z-add     *zeros        VABPKB
031400110315 xxx C***                z-add     *zeros        VABVLB
031500110315 xxx C***                endif
031600080527     C*
031700080527     C* Scrivo il VAB solo a rottura d spedizione
031800080527     C                   if        depspe = *zeros
031900080527     C  N31              eval      fivabds_sav = fivabds
032000080527     C                   eval      depspe = vabNSP
032100080527     C                   else
032200080527     C                   if        depspe = vabNSP AND *in65 = *off
032300100413     C   51              add       VABNCL        sav_VABNCL
032400100413     C   51              add       VABPKB        sav_VABPKB
032500100413     C   51              add       VABVLB        sav_VABVLB
032600100720     C   52              z-add     VABNCL        sav_VABNCL
032700100720     C   52              z-add     VABPKB        sav_VABPKB
032800100720     C   52              z-add     VABVLB        sav_VABVLB
032900100720     C   53              add       VABNCL        sav_VABNCL
033000100720     C   53              z-add     VABPKB        sav_VABPKB
033100100720     C   53              z-add     VABVLB        sav_VABVLB
033200080527     C                   else
033300080527     C  N31              eval      fivabds_ok = fivabds_sav
033400080527     C  N31              exsr      wrivab
033500080527     C  N31              eval      fivabds_sav = fivabds
033600080527     C                   eval      depspe = vabNSP
033700080527     C                   endif
033800080527     C                   endif
033900080527     C*
034000080527     C                   ENDSR
034100040119
034200040119
034300040119
034400040119     C*----------------------------------------------------*
034500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034600040119     C*----------------------------------------------------*
034700040510     C     WRIVAB        BEGSR
034800080513     C*
034900080513     C* Gestisco l'eventuale rottura x numero spedizione
035000080527     C                   if        VABNSP <> depspe or *in65 = *on
035100070301     C                   if        wVATNOT_E <> *blanks
035200070301     C                   eval      VABCTM = '7Q'
035300070301     C                   endif
035400100413     C                   write     fivab000
035500080513     C                   endif
035600040119     C*
035700040119     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036300040119     C*----------------------------------------------------*
036400040510     C     WRIVAT        BEGSR
036500060512     C*
036600060512     C* Inizializzo i campi chiave
036700060512     C                   eval      VATFGS = VABFGS
036800060512     C                   eval      VATAAS = VABAAS
036900060512     C                   eval      VATCCM = VABCCM
037000070823     C                   eval      VATNRS = VABNRS
037100060512     C                   eval      VATNSP = VABNSP
037200060512     C                   eval      VATLNP = VABLNP
037300040119     C*
037400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037500040510     C                   if        VATTRC = *blanks
037600040510     C                   eval      VATTRC = 'E'
037700040510     C                   endif
037800110318     C                   if        VATNOT <> *blanks
037900040510     C                   write     FIVAT000
038000040119     C                   add       1             §CTROKVT
038100110318     C                   endif
038200060331     C*
038300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038400060331     C                   if        wVATNOT_A <> *blanks
038500060331     C                   eval      VATNOT = wVATNOT_A
038600060331     C                   eval      VATTRC = 'A'
038700060331     C                   write     FIVAT000
038800060331     C                   add       1             §CTROKVT
038900060331     C                   endif
039000060331     C*
039100060331     C                   if        wVATNOT_B <> *blanks
039200060331     C                   eval      VATNOT = wVATNOT_B
039300060331     C                   eval      VATTRC = 'B'
039400060331     C                   write     FIVAT000
039500060331     C                   add       1             §CTROKVT
039600060331     C                   endif
039700060331     C*
039800120320     C*
039900120320     C* i rcd di FIVAT con tipo E sono più d'uno, tanti quanti sono i colli della spedizione
040000060331     C                   if        wVATNOT_E <> *blanks
040100110201     C                   eval      VATNOT = wVATNOT_E
040200060331     C                   eval      VATTRC = 'E'
040300120320     C*
040400120320     C* Sviluppati i "CHI SONO" in relazione al numero colli
040500120320     C*
040600120320     C     1             DO        VABNCL        wNumCollo         3 0
040700120320      *** da analisi del POC il progressivo colli è max 3 char e non contiene gli zeri non
040800120320      *** significativi ma neanche i blank a sx
040900120320     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
041000120320     C                                %trim(%char(wNumCollo))
041100120320     C                   write     FIVAT000
041200120320     C                   add       1             §CTROKVT
041300120320     C                   ENDDO
041400120320     C*
041500120320     C                   endif
041600120320     C*
041700110927     C*
041800110927     C                   if        wVATNOT_I <> *blanks
041900110927     C                   eval      VATNOT = wVATNOT_I
042000110927     C                   eval      VATTRC = 'I'
042100110927     C                   write     FIVAT000
042200110927     C                   add       1             §CTROKVT
042300110927     C                   endif
042400040119     C*
042500040119     C                   ENDSR
042600040119
042700040119
042800040119
042900040119     C*----------------------------------------------------*
043000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043100040119     C*----------------------------------------------------*
043200040119     C     PREVAT        BEGSR
043300040119     C*
043400040119     C* Compongo il nome del membro da dare al FIVATWWR
043500040119     C                   eval      parmbr = vlrhdl
043600040119     C                   movel     'M'           parmbr
043700040510     C                   eval      parccm = vlrksc
043800040119     C                   eval      paropz = '1'
043900040119     C* Effettuo la chiamata al CLLE preposto
044000040119     C                   call(e)   'TITVVTC'
044100040119     C                   parm                    parccm
044200040119     C                   parm                    parmbr
044300040119     C                   parm                    paropz
044400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044500040119     C                   if        %error
044600040119     C                   movel     '1'           chkcall
044700040119     C                   else
044800040119     C                   movel     '0'           chkcall
044900040119     C                   endif
045000040119     C*
045100040119     C                   ENDSR
045200010601
045300010601
045400010601
045500010330     C*----------------------------------------------------*
045600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045700010330     C*----------------------------------------------------*
045800040119     C     INZVAR        BEGSR
045900080527     C*
046000080527     C                   setoff                                       65
046100010330     C*
046200030822     C* Inizializzo variabili di wrk
046300020204     C                   Z-ADD     *zeros        Num5_0            5 0
046400101018     C                   MOVEL     *blanks       wFlgCAS
046500060331     C                   MOVEL     *blanks       wVATNOT_A
046600060331     C                   MOVEL     *blanks       wVATNOT_B
046700060331     C                   MOVEL     *blanks       wVATNOT_E
046800110927     C                   MOVEL     *blanks       wVATNOT_I
046900030822     C*
047000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047100030822     C                   CLEAR                   SkSplitCSV
047200030822     C                   EVAL      i = 1
047300030822     C                   EVAL      posDa = *zeros
047400030822     C                   EVAL      posA  = *zeros
047500080527     C*
047600080527     C                   CLEAR                   FIVABDS
047700080527     C                   CLEAR                   FIVAT000
047800030822     C*
047900030822     C* Reimposto i valori di default
048000030822     C                   EXSR      DEFCAM
048100030822     C*
048200010330     C                   ENDSR
048300010330     C*----------------------------------------------------*
048400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048500010330     C*----------------------------------------------------*
048600010330     C     DEFCAM        BEGSR
048700030822     C*
048800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048900030715     C* e delimitatore testo.
049000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049300100318     C*
049400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049500100318     C                   EVAL      CharSOS = CharNUM
049600030822     C*
049700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049800100720     C                   SETOFF                                       505152
049900100720     C                   SETOFF                                       53
050000100413     C                   SELECT
050100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050200100720     C                   SETON                                        50
050300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
050400100413     C                   SETON                                        51
050500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
050600100720     C                   SETON                                        52
050700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
050800100720     C                   SETON                                        53
050900100413     C                   ENDSL
051000030822     C*
051100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051200030822     C                   EVAL      posDaDft = 1
051300030822     C                   EVAL      posADft  = 0
051400030822     C                   EVAL      wGiroDft = 0
051500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051600030822     C                             posDaDft > 0
051700030822     C*
051800030822     C* Gestisco il 1° giro
051900030822     C                   IF        wGiroDft = 0
052000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052200030822     C* Incremento il contatore dei "giri"
052300030822     C                   EVAL      wGiroDft = 1
052400030822     C                   ELSE
052500030822     C                   EVAL      posDaDft = posADft
052600030822     C                   ENDIF
052700030822     C* Eseguo lo scan x trovare la fine del campo corrente
052800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052900030822     C*
053000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053200030822     C                   IF        posDaDft > 0 AND
053300030822     C                             posADft  > 0
053400030822     C* NCL
053500030822     C                   IF        %subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):1:3)
053800030822     C                             = 'NCL'
053900030822     C                   EVAL      PiStr=%trim(%subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054200030822     C                   EXSR      CHKNUM
054300030822     C                   IF        PiInt=*on
054400030822     C                   Z-ADD     PiVal         VABNCL
054500030822     C                   ENDIF
054600030822     C                   ENDIF
054700030822     C* CCM
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'CCM'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABCCM
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* LNP
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'LNP'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABLNP
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* NRS
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'NRS'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABNRS
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* CTR
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'CTR'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABCTR
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* PKB
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'PKB'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiNum=*on
060900030822     C                   Z-ADD     PiVal         VABPKB
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* VLB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'VLB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200030822     C                   Z-ADD     PiVal         VABVLB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* QFT
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'QFT'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABQFT
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* CBO
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'CBO'
064300030822     C                   EVAL      VABCBO=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   ENDIF
064700030822     C* TSP
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'TSP'
065200030822     C                   EVAL      VABTSP=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* VAS
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'VAS'
066100030822     C                   EVAL      VABVAS=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* VCA
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'VCA'
067000030822     C                   EVAL      VABVCA=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400070925     C* TIC
067500070925     C                   IF        %subst(
067600070925     C                             %subst(vlrppt:posDaDft+1:
067700070925     C                             posADft-posDaDft-1):1:3)
067800070925     C                             = 'TIC'
067900070925     C                   EVAL      VABTIC=%trim(%subst(
068000070925     C                             %subst(vlrppt:posDaDft+1:
068100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200070925     C                   ENDIF
068300030822     C* GCA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'GCA'
068800030822     C                   EVAL      VABGCA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* CTM
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'CTM'
069700030822     C                   EVAL      VABCTM=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* FFD
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'FFD'
070600030822     C                   EVAL      VABFFD=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* VAD
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'VAD'
071500030822     C                   EVAL      VABVAD=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* GMA
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'GMA'
072400030822     C                   EVAL      VABGMA=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* GGA
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'GGA'
073300030822     C                   EVAL      VABGGA=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GVA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GVA'
074200030822     C                   EVAL      VABGVA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* TC1
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'TC1'
075100030822     C                   EVAL      VABTC1=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* TC2
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'TC2'
076000030822     C                   EVAL      VABTC2=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400040714     C* VATTRC
076500040714     C                   IF        %subst(
076600040714     C                             %subst(vlrppt:posDaDft+1:
076700040714     C                             posADft-posDaDft-1):1:3)
076800040714     C                             = 'TRC'
076900040714     C                   EVAL      VATTRC=%trim(%subst(
077000040714     C                             %subst(vlrppt:posDaDft+1:
077100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200040714     C                   ENDIF
077300030822     C* ...
077400030822     C                   ENDIF
077500030822     C                   ENDDO
077600020204     C*
077700010330     C                   ENDSR
077800010607     C*----------------------------------------------------*
077900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078000010607     C*----------------------------------------------------*
078100010607     C     IMPVAB        BEGSR
078200010607     C*
078300010607     C                   SETOFF                                       3132
078400110315 xxx C***                SETON                                        35
078500030515     C*
078600010607     C                   MOVEL     datcor        VABAAS
078700010607     C                   MOVE      datcor        VABMGS
078800010607     C*
078900030715     C**********
079000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079100030715     C**********
079200030715     C                   IF        wGiro = *zeros
079300040113     C*
079400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079700080429     C                             %trim(intcol(5))<>*blanks
079800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080000080429     C                                      %trim(intcol(5))
080100070823     C     ';':CharCSV   XLATE     vindta        vindta
080200040510     C                   ENDIF
080300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080400031201     C     minu:maiu     XLATE     vindta        vindta
080500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080600030715     C                   DOW       posDa <= %len(%trim(vindta))
080700030715     C*
080800030715     C* Gestisco il 1° campo
080900030715     C                   IF        i = 1
081000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081100030715     C                   EVAL      posDa = 1
081200030715     C* Eseguo lo scan x trovare la fine del primo campo
081300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081400030715     C* A questo punto "estraggo" il campo corrente
081500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081600030715     C                                                    (posA-posDa))
081700030715     C* X i campi successivi al 1°
081800030715     C                   ELSE
081900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082000030715     C                   EVAL      posDa = posA + 1
082100030715     C* Eseguo lo scan x trovare la fine del campo corrente
082200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300030715     C* Gestisco l'ultimo campo
082400030715     C                   IF        posA = *zeros
082500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C                   LEAVE
083000030715     C                   ELSE
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C                   ENDIF
083500030715     C                   ENDIF
083600030715     C* Incremento il contatore d campo
083700030715     C                   EVAL      i = i +1
083800030715     C                   ENDDO
083900030715     C*
084000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084100030715     C* delimitazione testo
084200030715     C                   EVAL      i = 1
084300030715     C                   DOW       i < %elem(SkSplitFLD)
084400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084600030715     C                   EVAL      i = i + 1
084700030715     C                   ENDDO
084800030715     C                   EVAL      wGiro = 1
084900030715     C                   ELSE
085000030715     C                   EVAL      wGiro = 2
085100031204     C**********
085200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085300031204     C* d separatore campo
085400031204     C**********
085500031204     C                   EVAL      vindta = %trim(vindta)
085600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085700031204     C                   EVAL      vindta = %subst(vindta:2)
085800031204     C                   ENDDO
085900031204     C*
086000031204     C                   Z-ADD     *zeros        lunghInput        4 0
086100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086500110912     C                   ENDDO
086600030715     C**********
086700030715     C* Effettuo lo split del campo dati d input
086800030715     C**********
086900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087000030715     C                   DOW       posDa <= %len(%trim(vindta))
087100030715     C*
087200030715     C* Gestisco il 1° campo
087300030715     C                   IF        i = 1
087400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087500030715     C                   EVAL      posDa = 1
087600030715     C* Eseguo lo scan x trovare la fine del primo campo
087700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087800030715     C* A questo punto "estraggo" il campo corrente
087900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088000030715     C                                                    (posA-posDa))
088100030715     C* X i campi successivi al 1°
088200030715     C                   ELSE
088300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088400030715     C                   EVAL      posDa = posA + 1
088500031204     C* Verifico che nn vi sia il campo nn valorizzato
088600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088700031204     C* Se campo nn valorizzato skippo al prossimo
088800031204     C                   EVAL      SkSplitCSV(i) = *blanks
088900031204     C                   EVAL      posA  = posA + 1
089000031204     C                   ELSE
089100030715     C* Eseguo lo scan x trovare la fine del campo corrente
089200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089300030715     C* Gestisco l'ultimo campo
089400030715     C                   IF        posA = *zeros
089500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C                   LEAVE
090000030715     C                   ELSE
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C                   ENDIF
090500031204     C                   ENDIF
090600030715     C                   ENDIF
090700030715     C* Incremento il contatore d campo
090800030715     C                   EVAL      i = i +1
090900030715     C                   ENDDO
091000030715     C*
091100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091200030715     C* delimitazione testo
091300030715     C                   EVAL      i = 1
091400030715     C                   DOW       i < %elem(SkSplitCSV)
091500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091700030715     C                   EVAL      i = i + 1
091800030715     C                   ENDDO
091900100720     C*
092000100720     C* Stacco un progressivo x numero spedizione
092100120301 xxx C                   EXSR      repNSP
092200100326     C*
092300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092400030715     C                   EVAL      i = 1
092500030715     C                   DOW       i < %elem(SkSplitFLD)
092600030715     C*
092700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092900030715     C                             SkSplitCSV(i) <> *blanks
093000030721     C*** ATB
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093200030721     C                   EVAL      VABATB = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030715     C*** RSD
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093900030715     C* ==
094000030715     C                   ENDIF
094100030721     C*** RD2
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030715     C*** IND
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094700030715     C                   EVAL      VABIND = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** LOD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** PRD
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030721     C*** NZD
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030715     C*** RMA
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** NOT
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** NT2
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** VCA
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** VAS
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030715     C*** FFD
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030721     C*** GC1
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** GC2
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** TSP
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099600030721     C                   ENDIF
099700040510     C*** CBO
099800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100000040510     C                   ENDIF
100100030721     C*** NAS
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** TIC
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GCA
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** XCO
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** CTM
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** TCR
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** CTS
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** FTM
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** VAD
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** GMA
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** GGA
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** GVA
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TC1
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** TC2
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** SCL
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** RMO
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** NMO
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*
107000030721     C* Reperisco quindi i campi numerici...
107100040510     C*** NSP
107200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107300040510     C                   EVAL      PiStr=SkSplitCSV(i)
107400040510     C                   EXSR      CHKNUM
107500040510     C                   IF        PiInt=*on
107600040510     C                   Z-ADD     PiVal         VABNSP
107700040510     C                   ELSE
107800040510     C                   SETON                                        31
107900040510     C                   EVAL      VABNSP = *zeros
108000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108100040510     C                             + ' ' + 'VABNSP'
108200040510     C                   ENDIF
108300040510     C                   ENDIF
108400071106     C*** RMN
108500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108600071106     C                   EVAL      PiStr=SkSplitCSV(i)
108700071106     C                   EXSR      CHKNUM
108800071106     C                   IF        PiInt=*on
108900071106     C                   Z-ADD     PiVal         VABRMN
109000071106     C                   ELSE
109100071106     C                   SETON                                        32
109200071106     C                   EVAL      VABRMN = *zeros
109300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109400071106     C                             + ' ' + 'VABRMN'
109500071106     C                   ENDIF
109600071106     C                   ENDIF
109700030721     C*** CAD
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on
110200030721     C                   Z-ADD     PiVal         Num5_0
110300030721     C                   MOVEL(p)  Num5_0        VABCAD
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      VABCAD = *zeros
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800030721     C                             + ' ' + 'VABCAD'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** NCL
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiInt=*on
111600030721     C                   Z-ADD     PiVal         VABNCL
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABNCL = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100040122     C                             + ' ' + 'VABNCL'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** PKB
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiNum=*on
112900030721     C                   Z-ADD     PiVal         VABPKB
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABPKB = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400030721     C                             + ' ' + 'VABPKB'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** CAS
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114000111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
114100111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
114200120227 xxx C                   EVAL      VABvca = 'EUR'
114300101018     C                   EVAL      wFlgCAS = '1'
114400101018     C                   ENDIF
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiNum=*on
114800030721     C                   Z-ADD     PiVal         VABCAS
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      VABCAS = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABCAS'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** CCM
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on      AND
116100030721     C                             PiVal<=9999999 AND
116200030721     C                             PiVal>*zeros
116300030721     C                   Z-ADD     PiVal         VABCCM
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABCCM = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800040510     C                             + ' ' + 'VABCCM'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** LNP
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on    AND
117600030721     C                             PiVal<=999   AND
117700030721     C                             PiVal>*zeros
117800030721     C                   Z-ADD     PiVal         VABLNP
117900030721     C                   ELSE
118000040715     C                   SETON                                        32
118100030721     C                   EVAL      VABLNP = *zeros
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300040510     C                             + ' ' + 'VABLNP'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** NRS
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiInt=*on AND
119100030721     C                             PiVal<=99
119200030721     C                   Z-ADD     PiVal         VABNRS
119300030721     C                   ELSE
119400030721     C                   SETON                                        32
119500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119600030721     C                             + ' ' + 'VABNRS'
119700030721     C                   ENDIF
119800030721     C                   ENDIF
119900030721     C*** LNA
120000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120100030721     C                   EVAL      PiStr=SkSplitCSV(i)
120200030721     C                   EXSR      CHKNUM
120300030721     C                   IF        PiInt=*on    AND
120400030721     C                             PiVal<=999   AND
120500030721     C                             PiVal>*zeros
120600030721     C                   Z-ADD     PiVal         VABLNA
120700030721     C                   ELSE
120800030721     C                   SETON                                        32
120900030721     C                   EVAL      VABLNA = *zeros
121000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121100030721     C                             + ' ' + 'VABLNA'
121200030721     C                   ENDIF
121300030721     C                   ENDIF
121400030721     C*** CTR
121500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121600030721     C                   EVAL      PiStr=SkSplitCSV(i)
121700030721     C                   EXSR      CHKNUM
121800030721     C                   IF        PiInt=*on  AND
121900030721     C                             PiVal<=999
122000030721     C                   Z-ADD     PiVal         VABCTR
122100030721     C                   ELSE
122200030721     C                   SETON                                        32
122300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122400030721     C                             + ' ' + 'VABCTR'
122500030721     C                   ENDIF
122600030721     C                   ENDIF
122700030721     C*** IAS
122800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122900030721     C                   EVAL      PiStr=SkSplitCSV(i)
123000030721     C                   EXSR      CHKNUM
123100030721     C                   IF        PiNum=*on
123200030721     C                   Z-ADD     PiVal         VABIAS
123300030721     C                   ELSE
123400030721     C                   SETON                                        32
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABIAS'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** VLB
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiNum=*on
124400030721     C                   Z-ADD     PiVal         VABVLB
124500030721     C                   ELSE
124600030721     C                   SETON                                        32
124700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124800030721     C                             + ' ' + 'VABVLB'
124900030721     C                   ENDIF
125000030721     C                   ENDIF
125100030721     C*** QFT
125200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125300030721     C                   EVAL      PiStr=SkSplitCSV(i)
125400030721     C                   EXSR      CHKNUM
125500030721     C                   IF        PiNum=*on
125600030721     C                   Z-ADD     PiVal         VABQFT
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABQFT'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** NCD
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on      AND
126800030721     C                             PiVal<=9999999
126900030721     C                   Z-ADD     PiVal         VABNCD
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABNCD'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** NCA
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on      AND
128100030721     C                             PiVal<=9999999
128200030721     C                   Z-ADD     PiVal         VABNCA
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABNCA'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** ZNC
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiInt=*on AND
129400030721     C                             PiVal<=99
129500030721     C                   Z-ADD     PiVal         VABZNC
129600030721     C                   ELSE
129700030721     C                   SETON                                        32
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABZNC'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200030721     C*** DCR
130300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130400030721     C                   EVAL      PiStr=SkSplitCSV(i)
130500030721     C                   EXSR      CHKNUM
130600030721     C                   IF        PiInt=*on       AND
130700030721     C                             PiVal<=99999999
130800030721     C                   Z-ADD     PiVal         VABDCR
130900030721     C                   ELSE
131000030721     C                   SETON                                        32
131100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131200030721     C                             + ' ' + 'VABDCR'
131300030721     C                   ENDIF
131400030721     C                   ENDIF
131500030721     C*** HCR
131600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131700030721     C                   EVAL      PiStr=SkSplitCSV(i)
131800030721     C                   EXSR      CHKNUM
131900030721     C                   IF        PiInt=*on   AND
132000030721     C                             PiVal<=9999
132100030721     C                   Z-ADD     PiVal         VABHCR
132200030721     C                   ELSE
132300030721     C                   SETON                                        32
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABHCR'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800030721     C*** VMD
132900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133000030721     C                   EVAL      PiStr=SkSplitCSV(i)
133100030721     C                   EXSR      CHKNUM
133200030721     C                   IF        PiNum=*on
133300030721     C                   Z-ADD     PiVal         VABVMD
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABVMD'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** ANT
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on        AND
134500030721     C                             PiVal<=999999999
134600030721     C                   Z-ADD     PiVal         VABANT
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABANT'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** CMO
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiInt=*on
135800030721     C                   Z-ADD     PiVal         Num5_0
135900030721     C                   MOVEL(p)  Num5_0        VABCMO
136000030721     C                   ELSE
136100030721     C                   SETON                                        32
136200030721     C                   EVAL      VABCMO = *zeros
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABCMO'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700040510     C*
136800040510     C* Reperisco i campi del FIVAT
136900040510     C*** VATNOT
137000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137200040510     C                   ENDIF
137300040510     C*** VATTRC
137400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137600040510     C                   ENDIF
137700060331     C*** VATNOT_A
137800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138000060331     C                   ENDIF
138100060331     C*** VATNOT_B
138200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138400060331     C                   ENDIF
138500060331     C*** VATNOT_E
138600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900110927     C*** VATNOT_I
139000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
139100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
139200110927     C                   ENDIF
139300070823     C*
139400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139500120301
139600120320      *** W_VABRSD = VABRSD + VABRD2
139700120320     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
139800120320     C                   MOVEL     *blanks       wRSD             70
139900120320     C                   EVAL      wRSD=%trim(SkSplitCSV(i))
140000120320     C                   EVAL      VABRSD=%subst(wRSD:1:35)
140100120320     C                   EVAL      VABRD2=%subst(wRSD:36:35)
140200120301     C                   ENDIF
140300120320      *** W_VABNOT = VABNOT + VABNT2
140400120320     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
140500120320     C                   MOVEL     *blanks       wNOT             70
140600120320     C                   EVAL      wNOT=%trim(SkSplitCSV(i))
140700120320     C                   EVAL      VABNOT=%subst(wNOT:1:35)
140800120320     C                   EVAL      VABNT2=%subst(wNOT:36:35)
140900120320     C                   ENDIF
141000120320      *** VATNOT_E da VABCCM+VABNSP+num.collo
141100120320      *** in questa parte di pgm imposto solo VABCCM+VABNSP mentre sarà la routine WRIVAT a
141200120320      *** sviluppare il resto del barcode rispetto al nr. colli
141300120320      *** da analisi del POC il nr. spedizione su barcode è di 5 char
141400120320     C                   eval      wVATNOT_E = %editc(VABCCM:'X')  +
141500120320     C                                %subst(%editc(VABNSP:'X') : 3 : 5)
141600110421     C*
141700070925     C*
141800071029     C***  <===  -----------------------------------------
141900040510     C*
142000030715     C                   ENDIF
142100030715     C                   EVAL      i = i + 1
142200030715     C                   ENDDO
142300070925     C*
142400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142500100831     C*
142600070925     C*
142700071029     C***  <===  -----------------------------------------
142800070925     C*
142900070925     C* Considerazioni finali su CBO/CAS
143000100318     C                   IF        wFlgCAS = '1'    OR
143100100318     C                             VABCAS  > *zeros
143200070925     C                   IF        VABCBO = '1'
143300070925     C                   EVAL      VABCBO = '4'
143400070925     C                   ENDIF
143500070925     C                   IF        VABCBO = '2'
143600070925     C                   EVAL      VABCBO = '6'
143700070925     C                   ENDIF
143800070925     C                   ENDIF
143900070925     C***  <===  ----------------------------
144000040510     C*
144100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144200040510     C                   IF        VABNSP = *zeros
144300040510     C                   SETON                                        31
144400040510     C                   ENDIF
144500040715     C*
144600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144700040715     C                   IF        VABLNP = *zeros
144800040715     C                   SETON                                        31
144900040715     C                   ENDIF
145000040510     C*
145100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145200040510     C                   IF        VABRMN = *zeros
145300040510     C                   Z-ADD     VABNSP        VABRMN
145400040510     C                   ENDIF
145500040510     C*
145600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145700040510     C                   IF        VABRMA = *blanks
145800040714     C                   MOVEL     VABRMN        VABRMA
145900040510     C                   ENDIF
146000040128     C*
146100040510     C* Se provincia nn valorizzata la reperisco
146200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146300110912     C                   IF        VABPRD =  *blanks AND
146400110912     C                             VABNZD =  *blanks AND
146500110912     C                             VABCAD <> *blanks AND
146600110912     C                             VABLOD <> *blanks
146700040128     C                   CLEAR                   TISI95DS
146800040128     C                   EVAL      I95TCN = '3'
146900040128     C                   Z-ADD     datcor        I95DAT
147000040128     C                   EVAL      I95NAR = VABNZD
147100040128     C                   EVAL      I95CAP = VABCAD
147200040128     C                   EVAL      I95LOC = VABLOD
147300040128     C                   CALL      'TISI95R'
147400040128     C                   PARM                    TISI95DS
147500040128     C                   EVAL      VABPRD = O95PRV
147600040510     C                   ENDIF
147700020204     C*
147800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147900020204     C                   EXSR      CHKIMPDIV
148000030715     C                   ENDIF
148100020204     C*
148200010607     C                   ENDSR
148300010607     C*----------------------------------------------------*
148400010601
148500020204
148600020204     C*----------------------------------------------------*
148700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148800020204     C*----------------------------------------------------*
148900020204     C     CHKIMPDIV     BEGSR
149000020204     C*
149100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149200020204     C                   Z-ADD     *zeros        wrkDec            9 9
149300020204     C*
149400020204     C* Come prima cosa effettuo considerazioni sulla divisa
149500020204     C                   IF        vabIAS > *zeros
149600020204     C                   IF        vabVAS <> 'EUR'
149700020204     C                   EVAL      vabVAS =  'ITL'
149800020204     C                   ENDIF
149900020204     C                   ENDIF
150000020204     C*
150100020204     C                   IF        vabCAS > *zeros
150200020204     C                   IF        vabVCA <> 'EUR'
150300020204     C                   EVAL      vabVCA =  'ITL'
150400020204     C                   ENDIF
150500020204     C                   ENDIF
150600020204     C*
150700020204     C                   IF        vabVMD > *zeros
150800020204     C                   IF        vabVAD <> 'EUR'
150900020204     C                   EVAL      vabVAD =  'ITL'
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151400020204     C                   Z-ADD     vabIAS        wrkDec
151500020204     C                   IF        wrkDec > *zeros
151600020204     C                   IF        vabVAS = 'ITL'
151700020204     C                   EVAL      vabIAS = *zeros
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152200020204     C                   Z-ADD     vabCAS        wrkDec
152300020204     C                   IF        wrkDec > *zeros
152400020204     C                   IF        vabVCA = 'ITL'
152500020204     C                   EVAL      vabCAS = *zeros
152600020204     C                   ENDIF
152700020204     C                   ENDIF
152800020204     C*
152900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153000020204     C                   Z-ADD     vabVMD        wrkDec
153100020204     C                   IF        wrkDec > *zeros
153200020204     C                   IF        vabVAD = 'ITL'
153300020204     C                   EVAL      vabVMD = *zeros
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C                   ENDSR
153800020204     C***
153900010330
154000100318
154100100318
154200100318     C*----------------------------------------------------*
154300100318     C*  CONTROLLO NUMERICITA' CAMPI
154400100318     C*----------------------------------------------------*
154500100318     C     CHKNUM        BEGSR
154600100318     C*
154700100318     C                   IF        PiDecChr = *blanks
154800100318     C                   EVAL      PiDecChr = CharNUM
154900100318     C                   ENDIF
155000100318     C*
155100100318     C                   callp     UBISNUM_Check(PiStr
155200100318     C                                          :PiDecChr
155300100318     C                                          :PiVal
155400100318     C                                          :PiNum
155500100318     C                                          :PiInt)
155600100318     C*
155700100318     C                   ENDSR
155800100318     C***
155900010601
156000010601
156100010601
156200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156300010601     C     repfil        BEGSR
156400010601     C*
156500010601     C                   if        invfil = *zeros and
156600010601     C                             depfil > *zeros and
156700010629     C                             (vinflg = *blanks or
156800010629     C                              vinflg = *zeros)
156900010601     C
157000010601     C                   eval      invfil = depfil
157100010601     C                   endif
157200010601     C*
157300010601     C                   if        depfil <> invfil and
157400010601     C                             invfil > *zeros
157500010601     C                   eval      flgMulti = '1'
157600010601     C                   if        vinflg = *blanks
157700010601     C                   add       1             cntNonEl
157800010601     C                   endif
157900010601     C                   endif
158000010601     C*
158100010601     C                   if        vinflg = '2'
158200010601     C                   eval      flgStato = '2'
158300010601     C                   endif
158400010601     C*
158500010601     C                   ENDSR
158600010601     C***
158700010601
158800010601
158900010601
159000010330
159100040119      /TITLE Invio dei dati al punto operativo.
159200040119     C     invio         BEGSR
159300040119     C*
159400040119     C* 1° invio FIVAT
159500040119     C                   reset                   dscmz
159600040119     C                   move      vlrpoi        cmzdst
159700040119     C                   eval      cmzfld = 'FIVATWWR'
159800040119     C                   eval      cmzmbd = vlrhdl
159900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160000040119     C***                if        prmfir = *blanks
160100040119     C                   eval      cmzfla = 'FIVAT00F'
160200040119     C                   eval      cmzmba = 'FIVAT00F'
160300040119     C***                else
160400040119     C***                eval      cmzfla = prmfir
160500040119     C***                eval      cmzmba = prmfir
160600040119     C***                endif
160700040119     C                   eval      cmznrr = *zeros
160800040119     C                   move      §ctrokvt      cmznrr
160900040119     C                   eval      cmzlba = vlrfl1
161000040119     C                   call(e)   'TIS711C'
161100040119     C                   parm                    dscmz
161200040119     C                   parm      *blanks       esito
161300040119     C                   if        %error
161400040119     C                             or cmzerr = '1'
161500040119     C                             or esito  = '1'
161600040119     C                   eval      wrkesito = '3'
161700040119     C                   else
161800040119     C*
161900040119     C* 2° invio FIVAB
162000040119     C                   reset                   dscmz
162100040119     C                   move      vlrpoi        cmzdst
162200040119     C                   eval      cmzfld = vlrfou
162300040119     C                   eval      cmzmbd = vlrhdl
162400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162500040119     C***                if        prmfir = *blanks
162600040119     C                   eval      cmzfla = 'FIVAB00F'
162700040119     C                   eval      cmzmba = 'FIVAB00F'
162800040119     C***                else
162900040119     C***                eval      cmzfla = prmfir
163000040119     C***                eval      cmzmba = prmfir
163100040119     C***                endif
163200040119     C                   eval      cmznrr = *zeros
163300040119     C                   move      §ctrokvb      cmznrr
163400040119     C                   eval      cmzlba = vlrfl1
163500040119     C                   call(e)   'TIS711C'
163600040119     C                   parm                    dscmz
163700040119     C                   parm      *blanks       esito
163800040119     C                   if        %error
163900040119     C                             or cmzerr = '1'
164000040119     C                             or esito  = '1'
164100040119     C                   eval      wrkesito = '3'
164200040119     C                   endif
164300040119     C                   endif
164400040119     C*
164500040119     C                   ENDSR
164600040119     C***
164700010601
164800010601
164900010601
165000010601
165100010601      /TITLE Invio dei dati al punto operativo.
165200010601     C     opeini        BEGSR
165300010601     C*
165400010601     C* Inizializzo flag e contatori operativi
165500010601     C                   movel     '0'           flgGiro           1
165600010601     C                   movel     '0'           flgMulti          1
165700010601     C                   movel     '1'           flgStato          1
165800010615     C                   movel     '0'           flgOk             1
165900010601     C                   z-add     *zeros        cntNonEl         10 0
166000010601     C                   z-add     *zeros        depfil            3 0
166100010601     C                   z-add     *zeros        invfil            3 0
166200010601     C*
166300010601     C                   ENDSR
166400010601     C***
166500070326
166600070326
166700070326
166800070326
166900070326     C     *pssr         BEGSR
167000070329     C*
167100070329     C                   if        %open(tivin00r)
167200070329     C                   close     tivin00r
167300070329     C                   endif
167400070329     C                   if        %open(fivabwwr)
167500070329     C                   close     fivabwwr
167600070329     C                   endif
167700070329     C                   if        %open(fivatwwr)
167800070329     C                   close     fivatwwr
167900070329     C                   endif
168000070326     C*
168100070326     C* Effettuo la chiamata al CLLE preposto
168200070326     C                   call(e)   'TITVVTC'
168300070326     C                   parm                    parccm
168400070326     C                   parm                    parmbr
168500070326     C                   parm      '2'           paropz
168600070326     C*
168700070326     C                   eval      wrkesito = '2'
168800070404     C*
168900070404     C                   seton                                        LR
169000070326     C*
169100070326     C                   ENDSR     '*CANCL'
169200070326     C***
169300100318
169400100318
169500100318
169600100318     C     normCSV       BEGSR
169700100318     C*
169800100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
169900100318     C* separatori campi
170000100318     C                   if        CharTXT <> *blanks
170100100318     C                   z-add     *zeros        wLenVINDTA        4 0
170200100318     C                   z-add     *zeros        wFlgVINDTA        1 0
170300100318     C                   setoff                                       80
170400100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
170500100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
170600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
170700100318     C                   if        not *in80
170800100318     C                   seton                                        80
170900100318     C                   else
171000100318     C                   setoff                                       80
171100100318     C                   endif
171200100318     C                   endif
171300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
171400100318     C                             and *in80 = *on
171500100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
171600100318     C                   endif
171700100318     C                   enddo
171800100318     C                   endif
171900100318     C*
172000100318     C                   ENDSR
172100100318     C***
172200070326
172300100326
172400100326
172500100326     C*----------------------------------------------------*
172600100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
172700100326     C*----------------------------------------------------*
172800100326     C     REPNSP        BEGSR
172900100326     C*
173000100326     C* NSP => Stacco un numeratore da AZNUM
173100100326     C                   clear                   TRUL33DS
173200100326     C                   eval      I33OPE = *zeros
173300100326     C                   eval      I33CNU = 302
173400100326     C                   eval      I33NUM = 1
173500100326     C                   movel     TRUL33DS      KPJBU
173600100326     C                   call      'TRUL33R'
173700100326     C                   parm                    KPJBA
173800100326     C                   movel     KPJBU         TRUL33DS
173900100326     C                   if        O33ERR = *zeros
174000100326     C                   z-add     O33NRF        VABNSP
174100100326     C                   z-add     O33NRF        VATNSP
174200100326     C                   else
174300100326     C                   SETON                                        31
174400100326     C                   EVAL      vinmsg = %trimr(vinmsg)
174500100326     C                             + ' ' + 'VABNSP VATNSP'
174600100326     C                   endif
174700100326     C*
174800100326     C                   ENDSR
174900010330
175000100326
175100010330
175200000613     C     *inzsr        BEGSR
175300990910     C*
175400990910     C     *entry        plist
175500990920     C                   parm                    tivlrds
175600990921     C                   parm      wrkesito      esito
175700000724     C                   parm                    prmlit
175800000710     C                   parm                    prmfir
175900010330     C*
176000010330     C* CALCOLA LA DATA CORRENTE
176100100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
176200100318     C                   eval      datcor = %dec(%date() : *ISO)
176300000613     C*
176400000613     C                   ENDSR
176500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176600120320VABCCM;VABCTR;VABLNP;VABNSP;W_VABRSD;VATNOT_B;VABIND;VABCAD;VABLOD;VABPRD;
176700120320VABNZD;VABNCL;VABPKB;W_VABNOT;VABTIC;VABCAS;VABIAS;VABTSP;VABCBO;VABTC1
176800120227
176900111116
177000100318
