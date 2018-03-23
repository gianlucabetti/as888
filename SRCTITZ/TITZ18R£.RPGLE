000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130227      *
000300130227      * PARTICOLARITA':
000400130227      * cliente ha aggiunto un nuovo campo denominato VABTIC che conterrà il valore BM solo per le
000500130227      * spedizioni per le quali accetta la modalità incasso contrassegno.
000600130227      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800120209     F*
000900990910     Ftivin00r  uF   E             DISK    usropn
001000100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001100040119     FFIVATwwr  O    E             DISK    usropn
001200000313     D*
001300040113     D*------------
001400040113     D* SCHIEDE A PROGRAMMA
001500040113     D*------------
001600080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001700010330     D*----------------------------------------------------
001800010330     D* DICHIARAZIOINE VARIABILI DI WRK
001900010330     D*----------------------------------------------------
002000010330     D dscmz         e ds                  inz
002100010330     D psds           sds
002200010330     D  procname         *PROC
002300010330     D tivlrds       e ds                  extname(tivlr00f)
002400040128     D tisi95ds      e ds
002500100413     D fivabds       e ds                  extname(fivab00f)
002600100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002700100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002800010330     D esito           s              1
002900010330     D prmlit          s             10
003000010330     D prmfir          s             10
003100010330     D wrkesito        s                   like(esito)
003200010330     D rrnum           s              6  0 INZ(*zeros)
003300040510     D parccm          s              8    INZ(*blanks)
003400040119     D parmbr          s             10    INZ(*blanks)
003500040119     D paropz          s              1    INZ(*blanks)
003600040119     D chkcall         s              1    INZ(*blanks)
003700080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003800080418     D wFlgCAS         s              1    INZ(*blanks)
003900100326     D*------------------
004000100326     D* DS REPERIMENTO NUMERATORE
004100100326     D*------------------
004200100326     D trul33ds      e ds                  inz
004300100326     D*------------------
004400100326     D* DS ARCHITETTURA
004500100326     D*------------------
004600100326     D kpjba         e ds                  inz
004700100326
004800030715     D*------------------
004900030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005000030715     D*------------------
005100030715     D SkSplitFLD      S             10    DIM(100)
005200030715     D SkSplitCSV      S            256    DIM(100)
005300030715     D CharCSV         S              1
005400030715     D CharTXT         S              1
005500030715     D CharNUM         S              1
005600100318     D CharSOS         S              1
005700030715     D posDa           S              3  0 INZ(*zeros)
005800030715     D posA            S              3  0 INZ(*zeros)
005900030715     D i               s              3  0 INZ(1)
006000030715     D wGiro           s              1  0 INZ(*zeros)
006100070823     D wVATNOT_A       s                   LIKE(VATNOT)
006200060331     D wVATNOT_B       s                   LIKE(VATNOT)
006300060331     D wVATNOT_E       s                   LIKE(VATNOT)
006400110927     D wVATNOT_I       s                   LIKE(VATNOT)
006500030822     D*------------------
006600030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006700030822     D*------------------
006800030822     D posDaDft        S              3  0 INZ(*zeros)
006900030822     D posADft         S              3  0 INZ(*zeros)
007000030822     D j               s              3  0 INZ(1)
007100030822     D wGiroDft        s              1  0 INZ(*zeros)
007200080418     D*------------------
007300080418     D* Costanti
007400080418     D*------------------
007500080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007600080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300100318
008400010330
008500010330
008600010330
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300010601     C*
009400010601     C                   seton                                        lr
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400010601     C                   if        invfil <> *zeros and
010500010601     C                             flgGiro = '0'
010600010601     C*
010700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010800010601     C                   eval      flgGiro = '1'
010900010601     C*
011000010601     C                   endif
011100010601     C*
011200010601     C                   ENDSR
011300010601     C***
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200040119     C*
012300040119     C* Effettuo la chiamata al CLLE preposto
012400040119     C                   call(e)   'TITVVTC'
012500040510     C                   parm                    parccm
012600040119     C                   parm                    parmbr
012700040119     C                   parm      '2'           paropz
012800050201     C*
012900050201     C* Effettuo lancio TISI95 solo x chiusura
013000050201     C                   CLEAR                   TISI95DS
013100050201     C                   EVAL      I95TLA = 'C'
013200050201     C                   CALL      'TISI95R'
013300050201     C                   PARM                    TISI95DS
013400000616     C*
013500010601     C                   ENDSR
013600010601
013700010601
013800010330     C*--------------------------------------------------------
013900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014000010330     C*--------------------------------------------------------
014100010605     C     RWVAB         BEGSR
014200010330     C*
014300010330     C                   if        not %open(tivin00r)
014400010330     C                   open      tivin00r
014500010330     C                   endif
014600021025     C                   if        not %open(fivabwwr)
014700021025     C                   open      fivabwwr
014800010330     C                   endif
014900040119     C*
015000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015100040119     C                   exsr      prevat
015200040119     C*
015300040119     C                   if        chkcall = '0'
015400040119     C*
015500040119     C                   if        not %open(fivatwwr)
015600040119     C                   open      fivatwwr
015700040119     C                   endif
015800010330     C*
015900040119     C                   clear                   §CTROKVB          7 0
016000040119     C                   clear                   §CTROKVT          7 0
016100040119     C                   clear                   §CTRMO            7 0
016200040119     C                   clear                   §CTRNO            7 0
016300070823     C*
016400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016600120227     C                   exsr      inzvar
016700120227     C                   exsr      impvab
016800110131     C*
016900110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017000120301 xxx C                   read      tivin00r                               70
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
028800021025     C                   if        %open(fivabwwr)
028900021025     C                   close     fivabwwr
029000010330     C                   endif
029100040512     C                   if        %open(fivatwwr)
029200040512     C                   close     fivatwwr
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
032700080527     C  N31              eval      fivabds_sav = fivabds
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
034100080527     C  N31              eval      fivabds_ok = fivabds_sav
034200080527     C  N31              exsr      wrivab
034300080527     C  N31              eval      fivabds_sav = fivabds
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
035800080527     C                   if        VABNSP <> depspe or *in65 = *on
035900070301     C                   if        wVATNOT_E <> *blanks
036000070301     C                   eval      VABCTM = '7Q'
036100070301     C                   endif
036200100413     C                   write     fivab000
036300080513     C                   endif
036400040119     C*
036500040119     C                   ENDSR
036600040119
036700040119
036800040119
036900040119     C*----------------------------------------------------*
037000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037100040119     C*----------------------------------------------------*
037200040510     C     WRIVAT        BEGSR
037300060512     C*
037400060512     C* Inizializzo i campi chiave
037500060512     C                   eval      VATFGS = VABFGS
037600060512     C                   eval      VATAAS = VABAAS
037700060512     C                   eval      VATCCM = VABCCM
037800070823     C                   eval      VATNRS = VABNRS
037900060512     C                   eval      VATNSP = VABNSP
038000060512     C                   eval      VATLNP = VABLNP
038100040119     C*
038200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038300040510     C                   if        VATTRC = *blanks
038400040510     C                   eval      VATTRC = 'E'
038500040510     C                   endif
038600110318     C                   if        VATNOT <> *blanks
038700040510     C                   write     FIVAT000
038800040119     C                   add       1             §CTROKVT
038900110318     C                   endif
039000060331     C*
039100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039200060331     C                   if        wVATNOT_A <> *blanks
039300060331     C                   eval      VATNOT = wVATNOT_A
039400060331     C                   eval      VATTRC = 'A'
039500060331     C                   write     FIVAT000
039600060331     C                   add       1             §CTROKVT
039700060331     C                   endif
039800060331     C*
039900060331     C                   if        wVATNOT_B <> *blanks
040000060331     C                   eval      VATNOT = wVATNOT_B
040100060331     C                   eval      VATTRC = 'B'
040200060331     C                   write     FIVAT000
040300060331     C                   add       1             §CTROKVT
040400060331     C                   endif
040500060331     C*
040600060331     C                   if        wVATNOT_E <> *blanks
040700110201     C                   eval      VATNOT = wVATNOT_E
040800060331     C                   eval      VATTRC = 'E'
040900110201     C                   write     FIVAT000
041000110201     C                   add       1             §CTROKVT
041100110201     C                   endif
041200110927     C*
041300110927     C                   if        wVATNOT_I <> *blanks
041400110927     C                   eval      VATNOT = wVATNOT_I
041500110927     C                   eval      VATTRC = 'I'
041600110927     C                   write     FIVAT000
041700110927     C                   add       1             §CTROKVT
041800110927     C                   endif
041900040119     C*
042000040119     C                   ENDSR
042100040119
042200040119
042300040119
042400040119     C*----------------------------------------------------*
042500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042600040119     C*----------------------------------------------------*
042700040119     C     PREVAT        BEGSR
042800040119     C*
042900040119     C* Compongo il nome del membro da dare al FIVATWWR
043000040119     C                   eval      parmbr = vlrhdl
043100040119     C                   movel     'M'           parmbr
043200040510     C                   eval      parccm = vlrksc
043300040119     C                   eval      paropz = '1'
043400040119     C* Effettuo la chiamata al CLLE preposto
043500040119     C                   call(e)   'TITVVTC'
043600040119     C                   parm                    parccm
043700040119     C                   parm                    parmbr
043800040119     C                   parm                    paropz
043900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044000040119     C                   if        %error
044100040119     C                   movel     '1'           chkcall
044200040119     C                   else
044300040119     C                   movel     '0'           chkcall
044400040119     C                   endif
044500040119     C*
044600040119     C                   ENDSR
044700010601
044800010601
044900010601
045000010330     C*----------------------------------------------------*
045100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045200010330     C*----------------------------------------------------*
045300040119     C     INZVAR        BEGSR
045400080527     C*
045500080527     C                   setoff                                       65
045600010330     C*
045700030822     C* Inizializzo variabili di wrk
045800020204     C                   Z-ADD     *zeros        Num5_0            5 0
045900101018     C                   MOVEL     *blanks       wFlgCAS
046000060331     C                   MOVEL     *blanks       wVATNOT_A
046100060331     C                   MOVEL     *blanks       wVATNOT_B
046200060331     C                   MOVEL     *blanks       wVATNOT_E
046300110927     C                   MOVEL     *blanks       wVATNOT_I
046400030822     C*
046500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046600030822     C                   CLEAR                   SkSplitCSV
046700030822     C                   EVAL      i = 1
046800030822     C                   EVAL      posDa = *zeros
046900030822     C                   EVAL      posA  = *zeros
047000080527     C*
047100080527     C                   CLEAR                   FIVABDS
047200080527     C                   CLEAR                   FIVAT000
047300030822     C*
047400030822     C* Reimposto i valori di default
047500030822     C                   EXSR      DEFCAM
047600030822     C*
047700010330     C                   ENDSR
047800010330     C*----------------------------------------------------*
047900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048000010330     C*----------------------------------------------------*
048100010330     C     DEFCAM        BEGSR
048200030822     C*
048300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048400030715     C* e delimitatore testo.
048500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048800100318     C*
048900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049000100318     C                   EVAL      CharSOS = CharNUM
049100030822     C*
049200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049300100720     C                   SETOFF                                       505152
049400100720     C                   SETOFF                                       53
049500100413     C                   SELECT
049600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
049700100720     C                   SETON                                        50
049800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
049900100413     C                   SETON                                        51
050000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
050100100720     C                   SETON                                        52
050200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
050300100720     C                   SETON                                        53
050400100413     C                   ENDSL
050500030822     C*
050600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050700030822     C                   EVAL      posDaDft = 1
050800030822     C                   EVAL      posADft  = 0
050900030822     C                   EVAL      wGiroDft = 0
051000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051100030822     C                             posDaDft > 0
051200030822     C*
051300030822     C* Gestisco il 1° giro
051400030822     C                   IF        wGiroDft = 0
051500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051700030822     C* Incremento il contatore dei "giri"
051800030822     C                   EVAL      wGiroDft = 1
051900030822     C                   ELSE
052000030822     C                   EVAL      posDaDft = posADft
052100030822     C                   ENDIF
052200030822     C* Eseguo lo scan x trovare la fine del campo corrente
052300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052400030822     C*
052500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052700030822     C                   IF        posDaDft > 0 AND
052800030822     C                             posADft  > 0
052900030822     C* NCL
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'NCL'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiInt=*on
053900030822     C                   Z-ADD     PiVal         VABNCL
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* CCM
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'CCM'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiInt=*on
055200030822     C                   Z-ADD     PiVal         VABCCM
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* LNP
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'LNP'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiInt=*on
056500030822     C                   Z-ADD     PiVal         VABLNP
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* NRS
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'NRS'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABNRS
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* CTR
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'CTR'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABCTR
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* PKB
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'PKB'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiNum=*on
060400030822     C                   Z-ADD     PiVal         VABPKB
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* VLB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'VLB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABVLB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* QFT
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'QFT'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABQFT
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* CBO
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'CBO'
063800030822     C                   EVAL      VABCBO=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* TSP
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'TSP'
064700030822     C                   EVAL      VABTSP=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VAS
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VAS'
065600030822     C                   EVAL      VABVAS=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* VCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'VCA'
066500030822     C                   EVAL      VABVCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900070925     C* TIC
067000070925     C                   IF        %subst(
067100070925     C                             %subst(vlrppt:posDaDft+1:
067200070925     C                             posADft-posDaDft-1):1:3)
067300070925     C                             = 'TIC'
067400070925     C                   EVAL      VABTIC=%trim(%subst(
067500070925     C                             %subst(vlrppt:posDaDft+1:
067600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700070925     C                   ENDIF
067800030822     C* GCA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'GCA'
068300030822     C                   EVAL      VABGCA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* CTM
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'CTM'
069200030822     C                   EVAL      VABCTM=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* FFD
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'FFD'
070100030822     C                   EVAL      VABFFD=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* VAD
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'VAD'
071000030822     C                   EVAL      VABVAD=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GMA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GMA'
071900030822     C                   EVAL      VABGMA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* GGA
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'GGA'
072800030822     C                   EVAL      VABGGA=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* GVA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'GVA'
073700030822     C                   EVAL      VABGVA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* TC1
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'TC1'
074600030822     C                   EVAL      VABTC1=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* TC2
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'TC2'
075500030822     C                   EVAL      VABTC2=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900040714     C* VATTRC
076000040714     C                   IF        %subst(
076100040714     C                             %subst(vlrppt:posDaDft+1:
076200040714     C                             posADft-posDaDft-1):1:3)
076300040714     C                             = 'TRC'
076400040714     C                   EVAL      VATTRC=%trim(%subst(
076500040714     C                             %subst(vlrppt:posDaDft+1:
076600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700040714     C                   ENDIF
076800030822     C* ...
076900030822     C                   ENDIF
077000030822     C                   ENDDO
077100020204     C*
077200010330     C                   ENDSR
077300010607     C*----------------------------------------------------*
077400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077500010607     C*----------------------------------------------------*
077600010607     C     IMPVAB        BEGSR
077700010607     C*
077800010607     C                   SETOFF                                       3132
077900110315 xxx C***                SETON                                        35
078000030515     C*
078100010607     C                   MOVEL     datcor        VABAAS
078200010607     C                   MOVE      datcor        VABMGS
078300010607     C*
078400030715     C**********
078500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078600030715     C**********
078700030715     C                   IF        wGiro = *zeros
078800040113     C*
078900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079200080429     C                             %trim(intcol(5))<>*blanks
079300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
079500080429     C                                      %trim(intcol(5))
079600070823     C     ';':CharCSV   XLATE     vindta        vindta
079700040510     C                   ENDIF
079800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079900031201     C     minu:maiu     XLATE     vindta        vindta
080000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080100030715     C                   DOW       posDa <= %len(%trim(vindta))
080200030715     C*
080300030715     C* Gestisco il 1° campo
080400030715     C                   IF        i = 1
080500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080600030715     C                   EVAL      posDa = 1
080700030715     C* Eseguo lo scan x trovare la fine del primo campo
080800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080900030715     C* A questo punto "estraggo" il campo corrente
081000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081100030715     C                                                    (posA-posDa))
081200030715     C* X i campi successivi al 1°
081300030715     C                   ELSE
081400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081500030715     C                   EVAL      posDa = posA + 1
081600030715     C* Eseguo lo scan x trovare la fine del campo corrente
081700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081800030715     C* Gestisco l'ultimo campo
081900030715     C                   IF        posA = *zeros
082000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082100030715     C* A questo punto "estraggo" il campo corrente
082200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082300030715     C                                                    (posA-posDa))
082400030715     C                   LEAVE
082500030715     C                   ELSE
082600030715     C* A questo punto "estraggo" il campo corrente
082700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082800030715     C                                                    (posA-posDa))
082900030715     C                   ENDIF
083000030715     C                   ENDIF
083100030715     C* Incremento il contatore d campo
083200030715     C                   EVAL      i = i +1
083300030715     C                   ENDDO
083400030715     C*
083500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083600030715     C* delimitazione testo
083700030715     C                   EVAL      i = 1
083800030715     C                   DOW       i < %elem(SkSplitFLD)
083900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084100030715     C                   EVAL      i = i + 1
084200030715     C                   ENDDO
084300030715     C                   EVAL      wGiro = 1
084400030715     C                   ELSE
084500030715     C                   EVAL      wGiro = 2
084600031204     C**********
084700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084800031204     C* d separatore campo
084900031204     C**********
085000031204     C                   EVAL      vindta = %trim(vindta)
085100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085200031204     C                   EVAL      vindta = %subst(vindta:2)
085300031204     C                   ENDDO
085400031204     C*
085500031204     C                   Z-ADD     *zeros        lunghInput        4 0
085600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086000110912     C                   ENDDO
086100030715     C**********
086200030715     C* Effettuo lo split del campo dati d input
086300030715     C**********
086400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086500030715     C                   DOW       posDa <= %len(%trim(vindta))
086600030715     C*
086700030715     C* Gestisco il 1° campo
086800030715     C                   IF        i = 1
086900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087000030715     C                   EVAL      posDa = 1
087100030715     C* Eseguo lo scan x trovare la fine del primo campo
087200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087300030715     C* A questo punto "estraggo" il campo corrente
087400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087500030715     C                                                    (posA-posDa))
087600030715     C* X i campi successivi al 1°
087700030715     C                   ELSE
087800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087900030715     C                   EVAL      posDa = posA + 1
088000031204     C* Verifico che nn vi sia il campo nn valorizzato
088100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088200031204     C* Se campo nn valorizzato skippo al prossimo
088300031204     C                   EVAL      SkSplitCSV(i) = *blanks
088400031204     C                   EVAL      posA  = posA + 1
088500031204     C                   ELSE
088600030715     C* Eseguo lo scan x trovare la fine del campo corrente
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* Gestisco l'ultimo campo
088900030715     C                   IF        posA = *zeros
089000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C                   LEAVE
089500030715     C                   ELSE
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C                   ENDIF
090000031204     C                   ENDIF
090100030715     C                   ENDIF
090200030715     C* Incremento il contatore d campo
090300030715     C                   EVAL      i = i +1
090400030715     C                   ENDDO
090500030715     C*
090600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090700030715     C* delimitazione testo
090800030715     C                   EVAL      i = 1
090900030715     C                   DOW       i < %elem(SkSplitCSV)
091000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091200030715     C                   EVAL      i = i + 1
091300030715     C                   ENDDO
091400100720     C*
091500100720     C* Stacco un progressivo x numero spedizione
091600120301 xxx C                   EXSR      repNSP
091700100326     C*
091800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091900030715     C                   EVAL      i = 1
092000030715     C                   DOW       i < %elem(SkSplitFLD)
092100030715     C*
092200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092400030715     C                             SkSplitCSV(i) <> *blanks
092500030721     C*** ATB
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092700030721     C                   EVAL      VABATB = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030715     C*** RSD
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093400030715     C* ==
093500030715     C                   ENDIF
093600030721     C*** RD2
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030715     C*** IND
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094200030715     C                   EVAL      VABIND = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** LOD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030715     C*** PRD
094900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095100030715     C                   ENDIF
095200030721     C*** NZD
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030715     C*** RMA
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
095900030715     C                   ENDIF
096000030715     C*** NOT
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030715     C*** NT2
096500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096700030715     C                   ENDIF
096800030715     C*** VCA
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030715     C*** VAS
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** FFD
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030721     C*** GC1
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** GC2
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** TSP
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099100030721     C                   ENDIF
099200040510     C*** CBO
099300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099500040510     C                   ENDIF
099600030721     C*** NAS
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TIC
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** GCA
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** XCO
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** CTM
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** TCR
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** CTS
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** FTM
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** VAD
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** GMA
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** GGA
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** GVA
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** TC1
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** TC2
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** SCL
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** RMO
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** NMO
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*
106500030721     C* Reperisco quindi i campi numerici...
106600040510     C*** NSP
106700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106800040510     C                   EVAL      PiStr=SkSplitCSV(i)
106900040510     C                   EXSR      CHKNUM
107000040510     C                   IF        PiInt=*on
107100040510     C                   Z-ADD     PiVal         VABNSP
107200040510     C                   ELSE
107300040510     C                   SETON                                        31
107400040510     C                   EVAL      VABNSP = *zeros
107500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107600040510     C                             + ' ' + 'VABNSP'
107700040510     C                   ENDIF
107800040510     C                   ENDIF
107900071106     C*** RMN
108000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108100071106     C                   EVAL      PiStr=SkSplitCSV(i)
108200071106     C                   EXSR      CHKNUM
108300071106     C                   IF        PiInt=*on
108400071106     C                   Z-ADD     PiVal         VABRMN
108500071106     C                   ELSE
108600071106     C                   SETON                                        32
108700071106     C                   EVAL      VABRMN = *zeros
108800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
108900071106     C                             + ' ' + 'VABRMN'
109000071106     C                   ENDIF
109100071106     C                   ENDIF
109200030721     C*** CAD
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiInt=*on
109700030721     C                   Z-ADD     PiVal         Num5_0
109800030721     C                   MOVEL(p)  Num5_0        VABCAD
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      VABCAD = *zeros
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300030721     C                             + ' ' + 'VABCAD'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** NCL
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiInt=*on
111100030721     C                   Z-ADD     PiVal         VABNCL
111200030721     C                   ELSE
111300030721     C                   SETON                                        32
111400030721     C                   EVAL      VABNCL = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040122     C                             + ' ' + 'VABNCL'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** PKB
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiNum=*on
112400030721     C                   Z-ADD     PiVal         VABPKB
112500030721     C                   ELSE
112600030721     C                   SETON                                        32
112700030721     C                   EVAL      VABPKB = *zeros
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABPKB'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** CAS
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
113500111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
113600111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
113700120227 xxx C                   EVAL      VABvca = 'EUR'
113800130227     C***                EVAL      VABTIC = 'BM'
113900101018     C                   EVAL      wFlgCAS = '1'
114000101018     C                   ENDIF
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiNum=*on
114400030721     C                   Z-ADD     PiVal         VABCAS
114500030721     C                   ELSE
114600030721     C                   SETON                                        32
114700030721     C                   EVAL      VABCAS = *zeros
114800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114900030721     C                             + ' ' + 'VABCAS'
115000030721     C                   ENDIF
115100030721     C                   ENDIF
115200030721     C*** CCM
115300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115400030721     C                   EVAL      PiStr=SkSplitCSV(i)
115500030721     C                   EXSR      CHKNUM
115600030721     C                   IF        PiInt=*on      AND
115700030721     C                             PiVal<=9999999 AND
115800030721     C                             PiVal>*zeros
115900030721     C                   Z-ADD     PiVal         VABCCM
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABCCM = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400040510     C                             + ' ' + 'VABCCM'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** LNP
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on    AND
117200030721     C                             PiVal<=999   AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABLNP
117500030721     C                   ELSE
117600040715     C                   SETON                                        32
117700030721     C                   EVAL      VABLNP = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040510     C                             + ' ' + 'VABLNP'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** NRS
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on AND
118700030721     C                             PiVal<=99
118800030721     C                   Z-ADD     PiVal         VABNRS
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABNRS'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** LNA
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiInt=*on    AND
120000030721     C                             PiVal<=999   AND
120100030721     C                             PiVal>*zeros
120200030721     C                   Z-ADD     PiVal         VABLNA
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      VABLNA = *zeros
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABLNA'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** CTR
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on  AND
121500030721     C                             PiVal<=999
121600030721     C                   Z-ADD     PiVal         VABCTR
121700030721     C                   ELSE
121800030721     C                   SETON                                        32
121900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122000030721     C                             + ' ' + 'VABCTR'
122100030721     C                   ENDIF
122200030721     C                   ENDIF
122300030721     C*** IAS
122400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122500030721     C                   EVAL      PiStr=SkSplitCSV(i)
122600030721     C                   EXSR      CHKNUM
122700030721     C                   IF        PiNum=*on
122800030721     C                   Z-ADD     PiVal         VABIAS
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABIAS'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** VLB
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiNum=*on
124000030721     C                   Z-ADD     PiVal         VABVLB
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABVLB'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** QFT
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABQFT
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABQFT'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** NCD
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiInt=*on      AND
126400030721     C                             PiVal<=9999999
126500030721     C                   Z-ADD     PiVal         VABNCD
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABNCD'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** NCA
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiInt=*on      AND
127700030721     C                             PiVal<=9999999
127800030721     C                   Z-ADD     PiVal         VABNCA
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABNCA'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** ZNC
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiInt=*on AND
129000030721     C                             PiVal<=99
129100030721     C                   Z-ADD     PiVal         VABZNC
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABZNC'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** DCR
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiInt=*on       AND
130300030721     C                             PiVal<=99999999
130400030721     C                   Z-ADD     PiVal         VABDCR
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABDCR'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** HCR
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on   AND
131600030721     C                             PiVal<=9999
131700030721     C                   Z-ADD     PiVal         VABHCR
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABHCR'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** VMD
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiNum=*on
132900030721     C                   Z-ADD     PiVal         VABVMD
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABVMD'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** ANT
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on        AND
134100030721     C                             PiVal<=999999999
134200030721     C                   Z-ADD     PiVal         VABANT
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABANT'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** CMO
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on
135400030721     C                   Z-ADD     PiVal         Num5_0
135500030721     C                   MOVEL(p)  Num5_0        VABCMO
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      VABCMO = *zeros
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABCMO'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300040510     C*
136400040510     C* Reperisco i campi del FIVAT
136500040510     C*** VATNOT
136600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136800040510     C                   ENDIF
136900040510     C*** VATTRC
137000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137200040510     C                   ENDIF
137300060331     C*** VATNOT_A
137400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137600060331     C                   ENDIF
137700060331     C*** VATNOT_B
137800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138000060331     C                   ENDIF
138100060331     C*** VATNOT_E
138200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138400060331     C                   ENDIF
138500110927     C*** VATNOT_I
138600110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
138700110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
138800110927     C                   ENDIF
138900070823     C*
139000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139100120301
139200120301      *** W_TEL + W_CEL = VABNT2
139300120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_TEL'
139400120301     C                   EVAL      VABNT2 = %trim(SkSplitCSV(i))
139500120301     C                   ENDIF
139600120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
139700120301     C                   EVAL      VABNT2 = %trim(VABNT2) + ' ' +
139800120301     C                                      %trim(SkSplitCSV(i))
139900120301     C                   ENDIF
140000120301      *** W_CAS se = 'CONTRASSEGNO' ok x il c/assegno
140100120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CAS'
140200120301     C                             and %trim(SkSplitCSV(i)) <> 'Contrassegno'
140300120301     C                   clear                   VABCAS
140400120301     C                   clear                   VABVCA
140500130227     C***                clear                   VABTIC
140600120301     C                   EVAL      wFlgCAS = '0'
140700120301     C                   ENDIF
140800120301      *** VATNOT_E da VABRMN
140900120301     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
141000120316     C                   eval      VATTRC = 'E'
141100120309      *** CAD
141200120309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
141300120309     C                   eval      VABCAD = %subst(SkSplitCSV(i):2)
141400120309     C                   ENDIF
141500070925     C*
141600071029     C***  <===  -----------------------------------------
141700040510     C*
141800030715     C                   ENDIF
141900030715     C                   EVAL      i = i + 1
142000030715     C                   ENDDO
142100070925     C*
142200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142300100831     C*
142400120316     C                   IF        wVATNOT_E <> *blanks
142500120316     C                   EVAL      VABCTM = '7Q'
142600120316     C                   ENDIF
142700120316     C*
142800120316     C                   EVAL      wVATNOT_B =  VABNT2
142900070925     C*
143000071029     C***  <===  -----------------------------------------
143100070925     C*
143200070925     C* Considerazioni finali su CBO/CAS
143300100318     C                   IF        wFlgCAS = '1'    OR
143400100318     C                             VABCAS  > *zeros
143500070925     C                   IF        VABCBO = '1'
143600070925     C                   EVAL      VABCBO = '4'
143700070925     C                   ENDIF
143800070925     C                   IF        VABCBO = '2'
143900070925     C                   EVAL      VABCBO = '6'
144000070925     C                   ENDIF
144100070925     C                   ENDIF
144200070925     C***  <===  ----------------------------
144300040510     C*
144400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144500040510     C                   IF        VABNSP = *zeros
144600040510     C                   SETON                                        31
144700040510     C                   ENDIF
144800040715     C*
144900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145000040715     C                   IF        VABLNP = *zeros
145100040715     C                   SETON                                        31
145200040715     C                   ENDIF
145300040510     C*
145400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145500040510     C                   IF        VABRMN = *zeros
145600040510     C                   Z-ADD     VABNSP        VABRMN
145700040510     C                   ENDIF
145800040510     C*
145900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146000040510     C                   IF        VABRMA = *blanks
146100040714     C                   MOVEL     VABRMN        VABRMA
146200040510     C                   ENDIF
146300040128     C*
146400040510     C* Se provincia nn valorizzata la reperisco
146500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146600110912     C                   IF        VABPRD =  *blanks AND
146700110912     C                             VABNZD =  *blanks AND
146800110912     C                             VABCAD <> *blanks AND
146900110912     C                             VABLOD <> *blanks
147000040128     C                   CLEAR                   TISI95DS
147100040128     C                   EVAL      I95TCN = '3'
147200040128     C                   Z-ADD     datcor        I95DAT
147300040128     C                   EVAL      I95NAR = VABNZD
147400040128     C                   EVAL      I95CAP = VABCAD
147500040128     C                   EVAL      I95LOC = VABLOD
147600040128     C                   CALL      'TISI95R'
147700040128     C                   PARM                    TISI95DS
147800040128     C                   EVAL      VABPRD = O95PRV
147900040510     C                   ENDIF
148000020204     C*
148100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148200020204     C                   EXSR      CHKIMPDIV
148300030715     C                   ENDIF
148400020204     C*
148500010607     C                   ENDSR
148600010607     C*----------------------------------------------------*
148700010601
148800020204
148900020204     C*----------------------------------------------------*
149000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149100020204     C*----------------------------------------------------*
149200020204     C     CHKIMPDIV     BEGSR
149300020204     C*
149400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149500020204     C                   Z-ADD     *zeros        wrkDec            9 9
149600020204     C*
149700020204     C* Come prima cosa effettuo considerazioni sulla divisa
149800020204     C                   IF        vabIAS > *zeros
149900020204     C                   IF        vabVAS <> 'EUR'
150000020204     C                   EVAL      vabVAS =  'ITL'
150100020204     C                   ENDIF
150200020204     C                   ENDIF
150300020204     C*
150400020204     C                   IF        vabCAS > *zeros
150500020204     C                   IF        vabVCA <> 'EUR'
150600020204     C                   EVAL      vabVCA =  'ITL'
150700020204     C                   ENDIF
150800020204     C                   ENDIF
150900020204     C*
151000020204     C                   IF        vabVMD > *zeros
151100020204     C                   IF        vabVAD <> 'EUR'
151200020204     C                   EVAL      vabVAD =  'ITL'
151300020204     C                   ENDIF
151400020204     C                   ENDIF
151500020204     C*
151600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151700020204     C                   Z-ADD     vabIAS        wrkDec
151800020204     C                   IF        wrkDec > *zeros
151900020204     C                   IF        vabVAS = 'ITL'
152000020204     C                   EVAL      vabIAS = *zeros
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152500020204     C                   Z-ADD     vabCAS        wrkDec
152600020204     C                   IF        wrkDec > *zeros
152700020204     C                   IF        vabVCA = 'ITL'
152800020204     C                   EVAL      vabCAS = *zeros
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153300020204     C                   Z-ADD     vabVMD        wrkDec
153400020204     C                   IF        wrkDec > *zeros
153500020204     C                   IF        vabVAD = 'ITL'
153600020204     C                   EVAL      vabVMD = *zeros
153700020204     C                   ENDIF
153800020204     C                   ENDIF
153900020204     C*
154000020204     C                   ENDSR
154100020204     C***
154200010330
154300100318
154400100318
154500100318     C*----------------------------------------------------*
154600100318     C*  CONTROLLO NUMERICITA' CAMPI
154700100318     C*----------------------------------------------------*
154800100318     C     CHKNUM        BEGSR
154900100318     C*
155000100318     C                   IF        PiDecChr = *blanks
155100100318     C                   EVAL      PiDecChr = CharNUM
155200100318     C                   ENDIF
155300100318     C*
155400100318     C                   callp     UBISNUM_Check(PiStr
155500100318     C                                          :PiDecChr
155600100318     C                                          :PiVal
155700100318     C                                          :PiNum
155800100318     C                                          :PiInt)
155900100318     C*
156000100318     C                   ENDSR
156100100318     C***
156200010601
156300010601
156400010601
156500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156600010601     C     repfil        BEGSR
156700010601     C*
156800010601     C                   if        invfil = *zeros and
156900010601     C                             depfil > *zeros and
157000010629     C                             (vinflg = *blanks or
157100010629     C                              vinflg = *zeros)
157200010601     C
157300010601     C                   eval      invfil = depfil
157400010601     C                   endif
157500010601     C*
157600010601     C                   if        depfil <> invfil and
157700010601     C                             invfil > *zeros
157800010601     C                   eval      flgMulti = '1'
157900010601     C                   if        vinflg = *blanks
158000010601     C                   add       1             cntNonEl
158100010601     C                   endif
158200010601     C                   endif
158300010601     C*
158400010601     C                   if        vinflg = '2'
158500010601     C                   eval      flgStato = '2'
158600010601     C                   endif
158700010601     C*
158800010601     C                   ENDSR
158900010601     C***
159000010601
159100010601
159200010601
159300010330
159400040119      /TITLE Invio dei dati al punto operativo.
159500040119     C     invio         BEGSR
159600040119     C*
159700040119     C* 1° invio FIVAT
159800040119     C                   reset                   dscmz
159900040119     C                   move      vlrpoi        cmzdst
160000040119     C                   eval      cmzfld = 'FIVATWWR'
160100040119     C                   eval      cmzmbd = vlrhdl
160200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160300040119     C***                if        prmfir = *blanks
160400040119     C                   eval      cmzfla = 'FIVAT00F'
160500040119     C                   eval      cmzmba = 'FIVAT00F'
160600040119     C***                else
160700040119     C***                eval      cmzfla = prmfir
160800040119     C***                eval      cmzmba = prmfir
160900040119     C***                endif
161000040119     C                   eval      cmznrr = *zeros
161100040119     C                   move      §ctrokvt      cmznrr
161200040119     C                   eval      cmzlba = vlrfl1
161300040119     C                   call(e)   'TIS711C'
161400040119     C                   parm                    dscmz
161500040119     C                   parm      *blanks       esito
161600040119     C                   if        %error
161700040119     C                             or cmzerr = '1'
161800040119     C                             or esito  = '1'
161900040119     C                   eval      wrkesito = '3'
162000040119     C                   else
162100040119     C*
162200040119     C* 2° invio FIVAB
162300040119     C                   reset                   dscmz
162400040119     C                   move      vlrpoi        cmzdst
162500040119     C                   eval      cmzfld = vlrfou
162600040119     C                   eval      cmzmbd = vlrhdl
162700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162800040119     C***                if        prmfir = *blanks
162900040119     C                   eval      cmzfla = 'FIVAB00F'
163000040119     C                   eval      cmzmba = 'FIVAB00F'
163100040119     C***                else
163200040119     C***                eval      cmzfla = prmfir
163300040119     C***                eval      cmzmba = prmfir
163400040119     C***                endif
163500040119     C                   eval      cmznrr = *zeros
163600040119     C                   move      §ctrokvb      cmznrr
163700040119     C                   eval      cmzlba = vlrfl1
163800040119     C                   call(e)   'TIS711C'
163900040119     C                   parm                    dscmz
164000040119     C                   parm      *blanks       esito
164100040119     C                   if        %error
164200040119     C                             or cmzerr = '1'
164300040119     C                             or esito  = '1'
164400040119     C                   eval      wrkesito = '3'
164500040119     C                   endif
164600040119     C                   endif
164700040119     C*
164800040119     C                   ENDSR
164900040119     C***
165000010601
165100010601
165200010601
165300010601
165400010601      /TITLE Invio dei dati al punto operativo.
165500010601     C     opeini        BEGSR
165600010601     C*
165700010601     C* Inizializzo flag e contatori operativi
165800010601     C                   movel     '0'           flgGiro           1
165900010601     C                   movel     '0'           flgMulti          1
166000010601     C                   movel     '1'           flgStato          1
166100010615     C                   movel     '0'           flgOk             1
166200010601     C                   z-add     *zeros        cntNonEl         10 0
166300010601     C                   z-add     *zeros        depfil            3 0
166400010601     C                   z-add     *zeros        invfil            3 0
166500010601     C*
166600010601     C                   ENDSR
166700010601     C***
166800070326
166900070326
167000070326
167100070326
167200070326     C     *pssr         BEGSR
167300070329     C*
167400070329     C                   if        %open(tivin00r)
167500070329     C                   close     tivin00r
167600070329     C                   endif
167700070329     C                   if        %open(fivabwwr)
167800070329     C                   close     fivabwwr
167900070329     C                   endif
168000070329     C                   if        %open(fivatwwr)
168100070329     C                   close     fivatwwr
168200070329     C                   endif
168300070326     C*
168400070326     C* Effettuo la chiamata al CLLE preposto
168500070326     C                   call(e)   'TITVVTC'
168600070326     C                   parm                    parccm
168700070326     C                   parm                    parmbr
168800070326     C                   parm      '2'           paropz
168900070326     C*
169000070326     C                   eval      wrkesito = '2'
169100070404     C*
169200070404     C                   seton                                        LR
169300070326     C*
169400070326     C                   ENDSR     '*CANCL'
169500070326     C***
169600100318
169700100318
169800100318
169900100318     C     normCSV       BEGSR
170000100318     C*
170100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170200100318     C* separatori campi
170300100318     C                   if        CharTXT <> *blanks
170400100318     C                   z-add     *zeros        wLenVINDTA        4 0
170500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
170600100318     C                   setoff                                       80
170700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
170800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
170900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
171000100318     C                   if        not *in80
171100100318     C                   seton                                        80
171200100318     C                   else
171300100318     C                   setoff                                       80
171400100318     C                   endif
171500100318     C                   endif
171600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
171700100318     C                             and *in80 = *on
171800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
171900100318     C                   endif
172000100318     C                   enddo
172100100318     C                   endif
172200100318     C*
172300100318     C                   ENDSR
172400100318     C***
172500070326
172600100326
172700100326
172800100326     C*----------------------------------------------------*
172900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
173000100326     C*----------------------------------------------------*
173100100326     C     REPNSP        BEGSR
173200100326     C*
173300100326     C* NSP => Stacco un numeratore da AZNUM
173400100326     C                   clear                   TRUL33DS
173500100326     C                   eval      I33OPE = *zeros
173600100326     C                   eval      I33CNU = 302
173700100326     C                   eval      I33NUM = 1
173800100326     C                   movel     TRUL33DS      KPJBU
173900100326     C                   call      'TRUL33R'
174000100326     C                   parm                    KPJBA
174100100326     C                   movel     KPJBU         TRUL33DS
174200100326     C                   if        O33ERR = *zeros
174300100326     C                   z-add     O33NRF        VABNSP
174400100326     C                   z-add     O33NRF        VATNSP
174500100326     C                   else
174600100326     C                   SETON                                        31
174700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
174800100326     C                             + ' ' + 'VABNSP VATNSP'
174900100326     C                   endif
175000100326     C*
175100100326     C                   ENDSR
175200010330
175300100326
175400010330
175500000613     C     *inzsr        BEGSR
175600990910     C*
175700990910     C     *entry        plist
175800990920     C                   parm                    tivlrds
175900990921     C                   parm      wrkesito      esito
176000000724     C                   parm                    prmlit
176100000710     C                   parm                    prmfir
176200010330     C*
176300010330     C* CALCOLA LA DATA CORRENTE
176400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
176500100318     C                   eval      datcor = %dec(%date() : *ISO)
176600000613     C*
176700000613     C                   ENDSR
176800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
176900120309VABRMN;VABRMA;VABRD2;VABRSD;VABIND;VABLOD;W_VABCAD;VABPRD;W_TEL;W_CEL;
177000130227X;X;VABCAS;W_CAS;VABNOT;VABTIC
177100120227
177200111116
177300100318
