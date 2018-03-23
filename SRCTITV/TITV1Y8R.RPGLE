000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
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
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600080418     D*------------------
006700080418     D* Costanti
006800080418     D*------------------
006900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100100318
007200100318     D*------------------
007300100318     D* LINKING A DEFINIZIONI ESTERNE
007400100318     D*------------------
007500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700100318
007800010330
007900010330
008000010330
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010601     C*
008500010601     C                   exsr      opeini
008600010605     C                   exsr      rwvab
008700010601     C*
008800010601     C                   seton                                        lr
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010601     C*--------------------------------------------------------
009500010601     C     PREELA        BEGSR
009600010601     C*
009700010601     C* SE OCCORRE SPEDIRE IN FILIALE
009800010601     C                   if        invfil <> *zeros and
009900010601     C                             flgGiro = '0'
010000010601     C*
010100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010601     C                   eval      flgGiro = '1'
010300010601     C*
010400010601     C                   endif
010500010601     C*
010600010601     C                   ENDSR
010700010601     C***
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600040119     C*
011700040119     C* Effettuo la chiamata al CLLE preposto
011800040119     C                   call(e)   'TITVVTC'
011900040510     C                   parm                    parccm
012000040119     C                   parm                    parmbr
012100040119     C                   parm      '2'           paropz
012200050201     C*
012300050201     C* Effettuo lancio TISI95 solo x chiusura
012400050201     C                   CLEAR                   TISI95DS
012500050201     C                   EVAL      I95TLA = 'C'
012600050201     C                   CALL      'TISI95R'
012700050201     C                   PARM                    TISI95DS
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100000613
013200010601
013300010601
013400010330     C*--------------------------------------------------------
013500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013600010330     C*--------------------------------------------------------
013700010605     C     RWVAB         BEGSR
013800010330     C*
013900010330     C                   if        not %open(tivin00r)
014000010330     C                   open      tivin00r
014100010330     C                   endif
014200021025     C                   if        not %open(fivabwwr)
014300021025     C                   open      fivabwwr
014400010330     C                   endif
014500040119     C*
014600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014700040119     C                   exsr      prevat
014800040119     C*
014900040119     C                   if        chkcall = '0'
015000040119     C*
015100040119     C                   if        not %open(fivatwwr)
015200040119     C                   open      fivatwwr
015300040119     C                   endif
015400010330     C*
015500040119     C                   clear                   §CTROKVB          7 0
015600040119     C                   clear                   §CTROKVT          7 0
015700040119     C                   clear                   §CTRMO            7 0
015800040119     C                   clear                   §CTRNO            7 0
015900070823     C*
016000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016200120913     C                   exsr      inzvar
016300120913     C                   exsr      impvab
016400010330     C*
016500030822     C                   do        *HIVAL
016600010330     C*
016700030822     C                   read      tivin00r                               70
016800010618     C*
016900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017000010618     C                   if        vinflg = '1'
017100010618     C                   eval      flgOk = '1'
017200010618     C                   endif
017300040510     C*
017400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017500040510     C                   z-add     *zeros        wLenVINDTA        4 0
017600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
017800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
017900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018000040510     C                   z-add     1             wFlgVINDTA
018100040510     C                   leave
018200040510     C                   endif
018300040510     C                   enddo
018400010618     C*
018500040510     C                   if        vindta > *blanks AND
018600040510     C                             wFlgVINDTA = 1
018700010330     C                   add       1             rrnum
018800010330     C*
018900010601     C                   if        *in70 = *off and
019000010330     C                             (vinflg = *blanks
019100010330     C                              or vinflg = '0'
019200010330     C                              or vinflg = '2')
019300010330     C*
019400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019500010711     C                   if        vinflg = *blanks or vinflg = '0'
019600010711     C                   clear                   vinmsg
019700010711     C                   endif
019800010601     C*
019900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020000070925     C                   eval      vindta  = ' ' + %trim(vindta)
020100070925     C*
020200080527     C                   exsr      inzvar
020300100318     C                   exsr      normCSV
020400010605     C                   exsr      impvab
020500010601     C*
020600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020700010605     C                   eval      depfil = VABLNP
020800010601     C                   exsr      repfil
020900010601     C                   if        depfil = invfil
021000021025     C                   if        vlrpoi = 999
021100030822     C                   move(P)   invfil        VABFGS
021200021025     C                   else
021300030822     C                   move(P)   vlrpoi        VABFGS
021400021025     C                   endif
021500010601     C*
021600010601     C                   exsr      PREELA
021700010601     C*
021800010604     C* Ebbene...
021900010604     C*
022000040119     C  N31              add       1             §CTROKVB
022100040119     C   32              add       1             §CTRMO
022200040119     C   31              add       1             §CTRNO
022300080418     C*
022400030822     C                   if        wGiro = 2
022500080513     C* Scrivo sempre il VAT
022600080513     C  N31              exsr      wrivat
022700080527     C                   exsr      chkvab
022800080513     C                   endif
022900010604     C*
023000010604     C                   if        *in31 = *off and
023100010604     C                             *in32 = *off
023200010604     C                   eval      vinflg = '1'
023300010604     C                   else
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
024700040119     C*
024800080527     C* Verifico dati rimasti "in cannna"...
024900080527     C                   seton                                        65
025000080527     C                   exsr      chkvab
025100080527     C*
025200040119     C                   endif                                                  (endif - chkcall)
025300010601     C*
025400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025500010601     C                   if        cntNonEl = *zeros or
025600010601     C                             flgMulti = '0'
025700010330     C* Se non ci sono record con errori ...
025800010601     C                   if        §ctrno = 0 and
025900010604     C                             §ctrmo = 0 and
026000010601     C                             flgStato <> '2'
026100010330     C* ... restituisco esito OK.
026200010330     C                   eval      wrkesito = '0'
026300010330     C                   else
026400040119     C                   if        §ctrokvb > 0
026500010330     C                   eval      wrkesito = '1'
026600010330     C                   else
026700010615     C                   if        flgOk = '0'
026800010615     C                   eval      wrkesito = '2'
026900010615     C                   else
027000010615     C                   eval      wrkesito = '6'
027100010615     C                   endif
027200010330     C                   endif
027300010330     C                   endif
027400010601     C                   else
027500010601     C                   eval      wrkesito = '9'
027600010601     C                   endif
027700010330     C*
027800010330     C                   if        %open(tivin00r)
027900010330     C                   close     tivin00r
028000010330     C                   endif
028100021025     C                   if        %open(fivabwwr)
028200021025     C                   close     fivabwwr
028300010330     C                   endif
028400040512     C                   if        %open(fivatwwr)
028500040512     C                   close     fivatwwr
028600040512     C                   endif
028700010601     C*
028800010601     C                   if        vlrpoi <> 999
028900010601     C                   eval      invfil = vlrpoi
029000010601     C                   endif
029100010330     C*
029200040119     C                   if        §ctrokvb > 0
029300010601     C                             and invfil > *zeros
029400010330     C                   exsr      invio
029500010330     C                   endif
029600010601     C*
029700010618     C                   if        flgGiro = '1'
029800010601     C                   exsr      endela
029900010618     C                   endif
030000010330     C*
030100010330     C                   ENDSR
030200010330     C***
030300080527
030400080527
030500080527
030600080527     C*----------------------------------------------------*
030700080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
030800080527     C*----------------------------------------------------*
030900100413     C     CHKVAB        BEGSR
031000080527     C*
031100080527     C* Scrivo il VAB solo a rottura d spedizione
031200080527     C                   if        depspe = *zeros
031300080527     C  N31              eval      fivabds_sav = fivabds
031400080527     C                   eval      depspe = vabNSP
031500080527     C                   else
031600080527     C                   if        depspe = vabNSP AND *in65 = *off
031700100413     C   51              add       VABNCL        sav_VABNCL
031800100413     C   51              add       VABPKB        sav_VABPKB
031900100413     C   51              add       VABVLB        sav_VABVLB
032000100720     C   52              z-add     VABNCL        sav_VABNCL
032100100720     C   52              z-add     VABPKB        sav_VABPKB
032200100720     C   52              z-add     VABVLB        sav_VABVLB
032300100720     C   53              add       VABNCL        sav_VABNCL
032400100720     C   53              z-add     VABPKB        sav_VABPKB
032500100720     C   53              z-add     VABVLB        sav_VABVLB
032600080527     C                   else
032700080527     C  N31              eval      fivabds_ok = fivabds_sav
032800080527     C  N31              exsr      wrivab
032900080527     C  N31              eval      fivabds_sav = fivabds
033000080527     C                   eval      depspe = vabNSP
033100080527     C                   endif
033200080527     C                   endif
033300080527     C*
033400080527     C                   ENDSR
033500040119
033600040119
033700040119
033800040119     C*----------------------------------------------------*
033900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034000040119     C*----------------------------------------------------*
034100040510     C     WRIVAB        BEGSR
034200080513     C*
034300080513     C* Gestisco l'eventuale rottura x numero spedizione
034400080527     C                   if        VABNSP <> depspe or *in65 = *on
034500070301     C                   if        wVATNOT_E <> *blanks
034600070301     C                   eval      VABCTM = '7Q'
034700070301     C                   endif
034800100413     C                   write     fivab000
034900080513     C                   endif
035000040119     C*
035100040119     C                   ENDSR
035200040119
035300040119
035400040119
035500040119     C*----------------------------------------------------*
035600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
035700040119     C*----------------------------------------------------*
035800040510     C     WRIVAT        BEGSR
035900060512     C*
036000060512     C* Inizializzo i campi chiave
036100060512     C                   eval      VATFGS = VABFGS
036200060512     C                   eval      VATAAS = VABAAS
036300060512     C                   eval      VATCCM = VABCCM
036400070823     C                   eval      VATNRS = VABNRS
036500060512     C                   eval      VATNSP = VABNSP
036600060512     C                   eval      VATLNP = VABLNP
036700040714     C*
036800040714     C* Solo se VATNOT valorizzato scarico il buffer
036900040714     C                   if        VATNOT <> *blanks
037000040119     C*
037100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037200040510     C                   if        VATTRC = *blanks
037300040510     C                   eval      VATTRC = 'E'
037400040510     C                   endif
037500040510     C                   write     FIVAT000
037600040119     C                   add       1             §CTROKVT
037700040714     C*
037800040714     C                   endif
037900060331     C*
038000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038100060331     C                   if        wVATNOT_A <> *blanks
038200060331     C                   eval      VATNOT = wVATNOT_A
038300060331     C                   eval      VATTRC = 'A'
038400060331     C                   write     FIVAT000
038500060331     C                   add       1             §CTROKVT
038600060331     C                   endif
038700060331     C*
038800060331     C                   if        wVATNOT_B <> *blanks
038900060331     C                   eval      VATNOT = wVATNOT_B
039000060331     C                   eval      VATTRC = 'B'
039100060331     C                   write     FIVAT000
039200060331     C                   add       1             §CTROKVT
039300060331     C                   endif
039400060331     C*
039500060331     C                   if        wVATNOT_E <> *blanks
039600060331     C                   eval      VATNOT = wVATNOT_E
039700060331     C                   eval      VATTRC = 'E'
039800060331     C                   write     FIVAT000
039900060331     C                   add       1             §CTROKVT
040000060331     C                   endif
040100040119     C*
040200040119     C                   ENDSR
040300040119
040400040119
040500040119
040600040119     C*----------------------------------------------------*
040700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040800040119     C*----------------------------------------------------*
040900040119     C     PREVAT        BEGSR
041000040119     C*
041100040119     C* Compongo il nome del membro da dare al FIVATWWR
041200040119     C                   eval      parmbr = vlrhdl
041300040119     C                   movel     'M'           parmbr
041400040510     C                   eval      parccm = vlrksc
041500040119     C                   eval      paropz = '1'
041600040119     C* Effettuo la chiamata al CLLE preposto
041700040119     C                   call(e)   'TITVVTC'
041800040119     C                   parm                    parccm
041900040119     C                   parm                    parmbr
042000040119     C                   parm                    paropz
042100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042200040119     C                   if        %error
042300040119     C                   movel     '1'           chkcall
042400040119     C                   else
042500040119     C                   movel     '0'           chkcall
042600040119     C                   endif
042700040119     C*
042800040119     C                   ENDSR
042900010601
043000010601
043100010601
043200010330     C*----------------------------------------------------*
043300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043400010330     C*----------------------------------------------------*
043500040119     C     INZVAR        BEGSR
043600080527     C*
043700080527     C                   setoff                                       65
043800010330     C*
043900030822     C* Inizializzo variabili di wrk
044000020204     C                   Z-ADD     *zeros        Num5_0            5 0
044100060331     C                   MOVEL     *blanks       wVATNOT_A
044200060331     C                   MOVEL     *blanks       wVATNOT_B
044300060331     C                   MOVEL     *blanks       wVATNOT_E
044400030822     C*
044500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044600030822     C                   CLEAR                   SkSplitCSV
044700030822     C                   EVAL      i = 1
044800030822     C                   EVAL      posDa = *zeros
044900030822     C                   EVAL      posA  = *zeros
045000080527     C*
045100080527     C                   CLEAR                   FIVABDS
045200080527     C                   CLEAR                   FIVAT000
045300030822     C*
045400030822     C* Reimposto i valori di default
045500030822     C                   EXSR      DEFCAM
045600030822     C*
045700010330     C                   ENDSR
045800010330     C*----------------------------------------------------*
045900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046000010330     C*----------------------------------------------------*
046100010330     C     DEFCAM        BEGSR
046200030822     C*
046300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046400030715     C* e delimitatore testo.
046500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046800100318     C*
046900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
047000100318     C                   EVAL      CharSOS = CharNUM
047100030822     C*
047200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
047300100720     C                   SETOFF                                       505152
047400100720     C                   SETOFF                                       53
047500100413     C                   SELECT
047600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
047700100720     C                   SETON                                        50
047800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
047900100413     C                   SETON                                        51
048000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
048100100720     C                   SETON                                        52
048200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
048300100720     C                   SETON                                        53
048400100413     C                   ENDSL
048500030822     C*
048600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048700030822     C                   EVAL      posDaDft = 1
048800030822     C                   EVAL      posADft  = 0
048900030822     C                   EVAL      wGiroDft = 0
049000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049100030822     C                             posDaDft > 0
049200030822     C*
049300030822     C* Gestisco il 1° giro
049400030822     C                   IF        wGiroDft = 0
049500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049700030822     C* Incremento il contatore dei "giri"
049800030822     C                   EVAL      wGiroDft = 1
049900030822     C                   ELSE
050000030822     C                   EVAL      posDaDft = posADft
050100030822     C                   ENDIF
050200030822     C* Eseguo lo scan x trovare la fine del campo corrente
050300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050400030822     C*
050500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050700030822     C                   IF        posDaDft > 0 AND
050800030822     C                             posADft  > 0
050900030822     C* NCL
051000030822     C                   IF        %subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):1:3)
051300030822     C                             = 'NCL'
051400030822     C                   EVAL      PiStr=%trim(%subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051700030822     C                   EXSR      CHKNUM
051800030822     C                   IF        PiInt=*on
051900030822     C                   Z-ADD     PiVal         VABNCL
052000030822     C                   ENDIF
052100030822     C                   ENDIF
052200030822     C* CCM
052300030822     C                   IF        %subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):1:3)
052600030822     C                             = 'CCM'
052700030822     C                   EVAL      PiStr=%trim(%subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053000030822     C                   EXSR      CHKNUM
053100030822     C                   IF        PiInt=*on
053200030822     C                   Z-ADD     PiVal         VABCCM
053300030822     C                   ENDIF
053400030822     C                   ENDIF
053500030822     C* LNP
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'LNP'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABLNP
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* NRS
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'NRS'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABNRS
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* CTR
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'CTR'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABCTR
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* PKB
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'PKB'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiNum=*on
058400030822     C                   Z-ADD     PiVal         VABPKB
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* VLB
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'VLB'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiNum=*on
059700030822     C                   Z-ADD     PiVal         VABVLB
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* QFT
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'QFT'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiNum=*on
061000030822     C                   Z-ADD     PiVal         VABQFT
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* CBO
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'CBO'
061800030822     C                   EVAL      VABCBO=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   ENDIF
062200030822     C* TSP
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'TSP'
062700030822     C                   EVAL      VABTSP=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   ENDIF
063100030822     C* VAS
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'VAS'
063600030822     C                   EVAL      VABVAS=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   ENDIF
064000030822     C* VCA
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'VCA'
064500030822     C                   EVAL      VABVCA=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900070925     C* TIC
065000070925     C                   IF        %subst(
065100070925     C                             %subst(vlrppt:posDaDft+1:
065200070925     C                             posADft-posDaDft-1):1:3)
065300070925     C                             = 'TIC'
065400070925     C                   EVAL      VABTIC=%trim(%subst(
065500070925     C                             %subst(vlrppt:posDaDft+1:
065600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700070925     C                   ENDIF
065800030822     C* GCA
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'GCA'
066300030822     C                   EVAL      VABGCA=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* CTM
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'CTM'
067200030822     C                   EVAL      VABCTM=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* FFD
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'FFD'
068100030822     C                   EVAL      VABFFD=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VAD
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VAD'
069000030822     C                   EVAL      VABVAD=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* GMA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'GMA'
069900030822     C                   EVAL      VABGMA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300030822     C* GGA
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'GGA'
070800030822     C                   EVAL      VABGGA=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* GVA
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'GVA'
071700030822     C                   EVAL      VABGVA=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* TC1
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'TC1'
072600030822     C                   EVAL      VABTC1=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* TC2
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'TC2'
073500030822     C                   EVAL      VABTC2=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900040714     C* VATTRC
074000040714     C                   IF        %subst(
074100040714     C                             %subst(vlrppt:posDaDft+1:
074200040714     C                             posADft-posDaDft-1):1:3)
074300040714     C                             = 'TRC'
074400040714     C                   EVAL      VATTRC=%trim(%subst(
074500040714     C                             %subst(vlrppt:posDaDft+1:
074600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700040714     C                   ENDIF
074800030822     C* ...
074900030822     C                   ENDIF
075000030822     C                   ENDDO
075100020204     C*
075200010330     C                   ENDSR
075300010607     C*----------------------------------------------------*
075400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075500010607     C*----------------------------------------------------*
075600010607     C     IMPVAB        BEGSR
075700010607     C*
075800010607     C                   SETOFF                                       3132
075900030515     C*
076000010607     C                   MOVEL     datcor        VABAAS
076100010607     C                   MOVE      datcor        VABMGS
076200010607     C*
076300030715     C**********
076400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076500030715     C**********
076600030715     C                   IF        wGiro = *zeros
076700040113     C*
076800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
076900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
077100080429     C                             %trim(intcol(5))<>*blanks
077200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
077400080429     C                                      %trim(intcol(5))
077500070823     C     ';':CharCSV   XLATE     vindta        vindta
077600040510     C                   ENDIF
077700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077800031201     C     minu:maiu     XLATE     vindta        vindta
077900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078000030715     C                   DOW       posDa <= %len(%trim(vindta))
078100030715     C*
078200030715     C* Gestisco il 1° campo
078300030715     C                   IF        i = 1
078400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078500030715     C                   EVAL      posDa = 1
078600030715     C* Eseguo lo scan x trovare la fine del primo campo
078700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078800030715     C* A questo punto "estraggo" il campo corrente
078900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079000030715     C                                                    (posA-posDa))
079100030715     C* X i campi successivi al 1°
079200030715     C                   ELSE
079300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079400030715     C                   EVAL      posDa = posA + 1
079500030715     C* Eseguo lo scan x trovare la fine del campo corrente
079600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079700030715     C* Gestisco l'ultimo campo
079800030715     C                   IF        posA = *zeros
079900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   LEAVE
080400030715     C                   ELSE
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   ENDIF
080900030715     C                   ENDIF
081000030715     C* Incremento il contatore d campo
081100030715     C                   EVAL      i = i +1
081200030715     C                   ENDDO
081300030715     C*
081400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081500030715     C* delimitazione testo
081600030715     C                   EVAL      i = 1
081700030715     C                   DOW       i < %elem(SkSplitFLD)
081800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082000030715     C                   EVAL      i = i + 1
082100030715     C                   ENDDO
082200030715     C                   EVAL      wGiro = 1
082300030715     C                   ELSE
082400030715     C                   EVAL      wGiro = 2
082500031204     C**********
082600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082700031204     C* d separatore campo
082800031204     C**********
082900031204     C                   EVAL      vindta = %trim(vindta)
083000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083100031204     C                   EVAL      vindta = %subst(vindta:2)
083200031204     C                   ENDDO
083300031204     C*
083400031204     C                   Z-ADD     *zeros        lunghInput        4 0
083500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083900031204     C                   ENDDO
084000030715     C**********
084100030715     C* Effettuo lo split del campo dati d input
084200030715     C**********
084300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084400030715     C                   DOW       posDa <= %len(%trim(vindta))
084500030715     C*
084600030715     C* Gestisco il 1° campo
084700030715     C                   IF        i = 1
084800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084900030715     C                   EVAL      posDa = 1
085000030715     C* Eseguo lo scan x trovare la fine del primo campo
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* A questo punto "estraggo" il campo corrente
085300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085400030715     C                                                    (posA-posDa))
085500030715     C* X i campi successivi al 1°
085600030715     C                   ELSE
085700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085800030715     C                   EVAL      posDa = posA + 1
085900031204     C* Verifico che nn vi sia il campo nn valorizzato
086000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086100031204     C* Se campo nn valorizzato skippo al prossimo
086200031204     C                   EVAL      SkSplitCSV(i) = *blanks
086300031204     C                   EVAL      posA  = posA + 1
086400031204     C                   ELSE
086500030715     C* Eseguo lo scan x trovare la fine del campo corrente
086600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086700030715     C* Gestisco l'ultimo campo
086800030715     C                   IF        posA = *zeros
086900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   LEAVE
087400030715     C                   ELSE
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   ENDIF
087900031204     C                   ENDIF
088000030715     C                   ENDIF
088100030715     C* Incremento il contatore d campo
088200030715     C                   EVAL      i = i +1
088300030715     C                   ENDDO
088400030715     C*
088500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088600030715     C* delimitazione testo
088700030715     C                   EVAL      i = 1
088800030715     C                   DOW       i < %elem(SkSplitCSV)
088900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089100030715     C                   EVAL      i = i + 1
089200030715     C                   ENDDO
089300100720     C*
089400100720     C* Stacco un progressivo x numero spedizione
089500100720 xxx C***                EXSR      repNSP
089600100326     C*
089700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089800030715     C                   EVAL      i = 1
089900030715     C                   DOW       i < %elem(SkSplitFLD)
090000030715     C*
090100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090300030715     C                             SkSplitCSV(i) <> *blanks
090400030721     C*** ATB
090500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090600030721     C                   EVAL      VABATB = SkSplitCSV(i)
090700030721     C                   ENDIF
090800030715     C*** RSD
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091300030715     C* ==
091400030715     C                   ENDIF
091500030721     C*** RD2
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030715     C*** IND
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092100030715     C                   EVAL      VABIND = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030715     C*** LOD
092400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092600030715     C                   ENDIF
092700030715     C*** PRD
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093000030715     C                   ENDIF
093100030721     C*** NZD
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030715     C*** RMA
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093800030715     C                   ENDIF
093900030715     C*** NOT
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** NT2
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030715     C*** VCA
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030715     C*** VAS
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** FFD
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030721     C*** GC1
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** GC2
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** TSP
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097000030721     C                   ENDIF
097100040510     C*** CBO
097200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097400040510     C                   ENDIF
097500030721     C*** NAS
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** TIC
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GCA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** XCO
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** CTM
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TCR
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** CTS
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** FTM
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** VAD
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** GMA
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** GGA
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GVA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TC1
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** TC2
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** SCL
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** RMO
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** NMO
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*
104400030721     C* Reperisco quindi i campi numerici...
104500040510     C*** NSP
104600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104700040510     C                   EVAL      PiStr=SkSplitCSV(i)
104800040510     C                   EXSR      CHKNUM
104900040510     C                   IF        PiInt=*on
105000040510     C                   Z-ADD     PiVal         VABNSP
105100040510     C                   ELSE
105200040510     C                   SETON                                        31
105300040510     C                   EVAL      VABNSP = *zeros
105400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105500040510     C                             + ' ' + 'VABNSP'
105600040510     C                   ENDIF
105700040510     C                   ENDIF
105800071106     C*** RMN
105900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
106000071106     C                   EVAL      PiStr=SkSplitCSV(i)
106100071106     C                   EXSR      CHKNUM
106200071106     C                   IF        PiInt=*on
106300071106     C                   Z-ADD     PiVal         VABRMN
106400071106     C                   ELSE
106500071106     C                   SETON                                        32
106600071106     C                   EVAL      VABRMN = *zeros
106700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
106800071106     C                             + ' ' + 'VABRMN'
106900071106     C                   ENDIF
107000071106     C                   ENDIF
107100030721     C*** CAD
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on
107600030721     C                   Z-ADD     PiVal         Num5_0
107700030721     C                   MOVEL(p)  Num5_0        VABCAD
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABCAD = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200030721     C                             + ' ' + 'VABCAD'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** NCL
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiInt=*on
109000030721     C                   Z-ADD     PiVal         VABNCL
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABNCL = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040122     C                             + ' ' + 'VABNCL'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** PKB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300030721     C                   Z-ADD     PiVal         VABPKB
110400030721     C                   ELSE
110500030721     C                   SETON                                        32
110600030721     C                   EVAL      VABPKB = *zeros
110700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110800030721     C                             + ' ' + 'VABPKB'
110900030721     C                   ENDIF
111000030721     C                   ENDIF
111100030721     C*** CAS
111200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111300030721     C                   EVAL      PiStr=SkSplitCSV(i)
111400030721     C                   EXSR      CHKNUM
111500030721     C                   IF        PiNum=*on
111600030721     C                   Z-ADD     PiVal         VABCAS
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAS = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAS'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** CCM
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on      AND
112900030721     C                             PiVal<=9999999 AND
113000030721     C                             PiVal>*zeros
113100030721     C                   Z-ADD     PiVal         VABCCM
113200030721     C                   ELSE
113300030721     C                   SETON                                        32
113400030721     C                   EVAL      VABCCM = *zeros
113500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113600040510     C                             + ' ' + 'VABCCM'
113700030721     C                   ENDIF
113800030721     C                   ENDIF
113900030721     C*** LNP
114000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114100030721     C                   EVAL      PiStr=SkSplitCSV(i)
114200030721     C                   EXSR      CHKNUM
114300030721     C                   IF        PiInt=*on    AND
114400030721     C                             PiVal<=999   AND
114500030721     C                             PiVal>*zeros
114600030721     C                   Z-ADD     PiVal         VABLNP
114700030721     C                   ELSE
114800040715     C                   SETON                                        32
114900030721     C                   EVAL      VABLNP = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100040510     C                             + ' ' + 'VABLNP'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** NRS
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on AND
115900030721     C                             PiVal<=99
116000030721     C                   Z-ADD     PiVal         VABNRS
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABNRS'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** LNA
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on    AND
117200030721     C                             PiVal<=999   AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABLNA
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABLNA = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900030721     C                             + ' ' + 'VABLNA'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** CTR
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on  AND
118700030721     C                             PiVal<=999
118800030721     C                   Z-ADD     PiVal         VABCTR
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABCTR'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** IAS
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiNum=*on
120000030721     C                   Z-ADD     PiVal         VABIAS
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABIAS'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** VLB
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiNum=*on
121200030721     C                   Z-ADD     PiVal         VABVLB
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABVLB'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** QFT
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiNum=*on
122400030721     C                   Z-ADD     PiVal         VABQFT
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABQFT'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** NCD
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on      AND
123600030721     C                             PiVal<=9999999
123700030721     C                   Z-ADD     PiVal         VABNCD
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABNCD'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** NCA
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on      AND
124900030721     C                             PiVal<=9999999
125000030721     C                   Z-ADD     PiVal         VABNCA
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABNCA'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** ZNC
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on AND
126200030721     C                             PiVal<=99
126300030721     C                   Z-ADD     PiVal         VABZNC
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABZNC'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** DCR
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on       AND
127500030721     C                             PiVal<=99999999
127600030721     C                   Z-ADD     PiVal         VABDCR
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABDCR'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** HCR
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on   AND
128800030721     C                             PiVal<=9999
128900030721     C                   Z-ADD     PiVal         VABHCR
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABHCR'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** VMD
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiNum=*on
130100030721     C                   Z-ADD     PiVal         VABVMD
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130500030721     C                             + ' ' + 'VABVMD'
130600030721     C                   ENDIF
130700030721     C                   ENDIF
130800030721     C*** ANT
130900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131000030721     C                   EVAL      PiStr=SkSplitCSV(i)
131100030721     C                   EXSR      CHKNUM
131200030721     C                   IF        PiInt=*on        AND
131300030721     C                             PiVal<=999999999
131400030721     C                   Z-ADD     PiVal         VABANT
131500030721     C                   ELSE
131600030721     C                   SETON                                        32
131700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131800030721     C                             + ' ' + 'VABANT'
131900030721     C                   ENDIF
132000030721     C                   ENDIF
132100030721     C*** CMO
132200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132300030721     C                   EVAL      PiStr=SkSplitCSV(i)
132400030721     C                   EXSR      CHKNUM
132500030721     C                   IF        PiInt=*on
132600030721     C                   Z-ADD     PiVal         Num5_0
132700030721     C                   MOVEL(p)  Num5_0        VABCMO
132800030721     C                   ELSE
132900030721     C                   SETON                                        32
133000030721     C                   EVAL      VABCMO = *zeros
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABCMO'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500040510     C*
133600040510     C* Reperisco i campi del FIVAT
133700040510     C*** VATNOT
133800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
134000040510     C                   ENDIF
134100040510     C*** VATTRC
134200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
134300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
134400040510     C                   ENDIF
134500060331     C*** VATNOT_A
134600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134800060331     C                   ENDIF
134900060331     C*** VATNOT_B
135000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
135100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
135200060331     C                   ENDIF
135300060331     C*** VATNOT_E
135400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135600060331     C                   ENDIF
135700070823     C*
135800071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135900100720     C*** RMN / NSP
136000100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
136100100413     C                   EVAL      PiStr=SkSplitCSV(i)
136200100413     C                   EXSR      CHKNUM
136300100413     C                   IF        PiInt=*on
136400100419     C                   Z-ADD     PiVal         VABNSP
136500100720     C                   Z-ADD     PiVal         VABRMN
136600100413     C                   ELSE
136700100720     C                   Z-ADD     *zeros        VABRMN
136800100413     C                   SETON                                        32
136900100413     C                   EVAL      vinmsg = %trimr(vinmsg)
137000100720     C                             + ' ' + 'VABNSP VABRMN'
137100100413     C                   ENDIF
137200100326     C                   ENDIF
137300100720     C*** PKB
137400100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
137500100720     C                   EVAL      PiStr=SkSplitCSV(i)
137600100720     C                   EXSR      CHKNUM
137700100720     C                   IF        PiNum=*on
137800100720     C                   EVAL      PiVal = PiVal / 1000                         * da grammi a Kg
137900100720     C                   Z-ADD     PiVal         VABPKB
138000100720     C                   ELSE
138100100720     C                   SETON                                        32
138200100720     C                   EVAL      VABPKB = *zeros
138300100720     C                   EVAL      vinmsg = %trimr(vinmsg)
138400100720     C                             + ' ' + 'VABPKB'
138500100720     C                   ENDIF
138600100720     C                   ENDIF
138700100720     C*** RD2 / NOT_A
138800100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRD2'
138900100720     C                   EVAL      VABRD2    = %trim(SkSplitCSV(i))
139000100720     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139100100720     C                   ENDIF
139200070925     C*
139300071029     C***  <===  -----------------------------------------
139400040510     C*
139500030715     C                   ENDIF
139600030715     C                   EVAL      i = i + 1
139700030715     C                   ENDDO
139800070925     C*
139900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
140000070925     C*
140100071029     C***  <===  -----------------------------------------
140200070925     C*
140300070925     C* Considerazioni finali su CBO/CAS
140400100318     C                   IF        wFlgCAS = '1'    OR
140500100318     C                             VABCAS  > *zeros
140600070925     C                   IF        VABCBO = '1'
140700070925     C                   EVAL      VABCBO = '4'
140800070925     C                   ENDIF
140900070925     C                   IF        VABCBO = '2'
141000070925     C                   EVAL      VABCBO = '6'
141100070925     C                   ENDIF
141200070925     C                   ENDIF
141300070925     C***  <===  ----------------------------
141400040510     C*
141500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
141600040510     C                   IF        VABNSP = *zeros
141700040510     C                   SETON                                        31
141800040510     C                   ENDIF
141900040715     C*
142000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
142100040715     C                   IF        VABLNP = *zeros
142200040715     C                   SETON                                        31
142300040715     C                   ENDIF
142400040510     C*
142500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
142600040510     C                   IF        VABRMN = *zeros
142700040510     C                   Z-ADD     VABNSP        VABRMN
142800040510     C                   ENDIF
142900040510     C*
143000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
143100040510     C                   IF        VABRMA = *blanks
143200040714     C                   MOVEL     VABRMN        VABRMA
143300040510     C                   ENDIF
143400040128     C*
143500040510     C* Se provincia nn valorizzata la reperisco
143600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
143700040510     C                   IF        VABPRD = *blanks
143800040128     C                   CLEAR                   TISI95DS
143900040128     C                   EVAL      I95TCN = '3'
144000040128     C                   Z-ADD     datcor        I95DAT
144100040128     C                   EVAL      I95NAR = VABNZD
144200040128     C                   EVAL      I95CAP = VABCAD
144300040128     C                   EVAL      I95LOC = VABLOD
144400040128     C                   CALL      'TISI95R'
144500040128     C                   PARM                    TISI95DS
144600040128     C                   EVAL      VABPRD = O95PRV
144700040510     C                   ENDIF
144800020204     C*
144900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
145000020204     C                   EXSR      CHKIMPDIV
145100030715     C                   ENDIF
145200020204     C*
145300010607     C                   ENDSR
145400010607     C*----------------------------------------------------*
145500010601
145600020204
145700020204     C*----------------------------------------------------*
145800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
145900020204     C*----------------------------------------------------*
146000020204     C     CHKIMPDIV     BEGSR
146100020204     C*
146200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
146300020204     C                   Z-ADD     *zeros        wrkDec            9 9
146400020204     C*
146500020204     C* Come prima cosa effettuo considerazioni sulla divisa
146600020204     C                   IF        vabIAS > *zeros
146700020204     C                   IF        vabVAS <> 'EUR'
146800020204     C                   EVAL      vabVAS =  'ITL'
146900020204     C                   ENDIF
147000020204     C                   ENDIF
147100020204     C*
147200020204     C                   IF        vabCAS > *zeros
147300020204     C                   IF        vabVCA <> 'EUR'
147400020204     C                   EVAL      vabVCA =  'ITL'
147500020204     C                   ENDIF
147600020204     C                   ENDIF
147700020204     C*
147800020204     C                   IF        vabVMD > *zeros
147900020204     C                   IF        vabVAD <> 'EUR'
148000020204     C                   EVAL      vabVAD =  'ITL'
148100020204     C                   ENDIF
148200020204     C                   ENDIF
148300020204     C*
148400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
148500020204     C                   Z-ADD     vabIAS        wrkDec
148600020204     C                   IF        wrkDec > *zeros
148700020204     C                   IF        vabVAS = 'ITL'
148800020204     C                   EVAL      vabIAS = *zeros
148900020204     C                   ENDIF
149000020204     C                   ENDIF
149100020204     C*
149200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
149300020204     C                   Z-ADD     vabCAS        wrkDec
149400020204     C                   IF        wrkDec > *zeros
149500020204     C                   IF        vabVCA = 'ITL'
149600020204     C                   EVAL      vabCAS = *zeros
149700020204     C                   ENDIF
149800020204     C                   ENDIF
149900020204     C*
150000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
150100020204     C                   Z-ADD     vabVMD        wrkDec
150200020204     C                   IF        wrkDec > *zeros
150300020204     C                   IF        vabVAD = 'ITL'
150400020204     C                   EVAL      vabVMD = *zeros
150500020204     C                   ENDIF
150600020204     C                   ENDIF
150700020204     C*
150800020204     C                   ENDSR
150900020204     C***
151000010330
151100100318
151200100318
151300100318     C*----------------------------------------------------*
151400100318     C*  CONTROLLO NUMERICITA' CAMPI
151500100318     C*----------------------------------------------------*
151600100318     C     CHKNUM        BEGSR
151700100318     C*
151800100318     C                   IF        PiDecChr = *blanks
151900100318     C                   EVAL      PiDecChr = CharNUM
152000100318     C                   ENDIF
152100100318     C*
152200100318     C                   callp     UBISNUM_Check(PiStr
152300100318     C                                          :PiDecChr
152400100318     C                                          :PiVal
152500100318     C                                          :PiNum
152600100318     C                                          :PiInt)
152700100318     C*
152800100318     C                   ENDSR
152900100318     C***
153000010601
153100010601
153200010601
153300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
153400010601     C     repfil        BEGSR
153500010601     C*
153600010601     C                   if        invfil = *zeros and
153700010601     C                             depfil > *zeros and
153800010629     C                             (vinflg = *blanks or
153900010629     C                              vinflg = *zeros)
154000010601     C
154100010601     C                   eval      invfil = depfil
154200010601     C                   endif
154300010601     C*
154400010601     C                   if        depfil <> invfil and
154500010601     C                             invfil > *zeros
154600010601     C                   eval      flgMulti = '1'
154700010601     C                   if        vinflg = *blanks
154800010601     C                   add       1             cntNonEl
154900010601     C                   endif
155000010601     C                   endif
155100010601     C*
155200010601     C                   if        vinflg = '2'
155300010601     C                   eval      flgStato = '2'
155400010601     C                   endif
155500010601     C*
155600010601     C                   ENDSR
155700010601     C***
155800010601
155900010601
156000010601
156100010330
156200040119      /TITLE Invio dei dati al punto operativo.
156300040119     C     invio         BEGSR
156400040119     C*
156500040119     C* 1° invio FIVAT
156600040119     C                   reset                   dscmz
156700040119     C                   move      vlrpoi        cmzdst
156800040119     C                   eval      cmzfld = 'FIVATWWR'
156900040119     C                   eval      cmzmbd = vlrhdl
157000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
157100040119     C***                if        prmfir = *blanks
157200040119     C                   eval      cmzfla = 'FIVAT00F'
157300040119     C                   eval      cmzmba = 'FIVAT00F'
157400040119     C***                else
157500040119     C***                eval      cmzfla = prmfir
157600040119     C***                eval      cmzmba = prmfir
157700040119     C***                endif
157800040119     C                   eval      cmznrr = *zeros
157900040119     C                   move      §ctrokvt      cmznrr
158000040119     C                   eval      cmzlba = vlrfl1
158100040119     C                   call(e)   'TIS711C'
158200040119     C                   parm                    dscmz
158300040119     C                   parm      *blanks       esito
158400040119     C                   if        %error
158500040119     C                             or cmzerr = '1'
158600040119     C                             or esito  = '1'
158700040119     C                   eval      wrkesito = '3'
158800040119     C                   else
158900040119     C*
159000040119     C* 2° invio FIVAB
159100040119     C                   reset                   dscmz
159200040119     C                   move      vlrpoi        cmzdst
159300040119     C                   eval      cmzfld = vlrfou
159400040119     C                   eval      cmzmbd = vlrhdl
159500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159600040119     C***                if        prmfir = *blanks
159700040119     C                   eval      cmzfla = 'FIVAB00F'
159800040119     C                   eval      cmzmba = 'FIVAB00F'
159900040119     C***                else
160000040119     C***                eval      cmzfla = prmfir
160100040119     C***                eval      cmzmba = prmfir
160200040119     C***                endif
160300040119     C                   eval      cmznrr = *zeros
160400040119     C                   move      §ctrokvb      cmznrr
160500040119     C                   eval      cmzlba = vlrfl1
160600040119     C                   call(e)   'TIS711C'
160700040119     C                   parm                    dscmz
160800040119     C                   parm      *blanks       esito
160900040119     C                   if        %error
161000040119     C                             or cmzerr = '1'
161100040119     C                             or esito  = '1'
161200040119     C                   eval      wrkesito = '3'
161300040119     C                   endif
161400040119     C                   endif
161500040119     C*
161600040119     C                   ENDSR
161700040119     C***
161800010601
161900010601
162000010601
162100010601
162200010601      /TITLE Invio dei dati al punto operativo.
162300010601     C     opeini        BEGSR
162400010601     C*
162500010601     C* Inizializzo flag e contatori operativi
162600010601     C                   movel     '0'           flgGiro           1
162700010601     C                   movel     '0'           flgMulti          1
162800010601     C                   movel     '1'           flgStato          1
162900010615     C                   movel     '0'           flgOk             1
163000010601     C                   z-add     *zeros        cntNonEl         10 0
163100010601     C                   z-add     *zeros        depfil            3 0
163200010601     C                   z-add     *zeros        invfil            3 0
163300010601     C*
163400010601     C                   ENDSR
163500010601     C***
163600070326
163700070326
163800070326
163900070326
164000070326     C     *pssr         BEGSR
164100070329     C*
164200070329     C                   if        %open(tivin00r)
164300070329     C                   close     tivin00r
164400070329     C                   endif
164500070329     C                   if        %open(fivabwwr)
164600070329     C                   close     fivabwwr
164700070329     C                   endif
164800070329     C                   if        %open(fivatwwr)
164900070329     C                   close     fivatwwr
165000070329     C                   endif
165100070326     C*
165200070326     C* Effettuo la chiamata al CLLE preposto
165300070326     C                   call(e)   'TITVVTC'
165400070326     C                   parm                    parccm
165500070326     C                   parm                    parmbr
165600070326     C                   parm      '2'           paropz
165700070326     C*
165800070326     C                   eval      wrkesito = '2'
165900070404     C*
166000070404     C                   seton                                        LR
166100070326     C*
166200070326     C                   ENDSR     '*CANCL'
166300070326     C***
166400100318
166500100318
166600100318
166700100318     C     normCSV       BEGSR
166800100318     C*
166900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
167000100318     C* separatori campi
167100100318     C                   if        CharTXT <> *blanks
167200100318     C                   z-add     *zeros        wLenVINDTA        4 0
167300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
167400100318     C                   setoff                                       80
167500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
167600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
167700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
167800100318     C                   if        not *in80
167900100318     C                   seton                                        80
168000100318     C                   else
168100100318     C                   setoff                                       80
168200100318     C                   endif
168300100318     C                   endif
168400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
168500100318     C                             and *in80 = *on
168600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
168700100318     C                   endif
168800100318     C                   enddo
168900100318     C                   endif
169000100318     C*
169100100318     C                   ENDSR
169200100318     C***
169300070326
169400100326
169500100326
169600100326     C*----------------------------------------------------*
169700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
169800100326     C*----------------------------------------------------*
169900100326     C     REPNSP        BEGSR
170000100326     C*
170100100326     C* NSP => Stacco un numeratore da AZNUM
170200100326     C                   clear                   TRUL33DS
170300100326     C                   eval      I33OPE = *zeros
170400100326     C                   eval      I33CNU = 302
170500100326     C                   eval      I33NUM = 1
170600100326     C                   movel     TRUL33DS      KPJBU
170700100326     C                   call      'TRUL33R'
170800100326     C                   parm                    KPJBA
170900100326     C                   movel     KPJBU         TRUL33DS
171000100326     C                   if        O33ERR = *zeros
171100100326     C                   z-add     O33NRF        VABNSP
171200100326     C                   z-add     O33NRF        VATNSP
171300100326     C                   else
171400100326     C                   SETON                                        31
171500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
171600100326     C                             + ' ' + 'VABNSP VATNSP'
171700100326     C                   endif
171800100326     C*
171900100326     C                   ENDSR
172000010330
172100100326
172200010330
172300000613     C     *inzsr        BEGSR
172400990910     C*
172500990910     C     *entry        plist
172600990920     C                   parm                    tivlrds
172700990921     C                   parm      wrkesito      esito
172800000724     C                   parm                    prmlit
172900000710     C                   parm                    prmfir
173000010330     C*
173100010330     C* CALCOLA LA DATA CORRENTE
173200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
173300100318     C                   eval      datcor = %dec(%date() : *ISO)
173400000613     C*
173500000613     C                   ENDSR
173600000613     C***
173700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173800120221VATNOT_E;W_VABRD2;VABRSD;VABIND;VABCAD;VABLOD;XXX;XXX;W_VABPKB;
173900120913XXX;XXX;VABRMA;VATNOT_B;VABNCL;W_VABRMN;VABCAS;VABTIC;VABRMO;VABNMO;VABDCR
174000100318
174100100318
174200100318
