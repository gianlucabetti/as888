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
016300110131     C*
016400110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
016500120227 xxx C***                read      tivin00r                               70
016600010330     C*
016700030822     C                   do        *HIVAL
016800010330     C*
016900030822     C                   read      tivin00r                               70
017000010618     C*
017100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017200010618     C                   if        vinflg = '1'
017300010618     C                   eval      flgOk = '1'
017400010618     C                   endif
017500040510     C*
017600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017700040510     C                   z-add     *zeros        wLenVINDTA        4 0
017800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
017900040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018100040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018200040510     C                   z-add     1             wFlgVINDTA
018300040510     C                   leave
018400040510     C                   endif
018500040510     C                   enddo
018600010618     C*
018700040510     C                   if        vindta > *blanks AND
018800040510     C                             wFlgVINDTA = 1
018900010330     C                   add       1             rrnum
019000010330     C*
019100010601     C                   if        *in70 = *off and
019200010330     C                             (vinflg = *blanks
019300010330     C                              or vinflg = '0'
019400010330     C                              or vinflg = '2')
019500010330     C*
019600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019700010711     C                   if        vinflg = *blanks or vinflg = '0'
019800010711     C                   clear                   vinmsg
019900010711     C                   endif
020000010601     C*
020100070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020200070925     C                   eval      vindta  = ' ' + %trim(vindta)
020300070925     C*
020400080527     C                   exsr      inzvar
020500100318     C                   exsr      normCSV
020600010605     C                   exsr      impvab
020700010601     C*
020800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020900010605     C                   eval      depfil = VABLNP
021000010601     C                   exsr      repfil
021100010601     C                   if        depfil = invfil
021200021025     C                   if        vlrpoi = 999
021300030822     C                   move(P)   invfil        VABFGS
021400021025     C                   else
021500030822     C                   move(P)   vlrpoi        VABFGS
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200040119     C  N31              add       1             §CTROKVB
022300040119     C   32              add       1             §CTRMO
022400040119     C   31              add       1             §CTRNO
022500080418     C*
022600030822     C                   if        wGiro = 2
022700080513     C* Scrivo sempre il VAT
022800080513     C  N31              exsr      wrivat
022900110201     C                   exsr      chkvab
023000080513     C                   endif
023100010604     C*
023200010604     C                   if        *in31 = *off and
023300010604     C                             *in32 = *off
023400010604     C                   eval      vinflg = '1'
023500010604     C                   else
023600010604     C                   eval      vinflg = '2'
023700010604     C                   endif
023800010604     C                   endif
023900010601     C*
024000010604     C                   endif
024100010604     C*
024200010330     C                   else
024300010330     C                   eval      vinflg = '1'
024400010330     C                   endif
024500010601     C*
024600010601     C  N70              update    tivin000
024700010330     C*
024800030822     C  N70              enddo
024900040119     C*
025000080527     C* Verifico dati rimasti "in cannna"...
025100080527     C                   seton                                        65
025200110201     C                   exsr      chkvab
025300080527     C*
025400040119     C                   endif                                                  (endif - chkcall)
025500010601     C*
025600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025700010601     C                   if        cntNonEl = *zeros or
025800010601     C                             flgMulti = '0'
025900010330     C* Se non ci sono record con errori ...
026000010601     C                   if        §ctrno = 0 and
026100010604     C                             §ctrmo = 0 and
026200010601     C                             flgStato <> '2'
026300010330     C* ... restituisco esito OK.
026400010330     C                   eval      wrkesito = '0'
026500010330     C                   else
026600040119     C                   if        §ctrokvb > 0
026700010330     C                   eval      wrkesito = '1'
026800010330     C                   else
026900010615     C                   if        flgOk = '0'
027000010615     C                   eval      wrkesito = '2'
027100010615     C                   else
027200010615     C                   eval      wrkesito = '6'
027300010615     C                   endif
027400010330     C                   endif
027500010330     C                   endif
027600010601     C                   else
027700010601     C                   eval      wrkesito = '9'
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        %open(tivin00r)
028100010330     C                   close     tivin00r
028200010330     C                   endif
028300021025     C                   if        %open(fivabwwr)
028400021025     C                   close     fivabwwr
028500010330     C                   endif
028600040512     C                   if        %open(fivatwwr)
028700040512     C                   close     fivatwwr
028800040512     C                   endif
028900010601     C*
029000010601     C                   if        vlrpoi <> 999
029100010601     C                   eval      invfil = vlrpoi
029200010601     C                   endif
029300010330     C*
029400040119     C                   if        §ctrokvb > 0
029500010601     C                             and invfil > *zeros
029600010330     C                   exsr      invio
029700010330     C                   endif
029800010601     C*
029900010618     C                   if        flgGiro = '1'
030000010601     C                   exsr      endela
030100010618     C                   endif
030200010330     C*
030300010330     C                   ENDSR
030400010330     C***
030500080527
030600080527
030700080527
030800080527     C*----------------------------------------------------*
030900080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031000080527     C*----------------------------------------------------*
031100100413     C     CHKVAB        BEGSR
031200110201     C*
031300110201     C* Se richiesti ignorare colli / peso / volume
031400110315 xxx C***                if        *in35
031500110315 xxx C***                z-add     *zeros        VABNCL
031600110315 xxx C***                z-add     *zeros        VABPKB
031700110315 xxx C***                z-add     *zeros        VABVLB
031800110315 xxx C***                endif
031900080527     C*
032000080527     C* Scrivo il VAB solo a rottura d spedizione
032100080527     C                   if        depspe = *zeros
032200080527     C  N31              eval      fivabds_sav = fivabds
032300080527     C                   eval      depspe = vabNSP
032400080527     C                   else
032500080527     C                   if        depspe = vabNSP AND *in65 = *off
032600100413     C   51              add       VABNCL        sav_VABNCL
032700100413     C   51              add       VABPKB        sav_VABPKB
032800100413     C   51              add       VABVLB        sav_VABVLB
032900100720     C   52              z-add     VABNCL        sav_VABNCL
033000100720     C   52              z-add     VABPKB        sav_VABPKB
033100100720     C   52              z-add     VABVLB        sav_VABVLB
033200100720     C   53              add       VABNCL        sav_VABNCL
033300100720     C   53              z-add     VABPKB        sav_VABPKB
033400100720     C   53              z-add     VABVLB        sav_VABVLB
033500080527     C                   else
033600080527     C  N31              eval      fivabds_ok = fivabds_sav
033700080527     C  N31              exsr      wrivab
033800080527     C  N31              eval      fivabds_sav = fivabds
033900080527     C                   eval      depspe = vabNSP
034000080527     C                   endif
034100080527     C                   endif
034200080527     C*
034300080527     C                   ENDSR
034400040119
034500040119
034600040119
034700040119     C*----------------------------------------------------*
034800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034900040119     C*----------------------------------------------------*
035000040510     C     WRIVAB        BEGSR
035100080513     C*
035200080513     C* Gestisco l'eventuale rottura x numero spedizione
035300080527     C                   if        VABNSP <> depspe or *in65 = *on
035400070301     C                   if        wVATNOT_E <> *blanks
035500070301     C                   eval      VABCTM = '7Q'
035600070301     C                   endif
035700100413     C                   write     fivab000
035800080513     C                   endif
035900040119     C*
036000040119     C                   ENDSR
036100040119
036200040119
036300040119
036400040119     C*----------------------------------------------------*
036500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036600040119     C*----------------------------------------------------*
036700040510     C     WRIVAT        BEGSR
036800060512     C*
036900060512     C* Inizializzo i campi chiave
037000060512     C                   eval      VATFGS = VABFGS
037100060512     C                   eval      VATAAS = VABAAS
037200060512     C                   eval      VATCCM = VABCCM
037300070823     C                   eval      VATNRS = VABNRS
037400060512     C                   eval      VATNSP = VABNSP
037500060512     C                   eval      VATLNP = VABLNP
037600040119     C*
037700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037800040510     C                   if        VATTRC = *blanks
037900040510     C                   eval      VATTRC = 'E'
038000040510     C                   endif
038100110318     C                   if        VATNOT <> *blanks
038200040510     C                   write     FIVAT000
038300040119     C                   add       1             §CTROKVT
038400110318     C                   endif
038500060331     C*
038600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038700060331     C                   if        wVATNOT_A <> *blanks
038800060331     C                   eval      VATNOT = wVATNOT_A
038900060331     C                   eval      VATTRC = 'A'
039000060331     C                   write     FIVAT000
039100060331     C                   add       1             §CTROKVT
039200060331     C                   endif
039300060331     C*
039400060331     C                   if        wVATNOT_B <> *blanks
039500060331     C                   eval      VATNOT = wVATNOT_B
039600060331     C                   eval      VATTRC = 'B'
039700060331     C                   write     FIVAT000
039800060331     C                   add       1             §CTROKVT
039900060331     C                   endif
040000060331     C*
040100060331     C                   if        wVATNOT_E <> *blanks
040200110201     C                   eval      VATNOT = wVATNOT_E
040300060331     C                   eval      VATTRC = 'E'
040400110201     C                   write     FIVAT000
040500110201     C                   add       1             §CTROKVT
040600110201     C                   endif
040700110927     C*
040800110927     C                   if        wVATNOT_I <> *blanks
040900110927     C                   eval      VATNOT = wVATNOT_I
041000110927     C                   eval      VATTRC = 'I'
041100110927     C                   write     FIVAT000
041200110927     C                   add       1             §CTROKVT
041300110927     C                   endif
041400040119     C*
041500040119     C                   ENDSR
041600040119
041700040119
041800040119
041900040119     C*----------------------------------------------------*
042000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100040119     C*----------------------------------------------------*
042200040119     C     PREVAT        BEGSR
042300040119     C*
042400040119     C* Compongo il nome del membro da dare al FIVATWWR
042500040119     C                   eval      parmbr = vlrhdl
042600040119     C                   movel     'M'           parmbr
042700040510     C                   eval      parccm = vlrksc
042800040119     C                   eval      paropz = '1'
042900040119     C* Effettuo la chiamata al CLLE preposto
043000040119     C                   call(e)   'TITVVTC'
043100040119     C                   parm                    parccm
043200040119     C                   parm                    parmbr
043300040119     C                   parm                    paropz
043400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500040119     C                   if        %error
043600040119     C                   movel     '1'           chkcall
043700040119     C                   else
043800040119     C                   movel     '0'           chkcall
043900040119     C                   endif
044000040119     C*
044100040119     C                   ENDSR
044200010601
044300010601
044400010601
044500010330     C*----------------------------------------------------*
044600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044700010330     C*----------------------------------------------------*
044800040119     C     INZVAR        BEGSR
044900080527     C*
045000080527     C                   setoff                                       65
045100010330     C*
045200030822     C* Inizializzo variabili di wrk
045300020204     C                   Z-ADD     *zeros        Num5_0            5 0
045400101018     C                   MOVEL     *blanks       wFlgCAS
045500060331     C                   MOVEL     *blanks       wVATNOT_A
045600060331     C                   MOVEL     *blanks       wVATNOT_B
045700060331     C                   MOVEL     *blanks       wVATNOT_E
045800110927     C                   MOVEL     *blanks       wVATNOT_I
045900030822     C*
046000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046100030822     C                   CLEAR                   SkSplitCSV
046200030822     C                   EVAL      i = 1
046300030822     C                   EVAL      posDa = *zeros
046400030822     C                   EVAL      posA  = *zeros
046500080527     C*
046600080527     C                   CLEAR                   FIVABDS
046700080527     C                   CLEAR                   FIVAT000
046800030822     C*
046900030822     C* Reimposto i valori di default
047000030822     C                   EXSR      DEFCAM
047100030822     C*
047200010330     C                   ENDSR
047300010330     C*----------------------------------------------------*
047400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047500010330     C*----------------------------------------------------*
047600010330     C     DEFCAM        BEGSR
047700030822     C*
047800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047900030715     C* e delimitatore testo.
048000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048300100318     C*
048400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
048500100318     C                   EVAL      CharSOS = CharNUM
048600030822     C*
048700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
048800100720     C                   SETOFF                                       505152
048900100720     C                   SETOFF                                       53
049000100413     C                   SELECT
049100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
049200100720     C                   SETON                                        50
049300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
049400100413     C                   SETON                                        51
049500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
049600100720     C                   SETON                                        52
049700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
049800100720     C                   SETON                                        53
049900100413     C                   ENDSL
050000030822     C*
050100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050200030822     C                   EVAL      posDaDft = 1
050300030822     C                   EVAL      posADft  = 0
050400030822     C                   EVAL      wGiroDft = 0
050500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050600030822     C                             posDaDft > 0
050700030822     C*
050800030822     C* Gestisco il 1° giro
050900030822     C                   IF        wGiroDft = 0
051000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051200030822     C* Incremento il contatore dei "giri"
051300030822     C                   EVAL      wGiroDft = 1
051400030822     C                   ELSE
051500030822     C                   EVAL      posDaDft = posADft
051600030822     C                   ENDIF
051700030822     C* Eseguo lo scan x trovare la fine del campo corrente
051800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051900030822     C*
052000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052200030822     C                   IF        posDaDft > 0 AND
052300030822     C                             posADft  > 0
052400030822     C* NCL
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'NCL'
052900030822     C                   EVAL      PiStr=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   EXSR      CHKNUM
053300030822     C                   IF        PiInt=*on
053400030822     C                   Z-ADD     PiVal         VABNCL
053500030822     C                   ENDIF
053600030822     C                   ENDIF
053700030822     C* CCM
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'CCM'
054200030822     C                   EVAL      PiStr=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   EXSR      CHKNUM
054600030822     C                   IF        PiInt=*on
054700030822     C                   Z-ADD     PiVal         VABCCM
054800030822     C                   ENDIF
054900030822     C                   ENDIF
055000030822     C* LNP
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'LNP'
055500030822     C                   EVAL      PiStr=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   EXSR      CHKNUM
055900030822     C                   IF        PiInt=*on
056000030822     C                   Z-ADD     PiVal         VABLNP
056100030822     C                   ENDIF
056200030822     C                   ENDIF
056300030822     C* NRS
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NRS'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNRS
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CTR
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CTR'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCTR
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* PKB
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'PKB'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiNum=*on
059900030822     C                   Z-ADD     PiVal         VABPKB
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* VLB
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'VLB'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiNum=*on
061200030822     C                   Z-ADD     PiVal         VABVLB
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* QFT
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'QFT'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiNum=*on
062500030822     C                   Z-ADD     PiVal         VABQFT
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* CBO
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'CBO'
063300030822     C                   EVAL      VABCBO=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* TSP
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'TSP'
064200030822     C                   EVAL      VABTSP=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* VAS
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'VAS'
065100030822     C                   EVAL      VABVAS=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* VCA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'VCA'
066000030822     C                   EVAL      VABVCA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400070925     C* TIC
066500070925     C                   IF        %subst(
066600070925     C                             %subst(vlrppt:posDaDft+1:
066700070925     C                             posADft-posDaDft-1):1:3)
066800070925     C                             = 'TIC'
066900070925     C                   EVAL      VABTIC=%trim(%subst(
067000070925     C                             %subst(vlrppt:posDaDft+1:
067100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200070925     C                   ENDIF
067300030822     C* GCA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'GCA'
067800030822     C                   EVAL      VABGCA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* CTM
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'CTM'
068700030822     C                   EVAL      VABCTM=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* FFD
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'FFD'
069600030822     C                   EVAL      VABFFD=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VAD
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VAD'
070500030822     C                   EVAL      VABVAD=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* GMA
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'GMA'
071400030822     C                   EVAL      VABGMA=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* GGA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GGA'
072300030822     C                   EVAL      VABGGA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* GVA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GVA'
073200030822     C                   EVAL      VABGVA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* TC1
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'TC1'
074100030822     C                   EVAL      VABTC1=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* TC2
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'TC2'
075000030822     C                   EVAL      VABTC2=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400040714     C* VATTRC
075500040714     C                   IF        %subst(
075600040714     C                             %subst(vlrppt:posDaDft+1:
075700040714     C                             posADft-posDaDft-1):1:3)
075800040714     C                             = 'TRC'
075900040714     C                   EVAL      VATTRC=%trim(%subst(
076000040714     C                             %subst(vlrppt:posDaDft+1:
076100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200040714     C                   ENDIF
076300030822     C* ...
076400030822     C                   ENDIF
076500030822     C                   ENDDO
076600020204     C*
076700010330     C                   ENDSR
076800010607     C*----------------------------------------------------*
076900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077000010607     C*----------------------------------------------------*
077100010607     C     IMPVAB        BEGSR
077200010607     C*
077300010607     C                   SETOFF                                       3132
077400110315 xxx C***                SETON                                        35
077500030515     C*
077600010607     C                   MOVEL     datcor        VABAAS
077700010607     C                   MOVE      datcor        VABMGS
077800010607     C*
077900030715     C**********
078000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078100030715     C**********
078200030715     C                   IF        wGiro = *zeros
078300040113     C*
078400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
078500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
078700080429     C                             %trim(intcol(5))<>*blanks
078800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
079000080429     C                                      %trim(intcol(5))
079100070823     C     ';':CharCSV   XLATE     vindta        vindta
079200040510     C                   ENDIF
079300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079400031201     C     minu:maiu     XLATE     vindta        vindta
079500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079600030715     C                   DOW       posDa <= %len(%trim(vindta))
079700030715     C*
079800030715     C* Gestisco il 1° campo
079900030715     C                   IF        i = 1
080000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080100030715     C                   EVAL      posDa = 1
080200030715     C* Eseguo lo scan x trovare la fine del primo campo
080300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080400030715     C* A questo punto "estraggo" il campo corrente
080500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080600030715     C                                                    (posA-posDa))
080700030715     C* X i campi successivi al 1°
080800030715     C                   ELSE
080900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081000030715     C                   EVAL      posDa = posA + 1
081100030715     C* Eseguo lo scan x trovare la fine del campo corrente
081200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081300030715     C* Gestisco l'ultimo campo
081400030715     C                   IF        posA = *zeros
081500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081600030715     C* A questo punto "estraggo" il campo corrente
081700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081800030715     C                                                    (posA-posDa))
081900030715     C                   LEAVE
082000030715     C                   ELSE
082100030715     C* A questo punto "estraggo" il campo corrente
082200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082300030715     C                                                    (posA-posDa))
082400030715     C                   ENDIF
082500030715     C                   ENDIF
082600030715     C* Incremento il contatore d campo
082700030715     C                   EVAL      i = i +1
082800030715     C                   ENDDO
082900030715     C*
083000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083100030715     C* delimitazione testo
083200030715     C                   EVAL      i = 1
083300030715     C                   DOW       i < %elem(SkSplitFLD)
083400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
083500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083600030715     C                   EVAL      i = i + 1
083700030715     C                   ENDDO
083800030715     C                   EVAL      wGiro = 1
083900030715     C                   ELSE
084000030715     C                   EVAL      wGiro = 2
084100031204     C**********
084200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084300031204     C* d separatore campo
084400031204     C**********
084500031204     C                   EVAL      vindta = %trim(vindta)
084600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084700031204     C                   EVAL      vindta = %subst(vindta:2)
084800031204     C                   ENDDO
084900031204     C*
085000031204     C                   Z-ADD     *zeros        lunghInput        4 0
085100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085500110912     C                   ENDDO
085600030715     C**********
085700030715     C* Effettuo lo split del campo dati d input
085800030715     C**********
085900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086000030715     C                   DOW       posDa <= %len(%trim(vindta))
086100030715     C*
086200030715     C* Gestisco il 1° campo
086300030715     C                   IF        i = 1
086400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086500030715     C                   EVAL      posDa = 1
086600030715     C* Eseguo lo scan x trovare la fine del primo campo
086700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C* X i campi successivi al 1°
087200030715     C                   ELSE
087300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087400030715     C                   EVAL      posDa = posA + 1
087500031204     C* Verifico che nn vi sia il campo nn valorizzato
087600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087700031204     C* Se campo nn valorizzato skippo al prossimo
087800031204     C                   EVAL      SkSplitCSV(i) = *blanks
087900031204     C                   EVAL      posA  = posA + 1
088000031204     C                   ELSE
088100030715     C* Eseguo lo scan x trovare la fine del campo corrente
088200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088300030715     C* Gestisco l'ultimo campo
088400030715     C                   IF        posA = *zeros
088500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088600030715     C* A questo punto "estraggo" il campo corrente
088700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088800030715     C                                                    (posA-posDa))
088900030715     C                   LEAVE
089000030715     C                   ELSE
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C                   ENDIF
089500031204     C                   ENDIF
089600030715     C                   ENDIF
089700030715     C* Incremento il contatore d campo
089800030715     C                   EVAL      i = i +1
089900030715     C                   ENDDO
090000030715     C*
090100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090200030715     C* delimitazione testo
090300030715     C                   EVAL      i = 1
090400030715     C                   DOW       i < %elem(SkSplitCSV)
090500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090700030715     C                   EVAL      i = i + 1
090800030715     C                   ENDDO
090900100720     C*
091000100720     C* Stacco un progressivo x numero spedizione
091100110912 xxx C***                EXSR      repNSP
091200100326     C*
091300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
091400030715     C                   EVAL      i = 1
091500030715     C                   DOW       i < %elem(SkSplitFLD)
091600030715     C*
091700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091900030715     C                             SkSplitCSV(i) <> *blanks
092000030721     C*** ATB
092100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092200030721     C                   EVAL      VABATB = SkSplitCSV(i)
092300030721     C                   ENDIF
092400030715     C*** RSD
092500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
092700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092900030715     C* ==
093000030715     C                   ENDIF
093100030721     C*** RD2
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030715     C*** IND
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093700030715     C                   EVAL      VABIND = SkSplitCSV(i)
093800030715     C                   ENDIF
093900030715     C*** LOD
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** PRD
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
094500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030721     C*** NZD
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** RMA
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** NOT
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** NT2
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** VCA
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
096500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** VAS
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** FFD
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030721     C*** GC1
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** GC2
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** TSP
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098600030721     C                   ENDIF
098700040510     C*** CBO
098800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
098900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099000040510     C                   ENDIF
099100030721     C*** NAS
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TIC
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
099700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** GCA
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** XCO
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** CTM
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** TCR
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** CTS
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
101700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** FTM
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** VAD
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GMA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** GGA
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** GVA
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
103700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TC1
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** TC2
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** SCL
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** RMO
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** NMO
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
105700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*
106000030721     C* Reperisco quindi i campi numerici...
106100040510     C*** NSP
106200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106300040510     C                   EVAL      PiStr=SkSplitCSV(i)
106400040510     C                   EXSR      CHKNUM
106500040510     C                   IF        PiInt=*on
106600040510     C                   Z-ADD     PiVal         VABNSP
106700040510     C                   ELSE
106800040510     C                   SETON                                        31
106900040510     C                   EVAL      VABNSP = *zeros
107000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107100040510     C                             + ' ' + 'VABNSP'
107200040510     C                   ENDIF
107300040510     C                   ENDIF
107400071106     C*** RMN
107500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107600071106     C                   EVAL      PiStr=SkSplitCSV(i)
107700071106     C                   EXSR      CHKNUM
107800071106     C                   IF        PiInt=*on
107900071106     C                   Z-ADD     PiVal         VABRMN
108000071106     C                   ELSE
108100071106     C                   SETON                                        32
108200071106     C                   EVAL      VABRMN = *zeros
108300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
108400071106     C                             + ' ' + 'VABRMN'
108500071106     C                   ENDIF
108600071106     C                   ENDIF
108700030721     C*** CAD
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108900030721     C                   EVAL      PiStr=SkSplitCSV(i)
109000030721     C                   EXSR      CHKNUM
109100030721     C                   IF        PiInt=*on
109200030721     C                   Z-ADD     PiVal         Num5_0
109300030721     C                   MOVEL(p)  Num5_0        VABCAD
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      VABCAD = *zeros
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800030721     C                             + ' ' + 'VABCAD'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** NCL
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiInt=*on
110600030721     C                   Z-ADD     PiVal         VABNCL
110700030721     C                   ELSE
110800030721     C                   SETON                                        32
110900030721     C                   EVAL      VABNCL = *zeros
111000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111100040122     C                             + ' ' + 'VABNCL'
111200030721     C                   ENDIF
111300030721     C                   ENDIF
111400030721     C*** PKB
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiNum=*on
111900030721     C                   Z-ADD     PiVal         VABPKB
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      VABPKB = *zeros
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400030721     C                             + ' ' + 'VABPKB'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** CAS
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
113000111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
113100111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
113200120227 xxx C                   EVAL      VABvca = 'EUR'
113300101018     C                   EVAL      wFlgCAS = '1'
113400101018     C                   ENDIF
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiNum=*on
113800030721     C                   Z-ADD     PiVal         VABCAS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAS = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAS'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** CCM
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on      AND
115100030721     C                             PiVal<=9999999 AND
115200030721     C                             PiVal>*zeros
115300030721     C                   Z-ADD     PiVal         VABCCM
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABCCM = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800040510     C                             + ' ' + 'VABCCM'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** LNP
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on    AND
116600030721     C                             PiVal<=999   AND
116700030721     C                             PiVal>*zeros
116800030721     C                   Z-ADD     PiVal         VABLNP
116900030721     C                   ELSE
117000040715     C                   SETON                                        32
117100030721     C                   EVAL      VABLNP = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040510     C                             + ' ' + 'VABLNP'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** NRS
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on AND
118100030721     C                             PiVal<=99
118200030721     C                   Z-ADD     PiVal         VABNRS
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABNRS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** LNA
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on    AND
119400030721     C                             PiVal<=999   AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABLNA
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABLNA = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABLNA'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CTR
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on  AND
120900030721     C                             PiVal<=999
121000030721     C                   Z-ADD     PiVal         VABCTR
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABCTR'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** IAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiNum=*on
122200030721     C                   Z-ADD     PiVal         VABIAS
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABIAS'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** VLB
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABVLB
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABVLB'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** QFT
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiNum=*on
124600030721     C                   Z-ADD     PiVal         VABQFT
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABQFT'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NCD
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on      AND
125800030721     C                             PiVal<=9999999
125900030721     C                   Z-ADD     PiVal         VABNCD
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNCD'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** NCA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on      AND
127100030721     C                             PiVal<=9999999
127200030721     C                   Z-ADD     PiVal         VABNCA
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABNCA'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** ZNC
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on AND
128400030721     C                             PiVal<=99
128500030721     C                   Z-ADD     PiVal         VABZNC
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABZNC'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** DCR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on       AND
129700030721     C                             PiVal<=99999999
129800030721     C                   Z-ADD     PiVal         VABDCR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABDCR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** HCR
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on   AND
131000030721     C                             PiVal<=9999
131100030721     C                   Z-ADD     PiVal         VABHCR
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABHCR'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** VMD
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABVMD
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABVMD'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** ANT
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on        AND
133500030721     C                             PiVal<=999999999
133600030721     C                   Z-ADD     PiVal         VABANT
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABANT'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** CMO
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on
134800030721     C                   Z-ADD     PiVal         Num5_0
134900030721     C                   MOVEL(p)  Num5_0        VABCMO
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      VABCMO = *zeros
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABCMO'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700040510     C*
135800040510     C* Reperisco i campi del FIVAT
135900040510     C*** VATNOT
136000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136200040510     C                   ENDIF
136300040510     C*** VATTRC
136400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
136600040510     C                   ENDIF
136700060331     C*** VATNOT_A
136800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
136900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137000060331     C                   ENDIF
137100060331     C*** VATNOT_B
137200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137400060331     C                   ENDIF
137500060331     C*** VATNOT_E
137600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
137800060331     C                   ENDIF
137900110927     C*** VATNOT_I
138000110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
138100110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
138200110927     C                   ENDIF
138300070823     C*
138400100831     C***  ===>  Gestione campi "particolari" dentro ciclo
138500110421     C*
138600070925     C*
138700071029     C***  <===  -----------------------------------------
138800040510     C*
138900030715     C                   ENDIF
139000030715     C                   EVAL      i = i + 1
139100030715     C                   ENDDO
139200070925     C*
139300071029     C***  ===>  Gestione campi "particolari" fuori ciclo
139400100831     C*
139500070925     C*
139600071029     C***  <===  -----------------------------------------
139700070925     C*
139800070925     C* Considerazioni finali su CBO/CAS
139900100318     C                   IF        wFlgCAS = '1'    OR
140000100318     C                             VABCAS  > *zeros
140100070925     C                   IF        VABCBO = '1'
140200070925     C                   EVAL      VABCBO = '4'
140300070925     C                   ENDIF
140400070925     C                   IF        VABCBO = '2'
140500070925     C                   EVAL      VABCBO = '6'
140600070925     C                   ENDIF
140700070925     C                   ENDIF
140800070925     C***  <===  ----------------------------
140900040510     C*
141000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
141100040510     C                   IF        VABNSP = *zeros
141200040510     C                   SETON                                        31
141300040510     C                   ENDIF
141400040715     C*
141500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
141600040715     C                   IF        VABLNP = *zeros
141700040715     C                   SETON                                        31
141800040715     C                   ENDIF
141900040510     C*
142000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
142100040510     C                   IF        VABRMN = *zeros
142200040510     C                   Z-ADD     VABNSP        VABRMN
142300040510     C                   ENDIF
142400040510     C*
142500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
142600040510     C                   IF        VABRMA = *blanks
142700040714     C                   MOVEL     VABRMN        VABRMA
142800040510     C                   ENDIF
142900040128     C*
143000040510     C* Se provincia nn valorizzata la reperisco
143100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
143200110912     C                   IF        VABPRD =  *blanks AND
143300110912     C                             VABNZD =  *blanks AND
143400110912     C                             VABCAD <> *blanks AND
143500110912     C                             VABLOD <> *blanks
143600040128     C                   CLEAR                   TISI95DS
143700040128     C                   EVAL      I95TCN = '3'
143800040128     C                   Z-ADD     datcor        I95DAT
143900040128     C                   EVAL      I95NAR = VABNZD
144000040128     C                   EVAL      I95CAP = VABCAD
144100040128     C                   EVAL      I95LOC = VABLOD
144200040128     C                   CALL      'TISI95R'
144300040128     C                   PARM                    TISI95DS
144400040128     C                   EVAL      VABPRD = O95PRV
144500040510     C                   ENDIF
144600020204     C*
144700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
144800020204     C                   EXSR      CHKIMPDIV
144900030715     C                   ENDIF
145000020204     C*
145100010607     C                   ENDSR
145200010607     C*----------------------------------------------------*
145300010601
145400020204
145500020204     C*----------------------------------------------------*
145600020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
145700020204     C*----------------------------------------------------*
145800020204     C     CHKIMPDIV     BEGSR
145900020204     C*
146000020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
146100020204     C                   Z-ADD     *zeros        wrkDec            9 9
146200020204     C*
146300020204     C* Come prima cosa effettuo considerazioni sulla divisa
146400020204     C                   IF        vabIAS > *zeros
146500020204     C                   IF        vabVAS <> 'EUR'
146600020204     C                   EVAL      vabVAS =  'ITL'
146700020204     C                   ENDIF
146800020204     C                   ENDIF
146900020204     C*
147000020204     C                   IF        vabCAS > *zeros
147100020204     C                   IF        vabVCA <> 'EUR'
147200020204     C                   EVAL      vabVCA =  'ITL'
147300020204     C                   ENDIF
147400020204     C                   ENDIF
147500020204     C*
147600020204     C                   IF        vabVMD > *zeros
147700020204     C                   IF        vabVAD <> 'EUR'
147800020204     C                   EVAL      vabVAD =  'ITL'
147900020204     C                   ENDIF
148000020204     C                   ENDIF
148100020204     C*
148200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
148300020204     C                   Z-ADD     vabIAS        wrkDec
148400020204     C                   IF        wrkDec > *zeros
148500020204     C                   IF        vabVAS = 'ITL'
148600020204     C                   EVAL      vabIAS = *zeros
148700020204     C                   ENDIF
148800020204     C                   ENDIF
148900020204     C*
149000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
149100020204     C                   Z-ADD     vabCAS        wrkDec
149200020204     C                   IF        wrkDec > *zeros
149300020204     C                   IF        vabVCA = 'ITL'
149400020204     C                   EVAL      vabCAS = *zeros
149500020204     C                   ENDIF
149600020204     C                   ENDIF
149700020204     C*
149800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
149900020204     C                   Z-ADD     vabVMD        wrkDec
150000020204     C                   IF        wrkDec > *zeros
150100020204     C                   IF        vabVAD = 'ITL'
150200020204     C                   EVAL      vabVMD = *zeros
150300020204     C                   ENDIF
150400020204     C                   ENDIF
150500020204     C*
150600020204     C                   ENDSR
150700020204     C***
150800010330
150900100318
151000100318
151100100318     C*----------------------------------------------------*
151200100318     C*  CONTROLLO NUMERICITA' CAMPI
151300100318     C*----------------------------------------------------*
151400100318     C     CHKNUM        BEGSR
151500100318     C*
151600100318     C                   IF        PiDecChr = *blanks
151700100318     C                   EVAL      PiDecChr = CharNUM
151800100318     C                   ENDIF
151900100318     C*
152000100318     C                   callp     UBISNUM_Check(PiStr
152100100318     C                                          :PiDecChr
152200100318     C                                          :PiVal
152300100318     C                                          :PiNum
152400100318     C                                          :PiInt)
152500100318     C*
152600100318     C                   ENDSR
152700100318     C***
152800010601
152900010601
153000010601
153100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
153200010601     C     repfil        BEGSR
153300010601     C*
153400010601     C                   if        invfil = *zeros and
153500010601     C                             depfil > *zeros and
153600010629     C                             (vinflg = *blanks or
153700010629     C                              vinflg = *zeros)
153800010601     C
153900010601     C                   eval      invfil = depfil
154000010601     C                   endif
154100010601     C*
154200010601     C                   if        depfil <> invfil and
154300010601     C                             invfil > *zeros
154400010601     C                   eval      flgMulti = '1'
154500010601     C                   if        vinflg = *blanks
154600010601     C                   add       1             cntNonEl
154700010601     C                   endif
154800010601     C                   endif
154900010601     C*
155000010601     C                   if        vinflg = '2'
155100010601     C                   eval      flgStato = '2'
155200010601     C                   endif
155300010601     C*
155400010601     C                   ENDSR
155500010601     C***
155600010601
155700010601
155800010601
155900010330
156000040119      /TITLE Invio dei dati al punto operativo.
156100040119     C     invio         BEGSR
156200040119     C*
156300040119     C* 1° invio FIVAT
156400040119     C                   reset                   dscmz
156500040119     C                   move      vlrpoi        cmzdst
156600040119     C                   eval      cmzfld = 'FIVATWWR'
156700040119     C                   eval      cmzmbd = vlrhdl
156800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156900040119     C***                if        prmfir = *blanks
157000040119     C                   eval      cmzfla = 'FIVAT00F'
157100040119     C                   eval      cmzmba = 'FIVAT00F'
157200040119     C***                else
157300040119     C***                eval      cmzfla = prmfir
157400040119     C***                eval      cmzmba = prmfir
157500040119     C***                endif
157600040119     C                   eval      cmznrr = *zeros
157700040119     C                   move      §ctrokvt      cmznrr
157800040119     C                   eval      cmzlba = vlrfl1
157900040119     C                   call(e)   'TIS711C'
158000040119     C                   parm                    dscmz
158100040119     C                   parm      *blanks       esito
158200040119     C                   if        %error
158300040119     C                             or cmzerr = '1'
158400040119     C                             or esito  = '1'
158500040119     C                   eval      wrkesito = '3'
158600040119     C                   else
158700040119     C*
158800040119     C* 2° invio FIVAB
158900040119     C                   reset                   dscmz
159000040119     C                   move      vlrpoi        cmzdst
159100040119     C                   eval      cmzfld = vlrfou
159200040119     C                   eval      cmzmbd = vlrhdl
159300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159400040119     C***                if        prmfir = *blanks
159500040119     C                   eval      cmzfla = 'FIVAB00F'
159600040119     C                   eval      cmzmba = 'FIVAB00F'
159700040119     C***                else
159800040119     C***                eval      cmzfla = prmfir
159900040119     C***                eval      cmzmba = prmfir
160000040119     C***                endif
160100040119     C                   eval      cmznrr = *zeros
160200040119     C                   move      §ctrokvb      cmznrr
160300040119     C                   eval      cmzlba = vlrfl1
160400040119     C                   call(e)   'TIS711C'
160500040119     C                   parm                    dscmz
160600040119     C                   parm      *blanks       esito
160700040119     C                   if        %error
160800040119     C                             or cmzerr = '1'
160900040119     C                             or esito  = '1'
161000040119     C                   eval      wrkesito = '3'
161100040119     C                   endif
161200040119     C                   endif
161300040119     C*
161400040119     C                   ENDSR
161500040119     C***
161600010601
161700010601
161800010601
161900010601
162000010601      /TITLE Invio dei dati al punto operativo.
162100010601     C     opeini        BEGSR
162200010601     C*
162300010601     C* Inizializzo flag e contatori operativi
162400010601     C                   movel     '0'           flgGiro           1
162500010601     C                   movel     '0'           flgMulti          1
162600010601     C                   movel     '1'           flgStato          1
162700010615     C                   movel     '0'           flgOk             1
162800010601     C                   z-add     *zeros        cntNonEl         10 0
162900010601     C                   z-add     *zeros        depfil            3 0
163000010601     C                   z-add     *zeros        invfil            3 0
163100010601     C*
163200010601     C                   ENDSR
163300010601     C***
163400070326
163500070326
163600070326
163700070326
163800070326     C     *pssr         BEGSR
163900070329     C*
164000070329     C                   if        %open(tivin00r)
164100070329     C                   close     tivin00r
164200070329     C                   endif
164300070329     C                   if        %open(fivabwwr)
164400070329     C                   close     fivabwwr
164500070329     C                   endif
164600070329     C                   if        %open(fivatwwr)
164700070329     C                   close     fivatwwr
164800070329     C                   endif
164900070326     C*
165000070326     C* Effettuo la chiamata al CLLE preposto
165100070326     C                   call(e)   'TITVVTC'
165200070326     C                   parm                    parccm
165300070326     C                   parm                    parmbr
165400070326     C                   parm      '2'           paropz
165500070326     C*
165600070326     C                   eval      wrkesito = '2'
165700070404     C*
165800070404     C                   seton                                        LR
165900070326     C*
166000070326     C                   ENDSR     '*CANCL'
166100070326     C***
166200100318
166300100318
166400100318
166500100318     C     normCSV       BEGSR
166600100318     C*
166700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
166800100318     C* separatori campi
166900100318     C                   if        CharTXT <> *blanks
167000100318     C                   z-add     *zeros        wLenVINDTA        4 0
167100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
167200100318     C                   setoff                                       80
167300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
167400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
167500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
167600100318     C                   if        not *in80
167700100318     C                   seton                                        80
167800100318     C                   else
167900100318     C                   setoff                                       80
168000100318     C                   endif
168100100318     C                   endif
168200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
168300100318     C                             and *in80 = *on
168400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
168500100318     C                   endif
168600100318     C                   enddo
168700100318     C                   endif
168800100318     C*
168900100318     C                   ENDSR
169000100318     C***
169100070326
169200100326
169300100326
169400100326     C*----------------------------------------------------*
169500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
169600100326     C*----------------------------------------------------*
169700100326     C     REPNSP        BEGSR
169800100326     C*
169900100326     C* NSP => Stacco un numeratore da AZNUM
170000100326     C                   clear                   TRUL33DS
170100100326     C                   eval      I33OPE = *zeros
170200100326     C                   eval      I33CNU = 302
170300100326     C                   eval      I33NUM = 1
170400100326     C                   movel     TRUL33DS      KPJBU
170500100326     C                   call      'TRUL33R'
170600100326     C                   parm                    KPJBA
170700100326     C                   movel     KPJBU         TRUL33DS
170800100326     C                   if        O33ERR = *zeros
170900100326     C                   z-add     O33NRF        VABNSP
171000100326     C                   z-add     O33NRF        VATNSP
171100100326     C                   else
171200100326     C                   SETON                                        31
171300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
171400100326     C                             + ' ' + 'VABNSP VATNSP'
171500100326     C                   endif
171600100326     C*
171700100326     C                   ENDSR
171800010330
171900100326
172000010330
172100000613     C     *inzsr        BEGSR
172200990910     C*
172300990910     C     *entry        plist
172400990920     C                   parm                    tivlrds
172500990921     C                   parm      wrkesito      esito
172600000724     C                   parm                    prmlit
172700000710     C                   parm                    prmfir
172800010330     C*
172900010330     C* CALCOLA LA DATA CORRENTE
173000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
173100100318     C                   eval      datcor = %dec(%date() : *ISO)
173200000613     C*
173300000613     C                   ENDSR
173400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173500120227VABNSP;FIL001;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;VABCAS;
173600120227
173700120227
173800111116
173900100318
