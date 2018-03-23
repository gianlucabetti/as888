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
005900110927     D wVATNOT_I       s                   LIKE(VATNOT)
006000140902     D wVATNOT_S       s                   LIKE(VATNOT)
006100030822     D*------------------
006200030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006300030822     D*------------------
006400030822     D posDaDft        S              3  0 INZ(*zeros)
006500030822     D posADft         S              3  0 INZ(*zeros)
006600030822     D j               s              3  0 INZ(1)
006700030822     D wGiroDft        s              1  0 INZ(*zeros)
006800080418     D*------------------
006900080418     D* Costanti
007000080418     D*------------------
007100080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007200080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007300100318
007400100318     D*------------------
007500100318     D* LINKING A DEFINIZIONI ESTERNE
007600100318     D*------------------
007700100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007800100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007900100318
008000100909     D*------------------
008100100909     D* DS CALCOLO CHECK-DIGITS
008200100909     D*------------------
008300100909     D TRUL28DSX     e ds                  inz
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
013600010601     C***
013700000613
013800010601
013900010601
014000010330     C*--------------------------------------------------------
014100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014200010330     C*--------------------------------------------------------
014300010605     C     RWVAB         BEGSR
014400010330     C*
014500010330     C                   if        not %open(tivin00r)
014600010330     C                   open      tivin00r
014700010330     C                   endif
014800021025     C                   if        not %open(fivabwwr)
014900021025     C                   open      fivabwwr
015000010330     C                   endif
015100040119     C*
015200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015300040119     C                   exsr      prevat
015400040119     C*
015500040119     C                   if        chkcall = '0'
015600040119     C*
015700040119     C                   if        not %open(fivatwwr)
015800040119     C                   open      fivatwwr
015900040119     C                   endif
016000010330     C*
016100040119     C                   clear                   §CTROKVB          7 0
016200040119     C                   clear                   §CTROKVT          7 0
016300040119     C                   clear                   §CTRMO            7 0
016400040119     C                   clear                   §CTRNO            7 0
016500070823     C*
016600070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016700070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016800111025     C                   exsr      inzvar
016900111025     C                   exsr      impvab
017000110131     C*
017100110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017200110912 xxx C***                read      tivin00r                               70
017300010330     C*
017400030822     C                   do        *HIVAL
017500010330     C*
017600030822     C                   read      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200040510     C*
018300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018400040510     C                   z-add     *zeros        wLenVINDTA        4 0
018500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018900040510     C                   z-add     1             wFlgVINDTA
019000040510     C                   leave
019100040510     C                   endif
019200040510     C                   enddo
019300010618     C*
019400040510     C                   if        vindta > *blanks AND
019500040510     C                             wFlgVINDTA = 1
019600010330     C                   add       1             rrnum
019700010330     C*
019800010601     C                   if        *in70 = *off and
019900010330     C                             (vinflg = *blanks
020000010330     C                              or vinflg = '0'
020100010330     C                              or vinflg = '2')
020200010330     C*
020300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020400010711     C                   if        vinflg = *blanks or vinflg = '0'
020500010711     C                   clear                   vinmsg
020600010711     C                   endif
020700010601     C*
020800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020900070925     C                   eval      vindta  = ' ' + %trim(vindta)
021000070925     C*
021100080527     C                   exsr      inzvar
021200100318     C                   exsr      normCSV
021300010605     C                   exsr      impvab
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600010605     C                   eval      depfil = VABLNP
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000030822     C                   move(P)   invfil        VABFGS
022100021025     C                   else
022200030822     C                   move(P)   vlrpoi        VABFGS
022300021025     C                   endif
022400010601     C*
022500010601     C                   exsr      PREELA
022600010601     C*
022700010604     C* Ebbene...
022800010604     C*
022900040119     C  N31              add       1             §CTROKVB
023000040119     C   32              add       1             §CTRMO
023100040119     C   31              add       1             §CTRNO
023200080418     C*
023300030822     C                   if        wGiro = 2
023400080513     C* Scrivo sempre il VAT
023500080513     C  N31              exsr      wrivat
023600110201     C                   exsr      chkvab
023700080513     C                   endif
023800010604     C*
023900010604     C                   if        *in31 = *off and
024000010604     C                             *in32 = *off
024100010604     C                   eval      vinflg = '1'
024200010604     C                   else
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
025600040119     C*
025700080527     C* Verifico dati rimasti "in cannna"...
025800080527     C                   seton                                        65
025900110201     C                   exsr      chkvab
026000080527     C*
026100040119     C                   endif                                                  (endif - chkcall)
026200010601     C*
026300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026400010601     C                   if        cntNonEl = *zeros or
026500010601     C                             flgMulti = '0'
026600010330     C* Se non ci sono record con errori ...
026700010601     C                   if        §ctrno = 0 and
026800010604     C                             §ctrmo = 0 and
026900010601     C                             flgStato <> '2'
027000010330     C* ... restituisco esito OK.
027100010330     C                   eval      wrkesito = '0'
027200010330     C                   else
027300040119     C                   if        §ctrokvb > 0
027400010330     C                   eval      wrkesito = '1'
027500010330     C                   else
027600010615     C                   if        flgOk = '0'
027700010615     C                   eval      wrkesito = '2'
027800010615     C                   else
027900010615     C                   eval      wrkesito = '6'
028000010615     C                   endif
028100010330     C                   endif
028200010330     C                   endif
028300010601     C                   else
028400010601     C                   eval      wrkesito = '9'
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        %open(tivin00r)
028800010330     C                   close     tivin00r
028900010330     C                   endif
029000021025     C                   if        %open(fivabwwr)
029100021025     C                   close     fivabwwr
029200010330     C                   endif
029300040512     C                   if        %open(fivatwwr)
029400040512     C                   close     fivatwwr
029500040512     C                   endif
029600010601     C*
029700010601     C                   if        vlrpoi <> 999
029800010601     C                   eval      invfil = vlrpoi
029900010601     C                   endif
030000010330     C*
030100040119     C                   if        §ctrokvb > 0
030200010601     C                             and invfil > *zeros
030300010330     C                   exsr      invio
030400010330     C                   endif
030500010601     C*
030600010618     C                   if        flgGiro = '1'
030700010601     C                   exsr      endela
030800010618     C                   endif
030900010330     C*
031000010330     C                   ENDSR
031100010330     C***
031200080527
031300080527
031400080527
031500080527     C*----------------------------------------------------*
031600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031700080527     C*----------------------------------------------------*
031800100413     C     CHKVAB        BEGSR
031900110201     C*
032000110201     C* Se richiesti ignorare colli / peso / volume
032100110315 xxx C***                if        *in35
032200110315 xxx C***                z-add     *zeros        VABNCL
032300110315 xxx C***                z-add     *zeros        VABPKB
032400110315 xxx C***                z-add     *zeros        VABVLB
032500110315 xxx C***                endif
032600080527     C*
032700080527     C* Scrivo il VAB solo a rottura d spedizione
032800080527     C                   if        depspe = *zeros
032900080527     C  N31              eval      fivabds_sav = fivabds
033000080527     C                   eval      depspe = vabNSP
033100080527     C                   else
033200080527     C                   if        depspe = vabNSP AND *in65 = *off
033300100413     C   51              add       VABNCL        sav_VABNCL
033400100413     C   51              add       VABPKB        sav_VABPKB
033500100413     C   51              add       VABVLB        sav_VABVLB
033600100720     C   52              z-add     VABNCL        sav_VABNCL
033700100720     C   52              z-add     VABPKB        sav_VABPKB
033800100720     C   52              z-add     VABVLB        sav_VABVLB
033900100720     C   53              add       VABNCL        sav_VABNCL
034000100720     C   53              z-add     VABPKB        sav_VABPKB
034100100720     C   53              z-add     VABVLB        sav_VABVLB
034200080527     C                   else
034300080527     C  N31              eval      fivabds_ok = fivabds_sav
034400080527     C  N31              exsr      wrivab
034500080527     C  N31              eval      fivabds_sav = fivabds
034600080527     C                   eval      depspe = vabNSP
034700080527     C                   endif
034800080527     C                   endif
034900080527     C*
035000080527     C                   ENDSR
035100040119
035200040119
035300040119
035400040119     C*----------------------------------------------------*
035500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035600040119     C*----------------------------------------------------*
035700040510     C     WRIVAB        BEGSR
035800080513     C*
035900080513     C* Gestisco l'eventuale rottura x numero spedizione
036000080527     C                   if        VABNSP <> depspe or *in65 = *on
036100070301     C                   if        wVATNOT_E <> *blanks
036200070301     C                   eval      VABCTM = '7Q'
036300070301     C                   endif
036400100413     C                   write     fivab000
036500080513     C                   endif
036600040119     C*
036700040119     C                   ENDSR
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037300040119     C*----------------------------------------------------*
037400040510     C     WRIVAT        BEGSR
037500060512     C*
037600060512     C* Inizializzo i campi chiave
037700060512     C                   eval      VATFGS = VABFGS
037800060512     C                   eval      VATAAS = VABAAS
037900060512     C                   eval      VATCCM = VABCCM
038000070823     C                   eval      VATNRS = VABNRS
038100060512     C                   eval      VATNSP = VABNSP
038200060512     C                   eval      VATLNP = VABLNP
038300040119     C*
038400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038500040510     C                   if        VATTRC = *blanks
038600040510     C                   eval      VATTRC = 'E'
038700040510     C                   endif
038800110318     C                   if        VATNOT <> *blanks
038900040510     C                   write     FIVAT000
039000040119     C                   add       1             §CTROKVT
039100110318     C                   endif
039200060331     C*
039300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039400060331     C                   if        wVATNOT_A <> *blanks
039500060331     C                   eval      VATNOT = wVATNOT_A
039600060331     C                   eval      VATTRC = 'A'
039700060331     C                   write     FIVAT000
039800060331     C                   add       1             §CTROKVT
039900060331     C                   endif
040000060331     C*
040100060331     C                   if        wVATNOT_B <> *blanks
040200060331     C                   eval      VATNOT = wVATNOT_B
040300060331     C                   eval      VATTRC = 'B'
040400060331     C                   write     FIVAT000
040500060331     C                   add       1             §CTROKVT
040600060331     C                   endif
040700060331     C*
040800060331     C                   if        wVATNOT_E <> *blanks
040900110201     C                   eval      VATNOT = wVATNOT_E
041000060331     C                   eval      VATTRC = 'E'
041100110201     C                   write     FIVAT000
041200110201     C                   add       1             §CTROKVT
041300110201     C                   endif
041400110927     C*
041500110927     C                   if        wVATNOT_I <> *blanks
041600110927     C                   eval      VATNOT = wVATNOT_I
041700110927     C                   eval      VATTRC = 'I'
041800110927     C                   write     FIVAT000
041900110927     C                   add       1             §CTROKVT
042000110927     C                   endif
042100140902     C*
042200140902     C                   if        wVATNOT_S <> *blanks
042300140902     C                   eval      VATNOT = wVATNOT_S
042400140902     C                   eval      VATTRC = 'S'
042500140902     C                   write     FIVAT000
042600140902     C                   add       1             §CTROKVT
042700140902     C                   endif
042800040119     C*
042900040119     C                   ENDSR
043000040119
043100040119
043200040119
043300040119     C*----------------------------------------------------*
043400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043500040119     C*----------------------------------------------------*
043600040119     C     PREVAT        BEGSR
043700040119     C*
043800040119     C* Compongo il nome del membro da dare al FIVATWWR
043900040119     C                   eval      parmbr = vlrhdl
044000040119     C                   movel     'M'           parmbr
044100040510     C                   eval      parccm = vlrksc
044200040119     C                   eval      paropz = '1'
044300040119     C* Effettuo la chiamata al CLLE preposto
044400040119     C                   call(e)   'TITVVTC'
044500040119     C                   parm                    parccm
044600040119     C                   parm                    parmbr
044700040119     C                   parm                    paropz
044800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044900040119     C                   if        %error
045000040119     C                   movel     '1'           chkcall
045100040119     C                   else
045200040119     C                   movel     '0'           chkcall
045300040119     C                   endif
045400040119     C*
045500040119     C                   ENDSR
045600010601
045700010601
045800010601
045900010330     C*----------------------------------------------------*
046000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046100010330     C*----------------------------------------------------*
046200040119     C     INZVAR        BEGSR
046300080527     C*
046400080527     C                   setoff                                       65
046500010330     C*
046600030822     C* Inizializzo variabili di wrk
046700020204     C                   Z-ADD     *zeros        Num5_0            5 0
046800101018     C                   MOVEL     *blanks       wFlgCAS
046900060331     C                   MOVEL     *blanks       wVATNOT_A
047000060331     C                   MOVEL     *blanks       wVATNOT_B
047100060331     C                   MOVEL     *blanks       wVATNOT_E
047200110927     C                   MOVEL     *blanks       wVATNOT_I
047300140902     C                   MOVEL     *blanks       wVATNOT_S
047400030822     C*
047500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047600030822     C                   CLEAR                   SkSplitCSV
047700030822     C                   EVAL      i = 1
047800030822     C                   EVAL      posDa = *zeros
047900030822     C                   EVAL      posA  = *zeros
048000080527     C*
048100080527     C                   CLEAR                   FIVABDS
048200080527     C                   CLEAR                   FIVAT000
048300030822     C*
048400030822     C* Reimposto i valori di default
048500030822     C                   EXSR      DEFCAM
048600030822     C*
048700010330     C                   ENDSR
048800010330     C*----------------------------------------------------*
048900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049000010330     C*----------------------------------------------------*
049100010330     C     DEFCAM        BEGSR
049200030822     C*
049300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049400030715     C* e delimitatore testo.
049500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049800100318     C*
049900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050000100318     C                   EVAL      CharSOS = CharNUM
050100030822     C*
050200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050300100720     C                   SETOFF                                       505152
050400100720     C                   SETOFF                                       53
050500100413     C                   SELECT
050600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050700100720     C                   SETON                                        50
050800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
050900100413     C                   SETON                                        51
051000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051100100720     C                   SETON                                        52
051200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051300100720     C                   SETON                                        53
051400100413     C                   ENDSL
051500030822     C*
051600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051700030822     C                   EVAL      posDaDft = 1
051800030822     C                   EVAL      posADft  = 0
051900030822     C                   EVAL      wGiroDft = 0
052000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052100030822     C                             posDaDft > 0
052200030822     C*
052300030822     C* Gestisco il 1° giro
052400030822     C                   IF        wGiroDft = 0
052500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052700030822     C* Incremento il contatore dei "giri"
052800030822     C                   EVAL      wGiroDft = 1
052900030822     C                   ELSE
053000030822     C                   EVAL      posDaDft = posADft
053100030822     C                   ENDIF
053200030822     C* Eseguo lo scan x trovare la fine del campo corrente
053300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053400030822     C*
053500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053700030822     C                   IF        posDaDft > 0 AND
053800030822     C                             posADft  > 0
053900030822     C* NCL
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'NCL'
054400030822     C                   EVAL      PiStr=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   EXSR      CHKNUM
054800030822     C                   IF        PiInt=*on
054900030822     C                   Z-ADD     PiVal         VABNCL
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* CCM
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'CCM'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABCCM
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* LNP
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'LNP'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABLNP
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* NRS
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'NRS'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABNRS
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* CTR
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'CTR'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABCTR
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* PKB
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'PKB'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiNum=*on
061400030822     C                   Z-ADD     PiVal         VABPKB
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* VLB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'VLB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABVLB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* QFT
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'QFT'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABQFT
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* CBO
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'CBO'
064800030822     C                   EVAL      VABCBO=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* TSP
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'TSP'
065700030822     C                   EVAL      VABTSP=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* VAS
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'VAS'
066600030822     C                   EVAL      VABVAS=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* VCA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'VCA'
067500030822     C                   EVAL      VABVCA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900070925     C* TIC
068000070925     C                   IF        %subst(
068100070925     C                             %subst(vlrppt:posDaDft+1:
068200070925     C                             posADft-posDaDft-1):1:3)
068300070925     C                             = 'TIC'
068400070925     C                   EVAL      VABTIC=%trim(%subst(
068500070925     C                             %subst(vlrppt:posDaDft+1:
068600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700070925     C                   ENDIF
068800030822     C* GCA
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'GCA'
069300030822     C                   EVAL      VABGCA=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* CTM
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'CTM'
070200030822     C                   EVAL      VABCTM=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600030822     C* FFD
070700030822     C                   IF        %subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):1:3)
071000030822     C                             = 'FFD'
071100030822     C                   EVAL      VABFFD=%trim(%subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400030822     C                   ENDIF
071500030822     C* VAD
071600030822     C                   IF        %subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):1:3)
071900030822     C                             = 'VAD'
072000030822     C                   EVAL      VABVAD=%trim(%subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072300030822     C                   ENDIF
072400030822     C* GMA
072500030822     C                   IF        %subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):1:3)
072800030822     C                             = 'GMA'
072900030822     C                   EVAL      VABGMA=%trim(%subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073200030822     C                   ENDIF
073300030822     C* GGA
073400030822     C                   IF        %subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):1:3)
073700030822     C                             = 'GGA'
073800030822     C                   EVAL      VABGGA=%trim(%subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074100030822     C                   ENDIF
074200030822     C* GVA
074300030822     C                   IF        %subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):1:3)
074600030822     C                             = 'GVA'
074700030822     C                   EVAL      VABGVA=%trim(%subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075000030822     C                   ENDIF
075100030822     C* TC1
075200030822     C                   IF        %subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):1:3)
075500030822     C                             = 'TC1'
075600030822     C                   EVAL      VABTC1=%trim(%subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075900030822     C                   ENDIF
076000030822     C* TC2
076100030822     C                   IF        %subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):1:3)
076400030822     C                             = 'TC2'
076500030822     C                   EVAL      VABTC2=%trim(%subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076800030822     C                   ENDIF
076900040714     C* VATTRC
077000040714     C                   IF        %subst(
077100040714     C                             %subst(vlrppt:posDaDft+1:
077200040714     C                             posADft-posDaDft-1):1:3)
077300040714     C                             = 'TRC'
077400040714     C                   EVAL      VATTRC=%trim(%subst(
077500040714     C                             %subst(vlrppt:posDaDft+1:
077600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077700040714     C                   ENDIF
077800030822     C* ...
077900030822     C                   ENDIF
078000030822     C                   ENDDO
078100020204     C*
078200010330     C                   ENDSR
078300010607     C*----------------------------------------------------*
078400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078500010607     C*----------------------------------------------------*
078600010607     C     IMPVAB        BEGSR
078700010607     C*
078800010607     C                   SETOFF                                       3132
078900110315 xxx C***                SETON                                        35
079000030515     C*
079100010607     C                   MOVEL     datcor        VABAAS
079200010607     C                   MOVE      datcor        VABMGS
079300010607     C*
079400030715     C**********
079500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079600030715     C**********
079700030715     C                   IF        wGiro = *zeros
079800040113     C*
079900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080200080429     C                             %trim(intcol(5))<>*blanks
080300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080500080429     C                                      %trim(intcol(5))
080600070823     C     ';':CharCSV   XLATE     vindta        vindta
080700040510     C                   ENDIF
080800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080900031201     C     minu:maiu     XLATE     vindta        vindta
081000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081100030715     C                   DOW       posDa <= %len(%trim(vindta))
081200030715     C*
081300030715     C* Gestisco il 1° campo
081400030715     C                   IF        i = 1
081500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081600030715     C                   EVAL      posDa = 1
081700030715     C* Eseguo lo scan x trovare la fine del primo campo
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* A questo punto "estraggo" il campo corrente
082000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082100030715     C                                                    (posA-posDa))
082200030715     C* X i campi successivi al 1°
082300030715     C                   ELSE
082400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082500030715     C                   EVAL      posDa = posA + 1
082600030715     C* Eseguo lo scan x trovare la fine del campo corrente
082700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082800030715     C* Gestisco l'ultimo campo
082900030715     C                   IF        posA = *zeros
083000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C                   LEAVE
083500030715     C                   ELSE
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   ENDIF
084000030715     C                   ENDIF
084100030715     C* Incremento il contatore d campo
084200030715     C                   EVAL      i = i +1
084300030715     C                   ENDDO
084400030715     C*
084500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084600030715     C* delimitazione testo
084700030715     C                   EVAL      i = 1
084800030715     C                   DOW       i < %elem(SkSplitFLD)
084900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085100030715     C                   EVAL      i = i + 1
085200030715     C                   ENDDO
085300030715     C                   EVAL      wGiro = 1
085400030715     C                   ELSE
085500030715     C                   EVAL      wGiro = 2
085600031204     C**********
085700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085800031204     C* d separatore campo
085900031204     C**********
086000031204     C                   EVAL      vindta = %trim(vindta)
086100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086200031204     C                   EVAL      vindta = %subst(vindta:2)
086300031204     C                   ENDDO
086400031204     C*
086500031204     C                   Z-ADD     *zeros        lunghInput        4 0
086600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087000110912     C                   ENDDO
087100030715     C**********
087200030715     C* Effettuo lo split del campo dati d input
087300030715     C**********
087400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087500030715     C                   DOW       posDa <= %len(%trim(vindta))
087600030715     C*
087700030715     C* Gestisco il 1° campo
087800030715     C                   IF        i = 1
087900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088000030715     C                   EVAL      posDa = 1
088100030715     C* Eseguo lo scan x trovare la fine del primo campo
088200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C* X i campi successivi al 1°
088700030715     C                   ELSE
088800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088900030715     C                   EVAL      posDa = posA + 1
089000031204     C* Verifico che nn vi sia il campo nn valorizzato
089100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089200031204     C* Se campo nn valorizzato skippo al prossimo
089300031204     C                   EVAL      SkSplitCSV(i) = *blanks
089400031204     C                   EVAL      posA  = posA + 1
089500031204     C                   ELSE
089600030715     C* Eseguo lo scan x trovare la fine del campo corrente
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* Gestisco l'ultimo campo
089900030715     C                   IF        posA = *zeros
090000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090100030715     C* A questo punto "estraggo" il campo corrente
090200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090300030715     C                                                    (posA-posDa))
090400030715     C                   LEAVE
090500030715     C                   ELSE
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   ENDIF
091000031204     C                   ENDIF
091100030715     C                   ENDIF
091200030715     C* Incremento il contatore d campo
091300030715     C                   EVAL      i = i +1
091400030715     C                   ENDDO
091500030715     C*
091600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091700030715     C* delimitazione testo
091800030715     C                   EVAL      i = 1
091900030715     C                   DOW       i < %elem(SkSplitCSV)
092000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092200030715     C                   EVAL      i = i + 1
092300030715     C                   ENDDO
092400100720     C*
092500100720     C* Stacco un progressivo x numero spedizione
092600110912 xxx C***                EXSR      repNSP
092700100326     C*
092800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitFLD)
093100030715     C*
093200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093400030715     C                             SkSplitCSV(i) <> *blanks
093500030721     C*** ATB
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093700030721     C                   EVAL      VABATB = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030715     C*** RSD
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094400030715     C* ==
094500030715     C                   ENDIF
094600030721     C*** RD2
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030715     C*** IND
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095200030715     C                   EVAL      VABIND = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** LOD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030715     C*** PRD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030721     C*** NZD
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030715     C*** RMA
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** NOT
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** NT2
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** VCA
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** VAS
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** FFD
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030721     C*** GC1
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** GC2
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** TSP
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100100030721     C                   ENDIF
100200040510     C*** CBO
100300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100500040510     C                   ENDIF
100600030721     C*** NAS
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** TIC
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** GCA
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** XCO
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** CTM
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TCR
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTS
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** FTM
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** VAD
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GMA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GGA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GVA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TC1
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** TC2
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** SCL
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** RMO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** NMO
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*
107500030721     C* Reperisco quindi i campi numerici...
107600040510     C*** NSP
107700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107800040510     C                   EVAL      PiStr=SkSplitCSV(i)
107900040510     C                   EXSR      CHKNUM
108000040510     C                   IF        PiInt=*on
108100040510     C                   Z-ADD     PiVal         VABNSP
108200040510     C                   ELSE
108300040510     C                   SETON                                        31
108400040510     C                   EVAL      VABNSP = *zeros
108500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040510     C                             + ' ' + 'VABNSP'
108700040510     C                   ENDIF
108800040510     C                   ENDIF
108900071106     C*** RMN
109000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109100071106     C                   EVAL      PiStr=SkSplitCSV(i)
109200071106     C                   EXSR      CHKNUM
109300071106     C                   IF        PiInt=*on
109400071106     C                   Z-ADD     PiVal         VABRMN
109500071106     C                   ELSE
109600071106     C                   SETON                                        32
109700071106     C                   EVAL      VABRMN = *zeros
109800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109900071106     C                             + ' ' + 'VABRMN'
110000071106     C                   ENDIF
110100071106     C                   ENDIF
110200030721     C*** CAD
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on
110700030721     C                   Z-ADD     PiVal         Num5_0
110800030721     C                   MOVEL(p)  Num5_0        VABCAD
110900030721     C                   ELSE
111000030721     C                   SETON                                        32
111100030721     C                   EVAL      VABCAD = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300030721     C                             + ' ' + 'VABCAD'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NCL
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on
112100030721     C                   Z-ADD     PiVal         VABNCL
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      VABNCL = *zeros
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600040122     C                             + ' ' + 'VABNCL'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** PKB
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiNum=*on
113400030721     C                   Z-ADD     PiVal         VABPKB
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABPKB = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900030721     C                             + ' ' + 'VABPKB'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** CAS
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114500110506     C                             %trim(SkSplitCSV(i)) <> *zeros
114600101018     C                   EVAL      wFlgCAS = '1'
114700101018     C                   ENDIF
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiNum=*on
115100030721     C                   Z-ADD     PiVal         VABCAS
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABCAS = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600030721     C                             + ' ' + 'VABCAS'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** CCM
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiInt=*on      AND
116400030721     C                             PiVal<=9999999 AND
116500030721     C                             PiVal>*zeros
116600030721     C                   Z-ADD     PiVal         VABCCM
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      VABCCM = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100040510     C                             + ' ' + 'VABCCM'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** LNP
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on    AND
117900030721     C                             PiVal<=999   AND
118000030721     C                             PiVal>*zeros
118100030721     C                   Z-ADD     PiVal         VABLNP
118200030721     C                   ELSE
118300040715     C                   SETON                                        32
118400030721     C                   EVAL      VABLNP = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040510     C                             + ' ' + 'VABLNP'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** NRS
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on AND
119400030721     C                             PiVal<=99
119500030721     C                   Z-ADD     PiVal         VABNRS
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABNRS'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** LNA
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on    AND
120700030721     C                             PiVal<=999   AND
120800030721     C                             PiVal>*zeros
120900030721     C                   Z-ADD     PiVal         VABLNA
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      VABLNA = *zeros
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABLNA'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** CTR
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiInt=*on  AND
122200030721     C                             PiVal<=999
122300030721     C                   Z-ADD     PiVal         VABCTR
122400030721     C                   ELSE
122500030721     C                   SETON                                        32
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABCTR'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** IAS
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiNum=*on
123500030721     C                   Z-ADD     PiVal         VABIAS
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABIAS'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** VLB
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiNum=*on
124700030721     C                   Z-ADD     PiVal         VABVLB
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABVLB'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** QFT
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABQFT
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABQFT'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** NCD
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on      AND
127100030721     C                             PiVal<=9999999
127200030721     C                   Z-ADD     PiVal         VABNCD
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABNCD'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** NCA
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on      AND
128400030721     C                             PiVal<=9999999
128500030721     C                   Z-ADD     PiVal         VABNCA
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABNCA'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** ZNC
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on AND
129700030721     C                             PiVal<=99
129800030721     C                   Z-ADD     PiVal         VABZNC
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABZNC'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** DCR
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on       AND
131000030721     C                             PiVal<=99999999
131100030721     C                   Z-ADD     PiVal         VABDCR
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABDCR'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** HCR
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiInt=*on   AND
132300030721     C                             PiVal<=9999
132400030721     C                   Z-ADD     PiVal         VABHCR
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABHCR'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100030721     C*** VMD
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiNum=*on
133600030721     C                   Z-ADD     PiVal         VABVMD
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABVMD'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** ANT
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on        AND
134800030721     C                             PiVal<=999999999
134900030721     C                   Z-ADD     PiVal         VABANT
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABANT'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** CMO
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on
136100030721     C                   Z-ADD     PiVal         Num5_0
136200030721     C                   MOVEL(p)  Num5_0        VABCMO
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      VABCMO = *zeros
136600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136700030721     C                             + ' ' + 'VABCMO'
136800030721     C                   ENDIF
136900030721     C                   ENDIF
137000040510     C*
137100040510     C* Reperisco i campi del FIVAT
137200040510     C*** VATNOT
137300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137500040510     C                   ENDIF
137600040510     C*** VATTRC
137700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137900040510     C                   ENDIF
138000060331     C*** VATNOT_A
138100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400060331     C*** VATNOT_B
138500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138700060331     C                   ENDIF
138800060331     C*** VATNOT_E
138900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139100060331     C                   ENDIF
139200110927     C*** VATNOT_I
139300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
139400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
139500110927     C                   ENDIF
139600140902     C*** VATNOT_S
139700140902     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
139800140902     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
139900140902     C                   ENDIF
140000070823     C*
140100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140200110421     C*
140300111025     C*** RMN / NSP
140400111025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
140500111025     C                   EVAL      PiStr=SkSplitCSV(i)
140600111025     C                   EXSR      CHKNUM
140700111025     C                   IF        PiInt=*on
140800111025     C                   Z-ADD     PiVal         VABNSP
140900111025     C                   Z-ADD     PiVal         VABRMN
141000111025     C                   ELSE
141100111025     C                   Z-ADD     *zeros        VABNSP
141200111025     C                   Z-ADD     *zeros        VABRMN
141300111025     C                   SETON                                        31
141400111025     C                   EVAL      vinmsg = %trimr(vinmsg)
141500111025     C                             + ' ' + 'VABNSP VABRMN'
141600111025     C                   ENDIF
141700111025     C                   ENDIF
141800111025     C*** IAS
141900111025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIAS'
142000110912     C                   EVAL      PiStr=SkSplitCSV(i)
142100110912     C                   EXSR      CHKNUM
142200111025     C                   IF        PiNum=*on
142300111025     C                   EVAL      PiVal = PiVal / 100
142400111025     C                   Z-ADD     PiVal         VABIAS
142500110912     C                   ELSE
142600111025     C                   SETON                                        32
142700110912     C                   EVAL      vinmsg = %trimr(vinmsg)
142800111025     C                             + ' ' + 'VABIAS'
142900110912     C                   ENDIF
143000110415     C                   ENDIF
143100111025     C*** CAS
143200111025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAS'
143300111025     C                   EVAL      PiStr=SkSplitCSV(i)
143400111025     C                   EXSR      CHKNUM
143500111025     C                   IF        PiNum=*on
143600111025     C                   EVAL      PiVal = PiVal / 100
143700111025     C                   Z-ADD     PiVal         VABCAS
143800111025     C                   ELSE
143900111025     C                   SETON                                        32
144000111025     C                   EVAL      vinmsg = %trimr(vinmsg)
144100111025     C                             + ' ' + 'VABCAS'
144200111025     C                   ENDIF
144300111025     C                   ENDIF
144400111025     C*** PKB
144500111025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
144600111025     C                   EVAL      PiStr=SkSplitCSV(i)
144700111025     C                   EXSR      CHKNUM
144800111025     C                   IF        PiNum=*on
144900111025     C                   EVAL      PiVal = PiVal / 1000
145000111025     C                   Z-ADD     PiVal         VABPKB
145100111025     C                   ELSE
145200111025     C                   SETON                                        32
145300111025     C                   EVAL      vinmsg = %trimr(vinmsg)
145400111025     C                             + ' ' + 'VABPKB'
145500111025     C                   ENDIF
145600111025     C                   ENDIF
145700111025     C*** NOT / NT2
145800111025     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
145900111025     C                   MOVEL     *blanks       wNOTE            70
146000111025     C                   EVAL      wNOTE=SkSplitCSV(i)
146100111025     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
146200111025     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
146300111025     C                   ENDIF
146400070925     C*
146500071029     C***  <===  -----------------------------------------
146600040510     C*
146700030715     C                   ENDIF
146800030715     C                   EVAL      i = i + 1
146900030715     C                   ENDDO
147000070925     C*
147100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147200100831     C*
147300070925     C*
147400071029     C***  <===  -----------------------------------------
147500070925     C*
147600070925     C* Considerazioni finali su CBO/CAS
147700100318     C                   IF        wFlgCAS = '1'    OR
147800100318     C                             VABCAS  > *zeros
147900070925     C                   IF        VABCBO = '1'
148000070925     C                   EVAL      VABCBO = '4'
148100070925     C                   ENDIF
148200070925     C                   IF        VABCBO = '2'
148300070925     C                   EVAL      VABCBO = '6'
148400070925     C                   ENDIF
148500070925     C                   ENDIF
148600070925     C***  <===  ----------------------------
148700040510     C*
148800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148900040510     C                   IF        VABNSP = *zeros
149000040510     C                   SETON                                        31
149100040510     C                   ENDIF
149200040715     C*
149300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149400040715     C                   IF        VABLNP = *zeros
149500040715     C                   SETON                                        31
149600040715     C                   ENDIF
149700040510     C*
149800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149900040510     C                   IF        VABRMN = *zeros
150000040510     C                   Z-ADD     VABNSP        VABRMN
150100040510     C                   ENDIF
150200040510     C*
150300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150400040510     C                   IF        VABRMA = *blanks
150500040714     C                   MOVEL     VABRMN        VABRMA
150600040510     C                   ENDIF
150700040128     C*
150800040510     C* Se provincia nn valorizzata la reperisco
150900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151000110912     C                   IF        VABPRD =  *blanks AND
151100110912     C                             VABNZD =  *blanks AND
151200110912     C                             VABCAD <> *blanks AND
151300110912     C                             VABLOD <> *blanks
151400040128     C                   CLEAR                   TISI95DS
151500040128     C                   EVAL      I95TCN = '3'
151600040128     C                   Z-ADD     datcor        I95DAT
151700040128     C                   EVAL      I95NAR = VABNZD
151800040128     C                   EVAL      I95CAP = VABCAD
151900040128     C                   EVAL      I95LOC = VABLOD
152000040128     C                   CALL      'TISI95R'
152100040128     C                   PARM                    TISI95DS
152200040128     C                   EVAL      VABPRD = O95PRV
152300040510     C                   ENDIF
152400020204     C*
152500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152600020204     C                   EXSR      CHKIMPDIV
152700030715     C                   ENDIF
152800020204     C*
152900010607     C                   ENDSR
153000010607     C*----------------------------------------------------*
153100010601
153200020204
153300020204     C*----------------------------------------------------*
153400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153500020204     C*----------------------------------------------------*
153600020204     C     CHKIMPDIV     BEGSR
153700020204     C*
153800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153900020204     C                   Z-ADD     *zeros        wrkDec            9 9
154000020204     C*
154100020204     C* Come prima cosa effettuo considerazioni sulla divisa
154200020204     C                   IF        vabIAS > *zeros
154300020204     C                   IF        vabVAS <> 'EUR'
154400020204     C                   EVAL      vabVAS =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C                   IF        vabCAS > *zeros
154900020204     C                   IF        vabVCA <> 'EUR'
155000020204     C                   EVAL      vabVCA =  'ITL'
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C                   IF        vabVMD > *zeros
155500020204     C                   IF        vabVAD <> 'EUR'
155600020204     C                   EVAL      vabVAD =  'ITL'
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156100020204     C                   Z-ADD     vabIAS        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVAS = 'ITL'
156400020204     C                   EVAL      vabIAS = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156900020204     C                   Z-ADD     vabCAS        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVCA = 'ITL'
157200020204     C                   EVAL      vabCAS = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157700020204     C                   Z-ADD     vabVMD        wrkDec
157800020204     C                   IF        wrkDec > *zeros
157900020204     C                   IF        vabVAD = 'ITL'
158000020204     C                   EVAL      vabVMD = *zeros
158100020204     C                   ENDIF
158200020204     C                   ENDIF
158300020204     C*
158400020204     C                   ENDSR
158500020204     C***
158600010330
158700100318
158800100318
158900100318     C*----------------------------------------------------*
159000100318     C*  CONTROLLO NUMERICITA' CAMPI
159100100318     C*----------------------------------------------------*
159200100318     C     CHKNUM        BEGSR
159300100318     C*
159400100318     C                   IF        PiDecChr = *blanks
159500100318     C                   EVAL      PiDecChr = CharNUM
159600100318     C                   ENDIF
159700100318     C*
159800100318     C                   callp     UBISNUM_Check(PiStr
159900100318     C                                          :PiDecChr
160000100318     C                                          :PiVal
160100100318     C                                          :PiNum
160200100318     C                                          :PiInt)
160300100318     C*
160400100318     C                   ENDSR
160500100318     C***
160600010601
160700010601
160800010601
160900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161000010601     C     repfil        BEGSR
161100010601     C*
161200010601     C                   if        invfil = *zeros and
161300010601     C                             depfil > *zeros and
161400010629     C                             (vinflg = *blanks or
161500010629     C                              vinflg = *zeros)
161600010601     C
161700010601     C                   eval      invfil = depfil
161800010601     C                   endif
161900010601     C*
162000010601     C                   if        depfil <> invfil and
162100010601     C                             invfil > *zeros
162200010601     C                   eval      flgMulti = '1'
162300010601     C                   if        vinflg = *blanks
162400010601     C                   add       1             cntNonEl
162500010601     C                   endif
162600010601     C                   endif
162700010601     C*
162800010601     C                   if        vinflg = '2'
162900010601     C                   eval      flgStato = '2'
163000010601     C                   endif
163100010601     C*
163200010601     C                   ENDSR
163300010601     C***
163400010601
163500010601
163600010601
163700010330
163800040119      /TITLE Invio dei dati al punto operativo.
163900040119     C     invio         BEGSR
164000040119     C*
164100040119     C* 1° invio FIVAT
164200040119     C                   reset                   dscmz
164300040119     C                   move      vlrpoi        cmzdst
164400040119     C                   eval      cmzfld = 'FIVATWWR'
164500040119     C                   eval      cmzmbd = vlrhdl
164600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164700040119     C***                if        prmfir = *blanks
164800040119     C                   eval      cmzfla = 'FIVAT00F'
164900040119     C                   eval      cmzmba = 'FIVAT00F'
165000040119     C***                else
165100040119     C***                eval      cmzfla = prmfir
165200040119     C***                eval      cmzmba = prmfir
165300040119     C***                endif
165400040119     C                   eval      cmznrr = *zeros
165500040119     C                   move      §ctrokvt      cmznrr
165600040119     C                   eval      cmzlba = vlrfl1
165700040119     C                   call(e)   'TIS711C'
165800040119     C                   parm                    dscmz
165900040119     C                   parm      *blanks       esito
166000040119     C                   if        %error
166100040119     C                             or cmzerr = '1'
166200040119     C                             or esito  = '1'
166300040119     C                   eval      wrkesito = '3'
166400040119     C                   else
166500040119     C*
166600040119     C* 2° invio FIVAB
166700040119     C                   reset                   dscmz
166800040119     C                   move      vlrpoi        cmzdst
166900040119     C                   eval      cmzfld = vlrfou
167000040119     C                   eval      cmzmbd = vlrhdl
167100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167200040119     C***                if        prmfir = *blanks
167300040119     C                   eval      cmzfla = 'FIVAB00F'
167400040119     C                   eval      cmzmba = 'FIVAB00F'
167500040119     C***                else
167600040119     C***                eval      cmzfla = prmfir
167700040119     C***                eval      cmzmba = prmfir
167800040119     C***                endif
167900040119     C                   eval      cmznrr = *zeros
168000040119     C                   move      §ctrokvb      cmznrr
168100040119     C                   eval      cmzlba = vlrfl1
168200040119     C                   call(e)   'TIS711C'
168300040119     C                   parm                    dscmz
168400040119     C                   parm      *blanks       esito
168500040119     C                   if        %error
168600040119     C                             or cmzerr = '1'
168700040119     C                             or esito  = '1'
168800040119     C                   eval      wrkesito = '3'
168900040119     C                   endif
169000040119     C                   endif
169100040119     C*
169200040119     C                   ENDSR
169300040119     C***
169400010601
169500010601
169600010601
169700010601
169800010601      /TITLE Invio dei dati al punto operativo.
169900010601     C     opeini        BEGSR
170000010601     C*
170100010601     C* Inizializzo flag e contatori operativi
170200010601     C                   movel     '0'           flgGiro           1
170300010601     C                   movel     '0'           flgMulti          1
170400010601     C                   movel     '1'           flgStato          1
170500010615     C                   movel     '0'           flgOk             1
170600010601     C                   z-add     *zeros        cntNonEl         10 0
170700010601     C                   z-add     *zeros        depfil            3 0
170800010601     C                   z-add     *zeros        invfil            3 0
170900010601     C*
171000010601     C                   ENDSR
171100010601     C***
171200070326
171300070326
171400070326
171500070326
171600070326     C     *pssr         BEGSR
171700070329     C*
171800070329     C                   if        %open(tivin00r)
171900070329     C                   close     tivin00r
172000070329     C                   endif
172100070329     C                   if        %open(fivabwwr)
172200070329     C                   close     fivabwwr
172300070329     C                   endif
172400070329     C                   if        %open(fivatwwr)
172500070329     C                   close     fivatwwr
172600070329     C                   endif
172700070326     C*
172800070326     C* Effettuo la chiamata al CLLE preposto
172900070326     C                   call(e)   'TITVVTC'
173000070326     C                   parm                    parccm
173100070326     C                   parm                    parmbr
173200070326     C                   parm      '2'           paropz
173300070326     C*
173400070326     C                   eval      wrkesito = '2'
173500070404     C*
173600070404     C                   seton                                        LR
173700070326     C*
173800070326     C                   ENDSR     '*CANCL'
173900070326     C***
174000100318
174100100318
174200100318
174300100318     C     normCSV       BEGSR
174400100318     C*
174500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174600100318     C* separatori campi
174700100318     C                   if        CharTXT <> *blanks
174800100318     C                   z-add     *zeros        wLenVINDTA        4 0
174900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175000100318     C                   setoff                                       80
175100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175400100318     C                   if        not *in80
175500100318     C                   seton                                        80
175600100318     C                   else
175700100318     C                   setoff                                       80
175800100318     C                   endif
175900100318     C                   endif
176000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176100100318     C                             and *in80 = *on
176200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176300100318     C                   endif
176400100318     C                   enddo
176500100318     C                   endif
176600100318     C*
176700100318     C                   ENDSR
176800100318     C***
176900070326
177000100326
177100100326
177200100326     C*----------------------------------------------------*
177300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177400100326     C*----------------------------------------------------*
177500100326     C     REPNSP        BEGSR
177600100326     C*
177700100326     C* NSP => Stacco un numeratore da AZNUM
177800100326     C                   clear                   TRUL33DS
177900100326     C                   eval      I33OPE = *zeros
178000100326     C                   eval      I33CNU = 302
178100100326     C                   eval      I33NUM = 1
178200100326     C                   movel     TRUL33DS      KPJBU
178300100326     C                   call      'TRUL33R'
178400100326     C                   parm                    KPJBA
178500100326     C                   movel     KPJBU         TRUL33DS
178600100326     C                   if        O33ERR = *zeros
178700100326     C                   z-add     O33NRF        VABNSP
178800100326     C                   z-add     O33NRF        VATNSP
178900100326     C                   else
179000100326     C                   SETON                                        31
179100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179200100326     C                             + ' ' + 'VABNSP VATNSP'
179300100326     C                   endif
179400100326     C*
179500100326     C                   ENDSR
179600010330
179700100326
179800010330
179900000613     C     *inzsr        BEGSR
180000990910     C*
180100990910     C     *entry        plist
180200990920     C                   parm                    tivlrds
180300990921     C                   parm      wrkesito      esito
180400000724     C                   parm                    prmlit
180500000710     C                   parm                    prmfir
180600010330     C*
180700010330     C* CALCOLA LA DATA CORRENTE
180800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180900100318     C                   eval      datcor = %dec(%date() : *ISO)
181000000613     C*
181100000613     C                   ENDSR
181200000613     C***
181300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181400111025X;X;X;X;X;VABCBO;X;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VABPRD;VABNZD;X;
181500111025X;VABCTR;VABTSP;W_VABIAS;VABVAS;X;VABNCL;W_VABPKB;X;X;W_VABCAS;VABTIC;
181600130826VABVCA;X;W_VABRMN;VABRMA;X;X;X;X;X;X;VABFFD;X;VABDCR;X;X;X;X;X;X;X;X;X;
181700140902X;X;X;VATNOT_A;VATNOT_B;W_VABNOT;X;VATNOT_I;VATNOT_S;
181800100318
