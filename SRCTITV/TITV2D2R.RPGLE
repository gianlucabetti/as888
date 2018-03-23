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
005900100831     D wVABRSD         s                   LIKE(VABRSD)
006000100831     D wVABIND         s                   LIKE(VABIND)
006100100831     D wVABCAD         s                   LIKE(VABCAD)
006200100831     D wVABLOD         s                   LIKE(VABLOD)
006300100831     D wVABPRD         s                   LIKE(VABPRD)
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
012700040119     C                   call(e)   'TITVVTC'
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
014400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014500010330     C*--------------------------------------------------------
014600010605     C     RWVAB         BEGSR
014700010330     C*
014800010330     C                   if        not %open(tivin00r)
014900010330     C                   open      tivin00r
015000010330     C                   endif
015100021025     C                   if        not %open(fivabwwr)
015200021025     C                   open      fivabwwr
015300010330     C                   endif
015400040119     C*
015500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015600040119     C                   exsr      prevat
015700040119     C*
015800040119     C                   if        chkcall = '0'
015900040119     C*
016000040119     C                   if        not %open(fivatwwr)
016100040119     C                   open      fivatwwr
016200040119     C                   endif
016300010330     C*
016400040119     C                   clear                   §CTROKVB          7 0
016500040119     C                   clear                   §CTROKVT          7 0
016600040119     C                   clear                   §CTRMO            7 0
016700040119     C                   clear                   §CTRNO            7 0
016800070823     C*
016900070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017000070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017100110315     C                   exsr      inzvar
017200110315     C                   exsr      impvab
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
029300021025     C                   if        %open(fivabwwr)
029400021025     C                   close     fivabwwr
029500010330     C                   endif
029600040512     C                   if        %open(fivatwwr)
029700040512     C                   close     fivatwwr
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
036100080513     C*
036200080513     C* Gestisco l'eventuale rottura x numero spedizione
036300080527     C                   if        VABNSP <> depspe or *in65 = *on
036400070301     C                   if        wVATNOT_E <> *blanks
036500070301     C                   eval      VABCTM = '7Q'
036600070301     C                   endif
036700100413     C                   write     fivab000
036800080513     C                   endif
036900040119     C*
037000040119     C                   ENDSR
037100040119
037200040119
037300040119
037400040119     C*----------------------------------------------------*
037500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037600040119     C*----------------------------------------------------*
037700040510     C     WRIVAT        BEGSR
037800060512     C*
037900060512     C* Inizializzo i campi chiave
038000060512     C                   eval      VATFGS = VABFGS
038100060512     C                   eval      VATAAS = VABAAS
038200060512     C                   eval      VATCCM = VABCCM
038300070823     C                   eval      VATNRS = VABNRS
038400060512     C                   eval      VATNSP = VABNSP
038500060512     C                   eval      VATLNP = VABLNP
038600040714     C*
038700040714     C* Solo se VATNOT valorizzato scarico il buffer
038800040714     C                   if        VATNOT <> *blanks
038900040119     C*
039000040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039100040510     C                   if        VATTRC = *blanks
039200040510     C                   eval      VATTRC = 'E'
039300040510     C                   endif
039400040510     C                   write     FIVAT000
039500040119     C                   add       1             §CTROKVT
039600040714     C*
039700040714     C                   endif
039800060331     C*
039900060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040000060331     C                   if        wVATNOT_A <> *blanks
040100060331     C                   eval      VATNOT = wVATNOT_A
040200060331     C                   eval      VATTRC = 'A'
040300060331     C                   write     FIVAT000
040400060331     C                   add       1             §CTROKVT
040500060331     C                   endif
040600060331     C*
040700060331     C                   if        wVATNOT_B <> *blanks
040800060331     C                   eval      VATNOT = wVATNOT_B
040900060331     C                   eval      VATTRC = 'B'
041000060331     C                   write     FIVAT000
041100060331     C                   add       1             §CTROKVT
041200060331     C                   endif
041300060331     C*
041400060331     C                   if        wVATNOT_E <> *blanks
041500110201     C                   eval      VATNOT = wVATNOT_E
041600060331     C                   eval      VATTRC = 'E'
041700110201     C                   write     FIVAT000
041800110201     C                   add       1             §CTROKVT
041900110201     C                   endif
042000040119     C*
042100040119     C                   ENDSR
042200040119
042300040119
042400040119
042500040119     C*----------------------------------------------------*
042600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042700040119     C*----------------------------------------------------*
042800040119     C     PREVAT        BEGSR
042900040119     C*
043000040119     C* Compongo il nome del membro da dare al FIVATWWR
043100040119     C                   eval      parmbr = vlrhdl
043200040119     C                   movel     'M'           parmbr
043300040510     C                   eval      parccm = vlrksc
043400040119     C                   eval      paropz = '1'
043500040119     C* Effettuo la chiamata al CLLE preposto
043600040119     C                   call(e)   'TITVVTC'
043700040119     C                   parm                    parccm
043800040119     C                   parm                    parmbr
043900040119     C                   parm                    paropz
044000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044100040119     C                   if        %error
044200040119     C                   movel     '1'           chkcall
044300040119     C                   else
044400040119     C                   movel     '0'           chkcall
044500040119     C                   endif
044600040119     C*
044700040119     C                   ENDSR
044800010601
044900010601
045000010601
045100010330     C*----------------------------------------------------*
045200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045300010330     C*----------------------------------------------------*
045400040119     C     INZVAR        BEGSR
045500080527     C*
045600080527     C                   setoff                                       65
045700010330     C*
045800030822     C* Inizializzo variabili di wrk
045900020204     C                   Z-ADD     *zeros        Num5_0            5 0
046000101018     C                   MOVEL     *blanks       wFlgCAS
046100060331     C                   MOVEL     *blanks       wVATNOT_A
046200060331     C                   MOVEL     *blanks       wVATNOT_B
046300060331     C                   MOVEL     *blanks       wVATNOT_E
046400100831     C                   MOVEL     *blanks       wVABRSD
046500100831     C                   MOVEL     *blanks       wVABIND
046600100831     C                   MOVEL     *blanks       wVABCAD
046700100831     C                   MOVEL     *blanks       wVABLOD
046800100831     C                   MOVEL     *blanks       wVABPRD
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
086500031204     C                   ENDDO
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
092100100720 xxx C***                EXSR      repNSP
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
113900101018     C                   IF        SkSplitCSV(i) <> *blanks AND
114000101018     C                             SkSplitCSV(i) <> *zeros
114100101018     C                   EVAL      wFlgCAS = '1'
114200101018     C                   ENDIF
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiNum=*on
114600030721     C                   Z-ADD     PiVal         VABCAS
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABCAS = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABCAS'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** CCM
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiInt=*on      AND
115900030721     C                             PiVal<=9999999 AND
116000030721     C                             PiVal>*zeros
116100030721     C                   Z-ADD     PiVal         VABCCM
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      VABCCM = *zeros
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600040510     C                             + ' ' + 'VABCCM'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** LNP
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiInt=*on    AND
117400030721     C                             PiVal<=999   AND
117500030721     C                             PiVal>*zeros
117600030721     C                   Z-ADD     PiVal         VABLNP
117700030721     C                   ELSE
117800040715     C                   SETON                                        32
117900030721     C                   EVAL      VABLNP = *zeros
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100040510     C                             + ' ' + 'VABLNP'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** NRS
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiInt=*on AND
118900030721     C                             PiVal<=99
119000030721     C                   Z-ADD     PiVal         VABNRS
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400030721     C                             + ' ' + 'VABNRS'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** LNA
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on    AND
120200030721     C                             PiVal<=999   AND
120300030721     C                             PiVal>*zeros
120400030721     C                   Z-ADD     PiVal         VABLNA
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      VABLNA = *zeros
120800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120900030721     C                             + ' ' + 'VABLNA'
121000030721     C                   ENDIF
121100030721     C                   ENDIF
121200030721     C*** CTR
121300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121400030721     C                   EVAL      PiStr=SkSplitCSV(i)
121500030721     C                   EXSR      CHKNUM
121600030721     C                   IF        PiInt=*on  AND
121700030721     C                             PiVal<=999
121800030721     C                   Z-ADD     PiVal         VABCTR
121900030721     C                   ELSE
122000030721     C                   SETON                                        32
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABCTR'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** IAS
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiNum=*on
123000030721     C                   Z-ADD     PiVal         VABIAS
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABIAS'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** VLB
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiNum=*on
124200030721     C                   Z-ADD     PiVal         VABVLB
124300030721     C                   ELSE
124400030721     C                   SETON                                        32
124500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124600030721     C                             + ' ' + 'VABVLB'
124700030721     C                   ENDIF
124800030721     C                   ENDIF
124900030721     C*** QFT
125000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125100030721     C                   EVAL      PiStr=SkSplitCSV(i)
125200030721     C                   EXSR      CHKNUM
125300030721     C                   IF        PiNum=*on
125400030721     C                   Z-ADD     PiVal         VABQFT
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABQFT'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** NCD
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on      AND
126600030721     C                             PiVal<=9999999
126700030721     C                   Z-ADD     PiVal         VABNCD
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABNCD'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** NCA
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on      AND
127900030721     C                             PiVal<=9999999
128000030721     C                   Z-ADD     PiVal         VABNCA
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABNCA'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** ZNC
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on AND
129200030721     C                             PiVal<=99
129300030721     C                   Z-ADD     PiVal         VABZNC
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABZNC'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** DCR
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on       AND
130500030721     C                             PiVal<=99999999
130600030721     C                   Z-ADD     PiVal         VABDCR
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABDCR'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** HCR
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on   AND
131800030721     C                             PiVal<=9999
131900030721     C                   Z-ADD     PiVal         VABHCR
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABHCR'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600030721     C*** VMD
132700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiNum=*on
133100030721     C                   Z-ADD     PiVal         VABVMD
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABVMD'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** ANT
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on        AND
134300030721     C                             PiVal<=999999999
134400030721     C                   Z-ADD     PiVal         VABANT
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABANT'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** CMO
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on
135600030721     C                   Z-ADD     PiVal         Num5_0
135700030721     C                   MOVEL(p)  Num5_0        VABCMO
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      VABCMO = *zeros
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABCMO'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500040510     C*
136600040510     C* Reperisco i campi del FIVAT
136700040510     C*** VATNOT
136800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137000040510     C                   ENDIF
137100040510     C*** VATTRC
137200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137400040510     C                   ENDIF
137500060331     C*** VATNOT_A
137600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137800060331     C                   ENDIF
137900060331     C*** VATNOT_B
138000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138200060331     C                   ENDIF
138300060331     C*** VATNOT_E
138400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138600060331     C                   ENDIF
138700070823     C*
138800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
138900110131     C*** NSP / RMN
139000101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
139100110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
139200101021     C                   EXSR      CHKNUM
139300101021     C                   IF        PiInt=*on
139400101021     C                   Z-ADD     PiVal         VABNSP
139500101021     C                   Z-ADD     PiVal         VABRMN
139600101021     C                   ELSE
139700101021     C                   SETON                                        31
139800101021     C                   EVAL      VABNSP = *zeros
139900101021     C                   EVAL      VABRMN = *zeros
140000101021     C                   EVAL      vinmsg = %trimr(vinmsg)
140100101021     C                             + ' ' + 'VABNSP VABRMN'
140200101021     C                   ENDIF
140300101021     C                   ENDIF
140400070925     C*
140500071029     C***  <===  -----------------------------------------
140600040510     C*
140700030715     C                   ENDIF
140800030715     C                   EVAL      i = i + 1
140900030715     C                   ENDDO
141000070925     C*
141100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
141200100831     C*
141300100831     C                   IF        wVABRSD <> *blanks
141400100831     C                   EVAL      VABRSD = wVABRSD
141500100831     C                   ENDIF
141600100831     C*
141700100831     C                   IF        wVABIND <> *blanks
141800100831     C                   EVAL      VABIND = wVABIND
141900100831     C                   ENDIF
142000100831     C*
142100100831     C                   IF        wVABCAD <> *blanks
142200100831     C                   EVAL      VABCAD = wVABCAD
142300100831     C                   ENDIF
142400100831     C*
142500100831     C                   IF        wVABLOD <> *blanks
142600100831     C                   EVAL      VABLOD = wVABLOD
142700100831     C                   ENDIF
142800100831     C*
142900100831     C                   IF        wVABPRD <> *blanks
143000100831     C                   EVAL      VABPRD = wVABPRD
143100100831     C                   ENDIF
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400070925     C*
143500070925     C* Considerazioni finali su CBO/CAS
143600100318     C                   IF        wFlgCAS = '1'    OR
143700100318     C                             VABCAS  > *zeros
143800070925     C                   IF        VABCBO = '1'
143900070925     C                   EVAL      VABCBO = '4'
144000070925     C                   ENDIF
144100070925     C                   IF        VABCBO = '2'
144200070925     C                   EVAL      VABCBO = '6'
144300070925     C                   ENDIF
144400070925     C                   ENDIF
144500070925     C***  <===  ----------------------------
144600040510     C*
144700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144800040510     C                   IF        VABNSP = *zeros
144900040510     C                   SETON                                        31
145000040510     C                   ENDIF
145100040715     C*
145200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145300040715     C                   IF        VABLNP = *zeros
145400040715     C                   SETON                                        31
145500040715     C                   ENDIF
145600040510     C*
145700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145800040510     C                   IF        VABRMN = *zeros
145900040510     C                   Z-ADD     VABNSP        VABRMN
146000040510     C                   ENDIF
146100040510     C*
146200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146300040510     C                   IF        VABRMA = *blanks
146400040714     C                   MOVEL     VABRMN        VABRMA
146500040510     C                   ENDIF
146600040128     C*
146700040510     C* Se provincia nn valorizzata la reperisco
146800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146900040510     C                   IF        VABPRD = *blanks
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
176800000613     C***
176900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177000110315VABRMA;FILXX2;W_VABNSP;FILXX4;FILXX5;FILXX6;FILXX7;FILXX8;FILXX9;FILX10;
177100110315FILX11;FILX12;FILX13;FILX14;VABRSD;FILX16;VABIND;VABNOT;VABRD2;VABLOD;
177200110315VABCAD;FILX22;FILX23;VATNOT_B;FILX25;FILX26;FILX27;FILX28;FILX29;FILX30;
177300110315VABPRD;FILX32;VABCAS;FILX34;VABPKB;VABNCL;FILX37;
177400100318
