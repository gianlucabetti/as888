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
038600040119     C*
038700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038800040510     C                   if        VATTRC = *blanks
038900040510     C                   eval      VATTRC = 'E'
039000040510     C                   endif
039100110318     C                   if        VATNOT <> *blanks
039200040510     C                   write     FIVAT000
039300040119     C                   add       1             §CTROKVT
039400110318     C                   endif
039500060331     C*
039600060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039700060331     C                   if        wVATNOT_A <> *blanks
039800060331     C                   eval      VATNOT = wVATNOT_A
039900060331     C                   eval      VATTRC = 'A'
040000060331     C                   write     FIVAT000
040100060331     C                   add       1             §CTROKVT
040200060331     C                   endif
040300060331     C*
040400060331     C                   if        wVATNOT_B <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_B
040600060331     C                   eval      VATTRC = 'B'
040700060331     C                   write     FIVAT000
040800060331     C                   add       1             §CTROKVT
040900060331     C                   endif
041000060331     C*
041100060331     C                   if        wVATNOT_E <> *blanks
041200110201     C                   eval      VATNOT = wVATNOT_E
041300060331     C                   eval      VATTRC = 'E'
041400110201     C                   write     FIVAT000
041500110201     C                   add       1             §CTROKVT
041600110201     C                   endif
041700040119     C*
041800040119     C                   ENDSR
041900040119
042000040119
042100040119
042200040119     C*----------------------------------------------------*
042300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042400040119     C*----------------------------------------------------*
042500040119     C     PREVAT        BEGSR
042600040119     C*
042700040119     C* Compongo il nome del membro da dare al FIVATWWR
042800040119     C                   eval      parmbr = vlrhdl
042900040119     C                   movel     'M'           parmbr
043000040510     C                   eval      parccm = vlrksc
043100040119     C                   eval      paropz = '1'
043200040119     C* Effettuo la chiamata al CLLE preposto
043300040119     C                   call(e)   'TITVVTC'
043400040119     C                   parm                    parccm
043500040119     C                   parm                    parmbr
043600040119     C                   parm                    paropz
043700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043800040119     C                   if        %error
043900040119     C                   movel     '1'           chkcall
044000040119     C                   else
044100040119     C                   movel     '0'           chkcall
044200040119     C                   endif
044300040119     C*
044400040119     C                   ENDSR
044500010601
044600010601
044700010601
044800010330     C*----------------------------------------------------*
044900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045000010330     C*----------------------------------------------------*
045100040119     C     INZVAR        BEGSR
045200080527     C*
045300080527     C                   setoff                                       65
045400010330     C*
045500030822     C* Inizializzo variabili di wrk
045600020204     C                   Z-ADD     *zeros        Num5_0            5 0
045700101018     C                   MOVEL     *blanks       wFlgCAS
045800060331     C                   MOVEL     *blanks       wVATNOT_A
045900060331     C                   MOVEL     *blanks       wVATNOT_B
046000060331     C                   MOVEL     *blanks       wVATNOT_E
046100100831     C                   MOVEL     *blanks       wVABRSD
046200100831     C                   MOVEL     *blanks       wVABIND
046300100831     C                   MOVEL     *blanks       wVABCAD
046400100831     C                   MOVEL     *blanks       wVABLOD
046500100831     C                   MOVEL     *blanks       wVABPRD
046600030822     C*
046700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046800030822     C                   CLEAR                   SkSplitCSV
046900030822     C                   EVAL      i = 1
047000030822     C                   EVAL      posDa = *zeros
047100030822     C                   EVAL      posA  = *zeros
047200080527     C*
047300080527     C                   CLEAR                   FIVABDS
047400080527     C                   CLEAR                   FIVAT000
047500030822     C*
047600030822     C* Reimposto i valori di default
047700030822     C                   EXSR      DEFCAM
047800030822     C*
047900010330     C                   ENDSR
048000010330     C*----------------------------------------------------*
048100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048200010330     C*----------------------------------------------------*
048300010330     C     DEFCAM        BEGSR
048400030822     C*
048500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048600030715     C* e delimitatore testo.
048700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049000100318     C*
049100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049200100318     C                   EVAL      CharSOS = CharNUM
049300030822     C*
049400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049500100720     C                   SETOFF                                       505152
049600100720     C                   SETOFF                                       53
049700100413     C                   SELECT
049800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
049900100720     C                   SETON                                        50
050000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
050100100413     C                   SETON                                        51
050200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
050300100720     C                   SETON                                        52
050400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
050500100720     C                   SETON                                        53
050600100413     C                   ENDSL
050700030822     C*
050800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050900030822     C                   EVAL      posDaDft = 1
051000030822     C                   EVAL      posADft  = 0
051100030822     C                   EVAL      wGiroDft = 0
051200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051300030822     C                             posDaDft > 0
051400030822     C*
051500030822     C* Gestisco il 1° giro
051600030822     C                   IF        wGiroDft = 0
051700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051900030822     C* Incremento il contatore dei "giri"
052000030822     C                   EVAL      wGiroDft = 1
052100030822     C                   ELSE
052200030822     C                   EVAL      posDaDft = posADft
052300030822     C                   ENDIF
052400030822     C* Eseguo lo scan x trovare la fine del campo corrente
052500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052600030822     C*
052700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052900030822     C                   IF        posDaDft > 0 AND
053000030822     C                             posADft  > 0
053100030822     C* NCL
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'NCL'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABNCL
054200030822     C                   ENDIF
054300030822     C                   ENDIF
054400030822     C* CCM
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'CCM'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400030822     C                   Z-ADD     PiVal         VABCCM
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* LNP
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'LNP'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABLNP
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* NRS
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'NRS'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABNRS
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* CTR
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'CTR'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABCTR
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* PKB
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'PKB'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiNum=*on
060600030822     C                   Z-ADD     PiVal         VABPKB
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* VLB
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'VLB'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiNum=*on
061900030822     C                   Z-ADD     PiVal         VABVLB
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* QFT
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'QFT'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200030822     C                   Z-ADD     PiVal         VABQFT
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* CBO
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'CBO'
064000030822     C                   EVAL      VABCBO=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   ENDIF
064400030822     C* TSP
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'TSP'
064900030822     C                   EVAL      VABTSP=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* VAS
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'VAS'
065800030822     C                   EVAL      VABVAS=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* VCA
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'VCA'
066700030822     C                   EVAL      VABVCA=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100070925     C* TIC
067200070925     C                   IF        %subst(
067300070925     C                             %subst(vlrppt:posDaDft+1:
067400070925     C                             posADft-posDaDft-1):1:3)
067500070925     C                             = 'TIC'
067600070925     C                   EVAL      VABTIC=%trim(%subst(
067700070925     C                             %subst(vlrppt:posDaDft+1:
067800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900070925     C                   ENDIF
068000030822     C* GCA
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'GCA'
068500030822     C                   EVAL      VABGCA=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* CTM
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'CTM'
069400030822     C                   EVAL      VABCTM=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* FFD
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'FFD'
070300030822     C                   EVAL      VABFFD=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* VAD
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'VAD'
071200030822     C                   EVAL      VABVAD=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* GMA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'GMA'
072100030822     C                   EVAL      VABGMA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* GGA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'GGA'
073000030822     C                   EVAL      VABGGA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* GVA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GVA'
073900030822     C                   EVAL      VABGVA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* TC1
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'TC1'
074800030822     C                   EVAL      VABTC1=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* TC2
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'TC2'
075700030822     C                   EVAL      VABTC2=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100040714     C* VATTRC
076200040714     C                   IF        %subst(
076300040714     C                             %subst(vlrppt:posDaDft+1:
076400040714     C                             posADft-posDaDft-1):1:3)
076500040714     C                             = 'TRC'
076600040714     C                   EVAL      VATTRC=%trim(%subst(
076700040714     C                             %subst(vlrppt:posDaDft+1:
076800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900040714     C                   ENDIF
077000030822     C* ...
077100030822     C                   ENDIF
077200030822     C                   ENDDO
077300020204     C*
077400010330     C                   ENDSR
077500010607     C*----------------------------------------------------*
077600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077700010607     C*----------------------------------------------------*
077800010607     C     IMPVAB        BEGSR
077900010607     C*
078000010607     C                   SETOFF                                       3132
078100110315 xxx C***                SETON                                        35
078200030515     C*
078300010607     C                   MOVEL     datcor        VABAAS
078400010607     C                   MOVE      datcor        VABMGS
078500010607     C*
078600030715     C**********
078700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078800030715     C**********
078900030715     C                   IF        wGiro = *zeros
079000040113     C*
079100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079400080429     C                             %trim(intcol(5))<>*blanks
079500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
079700080429     C                                      %trim(intcol(5))
079800070823     C     ';':CharCSV   XLATE     vindta        vindta
079900040510     C                   ENDIF
080000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080100031201     C     minu:maiu     XLATE     vindta        vindta
080200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080300030715     C                   DOW       posDa <= %len(%trim(vindta))
080400030715     C*
080500030715     C* Gestisco il 1° campo
080600030715     C                   IF        i = 1
080700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080800030715     C                   EVAL      posDa = 1
080900030715     C* Eseguo lo scan x trovare la fine del primo campo
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C* X i campi successivi al 1°
081500030715     C                   ELSE
081600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081700030715     C                   EVAL      posDa = posA + 1
081800030715     C* Eseguo lo scan x trovare la fine del campo corrente
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* Gestisco l'ultimo campo
082100030715     C                   IF        posA = *zeros
082200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C                   LEAVE
082700030715     C                   ELSE
082800030715     C* A questo punto "estraggo" il campo corrente
082900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083000030715     C                                                    (posA-posDa))
083100030715     C                   ENDIF
083200030715     C                   ENDIF
083300030715     C* Incremento il contatore d campo
083400030715     C                   EVAL      i = i +1
083500030715     C                   ENDDO
083600030715     C*
083700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083800030715     C* delimitazione testo
083900030715     C                   EVAL      i = 1
084000030715     C                   DOW       i < %elem(SkSplitFLD)
084100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084300030715     C                   EVAL      i = i + 1
084400030715     C                   ENDDO
084500030715     C                   EVAL      wGiro = 1
084600030715     C                   ELSE
084700030715     C                   EVAL      wGiro = 2
084800031204     C**********
084900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085000031204     C* d separatore campo
085100031204     C**********
085200031204     C                   EVAL      vindta = %trim(vindta)
085300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085400031204     C                   EVAL      vindta = %subst(vindta:2)
085500031204     C                   ENDDO
085600031204     C*
085700031204     C                   Z-ADD     *zeros        lunghInput        4 0
085800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086200031204     C                   ENDDO
086300030715     C**********
086400030715     C* Effettuo lo split del campo dati d input
086500030715     C**********
086600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086700030715     C                   DOW       posDa <= %len(%trim(vindta))
086800030715     C*
086900030715     C* Gestisco il 1° campo
087000030715     C                   IF        i = 1
087100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087200030715     C                   EVAL      posDa = 1
087300030715     C* Eseguo lo scan x trovare la fine del primo campo
087400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C* X i campi successivi al 1°
087900030715     C                   ELSE
088000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088100030715     C                   EVAL      posDa = posA + 1
088200031204     C* Verifico che nn vi sia il campo nn valorizzato
088300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088400031204     C* Se campo nn valorizzato skippo al prossimo
088500031204     C                   EVAL      SkSplitCSV(i) = *blanks
088600031204     C                   EVAL      posA  = posA + 1
088700031204     C                   ELSE
088800030715     C* Eseguo lo scan x trovare la fine del campo corrente
088900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089000030715     C* Gestisco l'ultimo campo
089100030715     C                   IF        posA = *zeros
089200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089300030715     C* A questo punto "estraggo" il campo corrente
089400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089500030715     C                                                    (posA-posDa))
089600030715     C                   LEAVE
089700030715     C                   ELSE
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C                   ENDIF
090200031204     C                   ENDIF
090300030715     C                   ENDIF
090400030715     C* Incremento il contatore d campo
090500030715     C                   EVAL      i = i +1
090600030715     C                   ENDDO
090700030715     C*
090800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090900030715     C* delimitazione testo
091000030715     C                   EVAL      i = 1
091100030715     C                   DOW       i < %elem(SkSplitCSV)
091200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091400030715     C                   EVAL      i = i + 1
091500030715     C                   ENDDO
091600100720     C*
091700100720     C* Stacco un progressivo x numero spedizione
091800100720 xxx C***                EXSR      repNSP
091900100326     C*
092000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092100030715     C                   EVAL      i = 1
092200030715     C                   DOW       i < %elem(SkSplitFLD)
092300030715     C*
092400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092600030715     C                             SkSplitCSV(i) <> *blanks
092700030721     C*** ATB
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092900030721     C                   EVAL      VABATB = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030715     C*** RSD
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093600030715     C* ==
093700030715     C                   ENDIF
093800030721     C*** RD2
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030715     C*** IND
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094400030715     C                   EVAL      VABIND = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** LOD
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** PRD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030721     C*** NZD
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** RMA
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** NOT
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** NT2
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** VCA
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** VAS
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** FFD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030721     C*** GC1
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** GC2
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** TSP
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099300030721     C                   ENDIF
099400040510     C*** CBO
099500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099700040510     C                   ENDIF
099800030721     C*** NAS
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** TIC
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GCA
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** XCO
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** CTM
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TCR
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** CTS
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** FTM
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** VAD
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** GMA
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GGA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GVA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** TC1
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TC2
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** SCL
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** RMO
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** NMO
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*
106700030721     C* Reperisco quindi i campi numerici...
106800040510     C*** NSP
106900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107000040510     C                   EVAL      PiStr=SkSplitCSV(i)
107100040510     C                   EXSR      CHKNUM
107200040510     C                   IF        PiInt=*on
107300040510     C                   Z-ADD     PiVal         VABNSP
107400040510     C                   ELSE
107500040510     C                   SETON                                        31
107600040510     C                   EVAL      VABNSP = *zeros
107700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107800040510     C                             + ' ' + 'VABNSP'
107900040510     C                   ENDIF
108000040510     C                   ENDIF
108100071106     C*** RMN
108200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108300071106     C                   EVAL      PiStr=SkSplitCSV(i)
108400071106     C                   EXSR      CHKNUM
108500071106     C                   IF        PiInt=*on
108600071106     C                   Z-ADD     PiVal         VABRMN
108700071106     C                   ELSE
108800071106     C                   SETON                                        32
108900071106     C                   EVAL      VABRMN = *zeros
109000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109100071106     C                             + ' ' + 'VABRMN'
109200071106     C                   ENDIF
109300071106     C                   ENDIF
109400030721     C*** CAD
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109600170309     C* siccome dal 2017/03/09 gestiranno anche l'estero, il CAP/ZIP lo prendo così com'è
109700170309     C                   EVAL      VABCAD = SkSplitCSV(i)
109800170309     C***                EVAL      PiStr=SkSplitCSV(i)
109900170309     C***                EXSR      CHKNUM
110000170309     C***                IF        PiInt=*on
110100170309     C***                Z-ADD     PiVal         Num5_0
110200170309     C***                MOVEL(p)  Num5_0        VABCAD
110300170309     C***                ELSE
110400170309     C***                SETON                                        32
110500170309     C***                EVAL      VABCAD = *zeros
110600170309     C***                EVAL      vinmsg = %trimr(vinmsg)
110700170309     C***                          + ' ' + 'VABCAD'
110800170309     C***                ENDIF
110900030721     C                   ENDIF
111000030721     C*** NCL
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         VABNCL
111600030721     C                   ELSE
111700030721     C                   SETON                                        32
111800030721     C                   EVAL      VABNCL = *zeros
111900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112000040122     C                             + ' ' + 'VABNCL'
112100030721     C                   ENDIF
112200030721     C                   ENDIF
112300030721     C*** PKB
112400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112500030721     C                   EVAL      PiStr=SkSplitCSV(i)
112600030721     C                   EXSR      CHKNUM
112700030721     C                   IF        PiNum=*on
112800030721     C                   Z-ADD     PiVal         VABPKB
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      VABPKB = *zeros
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300030721     C                             + ' ' + 'VABPKB'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** CAS
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113800101018     C                   IF        SkSplitCSV(i) <> *blanks AND
113900101018     C                             SkSplitCSV(i) <> *zeros
114000101018     C                   EVAL      wFlgCAS = '1'
114100101018     C                   ENDIF
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiNum=*on
114500030721     C                   Z-ADD     PiVal         VABCAS
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABCAS = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABCAS'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** CCM
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiInt=*on      AND
115800030721     C                             PiVal<=9999999 AND
115900030721     C                             PiVal>*zeros
116000030721     C                   Z-ADD     PiVal         VABCCM
116100030721     C                   ELSE
116200030721     C                   SETON                                        32
116300030721     C                   EVAL      VABCCM = *zeros
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500040510     C                             + ' ' + 'VABCCM'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** LNP
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on    AND
117300030721     C                             PiVal<=999   AND
117400030721     C                             PiVal>*zeros
117500030721     C                   Z-ADD     PiVal         VABLNP
117600030721     C                   ELSE
117700040715     C                   SETON                                        32
117800030721     C                   EVAL      VABLNP = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000040510     C                             + ' ' + 'VABLNP'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** NRS
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on AND
118800030721     C                             PiVal<=99
118900030721     C                   Z-ADD     PiVal         VABNRS
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABNRS'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** LNA
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on    AND
120100030721     C                             PiVal<=999   AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABLNA
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      VABLNA = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABLNA'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** CTR
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on  AND
121600030721     C                             PiVal<=999
121700030721     C                   Z-ADD     PiVal         VABCTR
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABCTR'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** IAS
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiNum=*on
122900030721     C                   Z-ADD     PiVal         VABIAS
123000030721     C                   ELSE
123100030721     C                   SETON                                        32
123200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123300030721     C                             + ' ' + 'VABIAS'
123400030721     C                   ENDIF
123500030721     C                   ENDIF
123600030721     C*** VLB
123700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123800030721     C                   EVAL      PiStr=SkSplitCSV(i)
123900030721     C                   EXSR      CHKNUM
124000030721     C                   IF        PiNum=*on
124100030721     C                   Z-ADD     PiVal         VABVLB
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABVLB'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** QFT
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiNum=*on
125300030721     C                   Z-ADD     PiVal         VABQFT
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABQFT'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** NCD
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on      AND
126500030721     C                             PiVal<=9999999
126600030721     C                   Z-ADD     PiVal         VABNCD
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABNCD'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** NCA
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on      AND
127800030721     C                             PiVal<=9999999
127900030721     C                   Z-ADD     PiVal         VABNCA
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABNCA'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** ZNC
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on AND
129100030721     C                             PiVal<=99
129200030721     C                   Z-ADD     PiVal         VABZNC
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABZNC'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** DCR
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on       AND
130400030721     C                             PiVal<=99999999
130500030721     C                   Z-ADD     PiVal         VABDCR
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABDCR'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** HCR
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on   AND
131700030721     C                             PiVal<=9999
131800030721     C                   Z-ADD     PiVal         VABHCR
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABHCR'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500030721     C*** VMD
132600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132700030721     C                   EVAL      PiStr=SkSplitCSV(i)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiNum=*on
133000030721     C                   Z-ADD     PiVal         VABVMD
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133400030721     C                             + ' ' + 'VABVMD'
133500030721     C                   ENDIF
133600030721     C                   ENDIF
133700030721     C*** ANT
133800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133900030721     C                   EVAL      PiStr=SkSplitCSV(i)
134000030721     C                   EXSR      CHKNUM
134100030721     C                   IF        PiInt=*on        AND
134200030721     C                             PiVal<=999999999
134300030721     C                   Z-ADD     PiVal         VABANT
134400030721     C                   ELSE
134500030721     C                   SETON                                        32
134600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134700030721     C                             + ' ' + 'VABANT'
134800030721     C                   ENDIF
134900030721     C                   ENDIF
135000030721     C*** CMO
135100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135200030721     C                   EVAL      PiStr=SkSplitCSV(i)
135300030721     C                   EXSR      CHKNUM
135400030721     C                   IF        PiInt=*on
135500030721     C                   Z-ADD     PiVal         Num5_0
135600030721     C                   MOVEL(p)  Num5_0        VABCMO
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      VABCMO = *zeros
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABCMO'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400040510     C*
136500040510     C* Reperisco i campi del FIVAT
136600040510     C*** VATNOT
136700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136900040510     C                   ENDIF
137000040510     C*** VATTRC
137100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137300040510     C                   ENDIF
137400060331     C*** VATNOT_A
137500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137700060331     C                   ENDIF
137800060331     C*** VATNOT_B
137900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138100060331     C                   ENDIF
138200060331     C*** VATNOT_E
138300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138500060331     C                   ENDIF
138600070823     C*
138700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
138800110427     C*** NSP / RMN / RMA
138900101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
139000110427     C                   EVAL      VABRMA=%trim(SkSplitCSV(i))
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
140100110415     C                             + ' ' + 'VABNSP VABRMN'
140200101021     C                   ENDIF
140300101021     C                   ENDIF
140400110415     C*** CBO
140500110415     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
140600110415     C                   SELECT
140700110415     C                   WHEN      SkSplitCSV(i) = 'F'
140800110415     C                   EVAL      VABCBO = '1'
140900110415     C                   WHEN      SkSplitCSV(i) = 'A'
141000110415     C                   EVAL      VABCBO = '2'
141100110415     C                   ENDSL
141200110415     C                   ENDIF
141300111024     C*** NOT / NT2
141400111024     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
141500111024     C                   MOVEL     *blanks       wNOTE            70
141600111024     C                   EVAL      wNOTE=%trim(SkSplitCSV(i))
141700111024     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
141800111024     C                   EVAL      VABNT2=%subst(wNOTE:36:35)
141900111024     C                   ENDIF
142000111102     C*** DCR
142100111102     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABDCR'
142200111102     C                   IF        %trim(SkSplitCSV(i)) <> '0'     AND
142300111102     C                             %trim(SkSplitCSV(i)) <> *blanks
142400111102     C                   IF        %len(%trim(SkSplitCSV(i))) = 7
142500111102     C                   EVAL      SkSplitCSV(i)='0'+%trim(SkSplitCSV(i))
142600111102     C                   ENDIF
142700111102     C                   IF        %len(%trim(SkSplitCSV(i))) = 8
142800111102     C                   EVAL      PiStr=%subst(SkSplitCSV(i):5:4)+
142900111102     C                                   %subst(SkSplitCSV(i):3:2)+
143000111102     C                                   %subst(SkSplitCSV(i):1:2)
143100111102     C                   EXSR      CHKNUM
143200111102     C                   IF        PiInt=*on       AND
143300111102     C                             PiVal<=99999999
143400111102     C                   Z-ADD     PiVal         VABDCR
143500111102     C                   ELSE
143600111102     C                   SETON                                        32
143700111102     C                   EVAL      vinmsg = %trimr(vinmsg)
143800111102     C                             + ' ' + 'VABDCR'
143900111102     C                   ENDIF
144000111102     C*
144100111102     C                   ELSE
144200111102     C                   SETON                                        32
144300111102     C                   EVAL      vinmsg = %trimr(vinmsg)
144400111102     C                             + ' ' + 'VABDCR'
144500111102     C                   ENDIF
144600111102     C                   ENDIF
144700111102     C                   ENDIF
144800110318     C*
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100040510     C*
145200030715     C                   ENDIF
145300030715     C                   EVAL      i = i + 1
145400030715     C                   ENDDO
145500070925     C*
145600071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145700100831     C*
145800100831     C                   IF        wVABRSD <> *blanks
145900100831     C                   EVAL      VABRSD = wVABRSD
146000100831     C                   ENDIF
146100100831     C*
146200100831     C                   IF        wVABIND <> *blanks
146300100831     C                   EVAL      VABIND = wVABIND
146400100831     C                   ENDIF
146500100831     C*
146600100831     C                   IF        wVABCAD <> *blanks
146700100831     C                   EVAL      VABCAD = wVABCAD
146800100831     C                   ENDIF
146900100831     C*
147000100831     C                   IF        wVABLOD <> *blanks
147100100831     C                   EVAL      VABLOD = wVABLOD
147200100831     C                   ENDIF
147300100831     C*
147400100831     C                   IF        wVABPRD <> *blanks
147500100831     C                   EVAL      VABPRD = wVABPRD
147600100831     C                   ENDIF
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900070925     C*
148000070925     C* Considerazioni finali su CBO/CAS
148100100318     C                   IF        wFlgCAS = '1'    OR
148200100318     C                             VABCAS  > *zeros
148300070925     C                   IF        VABCBO = '1'
148400070925     C                   EVAL      VABCBO = '4'
148500070925     C                   ENDIF
148600070925     C                   IF        VABCBO = '2'
148700070925     C                   EVAL      VABCBO = '6'
148800070925     C                   ENDIF
148900070925     C                   ENDIF
149000070925     C***  <===  ----------------------------
149100040510     C*
149200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149300040510     C                   IF        VABNSP = *zeros
149400040510     C                   SETON                                        31
149500040510     C                   ENDIF
149600040715     C*
149700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149800040715     C                   IF        VABLNP = *zeros
149900040715     C                   SETON                                        31
150000040715     C                   ENDIF
150100040510     C*
150200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150300040510     C                   IF        VABRMN = *zeros
150400040510     C                   Z-ADD     VABNSP        VABRMN
150500040510     C                   ENDIF
150600040510     C*
150700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150800040510     C                   IF        VABRMA = *blanks
150900040714     C                   MOVEL     VABRMN        VABRMA
151000040510     C                   ENDIF
151100040128     C*
151200040510     C* Se provincia nn valorizzata la reperisco
151300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151400040510     C                   IF        VABPRD = *blanks
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
164200040119     C* 1° invio FIVAT
164300040119     C                   reset                   dscmz
164400040119     C                   move      vlrpoi        cmzdst
164500040119     C                   eval      cmzfld = 'FIVATWWR'
164600040119     C                   eval      cmzmbd = vlrhdl
164700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164800040119     C***                if        prmfir = *blanks
164900040119     C                   eval      cmzfla = 'FIVAT00F'
165000040119     C                   eval      cmzmba = 'FIVAT00F'
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
166700040119     C* 2° invio FIVAB
166800040119     C                   reset                   dscmz
166900040119     C                   move      vlrpoi        cmzdst
167000040119     C                   eval      cmzfld = vlrfou
167100040119     C                   eval      cmzmbd = vlrhdl
167200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167300040119     C***                if        prmfir = *blanks
167400040119     C                   eval      cmzfla = 'FIVAB00F'
167500040119     C                   eval      cmzmba = 'FIVAB00F'
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
172200070329     C                   if        %open(fivabwwr)
172300070329     C                   close     fivabwwr
172400070329     C                   endif
172500070329     C                   if        %open(fivatwwr)
172600070329     C                   close     fivatwwr
172700070329     C                   endif
172800070326     C*
172900070326     C* Effettuo la chiamata al CLLE preposto
173000070326     C                   call(e)   'TITVVTC'
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
180900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181000100318     C                   eval      datcor = %dec(%date() : *ISO)
181100000613     C*
181200000613     C                   ENDSR
181300000613     C***
181400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181500110427VABPKB;VABNCL;W_VABCBO;W_VABNSP;FILXXX;VABNAS;VABRSD;VABRD2;VABIND;VABCAD;
181600170308VABLOD;VABPRD;FILXXX;FILXXX;W_VABNOT;W_VABDCR;VABTSP;VABTC1;VABNZD;VABCTR;
181700110315
181800110315
181900100318
