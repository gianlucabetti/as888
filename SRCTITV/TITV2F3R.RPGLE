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
006400120911     D wAnno           s              4  0
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
008400100909     D*------------------
008500100909     D* DS CALCOLO CHECK-DIGITS
008600100909     D*------------------
008700100909     D TRUL28DSX     e ds                  inz
008800010330
008900010330
009000010330
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400010601     C*
009500010601     C                   exsr      opeini
009600010605     C                   exsr      rwvab
009700010601     C*
009800010601     C                   seton                                        lr
009900010601
010000010601
010100010601
010200010601     C*--------------------------------------------------------
010300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010400010601     C*--------------------------------------------------------
010500010601     C     PREELA        BEGSR
010600010601     C*
010700010601     C* SE OCCORRE SPEDIRE IN FILIALE
010800010601     C                   if        invfil <> *zeros and
010900010601     C                             flgGiro = '0'
011000010601     C*
011100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011200010601     C                   eval      flgGiro = '1'
011300010601     C*
011400010601     C                   endif
011500010601     C*
011600010601     C                   ENDSR
011700010601     C***
011800010601
011900010601
012000010601
012100010601
012200010601     C*--------------------------------------------------------
012300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012400010601     C*--------------------------------------------------------
012500010601     C     ENDELA        BEGSR
012600040119     C*
012700040119     C* Effettuo la chiamata al CLLE preposto
012800040119     C                   call(e)   'TITVVTC'
012900040510     C                   parm                    parccm
013000040119     C                   parm                    parmbr
013100040119     C                   parm      '2'           paropz
013200050201     C*
013300050201     C* Effettuo lancio TISI95 solo x chiusura
013400050201     C                   CLEAR                   TISI95DS
013500050201     C                   EVAL      I95TLA = 'C'
013600050201     C                   CALL      'TISI95R'
013700050201     C                   PARM                    TISI95DS
013800000616     C*
013900010601     C                   ENDSR
014000010601     C***
014100000613
014200010601
014300010601
014400010330     C*--------------------------------------------------------
014500021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014600010330     C*--------------------------------------------------------
014700010605     C     RWVAB         BEGSR
014800010330     C*
014900010330     C                   if        not %open(tivin00r)
015000010330     C                   open      tivin00r
015100010330     C                   endif
015200021025     C                   if        not %open(fivabwwr)
015300021025     C                   open      fivabwwr
015400010330     C                   endif
015500040119     C*
015600040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015700040119     C                   exsr      prevat
015800040119     C*
015900040119     C                   if        chkcall = '0'
016000040119     C*
016100040119     C                   if        not %open(fivatwwr)
016200040119     C                   open      fivatwwr
016300040119     C                   endif
016400010330     C*
016500040119     C                   clear                   §CTROKVB          7 0
016600040119     C                   clear                   §CTROKVT          7 0
016700040119     C                   clear                   §CTRMO            7 0
016800040119     C                   clear                   §CTRNO            7 0
016900070823     C*
017000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017200110415     C                   exsr      inzvar
017300110415     C                   exsr      impvab
017400110131     C*
017500110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017600110315 xxx C***                read      tivin00r                               70
017700010330     C*
017800030822     C                   do        *HIVAL
017900010330     C*
018000030822     C                   read      tivin00r                               70
018100010618     C*
018200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018300010618     C                   if        vinflg = '1'
018400010618     C                   eval      flgOk = '1'
018500010618     C                   endif
018600040510     C*
018700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018800040510     C                   z-add     *zeros        wLenVINDTA        4 0
018900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019300040510     C                   z-add     1             wFlgVINDTA
019400040510     C                   leave
019500040510     C                   endif
019600040510     C                   enddo
019700010618     C*
019800040510     C                   if        vindta > *blanks AND
019900040510     C                             wFlgVINDTA = 1
020000010330     C                   add       1             rrnum
020100010330     C*
020200010601     C                   if        *in70 = *off and
020300010330     C                             (vinflg = *blanks
020400010330     C                              or vinflg = '0'
020500010330     C                              or vinflg = '2')
020600010330     C*
020700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020800010711     C                   if        vinflg = *blanks or vinflg = '0'
020900010711     C                   clear                   vinmsg
021000010711     C                   endif
021100010601     C*
021200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021300070925     C                   eval      vindta  = ' ' + %trim(vindta)
021400070925     C*
021500080527     C                   exsr      inzvar
021600100318     C                   exsr      normCSV
021700010605     C                   exsr      impvab
021800010601     C*
021900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022000010605     C                   eval      depfil = VABLNP
022100010601     C                   exsr      repfil
022200010601     C                   if        depfil = invfil
022300021025     C                   if        vlrpoi = 999
022400030822     C                   move(P)   invfil        VABFGS
022500021025     C                   else
022600030822     C                   move(P)   vlrpoi        VABFGS
022700021025     C                   endif
022800010601     C*
022900010601     C                   exsr      PREELA
023000010601     C*
023100010604     C* Ebbene...
023200010604     C*
023300040119     C  N31              add       1             §CTROKVB
023400040119     C   32              add       1             §CTRMO
023500040119     C   31              add       1             §CTRNO
023600080418     C*
023700030822     C                   if        wGiro = 2
023800080513     C* Scrivo sempre il VAT
023900080513     C  N31              exsr      wrivat
024000110201     C                   exsr      chkvab
024100080513     C                   endif
024200010604     C*
024300010604     C                   if        *in31 = *off and
024400010604     C                             *in32 = *off
024500010604     C                   eval      vinflg = '1'
024600010604     C                   else
024700010604     C                   eval      vinflg = '2'
024800010604     C                   endif
024900010604     C                   endif
025000010601     C*
025100010604     C                   endif
025200010604     C*
025300010330     C                   else
025400010330     C                   eval      vinflg = '1'
025500010330     C                   endif
025600010601     C*
025700010601     C  N70              update    tivin000
025800010330     C*
025900030822     C  N70              enddo
026000040119     C*
026100080527     C* Verifico dati rimasti "in cannna"...
026200080527     C                   seton                                        65
026300110201     C                   exsr      chkvab
026400080527     C*
026500040119     C                   endif                                                  (endif - chkcall)
026600010601     C*
026700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026800010601     C                   if        cntNonEl = *zeros or
026900010601     C                             flgMulti = '0'
027000010330     C* Se non ci sono record con errori ...
027100010601     C                   if        §ctrno = 0 and
027200010604     C                             §ctrmo = 0 and
027300010601     C                             flgStato <> '2'
027400010330     C* ... restituisco esito OK.
027500010330     C                   eval      wrkesito = '0'
027600010330     C                   else
027700040119     C                   if        §ctrokvb > 0
027800010330     C                   eval      wrkesito = '1'
027900010330     C                   else
028000010615     C                   if        flgOk = '0'
028100010615     C                   eval      wrkesito = '2'
028200010615     C                   else
028300010615     C                   eval      wrkesito = '6'
028400010615     C                   endif
028500010330     C                   endif
028600010330     C                   endif
028700010601     C                   else
028800010601     C                   eval      wrkesito = '9'
028900010601     C                   endif
029000010330     C*
029100010330     C                   if        %open(tivin00r)
029200010330     C                   close     tivin00r
029300010330     C                   endif
029400021025     C                   if        %open(fivabwwr)
029500021025     C                   close     fivabwwr
029600010330     C                   endif
029700040512     C                   if        %open(fivatwwr)
029800040512     C                   close     fivatwwr
029900040512     C                   endif
030000010601     C*
030100010601     C                   if        vlrpoi <> 999
030200010601     C                   eval      invfil = vlrpoi
030300010601     C                   endif
030400010330     C*
030500040119     C                   if        §ctrokvb > 0
030600010601     C                             and invfil > *zeros
030700010330     C                   exsr      invio
030800010330     C                   endif
030900010601     C*
031000010618     C                   if        flgGiro = '1'
031100010601     C                   exsr      endela
031200010618     C                   endif
031300010330     C*
031400010330     C                   ENDSR
031500010330     C***
031600080527
031700080527
031800080527
031900080527     C*----------------------------------------------------*
032000080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032100080527     C*----------------------------------------------------*
032200100413     C     CHKVAB        BEGSR
032300110201     C*
032400110201     C* Se richiesti ignorare colli / peso / volume
032500110315 xxx C***                if        *in35
032600110315 xxx C***                z-add     *zeros        VABNCL
032700110315 xxx C***                z-add     *zeros        VABPKB
032800110315 xxx C***                z-add     *zeros        VABVLB
032900110315 xxx C***                endif
033000080527     C*
033100080527     C* Scrivo il VAB solo a rottura d spedizione
033200080527     C                   if        depspe = *zeros
033300080527     C  N31              eval      fivabds_sav = fivabds
033400080527     C                   eval      depspe = vabNSP
033500080527     C                   else
033600080527     C                   if        depspe = vabNSP AND *in65 = *off
033700100413     C   51              add       VABNCL        sav_VABNCL
033800100413     C   51              add       VABPKB        sav_VABPKB
033900100413     C   51              add       VABVLB        sav_VABVLB
034000100720     C   52              z-add     VABNCL        sav_VABNCL
034100100720     C   52              z-add     VABPKB        sav_VABPKB
034200100720     C   52              z-add     VABVLB        sav_VABVLB
034300100720     C   53              add       VABNCL        sav_VABNCL
034400100720     C   53              z-add     VABPKB        sav_VABPKB
034500100720     C   53              z-add     VABVLB        sav_VABVLB
034600080527     C                   else
034700080527     C  N31              eval      fivabds_ok = fivabds_sav
034800080527     C  N31              exsr      wrivab
034900080527     C  N31              eval      fivabds_sav = fivabds
035000080527     C                   eval      depspe = vabNSP
035100080527     C                   endif
035200080527     C                   endif
035300080527     C*
035400080527     C                   ENDSR
035500040119
035600040119
035700040119
035800040119     C*----------------------------------------------------*
035900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036000040119     C*----------------------------------------------------*
036100040510     C     WRIVAB        BEGSR
036200080513     C*
036300080513     C* Gestisco l'eventuale rottura x numero spedizione
036400080527     C                   if        VABNSP <> depspe or *in65 = *on
036500070301     C                   if        wVATNOT_E <> *blanks
036600070301     C                   eval      VABCTM = '7Q'
036700070301     C                   endif
036800100413     C                   write     fivab000
036900080513     C                   endif
037000040119     C*
037100040119     C                   ENDSR
037200040119
037300040119
037400040119
037500040119     C*----------------------------------------------------*
037600040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037700040119     C*----------------------------------------------------*
037800040510     C     WRIVAT        BEGSR
037900060512     C*
038000060512     C* Inizializzo i campi chiave
038100060512     C                   eval      VATFGS = VABFGS
038200060512     C                   eval      VATAAS = VABAAS
038300060512     C                   eval      VATCCM = VABCCM
038400070823     C                   eval      VATNRS = VABNRS
038500060512     C                   eval      VATNSP = VABNSP
038600060512     C                   eval      VATLNP = VABLNP
038700040119     C*
038800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038900040510     C                   if        VATTRC = *blanks
039000040510     C                   eval      VATTRC = 'E'
039100040510     C                   endif
039200110318     C                   if        VATNOT <> *blanks
039300040510     C                   write     FIVAT000
039400040119     C                   add       1             §CTROKVT
039500110318     C                   endif
039600060331     C*
039700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039800060331     C                   if        wVATNOT_A <> *blanks
039900060331     C                   eval      VATNOT = wVATNOT_A
040000060331     C                   eval      VATTRC = 'A'
040100060331     C                   write     FIVAT000
040200060331     C                   add       1             §CTROKVT
040300060331     C                   endif
040400060331     C*
040500060331     C                   if        wVATNOT_B <> *blanks
040600060331     C                   eval      VATNOT = wVATNOT_B
040700060331     C                   eval      VATTRC = 'B'
040800060331     C                   write     FIVAT000
040900060331     C                   add       1             §CTROKVT
041000060331     C                   endif
041100060331     C*
041200120911     C* il barcode è costrito fuori ciclo della riga CSV
041300060331     C                   if        wVATNOT_E <> *blanks
041400110201     C                   eval      VATNOT = wVATNOT_E
041500060331     C                   eval      VATTRC = 'E'
041600120913     C*
041700120913     C* Sviluppati i "CHI SONO" in relazione al numero colli
041800120913     C*
041900120913     C     1             DO        VABNCL        wNumCollo         2 0
042000120913      *** da analisi del POC il progressivo colli è max 2 char e non contiene gli zeri non
042100120913      *** significativi ma neanche i blank a sx
042200120913     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
042300120913     C                                %trim(%char(wNumCollo))
042400120913     C                   write     FIVAT000
042500120913     C                   add       1             §CTROKVT
042600120913     C                   ENDDO
042700120913     C*
042800110201     C                   endif
042900040119     C*
043000040119     C                   ENDSR
043100040119
043200040119
043300040119
043400040119     C*----------------------------------------------------*
043500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043600040119     C*----------------------------------------------------*
043700040119     C     PREVAT        BEGSR
043800040119     C*
043900040119     C* Compongo il nome del membro da dare al FIVATWWR
044000040119     C                   eval      parmbr = vlrhdl
044100040119     C                   movel     'M'           parmbr
044200040510     C                   eval      parccm = vlrksc
044300040119     C                   eval      paropz = '1'
044400040119     C* Effettuo la chiamata al CLLE preposto
044500040119     C                   call(e)   'TITVVTC'
044600040119     C                   parm                    parccm
044700040119     C                   parm                    parmbr
044800040119     C                   parm                    paropz
044900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045000040119     C                   if        %error
045100040119     C                   movel     '1'           chkcall
045200040119     C                   else
045300040119     C                   movel     '0'           chkcall
045400040119     C                   endif
045500040119     C*
045600040119     C                   ENDSR
045700010601
045800010601
045900010601
046000010330     C*----------------------------------------------------*
046100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046200010330     C*----------------------------------------------------*
046300040119     C     INZVAR        BEGSR
046400080527     C*
046500080527     C                   setoff                                       65
046600010330     C*
046700030822     C* Inizializzo variabili di wrk
046800020204     C                   Z-ADD     *zeros        Num5_0            5 0
046900120911     C                   Z-ADD     *zeros        wAnno
047000101018     C                   MOVEL     *blanks       wFlgCAS
047100060331     C                   MOVEL     *blanks       wVATNOT_A
047200060331     C                   MOVEL     *blanks       wVATNOT_B
047300060331     C                   MOVEL     *blanks       wVATNOT_E
047400100831     C                   MOVEL     *blanks       wVABRSD
047500100831     C                   MOVEL     *blanks       wVABIND
047600100831     C                   MOVEL     *blanks       wVABCAD
047700100831     C                   MOVEL     *blanks       wVABLOD
047800100831     C                   MOVEL     *blanks       wVABPRD
047900030822     C*
048000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048100030822     C                   CLEAR                   SkSplitCSV
048200030822     C                   EVAL      i = 1
048300030822     C                   EVAL      posDa = *zeros
048400030822     C                   EVAL      posA  = *zeros
048500080527     C*
048600080527     C                   CLEAR                   FIVABDS
048700080527     C                   CLEAR                   FIVAT000
048800030822     C*
048900030822     C* Reimposto i valori di default
049000030822     C                   EXSR      DEFCAM
049100030822     C*
049200010330     C                   ENDSR
049300010330     C*----------------------------------------------------*
049400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049500010330     C*----------------------------------------------------*
049600010330     C     DEFCAM        BEGSR
049700030822     C*
049800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049900030715     C* e delimitatore testo.
050000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050300100318     C*
050400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050500100318     C                   EVAL      CharSOS = CharNUM
050600030822     C*
050700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050800100720     C                   SETOFF                                       505152
050900100720     C                   SETOFF                                       53
051000100413     C                   SELECT
051100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051200100720     C                   SETON                                        50
051300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051400100413     C                   SETON                                        51
051500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051600100720     C                   SETON                                        52
051700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051800100720     C                   SETON                                        53
051900100413     C                   ENDSL
052000030822     C*
052100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052200030822     C                   EVAL      posDaDft = 1
052300030822     C                   EVAL      posADft  = 0
052400030822     C                   EVAL      wGiroDft = 0
052500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052600030822     C                             posDaDft > 0
052700030822     C*
052800030822     C* Gestisco il 1° giro
052900030822     C                   IF        wGiroDft = 0
053000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053200030822     C* Incremento il contatore dei "giri"
053300030822     C                   EVAL      wGiroDft = 1
053400030822     C                   ELSE
053500030822     C                   EVAL      posDaDft = posADft
053600030822     C                   ENDIF
053700030822     C* Eseguo lo scan x trovare la fine del campo corrente
053800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053900030822     C*
054000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054200030822     C                   IF        posDaDft > 0 AND
054300030822     C                             posADft  > 0
054400030822     C* NCL
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'NCL'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiInt=*on
055400030822     C                   Z-ADD     PiVal         VABNCL
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* CCM
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'CCM'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiInt=*on
056700030822     C                   Z-ADD     PiVal         VABCCM
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* LNP
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'LNP'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiInt=*on
058000030822     C                   Z-ADD     PiVal         VABLNP
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* NRS
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'NRS'
058800030822     C                   EVAL      PiStr=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   EXSR      CHKNUM
059200030822     C                   IF        PiInt=*on
059300030822     C                   Z-ADD     PiVal         VABNRS
059400030822     C                   ENDIF
059500030822     C                   ENDIF
059600030822     C* CTR
059700030822     C                   IF        %subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):1:3)
060000030822     C                             = 'CTR'
060100030822     C                   EVAL      PiStr=%trim(%subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060400030822     C                   EXSR      CHKNUM
060500030822     C                   IF        PiInt=*on
060600030822     C                   Z-ADD     PiVal         VABCTR
060700030822     C                   ENDIF
060800030822     C                   ENDIF
060900030822     C* PKB
061000030822     C                   IF        %subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):1:3)
061300030822     C                             = 'PKB'
061400030822     C                   EVAL      PiStr=%trim(%subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061700030822     C                   EXSR      CHKNUM
061800030822     C                   IF        PiNum=*on
061900030822     C                   Z-ADD     PiVal         VABPKB
062000030822     C                   ENDIF
062100030822     C                   ENDIF
062200030822     C* VLB
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'VLB'
062700030822     C                   EVAL      PiStr=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   EXSR      CHKNUM
063100030822     C                   IF        PiNum=*on
063200030822     C                   Z-ADD     PiVal         VABVLB
063300030822     C                   ENDIF
063400030822     C                   ENDIF
063500030822     C* QFT
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'QFT'
064000030822     C                   EVAL      PiStr=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   EXSR      CHKNUM
064400030822     C                   IF        PiNum=*on
064500030822     C                   Z-ADD     PiVal         VABQFT
064600030822     C                   ENDIF
064700030822     C                   ENDIF
064800030822     C* CBO
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CBO'
065300030822     C                   EVAL      VABCBO=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* TSP
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'TSP'
066200030822     C                   EVAL      VABTSP=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VAS
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VAS'
067100030822     C                   EVAL      VABVAS=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* VCA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VCA'
068000030822     C                   EVAL      VABVCA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400070925     C* TIC
068500070925     C                   IF        %subst(
068600070925     C                             %subst(vlrppt:posDaDft+1:
068700070925     C                             posADft-posDaDft-1):1:3)
068800070925     C                             = 'TIC'
068900070925     C                   EVAL      VABTIC=%trim(%subst(
069000070925     C                             %subst(vlrppt:posDaDft+1:
069100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200070925     C                   ENDIF
069300030822     C* GCA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GCA'
069800030822     C                   EVAL      VABGCA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* CTM
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'CTM'
070700030822     C                   EVAL      VABCTM=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* FFD
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'FFD'
071600030822     C                   EVAL      VABFFD=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* VAD
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'VAD'
072500030822     C                   EVAL      VABVAD=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* GMA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GMA'
073400030822     C                   EVAL      VABGMA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* GGA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GGA'
074300030822     C                   EVAL      VABGGA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* GVA
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'GVA'
075200030822     C                   EVAL      VABGVA=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* TC1
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'TC1'
076100030822     C                   EVAL      VABTC1=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500030822     C* TC2
076600030822     C                   IF        %subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):1:3)
076900030822     C                             = 'TC2'
077000030822     C                   EVAL      VABTC2=%trim(%subst(
077100030822     C                             %subst(vlrppt:posDaDft+1:
077200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300030822     C                   ENDIF
077400040714     C* VATTRC
077500040714     C                   IF        %subst(
077600040714     C                             %subst(vlrppt:posDaDft+1:
077700040714     C                             posADft-posDaDft-1):1:3)
077800040714     C                             = 'TRC'
077900040714     C                   EVAL      VATTRC=%trim(%subst(
078000040714     C                             %subst(vlrppt:posDaDft+1:
078100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078200040714     C                   ENDIF
078300030822     C* ...
078400030822     C                   ENDIF
078500030822     C                   ENDDO
078600020204     C*
078700010330     C                   ENDSR
078800010607     C*----------------------------------------------------*
078900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079000010607     C*----------------------------------------------------*
079100010607     C     IMPVAB        BEGSR
079200010607     C*
079300010607     C                   SETOFF                                       3132
079400110315 xxx C***                SETON                                        35
079500030515     C*
079600010607     C                   MOVEL     datcor        VABAAS
079700010607     C                   MOVE      datcor        VABMGS
079800010607     C*
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080100030715     C**********
080200030715     C                   IF        wGiro = *zeros
080300040113     C*
080400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080700080429     C                             %trim(intcol(5))<>*blanks
080800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                                      %trim(intcol(5))
081100070823     C     ';':CharCSV   XLATE     vindta        vindta
081200040510     C                   ENDIF
081300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081400031201     C     minu:maiu     XLATE     vindta        vindta
081500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081600030715     C                   DOW       posDa <= %len(%trim(vindta))
081700030715     C*
081800030715     C* Gestisco il 1° campo
081900030715     C                   IF        i = 1
082000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082100030715     C                   EVAL      posDa = 1
082200030715     C* Eseguo lo scan x trovare la fine del primo campo
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C* X i campi successivi al 1°
082800030715     C                   ELSE
082900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083000030715     C                   EVAL      posDa = posA + 1
083100030715     C* Eseguo lo scan x trovare la fine del campo corrente
083200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083300030715     C* Gestisco l'ultimo campo
083400030715     C                   IF        posA = *zeros
083500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083600030715     C* A questo punto "estraggo" il campo corrente
083700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083800030715     C                                                    (posA-posDa))
083900030715     C                   LEAVE
084000030715     C                   ELSE
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   ENDIF
084500030715     C                   ENDIF
084600030715     C* Incremento il contatore d campo
084700030715     C                   EVAL      i = i +1
084800030715     C                   ENDDO
084900030715     C*
085000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085100030715     C* delimitazione testo
085200030715     C                   EVAL      i = 1
085300030715     C                   DOW       i < %elem(SkSplitFLD)
085400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085600030715     C                   EVAL      i = i + 1
085700030715     C                   ENDDO
085800030715     C                   EVAL      wGiro = 1
085900030715     C                   ELSE
086000030715     C                   EVAL      wGiro = 2
086100031204     C**********
086200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086300031204     C* d separatore campo
086400031204     C**********
086500031204     C                   EVAL      vindta = %trim(vindta)
086600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086700031204     C                   EVAL      vindta = %subst(vindta:2)
086800031204     C                   ENDDO
086900031204     C*
087000031204     C                   Z-ADD     *zeros        lunghInput        4 0
087100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   ENDDO
087600030715     C**********
087700030715     C* Effettuo lo split del campo dati d input
087800030715     C**********
087900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088000030715     C                   DOW       posDa <= %len(%trim(vindta))
088100030715     C*
088200030715     C* Gestisco il 1° campo
088300030715     C                   IF        i = 1
088400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088500030715     C                   EVAL      posDa = 1
088600030715     C* Eseguo lo scan x trovare la fine del primo campo
088700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C* X i campi successivi al 1°
089200030715     C                   ELSE
089300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089400030715     C                   EVAL      posDa = posA + 1
089500031204     C* Verifico che nn vi sia il campo nn valorizzato
089600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089700031204     C* Se campo nn valorizzato skippo al prossimo
089800031204     C                   EVAL      SkSplitCSV(i) = *blanks
089900031204     C                   EVAL      posA  = posA + 1
090000031204     C                   ELSE
090100030715     C* Eseguo lo scan x trovare la fine del campo corrente
090200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090300030715     C* Gestisco l'ultimo campo
090400030715     C                   IF        posA = *zeros
090500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090600030715     C* A questo punto "estraggo" il campo corrente
090700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090800030715     C                                                    (posA-posDa))
090900030715     C                   LEAVE
091000030715     C                   ELSE
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C                   ENDIF
091500031204     C                   ENDIF
091600030715     C                   ENDIF
091700030715     C* Incremento il contatore d campo
091800030715     C                   EVAL      i = i +1
091900030715     C                   ENDDO
092000030715     C*
092100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092200030715     C* delimitazione testo
092300030715     C                   EVAL      i = 1
092400030715     C                   DOW       i < %elem(SkSplitCSV)
092500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092700030715     C                   EVAL      i = i + 1
092800030715     C                   ENDDO
092900100720     C*
093000100720     C* Stacco un progressivo x numero spedizione
093100110421 xxx C                   EXSR      repNSP
093200100326     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094200030721     C                   EVAL      VABATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RSD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094900030715     C* ==
095000030715     C                   ENDIF
095100030721     C*** RD2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** IND
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095700030715     C                   EVAL      VABIND = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** LOD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** PRD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030721     C*** NZD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** RMA
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NOT
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NT2
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VCA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VAS
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** FFD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** GC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** GC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** TSP
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100600030721     C                   ENDIF
100700040510     C*** CBO
100800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101000040510     C                   ENDIF
101100030721     C*** NAS
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** TIC
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GCA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** XCO
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** CTM
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** TCR
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTS
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** FTM
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** VAD
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GMA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GGA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GVA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TC1
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** TC2
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** SCL
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** RMO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** NMO
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*
108000030721     C* Reperisco quindi i campi numerici...
108100040510     C*** NSP
108200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108300040510     C                   EVAL      PiStr=SkSplitCSV(i)
108400040510     C                   EXSR      CHKNUM
108500040510     C                   IF        PiInt=*on
108600040510     C                   Z-ADD     PiVal         VABNSP
108700040510     C                   ELSE
108800040510     C                   SETON                                        31
108900040510     C                   EVAL      VABNSP = *zeros
109000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109100040510     C                             + ' ' + 'VABNSP'
109200040510     C                   ENDIF
109300040510     C                   ENDIF
109400071106     C*** RMN
109500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109600071106     C                   EVAL      PiStr=SkSplitCSV(i)
109700071106     C                   EXSR      CHKNUM
109800071106     C                   IF        PiInt=*on
109900071106     C                   Z-ADD     PiVal         VABRMN
110000071106     C                   ELSE
110100071106     C                   SETON                                        32
110200071106     C                   EVAL      VABRMN = *zeros
110300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110400071106     C                             + ' ' + 'VABRMN'
110500071106     C                   ENDIF
110600071106     C                   ENDIF
110700030721     C*** CAD
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         Num5_0
111300030721     C                   MOVEL(p)  Num5_0        VABCAD
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAD = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAD'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** NCL
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         VABNCL
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABNCL = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100040122     C                             + ' ' + 'VABNCL'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** PKB
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113600030721     C                   EVAL      PiStr=SkSplitCSV(i)
113700030721     C                   EXSR      CHKNUM
113800030721     C                   IF        PiNum=*on
113900030721     C                   Z-ADD     PiVal         VABPKB
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABPKB = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABPKB'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** CAS
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115000110506     C                             %trim(SkSplitCSV(i)) <> *zeros
115100101018     C                   EVAL      wFlgCAS = '1'
115200101018     C                   ENDIF
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiNum=*on
115600030721     C                   Z-ADD     PiVal         VABCAS
115700030721     C                   ELSE
115800030721     C                   SETON                                        32
115900030721     C                   EVAL      VABCAS = *zeros
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABCAS'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** CCM
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on      AND
116900030721     C                             PiVal<=9999999 AND
117000030721     C                             PiVal>*zeros
117100030721     C                   Z-ADD     PiVal         VABCCM
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABCCM = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600040510     C                             + ' ' + 'VABCCM'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** LNP
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on    AND
118400030721     C                             PiVal<=999   AND
118500030721     C                             PiVal>*zeros
118600030721     C                   Z-ADD     PiVal         VABLNP
118700030721     C                   ELSE
118800040715     C                   SETON                                        32
118900030721     C                   EVAL      VABLNP = *zeros
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100040510     C                             + ' ' + 'VABLNP'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** NRS
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on AND
119900030721     C                             PiVal<=99
120000030721     C                   Z-ADD     PiVal         VABNRS
120100030721     C                   ELSE
120200030721     C                   SETON                                        32
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400030721     C                             + ' ' + 'VABNRS'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** LNA
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on    AND
121200030721     C                             PiVal<=999   AND
121300030721     C                             PiVal>*zeros
121400030721     C                   Z-ADD     PiVal         VABLNA
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      VABLNA = *zeros
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABLNA'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** CTR
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on  AND
122700030721     C                             PiVal<=999
122800030721     C                   Z-ADD     PiVal         VABCTR
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABCTR'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** IAS
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiNum=*on
124000030721     C                   Z-ADD     PiVal         VABIAS
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABIAS'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** VLB
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiNum=*on
125200030721     C                   Z-ADD     PiVal         VABVLB
125300030721     C                   ELSE
125400030721     C                   SETON                                        32
125500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125600030721     C                             + ' ' + 'VABVLB'
125700030721     C                   ENDIF
125800030721     C                   ENDIF
125900030721     C*** QFT
126000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126100030721     C                   EVAL      PiStr=SkSplitCSV(i)
126200030721     C                   EXSR      CHKNUM
126300030721     C                   IF        PiNum=*on
126400030721     C                   Z-ADD     PiVal         VABQFT
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABQFT'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NCD
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on      AND
127600030721     C                             PiVal<=9999999
127700030721     C                   Z-ADD     PiVal         VABNCD
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNCD'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCA
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCA
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCA'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** ZNC
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on AND
130200030721     C                             PiVal<=99
130300030721     C                   Z-ADD     PiVal         VABZNC
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABZNC'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** DCR
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on       AND
131500030721     C                             PiVal<=99999999
131600030721     C                   Z-ADD     PiVal         VABDCR
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABDCR'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** HCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on   AND
132800030721     C                             PiVal<=9999
132900030721     C                   Z-ADD     PiVal         VABHCR
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABHCR'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** VMD
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiNum=*on
134100030721     C                   Z-ADD     PiVal         VABVMD
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABVMD'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** ANT
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on        AND
135300030721     C                             PiVal<=999999999
135400030721     C                   Z-ADD     PiVal         VABANT
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABANT'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** CMO
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiInt=*on
136600030721     C                   Z-ADD     PiVal         Num5_0
136700030721     C                   MOVEL(p)  Num5_0        VABCMO
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      VABCMO = *zeros
137100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137200030721     C                             + ' ' + 'VABCMO'
137300030721     C                   ENDIF
137400030721     C                   ENDIF
137500040510     C*
137600040510     C* Reperisco i campi del FIVAT
137700040510     C*** VATNOT
137800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138000040510     C                   ENDIF
138100040510     C*** VATTRC
138200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138400040510     C                   ENDIF
138500060331     C*** VATNOT_A
138600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900060331     C*** VATNOT_B
139000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300060331     C*** VATNOT_E
139400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700070823     C*
139800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139900110421     C*
140000110506     C*** TIC
140100110506     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
140200110506     C                   SELECT
140300110506     C                   WHEN      %trim(SkSplitCSV(i)) = '7'  or
140400110506     C                             %trim(SkSplitCSV(i)) = '10' or
140500110506     C                             %trim(SkSplitCSV(i)) = '51' or
140600110506     C                             %trim(SkSplitCSV(i)) = '59'
140700110506     C                   EVAL      VABTIC = 'BM'
140800110506     C                   WHEN      %trim(SkSplitCSV(i)) = '144'
140900110506     C                   EVAL      VABTIC = 'CM'
141000120726     C                   WHEN      %trim(SkSplitCSV(i)) = '72' or
141100120726     C                             %trim(SkSplitCSV(i)) = '82'
141200120726     C                   EVAL      VABTIC = *blanks
141300110506     C                   WHEN      %trim(SkSplitCSV(i)) = '20'
141400110506     C                   EVAL      VABTIC = 'TM'
141500110506     C                   ENDSL
141600110415     C                   ENDIF
141700120911     C*** Anno spedizione dal cliente
141800120911     C                   IF        %trim(SkSplitFLD(i)) = 'WANNO'
141900120911     C                   EVAL      PiStr=SkSplitCSV(i)
142000120911     C                   EXSR      CHKNUM
142100120911     C                   IF        PiInt=*on
142200120911     C                   Z-ADD     PiVal         wAnno
142300120911     C                   ELSE
142400120911     C                   SETON                                        32
142500120911     C                   EVAL      vinmsg = %trimr(vinmsg)
142600120911     C                             + ' ' + 'WANNO'
142700120911     C                   ENDIF
142800120911     C                   ENDIF
142900130702     C*** CBO
143000130702     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
143100130702     C                   IF        %trim(SkSplitCSV(i)) = 'F'
143200130702     C                   EVAL      VABCBO = '1'
143300130702     C                   ENDIF
143400130702     C                   IF        %trim(SkSplitCSV(i)) = 'A'
143500130702     C                   EVAL      VABCBO = '2'
143600130702     C                   ENDIF
143700130702     C                   ENDIF
143800070925     C*
143900071029     C***  <===  -----------------------------------------
144000040510     C*
144100030715     C                   ENDIF
144200030715     C                   EVAL      i = i + 1
144300030715     C                   ENDDO
144400070925     C*
144500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144600100831     C*
144700100831     C                   IF        wVABRSD <> *blanks
144800100831     C                   EVAL      VABRSD = wVABRSD
144900100831     C                   ENDIF
145000100831     C*
145100100831     C                   IF        wVABIND <> *blanks
145200100831     C                   EVAL      VABIND = wVABIND
145300100831     C                   ENDIF
145400100831     C*
145500100831     C                   IF        wVABCAD <> *blanks
145600100831     C                   EVAL      VABCAD = wVABCAD
145700100831     C                   ENDIF
145800100831     C*
145900100831     C                   IF        wVABLOD <> *blanks
146000100831     C                   EVAL      VABLOD = wVABLOD
146100100831     C                   ENDIF
146200100831     C*
146300100831     C                   IF        wVABPRD <> *blanks
146400100831     C                   EVAL      VABPRD = wVABPRD
146500100831     C                   ENDIF
146600120911     C*
146700120913     C* creo la prima parte del barcode come:
146800120913     C* AGB fisso
146900120911     C* 4 char = anno cliente (1a colonna CSV)
147000120911     C* 3 char = n. DDT (VABRMN)
147100120913     C* poi svilupperò rispetto ai colli
147200120911     C* max 2 char = progressivo collo
147300120911     C                   EVAL      wVATNOT_E = 'AGB' + %char(wAnno) +
147400120913     C                             %subst(%editc(VABRMN:'X') : 11 : 5)
147500070925     C*
147600071029     C***  <===  -----------------------------------------
147700070925     C*
147800070925     C* Considerazioni finali su CBO/CAS
147900100318     C                   IF        wFlgCAS = '1'    OR
148000100318     C                             VABCAS  > *zeros
148100070925     C                   IF        VABCBO = '1'
148200070925     C                   EVAL      VABCBO = '4'
148300070925     C                   ENDIF
148400070925     C                   IF        VABCBO = '2'
148500070925     C                   EVAL      VABCBO = '6'
148600070925     C                   ENDIF
148700070925     C                   ENDIF
148800070925     C***  <===  ----------------------------
148900040510     C*
149000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149100040510     C                   IF        VABNSP = *zeros
149200040510     C                   SETON                                        31
149300040510     C                   ENDIF
149400040715     C*
149500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149600040715     C                   IF        VABLNP = *zeros
149700040715     C                   SETON                                        31
149800040715     C                   ENDIF
149900040510     C*
150000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150100040510     C                   IF        VABRMN = *zeros
150200040510     C                   Z-ADD     VABNSP        VABRMN
150300040510     C                   ENDIF
150400040510     C*
150500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150600040510     C                   IF        VABRMA = *blanks
150700040714     C                   MOVEL     VABRMN        VABRMA
150800040510     C                   ENDIF
150900040128     C*
151000040510     C* Se provincia nn valorizzata la reperisco
151100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151200040510     C                   IF        VABPRD = *blanks
151300040128     C                   CLEAR                   TISI95DS
151400040128     C                   EVAL      I95TCN = '3'
151500040128     C                   Z-ADD     datcor        I95DAT
151600040128     C                   EVAL      I95NAR = VABNZD
151700040128     C                   EVAL      I95CAP = VABCAD
151800040128     C                   EVAL      I95LOC = VABLOD
151900040128     C                   CALL      'TISI95R'
152000040128     C                   PARM                    TISI95DS
152100040128     C                   EVAL      VABPRD = O95PRV
152200040510     C                   ENDIF
152300020204     C*
152400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152500020204     C                   EXSR      CHKIMPDIV
152600030715     C                   ENDIF
152700020204     C*
152800010607     C                   ENDSR
152900010607     C*----------------------------------------------------*
153000010601
153100020204
153200020204     C*----------------------------------------------------*
153300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153400020204     C*----------------------------------------------------*
153500020204     C     CHKIMPDIV     BEGSR
153600020204     C*
153700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153800020204     C                   Z-ADD     *zeros        wrkDec            9 9
153900020204     C*
154000020204     C* Come prima cosa effettuo considerazioni sulla divisa
154100020204     C                   IF        vabIAS > *zeros
154200020204     C                   IF        vabVAS <> 'EUR'
154300020204     C                   EVAL      vabVAS =  'ITL'
154400020204     C                   ENDIF
154500020204     C                   ENDIF
154600020204     C*
154700020204     C                   IF        vabCAS > *zeros
154800020204     C                   IF        vabVCA <> 'EUR'
154900020204     C                   EVAL      vabVCA =  'ITL'
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C                   IF        vabVMD > *zeros
155400020204     C                   IF        vabVAD <> 'EUR'
155500020204     C                   EVAL      vabVAD =  'ITL'
155600020204     C                   ENDIF
155700020204     C                   ENDIF
155800020204     C*
155900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156000020204     C                   Z-ADD     vabIAS        wrkDec
156100020204     C                   IF        wrkDec > *zeros
156200020204     C                   IF        vabVAS = 'ITL'
156300020204     C                   EVAL      vabIAS = *zeros
156400020204     C                   ENDIF
156500020204     C                   ENDIF
156600020204     C*
156700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156800020204     C                   Z-ADD     vabCAS        wrkDec
156900020204     C                   IF        wrkDec > *zeros
157000020204     C                   IF        vabVCA = 'ITL'
157100020204     C                   EVAL      vabCAS = *zeros
157200020204     C                   ENDIF
157300020204     C                   ENDIF
157400020204     C*
157500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157600020204     C                   Z-ADD     vabVMD        wrkDec
157700020204     C                   IF        wrkDec > *zeros
157800020204     C                   IF        vabVAD = 'ITL'
157900020204     C                   EVAL      vabVMD = *zeros
158000020204     C                   ENDIF
158100020204     C                   ENDIF
158200020204     C*
158300020204     C                   ENDSR
158400020204     C***
158500010330
158600100318
158700100318
158800100318     C*----------------------------------------------------*
158900100318     C*  CONTROLLO NUMERICITA' CAMPI
159000100318     C*----------------------------------------------------*
159100100318     C     CHKNUM        BEGSR
159200100318     C*
159300100318     C                   IF        PiDecChr = *blanks
159400100318     C                   EVAL      PiDecChr = CharNUM
159500100318     C                   ENDIF
159600100318     C*
159700100318     C                   callp     UBISNUM_Check(PiStr
159800100318     C                                          :PiDecChr
159900100318     C                                          :PiVal
160000100318     C                                          :PiNum
160100100318     C                                          :PiInt)
160200100318     C*
160300100318     C                   ENDSR
160400100318     C***
160500010601
160600010601
160700010601
160800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160900010601     C     repfil        BEGSR
161000010601     C*
161100010601     C                   if        invfil = *zeros and
161200010601     C                             depfil > *zeros and
161300010629     C                             (vinflg = *blanks or
161400010629     C                              vinflg = *zeros)
161500010601     C
161600010601     C                   eval      invfil = depfil
161700010601     C                   endif
161800010601     C*
161900010601     C                   if        depfil <> invfil and
162000010601     C                             invfil > *zeros
162100010601     C                   eval      flgMulti = '1'
162200010601     C                   if        vinflg = *blanks
162300010601     C                   add       1             cntNonEl
162400010601     C                   endif
162500010601     C                   endif
162600010601     C*
162700010601     C                   if        vinflg = '2'
162800010601     C                   eval      flgStato = '2'
162900010601     C                   endif
163000010601     C*
163100010601     C                   ENDSR
163200010601     C***
163300010601
163400010601
163500010601
163600010330
163700040119      /TITLE Invio dei dati al punto operativo.
163800040119     C     invio         BEGSR
163900040119     C*
164000040119     C* 1° invio FIVAT
164100040119     C                   reset                   dscmz
164200040119     C                   move      vlrpoi        cmzdst
164300040119     C                   eval      cmzfld = 'FIVATWWR'
164400040119     C                   eval      cmzmbd = vlrhdl
164500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164600040119     C***                if        prmfir = *blanks
164700040119     C                   eval      cmzfla = 'FIVAT00F'
164800040119     C                   eval      cmzmba = 'FIVAT00F'
164900040119     C***                else
165000040119     C***                eval      cmzfla = prmfir
165100040119     C***                eval      cmzmba = prmfir
165200040119     C***                endif
165300040119     C                   eval      cmznrr = *zeros
165400040119     C                   move      §ctrokvt      cmznrr
165500040119     C                   eval      cmzlba = vlrfl1
165600040119     C                   call(e)   'TIS711C'
165700040119     C                   parm                    dscmz
165800040119     C                   parm      *blanks       esito
165900040119     C                   if        %error
166000040119     C                             or cmzerr = '1'
166100040119     C                             or esito  = '1'
166200040119     C                   eval      wrkesito = '3'
166300040119     C                   else
166400040119     C*
166500040119     C* 2° invio FIVAB
166600040119     C                   reset                   dscmz
166700040119     C                   move      vlrpoi        cmzdst
166800040119     C                   eval      cmzfld = vlrfou
166900040119     C                   eval      cmzmbd = vlrhdl
167000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167100040119     C***                if        prmfir = *blanks
167200040119     C                   eval      cmzfla = 'FIVAB00F'
167300040119     C                   eval      cmzmba = 'FIVAB00F'
167400040119     C***                else
167500040119     C***                eval      cmzfla = prmfir
167600040119     C***                eval      cmzmba = prmfir
167700040119     C***                endif
167800040119     C                   eval      cmznrr = *zeros
167900040119     C                   move      §ctrokvb      cmznrr
168000040119     C                   eval      cmzlba = vlrfl1
168100040119     C                   call(e)   'TIS711C'
168200040119     C                   parm                    dscmz
168300040119     C                   parm      *blanks       esito
168400040119     C                   if        %error
168500040119     C                             or cmzerr = '1'
168600040119     C                             or esito  = '1'
168700040119     C                   eval      wrkesito = '3'
168800040119     C                   endif
168900040119     C                   endif
169000040119     C*
169100040119     C                   ENDSR
169200040119     C***
169300010601
169400010601
169500010601
169600010601
169700010601      /TITLE Invio dei dati al punto operativo.
169800010601     C     opeini        BEGSR
169900010601     C*
170000010601     C* Inizializzo flag e contatori operativi
170100010601     C                   movel     '0'           flgGiro           1
170200010601     C                   movel     '0'           flgMulti          1
170300010601     C                   movel     '1'           flgStato          1
170400010615     C                   movel     '0'           flgOk             1
170500010601     C                   z-add     *zeros        cntNonEl         10 0
170600010601     C                   z-add     *zeros        depfil            3 0
170700010601     C                   z-add     *zeros        invfil            3 0
170800010601     C*
170900010601     C                   ENDSR
171000010601     C***
171100070326
171200070326
171300070326
171400070326
171500070326     C     *pssr         BEGSR
171600070329     C*
171700070329     C                   if        %open(tivin00r)
171800070329     C                   close     tivin00r
171900070329     C                   endif
172000070329     C                   if        %open(fivabwwr)
172100070329     C                   close     fivabwwr
172200070329     C                   endif
172300070329     C                   if        %open(fivatwwr)
172400070329     C                   close     fivatwwr
172500070329     C                   endif
172600070326     C*
172700070326     C* Effettuo la chiamata al CLLE preposto
172800070326     C                   call(e)   'TITVVTC'
172900070326     C                   parm                    parccm
173000070326     C                   parm                    parmbr
173100070326     C                   parm      '2'           paropz
173200070326     C*
173300070326     C                   eval      wrkesito = '2'
173400070404     C*
173500070404     C                   seton                                        LR
173600070326     C*
173700070326     C                   ENDSR     '*CANCL'
173800070326     C***
173900100318
174000100318
174100100318
174200100318     C     normCSV       BEGSR
174300100318     C*
174400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174500100318     C* separatori campi
174600100318     C                   if        CharTXT <> *blanks
174700100318     C                   z-add     *zeros        wLenVINDTA        4 0
174800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174900100318     C                   setoff                                       80
175000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175300100318     C                   if        not *in80
175400100318     C                   seton                                        80
175500100318     C                   else
175600100318     C                   setoff                                       80
175700100318     C                   endif
175800100318     C                   endif
175900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176000100318     C                             and *in80 = *on
176100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176200100318     C                   endif
176300100318     C                   enddo
176400100318     C                   endif
176500100318     C*
176600100318     C                   ENDSR
176700100318     C***
176800070326
176900100326
177000100326
177100100326     C*----------------------------------------------------*
177200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177300100326     C*----------------------------------------------------*
177400100326     C     REPNSP        BEGSR
177500100326     C*
177600100326     C* NSP => Stacco un numeratore da AZNUM
177700100326     C                   clear                   TRUL33DS
177800100326     C                   eval      I33OPE = *zeros
177900100326     C                   eval      I33CNU = 302
178000100326     C                   eval      I33NUM = 1
178100100326     C                   movel     TRUL33DS      KPJBU
178200100326     C                   call      'TRUL33R'
178300100326     C                   parm                    KPJBA
178400100326     C                   movel     KPJBU         TRUL33DS
178500100326     C                   if        O33ERR = *zeros
178600100326     C                   z-add     O33NRF        VABNSP
178700100326     C                   z-add     O33NRF        VATNSP
178800100326     C                   else
178900100326     C                   SETON                                        31
179000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179100100326     C                             + ' ' + 'VABNSP VATNSP'
179200100326     C                   endif
179300100326     C*
179400100326     C                   ENDSR
179500010330
179600100326
179700010330
179800000613     C     *inzsr        BEGSR
179900990910     C*
180000990910     C     *entry        plist
180100990920     C                   parm                    tivlrds
180200990921     C                   parm      wrkesito      esito
180300000724     C                   parm                    prmlit
180400000710     C                   parm                    prmfir
180500010330     C*
180600010330     C* CALCOLA LA DATA CORRENTE
180700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180800100318     C                   eval      datcor = %dec(%date() : *ISO)
180900000613     C*
181000000613     C                   ENDSR
181100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181200120911WANNO;FILXX2;FILXX3;VABRMN;FILXX5;FILXX6;VABRSD;VABIND;VABLOD;VABCAD;
181300130702VABPRD;W_VABTIC;FILX13;VABCAS;VABNT2;VABPKB;FILX17;VABNCL;VABNOT;W_VABCBO
181400110315
181500110315
181600100318
