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
036600161114     C                   eval      OK_VABCTM = '7Q'
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
041400120911     C                   eval      VATNOT = wVATNOT_E
041500120913     C                   eval      VATTRC = 'E'
041600120913     C*
041700120913     C* Sviluppati i "CHI SONO" in relazione al numero colli
041800120913     C*
041900160322     C                   Z-ADD     *zeros        wNumCollo         3 0
042000160322     C                   DOW       wNumCollo < VABNCL AND
042100160322     C                             wNumCollo < 99
042200160322     C                   add       1             wNumCollo
042300120913      *** da analisi del POC il progressivo colli è max 2 char e non contiene gli zeri non
042400120913      *** significativi ma neanche i blank a sx
042500120913     C                   EVAL      VATNOT = %trimr(wVATNOT_E) +
042600120913     C                                %trim(%char(wNumCollo))
042700120913     C                   write     FIVAT000
042800120913     C                   add       1             §CTROKVT
042900120913     C                   ENDDO
043000120913     C*
043100110201     C                   endif
043200040119     C*
043300040119     C                   ENDSR
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900040119     C*----------------------------------------------------*
044000040119     C     PREVAT        BEGSR
044100040119     C*
044200040119     C* Compongo il nome del membro da dare al FIVATWWR
044300040119     C                   eval      parmbr = vlrhdl
044400040119     C                   movel     'M'           parmbr
044500040510     C                   eval      parccm = vlrksc
044600040119     C                   eval      paropz = '1'
044700040119     C* Effettuo la chiamata al CLLE preposto
044800040119     C                   call(e)   'TITVVTC'
044900040119     C                   parm                    parccm
045000040119     C                   parm                    parmbr
045100040119     C                   parm                    paropz
045200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300040119     C                   if        %error
045400040119     C                   movel     '1'           chkcall
045500040119     C                   else
045600040119     C                   movel     '0'           chkcall
045700040119     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000010601
046100010601
046200010601
046300010330     C*----------------------------------------------------*
046400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046500010330     C*----------------------------------------------------*
046600040119     C     INZVAR        BEGSR
046700080527     C*
046800080527     C                   setoff                                       65
046900010330     C*
047000030822     C* Inizializzo variabili di wrk
047100020204     C                   Z-ADD     *zeros        Num5_0            5 0
047200120911     C                   Z-ADD     *zeros        wAnno
047300101018     C                   MOVEL     *blanks       wFlgCAS
047400060331     C                   MOVEL     *blanks       wVATNOT_A
047500060331     C                   MOVEL     *blanks       wVATNOT_B
047600060331     C                   MOVEL     *blanks       wVATNOT_E
047700100831     C                   MOVEL     *blanks       wVABRSD
047800100831     C                   MOVEL     *blanks       wVABIND
047900100831     C                   MOVEL     *blanks       wVABCAD
048000100831     C                   MOVEL     *blanks       wVABLOD
048100100831     C                   MOVEL     *blanks       wVABPRD
048200030822     C*
048300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048400030822     C                   CLEAR                   SkSplitCSV
048500030822     C                   EVAL      i = 1
048600030822     C                   EVAL      posDa = *zeros
048700030822     C                   EVAL      posA  = *zeros
048800080527     C*
048900080527     C                   CLEAR                   FIVABDS
049000080527     C                   CLEAR                   FIVAT000
049100030822     C*
049200030822     C* Reimposto i valori di default
049300030822     C                   EXSR      DEFCAM
049400030822     C*
049500010330     C                   ENDSR
049600010330     C*----------------------------------------------------*
049700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049800010330     C*----------------------------------------------------*
049900010330     C     DEFCAM        BEGSR
050000030822     C*
050100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050200030715     C* e delimitatore testo.
050300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050600100318     C*
050700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050800100318     C                   EVAL      CharSOS = CharNUM
050900030822     C*
051000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051100100720     C                   SETOFF                                       505152
051200100720     C                   SETOFF                                       53
051300100413     C                   SELECT
051400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051500100720     C                   SETON                                        50
051600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051700100413     C                   SETON                                        51
051800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051900100720     C                   SETON                                        52
052000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052100100720     C                   SETON                                        53
052200100413     C                   ENDSL
052300030822     C*
052400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052500030822     C                   EVAL      posDaDft = 1
052600030822     C                   EVAL      posADft  = 0
052700030822     C                   EVAL      wGiroDft = 0
052800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052900030822     C                             posDaDft > 0
053000030822     C*
053100030822     C* Gestisco il 1° giro
053200030822     C                   IF        wGiroDft = 0
053300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053500030822     C* Incremento il contatore dei "giri"
053600030822     C                   EVAL      wGiroDft = 1
053700030822     C                   ELSE
053800030822     C                   EVAL      posDaDft = posADft
053900030822     C                   ENDIF
054000030822     C* Eseguo lo scan x trovare la fine del campo corrente
054100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054200030822     C*
054300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054500030822     C                   IF        posDaDft > 0 AND
054600030822     C                             posADft  > 0
054700030822     C* NCL
054800030822     C                   IF        %subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):1:3)
055100030822     C                             = 'NCL'
055200030822     C                   EVAL      PiStr=%trim(%subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055500030822     C                   EXSR      CHKNUM
055600030822     C                   IF        PiInt=*on
055700030822     C                   Z-ADD     PiVal         VABNCL
055800030822     C                   ENDIF
055900030822     C                   ENDIF
056000030822     C* CCM
056100030822     C                   IF        %subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):1:3)
056400030822     C                             = 'CCM'
056500030822     C                   EVAL      PiStr=%trim(%subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056800030822     C                   EXSR      CHKNUM
056900030822     C                   IF        PiInt=*on
057000030822     C                   Z-ADD     PiVal         VABCCM
057100030822     C                   ENDIF
057200030822     C                   ENDIF
057300030822     C* LNP
057400030822     C                   IF        %subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):1:3)
057700030822     C                             = 'LNP'
057800030822     C                   EVAL      PiStr=%trim(%subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058100030822     C                   EXSR      CHKNUM
058200030822     C                   IF        PiInt=*on
058300030822     C                   Z-ADD     PiVal         VABLNP
058400030822     C                   ENDIF
058500030822     C                   ENDIF
058600030822     C* NRS
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'NRS'
059100030822     C                   EVAL      PiStr=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   EXSR      CHKNUM
059500030822     C                   IF        PiInt=*on
059600030822     C                   Z-ADD     PiVal         VABNRS
059700030822     C                   ENDIF
059800030822     C                   ENDIF
059900030822     C* CTR
060000030822     C                   IF        %subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):1:3)
060300030822     C                             = 'CTR'
060400030822     C                   EVAL      PiStr=%trim(%subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700030822     C                   EXSR      CHKNUM
060800030822     C                   IF        PiInt=*on
060900030822     C                   Z-ADD     PiVal         VABCTR
061000030822     C                   ENDIF
061100030822     C                   ENDIF
061200030822     C* PKB
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'PKB'
061700030822     C                   EVAL      PiStr=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   EXSR      CHKNUM
062100030822     C                   IF        PiNum=*on
062200030822     C                   Z-ADD     PiVal         VABPKB
062300030822     C                   ENDIF
062400030822     C                   ENDIF
062500030822     C* VLB
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'VLB'
063000030822     C                   EVAL      PiStr=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   EXSR      CHKNUM
063400030822     C                   IF        PiNum=*on
063500030822     C                   Z-ADD     PiVal         VABVLB
063600030822     C                   ENDIF
063700030822     C                   ENDIF
063800030822     C* QFT
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'QFT'
064300030822     C                   EVAL      PiStr=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   EXSR      CHKNUM
064700030822     C                   IF        PiNum=*on
064800030822     C                   Z-ADD     PiVal         VABQFT
064900030822     C                   ENDIF
065000030822     C                   ENDIF
065100030822     C* CBO
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'CBO'
065600030822     C                   EVAL      VABCBO=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* TSP
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'TSP'
066500030822     C                   EVAL      VABTSP=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* VAS
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'VAS'
067400030822     C                   EVAL      VABVAS=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* VCA
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'VCA'
068300030822     C                   EVAL      VABVCA=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700070925     C* TIC
068800070925     C                   IF        %subst(
068900070925     C                             %subst(vlrppt:posDaDft+1:
069000070925     C                             posADft-posDaDft-1):1:3)
069100070925     C                             = 'TIC'
069200070925     C                   EVAL      VABTIC=%trim(%subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500070925     C                   ENDIF
069600030822     C* GCA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GCA'
070100030822     C                   EVAL      VABGCA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* CTM
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'CTM'
071000030822     C                   EVAL      VABCTM=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* FFD
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'FFD'
071900030822     C                   EVAL      VABFFD=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* VAD
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'VAD'
072800030822     C                   EVAL      VABVAD=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* GMA
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'GMA'
073700030822     C                   EVAL      VABGMA=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100030822     C* GGA
074200030822     C                   IF        %subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):1:3)
074500030822     C                             = 'GGA'
074600030822     C                   EVAL      VABGGA=%trim(%subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900030822     C                   ENDIF
075000030822     C* GVA
075100030822     C                   IF        %subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):1:3)
075400030822     C                             = 'GVA'
075500030822     C                   EVAL      VABGVA=%trim(%subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800030822     C                   ENDIF
075900030822     C* TC1
076000030822     C                   IF        %subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):1:3)
076300030822     C                             = 'TC1'
076400030822     C                   EVAL      VABTC1=%trim(%subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076700030822     C                   ENDIF
076800030822     C* TC2
076900030822     C                   IF        %subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):1:3)
077200030822     C                             = 'TC2'
077300030822     C                   EVAL      VABTC2=%trim(%subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077600030822     C                   ENDIF
077700040714     C* VATTRC
077800040714     C                   IF        %subst(
077900040714     C                             %subst(vlrppt:posDaDft+1:
078000040714     C                             posADft-posDaDft-1):1:3)
078100040714     C                             = 'TRC'
078200040714     C                   EVAL      VATTRC=%trim(%subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078500040714     C                   ENDIF
078600030822     C* ...
078700030822     C                   ENDIF
078800030822     C                   ENDDO
078900020204     C*
079000010330     C                   ENDSR
079100010607     C*----------------------------------------------------*
079200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079300010607     C*----------------------------------------------------*
079400010607     C     IMPVAB        BEGSR
079500010607     C*
079600010607     C                   SETOFF                                       3132
079700110315 xxx C***                SETON                                        35
079800030515     C*
079900010607     C                   MOVEL     datcor        VABAAS
080000010607     C                   MOVE      datcor        VABMGS
080100010607     C*
080200030715     C**********
080300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080400030715     C**********
080500030715     C                   IF        wGiro = *zeros
080600040113     C*
080700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081000080429     C                             %trim(intcol(5))<>*blanks
081100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081300080429     C                                      %trim(intcol(5))
081400070823     C     ';':CharCSV   XLATE     vindta        vindta
081500040510     C                   ENDIF
081600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081700031201     C     minu:maiu     XLATE     vindta        vindta
081800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081900030715     C                   DOW       posDa <= %len(%trim(vindta))
082000030715     C*
082100030715     C* Gestisco il 1° campo
082200030715     C                   IF        i = 1
082300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082400030715     C                   EVAL      posDa = 1
082500030715     C* Eseguo lo scan x trovare la fine del primo campo
082600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C* X i campi successivi al 1°
083100030715     C                   ELSE
083200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083300030715     C                   EVAL      posDa = posA + 1
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitFLD)
085700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C                   EVAL      wGiro = 1
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800031204     C                   EVAL      vindta = %trim(vindta)
086900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000031204     C                   EVAL      vindta = %subst(vindta:2)
087100031204     C                   ENDDO
087200031204     C*
087300031204     C                   Z-ADD     *zeros        lunghInput        4 0
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800031204     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300030715     C                   DOW       posDa <= %len(%trim(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200100720     C*
093300100720     C* Stacco un progressivo x numero spedizione
093400110421 xxx C                   EXSR      repNSP
093500100326     C*
093600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitFLD)
093900030715     C*
094000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094200030715     C                             SkSplitCSV(i) <> *blanks
094300030721     C*** ATB
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094500030721     C                   EVAL      VABATB = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030715     C*** RSD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095200030715     C* ==
095300030715     C                   ENDIF
095400030721     C*** RD2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** IND
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096000030715     C                   EVAL      VABIND = SkSplitCSV(i)
096100030715     C                   ENDIF
096200030715     C*** LOD
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** PRD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030721     C*** NZD
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** RMA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** NOT
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NT2
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** VCA
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VAS
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** FFD
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030721     C*** GC1
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GC2
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TSP
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100900030721     C                   ENDIF
101000040510     C*** CBO
101100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101300040510     C                   ENDIF
101400030721     C*** NAS
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TIC
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GCA
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** XCO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** CTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TCR
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** CTS
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** FTM
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** VAD
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** GMA
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GGA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GVA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** TC1
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC2
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** SCL
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** RMO
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** NMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*
108300030721     C* Reperisco quindi i campi numerici...
108400040510     C*** NSP
108500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108600040510     C                   EVAL      PiStr=SkSplitCSV(i)
108700040510     C                   EXSR      CHKNUM
108800040510     C                   IF        PiInt=*on
108900040510     C                   Z-ADD     PiVal         VABNSP
109000040510     C                   ELSE
109100040510     C                   SETON                                        31
109200040510     C                   EVAL      VABNSP = *zeros
109300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABNSP'
109500040510     C                   ENDIF
109600040510     C                   ENDIF
109700071106     C*** RMN
109800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109900071106     C                   EVAL      PiStr=SkSplitCSV(i)
110000071106     C                   EXSR      CHKNUM
110100071106     C                   IF        PiInt=*on
110200071106     C                   Z-ADD     PiVal         VABRMN
110300071106     C                   ELSE
110400071106     C                   SETON                                        32
110500071106     C                   EVAL      VABRMN = *zeros
110600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110700071106     C                             + ' ' + 'VABRMN'
110800071106     C                   ENDIF
110900071106     C                   ENDIF
111000030721     C*** CAD
111100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111200030721     C                   EVAL      PiStr=SkSplitCSV(i)
111300030721     C                   EXSR      CHKNUM
111400030721     C                   IF        PiInt=*on
111500030721     C                   Z-ADD     PiVal         Num5_0
111600030721     C                   MOVEL(p)  Num5_0        VABCAD
111700030721     C                   ELSE
111800030721     C                   SETON                                        32
111900030721     C                   EVAL      VABCAD = *zeros
112000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112100030721     C                             + ' ' + 'VABCAD'
112200030721     C                   ENDIF
112300030721     C                   ENDIF
112400030721     C*** NCL
112500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112600030721     C                   EVAL      PiStr=SkSplitCSV(i)
112700030721     C                   EXSR      CHKNUM
112800030721     C                   IF        PiInt=*on
112900030721     C                   Z-ADD     PiVal         VABNCL
113000030721     C                   ELSE
113100030721     C                   SETON                                        32
113200030721     C                   EVAL      VABNCL = *zeros
113300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113400040122     C                             + ' ' + 'VABNCL'
113500030721     C                   ENDIF
113600030721     C                   ENDIF
113700030721     C*** PKB
113800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113900030721     C                   EVAL      PiStr=SkSplitCSV(i)
114000030721     C                   EXSR      CHKNUM
114100030721     C                   IF        PiNum=*on
114200030721     C                   Z-ADD     PiVal         VABPKB
114300030721     C                   ELSE
114400030721     C                   SETON                                        32
114500030721     C                   EVAL      VABPKB = *zeros
114600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114700030721     C                             + ' ' + 'VABPKB'
114800030721     C                   ENDIF
114900030721     C                   ENDIF
115000030721     C*** CAS
115100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115300110506     C                             %trim(SkSplitCSV(i)) <> *zeros
115400101018     C                   EVAL      wFlgCAS = '1'
115500101018     C                   ENDIF
115600030721     C                   EVAL      PiStr=SkSplitCSV(i)
115700030721     C                   EXSR      CHKNUM
115800030721     C                   IF        PiNum=*on
115900030721     C                   Z-ADD     PiVal         VABCAS
116000030721     C                   ELSE
116100030721     C                   SETON                                        32
116200030721     C                   EVAL      VABCAS = *zeros
116300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116400030721     C                             + ' ' + 'VABCAS'
116500030721     C                   ENDIF
116600030721     C                   ENDIF
116700030721     C*** CCM
116800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116900030721     C                   EVAL      PiStr=SkSplitCSV(i)
117000030721     C                   EXSR      CHKNUM
117100030721     C                   IF        PiInt=*on      AND
117200030721     C                             PiVal<=9999999 AND
117300030721     C                             PiVal>*zeros
117400030721     C                   Z-ADD     PiVal         VABCCM
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      VABCCM = *zeros
117800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117900040510     C                             + ' ' + 'VABCCM'
118000030721     C                   ENDIF
118100030721     C                   ENDIF
118200030721     C*** LNP
118300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118400030721     C                   EVAL      PiStr=SkSplitCSV(i)
118500030721     C                   EXSR      CHKNUM
118600030721     C                   IF        PiInt=*on    AND
118700030721     C                             PiVal<=999   AND
118800030721     C                             PiVal>*zeros
118900030721     C                   Z-ADD     PiVal         VABLNP
119000030721     C                   ELSE
119100040715     C                   SETON                                        32
119200030721     C                   EVAL      VABLNP = *zeros
119300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119400040510     C                             + ' ' + 'VABLNP'
119500030721     C                   ENDIF
119600030721     C                   ENDIF
119700030721     C*** NRS
119800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119900030721     C                   EVAL      PiStr=SkSplitCSV(i)
120000030721     C                   EXSR      CHKNUM
120100030721     C                   IF        PiInt=*on AND
120200030721     C                             PiVal<=99
120300030721     C                   Z-ADD     PiVal         VABNRS
120400030721     C                   ELSE
120500030721     C                   SETON                                        32
120600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120700030721     C                             + ' ' + 'VABNRS'
120800030721     C                   ENDIF
120900030721     C                   ENDIF
121000030721     C*** LNA
121100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121200030721     C                   EVAL      PiStr=SkSplitCSV(i)
121300030721     C                   EXSR      CHKNUM
121400030721     C                   IF        PiInt=*on    AND
121500030721     C                             PiVal<=999   AND
121600030721     C                             PiVal>*zeros
121700030721     C                   Z-ADD     PiVal         VABLNA
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      VABLNA = *zeros
122100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122200030721     C                             + ' ' + 'VABLNA'
122300030721     C                   ENDIF
122400030721     C                   ENDIF
122500030721     C*** CTR
122600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122700030721     C                   EVAL      PiStr=SkSplitCSV(i)
122800030721     C                   EXSR      CHKNUM
122900030721     C                   IF        PiInt=*on  AND
123000030721     C                             PiVal<=999
123100030721     C                   Z-ADD     PiVal         VABCTR
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABCTR'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** IAS
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABIAS
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABIAS'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** VLB
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABVLB
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABVLB'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** QFT
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiNum=*on
126700030721     C                   Z-ADD     PiVal         VABQFT
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABQFT'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** NCD
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on      AND
127900030721     C                             PiVal<=9999999
128000030721     C                   Z-ADD     PiVal         VABNCD
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABNCD'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** NCA
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiInt=*on      AND
129200030721     C                             PiVal<=9999999
129300030721     C                   Z-ADD     PiVal         VABNCA
129400030721     C                   ELSE
129500030721     C                   SETON                                        32
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABNCA'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000030721     C*** ZNC
130100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130200030721     C                   EVAL      PiStr=SkSplitCSV(i)
130300030721     C                   EXSR      CHKNUM
130400030721     C                   IF        PiInt=*on AND
130500030721     C                             PiVal<=99
130600030721     C                   Z-ADD     PiVal         VABZNC
130700030721     C                   ELSE
130800030721     C                   SETON                                        32
130900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131000030721     C                             + ' ' + 'VABZNC'
131100030721     C                   ENDIF
131200030721     C                   ENDIF
131300030721     C*** DCR
131400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
131500030721     C                   EVAL      PiStr=SkSplitCSV(i)
131600030721     C                   EXSR      CHKNUM
131700030721     C                   IF        PiInt=*on       AND
131800030721     C                             PiVal<=99999999
131900030721     C                   Z-ADD     PiVal         VABDCR
132000030721     C                   ELSE
132100030721     C                   SETON                                        32
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABDCR'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600030721     C*** HCR
132700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132800030721     C                   EVAL      PiStr=SkSplitCSV(i)
132900030721     C                   EXSR      CHKNUM
133000030721     C                   IF        PiInt=*on   AND
133100030721     C                             PiVal<=9999
133200030721     C                   Z-ADD     PiVal         VABHCR
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABHCR'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** VMD
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiNum=*on
134400030721     C                   Z-ADD     PiVal         VABVMD
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABVMD'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** ANT
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiInt=*on        AND
135600030721     C                             PiVal<=999999999
135700030721     C                   Z-ADD     PiVal         VABANT
135800030721     C                   ELSE
135900030721     C                   SETON                                        32
136000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136100030721     C                             + ' ' + 'VABANT'
136200030721     C                   ENDIF
136300030721     C                   ENDIF
136400030721     C*** CMO
136500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
136600030721     C                   EVAL      PiStr=SkSplitCSV(i)
136700030721     C                   EXSR      CHKNUM
136800030721     C                   IF        PiInt=*on
136900030721     C                   Z-ADD     PiVal         Num5_0
137000030721     C                   MOVEL(p)  Num5_0        VABCMO
137100030721     C                   ELSE
137200030721     C                   SETON                                        32
137300030721     C                   EVAL      VABCMO = *zeros
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABCMO'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800040510     C*
137900040510     C* Reperisco i campi del FIVAT
138000040510     C*** VATNOT
138100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138300040510     C                   ENDIF
138400040510     C*** VATTRC
138500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
138600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138700040510     C                   ENDIF
138800060331     C*** VATNOT_A
138900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139100060331     C                   ENDIF
139200060331     C*** VATNOT_B
139300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
139500060331     C                   ENDIF
139600060331     C*** VATNOT_E
139700070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139900060331     C                   ENDIF
140000070823     C*
140100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140200110421     C*
140300110506     C*** TIC
140400110506     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
140500110506     C                   SELECT
140600110506     C                   WHEN      %trim(SkSplitCSV(i)) = '22'  or
140700110506     C                             %trim(SkSplitCSV(i)) = '28'  or
140800110506     C                             %trim(SkSplitCSV(i)) = '32'  or
140900110506     C                             %trim(SkSplitCSV(i)) = '561' or
141000110506     C                             %trim(SkSplitCSV(i)) = '684'
141100110506     C                   EVAL      VABTIC = 'BM'
141200120726     C                   WHEN      %trim(SkSplitCSV(i)) = '689'
141300110506     C                   EVAL      VABTIC = 'CM'
141400120726     C                   WHEN      %trim(SkSplitCSV(i)) = '70'  or
141500120726     C                             %trim(SkSplitCSV(i)) = '150'
141600110506     C                   EVAL      VABTIC = *blanks
141700110506     C                   WHEN      %trim(SkSplitCSV(i)) = '350' or
141800110506     C                             %trim(SkSplitCSV(i)) = '681'
141900110506     C                   EVAL      VABTIC = 'TM'
142000110506     C                   ENDSL
142100110415     C                   ENDIF
142200120911     C*** Anno spedizione dal cliente
142300120911     C                   IF        %trim(SkSplitFLD(i)) = 'WANNO'
142400120911     C                   EVAL      PiStr=SkSplitCSV(i)
142500120911     C                   EXSR      CHKNUM
142600120911     C                   IF        PiInt=*on
142700120911     C                   Z-ADD     PiVal         wAnno
142800120911     C                   ELSE
142900120911     C                   SETON                                        32
143000120911     C                   EVAL      vinmsg = %trimr(vinmsg)
143100120911     C                             + ' ' + 'WANNO'
143200120911     C                   ENDIF
143300120911     C                   ENDIF
143400130702     C*** CBO
143500130702     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
143600130702     C                   IF        %trim(SkSplitCSV(i)) = 'F'
143700130702     C                   EVAL      VABCBO = '1'
143800130702     C                   ENDIF
143900130702     C                   IF        %trim(SkSplitCSV(i)) = 'A'
144000130702     C                   EVAL      VABCBO = '2'
144100130702     C                   ENDIF
144200130702     C                   ENDIF
144300070925     C*
144400071029     C***  <===  -----------------------------------------
144500040510     C*
144600030715     C                   ENDIF
144700030715     C                   EVAL      i = i + 1
144800030715     C                   ENDDO
144900070925     C*
145000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145100100831     C*
145200100831     C                   IF        wVABRSD <> *blanks
145300100831     C                   EVAL      VABRSD = wVABRSD
145400100831     C                   ENDIF
145500100831     C*
145600100831     C                   IF        wVABIND <> *blanks
145700100831     C                   EVAL      VABIND = wVABIND
145800100831     C                   ENDIF
145900100831     C*
146000100831     C                   IF        wVABCAD <> *blanks
146100100831     C                   EVAL      VABCAD = wVABCAD
146200100831     C                   ENDIF
146300100831     C*
146400100831     C                   IF        wVABLOD <> *blanks
146500100831     C                   EVAL      VABLOD = wVABLOD
146600100831     C                   ENDIF
146700100831     C*
146800100831     C                   IF        wVABPRD <> *blanks
146900100831     C                   EVAL      VABPRD = wVABPRD
147000100831     C                   ENDIF
147100120911     C*
147200120913     C* creo la prima parte del barcode come:
147300120913     C* - ETM fisso
147400120913     C* - 4 char = anno cliente (1a colonna CSV)
147500120913     C* - 3 char = n. DDT (VABRMN)
147600120913     C* poi svilupperò rispetto ai colli
147700120913     C* - max 2 char = progressivo collo
147800120911     C                   EVAL      wVATNOT_E = 'ETM' + %char(wAnno) +
147900120913     C                             %subst(%editc(VABRMN:'X') : 11 : 5)
148000070925     C*
148100071029     C***  <===  -----------------------------------------
148200070925     C*
148300070925     C* Considerazioni finali su CBO/CAS
148400100318     C                   IF        wFlgCAS = '1'    OR
148500100318     C                             VABCAS  > *zeros
148600070925     C                   IF        VABCBO = '1'
148700070925     C                   EVAL      VABCBO = '4'
148800070925     C                   ENDIF
148900070925     C                   IF        VABCBO = '2'
149000070925     C                   EVAL      VABCBO = '6'
149100070925     C                   ENDIF
149200070925     C                   ENDIF
149300070925     C***  <===  ----------------------------
149400040510     C*
149500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149600040510     C                   IF        VABNSP = *zeros
149700040510     C                   SETON                                        31
149800040510     C                   ENDIF
149900040715     C*
150000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
150100040715     C                   IF        VABLNP = *zeros
150200040715     C                   SETON                                        31
150300040715     C                   ENDIF
150400040510     C*
150500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150600040510     C                   IF        VABRMN = *zeros
150700040510     C                   Z-ADD     VABNSP        VABRMN
150800040510     C                   ENDIF
150900040510     C*
151000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
151100040510     C                   IF        VABRMA = *blanks
151200040714     C                   MOVEL     VABRMN        VABRMA
151300040510     C                   ENDIF
151400040128     C*
151500040510     C* Se provincia nn valorizzata la reperisco
151600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151700040510     C                   IF        VABPRD = *blanks
151800040128     C                   CLEAR                   TISI95DS
151900040128     C                   EVAL      I95TCN = '3'
152000040128     C                   Z-ADD     datcor        I95DAT
152100040128     C                   EVAL      I95NAR = VABNZD
152200040128     C                   EVAL      I95CAP = VABCAD
152300040128     C                   EVAL      I95LOC = VABLOD
152400040128     C                   CALL      'TISI95R'
152500040128     C                   PARM                    TISI95DS
152600040128     C                   EVAL      VABPRD = O95PRV
152700040510     C                   ENDIF
152800020204     C*
152900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
153000020204     C                   EXSR      CHKIMPDIV
153100030715     C                   ENDIF
153200020204     C*
153300010607     C                   ENDSR
153400010607     C*----------------------------------------------------*
153500010601
153600020204
153700020204     C*----------------------------------------------------*
153800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153900020204     C*----------------------------------------------------*
154000020204     C     CHKIMPDIV     BEGSR
154100020204     C*
154200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
154300020204     C                   Z-ADD     *zeros        wrkDec            9 9
154400020204     C*
154500020204     C* Come prima cosa effettuo considerazioni sulla divisa
154600020204     C                   IF        vabIAS > *zeros
154700020204     C                   IF        vabVAS <> 'EUR'
154800020204     C                   EVAL      vabVAS =  'ITL'
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C                   IF        vabCAS > *zeros
155300020204     C                   IF        vabVCA <> 'EUR'
155400020204     C                   EVAL      vabVCA =  'ITL'
155500020204     C                   ENDIF
155600020204     C                   ENDIF
155700020204     C*
155800020204     C                   IF        vabVMD > *zeros
155900020204     C                   IF        vabVAD <> 'EUR'
156000020204     C                   EVAL      vabVAD =  'ITL'
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
156500020204     C                   Z-ADD     vabIAS        wrkDec
156600020204     C                   IF        wrkDec > *zeros
156700020204     C                   IF        vabVAS = 'ITL'
156800020204     C                   EVAL      vabIAS = *zeros
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
157300020204     C                   Z-ADD     vabCAS        wrkDec
157400020204     C                   IF        wrkDec > *zeros
157500020204     C                   IF        vabVCA = 'ITL'
157600020204     C                   EVAL      vabCAS = *zeros
157700020204     C                   ENDIF
157800020204     C                   ENDIF
157900020204     C*
158000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
158100020204     C                   Z-ADD     vabVMD        wrkDec
158200020204     C                   IF        wrkDec > *zeros
158300020204     C                   IF        vabVAD = 'ITL'
158400020204     C                   EVAL      vabVMD = *zeros
158500020204     C                   ENDIF
158600020204     C                   ENDIF
158700020204     C*
158800020204     C                   ENDSR
158900020204     C***
159000010330
159100100318
159200100318
159300100318     C*----------------------------------------------------*
159400100318     C*  CONTROLLO NUMERICITA' CAMPI
159500100318     C*----------------------------------------------------*
159600100318     C     CHKNUM        BEGSR
159700100318     C*
159800100318     C                   IF        PiDecChr = *blanks
159900100318     C                   EVAL      PiDecChr = CharNUM
160000100318     C                   ENDIF
160100100318     C*
160200100318     C                   callp     UBISNUM_Check(PiStr
160300100318     C                                          :PiDecChr
160400100318     C                                          :PiVal
160500100318     C                                          :PiNum
160600100318     C                                          :PiInt)
160700100318     C*
160800100318     C                   ENDSR
160900100318     C***
161000010601
161100010601
161200010601
161300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
161400010601     C     repfil        BEGSR
161500010601     C*
161600010601     C                   if        invfil = *zeros and
161700010601     C                             depfil > *zeros and
161800010629     C                             (vinflg = *blanks or
161900010629     C                              vinflg = *zeros)
162000010601     C
162100010601     C                   eval      invfil = depfil
162200010601     C                   endif
162300010601     C*
162400010601     C                   if        depfil <> invfil and
162500010601     C                             invfil > *zeros
162600010601     C                   eval      flgMulti = '1'
162700010601     C                   if        vinflg = *blanks
162800010601     C                   add       1             cntNonEl
162900010601     C                   endif
163000010601     C                   endif
163100010601     C*
163200010601     C                   if        vinflg = '2'
163300010601     C                   eval      flgStato = '2'
163400010601     C                   endif
163500010601     C*
163600010601     C                   ENDSR
163700010601     C***
163800010601
163900010601
164000010601
164100010330
164200040119      /TITLE Invio dei dati al punto operativo.
164300040119     C     invio         BEGSR
164400040119     C*
164500040119     C* 1° invio FIVAT
164600040119     C                   reset                   dscmz
164700040119     C                   move      vlrpoi        cmzdst
164800040119     C                   eval      cmzfld = 'FIVATWWR'
164900040119     C                   eval      cmzmbd = vlrhdl
165000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165100040119     C***                if        prmfir = *blanks
165200040119     C                   eval      cmzfla = 'FIVAT00F'
165300040119     C                   eval      cmzmba = 'FIVAT00F'
165400040119     C***                else
165500040119     C***                eval      cmzfla = prmfir
165600040119     C***                eval      cmzmba = prmfir
165700040119     C***                endif
165800040119     C                   eval      cmznrr = *zeros
165900040119     C                   move      §ctrokvt      cmznrr
166000040119     C                   eval      cmzlba = vlrfl1
166100040119     C                   call(e)   'TIS711C'
166200040119     C                   parm                    dscmz
166300040119     C                   parm      *blanks       esito
166400040119     C                   if        %error
166500040119     C                             or cmzerr = '1'
166600040119     C                             or esito  = '1'
166700040119     C                   eval      wrkesito = '3'
166800040119     C                   else
166900040119     C*
167000040119     C* 2° invio FIVAB
167100040119     C                   reset                   dscmz
167200040119     C                   move      vlrpoi        cmzdst
167300040119     C                   eval      cmzfld = vlrfou
167400040119     C                   eval      cmzmbd = vlrhdl
167500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
167600040119     C***                if        prmfir = *blanks
167700040119     C                   eval      cmzfla = 'FIVAB00F'
167800040119     C                   eval      cmzmba = 'FIVAB00F'
167900040119     C***                else
168000040119     C***                eval      cmzfla = prmfir
168100040119     C***                eval      cmzmba = prmfir
168200040119     C***                endif
168300040119     C                   eval      cmznrr = *zeros
168400040119     C                   move      §ctrokvb      cmznrr
168500040119     C                   eval      cmzlba = vlrfl1
168600040119     C                   call(e)   'TIS711C'
168700040119     C                   parm                    dscmz
168800040119     C                   parm      *blanks       esito
168900040119     C                   if        %error
169000040119     C                             or cmzerr = '1'
169100040119     C                             or esito  = '1'
169200040119     C                   eval      wrkesito = '3'
169300040119     C                   endif
169400040119     C                   endif
169500040119     C*
169600040119     C                   ENDSR
169700040119     C***
169800010601
169900010601
170000010601
170100010601
170200010601      /TITLE Invio dei dati al punto operativo.
170300010601     C     opeini        BEGSR
170400010601     C*
170500010601     C* Inizializzo flag e contatori operativi
170600010601     C                   movel     '0'           flgGiro           1
170700010601     C                   movel     '0'           flgMulti          1
170800010601     C                   movel     '1'           flgStato          1
170900010615     C                   movel     '0'           flgOk             1
171000010601     C                   z-add     *zeros        cntNonEl         10 0
171100010601     C                   z-add     *zeros        depfil            3 0
171200010601     C                   z-add     *zeros        invfil            3 0
171300010601     C*
171400010601     C                   ENDSR
171500010601     C***
171600070326
171700070326
171800070326
171900070326
172000070326     C     *pssr         BEGSR
172100070329     C*
172200070329     C                   if        %open(tivin00r)
172300070329     C                   close     tivin00r
172400070329     C                   endif
172500070329     C                   if        %open(fivabwwr)
172600070329     C                   close     fivabwwr
172700070329     C                   endif
172800070329     C                   if        %open(fivatwwr)
172900070329     C                   close     fivatwwr
173000070329     C                   endif
173100070326     C*
173200070326     C* Effettuo la chiamata al CLLE preposto
173300070326     C                   call(e)   'TITVVTC'
173400070326     C                   parm                    parccm
173500070326     C                   parm                    parmbr
173600070326     C                   parm      '2'           paropz
173700070326     C*
173800070326     C                   eval      wrkesito = '2'
173900070404     C*
174000070404     C                   seton                                        LR
174100070326     C*
174200070326     C                   ENDSR     '*CANCL'
174300070326     C***
174400100318
174500100318
174600100318
174700100318     C     normCSV       BEGSR
174800100318     C*
174900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
175000100318     C* separatori campi
175100100318     C                   if        CharTXT <> *blanks
175200100318     C                   z-add     *zeros        wLenVINDTA        4 0
175300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
175400100318     C                   setoff                                       80
175500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
175600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
175700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
175800100318     C                   if        not *in80
175900100318     C                   seton                                        80
176000100318     C                   else
176100100318     C                   setoff                                       80
176200100318     C                   endif
176300100318     C                   endif
176400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
176500100318     C                             and *in80 = *on
176600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
176700100318     C                   endif
176800100318     C                   enddo
176900100318     C                   endif
177000100318     C*
177100100318     C                   ENDSR
177200100318     C***
177300070326
177400100326
177500100326
177600100326     C*----------------------------------------------------*
177700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
177800100326     C*----------------------------------------------------*
177900100326     C     REPNSP        BEGSR
178000100326     C*
178100100326     C* NSP => Stacco un numeratore da AZNUM
178200100326     C                   clear                   TRUL33DS
178300100326     C                   eval      I33OPE = *zeros
178400100326     C                   eval      I33CNU = 302
178500100326     C                   eval      I33NUM = 1
178600100326     C                   movel     TRUL33DS      KPJBU
178700100326     C                   call      'TRUL33R'
178800100326     C                   parm                    KPJBA
178900100326     C                   movel     KPJBU         TRUL33DS
179000100326     C                   if        O33ERR = *zeros
179100100326     C                   z-add     O33NRF        VABNSP
179200100326     C                   z-add     O33NRF        VATNSP
179300100326     C                   else
179400100326     C                   SETON                                        31
179500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
179600100326     C                             + ' ' + 'VABNSP VATNSP'
179700100326     C                   endif
179800100326     C*
179900100326     C                   ENDSR
180000010330
180100100326
180200010330
180300000613     C     *inzsr        BEGSR
180400990910     C*
180500990910     C     *entry        plist
180600990920     C                   parm                    tivlrds
180700990921     C                   parm      wrkesito      esito
180800000724     C                   parm                    prmlit
180900000710     C                   parm                    prmfir
181000010330     C*
181100010330     C* CALCOLA LA DATA CORRENTE
181200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
181300100318     C                   eval      datcor = %dec(%date() : *ISO)
181400000613     C*
181500000613     C                   ENDSR
181600000613     C***
181700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181800120911WANNO;FILXX2;FILXX3;VABRMN;FILXX5;FILXX6;VABRSD;VABIND;VABLOD;VABCAD;
181900130702VABPRD;W_VABTIC;FILX13;VABCAS;VABNT2;VABPKB;FILX17;VABNCL;VABNOT;W_VABCBO
182000110315
182100110315
182200100318
