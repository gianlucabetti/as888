000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
005900111117     D wVATNOT_I       s                   LIKE(VATNOT)
006000111117     D wVATNOT_J       s                   LIKE(VATNOT)
006100100831     D wVABRSD         s                   LIKE(VABRSD)
006200100831     D wVABIND         s                   LIKE(VABIND)
006300100831     D wVABCAD         s                   LIKE(VABCAD)
006400100831     D wVABLOD         s                   LIKE(VABLOD)
006500100831     D wVABPRD         s                   LIKE(VABPRD)
006600120104     D wVABNZD         s                   LIKE(VABNZD)
006700030822     D*------------------
006800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006900030822     D*------------------
007000030822     D posDaDft        S              3  0 INZ(*zeros)
007100030822     D posADft         S              3  0 INZ(*zeros)
007200030822     D j               s              3  0 INZ(1)
007300030822     D wGiroDft        s              1  0 INZ(*zeros)
007400080418     D*------------------
007500080418     D* Costanti
007600080418     D*------------------
007700080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007800080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007900100318
008000100318     D*------------------
008100100318     D* LINKING A DEFINIZIONI ESTERNE
008200100318     D*------------------
008300100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008400100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008500100318
008600010330
008700010330
008800010330
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100010601     C*
011200010601     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400040119     C*
012500040119     C* Effettuo la chiamata al CLLE preposto
012600040119     C                   call(e)   'TITVVTC'
012700040510     C                   parm                    parccm
012800040119     C                   parm                    parmbr
012900040119     C                   parm      '2'           paropz
013000050201     C*
013100050201     C* Effettuo lancio TISI95 solo x chiusura
013200050201     C                   CLEAR                   TISI95DS
013300050201     C                   EVAL      I95TLA = 'C'
013400050201     C                   CALL      'TISI95R'
013500050201     C                   PARM                    TISI95DS
013600000616     C*
013700010601     C                   ENDSR
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
016800100914     C                   exsr      inzvar
016900100914     C                   exsr      impvab
017000010330     C*
017100030822     C                   do        *HIVAL
017200010330     C*
017300030822     C                   read      tivin00r                               70
017400010618     C*
017500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017600010618     C                   if        vinflg = '1'
017700010618     C                   eval      flgOk = '1'
017800010618     C                   endif
017900040510     C*
018000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018100040510     C                   z-add     *zeros        wLenVINDTA        4 0
018200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018600040510     C                   z-add     1             wFlgVINDTA
018700040510     C                   leave
018800040510     C                   endif
018900040510     C                   enddo
019000010618     C*
019100040510     C                   if        vindta > *blanks AND
019200040510     C                             wFlgVINDTA = 1
019300010330     C                   add       1             rrnum
019400010330     C*
019500010601     C                   if        *in70 = *off and
019600010330     C                             (vinflg = *blanks
019700010330     C                              or vinflg = '0'
019800010330     C                              or vinflg = '2')
019900010330     C*
020000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020100010711     C                   if        vinflg = *blanks or vinflg = '0'
020200010711     C                   clear                   vinmsg
020300010711     C                   endif
020400010601     C*
020500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020600070925     C                   eval      vindta  = ' ' + %trim(vindta)
020700070925     C*
020800080527     C                   exsr      inzvar
020900100318     C                   exsr      normCSV
021000010605     C                   exsr      impvab
021100010601     C*
021200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021300010605     C                   eval      depfil = VABLNP
021400010601     C                   exsr      repfil
021500010601     C                   if        depfil = invfil
021600021025     C                   if        vlrpoi = 999
021700030822     C                   move(P)   invfil        VABFGS
021800021025     C                   else
021900030822     C                   move(P)   vlrpoi        VABFGS
022000021025     C                   endif
022100010601     C*
022200010601     C                   exsr      PREELA
022300010601     C*
022400010604     C* Ebbene...
022500010604     C*
022600040119     C  N31              add       1             §CTROKVB
022700040119     C   32              add       1             §CTRMO
022800040119     C   31              add       1             §CTRNO
022900080418     C*
023000030822     C                   if        wGiro = 2
023100080513     C* Scrivo sempre il VAT
023200080513     C  N31              exsr      wrivat
023300080527     C                   exsr      chkvab
023400080513     C                   endif
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200030822     C  N70              enddo
025300040119     C*
025400080527     C* Verifico dati rimasti "in cannna"...
025500080527     C                   seton                                        65
025600080527     C                   exsr      chkvab
025700080527     C*
025800040119     C                   endif                                                  (endif - chkcall)
025900010601     C*
026000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026100010601     C                   if        cntNonEl = *zeros or
026200010601     C                             flgMulti = '0'
026300010330     C* Se non ci sono record con errori ...
026400010601     C                   if        §ctrno = 0 and
026500010604     C                             §ctrmo = 0 and
026600010601     C                             flgStato <> '2'
026700010330     C* ... restituisco esito OK.
026800010330     C                   eval      wrkesito = '0'
026900010330     C                   else
027000040119     C                   if        §ctrokvb > 0
027100010330     C                   eval      wrkesito = '1'
027200010330     C                   else
027300010615     C                   if        flgOk = '0'
027400010615     C                   eval      wrkesito = '2'
027500010615     C                   else
027600010615     C                   eval      wrkesito = '6'
027700010615     C                   endif
027800010330     C                   endif
027900010330     C                   endif
028000010601     C                   else
028100010601     C                   eval      wrkesito = '9'
028200010601     C                   endif
028300010330     C*
028400010330     C                   if        %open(tivin00r)
028500010330     C                   close     tivin00r
028600010330     C                   endif
028700021025     C                   if        %open(fivabwwr)
028800021025     C                   close     fivabwwr
028900010330     C                   endif
029000040512     C                   if        %open(fivatwwr)
029100040512     C                   close     fivatwwr
029200040512     C                   endif
029300010601     C*
029400010601     C                   if        vlrpoi <> 999
029500010601     C                   eval      invfil = vlrpoi
029600010601     C                   endif
029700010330     C*
029800040119     C                   if        §ctrokvb > 0
029900010601     C                             and invfil > *zeros
030000010330     C                   exsr      invio
030100010330     C                   endif
030200010601     C*
030300010618     C                   if        flgGiro = '1'
030400010601     C                   exsr      endela
030500010618     C                   endif
030600010330     C*
030700010330     C                   ENDSR
030800010330     C***
030900080527
031000080527
031100080527
031200080527     C*----------------------------------------------------*
031300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031400080527     C*----------------------------------------------------*
031500100413     C     CHKVAB        BEGSR
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
037300040714     C*
037400040714     C* Solo se VATNOT valorizzato scarico il buffer
037500040714     C                   if        VATNOT <> *blanks
037600040119     C*
037700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037800040510     C                   if        VATTRC = *blanks
037900040510     C                   eval      VATTRC = 'E'
038000040510     C                   endif
038100040510     C                   write     FIVAT000
038200040119     C                   add       1             §CTROKVT
038300040714     C*
038400040714     C                   endif
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
040000111117
040100111117      * Indirizzo e-mail destinatario
040200111117     C                   if        wVATNOT_I <> *blanks
040300111117     C                   eval      VATNOT = wVATNOT_I
040400111117     C                   eval      VATTRC = 'I'
040500111117     C                   write     FIVAT000
040600111117     C                   add       1             §CTROKVT
040700111117     C                   endif
040800111117     C                   if        wVATNOT_J <> *blanks
040900111117     C                   eval      VATNOT = wVATNOT_J
041000111117     C                   eval      VATTRC = 'J'
041100111117     C                   write     FIVAT000
041200111117     C                   add       1             §CTROKVT
041300111117     C                   endif
041400120104      * Barcode
041500060331     C                   if        wVATNOT_E <> *blanks
041600060331     C                   eval      VATTRC = 'E'
041700100917     C                   eval      VATNOT = wVATNOT_E
041800100901     C                   write     FIVAT000
041900100901     C                   add       1             §CTROKVT
042000060331     C                   endif
042100040119     C*
042200040119     C                   ENDSR
042300040119
042400040119
042500040119
042600040119     C*----------------------------------------------------*
042700040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042800040119     C*----------------------------------------------------*
042900040119     C     PREVAT        BEGSR
043000040119     C*
043100040119     C* Compongo il nome del membro da dare al FIVATWWR
043200040119     C                   eval      parmbr = vlrhdl
043300040119     C                   movel     'M'           parmbr
043400040510     C                   eval      parccm = vlrksc
043500040119     C                   eval      paropz = '1'
043600040119     C* Effettuo la chiamata al CLLE preposto
043700040119     C                   call(e)   'TITVVTC'
043800040119     C                   parm                    parccm
043900040119     C                   parm                    parmbr
044000040119     C                   parm                    paropz
044100040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044200040119     C                   if        %error
044300040119     C                   movel     '1'           chkcall
044400040119     C                   else
044500040119     C                   movel     '0'           chkcall
044600040119     C                   endif
044700040119     C*
044800040119     C                   ENDSR
044900010601
045000010601
045100010601
045200010330     C*----------------------------------------------------*
045300030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045400010330     C*----------------------------------------------------*
045500040119     C     INZVAR        BEGSR
045600080527     C*
045700080527     C                   setoff                                       65
045800010330     C*
045900030822     C* Inizializzo variabili di wrk
046000020204     C                   Z-ADD     *zeros        Num5_0            5 0
046100120215     C                   clear                   wFlgCAS
046200060331     C                   MOVEL     *blanks       wVATNOT_A
046300060331     C                   MOVEL     *blanks       wVATNOT_B
046400060331     C                   MOVEL     *blanks       wVATNOT_E
046500111117     C                   MOVEL     *blanks       wVATNOT_I
046600111117     C                   MOVEL     *blanks       wVATNOT_J
046700100831     C                   MOVEL     *blanks       wVABRSD
046800100831     C                   MOVEL     *blanks       wVABIND
046900100831     C                   MOVEL     *blanks       wVABCAD
047000100831     C                   MOVEL     *blanks       wVABLOD
047100100831     C                   MOVEL     *blanks       wVABPRD
047200120104     C                   MOVEL     *blanks       wVABNZD
047300100910     C                   MOVEL     *blanks       wRSD             70
047400100910     C                   MOVEL     *blanks       wVABIND_1        35
047500100910     C                   MOVEL     *blanks       wVABIND_2        35
047600111117     C                   MOVEL     *blanks       wVATNOT_IJ       70
047700030822     C*
047800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047900030822     C                   CLEAR                   SkSplitCSV
048000030822     C                   EVAL      i = 1
048100030822     C                   EVAL      posDa = *zeros
048200030822     C                   EVAL      posA  = *zeros
048300080527     C*
048400080527     C                   CLEAR                   FIVABDS
048500080527     C                   CLEAR                   FIVAT000
048600030822     C*
048700030822     C* Reimposto i valori di default
048800030822     C                   EXSR      DEFCAM
048900030822     C*
049000010330     C                   ENDSR
049100010330     C*----------------------------------------------------*
049200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049300010330     C*----------------------------------------------------*
049400010330     C     DEFCAM        BEGSR
049500030822     C*
049600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049700030715     C* e delimitatore testo.
049800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
050100100318     C*
050200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050300100318     C                   EVAL      CharSOS = CharNUM
050400030822     C*
050500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050600100720     C                   SETOFF                                       505152
050700100720     C                   SETOFF                                       53
050800100413     C                   SELECT
050900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051000100720     C                   SETON                                        50
051100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051200100413     C                   SETON                                        51
051300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051400100720     C                   SETON                                        52
051500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051600100720     C                   SETON                                        53
051700100413     C                   ENDSL
051800030822     C*
051900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052000030822     C                   EVAL      posDaDft = 1
052100030822     C                   EVAL      posADft  = 0
052200030822     C                   EVAL      wGiroDft = 0
052300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052400030822     C                             posDaDft > 0
052500030822     C*
052600030822     C* Gestisco il 1° giro
052700030822     C                   IF        wGiroDft = 0
052800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053000030822     C* Incremento il contatore dei "giri"
053100030822     C                   EVAL      wGiroDft = 1
053200030822     C                   ELSE
053300030822     C                   EVAL      posDaDft = posADft
053400030822     C                   ENDIF
053500030822     C* Eseguo lo scan x trovare la fine del campo corrente
053600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053700030822     C*
053800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054000030822     C                   IF        posDaDft > 0 AND
054100030822     C                             posADft  > 0
054200030822     C* NCL
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'NCL'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiInt=*on
055200030822     C                   Z-ADD     PiVal         VABNCL
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* CCM
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'CCM'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiInt=*on
056500030822     C                   Z-ADD     PiVal         VABCCM
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* LNP
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'LNP'
057300030822     C                   EVAL      PiStr=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   EXSR      CHKNUM
057700030822     C                   IF        PiInt=*on
057800030822     C                   Z-ADD     PiVal         VABLNP
057900030822     C                   ENDIF
058000030822     C                   ENDIF
058100030822     C* NRS
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'NRS'
058600030822     C                   EVAL      PiStr=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   EXSR      CHKNUM
059000030822     C                   IF        PiInt=*on
059100030822     C                   Z-ADD     PiVal         VABNRS
059200030822     C                   ENDIF
059300030822     C                   ENDIF
059400030822     C* CTR
059500030822     C                   IF        %subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):1:3)
059800030822     C                             = 'CTR'
059900030822     C                   EVAL      PiStr=%trim(%subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060200030822     C                   EXSR      CHKNUM
060300030822     C                   IF        PiInt=*on
060400030822     C                   Z-ADD     PiVal         VABCTR
060500030822     C                   ENDIF
060600030822     C                   ENDIF
060700030822     C* PKB
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'PKB'
061200030822     C                   EVAL      PiStr=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   EXSR      CHKNUM
061600030822     C                   IF        PiNum=*on
061700030822     C                   Z-ADD     PiVal         VABPKB
061800030822     C                   ENDIF
061900030822     C                   ENDIF
062000030822     C* VLB
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'VLB'
062500030822     C                   EVAL      PiStr=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   EXSR      CHKNUM
062900030822     C                   IF        PiNum=*on
063000030822     C                   Z-ADD     PiVal         VABVLB
063100030822     C                   ENDIF
063200030822     C                   ENDIF
063300030822     C* QFT
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'QFT'
063800030822     C                   EVAL      PiStr=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   EXSR      CHKNUM
064200030822     C                   IF        PiNum=*on
064300030822     C                   Z-ADD     PiVal         VABQFT
064400030822     C                   ENDIF
064500030822     C                   ENDIF
064600030822     C* CBO
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'CBO'
065100030822     C                   EVAL      VABCBO=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* TSP
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'TSP'
066000030822     C                   EVAL      VABTSP=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* VAS
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'VAS'
066900030822     C                   EVAL      VABVAS=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VCA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VCA'
067800030822     C                   EVAL      VABVCA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200070925     C* TIC
068300070925     C                   IF        %subst(
068400070925     C                             %subst(vlrppt:posDaDft+1:
068500070925     C                             posADft-posDaDft-1):1:3)
068600070925     C                             = 'TIC'
068700070925     C                   EVAL      VABTIC=%trim(%subst(
068800070925     C                             %subst(vlrppt:posDaDft+1:
068900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000070925     C                   ENDIF
069100030822     C* GCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GCA'
069600030822     C                   EVAL      VABGCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* CTM
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'CTM'
070500030822     C                   EVAL      VABCTM=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* FFD
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'FFD'
071400030822     C                   EVAL      VABFFD=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VAD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VAD'
072300030822     C                   EVAL      VABVAD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* GMA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GMA'
073200030822     C                   EVAL      VABGMA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GGA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GGA'
074100030822     C                   EVAL      VABGGA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GVA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GVA'
075000030822     C                   EVAL      VABGVA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* TC1
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'TC1'
075900030822     C                   EVAL      VABTC1=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC2
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC2'
076800030822     C                   EVAL      VABTC2=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200040714     C* VATTRC
077300040714     C                   IF        %subst(
077400040714     C                             %subst(vlrppt:posDaDft+1:
077500040714     C                             posADft-posDaDft-1):1:3)
077600040714     C                             = 'TRC'
077700040714     C                   EVAL      VATTRC=%trim(%subst(
077800040714     C                             %subst(vlrppt:posDaDft+1:
077900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000040714     C                   ENDIF
078100030822     C* ...
078200030822     C                   ENDIF
078300030822     C                   ENDDO
078400020204     C*
078500010330     C                   ENDSR
078600010607     C*----------------------------------------------------*
078700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078800010607     C*----------------------------------------------------*
078900010607     C     IMPVAB        BEGSR
079000010607     C*
079100010607     C                   SETOFF                                       3132
079200030515     C*
079300010607     C                   MOVEL     datcor        VABAAS
079400010607     C                   MOVE      datcor        VABMGS
079500010607     C*
079600030715     C**********
079700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079800030715     C**********
079900030715     C                   IF        wGiro = *zeros
080000040113     C*
080100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080400080429     C                             %trim(intcol(5))<>*blanks
080500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080700080429     C                                      %trim(intcol(5))
080800070823     C     ';':CharCSV   XLATE     vindta        vindta
080900040510     C                   ENDIF
081000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081100031201     C     minu:maiu     XLATE     vindta        vindta
081200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081300030715     C                   DOW       posDa <= %len(%trim(vindta))
081400030715     C*
081500030715     C* Gestisco il 1° campo
081600030715     C                   IF        i = 1
081700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081800030715     C                   EVAL      posDa = 1
081900030715     C* Eseguo lo scan x trovare la fine del primo campo
082000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082100030715     C* A questo punto "estraggo" il campo corrente
082200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082300030715     C                                                    (posA-posDa))
082400030715     C* X i campi successivi al 1°
082500030715     C                   ELSE
082600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082700030715     C                   EVAL      posDa = posA + 1
082800030715     C* Eseguo lo scan x trovare la fine del campo corrente
082900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083000030715     C* Gestisco l'ultimo campo
083100030715     C                   IF        posA = *zeros
083200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083300030715     C* A questo punto "estraggo" il campo corrente
083400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083500030715     C                                                    (posA-posDa))
083600030715     C                   LEAVE
083700030715     C                   ELSE
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C                   ENDIF
084200030715     C                   ENDIF
084300030715     C* Incremento il contatore d campo
084400030715     C                   EVAL      i = i +1
084500030715     C                   ENDDO
084600030715     C*
084700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084800030715     C* delimitazione testo
084900030715     C                   EVAL      i = 1
085000030715     C                   DOW       i < %elem(SkSplitFLD)
085100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085300030715     C                   EVAL      i = i + 1
085400030715     C                   ENDDO
085500030715     C                   EVAL      wGiro = 1
085600030715     C                   ELSE
085700030715     C                   EVAL      wGiro = 2
085800031204     C**********
085900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086000031204     C* d separatore campo
086100031204     C**********
086200031204     C                   EVAL      vindta = %trim(vindta)
086300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086400031204     C                   EVAL      vindta = %subst(vindta:2)
086500031204     C                   ENDDO
086600031204     C*
086700031204     C                   Z-ADD     *zeros        lunghInput        4 0
086800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087200031204     C                   ENDDO
087300030715     C**********
087400030715     C* Effettuo lo split del campo dati d input
087500030715     C**********
087600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087700030715     C                   DOW       posDa <= %len(%trim(vindta))
087800030715     C*
087900030715     C* Gestisco il 1° campo
088000030715     C                   IF        i = 1
088100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088200030715     C                   EVAL      posDa = 1
088300030715     C* Eseguo lo scan x trovare la fine del primo campo
088400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088500030715     C* A questo punto "estraggo" il campo corrente
088600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088700030715     C                                                    (posA-posDa))
088800030715     C* X i campi successivi al 1°
088900030715     C                   ELSE
089000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089100030715     C                   EVAL      posDa = posA + 1
089200031204     C* Verifico che nn vi sia il campo nn valorizzato
089300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089400031204     C* Se campo nn valorizzato skippo al prossimo
089500031204     C                   EVAL      SkSplitCSV(i) = *blanks
089600031204     C                   EVAL      posA  = posA + 1
089700031204     C                   ELSE
089800030715     C* Eseguo lo scan x trovare la fine del campo corrente
089900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090000030715     C* Gestisco l'ultimo campo
090100030715     C                   IF        posA = *zeros
090200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090300030715     C* A questo punto "estraggo" il campo corrente
090400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090500030715     C                                                    (posA-posDa))
090600030715     C                   LEAVE
090700030715     C                   ELSE
090800030715     C* A questo punto "estraggo" il campo corrente
090900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091000030715     C                                                    (posA-posDa))
091100030715     C                   ENDIF
091200031204     C                   ENDIF
091300030715     C                   ENDIF
091400030715     C* Incremento il contatore d campo
091500030715     C                   EVAL      i = i +1
091600030715     C                   ENDDO
091700030715     C*
091800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091900030715     C* delimitazione testo
092000030715     C                   EVAL      i = 1
092100030715     C                   DOW       i < %elem(SkSplitCSV)
092200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092400030715     C                   EVAL      i = i + 1
092500030715     C                   ENDDO
092600100720     C*
092700100720     C* Stacco un progressivo x numero spedizione
092800120104 xxx C****               EXSR      repNSP
092900100326     C*
093000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093100030715     C                   EVAL      i = 1
093200030715     C                   DOW       i < %elem(SkSplitFLD)
093300030715     C*
093400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093600030715     C                             SkSplitCSV(i) <> *blanks
093700030721     C*** ATB
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093900030721     C                   EVAL      VABATB = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030715     C*** RSD
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094600030715     C* ==
094700030715     C                   ENDIF
094800030721     C*** RD2
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030715     C*** IND
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095400030715     C                   EVAL      VABIND = SkSplitCSV(i)
095500030715     C                   ENDIF
095600030715     C*** LOD
095700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095900030715     C                   ENDIF
096000030715     C*** PRD
096100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096300030715     C                   ENDIF
096400030721     C*** NZD
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030715     C*** RMA
096900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097100030715     C                   ENDIF
097200030715     C*** NOT
097300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097500030715     C                   ENDIF
097600030715     C*** NT2
097700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097900030715     C                   ENDIF
098000030715     C*** VCA
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** VAS
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** FFD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030721     C*** GC1
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** GC2
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TSP
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100300030721     C                   ENDIF
100400040510     C*** CBO
100500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100700040510     C                   ENDIF
100800030721     C*** NAS
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** TIC
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GCA
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** XCO
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** CTM
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TCR
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** CTS
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** FTM
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** VAD
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** GMA
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** GGA
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** GVA
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TC1
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** TC2
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** SCL
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** RMO
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** NMO
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*
107700030721     C* Reperisco quindi i campi numerici...
107800040510     C*** NSP
107900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108000040510     C                   EVAL      PiStr=SkSplitCSV(i)
108100040510     C                   EXSR      CHKNUM
108200040510     C                   IF        PiInt=*on
108300040510     C                   Z-ADD     PiVal         VABNSP
108400040510     C                   ELSE
108500040510     C                   SETON                                        31
108600040510     C                   EVAL      VABNSP = *zeros
108700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108800040510     C                             + ' ' + 'VABNSP'
108900040510     C                   ENDIF
109000040510     C                   ENDIF
109100071106     C*** RMN
109200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109300071106     C                   EVAL      PiStr=SkSplitCSV(i)
109400071106     C                   EXSR      CHKNUM
109500071106     C                   IF        PiInt=*on
109600071106     C                   Z-ADD     PiVal         VABRMN
109700071106     C                   ELSE
109800071106     C                   SETON                                        32
109900071106     C                   EVAL      VABRMN = *zeros
110000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110100071106     C                             + ' ' + 'VABRMN'
110200071106     C                   ENDIF
110300071106     C                   ENDIF
110400030721     C*** CAD
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiInt=*on
110900030721     C                   Z-ADD     PiVal         Num5_0
111000030721     C                   MOVEL(p)  Num5_0        VABCAD
111100030721     C                   ELSE
111200030721     C                   SETON                                        32
111300030721     C                   EVAL      VABCAD = *zeros
111400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111500030721     C                             + ' ' + 'VABCAD'
111600030721     C                   ENDIF
111700030721     C                   ENDIF
111800030721     C*** NCL
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on
112300030721     C                   Z-ADD     PiVal         VABNCL
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABNCL = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040122     C                             + ' ' + 'VABNCL'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** PKB
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiNum=*on
113600030721     C                   Z-ADD     PiVal         VABPKB
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABPKB = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABPKB'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** CAS
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114600120210     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114700170726     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
114800170726     C                             %trim(SkSplitCSV(i)) <> '0,000' AND
114900170726     C                             %trim(SkSplitCSV(i)) <> '0,00'  AND
115000170726     C                             %trim(SkSplitCSV(i)) <> '0.000' AND
115100170726     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
115200170726     C                             %trim(SkSplitCSV(i)) <> '0'
115300120210     C                   EVAL      wFlgCAS = '1'
115400120210     C                   ENDIF
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiNum=*on
115800030721     C                   Z-ADD     PiVal         VABCAS
115900120210     C                   EVAL      VABVCA = 'EUR'
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
131500120210     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):7:4)+
131600120210     C                                   %subst(%trim(SkSplitCSV(i)):4:2)+
131700120210     C                                   %subst(%trim(SkSplitCSV(i)):1:2)
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
140200111117     C*** VATNOT_I
140300111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140400111117     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140500111117     C                   ENDIF
140600111117     C*** VATNOT_J
140700111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
140800111117     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
140900111117     C                   ENDIF
141000070823     C*
141100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141200120104     C*** RMN / NSP
141300100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
141400120104     C                   EVAL      PiStr=SkSplitCSV(i)
141500120104     C                   EXSR      CHKNUM
141600100413     C                   IF        PiInt=*on
141700120104     C                   Z-ADD     PiVal         VABRMN
141800120104     C                   Z-ADD     PiVal         VABNSP
141900100413     C                   ELSE
142000100720     C                   Z-ADD     *zeros        VABRMN
142100100413     C                   SETON                                        32
142200100413     C                   EVAL      vinmsg = %trimr(vinmsg)
142300120104     C                             + ' ' + 'VABNSP VABRMN'
142400100413     C                   ENDIF
142500100326     C                   ENDIF
142600100831     C*** RSD
142700100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
142800100910     C                   EVAL      wRSD    = %trim(SkSplitCSV(i))
142900100831     C                   ENDIF
143000100910     C*** IND_1
143100100910     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_1'
143200100910     C                   EVAL      wVABIND_1 = %trim(SkSplitCSV(i))
143300100831     C                   ENDIF
143400100910     C*** IND_2
143500100910     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND_2'
143600100910     C                   EVAL      wVABIND_2 = %trim(SkSplitCSV(i))
143700100910     C                   ENDIF
143800120104     C*** LOD
143900120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
144000120104     C                   EVAL      wVABCAD =
144100120104     C                             %subst(%trim(SkSplitCSV(i)):1:5)
144200120104     C                   EVAL      wVABLOD =
144300120215     C                             %subst(%trim(SkSplitCSV(i)):6+1:
144400120216     C                             %len(%trim(SkSplitCSV(i)))-3-5)
144500120104     C                   EVAL      wVABPRD =
144600120104     C                             %subst(%trim(SkSplitCSV(i)):
144700120104     C                             %len(%trim(SkSplitCSV(i)))-1:2)
144800120104     C                   ENDIF
144900120104     C*** NZD
145000120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
145100120104     C                   EVAL      wVABNZD = %trim(SkSplitCSV(i))
145200120104     c                   IF        wVABNZD = 'ITA'
145300120104     c                   clear                   wVABNZD
145400120104     c                   ENDIF
145500120104     C                   ENDIF
145600120210     C*** NOT/NT2
145700120210     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
145800120210     C                   MOVEL     *blanks       wNOT             60
145900120210     C                   EVAL      wNOT=SkSplitCSV(i)
146000120210     C                   EVAL      VABNOT=%subst(wNOT:1:35)
146100120210     C                   EVAL      VABNT2=%subst(wNOT:36:25)
146200120210     C                   ENDIF
146300120104     C*** NOT_E
146400120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
146500120104     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
146600120104     C                   ENDIF
146700111117     C*** NOT_I/NOT_J
146800111117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
146900111117     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
147000111117     C                   ENDIF
147100070925     C*
147200071029     C***  <===  -----------------------------------------
147300040510     C*
147400030715     C                   ENDIF
147500030715     C                   EVAL      i = i + 1
147600030715     C                   ENDDO
147700070925     C*
147800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147900100831     C*
148000100831     C                   IF        wVABRSD <> *blanks
148100100831     C                   EVAL      VABRSD = wVABRSD
148200100831     C                   ENDIF
148300100831     C*
148400100831     C                   IF        wVABIND <> *blanks
148500100831     C                   EVAL      VABIND = wVABIND
148600100831     C                   ENDIF
148700100831     C*
148800100831     C                   IF        wVABCAD <> *blanks
148900100831     C                   EVAL      VABCAD = wVABCAD
149000100831     C                   ENDIF
149100100831     C*
149200100831     C                   IF        wVABLOD <> *blanks
149300100831     C                   EVAL      VABLOD = wVABLOD
149400100831     C                   ENDIF
149500100831     C*
149600100831     C                   IF        wVABPRD <> *blanks
149700100831     C                   EVAL      VABPRD = wVABPRD
149800100831     C                   ENDIF
149900120104     C*
150000120104     C                   IF        wVABNZD <> *blanks
150100120104     C                   EVAL      VABNZD = wVABNZD
150200120104     C                   ENDIF
150300100910     C*
150400100910     C                   IF        %trim(wVABIND_1)+%trim(wVABIND_2) <> *blanks
150500100910     C                   EVAL      VABIND=%trim(wVABIND_1)+' '+%trim(wVABIND_2)
150600100910     C                   ENDIF
150700100910     C*
150800100910     C                   IF        %trim(wRSD) <> *blanks
150900100910     C                   EVAL      VABRSD = %subst(wRSD:1:35)
151000100910     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
151100100910     C                   ENDIF
151200111117     C*
151300111117     C                   IF        %trim(wVATNOT_IJ) <> *blanks
151400111117     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
151500111117     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
151600111117     C                   ENDIF
151700070925     C*
151800071029     C***  <===  -----------------------------------------
151900070925     C*
152000070925     C* Considerazioni finali su CBO/CAS
152100100318     C                   IF        wFlgCAS = '1'    OR
152200100318     C                             VABCAS  > *zeros
152300070925     C                   IF        VABCBO = '1'
152400070925     C                   EVAL      VABCBO = '4'
152500070925     C                   ENDIF
152600070925     C                   IF        VABCBO = '2'
152700070925     C                   EVAL      VABCBO = '6'
152800070925     C                   ENDIF
152900070925     C                   ENDIF
153000070925     C***  <===  ----------------------------
153100040510     C*
153200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153300040510     C                   IF        VABNSP = *zeros
153400040510     C                   SETON                                        31
153500040510     C                   ENDIF
153600040715     C*
153700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
153800040715     C                   IF        VABLNP = *zeros
153900040715     C                   SETON                                        31
154000040715     C                   ENDIF
154100040510     C*
154200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154300040510     C                   IF        VABRMN = *zeros
154400040510     C                   Z-ADD     VABNSP        VABRMN
154500040510     C                   ENDIF
154600040510     C*
154700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
154800040510     C                   IF        VABRMA = *blanks
154900040714     C                   MOVEL     VABRMN        VABRMA
155000040510     C                   ENDIF
155100040128     C*
155200040510     C* Se provincia nn valorizzata la reperisco
155300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
155400040510     C                   IF        VABPRD = *blanks
155500040128     C                   CLEAR                   TISI95DS
155600040128     C                   EVAL      I95TCN = '3'
155700040128     C                   Z-ADD     datcor        I95DAT
155800040128     C                   EVAL      I95NAR = VABNZD
155900040128     C                   EVAL      I95CAP = VABCAD
156000040128     C                   EVAL      I95LOC = VABLOD
156100040128     C                   CALL      'TISI95R'
156200040128     C                   PARM                    TISI95DS
156300040128     C                   EVAL      VABPRD = O95PRV
156400040510     C                   ENDIF
156500020204     C*
156600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
156700020204     C                   EXSR      CHKIMPDIV
156800030715     C                   ENDIF
156900020204     C*
157000010607     C                   ENDSR
157100010607     C*----------------------------------------------------*
157200010601
157300020204
157400020204     C*----------------------------------------------------*
157500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
157600020204     C*----------------------------------------------------*
157700020204     C     CHKIMPDIV     BEGSR
157800020204     C*
157900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158000020204     C                   Z-ADD     *zeros        wrkDec            9 9
158100020204     C*
158200020204     C* Come prima cosa effettuo considerazioni sulla divisa
158300020204     C                   IF        vabIAS > *zeros
158400020204     C                   IF        vabVAS <> 'EUR'
158500020204     C                   EVAL      vabVAS =  'ITL'
158600020204     C                   ENDIF
158700020204     C                   ENDIF
158800020204     C*
158900020204     C                   IF        vabCAS > *zeros
159000020204     C                   IF        vabVCA <> 'EUR'
159100020204     C                   EVAL      vabVCA =  'ITL'
159200020204     C                   ENDIF
159300020204     C                   ENDIF
159400020204     C*
159500020204     C                   IF        vabVMD > *zeros
159600020204     C                   IF        vabVAD <> 'EUR'
159700020204     C                   EVAL      vabVAD =  'ITL'
159800020204     C                   ENDIF
159900020204     C                   ENDIF
160000020204     C*
160100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160200020204     C                   Z-ADD     vabIAS        wrkDec
160300020204     C                   IF        wrkDec > *zeros
160400020204     C                   IF        vabVAS = 'ITL'
160500020204     C                   EVAL      vabIAS = *zeros
160600020204     C                   ENDIF
160700020204     C                   ENDIF
160800020204     C*
160900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161000020204     C                   Z-ADD     vabCAS        wrkDec
161100020204     C                   IF        wrkDec > *zeros
161200020204     C                   IF        vabVCA = 'ITL'
161300020204     C                   EVAL      vabCAS = *zeros
161400020204     C                   ENDIF
161500020204     C                   ENDIF
161600020204     C*
161700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
161800020204     C                   Z-ADD     vabVMD        wrkDec
161900020204     C                   IF        wrkDec > *zeros
162000020204     C                   IF        vabVAD = 'ITL'
162100020204     C                   EVAL      vabVMD = *zeros
162200020204     C                   ENDIF
162300020204     C                   ENDIF
162400020204     C*
162500020204     C                   ENDSR
162600020204     C***
162700010330
162800100318
162900100318
163000100318     C*----------------------------------------------------*
163100100318     C*  CONTROLLO NUMERICITA' CAMPI
163200100318     C*----------------------------------------------------*
163300100318     C     CHKNUM        BEGSR
163400100318     C*
163500100318     C                   IF        PiDecChr = *blanks
163600100318     C                   EVAL      PiDecChr = CharNUM
163700100318     C                   ENDIF
163800100318     C*
163900100318     C                   callp     UBISNUM_Check(PiStr
164000100318     C                                          :PiDecChr
164100100318     C                                          :PiVal
164200100318     C                                          :PiNum
164300100318     C                                          :PiInt)
164400100318     C*
164500100318     C                   ENDSR
164600100318     C***
164700010601
164800010601
164900010601
165000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165100010601     C     repfil        BEGSR
165200010601     C*
165300010601     C                   if        invfil = *zeros and
165400010601     C                             depfil > *zeros and
165500010629     C                             (vinflg = *blanks or
165600010629     C                              vinflg = *zeros)
165700010601     C
165800010601     C                   eval      invfil = depfil
165900010601     C                   endif
166000010601     C*
166100010601     C                   if        depfil <> invfil and
166200010601     C                             invfil > *zeros
166300010601     C                   eval      flgMulti = '1'
166400010601     C                   if        vinflg = *blanks
166500010601     C                   add       1             cntNonEl
166600010601     C                   endif
166700010601     C                   endif
166800010601     C*
166900010601     C                   if        vinflg = '2'
167000010601     C                   eval      flgStato = '2'
167100010601     C                   endif
167200010601     C*
167300010601     C                   ENDSR
167400010601     C***
167500010601
167600010601
167700010601
167800010330
167900040119      /TITLE Invio dei dati al punto operativo.
168000040119     C     invio         BEGSR
168100040119     C*
168200040119     C* 1° invio FIVAT
168300040119     C                   reset                   dscmz
168400040119     C                   move      vlrpoi        cmzdst
168500040119     C                   eval      cmzfld = 'FIVATWWR'
168600040119     C                   eval      cmzmbd = vlrhdl
168700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
168800040119     C***                if        prmfir = *blanks
168900040119     C                   eval      cmzfla = 'FIVAT00F'
169000040119     C                   eval      cmzmba = 'FIVAT00F'
169100040119     C***                else
169200040119     C***                eval      cmzfla = prmfir
169300040119     C***                eval      cmzmba = prmfir
169400040119     C***                endif
169500040119     C                   eval      cmznrr = *zeros
169600040119     C                   move      §ctrokvt      cmznrr
169700040119     C                   eval      cmzlba = vlrfl1
169800040119     C                   call(e)   'TIS711C'
169900040119     C                   parm                    dscmz
170000040119     C                   parm      *blanks       esito
170100040119     C                   if        %error
170200040119     C                             or cmzerr = '1'
170300040119     C                             or esito  = '1'
170400040119     C                   eval      wrkesito = '3'
170500040119     C                   else
170600040119     C*
170700040119     C* 2° invio FIVAB
170800040119     C                   reset                   dscmz
170900040119     C                   move      vlrpoi        cmzdst
171000040119     C                   eval      cmzfld = vlrfou
171100040119     C                   eval      cmzmbd = vlrhdl
171200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171300040119     C***                if        prmfir = *blanks
171400040119     C                   eval      cmzfla = 'FIVAB00F'
171500040119     C                   eval      cmzmba = 'FIVAB00F'
171600040119     C***                else
171700040119     C***                eval      cmzfla = prmfir
171800040119     C***                eval      cmzmba = prmfir
171900040119     C***                endif
172000040119     C                   eval      cmznrr = *zeros
172100040119     C                   move      §ctrokvb      cmznrr
172200040119     C                   eval      cmzlba = vlrfl1
172300040119     C                   call(e)   'TIS711C'
172400040119     C                   parm                    dscmz
172500040119     C                   parm      *blanks       esito
172600040119     C                   if        %error
172700040119     C                             or cmzerr = '1'
172800040119     C                             or esito  = '1'
172900040119     C                   eval      wrkesito = '3'
173000040119     C                   endif
173100040119     C                   endif
173200040119     C*
173300040119     C                   ENDSR
173400040119     C***
173500010601
173600010601
173700010601
173800010601
173900010601      /TITLE Invio dei dati al punto operativo.
174000010601     C     opeini        BEGSR
174100010601     C*
174200010601     C* Inizializzo flag e contatori operativi
174300010601     C                   movel     '0'           flgGiro           1
174400010601     C                   movel     '0'           flgMulti          1
174500010601     C                   movel     '1'           flgStato          1
174600010615     C                   movel     '0'           flgOk             1
174700010601     C                   z-add     *zeros        cntNonEl         10 0
174800010601     C                   z-add     *zeros        depfil            3 0
174900010601     C                   z-add     *zeros        invfil            3 0
175000010601     C*
175100010601     C                   ENDSR
175200010601     C***
175300070326
175400070326
175500070326
175600070326
175700070326     C     *pssr         BEGSR
175800070329     C*
175900070329     C                   if        %open(tivin00r)
176000070329     C                   close     tivin00r
176100070329     C                   endif
176200070329     C                   if        %open(fivabwwr)
176300070329     C                   close     fivabwwr
176400070329     C                   endif
176500070329     C                   if        %open(fivatwwr)
176600070329     C                   close     fivatwwr
176700070329     C                   endif
176800070326     C*
176900070326     C* Effettuo la chiamata al CLLE preposto
177000070326     C                   call(e)   'TITVVTC'
177100070326     C                   parm                    parccm
177200070326     C                   parm                    parmbr
177300070326     C                   parm      '2'           paropz
177400070326     C*
177500070326     C                   eval      wrkesito = '2'
177600070404     C*
177700070404     C                   seton                                        LR
177800070326     C*
177900070326     C                   ENDSR     '*CANCL'
178000070326     C***
178100100318
178200100318
178300100318
178400100318     C     normCSV       BEGSR
178500100318     C*
178600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
178700100318     C* separatori campi
178800100318     C                   if        CharTXT <> *blanks
178900100318     C                   z-add     *zeros        wLenVINDTA        4 0
179000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179100100318     C                   setoff                                       80
179200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
179400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
179500100318     C                   if        not *in80
179600100318     C                   seton                                        80
179700100318     C                   else
179800100318     C                   setoff                                       80
179900100318     C                   endif
180000100318     C                   endif
180100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180200100318     C                             and *in80 = *on
180300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
180400100318     C                   endif
180500100318     C                   enddo
180600100318     C                   endif
180700100318     C*
180800100318     C                   ENDSR
180900100318     C***
181000070326
181100100326
181200100326
181300100326     C*----------------------------------------------------*
181400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
181500100326     C*----------------------------------------------------*
181600100326     C     REPNSP        BEGSR
181700100326     C*
181800100326     C* NSP => Stacco un numeratore da AZNUM
181900100326     C                   clear                   TRUL33DS
182000100326     C                   eval      I33OPE = *zeros
182100100326     C                   eval      I33CNU = 302
182200100326     C                   eval      I33NUM = 1
182300100326     C                   movel     TRUL33DS      KPJBU
182400100326     C                   call      'TRUL33R'
182500100326     C                   parm                    KPJBA
182600100326     C                   movel     KPJBU         TRUL33DS
182700100326     C                   if        O33ERR = *zeros
182800100326     C                   z-add     O33NRF        VABNSP
182900100326     C                   z-add     O33NRF        VATNSP
183000100326     C                   else
183100100326     C                   SETON                                        31
183200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183300100326     C                             + ' ' + 'VABNSP VATNSP'
183400100326     C                   endif
183500100326     C*
183600100326     C                   ENDSR
183700010330
183800100326
183900010330
184000000613     C     *inzsr        BEGSR
184100990910     C*
184200990910     C     *entry        plist
184300990920     C                   parm                    tivlrds
184400990921     C                   parm      wrkesito      esito
184500000724     C                   parm                    prmlit
184600000710     C                   parm                    prmfir
184700010330     C*
184800010330     C* CALCOLA LA DATA CORRENTE
184900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185000100318     C                   eval      datcor = %dec(%date() : *ISO)
185100000613     C*
185200000613     C                   ENDSR
185300000613     C***
185400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
185500120215FIL001;VABRMA;FIL002;FIL003;FIL004;FIL005;FIL006;FIL007;VABRSD;
185600120301VABIND;FIL008;W_VABLOD;W_VABNZD;VABPKB;FIL009;VABVLB;FIL010;VABNCL;
185700120215W_VATNOT_E;VABCAS;FIL011;VABDCR;FIL012;W_VABRMN;FIL014;W_VABNOT;
185800100910
185900100318
