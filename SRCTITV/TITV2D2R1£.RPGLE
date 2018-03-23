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
036600120731     C                   eval      ok_VABCTM = '7Q'
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
038700040714     C*
038800040714     C* Solo se VATNOT valorizzato scarico il buffer
038900040714     C                   if        VATNOT <> *blanks
039000040119     C*
039100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039200040510     C                   if        VATTRC = *blanks
039300040510     C                   eval      VATTRC = 'E'
039400040510     C                   endif
039500040510     C                   write     FIVAT000
039600040119     C                   add       1             §CTROKVT
039700040714     C*
039800040714     C                   endif
039900060331     C*
040000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040100060331     C                   if        wVATNOT_A <> *blanks
040200060331     C                   eval      VATNOT = wVATNOT_A
040300060331     C                   eval      VATTRC = 'A'
040400060331     C                   write     FIVAT000
040500060331     C                   add       1             §CTROKVT
040600060331     C                   endif
040700060331     C*
040800060331     C                   if        wVATNOT_B <> *blanks
040900060331     C                   eval      VATNOT = wVATNOT_B
041000060331     C                   eval      VATTRC = 'B'
041100060331     C                   write     FIVAT000
041200060331     C                   add       1             §CTROKVT
041300060331     C                   endif
041400060331     C*
041500060331     C                   if        wVATNOT_E <> *blanks
041600110201     C                   eval      VATNOT = wVATNOT_E
041700060331     C                   eval      VATTRC = 'E'
041800110201     C                   write     FIVAT000
041900110201     C                   add       1             §CTROKVT
042000110201     C                   endif
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
046100101018     C                   MOVEL     *blanks       wFlgCAS
046200060331     C                   MOVEL     *blanks       wVATNOT_A
046300060331     C                   MOVEL     *blanks       wVATNOT_B
046400060331     C                   MOVEL     *blanks       wVATNOT_E
046500100831     C                   MOVEL     *blanks       wVABRSD
046600100831     C                   MOVEL     *blanks       wVABIND
046700100831     C                   MOVEL     *blanks       wVABCAD
046800100831     C                   MOVEL     *blanks       wVABLOD
046900100831     C                   MOVEL     *blanks       wVABPRD
047000030822     C*
047100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047200030822     C                   CLEAR                   SkSplitCSV
047300030822     C                   EVAL      i = 1
047400030822     C                   EVAL      posDa = *zeros
047500030822     C                   EVAL      posA  = *zeros
047600080527     C*
047700080527     C                   CLEAR                   FIVABDS
047800080527     C                   CLEAR                   FIVAT000
047900030822     C*
048000030822     C* Reimposto i valori di default
048100030822     C                   EXSR      DEFCAM
048200030822     C*
048300010330     C                   ENDSR
048400010330     C*----------------------------------------------------*
048500030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048600010330     C*----------------------------------------------------*
048700010330     C     DEFCAM        BEGSR
048800030822     C*
048900030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049000030715     C* e delimitatore testo.
049100030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049200030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049300030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049400100318     C*
049500100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049600100318     C                   EVAL      CharSOS = CharNUM
049700030822     C*
049800030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
049900100720     C                   SETOFF                                       505152
050000100720     C                   SETOFF                                       53
050100100413     C                   SELECT
050200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050300100720     C                   SETON                                        50
050400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
050500100413     C                   SETON                                        51
050600100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
050700100720     C                   SETON                                        52
050800100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
050900100720     C                   SETON                                        53
051000100413     C                   ENDSL
051100030822     C*
051200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051300030822     C                   EVAL      posDaDft = 1
051400030822     C                   EVAL      posADft  = 0
051500030822     C                   EVAL      wGiroDft = 0
051600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
051700030822     C                             posDaDft > 0
051800030822     C*
051900030822     C* Gestisco il 1° giro
052000030822     C                   IF        wGiroDft = 0
052100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052300030822     C* Incremento il contatore dei "giri"
052400030822     C                   EVAL      wGiroDft = 1
052500030822     C                   ELSE
052600030822     C                   EVAL      posDaDft = posADft
052700030822     C                   ENDIF
052800030822     C* Eseguo lo scan x trovare la fine del campo corrente
052900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053000030822     C*
053100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053300030822     C                   IF        posDaDft > 0 AND
053400030822     C                             posADft  > 0
053500030822     C* NCL
053600030822     C                   IF        %subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):1:3)
053900030822     C                             = 'NCL'
054000030822     C                   EVAL      PiStr=%trim(%subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054300030822     C                   EXSR      CHKNUM
054400030822     C                   IF        PiInt=*on
054500030822     C                   Z-ADD     PiVal         VABNCL
054600030822     C                   ENDIF
054700030822     C                   ENDIF
054800030822     C* CCM
054900030822     C                   IF        %subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):1:3)
055200030822     C                             = 'CCM'
055300030822     C                   EVAL      PiStr=%trim(%subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055600030822     C                   EXSR      CHKNUM
055700030822     C                   IF        PiInt=*on
055800030822     C                   Z-ADD     PiVal         VABCCM
055900030822     C                   ENDIF
056000030822     C                   ENDIF
056100030822     C* LNP
056200030822     C                   IF        %subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):1:3)
056500030822     C                             = 'LNP'
056600030822     C                   EVAL      PiStr=%trim(%subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056900030822     C                   EXSR      CHKNUM
057000030822     C                   IF        PiInt=*on
057100030822     C                   Z-ADD     PiVal         VABLNP
057200030822     C                   ENDIF
057300030822     C                   ENDIF
057400030822     C* NRS
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'NRS'
057900030822     C                   EVAL      PiStr=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   EXSR      CHKNUM
058300030822     C                   IF        PiInt=*on
058400030822     C                   Z-ADD     PiVal         VABNRS
058500030822     C                   ENDIF
058600030822     C                   ENDIF
058700030822     C* CTR
058800030822     C                   IF        %subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):1:3)
059100030822     C                             = 'CTR'
059200030822     C                   EVAL      PiStr=%trim(%subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059500030822     C                   EXSR      CHKNUM
059600030822     C                   IF        PiInt=*on
059700030822     C                   Z-ADD     PiVal         VABCTR
059800030822     C                   ENDIF
059900030822     C                   ENDIF
060000030822     C* PKB
060100030822     C                   IF        %subst(
060200030822     C                             %subst(vlrppt:posDaDft+1:
060300030822     C                             posADft-posDaDft-1):1:3)
060400030822     C                             = 'PKB'
060500030822     C                   EVAL      PiStr=%trim(%subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060800030822     C                   EXSR      CHKNUM
060900030822     C                   IF        PiNum=*on
061000030822     C                   Z-ADD     PiVal         VABPKB
061100030822     C                   ENDIF
061200030822     C                   ENDIF
061300030822     C* VLB
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'VLB'
061800030822     C                   EVAL      PiStr=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   EXSR      CHKNUM
062200030822     C                   IF        PiNum=*on
062300030822     C                   Z-ADD     PiVal         VABVLB
062400030822     C                   ENDIF
062500030822     C                   ENDIF
062600030822     C* QFT
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'QFT'
063100030822     C                   EVAL      PiStr=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   EXSR      CHKNUM
063500030822     C                   IF        PiNum=*on
063600030822     C                   Z-ADD     PiVal         VABQFT
063700030822     C                   ENDIF
063800030822     C                   ENDIF
063900030822     C* CBO
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'CBO'
064400030822     C                   EVAL      VABCBO=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* TSP
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'TSP'
065300030822     C                   EVAL      VABTSP=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* VAS
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'VAS'
066200030822     C                   EVAL      VABVAS=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VCA
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VCA'
067100030822     C                   EVAL      VABVCA=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500070925     C* TIC
067600070925     C                   IF        %subst(
067700070925     C                             %subst(vlrppt:posDaDft+1:
067800070925     C                             posADft-posDaDft-1):1:3)
067900070925     C                             = 'TIC'
068000070925     C                   EVAL      VABTIC=%trim(%subst(
068100070925     C                             %subst(vlrppt:posDaDft+1:
068200070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300070925     C                   ENDIF
068400030822     C* GCA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GCA'
068900030822     C                   EVAL      VABGCA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* CTM
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'CTM'
069800030822     C                   EVAL      VABCTM=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* FFD
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'FFD'
070700030822     C                   EVAL      VABFFD=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* VAD
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'VAD'
071600030822     C                   EVAL      VABVAD=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* GMA
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'GMA'
072500030822     C                   EVAL      VABGMA=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* GGA
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'GGA'
073400030822     C                   EVAL      VABGGA=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800030822     C* GVA
073900030822     C                   IF        %subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):1:3)
074200030822     C                             = 'GVA'
074300030822     C                   EVAL      VABGVA=%trim(%subst(
074400030822     C                             %subst(vlrppt:posDaDft+1:
074500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600030822     C                   ENDIF
074700030822     C* TC1
074800030822     C                   IF        %subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):1:3)
075100030822     C                             = 'TC1'
075200030822     C                   EVAL      VABTC1=%trim(%subst(
075300030822     C                             %subst(vlrppt:posDaDft+1:
075400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500030822     C                   ENDIF
075600030822     C* TC2
075700030822     C                   IF        %subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):1:3)
076000030822     C                             = 'TC2'
076100030822     C                   EVAL      VABTC2=%trim(%subst(
076200030822     C                             %subst(vlrppt:posDaDft+1:
076300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076400030822     C                   ENDIF
076500040714     C* VATTRC
076600040714     C                   IF        %subst(
076700040714     C                             %subst(vlrppt:posDaDft+1:
076800040714     C                             posADft-posDaDft-1):1:3)
076900040714     C                             = 'TRC'
077000040714     C                   EVAL      VATTRC=%trim(%subst(
077100040714     C                             %subst(vlrppt:posDaDft+1:
077200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077300040714     C                   ENDIF
077400030822     C* ...
077500030822     C                   ENDIF
077600030822     C                   ENDDO
077700020204     C*
077800010330     C                   ENDSR
077900010607     C*----------------------------------------------------*
078000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078100010607     C*----------------------------------------------------*
078200010607     C     IMPVAB        BEGSR
078300010607     C*
078400010607     C                   SETOFF                                       3132
078500110315 xxx C***                SETON                                        35
078600030515     C*
078700010607     C                   MOVEL     datcor        VABAAS
078800010607     C                   MOVE      datcor        VABMGS
078900010607     C*
079000030715     C**********
079100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079200030715     C**********
079300030715     C                   IF        wGiro = *zeros
079400040113     C*
079500040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079600040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079700080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079800080429     C                             %trim(intcol(5))<>*blanks
079900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080000080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080100080429     C                                      %trim(intcol(5))
080200070823     C     ';':CharCSV   XLATE     vindta        vindta
080300040510     C                   ENDIF
080400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080500031201     C     minu:maiu     XLATE     vindta        vindta
080600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080700030715     C                   DOW       posDa <= %len(%trim(vindta))
080800030715     C*
080900030715     C* Gestisco il 1° campo
081000030715     C                   IF        i = 1
081100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081200030715     C                   EVAL      posDa = 1
081300030715     C* Eseguo lo scan x trovare la fine del primo campo
081400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081500030715     C* A questo punto "estraggo" il campo corrente
081600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081700030715     C                                                    (posA-posDa))
081800030715     C* X i campi successivi al 1°
081900030715     C                   ELSE
082000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082100030715     C                   EVAL      posDa = posA + 1
082200030715     C* Eseguo lo scan x trovare la fine del campo corrente
082300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082400030715     C* Gestisco l'ultimo campo
082500030715     C                   IF        posA = *zeros
082600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   LEAVE
083100030715     C                   ELSE
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   ENDIF
083600030715     C                   ENDIF
083700030715     C* Incremento il contatore d campo
083800030715     C                   EVAL      i = i +1
083900030715     C                   ENDDO
084000030715     C*
084100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084200030715     C* delimitazione testo
084300030715     C                   EVAL      i = 1
084400030715     C                   DOW       i < %elem(SkSplitFLD)
084500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084700030715     C                   EVAL      i = i + 1
084800030715     C                   ENDDO
084900030715     C                   EVAL      wGiro = 1
085000030715     C                   ELSE
085100030715     C                   EVAL      wGiro = 2
085200031204     C**********
085300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085400031204     C* d separatore campo
085500031204     C**********
085600031204     C                   EVAL      vindta = %trim(vindta)
085700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085800031204     C                   EVAL      vindta = %subst(vindta:2)
085900031204     C                   ENDDO
086000031204     C*
086100031204     C                   Z-ADD     *zeros        lunghInput        4 0
086200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086600031204     C                   ENDDO
086700030715     C**********
086800030715     C* Effettuo lo split del campo dati d input
086900030715     C**********
087000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087100030715     C                   DOW       posDa <= %len(%trim(vindta))
087200030715     C*
087300030715     C* Gestisco il 1° campo
087400030715     C                   IF        i = 1
087500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087600030715     C                   EVAL      posDa = 1
087700030715     C* Eseguo lo scan x trovare la fine del primo campo
087800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087900030715     C* A questo punto "estraggo" il campo corrente
088000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088100030715     C                                                    (posA-posDa))
088200030715     C* X i campi successivi al 1°
088300030715     C                   ELSE
088400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088500030715     C                   EVAL      posDa = posA + 1
088600031204     C* Verifico che nn vi sia il campo nn valorizzato
088700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088800031204     C* Se campo nn valorizzato skippo al prossimo
088900031204     C                   EVAL      SkSplitCSV(i) = *blanks
089000031204     C                   EVAL      posA  = posA + 1
089100031204     C                   ELSE
089200030715     C* Eseguo lo scan x trovare la fine del campo corrente
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* Gestisco l'ultimo campo
089500030715     C                   IF        posA = *zeros
089600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   LEAVE
090100030715     C                   ELSE
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   ENDIF
090600031204     C                   ENDIF
090700030715     C                   ENDIF
090800030715     C* Incremento il contatore d campo
090900030715     C                   EVAL      i = i +1
091000030715     C                   ENDDO
091100030715     C*
091200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091300030715     C* delimitazione testo
091400030715     C                   EVAL      i = 1
091500030715     C                   DOW       i < %elem(SkSplitCSV)
091600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091800030715     C                   EVAL      i = i + 1
091900030715     C                   ENDDO
092000100720     C*
092100100720     C* Stacco un progressivo x numero spedizione
092200100720 xxx C***                EXSR      repNSP
092300100326     C*
092400030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092500030715     C                   EVAL      i = 1
092600030715     C                   DOW       i < %elem(SkSplitFLD)
092700030715     C*
092800030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092900030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093000030715     C                             SkSplitCSV(i) <> *blanks
093100030721     C*** ATB
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093300030721     C                   EVAL      VABATB = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030715     C*** RSD
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093700030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093800030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093900030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094000030715     C* ==
094100030715     C                   ENDIF
094200030721     C*** RD2
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094400030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030715     C*** IND
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094800030715     C                   EVAL      VABIND = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030715     C*** LOD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095200030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095300030715     C                   ENDIF
095400030715     C*** PRD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095600030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095700030715     C                   ENDIF
095800030721     C*** NZD
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096000030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030715     C*** RMA
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096400030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** NOT
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096800030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** NT2
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097200030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030715     C*** VCA
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097600030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** VAS
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098000030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** FFD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098400030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030721     C*** GC1
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098800030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** GC2
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099200030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** TSP
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099700030721     C                   ENDIF
099800040510     C*** CBO
099900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100100040510     C                   ENDIF
100200030721     C*** NAS
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** TIC
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GCA
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** XCO
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** CTM
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TCR
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** CTS
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** FTM
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** VAD
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GMA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GGA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** GVA
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TC1
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TC2
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** SCL
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** RMO
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** NMO
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*
107100030721     C* Reperisco quindi i campi numerici...
107200040510     C*** NSP
107300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107400040510     C                   EVAL      PiStr=SkSplitCSV(i)
107500040510     C                   EXSR      CHKNUM
107600040510     C                   IF        PiInt=*on
107700040510     C                   Z-ADD     PiVal         VABNSP
107800040510     C                   ELSE
107900040510     C                   SETON                                        31
108000040510     C                   EVAL      VABNSP = *zeros
108100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108200040510     C                             + ' ' + 'VABNSP'
108300040510     C                   ENDIF
108400040510     C                   ENDIF
108500071106     C*** RMN
108600071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108700071106     C                   EVAL      PiStr=SkSplitCSV(i)
108800071106     C                   EXSR      CHKNUM
108900071106     C                   IF        PiInt=*on
109000071106     C                   Z-ADD     PiVal         VABRMN
109100071106     C                   ELSE
109200071106     C                   SETON                                        32
109300071106     C                   EVAL      VABRMN = *zeros
109400071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109500071106     C                             + ' ' + 'VABRMN'
109600071106     C                   ENDIF
109700071106     C                   ENDIF
109800030721     C*** CAD
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on
110300030721     C                   Z-ADD     PiVal         Num5_0
110400030721     C                   MOVEL(p)  Num5_0        VABCAD
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABCAD = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABCAD'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** NCL
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on
111700030721     C                   Z-ADD     PiVal         VABNCL
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABNCL = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200040122     C                             + ' ' + 'VABNCL'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** PKB
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiNum=*on
113000030721     C                   Z-ADD     PiVal         VABPKB
113100030721     C                   ELSE
113200030721     C                   SETON                                        32
113300030721     C                   EVAL      VABPKB = *zeros
113400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113500030721     C                             + ' ' + 'VABPKB'
113600030721     C                   ENDIF
113700030721     C                   ENDIF
113800030721     C*** CAS
113900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114000141202     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114100141202     C                             %trim(SkSplitCSV(i)) <> *zeros
114200101018     C                   EVAL      wFlgCAS = '1'
114300101018     C                   ENDIF
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABCAS
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABCAS = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABCAS'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** CCM
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on      AND
116000030721     C                             PiVal<=9999999 AND
116100030721     C                             PiVal>*zeros
116200030721     C                   Z-ADD     PiVal         VABCCM
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABCCM = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700040510     C                             + ' ' + 'VABCCM'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** LNP
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on    AND
117500030721     C                             PiVal<=999   AND
117600030721     C                             PiVal>*zeros
117700030721     C                   Z-ADD     PiVal         VABLNP
117800030721     C                   ELSE
117900040715     C                   SETON                                        32
118000030721     C                   EVAL      VABLNP = *zeros
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200040510     C                             + ' ' + 'VABLNP'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** NRS
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiInt=*on AND
119000030721     C                             PiVal<=99
119100030721     C                   Z-ADD     PiVal         VABNRS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABNRS'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNA
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNA
120600030721     C                   ELSE
120700030721     C                   SETON                                        32
120800030721     C                   EVAL      VABLNA = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000030721     C                             + ' ' + 'VABLNA'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** CTR
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on  AND
121800030721     C                             PiVal<=999
121900030721     C                   Z-ADD     PiVal         VABCTR
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABCTR'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** IAS
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiNum=*on
123100030721     C                   Z-ADD     PiVal         VABIAS
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123500030721     C                             + ' ' + 'VABIAS'
123600030721     C                   ENDIF
123700030721     C                   ENDIF
123800030721     C*** VLB
123900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124000030721     C                   EVAL      PiStr=SkSplitCSV(i)
124100030721     C                   EXSR      CHKNUM
124200030721     C                   IF        PiNum=*on
124300030721     C                   Z-ADD     PiVal         VABVLB
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABVLB'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** QFT
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiNum=*on
125500030721     C                   Z-ADD     PiVal         VABQFT
125600030721     C                   ELSE
125700030721     C                   SETON                                        32
125800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125900030721     C                             + ' ' + 'VABQFT'
126000030721     C                   ENDIF
126100030721     C                   ENDIF
126200030721     C*** NCD
126300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126400030721     C                   EVAL      PiStr=SkSplitCSV(i)
126500030721     C                   EXSR      CHKNUM
126600030721     C                   IF        PiInt=*on      AND
126700030721     C                             PiVal<=9999999
126800030721     C                   Z-ADD     PiVal         VABNCD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABNCD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** NCA
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on      AND
128000030721     C                             PiVal<=9999999
128100030721     C                   Z-ADD     PiVal         VABNCA
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABNCA'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** ZNC
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on AND
129300030721     C                             PiVal<=99
129400030721     C                   Z-ADD     PiVal         VABZNC
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABZNC'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** DCR
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on       AND
130600030721     C                             PiVal<=99999999
130700030721     C                   Z-ADD     PiVal         VABDCR
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABDCR'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** HCR
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on   AND
131900030721     C                             PiVal<=9999
132000030721     C                   Z-ADD     PiVal         VABHCR
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABHCR'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** VMD
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiNum=*on
133200030721     C                   Z-ADD     PiVal         VABVMD
133300030721     C                   ELSE
133400030721     C                   SETON                                        32
133500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133600030721     C                             + ' ' + 'VABVMD'
133700030721     C                   ENDIF
133800030721     C                   ENDIF
133900030721     C*** ANT
134000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134100030721     C                   EVAL      PiStr=SkSplitCSV(i)
134200030721     C                   EXSR      CHKNUM
134300030721     C                   IF        PiInt=*on        AND
134400030721     C                             PiVal<=999999999
134500030721     C                   Z-ADD     PiVal         VABANT
134600030721     C                   ELSE
134700030721     C                   SETON                                        32
134800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134900030721     C                             + ' ' + 'VABANT'
135000030721     C                   ENDIF
135100030721     C                   ENDIF
135200030721     C*** CMO
135300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135400030721     C                   EVAL      PiStr=SkSplitCSV(i)
135500030721     C                   EXSR      CHKNUM
135600030721     C                   IF        PiInt=*on
135700030721     C                   Z-ADD     PiVal         Num5_0
135800030721     C                   MOVEL(p)  Num5_0        VABCMO
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      VABCMO = *zeros
136200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136300030721     C                             + ' ' + 'VABCMO'
136400030721     C                   ENDIF
136500030721     C                   ENDIF
136600040510     C*
136700040510     C* Reperisco i campi del FIVAT
136800040510     C*** VATNOT
136900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137100040510     C                   ENDIF
137200040510     C*** VATTRC
137300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137500040510     C                   ENDIF
137600060331     C*** VATNOT_A
137700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137900060331     C                   ENDIF
138000060331     C*** VATNOT_B
138100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400060331     C*** VATNOT_E
138500070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138700060331     C                   ENDIF
138800070823     C*
138900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139000110131     C*** NSP / RMN
139100101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
139200110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
139300101021     C                   EXSR      CHKNUM
139400101021     C                   IF        PiInt=*on
139500101021     C                   Z-ADD     PiVal         VABNSP
139600101021     C                   Z-ADD     PiVal         VABRMN
139700101021     C                   ELSE
139800101021     C                   SETON                                        31
139900101021     C                   EVAL      VABNSP = *zeros
140000101021     C                   EVAL      VABRMN = *zeros
140100101021     C                   EVAL      vinmsg = %trimr(vinmsg)
140200101021     C                             + ' ' + 'VABNSP VABRMN'
140300101021     C                   ENDIF
140400101021     C                   ENDIF
140500151028     C*
140600151028     C*** TIC / CBO
140700151028     C                   IF        wFlgCAS = '1'    OR
140800151028     C                             VABCAS  > *zeros
140900151028     C                   EVAL      VABTIC = 'BM'
141000151028     C                   IF        VABCBO = '1'
141100151028     C                   EVAL      VABCBO = '4'
141200151028     C                   ENDIF
141300151028     C                   IF        VABCBO = '2'
141400151028     C                   EVAL      VABCBO = '6'
141500151028     C                   ENDIF
141600151028     C                   ENDIF
141700070925     C*
141800071029     C***  <===  -----------------------------------------
141900040510     C*
142000030715     C                   ENDIF
142100030715     C                   EVAL      i = i + 1
142200030715     C                   ENDDO
142300070925     C*
142400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142500100831     C*
142600100831     C                   IF        wVABRSD <> *blanks
142700100831     C                   EVAL      VABRSD = wVABRSD
142800100831     C                   ENDIF
142900100831     C*
143000100831     C                   IF        wVABIND <> *blanks
143100100831     C                   EVAL      VABIND = wVABIND
143200100831     C                   ENDIF
143300100831     C*
143400100831     C                   IF        wVABCAD <> *blanks
143500100831     C                   EVAL      VABCAD = wVABCAD
143600100831     C                   ENDIF
143700100831     C*
143800100831     C                   IF        wVABLOD <> *blanks
143900100831     C                   EVAL      VABLOD = wVABLOD
144000100831     C                   ENDIF
144100100831     C*
144200100831     C                   IF        wVABPRD <> *blanks
144300100831     C                   EVAL      VABPRD = wVABPRD
144400100831     C                   ENDIF
144500070925     C*
144600071029     C***  <===  -----------------------------------------
144700070925     C*
144800070925     C* Considerazioni finali su CBO/CAS
144900100318     C                   IF        wFlgCAS = '1'    OR
145000100318     C                             VABCAS  > *zeros
145100070925     C                   IF        VABCBO = '1'
145200070925     C                   EVAL      VABCBO = '4'
145300070925     C                   ENDIF
145400070925     C                   IF        VABCBO = '2'
145500070925     C                   EVAL      VABCBO = '6'
145600070925     C                   ENDIF
145700070925     C                   ENDIF
145800070925     C***  <===  ----------------------------
145900040510     C*
146000040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146100040510     C                   IF        VABNSP = *zeros
146200040510     C                   SETON                                        31
146300040510     C                   ENDIF
146400040715     C*
146500040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146600040715     C                   IF        VABLNP = *zeros
146700040715     C                   SETON                                        31
146800040715     C                   ENDIF
146900040510     C*
147000040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147100040510     C                   IF        VABRMN = *zeros
147200040510     C                   Z-ADD     VABNSP        VABRMN
147300040510     C                   ENDIF
147400040510     C*
147500040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147600040510     C                   IF        VABRMA = *blanks
147700040714     C                   MOVEL     VABRMN        VABRMA
147800040510     C                   ENDIF
147900040128     C*
148000040510     C* Se provincia nn valorizzata la reperisco
148100040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148200040510     C                   IF        VABPRD = *blanks
148300040128     C                   CLEAR                   TISI95DS
148400040128     C                   EVAL      I95TCN = '3'
148500040128     C                   Z-ADD     datcor        I95DAT
148600040128     C                   EVAL      I95NAR = VABNZD
148700040128     C                   EVAL      I95CAP = VABCAD
148800040128     C                   EVAL      I95LOC = VABLOD
148900040128     C                   CALL      'TISI95R'
149000040128     C                   PARM                    TISI95DS
149100040128     C                   EVAL      VABPRD = O95PRV
149200040510     C                   ENDIF
149300020204     C*
149400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149500020204     C                   EXSR      CHKIMPDIV
149600030715     C                   ENDIF
149700020204     C*
149800010607     C                   ENDSR
149900010607     C*----------------------------------------------------*
150000010601
150100020204
150200020204     C*----------------------------------------------------*
150300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150400020204     C*----------------------------------------------------*
150500020204     C     CHKIMPDIV     BEGSR
150600020204     C*
150700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150800020204     C                   Z-ADD     *zeros        wrkDec            9 9
150900020204     C*
151000020204     C* Come prima cosa effettuo considerazioni sulla divisa
151100020204     C                   IF        vabIAS > *zeros
151200020204     C                   IF        vabVAS <> 'EUR'
151300020204     C                   EVAL      vabVAS =  'ITL'
151400020204     C                   ENDIF
151500020204     C                   ENDIF
151600020204     C*
151700020204     C                   IF        vabCAS > *zeros
151800141202     C                   EVAL      vabVCA = 'EUR'
151900141202     C                   EVAL      vabTIC = *blank
152000020204     C                   IF        vabVCA <> 'EUR'
152100020204     C                   EVAL      vabVCA =  'ITL'
152200020204     C                   ENDIF
152300020204     C                   ENDIF
152400020204     C*
152500020204     C                   IF        vabVMD > *zeros
152600020204     C                   IF        vabVAD <> 'EUR'
152700020204     C                   EVAL      vabVAD =  'ITL'
152800020204     C                   ENDIF
152900020204     C                   ENDIF
153000020204     C*
153100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153200020204     C                   Z-ADD     vabIAS        wrkDec
153300020204     C                   IF        wrkDec > *zeros
153400020204     C                   IF        vabVAS = 'ITL'
153500020204     C                   EVAL      vabIAS = *zeros
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154000020204     C                   Z-ADD     vabCAS        wrkDec
154100020204     C                   IF        wrkDec > *zeros
154200020204     C                   IF        vabVCA = 'ITL'
154300020204     C                   EVAL      vabCAS = *zeros
154400020204     C                   ENDIF
154500020204     C                   ENDIF
154600020204     C*
154700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154800020204     C                   Z-ADD     vabVMD        wrkDec
154900020204     C                   IF        wrkDec > *zeros
155000020204     C                   IF        vabVAD = 'ITL'
155100020204     C                   EVAL      vabVMD = *zeros
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C                   ENDSR
155600020204     C***
155700010330
155800100318
155900100318
156000100318     C*----------------------------------------------------*
156100100318     C*  CONTROLLO NUMERICITA' CAMPI
156200100318     C*----------------------------------------------------*
156300100318     C     CHKNUM        BEGSR
156400100318     C*
156500100318     C                   IF        PiDecChr = *blanks
156600100318     C                   EVAL      PiDecChr = CharNUM
156700100318     C                   ENDIF
156800100318     C*
156900100318     C                   callp     UBISNUM_Check(PiStr
157000100318     C                                          :PiDecChr
157100100318     C                                          :PiVal
157200100318     C                                          :PiNum
157300100318     C                                          :PiInt)
157400100318     C*
157500100318     C                   ENDSR
157600100318     C***
157700010601
157800010601
157900010601
158000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158100010601     C     repfil        BEGSR
158200010601     C*
158300010601     C                   if        invfil = *zeros and
158400010601     C                             depfil > *zeros and
158500010629     C                             (vinflg = *blanks or
158600010629     C                              vinflg = *zeros)
158700010601     C
158800010601     C                   eval      invfil = depfil
158900010601     C                   endif
159000010601     C*
159100010601     C                   if        depfil <> invfil and
159200010601     C                             invfil > *zeros
159300010601     C                   eval      flgMulti = '1'
159400010601     C                   if        vinflg = *blanks
159500010601     C                   add       1             cntNonEl
159600010601     C                   endif
159700010601     C                   endif
159800010601     C*
159900010601     C                   if        vinflg = '2'
160000010601     C                   eval      flgStato = '2'
160100010601     C                   endif
160200010601     C*
160300010601     C                   ENDSR
160400010601     C***
160500010601
160600010601
160700010601
160800010330
160900040119      /TITLE Invio dei dati al punto operativo.
161000040119     C     invio         BEGSR
161100040119     C*
161200040119     C* 1° invio FIVAT
161300040119     C                   reset                   dscmz
161400040119     C                   move      vlrpoi        cmzdst
161500040119     C                   eval      cmzfld = 'FIVATWWR'
161600040119     C                   eval      cmzmbd = vlrhdl
161700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161800040119     C***                if        prmfir = *blanks
161900040119     C                   eval      cmzfla = 'FIVAT00F'
162000040119     C                   eval      cmzmba = 'FIVAT00F'
162100040119     C***                else
162200040119     C***                eval      cmzfla = prmfir
162300040119     C***                eval      cmzmba = prmfir
162400040119     C***                endif
162500040119     C                   eval      cmznrr = *zeros
162600040119     C                   move      §ctrokvt      cmznrr
162700040119     C                   eval      cmzlba = vlrfl1
162800040119     C                   call(e)   'TIS711C'
162900040119     C                   parm                    dscmz
163000040119     C                   parm      *blanks       esito
163100040119     C                   if        %error
163200040119     C                             or cmzerr = '1'
163300040119     C                             or esito  = '1'
163400040119     C                   eval      wrkesito = '3'
163500040119     C                   else
163600040119     C*
163700040119     C* 2° invio FIVAB
163800040119     C                   reset                   dscmz
163900040119     C                   move      vlrpoi        cmzdst
164000040119     C                   eval      cmzfld = vlrfou
164100040119     C                   eval      cmzmbd = vlrhdl
164200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164300040119     C***                if        prmfir = *blanks
164400040119     C                   eval      cmzfla = 'FIVAB00F'
164500040119     C                   eval      cmzmba = 'FIVAB00F'
164600040119     C***                else
164700040119     C***                eval      cmzfla = prmfir
164800040119     C***                eval      cmzmba = prmfir
164900040119     C***                endif
165000040119     C                   eval      cmznrr = *zeros
165100040119     C                   move      §ctrokvb      cmznrr
165200040119     C                   eval      cmzlba = vlrfl1
165300040119     C                   call(e)   'TIS711C'
165400040119     C                   parm                    dscmz
165500040119     C                   parm      *blanks       esito
165600040119     C                   if        %error
165700040119     C                             or cmzerr = '1'
165800040119     C                             or esito  = '1'
165900040119     C                   eval      wrkesito = '3'
166000040119     C                   endif
166100040119     C                   endif
166200040119     C*
166300040119     C                   ENDSR
166400040119     C***
166500010601
166600010601
166700010601
166800010601
166900010601      /TITLE Invio dei dati al punto operativo.
167000010601     C     opeini        BEGSR
167100010601     C*
167200010601     C* Inizializzo flag e contatori operativi
167300010601     C                   movel     '0'           flgGiro           1
167400010601     C                   movel     '0'           flgMulti          1
167500010601     C                   movel     '1'           flgStato          1
167600010615     C                   movel     '0'           flgOk             1
167700010601     C                   z-add     *zeros        cntNonEl         10 0
167800010601     C                   z-add     *zeros        depfil            3 0
167900010601     C                   z-add     *zeros        invfil            3 0
168000010601     C*
168100010601     C                   ENDSR
168200010601     C***
168300070326
168400070326
168500070326
168600070326
168700070326     C     *pssr         BEGSR
168800070329     C*
168900070329     C                   if        %open(tivin00r)
169000070329     C                   close     tivin00r
169100070329     C                   endif
169200070329     C                   if        %open(fivabwwr)
169300070329     C                   close     fivabwwr
169400070329     C                   endif
169500070329     C                   if        %open(fivatwwr)
169600070329     C                   close     fivatwwr
169700070329     C                   endif
169800070326     C*
169900070326     C* Effettuo la chiamata al CLLE preposto
170000070326     C                   call(e)   'TITVVTC'
170100070326     C                   parm                    parccm
170200070326     C                   parm                    parmbr
170300070326     C                   parm      '2'           paropz
170400070326     C*
170500070326     C                   eval      wrkesito = '2'
170600070404     C*
170700070404     C                   seton                                        LR
170800070326     C*
170900070326     C                   ENDSR     '*CANCL'
171000070326     C***
171100100318
171200100318
171300100318
171400100318     C     normCSV       BEGSR
171500100318     C*
171600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171700100318     C* separatori campi
171800100318     C                   if        CharTXT <> *blanks
171900100318     C                   z-add     *zeros        wLenVINDTA        4 0
172000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172100100318     C                   setoff                                       80
172200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172500100318     C                   if        not *in80
172600100318     C                   seton                                        80
172700100318     C                   else
172800100318     C                   setoff                                       80
172900100318     C                   endif
173000100318     C                   endif
173100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173200100318     C                             and *in80 = *on
173300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173400100318     C                   endif
173500100318     C                   enddo
173600100318     C                   endif
173700100318     C*
173800100318     C                   ENDSR
173900100318     C***
174000070326
174100100326
174200100326
174300100326     C*----------------------------------------------------*
174400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174500100326     C*----------------------------------------------------*
174600100326     C     REPNSP        BEGSR
174700100326     C*
174800100326     C* NSP => Stacco un numeratore da AZNUM
174900100326     C                   clear                   TRUL33DS
175000100326     C                   eval      I33OPE = *zeros
175100100326     C                   eval      I33CNU = 302
175200100326     C                   eval      I33NUM = 1
175300100326     C                   movel     TRUL33DS      KPJBU
175400100326     C                   call      'TRUL33R'
175500100326     C                   parm                    KPJBA
175600100326     C                   movel     KPJBU         TRUL33DS
175700100326     C                   if        O33ERR = *zeros
175800100326     C                   z-add     O33NRF        VABNSP
175900100326     C                   z-add     O33NRF        VATNSP
176000100326     C                   else
176100100326     C                   SETON                                        31
176200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176300100326     C                             + ' ' + 'VABNSP VATNSP'
176400100326     C                   endif
176500100326     C*
176600100326     C                   ENDSR
176700010330
176800100326
176900010330
177000000613     C     *inzsr        BEGSR
177100990910     C*
177200990910     C     *entry        plist
177300990920     C                   parm                    tivlrds
177400990921     C                   parm      wrkesito      esito
177500000724     C                   parm                    prmlit
177600000710     C                   parm                    prmfir
177700010330     C*
177800010330     C* CALCOLA LA DATA CORRENTE
177900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178000100318     C                   eval      datcor = %dec(%date() : *ISO)
178100000613     C*
178200000613     C                   ENDSR
178300000613     C***
178400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178500120731VATNOT_E;FILXXX;W_VABNSP;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;
178600121005FILXXX;FILXXX;FILXXX;FILXXX;VABRSD;VABRD2;VABIND;FILXXX;FILXXX;VABLOD;
178700120731VABCAD;FILXXX;FILXXX;VATNOT_B;FILXXX;FILXXX;VABPKB;FILXXX;FILXXX;FILXXX;
178800141202FILXXX;FILXXX;VABCAS;FILXXX;FILXXX;FILXXX;FILXXX;
178900121005FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;VABRMA;FILXXX;FILXXX;
