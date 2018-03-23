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
003400160201     D w70             s             70    INZ(*blanks)
003500100326     D*------------------
003600100326     D* DS REPERIMENTO NUMERATORE
003700100326     D*------------------
003800100326     D trul33ds      e ds                  inz
003900100326     D*------------------
004000100326     D* DS ARCHITETTURA
004100100326     D*------------------
004200100326     D kpjba         e ds                  inz
004300100326
004400030715     D*------------------
004500030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004600030715     D*------------------
004700030715     D SkSplitFLD      S             10    DIM(100)
004800030715     D SkSplitCSV      S            256    DIM(100)
004900030715     D CharCSV         S              1
005000030715     D CharTXT         S              1
005100030715     D CharNUM         S              1
005200100318     D CharSOS         S              1
005300030715     D posDa           S              3  0 INZ(*zeros)
005400030715     D posA            S              3  0 INZ(*zeros)
005500030715     D i               s              3  0 INZ(1)
005600030715     D wGiro           s              1  0 INZ(*zeros)
005700070823     D wVATNOT_A       s                   LIKE(VATNOT)
005800060331     D wVATNOT_B       s                   LIKE(VATNOT)
005900060331     D wVATNOT_E       s                   LIKE(VATNOT)
006000160201     D wVATNOT_IJ      s                   LIKE(w70)
006100100831     D wVABRSD         s                   LIKE(VABRSD)
006200100831     D wVABIND         s                   LIKE(VABIND)
006300100831     D wVABCAD         s                   LIKE(VABCAD)
006400100831     D wVABLOD         s                   LIKE(VABLOD)
006500100831     D wVABPRD         s                   LIKE(VABPRD)
006600030822     D*------------------
006700030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006800030822     D*------------------
006900030822     D posDaDft        S              3  0 INZ(*zeros)
007000030822     D posADft         S              3  0 INZ(*zeros)
007100030822     D j               s              3  0 INZ(1)
007200030822     D wGiroDft        s              1  0 INZ(*zeros)
007300080418     D*------------------
007400080418     D* Costanti
007500080418     D*------------------
007600080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007700080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007800100318
007900100318     D*------------------
008000100318     D* LINKING A DEFINIZIONI ESTERNE
008100100318     D*------------------
008200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400100318
008500100909     D*------------------
008600100909     D* DS CALCOLO CHECK-DIGITS
008700100909     D*------------------
008800100909     D TRUL28DSX     e ds                  inz
008900010330
009000010330
009100010330
009200000913     C                   reset                   rrnum
009300990921     C                   reset                   esito
009400990921     C                   reset                   wrkesito
009500010601     C*
009600010601     C                   exsr      opeini
009700010605     C                   exsr      rwvab
009800010601     C*
009900010601     C                   seton                                        lr
010000010601
010100010601
010200010601
010300010601     C*--------------------------------------------------------
010400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010500010601     C*--------------------------------------------------------
010600010601     C     PREELA        BEGSR
010700010601     C*
010800010601     C* SE OCCORRE SPEDIRE IN FILIALE
010900010601     C                   if        invfil <> *zeros and
011000010601     C                             flgGiro = '0'
011100010601     C*
011200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011300010601     C                   eval      flgGiro = '1'
011400010601     C*
011500010601     C                   endif
011600010601     C*
011700010601     C                   ENDSR
011800010601     C***
011900010601
012000010601
012100010601
012200010601
012300010601     C*--------------------------------------------------------
012400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012500010601     C*--------------------------------------------------------
012600010601     C     ENDELA        BEGSR
012700040119     C*
012800040119     C* Effettuo la chiamata al CLLE preposto
012900040119     C                   call(e)   'TITVVTC'
013000040510     C                   parm                    parccm
013100040119     C                   parm                    parmbr
013200040119     C                   parm      '2'           paropz
013300050201     C*
013400050201     C* Effettuo lancio TISI95 solo x chiusura
013500050201     C                   CLEAR                   TISI95DS
013600050201     C                   EVAL      I95TLA = 'C'
013700050201     C                   CALL      'TISI95R'
013800050201     C                   PARM                    TISI95DS
013900000616     C*
014000010601     C                   ENDSR
014100010601     C***
014200000613
014300010601
014400010601
014500010330     C*--------------------------------------------------------
014600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014700010330     C*--------------------------------------------------------
014800010605     C     RWVAB         BEGSR
014900010330     C*
015000010330     C                   if        not %open(tivin00r)
015100010330     C                   open      tivin00r
015200010330     C                   endif
015300021025     C                   if        not %open(fivabwwr)
015400021025     C                   open      fivabwwr
015500010330     C                   endif
015600040119     C*
015700040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800040119     C                   exsr      prevat
015900040119     C*
016000040119     C                   if        chkcall = '0'
016100040119     C*
016200040119     C                   if        not %open(fivatwwr)
016300040119     C                   open      fivatwwr
016400040119     C                   endif
016500010330     C*
016600040119     C                   clear                   §CTROKVB          7 0
016700040119     C                   clear                   §CTROKVT          7 0
016800040119     C                   clear                   §CTRMO            7 0
016900040119     C                   clear                   §CTRNO            7 0
017000070823     C*
017100070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017200070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017300110315     C                   exsr      inzvar
017400110315     C                   exsr      impvab
017500110131     C*
017600110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017700110315 xxx C***                read      tivin00r                               70
017800010330     C*
017900030822     C                   do        *HIVAL
018000010330     C*
018100030822     C                   read      tivin00r                               70
018200010618     C*
018300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018400010618     C                   if        vinflg = '1'
018500010618     C                   eval      flgOk = '1'
018600010618     C                   endif
018700040510     C*
018800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018900040510     C                   z-add     *zeros        wLenVINDTA        4 0
019000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019400040510     C                   z-add     1             wFlgVINDTA
019500040510     C                   leave
019600040510     C                   endif
019700040510     C                   enddo
019800010618     C*
019900040510     C                   if        vindta > *blanks AND
020000040510     C                             wFlgVINDTA = 1
020100010330     C                   add       1             rrnum
020200010330     C*
020300010601     C                   if        *in70 = *off and
020400010330     C                             (vinflg = *blanks
020500010330     C                              or vinflg = '0'
020600010330     C                              or vinflg = '2')
020700010330     C*
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010601     C*
021300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021400070925     C                   eval      vindta  = ' ' + %trim(vindta)
021500070925     C*
021600080527     C                   exsr      inzvar
021700100318     C                   exsr      normCSV
021800010605     C                   exsr      impvab
021900010601     C*
022000010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022100010605     C                   eval      depfil = VABLNP
022200010601     C                   exsr      repfil
022300010601     C                   if        depfil = invfil
022400021025     C                   if        vlrpoi = 999
022500030822     C                   move(P)   invfil        VABFGS
022600021025     C                   else
022700030822     C                   move(P)   vlrpoi        VABFGS
022800021025     C                   endif
022900010601     C*
023000010601     C                   exsr      PREELA
023100010601     C*
023200010604     C* Ebbene...
023300010604     C*
023400040119     C  N31              add       1             §CTROKVB
023500040119     C   32              add       1             §CTRMO
023600040119     C   31              add       1             §CTRNO
023700080418     C*
023800030822     C                   if        wGiro = 2
023900080513     C* Scrivo sempre il VAT
024000080513     C  N31              exsr      wrivat
024100110201     C                   exsr      chkvab
024200080513     C                   endif
024300010604     C*
024400010604     C                   if        *in31 = *off and
024500010604     C                             *in32 = *off
024600010604     C                   eval      vinflg = '1'
024700010604     C                   else
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000030822     C  N70              enddo
026100040119     C*
026200080527     C* Verifico dati rimasti "in cannna"...
026300080527     C                   seton                                        65
026400110201     C                   exsr      chkvab
026500080527     C*
026600040119     C                   endif                                                  (endif - chkcall)
026700010601     C*
026800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026900010601     C                   if        cntNonEl = *zeros or
027000010601     C                             flgMulti = '0'
027100010330     C* Se non ci sono record con errori ...
027200010601     C                   if        §ctrno = 0 and
027300010604     C                             §ctrmo = 0 and
027400010601     C                             flgStato <> '2'
027500010330     C* ... restituisco esito OK.
027600010330     C                   eval      wrkesito = '0'
027700010330     C                   else
027800040119     C                   if        §ctrokvb > 0
027900010330     C                   eval      wrkesito = '1'
028000010330     C                   else
028100010615     C                   if        flgOk = '0'
028200010615     C                   eval      wrkesito = '2'
028300010615     C                   else
028400010615     C                   eval      wrkesito = '6'
028500010615     C                   endif
028600010330     C                   endif
028700010330     C                   endif
028800010601     C                   else
028900010601     C                   eval      wrkesito = '9'
029000010601     C                   endif
029100010330     C*
029200010330     C                   if        %open(tivin00r)
029300010330     C                   close     tivin00r
029400010330     C                   endif
029500021025     C                   if        %open(fivabwwr)
029600021025     C                   close     fivabwwr
029700010330     C                   endif
029800040512     C                   if        %open(fivatwwr)
029900040512     C                   close     fivatwwr
030000040512     C                   endif
030100010601     C*
030200010601     C                   if        vlrpoi <> 999
030300010601     C                   eval      invfil = vlrpoi
030400010601     C                   endif
030500010330     C*
030600040119     C                   if        §ctrokvb > 0
030700010601     C                             and invfil > *zeros
030800010330     C                   exsr      invio
030900010330     C                   endif
031000010601     C*
031100010618     C                   if        flgGiro = '1'
031200010601     C                   exsr      endela
031300010618     C                   endif
031400010330     C*
031500010330     C                   ENDSR
031600010330     C***
031700080527
031800080527
031900080527
032000080527     C*----------------------------------------------------*
032100080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032200080527     C*----------------------------------------------------*
032300100413     C     CHKVAB        BEGSR
032400110201     C*
032500110201     C* Se richiesti ignorare colli / peso / volume
032600110315 xxx C***                if        *in35
032700110315 xxx C***                z-add     *zeros        VABNCL
032800110315 xxx C***                z-add     *zeros        VABPKB
032900110315 xxx C***                z-add     *zeros        VABVLB
033000110315 xxx C***                endif
033100080527     C*
033200080527     C* Scrivo il VAB solo a rottura d spedizione
033300080527     C                   if        depspe = *zeros
033400080527     C  N31              eval      fivabds_sav = fivabds
033500080527     C                   eval      depspe = vabNSP
033600080527     C                   else
033700080527     C                   if        depspe = vabNSP AND *in65 = *off
033800100413     C   51              add       VABNCL        sav_VABNCL
033900100413     C   51              add       VABPKB        sav_VABPKB
034000100413     C   51              add       VABVLB        sav_VABVLB
034100100720     C   52              z-add     VABNCL        sav_VABNCL
034200100720     C   52              z-add     VABPKB        sav_VABPKB
034300100720     C   52              z-add     VABVLB        sav_VABVLB
034400100720     C   53              add       VABNCL        sav_VABNCL
034500100720     C   53              z-add     VABPKB        sav_VABPKB
034600100720     C   53              z-add     VABVLB        sav_VABVLB
034700080527     C                   else
034800080527     C  N31              eval      fivabds_ok = fivabds_sav
034900080527     C  N31              exsr      wrivab
035000080527     C  N31              eval      fivabds_sav = fivabds
035100080527     C                   eval      depspe = vabNSP
035200080527     C                   endif
035300080527     C                   endif
035400080527     C*
035500080527     C                   ENDSR
035600040119
035700040119
035800040119
035900040119     C*----------------------------------------------------*
036000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036100040119     C*----------------------------------------------------*
036200040510     C     WRIVAB        BEGSR
036300080513     C*
036400080513     C* Gestisco l'eventuale rottura x numero spedizione
036500080527     C                   if        VABNSP <> depspe or *in65 = *on
036600070301     C                   if        wVATNOT_E <> *blanks
036700070301     C                   eval      VABCTM = '7Q'
036800120731     C                   eval      ok_VABCTM = '7Q'
036900070301     C                   endif
037000100413     C                   write     fivab000
037100080513     C                   endif
037200040119     C*
037300040119     C                   ENDSR
037400040119
037500040119
037600040119
037700040119     C*----------------------------------------------------*
037800040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037900040119     C*----------------------------------------------------*
038000040510     C     WRIVAT        BEGSR
038100060512     C*
038200060512     C* Inizializzo i campi chiave
038300060512     C                   eval      VATFGS = VABFGS
038400060512     C                   eval      VATAAS = VABAAS
038500060512     C                   eval      VATCCM = VABCCM
038600070823     C                   eval      VATNRS = VABNRS
038700060512     C                   eval      VATNSP = VABNSP
038800060512     C                   eval      VATLNP = VABLNP
038900040714     C*
039000040714     C* Solo se VATNOT valorizzato scarico il buffer
039100040714     C                   if        VATNOT <> *blanks
039200040119     C*
039300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039400040510     C                   if        VATTRC = *blanks
039500040510     C                   eval      VATTRC = 'E'
039600040510     C                   endif
039700040510     C                   write     FIVAT000
039800040119     C                   add       1             §CTROKVT
039900040714     C*
040000040714     C                   endif
040100060331     C*
040200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040300060331     C                   if        wVATNOT_A <> *blanks
040400060331     C                   eval      VATNOT = wVATNOT_A
040500060331     C                   eval      VATTRC = 'A'
040600060331     C                   write     FIVAT000
040700060331     C                   add       1             §CTROKVT
040800060331     C                   endif
040900060331     C*
041000060331     C                   if        wVATNOT_B <> *blanks
041100060331     C                   eval      VATNOT = wVATNOT_B
041200060331     C                   eval      VATTRC = 'B'
041300060331     C                   write     FIVAT000
041400060331     C                   add       1             §CTROKVT
041500060331     C                   endif
041600060331     C*
041700060331     C                   if        wVATNOT_E <> *blanks
041800110201     C                   eval      VATNOT = wVATNOT_E
041900060331     C                   eval      VATTRC = 'E'
042000110201     C                   write     FIVAT000
042100110201     C                   add       1             §CTROKVT
042200110201     C                   endif
042300160201     C*
042400160201     C                   if        wVATNOT_IJ <> *blanks
042500160201     C                   eval      w70    = wVATNOT_IJ
042600160201     C                   eval      VATNOT = %subst(w70:1:35)
042700160201     C                   eval      VATTRC = 'I'
042800160201     C                   write     FIVAT000
042900160201     C                   add       1             §CTROKVT
043000160201     C                   if        %subst(w70:36:35) <> *blanks
043100160201     C                   eval      VATNOT = %subst(w70:36:35)
043200160201     C                   eval      VATTRC = 'J'
043300160201     C                   write     FIVAT000
043400160201     C                   add       1             §CTROKVT
043500160201     C                   endif
043600160201     C                   endif
043700040119     C*
043800040119     C                   ENDSR
043900040119
044000040119
044100040119
044200040119     C*----------------------------------------------------*
044300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044400040119     C*----------------------------------------------------*
044500040119     C     PREVAT        BEGSR
044600040119     C*
044700040119     C* Compongo il nome del membro da dare al FIVATWWR
044800040119     C                   eval      parmbr = vlrhdl
044900040119     C                   movel     'M'           parmbr
045000040510     C                   eval      parccm = vlrksc
045100040119     C                   eval      paropz = '1'
045200040119     C* Effettuo la chiamata al CLLE preposto
045300040119     C                   call(e)   'TITVVTC'
045400040119     C                   parm                    parccm
045500040119     C                   parm                    parmbr
045600040119     C                   parm                    paropz
045700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045800040119     C                   if        %error
045900040119     C                   movel     '1'           chkcall
046000040119     C                   else
046100040119     C                   movel     '0'           chkcall
046200040119     C                   endif
046300040119     C*
046400040119     C                   ENDSR
046500010601
046600010601
046700010601
046800010330     C*----------------------------------------------------*
046900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047000010330     C*----------------------------------------------------*
047100040119     C     INZVAR        BEGSR
047200080527     C*
047300080527     C                   setoff                                       65
047400010330     C*
047500030822     C* Inizializzo variabili di wrk
047600020204     C                   Z-ADD     *zeros        Num5_0            5 0
047700101018     C                   MOVEL     *blanks       wFlgCAS
047800060331     C                   MOVEL     *blanks       wVATNOT_A
047900060331     C                   MOVEL     *blanks       wVATNOT_B
048000060331     C                   MOVEL     *blanks       wVATNOT_E
048100160201     C                   MOVEL     *blanks       wVATNOT_IJ
048200100831     C                   MOVEL     *blanks       wVABRSD
048300100831     C                   MOVEL     *blanks       wVABIND
048400100831     C                   MOVEL     *blanks       wVABCAD
048500100831     C                   MOVEL     *blanks       wVABLOD
048600100831     C                   MOVEL     *blanks       wVABPRD
048700030822     C*
048800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048900030822     C                   CLEAR                   SkSplitCSV
049000030822     C                   EVAL      i = 1
049100030822     C                   EVAL      posDa = *zeros
049200030822     C                   EVAL      posA  = *zeros
049300080527     C*
049400080527     C                   CLEAR                   FIVABDS
049500080527     C                   CLEAR                   FIVAT000
049600030822     C*
049700030822     C* Reimposto i valori di default
049800030822     C                   EXSR      DEFCAM
049900030822     C*
050000010330     C                   ENDSR
050100010330     C*----------------------------------------------------*
050200030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050300010330     C*----------------------------------------------------*
050400010330     C     DEFCAM        BEGSR
050500030822     C*
050600030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050700030715     C* e delimitatore testo.
050800030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050900030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051000030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051100100318     C*
051200100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051300100318     C                   EVAL      CharSOS = CharNUM
051400030822     C*
051500030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051600100720     C                   SETOFF                                       505152
051700100720     C                   SETOFF                                       53
051800100413     C                   SELECT
051900100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052000100720     C                   SETON                                        50
052100100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052200100413     C                   SETON                                        51
052300100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052400100720     C                   SETON                                        52
052500100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052600100720     C                   SETON                                        53
052700100413     C                   ENDSL
052800030822     C*
052900030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053000030822     C                   EVAL      posDaDft = 1
053100030822     C                   EVAL      posADft  = 0
053200030822     C                   EVAL      wGiroDft = 0
053300030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053400030822     C                             posDaDft > 0
053500030822     C*
053600030822     C* Gestisco il 1° giro
053700030822     C                   IF        wGiroDft = 0
053800030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053900030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054000030822     C* Incremento il contatore dei "giri"
054100030822     C                   EVAL      wGiroDft = 1
054200030822     C                   ELSE
054300030822     C                   EVAL      posDaDft = posADft
054400030822     C                   ENDIF
054500030822     C* Eseguo lo scan x trovare la fine del campo corrente
054600030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054700030822     C*
054800030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054900030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055000030822     C                   IF        posDaDft > 0 AND
055100030822     C                             posADft  > 0
055200030822     C* NCL
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'NCL'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiInt=*on
056200030822     C                   Z-ADD     PiVal         VABNCL
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* CCM
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CCM'
057000030822     C                   EVAL      PiStr=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   EXSR      CHKNUM
057400030822     C                   IF        PiInt=*on
057500030822     C                   Z-ADD     PiVal         VABCCM
057600030822     C                   ENDIF
057700030822     C                   ENDIF
057800030822     C* LNP
057900030822     C                   IF        %subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):1:3)
058200030822     C                             = 'LNP'
058300030822     C                   EVAL      PiStr=%trim(%subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058600030822     C                   EXSR      CHKNUM
058700030822     C                   IF        PiInt=*on
058800030822     C                   Z-ADD     PiVal         VABLNP
058900030822     C                   ENDIF
059000030822     C                   ENDIF
059100030822     C* NRS
059200030822     C                   IF        %subst(
059300030822     C                             %subst(vlrppt:posDaDft+1:
059400030822     C                             posADft-posDaDft-1):1:3)
059500030822     C                             = 'NRS'
059600030822     C                   EVAL      PiStr=%trim(%subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059900030822     C                   EXSR      CHKNUM
060000030822     C                   IF        PiInt=*on
060100030822     C                   Z-ADD     PiVal         VABNRS
060200030822     C                   ENDIF
060300030822     C                   ENDIF
060400030822     C* CTR
060500030822     C                   IF        %subst(
060600030822     C                             %subst(vlrppt:posDaDft+1:
060700030822     C                             posADft-posDaDft-1):1:3)
060800030822     C                             = 'CTR'
060900030822     C                   EVAL      PiStr=%trim(%subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200030822     C                   EXSR      CHKNUM
061300030822     C                   IF        PiInt=*on
061400030822     C                   Z-ADD     PiVal         VABCTR
061500030822     C                   ENDIF
061600030822     C                   ENDIF
061700030822     C* PKB
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'PKB'
062200030822     C                   EVAL      PiStr=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   EXSR      CHKNUM
062600030822     C                   IF        PiNum=*on
062700030822     C                   Z-ADD     PiVal         VABPKB
062800030822     C                   ENDIF
062900030822     C                   ENDIF
063000030822     C* VLB
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VLB'
063500030822     C                   EVAL      PiStr=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   EXSR      CHKNUM
063900030822     C                   IF        PiNum=*on
064000030822     C                   Z-ADD     PiVal         VABVLB
064100030822     C                   ENDIF
064200030822     C                   ENDIF
064300030822     C* QFT
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'QFT'
064800030822     C                   EVAL      PiStr=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   EXSR      CHKNUM
065200030822     C                   IF        PiNum=*on
065300030822     C                   Z-ADD     PiVal         VABQFT
065400030822     C                   ENDIF
065500030822     C                   ENDIF
065600030822     C* CBO
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'CBO'
066100030822     C                   EVAL      VABCBO=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* TSP
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'TSP'
067000030822     C                   EVAL      VABTSP=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* VAS
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VAS'
067900030822     C                   EVAL      VABVAS=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* VCA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'VCA'
068800030822     C                   EVAL      VABVCA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200070925     C* TIC
069300070925     C                   IF        %subst(
069400070925     C                             %subst(vlrppt:posDaDft+1:
069500070925     C                             posADft-posDaDft-1):1:3)
069600070925     C                             = 'TIC'
069700070925     C                   EVAL      VABTIC=%trim(%subst(
069800070925     C                             %subst(vlrppt:posDaDft+1:
069900070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000070925     C                   ENDIF
070100030822     C* GCA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'GCA'
070600030822     C                   EVAL      VABGCA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* CTM
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'CTM'
071500030822     C                   EVAL      VABCTM=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* FFD
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'FFD'
072400030822     C                   EVAL      VABFFD=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* VAD
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'VAD'
073300030822     C                   EVAL      VABVAD=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* GMA
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'GMA'
074200030822     C                   EVAL      VABGMA=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600030822     C* GGA
074700030822     C                   IF        %subst(
074800030822     C                             %subst(vlrppt:posDaDft+1:
074900030822     C                             posADft-posDaDft-1):1:3)
075000030822     C                             = 'GGA'
075100030822     C                   EVAL      VABGGA=%trim(%subst(
075200030822     C                             %subst(vlrppt:posDaDft+1:
075300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400030822     C                   ENDIF
075500030822     C* GVA
075600030822     C                   IF        %subst(
075700030822     C                             %subst(vlrppt:posDaDft+1:
075800030822     C                             posADft-posDaDft-1):1:3)
075900030822     C                             = 'GVA'
076000030822     C                   EVAL      VABGVA=%trim(%subst(
076100030822     C                             %subst(vlrppt:posDaDft+1:
076200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300030822     C                   ENDIF
076400030822     C* TC1
076500030822     C                   IF        %subst(
076600030822     C                             %subst(vlrppt:posDaDft+1:
076700030822     C                             posADft-posDaDft-1):1:3)
076800030822     C                             = 'TC1'
076900030822     C                   EVAL      VABTC1=%trim(%subst(
077000030822     C                             %subst(vlrppt:posDaDft+1:
077100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077200030822     C                   ENDIF
077300030822     C* TC2
077400030822     C                   IF        %subst(
077500030822     C                             %subst(vlrppt:posDaDft+1:
077600030822     C                             posADft-posDaDft-1):1:3)
077700030822     C                             = 'TC2'
077800030822     C                   EVAL      VABTC2=%trim(%subst(
077900030822     C                             %subst(vlrppt:posDaDft+1:
078000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078100030822     C                   ENDIF
078200040714     C* VATTRC
078300040714     C                   IF        %subst(
078400040714     C                             %subst(vlrppt:posDaDft+1:
078500040714     C                             posADft-posDaDft-1):1:3)
078600040714     C                             = 'TRC'
078700040714     C                   EVAL      VATTRC=%trim(%subst(
078800040714     C                             %subst(vlrppt:posDaDft+1:
078900040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079000040714     C                   ENDIF
079100030822     C* ...
079200030822     C                   ENDIF
079300030822     C                   ENDDO
079400020204     C*
079500010330     C                   ENDSR
079600010607     C*----------------------------------------------------*
079700021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079800010607     C*----------------------------------------------------*
079900010607     C     IMPVAB        BEGSR
080000010607     C*
080100010607     C                   SETOFF                                       3132
080200110315 xxx C***                SETON                                        35
080300030515     C*
080400010607     C                   MOVEL     datcor        VABAAS
080500010607     C                   MOVE      datcor        VABMGS
080600010607     C*
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080900030715     C**********
081000030715     C                   IF        wGiro = *zeros
081100040113     C*
081200040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081300040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081400080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081500080429     C                             %trim(intcol(5))<>*blanks
081600040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081700080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081800080429     C                                      %trim(intcol(5))
081900070823     C     ';':CharCSV   XLATE     vindta        vindta
082000040510     C                   ENDIF
082100031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082200031201     C     minu:maiu     XLATE     vindta        vindta
082300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082400030715     C                   DOW       posDa <= %len(%trim(vindta))
082500030715     C*
082600030715     C* Gestisco il 1° campo
082700030715     C                   IF        i = 1
082800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082900030715     C                   EVAL      posDa = 1
083000030715     C* Eseguo lo scan x trovare la fine del primo campo
083100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C* X i campi successivi al 1°
083600030715     C                   ELSE
083700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083800030715     C                   EVAL      posDa = posA + 1
083900030715     C* Eseguo lo scan x trovare la fine del campo corrente
084000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084100030715     C* Gestisco l'ultimo campo
084200030715     C                   IF        posA = *zeros
084300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   LEAVE
084800030715     C                   ELSE
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C                   ENDIF
085300030715     C                   ENDIF
085400030715     C* Incremento il contatore d campo
085500030715     C                   EVAL      i = i +1
085600030715     C                   ENDDO
085700030715     C*
085800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085900030715     C* delimitazione testo
086000030715     C                   EVAL      i = 1
086100030715     C                   DOW       i < %elem(SkSplitFLD)
086200030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086300030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086400030715     C                   EVAL      i = i + 1
086500030715     C                   ENDDO
086600030715     C                   EVAL      wGiro = 1
086700030715     C                   ELSE
086800030715     C                   EVAL      wGiro = 2
086900031204     C**********
087000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087100031204     C* d separatore campo
087200031204     C**********
087300031204     C                   EVAL      vindta = %trim(vindta)
087400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087500031204     C                   EVAL      vindta = %subst(vindta:2)
087600031204     C                   ENDDO
087700031204     C*
087800031204     C                   Z-ADD     *zeros        lunghInput        4 0
087900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088300031204     C                   ENDDO
088400030715     C**********
088500030715     C* Effettuo lo split del campo dati d input
088600030715     C**********
088700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088800030715     C                   DOW       posDa <= %len(%trim(vindta))
088900030715     C*
089000030715     C* Gestisco il 1° campo
089100030715     C                   IF        i = 1
089200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089300030715     C                   EVAL      posDa = 1
089400030715     C* Eseguo lo scan x trovare la fine del primo campo
089500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089600030715     C* A questo punto "estraggo" il campo corrente
089700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089800030715     C                                                    (posA-posDa))
089900030715     C* X i campi successivi al 1°
090000030715     C                   ELSE
090100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090200030715     C                   EVAL      posDa = posA + 1
090300031204     C* Verifico che nn vi sia il campo nn valorizzato
090400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090500031204     C* Se campo nn valorizzato skippo al prossimo
090600031204     C                   EVAL      SkSplitCSV(i) = *blanks
090700031204     C                   EVAL      posA  = posA + 1
090800031204     C                   ELSE
090900030715     C* Eseguo lo scan x trovare la fine del campo corrente
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* Gestisco l'ultimo campo
091200030715     C                   IF        posA = *zeros
091300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   LEAVE
091800030715     C                   ELSE
091900030715     C* A questo punto "estraggo" il campo corrente
092000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092100030715     C                                                    (posA-posDa))
092200030715     C                   ENDIF
092300031204     C                   ENDIF
092400030715     C                   ENDIF
092500030715     C* Incremento il contatore d campo
092600030715     C                   EVAL      i = i +1
092700030715     C                   ENDDO
092800030715     C*
092900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093000030715     C* delimitazione testo
093100030715     C                   EVAL      i = 1
093200030715     C                   DOW       i < %elem(SkSplitCSV)
093300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093500030715     C                   EVAL      i = i + 1
093600030715     C                   ENDDO
093700100720     C*
093800100720     C* Stacco un progressivo x numero spedizione
093900100720 xxx C***                EXSR      repNSP
094000100326     C*
094100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094200030715     C                   EVAL      i = 1
094300030715     C                   DOW       i < %elem(SkSplitFLD)
094400030715     C*
094500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094700030715     C                             SkSplitCSV(i) <> *blanks
094800030721     C*** ATB
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095000030721     C                   EVAL      VABATB = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030715     C*** RSD
095300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095700030715     C* ==
095800030715     C                   ENDIF
095900030721     C*** RD2
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030715     C*** IND
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096500030715     C                   EVAL      VABIND = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030715     C*** LOD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** PRD
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030721     C*** NZD
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030715     C*** RMA
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** NOT
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** NT2
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** VCA
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030715     C*** VAS
099600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099800030715     C                   ENDIF
099900030715     C*** FFD
100000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100200030715     C                   ENDIF
100300030721     C*** GC1
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** GC2
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** TSP
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040510     C*** CBO
101600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101800040510     C                   ENDIF
101900030721     C*** NAS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TIC
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GCA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** XCO
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TCR
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** CTS
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** FTM
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** VAD
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GMA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GGA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GVA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** SCL
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** RMO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** NMO
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*
108800030721     C* Reperisco quindi i campi numerici...
108900040510     C*** NSP
109000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109100040510     C                   EVAL      PiStr=SkSplitCSV(i)
109200040510     C                   EXSR      CHKNUM
109300040510     C                   IF        PiInt=*on
109400040510     C                   Z-ADD     PiVal         VABNSP
109500040510     C                   ELSE
109600040510     C                   SETON                                        31
109700040510     C                   EVAL      VABNSP = *zeros
109800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040510     C                             + ' ' + 'VABNSP'
110000040510     C                   ENDIF
110100040510     C                   ENDIF
110200071106     C*** RMN
110300071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110400071106     C                   EVAL      PiStr=SkSplitCSV(i)
110500071106     C                   EXSR      CHKNUM
110600071106     C                   IF        PiInt=*on
110700071106     C                   Z-ADD     PiVal         VABRMN
110800071106     C                   ELSE
110900071106     C                   SETON                                        32
111000071106     C                   EVAL      VABRMN = *zeros
111100071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111200071106     C                             + ' ' + 'VABRMN'
111300071106     C                   ENDIF
111400071106     C                   ENDIF
111500030721     C*** CAD
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111700030721     C                   EVAL      PiStr=SkSplitCSV(i)
111800030721     C                   EXSR      CHKNUM
111900030721     C                   IF        PiInt=*on
112000030721     C                   Z-ADD     PiVal         Num5_0
112100030721     C                   MOVEL(p)  Num5_0        VABCAD
112200030721     C                   ELSE
112300030721     C                   SETON                                        32
112400030721     C                   EVAL      VABCAD = *zeros
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABCAD'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** NCL
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on
113400030721     C                   Z-ADD     PiVal         VABNCL
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      VABNCL = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900040122     C                             + ' ' + 'VABNCL'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** PKB
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiNum=*on
114700030721     C                   Z-ADD     PiVal         VABPKB
114800030721     C                   ELSE
114900030721     C                   SETON                                        32
115000030721     C                   EVAL      VABPKB = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABPKB'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** CAS
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115700141202     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115800141202     C                             %trim(SkSplitCSV(i)) <> *zeros
115900101018     C                   EVAL      wFlgCAS = '1'
116000101018     C                   ENDIF
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABCAS
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABCAS = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABCAS'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CCM
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on      AND
117700030721     C                             PiVal<=9999999 AND
117800030721     C                             PiVal>*zeros
117900030721     C                   Z-ADD     PiVal         VABCCM
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      VABCCM = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040510     C                             + ' ' + 'VABCCM'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** LNP
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on    AND
119200030721     C                             PiVal<=999   AND
119300030721     C                             PiVal>*zeros
119400030721     C                   Z-ADD     PiVal         VABLNP
119500030721     C                   ELSE
119600040715     C                   SETON                                        32
119700030721     C                   EVAL      VABLNP = *zeros
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900040510     C                             + ' ' + 'VABLNP'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NRS
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on AND
120700030721     C                             PiVal<=99
120800030721     C                   Z-ADD     PiVal         VABNRS
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNRS'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** LNA
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on    AND
122000030721     C                             PiVal<=999   AND
122100030721     C                             PiVal>*zeros
122200030721     C                   Z-ADD     PiVal         VABLNA
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      VABLNA = *zeros
122600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122700030721     C                             + ' ' + 'VABLNA'
122800030721     C                   ENDIF
122900030721     C                   ENDIF
123000030721     C*** CTR
123100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123200030721     C                   EVAL      PiStr=SkSplitCSV(i)
123300030721     C                   EXSR      CHKNUM
123400030721     C                   IF        PiInt=*on  AND
123500030721     C                             PiVal<=999
123600030721     C                   Z-ADD     PiVal         VABCTR
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABCTR'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** IAS
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABIAS
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABIAS'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** VLB
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABVLB
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABVLB'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** QFT
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiNum=*on
127200030721     C                   Z-ADD     PiVal         VABQFT
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABQFT'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** NCD
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on      AND
128400030721     C                             PiVal<=9999999
128500030721     C                   Z-ADD     PiVal         VABNCD
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABNCD'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** NCA
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on      AND
129700030721     C                             PiVal<=9999999
129800030721     C                   Z-ADD     PiVal         VABNCA
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABNCA'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** ZNC
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on AND
131000030721     C                             PiVal<=99
131100030721     C                   Z-ADD     PiVal         VABZNC
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABZNC'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** DCR
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiInt=*on       AND
132300030721     C                             PiVal<=99999999
132400030721     C                   Z-ADD     PiVal         VABDCR
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132800030721     C                             + ' ' + 'VABDCR'
132900030721     C                   ENDIF
133000030721     C                   ENDIF
133100030721     C*** HCR
133200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133300030721     C                   EVAL      PiStr=SkSplitCSV(i)
133400030721     C                   EXSR      CHKNUM
133500030721     C                   IF        PiInt=*on   AND
133600030721     C                             PiVal<=9999
133700030721     C                   Z-ADD     PiVal         VABHCR
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABHCR'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** VMD
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiNum=*on
134900030721     C                   Z-ADD     PiVal         VABVMD
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135300030721     C                             + ' ' + 'VABVMD'
135400030721     C                   ENDIF
135500030721     C                   ENDIF
135600030721     C*** ANT
135700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
135800030721     C                   EVAL      PiStr=SkSplitCSV(i)
135900030721     C                   EXSR      CHKNUM
136000030721     C                   IF        PiInt=*on        AND
136100030721     C                             PiVal<=999999999
136200030721     C                   Z-ADD     PiVal         VABANT
136300030721     C                   ELSE
136400030721     C                   SETON                                        32
136500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136600030721     C                             + ' ' + 'VABANT'
136700030721     C                   ENDIF
136800030721     C                   ENDIF
136900030721     C*** CMO
137000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137100030721     C                   EVAL      PiStr=SkSplitCSV(i)
137200030721     C                   EXSR      CHKNUM
137300030721     C                   IF        PiInt=*on
137400030721     C                   Z-ADD     PiVal         Num5_0
137500030721     C                   MOVEL(p)  Num5_0        VABCMO
137600030721     C                   ELSE
137700030721     C                   SETON                                        32
137800030721     C                   EVAL      VABCMO = *zeros
137900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138000030721     C                             + ' ' + 'VABCMO'
138100030721     C                   ENDIF
138200030721     C                   ENDIF
138300040510     C*
138400040510     C* Reperisco i campi del FIVAT
138500040510     C*** VATNOT
138600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
138800040510     C                   ENDIF
138900040510     C*** VATTRC
139000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139200040510     C                   ENDIF
139300060331     C*** VATNOT_A
139400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139600060331     C                   ENDIF
139700060331     C*** VATNOT_B
139800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
139900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140000060331     C                   ENDIF
140100060331     C*** VATNOT_E
140200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140400060331     C                   ENDIF
140500160201     C*** VATNOT_IJ
140600160201     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_IJ'
140700160201     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
140800160201     C                   ENDIF
140900070823     C*
141000100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141100110131     C*** NSP / RMN
141200101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141300110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
141400101021     C                   EXSR      CHKNUM
141500101021     C                   IF        PiInt=*on
141600101021     C                   Z-ADD     PiVal         VABNSP
141700101021     C                   Z-ADD     PiVal         VABRMN
141800101021     C                   ELSE
141900101021     C                   SETON                                        31
142000101021     C                   EVAL      VABNSP = *zeros
142100101021     C                   EVAL      VABRMN = *zeros
142200101021     C                   EVAL      vinmsg = %trimr(vinmsg)
142300101021     C                             + ' ' + 'VABNSP VABRMN'
142400101021     C                   ENDIF
142500101021     C                   ENDIF
142600151028     C*
142700151028     C*** TIC / CBO
142800151028     C                   IF        wFlgCAS = '1'    OR
142900151028     C                             VABCAS  > *zeros
143000151028     C                   EVAL      VABTIC = 'BM'
143100151028     C                   IF        VABCBO = '1'
143200151028     C                   EVAL      VABCBO = '4'
143300151028     C                   ENDIF
143400151028     C                   IF        VABCBO = '2'
143500151028     C                   EVAL      VABCBO = '6'
143600151028     C                   ENDIF
143700151028     C                   ENDIF
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
146600070925     C*
146700071029     C***  <===  -----------------------------------------
146800070925     C*
146900070925     C* Considerazioni finali su CBO/CAS
147000100318     C                   IF        wFlgCAS = '1'    OR
147100100318     C                             VABCAS  > *zeros
147200070925     C                   IF        VABCBO = '1'
147300070925     C                   EVAL      VABCBO = '4'
147400070925     C                   ENDIF
147500070925     C                   IF        VABCBO = '2'
147600070925     C                   EVAL      VABCBO = '6'
147700070925     C                   ENDIF
147800070925     C                   ENDIF
147900070925     C***  <===  ----------------------------
148000040510     C*
148100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148200040510     C                   IF        VABNSP = *zeros
148300040510     C                   SETON                                        31
148400040510     C                   ENDIF
148500040715     C*
148600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148700040715     C                   IF        VABLNP = *zeros
148800040715     C                   SETON                                        31
148900040715     C                   ENDIF
149000040510     C*
149100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149200040510     C                   IF        VABRMN = *zeros
149300040510     C                   Z-ADD     VABNSP        VABRMN
149400040510     C                   ENDIF
149500040510     C*
149600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149700040510     C                   IF        VABRMA = *blanks
149800040714     C                   MOVEL     VABRMN        VABRMA
149900040510     C                   ENDIF
150000040128     C*
150100040510     C* Se provincia nn valorizzata la reperisco
150200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150300040510     C                   IF        VABPRD = *blanks
150400040128     C                   CLEAR                   TISI95DS
150500040128     C                   EVAL      I95TCN = '3'
150600040128     C                   Z-ADD     datcor        I95DAT
150700040128     C                   EVAL      I95NAR = VABNZD
150800040128     C                   EVAL      I95CAP = VABCAD
150900040128     C                   EVAL      I95LOC = VABLOD
151000040128     C                   CALL      'TISI95R'
151100040128     C                   PARM                    TISI95DS
151200040128     C                   EVAL      VABPRD = O95PRV
151300040510     C                   ENDIF
151400020204     C*
151500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151600020204     C                   EXSR      CHKIMPDIV
151700030715     C                   ENDIF
151800020204     C*
151900010607     C                   ENDSR
152000010607     C*----------------------------------------------------*
152100010601
152200020204
152300020204     C*----------------------------------------------------*
152400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152500020204     C*----------------------------------------------------*
152600020204     C     CHKIMPDIV     BEGSR
152700020204     C*
152800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152900020204     C                   Z-ADD     *zeros        wrkDec            9 9
153000020204     C*
153100020204     C* Come prima cosa effettuo considerazioni sulla divisa
153200020204     C                   IF        vabIAS > *zeros
153300020204     C                   IF        vabVAS <> 'EUR'
153400020204     C                   EVAL      vabVAS =  'ITL'
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C                   IF        vabCAS > *zeros
153900141202     C                   EVAL      vabVCA = 'EUR'
154000141202     C                   EVAL      vabTIC = *blank
154100020204     C                   IF        vabVCA <> 'EUR'
154200020204     C                   EVAL      vabVCA =  'ITL'
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C                   IF        vabVMD > *zeros
154700020204     C                   IF        vabVAD <> 'EUR'
154800020204     C                   EVAL      vabVAD =  'ITL'
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155300020204     C                   Z-ADD     vabIAS        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVAS = 'ITL'
155600020204     C                   EVAL      vabIAS = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156100020204     C                   Z-ADD     vabCAS        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVCA = 'ITL'
156400020204     C                   EVAL      vabCAS = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156900020204     C                   Z-ADD     vabVMD        wrkDec
157000020204     C                   IF        wrkDec > *zeros
157100020204     C                   IF        vabVAD = 'ITL'
157200020204     C                   EVAL      vabVMD = *zeros
157300020204     C                   ENDIF
157400020204     C                   ENDIF
157500020204     C*
157600020204     C                   ENDSR
157700020204     C***
157800010330
157900100318
158000100318
158100100318     C*----------------------------------------------------*
158200100318     C*  CONTROLLO NUMERICITA' CAMPI
158300100318     C*----------------------------------------------------*
158400100318     C     CHKNUM        BEGSR
158500100318     C*
158600100318     C                   IF        PiDecChr = *blanks
158700100318     C                   EVAL      PiDecChr = CharNUM
158800100318     C                   ENDIF
158900100318     C*
159000100318     C                   callp     UBISNUM_Check(PiStr
159100100318     C                                          :PiDecChr
159200100318     C                                          :PiVal
159300100318     C                                          :PiNum
159400100318     C                                          :PiInt)
159500100318     C*
159600100318     C                   ENDSR
159700100318     C***
159800010601
159900010601
160000010601
160100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160200010601     C     repfil        BEGSR
160300010601     C*
160400010601     C                   if        invfil = *zeros and
160500010601     C                             depfil > *zeros and
160600010629     C                             (vinflg = *blanks or
160700010629     C                              vinflg = *zeros)
160800010601     C
160900010601     C                   eval      invfil = depfil
161000010601     C                   endif
161100010601     C*
161200010601     C                   if        depfil <> invfil and
161300010601     C                             invfil > *zeros
161400010601     C                   eval      flgMulti = '1'
161500010601     C                   if        vinflg = *blanks
161600010601     C                   add       1             cntNonEl
161700010601     C                   endif
161800010601     C                   endif
161900010601     C*
162000010601     C                   if        vinflg = '2'
162100010601     C                   eval      flgStato = '2'
162200010601     C                   endif
162300010601     C*
162400010601     C                   ENDSR
162500010601     C***
162600010601
162700010601
162800010601
162900010330
163000040119      /TITLE Invio dei dati al punto operativo.
163100040119     C     invio         BEGSR
163200040119     C*
163300040119     C* 1° invio FIVAT
163400040119     C                   reset                   dscmz
163500040119     C                   move      vlrpoi        cmzdst
163600040119     C                   eval      cmzfld = 'FIVATWWR'
163700040119     C                   eval      cmzmbd = vlrhdl
163800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163900040119     C***                if        prmfir = *blanks
164000040119     C                   eval      cmzfla = 'FIVAT00F'
164100040119     C                   eval      cmzmba = 'FIVAT00F'
164200040119     C***                else
164300040119     C***                eval      cmzfla = prmfir
164400040119     C***                eval      cmzmba = prmfir
164500040119     C***                endif
164600040119     C                   eval      cmznrr = *zeros
164700040119     C                   move      §ctrokvt      cmznrr
164800040119     C                   eval      cmzlba = vlrfl1
164900040119     C                   call(e)   'TIS711C'
165000040119     C                   parm                    dscmz
165100040119     C                   parm      *blanks       esito
165200040119     C                   if        %error
165300040119     C                             or cmzerr = '1'
165400040119     C                             or esito  = '1'
165500040119     C                   eval      wrkesito = '3'
165600040119     C                   else
165700040119     C*
165800040119     C* 2° invio FIVAB
165900040119     C                   reset                   dscmz
166000040119     C                   move      vlrpoi        cmzdst
166100040119     C                   eval      cmzfld = vlrfou
166200040119     C                   eval      cmzmbd = vlrhdl
166300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166400040119     C***                if        prmfir = *blanks
166500040119     C                   eval      cmzfla = 'FIVAB00F'
166600040119     C                   eval      cmzmba = 'FIVAB00F'
166700040119     C***                else
166800040119     C***                eval      cmzfla = prmfir
166900040119     C***                eval      cmzmba = prmfir
167000040119     C***                endif
167100040119     C                   eval      cmznrr = *zeros
167200040119     C                   move      §ctrokvb      cmznrr
167300040119     C                   eval      cmzlba = vlrfl1
167400040119     C                   call(e)   'TIS711C'
167500040119     C                   parm                    dscmz
167600040119     C                   parm      *blanks       esito
167700040119     C                   if        %error
167800040119     C                             or cmzerr = '1'
167900040119     C                             or esito  = '1'
168000040119     C                   eval      wrkesito = '3'
168100040119     C                   endif
168200040119     C                   endif
168300040119     C*
168400040119     C                   ENDSR
168500040119     C***
168600010601
168700010601
168800010601
168900010601
169000010601      /TITLE Invio dei dati al punto operativo.
169100010601     C     opeini        BEGSR
169200010601     C*
169300010601     C* Inizializzo flag e contatori operativi
169400010601     C                   movel     '0'           flgGiro           1
169500010601     C                   movel     '0'           flgMulti          1
169600010601     C                   movel     '1'           flgStato          1
169700010615     C                   movel     '0'           flgOk             1
169800010601     C                   z-add     *zeros        cntNonEl         10 0
169900010601     C                   z-add     *zeros        depfil            3 0
170000010601     C                   z-add     *zeros        invfil            3 0
170100010601     C*
170200010601     C                   ENDSR
170300010601     C***
170400070326
170500070326
170600070326
170700070326
170800070326     C     *pssr         BEGSR
170900070329     C*
171000070329     C                   if        %open(tivin00r)
171100070329     C                   close     tivin00r
171200070329     C                   endif
171300070329     C                   if        %open(fivabwwr)
171400070329     C                   close     fivabwwr
171500070329     C                   endif
171600070329     C                   if        %open(fivatwwr)
171700070329     C                   close     fivatwwr
171800070329     C                   endif
171900070326     C*
172000070326     C* Effettuo la chiamata al CLLE preposto
172100070326     C                   call(e)   'TITVVTC'
172200070326     C                   parm                    parccm
172300070326     C                   parm                    parmbr
172400070326     C                   parm      '2'           paropz
172500070326     C*
172600070326     C                   eval      wrkesito = '2'
172700070404     C*
172800070404     C                   seton                                        LR
172900070326     C*
173000070326     C                   ENDSR     '*CANCL'
173100070326     C***
173200100318
173300100318
173400100318
173500100318     C     normCSV       BEGSR
173600100318     C*
173700100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173800100318     C* separatori campi
173900100318     C                   if        CharTXT <> *blanks
174000100318     C                   z-add     *zeros        wLenVINDTA        4 0
174100100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174200100318     C                   setoff                                       80
174300100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174400100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174600100318     C                   if        not *in80
174700100318     C                   seton                                        80
174800100318     C                   else
174900100318     C                   setoff                                       80
175000100318     C                   endif
175100100318     C                   endif
175200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175300100318     C                             and *in80 = *on
175400100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175500100318     C                   endif
175600100318     C                   enddo
175700100318     C                   endif
175800100318     C*
175900100318     C                   ENDSR
176000100318     C***
176100070326
176200100326
176300100326
176400100326     C*----------------------------------------------------*
176500100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176600100326     C*----------------------------------------------------*
176700100326     C     REPNSP        BEGSR
176800100326     C*
176900100326     C* NSP => Stacco un numeratore da AZNUM
177000100326     C                   clear                   TRUL33DS
177100100326     C                   eval      I33OPE = *zeros
177200100326     C                   eval      I33CNU = 302
177300100326     C                   eval      I33NUM = 1
177400100326     C                   movel     TRUL33DS      KPJBU
177500100326     C                   call      'TRUL33R'
177600100326     C                   parm                    KPJBA
177700100326     C                   movel     KPJBU         TRUL33DS
177800100326     C                   if        O33ERR = *zeros
177900100326     C                   z-add     O33NRF        VABNSP
178000100326     C                   z-add     O33NRF        VATNSP
178100100326     C                   else
178200100326     C                   SETON                                        31
178300100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178400100326     C                             + ' ' + 'VABNSP VATNSP'
178500100326     C                   endif
178600100326     C*
178700100326     C                   ENDSR
178800010330
178900100326
179000010330
179100000613     C     *inzsr        BEGSR
179200990910     C*
179300990910     C     *entry        plist
179400990920     C                   parm                    tivlrds
179500990921     C                   parm      wrkesito      esito
179600000724     C                   parm                    prmlit
179700000710     C                   parm                    prmfir
179800010330     C*
179900010330     C* CALCOLA LA DATA CORRENTE
180000100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180100100318     C                   eval      datcor = %dec(%date() : *ISO)
180200000613     C*
180300000613     C                   ENDSR
180400000613     C***
180500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180600120731VATNOT_E;FILXXX;W_VABNSP;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;
180700160201FILXXX;FILXXX;FILXXX;FILXXX;VABRSD;VABRD2;VABIND;FILXXX;VATNOT_IJ;VABLOD;
180800120731VABCAD;FILXXX;FILXXX;VATNOT_B;FILXXX;FILXXX;VABPKB;FILXXX;FILXXX;FILXXX;
180900141202FILXXX;FILXXX;VABCAS;FILXXX;FILXXX;FILXXX;FILXXX;
181000121005FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;FILXXX;VABRMA;FILXXX;FILXXX;
