000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130114     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*CALLER)
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
017100120321     C                   exsr      inzvar
017200120321     C                   exsr      impvab
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
109600030721     C                   EVAL      PiStr=SkSplitCSV(i)
109700030721     C                   EXSR      CHKNUM
109800030721     C                   IF        PiInt=*on
109900030721     C                   Z-ADD     PiVal         Num5_0
110000030721     C                   MOVEL(p)  Num5_0        VABCAD
110100030721     C                   ELSE
110200030721     C                   SETON                                        32
110300030721     C                   EVAL      VABCAD = *zeros
110400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110500030721     C                             + ' ' + 'VABCAD'
110600030721     C                   ENDIF
110700030721     C                   ENDIF
110800030721     C*** NCL
110900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiInt=*on
111300030721     C                   Z-ADD     PiVal         VABNCL
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABNCL = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800040122     C                             + ' ' + 'VABNCL'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** PKB
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiNum=*on
112600030721     C                   Z-ADD     PiVal         VABPKB
112700030721     C                   ELSE
112800030721     C                   SETON                                        32
112900030721     C                   EVAL      VABPKB = *zeros
113000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113100030721     C                             + ' ' + 'VABPKB'
113200030721     C                   ENDIF
113300030721     C                   ENDIF
113400030721     C*** CAS
113500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113600101018     C                   IF        SkSplitCSV(i) <> *blanks AND
113700101018     C                             SkSplitCSV(i) <> *zeros
113800101018     C                   EVAL      wFlgCAS = '1'
113900101018     C                   ENDIF
114000030721     C                   EVAL      PiStr=SkSplitCSV(i)
114100030721     C                   EXSR      CHKNUM
114200030721     C                   IF        PiNum=*on
114300030721     C                   Z-ADD     PiVal         VABCAS
114400030721     C                   ELSE
114500030721     C                   SETON                                        32
114600030721     C                   EVAL      VABCAS = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800030721     C                             + ' ' + 'VABCAS'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** CCM
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on      AND
115600030721     C                             PiVal<=9999999 AND
115700030721     C                             PiVal>*zeros
115800030721     C                   Z-ADD     PiVal         VABCCM
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABCCM = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300040510     C                             + ' ' + 'VABCCM'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** LNP
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116800030721     C                   EVAL      PiStr=SkSplitCSV(i)
116900030721     C                   EXSR      CHKNUM
117000030721     C                   IF        PiInt=*on    AND
117100030721     C                             PiVal<=999   AND
117200030721     C                             PiVal>*zeros
117300030721     C                   Z-ADD     PiVal         VABLNP
117400030721     C                   ELSE
117500040715     C                   SETON                                        32
117600030721     C                   EVAL      VABLNP = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800040510     C                             + ' ' + 'VABLNP'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** NRS
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on AND
118600030721     C                             PiVal<=99
118700030721     C                   Z-ADD     PiVal         VABNRS
118800030721     C                   ELSE
118900030721     C                   SETON                                        32
119000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119100030721     C                             + ' ' + 'VABNRS'
119200030721     C                   ENDIF
119300030721     C                   ENDIF
119400030721     C*** LNA
119500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119600030721     C                   EVAL      PiStr=SkSplitCSV(i)
119700030721     C                   EXSR      CHKNUM
119800030721     C                   IF        PiInt=*on    AND
119900030721     C                             PiVal<=999   AND
120000030721     C                             PiVal>*zeros
120100030721     C                   Z-ADD     PiVal         VABLNA
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      VABLNA = *zeros
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABLNA'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** CTR
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on  AND
121400030721     C                             PiVal<=999
121500030721     C                   Z-ADD     PiVal         VABCTR
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABCTR'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** IAS
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiNum=*on
122700030721     C                   Z-ADD     PiVal         VABIAS
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABIAS'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** VLB
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiNum=*on
123900030721     C                   Z-ADD     PiVal         VABVLB
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124300030721     C                             + ' ' + 'VABVLB'
124400030721     C                   ENDIF
124500030721     C                   ENDIF
124600030721     C*** QFT
124700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124800030721     C                   EVAL      PiStr=SkSplitCSV(i)
124900030721     C                   EXSR      CHKNUM
125000030721     C                   IF        PiNum=*on
125100030721     C                   Z-ADD     PiVal         VABQFT
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABQFT'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** NCD
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiInt=*on      AND
126300030721     C                             PiVal<=9999999
126400030721     C                   Z-ADD     PiVal         VABNCD
126500030721     C                   ELSE
126600030721     C                   SETON                                        32
126700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126800030721     C                             + ' ' + 'VABNCD'
126900030721     C                   ENDIF
127000030721     C                   ENDIF
127100030721     C*** NCA
127200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127300030721     C                   EVAL      PiStr=SkSplitCSV(i)
127400030721     C                   EXSR      CHKNUM
127500030721     C                   IF        PiInt=*on      AND
127600030721     C                             PiVal<=9999999
127700030721     C                   Z-ADD     PiVal         VABNCA
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABNCA'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** ZNC
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on AND
128900030721     C                             PiVal<=99
129000030721     C                   Z-ADD     PiVal         VABZNC
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABZNC'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** DCR
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on       AND
130200030721     C                             PiVal<=99999999
130300030721     C                   Z-ADD     PiVal         VABDCR
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABDCR'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** HCR
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on   AND
131500030721     C                             PiVal<=9999
131600030721     C                   Z-ADD     PiVal         VABHCR
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABHCR'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** VMD
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiNum=*on
132800030721     C                   Z-ADD     PiVal         VABVMD
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABVMD'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** ANT
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on        AND
134000030721     C                             PiVal<=999999999
134100030721     C                   Z-ADD     PiVal         VABANT
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABANT'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** CMO
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on
135300030721     C                   Z-ADD     PiVal         Num5_0
135400030721     C                   MOVEL(p)  Num5_0        VABCMO
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      VABCMO = *zeros
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABCMO'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200040510     C*
136300040510     C* Reperisco i campi del FIVAT
136400040510     C*** VATNOT
136500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136700040510     C                   ENDIF
136800040510     C*** VATTRC
136900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137100040510     C                   ENDIF
137200060331     C*** VATNOT_A
137300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
137400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137500060331     C                   ENDIF
137600060331     C*** VATNOT_B
137700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137900060331     C                   ENDIF
138000060331     C*** VATNOT_E
138100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138300060331     C                   ENDIF
138400070823     C*
138500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
138600110318     C*** NSP / RMN / NOT_E
138700101021     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
138800110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
138900101021     C                   EXSR      CHKNUM
139000101021     C                   IF        PiInt=*on
139100101021     C                   Z-ADD     PiVal         VABNSP
139200101021     C                   Z-ADD     PiVal         VABRMN
139300110318     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139400101021     C                   ELSE
139500101021     C                   SETON                                        31
139600101021     C                   EVAL      VABNSP = *zeros
139700101021     C                   EVAL      VABRMN = *zeros
139800110318     C                   EVAL      wVATNOT_E = *blanks
139900101021     C                   EVAL      vinmsg = %trimr(vinmsg)
140000110318     C                             + ' ' + 'VABNSP VABRMN VATNOT_E'
140100101021     C                   ENDIF
140200101021     C                   ENDIF
140300110318     C*
140400110318     C*** PKB
140500110318     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPKB'
140600110318     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
140700110318     C                   EXSR      CHKNUM
140800110318     C                   IF        PiNum=*on
140900110318     C                   EVAL      PiVal = PiVal / 1000                         * da gr. a kg.
141000110318     C                   Z-ADD(H)  PiVal         VABPKB
141100110318     C                   ELSE
141200110318     C                   SETON                                        31
141300110318     C                   EVAL      VABPKB = *zeros
141400110318     C                   EVAL      vinmsg = %trimr(vinmsg)
141500110318     C                             + ' ' + 'VABPKB'
141600110318     C                   ENDIF
141700110318     C                   ENDIF
141800070925     C*
141900071029     C***  <===  -----------------------------------------
142000040510     C*
142100030715     C                   ENDIF
142200030715     C                   EVAL      i = i + 1
142300030715     C                   ENDDO
142400070925     C*
142500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142600100831     C*
142700100831     C                   IF        wVABRSD <> *blanks
142800100831     C                   EVAL      VABRSD = wVABRSD
142900100831     C                   ENDIF
143000100831     C*
143100100831     C                   IF        wVABIND <> *blanks
143200100831     C                   EVAL      VABIND = wVABIND
143300100831     C                   ENDIF
143400100831     C*
143500100831     C                   IF        wVABCAD <> *blanks
143600100831     C                   EVAL      VABCAD = wVABCAD
143700100831     C                   ENDIF
143800100831     C*
143900100831     C                   IF        wVABLOD <> *blanks
144000100831     C                   EVAL      VABLOD = wVABLOD
144100100831     C                   ENDIF
144200100831     C*
144300100831     C                   IF        wVABPRD <> *blanks
144400100831     C                   EVAL      VABPRD = wVABPRD
144500100831     C                   ENDIF
144600070925     C*
144700071029     C***  <===  -----------------------------------------
144800070925     C*
144900070925     C* Considerazioni finali su CBO/CAS
145000100318     C                   IF        wFlgCAS = '1'    OR
145100100318     C                             VABCAS  > *zeros
145200070925     C                   IF        VABCBO = '1'
145300070925     C                   EVAL      VABCBO = '4'
145400070925     C                   ENDIF
145500070925     C                   IF        VABCBO = '2'
145600070925     C                   EVAL      VABCBO = '6'
145700070925     C                   ENDIF
145800070925     C                   ENDIF
145900070925     C***  <===  ----------------------------
146000040510     C*
146100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146200040510     C                   IF        VABNSP = *zeros
146300040510     C                   SETON                                        31
146400040510     C                   ENDIF
146500040715     C*
146600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146700040715     C                   IF        VABLNP = *zeros
146800040715     C                   SETON                                        31
146900040715     C                   ENDIF
147000040510     C*
147100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147200040510     C                   IF        VABRMN = *zeros
147300040510     C                   Z-ADD     VABNSP        VABRMN
147400040510     C                   ENDIF
147500040510     C*
147600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147700040510     C                   IF        VABRMA = *blanks
147800040714     C                   MOVEL     VABRMN        VABRMA
147900040510     C                   ENDIF
148000040128     C*
148100040510     C* Se provincia nn valorizzata la reperisco
148200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148300040510     C                   IF        VABPRD = *blanks
148400040128     C                   CLEAR                   TISI95DS
148500040128     C                   EVAL      I95TCN = '3'
148600040128     C                   Z-ADD     datcor        I95DAT
148700040128     C                   EVAL      I95NAR = VABNZD
148800040128     C                   EVAL      I95CAP = VABCAD
148900040128     C                   EVAL      I95LOC = VABLOD
149000040128     C                   CALL      'TISI95R'
149100040128     C                   PARM                    TISI95DS
149200040128     C                   EVAL      VABPRD = O95PRV
149300040510     C                   ENDIF
149400020204     C*
149500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149600020204     C                   EXSR      CHKIMPDIV
149700030715     C                   ENDIF
149800020204     C*
149900010607     C                   ENDSR
150000010607     C*----------------------------------------------------*
150100010601
150200020204
150300020204     C*----------------------------------------------------*
150400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150500020204     C*----------------------------------------------------*
150600020204     C     CHKIMPDIV     BEGSR
150700020204     C*
150800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150900020204     C                   Z-ADD     *zeros        wrkDec            9 9
151000020204     C*
151100020204     C* Come prima cosa effettuo considerazioni sulla divisa
151200020204     C                   IF        vabIAS > *zeros
151300020204     C                   IF        vabVAS <> 'EUR'
151400020204     C                   EVAL      vabVAS =  'ITL'
151500020204     C                   ENDIF
151600020204     C                   ENDIF
151700020204     C*
151800020204     C                   IF        vabCAS > *zeros
151900020204     C                   IF        vabVCA <> 'EUR'
152000020204     C                   EVAL      vabVCA =  'ITL'
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C                   IF        vabVMD > *zeros
152500020204     C                   IF        vabVAD <> 'EUR'
152600020204     C                   EVAL      vabVAD =  'ITL'
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153100020204     C                   Z-ADD     vabIAS        wrkDec
153200020204     C                   IF        wrkDec > *zeros
153300020204     C                   IF        vabVAS = 'ITL'
153400020204     C                   EVAL      vabIAS = *zeros
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153900020204     C                   Z-ADD     vabCAS        wrkDec
154000020204     C                   IF        wrkDec > *zeros
154100020204     C                   IF        vabVCA = 'ITL'
154200020204     C                   EVAL      vabCAS = *zeros
154300020204     C                   ENDIF
154400020204     C                   ENDIF
154500020204     C*
154600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154700020204     C                   Z-ADD     vabVMD        wrkDec
154800020204     C                   IF        wrkDec > *zeros
154900020204     C                   IF        vabVAD = 'ITL'
155000020204     C                   EVAL      vabVMD = *zeros
155100020204     C                   ENDIF
155200020204     C                   ENDIF
155300020204     C*
155400020204     C                   ENDSR
155500020204     C***
155600010330
155700100318
155800100318
155900100318     C*----------------------------------------------------*
156000100318     C*  CONTROLLO NUMERICITA' CAMPI
156100100318     C*----------------------------------------------------*
156200100318     C     CHKNUM        BEGSR
156300100318     C*
156400100318     C                   IF        PiDecChr = *blanks
156500100318     C                   EVAL      PiDecChr = CharNUM
156600100318     C                   ENDIF
156700100318     C*
156800100318     C                   callp     UBISNUM_Check(PiStr
156900100318     C                                          :PiDecChr
157000100318     C                                          :PiVal
157100100318     C                                          :PiNum
157200100318     C                                          :PiInt)
157300100318     C*
157400100318     C                   ENDSR
157500100318     C***
157600010601
157700010601
157800010601
157900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158000010601     C     repfil        BEGSR
158100010601     C*
158200010601     C                   if        invfil = *zeros and
158300010601     C                             depfil > *zeros and
158400010629     C                             (vinflg = *blanks or
158500010629     C                              vinflg = *zeros)
158600010601     C
158700010601     C                   eval      invfil = depfil
158800010601     C                   endif
158900010601     C*
159000010601     C                   if        depfil <> invfil and
159100010601     C                             invfil > *zeros
159200010601     C                   eval      flgMulti = '1'
159300010601     C                   if        vinflg = *blanks
159400010601     C                   add       1             cntNonEl
159500010601     C                   endif
159600010601     C                   endif
159700010601     C*
159800010601     C                   if        vinflg = '2'
159900010601     C                   eval      flgStato = '2'
160000010601     C                   endif
160100010601     C*
160200010601     C                   ENDSR
160300010601     C***
160400010601
160500010601
160600010601
160700010330
160800040119      /TITLE Invio dei dati al punto operativo.
160900040119     C     invio         BEGSR
161000040119     C*
161100040119     C* 1° invio FIVAT
161200040119     C                   reset                   dscmz
161300040119     C                   move      vlrpoi        cmzdst
161400040119     C                   eval      cmzfld = 'FIVATWWR'
161500040119     C                   eval      cmzmbd = vlrhdl
161600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161700040119     C***                if        prmfir = *blanks
161800040119     C                   eval      cmzfla = 'FIVAT00F'
161900040119     C                   eval      cmzmba = 'FIVAT00F'
162000040119     C***                else
162100040119     C***                eval      cmzfla = prmfir
162200040119     C***                eval      cmzmba = prmfir
162300040119     C***                endif
162400040119     C                   eval      cmznrr = *zeros
162500040119     C                   move      §ctrokvt      cmznrr
162600040119     C                   eval      cmzlba = vlrfl1
162700040119     C                   call(e)   'TIS711C'
162800040119     C                   parm                    dscmz
162900040119     C                   parm      *blanks       esito
163000040119     C                   if        %error
163100040119     C                             or cmzerr = '1'
163200040119     C                             or esito  = '1'
163300040119     C                   eval      wrkesito = '3'
163400040119     C                   else
163500040119     C*
163600040119     C* 2° invio FIVAB
163700040119     C                   reset                   dscmz
163800040119     C                   move      vlrpoi        cmzdst
163900040119     C                   eval      cmzfld = vlrfou
164000040119     C                   eval      cmzmbd = vlrhdl
164100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164200040119     C***                if        prmfir = *blanks
164300040119     C                   eval      cmzfla = 'FIVAB00F'
164400040119     C                   eval      cmzmba = 'FIVAB00F'
164500040119     C***                else
164600040119     C***                eval      cmzfla = prmfir
164700040119     C***                eval      cmzmba = prmfir
164800040119     C***                endif
164900040119     C                   eval      cmznrr = *zeros
165000040119     C                   move      §ctrokvb      cmznrr
165100040119     C                   eval      cmzlba = vlrfl1
165200040119     C                   call(e)   'TIS711C'
165300040119     C                   parm                    dscmz
165400040119     C                   parm      *blanks       esito
165500040119     C                   if        %error
165600040119     C                             or cmzerr = '1'
165700040119     C                             or esito  = '1'
165800040119     C                   eval      wrkesito = '3'
165900040119     C                   endif
166000040119     C                   endif
166100040119     C*
166200040119     C                   ENDSR
166300040119     C***
166400010601
166500010601
166600010601
166700010601
166800010601      /TITLE Invio dei dati al punto operativo.
166900010601     C     opeini        BEGSR
167000010601     C*
167100010601     C* Inizializzo flag e contatori operativi
167200010601     C                   movel     '0'           flgGiro           1
167300010601     C                   movel     '0'           flgMulti          1
167400010601     C                   movel     '1'           flgStato          1
167500010615     C                   movel     '0'           flgOk             1
167600010601     C                   z-add     *zeros        cntNonEl         10 0
167700010601     C                   z-add     *zeros        depfil            3 0
167800010601     C                   z-add     *zeros        invfil            3 0
167900010601     C*
168000010601     C                   ENDSR
168100010601     C***
168200070326
168300070326
168400070326
168500070326
168600070326     C     *pssr         BEGSR
168700070329     C*
168800070329     C                   if        %open(tivin00r)
168900070329     C                   close     tivin00r
169000070329     C                   endif
169100070329     C                   if        %open(fivabwwr)
169200070329     C                   close     fivabwwr
169300070329     C                   endif
169400070329     C                   if        %open(fivatwwr)
169500070329     C                   close     fivatwwr
169600070329     C                   endif
169700070326     C*
169800070326     C* Effettuo la chiamata al CLLE preposto
169900070326     C                   call(e)   'TITVVTC'
170000070326     C                   parm                    parccm
170100070326     C                   parm                    parmbr
170200070326     C                   parm      '2'           paropz
170300070326     C*
170400070326     C                   eval      wrkesito = '2'
170500070404     C*
170600070404     C                   seton                                        LR
170700070326     C*
170800070326     C                   ENDSR     '*CANCL'
170900070326     C***
171000100318
171100100318
171200100318
171300100318     C     normCSV       BEGSR
171400100318     C*
171500100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171600100318     C* separatori campi
171700100318     C                   if        CharTXT <> *blanks
171800100318     C                   z-add     *zeros        wLenVINDTA        4 0
171900100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172000100318     C                   setoff                                       80
172100100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172200100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172300100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172400100318     C                   if        not *in80
172500100318     C                   seton                                        80
172600100318     C                   else
172700100318     C                   setoff                                       80
172800100318     C                   endif
172900100318     C                   endif
173000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173100100318     C                             and *in80 = *on
173200100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173300100318     C                   endif
173400100318     C                   enddo
173500100318     C                   endif
173600100318     C*
173700100318     C                   ENDSR
173800100318     C***
173900070326
174000100326
174100100326
174200100326     C*----------------------------------------------------*
174300100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174400100326     C*----------------------------------------------------*
174500100326     C     REPNSP        BEGSR
174600100326     C*
174700100326     C* NSP => Stacco un numeratore da AZNUM
174800100326     C                   clear                   TRUL33DS
174900100326     C                   eval      I33OPE = *zeros
175000100326     C                   eval      I33CNU = 302
175100100326     C                   eval      I33NUM = 1
175200100326     C                   movel     TRUL33DS      KPJBU
175300100326     C                   call      'TRUL33R'
175400100326     C                   parm                    KPJBA
175500100326     C                   movel     KPJBU         TRUL33DS
175600100326     C                   if        O33ERR = *zeros
175700100326     C                   z-add     O33NRF        VABNSP
175800100326     C                   z-add     O33NRF        VATNSP
175900100326     C                   else
176000100326     C                   SETON                                        31
176100100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176200100326     C                             + ' ' + 'VABNSP VATNSP'
176300100326     C                   endif
176400100326     C*
176500100326     C                   ENDSR
176600010330
176700100326
176800010330
176900000613     C     *inzsr        BEGSR
177000990910     C*
177100990910     C     *entry        plist
177200990920     C                   parm                    tivlrds
177300990921     C                   parm      wrkesito      esito
177400000724     C                   parm                    prmlit
177500000710     C                   parm                    prmfir
177600010330     C*
177700010330     C* CALCOLA LA DATA CORRENTE
177800100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
177900100318     C                   eval      datcor = %dec(%date() : *ISO)
178000000613     C*
178100000613     C                   ENDSR
178200000613     C***
178300040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178400110315W_VABNSP;FILXX2;VABRSD;VABRD2;VABIND;VABCAD;VABLOD;VATNOT_B;FILXX9;W_VABPKB;
178500110315FILX11;FILX12;FILX13;FILX14;FILX15;FILX16;FILX17;FILX18;FILX19;FILX20;
178600110315
178700110315
178800100318
