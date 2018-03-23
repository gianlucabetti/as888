000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200140520      *
000300140520      * PARTICOLARITA':
000400130227      * cliente ha aggiunto un nuovo campo denominato VABTIC che conterrà il valore BM solo per le
000500130227      * spedizioni per le quali accetta la modalità incasso contrassegno.
000600140520      * aggiunta la gestione alert sia via SMS che email
000700130227      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900120209     F*
001000990910     Ftivin00r  uF   E             DISK    usropn
001100100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
001200040119     FFIVATwwr  O    E             DISK    usropn
001300000313     D*
001400040113     D*------------
001500040113     D* SCHIEDE A PROGRAMMA
001600040113     D*------------
001700080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001800010330     D*----------------------------------------------------
001900010330     D* DICHIARAZIOINE VARIABILI DI WRK
002000010330     D*----------------------------------------------------
002100010330     D dscmz         e ds                  inz
002200010330     D psds           sds
002300010330     D  procname         *PROC
002400010330     D tivlrds       e ds                  extname(tivlr00f)
002500040128     D tisi95ds      e ds
002600100413     D fivabds       e ds                  extname(fivab00f)
002700100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002800100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002900010330     D esito           s              1
003000010330     D prmlit          s             10
003100010330     D prmfir          s             10
003200010330     D wrkesito        s                   like(esito)
003300010330     D rrnum           s              6  0 INZ(*zeros)
003400040510     D parccm          s              8    INZ(*blanks)
003500040119     D parmbr          s             10    INZ(*blanks)
003600040119     D paropz          s              1    INZ(*blanks)
003700040119     D chkcall         s              1    INZ(*blanks)
003800080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003900080418     D wFlgCAS         s              1    INZ(*blanks)
004000100326     D*------------------
004100100326     D* DS REPERIMENTO NUMERATORE
004200100326     D*------------------
004300100326     D trul33ds      e ds                  inz
004400100326     D*------------------
004500100326     D* DS ARCHITETTURA
004600100326     D*------------------
004700100326     D kpjba         e ds                  inz
004800100326
004900030715     D*------------------
005000030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005100030715     D*------------------
005200030715     D SkSplitFLD      S             10    DIM(100)
005300030715     D SkSplitCSV      S            256    DIM(100)
005400030715     D CharCSV         S              1
005500030715     D CharTXT         S              1
005600030715     D CharNUM         S              1
005700100318     D CharSOS         S              1
005800030715     D posDa           S              3  0 INZ(*zeros)
005900030715     D posA            S              3  0 INZ(*zeros)
006000030715     D i               s              3  0 INZ(1)
006100030715     D wGiro           s              1  0 INZ(*zeros)
006200070823     D wVATNOT_A       s                   LIKE(VATNOT)
006300060331     D wVATNOT_B       s                   LIKE(VATNOT)
006400060331     D wVATNOT_E       s                   LIKE(VATNOT)
006500110927     D wVATNOT_I       s                   LIKE(VATNOT)
006600140520     D wVATNOT_J       s                   LIKE(VATNOT)
006700140520     D wVATNOT_S       s                   LIKE(VATNOT)
006800140520     D wVATNOT_IJ      s             70
006900030822     D*------------------
007000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007100030822     D*------------------
007200030822     D posDaDft        S              3  0 INZ(*zeros)
007300030822     D posADft         S              3  0 INZ(*zeros)
007400030822     D j               s              3  0 INZ(1)
007500030822     D wGiroDft        s              1  0 INZ(*zeros)
007600080418     D*------------------
007700080418     D* Costanti
007800080418     D*------------------
007900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008100100318
008200100318     D*------------------
008300100318     D* LINKING A DEFINIZIONI ESTERNE
008400100318     D*------------------
008500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008700100318
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
014000010601
014100010601
014200010330     C*--------------------------------------------------------
014300021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014400010330     C*--------------------------------------------------------
014500010605     C     RWVAB         BEGSR
014600010330     C*
014700010330     C                   if        not %open(tivin00r)
014800010330     C                   open      tivin00r
014900010330     C                   endif
015000021025     C                   if        not %open(fivabwwr)
015100021025     C                   open      fivabwwr
015200010330     C                   endif
015300040119     C*
015400040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015500040119     C                   exsr      prevat
015600040119     C*
015700040119     C                   if        chkcall = '0'
015800040119     C*
015900040119     C                   if        not %open(fivatwwr)
016000040119     C                   open      fivatwwr
016100040119     C                   endif
016200010330     C*
016300040119     C                   clear                   §CTROKVB          7 0
016400040119     C                   clear                   §CTROKVT          7 0
016500040119     C                   clear                   §CTRMO            7 0
016600040119     C                   clear                   §CTRNO            7 0
016700070823     C*
016800070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016900070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017000120227     C                   exsr      inzvar
017100120227     C                   exsr      impvab
017200110131     C*
017300110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017400120301 xxx C                   read      tivin00r                               70
017500010330     C*
017600030822     C                   do        *HIVAL
017700010330     C*
017800030822     C                   read      tivin00r                               70
017900010618     C*
018000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018100010618     C                   if        vinflg = '1'
018200010618     C                   eval      flgOk = '1'
018300010618     C                   endif
018400040510     C*
018500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018600040510     C                   z-add     *zeros        wLenVINDTA        4 0
018700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018800040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019000040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019100040510     C                   z-add     1             wFlgVINDTA
019200040510     C                   leave
019300040510     C                   endif
019400040510     C                   enddo
019500010618     C*
019600040510     C                   if        vindta > *blanks AND
019700040510     C                             wFlgVINDTA = 1
019800010330     C                   add       1             rrnum
019900010330     C*
020000010601     C                   if        *in70 = *off and
020100010330     C                             (vinflg = *blanks
020200010330     C                              or vinflg = '0'
020300010330     C                              or vinflg = '2')
020400010330     C*
020500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020600010711     C                   if        vinflg = *blanks or vinflg = '0'
020700010711     C                   clear                   vinmsg
020800010711     C                   endif
020900010601     C*
021000070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021100070925     C                   eval      vindta  = ' ' + %trim(vindta)
021200070925     C*
021300080527     C                   exsr      inzvar
021400100318     C                   exsr      normCSV
021500010605     C                   exsr      impvab
021600010601     C*
021700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021800010605     C                   eval      depfil = VABLNP
021900010601     C                   exsr      repfil
022000010601     C                   if        depfil = invfil
022100021025     C                   if        vlrpoi = 999
022200030822     C                   move(P)   invfil        VABFGS
022300021025     C                   else
022400030822     C                   move(P)   vlrpoi        VABFGS
022500021025     C                   endif
022600010601     C*
022700010601     C                   exsr      PREELA
022800010601     C*
022900010604     C* Ebbene...
023000010604     C*
023100040119     C  N31              add       1             §CTROKVB
023200040119     C   32              add       1             §CTRMO
023300040119     C   31              add       1             §CTRNO
023400080418     C*
023500030822     C                   if        wGiro = 2
023600080513     C* Scrivo sempre il VAT
023700080513     C  N31              exsr      wrivat
023800110201     C                   exsr      chkvab
023900080513     C                   endif
024000010604     C*
024100010604     C                   if        *in31 = *off and
024200010604     C                             *in32 = *off
024300010604     C                   eval      vinflg = '1'
024400010604     C                   else
024500010604     C                   eval      vinflg = '2'
024600010604     C                   endif
024700010604     C                   endif
024800010601     C*
024900010604     C                   endif
025000010604     C*
025100010330     C                   else
025200010330     C                   eval      vinflg = '1'
025300010330     C                   endif
025400010601     C*
025500010601     C  N70              update    tivin000
025600010330     C*
025700030822     C  N70              enddo
025800040119     C*
025900080527     C* Verifico dati rimasti "in cannna"...
026000080527     C                   seton                                        65
026100110201     C                   exsr      chkvab
026200080527     C*
026300040119     C                   endif                                                  (endif - chkcall)
026400010601     C*
026500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026600010601     C                   if        cntNonEl = *zeros or
026700010601     C                             flgMulti = '0'
026800010330     C* Se non ci sono record con errori ...
026900010601     C                   if        §ctrno = 0 and
027000010604     C                             §ctrmo = 0 and
027100010601     C                             flgStato <> '2'
027200010330     C* ... restituisco esito OK.
027300010330     C                   eval      wrkesito = '0'
027400010330     C                   else
027500040119     C                   if        §ctrokvb > 0
027600010330     C                   eval      wrkesito = '1'
027700010330     C                   else
027800010615     C                   if        flgOk = '0'
027900010615     C                   eval      wrkesito = '2'
028000010615     C                   else
028100010615     C                   eval      wrkesito = '6'
028200010615     C                   endif
028300010330     C                   endif
028400010330     C                   endif
028500010601     C                   else
028600010601     C                   eval      wrkesito = '9'
028700010601     C                   endif
028800010330     C*
028900010330     C                   if        %open(tivin00r)
029000010330     C                   close     tivin00r
029100010330     C                   endif
029200021025     C                   if        %open(fivabwwr)
029300021025     C                   close     fivabwwr
029400010330     C                   endif
029500040512     C                   if        %open(fivatwwr)
029600040512     C                   close     fivatwwr
029700040512     C                   endif
029800010601     C*
029900010601     C                   if        vlrpoi <> 999
030000010601     C                   eval      invfil = vlrpoi
030100010601     C                   endif
030200010330     C*
030300040119     C                   if        §ctrokvb > 0
030400010601     C                             and invfil > *zeros
030500010330     C                   exsr      invio
030600010330     C                   endif
030700010601     C*
030800010618     C                   if        flgGiro = '1'
030900010601     C                   exsr      endela
031000010618     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400080527
031500080527
031600080527
031700080527     C*----------------------------------------------------*
031800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031900080527     C*----------------------------------------------------*
032000100413     C     CHKVAB        BEGSR
032100110201     C*
032200110201     C* Se richiesti ignorare colli / peso / volume
032300110315 xxx C***                if        *in35
032400110315 xxx C***                z-add     *zeros        VABNCL
032500110315 xxx C***                z-add     *zeros        VABPKB
032600110315 xxx C***                z-add     *zeros        VABVLB
032700110315 xxx C***                endif
032800080527     C*
032900080527     C* Scrivo il VAB solo a rottura d spedizione
033000080527     C                   if        depspe = *zeros
033100080527     C  N31              eval      fivabds_sav = fivabds
033200080527     C                   eval      depspe = vabNSP
033300080527     C                   else
033400080527     C                   if        depspe = vabNSP AND *in65 = *off
033500100413     C   51              add       VABNCL        sav_VABNCL
033600100413     C   51              add       VABPKB        sav_VABPKB
033700100413     C   51              add       VABVLB        sav_VABVLB
033800100720     C   52              z-add     VABNCL        sav_VABNCL
033900100720     C   52              z-add     VABPKB        sav_VABPKB
034000100720     C   52              z-add     VABVLB        sav_VABVLB
034100100720     C   53              add       VABNCL        sav_VABNCL
034200100720     C   53              z-add     VABPKB        sav_VABPKB
034300100720     C   53              z-add     VABVLB        sav_VABVLB
034400080527     C                   else
034500080527     C  N31              eval      fivabds_ok = fivabds_sav
034600080527     C  N31              exsr      wrivab
034700080527     C  N31              eval      fivabds_sav = fivabds
034800080527     C                   eval      depspe = vabNSP
034900080527     C                   endif
035000080527     C                   endif
035100080527     C*
035200080527     C                   ENDSR
035300040119
035400040119
035500040119
035600040119     C*----------------------------------------------------*
035700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035800040119     C*----------------------------------------------------*
035900040510     C     WRIVAB        BEGSR
036000080513     C*
036100080513     C* Gestisco l'eventuale rottura x numero spedizione
036200080527     C                   if        VABNSP <> depspe or *in65 = *on
036300070301     C                   if        wVATNOT_E <> *blanks
036400070301     C                   eval      VABCTM = '7Q'
036500070301     C                   endif
036600100413     C                   write     fivab000
036700080513     C                   endif
036800040119     C*
036900040119     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037500040119     C*----------------------------------------------------*
037600040510     C     WRIVAT        BEGSR
037700060512     C*
037800060512     C* Inizializzo i campi chiave
037900060512     C                   eval      VATFGS = VABFGS
038000060512     C                   eval      VATAAS = VABAAS
038100060512     C                   eval      VATCCM = VABCCM
038200070823     C                   eval      VATNRS = VABNRS
038300060512     C                   eval      VATNSP = VABNSP
038400060512     C                   eval      VATLNP = VABLNP
038500040119     C*
038600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038700040510     C                   if        VATTRC = *blanks
038800040510     C                   eval      VATTRC = 'E'
038900040510     C                   endif
039000110318     C                   if        VATNOT <> *blanks
039100040510     C                   write     FIVAT000
039200040119     C                   add       1             §CTROKVT
039300110318     C                   endif
039400060331     C*
039500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039600060331     C                   if        wVATNOT_A <> *blanks
039700060331     C                   eval      VATNOT = wVATNOT_A
039800060331     C                   eval      VATTRC = 'A'
039900060331     C                   write     FIVAT000
040000060331     C                   add       1             §CTROKVT
040100060331     C                   endif
040200060331     C*
040300060331     C                   if        wVATNOT_B <> *blanks
040400060331     C                   eval      VATNOT = wVATNOT_B
040500060331     C                   eval      VATTRC = 'B'
040600060331     C                   write     FIVAT000
040700060331     C                   add       1             §CTROKVT
040800060331     C                   endif
040900060331     C*
041000060331     C                   if        wVATNOT_E <> *blanks
041100110201     C                   eval      VATNOT = wVATNOT_E
041200060331     C                   eval      VATTRC = 'E'
041300110201     C                   write     FIVAT000
041400110201     C                   add       1             §CTROKVT
041500110201     C                   endif
041600110927     C*
041700140520     C                   if        %subst(wVATNOT_IJ:1:35) <> *blanks
041800140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
041900110927     C                   eval      VATTRC = 'I'
042000110927     C                   write     FIVAT000
042100110927     C                   add       1             §CTROKVT
042200110927     C                   endif
042300140520     C*
042400140520     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
042500140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
042600140520     C                   eval      VATTRC = 'J'
042700140520     C                   write     FIVAT000
042800140520     C                   add       1             §CTROKVT
042900140520     C                   endif
043000140520     C*
043100140520     C                   if        wVATNOT_S <> *blanks
043200140520     C                   EVAL      %subst(wVATNOT_S:17:2) = 'NN'
043300140520     C                   eval      VATNOT = wVATNOT_S
043400140520     C                   eval      VATTRC = 'S'
043500140520     C                   write     FIVAT000
043600140520     C                   add       1             §CTROKVT
043700140520     C                   endif
043800040119     C*
043900040119     C                   ENDSR
044000040119
044100040119
044200040119
044300040119     C*----------------------------------------------------*
044400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044500040119     C*----------------------------------------------------*
044600040119     C     PREVAT        BEGSR
044700040119     C*
044800040119     C* Compongo il nome del membro da dare al FIVATWWR
044900040119     C                   eval      parmbr = vlrhdl
045000040119     C                   movel     'M'           parmbr
045100040510     C                   eval      parccm = vlrksc
045200040119     C                   eval      paropz = '1'
045300040119     C* Effettuo la chiamata al CLLE preposto
045400040119     C                   call(e)   'TITVVTC'
045500040119     C                   parm                    parccm
045600040119     C                   parm                    parmbr
045700040119     C                   parm                    paropz
045800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045900040119     C                   if        %error
046000040119     C                   movel     '1'           chkcall
046100040119     C                   else
046200040119     C                   movel     '0'           chkcall
046300040119     C                   endif
046400040119     C*
046500040119     C                   ENDSR
046600010601
046700010601
046800010601
046900010330     C*----------------------------------------------------*
047000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047100010330     C*----------------------------------------------------*
047200040119     C     INZVAR        BEGSR
047300080527     C*
047400080527     C                   setoff                                       65
047500010330     C*
047600030822     C* Inizializzo variabili di wrk
047700020204     C                   Z-ADD     *zeros        Num5_0            5 0
047800101018     C                   MOVEL     *blanks       wFlgCAS
047900060331     C                   MOVEL     *blanks       wVATNOT_A
048000060331     C                   MOVEL     *blanks       wVATNOT_B
048100060331     C                   MOVEL     *blanks       wVATNOT_E
048200110927     C                   MOVEL     *blanks       wVATNOT_I
048300140520     C                   MOVEL     *blanks       wVATNOT_J
048400140520     C                   MOVEL     *blanks       wVATNOT_IJ
048500140520     C                   MOVEL     *blanks       wVATNOT_S
048600030822     C*
048700030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
048800030822     C                   CLEAR                   SkSplitCSV
048900030822     C                   EVAL      i = 1
049000030822     C                   EVAL      posDa = *zeros
049100030822     C                   EVAL      posA  = *zeros
049200080527     C*
049300080527     C                   CLEAR                   FIVABDS
049400080527     C                   CLEAR                   FIVAT000
049500030822     C*
049600030822     C* Reimposto i valori di default
049700030822     C                   EXSR      DEFCAM
049800030822     C*
049900010330     C                   ENDSR
050000010330     C*----------------------------------------------------*
050100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050200010330     C*----------------------------------------------------*
050300010330     C     DEFCAM        BEGSR
050400030822     C*
050500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050600030715     C* e delimitatore testo.
050700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
050800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
050900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051000100318     C*
051100100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051200100318     C                   EVAL      CharSOS = CharNUM
051300030822     C*
051400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051500100720     C                   SETOFF                                       505152
051600100720     C                   SETOFF                                       53
051700100413     C                   SELECT
051800100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
051900100720     C                   SETON                                        50
052000100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052100100413     C                   SETON                                        51
052200100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052300100720     C                   SETON                                        52
052400100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052500100720     C                   SETON                                        53
052600100413     C                   ENDSL
052700030822     C*
052800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052900030822     C                   EVAL      posDaDft = 1
053000030822     C                   EVAL      posADft  = 0
053100030822     C                   EVAL      wGiroDft = 0
053200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053300030822     C                             posDaDft > 0
053400030822     C*
053500030822     C* Gestisco il 1° giro
053600030822     C                   IF        wGiroDft = 0
053700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053900030822     C* Incremento il contatore dei "giri"
054000030822     C                   EVAL      wGiroDft = 1
054100030822     C                   ELSE
054200030822     C                   EVAL      posDaDft = posADft
054300030822     C                   ENDIF
054400030822     C* Eseguo lo scan x trovare la fine del campo corrente
054500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054600030822     C*
054700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054900030822     C                   IF        posDaDft > 0 AND
055000030822     C                             posADft  > 0
055100030822     C* NCL
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'NCL'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABNCL
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CCM
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CCM'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABCCM
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* LNP
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'LNP'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABLNP
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* NRS
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'NRS'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABNRS
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CTR
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CTR'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABCTR
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* PKB
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'PKB'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiNum=*on
062600030822     C                   Z-ADD     PiVal         VABPKB
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* VLB
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'VLB'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiNum=*on
063900030822     C                   Z-ADD     PiVal         VABVLB
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* QFT
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'QFT'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiNum=*on
065200030822     C                   Z-ADD     PiVal         VABQFT
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* CBO
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'CBO'
066000030822     C                   EVAL      VABCBO=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* TSP
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'TSP'
066900030822     C                   EVAL      VABTSP=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VAS
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VAS'
067800030822     C                   EVAL      VABVAS=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VCA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VCA'
068700030822     C                   EVAL      VABVCA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100070925     C* TIC
069200070925     C                   IF        %subst(
069300070925     C                             %subst(vlrppt:posDaDft+1:
069400070925     C                             posADft-posDaDft-1):1:3)
069500070925     C                             = 'TIC'
069600070925     C                   EVAL      VABTIC=%trim(%subst(
069700070925     C                             %subst(vlrppt:posDaDft+1:
069800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900070925     C                   ENDIF
070000030822     C* GCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'GCA'
070500030822     C                   EVAL      VABGCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* CTM
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'CTM'
071400030822     C                   EVAL      VABCTM=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* FFD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'FFD'
072300030822     C                   EVAL      VABFFD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* VAD
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'VAD'
073200030822     C                   EVAL      VABVAD=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GMA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GMA'
074100030822     C                   EVAL      VABGMA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GGA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GGA'
075000030822     C                   EVAL      VABGGA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GVA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GVA'
075900030822     C                   EVAL      VABGVA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC1
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC1'
076800030822     C                   EVAL      VABTC1=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* TC2
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'TC2'
077700030822     C                   EVAL      VABTC2=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100040714     C* VATTRC
078200040714     C                   IF        %subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):1:3)
078500040714     C                             = 'TRC'
078600040714     C                   EVAL      VATTRC=%trim(%subst(
078700040714     C                             %subst(vlrppt:posDaDft+1:
078800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900040714     C                   ENDIF
079000030822     C* ...
079100030822     C                   ENDIF
079200030822     C                   ENDDO
079300020204     C*
079400010330     C                   ENDSR
079500010607     C*----------------------------------------------------*
079600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
079700010607     C*----------------------------------------------------*
079800010607     C     IMPVAB        BEGSR
079900010607     C*
080000010607     C                   SETOFF                                       3132
080100110315 xxx C***                SETON                                        35
080200030515     C*
080300010607     C                   MOVEL     datcor        VABAAS
080400010607     C                   MOVE      datcor        VABMGS
080500010607     C*
080600030715     C**********
080700030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080800030715     C**********
080900030715     C                   IF        wGiro = *zeros
081000040113     C*
081100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081300080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081400080429     C                             %trim(intcol(5))<>*blanks
081500040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081600080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081700080429     C                                      %trim(intcol(5))
081800070823     C     ';':CharCSV   XLATE     vindta        vindta
081900040510     C                   ENDIF
082000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082100031201     C     minu:maiu     XLATE     vindta        vindta
082200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082300030715     C                   DOW       posDa <= %len(%trim(vindta))
082400030715     C*
082500030715     C* Gestisco il 1° campo
082600030715     C                   IF        i = 1
082700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082800030715     C                   EVAL      posDa = 1
082900030715     C* Eseguo lo scan x trovare la fine del primo campo
083000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083100030715     C* A questo punto "estraggo" il campo corrente
083200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083300030715     C                                                    (posA-posDa))
083400030715     C* X i campi successivi al 1°
083500030715     C                   ELSE
083600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083700030715     C                   EVAL      posDa = posA + 1
083800030715     C* Eseguo lo scan x trovare la fine del campo corrente
083900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084000030715     C* Gestisco l'ultimo campo
084100030715     C                   IF        posA = *zeros
084200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   LEAVE
084700030715     C                   ELSE
084800030715     C* A questo punto "estraggo" il campo corrente
084900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085000030715     C                                                    (posA-posDa))
085100030715     C                   ENDIF
085200030715     C                   ENDIF
085300030715     C* Incremento il contatore d campo
085400030715     C                   EVAL      i = i +1
085500030715     C                   ENDDO
085600030715     C*
085700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085800030715     C* delimitazione testo
085900030715     C                   EVAL      i = 1
086000030715     C                   DOW       i < %elem(SkSplitFLD)
086100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086300030715     C                   EVAL      i = i + 1
086400030715     C                   ENDDO
086500030715     C                   EVAL      wGiro = 1
086600030715     C                   ELSE
086700030715     C                   EVAL      wGiro = 2
086800031204     C**********
086900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087000031204     C* d separatore campo
087100031204     C**********
087200031204     C                   EVAL      vindta = %trim(vindta)
087300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087400031204     C                   EVAL      vindta = %subst(vindta:2)
087500031204     C                   ENDDO
087600031204     C*
087700031204     C                   Z-ADD     *zeros        lunghInput        4 0
087800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088200110912     C                   ENDDO
088300030715     C**********
088400030715     C* Effettuo lo split del campo dati d input
088500030715     C**********
088600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088700030715     C                   DOW       posDa <= %len(%trim(vindta))
088800030715     C*
088900030715     C* Gestisco il 1° campo
089000030715     C                   IF        i = 1
089100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089200030715     C                   EVAL      posDa = 1
089300030715     C* Eseguo lo scan x trovare la fine del primo campo
089400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089500030715     C* A questo punto "estraggo" il campo corrente
089600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089700030715     C                                                    (posA-posDa))
089800030715     C* X i campi successivi al 1°
089900030715     C                   ELSE
090000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090100030715     C                   EVAL      posDa = posA + 1
090200031204     C* Verifico che nn vi sia il campo nn valorizzato
090300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090400031204     C* Se campo nn valorizzato skippo al prossimo
090500031204     C                   EVAL      SkSplitCSV(i) = *blanks
090600031204     C                   EVAL      posA  = posA + 1
090700031204     C                   ELSE
090800030715     C* Eseguo lo scan x trovare la fine del campo corrente
090900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091000030715     C* Gestisco l'ultimo campo
091100030715     C                   IF        posA = *zeros
091200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C                   LEAVE
091700030715     C                   ELSE
091800030715     C* A questo punto "estraggo" il campo corrente
091900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092000030715     C                                                    (posA-posDa))
092100030715     C                   ENDIF
092200031204     C                   ENDIF
092300030715     C                   ENDIF
092400030715     C* Incremento il contatore d campo
092500030715     C                   EVAL      i = i +1
092600030715     C                   ENDDO
092700030715     C*
092800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092900030715     C* delimitazione testo
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitCSV)
093200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093400030715     C                   EVAL      i = i + 1
093500030715     C                   ENDDO
093600100720     C*
093700100720     C* Stacco un progressivo x numero spedizione
093800120301 xxx C                   EXSR      repNSP
093900100326     C*
094000030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094100030715     C                   EVAL      i = 1
094200030715     C                   DOW       i < %elem(SkSplitFLD)
094300030715     C*
094400030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094500030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094600030715     C                             SkSplitCSV(i) <> *blanks
094700030721     C*** ATB
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094900030721     C                   EVAL      VABATB = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** RSD
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095300030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095400030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095500030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095600030715     C* ==
095700030715     C                   ENDIF
095800030721     C*** RD2
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096000030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030715     C*** IND
096300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096400030715     C                   EVAL      VABIND = SkSplitCSV(i)
096500030715     C                   ENDIF
096600030715     C*** LOD
096700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096800030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096900030715     C                   ENDIF
097000030715     C*** PRD
097100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097200030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097300030715     C                   ENDIF
097400030721     C*** NZD
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097600030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** RMA
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098000030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** NOT
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098400030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** NT2
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098800030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030715     C*** VCA
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099200030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** VAS
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099600030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** FFD
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100000030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030721     C*** GC1
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100400030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GC2
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** TSP
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101300030721     C                   ENDIF
101400040510     C*** CBO
101500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101700040510     C                   ENDIF
101800030721     C*** NAS
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TIC
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** GCA
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** XCO
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** CTM
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** TCR
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** CTS
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** FTM
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** VAD
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** GMA
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** GGA
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** GVA
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** TC1
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** TC2
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** SCL
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** RMO
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** NMO
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*
108700030721     C* Reperisco quindi i campi numerici...
108800040510     C*** NSP
108900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109000040510     C                   EVAL      PiStr=SkSplitCSV(i)
109100040510     C                   EXSR      CHKNUM
109200040510     C                   IF        PiInt=*on
109300040510     C                   Z-ADD     PiVal         VABNSP
109400040510     C                   ELSE
109500040510     C                   SETON                                        31
109600040510     C                   EVAL      VABNSP = *zeros
109700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109800040510     C                             + ' ' + 'VABNSP'
109900040510     C                   ENDIF
110000040510     C                   ENDIF
110100071106     C*** RMN
110200071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110300071106     C                   EVAL      PiStr=SkSplitCSV(i)
110400071106     C                   EXSR      CHKNUM
110500071106     C                   IF        PiInt=*on
110600071106     C                   Z-ADD     PiVal         VABRMN
110700071106     C                   ELSE
110800071106     C                   SETON                                        32
110900071106     C                   EVAL      VABRMN = *zeros
111000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111100071106     C                             + ' ' + 'VABRMN'
111200071106     C                   ENDIF
111300071106     C                   ENDIF
111400030721     C*** CAD
111500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111600030721     C                   EVAL      PiStr=SkSplitCSV(i)
111700030721     C                   EXSR      CHKNUM
111800030721     C                   IF        PiInt=*on
111900030721     C                   Z-ADD     PiVal         Num5_0
112000030721     C                   MOVEL(p)  Num5_0        VABCAD
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      VABCAD = *zeros
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500030721     C                             + ' ' + 'VABCAD'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** NCL
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on
113300030721     C                   Z-ADD     PiVal         VABNCL
113400030721     C                   ELSE
113500030721     C                   SETON                                        32
113600030721     C                   EVAL      VABNCL = *zeros
113700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113800040122     C                             + ' ' + 'VABNCL'
113900030721     C                   ENDIF
114000030721     C                   ENDIF
114100030721     C*** PKB
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiNum=*on
114600030721     C                   Z-ADD     PiVal         VABPKB
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      VABPKB = *zeros
115000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115100030721     C                             + ' ' + 'VABPKB'
115200030721     C                   ENDIF
115300030721     C                   ENDIF
115400030721     C*** CAS
115500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115600110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115700111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115800111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
115900120227 xxx C                   EVAL      VABvca = 'EUR'
116000130227     C***                EVAL      VABTIC = 'BM'
116100101018     C                   EVAL      wFlgCAS = '1'
116200101018     C                   ENDIF
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiNum=*on
116600030721     C                   Z-ADD     PiVal         VABCAS
116700030721     C                   ELSE
116800030721     C                   SETON                                        32
116900030721     C                   EVAL      VABCAS = *zeros
117000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117100030721     C                             + ' ' + 'VABCAS'
117200030721     C                   ENDIF
117300030721     C                   ENDIF
117400030721     C*** CCM
117500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117600030721     C                   EVAL      PiStr=SkSplitCSV(i)
117700030721     C                   EXSR      CHKNUM
117800030721     C                   IF        PiInt=*on      AND
117900030721     C                             PiVal<=9999999 AND
118000030721     C                             PiVal>*zeros
118100030721     C                   Z-ADD     PiVal         VABCCM
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABCCM = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600040510     C                             + ' ' + 'VABCCM'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** LNP
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on    AND
119400030721     C                             PiVal<=999   AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABLNP
119700030721     C                   ELSE
119800040715     C                   SETON                                        32
119900030721     C                   EVAL      VABLNP = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040510     C                             + ' ' + 'VABLNP'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** NRS
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on AND
120900030721     C                             PiVal<=99
121000030721     C                   Z-ADD     PiVal         VABNRS
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABNRS'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** LNA
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiInt=*on    AND
122200030721     C                             PiVal<=999   AND
122300030721     C                             PiVal>*zeros
122400030721     C                   Z-ADD     PiVal         VABLNA
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      VABLNA = *zeros
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABLNA'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** CTR
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on  AND
123700030721     C                             PiVal<=999
123800030721     C                   Z-ADD     PiVal         VABCTR
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABCTR'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** IAS
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiNum=*on
125000030721     C                   Z-ADD     PiVal         VABIAS
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABIAS'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** VLB
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiNum=*on
126200030721     C                   Z-ADD     PiVal         VABVLB
126300030721     C                   ELSE
126400030721     C                   SETON                                        32
126500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126600030721     C                             + ' ' + 'VABVLB'
126700030721     C                   ENDIF
126800030721     C                   ENDIF
126900030721     C*** QFT
127000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127100030721     C                   EVAL      PiStr=SkSplitCSV(i)
127200030721     C                   EXSR      CHKNUM
127300030721     C                   IF        PiNum=*on
127400030721     C                   Z-ADD     PiVal         VABQFT
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABQFT'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** NCD
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on      AND
128600030721     C                             PiVal<=9999999
128700030721     C                   Z-ADD     PiVal         VABNCD
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABNCD'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** NCA
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on      AND
129900030721     C                             PiVal<=9999999
130000030721     C                   Z-ADD     PiVal         VABNCA
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABNCA'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** ZNC
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiInt=*on AND
131200030721     C                             PiVal<=99
131300030721     C                   Z-ADD     PiVal         VABZNC
131400030721     C                   ELSE
131500030721     C                   SETON                                        32
131600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131700030721     C                             + ' ' + 'VABZNC'
131800030721     C                   ENDIF
131900030721     C                   ENDIF
132000030721     C*** DCR
132100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132200030721     C                   EVAL      PiStr=SkSplitCSV(i)
132300030721     C                   EXSR      CHKNUM
132400030721     C                   IF        PiInt=*on       AND
132500030721     C                             PiVal<=99999999
132600030721     C                   Z-ADD     PiVal         VABDCR
132700030721     C                   ELSE
132800030721     C                   SETON                                        32
132900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133000030721     C                             + ' ' + 'VABDCR'
133100030721     C                   ENDIF
133200030721     C                   ENDIF
133300030721     C*** HCR
133400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
133500030721     C                   EVAL      PiStr=SkSplitCSV(i)
133600030721     C                   EXSR      CHKNUM
133700030721     C                   IF        PiInt=*on   AND
133800030721     C                             PiVal<=9999
133900030721     C                   Z-ADD     PiVal         VABHCR
134000030721     C                   ELSE
134100030721     C                   SETON                                        32
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABHCR'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600030721     C*** VMD
134700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
134800030721     C                   EVAL      PiStr=SkSplitCSV(i)
134900030721     C                   EXSR      CHKNUM
135000030721     C                   IF        PiNum=*on
135100030721     C                   Z-ADD     PiVal         VABVMD
135200030721     C                   ELSE
135300030721     C                   SETON                                        32
135400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135500030721     C                             + ' ' + 'VABVMD'
135600030721     C                   ENDIF
135700030721     C                   ENDIF
135800030721     C*** ANT
135900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136000030721     C                   EVAL      PiStr=SkSplitCSV(i)
136100030721     C                   EXSR      CHKNUM
136200030721     C                   IF        PiInt=*on        AND
136300030721     C                             PiVal<=999999999
136400030721     C                   Z-ADD     PiVal         VABANT
136500030721     C                   ELSE
136600030721     C                   SETON                                        32
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABANT'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100030721     C*** CMO
137200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137300030721     C                   EVAL      PiStr=SkSplitCSV(i)
137400030721     C                   EXSR      CHKNUM
137500030721     C                   IF        PiInt=*on
137600030721     C                   Z-ADD     PiVal         Num5_0
137700030721     C                   MOVEL(p)  Num5_0        VABCMO
137800030721     C                   ELSE
137900030721     C                   SETON                                        32
138000030721     C                   EVAL      VABCMO = *zeros
138100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138200030721     C                             + ' ' + 'VABCMO'
138300030721     C                   ENDIF
138400030721     C                   ENDIF
138500040510     C*
138600040510     C* Reperisco i campi del FIVAT
138700040510     C*** VATNOT
138800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
138900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139000040510     C                   ENDIF
139100040510     C*** VATTRC
139200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139400040510     C                   ENDIF
139500060331     C*** VATNOT_A
139600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
139700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
139800060331     C                   ENDIF
139900060331     C*** VATNOT_B
140000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140200060331     C                   ENDIF
140300060331     C*** VATNOT_E
140400070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
140500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
140600060331     C                   ENDIF
140700110927     C*** VATNOT_I
140800110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140900110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141000110927     C                   ENDIF
141100070823     C*
141200100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141300120301
141400120301      *** W_TEL + W_CEL = VABNT2
141500120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_TEL'
141600120301     C                   EVAL      VABNT2 = %trim(SkSplitCSV(i))
141700120301     C                   ENDIF
141800120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
141900120301     C                   EVAL      VABNT2 = %trim(VABNT2) + ' ' +
142000120301     C                                      %trim(SkSplitCSV(i))
142100120301     C                   ENDIF
142200120301      *** W_CAS se = 'CONTRASSEGNO' ok x il c/assegno
142300120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CAS'
142400120301     C                             and %trim(SkSplitCSV(i)) <> 'Contrassegno'
142500120301     C                   clear                   VABCAS
142600120301     C                   clear                   VABVCA
142700130227     C***                clear                   VABTIC
142800120301     C                   EVAL      wFlgCAS = '0'
142900120301     C                   ENDIF
143000120301      *** VATNOT_E da VABRMN
143100120301     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
143200120316     C                   eval      VATTRC = 'E'
143300120309      *** CAD
143400120309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
143500120309     C                   eval      VABCAD = %subst(SkSplitCSV(i):2)
143600120309     C                   ENDIF
143700140520
143800140520      *** W_CEL = VATNOT TRC=S
143900140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
144000140520     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
144100140520     C                   ENDIF
144200140520
144300140520      *** W_VATNOTIJ
144400140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
144500140520     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
144600140520     C                   ENDIF
144700070925     C*
144800071029     C***  <===  -----------------------------------------
144900040510     C*
145000030715     C                   ENDIF
145100030715     C                   EVAL      i = i + 1
145200030715     C                   ENDDO
145300070925     C*
145400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145500100831     C*
145600120316     C                   IF        wVATNOT_E <> *blanks
145700120316     C                   EVAL      VABCTM = '7Q'
145800120316     C                   ENDIF
145900120316     C*
146000120316     C                   EVAL      wVATNOT_B =  VABNT2
146100070925     C*
146200071029     C***  <===  -----------------------------------------
146300070925     C*
146400070925     C* Considerazioni finali su CBO/CAS
146500100318     C                   IF        wFlgCAS = '1'    OR
146600100318     C                             VABCAS  > *zeros
146700070925     C                   IF        VABCBO = '1'
146800070925     C                   EVAL      VABCBO = '4'
146900070925     C                   ENDIF
147000070925     C                   IF        VABCBO = '2'
147100070925     C                   EVAL      VABCBO = '6'
147200070925     C                   ENDIF
147300070925     C                   ENDIF
147400070925     C***  <===  ----------------------------
147500040510     C*
147600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147700040510     C                   IF        VABNSP = *zeros
147800040510     C                   SETON                                        31
147900040510     C                   ENDIF
148000040715     C*
148100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148200040715     C                   IF        VABLNP = *zeros
148300040715     C                   SETON                                        31
148400040715     C                   ENDIF
148500040510     C*
148600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148700040510     C                   IF        VABRMN = *zeros
148800040510     C                   Z-ADD     VABNSP        VABRMN
148900040510     C                   ENDIF
149000040510     C*
149100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149200040510     C                   IF        VABRMA = *blanks
149300040714     C                   MOVEL     VABRMN        VABRMA
149400040510     C                   ENDIF
149500040128     C*
149600040510     C* Se provincia nn valorizzata la reperisco
149700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149800110912     C                   IF        VABPRD =  *blanks AND
149900110912     C                             VABNZD =  *blanks AND
150000110912     C                             VABCAD <> *blanks AND
150100110912     C                             VABLOD <> *blanks
150200040128     C                   CLEAR                   TISI95DS
150300040128     C                   EVAL      I95TCN = '3'
150400040128     C                   Z-ADD     datcor        I95DAT
150500040128     C                   EVAL      I95NAR = VABNZD
150600040128     C                   EVAL      I95CAP = VABCAD
150700040128     C                   EVAL      I95LOC = VABLOD
150800040128     C                   CALL      'TISI95R'
150900040128     C                   PARM                    TISI95DS
151000040128     C                   EVAL      VABPRD = O95PRV
151100040510     C                   ENDIF
151200020204     C*
151300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151400020204     C                   EXSR      CHKIMPDIV
151500030715     C                   ENDIF
151600020204     C*
151700010607     C                   ENDSR
151800010607     C*----------------------------------------------------*
151900010601
152000020204
152100020204     C*----------------------------------------------------*
152200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152300020204     C*----------------------------------------------------*
152400020204     C     CHKIMPDIV     BEGSR
152500020204     C*
152600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152700020204     C                   Z-ADD     *zeros        wrkDec            9 9
152800020204     C*
152900020204     C* Come prima cosa effettuo considerazioni sulla divisa
153000020204     C                   IF        vabIAS > *zeros
153100020204     C                   IF        vabVAS <> 'EUR'
153200020204     C                   EVAL      vabVAS =  'ITL'
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C                   IF        vabCAS > *zeros
153700020204     C                   IF        vabVCA <> 'EUR'
153800020204     C                   EVAL      vabVCA =  'ITL'
153900020204     C                   ENDIF
154000020204     C                   ENDIF
154100020204     C*
154200020204     C                   IF        vabVMD > *zeros
154300020204     C                   IF        vabVAD <> 'EUR'
154400020204     C                   EVAL      vabVAD =  'ITL'
154500020204     C                   ENDIF
154600020204     C                   ENDIF
154700020204     C*
154800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154900020204     C                   Z-ADD     vabIAS        wrkDec
155000020204     C                   IF        wrkDec > *zeros
155100020204     C                   IF        vabVAS = 'ITL'
155200020204     C                   EVAL      vabIAS = *zeros
155300020204     C                   ENDIF
155400020204     C                   ENDIF
155500020204     C*
155600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155700020204     C                   Z-ADD     vabCAS        wrkDec
155800020204     C                   IF        wrkDec > *zeros
155900020204     C                   IF        vabVCA = 'ITL'
156000020204     C                   EVAL      vabCAS = *zeros
156100020204     C                   ENDIF
156200020204     C                   ENDIF
156300020204     C*
156400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156500020204     C                   Z-ADD     vabVMD        wrkDec
156600020204     C                   IF        wrkDec > *zeros
156700020204     C                   IF        vabVAD = 'ITL'
156800020204     C                   EVAL      vabVMD = *zeros
156900020204     C                   ENDIF
157000020204     C                   ENDIF
157100020204     C*
157200020204     C                   ENDSR
157300020204     C***
157400010330
157500100318
157600100318
157700100318     C*----------------------------------------------------*
157800100318     C*  CONTROLLO NUMERICITA' CAMPI
157900100318     C*----------------------------------------------------*
158000100318     C     CHKNUM        BEGSR
158100100318     C*
158200100318     C                   IF        PiDecChr = *blanks
158300100318     C                   EVAL      PiDecChr = CharNUM
158400100318     C                   ENDIF
158500100318     C*
158600100318     C                   callp     UBISNUM_Check(PiStr
158700100318     C                                          :PiDecChr
158800100318     C                                          :PiVal
158900100318     C                                          :PiNum
159000100318     C                                          :PiInt)
159100100318     C*
159200100318     C                   ENDSR
159300100318     C***
159400010601
159500010601
159600010601
159700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159800010601     C     repfil        BEGSR
159900010601     C*
160000010601     C                   if        invfil = *zeros and
160100010601     C                             depfil > *zeros and
160200010629     C                             (vinflg = *blanks or
160300010629     C                              vinflg = *zeros)
160400010601     C
160500010601     C                   eval      invfil = depfil
160600010601     C                   endif
160700010601     C*
160800010601     C                   if        depfil <> invfil and
160900010601     C                             invfil > *zeros
161000010601     C                   eval      flgMulti = '1'
161100010601     C                   if        vinflg = *blanks
161200010601     C                   add       1             cntNonEl
161300010601     C                   endif
161400010601     C                   endif
161500010601     C*
161600010601     C                   if        vinflg = '2'
161700010601     C                   eval      flgStato = '2'
161800010601     C                   endif
161900010601     C*
162000010601     C                   ENDSR
162100010601     C***
162200010601
162300010601
162400010601
162500010330
162600040119      /TITLE Invio dei dati al punto operativo.
162700040119     C     invio         BEGSR
162800040119     C*
162900040119     C* 1° invio FIVAT
163000040119     C                   reset                   dscmz
163100040119     C                   move      vlrpoi        cmzdst
163200040119     C                   eval      cmzfld = 'FIVATWWR'
163300040119     C                   eval      cmzmbd = vlrhdl
163400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163500040119     C***                if        prmfir = *blanks
163600040119     C                   eval      cmzfla = 'FIVAT00F'
163700040119     C                   eval      cmzmba = 'FIVAT00F'
163800040119     C***                else
163900040119     C***                eval      cmzfla = prmfir
164000040119     C***                eval      cmzmba = prmfir
164100040119     C***                endif
164200040119     C                   eval      cmznrr = *zeros
164300040119     C                   move      §ctrokvt      cmznrr
164400040119     C                   eval      cmzlba = vlrfl1
164500040119     C                   call(e)   'TIS711C'
164600040119     C                   parm                    dscmz
164700040119     C                   parm      *blanks       esito
164800040119     C                   if        %error
164900040119     C                             or cmzerr = '1'
165000040119     C                             or esito  = '1'
165100040119     C                   eval      wrkesito = '3'
165200040119     C                   else
165300040119     C*
165400040119     C* 2° invio FIVAB
165500040119     C                   reset                   dscmz
165600040119     C                   move      vlrpoi        cmzdst
165700040119     C                   eval      cmzfld = vlrfou
165800040119     C                   eval      cmzmbd = vlrhdl
165900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166000040119     C***                if        prmfir = *blanks
166100040119     C                   eval      cmzfla = 'FIVAB00F'
166200040119     C                   eval      cmzmba = 'FIVAB00F'
166300040119     C***                else
166400040119     C***                eval      cmzfla = prmfir
166500040119     C***                eval      cmzmba = prmfir
166600040119     C***                endif
166700040119     C                   eval      cmznrr = *zeros
166800040119     C                   move      §ctrokvb      cmznrr
166900040119     C                   eval      cmzlba = vlrfl1
167000040119     C                   call(e)   'TIS711C'
167100040119     C                   parm                    dscmz
167200040119     C                   parm      *blanks       esito
167300040119     C                   if        %error
167400040119     C                             or cmzerr = '1'
167500040119     C                             or esito  = '1'
167600040119     C                   eval      wrkesito = '3'
167700040119     C                   endif
167800040119     C                   endif
167900040119     C*
168000040119     C                   ENDSR
168100040119     C***
168200010601
168300010601
168400010601
168500010601
168600010601      /TITLE Invio dei dati al punto operativo.
168700010601     C     opeini        BEGSR
168800010601     C*
168900010601     C* Inizializzo flag e contatori operativi
169000010601     C                   movel     '0'           flgGiro           1
169100010601     C                   movel     '0'           flgMulti          1
169200010601     C                   movel     '1'           flgStato          1
169300010615     C                   movel     '0'           flgOk             1
169400010601     C                   z-add     *zeros        cntNonEl         10 0
169500010601     C                   z-add     *zeros        depfil            3 0
169600010601     C                   z-add     *zeros        invfil            3 0
169700010601     C*
169800010601     C                   ENDSR
169900010601     C***
170000070326
170100070326
170200070326
170300070326
170400070326     C     *pssr         BEGSR
170500070329     C*
170600070329     C                   if        %open(tivin00r)
170700070329     C                   close     tivin00r
170800070329     C                   endif
170900070329     C                   if        %open(fivabwwr)
171000070329     C                   close     fivabwwr
171100070329     C                   endif
171200070329     C                   if        %open(fivatwwr)
171300070329     C                   close     fivatwwr
171400070329     C                   endif
171500070326     C*
171600070326     C* Effettuo la chiamata al CLLE preposto
171700070326     C                   call(e)   'TITVVTC'
171800070326     C                   parm                    parccm
171900070326     C                   parm                    parmbr
172000070326     C                   parm      '2'           paropz
172100070326     C*
172200070326     C                   eval      wrkesito = '2'
172300070404     C*
172400070404     C                   seton                                        LR
172500070326     C*
172600070326     C                   ENDSR     '*CANCL'
172700070326     C***
172800100318
172900100318
173000100318
173100100318     C     normCSV       BEGSR
173200100318     C*
173300100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173400100318     C* separatori campi
173500100318     C                   if        CharTXT <> *blanks
173600100318     C                   z-add     *zeros        wLenVINDTA        4 0
173700100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173800100318     C                   setoff                                       80
173900100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174000100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174200100318     C                   if        not *in80
174300100318     C                   seton                                        80
174400100318     C                   else
174500100318     C                   setoff                                       80
174600100318     C                   endif
174700100318     C                   endif
174800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174900100318     C                             and *in80 = *on
175000100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175100100318     C                   endif
175200100318     C                   enddo
175300100318     C                   endif
175400100318     C*
175500100318     C                   ENDSR
175600100318     C***
175700070326
175800100326
175900100326
176000100326     C*----------------------------------------------------*
176100100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176200100326     C*----------------------------------------------------*
176300100326     C     REPNSP        BEGSR
176400100326     C*
176500100326     C* NSP => Stacco un numeratore da AZNUM
176600100326     C                   clear                   TRUL33DS
176700100326     C                   eval      I33OPE = *zeros
176800100326     C                   eval      I33CNU = 302
176900100326     C                   eval      I33NUM = 1
177000100326     C                   movel     TRUL33DS      KPJBU
177100100326     C                   call      'TRUL33R'
177200100326     C                   parm                    KPJBA
177300100326     C                   movel     KPJBU         TRUL33DS
177400100326     C                   if        O33ERR = *zeros
177500100326     C                   z-add     O33NRF        VABNSP
177600100326     C                   z-add     O33NRF        VATNSP
177700100326     C                   else
177800100326     C                   SETON                                        31
177900100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178000100326     C                             + ' ' + 'VABNSP VATNSP'
178100100326     C                   endif
178200100326     C*
178300100326     C                   ENDSR
178400010330
178500100326
178600010330
178700000613     C     *inzsr        BEGSR
178800990910     C*
178900990910     C     *entry        plist
179000990920     C                   parm                    tivlrds
179100990921     C                   parm      wrkesito      esito
179200000724     C                   parm                    prmlit
179300000710     C                   parm                    prmfir
179400010330     C*
179500010330     C* CALCOLA LA DATA CORRENTE
179600100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179700100318     C                   eval      datcor = %dec(%date() : *ISO)
179800000613     C*
179900000613     C                   ENDSR
180000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180100120309VABRMN;VABRMA;VABRD2;VABRSD;VABIND;VABLOD;W_VABCAD;VABPRD;W_TEL;W_CEL;
180200140520W_VATNOTIJ;X;VABCAS;W_CAS;VABNOT;VABTIC
180300120227
180400111116
180500100318
