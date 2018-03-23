000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
006000100831     D wVABRSD         s                   LIKE(VABRSD)
006100100831     D wVABIND         s                   LIKE(VABIND)
006200100831     D wVABCAD         s                   LIKE(VABCAD)
006300100831     D wVABLOD         s                   LIKE(VABLOD)
006400100831     D wVABPRD         s                   LIKE(VABPRD)
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
017200110912     C***                exsr      inzvar
017300110912     C***                exsr      impvab
017400110131     C*
017500110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017600110912 xxx C***                read      tivin00r                               70
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
041200060331     C                   if        wVATNOT_E <> *blanks
041300110201     C                   eval      VATNOT = wVATNOT_E
041400060331     C                   eval      VATTRC = 'E'
041500110201     C                   write     FIVAT000
041600110201     C                   add       1             §CTROKVT
041700110201     C                   endif
041800110927     C*
041900110927     C                   if        wVATNOT_I <> *blanks
042000110927     C                   eval      VATNOT = wVATNOT_I
042100110927     C                   eval      VATTRC = 'I'
042200110927     C                   write     FIVAT000
042300110927     C                   add       1             §CTROKVT
042400110927     C                   endif
042500040119     C*
042600040119     C                   ENDSR
042700040119
042800040119
042900040119
043000040119     C*----------------------------------------------------*
043100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043200040119     C*----------------------------------------------------*
043300040119     C     PREVAT        BEGSR
043400040119     C*
043500040119     C* Compongo il nome del membro da dare al FIVATWWR
043600040119     C                   eval      parmbr = vlrhdl
043700040119     C                   movel     'M'           parmbr
043800040510     C                   eval      parccm = vlrksc
043900040119     C                   eval      paropz = '1'
044000040119     C* Effettuo la chiamata al CLLE preposto
044100040119     C                   call(e)   'TITVVTC'
044200040119     C                   parm                    parccm
044300040119     C                   parm                    parmbr
044400040119     C                   parm                    paropz
044500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044600040119     C                   if        %error
044700040119     C                   movel     '1'           chkcall
044800040119     C                   else
044900040119     C                   movel     '0'           chkcall
045000040119     C                   endif
045100040119     C*
045200040119     C                   ENDSR
045300010601
045400010601
045500010601
045600010330     C*----------------------------------------------------*
045700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
045800010330     C*----------------------------------------------------*
045900040119     C     INZVAR        BEGSR
046000080527     C*
046100080527     C                   setoff                                       65
046200010330     C*
046300030822     C* Inizializzo variabili di wrk
046400020204     C                   Z-ADD     *zeros        Num5_0            5 0
046500101018     C                   MOVEL     *blanks       wFlgCAS
046600060331     C                   MOVEL     *blanks       wVATNOT_A
046700060331     C                   MOVEL     *blanks       wVATNOT_B
046800060331     C                   MOVEL     *blanks       wVATNOT_E
046900110927     C                   MOVEL     *blanks       wVATNOT_I
047000100831     C                   MOVEL     *blanks       wVABRSD
047100100831     C                   MOVEL     *blanks       wVABIND
047200100831     C                   MOVEL     *blanks       wVABCAD
047300100831     C                   MOVEL     *blanks       wVABLOD
047400100831     C                   MOVEL     *blanks       wVABPRD
047500030822     C*
047600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047700030822     C                   CLEAR                   SkSplitCSV
047800030822     C                   EVAL      i = 1
047900030822     C                   EVAL      posDa = *zeros
048000030822     C                   EVAL      posA  = *zeros
048100080527     C*
048200080527     C                   CLEAR                   FIVABDS
048300080527     C                   CLEAR                   FIVAT000
048400030822     C*
048500030822     C* Reimposto i valori di default
048600030822     C                   EXSR      DEFCAM
048700030822     C*
048800010330     C                   ENDSR
048900010330     C*----------------------------------------------------*
049000030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
049100010330     C*----------------------------------------------------*
049200010330     C     DEFCAM        BEGSR
049300030822     C*
049400030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049500030715     C* e delimitatore testo.
049600030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049700030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049800030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049900100318     C*
050000100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
050100100318     C                   EVAL      CharSOS = CharNUM
050200030822     C*
050300030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050400100720     C                   SETOFF                                       505152
050500100720     C                   SETOFF                                       53
050600100413     C                   SELECT
050700100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050800100720     C                   SETON                                        50
050900100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
051000100413     C                   SETON                                        51
051100100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
051200100720     C                   SETON                                        52
051300100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
051400100720     C                   SETON                                        53
051500100413     C                   ENDSL
051600030822     C*
051700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
051800030822     C                   EVAL      posDaDft = 1
051900030822     C                   EVAL      posADft  = 0
052000030822     C                   EVAL      wGiroDft = 0
052100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
052200030822     C                             posDaDft > 0
052300030822     C*
052400030822     C* Gestisco il 1° giro
052500030822     C                   IF        wGiroDft = 0
052600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
052700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
052800030822     C* Incremento il contatore dei "giri"
052900030822     C                   EVAL      wGiroDft = 1
053000030822     C                   ELSE
053100030822     C                   EVAL      posDaDft = posADft
053200030822     C                   ENDIF
053300030822     C* Eseguo lo scan x trovare la fine del campo corrente
053400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
053500030822     C*
053600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
053700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
053800030822     C                   IF        posDaDft > 0 AND
053900030822     C                             posADft  > 0
054000030822     C* NCL
054100030822     C                   IF        %subst(
054200030822     C                             %subst(vlrppt:posDaDft+1:
054300030822     C                             posADft-posDaDft-1):1:3)
054400030822     C                             = 'NCL'
054500030822     C                   EVAL      PiStr=%trim(%subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054800030822     C                   EXSR      CHKNUM
054900030822     C                   IF        PiInt=*on
055000030822     C                   Z-ADD     PiVal         VABNCL
055100030822     C                   ENDIF
055200030822     C                   ENDIF
055300030822     C* CCM
055400030822     C                   IF        %subst(
055500030822     C                             %subst(vlrppt:posDaDft+1:
055600030822     C                             posADft-posDaDft-1):1:3)
055700030822     C                             = 'CCM'
055800030822     C                   EVAL      PiStr=%trim(%subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056100030822     C                   EXSR      CHKNUM
056200030822     C                   IF        PiInt=*on
056300030822     C                   Z-ADD     PiVal         VABCCM
056400030822     C                   ENDIF
056500030822     C                   ENDIF
056600030822     C* LNP
056700030822     C                   IF        %subst(
056800030822     C                             %subst(vlrppt:posDaDft+1:
056900030822     C                             posADft-posDaDft-1):1:3)
057000030822     C                             = 'LNP'
057100030822     C                   EVAL      PiStr=%trim(%subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057400030822     C                   EXSR      CHKNUM
057500030822     C                   IF        PiInt=*on
057600030822     C                   Z-ADD     PiVal         VABLNP
057700030822     C                   ENDIF
057800030822     C                   ENDIF
057900030822     C* NRS
058000030822     C                   IF        %subst(
058100030822     C                             %subst(vlrppt:posDaDft+1:
058200030822     C                             posADft-posDaDft-1):1:3)
058300030822     C                             = 'NRS'
058400030822     C                   EVAL      PiStr=%trim(%subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058700030822     C                   EXSR      CHKNUM
058800030822     C                   IF        PiInt=*on
058900030822     C                   Z-ADD     PiVal         VABNRS
059000030822     C                   ENDIF
059100030822     C                   ENDIF
059200030822     C* CTR
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'CTR'
059700030822     C                   EVAL      PiStr=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   EXSR      CHKNUM
060100030822     C                   IF        PiInt=*on
060200030822     C                   Z-ADD     PiVal         VABCTR
060300030822     C                   ENDIF
060400030822     C                   ENDIF
060500030822     C* PKB
060600030822     C                   IF        %subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):1:3)
060900030822     C                             = 'PKB'
061000030822     C                   EVAL      PiStr=%trim(%subst(
061100030822     C                             %subst(vlrppt:posDaDft+1:
061200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061300030822     C                   EXSR      CHKNUM
061400030822     C                   IF        PiNum=*on
061500030822     C                   Z-ADD     PiVal         VABPKB
061600030822     C                   ENDIF
061700030822     C                   ENDIF
061800030822     C* VLB
061900030822     C                   IF        %subst(
062000030822     C                             %subst(vlrppt:posDaDft+1:
062100030822     C                             posADft-posDaDft-1):1:3)
062200030822     C                             = 'VLB'
062300030822     C                   EVAL      PiStr=%trim(%subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062600030822     C                   EXSR      CHKNUM
062700030822     C                   IF        PiNum=*on
062800030822     C                   Z-ADD     PiVal         VABVLB
062900030822     C                   ENDIF
063000030822     C                   ENDIF
063100030822     C* QFT
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'QFT'
063600030822     C                   EVAL      PiStr=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   EXSR      CHKNUM
064000030822     C                   IF        PiNum=*on
064100030822     C                   Z-ADD     PiVal         VABQFT
064200030822     C                   ENDIF
064300030822     C                   ENDIF
064400030822     C* CBO
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'CBO'
064900030822     C                   EVAL      VABCBO=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* TSP
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'TSP'
065800030822     C                   EVAL      VABTSP=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* VAS
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'VAS'
066700030822     C                   EVAL      VABVAS=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* VCA
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'VCA'
067600030822     C                   EVAL      VABVCA=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000070925     C* TIC
068100070925     C                   IF        %subst(
068200070925     C                             %subst(vlrppt:posDaDft+1:
068300070925     C                             posADft-posDaDft-1):1:3)
068400070925     C                             = 'TIC'
068500070925     C                   EVAL      VABTIC=%trim(%subst(
068600070925     C                             %subst(vlrppt:posDaDft+1:
068700070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800070925     C                   ENDIF
068900030822     C* GCA
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'GCA'
069400030822     C                   EVAL      VABGCA=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* CTM
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'CTM'
070300030822     C                   EVAL      VABCTM=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700030822     C* FFD
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'FFD'
071200030822     C                   EVAL      VABFFD=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* VAD
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'VAD'
072100030822     C                   EVAL      VABVAD=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* GMA
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'GMA'
073000030822     C                   EVAL      VABGMA=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* GGA
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'GGA'
073900030822     C                   EVAL      VABGGA=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* GVA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'GVA'
074800030822     C                   EVAL      VABGVA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* TC1
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'TC1'
075700030822     C                   EVAL      VABTC1=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* TC2
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'TC2'
076600030822     C                   EVAL      VABTC2=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000040714     C* VATTRC
077100040714     C                   IF        %subst(
077200040714     C                             %subst(vlrppt:posDaDft+1:
077300040714     C                             posADft-posDaDft-1):1:3)
077400040714     C                             = 'TRC'
077500040714     C                   EVAL      VATTRC=%trim(%subst(
077600040714     C                             %subst(vlrppt:posDaDft+1:
077700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800040714     C                   ENDIF
077900030822     C* ...
078000030822     C                   ENDIF
078100030822     C                   ENDDO
078200020204     C*
078300010330     C                   ENDSR
078400010607     C*----------------------------------------------------*
078500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
078600010607     C*----------------------------------------------------*
078700010607     C     IMPVAB        BEGSR
078800010607     C*
078900010607     C                   SETOFF                                       3132
079000110315 xxx C***                SETON                                        35
079100030515     C*
079200010607     C                   MOVEL     datcor        VABAAS
079300010607     C                   MOVE      datcor        VABMGS
079400010607     C*
079500030715     C**********
079600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
079700030715     C**********
079800030715     C                   IF        wGiro = *zeros
079900040113     C*
080000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
080100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
080200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
080300080429     C                             %trim(intcol(5))<>*blanks
080400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
080500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
080600080429     C                                      %trim(intcol(5))
080700070823     C     ';':CharCSV   XLATE     vindta        vindta
080800040510     C                   ENDIF
080900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081000031201     C     minu:maiu     XLATE     vindta        vindta
081100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081200030715     C                   DOW       posDa <= %len(%trim(vindta))
081300030715     C*
081400030715     C* Gestisco il 1° campo
081500030715     C                   IF        i = 1
081600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081700030715     C                   EVAL      posDa = 1
081800030715     C* Eseguo lo scan x trovare la fine del primo campo
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C* X i campi successivi al 1°
082400030715     C                   ELSE
082500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082600030715     C                   EVAL      posDa = posA + 1
082700030715     C* Eseguo lo scan x trovare la fine del campo corrente
082800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082900030715     C* Gestisco l'ultimo campo
083000030715     C                   IF        posA = *zeros
083100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083200030715     C* A questo punto "estraggo" il campo corrente
083300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083400030715     C                                                    (posA-posDa))
083500030715     C                   LEAVE
083600030715     C                   ELSE
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   ENDIF
084100030715     C                   ENDIF
084200030715     C* Incremento il contatore d campo
084300030715     C                   EVAL      i = i +1
084400030715     C                   ENDDO
084500030715     C*
084600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084700030715     C* delimitazione testo
084800030715     C                   EVAL      i = 1
084900030715     C                   DOW       i < %elem(SkSplitFLD)
085000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085200030715     C                   EVAL      i = i + 1
085300030715     C                   ENDDO
085400030715     C                   EVAL      wGiro = 1
085500030715     C                   ELSE
085600030715     C                   EVAL      wGiro = 2
085700031204     C**********
085800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
085900031204     C* d separatore campo
086000031204     C**********
086100031204     C                   EVAL      vindta = %trim(vindta)
086200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
086300031204     C                   EVAL      vindta = %subst(vindta:2)
086400031204     C                   ENDDO
086500031204     C*
086600031204     C                   Z-ADD     *zeros        lunghInput        4 0
086700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
086900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087100110912     C                   ENDDO
087200030715     C**********
087300030715     C* Effettuo lo split del campo dati d input
087400030715     C**********
087500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
087600030715     C                   DOW       posDa <= %len(%trim(vindta))
087700030715     C*
087800030715     C* Gestisco il 1° campo
087900030715     C                   IF        i = 1
088000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088100030715     C                   EVAL      posDa = 1
088200030715     C* Eseguo lo scan x trovare la fine del primo campo
088300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088400030715     C* A questo punto "estraggo" il campo corrente
088500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088600030715     C                                                    (posA-posDa))
088700030715     C* X i campi successivi al 1°
088800030715     C                   ELSE
088900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089000030715     C                   EVAL      posDa = posA + 1
089100031204     C* Verifico che nn vi sia il campo nn valorizzato
089200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
089300031204     C* Se campo nn valorizzato skippo al prossimo
089400031204     C                   EVAL      SkSplitCSV(i) = *blanks
089500031204     C                   EVAL      posA  = posA + 1
089600031204     C                   ELSE
089700030715     C* Eseguo lo scan x trovare la fine del campo corrente
089800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089900030715     C* Gestisco l'ultimo campo
090000030715     C                   IF        posA = *zeros
090100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C                   LEAVE
090600030715     C                   ELSE
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C                   ENDIF
091100031204     C                   ENDIF
091200030715     C                   ENDIF
091300030715     C* Incremento il contatore d campo
091400030715     C                   EVAL      i = i +1
091500030715     C                   ENDDO
091600030715     C*
091700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
091800030715     C* delimitazione testo
091900030715     C                   EVAL      i = 1
092000030715     C                   DOW       i < %elem(SkSplitCSV)
092100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
092300030715     C                   EVAL      i = i + 1
092400030715     C                   ENDDO
092500100720     C*
092600100720     C* Stacco un progressivo x numero spedizione
092700110912 xxx C***                EXSR      repNSP
092800100326     C*
092900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093000030715     C                   EVAL      i = 1
093100030715     C                   DOW       i < %elem(SkSplitFLD)
093200030715     C*
093300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093500030715     C                             SkSplitCSV(i) <> *blanks
093600030721     C*** ATB
093700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
093800030721     C                   EVAL      VABATB = SkSplitCSV(i)
093900030721     C                   ENDIF
094000030715     C*** RSD
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094500030715     C* ==
094600030715     C                   ENDIF
094700030721     C*** RD2
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
094900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030715     C*** IND
095200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095300030715     C                   EVAL      VABIND = SkSplitCSV(i)
095400030715     C                   ENDIF
095500030715     C*** LOD
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
095700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** PRD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030721     C*** NZD
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** RMA
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
096900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097000030715     C                   ENDIF
097100030715     C*** NOT
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NT2
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
097700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** VCA
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VAS
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** FFD
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
098900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030721     C*** GC1
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** GC2
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
099700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** TSP
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
100100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
100200030721     C                   ENDIF
100300040510     C*** CBO
100400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
100500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
100600040510     C                   ENDIF
100700030721     C*** NAS
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
100900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** TIC
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
101300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** GCA
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
101700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** XCO
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
102100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** CTM
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
102500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** TCR
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
102900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** CTS
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
103300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** FTM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
103700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** VAD
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
104100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** GMA
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
104500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** GGA
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
104900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** GVA
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
105300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** TC1
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
105700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** TC2
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
106100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** SCL
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
106500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** RMO
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
106900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** NMO
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
107300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*
107600030721     C* Reperisco quindi i campi numerici...
107700040510     C*** NSP
107800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
107900040510     C                   EVAL      PiStr=SkSplitCSV(i)
108000040510     C                   EXSR      CHKNUM
108100040510     C                   IF        PiInt=*on
108200040510     C                   Z-ADD     PiVal         VABNSP
108300040510     C                   ELSE
108400040510     C                   SETON                                        31
108500040510     C                   EVAL      VABNSP = *zeros
108600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
108700040510     C                             + ' ' + 'VABNSP'
108800040510     C                   ENDIF
108900040510     C                   ENDIF
109000071106     C*** RMN
109100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
109200071106     C                   EVAL      PiStr=SkSplitCSV(i)
109300071106     C                   EXSR      CHKNUM
109400071106     C                   IF        PiInt=*on
109500071106     C                   Z-ADD     PiVal         VABRMN
109600071106     C                   ELSE
109700071106     C                   SETON                                        32
109800071106     C                   EVAL      VABRMN = *zeros
109900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
110000071106     C                             + ' ' + 'VABRMN'
110100071106     C                   ENDIF
110200071106     C                   ENDIF
110300030721     C*** CAD
110400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
110500030721     C                   EVAL      PiStr=SkSplitCSV(i)
110600030721     C                   EXSR      CHKNUM
110700030721     C                   IF        PiInt=*on
110800030721     C                   Z-ADD     PiVal         Num5_0
110900030721     C                   MOVEL(p)  Num5_0        VABCAD
111000030721     C                   ELSE
111100030721     C                   SETON                                        32
111200030721     C                   EVAL      VABCAD = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400030721     C                             + ' ' + 'VABCAD'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** NCL
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on
112200030721     C                   Z-ADD     PiVal         VABNCL
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      VABNCL = *zeros
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040122     C                             + ' ' + 'VABNCL'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** PKB
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiNum=*on
113500030721     C                   Z-ADD     PiVal         VABPKB
113600030721     C                   ELSE
113700030721     C                   SETON                                        32
113800030721     C                   EVAL      VABPKB = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000030721     C                             + ' ' + 'VABPKB'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** CAS
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114500110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
114600110506     C                             %trim(SkSplitCSV(i)) <> *zeros
114700101018     C                   EVAL      wFlgCAS = '1'
114800101018     C                   ENDIF
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiNum=*on
115200030721     C                   Z-ADD     PiVal         VABCAS
115300030721     C                   ELSE
115400030721     C                   SETON                                        32
115500030721     C                   EVAL      VABCAS = *zeros
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABCAS'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** CCM
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiInt=*on      AND
116500030721     C                             PiVal<=9999999 AND
116600030721     C                             PiVal>*zeros
116700030721     C                   Z-ADD     PiVal         VABCCM
116800030721     C                   ELSE
116900030721     C                   SETON                                        32
117000030721     C                   EVAL      VABCCM = *zeros
117100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117200040510     C                             + ' ' + 'VABCCM'
117300030721     C                   ENDIF
117400030721     C                   ENDIF
117500030721     C*** LNP
117600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117700030721     C                   EVAL      PiStr=SkSplitCSV(i)
117800030721     C                   EXSR      CHKNUM
117900030721     C                   IF        PiInt=*on    AND
118000030721     C                             PiVal<=999   AND
118100030721     C                             PiVal>*zeros
118200030721     C                   Z-ADD     PiVal         VABLNP
118300030721     C                   ELSE
118400040715     C                   SETON                                        32
118500030721     C                   EVAL      VABLNP = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700040510     C                             + ' ' + 'VABLNP'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** NRS
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on AND
119500030721     C                             PiVal<=99
119600030721     C                   Z-ADD     PiVal         VABNRS
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120000030721     C                             + ' ' + 'VABNRS'
120100030721     C                   ENDIF
120200030721     C                   ENDIF
120300030721     C*** LNA
120400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120500030721     C                   EVAL      PiStr=SkSplitCSV(i)
120600030721     C                   EXSR      CHKNUM
120700030721     C                   IF        PiInt=*on    AND
120800030721     C                             PiVal<=999   AND
120900030721     C                             PiVal>*zeros
121000030721     C                   Z-ADD     PiVal         VABLNA
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      VABLNA = *zeros
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABLNA'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** CTR
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on  AND
122300030721     C                             PiVal<=999
122400030721     C                   Z-ADD     PiVal         VABCTR
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABCTR'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** IAS
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiNum=*on
123600030721     C                   Z-ADD     PiVal         VABIAS
123700030721     C                   ELSE
123800030721     C                   SETON                                        32
123900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124000030721     C                             + ' ' + 'VABIAS'
124100030721     C                   ENDIF
124200030721     C                   ENDIF
124300030721     C*** VLB
124400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124500030721     C                   EVAL      PiStr=SkSplitCSV(i)
124600030721     C                   EXSR      CHKNUM
124700030721     C                   IF        PiNum=*on
124800030721     C                   Z-ADD     PiVal         VABVLB
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABVLB'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** QFT
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiNum=*on
126000030721     C                   Z-ADD     PiVal         VABQFT
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABQFT'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** NCD
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on      AND
127200030721     C                             PiVal<=9999999
127300030721     C                   Z-ADD     PiVal         VABNCD
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABNCD'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** NCA
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on      AND
128500030721     C                             PiVal<=9999999
128600030721     C                   Z-ADD     PiVal         VABNCA
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABNCA'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** ZNC
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiInt=*on AND
129800030721     C                             PiVal<=99
129900030721     C                   Z-ADD     PiVal         VABZNC
130000030721     C                   ELSE
130100030721     C                   SETON                                        32
130200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130300030721     C                             + ' ' + 'VABZNC'
130400030721     C                   ENDIF
130500030721     C                   ENDIF
130600030721     C*** DCR
130700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130800030721     C                   EVAL      PiStr=SkSplitCSV(i)
130900030721     C                   EXSR      CHKNUM
131000030721     C                   IF        PiInt=*on       AND
131100030721     C                             PiVal<=99999999
131200030721     C                   Z-ADD     PiVal         VABDCR
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABDCR'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** HCR
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on   AND
132400030721     C                             PiVal<=9999
132500030721     C                   Z-ADD     PiVal         VABHCR
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABHCR'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** VMD
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiNum=*on
133700030721     C                   Z-ADD     PiVal         VABVMD
133800030721     C                   ELSE
133900030721     C                   SETON                                        32
134000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134100030721     C                             + ' ' + 'VABVMD'
134200030721     C                   ENDIF
134300030721     C                   ENDIF
134400030721     C*** ANT
134500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134600030721     C                   EVAL      PiStr=SkSplitCSV(i)
134700030721     C                   EXSR      CHKNUM
134800030721     C                   IF        PiInt=*on        AND
134900030721     C                             PiVal<=999999999
135000030721     C                   Z-ADD     PiVal         VABANT
135100030721     C                   ELSE
135200030721     C                   SETON                                        32
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABANT'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700030721     C*** CMO
135800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135900030721     C                   EVAL      PiStr=SkSplitCSV(i)
136000030721     C                   EXSR      CHKNUM
136100030721     C                   IF        PiInt=*on
136200030721     C                   Z-ADD     PiVal         Num5_0
136300030721     C                   MOVEL(p)  Num5_0        VABCMO
136400030721     C                   ELSE
136500030721     C                   SETON                                        32
136600030721     C                   EVAL      VABCMO = *zeros
136700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136800030721     C                             + ' ' + 'VABCMO'
136900030721     C                   ENDIF
137000030721     C                   ENDIF
137100040510     C*
137200040510     C* Reperisco i campi del FIVAT
137300040510     C*** VATNOT
137400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137600040510     C                   ENDIF
137700040510     C*** VATTRC
137800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
138000040510     C                   ENDIF
138100060331     C*** VATNOT_A
138200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138400060331     C                   ENDIF
138500060331     C*** VATNOT_B
138600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138800060331     C                   ENDIF
138900060331     C*** VATNOT_E
139000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
139100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139200060331     C                   ENDIF
139300110927     C*** VATNOT_I
139400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
139500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
139600110927     C                   ENDIF
139700070823     C*
139800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
139900110421     C*
140000110912     C*** RMN / RMA / NSP
140100110912     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
140200130218     C* se in questa colonna il primo carattere è % va omesso
140300130218     C                   IF        %subst(SkSplitCSV(i) : 1 : 1) = '%'
140400130218     C                   EVAL      %subst(SkSplitCSV(i) : 1 : 1) = *blank
140500130218     C                   EVAL      SkSplitCSV(i) = %triml(SkSplitCSV(i))
140600130218     C                   ENDIF
140700110912     C                   EVAL      VABRMA=SkSplitCSV(i)
140800110912     C                   EVAL      PiStr=SkSplitCSV(i)
140900110912     C                   EXSR      CHKNUM
141000110912     C                   IF        PiInt=*on
141100110912     C                   Z-ADD     PiVal         VABNSP
141200110912     C                   Z-ADD     PiVal         VABRMN
141300110912     C                   ELSE
141400110912     C                   SETON                                        31
141500110912     C                   EVAL      VABNSP = *zeros
141600110912     C                   EVAL      VABRMN = 1
141700110912     C                   EVAL      vinmsg = %trimr(vinmsg)
141800110912     C                             + ' ' + 'VABNSP VABRMN'
141900110912     C                   ENDIF
142000110415     C                   ENDIF
142100070925     C*
142200071029     C***  <===  -----------------------------------------
142300040510     C*
142400030715     C                   ENDIF
142500030715     C                   EVAL      i = i + 1
142600030715     C                   ENDDO
142700070925     C*
142800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142900100831     C*
143000100831     C                   IF        wVABRSD <> *blanks
143100100831     C                   EVAL      VABRSD = wVABRSD
143200100831     C                   ENDIF
143300100831     C*
143400100831     C                   IF        wVABIND <> *blanks
143500100831     C                   EVAL      VABIND = wVABIND
143600100831     C                   ENDIF
143700100831     C*
143800100831     C                   IF        wVABCAD <> *blanks
143900100831     C                   EVAL      VABCAD = wVABCAD
144000100831     C                   ENDIF
144100100831     C*
144200100831     C                   IF        wVABLOD <> *blanks
144300100831     C                   EVAL      VABLOD = wVABLOD
144400100831     C                   ENDIF
144500100831     C*
144600100831     C                   IF        wVABPRD <> *blanks
144700100831     C                   EVAL      VABPRD = wVABPRD
144800100831     C                   ENDIF
144900070925     C*
145000071029     C***  <===  -----------------------------------------
145100070925     C*
145200070925     C* Considerazioni finali su CBO/CAS
145300100318     C                   IF        wFlgCAS = '1'    OR
145400100318     C                             VABCAS  > *zeros
145500070925     C                   IF        VABCBO = '1'
145600070925     C                   EVAL      VABCBO = '4'
145700070925     C                   ENDIF
145800070925     C                   IF        VABCBO = '2'
145900070925     C                   EVAL      VABCBO = '6'
146000070925     C                   ENDIF
146100070925     C                   ENDIF
146200070925     C***  <===  ----------------------------
146300040510     C*
146400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
146500040510     C                   IF        VABNSP = *zeros
146600040510     C                   SETON                                        31
146700040510     C                   ENDIF
146800040715     C*
146900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147000040715     C                   IF        VABLNP = *zeros
147100040715     C                   SETON                                        31
147200040715     C                   ENDIF
147300040510     C*
147400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
147500040510     C                   IF        VABRMN = *zeros
147600040510     C                   Z-ADD     VABNSP        VABRMN
147700040510     C                   ENDIF
147800040510     C*
147900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148000040510     C                   IF        VABRMA = *blanks
148100040714     C                   MOVEL     VABRMN        VABRMA
148200040510     C                   ENDIF
148300040128     C*
148400040510     C* Se provincia nn valorizzata la reperisco
148500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
148600110912     C                   IF        VABPRD =  *blanks AND
148700110912     C                             VABNZD =  *blanks AND
148800110912     C                             VABCAD <> *blanks AND
148900110912     C                             VABLOD <> *blanks
149000040128     C                   CLEAR                   TISI95DS
149100040128     C                   EVAL      I95TCN = '3'
149200040128     C                   Z-ADD     datcor        I95DAT
149300040128     C                   EVAL      I95NAR = VABNZD
149400040128     C                   EVAL      I95CAP = VABCAD
149500040128     C                   EVAL      I95LOC = VABLOD
149600040128     C                   CALL      'TISI95R'
149700040128     C                   PARM                    TISI95DS
149800040128     C                   EVAL      VABPRD = O95PRV
149900040510     C                   ENDIF
150000020204     C*
150100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
150200020204     C                   EXSR      CHKIMPDIV
150300030715     C                   ENDIF
150400020204     C*
150500010607     C                   ENDSR
150600010607     C*----------------------------------------------------*
150700010601
150800020204
150900020204     C*----------------------------------------------------*
151000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151100020204     C*----------------------------------------------------*
151200020204     C     CHKIMPDIV     BEGSR
151300020204     C*
151400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
151500020204     C                   Z-ADD     *zeros        wrkDec            9 9
151600020204     C*
151700020204     C* Come prima cosa effettuo considerazioni sulla divisa
151800020204     C                   IF        vabIAS > *zeros
151900020204     C                   IF        vabVAS <> 'EUR'
152000020204     C                   EVAL      vabVAS =  'ITL'
152100020204     C                   ENDIF
152200020204     C                   ENDIF
152300020204     C*
152400020204     C                   IF        vabCAS > *zeros
152500020204     C                   IF        vabVCA <> 'EUR'
152600020204     C                   EVAL      vabVCA =  'ITL'
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C                   IF        vabVMD > *zeros
153100020204     C                   IF        vabVAD <> 'EUR'
153200020204     C                   EVAL      vabVAD =  'ITL'
153300020204     C                   ENDIF
153400020204     C                   ENDIF
153500020204     C*
153600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153700020204     C                   Z-ADD     vabIAS        wrkDec
153800020204     C                   IF        wrkDec > *zeros
153900020204     C                   IF        vabVAS = 'ITL'
154000020204     C                   EVAL      vabIAS = *zeros
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
154500020204     C                   Z-ADD     vabCAS        wrkDec
154600020204     C                   IF        wrkDec > *zeros
154700020204     C                   IF        vabVCA = 'ITL'
154800020204     C                   EVAL      vabCAS = *zeros
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
155300020204     C                   Z-ADD     vabVMD        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVAD = 'ITL'
155600020204     C                   EVAL      vabVMD = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C                   ENDSR
156100020204     C***
156200010330
156300100318
156400100318
156500100318     C*----------------------------------------------------*
156600100318     C*  CONTROLLO NUMERICITA' CAMPI
156700100318     C*----------------------------------------------------*
156800100318     C     CHKNUM        BEGSR
156900100318     C*
157000100318     C                   IF        PiDecChr = *blanks
157100100318     C                   EVAL      PiDecChr = CharNUM
157200100318     C                   ENDIF
157300100318     C*
157400100318     C                   callp     UBISNUM_Check(PiStr
157500100318     C                                          :PiDecChr
157600100318     C                                          :PiVal
157700100318     C                                          :PiNum
157800100318     C                                          :PiInt)
157900100318     C*
158000100318     C                   ENDSR
158100100318     C***
158200010601
158300010601
158400010601
158500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158600010601     C     repfil        BEGSR
158700010601     C*
158800010601     C                   if        invfil = *zeros and
158900010601     C                             depfil > *zeros and
159000010629     C                             (vinflg = *blanks or
159100010629     C                              vinflg = *zeros)
159200010601     C
159300010601     C                   eval      invfil = depfil
159400010601     C                   endif
159500010601     C*
159600010601     C                   if        depfil <> invfil and
159700010601     C                             invfil > *zeros
159800010601     C                   eval      flgMulti = '1'
159900010601     C                   if        vinflg = *blanks
160000010601     C                   add       1             cntNonEl
160100010601     C                   endif
160200010601     C                   endif
160300010601     C*
160400010601     C                   if        vinflg = '2'
160500010601     C                   eval      flgStato = '2'
160600010601     C                   endif
160700010601     C*
160800010601     C                   ENDSR
160900010601     C***
161000010601
161100010601
161200010601
161300010330
161400040119      /TITLE Invio dei dati al punto operativo.
161500040119     C     invio         BEGSR
161600040119     C*
161700040119     C* 1° invio FIVAT
161800040119     C                   reset                   dscmz
161900040119     C                   move      vlrpoi        cmzdst
162000040119     C                   eval      cmzfld = 'FIVATWWR'
162100040119     C                   eval      cmzmbd = vlrhdl
162200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162300040119     C***                if        prmfir = *blanks
162400040119     C                   eval      cmzfla = 'FIVAT00F'
162500040119     C                   eval      cmzmba = 'FIVAT00F'
162600040119     C***                else
162700040119     C***                eval      cmzfla = prmfir
162800040119     C***                eval      cmzmba = prmfir
162900040119     C***                endif
163000040119     C                   eval      cmznrr = *zeros
163100040119     C                   move      §ctrokvt      cmznrr
163200040119     C                   eval      cmzlba = vlrfl1
163300040119     C                   call(e)   'TIS711C'
163400040119     C                   parm                    dscmz
163500040119     C                   parm      *blanks       esito
163600040119     C                   if        %error
163700040119     C                             or cmzerr = '1'
163800040119     C                             or esito  = '1'
163900040119     C                   eval      wrkesito = '3'
164000040119     C                   else
164100040119     C*
164200040119     C* 2° invio FIVAB
164300040119     C                   reset                   dscmz
164400040119     C                   move      vlrpoi        cmzdst
164500040119     C                   eval      cmzfld = vlrfou
164600040119     C                   eval      cmzmbd = vlrhdl
164700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164800040119     C***                if        prmfir = *blanks
164900040119     C                   eval      cmzfla = 'FIVAB00F'
165000040119     C                   eval      cmzmba = 'FIVAB00F'
165100040119     C***                else
165200040119     C***                eval      cmzfla = prmfir
165300040119     C***                eval      cmzmba = prmfir
165400040119     C***                endif
165500040119     C                   eval      cmznrr = *zeros
165600040119     C                   move      §ctrokvb      cmznrr
165700040119     C                   eval      cmzlba = vlrfl1
165800040119     C                   call(e)   'TIS711C'
165900040119     C                   parm                    dscmz
166000040119     C                   parm      *blanks       esito
166100040119     C                   if        %error
166200040119     C                             or cmzerr = '1'
166300040119     C                             or esito  = '1'
166400040119     C                   eval      wrkesito = '3'
166500040119     C                   endif
166600040119     C                   endif
166700040119     C*
166800040119     C                   ENDSR
166900040119     C***
167000010601
167100010601
167200010601
167300010601
167400010601      /TITLE Invio dei dati al punto operativo.
167500010601     C     opeini        BEGSR
167600010601     C*
167700010601     C* Inizializzo flag e contatori operativi
167800010601     C                   movel     '0'           flgGiro           1
167900010601     C                   movel     '0'           flgMulti          1
168000010601     C                   movel     '1'           flgStato          1
168100010615     C                   movel     '0'           flgOk             1
168200010601     C                   z-add     *zeros        cntNonEl         10 0
168300010601     C                   z-add     *zeros        depfil            3 0
168400010601     C                   z-add     *zeros        invfil            3 0
168500010601     C*
168600010601     C                   ENDSR
168700010601     C***
168800070326
168900070326
169000070326
169100070326
169200070326     C     *pssr         BEGSR
169300070329     C*
169400070329     C                   if        %open(tivin00r)
169500070329     C                   close     tivin00r
169600070329     C                   endif
169700070329     C                   if        %open(fivabwwr)
169800070329     C                   close     fivabwwr
169900070329     C                   endif
170000070329     C                   if        %open(fivatwwr)
170100070329     C                   close     fivatwwr
170200070329     C                   endif
170300070326     C*
170400070326     C* Effettuo la chiamata al CLLE preposto
170500070326     C                   call(e)   'TITVVTC'
170600070326     C                   parm                    parccm
170700070326     C                   parm                    parmbr
170800070326     C                   parm      '2'           paropz
170900070326     C*
171000070326     C                   eval      wrkesito = '2'
171100070404     C*
171200070404     C                   seton                                        LR
171300070326     C*
171400070326     C                   ENDSR     '*CANCL'
171500070326     C***
171600100318
171700100318
171800100318
171900100318     C     normCSV       BEGSR
172000100318     C*
172100100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
172200100318     C* separatori campi
172300100318     C                   if        CharTXT <> *blanks
172400100318     C                   z-add     *zeros        wLenVINDTA        4 0
172500100318     C                   z-add     *zeros        wFlgVINDTA        1 0
172600100318     C                   setoff                                       80
172700100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172800100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173000100318     C                   if        not *in80
173100100318     C                   seton                                        80
173200100318     C                   else
173300100318     C                   setoff                                       80
173400100318     C                   endif
173500100318     C                   endif
173600100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173700100318     C                             and *in80 = *on
173800100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173900100318     C                   endif
174000100318     C                   enddo
174100100318     C                   endif
174200100318     C*
174300100318     C                   ENDSR
174400100318     C***
174500070326
174600100326
174700100326
174800100326     C*----------------------------------------------------*
174900100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175000100326     C*----------------------------------------------------*
175100100326     C     REPNSP        BEGSR
175200100326     C*
175300100326     C* NSP => Stacco un numeratore da AZNUM
175400100326     C                   clear                   TRUL33DS
175500100326     C                   eval      I33OPE = *zeros
175600100326     C                   eval      I33CNU = 302
175700100326     C                   eval      I33NUM = 1
175800100326     C                   movel     TRUL33DS      KPJBU
175900100326     C                   call      'TRUL33R'
176000100326     C                   parm                    KPJBA
176100100326     C                   movel     KPJBU         TRUL33DS
176200100326     C                   if        O33ERR = *zeros
176300100326     C                   z-add     O33NRF        VABNSP
176400100326     C                   z-add     O33NRF        VATNSP
176500100326     C                   else
176600100326     C                   SETON                                        31
176700100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176800100326     C                             + ' ' + 'VABNSP VATNSP'
176900100326     C                   endif
177000100326     C*
177100100326     C                   ENDSR
177200010330
177300100326
177400010330
177500000613     C     *inzsr        BEGSR
177600990910     C*
177700990910     C     *entry        plist
177800990920     C                   parm                    tivlrds
177900990921     C                   parm      wrkesito      esito
178000000724     C                   parm                    prmlit
178100000710     C                   parm                    prmfir
178200010330     C*
178300010330     C* CALCOLA LA DATA CORRENTE
178400100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
178500100318     C                   eval      datcor = %dec(%date() : *ISO)
178600000613     C*
178700000613     C                   ENDSR
178800000613     C***
178900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179000110912W_VABRMN;VABRSD;VABIND;VABRD2;VABLOD;X;VABCAD;X;VABNCL;VABPKB;X;X;X;X;
179100110912X;X;X;X;VATNOT_I;X;VATNOT_B;VATNOT_A;VATNOT_E;X;
179200110315
179300110315
179400100318
