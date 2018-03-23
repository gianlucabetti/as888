000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130121     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
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
004200110623     D*------------------
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
017200110623     C***                exsr      inzvar
017300110623     C***                exsr      impvab
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
041200060331     C                   if        wVATNOT_E <> *blanks
041300110623     C***                eval      VATNOT = wVATNOT_E
041400110623     C***                eval      VATTRC = 'E'
041500110623     C***                write     FIVAT000
041600110623     C***                add       1             §CTROKVT
041700110623     C*
041800110623     C* Sviluppati i "CHI SONO" in relazione al numero colli
041900110623     C*
042000110623     C* Inizializzo progressivo collo su totale colli della spedizione
042100110623     C                   Z-ADD     1             wNumColloDA       5 0
042200110623     C                   MOVEL     *blanks       wNumColloCor      5
042300110623     C                   MOVEL     *blanks       wNumWhoAre        7
042400110623     C*
042500110623     C                   DOW       wNumColloDA <= VABNCL
042600110623     C                   MOVEL(P)  wNumColloDA   wNumColloCor
042700110623     C                   MOVEL(P)  VABNSP        wNumWhoAre
042800110623     C                   EVAL      VATNOT = wNumWhoAre + wNumColloCor
042900110623     C*
043000110623     C* Calcolo in check-digits EAN13
043100110623     C                   clear                   TRUL28DSX
043200110623     C                   eval      I284STD = 'E13'
043300110623     C                   eval      I284COD = VATNOT
043400110623     C                   call      'TRUL28R4'
043500110623     C                   parm                    TRUL28DSX
043600110623     C*
043700110623     C* Se ok => valorizzo "CHI SONO", altrimenti nn effettuo nemmeno la scrittura
043800110623     C                   if        O284err = *blanks
043900110623     C                   eval      VATNOT = %trim(O284COD)
044000110623     C                   eval      VATTRC = 'E'
044100110623     C                   write     FIVAT000
044200110623     C                   add       1             §CTROKVT
044300110623     C                   endif
044400110623     C*
044500110623     C                   EVAL      wNumColloDA = wNumColloDA + 1
044600110623     C                   ENDDO
044700040119     C*
044800110623     C                   endif
044900110623     C*
045000040119     C                   ENDSR
045100040119
045200040119
045300040119
045400040119     C*----------------------------------------------------*
045500040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045600040119     C*----------------------------------------------------*
045700040119     C     PREVAT        BEGSR
045800040119     C*
045900040119     C* Compongo il nome del membro da dare al FIVATWWR
046000040119     C                   eval      parmbr = vlrhdl
046100040119     C                   movel     'M'           parmbr
046200040510     C                   eval      parccm = vlrksc
046300040119     C                   eval      paropz = '1'
046400040119     C* Effettuo la chiamata al CLLE preposto
046500040119     C                   call(e)   'TITVVTC'
046600040119     C                   parm                    parccm
046700040119     C                   parm                    parmbr
046800040119     C                   parm                    paropz
046900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047000040119     C                   if        %error
047100040119     C                   movel     '1'           chkcall
047200040119     C                   else
047300040119     C                   movel     '0'           chkcall
047400040119     C                   endif
047500040119     C*
047600040119     C                   ENDSR
047700010601
047800010601
047900010601
048000010330     C*----------------------------------------------------*
048100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048200010330     C*----------------------------------------------------*
048300040119     C     INZVAR        BEGSR
048400080527     C*
048500080527     C                   setoff                                       65
048600010330     C*
048700030822     C* Inizializzo variabili di wrk
048800020204     C                   Z-ADD     *zeros        Num5_0            5 0
048900101018     C                   MOVEL     *blanks       wFlgCAS
049000060331     C                   MOVEL     *blanks       wVATNOT_A
049100060331     C                   MOVEL     *blanks       wVATNOT_B
049200060331     C                   MOVEL     *blanks       wVATNOT_E
049300100831     C                   MOVEL     *blanks       wVABRSD
049400100831     C                   MOVEL     *blanks       wVABIND
049500100831     C                   MOVEL     *blanks       wVABCAD
049600100831     C                   MOVEL     *blanks       wVABLOD
049700100831     C                   MOVEL     *blanks       wVABPRD
049800030822     C*
049900030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050000030822     C                   CLEAR                   SkSplitCSV
050100030822     C                   EVAL      i = 1
050200030822     C                   EVAL      posDa = *zeros
050300030822     C                   EVAL      posA  = *zeros
050400080527     C*
050500080527     C                   CLEAR                   FIVABDS
050600080527     C                   CLEAR                   FIVAT000
050700030822     C*
050800030822     C* Reimposto i valori di default
050900030822     C                   EXSR      DEFCAM
051000030822     C*
051100010330     C                   ENDSR
051200010330     C*----------------------------------------------------*
051300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051400010330     C*----------------------------------------------------*
051500010330     C     DEFCAM        BEGSR
051600030822     C*
051700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051800030715     C* e delimitatore testo.
051900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052200100318     C*
052300100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052400100318     C                   EVAL      CharSOS = CharNUM
052500030822     C*
052600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052700100720     C                   SETOFF                                       505152
052800100720     C                   SETOFF                                       53
052900100413     C                   SELECT
053000100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053100100720     C                   SETON                                        50
053200100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053300100413     C                   SETON                                        51
053400100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053500100720     C                   SETON                                        52
053600100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053700100720     C                   SETON                                        53
053800100413     C                   ENDSL
053900030822     C*
054000030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054100030822     C                   EVAL      posDaDft = 1
054200030822     C                   EVAL      posADft  = 0
054300030822     C                   EVAL      wGiroDft = 0
054400030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054500030822     C                             posDaDft > 0
054600030822     C*
054700030822     C* Gestisco il 1° giro
054800030822     C                   IF        wGiroDft = 0
054900030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055000030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055100030822     C* Incremento il contatore dei "giri"
055200030822     C                   EVAL      wGiroDft = 1
055300030822     C                   ELSE
055400030822     C                   EVAL      posDaDft = posADft
055500030822     C                   ENDIF
055600030822     C* Eseguo lo scan x trovare la fine del campo corrente
055700030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055800030822     C*
055900030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056000030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056100030822     C                   IF        posDaDft > 0 AND
056200030822     C                             posADft  > 0
056300030822     C* NCL
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'NCL'
056800030822     C                   EVAL      PiStr=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   EXSR      CHKNUM
057200030822     C                   IF        PiInt=*on
057300030822     C                   Z-ADD     PiVal         VABNCL
057400030822     C                   ENDIF
057500030822     C                   ENDIF
057600030822     C* CCM
057700030822     C                   IF        %subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):1:3)
058000030822     C                             = 'CCM'
058100030822     C                   EVAL      PiStr=%trim(%subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058400030822     C                   EXSR      CHKNUM
058500030822     C                   IF        PiInt=*on
058600030822     C                   Z-ADD     PiVal         VABCCM
058700030822     C                   ENDIF
058800030822     C                   ENDIF
058900030822     C* LNP
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'LNP'
059400030822     C                   EVAL      PiStr=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   EXSR      CHKNUM
059800030822     C                   IF        PiInt=*on
059900030822     C                   Z-ADD     PiVal         VABLNP
060000030822     C                   ENDIF
060100030822     C                   ENDIF
060200030822     C* NRS
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'NRS'
060700030822     C                   EVAL      PiStr=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   EXSR      CHKNUM
061100030822     C                   IF        PiInt=*on
061200030822     C                   Z-ADD     PiVal         VABNRS
061300030822     C                   ENDIF
061400030822     C                   ENDIF
061500030822     C* CTR
061600030822     C                   IF        %subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):1:3)
061900030822     C                             = 'CTR'
062000030822     C                   EVAL      PiStr=%trim(%subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062300030822     C                   EXSR      CHKNUM
062400030822     C                   IF        PiInt=*on
062500030822     C                   Z-ADD     PiVal         VABCTR
062600030822     C                   ENDIF
062700030822     C                   ENDIF
062800030822     C* PKB
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'PKB'
063300030822     C                   EVAL      PiStr=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   EXSR      CHKNUM
063700030822     C                   IF        PiNum=*on
063800030822     C                   Z-ADD     PiVal         VABPKB
063900030822     C                   ENDIF
064000030822     C                   ENDIF
064100030822     C* VLB
064200030822     C                   IF        %subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):1:3)
064500030822     C                             = 'VLB'
064600030822     C                   EVAL      PiStr=%trim(%subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064900030822     C                   EXSR      CHKNUM
065000030822     C                   IF        PiNum=*on
065100030822     C                   Z-ADD     PiVal         VABVLB
065200030822     C                   ENDIF
065300030822     C                   ENDIF
065400030822     C* QFT
065500030822     C                   IF        %subst(
065600030822     C                             %subst(vlrppt:posDaDft+1:
065700030822     C                             posADft-posDaDft-1):1:3)
065800030822     C                             = 'QFT'
065900030822     C                   EVAL      PiStr=%trim(%subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066200030822     C                   EXSR      CHKNUM
066300030822     C                   IF        PiNum=*on
066400030822     C                   Z-ADD     PiVal         VABQFT
066500030822     C                   ENDIF
066600030822     C                   ENDIF
066700030822     C* CBO
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'CBO'
067200030822     C                   EVAL      VABCBO=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* TSP
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'TSP'
068100030822     C                   EVAL      VABTSP=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VAS
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VAS'
069000030822     C                   EVAL      VABVAS=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400030822     C* VCA
069500030822     C                   IF        %subst(
069600030822     C                             %subst(vlrppt:posDaDft+1:
069700030822     C                             posADft-posDaDft-1):1:3)
069800030822     C                             = 'VCA'
069900030822     C                   EVAL      VABVCA=%trim(%subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200030822     C                   ENDIF
070300070925     C* TIC
070400070925     C                   IF        %subst(
070500070925     C                             %subst(vlrppt:posDaDft+1:
070600070925     C                             posADft-posDaDft-1):1:3)
070700070925     C                             = 'TIC'
070800070925     C                   EVAL      VABTIC=%trim(%subst(
070900070925     C                             %subst(vlrppt:posDaDft+1:
071000070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100070925     C                   ENDIF
071200030822     C* GCA
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'GCA'
071700030822     C                   EVAL      VABGCA=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* CTM
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'CTM'
072600030822     C                   EVAL      VABCTM=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* FFD
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'FFD'
073500030822     C                   EVAL      VABFFD=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* VAD
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'VAD'
074400030822     C                   EVAL      VABVAD=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GMA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GMA'
075300030822     C                   EVAL      VABGMA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* GGA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GGA'
076200030822     C                   EVAL      VABGGA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* GVA
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'GVA'
077100030822     C                   EVAL      VABGVA=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* TC1
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'TC1'
078000030822     C                   EVAL      VABTC1=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400030822     C* TC2
078500030822     C                   IF        %subst(
078600030822     C                             %subst(vlrppt:posDaDft+1:
078700030822     C                             posADft-posDaDft-1):1:3)
078800030822     C                             = 'TC2'
078900030822     C                   EVAL      VABTC2=%trim(%subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200030822     C                   ENDIF
079300040714     C* VATTRC
079400040714     C                   IF        %subst(
079500040714     C                             %subst(vlrppt:posDaDft+1:
079600040714     C                             posADft-posDaDft-1):1:3)
079700040714     C                             = 'TRC'
079800040714     C                   EVAL      VATTRC=%trim(%subst(
079900040714     C                             %subst(vlrppt:posDaDft+1:
080000040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080100040714     C                   ENDIF
080200030822     C* ...
080300030822     C                   ENDIF
080400030822     C                   ENDDO
080500020204     C*
080600010330     C                   ENDSR
080700010607     C*----------------------------------------------------*
080800021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080900010607     C*----------------------------------------------------*
081000010607     C     IMPVAB        BEGSR
081100010607     C*
081200010607     C                   SETOFF                                       3132
081300110315 xxx C***                SETON                                        35
081400030515     C*
081500010607     C                   MOVEL     datcor        VABAAS
081600010607     C                   MOVE      datcor        VABMGS
081700010607     C*
081800030715     C**********
081900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082000030715     C**********
082100030715     C                   IF        wGiro = *zeros
082200040113     C*
082300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082500080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082600080429     C                             %trim(intcol(5))<>*blanks
082700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082800080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082900080429     C                                      %trim(intcol(5))
083000070823     C     ';':CharCSV   XLATE     vindta        vindta
083100040510     C                   ENDIF
083200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083300031201     C     minu:maiu     XLATE     vindta        vindta
083400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083500030715     C                   DOW       posDa <= %len(%trim(vindta))
083600030715     C*
083700030715     C* Gestisco il 1° campo
083800030715     C                   IF        i = 1
083900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084000030715     C                   EVAL      posDa = 1
084100030715     C* Eseguo lo scan x trovare la fine del primo campo
084200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C* X i campi successivi al 1°
084700030715     C                   ELSE
084800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084900030715     C                   EVAL      posDa = posA + 1
085000030715     C* Eseguo lo scan x trovare la fine del campo corrente
085100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085200030715     C* Gestisco l'ultimo campo
085300030715     C                   IF        posA = *zeros
085400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   LEAVE
085900030715     C                   ELSE
086000030715     C* A questo punto "estraggo" il campo corrente
086100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086200030715     C                                                    (posA-posDa))
086300030715     C                   ENDIF
086400030715     C                   ENDIF
086500030715     C* Incremento il contatore d campo
086600030715     C                   EVAL      i = i +1
086700030715     C                   ENDDO
086800030715     C*
086900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087000030715     C* delimitazione testo
087100030715     C                   EVAL      i = 1
087200030715     C                   DOW       i < %elem(SkSplitFLD)
087300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087500030715     C                   EVAL      i = i + 1
087600030715     C                   ENDDO
087700030715     C                   EVAL      wGiro = 1
087800030715     C                   ELSE
087900030715     C                   EVAL      wGiro = 2
088000031204     C**********
088100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088200031204     C* d separatore campo
088300031204     C**********
088400031204     C                   EVAL      vindta = %trim(vindta)
088500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088600031204     C                   EVAL      vindta = %subst(vindta:2)
088700031204     C                   ENDDO
088800031204     C*
088900031204     C                   Z-ADD     *zeros        lunghInput        4 0
089000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089400031204     C                   ENDDO
089500030715     C**********
089600030715     C* Effettuo lo split del campo dati d input
089700030715     C**********
089800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089900030715     C                   DOW       posDa <= %len(%trim(vindta))
090000030715     C*
090100030715     C* Gestisco il 1° campo
090200030715     C                   IF        i = 1
090300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090400030715     C                   EVAL      posDa = 1
090500030715     C* Eseguo lo scan x trovare la fine del primo campo
090600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090700030715     C* A questo punto "estraggo" il campo corrente
090800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090900030715     C                                                    (posA-posDa))
091000030715     C* X i campi successivi al 1°
091100030715     C                   ELSE
091200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091300030715     C                   EVAL      posDa = posA + 1
091400031204     C* Verifico che nn vi sia il campo nn valorizzato
091500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091600031204     C* Se campo nn valorizzato skippo al prossimo
091700031204     C                   EVAL      SkSplitCSV(i) = *blanks
091800031204     C                   EVAL      posA  = posA + 1
091900031204     C                   ELSE
092000030715     C* Eseguo lo scan x trovare la fine del campo corrente
092100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092200030715     C* Gestisco l'ultimo campo
092300030715     C                   IF        posA = *zeros
092400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C                   LEAVE
092900030715     C                   ELSE
093000030715     C* A questo punto "estraggo" il campo corrente
093100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093200030715     C                                                    (posA-posDa))
093300030715     C                   ENDIF
093400031204     C                   ENDIF
093500030715     C                   ENDIF
093600030715     C* Incremento il contatore d campo
093700030715     C                   EVAL      i = i +1
093800030715     C                   ENDDO
093900030715     C*
094000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094100030715     C* delimitazione testo
094200030715     C                   EVAL      i = 1
094300030715     C                   DOW       i < %elem(SkSplitCSV)
094400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094600030715     C                   EVAL      i = i + 1
094700030715     C                   ENDDO
094800100720     C*
094900100720     C* Stacco un progressivo x numero spedizione
095000110421 xxx C                   EXSR      repNSP
095100100326     C*
095200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095300030715     C                   EVAL      i = 1
095400030715     C                   DOW       i < %elem(SkSplitFLD)
095500030715     C*
095600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095800030715     C                             SkSplitCSV(i) <> *blanks
095900030721     C*** ATB
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096100030721     C                   EVAL      VABATB = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030715     C*** RSD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096800030715     C* ==
096900030715     C                   ENDIF
097000030721     C*** RD2
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030715     C*** IND
097500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097600030715     C                   EVAL      VABIND = SkSplitCSV(i)
097700030715     C                   ENDIF
097800030715     C*** LOD
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** PRD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030721     C*** NZD
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030715     C*** RMA
099100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099300030715     C                   ENDIF
099400030715     C*** NOT
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** NT2
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** VCA
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** VAS
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** FFD
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030721     C*** GC1
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** GC2
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102000030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** TSP
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102400030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102500030721     C                   ENDIF
102600040510     C*** CBO
102700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102800040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102900040510     C                   ENDIF
103000030721     C*** NAS
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103200030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*** TIC
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103600030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** GCA
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104000030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** XCO
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104400030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** CTM
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104800030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** TCR
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105200030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** CTS
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105600030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** FTM
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106000030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** VAD
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106400030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** GMA
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106800030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GGA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107200030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** GVA
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107600030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** TC1
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108000030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** TC2
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108400030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** SCL
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108800030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** RMO
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109200030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** NMO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109600030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*
109900030721     C* Reperisco quindi i campi numerici...
110000040510     C*** NSP
110100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110200040510     C                   EVAL      PiStr=SkSplitCSV(i)
110300040510     C                   EXSR      CHKNUM
110400040510     C                   IF        PiInt=*on
110500040510     C                   Z-ADD     PiVal         VABNSP
110600040510     C                   ELSE
110700040510     C                   SETON                                        31
110800040510     C                   EVAL      VABNSP = *zeros
110900040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111000040510     C                             + ' ' + 'VABNSP'
111100040510     C                   ENDIF
111200040510     C                   ENDIF
111300071106     C*** RMN
111400071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111500071106     C                   EVAL      PiStr=SkSplitCSV(i)
111600071106     C                   EXSR      CHKNUM
111700071106     C                   IF        PiInt=*on
111800071106     C                   Z-ADD     PiVal         VABRMN
111900071106     C                   ELSE
112000071106     C                   SETON                                        32
112100071106     C                   EVAL      VABRMN = *zeros
112200071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112300071106     C                             + ' ' + 'VABRMN'
112400071106     C                   ENDIF
112500071106     C                   ENDIF
112600030721     C*** CAD
112700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112800030721     C                   EVAL      PiStr=SkSplitCSV(i)
112900030721     C                   EXSR      CHKNUM
113000030721     C                   IF        PiInt=*on
113100030721     C                   Z-ADD     PiVal         Num5_0
113200030721     C                   MOVEL(p)  Num5_0        VABCAD
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABCAD = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABCAD'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** NCL
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on
114500030721     C                   Z-ADD     PiVal         VABNCL
114600030721     C                   ELSE
114700030721     C                   SETON                                        32
114800030721     C                   EVAL      VABNCL = *zeros
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000040122     C                             + ' ' + 'VABNCL'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** PKB
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiNum=*on
115800030721     C                   Z-ADD     PiVal         VABPKB
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      VABPKB = *zeros
116200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116300030721     C                             + ' ' + 'VABPKB'
116400030721     C                   ENDIF
116500030721     C                   ENDIF
116600030721     C*** CAS
116700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116800110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116900110506     C                             %trim(SkSplitCSV(i)) <> *zeros
117000101018     C                   EVAL      wFlgCAS = '1'
117100101018     C                   ENDIF
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiNum=*on
117500030721     C                   Z-ADD     PiVal         VABCAS
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABCAS = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABCAS'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CCM
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on      AND
118800030721     C                             PiVal<=9999999 AND
118900030721     C                             PiVal>*zeros
119000030721     C                   Z-ADD     PiVal         VABCCM
119100030721     C                   ELSE
119200030721     C                   SETON                                        32
119300030721     C                   EVAL      VABCCM = *zeros
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500040510     C                             + ' ' + 'VABCCM'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** LNP
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on    AND
120300030721     C                             PiVal<=999   AND
120400030721     C                             PiVal>*zeros
120500030721     C                   Z-ADD     PiVal         VABLNP
120600030721     C                   ELSE
120700040715     C                   SETON                                        32
120800030721     C                   EVAL      VABLNP = *zeros
120900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121000040510     C                             + ' ' + 'VABLNP'
121100030721     C                   ENDIF
121200030721     C                   ENDIF
121300030721     C*** NRS
121400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121500030721     C                   EVAL      PiStr=SkSplitCSV(i)
121600030721     C                   EXSR      CHKNUM
121700030721     C                   IF        PiInt=*on AND
121800030721     C                             PiVal<=99
121900030721     C                   Z-ADD     PiVal         VABNRS
122000030721     C                   ELSE
122100030721     C                   SETON                                        32
122200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122300030721     C                             + ' ' + 'VABNRS'
122400030721     C                   ENDIF
122500030721     C                   ENDIF
122600030721     C*** LNA
122700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122800030721     C                   EVAL      PiStr=SkSplitCSV(i)
122900030721     C                   EXSR      CHKNUM
123000030721     C                   IF        PiInt=*on    AND
123100030721     C                             PiVal<=999   AND
123200030721     C                             PiVal>*zeros
123300030721     C                   Z-ADD     PiVal         VABLNA
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      VABLNA = *zeros
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABLNA'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** CTR
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on  AND
124600030721     C                             PiVal<=999
124700030721     C                   Z-ADD     PiVal         VABCTR
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABCTR'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** IAS
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABIAS
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABIAS'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** VLB
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiNum=*on
127100030721     C                   Z-ADD     PiVal         VABVLB
127200030721     C                   ELSE
127300030721     C                   SETON                                        32
127400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127500030721     C                             + ' ' + 'VABVLB'
127600030721     C                   ENDIF
127700030721     C                   ENDIF
127800030721     C*** QFT
127900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128000030721     C                   EVAL      PiStr=SkSplitCSV(i)
128100030721     C                   EXSR      CHKNUM
128200030721     C                   IF        PiNum=*on
128300030721     C                   Z-ADD     PiVal         VABQFT
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABQFT'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** NCD
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on      AND
129500030721     C                             PiVal<=9999999
129600030721     C                   Z-ADD     PiVal         VABNCD
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABNCD'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** NCA
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on      AND
130800030721     C                             PiVal<=9999999
130900030721     C                   Z-ADD     PiVal         VABNCA
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABNCA'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** ZNC
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on AND
132100030721     C                             PiVal<=99
132200030721     C                   Z-ADD     PiVal         VABZNC
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABZNC'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** DCR
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiInt=*on       AND
133400030721     C                             PiVal<=99999999
133500030721     C                   Z-ADD     PiVal         VABDCR
133600030721     C                   ELSE
133700030721     C                   SETON                                        32
133800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133900030721     C                             + ' ' + 'VABDCR'
134000030721     C                   ENDIF
134100030721     C                   ENDIF
134200030721     C*** HCR
134300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134400030721     C                   EVAL      PiStr=SkSplitCSV(i)
134500030721     C                   EXSR      CHKNUM
134600030721     C                   IF        PiInt=*on   AND
134700030721     C                             PiVal<=9999
134800030721     C                   Z-ADD     PiVal         VABHCR
134900030721     C                   ELSE
135000030721     C                   SETON                                        32
135100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135200030721     C                             + ' ' + 'VABHCR'
135300030721     C                   ENDIF
135400030721     C                   ENDIF
135500030721     C*** VMD
135600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135700030721     C                   EVAL      PiStr=SkSplitCSV(i)
135800030721     C                   EXSR      CHKNUM
135900030721     C                   IF        PiNum=*on
136000030721     C                   Z-ADD     PiVal         VABVMD
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136400030721     C                             + ' ' + 'VABVMD'
136500030721     C                   ENDIF
136600030721     C                   ENDIF
136700030721     C*** ANT
136800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136900030721     C                   EVAL      PiStr=SkSplitCSV(i)
137000030721     C                   EXSR      CHKNUM
137100030721     C                   IF        PiInt=*on        AND
137200030721     C                             PiVal<=999999999
137300030721     C                   Z-ADD     PiVal         VABANT
137400030721     C                   ELSE
137500030721     C                   SETON                                        32
137600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137700030721     C                             + ' ' + 'VABANT'
137800030721     C                   ENDIF
137900030721     C                   ENDIF
138000030721     C*** CMO
138100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138200030721     C                   EVAL      PiStr=SkSplitCSV(i)
138300030721     C                   EXSR      CHKNUM
138400030721     C                   IF        PiInt=*on
138500030721     C                   Z-ADD     PiVal         Num5_0
138600030721     C                   MOVEL(p)  Num5_0        VABCMO
138700030721     C                   ELSE
138800030721     C                   SETON                                        32
138900030721     C                   EVAL      VABCMO = *zeros
139000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139100030721     C                             + ' ' + 'VABCMO'
139200030721     C                   ENDIF
139300030721     C                   ENDIF
139400040510     C*
139500040510     C* Reperisco i campi del FIVAT
139600040510     C*** VATNOT
139700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139800040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139900040510     C                   ENDIF
140000040510     C*** VATTRC
140100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140200040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140300040510     C                   ENDIF
140400060331     C*** VATNOT_A
140500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140600060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140700060331     C                   ENDIF
140800060331     C*** VATNOT_B
140900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141000060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141100060331     C                   ENDIF
141200060331     C*** VATNOT_E
141300070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141400060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141500060331     C                   ENDIF
141600070823     C*
141700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
141800110421     C*
141900110623     C*** NSP
142000110623     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
142100110623     C                   EVAL      PiStr=SkSplitCSV(i)
142200110623     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142300110623     C                   EXSR      CHKNUM
142400110623     C                   IF        PiInt=*on
142500110623     C                   Z-ADD     PiVal         VABNSP
142600110623     C                   ELSE
142700110623     C                   SETON                                        31
142800110623     C                   EVAL      VABNSP = *zeros
142900110623     C                   EVAL      vinmsg = %trimr(vinmsg)
143000110623     C                             + ' ' + 'VABNSP VATNOT_E'
143100110623     C                   ENDIF
143200110623     C                   ENDIF
143300070925     C*
143400071029     C***  <===  -----------------------------------------
143500040510     C*
143600030715     C                   ENDIF
143700030715     C                   EVAL      i = i + 1
143800030715     C                   ENDDO
143900070925     C*
144000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
144100100831     C*
144200100831     C                   IF        wVABRSD <> *blanks
144300100831     C                   EVAL      VABRSD = wVABRSD
144400100831     C                   ENDIF
144500100831     C*
144600100831     C                   IF        wVABIND <> *blanks
144700100831     C                   EVAL      VABIND = wVABIND
144800100831     C                   ENDIF
144900100831     C*
145000100831     C                   IF        wVABCAD <> *blanks
145100100831     C                   EVAL      VABCAD = wVABCAD
145200100831     C                   ENDIF
145300100831     C*
145400100831     C                   IF        wVABLOD <> *blanks
145500100831     C                   EVAL      VABLOD = wVABLOD
145600100831     C                   ENDIF
145700100831     C*
145800100831     C                   IF        wVABPRD <> *blanks
145900100831     C                   EVAL      VABPRD = wVABPRD
146000100831     C                   ENDIF
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
149800040510     C                   IF        VABPRD = *blanks
149900040128     C                   CLEAR                   TISI95DS
150000040128     C                   EVAL      I95TCN = '3'
150100040128     C                   Z-ADD     datcor        I95DAT
150200040128     C                   EVAL      I95NAR = VABNZD
150300040128     C                   EVAL      I95CAP = VABCAD
150400040128     C                   EVAL      I95LOC = VABLOD
150500040128     C                   CALL      'TISI95R'
150600040128     C                   PARM                    TISI95DS
150700040128     C                   EVAL      VABPRD = O95PRV
150800040510     C                   ENDIF
150900020204     C*
151000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151100020204     C                   EXSR      CHKIMPDIV
151200030715     C                   ENDIF
151300020204     C*
151400010607     C                   ENDSR
151500010607     C*----------------------------------------------------*
151600010601
151700020204
151800020204     C*----------------------------------------------------*
151900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
152000020204     C*----------------------------------------------------*
152100020204     C     CHKIMPDIV     BEGSR
152200020204     C*
152300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152400020204     C                   Z-ADD     *zeros        wrkDec            9 9
152500020204     C*
152600020204     C* Come prima cosa effettuo considerazioni sulla divisa
152700020204     C                   IF        vabIAS > *zeros
152800020204     C                   IF        vabVAS <> 'EUR'
152900020204     C                   EVAL      vabVAS =  'ITL'
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C                   IF        vabCAS > *zeros
153400020204     C                   IF        vabVCA <> 'EUR'
153500020204     C                   EVAL      vabVCA =  'ITL'
153600020204     C                   ENDIF
153700020204     C                   ENDIF
153800020204     C*
153900020204     C                   IF        vabVMD > *zeros
154000020204     C                   IF        vabVAD <> 'EUR'
154100020204     C                   EVAL      vabVAD =  'ITL'
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154600020204     C                   Z-ADD     vabIAS        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVAS = 'ITL'
154900020204     C                   EVAL      vabIAS = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155400020204     C                   Z-ADD     vabCAS        wrkDec
155500020204     C                   IF        wrkDec > *zeros
155600020204     C                   IF        vabVCA = 'ITL'
155700020204     C                   EVAL      vabCAS = *zeros
155800020204     C                   ENDIF
155900020204     C                   ENDIF
156000020204     C*
156100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156200020204     C                   Z-ADD     vabVMD        wrkDec
156300020204     C                   IF        wrkDec > *zeros
156400020204     C                   IF        vabVAD = 'ITL'
156500020204     C                   EVAL      vabVMD = *zeros
156600020204     C                   ENDIF
156700020204     C                   ENDIF
156800020204     C*
156900020204     C                   ENDSR
157000020204     C***
157100010330
157200100318
157300100318
157400100318     C*----------------------------------------------------*
157500100318     C*  CONTROLLO NUMERICITA' CAMPI
157600100318     C*----------------------------------------------------*
157700100318     C     CHKNUM        BEGSR
157800100318     C*
157900100318     C                   IF        PiDecChr = *blanks
158000100318     C                   EVAL      PiDecChr = CharNUM
158100100318     C                   ENDIF
158200100318     C*
158300100318     C                   callp     UBISNUM_Check(PiStr
158400100318     C                                          :PiDecChr
158500100318     C                                          :PiVal
158600100318     C                                          :PiNum
158700100318     C                                          :PiInt)
158800100318     C*
158900100318     C                   ENDSR
159000100318     C***
159100010601
159200010601
159300010601
159400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159500010601     C     repfil        BEGSR
159600010601     C*
159700010601     C                   if        invfil = *zeros and
159800010601     C                             depfil > *zeros and
159900010629     C                             (vinflg = *blanks or
160000010629     C                              vinflg = *zeros)
160100010601     C
160200010601     C                   eval      invfil = depfil
160300010601     C                   endif
160400010601     C*
160500010601     C                   if        depfil <> invfil and
160600010601     C                             invfil > *zeros
160700010601     C                   eval      flgMulti = '1'
160800010601     C                   if        vinflg = *blanks
160900010601     C                   add       1             cntNonEl
161000010601     C                   endif
161100010601     C                   endif
161200010601     C*
161300010601     C                   if        vinflg = '2'
161400010601     C                   eval      flgStato = '2'
161500010601     C                   endif
161600010601     C*
161700010601     C                   ENDSR
161800010601     C***
161900010601
162000010601
162100010601
162200010330
162300040119      /TITLE Invio dei dati al punto operativo.
162400040119     C     invio         BEGSR
162500040119     C*
162600040119     C* 1° invio FIVAT
162700040119     C                   reset                   dscmz
162800040119     C                   move      vlrpoi        cmzdst
162900040119     C                   eval      cmzfld = 'FIVATWWR'
163000040119     C                   eval      cmzmbd = vlrhdl
163100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163200040119     C***                if        prmfir = *blanks
163300040119     C                   eval      cmzfla = 'FIVAT00F'
163400040119     C                   eval      cmzmba = 'FIVAT00F'
163500040119     C***                else
163600040119     C***                eval      cmzfla = prmfir
163700040119     C***                eval      cmzmba = prmfir
163800040119     C***                endif
163900040119     C                   eval      cmznrr = *zeros
164000040119     C                   move      §ctrokvt      cmznrr
164100040119     C                   eval      cmzlba = vlrfl1
164200040119     C                   call(e)   'TIS711C'
164300040119     C                   parm                    dscmz
164400040119     C                   parm      *blanks       esito
164500040119     C                   if        %error
164600040119     C                             or cmzerr = '1'
164700040119     C                             or esito  = '1'
164800040119     C                   eval      wrkesito = '3'
164900040119     C                   else
165000040119     C*
165100040119     C* 2° invio FIVAB
165200040119     C                   reset                   dscmz
165300040119     C                   move      vlrpoi        cmzdst
165400040119     C                   eval      cmzfld = vlrfou
165500040119     C                   eval      cmzmbd = vlrhdl
165600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165700040119     C***                if        prmfir = *blanks
165800040119     C                   eval      cmzfla = 'FIVAB00F'
165900040119     C                   eval      cmzmba = 'FIVAB00F'
166000040119     C***                else
166100040119     C***                eval      cmzfla = prmfir
166200040119     C***                eval      cmzmba = prmfir
166300040119     C***                endif
166400040119     C                   eval      cmznrr = *zeros
166500040119     C                   move      §ctrokvb      cmznrr
166600040119     C                   eval      cmzlba = vlrfl1
166700040119     C                   call(e)   'TIS711C'
166800040119     C                   parm                    dscmz
166900040119     C                   parm      *blanks       esito
167000040119     C                   if        %error
167100040119     C                             or cmzerr = '1'
167200040119     C                             or esito  = '1'
167300040119     C                   eval      wrkesito = '3'
167400040119     C                   endif
167500040119     C                   endif
167600040119     C*
167700040119     C                   ENDSR
167800040119     C***
167900010601
168000010601
168100010601
168200010601
168300010601      /TITLE Invio dei dati al punto operativo.
168400010601     C     opeini        BEGSR
168500010601     C*
168600010601     C* Inizializzo flag e contatori operativi
168700010601     C                   movel     '0'           flgGiro           1
168800010601     C                   movel     '0'           flgMulti          1
168900010601     C                   movel     '1'           flgStato          1
169000010615     C                   movel     '0'           flgOk             1
169100010601     C                   z-add     *zeros        cntNonEl         10 0
169200010601     C                   z-add     *zeros        depfil            3 0
169300010601     C                   z-add     *zeros        invfil            3 0
169400010601     C*
169500010601     C                   ENDSR
169600010601     C***
169700070326
169800070326
169900070326
170000070326
170100070326     C     *pssr         BEGSR
170200070329     C*
170300070329     C                   if        %open(tivin00r)
170400070329     C                   close     tivin00r
170500070329     C                   endif
170600070329     C                   if        %open(fivabwwr)
170700070329     C                   close     fivabwwr
170800070329     C                   endif
170900070329     C                   if        %open(fivatwwr)
171000070329     C                   close     fivatwwr
171100070329     C                   endif
171200070326     C*
171300070326     C* Effettuo la chiamata al CLLE preposto
171400070326     C                   call(e)   'TITVVTC'
171500070326     C                   parm                    parccm
171600070326     C                   parm                    parmbr
171700070326     C                   parm      '2'           paropz
171800070326     C*
171900070326     C                   eval      wrkesito = '2'
172000070404     C*
172100070404     C                   seton                                        LR
172200070326     C*
172300070326     C                   ENDSR     '*CANCL'
172400070326     C***
172500100318
172600100318
172700100318
172800100318     C     normCSV       BEGSR
172900100318     C*
173000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173100100318     C* separatori campi
173200100318     C                   if        CharTXT <> *blanks
173300100318     C                   z-add     *zeros        wLenVINDTA        4 0
173400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173500100318     C                   setoff                                       80
173600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173900100318     C                   if        not *in80
174000100318     C                   seton                                        80
174100100318     C                   else
174200100318     C                   setoff                                       80
174300100318     C                   endif
174400100318     C                   endif
174500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174600100318     C                             and *in80 = *on
174700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174800100318     C                   endif
174900100318     C                   enddo
175000100318     C                   endif
175100100318     C*
175200100318     C                   ENDSR
175300100318     C***
175400070326
175500100326
175600100326
175700100326     C*----------------------------------------------------*
175800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175900100326     C*----------------------------------------------------*
176000100326     C     REPNSP        BEGSR
176100100326     C*
176200100326     C* NSP => Stacco un numeratore da AZNUM
176300100326     C                   clear                   TRUL33DS
176400100326     C                   eval      I33OPE = *zeros
176500100326     C                   eval      I33CNU = 302
176600100326     C                   eval      I33NUM = 1
176700100326     C                   movel     TRUL33DS      KPJBU
176800100326     C                   call      'TRUL33R'
176900100326     C                   parm                    KPJBA
177000100326     C                   movel     KPJBU         TRUL33DS
177100100326     C                   if        O33ERR = *zeros
177200100326     C                   z-add     O33NRF        VABNSP
177300100326     C                   z-add     O33NRF        VATNSP
177400100326     C                   else
177500100326     C                   SETON                                        31
177600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177700100326     C                             + ' ' + 'VABNSP VATNSP'
177800100326     C                   endif
177900100326     C*
178000100326     C                   ENDSR
178100010330
178200100326
178300010330
178400000613     C     *inzsr        BEGSR
178500990910     C*
178600990910     C     *entry        plist
178700990920     C                   parm                    tivlrds
178800990921     C                   parm      wrkesito      esito
178900000724     C                   parm                    prmlit
179000000710     C                   parm                    prmfir
179100010330     C*
179200010330     C* CALCOLA LA DATA CORRENTE
179300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179400100318     C                   eval      datcor = %dec(%date() : *ISO)
179500000613     C*
179600000613     C                   ENDSR
179700000613     C***
179800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179900110623
180000110623
180100110315
180200110315
180300100318
