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
006900140707     D wX              s              2  0 INZ(1)
007000030822     D*------------------
007100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007200030822     D*------------------
007300030822     D posDaDft        S              3  0 INZ(*zeros)
007400030822     D posADft         S              3  0 INZ(*zeros)
007500030822     D j               s              3  0 INZ(1)
007600030822     D wGiroDft        s              1  0 INZ(*zeros)
007700080418     D*------------------
007800080418     D* Costanti
007900080418     D*------------------
008000080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008100080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008200100318
008300100318     D*------------------
008400100318     D* LINKING A DEFINIZIONI ESTERNE
008500100318     D*------------------
008600100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008700100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008800100318
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
017100120227     C                   exsr      inzvar
017200120227     C                   exsr      impvab
017300110131     C*
017400110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017500120301 xxx C                   read      tivin00r                               70
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
034601140718     C* per impedire che il peso sia 0
034602140718     C                   IF        *in31 = '0' and
034603140718     C                             ok_VABPKB = 0
034604140718     C                   EVAL      ok_VABPKB = 0,2
034605140718     C                   ENDIF                                                                     C
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
041200060331     C                   eval      VATTRC = 'E'
041300140707     C*
041400140707     C* sviluppo i CHI SONO rispetto al nr.colli aggiungendo un progressivo di 2 char a dx
041500140707     C     1             DO        VABNCL        wX
041600140707     C                   eval      VATNOT = %trimr(wVATNOT_E) +
041700140707     C                                      %editc(wx:'X')
041800110201     C                   write     FIVAT000
041900140707     C                   ENDDO
042000140707     C*
042100110201     C                   add       1             §CTROKVT
042200110201     C                   endif
042300110927     C*
042400140520     C                   if        %subst(wVATNOT_IJ:1:35) <> *blanks
042500140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
042600110927     C                   eval      VATTRC = 'I'
042700110927     C                   write     FIVAT000
042800110927     C                   add       1             §CTROKVT
042900110927     C                   endif
043000140520     C*
043100140520     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
043200140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
043300140520     C                   eval      VATTRC = 'J'
043400140520     C                   write     FIVAT000
043500140520     C                   add       1             §CTROKVT
043600140520     C                   endif
043700140520     C*
043800140520     C                   if        wVATNOT_S <> *blanks
043900140520     C                   EVAL      %subst(wVATNOT_S:17:2) = 'NN'
044000140520     C                   eval      VATNOT = wVATNOT_S
044100140520     C                   eval      VATTRC = 'S'
044200140520     C                   write     FIVAT000
044300140520     C                   add       1             §CTROKVT
044400140520     C                   endif
044500040119     C*
044600040119     C                   ENDSR
044700040119
044800040119
044900040119
045000040119     C*----------------------------------------------------*
045100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
045200040119     C*----------------------------------------------------*
045300040119     C     PREVAT        BEGSR
045400040119     C*
045500040119     C* Compongo il nome del membro da dare al FIVATWWR
045600040119     C                   eval      parmbr = vlrhdl
045700040119     C                   movel     'M'           parmbr
045800040510     C                   eval      parccm = vlrksc
045900040119     C                   eval      paropz = '1'
046000040119     C* Effettuo la chiamata al CLLE preposto
046100040119     C                   call(e)   'TITVVTC'
046200040119     C                   parm                    parccm
046300040119     C                   parm                    parmbr
046400040119     C                   parm                    paropz
046500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046600040119     C                   if        %error
046700040119     C                   movel     '1'           chkcall
046800040119     C                   else
046900040119     C                   movel     '0'           chkcall
047000040119     C                   endif
047100040119     C*
047200040119     C                   ENDSR
047300010601
047400010601
047500010601
047600010330     C*----------------------------------------------------*
047700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047800010330     C*----------------------------------------------------*
047900040119     C     INZVAR        BEGSR
048000080527     C*
048100080527     C                   setoff                                       65
048200010330     C*
048300030822     C* Inizializzo variabili di wrk
048400020204     C                   Z-ADD     *zeros        Num5_0            5 0
048500101018     C                   MOVEL     *blanks       wFlgCAS
048600060331     C                   MOVEL     *blanks       wVATNOT_A
048700060331     C                   MOVEL     *blanks       wVATNOT_B
048800060331     C                   MOVEL     *blanks       wVATNOT_E
048900110927     C                   MOVEL     *blanks       wVATNOT_I
049000140520     C                   MOVEL     *blanks       wVATNOT_J
049100140520     C                   MOVEL     *blanks       wVATNOT_IJ
049200140520     C                   MOVEL     *blanks       wVATNOT_S
049300030822     C*
049400030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049500030822     C                   CLEAR                   SkSplitCSV
049600030822     C                   EVAL      i = 1
049700030822     C                   EVAL      posDa = *zeros
049800030822     C                   EVAL      posA  = *zeros
049900080527     C*
050000080527     C                   CLEAR                   FIVABDS
050100080527     C                   CLEAR                   FIVAT000
050200030822     C*
050300030822     C* Reimposto i valori di default
050400030822     C                   EXSR      DEFCAM
050500030822     C*
050600010330     C                   ENDSR
050700010330     C*----------------------------------------------------*
050800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050900010330     C*----------------------------------------------------*
051000010330     C     DEFCAM        BEGSR
051100030822     C*
051200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051300030715     C* e delimitatore testo.
051400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051700100318     C*
051800100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051900100318     C                   EVAL      CharSOS = CharNUM
052000030822     C*
052100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052200100720     C                   SETOFF                                       505152
052300100720     C                   SETOFF                                       53
052400100413     C                   SELECT
052500100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052600100720     C                   SETON                                        50
052700100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052800100413     C                   SETON                                        51
052900100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053000100720     C                   SETON                                        52
053100100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
053200100720     C                   SETON                                        53
053300100413     C                   ENDSL
053400030822     C*
053500030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053600030822     C                   EVAL      posDaDft = 1
053700030822     C                   EVAL      posADft  = 0
053800030822     C                   EVAL      wGiroDft = 0
053900030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054000030822     C                             posDaDft > 0
054100030822     C*
054200030822     C* Gestisco il 1° giro
054300030822     C                   IF        wGiroDft = 0
054400030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054500030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054600030822     C* Incremento il contatore dei "giri"
054700030822     C                   EVAL      wGiroDft = 1
054800030822     C                   ELSE
054900030822     C                   EVAL      posDaDft = posADft
055000030822     C                   ENDIF
055100030822     C* Eseguo lo scan x trovare la fine del campo corrente
055200030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
055300030822     C*
055400030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055500030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055600030822     C                   IF        posDaDft > 0 AND
055700030822     C                             posADft  > 0
055800030822     C* NCL
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'NCL'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiInt=*on
056800030822     C                   Z-ADD     PiVal         VABNCL
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* CCM
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'CCM'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiInt=*on
058100030822     C                   Z-ADD     PiVal         VABCCM
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* LNP
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'LNP'
058900030822     C                   EVAL      PiStr=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   EXSR      CHKNUM
059300030822     C                   IF        PiInt=*on
059400030822     C                   Z-ADD     PiVal         VABLNP
059500030822     C                   ENDIF
059600030822     C                   ENDIF
059700030822     C* NRS
059800030822     C                   IF        %subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):1:3)
060100030822     C                             = 'NRS'
060200030822     C                   EVAL      PiStr=%trim(%subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060500030822     C                   EXSR      CHKNUM
060600030822     C                   IF        PiInt=*on
060700030822     C                   Z-ADD     PiVal         VABNRS
060800030822     C                   ENDIF
060900030822     C                   ENDIF
061000030822     C* CTR
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'CTR'
061500030822     C                   EVAL      PiStr=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   EXSR      CHKNUM
061900030822     C                   IF        PiInt=*on
062000030822     C                   Z-ADD     PiVal         VABCTR
062100030822     C                   ENDIF
062200030822     C                   ENDIF
062300030822     C* PKB
062400030822     C                   IF        %subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):1:3)
062700030822     C                             = 'PKB'
062800030822     C                   EVAL      PiStr=%trim(%subst(
062900030822     C                             %subst(vlrppt:posDaDft+1:
063000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063100030822     C                   EXSR      CHKNUM
063200030822     C                   IF        PiNum=*on
063300030822     C                   Z-ADD     PiVal         VABPKB
063400030822     C                   ENDIF
063500030822     C                   ENDIF
063600030822     C* VLB
063700030822     C                   IF        %subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):1:3)
064000030822     C                             = 'VLB'
064100030822     C                   EVAL      PiStr=%trim(%subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064400030822     C                   EXSR      CHKNUM
064500030822     C                   IF        PiNum=*on
064600030822     C                   Z-ADD     PiVal         VABVLB
064700030822     C                   ENDIF
064800030822     C                   ENDIF
064900030822     C* QFT
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'QFT'
065400030822     C                   EVAL      PiStr=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   EXSR      CHKNUM
065800030822     C                   IF        PiNum=*on
065900030822     C                   Z-ADD     PiVal         VABQFT
066000030822     C                   ENDIF
066100030822     C                   ENDIF
066200030822     C* CBO
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'CBO'
066700030822     C                   EVAL      VABCBO=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* TSP
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'TSP'
067600030822     C                   EVAL      VABTSP=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* VAS
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'VAS'
068500030822     C                   EVAL      VABVAS=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* VCA
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'VCA'
069400030822     C                   EVAL      VABVCA=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800070925     C* TIC
069900070925     C                   IF        %subst(
070000070925     C                             %subst(vlrppt:posDaDft+1:
070100070925     C                             posADft-posDaDft-1):1:3)
070200070925     C                             = 'TIC'
070300070925     C                   EVAL      VABTIC=%trim(%subst(
070400070925     C                             %subst(vlrppt:posDaDft+1:
070500070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600070925     C                   ENDIF
070700030822     C* GCA
070800030822     C                   IF        %subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):1:3)
071100030822     C                             = 'GCA'
071200030822     C                   EVAL      VABGCA=%trim(%subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500030822     C                   ENDIF
071600030822     C* CTM
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'CTM'
072100030822     C                   EVAL      VABCTM=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* FFD
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'FFD'
073000030822     C                   EVAL      VABFFD=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* VAD
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'VAD'
073900030822     C                   EVAL      VABVAD=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* GMA
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'GMA'
074800030822     C                   EVAL      VABGMA=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GGA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GGA'
075700030822     C                   EVAL      VABGGA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GVA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GVA'
076600030822     C                   EVAL      VABGVA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* TC1
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'TC1'
077500030822     C                   EVAL      VABTC1=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* TC2
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'TC2'
078400030822     C                   EVAL      VABTC2=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800040714     C* VATTRC
078900040714     C                   IF        %subst(
079000040714     C                             %subst(vlrppt:posDaDft+1:
079100040714     C                             posADft-posDaDft-1):1:3)
079200040714     C                             = 'TRC'
079300040714     C                   EVAL      VATTRC=%trim(%subst(
079400040714     C                             %subst(vlrppt:posDaDft+1:
079500040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600040714     C                   ENDIF
079700030822     C* ...
079800030822     C                   ENDIF
079900030822     C                   ENDDO
080000020204     C*
080100010330     C                   ENDSR
080200010607     C*----------------------------------------------------*
080300021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080400010607     C*----------------------------------------------------*
080500010607     C     IMPVAB        BEGSR
080600010607     C*
080700010607     C                   SETOFF                                       3132
080800110315 xxx C***                SETON                                        35
080900030515     C*
081000010607     C                   MOVEL     datcor        VABAAS
081100010607     C                   MOVE      datcor        VABMGS
081200010607     C*
081300030715     C**********
081400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081500030715     C**********
081600030715     C                   IF        wGiro = *zeros
081700040113     C*
081800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082000080429     C                             %trim(intcol(3))+%trim(intcol(4))+
082100080429     C                             %trim(intcol(5))<>*blanks
082200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
082300080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082400080429     C                                      %trim(intcol(5))
082500070823     C     ';':CharCSV   XLATE     vindta        vindta
082600040510     C                   ENDIF
082700031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082800031201     C     minu:maiu     XLATE     vindta        vindta
082900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083000030715     C                   DOW       posDa <= %len(%trim(vindta))
083100030715     C*
083200030715     C* Gestisco il 1° campo
083300030715     C                   IF        i = 1
083400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083500030715     C                   EVAL      posDa = 1
083600030715     C* Eseguo lo scan x trovare la fine del primo campo
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C* X i campi successivi al 1°
084200030715     C                   ELSE
084300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084400030715     C                   EVAL      posDa = posA + 1
084500030715     C* Eseguo lo scan x trovare la fine del campo corrente
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* Gestisco l'ultimo campo
084800030715     C                   IF        posA = *zeros
084900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085000030715     C* A questo punto "estraggo" il campo corrente
085100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085200030715     C                                                    (posA-posDa))
085300030715     C                   LEAVE
085400030715     C                   ELSE
085500030715     C* A questo punto "estraggo" il campo corrente
085600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085700030715     C                                                    (posA-posDa))
085800030715     C                   ENDIF
085900030715     C                   ENDIF
086000030715     C* Incremento il contatore d campo
086100030715     C                   EVAL      i = i +1
086200030715     C                   ENDDO
086300030715     C*
086400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086500030715     C* delimitazione testo
086600030715     C                   EVAL      i = 1
086700030715     C                   DOW       i < %elem(SkSplitFLD)
086800030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086900030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087000030715     C                   EVAL      i = i + 1
087100030715     C                   ENDDO
087200030715     C                   EVAL      wGiro = 1
087300030715     C                   ELSE
087400030715     C                   EVAL      wGiro = 2
087500031204     C**********
087600031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087700031204     C* d separatore campo
087800031204     C**********
087900031204     C                   EVAL      vindta = %trim(vindta)
088000031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
088100031204     C                   EVAL      vindta = %subst(vindta:2)
088200031204     C                   ENDDO
088300031204     C*
088400031204     C                   Z-ADD     *zeros        lunghInput        4 0
088500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088600031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088700031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088900110912     C                   ENDDO
089000030715     C**********
089100030715     C* Effettuo lo split del campo dati d input
089200030715     C**********
089300030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089400030715     C                   DOW       posDa <= %len(%trim(vindta))
089500030715     C*
089600030715     C* Gestisco il 1° campo
089700030715     C                   IF        i = 1
089800030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089900030715     C                   EVAL      posDa = 1
090000030715     C* Eseguo lo scan x trovare la fine del primo campo
090100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090200030715     C* A questo punto "estraggo" il campo corrente
090300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090400030715     C                                                    (posA-posDa))
090500030715     C* X i campi successivi al 1°
090600030715     C                   ELSE
090700030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090800030715     C                   EVAL      posDa = posA + 1
090900031204     C* Verifico che nn vi sia il campo nn valorizzato
091000031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
091100031204     C* Se campo nn valorizzato skippo al prossimo
091200031204     C                   EVAL      SkSplitCSV(i) = *blanks
091300031204     C                   EVAL      posA  = posA + 1
091400031204     C                   ELSE
091500030715     C* Eseguo lo scan x trovare la fine del campo corrente
091600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091700030715     C* Gestisco l'ultimo campo
091800030715     C                   IF        posA = *zeros
091900030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092000030715     C* A questo punto "estraggo" il campo corrente
092100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092200030715     C                                                    (posA-posDa))
092300030715     C                   LEAVE
092400030715     C                   ELSE
092500030715     C* A questo punto "estraggo" il campo corrente
092600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092700030715     C                                                    (posA-posDa))
092800030715     C                   ENDIF
092900031204     C                   ENDIF
093000030715     C                   ENDIF
093100030715     C* Incremento il contatore d campo
093200030715     C                   EVAL      i = i +1
093300030715     C                   ENDDO
093400030715     C*
093500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093600030715     C* delimitazione testo
093700030715     C                   EVAL      i = 1
093800030715     C                   DOW       i < %elem(SkSplitCSV)
093900030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094000030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
094100030715     C                   EVAL      i = i + 1
094200030715     C                   ENDDO
094300100720     C*
094400100720     C* Stacco un progressivo x numero spedizione
094500120301 xxx C                   EXSR      repNSP
094600100326     C*
094700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094800030715     C                   EVAL      i = 1
094900030715     C                   DOW       i < %elem(SkSplitFLD)
095000030715     C*
095100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
095200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
095300030715     C                             SkSplitCSV(i) <> *blanks
095400030721     C*** ATB
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095600030721     C                   EVAL      VABATB = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030715     C*** RSD
095900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
096100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
096300030715     C* ==
096400030715     C                   ENDIF
096500030721     C*** RD2
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** IND
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
097100030715     C                   EVAL      VABIND = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** LOD
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** PRD
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030721     C*** NZD
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
098300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030715     C*** RMA
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** NOT
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
099100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** NT2
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** VCA
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** VAS
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
100300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030715     C*** FFD
100600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100800030715     C                   ENDIF
100900030721     C*** GC1
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
101100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GC2
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TSP
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102000030721     C                   ENDIF
102100040510     C*** CBO
102200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
102300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102400040510     C                   ENDIF
102500030721     C*** NAS
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** TIC
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
103100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GCA
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** XCO
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** CTM
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
104300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TCR
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** CTS
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
105100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** FTM
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** VAD
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** GMA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
106300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GGA
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** GVA
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
107100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TC1
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** TC2
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** SCL
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
108300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** RMO
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*** NMO
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
109100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
109200030721     C                   ENDIF
109300030721     C*
109400030721     C* Reperisco quindi i campi numerici...
109500040510     C*** NSP
109600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109700040510     C                   EVAL      PiStr=SkSplitCSV(i)
109800040510     C                   EXSR      CHKNUM
109900040510     C                   IF        PiInt=*on
110000040510     C                   Z-ADD     PiVal         VABNSP
110100040510     C                   ELSE
110200040510     C                   SETON                                        31
110300040510     C                   EVAL      VABNSP = *zeros
110400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110500040510     C                             + ' ' + 'VABNSP'
110600040510     C                   ENDIF
110700040510     C                   ENDIF
110800071106     C*** RMN
110900071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111000071106     C                   EVAL      PiStr=SkSplitCSV(i)
111100071106     C                   EXSR      CHKNUM
111200071106     C                   IF        PiInt=*on
111300071106     C                   Z-ADD     PiVal         VABRMN
111400071106     C                   ELSE
111500071106     C                   SETON                                        32
111600071106     C                   EVAL      VABRMN = *zeros
111700071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111800071106     C                             + ' ' + 'VABRMN'
111900071106     C                   ENDIF
112000071106     C                   ENDIF
112100030721     C*** CAD
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on
112600030721     C                   Z-ADD     PiVal         Num5_0
112700030721     C                   MOVEL(p)  Num5_0        VABCAD
112800030721     C                   ELSE
112900030721     C                   SETON                                        32
113000030721     C                   EVAL      VABCAD = *zeros
113100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113200030721     C                             + ' ' + 'VABCAD'
113300030721     C                   ENDIF
113400030721     C                   ENDIF
113500030721     C*** NCL
113600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113700030721     C                   EVAL      PiStr=SkSplitCSV(i)
113800030721     C                   EXSR      CHKNUM
113900030721     C                   IF        PiInt=*on
114000030721     C                   Z-ADD     PiVal         VABNCL
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      VABNCL = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040122     C                             + ' ' + 'VABNCL'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** PKB
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiNum=*on
115300030721     C                   Z-ADD     PiVal         VABPKB
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABPKB = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800030721     C                             + ' ' + 'VABPKB'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** CAS
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
116300110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116400111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
116500111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
116600120227 xxx C                   EVAL      VABvca = 'EUR'
116700130227     C***                EVAL      VABTIC = 'BM'
116800101018     C                   EVAL      wFlgCAS = '1'
116900101018     C                   ENDIF
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiNum=*on
117300030721     C                   Z-ADD     PiVal         VABCAS
117400030721     C                   ELSE
117500030721     C                   SETON                                        32
117600030721     C                   EVAL      VABCAS = *zeros
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABCAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** CCM
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiInt=*on      AND
118600030721     C                             PiVal<=9999999 AND
118700030721     C                             PiVal>*zeros
118800030721     C                   Z-ADD     PiVal         VABCCM
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      VABCCM = *zeros
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300040510     C                             + ' ' + 'VABCCM'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** LNP
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on    AND
120100030721     C                             PiVal<=999   AND
120200030721     C                             PiVal>*zeros
120300030721     C                   Z-ADD     PiVal         VABLNP
120400030721     C                   ELSE
120500040715     C                   SETON                                        32
120600030721     C                   EVAL      VABLNP = *zeros
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800040510     C                             + ' ' + 'VABLNP'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NRS
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on AND
121600030721     C                             PiVal<=99
121700030721     C                   Z-ADD     PiVal         VABNRS
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNRS'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** LNA
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on    AND
122900030721     C                             PiVal<=999   AND
123000030721     C                             PiVal>*zeros
123100030721     C                   Z-ADD     PiVal         VABLNA
123200030721     C                   ELSE
123300030721     C                   SETON                                        32
123400030721     C                   EVAL      VABLNA = *zeros
123500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123600030721     C                             + ' ' + 'VABLNA'
123700030721     C                   ENDIF
123800030721     C                   ENDIF
123900030721     C*** CTR
124000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124100030721     C                   EVAL      PiStr=SkSplitCSV(i)
124200030721     C                   EXSR      CHKNUM
124300030721     C                   IF        PiInt=*on  AND
124400030721     C                             PiVal<=999
124500030721     C                   Z-ADD     PiVal         VABCTR
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABCTR'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** IAS
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABIAS
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABIAS'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** VLB
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiNum=*on
126900030721     C                   Z-ADD     PiVal         VABVLB
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABVLB'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** QFT
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiNum=*on
128100030721     C                   Z-ADD     PiVal         VABQFT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABQFT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** NCD
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on      AND
129300030721     C                             PiVal<=9999999
129400030721     C                   Z-ADD     PiVal         VABNCD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABNCD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** NCA
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on      AND
130600030721     C                             PiVal<=9999999
130700030721     C                   Z-ADD     PiVal         VABNCA
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABNCA'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** ZNC
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on AND
131900030721     C                             PiVal<=99
132000030721     C                   Z-ADD     PiVal         VABZNC
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132400030721     C                             + ' ' + 'VABZNC'
132500030721     C                   ENDIF
132600030721     C                   ENDIF
132700030721     C*** DCR
132800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132900030721     C                   EVAL      PiStr=SkSplitCSV(i)
133000030721     C                   EXSR      CHKNUM
133100030721     C                   IF        PiInt=*on       AND
133200030721     C                             PiVal<=99999999
133300030721     C                   Z-ADD     PiVal         VABDCR
133400030721     C                   ELSE
133500030721     C                   SETON                                        32
133600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133700030721     C                             + ' ' + 'VABDCR'
133800030721     C                   ENDIF
133900030721     C                   ENDIF
134000030721     C*** HCR
134100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134200030721     C                   EVAL      PiStr=SkSplitCSV(i)
134300030721     C                   EXSR      CHKNUM
134400030721     C                   IF        PiInt=*on   AND
134500030721     C                             PiVal<=9999
134600030721     C                   Z-ADD     PiVal         VABHCR
134700030721     C                   ELSE
134800030721     C                   SETON                                        32
134900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135000030721     C                             + ' ' + 'VABHCR'
135100030721     C                   ENDIF
135200030721     C                   ENDIF
135300030721     C*** VMD
135400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135500030721     C                   EVAL      PiStr=SkSplitCSV(i)
135600030721     C                   EXSR      CHKNUM
135700030721     C                   IF        PiNum=*on
135800030721     C                   Z-ADD     PiVal         VABVMD
135900030721     C                   ELSE
136000030721     C                   SETON                                        32
136100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136200030721     C                             + ' ' + 'VABVMD'
136300030721     C                   ENDIF
136400030721     C                   ENDIF
136500030721     C*** ANT
136600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136700030721     C                   EVAL      PiStr=SkSplitCSV(i)
136800030721     C                   EXSR      CHKNUM
136900030721     C                   IF        PiInt=*on        AND
137000030721     C                             PiVal<=999999999
137100030721     C                   Z-ADD     PiVal         VABANT
137200030721     C                   ELSE
137300030721     C                   SETON                                        32
137400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137500030721     C                             + ' ' + 'VABANT'
137600030721     C                   ENDIF
137700030721     C                   ENDIF
137800030721     C*** CMO
137900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138000030721     C                   EVAL      PiStr=SkSplitCSV(i)
138100030721     C                   EXSR      CHKNUM
138200030721     C                   IF        PiInt=*on
138300030721     C                   Z-ADD     PiVal         Num5_0
138400030721     C                   MOVEL(p)  Num5_0        VABCMO
138500030721     C                   ELSE
138600030721     C                   SETON                                        32
138700030721     C                   EVAL      VABCMO = *zeros
138800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138900030721     C                             + ' ' + 'VABCMO'
139000030721     C                   ENDIF
139100030721     C                   ENDIF
139200040510     C*
139300040510     C* Reperisco i campi del FIVAT
139400040510     C*** VATNOT
139500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139700040510     C                   ENDIF
139800040510     C*** VATTRC
139900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140100040510     C                   ENDIF
140200060331     C*** VATNOT_A
140300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140500060331     C                   ENDIF
140600060331     C*** VATNOT_B
140700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140900060331     C                   ENDIF
141000060331     C*** VATNOT_E
141100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400110927     C*** VATNOT_I
141500110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141600110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141700110927     C                   ENDIF
141800070823     C*
141900100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142000120301
142100120301      *** W_TEL + W_CEL = VABNT2
142200120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_TEL'
142300120301     C                   EVAL      VABNT2 = %trim(SkSplitCSV(i))
142400120301     C                   ENDIF
142500120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
142600120301     C                   EVAL      VABNT2 = %trim(VABNT2) + ' ' +
142700120301     C                                      %trim(SkSplitCSV(i))
142800120301     C                   ENDIF
142900120301      *** W_CAS se = 'CONTRASSEGNO' ok x il c/assegno
143000120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CAS'
143100120301     C                             and %trim(SkSplitCSV(i)) <> 'Contrassegno'
143200120301     C                   clear                   VABCAS
143300120301     C                   clear                   VABVCA
143400130227     C***                clear                   VABTIC
143500120301     C                   EVAL      wFlgCAS = '0'
143600120301     C                   ENDIF
143700120301      *** VATNOT_E da VABRMN
143800120301     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
143900120316     C                   eval      VATTRC = 'E'
144000120309      *** CAD
144100120309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
144200120309     C                   eval      VABCAD = %subst(SkSplitCSV(i):2)
144300120309     C                   ENDIF
144400140520
144500140520      *** W_CEL = VATNOT TRC=S
144600140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
144700140520     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
144800140520     C                   ENDIF
144900140520
145000140520      *** W_VATNOTIJ
145100140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
145200140520     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
145300140520     C                   ENDIF
145400070925     C*
145500071029     C***  <===  -----------------------------------------
145600040510     C*
145700030715     C                   ENDIF
145800030715     C                   EVAL      i = i + 1
145900030715     C                   ENDDO
146000070925     C*
146100071029     C***  ===>  Gestione campi "particolari" fuori ciclo
146200100831     C*
146300120316     C                   IF        wVATNOT_E <> *blanks
146400120316     C                   EVAL      VABCTM = '7Q'
146500120316     C                   ENDIF
146600120316     C*
146700120316     C                   EVAL      wVATNOT_B =  VABNT2
146800070925     C*
146900071029     C***  <===  -----------------------------------------
147000070925     C*
147100070925     C* Considerazioni finali su CBO/CAS
147200100318     C                   IF        wFlgCAS = '1'    OR
147300100318     C                             VABCAS  > *zeros
147400070925     C                   IF        VABCBO = '1'
147500070925     C                   EVAL      VABCBO = '4'
147600070925     C                   ENDIF
147700070925     C                   IF        VABCBO = '2'
147800070925     C                   EVAL      VABCBO = '6'
147900070925     C                   ENDIF
148000070925     C                   ENDIF
148100070925     C***  <===  ----------------------------
148200040510     C*
148300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
148400040510     C                   IF        VABNSP = *zeros
148500040510     C                   SETON                                        31
148600040510     C                   ENDIF
148700040715     C*
148800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
148900040715     C                   IF        VABLNP = *zeros
149000040715     C                   SETON                                        31
149100040715     C                   ENDIF
149200040510     C*
149300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
149400040510     C                   IF        VABRMN = *zeros
149500040510     C                   Z-ADD     VABNSP        VABRMN
149600040510     C                   ENDIF
149700040510     C*
149800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
149900040510     C                   IF        VABRMA = *blanks
150000040714     C                   MOVEL     VABRMN        VABRMA
150100040510     C                   ENDIF
150200040128     C*
150300040510     C* Se provincia nn valorizzata la reperisco
150400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
150500110912     C                   IF        VABPRD =  *blanks AND
150600110912     C                             VABNZD =  *blanks AND
150700110912     C                             VABCAD <> *blanks AND
150800110912     C                             VABLOD <> *blanks
150900040128     C                   CLEAR                   TISI95DS
151000040128     C                   EVAL      I95TCN = '3'
151100040128     C                   Z-ADD     datcor        I95DAT
151200040128     C                   EVAL      I95NAR = VABNZD
151300040128     C                   EVAL      I95CAP = VABCAD
151400040128     C                   EVAL      I95LOC = VABLOD
151500040128     C                   CALL      'TISI95R'
151600040128     C                   PARM                    TISI95DS
151700040128     C                   EVAL      VABPRD = O95PRV
151800040510     C                   ENDIF
151900020204     C*
152000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
152100020204     C                   EXSR      CHKIMPDIV
152200030715     C                   ENDIF
152300020204     C*
152400010607     C                   ENDSR
152500010607     C*----------------------------------------------------*
152600010601
152700020204
152800020204     C*----------------------------------------------------*
152900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
153000020204     C*----------------------------------------------------*
153100020204     C     CHKIMPDIV     BEGSR
153200020204     C*
153300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
153400020204     C                   Z-ADD     *zeros        wrkDec            9 9
153500020204     C*
153600020204     C* Come prima cosa effettuo considerazioni sulla divisa
153700020204     C                   IF        vabIAS > *zeros
153800020204     C                   IF        vabVAS <> 'EUR'
153900020204     C                   EVAL      vabVAS =  'ITL'
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   IF        vabCAS > *zeros
154400020204     C                   IF        vabVCA <> 'EUR'
154500020204     C                   EVAL      vabVCA =  'ITL'
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   IF        vabVMD > *zeros
155000020204     C                   IF        vabVAD <> 'EUR'
155100020204     C                   EVAL      vabVAD =  'ITL'
155200020204     C                   ENDIF
155300020204     C                   ENDIF
155400020204     C*
155500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
155600020204     C                   Z-ADD     vabIAS        wrkDec
155700020204     C                   IF        wrkDec > *zeros
155800020204     C                   IF        vabVAS = 'ITL'
155900020204     C                   EVAL      vabIAS = *zeros
156000020204     C                   ENDIF
156100020204     C                   ENDIF
156200020204     C*
156300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
156400020204     C                   Z-ADD     vabCAS        wrkDec
156500020204     C                   IF        wrkDec > *zeros
156600020204     C                   IF        vabVCA = 'ITL'
156700020204     C                   EVAL      vabCAS = *zeros
156800020204     C                   ENDIF
156900020204     C                   ENDIF
157000020204     C*
157100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
157200020204     C                   Z-ADD     vabVMD        wrkDec
157300020204     C                   IF        wrkDec > *zeros
157400020204     C                   IF        vabVAD = 'ITL'
157500020204     C                   EVAL      vabVMD = *zeros
157600020204     C                   ENDIF
157700020204     C                   ENDIF
157800020204     C*
157900020204     C                   ENDSR
158000020204     C***
158100010330
158200100318
158300100318
158400100318     C*----------------------------------------------------*
158500100318     C*  CONTROLLO NUMERICITA' CAMPI
158600100318     C*----------------------------------------------------*
158700100318     C     CHKNUM        BEGSR
158800100318     C*
158900100318     C                   IF        PiDecChr = *blanks
159000100318     C                   EVAL      PiDecChr = CharNUM
159100100318     C                   ENDIF
159200100318     C*
159300100318     C                   callp     UBISNUM_Check(PiStr
159400100318     C                                          :PiDecChr
159500100318     C                                          :PiVal
159600100318     C                                          :PiNum
159700100318     C                                          :PiInt)
159800100318     C*
159900100318     C                   ENDSR
160000100318     C***
160100010601
160200010601
160300010601
160400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
160500010601     C     repfil        BEGSR
160600010601     C*
160700010601     C                   if        invfil = *zeros and
160800010601     C                             depfil > *zeros and
160900010629     C                             (vinflg = *blanks or
161000010629     C                              vinflg = *zeros)
161100010601     C
161200010601     C                   eval      invfil = depfil
161300010601     C                   endif
161400010601     C*
161500010601     C                   if        depfil <> invfil and
161600010601     C                             invfil > *zeros
161700010601     C                   eval      flgMulti = '1'
161800010601     C                   if        vinflg = *blanks
161900010601     C                   add       1             cntNonEl
162000010601     C                   endif
162100010601     C                   endif
162200010601     C*
162300010601     C                   if        vinflg = '2'
162400010601     C                   eval      flgStato = '2'
162500010601     C                   endif
162600010601     C*
162700010601     C                   ENDSR
162800010601     C***
162900010601
163000010601
163100010601
163200010330
163300040119      /TITLE Invio dei dati al punto operativo.
163400040119     C     invio         BEGSR
163500040119     C*
163600040119     C* 1° invio FIVAT
163700040119     C                   reset                   dscmz
163800040119     C                   move      vlrpoi        cmzdst
163900040119     C                   eval      cmzfld = 'FIVATWWR'
164000040119     C                   eval      cmzmbd = vlrhdl
164100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164200040119     C***                if        prmfir = *blanks
164300040119     C                   eval      cmzfla = 'FIVAT00F'
164400040119     C                   eval      cmzmba = 'FIVAT00F'
164500040119     C***                else
164600040119     C***                eval      cmzfla = prmfir
164700040119     C***                eval      cmzmba = prmfir
164800040119     C***                endif
164900040119     C                   eval      cmznrr = *zeros
165000040119     C                   move      §ctrokvt      cmznrr
165100040119     C                   eval      cmzlba = vlrfl1
165200040119     C                   call(e)   'TIS711C'
165300040119     C                   parm                    dscmz
165400040119     C                   parm      *blanks       esito
165500040119     C                   if        %error
165600040119     C                             or cmzerr = '1'
165700040119     C                             or esito  = '1'
165800040119     C                   eval      wrkesito = '3'
165900040119     C                   else
166000040119     C*
166100040119     C* 2° invio FIVAB
166200040119     C                   reset                   dscmz
166300040119     C                   move      vlrpoi        cmzdst
166400040119     C                   eval      cmzfld = vlrfou
166500040119     C                   eval      cmzmbd = vlrhdl
166600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
166700040119     C***                if        prmfir = *blanks
166800040119     C                   eval      cmzfla = 'FIVAB00F'
166900040119     C                   eval      cmzmba = 'FIVAB00F'
167000040119     C***                else
167100040119     C***                eval      cmzfla = prmfir
167200040119     C***                eval      cmzmba = prmfir
167300040119     C***                endif
167400040119     C                   eval      cmznrr = *zeros
167500040119     C                   move      §ctrokvb      cmznrr
167600040119     C                   eval      cmzlba = vlrfl1
167700040119     C                   call(e)   'TIS711C'
167800040119     C                   parm                    dscmz
167900040119     C                   parm      *blanks       esito
168000040119     C                   if        %error
168100040119     C                             or cmzerr = '1'
168200040119     C                             or esito  = '1'
168300040119     C                   eval      wrkesito = '3'
168400040119     C                   endif
168500040119     C                   endif
168600040119     C*
168700040119     C                   ENDSR
168800040119     C***
168900010601
169000010601
169100010601
169200010601
169300010601      /TITLE Invio dei dati al punto operativo.
169400010601     C     opeini        BEGSR
169500010601     C*
169600010601     C* Inizializzo flag e contatori operativi
169700010601     C                   movel     '0'           flgGiro           1
169800010601     C                   movel     '0'           flgMulti          1
169900010601     C                   movel     '1'           flgStato          1
170000010615     C                   movel     '0'           flgOk             1
170100010601     C                   z-add     *zeros        cntNonEl         10 0
170200010601     C                   z-add     *zeros        depfil            3 0
170300010601     C                   z-add     *zeros        invfil            3 0
170400010601     C*
170500010601     C                   ENDSR
170600010601     C***
170700070326
170800070326
170900070326
171000070326
171100070326     C     *pssr         BEGSR
171200070329     C*
171300070329     C                   if        %open(tivin00r)
171400070329     C                   close     tivin00r
171500070329     C                   endif
171600070329     C                   if        %open(fivabwwr)
171700070329     C                   close     fivabwwr
171800070329     C                   endif
171900070329     C                   if        %open(fivatwwr)
172000070329     C                   close     fivatwwr
172100070329     C                   endif
172200070326     C*
172300070326     C* Effettuo la chiamata al CLLE preposto
172400070326     C                   call(e)   'TITVVTC'
172500070326     C                   parm                    parccm
172600070326     C                   parm                    parmbr
172700070326     C                   parm      '2'           paropz
172800070326     C*
172900070326     C                   eval      wrkesito = '2'
173000070404     C*
173100070404     C                   seton                                        LR
173200070326     C*
173300070326     C                   ENDSR     '*CANCL'
173400070326     C***
173500100318
173600100318
173700100318
173800100318     C     normCSV       BEGSR
173900100318     C*
174000100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
174100100318     C* separatori campi
174200100318     C                   if        CharTXT <> *blanks
174300100318     C                   z-add     *zeros        wLenVINDTA        4 0
174400100318     C                   z-add     *zeros        wFlgVINDTA        1 0
174500100318     C                   setoff                                       80
174600100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
174700100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
174800100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
174900100318     C                   if        not *in80
175000100318     C                   seton                                        80
175100100318     C                   else
175200100318     C                   setoff                                       80
175300100318     C                   endif
175400100318     C                   endif
175500100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
175600100318     C                             and *in80 = *on
175700100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
175800100318     C                   endif
175900100318     C                   enddo
176000100318     C                   endif
176100100318     C*
176200100318     C                   ENDSR
176300100318     C***
176400070326
176500100326
176600100326
176700100326     C*----------------------------------------------------*
176800100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
176900100326     C*----------------------------------------------------*
177000100326     C     REPNSP        BEGSR
177100100326     C*
177200100326     C* NSP => Stacco un numeratore da AZNUM
177300100326     C                   clear                   TRUL33DS
177400100326     C                   eval      I33OPE = *zeros
177500100326     C                   eval      I33CNU = 302
177600100326     C                   eval      I33NUM = 1
177700100326     C                   movel     TRUL33DS      KPJBU
177800100326     C                   call      'TRUL33R'
177900100326     C                   parm                    KPJBA
178000100326     C                   movel     KPJBU         TRUL33DS
178100100326     C                   if        O33ERR = *zeros
178200100326     C                   z-add     O33NRF        VABNSP
178300100326     C                   z-add     O33NRF        VATNSP
178400100326     C                   else
178500100326     C                   SETON                                        31
178600100326     C                   EVAL      vinmsg = %trimr(vinmsg)
178700100326     C                             + ' ' + 'VABNSP VATNSP'
178800100326     C                   endif
178900100326     C*
179000100326     C                   ENDSR
179100010330
179200100326
179300010330
179400000613     C     *inzsr        BEGSR
179500990910     C*
179600990910     C     *entry        plist
179700990920     C                   parm                    tivlrds
179800990921     C                   parm      wrkesito      esito
179900000724     C                   parm                    prmlit
180000000710     C                   parm                    prmfir
180100010330     C*
180200010330     C* CALCOLA LA DATA CORRENTE
180300100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
180400100318     C                   eval      datcor = %dec(%date() : *ISO)
180500000613     C*
180600000613     C                   ENDSR
180700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
180800120309VABRMN;VABRMA;VABRD2;VABRSD;VABIND;VABLOD;W_VABCAD;VABPRD;W_TEL;W_CEL;
180900140707W_VATNOTIJ;X;VABCAS;W_CAS;VABNOT;VABTIC;VABNCL;VABPKB;
181000120227
181100111116
181200100318
