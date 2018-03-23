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
034700140718     C* per impedire che il peso sia 0
034800140718     C                   IF        *in31 = '0' and
034900140718     C                             ok_VABPKB = 0
035000140718     C                   EVAL      ok_VABPKB = 0,2
035100140718     C                   ENDIF                                                                     C
035200080527     C  N31              exsr      wrivab
035300080527     C  N31              eval      fivabds_sav = fivabds
035400080527     C                   eval      depspe = vabNSP
035500080527     C                   endif
035600080527     C                   endif
035700080527     C*
035800080527     C                   ENDSR
035900040119
036000040119
036100040119
036200040119     C*----------------------------------------------------*
036300040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036400040119     C*----------------------------------------------------*
036500040510     C     WRIVAB        BEGSR
036600080513     C*
036700080513     C* Gestisco l'eventuale rottura x numero spedizione
036800080527     C                   if        VABNSP <> depspe or *in65 = *on
036900070301     C                   if        wVATNOT_E <> *blanks
037000070301     C                   eval      VABCTM = '7Q'
037100070301     C                   endif
037200100413     C                   write     fivab000
037300080513     C                   endif
037400040119     C*
037500040119     C                   ENDSR
037600040119
037700040119
037800040119
037900040119     C*----------------------------------------------------*
038000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038100040119     C*----------------------------------------------------*
038200040510     C     WRIVAT        BEGSR
038300060512     C*
038400060512     C* Inizializzo i campi chiave
038500060512     C                   eval      VATFGS = VABFGS
038600060512     C                   eval      VATAAS = VABAAS
038700060512     C                   eval      VATCCM = VABCCM
038800070823     C                   eval      VATNRS = VABNRS
038900060512     C                   eval      VATNSP = VABNSP
039000060512     C                   eval      VATLNP = VABLNP
039100040119     C*
039200040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039300040510     C                   if        VATTRC = *blanks
039400040510     C                   eval      VATTRC = 'E'
039500040510     C                   endif
039600110318     C                   if        VATNOT <> *blanks
039700040510     C                   write     FIVAT000
039800040119     C                   add       1             §CTROKVT
039900110318     C                   endif
040000060331     C*
040100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
040200060331     C                   if        wVATNOT_A <> *blanks
040300060331     C                   eval      VATNOT = wVATNOT_A
040400060331     C                   eval      VATTRC = 'A'
040500060331     C                   write     FIVAT000
040600060331     C                   add       1             §CTROKVT
040700060331     C                   endif
040800060331     C*
040900060331     C                   if        wVATNOT_B <> *blanks
041000060331     C                   eval      VATNOT = wVATNOT_B
041100060331     C                   eval      VATTRC = 'B'
041200060331     C                   write     FIVAT000
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500060331     C*
041600060331     C                   if        wVATNOT_E <> *blanks
041700060331     C                   eval      VATTRC = 'E'
041800140707     C*
041900140707     C* sviluppo i CHI SONO rispetto al nr.colli aggiungendo un progressivo di 2 char a dx
042000140707     C     1             DO        VABNCL        wX
042100140707     C                   eval      VATNOT = %trimr(wVATNOT_E) +
042200140707     C                                      %editc(wx:'X')
042300110201     C                   write     FIVAT000
042400140707     C                   ENDDO
042500140707     C*
042600110201     C                   add       1             §CTROKVT
042700110201     C                   endif
042800110927     C*
042900140520     C                   if        %subst(wVATNOT_IJ:1:35) <> *blanks
043000140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
043100110927     C                   eval      VATTRC = 'I'
043200110927     C                   write     FIVAT000
043300110927     C                   add       1             §CTROKVT
043400110927     C                   endif
043500140520     C*
043600140520     C                   if        %subst(wVATNOT_IJ:36:35) <> *blanks
043700140520     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
043800140520     C                   eval      VATTRC = 'J'
043900140520     C                   write     FIVAT000
044000140520     C                   add       1             §CTROKVT
044100140520     C                   endif
044200140520     C*
044300140520     C                   if        wVATNOT_S <> *blanks
044400140520     C                   EVAL      %subst(wVATNOT_S:17:2) = 'NN'
044500150223     C* per il cliente 1710518 si attivano gli alert
044600150223     C                   IF        VATCCM = 1710518
044700150223     C                   EVAL      %subst(wVATNOT_S:17:2) = 'SS'
044800150223     C                   ENDIF
044900140520     C                   eval      VATNOT = wVATNOT_S
045000140520     C                   eval      VATTRC = 'S'
045100140520     C                   write     FIVAT000
045200140520     C                   add       1             §CTROKVT
045300140520     C                   endif
045400040119     C*
045500040119     C                   ENDSR
045600040119
045700040119
045800040119
045900040119     C*----------------------------------------------------*
046000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046100040119     C*----------------------------------------------------*
046200040119     C     PREVAT        BEGSR
046300040119     C*
046400040119     C* Compongo il nome del membro da dare al FIVATWWR
046500040119     C                   eval      parmbr = vlrhdl
046600040119     C                   movel     'M'           parmbr
046700040510     C                   eval      parccm = vlrksc
046800040119     C                   eval      paropz = '1'
046900040119     C* Effettuo la chiamata al CLLE preposto
047000040119     C                   call(e)   'TITVVTC'
047100040119     C                   parm                    parccm
047200040119     C                   parm                    parmbr
047300040119     C                   parm                    paropz
047400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047500040119     C                   if        %error
047600040119     C                   movel     '1'           chkcall
047700040119     C                   else
047800040119     C                   movel     '0'           chkcall
047900040119     C                   endif
048000040119     C*
048100040119     C                   ENDSR
048200010601
048300010601
048400010601
048500010330     C*----------------------------------------------------*
048600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048700010330     C*----------------------------------------------------*
048800040119     C     INZVAR        BEGSR
048900080527     C*
049000080527     C                   setoff                                       65
049100010330     C*
049200030822     C* Inizializzo variabili di wrk
049300020204     C                   Z-ADD     *zeros        Num5_0            5 0
049400101018     C                   MOVEL     *blanks       wFlgCAS
049500060331     C                   MOVEL     *blanks       wVATNOT_A
049600060331     C                   MOVEL     *blanks       wVATNOT_B
049700060331     C                   MOVEL     *blanks       wVATNOT_E
049800110927     C                   MOVEL     *blanks       wVATNOT_I
049900140520     C                   MOVEL     *blanks       wVATNOT_J
050000140520     C                   MOVEL     *blanks       wVATNOT_IJ
050100140520     C                   MOVEL     *blanks       wVATNOT_S
050200030822     C*
050300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050400030822     C                   CLEAR                   SkSplitCSV
050500030822     C                   EVAL      i = 1
050600030822     C                   EVAL      posDa = *zeros
050700030822     C                   EVAL      posA  = *zeros
050800080527     C*
050900080527     C                   CLEAR                   FIVABDS
051000080527     C                   CLEAR                   FIVAT000
051100030822     C*
051200030822     C* Reimposto i valori di default
051300030822     C                   EXSR      DEFCAM
051400030822     C*
051500010330     C                   ENDSR
051600010330     C*----------------------------------------------------*
051700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
051800010330     C*----------------------------------------------------*
051900010330     C     DEFCAM        BEGSR
052000030822     C*
052100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052200030715     C* e delimitatore testo.
052300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052600100318     C*
052700100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
052800100318     C                   EVAL      CharSOS = CharNUM
052900030822     C*
053000030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053100100720     C                   SETOFF                                       505152
053200100720     C                   SETOFF                                       53
053300100413     C                   SELECT
053400100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053500100720     C                   SETON                                        50
053600100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053700100413     C                   SETON                                        51
053800100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
053900100720     C                   SETON                                        52
054000100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054100100720     C                   SETON                                        53
054200100413     C                   ENDSL
054300030822     C*
054400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054500030822     C                   EVAL      posDaDft = 1
054600030822     C                   EVAL      posADft  = 0
054700030822     C                   EVAL      wGiroDft = 0
054800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
054900030822     C                             posDaDft > 0
055000030822     C*
055100030822     C* Gestisco il 1° giro
055200030822     C                   IF        wGiroDft = 0
055300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055500030822     C* Incremento il contatore dei "giri"
055600030822     C                   EVAL      wGiroDft = 1
055700030822     C                   ELSE
055800030822     C                   EVAL      posDaDft = posADft
055900030822     C                   ENDIF
056000030822     C* Eseguo lo scan x trovare la fine del campo corrente
056100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056200030822     C*
056300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056500030822     C                   IF        posDaDft > 0 AND
056600030822     C                             posADft  > 0
056700030822     C* NCL
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'NCL'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABNCL
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* CCM
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'CCM'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABCCM
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* LNP
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'LNP'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABLNP
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* NRS
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'NRS'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABNRS
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* CTR
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'CTR'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiInt=*on
062900030822     C                   Z-ADD     PiVal         VABCTR
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* PKB
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'PKB'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiNum=*on
064200030822     C                   Z-ADD     PiVal         VABPKB
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* VLB
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'VLB'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500030822     C                   Z-ADD     PiVal         VABVLB
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* QFT
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'QFT'
066300030822     C                   EVAL      PiStr=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   EXSR      CHKNUM
066700030822     C                   IF        PiNum=*on
066800030822     C                   Z-ADD     PiVal         VABQFT
066900030822     C                   ENDIF
067000030822     C                   ENDIF
067100030822     C* CBO
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'CBO'
067600030822     C                   EVAL      VABCBO=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* TSP
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'TSP'
068500030822     C                   EVAL      VABTSP=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900030822     C* VAS
069000030822     C                   IF        %subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):1:3)
069300030822     C                             = 'VAS'
069400030822     C                   EVAL      VABVAS=%trim(%subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700030822     C                   ENDIF
069800030822     C* VCA
069900030822     C                   IF        %subst(
070000030822     C                             %subst(vlrppt:posDaDft+1:
070100030822     C                             posADft-posDaDft-1):1:3)
070200030822     C                             = 'VCA'
070300030822     C                   EVAL      VABVCA=%trim(%subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070600030822     C                   ENDIF
070700070925     C* TIC
070800070925     C                   IF        %subst(
070900070925     C                             %subst(vlrppt:posDaDft+1:
071000070925     C                             posADft-posDaDft-1):1:3)
071100070925     C                             = 'TIC'
071200070925     C                   EVAL      VABTIC=%trim(%subst(
071300070925     C                             %subst(vlrppt:posDaDft+1:
071400070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071500070925     C                   ENDIF
071600030822     C* GCA
071700030822     C                   IF        %subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):1:3)
072000030822     C                             = 'GCA'
072100030822     C                   EVAL      VABGCA=%trim(%subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072400030822     C                   ENDIF
072500030822     C* CTM
072600030822     C                   IF        %subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):1:3)
072900030822     C                             = 'CTM'
073000030822     C                   EVAL      VABCTM=%trim(%subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073300030822     C                   ENDIF
073400030822     C* FFD
073500030822     C                   IF        %subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):1:3)
073800030822     C                             = 'FFD'
073900030822     C                   EVAL      VABFFD=%trim(%subst(
074000030822     C                             %subst(vlrppt:posDaDft+1:
074100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074200030822     C                   ENDIF
074300030822     C* VAD
074400030822     C                   IF        %subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):1:3)
074700030822     C                             = 'VAD'
074800030822     C                   EVAL      VABVAD=%trim(%subst(
074900030822     C                             %subst(vlrppt:posDaDft+1:
075000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075100030822     C                   ENDIF
075200030822     C* GMA
075300030822     C                   IF        %subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):1:3)
075600030822     C                             = 'GMA'
075700030822     C                   EVAL      VABGMA=%trim(%subst(
075800030822     C                             %subst(vlrppt:posDaDft+1:
075900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076000030822     C                   ENDIF
076100030822     C* GGA
076200030822     C                   IF        %subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):1:3)
076500030822     C                             = 'GGA'
076600030822     C                   EVAL      VABGGA=%trim(%subst(
076700030822     C                             %subst(vlrppt:posDaDft+1:
076800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076900030822     C                   ENDIF
077000030822     C* GVA
077100030822     C                   IF        %subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):1:3)
077400030822     C                             = 'GVA'
077500030822     C                   EVAL      VABGVA=%trim(%subst(
077600030822     C                             %subst(vlrppt:posDaDft+1:
077700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077800030822     C                   ENDIF
077900030822     C* TC1
078000030822     C                   IF        %subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):1:3)
078300030822     C                             = 'TC1'
078400030822     C                   EVAL      VABTC1=%trim(%subst(
078500030822     C                             %subst(vlrppt:posDaDft+1:
078600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078700030822     C                   ENDIF
078800030822     C* TC2
078900030822     C                   IF        %subst(
079000030822     C                             %subst(vlrppt:posDaDft+1:
079100030822     C                             posADft-posDaDft-1):1:3)
079200030822     C                             = 'TC2'
079300030822     C                   EVAL      VABTC2=%trim(%subst(
079400030822     C                             %subst(vlrppt:posDaDft+1:
079500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079600030822     C                   ENDIF
079700040714     C* VATTRC
079800040714     C                   IF        %subst(
079900040714     C                             %subst(vlrppt:posDaDft+1:
080000040714     C                             posADft-posDaDft-1):1:3)
080100040714     C                             = 'TRC'
080200040714     C                   EVAL      VATTRC=%trim(%subst(
080300040714     C                             %subst(vlrppt:posDaDft+1:
080400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080500040714     C                   ENDIF
080600030822     C* ...
080700030822     C                   ENDIF
080800030822     C                   ENDDO
080900020204     C*
081000010330     C                   ENDSR
081100010607     C*----------------------------------------------------*
081200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081300010607     C*----------------------------------------------------*
081400010607     C     IMPVAB        BEGSR
081500010607     C*
081600010607     C                   SETOFF                                       3132
081700110315 xxx C***                SETON                                        35
081800030515     C*
081900010607     C                   MOVEL     datcor        VABAAS
082000010607     C                   MOVE      datcor        VABMGS
082100010607     C*
082200030715     C**********
082300030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082400030715     C**********
082500030715     C                   IF        wGiro = *zeros
082600040113     C*
082700040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
082800040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
082900080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083000080429     C                             %trim(intcol(5))<>*blanks
083100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083200080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083300080429     C                                      %trim(intcol(5))
083400070823     C     ';':CharCSV   XLATE     vindta        vindta
083500040510     C                   ENDIF
083600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083700031201     C     minu:maiu     XLATE     vindta        vindta
083800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083900030715     C                   DOW       posDa <= %len(%trim(vindta))
084000030715     C*
084100030715     C* Gestisco il 1° campo
084200030715     C                   IF        i = 1
084300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084400030715     C                   EVAL      posDa = 1
084500030715     C* Eseguo lo scan x trovare la fine del primo campo
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C* X i campi successivi al 1°
085100030715     C                   ELSE
085200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085300030715     C                   EVAL      posDa = posA + 1
085400030715     C* Eseguo lo scan x trovare la fine del campo corrente
085500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085600030715     C* Gestisco l'ultimo campo
085700030715     C                   IF        posA = *zeros
085800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085900030715     C* A questo punto "estraggo" il campo corrente
086000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086100030715     C                                                    (posA-posDa))
086200030715     C                   LEAVE
086300030715     C                   ELSE
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C                   ENDIF
086800030715     C                   ENDIF
086900030715     C* Incremento il contatore d campo
087000030715     C                   EVAL      i = i +1
087100030715     C                   ENDDO
087200030715     C*
087300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087400030715     C* delimitazione testo
087500030715     C                   EVAL      i = 1
087600030715     C                   DOW       i < %elem(SkSplitFLD)
087700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
087800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
087900030715     C                   EVAL      i = i + 1
088000030715     C                   ENDDO
088100030715     C                   EVAL      wGiro = 1
088200030715     C                   ELSE
088300030715     C                   EVAL      wGiro = 2
088400031204     C**********
088500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088600031204     C* d separatore campo
088700031204     C**********
088800031204     C                   EVAL      vindta = %trim(vindta)
088900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089000031204     C                   EVAL      vindta = %subst(vindta:2)
089100031204     C                   ENDDO
089200031204     C*
089300031204     C                   Z-ADD     *zeros        lunghInput        4 0
089400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089800110912     C                   ENDDO
089900030715     C**********
090000030715     C* Effettuo lo split del campo dati d input
090100030715     C**********
090200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090300030715     C                   DOW       posDa <= %len(%trim(vindta))
090400030715     C*
090500030715     C* Gestisco il 1° campo
090600030715     C                   IF        i = 1
090700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
090800030715     C                   EVAL      posDa = 1
090900030715     C* Eseguo lo scan x trovare la fine del primo campo
091000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091100030715     C* A questo punto "estraggo" il campo corrente
091200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091300030715     C                                                    (posA-posDa))
091400030715     C* X i campi successivi al 1°
091500030715     C                   ELSE
091600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091700030715     C                   EVAL      posDa = posA + 1
091800031204     C* Verifico che nn vi sia il campo nn valorizzato
091900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092000031204     C* Se campo nn valorizzato skippo al prossimo
092100031204     C                   EVAL      SkSplitCSV(i) = *blanks
092200031204     C                   EVAL      posA  = posA + 1
092300031204     C                   ELSE
092400030715     C* Eseguo lo scan x trovare la fine del campo corrente
092500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092600030715     C* Gestisco l'ultimo campo
092700030715     C                   IF        posA = *zeros
092800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
092900030715     C* A questo punto "estraggo" il campo corrente
093000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093100030715     C                                                    (posA-posDa))
093200030715     C                   LEAVE
093300030715     C                   ELSE
093400030715     C* A questo punto "estraggo" il campo corrente
093500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093600030715     C                                                    (posA-posDa))
093700030715     C                   ENDIF
093800031204     C                   ENDIF
093900030715     C                   ENDIF
094000030715     C* Incremento il contatore d campo
094100030715     C                   EVAL      i = i +1
094200030715     C                   ENDDO
094300030715     C*
094400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094500030715     C* delimitazione testo
094600030715     C                   EVAL      i = 1
094700030715     C                   DOW       i < %elem(SkSplitCSV)
094800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
094900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095000030715     C                   EVAL      i = i + 1
095100030715     C                   ENDDO
095200100720     C*
095300100720     C* Stacco un progressivo x numero spedizione
095400120301 xxx C                   EXSR      repNSP
095500100326     C*
095600030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095700030715     C                   EVAL      i = 1
095800030715     C                   DOW       i < %elem(SkSplitFLD)
095900030715     C*
096000030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096100030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096200030715     C                             SkSplitCSV(i) <> *blanks
096300030721     C*** ATB
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096500030721     C                   EVAL      VABATB = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030715     C*** RSD
096800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
096900030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097000030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097100030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097200030715     C* ==
097300030715     C                   ENDIF
097400030721     C*** RD2
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097600030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030715     C*** IND
097900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098000030715     C                   EVAL      VABIND = SkSplitCSV(i)
098100030715     C                   ENDIF
098200030715     C*** LOD
098300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098400030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098500030715     C                   ENDIF
098600030715     C*** PRD
098700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
098800030715     C                   EVAL      VABPRD = SkSplitCSV(i)
098900030715     C                   ENDIF
099000030721     C*** NZD
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099200030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030715     C*** RMA
099500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099600030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099700030715     C                   ENDIF
099800030715     C*** NOT
099900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100000030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100100030715     C                   ENDIF
100200030715     C*** NT2
100300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100500030715     C                   ENDIF
100600030715     C*** VCA
100700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
100800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
100900030715     C                   ENDIF
101000030715     C*** VAS
101100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101300030715     C                   ENDIF
101400030715     C*** FFD
101500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101700030715     C                   ENDIF
101800030721     C*** GC1
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** GC2
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** TSP
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
102800030721     C                   EVAL      VABTSP = SkSplitCSV(i)
102900030721     C                   ENDIF
103000040510     C*** CBO
103100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103200040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103300040510     C                   ENDIF
103400030721     C*** NAS
103500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103600030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103700030721     C                   ENDIF
103800030721     C*** TIC
103900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104000030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104100030721     C                   ENDIF
104200030721     C*** GCA
104300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104400030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104500030721     C                   ENDIF
104600030721     C*** XCO
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
104800030721     C                   EVAL      VABXCO = SkSplitCSV(i)
104900030721     C                   ENDIF
105000030721     C*** CTM
105100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105200030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105300030721     C                   ENDIF
105400030721     C*** TCR
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105600030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105700030721     C                   ENDIF
105800030721     C*** CTS
105900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106000030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106100030721     C                   ENDIF
106200030721     C*** FTM
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106400030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106500030721     C                   ENDIF
106600030721     C*** VAD
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
106800030721     C                   EVAL      VABVAD = SkSplitCSV(i)
106900030721     C                   ENDIF
107000030721     C*** GMA
107100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107200030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107300030721     C                   ENDIF
107400030721     C*** GGA
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107600030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107700030721     C                   ENDIF
107800030721     C*** GVA
107900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108000030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108100030721     C                   ENDIF
108200030721     C*** TC1
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108400030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108500030721     C                   ENDIF
108600030721     C*** TC2
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
108800030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
108900030721     C                   ENDIF
109000030721     C*** SCL
109100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109200030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109300030721     C                   ENDIF
109400030721     C*** RMO
109500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109600030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109700030721     C                   ENDIF
109800030721     C*** NMO
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110000030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110100030721     C                   ENDIF
110200030721     C*
110300030721     C* Reperisco quindi i campi numerici...
110400040510     C*** NSP
110500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110600040510     C                   EVAL      PiStr=SkSplitCSV(i)
110700040510     C                   EXSR      CHKNUM
110800040510     C                   IF        PiInt=*on
110900040510     C                   Z-ADD     PiVal         VABNSP
111000040510     C                   ELSE
111100040510     C                   SETON                                        31
111200040510     C                   EVAL      VABNSP = *zeros
111300040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040510     C                             + ' ' + 'VABNSP'
111500040510     C                   ENDIF
111600040510     C                   ENDIF
111700071106     C*** RMN
111800071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
111900071106     C                   EVAL      PiStr=SkSplitCSV(i)
112000071106     C                   EXSR      CHKNUM
112100071106     C                   IF        PiInt=*on
112200071106     C                   Z-ADD     PiVal         VABRMN
112300071106     C                   ELSE
112400071106     C                   SETON                                        32
112500071106     C                   EVAL      VABRMN = *zeros
112600071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112700071106     C                             + ' ' + 'VABRMN'
112800071106     C                   ENDIF
112900071106     C                   ENDIF
113000030721     C*** CAD
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on
113500030721     C                   Z-ADD     PiVal         Num5_0
113600030721     C                   MOVEL(p)  Num5_0        VABCAD
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABCAD = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABCAD'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** NCL
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on
114900030721     C                   Z-ADD     PiVal         VABNCL
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABNCL = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400040122     C                             + ' ' + 'VABNCL'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** PKB
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiNum=*on
116200030721     C                   Z-ADD     PiVal         VABPKB
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABPKB = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABPKB'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** CAS
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117200110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117300111116     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
117400111116     C                             %trim(SkSplitCSV(i)) <> '0.00'
117500120227 xxx C                   EVAL      VABvca = 'EUR'
117600130227     C***                EVAL      VABTIC = 'BM'
117700101018     C                   EVAL      wFlgCAS = '1'
117800101018     C                   ENDIF
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200030721     C                   Z-ADD     PiVal         VABCAS
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      VABCAS = *zeros
118600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118700030721     C                             + ' ' + 'VABCAS'
118800030721     C                   ENDIF
118900030721     C                   ENDIF
119000030721     C*** CCM
119100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119200030721     C                   EVAL      PiStr=SkSplitCSV(i)
119300030721     C                   EXSR      CHKNUM
119400030721     C                   IF        PiInt=*on      AND
119500030721     C                             PiVal<=9999999 AND
119600030721     C                             PiVal>*zeros
119700030721     C                   Z-ADD     PiVal         VABCCM
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      VABCCM = *zeros
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200040510     C                             + ' ' + 'VABCCM'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** LNP
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on    AND
121000030721     C                             PiVal<=999   AND
121100030721     C                             PiVal>*zeros
121200030721     C                   Z-ADD     PiVal         VABLNP
121300030721     C                   ELSE
121400040715     C                   SETON                                        32
121500030721     C                   EVAL      VABLNP = *zeros
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700040510     C                             + ' ' + 'VABLNP'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** NRS
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on AND
122500030721     C                             PiVal<=99
122600030721     C                   Z-ADD     PiVal         VABNRS
122700030721     C                   ELSE
122800030721     C                   SETON                                        32
122900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123000030721     C                             + ' ' + 'VABNRS'
123100030721     C                   ENDIF
123200030721     C                   ENDIF
123300030721     C*** LNA
123400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123500030721     C                   EVAL      PiStr=SkSplitCSV(i)
123600030721     C                   EXSR      CHKNUM
123700030721     C                   IF        PiInt=*on    AND
123800030721     C                             PiVal<=999   AND
123900030721     C                             PiVal>*zeros
124000030721     C                   Z-ADD     PiVal         VABLNA
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      VABLNA = *zeros
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABLNA'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** CTR
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on  AND
125300030721     C                             PiVal<=999
125400030721     C                   Z-ADD     PiVal         VABCTR
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABCTR'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** IAS
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiNum=*on
126600030721     C                   Z-ADD     PiVal         VABIAS
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABIAS'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** VLB
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiNum=*on
127800030721     C                   Z-ADD     PiVal         VABVLB
127900030721     C                   ELSE
128000030721     C                   SETON                                        32
128100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128200030721     C                             + ' ' + 'VABVLB'
128300030721     C                   ENDIF
128400030721     C                   ENDIF
128500030721     C*** QFT
128600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128700030721     C                   EVAL      PiStr=SkSplitCSV(i)
128800030721     C                   EXSR      CHKNUM
128900030721     C                   IF        PiNum=*on
129000030721     C                   Z-ADD     PiVal         VABQFT
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABQFT'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** NCD
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999
130300030721     C                   Z-ADD     PiVal         VABNCD
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABNCD'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** NCA
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on      AND
131500030721     C                             PiVal<=9999999
131600030721     C                   Z-ADD     PiVal         VABNCA
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABNCA'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** ZNC
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132500030721     C                   EVAL      PiStr=SkSplitCSV(i)
132600030721     C                   EXSR      CHKNUM
132700030721     C                   IF        PiInt=*on AND
132800030721     C                             PiVal<=99
132900030721     C                   Z-ADD     PiVal         VABZNC
133000030721     C                   ELSE
133100030721     C                   SETON                                        32
133200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133300030721     C                             + ' ' + 'VABZNC'
133400030721     C                   ENDIF
133500030721     C                   ENDIF
133600030721     C*** DCR
133700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133800030721     C                   EVAL      PiStr=SkSplitCSV(i)
133900030721     C                   EXSR      CHKNUM
134000030721     C                   IF        PiInt=*on       AND
134100030721     C                             PiVal<=99999999
134200030721     C                   Z-ADD     PiVal         VABDCR
134300030721     C                   ELSE
134400030721     C                   SETON                                        32
134500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134600030721     C                             + ' ' + 'VABDCR'
134700030721     C                   ENDIF
134800030721     C                   ENDIF
134900030721     C*** HCR
135000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135100030721     C                   EVAL      PiStr=SkSplitCSV(i)
135200030721     C                   EXSR      CHKNUM
135300030721     C                   IF        PiInt=*on   AND
135400030721     C                             PiVal<=9999
135500030721     C                   Z-ADD     PiVal         VABHCR
135600030721     C                   ELSE
135700030721     C                   SETON                                        32
135800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135900030721     C                             + ' ' + 'VABHCR'
136000030721     C                   ENDIF
136100030721     C                   ENDIF
136200030721     C*** VMD
136300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136400030721     C                   EVAL      PiStr=SkSplitCSV(i)
136500030721     C                   EXSR      CHKNUM
136600030721     C                   IF        PiNum=*on
136700030721     C                   Z-ADD     PiVal         VABVMD
136800030721     C                   ELSE
136900030721     C                   SETON                                        32
137000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137100030721     C                             + ' ' + 'VABVMD'
137200030721     C                   ENDIF
137300030721     C                   ENDIF
137400030721     C*** ANT
137500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137600030721     C                   EVAL      PiStr=SkSplitCSV(i)
137700030721     C                   EXSR      CHKNUM
137800030721     C                   IF        PiInt=*on        AND
137900030721     C                             PiVal<=999999999
138000030721     C                   Z-ADD     PiVal         VABANT
138100030721     C                   ELSE
138200030721     C                   SETON                                        32
138300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138400030721     C                             + ' ' + 'VABANT'
138500030721     C                   ENDIF
138600030721     C                   ENDIF
138700030721     C*** CMO
138800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138900030721     C                   EVAL      PiStr=SkSplitCSV(i)
139000030721     C                   EXSR      CHKNUM
139100030721     C                   IF        PiInt=*on
139200030721     C                   Z-ADD     PiVal         Num5_0
139300030721     C                   MOVEL(p)  Num5_0        VABCMO
139400030721     C                   ELSE
139500030721     C                   SETON                                        32
139600030721     C                   EVAL      VABCMO = *zeros
139700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139800030721     C                             + ' ' + 'VABCMO'
139900030721     C                   ENDIF
140000030721     C                   ENDIF
140100040510     C*
140200040510     C* Reperisco i campi del FIVAT
140300040510     C*** VATNOT
140400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140500040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140600040510     C                   ENDIF
140700040510     C*** VATTRC
140800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140900040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
141000040510     C                   ENDIF
141100060331     C*** VATNOT_A
141200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141300060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141400060331     C                   ENDIF
141500060331     C*** VATNOT_B
141600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141700060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141800060331     C                   ENDIF
141900060331     C*** VATNOT_E
142000070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142100060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142200060331     C                   ENDIF
142300110927     C*** VATNOT_I
142400110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142500110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142600110927     C                   ENDIF
142700070823     C*
142800100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142900120301
143000120301      *** W_TEL + W_CEL = VABNT2
143100120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_TEL'
143200120301     C                   EVAL      VABNT2 = %trim(SkSplitCSV(i))
143300120301     C                   ENDIF
143400120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
143500120301     C                   EVAL      VABNT2 = %trim(VABNT2) + ' ' +
143600120301     C                                      %trim(SkSplitCSV(i))
143700120301     C                   ENDIF
143800120301      *** W_CAS se = 'CONTRASSEGNO' ok x il c/assegno
143900120301     C                   IF        %trim(SkSplitFLD(i)) = 'W_CAS'
144000120301     C                             and %trim(SkSplitCSV(i)) <> 'Contrassegno'
144100120301     C                   clear                   VABCAS
144200120301     C                   clear                   VABVCA
144300130227     C***                clear                   VABTIC
144400120301     C                   EVAL      wFlgCAS = '0'
144500120301     C                   ENDIF
144600120301      *** VATNOT_E da VABRMN
144700120301     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
144800120316     C                   eval      VATTRC = 'E'
144900120309      *** CAD
145000120309     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
145100120309     C                   eval      VABCAD = %subst(SkSplitCSV(i):2)
145200120309     C                   ENDIF
145300140520
145400140520      *** W_CEL = VATNOT TRC=S
145500140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_CEL'
145600140520     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
145700140520     C                   ENDIF
145800140520
145900140520      *** W_VATNOTIJ
146000140520     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
146100140520     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
146200140520     C                   ENDIF
146300070925     C*
146400071029     C***  <===  -----------------------------------------
146500040510     C*
146600030715     C                   ENDIF
146700030715     C                   EVAL      i = i + 1
146800030715     C                   ENDDO
146900070925     C*
147000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
147100100831     C*
147200120316     C                   IF        wVATNOT_E <> *blanks
147300120316     C                   EVAL      VABCTM = '7Q'
147400120316     C                   ENDIF
147500120316     C*
147600120316     C                   EVAL      wVATNOT_B =  VABNT2
147700070925     C*
147800071029     C***  <===  -----------------------------------------
147900070925     C*
148000070925     C* Considerazioni finali su CBO/CAS
148100100318     C                   IF        wFlgCAS = '1'    OR
148200100318     C                             VABCAS  > *zeros
148300070925     C                   IF        VABCBO = '1'
148400070925     C                   EVAL      VABCBO = '4'
148500070925     C                   ENDIF
148600070925     C                   IF        VABCBO = '2'
148700070925     C                   EVAL      VABCBO = '6'
148800070925     C                   ENDIF
148900070925     C                   ENDIF
149000070925     C***  <===  ----------------------------
149100040510     C*
149200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
149300040510     C                   IF        VABNSP = *zeros
149400040510     C                   SETON                                        31
149500040510     C                   ENDIF
149600040715     C*
149700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
149800040715     C                   IF        VABLNP = *zeros
149900040715     C                   SETON                                        31
150000040715     C                   ENDIF
150100040510     C*
150200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
150300040510     C                   IF        VABRMN = *zeros
150400040510     C                   Z-ADD     VABNSP        VABRMN
150500040510     C                   ENDIF
150600040510     C*
150700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
150800040510     C                   IF        VABRMA = *blanks
150900040714     C                   MOVEL     VABRMN        VABRMA
151000040510     C                   ENDIF
151100040128     C*
151200040510     C* Se provincia nn valorizzata la reperisco
151300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
151400110912     C                   IF        VABPRD =  *blanks AND
151500110912     C                             VABNZD =  *blanks AND
151600110912     C                             VABCAD <> *blanks AND
151700110912     C                             VABLOD <> *blanks
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
181600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
181700120309VABRMN;VABRMA;VABRD2;VABRSD;VABIND;VABLOD;W_VABCAD;VABPRD;W_TEL;W_CEL;
181800140707W_VATNOTIJ;X;VABCAS;W_CAS;VABNOT;VABTIC;VABNCL;VABPKB;
181900120227
182000111116
182100100318
