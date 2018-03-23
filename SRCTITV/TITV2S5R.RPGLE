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
002300140520     D dvats         e ds
002400010330     D esito           s              1
002500010330     D prmlit          s             10
002600010330     D prmfir          s             10
002700010330     D wrkesito        s                   like(esito)
002800010330     D rrnum           s              6  0 INZ(*zeros)
002900040510     D parccm          s              8    INZ(*blanks)
003000040119     D parmbr          s             10    INZ(*blanks)
003100040119     D paropz          s              1    INZ(*blanks)
003200040119     D chkcall         s              1    INZ(*blanks)
003300080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003400080418     D wFlgCAS         s              1    INZ(*blanks)
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
005700140520     D wVATNOT_A       s                   LIKE(VATNOT) INZ
005800140520     D wVATNOT_B       s                   LIKE(VATNOT) INZ
005900140520     D wVATNOT_E       s                   LIKE(VATNOT) INZ
006000140520     D wVATNOT_I       s                   LIKE(VATNOT) INZ
006100140520     D wVATNOT_J       s                   LIKE(VATNOT) INZ
006200140520     D wVATNOT_S       s                   LIKE(VATNOT) INZ
006300030822     D*------------------
006400030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006500030822     D*------------------
006600030822     D posDaDft        S              3  0 INZ(*zeros)
006700030822     D posADft         S              3  0 INZ(*zeros)
006800030822     D j               s              3  0 INZ(1)
006900030822     D wGiroDft        s              1  0 INZ(*zeros)
007000080418     D*------------------
007100080418     D* Costanti
007200080418     D*------------------
007300080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007400080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007500140526
007600140520
007700100318
007800100318     D*------------------
007900100318     D* LINKING A DEFINIZIONI ESTERNE
008000100318     D*------------------
008100100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008300100318
008400010330
008500010330
008600010330
008700000913     C                   reset                   rrnum
008800990921     C                   reset                   esito
008900990921     C                   reset                   wrkesito
009000010601     C*
009100010601     C                   exsr      opeini
009200010605     C                   exsr      rwvab
009300010601     C*
009400010601     C                   seton                                        lr
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010000010601     C*--------------------------------------------------------
010100010601     C     PREELA        BEGSR
010200010601     C*
010300010601     C* SE OCCORRE SPEDIRE IN FILIALE
010400010601     C                   if        invfil <> *zeros and
010500010601     C                             flgGiro = '0'
010600010601     C*
010700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010800010601     C                   eval      flgGiro = '1'
010900010601     C*
011000010601     C                   endif
011100010601     C*
011200010601     C                   ENDSR
011300010601     C***
011400010601
011500010601
011600010601
011700010601
011800010601     C*--------------------------------------------------------
011900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012000010601     C*--------------------------------------------------------
012100010601     C     ENDELA        BEGSR
012200040119     C*
012300040119     C* Effettuo la chiamata al CLLE preposto
012400040119     C                   call(e)   'TITVVTC'
012500040510     C                   parm                    parccm
012600040119     C                   parm                    parmbr
012700040119     C                   parm      '2'           paropz
012800050201     C*
012900050201     C* Effettuo lancio TISI95 solo x chiusura
013000050201     C                   CLEAR                   TISI95DS
013100050201     C                   EVAL      I95TLA = 'C'
013200050201     C                   CALL      'TISI95R'
013300050201     C                   PARM                    TISI95DS
013400000616     C*
013500010601     C                   ENDSR
013600010601     C***
013700000613
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
016800140526     C***                exsr      inzvar
016900140526     C***                exsr      impvab
017000110131     C*
017100110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017200110912 xxx C***                read      tivin00r                               70
017300010330     C*
017400030822     C                   do        *HIVAL
017500010330     C*
017600030822     C                   read      tivin00r                               70
017700010618     C*
017800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017900010618     C                   if        vinflg = '1'
018000010618     C                   eval      flgOk = '1'
018100010618     C                   endif
018200040510     C*
018300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018400040510     C                   z-add     *zeros        wLenVINDTA        4 0
018500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018800040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018900040510     C                   z-add     1             wFlgVINDTA
019000040510     C                   leave
019100040510     C                   endif
019200040510     C                   enddo
019300010618     C*
019400040510     C                   if        vindta > *blanks AND
019500040510     C                             wFlgVINDTA = 1
019600010330     C                   add       1             rrnum
019700010330     C*
019800010601     C                   if        *in70 = *off and
019900010330     C                             (vinflg = *blanks
020000010330     C                              or vinflg = '0'
020100010330     C                              or vinflg = '2')
020200010330     C*
020300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020400010711     C                   if        vinflg = *blanks or vinflg = '0'
020500010711     C                   clear                   vinmsg
020600010711     C                   endif
020700010601     C*
020800070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020900070925     C                   eval      vindta  = ' ' + %trim(vindta)
021000070925     C*
021100080527     C                   exsr      inzvar
021200100318     C                   exsr      normCSV
021300010605     C                   exsr      impvab
021400010601     C*
021500010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021600010605     C                   eval      depfil = VABLNP
021700010601     C                   exsr      repfil
021800010601     C                   if        depfil = invfil
021900021025     C                   if        vlrpoi = 999
022000030822     C                   move(P)   invfil        VABFGS
022100021025     C                   else
022200030822     C                   move(P)   vlrpoi        VABFGS
022300021025     C                   endif
022400010601     C*
022500010601     C                   exsr      PREELA
022600010601     C*
022700010604     C* Ebbene...
022800010604     C*
022900040119     C  N31              add       1             §CTROKVB
023000040119     C   32              add       1             §CTRMO
023100040119     C   31              add       1             §CTRNO
023200080418     C*
023300030822     C                   if        wGiro = 2
023400080513     C* Scrivo sempre il VAT
023500080513     C  N31              exsr      wrivat
023600110201     C                   exsr      chkvab
023700080513     C                   endif
023800010604     C*
023900010604     C                   if        *in31 = *off and
024000010604     C                             *in32 = *off
024100010604     C                   eval      vinflg = '1'
024200010604     C                   else
024300010604     C                   eval      vinflg = '2'
024400010604     C                   endif
024500010604     C                   endif
024600010601     C*
024700010604     C                   endif
024800010604     C*
024900010330     C                   else
025000010330     C                   eval      vinflg = '1'
025100010330     C                   endif
025200010601     C*
025300010601     C  N70              update    tivin000
025400010330     C*
025500030822     C  N70              enddo
025600040119     C*
025700080527     C* Verifico dati rimasti "in cannna"...
025800080527     C                   seton                                        65
025900110201     C                   exsr      chkvab
026000080527     C*
026100040119     C                   endif                                                  (endif - chkcall)
026200010601     C*
026300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026400010601     C                   if        cntNonEl = *zeros or
026500010601     C                             flgMulti = '0'
026600010330     C* Se non ci sono record con errori ...
026700010601     C                   if        §ctrno = 0 and
026800010604     C                             §ctrmo = 0 and
026900010601     C                             flgStato <> '2'
027000010330     C* ... restituisco esito OK.
027100010330     C                   eval      wrkesito = '0'
027200010330     C                   else
027300040119     C                   if        §ctrokvb > 0
027400010330     C                   eval      wrkesito = '1'
027500010330     C                   else
027600010615     C                   if        flgOk = '0'
027700010615     C                   eval      wrkesito = '2'
027800010615     C                   else
027900010615     C                   eval      wrkesito = '6'
028000010615     C                   endif
028100010330     C                   endif
028200010330     C                   endif
028300010601     C                   else
028400010601     C                   eval      wrkesito = '9'
028500010601     C                   endif
028600010330     C*
028700010330     C                   if        %open(tivin00r)
028800010330     C                   close     tivin00r
028900010330     C                   endif
029000021025     C                   if        %open(fivabwwr)
029100021025     C                   close     fivabwwr
029200010330     C                   endif
029300040512     C                   if        %open(fivatwwr)
029400040512     C                   close     fivatwwr
029500040512     C                   endif
029600010601     C*
029700010601     C                   if        vlrpoi <> 999
029800010601     C                   eval      invfil = vlrpoi
029900010601     C                   endif
030000010330     C*
030100040119     C                   if        §ctrokvb > 0
030200010601     C                             and invfil > *zeros
030300010330     C                   exsr      invio
030400010330     C                   endif
030500010601     C*
030600010618     C                   if        flgGiro = '1'
030700010601     C                   exsr      endela
030800010618     C                   endif
030900010330     C*
031000010330     C                   ENDSR
031100010330     C***
031200080527
031300080527
031400080527
031500080527     C*----------------------------------------------------*
031600080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031700080527     C*----------------------------------------------------*
031800100413     C     CHKVAB        BEGSR
031900110201     C*
032000110201     C* Se richiesti ignorare colli / peso / volume
032100110315 xxx C***                if        *in35
032200110315 xxx C***                z-add     *zeros        VABNCL
032300110315 xxx C***                z-add     *zeros        VABPKB
032400110315 xxx C***                z-add     *zeros        VABVLB
032500110315 xxx C***                endif
032600080527     C*
032700080527     C* Scrivo il VAB solo a rottura d spedizione
032800080527     C                   if        depspe = *zeros
032900080527     C  N31              eval      fivabds_sav = fivabds
033000080527     C                   eval      depspe = vabNSP
033100080527     C                   else
033200080527     C                   if        depspe = vabNSP AND *in65 = *off
033300100413     C   51              add       VABNCL        sav_VABNCL
033400100413     C   51              add       VABPKB        sav_VABPKB
033500100413     C   51              add       VABVLB        sav_VABVLB
033600100720     C   52              z-add     VABNCL        sav_VABNCL
033700100720     C   52              z-add     VABPKB        sav_VABPKB
033800100720     C   52              z-add     VABVLB        sav_VABVLB
033900100720     C   53              add       VABNCL        sav_VABNCL
034000100720     C   53              z-add     VABPKB        sav_VABPKB
034100100720     C   53              z-add     VABVLB        sav_VABVLB
034200080527     C                   else
034300080527     C  N31              eval      fivabds_ok = fivabds_sav
034400080527     C  N31              exsr      wrivab
034500080527     C  N31              eval      fivabds_sav = fivabds
034600080527     C                   eval      depspe = vabNSP
034700080527     C                   endif
034800080527     C                   endif
034900080527     C*
035000080527     C                   ENDSR
035100040119
035200040119
035300040119
035400040119     C*----------------------------------------------------*
035500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035600040119     C*----------------------------------------------------*
035700040510     C     WRIVAB        BEGSR
035800080513     C*
035900080513     C* Gestisco l'eventuale rottura x numero spedizione
036000080527     C                   if        VABNSP <> depspe or *in65 = *on
036100070301     C                   if        wVATNOT_E <> *blanks
036200070301     C                   eval      VABCTM = '7Q'
036300070301     C                   endif
036400100413     C                   write     fivab000
036500080513     C                   endif
036600040119     C*
036700040119     C                   ENDSR
036800040119
036900040119
037000040119
037100040119     C*----------------------------------------------------*
037200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037300040119     C*----------------------------------------------------*
037400040510     C     WRIVAT        BEGSR
037500060512     C*
037600060512     C* Inizializzo i campi chiave
037700060512     C                   eval      VATFGS = VABFGS
037800060512     C                   eval      VATAAS = VABAAS
037900060512     C                   eval      VATCCM = VABCCM
038000070823     C                   eval      VATNRS = VABNRS
038100060512     C                   eval      VATNSP = VABNSP
038200060512     C                   eval      VATLNP = VABLNP
038300040119     C*
038400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038500040510     C                   if        VATTRC = *blanks
038600040510     C                   eval      VATTRC = 'E'
038700040510     C                   endif
038800110318     C                   if        VATNOT <> *blanks
038900040510     C                   write     FIVAT000
039000040119     C                   add       1             §CTROKVT
039100110318     C                   endif
039200060331     C*
039300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039400060331     C                   if        wVATNOT_A <> *blanks
039500060331     C                   eval      VATNOT = wVATNOT_A
039600060331     C                   eval      VATTRC = 'A'
039700060331     C                   write     FIVAT000
039800060331     C                   add       1             §CTROKVT
039900060331     C                   endif
040000060331     C*
040100060331     C                   if        wVATNOT_B <> *blanks
040200060331     C                   eval      VATNOT = wVATNOT_B
040300060331     C                   eval      VATTRC = 'B'
040400060331     C                   write     FIVAT000
040500060331     C                   add       1             §CTROKVT
040600060331     C                   endif
040700140526     C*
040800140526     C                   if        wVATNOT_E <> *blanks
040900140526     C                   eval      VATNOT = wVATNOT_E
041000140526     C                   eval      VATTRC = 'E'
041100140526     C                   write     FIVAT000
041200140526     C                   add       1             §CTROKVT
041300140526     C                   endif
041400110927     C*
041500110927     C                   if        wVATNOT_I <> *blanks
041600110927     C                   eval      VATNOT = wVATNOT_I
041700110927     C                   eval      VATTRC = 'I'
041800110927     C                   write     FIVAT000
041900110927     C                   add       1             §CTROKVT
042000110927     C                   endif
042100140520     C*
042200140520     C                   if        wVATNOT_J <> *blanks
042300140520     C                   eval      VATNOT = wVATNOT_J
042400140520     C                   eval      VATTRC = 'J'
042500140520     C                   write     FIVAT000
042600140520     C                   add       1             §CTROKVT
042700140520     C                   endif
042800140520     C*
042900140520     C                   if        wVATNOT_S <> *blanks
043000140520     C                   eval      VATNOT = wVATNOT_S
043100140520     C                   eval      VATTRC = 'S'
043200140520     C                   write     FIVAT000
043300140520     C                   add       1             §CTROKVT
043400140520     C                   endif
043500040119     C*
043600040119     C                   ENDSR
043700040119
043800040119
043900040119
044000040119     C*----------------------------------------------------*
044100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044200040119     C*----------------------------------------------------*
044300040119     C     PREVAT        BEGSR
044400040119     C*
044500040119     C* Compongo il nome del membro da dare al FIVATWWR
044600040119     C                   eval      parmbr = vlrhdl
044700040119     C                   movel     'M'           parmbr
044800040510     C                   eval      parccm = vlrksc
044900040119     C                   eval      paropz = '1'
045000040119     C* Effettuo la chiamata al CLLE preposto
045100040119     C                   call(e)   'TITVVTC'
045200040119     C                   parm                    parccm
045300040119     C                   parm                    parmbr
045400040119     C                   parm                    paropz
045500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045600040119     C                   if        %error
045700040119     C                   movel     '1'           chkcall
045800040119     C                   else
045900040119     C                   movel     '0'           chkcall
046000040119     C                   endif
046100040119     C*
046200040119     C                   ENDSR
046300010601
046400010601
046500010601
046600010330     C*----------------------------------------------------*
046700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046800010330     C*----------------------------------------------------*
046900040119     C     INZVAR        BEGSR
047000080527     C*
047100080527     C                   setoff                                       65
047200010330     C*
047300030822     C* Inizializzo variabili di wrk
047400020204     C                   Z-ADD     *zeros        Num5_0            5 0
047500101018     C                   MOVEL     *blanks       wFlgCAS
047600060331     C                   MOVEL     *blanks       wVATNOT_A
047700060331     C                   MOVEL     *blanks       wVATNOT_B
047800060331     C                   MOVEL     *blanks       wVATNOT_E
047900110927     C                   MOVEL     *blanks       wVATNOT_I
048000140520     C                   MOVEL     *blanks       wVATNOT_J
048100140520     C                   MOVEL     *blanks       wVATNOT_S
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
087800110912     C                   ENDDO
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
093400110912 xxx C***                EXSR      repNSP
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
140000110927     C*** VATNOT_I
140100110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
140200110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
140300110927     C                   ENDIF
140400070823     C*
140500100831     C***  ===>  Gestione campi "particolari" dentro ciclo
140600110421     C*
140700140526     C*** RMN / NSP
140800140526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
140900140526     C                   EVAL      PiStr=%subst(SkSplitCSV(i):8:6)
141000140526     C                   EXSR      CHKNUM
141100140526     C                   IF        PiInt=*on
141200140526     C                   Z-ADD     PiVal         VABNSP
141300140526     C                   Z-ADD     PiVal         VABRMN
141400140526     C                   ELSE
141500140526     C                   SETON                                        31
141600140526     C                   EVAL      VABNSP = *zeros
141700140526     C                   EVAL      VABRMN = *zeros
141800140526     C                   EVAL      vinmsg = %trimr(vinmsg)
141900140526     C                             + ' ' + 'VABNSP/VABRMN'
142000140526     C                   ENDIF
142100140526     C                   ENDIF
142200140520     C*
142300070925     C*
142400071029     C***  <===  -----------------------------------------
142500040510     C*
142600030715     C                   ENDIF
142700030715     C                   EVAL      i = i + 1
142800030715     C                   ENDDO
142900070925     C*
143000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143100100831     C*
143200070925     C*
143300071029     C***  <===  -----------------------------------------
143400070925     C*
143500070925     C* Considerazioni finali su CBO/CAS
143600100318     C                   IF        wFlgCAS = '1'    OR
143700100318     C                             VABCAS  > *zeros
143800070925     C                   IF        VABCBO = '1'
143900070925     C                   EVAL      VABCBO = '4'
144000070925     C                   ENDIF
144100070925     C                   IF        VABCBO = '2'
144200070925     C                   EVAL      VABCBO = '6'
144300070925     C                   ENDIF
144400070925     C                   ENDIF
144500070925     C***  <===  ----------------------------
144600040510     C*
144700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144800040510     C                   IF        VABNSP = *zeros
144900040510     C                   SETON                                        31
145000040510     C                   ENDIF
145100040715     C*
145200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145300040715     C                   IF        VABLNP = *zeros
145400040715     C                   SETON                                        31
145500040715     C                   ENDIF
145600040510     C*
145700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145800040510     C                   IF        VABRMN = *zeros
145900040510     C                   Z-ADD     VABNSP        VABRMN
146000040510     C                   ENDIF
146100040510     C*
146200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146300040510     C                   IF        VABRMA = *blanks
146400040714     C                   MOVEL     VABRMN        VABRMA
146500040510     C                   ENDIF
146600040128     C*
146700040510     C* Se provincia nn valorizzata la reperisco
146800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146900110912     C                   IF        VABPRD =  *blanks AND
147000110912     C                             VABNZD =  *blanks AND
147100110912     C                             VABCAD <> *blanks AND
147200110912     C                             VABLOD <> *blanks
147300040128     C                   CLEAR                   TISI95DS
147400040128     C                   EVAL      I95TCN = '3'
147500040128     C                   Z-ADD     datcor        I95DAT
147600040128     C                   EVAL      I95NAR = VABNZD
147700040128     C                   EVAL      I95CAP = VABCAD
147800040128     C                   EVAL      I95LOC = VABLOD
147900040128     C                   CALL      'TISI95R'
148000040128     C                   PARM                    TISI95DS
148100040128     C                   EVAL      VABPRD = O95PRV
148200040510     C                   ENDIF
148300020204     C*
148400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148500020204     C                   EXSR      CHKIMPDIV
148600030715     C                   ENDIF
148700020204     C*
148800010607     C                   ENDSR
148900010607     C*----------------------------------------------------*
149000010601
149100020204
149200020204     C*----------------------------------------------------*
149300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149400020204     C*----------------------------------------------------*
149500020204     C     CHKIMPDIV     BEGSR
149600020204     C*
149700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149800020204     C                   Z-ADD     *zeros        wrkDec            9 9
149900020204     C*
150000020204     C* Come prima cosa effettuo considerazioni sulla divisa
150100020204     C                   IF        vabIAS > *zeros
150200020204     C                   IF        vabVAS <> 'EUR'
150300020204     C                   EVAL      vabVAS =  'ITL'
150400020204     C                   ENDIF
150500020204     C                   ENDIF
150600020204     C*
150700020204     C                   IF        vabCAS > *zeros
150800020204     C                   IF        vabVCA <> 'EUR'
150900020204     C                   EVAL      vabVCA =  'ITL'
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C                   IF        vabVMD > *zeros
151400020204     C                   IF        vabVAD <> 'EUR'
151500020204     C                   EVAL      vabVAD =  'ITL'
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152000020204     C                   Z-ADD     vabIAS        wrkDec
152100020204     C                   IF        wrkDec > *zeros
152200020204     C                   IF        vabVAS = 'ITL'
152300020204     C                   EVAL      vabIAS = *zeros
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152800020204     C                   Z-ADD     vabCAS        wrkDec
152900020204     C                   IF        wrkDec > *zeros
153000020204     C                   IF        vabVCA = 'ITL'
153100020204     C                   EVAL      vabCAS = *zeros
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153600020204     C                   Z-ADD     vabVMD        wrkDec
153700020204     C                   IF        wrkDec > *zeros
153800020204     C                   IF        vabVAD = 'ITL'
153900020204     C                   EVAL      vabVMD = *zeros
154000020204     C                   ENDIF
154100020204     C                   ENDIF
154200020204     C*
154300020204     C                   ENDSR
154400020204     C***
154500010330
154600100318
154700100318
154800100318     C*----------------------------------------------------*
154900100318     C*  CONTROLLO NUMERICITA' CAMPI
155000100318     C*----------------------------------------------------*
155100100318     C     CHKNUM        BEGSR
155200100318     C*
155300100318     C                   IF        PiDecChr = *blanks
155400100318     C                   EVAL      PiDecChr = CharNUM
155500100318     C                   ENDIF
155600100318     C*
155700100318     C                   callp     UBISNUM_Check(PiStr
155800100318     C                                          :PiDecChr
155900100318     C                                          :PiVal
156000100318     C                                          :PiNum
156100100318     C                                          :PiInt)
156200100318     C*
156300100318     C                   ENDSR
156400100318     C***
156500010601
156600010601
156700010601
156800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156900010601     C     repfil        BEGSR
157000010601     C*
157100010601     C                   if        invfil = *zeros and
157200010601     C                             depfil > *zeros and
157300010629     C                             (vinflg = *blanks or
157400010629     C                              vinflg = *zeros)
157500010601     C
157600010601     C                   eval      invfil = depfil
157700010601     C                   endif
157800010601     C*
157900010601     C                   if        depfil <> invfil and
158000010601     C                             invfil > *zeros
158100010601     C                   eval      flgMulti = '1'
158200010601     C                   if        vinflg = *blanks
158300010601     C                   add       1             cntNonEl
158400010601     C                   endif
158500010601     C                   endif
158600010601     C*
158700010601     C                   if        vinflg = '2'
158800010601     C                   eval      flgStato = '2'
158900010601     C                   endif
159000010601     C*
159100010601     C                   ENDSR
159200010601     C***
159300010601
159400010601
159500010601
159600010330
159700040119      /TITLE Invio dei dati al punto operativo.
159800040119     C     invio         BEGSR
159900040119     C*
160000040119     C* 1° invio FIVAT
160100040119     C                   reset                   dscmz
160200040119     C                   move      vlrpoi        cmzdst
160300040119     C                   eval      cmzfld = 'FIVATWWR'
160400040119     C                   eval      cmzmbd = vlrhdl
160500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160600040119     C***                if        prmfir = *blanks
160700040119     C                   eval      cmzfla = 'FIVAT00F'
160800040119     C                   eval      cmzmba = 'FIVAT00F'
160900040119     C***                else
161000040119     C***                eval      cmzfla = prmfir
161100040119     C***                eval      cmzmba = prmfir
161200040119     C***                endif
161300040119     C                   eval      cmznrr = *zeros
161400040119     C                   move      §ctrokvt      cmznrr
161500040119     C                   eval      cmzlba = vlrfl1
161600040119     C                   call(e)   'TIS711C'
161700040119     C                   parm                    dscmz
161800040119     C                   parm      *blanks       esito
161900040119     C                   if        %error
162000040119     C                             or cmzerr = '1'
162100040119     C                             or esito  = '1'
162200040119     C                   eval      wrkesito = '3'
162300040119     C                   else
162400040119     C*
162500040119     C* 2° invio FIVAB
162600040119     C                   reset                   dscmz
162700040119     C                   move      vlrpoi        cmzdst
162800040119     C                   eval      cmzfld = vlrfou
162900040119     C                   eval      cmzmbd = vlrhdl
163000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163100040119     C***                if        prmfir = *blanks
163200040119     C                   eval      cmzfla = 'FIVAB00F'
163300040119     C                   eval      cmzmba = 'FIVAB00F'
163400040119     C***                else
163500040119     C***                eval      cmzfla = prmfir
163600040119     C***                eval      cmzmba = prmfir
163700040119     C***                endif
163800040119     C                   eval      cmznrr = *zeros
163900040119     C                   move      §ctrokvb      cmznrr
164000040119     C                   eval      cmzlba = vlrfl1
164100040119     C                   call(e)   'TIS711C'
164200040119     C                   parm                    dscmz
164300040119     C                   parm      *blanks       esito
164400040119     C                   if        %error
164500040119     C                             or cmzerr = '1'
164600040119     C                             or esito  = '1'
164700040119     C                   eval      wrkesito = '3'
164800040119     C                   endif
164900040119     C                   endif
165000040119     C*
165100040119     C                   ENDSR
165200040119     C***
165300010601
165400010601
165500010601
165600010601
165700010601      /TITLE Invio dei dati al punto operativo.
165800010601     C     opeini        BEGSR
165900010601     C*
166000010601     C* Inizializzo flag e contatori operativi
166100010601     C                   movel     '0'           flgGiro           1
166200010601     C                   movel     '0'           flgMulti          1
166300010601     C                   movel     '1'           flgStato          1
166400010615     C                   movel     '0'           flgOk             1
166500010601     C                   z-add     *zeros        cntNonEl         10 0
166600010601     C                   z-add     *zeros        depfil            3 0
166700010601     C                   z-add     *zeros        invfil            3 0
166800010601     C*
166900010601     C                   ENDSR
167000010601     C***
167100070326
167200070326
167300070326
167400070326
167500070326     C     *pssr         BEGSR
167600070329     C*
167700070329     C                   if        %open(tivin00r)
167800070329     C                   close     tivin00r
167900070329     C                   endif
168000070329     C                   if        %open(fivabwwr)
168100070329     C                   close     fivabwwr
168200070329     C                   endif
168300070329     C                   if        %open(fivatwwr)
168400070329     C                   close     fivatwwr
168500070329     C                   endif
168600070326     C*
168700070326     C* Effettuo la chiamata al CLLE preposto
168800070326     C                   call(e)   'TITVVTC'
168900070326     C                   parm                    parccm
169000070326     C                   parm                    parmbr
169100070326     C                   parm      '2'           paropz
169200070326     C*
169300070326     C                   eval      wrkesito = '2'
169400070404     C*
169500070404     C                   seton                                        LR
169600070326     C*
169700070326     C                   ENDSR     '*CANCL'
169800070326     C***
169900100318
170000100318
170100100318
170200100318     C     normCSV       BEGSR
170300100318     C*
170400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170500100318     C* separatori campi
170600100318     C                   if        CharTXT <> *blanks
170700100318     C                   z-add     *zeros        wLenVINDTA        4 0
170800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
170900100318     C                   setoff                                       80
171000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
171100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
171200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
171300100318     C                   if        not *in80
171400100318     C                   seton                                        80
171500100318     C                   else
171600100318     C                   setoff                                       80
171700100318     C                   endif
171800100318     C                   endif
171900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
172000100318     C                             and *in80 = *on
172100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
172200100318     C                   endif
172300100318     C                   enddo
172400100318     C                   endif
172500100318     C*
172600100318     C                   ENDSR
172700100318     C***
172800070326
172900100326
173000100326
173100100326     C*----------------------------------------------------*
173200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
173300100326     C*----------------------------------------------------*
173400100326     C     REPNSP        BEGSR
173500100326     C*
173600100326     C* NSP => Stacco un numeratore da AZNUM
173700100326     C                   clear                   TRUL33DS
173800100326     C                   eval      I33OPE = *zeros
173900100326     C                   eval      I33CNU = 302
174000100326     C                   eval      I33NUM = 1
174100100326     C                   movel     TRUL33DS      KPJBU
174200100326     C                   call      'TRUL33R'
174300100326     C                   parm                    KPJBA
174400100326     C                   movel     KPJBU         TRUL33DS
174500100326     C                   if        O33ERR = *zeros
174600100326     C                   z-add     O33NRF        VABNSP
174700100326     C                   z-add     O33NRF        VATNSP
174800100326     C                   else
174900100326     C                   SETON                                        31
175000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
175100100326     C                             + ' ' + 'VABNSP VATNSP'
175200100326     C                   endif
175300100326     C*
175400100326     C                   ENDSR
175500010330
175600100326
175700010330
175800000613     C     *inzsr        BEGSR
175900990910     C*
176000990910     C     *entry        plist
176100990920     C                   parm                    tivlrds
176200990921     C                   parm      wrkesito      esito
176300000724     C                   parm                    prmlit
176400000710     C                   parm                    prmfir
176500010330     C*
176600010330     C* CALCOLA LA DATA CORRENTE
176700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
176800100318     C                   eval      datcor = %dec(%date() : *ISO)
176900000613     C*
177000000613     C                   ENDSR
177100000613     C***
177200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177300140526VABRMA;X;X;X;X;X;VABNAS;X;X;W_VABNSP;X;VABNCL;VABPKB;
177400140526VABRSD;VABIND;VABLOD;VABCAD;VABPRD;X;X;X;X
177500140526
177600140526
177700100318
