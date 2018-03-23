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
003500150202     D savVINDTA       s                   LIKE(VINDTA) INZ
003600100326     D*------------------
003700100326     D* DS REPERIMENTO NUMERATORE
003800100326     D*------------------
003900100326     D trul33ds      e ds                  inz
004000100326     D*------------------
004100100326     D* DS ARCHITETTURA
004200100326     D*------------------
004300100326     D kpjba         e ds                  inz
004400100326
004500030715     D*------------------
004600030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004700030715     D*------------------
004800030715     D SkSplitFLD      S             10    DIM(100)
004900030715     D SkSplitCSV      S            256    DIM(100)
005000030715     D CharCSV         S              1
005100030715     D CharTXT         S              1
005200030715     D CharNUM         S              1
005300100318     D CharSOS         S              1
005400030715     D posDa           S              3  0 INZ(*zeros)
005500030715     D posA            S              3  0 INZ(*zeros)
005600030715     D i               s              3  0 INZ(1)
005700030715     D wGiro           s              1  0 INZ(*zeros)
005800140520     D wVATNOT_A       s                   LIKE(VATNOT) INZ
005900140520     D wVATNOT_B       s                   LIKE(VATNOT) INZ
006000140520     D wVATNOT_E       s                   LIKE(VATNOT) INZ
006100140520     D wVATNOT_I       s                   LIKE(VATNOT) INZ
006200140520     D wVATNOT_J       s                   LIKE(VATNOT) INZ
006300140520     D wVATNOT_S       s                   LIKE(VATNOT) INZ
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
007600140526
007700140520
007800100318
007900100318     D*------------------
008000100318     D* LINKING A DEFINIZIONI ESTERNE
008100100318     D*------------------
008200100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008300100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008400100318
008500010330
008600010330
008700010330
008800000913     C                   reset                   rrnum
008900990921     C                   reset                   esito
009000990921     C                   reset                   wrkesito
009100010601     C*
009200010601     C                   exsr      opeini
009300010605     C                   exsr      rwvab
009400010601     C*
009500010601     C                   seton                                        lr
009600010601
009700010601
009800010601
009900010601     C*--------------------------------------------------------
010000010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010601     C*--------------------------------------------------------
010200010601     C     PREELA        BEGSR
010300010601     C*
010400010601     C* SE OCCORRE SPEDIRE IN FILIALE
010500010601     C                   if        invfil <> *zeros and
010600010601     C                             flgGiro = '0'
010700010601     C*
010800010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010601     C                   eval      flgGiro = '1'
011000010601     C*
011100010601     C                   endif
011200010601     C*
011300010601     C                   ENDSR
011400010601     C***
011500010601
011600010601
011700010601
011800010601
011900010601     C*--------------------------------------------------------
012000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012100010601     C*--------------------------------------------------------
012200010601     C     ENDELA        BEGSR
012300040119     C*
012400040119     C* Effettuo la chiamata al CLLE preposto
012500040119     C                   call(e)   'TITVVTC'
012600040510     C                   parm                    parccm
012700040119     C                   parm                    parmbr
012800040119     C                   parm      '2'           paropz
012900050201     C*
013000050201     C* Effettuo lancio TISI95 solo x chiusura
013100050201     C                   CLEAR                   TISI95DS
013200050201     C                   EVAL      I95TLA = 'C'
013300050201     C                   CALL      'TISI95R'
013400050201     C                   PARM                    TISI95DS
013500000616     C*
013600010601     C                   ENDSR
013700010601     C***
013800000613
013900010601
014000010601
014100010330     C*--------------------------------------------------------
014200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014300010330     C*--------------------------------------------------------
014400010605     C     RWVAB         BEGSR
014500010330     C*
014600010330     C                   if        not %open(tivin00r)
014700010330     C                   open      tivin00r
014800010330     C                   endif
014900021025     C                   if        not %open(fivabwwr)
015000021025     C                   open      fivabwwr
015100010330     C                   endif
015200040119     C*
015300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015400040119     C                   exsr      prevat
015500040119     C*
015600040119     C                   if        chkcall = '0'
015700040119     C*
015800040119     C                   if        not %open(fivatwwr)
015900040119     C                   open      fivatwwr
016000040119     C                   endif
016100010330     C*
016200040119     C                   clear                   §CTROKVB          7 0
016300040119     C                   clear                   §CTROKVT          7 0
016400040119     C                   clear                   §CTRMO            7 0
016500040119     C                   clear                   §CTRNO            7 0
016600070823     C*
016700070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016800070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016900140526     C***                exsr      inzvar
017000140526     C***                exsr      impvab
017100110131     C*
017200110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017300110912 xxx C***                read      tivin00r                               70
017400010330     C*
017500030822     C                   do        *HIVAL
017600010330     C*
017700030822     C                   read      tivin00r                               70
017800150202     C*
017900150202     C* Salvo il dato di input originale
018000150202     C                   eval      savVINDTA = vinDTA
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
025700150202     C* Ripristino il dato di input originale
025800150202     C                   eval      vinDTA = savVINDTA
025900150202     C*
026000010601     C  N70              update    tivin000
026100010330     C*
026200030822     C  N70              enddo
026300040119     C*
026400080527     C* Verifico dati rimasti "in cannna"...
026500080527     C                   seton                                        65
026600110201     C                   exsr      chkvab
026700080527     C*
026800040119     C                   endif                                                  (endif - chkcall)
026900010601     C*
027000010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027100010601     C                   if        cntNonEl = *zeros or
027200010601     C                             flgMulti = '0'
027300010330     C* Se non ci sono record con errori ...
027400010601     C                   if        §ctrno = 0 and
027500010604     C                             §ctrmo = 0 and
027600010601     C                             flgStato <> '2'
027700010330     C* ... restituisco esito OK.
027800010330     C                   eval      wrkesito = '0'
027900010330     C                   else
028000040119     C                   if        §ctrokvb > 0
028100010330     C                   eval      wrkesito = '1'
028200010330     C                   else
028300010615     C                   if        flgOk = '0'
028400010615     C                   eval      wrkesito = '2'
028500010615     C                   else
028600010615     C                   eval      wrkesito = '6'
028700010615     C                   endif
028800010330     C                   endif
028900010330     C                   endif
029000010601     C                   else
029100010601     C                   eval      wrkesito = '9'
029200010601     C                   endif
029300010330     C*
029400010330     C                   if        %open(tivin00r)
029500010330     C                   close     tivin00r
029600010330     C                   endif
029700021025     C                   if        %open(fivabwwr)
029800021025     C                   close     fivabwwr
029900010330     C                   endif
030000040512     C                   if        %open(fivatwwr)
030100040512     C                   close     fivatwwr
030200040512     C                   endif
030300010601     C*
030400010601     C                   if        vlrpoi <> 999
030500010601     C                   eval      invfil = vlrpoi
030600010601     C                   endif
030700010330     C*
030800040119     C                   if        §ctrokvb > 0
030900010601     C                             and invfil > *zeros
031000010330     C                   exsr      invio
031100010330     C                   endif
031200010601     C*
031300010618     C                   if        flgGiro = '1'
031400010601     C                   exsr      endela
031500010618     C                   endif
031600010330     C*
031700010330     C                   ENDSR
031800010330     C***
031900080527
032000080527
032100080527
032200080527     C*----------------------------------------------------*
032300080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032400080527     C*----------------------------------------------------*
032500100413     C     CHKVAB        BEGSR
032600110201     C*
032700110201     C* Se richiesti ignorare colli / peso / volume
032800110315 xxx C***                if        *in35
032900110315 xxx C***                z-add     *zeros        VABNCL
033000110315 xxx C***                z-add     *zeros        VABPKB
033100110315 xxx C***                z-add     *zeros        VABVLB
033200110315 xxx C***                endif
033300080527     C*
033400080527     C* Scrivo il VAB solo a rottura d spedizione
033500080527     C                   if        depspe = *zeros
033600080527     C  N31              eval      fivabds_sav = fivabds
033700080527     C                   eval      depspe = vabNSP
033800080527     C                   else
033900080527     C                   if        depspe = vabNSP AND *in65 = *off
034000100413     C   51              add       VABNCL        sav_VABNCL
034100100413     C   51              add       VABPKB        sav_VABPKB
034200100413     C   51              add       VABVLB        sav_VABVLB
034300100720     C   52              z-add     VABNCL        sav_VABNCL
034400100720     C   52              z-add     VABPKB        sav_VABPKB
034500100720     C   52              z-add     VABVLB        sav_VABVLB
034600100720     C   53              add       VABNCL        sav_VABNCL
034700100720     C   53              z-add     VABPKB        sav_VABPKB
034800100720     C   53              z-add     VABVLB        sav_VABVLB
034900080527     C                   else
035000080527     C  N31              eval      fivabds_ok = fivabds_sav
035100080527     C  N31              exsr      wrivab
035200080527     C  N31              eval      fivabds_sav = fivabds
035300080527     C                   eval      depspe = vabNSP
035400080527     C                   endif
035500080527     C                   endif
035600080527     C*
035700080527     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
036300040119     C*----------------------------------------------------*
036400040510     C     WRIVAB        BEGSR
036500080513     C*
036600080513     C* Gestisco l'eventuale rottura x numero spedizione
036700080527     C                   if        VABNSP <> depspe or *in65 = *on
036800070301     C                   if        wVATNOT_E <> *blanks
036900070301     C                   eval      VABCTM = '7Q'
037000070301     C                   endif
037100100413     C                   write     fivab000
037200080513     C                   endif
037300040119     C*
037400040119     C                   ENDSR
037500040119
037600040119
037700040119
037800040119     C*----------------------------------------------------*
037900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
038000040119     C*----------------------------------------------------*
038100040510     C     WRIVAT        BEGSR
038200060512     C*
038300060512     C* Inizializzo i campi chiave
038400060512     C                   eval      VATFGS = VABFGS
038500060512     C                   eval      VATAAS = VABAAS
038600060512     C                   eval      VATCCM = VABCCM
038700070823     C                   eval      VATNRS = VABNRS
038800060512     C                   eval      VATNSP = VABNSP
038900060512     C                   eval      VATLNP = VABLNP
039000040119     C*
039100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
039200040510     C                   if        VATTRC = *blanks
039300040510     C                   eval      VATTRC = 'E'
039400040510     C                   endif
039500110318     C                   if        VATNOT <> *blanks
039600040510     C                   write     FIVAT000
039700040119     C                   add       1             §CTROKVT
039800110318     C                   endif
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
041400140526     C*
041500140526     C                   if        wVATNOT_E <> *blanks
041600140526     C                   eval      VATNOT = wVATNOT_E
041700140526     C                   eval      VATTRC = 'E'
041800140526     C                   write     FIVAT000
041900140526     C                   add       1             §CTROKVT
042000140526     C                   endif
042100110927     C*
042200110927     C                   if        wVATNOT_I <> *blanks
042300110927     C                   eval      VATNOT = wVATNOT_I
042400110927     C                   eval      VATTRC = 'I'
042500110927     C                   write     FIVAT000
042600110927     C                   add       1             §CTROKVT
042700110927     C                   endif
042800140520     C*
042900140520     C                   if        wVATNOT_J <> *blanks
043000140520     C                   eval      VATNOT = wVATNOT_J
043100140520     C                   eval      VATTRC = 'J'
043200140520     C                   write     FIVAT000
043300140520     C                   add       1             §CTROKVT
043400140520     C                   endif
043500140520     C*
043600140520     C                   if        wVATNOT_S <> *blanks
043700140520     C                   eval      VATNOT = wVATNOT_S
043800140520     C                   eval      VATTRC = 'S'
043900140520     C                   write     FIVAT000
044000140520     C                   add       1             §CTROKVT
044100140520     C                   endif
044200040119     C*
044300040119     C                   ENDSR
044400040119
044500040119
044600040119
044700040119     C*----------------------------------------------------*
044800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
044900040119     C*----------------------------------------------------*
045000040119     C     PREVAT        BEGSR
045100040119     C*
045200040119     C* Compongo il nome del membro da dare al FIVATWWR
045300040119     C                   eval      parmbr = vlrhdl
045400040119     C                   movel     'M'           parmbr
045500040510     C                   eval      parccm = vlrksc
045600040119     C                   eval      paropz = '1'
045700040119     C* Effettuo la chiamata al CLLE preposto
045800040119     C                   call(e)   'TITVVTC'
045900040119     C                   parm                    parccm
046000040119     C                   parm                    parmbr
046100040119     C                   parm                    paropz
046200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046300040119     C                   if        %error
046400040119     C                   movel     '1'           chkcall
046500040119     C                   else
046600040119     C                   movel     '0'           chkcall
046700040119     C                   endif
046800040119     C*
046900040119     C                   ENDSR
047000010601
047100010601
047200010601
047300010330     C*----------------------------------------------------*
047400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
047500010330     C*----------------------------------------------------*
047600040119     C     INZVAR        BEGSR
047700080527     C*
047800080527     C                   setoff                                       65
047900010330     C*
048000030822     C* Inizializzo variabili di wrk
048100020204     C                   Z-ADD     *zeros        Num5_0            5 0
048200101018     C                   MOVEL     *blanks       wFlgCAS
048300060331     C                   MOVEL     *blanks       wVATNOT_A
048400060331     C                   MOVEL     *blanks       wVATNOT_B
048500060331     C                   MOVEL     *blanks       wVATNOT_E
048600110927     C                   MOVEL     *blanks       wVATNOT_I
048700140520     C                   MOVEL     *blanks       wVATNOT_J
048800140520     C                   MOVEL     *blanks       wVATNOT_S
048900030822     C*
049000030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049100030822     C                   CLEAR                   SkSplitCSV
049200030822     C                   EVAL      i = 1
049300030822     C                   EVAL      posDa = *zeros
049400030822     C                   EVAL      posA  = *zeros
049500080527     C*
049600080527     C                   CLEAR                   FIVABDS
049700080527     C                   CLEAR                   FIVAT000
049800030822     C*
049900030822     C* Reimposto i valori di default
050000030822     C                   EXSR      DEFCAM
050100030822     C*
050200010330     C                   ENDSR
050300010330     C*----------------------------------------------------*
050400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050500010330     C*----------------------------------------------------*
050600010330     C     DEFCAM        BEGSR
050700030822     C*
050800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
050900030715     C* e delimitatore testo.
051000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051300100318     C*
051400100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051500100318     C                   EVAL      CharSOS = CharNUM
051600030822     C*
051700030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
051800100720     C                   SETOFF                                       505152
051900100720     C                   SETOFF                                       53
052000100413     C                   SELECT
052100100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
052200100720     C                   SETON                                        50
052300100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
052400100413     C                   SETON                                        51
052500100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
052600100720     C                   SETON                                        52
052700100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
052800100720     C                   SETON                                        53
052900100413     C                   ENDSL
053000030822     C*
053100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
053200030822     C                   EVAL      posDaDft = 1
053300030822     C                   EVAL      posADft  = 0
053400030822     C                   EVAL      wGiroDft = 0
053500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053600030822     C                             posDaDft > 0
053700030822     C*
053800030822     C* Gestisco il 1° giro
053900030822     C                   IF        wGiroDft = 0
054000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
054100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
054200030822     C* Incremento il contatore dei "giri"
054300030822     C                   EVAL      wGiroDft = 1
054400030822     C                   ELSE
054500030822     C                   EVAL      posDaDft = posADft
054600030822     C                   ENDIF
054700030822     C* Eseguo lo scan x trovare la fine del campo corrente
054800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054900030822     C*
055000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
055100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
055200030822     C                   IF        posDaDft > 0 AND
055300030822     C                             posADft  > 0
055400030822     C* NCL
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'NCL'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiInt=*on
056400030822     C                   Z-ADD     PiVal         VABNCL
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* CCM
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'CCM'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiInt=*on
057700030822     C                   Z-ADD     PiVal         VABCCM
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* LNP
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'LNP'
058500030822     C                   EVAL      PiStr=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   EXSR      CHKNUM
058900030822     C                   IF        PiInt=*on
059000030822     C                   Z-ADD     PiVal         VABLNP
059100030822     C                   ENDIF
059200030822     C                   ENDIF
059300030822     C* NRS
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'NRS'
059800030822     C                   EVAL      PiStr=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   EXSR      CHKNUM
060200030822     C                   IF        PiInt=*on
060300030822     C                   Z-ADD     PiVal         VABNRS
060400030822     C                   ENDIF
060500030822     C                   ENDIF
060600030822     C* CTR
060700030822     C                   IF        %subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):1:3)
061000030822     C                             = 'CTR'
061100030822     C                   EVAL      PiStr=%trim(%subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061400030822     C                   EXSR      CHKNUM
061500030822     C                   IF        PiInt=*on
061600030822     C                   Z-ADD     PiVal         VABCTR
061700030822     C                   ENDIF
061800030822     C                   ENDIF
061900030822     C* PKB
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'PKB'
062400030822     C                   EVAL      PiStr=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   EXSR      CHKNUM
062800030822     C                   IF        PiNum=*on
062900030822     C                   Z-ADD     PiVal         VABPKB
063000030822     C                   ENDIF
063100030822     C                   ENDIF
063200030822     C* VLB
063300030822     C                   IF        %subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):1:3)
063600030822     C                             = 'VLB'
063700030822     C                   EVAL      PiStr=%trim(%subst(
063800030822     C                             %subst(vlrppt:posDaDft+1:
063900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064000030822     C                   EXSR      CHKNUM
064100030822     C                   IF        PiNum=*on
064200030822     C                   Z-ADD     PiVal         VABVLB
064300030822     C                   ENDIF
064400030822     C                   ENDIF
064500030822     C* QFT
064600030822     C                   IF        %subst(
064700030822     C                             %subst(vlrppt:posDaDft+1:
064800030822     C                             posADft-posDaDft-1):1:3)
064900030822     C                             = 'QFT'
065000030822     C                   EVAL      PiStr=%trim(%subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065300030822     C                   EXSR      CHKNUM
065400030822     C                   IF        PiNum=*on
065500030822     C                   Z-ADD     PiVal         VABQFT
065600030822     C                   ENDIF
065700030822     C                   ENDIF
065800030822     C* CBO
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'CBO'
066300030822     C                   EVAL      VABCBO=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* TSP
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'TSP'
067200030822     C                   EVAL      VABTSP=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* VAS
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'VAS'
068100030822     C                   EVAL      VABVAS=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* VCA
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'VCA'
069000030822     C                   EVAL      VABVCA=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400070925     C* TIC
069500070925     C                   IF        %subst(
069600070925     C                             %subst(vlrppt:posDaDft+1:
069700070925     C                             posADft-posDaDft-1):1:3)
069800070925     C                             = 'TIC'
069900070925     C                   EVAL      VABTIC=%trim(%subst(
070000070925     C                             %subst(vlrppt:posDaDft+1:
070100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200070925     C                   ENDIF
070300030822     C* GCA
070400030822     C                   IF        %subst(
070500030822     C                             %subst(vlrppt:posDaDft+1:
070600030822     C                             posADft-posDaDft-1):1:3)
070700030822     C                             = 'GCA'
070800030822     C                   EVAL      VABGCA=%trim(%subst(
070900030822     C                             %subst(vlrppt:posDaDft+1:
071000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071100030822     C                   ENDIF
071200030822     C* CTM
071300030822     C                   IF        %subst(
071400030822     C                             %subst(vlrppt:posDaDft+1:
071500030822     C                             posADft-posDaDft-1):1:3)
071600030822     C                             = 'CTM'
071700030822     C                   EVAL      VABCTM=%trim(%subst(
071800030822     C                             %subst(vlrppt:posDaDft+1:
071900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072000030822     C                   ENDIF
072100030822     C* FFD
072200030822     C                   IF        %subst(
072300030822     C                             %subst(vlrppt:posDaDft+1:
072400030822     C                             posADft-posDaDft-1):1:3)
072500030822     C                             = 'FFD'
072600030822     C                   EVAL      VABFFD=%trim(%subst(
072700030822     C                             %subst(vlrppt:posDaDft+1:
072800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072900030822     C                   ENDIF
073000030822     C* VAD
073100030822     C                   IF        %subst(
073200030822     C                             %subst(vlrppt:posDaDft+1:
073300030822     C                             posADft-posDaDft-1):1:3)
073400030822     C                             = 'VAD'
073500030822     C                   EVAL      VABVAD=%trim(%subst(
073600030822     C                             %subst(vlrppt:posDaDft+1:
073700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073800030822     C                   ENDIF
073900030822     C* GMA
074000030822     C                   IF        %subst(
074100030822     C                             %subst(vlrppt:posDaDft+1:
074200030822     C                             posADft-posDaDft-1):1:3)
074300030822     C                             = 'GMA'
074400030822     C                   EVAL      VABGMA=%trim(%subst(
074500030822     C                             %subst(vlrppt:posDaDft+1:
074600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074700030822     C                   ENDIF
074800030822     C* GGA
074900030822     C                   IF        %subst(
075000030822     C                             %subst(vlrppt:posDaDft+1:
075100030822     C                             posADft-posDaDft-1):1:3)
075200030822     C                             = 'GGA'
075300030822     C                   EVAL      VABGGA=%trim(%subst(
075400030822     C                             %subst(vlrppt:posDaDft+1:
075500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075600030822     C                   ENDIF
075700030822     C* GVA
075800030822     C                   IF        %subst(
075900030822     C                             %subst(vlrppt:posDaDft+1:
076000030822     C                             posADft-posDaDft-1):1:3)
076100030822     C                             = 'GVA'
076200030822     C                   EVAL      VABGVA=%trim(%subst(
076300030822     C                             %subst(vlrppt:posDaDft+1:
076400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076500030822     C                   ENDIF
076600030822     C* TC1
076700030822     C                   IF        %subst(
076800030822     C                             %subst(vlrppt:posDaDft+1:
076900030822     C                             posADft-posDaDft-1):1:3)
077000030822     C                             = 'TC1'
077100030822     C                   EVAL      VABTC1=%trim(%subst(
077200030822     C                             %subst(vlrppt:posDaDft+1:
077300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077400030822     C                   ENDIF
077500030822     C* TC2
077600030822     C                   IF        %subst(
077700030822     C                             %subst(vlrppt:posDaDft+1:
077800030822     C                             posADft-posDaDft-1):1:3)
077900030822     C                             = 'TC2'
078000030822     C                   EVAL      VABTC2=%trim(%subst(
078100030822     C                             %subst(vlrppt:posDaDft+1:
078200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078300030822     C                   ENDIF
078400040714     C* VATTRC
078500040714     C                   IF        %subst(
078600040714     C                             %subst(vlrppt:posDaDft+1:
078700040714     C                             posADft-posDaDft-1):1:3)
078800040714     C                             = 'TRC'
078900040714     C                   EVAL      VATTRC=%trim(%subst(
079000040714     C                             %subst(vlrppt:posDaDft+1:
079100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079200040714     C                   ENDIF
079300030822     C* ...
079400030822     C                   ENDIF
079500030822     C                   ENDDO
079600020204     C*
079700010330     C                   ENDSR
079800010607     C*----------------------------------------------------*
079900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
080000010607     C*----------------------------------------------------*
080100010607     C     IMPVAB        BEGSR
080200010607     C*
080300010607     C                   SETOFF                                       3132
080400110315 xxx C***                SETON                                        35
080500030515     C*
080600010607     C                   MOVEL     datcor        VABAAS
080700010607     C                   MOVE      datcor        VABMGS
080800010607     C*
080900030715     C**********
081000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
081100030715     C**********
081200030715     C                   IF        wGiro = *zeros
081300040113     C*
081400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081600080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081700080429     C                             %trim(intcol(5))<>*blanks
081800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081900080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
082000080429     C                                      %trim(intcol(5))
082100070823     C     ';':CharCSV   XLATE     vindta        vindta
082200040510     C                   ENDIF
082300031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082400031201     C     minu:maiu     XLATE     vindta        vindta
082500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082600030715     C                   DOW       posDa <= %len(%trim(vindta))
082700030715     C*
082800030715     C* Gestisco il 1° campo
082900030715     C                   IF        i = 1
083000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083100030715     C                   EVAL      posDa = 1
083200030715     C* Eseguo lo scan x trovare la fine del primo campo
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C* X i campi successivi al 1°
083800030715     C                   ELSE
083900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084000030715     C                   EVAL      posDa = posA + 1
084100030715     C* Eseguo lo scan x trovare la fine del campo corrente
084200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084300030715     C* Gestisco l'ultimo campo
084400030715     C                   IF        posA = *zeros
084500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   LEAVE
085000030715     C                   ELSE
085100030715     C* A questo punto "estraggo" il campo corrente
085200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085300030715     C                                                    (posA-posDa))
085400030715     C                   ENDIF
085500030715     C                   ENDIF
085600030715     C* Incremento il contatore d campo
085700030715     C                   EVAL      i = i +1
085800030715     C                   ENDDO
085900030715     C*
086000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
086100030715     C* delimitazione testo
086200030715     C                   EVAL      i = 1
086300030715     C                   DOW       i < %elem(SkSplitFLD)
086400030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086500030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086600030715     C                   EVAL      i = i + 1
086700030715     C                   ENDDO
086800030715     C                   EVAL      wGiro = 1
086900030715     C                   ELSE
087000030715     C                   EVAL      wGiro = 2
087100031204     C**********
087200031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
087300031204     C* d separatore campo
087400031204     C**********
087500031204     C                   EVAL      vindta = %trim(vindta)
087600031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087700031204     C                   EVAL      vindta = %subst(vindta:2)
087800031204     C                   ENDDO
087900031204     C*
088000031204     C                   Z-ADD     *zeros        lunghInput        4 0
088100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088200031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
088300031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088500110912     C                   ENDDO
088600030715     C**********
088700030715     C* Effettuo lo split del campo dati d input
088800030715     C**********
088900030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
089000030715     C                   DOW       posDa <= %len(%trim(vindta))
089100030715     C*
089200030715     C* Gestisco il 1° campo
089300030715     C                   IF        i = 1
089400030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089500030715     C                   EVAL      posDa = 1
089600030715     C* Eseguo lo scan x trovare la fine del primo campo
089700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089800030715     C* A questo punto "estraggo" il campo corrente
089900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
090000030715     C                                                    (posA-posDa))
090100030715     C* X i campi successivi al 1°
090200030715     C                   ELSE
090300030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090400030715     C                   EVAL      posDa = posA + 1
090500031204     C* Verifico che nn vi sia il campo nn valorizzato
090600031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090700031204     C* Se campo nn valorizzato skippo al prossimo
090800031204     C                   EVAL      SkSplitCSV(i) = *blanks
090900031204     C                   EVAL      posA  = posA + 1
091000031204     C                   ELSE
091100030715     C* Eseguo lo scan x trovare la fine del campo corrente
091200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091300030715     C* Gestisco l'ultimo campo
091400030715     C                   IF        posA = *zeros
091500030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091600030715     C* A questo punto "estraggo" il campo corrente
091700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091800030715     C                                                    (posA-posDa))
091900030715     C                   LEAVE
092000030715     C                   ELSE
092100030715     C* A questo punto "estraggo" il campo corrente
092200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
092300030715     C                                                    (posA-posDa))
092400030715     C                   ENDIF
092500031204     C                   ENDIF
092600030715     C                   ENDIF
092700030715     C* Incremento il contatore d campo
092800030715     C                   EVAL      i = i +1
092900030715     C                   ENDDO
093000030715     C*
093100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
093200030715     C* delimitazione testo
093300030715     C                   EVAL      i = 1
093400030715     C                   DOW       i < %elem(SkSplitCSV)
093500030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093600030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093700030715     C                   EVAL      i = i + 1
093800030715     C                   ENDDO
093900100720     C*
094000100720     C* Stacco un progressivo x numero spedizione
094100150202 xxx C                   EXSR      repNSP
094200100326     C*
094300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094400030715     C                   EVAL      i = 1
094500030715     C                   DOW       i < %elem(SkSplitFLD)
094600030715     C*
094700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094900030715     C                             SkSplitCSV(i) <> *blanks
095000030721     C*** ATB
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
095200030721     C                   EVAL      VABATB = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030715     C*** RSD
095500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095900030715     C* ==
096000030715     C                   ENDIF
096100030721     C*** RD2
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
096300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096400030721     C                   ENDIF
096500030715     C*** IND
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096700030715     C                   EVAL      VABIND = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** LOD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
097100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** PRD
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030721     C*** NZD
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030715     C*** RMA
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
098300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** NOT
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** NT2
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
099100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** VCA
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** VAS
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030715     C*** FFD
100200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
100300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100400030715     C                   ENDIF
100500030721     C*** GC1
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GC2
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
101100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TSP
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101600030721     C                   ENDIF
101700040510     C*** CBO
101800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
102000040510     C                   ENDIF
102100030721     C*** NAS
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** TIC
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** GCA
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
103100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** XCO
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** CTM
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TCR
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** CTS
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** FTM
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
105100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** VAD
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GMA
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** GGA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** GVA
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TC1
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
107100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** TC2
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** SCL
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** RMO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*** NMO
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108800030721     C                   ENDIF
108900030721     C*
109000030721     C* Reperisco quindi i campi numerici...
109100040510     C*** NSP
109200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109300040510     C                   EVAL      PiStr=SkSplitCSV(i)
109400040510     C                   EXSR      CHKNUM
109500040510     C                   IF        PiInt=*on
109600040510     C                   Z-ADD     PiVal         VABNSP
109700040510     C                   ELSE
109800040510     C                   SETON                                        31
109900040510     C                   EVAL      VABNSP = *zeros
110000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
110100040510     C                             + ' ' + 'VABNSP'
110200040510     C                   ENDIF
110300040510     C                   ENDIF
110400071106     C*** RMN
110500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110600071106     C                   EVAL      PiStr=SkSplitCSV(i)
110700071106     C                   EXSR      CHKNUM
110800071106     C                   IF        PiInt=*on
110900071106     C                   Z-ADD     PiVal         VABRMN
111000071106     C                   ELSE
111100071106     C                   SETON                                        32
111200071106     C                   EVAL      VABRMN = *zeros
111300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111400071106     C                             + ' ' + 'VABRMN'
111500071106     C                   ENDIF
111600071106     C                   ENDIF
111700030721     C*** CAD
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on
112200030721     C                   Z-ADD     PiVal         Num5_0
112300030721     C                   MOVEL(p)  Num5_0        VABCAD
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABCAD = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800030721     C                             + ' ' + 'VABCAD'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** NCL
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiInt=*on
113600030721     C                   Z-ADD     PiVal         VABNCL
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABNCL = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100040122     C                             + ' ' + 'VABNCL'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** PKB
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiNum=*on
114900030721     C                   Z-ADD     PiVal         VABPKB
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABPKB = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABPKB'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** CAS
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
115900110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
116000110506     C                             %trim(SkSplitCSV(i)) <> *zeros
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
141300110421     C*
141400140526     C*** RMN / NSP
141500140526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
141600140526     C                   EVAL      PiStr=%subst(SkSplitCSV(i):8:6)
141700140526     C                   EXSR      CHKNUM
141800140526     C                   IF        PiInt=*on
141900140526     C                   Z-ADD     PiVal         VABNSP
142000140526     C                   Z-ADD     PiVal         VABRMN
142100140526     C                   ELSE
142200140526     C                   SETON                                        31
142300140526     C                   EVAL      VABNSP = *zeros
142400140526     C                   EVAL      VABRMN = *zeros
142500140526     C                   EVAL      vinmsg = %trimr(vinmsg)
142600140526     C                             + ' ' + 'VABNSP/VABRMN'
142700140526     C                   ENDIF
142800140526     C                   ENDIF
142900140520     C*
143000070925     C*
143100071029     C***  <===  -----------------------------------------
143200040510     C*
143300030715     C                   ENDIF
143400030715     C                   EVAL      i = i + 1
143500030715     C                   ENDDO
143600070925     C*
143700071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143800100831     C*
143900070925     C*
144000071029     C***  <===  -----------------------------------------
144100070925     C*
144200070925     C* Considerazioni finali su CBO/CAS
144300100318     C                   IF        wFlgCAS = '1'    OR
144400100318     C                             VABCAS  > *zeros
144500150202     C                   IF        VABVCA = *blanks
144600150202     C                   EVAL      VABVCA = 'EUR'
144700150202     C                   ENDIF
144800070925     C                   IF        VABCBO = '1'
144900070925     C                   EVAL      VABCBO = '4'
145000070925     C                   ENDIF
145100070925     C                   IF        VABCBO = '2'
145200070925     C                   EVAL      VABCBO = '6'
145300070925     C                   ENDIF
145400070925     C                   ENDIF
145500070925     C***  <===  ----------------------------
145600040510     C*
145700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145800040510     C                   IF        VABNSP = *zeros
145900040510     C                   SETON                                        31
146000040510     C                   ENDIF
146100040715     C*
146200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
146300040715     C                   IF        VABLNP = *zeros
146400040715     C                   SETON                                        31
146500040715     C                   ENDIF
146600040510     C*
146700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146800040510     C                   IF        VABRMN = *zeros
146900040510     C                   Z-ADD     VABNSP        VABRMN
147000040510     C                   ENDIF
147100040510     C*
147200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
147300040510     C                   IF        VABRMA = *blanks
147400040714     C                   MOVEL     VABRMN        VABRMA
147500040510     C                   ENDIF
147600040128     C*
147700040510     C* Se provincia nn valorizzata la reperisco
147800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147900110912     C                   IF        VABPRD =  *blanks AND
148000110912     C                             VABNZD =  *blanks AND
148100110912     C                             VABCAD <> *blanks AND
148200110912     C                             VABLOD <> *blanks
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
151800020204     C                   IF        vabVCA <> 'EUR'
151900020204     C                   EVAL      vabVCA =  'ITL'
152000020204     C                   ENDIF
152100020204     C                   ENDIF
152200020204     C*
152300020204     C                   IF        vabVMD > *zeros
152400020204     C                   IF        vabVAD <> 'EUR'
152500020204     C                   EVAL      vabVAD =  'ITL'
152600020204     C                   ENDIF
152700020204     C                   ENDIF
152800020204     C*
152900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
153000020204     C                   Z-ADD     vabIAS        wrkDec
153100020204     C                   IF        wrkDec > *zeros
153200020204     C                   IF        vabVAS = 'ITL'
153300020204     C                   EVAL      vabIAS = *zeros
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153800020204     C                   Z-ADD     vabCAS        wrkDec
153900020204     C                   IF        wrkDec > *zeros
154000020204     C                   IF        vabVCA = 'ITL'
154100020204     C                   EVAL      vabCAS = *zeros
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154600020204     C                   Z-ADD     vabVMD        wrkDec
154700020204     C                   IF        wrkDec > *zeros
154800020204     C                   IF        vabVAD = 'ITL'
154900020204     C                   EVAL      vabVMD = *zeros
155000020204     C                   ENDIF
155100020204     C                   ENDIF
155200020204     C*
155300020204     C                   ENDSR
155400020204     C***
155500010330
155600100318
155700100318
155800100318     C*----------------------------------------------------*
155900100318     C*  CONTROLLO NUMERICITA' CAMPI
156000100318     C*----------------------------------------------------*
156100100318     C     CHKNUM        BEGSR
156200100318     C*
156300100318     C                   IF        PiDecChr = *blanks
156400100318     C                   EVAL      PiDecChr = CharNUM
156500100318     C                   ENDIF
156600100318     C*
156700100318     C                   callp     UBISNUM_Check(PiStr
156800100318     C                                          :PiDecChr
156900100318     C                                          :PiVal
157000100318     C                                          :PiNum
157100100318     C                                          :PiInt)
157200100318     C*
157300100318     C                   ENDSR
157400100318     C***
157500010601
157600010601
157700010601
157800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157900010601     C     repfil        BEGSR
158000010601     C*
158100010601     C                   if        invfil = *zeros and
158200010601     C                             depfil > *zeros and
158300010629     C                             (vinflg = *blanks or
158400010629     C                              vinflg = *zeros)
158500010601     C
158600010601     C                   eval      invfil = depfil
158700010601     C                   endif
158800010601     C*
158900010601     C                   if        depfil <> invfil and
159000010601     C                             invfil > *zeros
159100010601     C                   eval      flgMulti = '1'
159200010601     C                   if        vinflg = *blanks
159300010601     C                   add       1             cntNonEl
159400010601     C                   endif
159500010601     C                   endif
159600010601     C*
159700010601     C                   if        vinflg = '2'
159800010601     C                   eval      flgStato = '2'
159900010601     C                   endif
160000010601     C*
160100010601     C                   ENDSR
160200010601     C***
160300010601
160400010601
160500010601
160600010330
160700040119      /TITLE Invio dei dati al punto operativo.
160800040119     C     invio         BEGSR
160900040119     C*
161000040119     C* 1° invio FIVAT
161100040119     C                   reset                   dscmz
161200040119     C                   move      vlrpoi        cmzdst
161300040119     C                   eval      cmzfld = 'FIVATWWR'
161400040119     C                   eval      cmzmbd = vlrhdl
161500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161600040119     C***                if        prmfir = *blanks
161700040119     C                   eval      cmzfla = 'FIVAT00F'
161800040119     C                   eval      cmzmba = 'FIVAT00F'
161900040119     C***                else
162000040119     C***                eval      cmzfla = prmfir
162100040119     C***                eval      cmzmba = prmfir
162200040119     C***                endif
162300040119     C                   eval      cmznrr = *zeros
162400040119     C                   move      §ctrokvt      cmznrr
162500040119     C                   eval      cmzlba = vlrfl1
162600040119     C                   call(e)   'TIS711C'
162700040119     C                   parm                    dscmz
162800040119     C                   parm      *blanks       esito
162900040119     C                   if        %error
163000040119     C                             or cmzerr = '1'
163100040119     C                             or esito  = '1'
163200040119     C                   eval      wrkesito = '3'
163300040119     C                   else
163400040119     C*
163500040119     C* 2° invio FIVAB
163600040119     C                   reset                   dscmz
163700040119     C                   move      vlrpoi        cmzdst
163800040119     C                   eval      cmzfld = vlrfou
163900040119     C                   eval      cmzmbd = vlrhdl
164000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164100040119     C***                if        prmfir = *blanks
164200040119     C                   eval      cmzfla = 'FIVAB00F'
164300040119     C                   eval      cmzmba = 'FIVAB00F'
164400040119     C***                else
164500040119     C***                eval      cmzfla = prmfir
164600040119     C***                eval      cmzmba = prmfir
164700040119     C***                endif
164800040119     C                   eval      cmznrr = *zeros
164900040119     C                   move      §ctrokvb      cmznrr
165000040119     C                   eval      cmzlba = vlrfl1
165100040119     C                   call(e)   'TIS711C'
165200040119     C                   parm                    dscmz
165300040119     C                   parm      *blanks       esito
165400040119     C                   if        %error
165500040119     C                             or cmzerr = '1'
165600040119     C                             or esito  = '1'
165700040119     C                   eval      wrkesito = '3'
165800040119     C                   endif
165900040119     C                   endif
166000040119     C*
166100040119     C                   ENDSR
166200040119     C***
166300010601
166400010601
166500010601
166600010601
166700010601      /TITLE Invio dei dati al punto operativo.
166800010601     C     opeini        BEGSR
166900010601     C*
167000010601     C* Inizializzo flag e contatori operativi
167100010601     C                   movel     '0'           flgGiro           1
167200010601     C                   movel     '0'           flgMulti          1
167300010601     C                   movel     '1'           flgStato          1
167400010615     C                   movel     '0'           flgOk             1
167500010601     C                   z-add     *zeros        cntNonEl         10 0
167600010601     C                   z-add     *zeros        depfil            3 0
167700010601     C                   z-add     *zeros        invfil            3 0
167800010601     C*
167900010601     C                   ENDSR
168000010601     C***
168100070326
168200070326
168300070326
168400070326
168500070326     C     *pssr         BEGSR
168600070329     C*
168700070329     C                   if        %open(tivin00r)
168800070329     C                   close     tivin00r
168900070329     C                   endif
169000070329     C                   if        %open(fivabwwr)
169100070329     C                   close     fivabwwr
169200070329     C                   endif
169300070329     C                   if        %open(fivatwwr)
169400070329     C                   close     fivatwwr
169500070329     C                   endif
169600070326     C*
169700070326     C* Effettuo la chiamata al CLLE preposto
169800070326     C                   call(e)   'TITVVTC'
169900070326     C                   parm                    parccm
170000070326     C                   parm                    parmbr
170100070326     C                   parm      '2'           paropz
170200070326     C*
170300070326     C                   eval      wrkesito = '2'
170400070404     C*
170500070404     C                   seton                                        LR
170600070326     C*
170700070326     C                   ENDSR     '*CANCL'
170800070326     C***
170900100318
171000100318
171100100318
171200100318     C     normCSV       BEGSR
171300100318     C*
171400100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
171500100318     C* separatori campi
171600100318     C                   if        CharTXT <> *blanks
171700100318     C                   z-add     *zeros        wLenVINDTA        4 0
171800100318     C                   z-add     *zeros        wFlgVINDTA        1 0
171900100318     C                   setoff                                       80
172000100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
172100100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
172200100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
172300100318     C                   if        not *in80
172400100318     C                   seton                                        80
172500100318     C                   else
172600100318     C                   setoff                                       80
172700100318     C                   endif
172800100318     C                   endif
172900100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
173000100318     C                             and *in80 = *on
173100100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
173200100318     C                   endif
173300100318     C                   enddo
173400100318     C                   endif
173500100318     C*
173600100318     C                   ENDSR
173700100318     C***
173800070326
173900100326
174000100326
174100100326     C*----------------------------------------------------*
174200100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
174300100326     C*----------------------------------------------------*
174400100326     C     REPNSP        BEGSR
174500100326     C*
174600100326     C* NSP => Stacco un numeratore da AZNUM
174700100326     C                   clear                   TRUL33DS
174800100326     C                   eval      I33OPE = *zeros
174900100326     C                   eval      I33CNU = 302
175000100326     C                   eval      I33NUM = 1
175100100326     C                   movel     TRUL33DS      KPJBU
175200100326     C                   call      'TRUL33R'
175300100326     C                   parm                    KPJBA
175400100326     C                   movel     KPJBU         TRUL33DS
175500100326     C                   if        O33ERR = *zeros
175600100326     C                   z-add     O33NRF        VABNSP
175700100326     C                   z-add     O33NRF        VATNSP
175800100326     C                   else
175900100326     C                   SETON                                        31
176000100326     C                   EVAL      vinmsg = %trimr(vinmsg)
176100100326     C                             + ' ' + 'VABNSP VATNSP'
176200100326     C                   endif
176300100326     C*
176400100326     C                   ENDSR
176500010330
176600100326
176700010330
176800000613     C     *inzsr        BEGSR
176900990910     C*
177000990910     C     *entry        plist
177100990920     C                   parm                    tivlrds
177200990921     C                   parm      wrkesito      esito
177300000724     C                   parm                    prmlit
177400000710     C                   parm                    prmfir
177500010330     C*
177600010330     C* CALCOLA LA DATA CORRENTE
177700100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
177800100318     C                   eval      datcor = %dec(%date() : *ISO)
177900000613     C*
178000000613     C                   ENDSR
178100000613     C***
178200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
178300150202XXX;VABLNP;VABLNA;VABNRS;VABNCD;VABZNC;XXX;VABRSD;VABIND;VABCAD;
178400150202VABLOD;VABPRD;VABPKB;VABCAS;VABRMN;VABRMA;
178500140526
178600140526
178700100318
