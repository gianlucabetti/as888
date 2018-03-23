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
003600150513     D RcdTIVIN        s              9  0 INZ(*zeros)
003700100326     D*------------------
003800100326     D* DS REPERIMENTO NUMERATORE
003900100326     D*------------------
004000100326     D trul33ds      e ds                  inz
004100100326     D*------------------
004200100326     D* DS ARCHITETTURA
004300100326     D*------------------
004400100326     D kpjba         e ds                  inz
004500100326
004600030715     D*------------------
004700030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
004800030715     D*------------------
004900030715     D SkSplitFLD      S             10    DIM(100)
005000030715     D SkSplitCSV      S            256    DIM(100)
005100030715     D CharCSV         S              1
005200030715     D CharTXT         S              1
005300030715     D CharNUM         S              1
005400100318     D CharSOS         S              1
005500030715     D posDa           S              3  0 INZ(*zeros)
005600030715     D posA            S              3  0 INZ(*zeros)
005700030715     D i               s              3  0 INZ(1)
005800030715     D wGiro           s              1  0 INZ(*zeros)
005900140520     D wVATNOT_A       s                   LIKE(VATNOT) INZ
006000140520     D wVATNOT_B       s                   LIKE(VATNOT) INZ
006100140520     D wVATNOT_E       s                   LIKE(VATNOT) INZ
006200140520     D wVATNOT_I       s                   LIKE(VATNOT) INZ
006300140520     D wVATNOT_J       s                   LIKE(VATNOT) INZ
006400140520     D wVATNOT_S       s                   LIKE(VATNOT) INZ
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
007700140526
007800140520
007900100318
008000100318     D*------------------
008100100318     D* LINKING A DEFINIZIONI ESTERNE
008200100318     D*------------------
008300100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008400100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008500100318
008600010330
008700010330
008800010330
008900000913     C                   reset                   rrnum
009000990921     C                   reset                   esito
009100990921     C                   reset                   wrkesito
009200010601     C*
009300010601     C                   exsr      opeini
009400010605     C                   exsr      rwvab
009500010601     C*
009600010601     C                   seton                                        lr
009700010601
009800010601
009900010601
010000010601     C*--------------------------------------------------------
010100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010200010601     C*--------------------------------------------------------
010300010601     C     PREELA        BEGSR
010400010601     C*
010500010601     C* SE OCCORRE SPEDIRE IN FILIALE
010600010601     C                   if        invfil <> *zeros and
010700010601     C                             flgGiro = '0'
010800010601     C*
010900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011000010601     C                   eval      flgGiro = '1'
011100010601     C*
011200010601     C                   endif
011300010601     C*
011400010601     C                   ENDSR
011500010601     C***
011600010601
011700010601
011800010601
011900010601
012000010601     C*--------------------------------------------------------
012100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012200010601     C*--------------------------------------------------------
012300010601     C     ENDELA        BEGSR
012400040119     C*
012500040119     C* Effettuo la chiamata al CLLE preposto
012600040119     C                   call(e)   'TITVVTC'
012700040510     C                   parm                    parccm
012800040119     C                   parm                    parmbr
012900040119     C                   parm      '2'           paropz
013000050201     C*
013100050201     C* Effettuo lancio TISI95 solo x chiusura
013200050201     C                   CLEAR                   TISI95DS
013300050201     C                   EVAL      I95TLA = 'C'
013400050201     C                   CALL      'TISI95R'
013500050201     C                   PARM                    TISI95DS
013600000616     C*
013700010601     C                   ENDSR
013800010601     C***
013900000613
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
017000140526     C***                exsr      inzvar
017100140526     C***                exsr      impvab
017200110131     C*
017300110131     C* Effettuo una prima lettura fuori ciclo x ignorare la 1° riga
017400110912 xxx C***                read      tivin00r                               70
017500010330     C*
017600150513     C* conteggio rcd di TIVIN per sapere se stiamo elaborando il primo rcd (per non dichiarare
017700150513     C* come elaborata la intestazione) e quando rimane solo l'intestazione
017800150513     C                   EVAL      RcdTIVIN = 0
017900150513     C*
018000030822     C                   do        *HIVAL
018100010330     C*
018200030822     C                   read      tivin00r                               70
018300150202     C*
018400150513     C                   EVAL      RcdTIVIN += 1
018500150513     C*
018600150202     C* Salvo il dato di input originale
018700150202     C                   eval      savVINDTA = vinDTA
018800010618     C*
018900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
019000010618     C                   if        vinflg = '1'
019100010618     C                   eval      flgOk = '1'
019200010618     C                   endif
019300040510     C*
019400040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019500040510     C                   z-add     *zeros        wLenVINDTA        4 0
019600040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019700040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019800040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019900040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
020000040510     C                   z-add     1             wFlgVINDTA
020100040510     C                   leave
020200040510     C                   endif
020300040510     C                   enddo
020400010618     C*
020500040510     C                   if        vindta > *blanks AND
020600040510     C                             wFlgVINDTA = 1
020700010330     C                   add       1             rrnum
020800010330     C*
020900010601     C                   if        *in70 = *off and
021000010330     C                             (vinflg = *blanks
021100010330     C                              or vinflg = '0'
021200010330     C                              or vinflg = '2')
021300010330     C*
021400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021500010711     C                   if        vinflg = *blanks or vinflg = '0'
021600010711     C                   clear                   vinmsg
021700010711     C                   endif
021800010601     C*
021900070925     C* Aggiungo un byte a blanks all'inizio d ogni record
022000070925     C                   eval      vindta  = ' ' + %trim(vindta)
022100070925     C*
022200080527     C                   exsr      inzvar
022300100318     C                   exsr      normCSV
022400010605     C                   exsr      impvab
022500010601     C*
022600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022700010605     C                   eval      depfil = VABLNP
022800010601     C                   exsr      repfil
022900010601     C                   if        depfil = invfil
023000021025     C                   if        vlrpoi = 999
023100030822     C                   move(P)   invfil        VABFGS
023200021025     C                   else
023300030822     C                   move(P)   vlrpoi        VABFGS
023400021025     C                   endif
023500010601     C*
023600010601     C                   exsr      PREELA
023700010601     C*
023800010604     C* Ebbene...
023900010604     C*
024000040119     C  N31              add       1             §CTROKVB
024100040119     C   32              add       1             §CTRMO
024200040119     C   31              add       1             §CTRNO
024300080418     C*
024400030822     C                   if        wGiro = 2
024500080513     C* Scrivo sempre il VAT
024600080513     C  N31              exsr      wrivat
024700110201     C                   exsr      chkvab
024800080513     C                   endif
024900010604     C*
025000010604     C                   if        *in31 = *off and
025100010604     C                             *in32 = *off
025200150513     C* non aggiorno il rcd di intestazione
025300150513     C                   IF        RcdTIVIN <> 1
025400010604     C                   eval      vinflg = '1'
025500150513     C                   ENDIF
025600010604     C                   else
025700010604     C                   eval      vinflg = '2'
025800010604     C                   endif
025900010604     C                   endif
026000010601     C*
026100010604     C                   endif
026200010604     C*
026300010330     C                   else
026400150513     C* non aggiorno il rcd di intestazione
026500150513     C                   IF        RcdTIVIN <> 1
026600010330     C                   eval      vinflg = '1'
026700150513     C                   ENDIF
026800010330     C                   endif
026900010601     C*
027000150202     C* Ripristino il dato di input originale
027100150202     C                   eval      vinDTA = savVINDTA
027200150202     C*
027300010601     C  N70              update    tivin000
027400010330     C*
027500030822     C  N70              enddo
027600040119     C*
027700080527     C* Verifico dati rimasti "in cannna"...
027800080527     C                   seton                                        65
027900110201     C                   exsr      chkvab
028000080527     C*
028100040119     C                   endif                                                  (endif - chkcall)
028200010601     C*
028300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
028400010601     C                   if        cntNonEl = *zeros or
028500010601     C                             flgMulti = '0'
028600150513     C* oppure se ho letto solo 1 rcd che è l'intestazione
028700150513     C                             OR RcdTIVIN = 1
028800010330     C* Se non ci sono record con errori ...
028900010601     C                   if        §ctrno = 0 and
029000010604     C                             §ctrmo = 0 and
029100010601     C                             flgStato <> '2'
029200010330     C* ... restituisco esito OK.
029300010330     C                   eval      wrkesito = '0'
029400010330     C                   else
029500040119     C                   if        §ctrokvb > 0
029600010330     C                   eval      wrkesito = '1'
029700010330     C                   else
029800010615     C                   if        flgOk = '0'
029900010615     C                   eval      wrkesito = '2'
030000010615     C                   else
030100010615     C                   eval      wrkesito = '6'
030200010615     C                   endif
030300010330     C                   endif
030400010330     C                   endif
030500010601     C                   else
030600010601     C                   eval      wrkesito = '9'
030700010601     C                   endif
030800010330     C*
030900010330     C                   if        %open(tivin00r)
031000010330     C                   close     tivin00r
031100010330     C                   endif
031200021025     C                   if        %open(fivabwwr)
031300021025     C                   close     fivabwwr
031400010330     C                   endif
031500040512     C                   if        %open(fivatwwr)
031600040512     C                   close     fivatwwr
031700040512     C                   endif
031800010601     C*
031900010601     C                   if        vlrpoi <> 999
032000010601     C                   eval      invfil = vlrpoi
032100010601     C                   endif
032200010330     C*
032300040119     C                   if        §ctrokvb > 0
032400010601     C                             and invfil > *zeros
032500010330     C                   exsr      invio
032600010330     C                   endif
032700010601     C*
032800010618     C                   if        flgGiro = '1'
032900010601     C                   exsr      endela
033000010618     C                   endif
033100010330     C*
033200010330     C                   ENDSR
033300010330     C***
033400080527
033500080527
033600080527
033700080527     C*----------------------------------------------------*
033800080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
033900080527     C*----------------------------------------------------*
034000100413     C     CHKVAB        BEGSR
034100110201     C*
034200110201     C* Se richiesti ignorare colli / peso / volume
034300110315 xxx C***                if        *in35
034400110315 xxx C***                z-add     *zeros        VABNCL
034500110315 xxx C***                z-add     *zeros        VABPKB
034600110315 xxx C***                z-add     *zeros        VABVLB
034700110315 xxx C***                endif
034800080527     C*
034900080527     C* Scrivo il VAB solo a rottura d spedizione
035000080527     C                   if        depspe = *zeros
035100080527     C  N31              eval      fivabds_sav = fivabds
035200080527     C                   eval      depspe = vabNSP
035300080527     C                   else
035400080527     C                   if        depspe = vabNSP AND *in65 = *off
035500100413     C   51              add       VABNCL        sav_VABNCL
035600100413     C   51              add       VABPKB        sav_VABPKB
035700100413     C   51              add       VABVLB        sav_VABVLB
035800100720     C   52              z-add     VABNCL        sav_VABNCL
035900100720     C   52              z-add     VABPKB        sav_VABPKB
036000100720     C   52              z-add     VABVLB        sav_VABVLB
036100100720     C   53              add       VABNCL        sav_VABNCL
036200100720     C   53              z-add     VABPKB        sav_VABPKB
036300100720     C   53              z-add     VABVLB        sav_VABVLB
036400080527     C                   else
036500080527     C  N31              eval      fivabds_ok = fivabds_sav
036600080527     C  N31              exsr      wrivab
036700080527     C  N31              eval      fivabds_sav = fivabds
036800080527     C                   eval      depspe = vabNSP
036900080527     C                   endif
037000080527     C                   endif
037100080527     C*
037200080527     C                   ENDSR
037300040119
037400040119
037500040119
037600040119     C*----------------------------------------------------*
037700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
037800040119     C*----------------------------------------------------*
037900040510     C     WRIVAB        BEGSR
038000080513     C*
038100080513     C* Gestisco l'eventuale rottura x numero spedizione
038200080527     C                   if        VABNSP <> depspe or *in65 = *on
038300070301     C                   if        wVATNOT_E <> *blanks
038400070301     C                   eval      VABCTM = '7Q'
038500070301     C                   endif
038600100413     C                   write     fivab000
038700080513     C                   endif
038800040119     C*
038900040119     C                   ENDSR
039000040119
039100040119
039200040119
039300040119     C*----------------------------------------------------*
039400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
039500040119     C*----------------------------------------------------*
039600040510     C     WRIVAT        BEGSR
039700060512     C*
039800060512     C* Inizializzo i campi chiave
039900060512     C                   eval      VATFGS = VABFGS
040000060512     C                   eval      VATAAS = VABAAS
040100060512     C                   eval      VATCCM = VABCCM
040200070823     C                   eval      VATNRS = VABNRS
040300060512     C                   eval      VATNSP = VABNSP
040400060512     C                   eval      VATLNP = VABLNP
040500040119     C*
040600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
040700040510     C                   if        VATTRC = *blanks
040800040510     C                   eval      VATTRC = 'E'
040900040510     C                   endif
041000110318     C                   if        VATNOT <> *blanks
041100040510     C                   write     FIVAT000
041200040119     C                   add       1             §CTROKVT
041300110318     C                   endif
041400060331     C*
041500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
041600060331     C                   if        wVATNOT_A <> *blanks
041700060331     C                   eval      VATNOT = wVATNOT_A
041800060331     C                   eval      VATTRC = 'A'
041900060331     C                   write     FIVAT000
042000060331     C                   add       1             §CTROKVT
042100060331     C                   endif
042200060331     C*
042300060331     C                   if        wVATNOT_B <> *blanks
042400060331     C                   eval      VATNOT = wVATNOT_B
042500060331     C                   eval      VATTRC = 'B'
042600060331     C                   write     FIVAT000
042700060331     C                   add       1             §CTROKVT
042800060331     C                   endif
042900140526     C*
043000140526     C                   if        wVATNOT_E <> *blanks
043100140526     C                   eval      VATNOT = wVATNOT_E
043200140526     C                   eval      VATTRC = 'E'
043300140526     C                   write     FIVAT000
043400140526     C                   add       1             §CTROKVT
043500140526     C                   endif
043600110927     C*
043700110927     C                   if        wVATNOT_I <> *blanks
043800110927     C                   eval      VATNOT = wVATNOT_I
043900110927     C                   eval      VATTRC = 'I'
044000110927     C                   write     FIVAT000
044100110927     C                   add       1             §CTROKVT
044200110927     C                   endif
044300140520     C*
044400140520     C                   if        wVATNOT_J <> *blanks
044500140520     C                   eval      VATNOT = wVATNOT_J
044600140520     C                   eval      VATTRC = 'J'
044700140520     C                   write     FIVAT000
044800140520     C                   add       1             §CTROKVT
044900140520     C                   endif
045000140520     C*
045100140520     C                   if        wVATNOT_S <> *blanks
045200140520     C                   eval      VATNOT = wVATNOT_S
045300140520     C                   eval      VATTRC = 'S'
045400140520     C                   write     FIVAT000
045500140520     C                   add       1             §CTROKVT
045600140520     C                   endif
045700040119     C*
045800040119     C                   ENDSR
045900040119
046000040119
046100040119
046200040119     C*----------------------------------------------------*
046300040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
046400040119     C*----------------------------------------------------*
046500040119     C     PREVAT        BEGSR
046600040119     C*
046700040119     C* Compongo il nome del membro da dare al FIVATWWR
046800040119     C                   eval      parmbr = vlrhdl
046900040119     C                   movel     'M'           parmbr
047000040510     C                   eval      parccm = vlrksc
047100040119     C                   eval      paropz = '1'
047200040119     C* Effettuo la chiamata al CLLE preposto
047300040119     C                   call(e)   'TITVVTC'
047400040119     C                   parm                    parccm
047500040119     C                   parm                    parmbr
047600040119     C                   parm                    paropz
047700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
047800040119     C                   if        %error
047900040119     C                   movel     '1'           chkcall
048000040119     C                   else
048100040119     C                   movel     '0'           chkcall
048200040119     C                   endif
048300040119     C*
048400040119     C                   ENDSR
048500010601
048600010601
048700010601
048800010330     C*----------------------------------------------------*
048900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
049000010330     C*----------------------------------------------------*
049100040119     C     INZVAR        BEGSR
049200080527     C*
049300080527     C                   setoff                                       65
049400010330     C*
049500030822     C* Inizializzo variabili di wrk
049600020204     C                   Z-ADD     *zeros        Num5_0            5 0
049700101018     C                   MOVEL     *blanks       wFlgCAS
049800060331     C                   MOVEL     *blanks       wVATNOT_A
049900060331     C                   MOVEL     *blanks       wVATNOT_B
050000060331     C                   MOVEL     *blanks       wVATNOT_E
050100110927     C                   MOVEL     *blanks       wVATNOT_I
050200140520     C                   MOVEL     *blanks       wVATNOT_J
050300140520     C                   MOVEL     *blanks       wVATNOT_S
050400030822     C*
050500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
050600030822     C                   CLEAR                   SkSplitCSV
050700030822     C                   EVAL      i = 1
050800030822     C                   EVAL      posDa = *zeros
050900030822     C                   EVAL      posA  = *zeros
051000080527     C*
051100080527     C                   CLEAR                   FIVABDS
051200080527     C                   CLEAR                   FIVAT000
051300030822     C*
051400030822     C* Reimposto i valori di default
051500030822     C                   EXSR      DEFCAM
051600030822     C*
051700010330     C                   ENDSR
051800010330     C*----------------------------------------------------*
051900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
052000010330     C*----------------------------------------------------*
052100010330     C     DEFCAM        BEGSR
052200030822     C*
052300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
052400030715     C* e delimitatore testo.
052500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
052600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
052700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
052800100318     C*
052900100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
053000100318     C                   EVAL      CharSOS = CharNUM
053100030822     C*
053200030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
053300100720     C                   SETOFF                                       505152
053400100720     C                   SETOFF                                       53
053500100413     C                   SELECT
053600100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
053700100720     C                   SETON                                        50
053800100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
053900100413     C                   SETON                                        51
054000100720     C                   WHEN      %subst(vlrppt:1:1) = *blanks
054100100720     C                   SETON                                        52
054200100720     C                   WHEN      %subst(vlrppt:1:1) = 'C'
054300100720     C                   SETON                                        53
054400100413     C                   ENDSL
054500030822     C*
054600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
054700030822     C                   EVAL      posDaDft = 1
054800030822     C                   EVAL      posADft  = 0
054900030822     C                   EVAL      wGiroDft = 0
055000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
055100030822     C                             posDaDft > 0
055200030822     C*
055300030822     C* Gestisco il 1° giro
055400030822     C                   IF        wGiroDft = 0
055500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
055600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
055700030822     C* Incremento il contatore dei "giri"
055800030822     C                   EVAL      wGiroDft = 1
055900030822     C                   ELSE
056000030822     C                   EVAL      posDaDft = posADft
056100030822     C                   ENDIF
056200030822     C* Eseguo lo scan x trovare la fine del campo corrente
056300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
056400030822     C*
056500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
056600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
056700030822     C                   IF        posDaDft > 0 AND
056800030822     C                             posADft  > 0
056900030822     C* NCL
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'NCL'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABNCL
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* CCM
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'CCM'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiInt=*on
059200030822     C                   Z-ADD     PiVal         VABCCM
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* LNP
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'LNP'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiInt=*on
060500030822     C                   Z-ADD     PiVal         VABLNP
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* NRS
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'NRS'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiInt=*on
061800030822     C                   Z-ADD     PiVal         VABNRS
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CTR
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CTR'
062600030822     C                   EVAL      PiStr=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   EXSR      CHKNUM
063000030822     C                   IF        PiInt=*on
063100030822     C                   Z-ADD     PiVal         VABCTR
063200030822     C                   ENDIF
063300030822     C                   ENDIF
063400030822     C* PKB
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'PKB'
063900030822     C                   EVAL      PiStr=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   EXSR      CHKNUM
064300030822     C                   IF        PiNum=*on
064400030822     C                   Z-ADD     PiVal         VABPKB
064500030822     C                   ENDIF
064600030822     C                   ENDIF
064700030822     C* VLB
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'VLB'
065200030822     C                   EVAL      PiStr=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   EXSR      CHKNUM
065600030822     C                   IF        PiNum=*on
065700030822     C                   Z-ADD     PiVal         VABVLB
065800030822     C                   ENDIF
065900030822     C                   ENDIF
066000030822     C* QFT
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'QFT'
066500030822     C                   EVAL      PiStr=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   EXSR      CHKNUM
066900030822     C                   IF        PiNum=*on
067000030822     C                   Z-ADD     PiVal         VABQFT
067100030822     C                   ENDIF
067200030822     C                   ENDIF
067300030822     C* CBO
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'CBO'
067800030822     C                   EVAL      VABCBO=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TSP
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TSP'
068700030822     C                   EVAL      VABTSP=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* VAS
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'VAS'
069600030822     C                   EVAL      VABVAS=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* VCA
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'VCA'
070500030822     C                   EVAL      VABVCA=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900070925     C* TIC
071000070925     C                   IF        %subst(
071100070925     C                             %subst(vlrppt:posDaDft+1:
071200070925     C                             posADft-posDaDft-1):1:3)
071300070925     C                             = 'TIC'
071400070925     C                   EVAL      VABTIC=%trim(%subst(
071500070925     C                             %subst(vlrppt:posDaDft+1:
071600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700070925     C                   ENDIF
071800030822     C* GCA
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'GCA'
072300030822     C                   EVAL      VABGCA=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* CTM
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'CTM'
073200030822     C                   EVAL      VABCTM=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* FFD
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'FFD'
074100030822     C                   EVAL      VABFFD=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* VAD
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'VAD'
075000030822     C                   EVAL      VABVAD=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* GMA
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'GMA'
075900030822     C                   EVAL      VABGMA=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* GGA
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'GGA'
076800030822     C                   EVAL      VABGGA=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200030822     C* GVA
077300030822     C                   IF        %subst(
077400030822     C                             %subst(vlrppt:posDaDft+1:
077500030822     C                             posADft-posDaDft-1):1:3)
077600030822     C                             = 'GVA'
077700030822     C                   EVAL      VABGVA=%trim(%subst(
077800030822     C                             %subst(vlrppt:posDaDft+1:
077900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000030822     C                   ENDIF
078100030822     C* TC1
078200030822     C                   IF        %subst(
078300030822     C                             %subst(vlrppt:posDaDft+1:
078400030822     C                             posADft-posDaDft-1):1:3)
078500030822     C                             = 'TC1'
078600030822     C                   EVAL      VABTC1=%trim(%subst(
078700030822     C                             %subst(vlrppt:posDaDft+1:
078800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900030822     C                   ENDIF
079000030822     C* TC2
079100030822     C                   IF        %subst(
079200030822     C                             %subst(vlrppt:posDaDft+1:
079300030822     C                             posADft-posDaDft-1):1:3)
079400030822     C                             = 'TC2'
079500030822     C                   EVAL      VABTC2=%trim(%subst(
079600030822     C                             %subst(vlrppt:posDaDft+1:
079700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
079800030822     C                   ENDIF
079900040714     C* VATTRC
080000040714     C                   IF        %subst(
080100040714     C                             %subst(vlrppt:posDaDft+1:
080200040714     C                             posADft-posDaDft-1):1:3)
080300040714     C                             = 'TRC'
080400040714     C                   EVAL      VATTRC=%trim(%subst(
080500040714     C                             %subst(vlrppt:posDaDft+1:
080600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
080700040714     C                   ENDIF
080800030822     C* ...
080900030822     C                   ENDIF
081000030822     C                   ENDDO
081100020204     C*
081200010330     C                   ENDSR
081300010607     C*----------------------------------------------------*
081400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
081500010607     C*----------------------------------------------------*
081600010607     C     IMPVAB        BEGSR
081700010607     C*
081800010607     C                   SETOFF                                       3132
081900110315 xxx C***                SETON                                        35
082000030515     C*
082100010607     C                   MOVEL     datcor        VABAAS
082200010607     C                   MOVE      datcor        VABMGS
082300010607     C*
082400030715     C**********
082500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
082600030715     C**********
082700030715     C                   IF        wGiro = *zeros
082800040113     C*
082900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
083000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
083100080429     C                             %trim(intcol(3))+%trim(intcol(4))+
083200080429     C                             %trim(intcol(5))<>*blanks
083300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
083400080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
083500080429     C                                      %trim(intcol(5))
083600070823     C     ';':CharCSV   XLATE     vindta        vindta
083700040510     C                   ENDIF
083800031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
083900031201     C     minu:maiu     XLATE     vindta        vindta
084000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084100030715     C                   DOW       posDa <= %len(%trim(vindta))
084200030715     C*
084300030715     C* Gestisco il 1° campo
084400030715     C                   IF        i = 1
084500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084600030715     C                   EVAL      posDa = 1
084700030715     C* Eseguo lo scan x trovare la fine del primo campo
084800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084900030715     C* A questo punto "estraggo" il campo corrente
085000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
085100030715     C                                                    (posA-posDa))
085200030715     C* X i campi successivi al 1°
085300030715     C                   ELSE
085400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085500030715     C                   EVAL      posDa = posA + 1
085600030715     C* Eseguo lo scan x trovare la fine del campo corrente
085700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085800030715     C* Gestisco l'ultimo campo
085900030715     C                   IF        posA = *zeros
086000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086100030715     C* A questo punto "estraggo" il campo corrente
086200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086300030715     C                                                    (posA-posDa))
086400030715     C                   LEAVE
086500030715     C                   ELSE
086600030715     C* A questo punto "estraggo" il campo corrente
086700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
086800030715     C                                                    (posA-posDa))
086900030715     C                   ENDIF
087000030715     C                   ENDIF
087100030715     C* Incremento il contatore d campo
087200030715     C                   EVAL      i = i +1
087300030715     C                   ENDDO
087400030715     C*
087500030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087600030715     C* delimitazione testo
087700030715     C                   EVAL      i = 1
087800030715     C                   DOW       i < %elem(SkSplitFLD)
087900030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
088000030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
088100030715     C                   EVAL      i = i + 1
088200030715     C                   ENDDO
088300030715     C                   EVAL      wGiro = 1
088400030715     C                   ELSE
088500030715     C                   EVAL      wGiro = 2
088600031204     C**********
088700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
088800031204     C* d separatore campo
088900031204     C**********
089000031204     C                   EVAL      vindta = %trim(vindta)
089100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
089200031204     C                   EVAL      vindta = %subst(vindta:2)
089300031204     C                   ENDDO
089400031204     C*
089500031204     C                   Z-ADD     *zeros        lunghInput        4 0
089600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
089700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
089800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
089900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
090000110912     C                   ENDDO
090100030715     C**********
090200030715     C* Effettuo lo split del campo dati d input
090300030715     C**********
090400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
090500030715     C                   DOW       posDa <= %len(%trim(vindta))
090600030715     C*
090700030715     C* Gestisco il 1° campo
090800030715     C                   IF        i = 1
090900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
091000030715     C                   EVAL      posDa = 1
091100030715     C* Eseguo lo scan x trovare la fine del primo campo
091200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
091300030715     C* A questo punto "estraggo" il campo corrente
091400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091500030715     C                                                    (posA-posDa))
091600030715     C* X i campi successivi al 1°
091700030715     C                   ELSE
091800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
091900030715     C                   EVAL      posDa = posA + 1
092000031204     C* Verifico che nn vi sia il campo nn valorizzato
092100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
092200031204     C* Se campo nn valorizzato skippo al prossimo
092300031204     C                   EVAL      SkSplitCSV(i) = *blanks
092400031204     C                   EVAL      posA  = posA + 1
092500031204     C                   ELSE
092600030715     C* Eseguo lo scan x trovare la fine del campo corrente
092700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
092800030715     C* Gestisco l'ultimo campo
092900030715     C                   IF        posA = *zeros
093000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
093100030715     C* A questo punto "estraggo" il campo corrente
093200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093300030715     C                                                    (posA-posDa))
093400030715     C                   LEAVE
093500030715     C                   ELSE
093600030715     C* A questo punto "estraggo" il campo corrente
093700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
093800030715     C                                                    (posA-posDa))
093900030715     C                   ENDIF
094000031204     C                   ENDIF
094100030715     C                   ENDIF
094200030715     C* Incremento il contatore d campo
094300030715     C                   EVAL      i = i +1
094400030715     C                   ENDDO
094500030715     C*
094600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
094700030715     C* delimitazione testo
094800030715     C                   EVAL      i = 1
094900030715     C                   DOW       i < %elem(SkSplitCSV)
095000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
095100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
095200030715     C                   EVAL      i = i + 1
095300030715     C                   ENDDO
095400100720     C*
095500100720     C* Stacco un progressivo x numero spedizione
095600150202 xxx C                   EXSR      repNSP
095700100326     C*
095800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
095900030715     C                   EVAL      i = 1
096000030715     C                   DOW       i < %elem(SkSplitFLD)
096100030715     C*
096200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
096300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
096400030715     C                             SkSplitCSV(i) <> *blanks
096500030721     C*** ATB
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
096700030721     C                   EVAL      VABATB = SkSplitCSV(i)
096800030721     C                   ENDIF
096900030715     C*** RSD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
097100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
097200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
097300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
097400030715     C* ==
097500030715     C                   ENDIF
097600030721     C*** RD2
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
097800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030715     C*** IND
098100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
098200030715     C                   EVAL      VABIND = SkSplitCSV(i)
098300030715     C                   ENDIF
098400030715     C*** LOD
098500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
098600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
098700030715     C                   ENDIF
098800030715     C*** PRD
098900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
099000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
099100030715     C                   ENDIF
099200030721     C*** NZD
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
099400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030715     C*** RMA
099700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
099800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
099900030715     C                   ENDIF
100000030715     C*** NOT
100100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
100200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
100300030715     C                   ENDIF
100400030715     C*** NT2
100500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
100600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
100700030715     C                   ENDIF
100800030715     C*** VCA
100900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
101000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
101100030715     C                   ENDIF
101200030715     C*** VAS
101300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
101400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
101500030715     C                   ENDIF
101600030715     C*** FFD
101700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
101800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
101900030715     C                   ENDIF
102000030721     C*** GC1
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
102200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** GC2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
102600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** TSP
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
103000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
103100030721     C                   ENDIF
103200040510     C*** CBO
103300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
103400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
103500040510     C                   ENDIF
103600030721     C*** NAS
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
103800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*** TIC
104100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
104200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
104300030721     C                   ENDIF
104400030721     C*** GCA
104500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
104600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
104700030721     C                   ENDIF
104800030721     C*** XCO
104900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
105000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
105100030721     C                   ENDIF
105200030721     C*** CTM
105300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
105400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
105500030721     C                   ENDIF
105600030721     C*** TCR
105700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
105800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
105900030721     C                   ENDIF
106000030721     C*** CTS
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
106200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
106300030721     C                   ENDIF
106400030721     C*** FTM
106500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
106600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
106700030721     C                   ENDIF
106800030721     C*** VAD
106900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
107000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
107100030721     C                   ENDIF
107200030721     C*** GMA
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
107400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
107500030721     C                   ENDIF
107600030721     C*** GGA
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
107800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
107900030721     C                   ENDIF
108000030721     C*** GVA
108100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
108200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
108300030721     C                   ENDIF
108400030721     C*** TC1
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
108600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
108700030721     C                   ENDIF
108800030721     C*** TC2
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
109000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
109100030721     C                   ENDIF
109200030721     C*** SCL
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
109400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
109500030721     C                   ENDIF
109600030721     C*** RMO
109700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
109800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
109900030721     C                   ENDIF
110000030721     C*** NMO
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
110200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
110300030721     C                   ENDIF
110400030721     C*
110500030721     C* Reperisco quindi i campi numerici...
110600040510     C*** NSP
110700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
110800040510     C                   EVAL      PiStr=SkSplitCSV(i)
110900040510     C                   EXSR      CHKNUM
111000040510     C                   IF        PiInt=*on
111100040510     C                   Z-ADD     PiVal         VABNSP
111200040510     C                   ELSE
111300040510     C                   SETON                                        31
111400040510     C                   EVAL      VABNSP = *zeros
111500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040510     C                             + ' ' + 'VABNSP'
111700040510     C                   ENDIF
111800040510     C                   ENDIF
111900071106     C*** RMN
112000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
112100071106     C                   EVAL      PiStr=SkSplitCSV(i)
112200071106     C                   EXSR      CHKNUM
112300071106     C                   IF        PiInt=*on
112400071106     C                   Z-ADD     PiVal         VABRMN
112500071106     C                   ELSE
112600071106     C                   SETON                                        32
112700071106     C                   EVAL      VABRMN = *zeros
112800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
112900071106     C                             + ' ' + 'VABRMN'
113000071106     C                   ENDIF
113100071106     C                   ENDIF
113200030721     C*** CAD
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on
113700030721     C                   Z-ADD     PiVal         Num5_0
113800030721     C                   MOVEL(p)  Num5_0        VABCAD
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAD = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAD'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** NCL
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on
115100030721     C                   Z-ADD     PiVal         VABNCL
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      VABNCL = *zeros
115500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115600040122     C                             + ' ' + 'VABNCL'
115700030721     C                   ENDIF
115800030721     C                   ENDIF
115900030721     C*** PKB
116000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
116100030721     C                   EVAL      PiStr=SkSplitCSV(i)
116200030721     C                   EXSR      CHKNUM
116300030721     C                   IF        PiNum=*on
116400030721     C                   Z-ADD     PiVal         VABPKB
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABPKB = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABPKB'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** CAS
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
117400110506     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
117500110506     C                             %trim(SkSplitCSV(i)) <> *zeros
117600101018     C                   EVAL      wFlgCAS = '1'
117700101018     C                   ENDIF
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiNum=*on
118100030721     C                   Z-ADD     PiVal         VABCAS
118200030721     C                   ELSE
118300030721     C                   SETON                                        32
118400030721     C                   EVAL      VABCAS = *zeros
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABCAS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** CCM
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on      AND
119400030721     C                             PiVal<=9999999 AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABCCM
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABCCM = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100040510     C                             + ' ' + 'VABCCM'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** LNP
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on    AND
120900030721     C                             PiVal<=999   AND
121000030721     C                             PiVal>*zeros
121100030721     C                   Z-ADD     PiVal         VABLNP
121200030721     C                   ELSE
121300040715     C                   SETON                                        32
121400030721     C                   EVAL      VABLNP = *zeros
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600040510     C                             + ' ' + 'VABLNP'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** NRS
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on AND
122400030721     C                             PiVal<=99
122500030721     C                   Z-ADD     PiVal         VABNRS
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABNRS'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** LNA
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on    AND
123700030721     C                             PiVal<=999   AND
123800030721     C                             PiVal>*zeros
123900030721     C                   Z-ADD     PiVal         VABLNA
124000030721     C                   ELSE
124100030721     C                   SETON                                        32
124200030721     C                   EVAL      VABLNA = *zeros
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABLNA'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** CTR
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on  AND
125200030721     C                             PiVal<=999
125300030721     C                   Z-ADD     PiVal         VABCTR
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABCTR'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** IAS
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABIAS
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABIAS'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** VLB
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABVLB
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABVLB'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** QFT
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiNum=*on
128900030721     C                   Z-ADD     PiVal         VABQFT
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129300030721     C                             + ' ' + 'VABQFT'
129400030721     C                   ENDIF
129500030721     C                   ENDIF
129600030721     C*** NCD
129700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
129800030721     C                   EVAL      PiStr=SkSplitCSV(i)
129900030721     C                   EXSR      CHKNUM
130000030721     C                   IF        PiInt=*on      AND
130100030721     C                             PiVal<=9999999
130200030721     C                   Z-ADD     PiVal         VABNCD
130300030721     C                   ELSE
130400030721     C                   SETON                                        32
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABNCD'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900030721     C*** NCA
131000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
131100030721     C                   EVAL      PiStr=SkSplitCSV(i)
131200030721     C                   EXSR      CHKNUM
131300030721     C                   IF        PiInt=*on      AND
131400030721     C                             PiVal<=9999999
131500030721     C                   Z-ADD     PiVal         VABNCA
131600030721     C                   ELSE
131700030721     C                   SETON                                        32
131800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131900030721     C                             + ' ' + 'VABNCA'
132000030721     C                   ENDIF
132100030721     C                   ENDIF
132200030721     C*** ZNC
132300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
132400030721     C                   EVAL      PiStr=SkSplitCSV(i)
132500030721     C                   EXSR      CHKNUM
132600030721     C                   IF        PiInt=*on AND
132700030721     C                             PiVal<=99
132800030721     C                   Z-ADD     PiVal         VABZNC
132900030721     C                   ELSE
133000030721     C                   SETON                                        32
133100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133200030721     C                             + ' ' + 'VABZNC'
133300030721     C                   ENDIF
133400030721     C                   ENDIF
133500030721     C*** DCR
133600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
133700030721     C                   EVAL      PiStr=SkSplitCSV(i)
133800030721     C                   EXSR      CHKNUM
133900030721     C                   IF        PiInt=*on       AND
134000030721     C                             PiVal<=99999999
134100030721     C                   Z-ADD     PiVal         VABDCR
134200030721     C                   ELSE
134300030721     C                   SETON                                        32
134400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134500030721     C                             + ' ' + 'VABDCR'
134600030721     C                   ENDIF
134700030721     C                   ENDIF
134800030721     C*** HCR
134900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
135000030721     C                   EVAL      PiStr=SkSplitCSV(i)
135100030721     C                   EXSR      CHKNUM
135200030721     C                   IF        PiInt=*on   AND
135300030721     C                             PiVal<=9999
135400030721     C                   Z-ADD     PiVal         VABHCR
135500030721     C                   ELSE
135600030721     C                   SETON                                        32
135700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135800030721     C                             + ' ' + 'VABHCR'
135900030721     C                   ENDIF
136000030721     C                   ENDIF
136100030721     C*** VMD
136200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
136300030721     C                   EVAL      PiStr=SkSplitCSV(i)
136400030721     C                   EXSR      CHKNUM
136500030721     C                   IF        PiNum=*on
136600030721     C                   Z-ADD     PiVal         VABVMD
136700030721     C                   ELSE
136800030721     C                   SETON                                        32
136900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137000030721     C                             + ' ' + 'VABVMD'
137100030721     C                   ENDIF
137200030721     C                   ENDIF
137300030721     C*** ANT
137400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
137500030721     C                   EVAL      PiStr=SkSplitCSV(i)
137600030721     C                   EXSR      CHKNUM
137700030721     C                   IF        PiInt=*on        AND
137800030721     C                             PiVal<=999999999
137900030721     C                   Z-ADD     PiVal         VABANT
138000030721     C                   ELSE
138100030721     C                   SETON                                        32
138200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138300030721     C                             + ' ' + 'VABANT'
138400030721     C                   ENDIF
138500030721     C                   ENDIF
138600030721     C*** CMO
138700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
138800030721     C                   EVAL      PiStr=SkSplitCSV(i)
138900030721     C                   EXSR      CHKNUM
139000030721     C                   IF        PiInt=*on
139100030721     C                   Z-ADD     PiVal         Num5_0
139200030721     C                   MOVEL(p)  Num5_0        VABCMO
139300030721     C                   ELSE
139400030721     C                   SETON                                        32
139500030721     C                   EVAL      VABCMO = *zeros
139600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
139700030721     C                             + ' ' + 'VABCMO'
139800030721     C                   ENDIF
139900030721     C                   ENDIF
140000040510     C*
140100040510     C* Reperisco i campi del FIVAT
140200040510     C*** VATNOT
140300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
140400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
140500040510     C                   ENDIF
140600040510     C*** VATTRC
140700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
140800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
140900040510     C                   ENDIF
141000060331     C*** VATNOT_A
141100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
141200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
141300060331     C                   ENDIF
141400060331     C*** VATNOT_B
141500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
141600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
141700060331     C                   ENDIF
141800060331     C*** VATNOT_E
141900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
142000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
142100060331     C                   ENDIF
142200110927     C*** VATNOT_I
142300110927     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
142400110927     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
142500110927     C                   ENDIF
142600070823     C*
142700100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142800110421     C*
142900140526     C*** RMN / NSP
143000140526     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
143100140526     C                   EVAL      PiStr=%subst(SkSplitCSV(i):8:6)
143200140526     C                   EXSR      CHKNUM
143300140526     C                   IF        PiInt=*on
143400140526     C                   Z-ADD     PiVal         VABNSP
143500140526     C                   Z-ADD     PiVal         VABRMN
143600140526     C                   ELSE
143700140526     C                   SETON                                        31
143800140526     C                   EVAL      VABNSP = *zeros
143900140526     C                   EVAL      VABRMN = *zeros
144000140526     C                   EVAL      vinmsg = %trimr(vinmsg)
144100140526     C                             + ' ' + 'VABNSP/VABRMN'
144200140526     C                   ENDIF
144300140526     C                   ENDIF
144400140520     C*
144500070925     C*
144600071029     C***  <===  -----------------------------------------
144700040510     C*
144800030715     C                   ENDIF
144900030715     C                   EVAL      i = i + 1
145000030715     C                   ENDDO
145100070925     C*
145200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145300100831     C*
145400070925     C*
145500071029     C***  <===  -----------------------------------------
145600070925     C*
145700070925     C* Considerazioni finali su CBO/CAS
145800100318     C                   IF        wFlgCAS = '1'    OR
145900100318     C                             VABCAS  > *zeros
146000150202     C                   IF        VABVCA = *blanks
146100150202     C                   EVAL      VABVCA = 'EUR'
146200150202     C                   ENDIF
146300070925     C                   IF        VABCBO = '1'
146400070925     C                   EVAL      VABCBO = '4'
146500070925     C                   ENDIF
146600070925     C                   IF        VABCBO = '2'
146700070925     C                   EVAL      VABCBO = '6'
146800070925     C                   ENDIF
146900070925     C                   ENDIF
147000070925     C***  <===  ----------------------------
147100040510     C*
147200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
147300040510     C                   IF        VABNSP = *zeros
147400040510     C                   SETON                                        31
147500040510     C                   ENDIF
147600040715     C*
147700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
147800040715     C                   IF        VABLNP = *zeros
147900040715     C                   SETON                                        31
148000040715     C                   ENDIF
148100040510     C*
148200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
148300040510     C                   IF        VABRMN = *zeros
148400040510     C                   Z-ADD     VABNSP        VABRMN
148500040510     C                   ENDIF
148600040510     C*
148700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
148800040510     C                   IF        VABRMA = *blanks
148900040714     C                   MOVEL     VABRMN        VABRMA
149000040510     C                   ENDIF
149100040128     C*
149200040510     C* Se provincia nn valorizzata la reperisco
149300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
149400110912     C                   IF        VABPRD =  *blanks AND
149500110912     C                             VABNZD =  *blanks AND
149600110912     C                             VABCAD <> *blanks AND
149700110912     C                             VABLOD <> *blanks
149800040128     C                   CLEAR                   TISI95DS
149900040128     C                   EVAL      I95TCN = '3'
150000040128     C                   Z-ADD     datcor        I95DAT
150100040128     C                   EVAL      I95NAR = VABNZD
150200040128     C                   EVAL      I95CAP = VABCAD
150300040128     C                   EVAL      I95LOC = VABLOD
150400040128     C                   CALL      'TISI95R'
150500040128     C                   PARM                    TISI95DS
150600040128     C                   EVAL      VABPRD = O95PRV
150700040510     C                   ENDIF
150800020204     C*
150900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
151000020204     C                   EXSR      CHKIMPDIV
151100030715     C                   ENDIF
151200020204     C*
151300010607     C                   ENDSR
151400010607     C*----------------------------------------------------*
151500010601
151600020204
151700020204     C*----------------------------------------------------*
151800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
151900020204     C*----------------------------------------------------*
152000020204     C     CHKIMPDIV     BEGSR
152100020204     C*
152200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
152300020204     C                   Z-ADD     *zeros        wrkDec            9 9
152400020204     C*
152500020204     C* Come prima cosa effettuo considerazioni sulla divisa
152600020204     C                   IF        vabIAS > *zeros
152700020204     C                   IF        vabVAS <> 'EUR'
152800020204     C                   EVAL      vabVAS =  'ITL'
152900020204     C                   ENDIF
153000020204     C                   ENDIF
153100020204     C*
153200020204     C                   IF        vabCAS > *zeros
153300020204     C                   IF        vabVCA <> 'EUR'
153400020204     C                   EVAL      vabVCA =  'ITL'
153500020204     C                   ENDIF
153600020204     C                   ENDIF
153700020204     C*
153800020204     C                   IF        vabVMD > *zeros
153900020204     C                   IF        vabVAD <> 'EUR'
154000020204     C                   EVAL      vabVAD =  'ITL'
154100020204     C                   ENDIF
154200020204     C                   ENDIF
154300020204     C*
154400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
154500020204     C                   Z-ADD     vabIAS        wrkDec
154600020204     C                   IF        wrkDec > *zeros
154700020204     C                   IF        vabVAS = 'ITL'
154800020204     C                   EVAL      vabIAS = *zeros
154900020204     C                   ENDIF
155000020204     C                   ENDIF
155100020204     C*
155200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
155300020204     C                   Z-ADD     vabCAS        wrkDec
155400020204     C                   IF        wrkDec > *zeros
155500020204     C                   IF        vabVCA = 'ITL'
155600020204     C                   EVAL      vabCAS = *zeros
155700020204     C                   ENDIF
155800020204     C                   ENDIF
155900020204     C*
156000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
156100020204     C                   Z-ADD     vabVMD        wrkDec
156200020204     C                   IF        wrkDec > *zeros
156300020204     C                   IF        vabVAD = 'ITL'
156400020204     C                   EVAL      vabVMD = *zeros
156500020204     C                   ENDIF
156600020204     C                   ENDIF
156700020204     C*
156800020204     C                   ENDSR
156900020204     C***
157000010330
157100100318
157200100318
157300100318     C*----------------------------------------------------*
157400100318     C*  CONTROLLO NUMERICITA' CAMPI
157500100318     C*----------------------------------------------------*
157600100318     C     CHKNUM        BEGSR
157700100318     C*
157800100318     C                   IF        PiDecChr = *blanks
157900100318     C                   EVAL      PiDecChr = CharNUM
158000100318     C                   ENDIF
158100100318     C*
158200100318     C                   callp     UBISNUM_Check(PiStr
158300100318     C                                          :PiDecChr
158400100318     C                                          :PiVal
158500100318     C                                          :PiNum
158600100318     C                                          :PiInt)
158700100318     C*
158800100318     C                   ENDSR
158900100318     C***
159000010601
159100010601
159200010601
159300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159400010601     C     repfil        BEGSR
159500010601     C*
159600010601     C                   if        invfil = *zeros and
159700010601     C                             depfil > *zeros and
159800010629     C                             (vinflg = *blanks or
159900010629     C                              vinflg = *zeros)
160000010601     C
160100010601     C                   eval      invfil = depfil
160200010601     C                   endif
160300010601     C*
160400010601     C                   if        depfil <> invfil and
160500010601     C                             invfil > *zeros
160600010601     C                   eval      flgMulti = '1'
160700010601     C                   if        vinflg = *blanks
160800010601     C                   add       1             cntNonEl
160900010601     C                   endif
161000010601     C                   endif
161100010601     C*
161200010601     C                   if        vinflg = '2'
161300010601     C                   eval      flgStato = '2'
161400010601     C                   endif
161500010601     C*
161600010601     C                   ENDSR
161700010601     C***
161800010601
161900010601
162000010601
162100010330
162200040119      /TITLE Invio dei dati al punto operativo.
162300040119     C     invio         BEGSR
162400040119     C*
162500040119     C* 1° invio FIVAT
162600040119     C                   reset                   dscmz
162700040119     C                   move      vlrpoi        cmzdst
162800040119     C                   eval      cmzfld = 'FIVATWWR'
162900040119     C                   eval      cmzmbd = vlrhdl
163000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163100040119     C***                if        prmfir = *blanks
163200040119     C                   eval      cmzfla = 'FIVAT00F'
163300040119     C                   eval      cmzmba = 'FIVAT00F'
163400040119     C***                else
163500040119     C***                eval      cmzfla = prmfir
163600040119     C***                eval      cmzmba = prmfir
163700040119     C***                endif
163800040119     C                   eval      cmznrr = *zeros
163900040119     C                   move      §ctrokvt      cmznrr
164000040119     C                   eval      cmzlba = vlrfl1
164100040119     C                   call(e)   'TIS711C'
164200040119     C                   parm                    dscmz
164300040119     C                   parm      *blanks       esito
164400040119     C                   if        %error
164500040119     C                             or cmzerr = '1'
164600040119     C                             or esito  = '1'
164700040119     C                   eval      wrkesito = '3'
164800040119     C                   else
164900040119     C*
165000040119     C* 2° invio FIVAB
165100040119     C                   reset                   dscmz
165200040119     C                   move      vlrpoi        cmzdst
165300040119     C                   eval      cmzfld = vlrfou
165400040119     C                   eval      cmzmbd = vlrhdl
165500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165600040119     C***                if        prmfir = *blanks
165700040119     C                   eval      cmzfla = 'FIVAB00F'
165800040119     C                   eval      cmzmba = 'FIVAB00F'
165900040119     C***                else
166000040119     C***                eval      cmzfla = prmfir
166100040119     C***                eval      cmzmba = prmfir
166200040119     C***                endif
166300040119     C                   eval      cmznrr = *zeros
166400040119     C                   move      §ctrokvb      cmznrr
166500040119     C                   eval      cmzlba = vlrfl1
166600040119     C                   call(e)   'TIS711C'
166700040119     C                   parm                    dscmz
166800040119     C                   parm      *blanks       esito
166900040119     C                   if        %error
167000040119     C                             or cmzerr = '1'
167100040119     C                             or esito  = '1'
167200040119     C                   eval      wrkesito = '3'
167300040119     C                   endif
167400040119     C                   endif
167500040119     C*
167600040119     C                   ENDSR
167700040119     C***
167800010601
167900010601
168000010601
168100010601
168200010601      /TITLE Invio dei dati al punto operativo.
168300010601     C     opeini        BEGSR
168400010601     C*
168500010601     C* Inizializzo flag e contatori operativi
168600010601     C                   movel     '0'           flgGiro           1
168700010601     C                   movel     '0'           flgMulti          1
168800010601     C                   movel     '1'           flgStato          1
168900010615     C                   movel     '0'           flgOk             1
169000010601     C                   z-add     *zeros        cntNonEl         10 0
169100010601     C                   z-add     *zeros        depfil            3 0
169200010601     C                   z-add     *zeros        invfil            3 0
169300010601     C*
169400010601     C                   ENDSR
169500010601     C***
169600070326
169700070326
169800070326
169900070326
170000070326     C     *pssr         BEGSR
170100070329     C*
170200070329     C                   if        %open(tivin00r)
170300070329     C                   close     tivin00r
170400070329     C                   endif
170500070329     C                   if        %open(fivabwwr)
170600070329     C                   close     fivabwwr
170700070329     C                   endif
170800070329     C                   if        %open(fivatwwr)
170900070329     C                   close     fivatwwr
171000070329     C                   endif
171100070326     C*
171200070326     C* Effettuo la chiamata al CLLE preposto
171300070326     C                   call(e)   'TITVVTC'
171400070326     C                   parm                    parccm
171500070326     C                   parm                    parmbr
171600070326     C                   parm      '2'           paropz
171700070326     C*
171800070326     C                   eval      wrkesito = '2'
171900070404     C*
172000070404     C                   seton                                        LR
172100070326     C*
172200070326     C                   ENDSR     '*CANCL'
172300070326     C***
172400100318
172500100318
172600100318
172700100318     C     normCSV       BEGSR
172800100318     C*
172900100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
173000100318     C* separatori campi
173100100318     C                   if        CharTXT <> *blanks
173200100318     C                   z-add     *zeros        wLenVINDTA        4 0
173300100318     C                   z-add     *zeros        wFlgVINDTA        1 0
173400100318     C                   setoff                                       80
173500100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
173600100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
173700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
173800100318     C                   if        not *in80
173900100318     C                   seton                                        80
174000100318     C                   else
174100100318     C                   setoff                                       80
174200100318     C                   endif
174300100318     C                   endif
174400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
174500100318     C                             and *in80 = *on
174600100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
174700100318     C                   endif
174800100318     C                   enddo
174900100318     C                   endif
175000100318     C*
175100100318     C                   ENDSR
175200100318     C***
175300070326
175400100326
175500100326
175600100326     C*----------------------------------------------------*
175700100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
175800100326     C*----------------------------------------------------*
175900100326     C     REPNSP        BEGSR
176000100326     C*
176100100326     C* NSP => Stacco un numeratore da AZNUM
176200100326     C                   clear                   TRUL33DS
176300100326     C                   eval      I33OPE = *zeros
176400100326     C                   eval      I33CNU = 302
176500100326     C                   eval      I33NUM = 1
176600100326     C                   movel     TRUL33DS      KPJBU
176700100326     C                   call      'TRUL33R'
176800100326     C                   parm                    KPJBA
176900100326     C                   movel     KPJBU         TRUL33DS
177000100326     C                   if        O33ERR = *zeros
177100100326     C                   z-add     O33NRF        VABNSP
177200100326     C                   z-add     O33NRF        VATNSP
177300100326     C                   else
177400100326     C                   SETON                                        31
177500100326     C                   EVAL      vinmsg = %trimr(vinmsg)
177600100326     C                             + ' ' + 'VABNSP VATNSP'
177700100326     C                   endif
177800100326     C*
177900100326     C                   ENDSR
178000010330
178100100326
178200010330
178300000613     C     *inzsr        BEGSR
178400990910     C*
178500990910     C     *entry        plist
178600990920     C                   parm                    tivlrds
178700990921     C                   parm      wrkesito      esito
178800000724     C                   parm                    prmlit
178900000710     C                   parm                    prmfir
179000010330     C*
179100010330     C* CALCOLA LA DATA CORRENTE
179200100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
179300100318     C                   eval      datcor = %dec(%date() : *ISO)
179400000613     C*
179500000613     C                   ENDSR
179600000613     C***
179700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
179800150202XXX;VABLNP;VABLNA;VABNRS;VABNCD;VABZNC;XXX;VABRSD;VABIND;VABCAD;
179900150202VABLOD;VABPRD;VABPKB;VABCAS;VABRMN;VABRMA;
180000140526
180100140526
180200100318
