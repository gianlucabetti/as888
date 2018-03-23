000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200180131      *
000300180131      * PARTICOLARITA':
000400180131      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600000313     F*
000700990910     Ftivin00r  uF   E             DISK    usropn
000800100413     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000900040119     FFIVATwwr  O    E             DISK    usropn
001000000313     D*
001100040113     D*------------
001200040113     D* SCHIEDE A PROGRAMMA
001300040113     D*------------
001400080429     D intcol          S            100    DIM(5) CTDATA PERRCD(1)
001500010330     D*----------------------------------------------------
001600010330     D* DICHIARAZIOINE VARIABILI DI WRK
001700010330     D*----------------------------------------------------
001800010330     D dscmz         e ds                  inz
001900010330     D psds           sds
002000010330     D  procname         *PROC
002100010330     D tivlrds       e ds                  extname(tivlr00f)
002200040128     D tisi95ds      e ds
002300100413     D fivabds       e ds                  extname(fivab00f)
002400100413     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
002500100413     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
002600010330     D esito           s              1
002700010330     D prmlit          s             10
002800010330     D prmfir          s             10
002900010330     D wrkesito        s                   like(esito)
003000010330     D rrnum           s              6  0 INZ(*zeros)
003100040510     D parccm          s              8    INZ(*blanks)
003200040119     D parmbr          s             10    INZ(*blanks)
003300040119     D paropz          s              1    INZ(*blanks)
003400040119     D chkcall         s              1    INZ(*blanks)
003500080513     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
003600080418     D wFlgCAS         s              1    INZ(*blanks)
003700180131     D* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
003800180131     D* variabile Intestazioni = *off se nel .csv ci sono solo dati
003900180131     D* in questo caso è la seconda
004000180131     D Intestazioni    s               n   inz(*on)
004100100326     D*------------------
004200100326     D* DS REPERIMENTO NUMERATORE
004300100326     D*------------------
004400100326     D trul33ds      e ds                  inz
004500100326     D*------------------
004600100326     D* DS ARCHITETTURA
004700100326     D*------------------
004800100326     D kpjba         e ds                  inz
004900100326
005000030715     D*------------------
005100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
005200030715     D*------------------
005300030715     D SkSplitFLD      S             10    DIM(100)
005400030715     D SkSplitCSV      S            256    DIM(100)
005500030715     D CharCSV         S              1
005600030715     D CharTXT         S              1
005700030715     D CharNUM         S              1
005800100318     D CharSOS         S              1
005900030715     D posDa           S              3  0 INZ(*zeros)
006000030715     D posA            S              3  0 INZ(*zeros)
006100030715     D i               s              3  0 INZ(1)
006200030715     D wGiro           s              1  0 INZ(*zeros)
006300070823     D wVATNOT_A       s                   LIKE(VATNOT)
006400060331     D wVATNOT_B       s                   LIKE(VATNOT)
006500060331     D wVATNOT_E       s                   LIKE(VATNOT)
006600111117     D wVATNOT_I       s                   LIKE(VATNOT)
006700111117     D wVATNOT_J       s                   LIKE(VATNOT)
006800100831     D wVABRSD         s                   LIKE(VABRSD)
006900100831     D wVABIND         s                   LIKE(VABIND)
007000180201     D wVABCAD         s              5  0
007100100831     D wVABLOD         s                   LIKE(VABLOD)
007200100831     D wVABPRD         s                   LIKE(VABPRD)
007300120104     D wVABNZD         s                   LIKE(VABNZD)
007400030822     D*------------------
007500030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
007600030822     D*------------------
007700030822     D posDaDft        S              3  0 INZ(*zeros)
007800030822     D posADft         S              3  0 INZ(*zeros)
007900030822     D j               s              3  0 INZ(1)
008000030822     D wGiroDft        s              1  0 INZ(*zeros)
008100080418     D*------------------
008200080418     D* Costanti
008300080418     D*------------------
008400080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
008500080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
008600100318
008700100318     D*------------------
008800100318     D* LINKING A DEFINIZIONI ESTERNE
008900100318     D*------------------
009000100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009100100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
009200100318
009300010330
009400010330
009500010330
009600000913     C                   reset                   rrnum
009700990921     C                   reset                   esito
009800990921     C                   reset                   wrkesito
009900010601     C*
010000010601     C                   exsr      opeini
010100010605     C                   exsr      rwvab
010200010601     C*
010300010601     C                   seton                                        lr
010400010601
010500010601
010600010601
010700010601     C*--------------------------------------------------------
010800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010900010601     C*--------------------------------------------------------
011000010601     C     PREELA        BEGSR
011100010601     C*
011200010601     C* SE OCCORRE SPEDIRE IN FILIALE
011300010601     C                   if        invfil <> *zeros and
011400010601     C                             flgGiro = '0'
011500010601     C*
011600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011700010601     C                   eval      flgGiro = '1'
011800010601     C*
011900010601     C                   endif
012000010601     C*
012100010601     C                   ENDSR
012200010601     C***
012300010601
012400010601
012500010601
012600010601
012700010601     C*--------------------------------------------------------
012800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012900010601     C*--------------------------------------------------------
013000010601     C     ENDELA        BEGSR
013100040119     C*
013200040119     C* Effettuo la chiamata al CLLE preposto
013300040119     C                   call(e)   'TITVVTC'
013400040510     C                   parm                    parccm
013500040119     C                   parm                    parmbr
013600040119     C                   parm      '2'           paropz
013700050201     C*
013800050201     C* Effettuo lancio TISI95 solo x chiusura
013900050201     C                   CLEAR                   TISI95DS
014000050201     C                   EVAL      I95TLA = 'C'
014100050201     C                   CALL      'TISI95R'
014200050201     C                   PARM                    TISI95DS
014300000616     C*
014400010601     C                   ENDSR
014500010601
014600010601
014700010330     C*--------------------------------------------------------
014800021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
014900010330     C*--------------------------------------------------------
015000010605     C     RWVAB         BEGSR
015100010330     C*
015200010330     C                   if        not %open(tivin00r)
015300010330     C                   open      tivin00r
015400010330     C                   endif
015500021025     C                   if        not %open(fivabwwr)
015600021025     C                   open      fivabwwr
015700010330     C                   endif
015800040119     C*
015900040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016000040119     C                   exsr      prevat
016100040119     C*
016200040119     C                   if        chkcall = '0'
016300040119     C*
016400040119     C                   if        not %open(fivatwwr)
016500040119     C                   open      fivatwwr
016600040119     C                   endif
016700010330     C*
016800040119     C                   clear                   §CTROKVB          7 0
016900040119     C                   clear                   §CTROKVT          7 0
017000040119     C                   clear                   §CTRMO            7 0
017100040119     C                   clear                   §CTRNO            7 0
017200070823     C*
017300180131     C* variabile Intestazioni = *on se nel .csv la prima riga contiene le intestazioni di colonna
017400180131     C* variabile Intestazioni = *off se nel .csv ci sono solo dati
017500180131     C                   IF        Intestazioni = *off
017600070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800100914     C                   exsr      inzvar
017900100914     C                   exsr      impvab
018000180131     C                   ENDIF
018100010330     C*
018200030822     C                   do        *HIVAL
018300010330     C*
018400030822     C                   read      tivin00r                               70
018500010618     C*
018600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018700010618     C                   if        vinflg = '1'
018800010618     C                   eval      flgOk = '1'
018900010618     C                   endif
019000040510     C*
019100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019200040510     C                   z-add     *zeros        wLenVINDTA        4 0
019300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019400040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019700040510     C                   z-add     1             wFlgVINDTA
019800040510     C                   leave
019900040510     C                   endif
020000040510     C                   enddo
020100010618     C*
020200040510     C                   if        vindta > *blanks AND
020300040510     C                             wFlgVINDTA = 1
020400010330     C                   add       1             rrnum
020500010330     C*
020600010601     C                   if        *in70 = *off and
020700010330     C                             (vinflg = *blanks
020800010330     C                              or vinflg = '0'
020900010330     C                              or vinflg = '2')
021000010330     C*
021100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021200010711     C                   if        vinflg = *blanks or vinflg = '0'
021300010711     C                   clear                   vinmsg
021400010711     C                   endif
021500010601     C*
021600070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021700070925     C                   eval      vindta  = ' ' + %trim(vindta)
021800070925     C*
021900080527     C                   exsr      inzvar
022000100318     C                   exsr      normCSV
022100010605     C                   exsr      impvab
022200010601     C*
022300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022400010605     C                   eval      depfil = VABLNP
022500010601     C                   exsr      repfil
022600010601     C                   if        depfil = invfil
022700021025     C                   if        vlrpoi = 999
022800030822     C                   move(P)   invfil        VABFGS
022900021025     C                   else
023000030822     C                   move(P)   vlrpoi        VABFGS
023100021025     C                   endif
023200010601     C*
023300010601     C                   exsr      PREELA
023400010601     C*
023500010604     C* Ebbene...
023600010604     C*
023700040119     C  N31              add       1             §CTROKVB
023800040119     C   32              add       1             §CTRMO
023900040119     C   31              add       1             §CTRNO
024000080418     C*
024100030822     C                   if        wGiro = 2
024200080513     C* Scrivo sempre il VAT
024300080513     C  N31              exsr      wrivat
024400080527     C                   exsr      chkvab
024500080513     C                   endif
024600010604     C*
024700010604     C                   if        *in31 = *off and
024800010604     C                             *in32 = *off
024900010604     C                   eval      vinflg = '1'
025000010604     C                   else
025100010604     C                   eval      vinflg = '2'
025200010604     C                   endif
025300010604     C                   endif
025400010601     C*
025500010604     C                   endif
025600010604     C*
025700010330     C                   else
025800010330     C                   eval      vinflg = '1'
025900010330     C                   endif
026000010601     C*
026100010601     C  N70              update    tivin000
026200010330     C*
026300030822     C  N70              enddo
026400040119     C*
026500080527     C* Verifico dati rimasti "in cannna"...
026600080527     C                   seton                                        65
026700080527     C                   exsr      chkvab
026800080527     C*
026900040119     C                   endif                                                  (endif - chkcall)
027000010601     C*
027100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027200010601     C                   if        cntNonEl = *zeros or
027300010601     C                             flgMulti = '0'
027400010330     C* Se non ci sono record con errori ...
027500010601     C                   if        §ctrno = 0 and
027600010604     C                             §ctrmo = 0 and
027700010601     C                             flgStato <> '2'
027800010330     C* ... restituisco esito OK.
027900010330     C                   eval      wrkesito = '0'
028000010330     C                   else
028100040119     C                   if        §ctrokvb > 0
028200010330     C                   eval      wrkesito = '1'
028300010330     C                   else
028400010615     C                   if        flgOk = '0'
028500010615     C                   eval      wrkesito = '2'
028600010615     C                   else
028700010615     C                   eval      wrkesito = '6'
028800010615     C                   endif
028900010330     C                   endif
029000010330     C                   endif
029100010601     C                   else
029200010601     C                   eval      wrkesito = '9'
029300010601     C                   endif
029400010330     C*
029500010330     C                   if        %open(tivin00r)
029600010330     C                   close     tivin00r
029700010330     C                   endif
029800021025     C                   if        %open(fivabwwr)
029900021025     C                   close     fivabwwr
030000010330     C                   endif
030100040512     C                   if        %open(fivatwwr)
030200040512     C                   close     fivatwwr
030300040512     C                   endif
030400010601     C*
030500010601     C                   if        vlrpoi <> 999
030600010601     C                   eval      invfil = vlrpoi
030700010601     C                   endif
030800010330     C*
030900040119     C                   if        §ctrokvb > 0
031000010601     C                             and invfil > *zeros
031100010330     C                   exsr      invio
031200010330     C                   endif
031300010601     C*
031400010618     C                   if        flgGiro = '1'
031500010601     C                   exsr      endela
031600010618     C                   endif
031700010330     C*
031800010330     C                   ENDSR
031900010330     C***
032000080527
032100080527
032200080527
032300080527     C*----------------------------------------------------*
032400080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
032500080527     C*----------------------------------------------------*
032600100413     C     CHKVAB        BEGSR
032700080527     C*
032800080527     C* Scrivo il VAB solo a rottura d spedizione
032900080527     C                   if        depspe = *zeros
033000080527     C  N31              eval      fivabds_sav = fivabds
033100080527     C                   eval      depspe = vabNSP
033200080527     C                   else
033300080527     C                   if        depspe = vabNSP AND *in65 = *off
033400100413     C   51              add       VABNCL        sav_VABNCL
033500100413     C   51              add       VABPKB        sav_VABPKB
033600100413     C   51              add       VABVLB        sav_VABVLB
033700100720     C   52              z-add     VABNCL        sav_VABNCL
033800100720     C   52              z-add     VABPKB        sav_VABPKB
033900100720     C   52              z-add     VABVLB        sav_VABVLB
034000100720     C   53              add       VABNCL        sav_VABNCL
034100100720     C   53              z-add     VABPKB        sav_VABPKB
034200100720     C   53              z-add     VABVLB        sav_VABVLB
034300080527     C                   else
034400080527     C  N31              eval      fivabds_ok = fivabds_sav
034500080527     C  N31              exsr      wrivab
034600080527     C  N31              eval      fivabds_sav = fivabds
034700080527     C                   eval      depspe = vabNSP
034800080527     C                   endif
034900080527     C                   endif
035000080527     C*
035100080527     C                   ENDSR
035200040119
035300040119
035400040119
035500040119     C*----------------------------------------------------*
035600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
035700040119     C*----------------------------------------------------*
035800040510     C     WRIVAB        BEGSR
035900080513     C*
036000080513     C* Gestisco l'eventuale rottura x numero spedizione
036100080527     C                   if        VABNSP <> depspe or *in65 = *on
036200070301     C                   if        wVATNOT_E <> *blanks
036300070301     C                   eval      VABCTM = '7Q'
036400070301     C                   endif
036500100413     C                   write     fivab000
036600080513     C                   endif
036700040119     C*
036800040119     C                   ENDSR
036900040119
037000040119
037100040119
037200040119     C*----------------------------------------------------*
037300040119     C*  SCARICAMENTO BUFFER RECORDS VAT
037400040119     C*----------------------------------------------------*
037500040510     C     WRIVAT        BEGSR
037600060512     C*
037700060512     C* Inizializzo i campi chiave
037800060512     C                   eval      VATFGS = VABFGS
037900060512     C                   eval      VATAAS = VABAAS
038000060512     C                   eval      VATCCM = VABCCM
038100070823     C                   eval      VATNRS = VABNRS
038200060512     C                   eval      VATNSP = VABNSP
038300060512     C                   eval      VATLNP = VABLNP
038400040714     C*
038500040714     C* Solo se VATNOT valorizzato scarico il buffer
038600040714     C                   if        VATNOT <> *blanks
038700040119     C*
038800040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
038900040510     C                   if        VATTRC = *blanks
039000040510     C                   eval      VATTRC = 'E'
039100040510     C                   endif
039200040510     C                   write     FIVAT000
039300040119     C                   add       1             §CTROKVT
039400040714     C*
039500040714     C                   endif
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
041100111117
041200111117      * Indirizzo e-mail destinatario
041300111117     C                   if        wVATNOT_I <> *blanks
041400111117     C                   eval      VATNOT = wVATNOT_I
041500111117     C                   eval      VATTRC = 'I'
041600111117     C                   write     FIVAT000
041700111117     C                   add       1             §CTROKVT
041800111117     C                   endif
041900111117     C                   if        wVATNOT_J <> *blanks
042000111117     C                   eval      VATNOT = wVATNOT_J
042100111117     C                   eval      VATTRC = 'J'
042200111117     C                   write     FIVAT000
042300111117     C                   add       1             §CTROKVT
042400111117     C                   endif
042500120104      * Barcode
042600060331     C                   if        wVATNOT_E <> *blanks
042700060331     C                   eval      VATTRC = 'E'
042800100917     C                   eval      VATNOT = wVATNOT_E
042900100901     C                   write     FIVAT000
043000100901     C                   add       1             §CTROKVT
043100060331     C                   endif
043200040119     C*
043300040119     C                   ENDSR
043400040119
043500040119
043600040119
043700040119     C*----------------------------------------------------*
043800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043900040119     C*----------------------------------------------------*
044000040119     C     PREVAT        BEGSR
044100040119     C*
044200040119     C* Compongo il nome del membro da dare al FIVATWWR
044300040119     C                   eval      parmbr = vlrhdl
044400040119     C                   movel     'M'           parmbr
044500040510     C                   eval      parccm = vlrksc
044600040119     C                   eval      paropz = '1'
044700040119     C* Effettuo la chiamata al CLLE preposto
044800040119     C                   call(e)   'TITVVTC'
044900040119     C                   parm                    parccm
045000040119     C                   parm                    parmbr
045100040119     C                   parm                    paropz
045200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
045300040119     C                   if        %error
045400040119     C                   movel     '1'           chkcall
045500040119     C                   else
045600040119     C                   movel     '0'           chkcall
045700040119     C                   endif
045800040119     C*
045900040119     C                   ENDSR
046000010601
046100010601
046200010601
046300010330     C*----------------------------------------------------*
046400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046500010330     C*----------------------------------------------------*
046600040119     C     INZVAR        BEGSR
046700080527     C*
046800080527     C                   setoff                                       65
046900010330     C*
047000030822     C* Inizializzo variabili di wrk
047100020204     C                   Z-ADD     *zeros        Num5_0            5 0
047200120215     C                   clear                   wFlgCAS
047300060331     C                   MOVEL     *blanks       wVATNOT_A
047400060331     C                   MOVEL     *blanks       wVATNOT_B
047500060331     C                   MOVEL     *blanks       wVATNOT_E
047600111117     C                   MOVEL     *blanks       wVATNOT_I
047700111117     C                   MOVEL     *blanks       wVATNOT_J
047800100831     C                   MOVEL     *blanks       wVABRSD
047900100831     C                   MOVEL     *blanks       wVABIND
048000180201     C                   CLEAR                   wVABCAD
048100100831     C                   MOVEL     *blanks       wVABLOD
048200100831     C                   MOVEL     *blanks       wVABPRD
048300120104     C                   MOVEL     *blanks       wVABNZD
048400100910     C                   MOVEL     *blanks       wRSD             70
048500111117     C                   MOVEL     *blanks       wVATNOT_IJ       70
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
080100030515     C*
080200010607     C                   MOVEL     datcor        VABAAS
080300010607     C                   MOVE      datcor        VABMGS
080400010607     C*
080500030715     C**********
080600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080700030715     C**********
080800030715     C                   IF        wGiro = *zeros
080900040113     C*
081000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
081300080429     C                             %trim(intcol(5))<>*blanks
081400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
081600080429     C                                      %trim(intcol(5))
081700070823     C     ';':CharCSV   XLATE     vindta        vindta
081800040510     C                   ENDIF
081900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
082000031201     C     minu:maiu     XLATE     vindta        vindta
082100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
082200030715     C                   DOW       posDa <= %len(%trim(vindta))
082300030715     C*
082400030715     C* Gestisco il 1° campo
082500030715     C                   IF        i = 1
082600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082700030715     C                   EVAL      posDa = 1
082800030715     C* Eseguo lo scan x trovare la fine del primo campo
082900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083000030715     C* A questo punto "estraggo" il campo corrente
083100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
083200030715     C                                                    (posA-posDa))
083300030715     C* X i campi successivi al 1°
083400030715     C                   ELSE
083500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083600030715     C                   EVAL      posDa = posA + 1
083700030715     C* Eseguo lo scan x trovare la fine del campo corrente
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* Gestisco l'ultimo campo
084000030715     C                   IF        posA = *zeros
084100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   LEAVE
084600030715     C                   ELSE
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C                   ENDIF
085100030715     C                   ENDIF
085200030715     C* Incremento il contatore d campo
085300030715     C                   EVAL      i = i +1
085400030715     C                   ENDDO
085500030715     C*
085600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085700030715     C* delimitazione testo
085800030715     C                   EVAL      i = 1
085900030715     C                   DOW       i < %elem(SkSplitFLD)
086000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
086100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
086200030715     C                   EVAL      i = i + 1
086300030715     C                   ENDDO
086400030715     C                   EVAL      wGiro = 1
086500030715     C                   ELSE
086600030715     C                   EVAL      wGiro = 2
086700031204     C**********
086800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086900031204     C* d separatore campo
087000031204     C**********
087100031204     C                   EVAL      vindta = %trim(vindta)
087200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087300031204     C                   EVAL      vindta = %subst(vindta:2)
087400031204     C                   ENDDO
087500031204     C*
087600031204     C                   Z-ADD     *zeros        lunghInput        4 0
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
088000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
088100031204     C                   ENDDO
088200030715     C**********
088300030715     C* Effettuo lo split del campo dati d input
088400030715     C**********
088500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088600030715     C                   DOW       posDa <= %len(%trim(vindta))
088700030715     C*
088800030715     C* Gestisco il 1° campo
088900030715     C                   IF        i = 1
089000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
089100030715     C                   EVAL      posDa = 1
089200030715     C* Eseguo lo scan x trovare la fine del primo campo
089300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089400030715     C* A questo punto "estraggo" il campo corrente
089500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089600030715     C                                                    (posA-posDa))
089700030715     C* X i campi successivi al 1°
089800030715     C                   ELSE
089900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
090000030715     C                   EVAL      posDa = posA + 1
090100031204     C* Verifico che nn vi sia il campo nn valorizzato
090200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090300031204     C* Se campo nn valorizzato skippo al prossimo
090400031204     C                   EVAL      SkSplitCSV(i) = *blanks
090500031204     C                   EVAL      posA  = posA + 1
090600031204     C                   ELSE
090700030715     C* Eseguo lo scan x trovare la fine del campo corrente
090800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090900030715     C* Gestisco l'ultimo campo
091000030715     C                   IF        posA = *zeros
091100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
091200030715     C* A questo punto "estraggo" il campo corrente
091300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091400030715     C                                                    (posA-posDa))
091500030715     C                   LEAVE
091600030715     C                   ELSE
091700030715     C* A questo punto "estraggo" il campo corrente
091800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091900030715     C                                                    (posA-posDa))
092000030715     C                   ENDIF
092100031204     C                   ENDIF
092200030715     C                   ENDIF
092300030715     C* Incremento il contatore d campo
092400030715     C                   EVAL      i = i +1
092500030715     C                   ENDDO
092600030715     C*
092700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092800030715     C* delimitazione testo
092900030715     C                   EVAL      i = 1
093000030715     C                   DOW       i < %elem(SkSplitCSV)
093100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
093200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093300030715     C                   EVAL      i = i + 1
093400030715     C                   ENDDO
093500100720     C*
093600100720     C* Stacco un progressivo x numero spedizione
093700180202 xxx C***                EXSR      repNSP
093800100326     C*
093900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
094000030715     C                   EVAL      i = 1
094100030715     C                   DOW       i < %elem(SkSplitFLD)
094200030715     C*
094300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
094400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
094500030715     C                             SkSplitCSV(i) <> *blanks
094600030721     C*** ATB
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094800030721     C                   EVAL      VABATB = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030715     C*** RSD
095100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
095200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
095300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
095400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
095500030715     C* ==
095600030715     C                   ENDIF
095700030721     C*** RD2
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
096000030721     C                   ENDIF
096100030715     C*** IND
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
096300030715     C                   EVAL      VABIND = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** LOD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** PRD
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
097100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030721     C*** NZD
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
097500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030715     C*** RMA
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030715     C*** NOT
098200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
098300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
098400030715     C                   ENDIF
098500030715     C*** NT2
098600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098800030715     C                   ENDIF
098900030715     C*** VCA
099000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
099100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
099200030715     C                   ENDIF
099300030715     C*** VAS
099400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
099500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099600030715     C                   ENDIF
099700030715     C*** FFD
099800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
100000030715     C                   ENDIF
100100030721     C*** GC1
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
100300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** GC2
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** TSP
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101200030721     C                   ENDIF
101300040510     C*** CBO
101400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101600040510     C                   ENDIF
101700030721     C*** NAS
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
101900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** TIC
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GCA
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** XCO
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** CTM
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** TCR
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
103900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** CTS
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** FTM
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** VAD
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** GMA
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** GGA
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
105900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** GVA
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*** TC1
106600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
106800030721     C                   ENDIF
106900030721     C*** TC2
107000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107200030721     C                   ENDIF
107300030721     C*** SCL
107400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107600030721     C                   ENDIF
107700030721     C*** RMO
107800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
107900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108000030721     C                   ENDIF
108100030721     C*** NMO
108200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108400030721     C                   ENDIF
108500030721     C*
108600030721     C* Reperisco quindi i campi numerici...
108700040510     C*** NSP
108800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
108900040510     C                   EVAL      PiStr=SkSplitCSV(i)
109000040510     C                   EXSR      CHKNUM
109100040510     C                   IF        PiInt=*on
109200040510     C                   Z-ADD     PiVal         VABNSP
109300040510     C                   ELSE
109400040510     C                   SETON                                        31
109500040510     C                   EVAL      VABNSP = *zeros
109600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109700040510     C                             + ' ' + 'VABNSP'
109800040510     C                   ENDIF
109900040510     C                   ENDIF
110000071106     C*** RMN
110100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110200071106     C                   EVAL      PiStr=SkSplitCSV(i)
110300071106     C                   EXSR      CHKNUM
110400071106     C                   IF        PiInt=*on
110500071106     C                   Z-ADD     PiVal         VABRMN
110600180202     C                   Z-ADD     PiVal         VABNSP
110700071106     C                   ELSE
110800071106     C                   SETON                                        32
110900071106     C                   EVAL      VABRMN = *zeros
111000071106     C                   EVAL      vinmsg = %trimr(vinmsg)
111100180202     C                             + ' ' + 'VABRMN VABNSP'
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
115600120210     C                   IF        %trim(SkSplitCSV(i)) <> *blanks AND
115700170726     C                             %trim(SkSplitCSV(i)) <> *zeros  AND
115800170726     C                             %trim(SkSplitCSV(i)) <> '0,000' AND
115900170726     C                             %trim(SkSplitCSV(i)) <> '0,00'  AND
116000170726     C                             %trim(SkSplitCSV(i)) <> '0.000' AND
116100170726     C                             %trim(SkSplitCSV(i)) <> '0.00'  AND
116200170726     C                             %trim(SkSplitCSV(i)) <> '0'
116300120210     C                   EVAL      wFlgCAS = '1'
116400120210     C                   ENDIF
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiNum=*on
116800030721     C                   Z-ADD     PiVal         VABCAS
116900120210     C                   EVAL      VABVCA = 'EUR'
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      VABCAS = *zeros
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABCAS'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** CCM
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiInt=*on      AND
118200030721     C                             PiVal<=9999999 AND
118300030721     C                             PiVal>*zeros
118400030721     C                   Z-ADD     PiVal         VABCCM
118500030721     C                   ELSE
118600030721     C                   SETON                                        32
118700030721     C                   EVAL      VABCCM = *zeros
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900040510     C                             + ' ' + 'VABCCM'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** LNP
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiInt=*on    AND
119700030721     C                             PiVal<=999   AND
119800030721     C                             PiVal>*zeros
119900030721     C                   Z-ADD     PiVal         VABLNP
120000030721     C                   ELSE
120100040715     C                   SETON                                        32
120200030721     C                   EVAL      VABLNP = *zeros
120300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120400040510     C                             + ' ' + 'VABLNP'
120500030721     C                   ENDIF
120600030721     C                   ENDIF
120700030721     C*** NRS
120800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
120900030721     C                   EVAL      PiStr=SkSplitCSV(i)
121000030721     C                   EXSR      CHKNUM
121100030721     C                   IF        PiInt=*on AND
121200030721     C                             PiVal<=99
121300030721     C                   Z-ADD     PiVal         VABNRS
121400030721     C                   ELSE
121500030721     C                   SETON                                        32
121600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121700030721     C                             + ' ' + 'VABNRS'
121800030721     C                   ENDIF
121900030721     C                   ENDIF
122000030721     C*** LNA
122100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
122200030721     C                   EVAL      PiStr=SkSplitCSV(i)
122300030721     C                   EXSR      CHKNUM
122400030721     C                   IF        PiInt=*on    AND
122500030721     C                             PiVal<=999   AND
122600030721     C                             PiVal>*zeros
122700030721     C                   Z-ADD     PiVal         VABLNA
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      VABLNA = *zeros
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABLNA'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** CTR
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on  AND
124000030721     C                             PiVal<=999
124100030721     C                   Z-ADD     PiVal         VABCTR
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABCTR'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** IAS
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiNum=*on
125300030721     C                   Z-ADD     PiVal         VABIAS
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABIAS'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** VLB
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiNum=*on
126500030721     C                   Z-ADD     PiVal         VABVLB
126600030721     C                   ELSE
126700030721     C                   SETON                                        32
126800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126900030721     C                             + ' ' + 'VABVLB'
127000030721     C                   ENDIF
127100030721     C                   ENDIF
127200030721     C*** QFT
127300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
127400030721     C                   EVAL      PiStr=SkSplitCSV(i)
127500030721     C                   EXSR      CHKNUM
127600030721     C                   IF        PiNum=*on
127700030721     C                   Z-ADD     PiVal         VABQFT
127800030721     C                   ELSE
127900030721     C                   SETON                                        32
128000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128100030721     C                             + ' ' + 'VABQFT'
128200030721     C                   ENDIF
128300030721     C                   ENDIF
128400030721     C*** NCD
128500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
128600030721     C                   EVAL      PiStr=SkSplitCSV(i)
128700030721     C                   EXSR      CHKNUM
128800030721     C                   IF        PiInt=*on      AND
128900030721     C                             PiVal<=9999999
129000030721     C                   Z-ADD     PiVal         VABNCD
129100030721     C                   ELSE
129200030721     C                   SETON                                        32
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABNCD'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700030721     C*** NCA
129800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
129900030721     C                   EVAL      PiStr=SkSplitCSV(i)
130000030721     C                   EXSR      CHKNUM
130100030721     C                   IF        PiInt=*on      AND
130200030721     C                             PiVal<=9999999
130300030721     C                   Z-ADD     PiVal         VABNCA
130400030721     C                   ELSE
130500030721     C                   SETON                                        32
130600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130700030721     C                             + ' ' + 'VABNCA'
130800030721     C                   ENDIF
130900030721     C                   ENDIF
131000030721     C*** ZNC
131100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
131200030721     C                   EVAL      PiStr=SkSplitCSV(i)
131300030721     C                   EXSR      CHKNUM
131400030721     C                   IF        PiInt=*on AND
131500030721     C                             PiVal<=99
131600030721     C                   Z-ADD     PiVal         VABZNC
131700030721     C                   ELSE
131800030721     C                   SETON                                        32
131900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132000030721     C                             + ' ' + 'VABZNC'
132100030721     C                   ENDIF
132200030721     C                   ENDIF
132300030721     C*** DCR
132400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
132500120210     C                   EVAL      PiStr=%subst(%trim(SkSplitCSV(i)):7:4)+
132600120210     C                                   %subst(%trim(SkSplitCSV(i)):4:2)+
132700120210     C                                   %subst(%trim(SkSplitCSV(i)):1:2)
132800030721     C                   EXSR      CHKNUM
132900030721     C                   IF        PiInt=*on       AND
133000030721     C                             PiVal<=99999999
133100030721     C                   Z-ADD     PiVal         VABDCR
133200030721     C                   ELSE
133300030721     C                   SETON                                        32
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABDCR'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800030721     C*** HCR
133900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
134000030721     C                   EVAL      PiStr=SkSplitCSV(i)
134100030721     C                   EXSR      CHKNUM
134200030721     C                   IF        PiInt=*on   AND
134300030721     C                             PiVal<=9999
134400030721     C                   Z-ADD     PiVal         VABHCR
134500030721     C                   ELSE
134600030721     C                   SETON                                        32
134700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134800030721     C                             + ' ' + 'VABHCR'
134900030721     C                   ENDIF
135000030721     C                   ENDIF
135100030721     C*** VMD
135200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
135300030721     C                   EVAL      PiStr=SkSplitCSV(i)
135400030721     C                   EXSR      CHKNUM
135500030721     C                   IF        PiNum=*on
135600030721     C                   Z-ADD     PiVal         VABVMD
135700030721     C                   ELSE
135800030721     C                   SETON                                        32
135900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136000030721     C                             + ' ' + 'VABVMD'
136100030721     C                   ENDIF
136200030721     C                   ENDIF
136300030721     C*** ANT
136400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
136500030721     C                   EVAL      PiStr=SkSplitCSV(i)
136600030721     C                   EXSR      CHKNUM
136700030721     C                   IF        PiInt=*on        AND
136800030721     C                             PiVal<=999999999
136900030721     C                   Z-ADD     PiVal         VABANT
137000030721     C                   ELSE
137100030721     C                   SETON                                        32
137200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
137300030721     C                             + ' ' + 'VABANT'
137400030721     C                   ENDIF
137500030721     C                   ENDIF
137600030721     C*** CMO
137700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
137800030721     C                   EVAL      PiStr=SkSplitCSV(i)
137900030721     C                   EXSR      CHKNUM
138000030721     C                   IF        PiInt=*on
138100030721     C                   Z-ADD     PiVal         Num5_0
138200030721     C                   MOVEL(p)  Num5_0        VABCMO
138300030721     C                   ELSE
138400030721     C                   SETON                                        32
138500030721     C                   EVAL      VABCMO = *zeros
138600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
138700030721     C                             + ' ' + 'VABCMO'
138800030721     C                   ENDIF
138900030721     C                   ENDIF
139000040510     C*
139100040510     C* Reperisco i campi del FIVAT
139200040510     C*** VATNOT
139300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
139400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
139500040510     C                   ENDIF
139600040510     C*** VATTRC
139700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
139800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
139900040510     C                   ENDIF
140000060331     C*** VATNOT_A
140100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
140200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
140300060331     C                   ENDIF
140400060331     C*** VATNOT_B
140500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
140600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
140700060331     C                   ENDIF
140800060331     C*** VATNOT_E
140900070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141100060331     C                   ENDIF
141200111117     C*** VATNOT_I
141300111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
141400111117     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
141500111117     C                   ENDIF
141600111117     C*** VATNOT_J
141700111117     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
141800111117     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
141900111117     C                   ENDIF
142000070823     C*
142100100831     C***  ===>  Gestione campi "particolari" dentro ciclo
142200120104     C*** RMN / NSP
142300180131     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
142400120104     C                   EVAL      PiStr=SkSplitCSV(i)
142500120104     C                   EXSR      CHKNUM
142600100413     C                   IF        PiInt=*on
142700120104     C                   Z-ADD     PiVal         VABRMN
142800180131     C***                Z-ADD     PiVal         VABNSP
142900100413     C                   ELSE
143000100720     C                   Z-ADD     *zeros        VABRMN
143100100413     C                   SETON                                        32
143200100413     C                   EVAL      vinmsg = %trimr(vinmsg)
143300180131     C                             + ' ' + 'VABRMN'
143400180131     C***                          + ' ' + 'VABNSP VABRMN'
143500100413     C                   ENDIF
143600100326     C                   ENDIF
143700100831     C*** RSD
143800100831     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
143900100910     C                   EVAL      wRSD    = %trim(SkSplitCSV(i))
144000100831     C                   ENDIF
144100120104     C*** LOD
144200180201     C* se in questo campone non c'è il CAP (che essendo numerico è l'unico che può dare errore)
144300180201     C* metto il tutto in VABLOD
144400120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
144500180201     C                   MONITOR
144600120104     C                   EVAL      wVABCAD =
144700180201     C                             %dec(%subst(%trim(SkSplitCSV(i)):1:5):5:0)
144800120104     C                   EVAL      wVABLOD =
144900120215     C                             %subst(%trim(SkSplitCSV(i)):6+1:
145000120216     C                             %len(%trim(SkSplitCSV(i)))-3-5)
145100120104     C                   EVAL      wVABPRD =
145200120104     C                             %subst(%trim(SkSplitCSV(i)):
145300120104     C                             %len(%trim(SkSplitCSV(i)))-1:2)
145400180201     C                   ON-ERROR
145500180201     C                   EVAL      wVABLOD =
145600180201     C                             (%trim(SkSplitCSV(i)))
145700180201     C                   ENDMON
145800120104     C                   ENDIF
145900120104     C*** NZD
146000120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
146100120104     C                   EVAL      wVABNZD = %trim(SkSplitCSV(i))
146200120104     c                   IF        wVABNZD = 'ITA'
146300120104     c                   clear                   wVABNZD
146400120104     c                   ENDIF
146500120104     C                   ENDIF
146600120210     C*** NOT/NT2
146700120210     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT'
146800120210     C                   MOVEL     *blanks       wNOT             60
146900120210     C                   EVAL      wNOT=SkSplitCSV(i)
147000120210     C                   EVAL      VABNOT=%subst(wNOT:1:35)
147100120210     C                   EVAL      VABNT2=%subst(wNOT:36:25)
147200120210     C                   ENDIF
147300120104     C*** NOT_E
147400120104     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_E'
147500120104     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
147600120104     C                   ENDIF
147700111117     C*** NOT_I/NOT_J
147800111117     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_I'
147900111117     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
148000111117     C                   ENDIF
148100070925     C*
148200071029     C***  <===  -----------------------------------------
148300040510     C*
148400030715     C                   ENDIF
148500030715     C                   EVAL      i = i + 1
148600030715     C                   ENDDO
148700070925     C*
148800071029     C***  ===>  Gestione campi "particolari" fuori ciclo
148900100831     C*
149000100831     C                   IF        wVABRSD <> *blanks
149100100831     C                   EVAL      VABRSD = wVABRSD
149200100831     C                   ENDIF
149300100831     C*
149400100831     C                   IF        wVABIND <> *blanks
149500100831     C                   EVAL      VABIND = wVABIND
149600100831     C                   ENDIF
149700100831     C*
149800180201     C                   IF        wVABCAD <> 0
149900180201     C                   EVAL      VABCAD = %editc(wVABCAD:'X')
150000100831     C                   ENDIF
150100100831     C*
150200100831     C                   IF        wVABLOD <> *blanks
150300100831     C                   EVAL      VABLOD = wVABLOD
150400100831     C                   ENDIF
150500100831     C*
150600100831     C                   IF        wVABPRD <> *blanks
150700100831     C                   EVAL      VABPRD = wVABPRD
150800100831     C                   ENDIF
150900120104     C*
151000120104     C                   IF        wVABNZD <> *blanks
151100120104     C                   EVAL      VABNZD = wVABNZD
151200120104     C                   ENDIF
151300100910     C*
151400100910     C                   IF        %trim(wRSD) <> *blanks
151500100910     C                   EVAL      VABRSD = %subst(wRSD:1:35)
151600100910     C                   EVAL      VABRD2 = %subst(wRSD:36:35)
151700100910     C                   ENDIF
151800111117     C*
151900111117     C                   IF        %trim(wVATNOT_IJ) <> *blanks
152000111117     C                   EVAL      wVATNOT_I = %subst(wVATNOT_IJ:1:35)
152100111117     C                   EVAL      wVATNOT_J = %subst(wVATNOT_IJ:36:35)
152200111117     C                   ENDIF
152300070925     C*
152400071029     C***  <===  -----------------------------------------
152500070925     C*
152600070925     C* Considerazioni finali su CBO/CAS
152700100318     C                   IF        wFlgCAS = '1'    OR
152800100318     C                             VABCAS  > *zeros
152900070925     C                   IF        VABCBO = '1'
153000070925     C                   EVAL      VABCBO = '4'
153100070925     C                   ENDIF
153200070925     C                   IF        VABCBO = '2'
153300070925     C                   EVAL      VABCBO = '6'
153400070925     C                   ENDIF
153500070925     C                   ENDIF
153600070925     C***  <===  ----------------------------
153700040510     C*
153800040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
153900040510     C                   IF        VABNSP = *zeros
154000040510     C                   SETON                                        31
154100040510     C                   ENDIF
154200040715     C*
154300040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
154400040715     C                   IF        VABLNP = *zeros
154500040715     C                   SETON                                        31
154600040715     C                   ENDIF
154700040510     C*
154800040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
154900040510     C                   IF        VABRMN = *zeros
155000040510     C                   Z-ADD     VABNSP        VABRMN
155100040510     C                   ENDIF
155200040510     C*
155300040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
155400040510     C                   IF        VABRMA = *blanks
155500040714     C                   MOVEL     VABRMN        VABRMA
155600040510     C                   ENDIF
155700040128     C*
155800040510     C* Se provincia nn valorizzata la reperisco
155900040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
156000040510     C                   IF        VABPRD = *blanks
156100040128     C                   CLEAR                   TISI95DS
156200040128     C                   EVAL      I95TCN = '3'
156300040128     C                   Z-ADD     datcor        I95DAT
156400040128     C                   EVAL      I95NAR = VABNZD
156500040128     C                   EVAL      I95CAP = VABCAD
156600040128     C                   EVAL      I95LOC = VABLOD
156700040128     C                   CALL      'TISI95R'
156800040128     C                   PARM                    TISI95DS
156900040128     C                   EVAL      VABPRD = O95PRV
157000040510     C                   ENDIF
157100020204     C*
157200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
157300020204     C                   EXSR      CHKIMPDIV
157400030715     C                   ENDIF
157500020204     C*
157600010607     C                   ENDSR
157700010607     C*----------------------------------------------------*
157800010601
157900020204
158000020204     C*----------------------------------------------------*
158100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
158200020204     C*----------------------------------------------------*
158300020204     C     CHKIMPDIV     BEGSR
158400020204     C*
158500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
158600020204     C                   Z-ADD     *zeros        wrkDec            9 9
158700020204     C*
158800020204     C* Come prima cosa effettuo considerazioni sulla divisa
158900020204     C                   IF        vabIAS > *zeros
159000020204     C                   IF        vabVAS <> 'EUR'
159100020204     C                   EVAL      vabVAS =  'ITL'
159200020204     C                   ENDIF
159300020204     C                   ENDIF
159400020204     C*
159500020204     C                   IF        vabCAS > *zeros
159600020204     C                   IF        vabVCA <> 'EUR'
159700020204     C                   EVAL      vabVCA =  'ITL'
159800020204     C                   ENDIF
159900020204     C                   ENDIF
160000020204     C*
160100020204     C                   IF        vabVMD > *zeros
160200020204     C                   IF        vabVAD <> 'EUR'
160300020204     C                   EVAL      vabVAD =  'ITL'
160400020204     C                   ENDIF
160500020204     C                   ENDIF
160600020204     C*
160700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
160800020204     C                   Z-ADD     vabIAS        wrkDec
160900020204     C                   IF        wrkDec > *zeros
161000020204     C                   IF        vabVAS = 'ITL'
161100020204     C                   EVAL      vabIAS = *zeros
161200020204     C                   ENDIF
161300020204     C                   ENDIF
161400020204     C*
161500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
161600020204     C                   Z-ADD     vabCAS        wrkDec
161700020204     C                   IF        wrkDec > *zeros
161800020204     C                   IF        vabVCA = 'ITL'
161900020204     C                   EVAL      vabCAS = *zeros
162000020204     C                   ENDIF
162100020204     C                   ENDIF
162200020204     C*
162300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
162400020204     C                   Z-ADD     vabVMD        wrkDec
162500020204     C                   IF        wrkDec > *zeros
162600020204     C                   IF        vabVAD = 'ITL'
162700020204     C                   EVAL      vabVMD = *zeros
162800020204     C                   ENDIF
162900020204     C                   ENDIF
163000020204     C*
163100020204     C                   ENDSR
163200020204     C***
163300010330
163400100318
163500100318
163600100318     C*----------------------------------------------------*
163700100318     C*  CONTROLLO NUMERICITA' CAMPI
163800100318     C*----------------------------------------------------*
163900100318     C     CHKNUM        BEGSR
164000100318     C*
164100100318     C                   IF        PiDecChr = *blanks
164200100318     C                   EVAL      PiDecChr = CharNUM
164300100318     C                   ENDIF
164400100318     C*
164500100318     C                   callp     UBISNUM_Check(PiStr
164600100318     C                                          :PiDecChr
164700100318     C                                          :PiVal
164800100318     C                                          :PiNum
164900100318     C                                          :PiInt)
165000100318     C*
165100100318     C                   ENDSR
165200100318     C***
165300010601
165400010601
165500010601
165600010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
165700010601     C     repfil        BEGSR
165800010601     C*
165900010601     C                   if        invfil = *zeros and
166000010601     C                             depfil > *zeros and
166100010629     C                             (vinflg = *blanks or
166200010629     C                              vinflg = *zeros)
166300010601     C
166400010601     C                   eval      invfil = depfil
166500010601     C                   endif
166600010601     C*
166700010601     C                   if        depfil <> invfil and
166800010601     C                             invfil > *zeros
166900010601     C                   eval      flgMulti = '1'
167000010601     C                   if        vinflg = *blanks
167100010601     C                   add       1             cntNonEl
167200010601     C                   endif
167300010601     C                   endif
167400010601     C*
167500010601     C                   if        vinflg = '2'
167600010601     C                   eval      flgStato = '2'
167700010601     C                   endif
167800010601     C*
167900010601     C                   ENDSR
168000010601     C***
168100010601
168200010601
168300010601
168400010330
168500040119      /TITLE Invio dei dati al punto operativo.
168600040119     C     invio         BEGSR
168700040119     C*
168800040119     C* 1° invio FIVAT
168900040119     C                   reset                   dscmz
169000040119     C                   move      vlrpoi        cmzdst
169100040119     C                   eval      cmzfld = 'FIVATWWR'
169200040119     C                   eval      cmzmbd = vlrhdl
169300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
169400040119     C***                if        prmfir = *blanks
169500040119     C                   eval      cmzfla = 'FIVAT00F'
169600040119     C                   eval      cmzmba = 'FIVAT00F'
169700040119     C***                else
169800040119     C***                eval      cmzfla = prmfir
169900040119     C***                eval      cmzmba = prmfir
170000040119     C***                endif
170100040119     C                   eval      cmznrr = *zeros
170200040119     C                   move      §ctrokvt      cmznrr
170300040119     C                   eval      cmzlba = vlrfl1
170400040119     C                   call(e)   'TIS711C'
170500040119     C                   parm                    dscmz
170600040119     C                   parm      *blanks       esito
170700040119     C                   if        %error
170800040119     C                             or cmzerr = '1'
170900040119     C                             or esito  = '1'
171000040119     C                   eval      wrkesito = '3'
171100040119     C                   else
171200040119     C*
171300040119     C* 2° invio FIVAB
171400040119     C                   reset                   dscmz
171500040119     C                   move      vlrpoi        cmzdst
171600040119     C                   eval      cmzfld = vlrfou
171700040119     C                   eval      cmzmbd = vlrhdl
171800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
171900040119     C***                if        prmfir = *blanks
172000040119     C                   eval      cmzfla = 'FIVAB00F'
172100040119     C                   eval      cmzmba = 'FIVAB00F'
172200040119     C***                else
172300040119     C***                eval      cmzfla = prmfir
172400040119     C***                eval      cmzmba = prmfir
172500040119     C***                endif
172600040119     C                   eval      cmznrr = *zeros
172700040119     C                   move      §ctrokvb      cmznrr
172800040119     C                   eval      cmzlba = vlrfl1
172900040119     C                   call(e)   'TIS711C'
173000040119     C                   parm                    dscmz
173100040119     C                   parm      *blanks       esito
173200040119     C                   if        %error
173300040119     C                             or cmzerr = '1'
173400040119     C                             or esito  = '1'
173500040119     C                   eval      wrkesito = '3'
173600040119     C                   endif
173700040119     C                   endif
173800040119     C*
173900040119     C                   ENDSR
174000040119     C***
174100010601
174200010601
174300010601
174400010601
174500010601      /TITLE Invio dei dati al punto operativo.
174600010601     C     opeini        BEGSR
174700010601     C*
174800010601     C* Inizializzo flag e contatori operativi
174900010601     C                   movel     '0'           flgGiro           1
175000010601     C                   movel     '0'           flgMulti          1
175100010601     C                   movel     '1'           flgStato          1
175200010615     C                   movel     '0'           flgOk             1
175300010601     C                   z-add     *zeros        cntNonEl         10 0
175400010601     C                   z-add     *zeros        depfil            3 0
175500010601     C                   z-add     *zeros        invfil            3 0
175600010601     C*
175700010601     C                   ENDSR
175800010601     C***
175900070326
176000070326
176100070326
176200070326
176300070326     C     *pssr         BEGSR
176400070329     C*
176500070329     C                   if        %open(tivin00r)
176600070329     C                   close     tivin00r
176700070329     C                   endif
176800070329     C                   if        %open(fivabwwr)
176900070329     C                   close     fivabwwr
177000070329     C                   endif
177100070329     C                   if        %open(fivatwwr)
177200070329     C                   close     fivatwwr
177300070329     C                   endif
177400070326     C*
177500070326     C* Effettuo la chiamata al CLLE preposto
177600070326     C                   call(e)   'TITVVTC'
177700070326     C                   parm                    parccm
177800070326     C                   parm                    parmbr
177900070326     C                   parm      '2'           paropz
178000070326     C*
178100070326     C                   eval      wrkesito = '2'
178200070404     C*
178300070404     C                   seton                                        LR
178400070326     C*
178500070326     C                   ENDSR     '*CANCL'
178600070326     C***
178700100318
178800100318
178900100318
179000100318     C     normCSV       BEGSR
179100100318     C*
179200100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
179300100318     C* separatori campi
179400100318     C                   if        CharTXT <> *blanks
179500100318     C                   z-add     *zeros        wLenVINDTA        4 0
179600100318     C                   z-add     *zeros        wFlgVINDTA        1 0
179700100318     C                   setoff                                       80
179800100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
179900100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
180000100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
180100100318     C                   if        not *in80
180200100318     C                   seton                                        80
180300100318     C                   else
180400100318     C                   setoff                                       80
180500100318     C                   endif
180600100318     C                   endif
180700100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
180800100318     C                             and *in80 = *on
180900100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
181000100318     C                   endif
181100100318     C                   enddo
181200100318     C                   endif
181300100318     C*
181400100318     C                   ENDSR
181500100318     C***
181600070326
181700100326
181800100326
181900100326     C*----------------------------------------------------*
182000100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
182100100326     C*----------------------------------------------------*
182200100326     C     REPNSP        BEGSR
182300100326     C*
182400100326     C* NSP => Stacco un numeratore da AZNUM
182500100326     C                   clear                   TRUL33DS
182600100326     C                   eval      I33OPE = *zeros
182700100326     C                   eval      I33CNU = 302
182800100326     C                   eval      I33NUM = 1
182900100326     C                   movel     TRUL33DS      KPJBU
183000100326     C                   call      'TRUL33R'
183100100326     C                   parm                    KPJBA
183200100326     C                   movel     KPJBU         TRUL33DS
183300100326     C                   if        O33ERR = *zeros
183400100326     C                   z-add     O33NRF        VABNSP
183500100326     C                   z-add     O33NRF        VATNSP
183600100326     C                   else
183700100326     C                   SETON                                        31
183800100326     C                   EVAL      vinmsg = %trimr(vinmsg)
183900100326     C                             + ' ' + 'VABNSP VATNSP'
184000100326     C                   endif
184100100326     C*
184200100326     C                   ENDSR
184300010330
184400100326
184500010330
184600000613     C     *inzsr        BEGSR
184700990910     C*
184800990910     C     *entry        plist
184900990920     C                   parm                    tivlrds
185000990921     C                   parm      wrkesito      esito
185100000724     C                   parm                    prmlit
185200000710     C                   parm                    prmfir
185300010330     C*
185400010330     C* CALCOLA LA DATA CORRENTE
185500100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
185600100318     C                   eval      datcor = %dec(%date() : *ISO)
185700000613     C*
185800000613     C                   ENDSR
185900000613     C***
186000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
186100180131X;VABRMN;X;X;X;X;X;X;VABRSD;
186200180131VABIND;X;W_VABLOD;W_VABNZD;X;VABPKB;VABVLB;X;VABNCL;
186300180131W_VATNOT_E;VABCAS;X_VABNOT;VABDCR;X;VABRMA;X;W_VABNOT;
186400100910
186500100318
