000100120306      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200100318     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500120306     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600120306     FEDIVATwr  O    E             DISK    usropn
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
002000120306     D edivabs       e ds                  extname(edivab0f)
002100120306     D edivabs_sav   e ds                  extname(edivab0f) prefix(sav_)
002200120306     D edivabs_ok    e ds                  extname(edivab0f) prefix(ok_)
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
005900030822     D*------------------
006000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
006100030822     D*------------------
006200030822     D posDaDft        S              3  0 INZ(*zeros)
006300030822     D posADft         S              3  0 INZ(*zeros)
006400030822     D j               s              3  0 INZ(1)
006500030822     D wGiroDft        s              1  0 INZ(*zeros)
006600080418     D*------------------
006700080418     D* Costanti
006800080418     D*------------------
006900080418     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007000080418     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007100100318
007200100318     D*------------------
007300100318     D* LINKING A DEFINIZIONI ESTERNE
007400100318     D*------------------
007500100318     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007600100318     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007700100318
007800010330
007900010330
008000010330
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010601     C*
008500010601     C                   exsr      opeini
008600010605     C                   exsr      rwvab
008700010601     C*
008800010601     C                   seton                                        lr
008900010601
009000010601
009100010601
009200010601     C*--------------------------------------------------------
009300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009400010601     C*--------------------------------------------------------
009500010601     C     PREELA        BEGSR
009600010601     C*
009700010601     C* SE OCCORRE SPEDIRE IN FILIALE
009800010601     C                   if        invfil <> *zeros and
009900010601     C                             flgGiro = '0'
010000010601     C*
010100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010200010601     C                   eval      flgGiro = '1'
010300010601     C*
010400010601     C                   endif
010500010601     C*
010600010601     C                   ENDSR
010700010601     C***
010800010601
010900010601
011000010601
011100010601
011200010601     C*--------------------------------------------------------
011300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011400010601     C*--------------------------------------------------------
011500010601     C     ENDELA        BEGSR
011600040119     C*
011700040119     C* Effettuo la chiamata al CLLE preposto
011800120306     C                   call(e)   'TITVEVTC'
011900040510     C                   parm                    parccm
012000040119     C                   parm                    parmbr
012100040119     C                   parm      '2'           paropz
012200050201     C*
012300050201     C* Effettuo lancio TISI95 solo x chiusura
012400050201     C                   CLEAR                   TISI95DS
012500050201     C                   EVAL      I95TLA = 'C'
012600050201     C                   CALL      'TISI95R'
012700050201     C                   PARM                    TISI95DS
012800000616     C*
012900010601     C                   ENDSR
013000010601     C***
013100000613
013200010601
013300010601
013400010330     C*--------------------------------------------------------
013500120306     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013600010330     C*--------------------------------------------------------
013700010605     C     RWVAB         BEGSR
013800010330     C*
013900010330     C                   if        not %open(tivin00r)
014000010330     C                   open      tivin00r
014100010330     C                   endif
014200120306     C                   if        not %open(edivabwr)
014300120306     C                   open      edivabwr
014400010330     C                   endif
014500040119     C*
014600120306     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014700040119     C                   exsr      prevat
014800040119     C*
014900040119     C                   if        chkcall = '0'
015000040119     C*
015100120306     C                   if        not %open(edivatwr)
015200120306     C                   open      edivatwr
015300040119     C                   endif
015400010330     C*
015500040119     C                   clear                   §CTROKVB          7 0
015600040119     C                   clear                   §CTROKVT          7 0
015700040119     C                   clear                   §CTRMO            7 0
015800040119     C                   clear                   §CTRNO            7 0
015900070823     C*
016000070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
016100070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
016200100318     C***                exsr      inzvar
016300100318     C***                exsr      impvab
016400010330     C*
016500030822     C                   do        *HIVAL
016600010330     C*
016700030822     C                   read      tivin00r                               70
016800120306     C*
016900120306 xxx C                   seton                                        65
017000120306     C*
017100010618     C*
017200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017300010618     C                   if        vinflg = '1'
017400010618     C                   eval      flgOk = '1'
017500010618     C                   endif
017600040510     C*
017700040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
017800040510     C                   z-add     *zeros        wLenVINDTA        4 0
017900040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018000040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018100040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018200040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018300040510     C                   z-add     1             wFlgVINDTA
018400040510     C                   leave
018500040510     C                   endif
018600040510     C                   enddo
018700010618     C*
018800040510     C                   if        vindta > *blanks AND
018900040510     C                             wFlgVINDTA = 1
019000010330     C                   add       1             rrnum
019100010330     C*
019200010601     C                   if        *in70 = *off and
019300010330     C                             (vinflg = *blanks
019400010330     C                              or vinflg = '0'
019500010330     C                              or vinflg = '2')
019600010330     C*
019700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
019800010711     C                   if        vinflg = *blanks or vinflg = '0'
019900010711     C                   clear                   vinmsg
020000010711     C                   endif
020100010601     C*
020200070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020300070925     C                   eval      vindta  = ' ' + %trim(vindta)
020400070925     C*
020500080527     C                   exsr      inzvar
020600100318     C                   exsr      normCSV
020700010605     C                   exsr      impvab
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010605     C                   eval      depfil = VABLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300021025     C                   if        vlrpoi = 999
021400030822     C                   move(P)   invfil        VABFGS
021500021025     C                   else
021600030822     C                   move(P)   vlrpoi        VABFGS
021700021025     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300040119     C  N31              add       1             §CTROKVB
022400040119     C   32              add       1             §CTRMO
022500040119     C   31              add       1             §CTRNO
022600080418     C*
022700030822     C                   if        wGiro = 2
022800080513     C* Scrivo sempre il VAT
022900080513     C  N31              exsr      wrivat
023000080527     C                   exsr      chkvab
023100080513     C                   endif
023200010604     C*
023300010604     C                   if        *in31 = *off and
023400010604     C                             *in32 = *off
023500010604     C                   eval      vinflg = '1'
023600010604     C                   else
023700010604     C                   eval      vinflg = '2'
023800010604     C                   endif
023900010604     C                   endif
024000010601     C*
024100010604     C                   endif
024200010604     C*
024300010330     C                   else
024400010330     C                   eval      vinflg = '1'
024500010330     C                   endif
024600010601     C*
024700010601     C  N70              update    tivin000
024800010330     C*
024900030822     C  N70              enddo
025000040119     C*
025100080527     C* Verifico dati rimasti "in cannna"...
025200080527     C                   seton                                        65
025300080527     C                   exsr      chkvab
025400080527     C*
025500040119     C                   endif                                                  (endif - chkcall)
025600010601     C*
025700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025800010601     C                   if        cntNonEl = *zeros or
025900010601     C                             flgMulti = '0'
026000010330     C* Se non ci sono record con errori ...
026100010601     C                   if        §ctrno = 0 and
026200010604     C                             §ctrmo = 0 and
026300010601     C                             flgStato <> '2'
026400010330     C* ... restituisco esito OK.
026500010330     C                   eval      wrkesito = '0'
026600010330     C                   else
026700040119     C                   if        §ctrokvb > 0
026800010330     C                   eval      wrkesito = '1'
026900010330     C                   else
027000010615     C                   if        flgOk = '0'
027100010615     C                   eval      wrkesito = '2'
027200010615     C                   else
027300010615     C                   eval      wrkesito = '6'
027400010615     C                   endif
027500010330     C                   endif
027600010330     C                   endif
027700010601     C                   else
027800010601     C                   eval      wrkesito = '9'
027900010601     C                   endif
028000010330     C*
028100010330     C                   if        %open(tivin00r)
028200010330     C                   close     tivin00r
028300010330     C                   endif
028400120306     C                   if        %open(edivabwr)
028500120306     C                   close     edivabwr
028600010330     C                   endif
028700120306     C                   if        %open(edivatwr)
028800120306     C                   close     edivatwr
028900040512     C                   endif
029000010601     C*
029100010601     C                   if        vlrpoi <> 999
029200010601     C                   eval      invfil = vlrpoi
029300010601     C                   endif
029400010330     C*
029500040119     C                   if        §ctrokvb > 0
029600010601     C                             and invfil > *zeros
029700010330     C                   exsr      invio
029800010330     C                   endif
029900010601     C*
030000010618     C                   if        flgGiro = '1'
030100010601     C                   exsr      endela
030200010618     C                   endif
030300010330     C*
030400010330     C                   ENDSR
030500010330     C***
030600080527
030700080527
030800080527
030900080527     C*----------------------------------------------------*
031000080527     C*  CONSIDERAZIONI SU SCRITTURA BUFFER RECORDS VAB
031100080527     C*----------------------------------------------------*
031200100413     C     CHKVAB        BEGSR
031300080527     C*
031400080527     C* Scrivo il VAB solo a rottura d spedizione
031500080527     C                   if        depspe = *zeros
031600120306     C  N31              eval      edivabs_sav = edivabs
031700080527     C                   eval      depspe = vabNSP
031800080527     C                   else
031900080527     C                   if        depspe = vabNSP AND *in65 = *off
032000100413     C   51              add       VABNCL        sav_VABNCL
032100100413     C   51              add       VABPKB        sav_VABPKB
032200100413     C   51              add       VABVLB        sav_VABVLB
032300100413     C  N51              z-add     VABNCL        sav_VABNCL
032400100413     C  N51              z-add     VABPKB        sav_VABPKB
032500100413     C  N51              z-add     VABVLB        sav_VABVLB
032600080527     C                   else
032700120306     C  N31              eval      edivabs_ok = edivabs_sav
032800080527     C  N31              exsr      wrivab
032900120306     C  N31              eval      edivabs_sav = edivabs
033000080527     C                   eval      depspe = vabNSP
033100080527     C                   endif
033200080527     C                   endif
033300080527     C*
033400080527     C                   ENDSR
033500040119
033600040119
033700040119
033800040119     C*----------------------------------------------------*
033900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034000040119     C*----------------------------------------------------*
034100040510     C     WRIVAB        BEGSR
034200080513     C*
034300080513     C* Gestisco l'eventuale rottura x numero spedizione
034400080527     C                   if        VABNSP <> depspe or *in65 = *on
034500070301     C                   if        wVATNOT_E <> *blanks
034600070301     C                   eval      VABCTM = '7Q'
034700070301     C                   endif
034800120306     C*
034900120306     C* Imposto i dati del CMR
035000120306     C                   eval      ok_VABCMR = 'LOHMANN'
035100120306     C                   eval      ok_VABDCM = datcor
035200120306     C                   eval      ok_VABCNT = 1
035300120306     C*
035400120306     C                   write     edivab00
035500080513     C                   endif
035600040119     C*
035700040119     C                   ENDSR
035800040119
035900040119
036000040119
036100040119     C*----------------------------------------------------*
036200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036300040119     C*----------------------------------------------------*
036400040510     C     WRIVAT        BEGSR
036500060512     C*
036600060512     C* Inizializzo i campi chiave
036700060512     C                   eval      VATFGS = VABFGS
036800060512     C                   eval      VATAAS = VABAAS
036900060512     C                   eval      VATCCM = VABCCM
037000070823     C                   eval      VATNRS = VABNRS
037100060512     C                   eval      VATNSP = VABNSP
037200060512     C                   eval      VATLNP = VABLNP
037300040714     C*
037400040714     C* Solo se VATNOT valorizzato scarico il buffer
037500040714     C                   if        VATNOT <> *blanks
037600040119     C*
037700040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037800040510     C                   if        VATTRC = *blanks
037900040510     C                   eval      VATTRC = 'E'
038000040510     C                   endif
038100120306     C*
038200120306     C* Imposto i dati del CMR
038300120306     C                   eval      VATCMR = 'LOHMANN'
038400120306     C                   eval      VATCNT = 1
038500120306     C*
038600120306     C                   write     EDIVAT00
038700040119     C                   add       1             §CTROKVT
038800040714     C*
038900040714     C                   endif
039000060331     C*
039100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
039200060331     C                   if        wVATNOT_A <> *blanks
039300060331     C                   eval      VATNOT = wVATNOT_A
039400060331     C                   eval      VATTRC = 'A'
039500120306     C*
039600120306     C* Imposto i dati del CMR
039700120306     C                   eval      VATCMR = 'LOHMANN'
039800120306     C                   eval      VATCNT = 1
039900120306     C*
040000120306     C                   write     EDIVAT00
040100060331     C                   add       1             §CTROKVT
040200060331     C                   endif
040300060331     C*
040400060331     C                   if        wVATNOT_B <> *blanks
040500060331     C                   eval      VATNOT = wVATNOT_B
040600060331     C                   eval      VATTRC = 'B'
040700120306     C*
040800120306     C* Imposto i dati del CMR
040900120306     C                   eval      VATCMR = 'LOHMANN'
041000120306     C                   eval      VATCNT = 1
041100120306     C*
041200120306     C                   write     EDIVAT00
041300060331     C                   add       1             §CTROKVT
041400060331     C                   endif
041500060331     C*
041600060331     C                   if        wVATNOT_E <> *blanks
041700060331     C                   eval      VATNOT = wVATNOT_E
041800060331     C                   eval      VATTRC = 'E'
041900120306     C*
042000120306     C* Imposto i dati del CMR
042100120306     C                   eval      VATCMR = 'LOHMANN'
042200120306     C                   eval      VATCNT = 1
042300120306     C*
042400120306     C                   write     EDIVAT00
042500060331     C                   add       1             §CTROKVT
042600060331     C                   endif
042700040119     C*
042800040119     C                   ENDSR
042900040119
043000040119
043100040119
043200040119     C*----------------------------------------------------*
043300120306     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
043400040119     C*----------------------------------------------------*
043500040119     C     PREVAT        BEGSR
043600040119     C*
043700120306     C* Compongo il nome del membro da dare al EDIVATWR
043800040119     C                   eval      parmbr = vlrhdl
043900040119     C                   movel     'M'           parmbr
044000040510     C                   eval      parccm = vlrksc
044100040119     C                   eval      paropz = '1'
044200040119     C* Effettuo la chiamata al CLLE preposto
044300120306     C                   call(e)   'TITVEVTC'
044400040119     C                   parm                    parccm
044500040119     C                   parm                    parmbr
044600040119     C                   parm                    paropz
044700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044800040119     C                   if        %error
044900040119     C                   movel     '1'           chkcall
045000040119     C                   else
045100040119     C                   movel     '0'           chkcall
045200040119     C                   endif
045300040119     C*
045400040119     C                   ENDSR
045500010601
045600010601
045700010601
045800010330     C*----------------------------------------------------*
045900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
046000010330     C*----------------------------------------------------*
046100040119     C     INZVAR        BEGSR
046200080527     C*
046300080527     C                   setoff                                       65
046400010330     C*
046500030822     C* Inizializzo variabili di wrk
046600020204     C                   Z-ADD     *zeros        Num5_0            5 0
046700060331     C                   MOVEL     *blanks       wVATNOT_A
046800060331     C                   MOVEL     *blanks       wVATNOT_B
046900060331     C                   MOVEL     *blanks       wVATNOT_E
047000100824     C                   MOVEL     *blanks       wNOTE            80
047100030822     C*
047200030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
047300030822     C                   CLEAR                   SkSplitCSV
047400030822     C                   EVAL      i = 1
047500030822     C                   EVAL      posDa = *zeros
047600030822     C                   EVAL      posA  = *zeros
047700080527     C*
047800120306     C                   CLEAR                   EDIVABS
047900120306     C                   CLEAR                   EDIVAT00
048000030822     C*
048100030822     C* Reimposto i valori di default
048200030822     C                   EXSR      DEFCAM
048300030822     C*
048400010330     C                   ENDSR
048500010330     C*----------------------------------------------------*
048600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048700010330     C*----------------------------------------------------*
048800010330     C     DEFCAM        BEGSR
048900030822     C*
049000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
049100030715     C* e delimitatore testo.
049200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
049300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
049400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
049500100318     C*
049600100318     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049700100318     C                   EVAL      CharSOS = CharNUM
049800030822     C*
049900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
050000100413     C                   SETOFF                                       5051
050100100413     C                   SELECT
050200100413     C                   WHEN      %subst(vlrppt:1:1) = 'S'
050300030715     C                   SETON                                        50
050400100413     C                   WHEN      %subst(vlrppt:1:1) = 'A'
050500100413     C                   SETON                                        51
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
077600120306     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
077700010607     C*----------------------------------------------------*
077800010607     C     IMPVAB        BEGSR
077900010607     C*
078000010607     C                   SETOFF                                       3132
078100030515     C*
078200010607     C                   MOVEL     datcor        VABAAS
078300010607     C                   MOVE      datcor        VABMGS
078400010607     C*
078500030715     C**********
078600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078700030715     C**********
078800030715     C                   IF        wGiro = *zeros
078900040113     C*
079000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
079100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
079200080429     C                             %trim(intcol(3))+%trim(intcol(4))+
079300080429     C                             %trim(intcol(5))<>*blanks
079400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
079500080429     C                                      %trim(intcol(3))+%trim(intcol(4))+
079600080429     C                                      %trim(intcol(5))
079700070823     C     ';':CharCSV   XLATE     vindta        vindta
079800040510     C                   ENDIF
079900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
080000031201     C     minu:maiu     XLATE     vindta        vindta
080100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080200030715     C                   DOW       posDa <= %len(%trim(vindta))
080300030715     C*
080400030715     C* Gestisco il 1° campo
080500030715     C                   IF        i = 1
080600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080700030715     C                   EVAL      posDa = 1
080800030715     C* Eseguo lo scan x trovare la fine del primo campo
080900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C* X i campi successivi al 1°
081400030715     C                   ELSE
081500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081600030715     C                   EVAL      posDa = posA + 1
081700030715     C* Eseguo lo scan x trovare la fine del campo corrente
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* Gestisco l'ultimo campo
082000030715     C                   IF        posA = *zeros
082100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082200030715     C* A questo punto "estraggo" il campo corrente
082300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082400030715     C                                                    (posA-posDa))
082500030715     C                   LEAVE
082600030715     C                   ELSE
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C                   ENDIF
083100030715     C                   ENDIF
083200030715     C* Incremento il contatore d campo
083300030715     C                   EVAL      i = i +1
083400030715     C                   ENDDO
083500030715     C*
083600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
083700030715     C* delimitazione testo
083800030715     C                   EVAL      i = 1
083900030715     C                   DOW       i < %elem(SkSplitFLD)
084000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
084100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
084200030715     C                   EVAL      i = i + 1
084300030715     C                   ENDDO
084400030715     C                   EVAL      wGiro = 1
084500030715     C                   ELSE
084600030715     C                   EVAL      wGiro = 2
084700031204     C**********
084800031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084900031204     C* d separatore campo
085000031204     C**********
085100031204     C                   EVAL      vindta = %trim(vindta)
085200031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
085300031204     C                   EVAL      vindta = %subst(vindta:2)
085400031204     C                   ENDDO
085500031204     C*
085600031204     C                   Z-ADD     *zeros        lunghInput        4 0
085700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085800031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085900031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
086000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
086100031204     C                   ENDDO
086200030715     C**********
086300030715     C* Effettuo lo split del campo dati d input
086400030715     C**********
086500030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
086600030715     C                   DOW       posDa <= %len(%trim(vindta))
086700030715     C*
086800030715     C* Gestisco il 1° campo
086900030715     C                   IF        i = 1
087000030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
087100030715     C                   EVAL      posDa = 1
087200030715     C* Eseguo lo scan x trovare la fine del primo campo
087300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087400030715     C* A questo punto "estraggo" il campo corrente
087500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087600030715     C                                                    (posA-posDa))
087700030715     C* X i campi successivi al 1°
087800030715     C                   ELSE
087900030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
088000030715     C                   EVAL      posDa = posA + 1
088100031204     C* Verifico che nn vi sia il campo nn valorizzato
088200031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
088300031204     C* Se campo nn valorizzato skippo al prossimo
088400031204     C                   EVAL      SkSplitCSV(i) = *blanks
088500031204     C                   EVAL      posA  = posA + 1
088600031204     C                   ELSE
088700030715     C* Eseguo lo scan x trovare la fine del campo corrente
088800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088900030715     C* Gestisco l'ultimo campo
089000030715     C                   IF        posA = *zeros
089100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
089200030715     C* A questo punto "estraggo" il campo corrente
089300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089400030715     C                                                    (posA-posDa))
089500030715     C                   LEAVE
089600030715     C                   ELSE
089700030715     C* A questo punto "estraggo" il campo corrente
089800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089900030715     C                                                    (posA-posDa))
090000030715     C                   ENDIF
090100031204     C                   ENDIF
090200030715     C                   ENDIF
090300030715     C* Incremento il contatore d campo
090400030715     C                   EVAL      i = i +1
090500030715     C                   ENDDO
090600030715     C*
090700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
090800030715     C* delimitazione testo
090900030715     C                   EVAL      i = 1
091000030715     C                   DOW       i < %elem(SkSplitCSV)
091100030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
091200030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
091300030715     C                   EVAL      i = i + 1
091400030715     C                   ENDDO
091500100720     C*
091600100720     C* Stacco un progressivo x numero spedizione
091700100720 xxx C***                EXSR      repNSP
091800100326     C*
091900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
092000030715     C                   EVAL      i = 1
092100030715     C                   DOW       i < %elem(SkSplitFLD)
092200030715     C*
092300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
092400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
092500030715     C                             SkSplitCSV(i) <> *blanks
092600030721     C*** ATB
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
092800030721     C                   EVAL      VABATB = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030715     C*** RSD
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
093200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
093300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
093400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
093500030715     C* ==
093600030715     C                   ENDIF
093700030721     C*** RD2
093800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
093900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
094000030721     C                   ENDIF
094100030715     C*** IND
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
094300030715     C                   EVAL      VABIND = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** LOD
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
094700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** PRD
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
095100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030721     C*** NZD
095400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
095500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
095600030721     C                   ENDIF
095700030715     C*** RMA
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
095900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** NOT
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
096300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** NT2
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
096700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030715     C*** VCA
097000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
097100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
097200030715     C                   ENDIF
097300030715     C*** VAS
097400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
097500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
097600030715     C                   ENDIF
097700030715     C*** FFD
097800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
097900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
098000030715     C                   ENDIF
098100030721     C*** GC1
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
098300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** GC2
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
098700030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** TSP
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
099100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
099200030721     C                   ENDIF
099300040510     C*** CBO
099400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099600040510     C                   ENDIF
099700030721     C*** NAS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** TIC
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
100300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** GCA
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** XCO
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
101100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** CTM
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** TCR
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** CTS
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
102300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** FTM
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** VAD
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
103100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GMA
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** GGA
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** GVA
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
104300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TC1
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** TC2
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
105100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** SCL
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** RMO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*** NMO
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
106300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106400030721     C                   ENDIF
106500030721     C*
106600030721     C* Reperisco quindi i campi numerici...
106700040510     C*** NSP
106800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106900040510     C                   EVAL      PiStr=SkSplitCSV(i)
107000040510     C                   EXSR      CHKNUM
107100040510     C                   IF        PiInt=*on
107200040510     C                   Z-ADD     PiVal         VABNSP
107300040510     C                   ELSE
107400040510     C                   SETON                                        31
107500040510     C                   EVAL      VABNSP = *zeros
107600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107700040510     C                             + ' ' + 'VABNSP'
107800040510     C                   ENDIF
107900040510     C                   ENDIF
108000071106     C*** RMN
108100071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
108200071106     C                   EVAL      PiStr=SkSplitCSV(i)
108300071106     C                   EXSR      CHKNUM
108400071106     C                   IF        PiInt=*on
108500071106     C                   Z-ADD     PiVal         VABRMN
108600071106     C                   ELSE
108700071106     C                   SETON                                        32
108800071106     C                   EVAL      VABRMN = *zeros
108900071106     C                   EVAL      vinmsg = %trimr(vinmsg)
109000071106     C                             + ' ' + 'VABRMN'
109100071106     C                   ENDIF
109200071106     C                   ENDIF
109300030721     C*** CAD
109400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109500030721     C                   EVAL      PiStr=SkSplitCSV(i)
109600030721     C                   EXSR      CHKNUM
109700030721     C                   IF        PiInt=*on
109800030721     C                   Z-ADD     PiVal         Num5_0
109900030721     C                   MOVEL(p)  Num5_0        VABCAD
110000030721     C                   ELSE
110100030721     C                   SETON                                        32
110200030721     C                   EVAL      VABCAD = *zeros
110300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110400030721     C                             + ' ' + 'VABCAD'
110500030721     C                   ENDIF
110600030721     C                   ENDIF
110700030721     C*** NCL
110800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
110900030721     C                   EVAL      PiStr=SkSplitCSV(i)
111000030721     C                   EXSR      CHKNUM
111100030721     C                   IF        PiInt=*on
111200030721     C                   Z-ADD     PiVal         VABNCL
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABNCL = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700040122     C                             + ' ' + 'VABNCL'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** PKB
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiNum=*on
112500030721     C                   Z-ADD     PiVal         VABPKB
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABPKB = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABPKB'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** CAS
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiNum=*on
113800030721     C                   Z-ADD     PiVal         VABCAS
113900030721     C                   ELSE
114000030721     C                   SETON                                        32
114100030721     C                   EVAL      VABCAS = *zeros
114200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114300030721     C                             + ' ' + 'VABCAS'
114400030721     C                   ENDIF
114500030721     C                   ENDIF
114600030721     C*** CCM
114700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
114800030721     C                   EVAL      PiStr=SkSplitCSV(i)
114900030721     C                   EXSR      CHKNUM
115000030721     C                   IF        PiInt=*on      AND
115100030721     C                             PiVal<=9999999 AND
115200030721     C                             PiVal>*zeros
115300030721     C                   Z-ADD     PiVal         VABCCM
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      VABCCM = *zeros
115700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115800040510     C                             + ' ' + 'VABCCM'
115900030721     C                   ENDIF
116000030721     C                   ENDIF
116100030721     C*** LNP
116200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
116300030721     C                   EVAL      PiStr=SkSplitCSV(i)
116400030721     C                   EXSR      CHKNUM
116500030721     C                   IF        PiInt=*on    AND
116600030721     C                             PiVal<=999   AND
116700030721     C                             PiVal>*zeros
116800030721     C                   Z-ADD     PiVal         VABLNP
116900030721     C                   ELSE
117000040715     C                   SETON                                        32
117100030721     C                   EVAL      VABLNP = *zeros
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300040510     C                             + ' ' + 'VABLNP'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** NRS
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on AND
118100030721     C                             PiVal<=99
118200030721     C                   Z-ADD     PiVal         VABNRS
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABNRS'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** LNA
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on    AND
119400030721     C                             PiVal<=999   AND
119500030721     C                             PiVal>*zeros
119600030721     C                   Z-ADD     PiVal         VABLNA
119700030721     C                   ELSE
119800030721     C                   SETON                                        32
119900030721     C                   EVAL      VABLNA = *zeros
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABLNA'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** CTR
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiInt=*on  AND
120900030721     C                             PiVal<=999
121000030721     C                   Z-ADD     PiVal         VABCTR
121100030721     C                   ELSE
121200030721     C                   SETON                                        32
121300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121400030721     C                             + ' ' + 'VABCTR'
121500030721     C                   ENDIF
121600030721     C                   ENDIF
121700030721     C*** IAS
121800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
121900030721     C                   EVAL      PiStr=SkSplitCSV(i)
122000030721     C                   EXSR      CHKNUM
122100030721     C                   IF        PiNum=*on
122200030721     C                   Z-ADD     PiVal         VABIAS
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABIAS'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** VLB
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400030721     C                   Z-ADD     PiVal         VABVLB
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABVLB'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** QFT
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiNum=*on
124600030721     C                   Z-ADD     PiVal         VABQFT
124700030721     C                   ELSE
124800030721     C                   SETON                                        32
124900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125000030721     C                             + ' ' + 'VABQFT'
125100030721     C                   ENDIF
125200030721     C                   ENDIF
125300030721     C*** NCD
125400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
125500030721     C                   EVAL      PiStr=SkSplitCSV(i)
125600030721     C                   EXSR      CHKNUM
125700030721     C                   IF        PiInt=*on      AND
125800030721     C                             PiVal<=9999999
125900030721     C                   Z-ADD     PiVal         VABNCD
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABNCD'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** NCA
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on      AND
127100030721     C                             PiVal<=9999999
127200030721     C                   Z-ADD     PiVal         VABNCA
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABNCA'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** ZNC
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on AND
128400030721     C                             PiVal<=99
128500030721     C                   Z-ADD     PiVal         VABZNC
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128900030721     C                             + ' ' + 'VABZNC'
129000030721     C                   ENDIF
129100030721     C                   ENDIF
129200030721     C*** DCR
129300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
129400030721     C                   EVAL      PiStr=SkSplitCSV(i)
129500030721     C                   EXSR      CHKNUM
129600030721     C                   IF        PiInt=*on       AND
129700030721     C                             PiVal<=99999999
129800030721     C                   Z-ADD     PiVal         VABDCR
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABDCR'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** HCR
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on   AND
131000030721     C                             PiVal<=9999
131100030721     C                   Z-ADD     PiVal         VABHCR
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABHCR'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** VMD
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiNum=*on
132300030721     C                   Z-ADD     PiVal         VABVMD
132400030721     C                   ELSE
132500030721     C                   SETON                                        32
132600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132700030721     C                             + ' ' + 'VABVMD'
132800030721     C                   ENDIF
132900030721     C                   ENDIF
133000030721     C*** ANT
133100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
133200030721     C                   EVAL      PiStr=SkSplitCSV(i)
133300030721     C                   EXSR      CHKNUM
133400030721     C                   IF        PiInt=*on        AND
133500030721     C                             PiVal<=999999999
133600030721     C                   Z-ADD     PiVal         VABANT
133700030721     C                   ELSE
133800030721     C                   SETON                                        32
133900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134000030721     C                             + ' ' + 'VABANT'
134100030721     C                   ENDIF
134200030721     C                   ENDIF
134300030721     C*** CMO
134400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
134500030721     C                   EVAL      PiStr=SkSplitCSV(i)
134600030721     C                   EXSR      CHKNUM
134700030721     C                   IF        PiInt=*on
134800030721     C                   Z-ADD     PiVal         Num5_0
134900030721     C                   MOVEL(p)  Num5_0        VABCMO
135000030721     C                   ELSE
135100030721     C                   SETON                                        32
135200030721     C                   EVAL      VABCMO = *zeros
135300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135400030721     C                             + ' ' + 'VABCMO'
135500030721     C                   ENDIF
135600030721     C                   ENDIF
135700040510     C*
135800120306     C* Reperisco i campi del EDIVAT
135900040510     C*** VATNOT
136000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
136100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
136200040510     C                   ENDIF
136300040510     C*** VATTRC
136400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
136500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
136600040510     C                   ENDIF
136700060331     C*** VATNOT_A
136800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
136900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
137000060331     C                   ENDIF
137100060331     C*** VATNOT_B
137200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
137300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
137400060331     C                   ENDIF
137500060331     C*** VATNOT_E
137600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
137800060331     C                   ENDIF
137900070823     C*
138000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
138100100720     C*** RMA / NSP
138200100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMA'
138300100419     C                   EVAL      VABRMA = %trim(SkSplitCSV(i))
138400100413     C                   EVAL      PiStr=SkSplitCSV(i)
138500100413     C                   EXSR      CHKNUM
138600100413     C                   IF        PiInt=*on
138700100419     C                   Z-ADD     PiVal         VABNSP
138800100413     C                   ELSE
138900100413     C                   SETON                                        32
139000100413     C                   EVAL      vinmsg = %trimr(vinmsg)
139100100720     C                             + ' ' + 'VABNSP'
139200100413     C                   ENDIF
139300100326     C                   ENDIF
139400100720     C*** VLB
139500100720     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
139600100720     C                   EVAL      PiStr=SkSplitCSV(i)
139700100720     C                   EXSR      CHKNUM
139800100720     C                   IF        PiNum=*on
139900100720     C                   EVAL      PiVal = PiVal / 1000000                      * da cm3 a m3
140000100720     C                   Z-ADD     PiVal         VABVLB
140100100720     C                   ELSE
140200100720     C                   SETON                                        32
140300100720     C                   EVAL      vinmsg = %trimr(vinmsg)
140400100720     C                             + ' ' + 'VABVLB'
140500100720     C                   ENDIF
140600100720     C                   ENDIF
140700100419     C*** CBO
140800100419     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCBO'
140900100419     C                   SELECT
141000100720     C                   WHEN      %trim(SkSplitCSV(i)) = 'X'
141100100720     C                   EVAL      VABCBO = '4'
141200120306     C                   MOVE(P)   *all'9'       VABCAS
141300120306     C                   MOVEL     'EUR'         VABVCA
141400100419     C                   ENDSL
141500100413     C                   ENDIF
141600100720     C*** NOT / NT2
141700100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_1'
141800100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
141900100824     C                                     SkSplitCSV(i)
142000100720     C                   ENDIF
142100100824     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNOT_2'
142200100824     C                   EVAL      wNOTE = %trim(wNOTE) + ' ' +
142300100824     C                                     SkSplitCSV(i)
142400100824     C                   ENDIF
142500070925     C*
142600071029     C***  <===  -----------------------------------------
142700040510     C*
142800030715     C                   ENDIF
142900030715     C                   EVAL      i = i + 1
143000030715     C                   ENDDO
143100070925     C*
143200071029     C***  ===>  Gestione campi "particolari" fuori ciclo
143300100824     C*** NOT / NT2
143400100824     C                   EVAL      wNOTE = %trim(wNOTE)
143500100824     C                   EVAL      VABNOT = %subst(wNOTE:1:35)
143600100824     C                   EVAL      VABNT2 = %subst(wNOTE:36:35)
143700070925     C*
143800071029     C***  <===  -----------------------------------------
143900070925     C*
144000070925     C* Considerazioni finali su CBO/CAS
144100100318     C                   IF        wFlgCAS = '1'    OR
144200100318     C                             VABCAS  > *zeros
144300070925     C                   IF        VABCBO = '1'
144400070925     C                   EVAL      VABCBO = '4'
144500070925     C                   ENDIF
144600070925     C                   IF        VABCBO = '2'
144700070925     C                   EVAL      VABCBO = '6'
144800070925     C                   ENDIF
144900070925     C                   ENDIF
145000070925     C***  <===  ----------------------------
145100040510     C*
145200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
145300040510     C                   IF        VABNSP = *zeros
145400040510     C                   SETON                                        31
145500040510     C                   ENDIF
145600040715     C*
145700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
145800040715     C                   IF        VABLNP = *zeros
145900040715     C                   SETON                                        31
146000040715     C                   ENDIF
146100040510     C*
146200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
146300040510     C                   IF        VABRMN = *zeros
146400040510     C                   Z-ADD     VABNSP        VABRMN
146500040510     C                   ENDIF
146600040510     C*
146700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
146800040510     C                   IF        VABRMA = *blanks
146900040714     C                   MOVEL     VABRMN        VABRMA
147000040510     C                   ENDIF
147100040128     C*
147200040510     C* Se provincia nn valorizzata la reperisco
147300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
147400040510     C                   IF        VABPRD = *blanks
147500040128     C                   CLEAR                   TISI95DS
147600040128     C                   EVAL      I95TCN = '3'
147700040128     C                   Z-ADD     datcor        I95DAT
147800040128     C                   EVAL      I95NAR = VABNZD
147900040128     C                   EVAL      I95CAP = VABCAD
148000040128     C                   EVAL      I95LOC = VABLOD
148100040128     C                   CALL      'TISI95R'
148200040128     C                   PARM                    TISI95DS
148300040128     C                   EVAL      VABPRD = O95PRV
148400040510     C                   ENDIF
148500020204     C*
148600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
148700020204     C                   EXSR      CHKIMPDIV
148800030715     C                   ENDIF
148900020204     C*
149000010607     C                   ENDSR
149100010607     C*----------------------------------------------------*
149200010601
149300020204
149400020204     C*----------------------------------------------------*
149500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
149600020204     C*----------------------------------------------------*
149700020204     C     CHKIMPDIV     BEGSR
149800020204     C*
149900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150000020204     C                   Z-ADD     *zeros        wrkDec            9 9
150100020204     C*
150200020204     C* Come prima cosa effettuo considerazioni sulla divisa
150300020204     C                   IF        vabIAS > *zeros
150400020204     C                   IF        vabVAS <> 'EUR'
150500020204     C                   EVAL      vabVAS =  'ITL'
150600020204     C                   ENDIF
150700020204     C                   ENDIF
150800020204     C*
150900020204     C                   IF        vabCAS > *zeros
151000020204     C                   IF        vabVCA <> 'EUR'
151100020204     C                   EVAL      vabVCA =  'ITL'
151200020204     C                   ENDIF
151300020204     C                   ENDIF
151400020204     C*
151500020204     C                   IF        vabVMD > *zeros
151600020204     C                   IF        vabVAD <> 'EUR'
151700020204     C                   EVAL      vabVAD =  'ITL'
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152200020204     C                   Z-ADD     vabIAS        wrkDec
152300020204     C                   IF        wrkDec > *zeros
152400020204     C                   IF        vabVAS = 'ITL'
152500020204     C                   EVAL      vabIAS = *zeros
152600020204     C                   ENDIF
152700020204     C                   ENDIF
152800020204     C*
152900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153000020204     C                   Z-ADD     vabCAS        wrkDec
153100020204     C                   IF        wrkDec > *zeros
153200020204     C                   IF        vabVCA = 'ITL'
153300020204     C                   EVAL      vabCAS = *zeros
153400020204     C                   ENDIF
153500020204     C                   ENDIF
153600020204     C*
153700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
153800020204     C                   Z-ADD     vabVMD        wrkDec
153900020204     C                   IF        wrkDec > *zeros
154000020204     C                   IF        vabVAD = 'ITL'
154100020204     C                   EVAL      vabVMD = *zeros
154200020204     C                   ENDIF
154300020204     C                   ENDIF
154400020204     C*
154500020204     C                   ENDSR
154600020204     C***
154700010330
154800100318
154900100318
155000100318     C*----------------------------------------------------*
155100100318     C*  CONTROLLO NUMERICITA' CAMPI
155200100318     C*----------------------------------------------------*
155300100318     C     CHKNUM        BEGSR
155400100318     C*
155500100318     C                   IF        PiDecChr = *blanks
155600100318     C                   EVAL      PiDecChr = CharNUM
155700100318     C                   ENDIF
155800100318     C*
155900100318     C                   callp     UBISNUM_Check(PiStr
156000100318     C                                          :PiDecChr
156100100318     C                                          :PiVal
156200100318     C                                          :PiNum
156300100318     C                                          :PiInt)
156400100318     C*
156500100318     C                   ENDSR
156600100318     C***
156700010601
156800010601
156900010601
157000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
157100010601     C     repfil        BEGSR
157200010601     C*
157300010601     C                   if        invfil = *zeros and
157400010601     C                             depfil > *zeros and
157500010629     C                             (vinflg = *blanks or
157600010629     C                              vinflg = *zeros)
157700010601     C
157800010601     C                   eval      invfil = depfil
157900010601     C                   endif
158000010601     C*
158100010601     C                   if        depfil <> invfil and
158200010601     C                             invfil > *zeros
158300010601     C                   eval      flgMulti = '1'
158400010601     C                   if        vinflg = *blanks
158500010601     C                   add       1             cntNonEl
158600010601     C                   endif
158700010601     C                   endif
158800010601     C*
158900010601     C                   if        vinflg = '2'
159000010601     C                   eval      flgStato = '2'
159100010601     C                   endif
159200010601     C*
159300010601     C                   ENDSR
159400010601     C***
159500010601
159600010601
159700010601
159800010330
159900040119      /TITLE Invio dei dati al punto operativo.
160000040119     C     invio         BEGSR
160100040119     C*
160200120306     C* 1° invio EDIVAT
160300040119     C                   reset                   dscmz
160400040119     C                   move      vlrpoi        cmzdst
160500120306     C                   eval      cmzfld = 'EDIVATWR'
160600040119     C                   eval      cmzmbd = vlrhdl
160700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
160800040119     C***                if        prmfir = *blanks
160900120306     C                   eval      cmzfla = 'EDIVAT0F'
161000120306     C                   eval      cmzmba = 'EDIVAT0F'
161100040119     C***                else
161200040119     C***                eval      cmzfla = prmfir
161300040119     C***                eval      cmzmba = prmfir
161400040119     C***                endif
161500040119     C                   eval      cmznrr = *zeros
161600040119     C                   move      §ctrokvt      cmznrr
161700040119     C                   eval      cmzlba = vlrfl1
161800040119     C                   call(e)   'TIS711C'
161900040119     C                   parm                    dscmz
162000040119     C                   parm      *blanks       esito
162100040119     C                   if        %error
162200040119     C                             or cmzerr = '1'
162300040119     C                             or esito  = '1'
162400040119     C                   eval      wrkesito = '3'
162500040119     C                   else
162600040119     C*
162700120306     C* 2° invio EDIVAB
162800040119     C                   reset                   dscmz
162900040119     C                   move      vlrpoi        cmzdst
163000040119     C                   eval      cmzfld = vlrfou
163100040119     C                   eval      cmzmbd = vlrhdl
163200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163300040119     C***                if        prmfir = *blanks
163400120306     C                   eval      cmzfla = 'EDIVAB0F'
163500120306     C                   eval      cmzmba = 'EDIVAB0F'
163600040119     C***                else
163700040119     C***                eval      cmzfla = prmfir
163800040119     C***                eval      cmzmba = prmfir
163900040119     C***                endif
164000040119     C                   eval      cmznrr = *zeros
164100040119     C                   move      §ctrokvb      cmznrr
164200040119     C                   eval      cmzlba = vlrfl1
164300040119     C                   call(e)   'TIS711C'
164400040119     C                   parm                    dscmz
164500040119     C                   parm      *blanks       esito
164600040119     C                   if        %error
164700040119     C                             or cmzerr = '1'
164800040119     C                             or esito  = '1'
164900040119     C                   eval      wrkesito = '3'
165000040119     C                   endif
165100040119     C                   endif
165200040119     C*
165300040119     C                   ENDSR
165400040119     C***
165500010601
165600010601
165700010601
165800010601
165900010601      /TITLE Invio dei dati al punto operativo.
166000010601     C     opeini        BEGSR
166100010601     C*
166200010601     C* Inizializzo flag e contatori operativi
166300010601     C                   movel     '0'           flgGiro           1
166400010601     C                   movel     '0'           flgMulti          1
166500010601     C                   movel     '1'           flgStato          1
166600010615     C                   movel     '0'           flgOk             1
166700010601     C                   z-add     *zeros        cntNonEl         10 0
166800010601     C                   z-add     *zeros        depfil            3 0
166900010601     C                   z-add     *zeros        invfil            3 0
167000010601     C*
167100010601     C                   ENDSR
167200010601     C***
167300070326
167400070326
167500070326
167600070326
167700070326     C     *pssr         BEGSR
167800070329     C*
167900070329     C                   if        %open(tivin00r)
168000070329     C                   close     tivin00r
168100070329     C                   endif
168200120306     C                   if        %open(edivabwr)
168300120306     C                   close     edivabwr
168400070329     C                   endif
168500120306     C                   if        %open(edivatwr)
168600120306     C                   close     edivatwr
168700070329     C                   endif
168800070326     C*
168900070326     C* Effettuo la chiamata al CLLE preposto
169000120306     C                   call(e)   'TITVEVTC'
169100070326     C                   parm                    parccm
169200070326     C                   parm                    parmbr
169300070326     C                   parm      '2'           paropz
169400070326     C*
169500070326     C                   eval      wrkesito = '2'
169600070404     C*
169700070404     C                   seton                                        LR
169800070326     C*
169900070326     C                   ENDSR     '*CANCL'
170000070326     C***
170100100318
170200100318
170300100318
170400100318     C     normCSV       BEGSR
170500100318     C*
170600100318     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
170700100318     C* separatori campi
170800100318     C                   if        CharTXT <> *blanks
170900100318     C                   z-add     *zeros        wLenVINDTA        4 0
171000100318     C                   z-add     *zeros        wFlgVINDTA        1 0
171100100318     C                   setoff                                       80
171200100318     C                   dow       wLenVINDTA < %len(%trim(vindta))
171300100318     C                   eval      wLenVINDTA = wLenVINDTA + 1
171400100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
171500100318     C                   if        not *in80
171600100318     C                   seton                                        80
171700100318     C                   else
171800100318     C                   setoff                                       80
171900100318     C                   endif
172000100318     C                   endif
172100100318     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
172200100318     C                             and *in80 = *on
172300100318     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
172400100318     C                   endif
172500100318     C                   enddo
172600100318     C                   endif
172700100318     C*
172800100318     C                   ENDSR
172900100318     C***
173000070326
173100100326
173200100326
173300100326     C*----------------------------------------------------*
173400100326     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
173500100326     C*----------------------------------------------------*
173600100326     C     REPNSP        BEGSR
173700100326     C*
173800100326     C* NSP => Stacco un numeratore da AZNUM
173900100326     C                   clear                   TRUL33DS
174000100326     C                   eval      I33OPE = *zeros
174100100326     C                   eval      I33CNU = 302
174200100326     C                   eval      I33NUM = 1
174300100326     C                   movel     TRUL33DS      KPJBU
174400100326     C                   call      'TRUL33R'
174500100326     C                   parm                    KPJBA
174600100326     C                   movel     KPJBU         TRUL33DS
174700100326     C                   if        O33ERR = *zeros
174800100326     C                   z-add     O33NRF        VABNSP
174900100326     C                   z-add     O33NRF        VATNSP
175000100326     C                   else
175100100326     C                   SETON                                        31
175200100326     C                   EVAL      vinmsg = %trimr(vinmsg)
175300100326     C                             + ' ' + 'VABNSP VATNSP'
175400100326     C                   endif
175500100326     C*
175600100326     C                   ENDSR
175700010330
175800100326
175900010330
176000000613     C     *inzsr        BEGSR
176100990910     C*
176200990910     C     *entry        plist
176300990920     C                   parm                    tivlrds
176400990921     C                   parm      wrkesito      esito
176500000724     C                   parm                    prmlit
176600000710     C                   parm                    prmfir
176700010330     C*
176800010330     C* CALCOLA LA DATA CORRENTE
176900100318     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
177000100318     C                   eval      datcor = %dec(%date() : *ISO)
177100000613     C*
177200000613     C                   ENDSR
177300000613     C***
177400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
177500100921VABRMN;VABNCL;VABPKB;W_VABVLB;W_VABRMA;VABRSD;VABIND;VABLOD;VABCAD;
177600100921VATNOT_A;VATNOT_B;FIL001;W_VABCBO;VABPRD;W_VABNOT_1;W_VABNOT_2;
177700100921VABDCR;
177800100318
177900100318
