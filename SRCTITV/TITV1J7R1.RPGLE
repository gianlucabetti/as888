000100081028      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500081028     FEDIVABwr  O    E             DISK    usropn
000600081028     FEDIVATwr  O    E             DISK    usropn
000700000313     D*
000800040113     D*------------
000900040113     D* SCHIEDE A PROGRAMMA
001000040113     D*------------
001100040113     D intcol          S            100    DIM(3) CTDATA PERRCD(1)
001200010330     D*----------------------------------------------------
001300010330     D* DICHIARAZIOINE VARIABILI DI WRK
001400010330     D*----------------------------------------------------
001500010330     D dscmz         e ds                  inz
001600010330     D psds           sds
001700010330     D  procname         *PROC
001800010330     D tivlrds       e ds                  extname(tivlr00f)
001900040128     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500040119     D depspe          s                   LIKE(VABNSP) INZ(*zeros)
002600040510     D parccm          s              8    INZ(*blanks)
002700040119     D parmbr          s             10    INZ(*blanks)
002800040119     D paropz          s              1    INZ(*blanks)
002900040119     D chkcall         s              1    INZ(*blanks)
003000080125     D wFlgCAS         s              1    INZ(*blanks)
003100080716     D depvinDTA       s                   LIKE(vinDTA) INZ(*blanks)
003200030715     D*------------------
003300030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003400030715     D*------------------
003500030715     D SkSplitFLD      S             10    DIM(100)
003600030715     D SkSplitCSV      S            256    DIM(100)
003700030715     D CharCSV         S              1
003800030715     D CharTXT         S              1
003900030715     D CharNUM         S              1
004000030715     D posDa           S              3  0 INZ(*zeros)
004100030715     D posA            S              3  0 INZ(*zeros)
004200030715     D i               s              3  0 INZ(1)
004300030715     D wGiro           s              1  0 INZ(*zeros)
004400080125     D wVATNOT_A       s                   LIKE(VATNOT)
004500080125     D wVATNOT_B       s                   LIKE(VATNOT)
004600080125     D wVATNOT_E       s                   LIKE(VATNOT)
004700140512     D wVATNOT_IJ      s             70
004800140512     D wVATNOT_S       s                   LIKE(VATNOT)
004900030822     D*------------------
005000030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005100030822     D*------------------
005200030822     D posDaDft        S              3  0 INZ(*zeros)
005300030822     D posADft         S              3  0 INZ(*zeros)
005400030822     D j               s              3  0 INZ(1)
005500030822     D wGiroDft        s              1  0 INZ(*zeros)
005600010330     D*------------------
005700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005800010330     D*------------------
005900010330     D WLBDA8          DS                  INZ
006000010330     D  G08DAT                 1      8  0
006100010330     D  G08INV                 9     16  0
006200010330     D  G08ERR                17     17
006300010330     D  G08TGI                18     22  0
006400031201     D*------------------
006500031201     D* Costanti
006600031201     D*------------------
006700031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006800031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006900080617     D*------------------
007000080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
007100080617     D*------------------
007200080617     D PiStr           S            100A   INZ
007300080617     D PiDecChr        S              1A   INZ
007400080617     D PiVal           S             63S30 INZ
007500080617     D PiInt           S               N   INZ
007600080617     D PiNum           S               N   INZ
007700080617
007800080617     D*------------------
007900080617     D* LINKING A DEFINIZIONI ESTERNE
008000080617     D*------------------
008100080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008200010330
008300010330
008400010330
008500000913     C                   reset                   rrnum
008600990921     C                   reset                   esito
008700081014     C                   reset                   wrkesito
008800010601     C*
008900010601     C                   exsr      opeini
009000010605     C                   exsr      rwvab
009100081014     C*
009200081014     C                   exsr      endela
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
012200081014     C*
012300081014     C                   if        %open(tivin00r)
012400081014     C                   close     tivin00r
012500081014     C                   endif
012600081028     C                   if        %open(edivabwr)
012700081028     C                   close     edivabwr
012800081014     C                   endif
012900081028     C                   if        %open(edivatwr)
013000081028     C                   close     edivatwr
013100081014     C                   endif
013200040119     C*
013300040119     C* Effettuo la chiamata al CLLE preposto
013400081028     C                   call(e)   'TITVEVTC'
013500040510     C                   parm                    parccm
013600040119     C                   parm                    parmbr
013700040119     C                   parm      '2'           paropz
013800050201     C*
013900050201     C* Effettuo lancio TISI95 solo x chiusura
014000050201     C                   CLEAR                   TISI95DS
014100050201     C                   EVAL      I95TLA = 'C'
014200050201     C                   CALL      'TISI95R'
014300050201     C                   PARM                    TISI95DS
014400000616     C*
014500010601     C                   ENDSR
014600010601     C***
014700000613
014800010601
014900010601
015000010330     C*--------------------------------------------------------
015100081028     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015200010330     C*--------------------------------------------------------
015300010605     C     RWVAB         BEGSR
015400010330     C*
015500010330     C                   if        not %open(tivin00r)
015600010330     C                   open      tivin00r
015700010330     C                   endif
015800081028     C                   if        not %open(edivabwr)
015900081028     C                   open      edivabwr
016000010330     C                   endif
016100040119     C*
016200081028     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016300040119     C                   exsr      prevat
016400040119     C*
016500040119     C                   if        chkcall = '0'
016600040119     C*
016700081028     C                   if        not %open(edivatwr)
016800081028     C                   open      edivatwr
016900040119     C                   endif
017000010330     C*
017100040119     C                   clear                   §CTROKVB          7 0
017200040119     C                   clear                   §CTROKVT          7 0
017300040119     C                   clear                   §CTRMO            7 0
017400040119     C                   clear                   §CTRNO            7 0
017500070823     C*
017600070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017700070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017800081016     C***                exsr      inzvar
017900081016     C***                exsr      impvab
018000010330     C*
018100030822     C                   do        *HIVAL
018200010330     C*
018300030822     C                   read      tivin00r                               70
018400010618     C*
018500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018600010618     C                   if        vinflg = '1'
018700010618     C                   eval      flgOk = '1'
018800010618     C                   endif
018900040510     C*
019000040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
019100040510     C                   z-add     *zeros        wLenVINDTA        4 0
019200040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019300040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019400040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019500040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019600040510     C                   z-add     1             wFlgVINDTA
019700040510     C                   leave
019800040510     C                   endif
019900040510     C                   enddo
020000010618     C*
020100040510     C                   if        vindta > *blanks AND
020200040510     C                             wFlgVINDTA = 1
020300010330     C                   add       1             rrnum
020400010330     C*
020500010601     C                   if        *in70 = *off and
020600010330     C                             (vinflg = *blanks
020700010330     C                              or vinflg = '0'
020800010330     C                              or vinflg = '2')
020900010330     C*
021000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
021100010711     C                   if        vinflg = *blanks or vinflg = '0'
021200010711     C                   clear                   vinmsg
021300010711     C                   endif
021400010601     C*
021500070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021600070925     C                   eval      vindta  = ' ' + %trim(vindta)
021700070925     C*
021800010330     C                   exsr      inzvar
021900010605     C                   exsr      impvab
022000010601     C*
022100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022200010605     C                   eval      depfil = VABLNP
022300010601     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021025     C                   if        vlrpoi = 999
022600030822     C                   move(P)   invfil        VABFGS
022700021025     C                   else
022800030822     C                   move(P)   vlrpoi        VABFGS
022900021025     C                   endif
023000010601     C*
023100010601     C                   exsr      PREELA
023200010601     C*
023300010604     C* Ebbene...
023400010604     C*
023500040119     C  N31              add       1             §CTROKVB
023600040119     C   32              add       1             §CTRMO
023700040119     C   31              add       1             §CTRNO
023800030822     C                   if        wGiro = 2
023900040510     C* Scrivo sempre il VAT
024000040510     C  N31              exsr      wrivat
024100040510     C* Scrivo il VAB solo a rottura d spedizione
024200040119     C                   if        depspe =  *zeros OR
024300040119     C                             depspe <> vabNSP
024400040119     C  N31              exsr      wrivab
024500040119     C  N31              eval      depspe = vabNSP
024600040119     C                   endif
024700040119     C                   endif
024800010604     C*
024900010604     C                   if        *in31 = *off and
025000010604     C                             *in32 = *off
025100010604     C                   eval      vinflg = '1'
025200010604     C                   else
025300010604     C                   eval      vinflg = '2'
025400010604     C                   endif
025500010604     C                   endif
025600010601     C*
025700010604     C                   endif
025800010604     C*
025900010330     C                   else
026000010330     C                   eval      vinflg = '1'
026100010330     C                   endif
026200010601     C*
026300010601     C  N70              update    tivin000
026400010330     C*
026500030822     C  N70              enddo
026600040119     C*
026700040119     C                   endif                                                  (endif - chkcall)
026800010601     C*
026900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
027000010601     C                   if        cntNonEl = *zeros or
027100010601     C                             flgMulti = '0'
027200010330     C* Se non ci sono record con errori ...
027300010601     C                   if        §ctrno = 0 and
027400010604     C                             §ctrmo = 0 and
027500010601     C                             flgStato <> '2'
027600010330     C* ... restituisco esito OK.
027700010330     C                   eval      wrkesito = '0'
027800010330     C                   else
027900040119     C                   if        §ctrokvb > 0
028000010330     C                   eval      wrkesito = '1'
028100010330     C                   else
028200010615     C                   if        flgOk = '0'
028300010615     C                   eval      wrkesito = '2'
028400010615     C                   else
028500010615     C                   eval      wrkesito = '6'
028600010615     C                   endif
028700010330     C                   endif
028800010330     C                   endif
028900010601     C                   else
029000010601     C                   eval      wrkesito = '9'
029100010601     C                   endif
029200010330     C*
029300010330     C                   if        %open(tivin00r)
029400010330     C                   close     tivin00r
029500010330     C                   endif
029600081028     C                   if        %open(edivabwr)
029700081028     C                   close     edivabwr
029800010330     C                   endif
029900081028     C                   if        %open(edivatwr)
030000081028     C                   close     edivatwr
030100040512     C                   endif
030200010601     C*
030300010601     C                   if        vlrpoi <> 999
030400010601     C                   eval      invfil = vlrpoi
030500010601     C                   endif
030600010330     C*
030700040119     C                   if        §ctrokvb > 0
030800010601     C                             and invfil > *zeros
030900010330     C                   exsr      invio
031000010330     C                   endif
031100010330     C*
031200010330     C                   ENDSR
031300010330     C***
031400040119
031500040119
031600040119
031700040119     C*----------------------------------------------------*
031800040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031900040119     C*----------------------------------------------------*
032000040510     C     WRIVAB        BEGSR
032100081028     C*
032200081028     C* VALORIZZO CAMPI RELATIVI AL "CMR"
032300081028     C                   eval      VABCMR = 'AKU ' + %char(datcor)
032400081028     C                   eval      VABDCM = DATCOR
032500081028     C                   eval      VABDTS = DATCOR
032600081028     C                   eval      VABHMS = ORACOR
032700081028     C                   eval      VABCNT = 1
032800040119     C*
032900060331     C* Gestisco l'eventuale rottura x numero spedizione
033000070823     C                   if        VABNSP <> depspe
033100070301     C                   if        wVATNOT_E <> *blanks
033200080125     C                   if        VABCTM = *blanks
033300080125     C                   eval      VABCTM = '7Q'
033400080125     C                   endif
033500070301     C                   endif
033600081028     C                   write     edivab00                                     => scarico il VAB
033700060331     C                   endif
033800040119     C*
033900040119     C                   ENDSR
034000040119
034100040119
034200040119
034300040119     C*----------------------------------------------------*
034400040119     C*  SCARICAMENTO BUFFER RECORDS VAT
034500040119     C*----------------------------------------------------*
034600040510     C     WRIVAT        BEGSR
034700060512     C*
034800060512     C* Inizializzo i campi chiave
034900060512     C                   eval      VATFGS = VABFGS
035000060512     C                   eval      VATAAS = VABAAS
035100060512     C                   eval      VATCCM = VABCCM
035200070823     C                   eval      VATNRS = VABNRS
035300060512     C                   eval      VATNSP = VABNSP
035400060512     C                   eval      VATLNP = VABLNP
035500081028     C*
035600081028     C* Valorizzo i campi relativi al CMR
035700140516     C                   eval      VATCMR = 'AKU ' + %char(datcor)
035800140516     C                   eval      VATCNT = 1
035900060331     C*
036000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
036100060331     C                   if        wVATNOT_A <> *blanks
036200060331     C                   eval      VATNOT = wVATNOT_A
036300060331     C                   eval      VATTRC = 'A'
036400081028     C                   write     EDIVAT00
036500060331     C                   add       1             §CTROKVT
036600060331     C                   endif
036700060331     C*
036800060331     C                   if        wVATNOT_B <> *blanks
036900060331     C                   eval      VATNOT = wVATNOT_B
037000060331     C                   eval      VATTRC = 'B'
037100081028     C                   write     EDIVAT00
037200060331     C                   add       1             §CTROKVT
037300060331     C                   endif
037400140512     C*
037500140512     C                   if        wVATNOT_IJ <> *blanks
037600140512     C                   eval      VATNOT = %subst(wVATNOT_IJ:1:35)
037700140512     C                   eval      VATTRC = 'I'
037800140512     C                   write     EDIVAT00
037900140512     C                   IF        %subst(wVATNOT_IJ:36:35) <> *blank
038000140512     C                   eval      VATNOT = %subst(wVATNOT_IJ:36:35)
038100140512     C                   eval      VATTRC = 'J'
038200140512     C                   write     EDIVAT00
038300140512     C                   ENDIF
038400140512     C                   add       1             §CTROKVT
038500140512     C                   endif
038600140512     C*
038700140512     C                   if        wVATNOT_S <> *blanks
038800140512     C                   eval      VATNOT = wVATNOT_S
038900140512     C                   eval      VATTRC = 'S'
039000140512     C                   write     EDIVAT00
039100140512     C                   add       1             §CTROKVT
039200140512     C                   endif
039300060331     C*
039400080625     C                   if        wVATNOT_E <> *blanks
039500060331     C                   eval      VATNOT = wVATNOT_E
039600060331     C                   eval      VATTRC = 'E'
039700081028     C                   write     EDIVAT00
039800080716     C                   add       1             §CTROKVT
039900080625     C                   endif
040000040119     C*
040100040119     C                   ENDSR
040200040119
040300040119
040400040119
040500040119     C*----------------------------------------------------*
040600081028     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
040700040119     C*----------------------------------------------------*
040800040119     C     PREVAT        BEGSR
040900040119     C*
041000081028     C* Compongo il nome del membro da dare al EDIVATWR
041100040119     C                   eval      parmbr = vlrhdl
041200040119     C                   movel     'M'           parmbr
041300040510     C                   eval      parccm = vlrksc
041400040119     C                   eval      paropz = '1'
041500040119     C* Effettuo la chiamata al CLLE preposto
041600081028     C                   call(e)   'TITVEVTC'
041700040119     C                   parm                    parccm
041800040119     C                   parm                    parmbr
041900040119     C                   parm                    paropz
042000040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042100040119     C                   if        %error
042200040119     C                   movel     '1'           chkcall
042300040119     C                   else
042400040119     C                   movel     '0'           chkcall
042500040119     C                   endif
042600040119     C*
042700040119     C                   ENDSR
042800010601
042900010601
043000010601
043100010330     C*----------------------------------------------------*
043200030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
043300010330     C*----------------------------------------------------*
043400040119     C     INZVAR        BEGSR
043500010330     C*
043600030822     C* Inizializzo variabili di wrk
043700020204     C                   Z-ADD     *zeros        Num5_0            5 0
043800081020     C                   MOVEL     *blanks       wNOTE            70
043900060331     C                   MOVEL     *blanks       wVATNOT_A
044000060331     C                   MOVEL     *blanks       wVATNOT_B
044100060331     C                   MOVEL     *blanks       wVATNOT_E
044200140512     C                   MOVEL     *blanks       wVATNOT_IJ
044300140512     C                   MOVEL     *blanks       wVATNOT_S
044400080617     C                   EVAL      wFlgCAS = *blanks
044500030822     C*
044600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044700081028     C                   CLEAR                   EDIVAB00
044800081028     C                   CLEAR                   EDIVAT00
044900030822     C                   CLEAR                   SkSplitCSV
045000030822     C                   EVAL      i = 1
045100030822     C                   EVAL      posDa = *zeros
045200030822     C                   EVAL      posA  = *zeros
045300030822     C*
045400030822     C* Reimposto i valori di default
045500030822     C                   EXSR      DEFCAM
045600030822     C*
045700010330     C                   ENDSR
045800010330     C*----------------------------------------------------*
045900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046000010330     C*----------------------------------------------------*
046100010330     C     DEFCAM        BEGSR
046200030822     C*
046300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046400030715     C* e delimitatore testo.
046500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046800030822     C*
046900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
047000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
047100030715     C                   SETON                                        50
047200030715     C                   ELSE
047300030715     C                   SETOFF                                       50
047400030715     C                   ENDIF
047500030822     C*
047600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
047700030822     C                   EVAL      posDaDft = 1
047800030822     C                   EVAL      posADft  = 0
047900030822     C                   EVAL      wGiroDft = 0
048000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
048100030822     C                             posDaDft > 0
048200030822     C*
048300030822     C* Gestisco il 1° giro
048400030822     C                   IF        wGiroDft = 0
048500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
048600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
048700030822     C* Incremento il contatore dei "giri"
048800030822     C                   EVAL      wGiroDft = 1
048900030822     C                   ELSE
049000030822     C                   EVAL      posDaDft = posADft
049100030822     C                   ENDIF
049200030822     C* Eseguo lo scan x trovare la fine del campo corrente
049300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
049400030822     C*
049500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
049600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
049700030822     C                   IF        posDaDft > 0 AND
049800030822     C                             posADft  > 0
049900030822     C* NCL
050000030822     C                   IF        %subst(
050100030822     C                             %subst(vlrppt:posDaDft+1:
050200030822     C                             posADft-posDaDft-1):1:3)
050300030822     C                             = 'NCL'
050400030822     C                   EVAL      PiStr=%trim(%subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050700030822     C                   EXSR      CHKNUM
050800030822     C                   IF        PiInt=*on
050900030822     C                   Z-ADD     PiVal         VABNCL
051000030822     C                   ENDIF
051100030822     C                   ENDIF
051200030822     C* CCM
051300030822     C                   IF        %subst(
051400030822     C                             %subst(vlrppt:posDaDft+1:
051500030822     C                             posADft-posDaDft-1):1:3)
051600030822     C                             = 'CCM'
051700030822     C                   EVAL      PiStr=%trim(%subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052000030822     C                   EXSR      CHKNUM
052100030822     C                   IF        PiInt=*on
052200030822     C                   Z-ADD     PiVal         VABCCM
052300030822     C                   ENDIF
052400030822     C                   ENDIF
052500030822     C* LNP
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'LNP'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiInt=*on
053500030822     C                   Z-ADD     PiVal         VABLNP
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* NRS
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'NRS'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABNRS
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* CTR
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'CTR'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABCTR
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* PKB
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'PKB'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiNum=*on
057400030822     C                   Z-ADD     PiVal         VABPKB
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* VLB
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'VLB'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiNum=*on
058700030822     C                   Z-ADD     PiVal         VABVLB
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* QFT
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'QFT'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiNum=*on
060000030822     C                   Z-ADD     PiVal         VABQFT
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CBO
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CBO'
060800030822     C                   EVAL      VABCBO=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   ENDIF
061200030822     C* TSP
061300030822     C                   IF        %subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):1:3)
061600030822     C                             = 'TSP'
061700030822     C                   EVAL      VABTSP=%trim(%subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062000030822     C                   ENDIF
062100030822     C* VAS
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'VAS'
062600030822     C                   EVAL      VABVAS=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* VCA
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'VCA'
063500030822     C                   EVAL      VABVCA=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900070925     C* TIC
064000070925     C                   IF        %subst(
064100070925     C                             %subst(vlrppt:posDaDft+1:
064200070925     C                             posADft-posDaDft-1):1:3)
064300070925     C                             = 'TIC'
064400070925     C                   EVAL      VABTIC=%trim(%subst(
064500070925     C                             %subst(vlrppt:posDaDft+1:
064600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700070925     C                   ENDIF
064800030822     C* GCA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'GCA'
065300030822     C                   EVAL      VABGCA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* CTM
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'CTM'
066200030822     C                   EVAL      VABCTM=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* FFD
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'FFD'
067100030822     C                   EVAL      VABFFD=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* VAD
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'VAD'
068000030822     C                   EVAL      VABVAD=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* GMA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GMA'
068900030822     C                   EVAL      VABGMA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* GGA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GGA'
069800030822     C                   EVAL      VABGGA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* GVA
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'GVA'
070700030822     C                   EVAL      VABGVA=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TC1
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TC1'
071600030822     C                   EVAL      VABTC1=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* TC2
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'TC2'
072500030822     C                   EVAL      VABTC2=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900040714     C* VATTRC
073000040714     C                   IF        %subst(
073100040714     C                             %subst(vlrppt:posDaDft+1:
073200040714     C                             posADft-posDaDft-1):1:3)
073300040714     C                             = 'TRC'
073400040714     C                   EVAL      VATTRC=%trim(%subst(
073500040714     C                             %subst(vlrppt:posDaDft+1:
073600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700040714     C                   ENDIF
073800030822     C* ...
073900030822     C                   ENDIF
074000030822     C                   ENDDO
074100020204     C*
074200010330     C                   ENDSR
074300010607     C*----------------------------------------------------*
074400081028     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
074500010607     C*----------------------------------------------------*
074600010607     C     IMPVAB        BEGSR
074700010607     C*
074800010607     C                   SETOFF                                       3132
074900030515     C*
075000010607     C                   MOVEL     datcor        VABAAS
075100010607     C                   MOVE      datcor        VABMGS
075200010607     C*
075300030715     C**********
075400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
075500030715     C**********
075600030715     C                   IF        wGiro = *zeros
075700080716     C*
075800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
075900080716     C                   EVAL      depvinDTA = vinDTA
076000040113     C*
076100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
076200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
076300040510     C                             %trim(intcol(3))<>*blanks
076400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
076500040113     C                                      %trim(intcol(3))
076600070823     C     ';':CharCSV   XLATE     vindta        vindta
076700040510     C                   ENDIF
076800080626     C*
076900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077000031201     C     minu:maiu     XLATE     vindta        vindta
077100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077200030715     C                   DOW       posDa <= %len(%trim(vindta))
077300030715     C*
077400030715     C* Gestisco il 1° campo
077500030715     C                   IF        i = 1
077600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077700030715     C                   EVAL      posDa = 1
077800030715     C* Eseguo lo scan x trovare la fine del primo campo
077900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078000030715     C* A questo punto "estraggo" il campo corrente
078100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078200030715     C                                                    (posA-posDa))
078300030715     C* X i campi successivi al 1°
078400030715     C                   ELSE
078500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078600030715     C                   EVAL      posDa = posA + 1
078700030715     C* Eseguo lo scan x trovare la fine del campo corrente
078800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078900030715     C* Gestisco l'ultimo campo
079000030715     C                   IF        posA = *zeros
079100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079200030715     C* A questo punto "estraggo" il campo corrente
079300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079400030715     C                                                    (posA-posDa))
079500030715     C                   LEAVE
079600030715     C                   ELSE
079700030715     C* A questo punto "estraggo" il campo corrente
079800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079900030715     C                                                    (posA-posDa))
080000030715     C                   ENDIF
080100030715     C                   ENDIF
080200030715     C* Incremento il contatore d campo
080300030715     C                   EVAL      i = i +1
080400030715     C                   ENDDO
080500030715     C*
080600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080700030715     C* delimitazione testo
080800030715     C                   EVAL      i = 1
080900030715     C                   DOW       i < %elem(SkSplitFLD)
081000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081200030715     C                   EVAL      i = i + 1
081300030715     C                   ENDDO
081400030715     C                   EVAL      wGiro = 1
081500080716     C* Ripristino la 1° riga originale
081600080716     C                   EVAL      vinDTA = depvinDTA
081700030715     C                   ELSE
081800030715     C                   EVAL      wGiro = 2
081900031204     C**********
082000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082100031204     C* d separatore campo
082200031204     C**********
082300031204     C                   EVAL      vindta = %trim(vindta)
082400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082500031204     C                   EVAL      vindta = %subst(vindta:2)
082600031204     C                   ENDDO
082700031204     C*
082800031204     C                   Z-ADD     *zeros        lunghInput        4 0
082900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083300031204     C                   ENDDO
083400030715     C**********
083500030715     C* Effettuo lo split del campo dati d input
083600030715     C**********
083700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083800030715     C                   DOW       posDa <= %len(%trim(vindta))
083900030715     C*
084000030715     C* Gestisco il 1° campo
084100030715     C                   IF        i = 1
084200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084300030715     C                   EVAL      posDa = 1
084400030715     C* Eseguo lo scan x trovare la fine del primo campo
084500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C* X i campi successivi al 1°
085000030715     C                   ELSE
085100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085200030715     C                   EVAL      posDa = posA + 1
085300031204     C* Verifico che nn vi sia il campo nn valorizzato
085400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085500031204     C* Se campo nn valorizzato skippo al prossimo
085600031204     C                   EVAL      SkSplitCSV(i) = *blanks
085700031204     C                   EVAL      posA  = posA + 1
085800031204     C                   ELSE
085900030715     C* Eseguo lo scan x trovare la fine del campo corrente
086000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086100030715     C* Gestisco l'ultimo campo
086200030715     C                   IF        posA = *zeros
086300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086400030715     C* A questo punto "estraggo" il campo corrente
086500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086600030715     C                                                    (posA-posDa))
086700030715     C                   LEAVE
086800030715     C                   ELSE
086900030715     C* A questo punto "estraggo" il campo corrente
087000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087100030715     C                                                    (posA-posDa))
087200030715     C                   ENDIF
087300031204     C                   ENDIF
087400030715     C                   ENDIF
087500030715     C* Incremento il contatore d campo
087600030715     C                   EVAL      i = i +1
087700030715     C                   ENDDO
087800030715     C*
087900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088000030715     C* delimitazione testo
088100030715     C                   EVAL      i = 1
088200030715     C                   DOW       i < %elem(SkSplitCSV)
088300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088500030715     C                   EVAL      i = i + 1
088600030715     C                   ENDDO
088700030715     C*
088800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088900030715     C                   EVAL      i = 1
089000030715     C                   DOW       i < %elem(SkSplitFLD)
089100030715     C*
089200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089400030715     C                             SkSplitCSV(i) <> *blanks
089500030721     C*** ATB
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089700030721     C                   EVAL      VABATB = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030715     C*** RSD
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090400030715     C* ==
090500030715     C                   ENDIF
090600030721     C*** RD2
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030715     C*** IND
091100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091200030715     C                   EVAL      VABIND = SkSplitCSV(i)
091300030715     C                   ENDIF
091400030715     C*** LOD
091500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091700030715     C                   ENDIF
091800030715     C*** PRD
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092100030715     C                   ENDIF
092200030721     C*** NZD
092300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092500030721     C                   ENDIF
092600030715     C*** RMA
092700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092900030715     C                   ENDIF
093000030715     C*** NOT
093100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093300030715     C                   ENDIF
093400030715     C*** NT2
093500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093700030715     C                   ENDIF
093800030715     C*** VCA
093900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094100030715     C                   ENDIF
094200030715     C*** VAS
094300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094500030715     C                   ENDIF
094600030715     C*** FFD
094700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
094900030715     C                   ENDIF
095000030721     C*** GC1
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** GC2
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** TSP
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096100030721     C                   ENDIF
096200040510     C*** CBO
096300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
096400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
096500040510     C                   ENDIF
096600030721     C*** NAS
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
096800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** TIC
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** GCA
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
097600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** XCO
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** CTM
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
098400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** TCR
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
098800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** CTS
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*** FTM
099500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
099600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
099700030721     C                   ENDIF
099800030721     C*** VAD
099900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100100030721     C                   ENDIF
100200030721     C*** GMA
100300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
100400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
100500030721     C                   ENDIF
100600030721     C*** GGA
100700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
100800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
100900030721     C                   ENDIF
101000030721     C*** GVA
101100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101300030721     C                   ENDIF
101400030721     C*** TC1
101500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
101600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
101700030721     C                   ENDIF
101800030721     C*** TC2
101900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102100030721     C                   ENDIF
102200030721     C*** SCL
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
102400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
102500030721     C                   ENDIF
102600030721     C*** RMO
102700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
102800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
102900030721     C                   ENDIF
103000030721     C*** NMO
103100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103300030721     C                   ENDIF
103400030721     C*
103500030721     C* Reperisco quindi i campi numerici...
103600040510     C*** NSP
103700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
103800040510     C                   EVAL      PiStr=SkSplitCSV(i)
103900040510     C                   EXSR      CHKNUM
104000040510     C                   IF        PiInt=*on
104100040510     C                   Z-ADD     PiVal         VABNSP
104200040510     C                   ELSE
104300040510     C                   SETON                                        31
104400040510     C                   EVAL      VABNSP = *zeros
104500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
104600040510     C                             + ' ' + 'VABNSP'
104700040510     C                   ENDIF
104800040510     C                   ENDIF
104900071106     C*** RMN
105000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105100071106     C                   EVAL      PiStr=SkSplitCSV(i)
105200071106     C                   EXSR      CHKNUM
105300071106     C                   IF        PiInt=*on
105400071106     C                   Z-ADD     PiVal         VABRMN
105500071106     C                   ELSE
105600071106     C                   SETON                                        32
105700071106     C                   EVAL      VABRMN = *zeros
105800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
105900071106     C                             + ' ' + 'VABRMN'
106000071106     C                   ENDIF
106100071106     C                   ENDIF
106200030721     C*** CAD
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106400030721     C                   EVAL      PiStr=SkSplitCSV(i)
106500030721     C                   EXSR      CHKNUM
106600030721     C                   IF        PiInt=*on
106700030721     C                   Z-ADD     PiVal         Num5_0
106800030721     C                   MOVEL(p)  Num5_0        VABCAD
106900030721     C                   ELSE
107000030721     C                   SETON                                        32
107100030721     C                   EVAL      VABCAD = *zeros
107200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107300030721     C                             + ' ' + 'VABCAD'
107400030721     C                   ENDIF
107500030721     C                   ENDIF
107600030721     C*** NCL
107700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107800030721     C                   EVAL      PiStr=SkSplitCSV(i)
107900030721     C                   EXSR      CHKNUM
108000030721     C                   IF        PiInt=*on
108100030721     C                   Z-ADD     PiVal         VABNCL
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABNCL = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600040122     C                             + ' ' + 'VABNCL'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** PKB
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiNum=*on
109400030721     C                   Z-ADD     PiVal         VABPKB
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      VABPKB = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABPKB'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** CAS
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110400080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
110500080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
110600081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
110700081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
110800080125     C                   EVAL      wFlgCAS = '1'
110900080125     C                   ENDIF
111000030721     C                   EVAL      PiStr=SkSplitCSV(i)
111100030721     C                   EXSR      CHKNUM
111200030721     C                   IF        PiNum=*on
111300030721     C                   Z-ADD     PiVal         VABCAS
111400030721     C                   ELSE
111500030721     C                   SETON                                        32
111600030721     C                   EVAL      VABCAS = *zeros
111700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111800030721     C                             + ' ' + 'VABCAS'
111900030721     C                   ENDIF
112000030721     C                   ENDIF
112100030721     C*** CCM
112200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112300030721     C                   EVAL      PiStr=SkSplitCSV(i)
112400030721     C                   EXSR      CHKNUM
112500030721     C                   IF        PiInt=*on      AND
112600030721     C                             PiVal<=9999999 AND
112700030721     C                             PiVal>*zeros
112800030721     C                   Z-ADD     PiVal         VABCCM
112900030721     C                   ELSE
113000030721     C                   SETON                                        32
113100030721     C                   EVAL      VABCCM = *zeros
113200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113300040510     C                             + ' ' + 'VABCCM'
113400030721     C                   ENDIF
113500030721     C                   ENDIF
113600030721     C*** LNP
113700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113800030721     C                   EVAL      PiStr=SkSplitCSV(i)
113900030721     C                   EXSR      CHKNUM
114000030721     C                   IF        PiInt=*on    AND
114100030721     C                             PiVal<=999   AND
114200030721     C                             PiVal>*zeros
114300030721     C                   Z-ADD     PiVal         VABLNP
114400030721     C                   ELSE
114500040715     C                   SETON                                        32
114600030721     C                   EVAL      VABLNP = *zeros
114700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114800040510     C                             + ' ' + 'VABLNP'
114900030721     C                   ENDIF
115000030721     C                   ENDIF
115100030721     C*** NRS
115200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115300030721     C                   EVAL      PiStr=SkSplitCSV(i)
115400030721     C                   EXSR      CHKNUM
115500030721     C                   IF        PiInt=*on AND
115600030721     C                             PiVal<=99
115700030721     C                   Z-ADD     PiVal         VABNRS
115800030721     C                   ELSE
115900030721     C                   SETON                                        32
116000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116100030721     C                             + ' ' + 'VABNRS'
116200030721     C                   ENDIF
116300030721     C                   ENDIF
116400030721     C*** LNA
116500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116600030721     C                   EVAL      PiStr=SkSplitCSV(i)
116700030721     C                   EXSR      CHKNUM
116800030721     C                   IF        PiInt=*on    AND
116900030721     C                             PiVal<=999   AND
117000030721     C                             PiVal>*zeros
117100030721     C                   Z-ADD     PiVal         VABLNA
117200030721     C                   ELSE
117300030721     C                   SETON                                        32
117400030721     C                   EVAL      VABLNA = *zeros
117500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117600030721     C                             + ' ' + 'VABLNA'
117700030721     C                   ENDIF
117800030721     C                   ENDIF
117900030721     C*** CTR
118000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118100030721     C                   EVAL      PiStr=SkSplitCSV(i)
118200030721     C                   EXSR      CHKNUM
118300030721     C                   IF        PiInt=*on  AND
118400030721     C                             PiVal<=999
118500030721     C                   Z-ADD     PiVal         VABCTR
118600030721     C                   ELSE
118700030721     C                   SETON                                        32
118800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118900030721     C                             + ' ' + 'VABCTR'
119000030721     C                   ENDIF
119100030721     C                   ENDIF
119200030721     C*** IAS
119300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119400030721     C                   EVAL      PiStr=SkSplitCSV(i)
119500030721     C                   EXSR      CHKNUM
119600030721     C                   IF        PiNum=*on
119700030721     C                   Z-ADD     PiVal         VABIAS
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABIAS'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** VLB
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABVLB
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABVLB'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** QFT
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiNum=*on
122100030721     C                   Z-ADD     PiVal         VABQFT
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABQFT'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** NCD
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on      AND
123300030721     C                             PiVal<=9999999
123400030721     C                   Z-ADD     PiVal         VABNCD
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABNCD'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** NCA
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on      AND
124600030721     C                             PiVal<=9999999
124700030721     C                   Z-ADD     PiVal         VABNCA
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABNCA'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** ZNC
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiInt=*on AND
125900030721     C                             PiVal<=99
126000030721     C                   Z-ADD     PiVal         VABZNC
126100030721     C                   ELSE
126200030721     C                   SETON                                        32
126300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126400030721     C                             + ' ' + 'VABZNC'
126500030721     C                   ENDIF
126600030721     C                   ENDIF
126700030721     C*** DCR
126800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126900030721     C                   EVAL      PiStr=SkSplitCSV(i)
127000030721     C                   EXSR      CHKNUM
127100030721     C                   IF        PiInt=*on       AND
127200030721     C                             PiVal<=99999999
127300030721     C                   Z-ADD     PiVal         VABDCR
127400030721     C                   ELSE
127500030721     C                   SETON                                        32
127600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127700030721     C                             + ' ' + 'VABDCR'
127800030721     C                   ENDIF
127900030721     C                   ENDIF
128000030721     C*** HCR
128100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128200030721     C                   EVAL      PiStr=SkSplitCSV(i)
128300030721     C                   EXSR      CHKNUM
128400030721     C                   IF        PiInt=*on   AND
128500030721     C                             PiVal<=9999
128600030721     C                   Z-ADD     PiVal         VABHCR
128700030721     C                   ELSE
128800030721     C                   SETON                                        32
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABHCR'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300030721     C*** VMD
129400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129500030721     C                   EVAL      PiStr=SkSplitCSV(i)
129600030721     C                   EXSR      CHKNUM
129700030721     C                   IF        PiNum=*on
129800030721     C                   Z-ADD     PiVal         VABVMD
129900030721     C                   ELSE
130000030721     C                   SETON                                        32
130100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130200030721     C                             + ' ' + 'VABVMD'
130300030721     C                   ENDIF
130400030721     C                   ENDIF
130500030721     C*** ANT
130600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130700030721     C                   EVAL      PiStr=SkSplitCSV(i)
130800030721     C                   EXSR      CHKNUM
130900030721     C                   IF        PiInt=*on        AND
131000030721     C                             PiVal<=999999999
131100030721     C                   Z-ADD     PiVal         VABANT
131200030721     C                   ELSE
131300030721     C                   SETON                                        32
131400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131500030721     C                             + ' ' + 'VABANT'
131600030721     C                   ENDIF
131700030721     C                   ENDIF
131800030721     C*** CMO
131900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132000030721     C                   EVAL      PiStr=SkSplitCSV(i)
132100030721     C                   EXSR      CHKNUM
132200030721     C                   IF        PiInt=*on
132300030721     C                   Z-ADD     PiVal         Num5_0
132400030721     C                   MOVEL(p)  Num5_0        VABCMO
132500030721     C                   ELSE
132600030721     C                   SETON                                        32
132700030721     C                   EVAL      VABCMO = *zeros
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABCMO'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200040510     C*
133300081028     C* Reperisco i campi del EDIVAT
133400040510     C*** VATNOT
133500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133700040510     C                   ENDIF
133800040510     C*** VATTRC
133900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
134000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
134100040510     C                   ENDIF
134200060331     C*** VATNOT_A
134300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134500060331     C                   ENDIF
134600060331     C*** VATNOT_B
134700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134900060331     C                   ENDIF
135000060331     C*** VATNOT_E
135100070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135300060331     C                   ENDIF
135400070823     C*
135500071029     C***  ===>  Gestione campi "particolari" dentro ciclo
135600081020     C*
135700081020     C*** RSD
135800081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
135900081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136000081020     C                   EVAL      VABRSD = SkSplitCSV(i)
136100081020     C                   ENDIF
136200081020     C                   ENDIF
136300081020     C*** IND
136400081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND'
136500081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136600081020     C                   EVAL      VABIND = SkSplitCSV(i)
136700081020     C                   ENDIF
136800081020     C                   ENDIF
136900081020     C*** LOD
137000081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
137100081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
137200081020     C                   EVAL      VABLOD = SkSplitCSV(i)
137300081020     C                   ENDIF
137400081020     C                   ENDIF
137500081020     C*** PRD
137600081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD'
137700081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
137800081020     C                   EVAL      VABPRD = SkSplitCSV(i)
137900081020     C                   ENDIF
138000081020     C                   ENDIF
138100081020     C*** NZD
138200081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
138300081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
138400081020     C                   EVAL      VABNZD = SkSplitCSV(i)
138500081020     C                   ENDIF
138600081020     C                   ENDIF
138700081020     C*** CAD
138800081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
138900081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
139000081020     C                   EVAL      PiStr=SkSplitCSV(i)
139100081020     C                   EXSR      CHKNUM
139200081020     C                   IF        PiInt=*on
139300081020     C                   Z-ADD     PiVal         Num5_0
139400081020     C                   MOVEL(p)  Num5_0        VABCAD
139500081020     C                   ELSE
139600081020     C                   SETON                                        32
139700081020     C                   EVAL      VABCAD = *zeros
139800081020     C                   EVAL      vinmsg = %trimr(vinmsg)
139900081020     C                             + ' ' + 'VABCAD'
140000081020     C                   ENDIF
140100081020     C                   ENDIF
140200081020     C                   ENDIF
140300081020     C*** RMN
140400081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
140500081020     C                   EVAL      VABRMA=SkSplitCSV(i)
140600081020     C                   EVAL      PiStr=SkSplitCSV(i)
140700081020     C                   EXSR      CHKNUM
140800081020     C                   IF        PiInt=*on
140900081020     C                   Z-ADD     PiVal         VABRMN
141000081020     C                   Z-ADD     PiVal         VABNSP
141100081020     C                   ELSE
141200081020     C                   SETON                                        32
141300081020     C                   EVAL      VABRMN = *zeros
141400081020     C                   EVAL      vinmsg = %trimr(vinmsg)
141500081020     C                             + ' ' + 'VABRMN VABNSP'
141600081020     C                   ENDIF
141700081020     C                   ENDIF
141800081020     C*** NOT+NT2
141900081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
142000081020     C                   EVAL      wNOTE=SkSplitCSV(i)
142100081028     C                   ENDIF
142200081028     C*** VLB
142300081028     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
142400081028     C                   EVAL      PiStr=SkSplitCSV(i)
142500081028     C                   EXSR      CHKNUM
142600081028     C                   IF        PiNum=*on
142700090127     C***                EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
142800081028     C                   Z-ADD(H)  PiVal         VABVLB
142900081105     C                   Z-ADD(H)  PiVal         VABQFT
143000081028     C                   ELSE
143100081028     C                   SETON                                        32
143200081028     C                   EVAL      vinmsg = %trimr(vinmsg)
143300081105     C                             + ' ' + 'VABVLB VABQFT'
143400081028     C                   ENDIF
143500081028     C                   ENDIF
143600140512     C*** VATNOT_IJ
143700140512     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOTIJ'
143800140512     C                   EVAL      wVATNOT_IJ = %trim(SkSplitCSV(i))
143900140512     C* sostituisco al valori ',' (virgola) il carattere ';' (punto e virgola)
144000140512     C                   EVAL      wVATNOT_IJ = %xlate(',' : ';' : wVATNOT_IJ)
144100140512     C                   ENDIF
144200140512     C*** VATNOT_S
144300140512     C                   IF        %trim(SkSplitFLD(i)) = 'W_VATNOT_S'
144400140512     C                   EVAL      wVATNOT_S = %trim(SkSplitCSV(i))
144500140512     C                   EVAL      %subst(wVATNOT_S:17:2) = *blank
144600140512     C                   ENDIF
144700070925     C*
144800071029     C***  <===  -----------------------------------------
144900040510     C*
145000030715     C                   ENDIF
145100030715     C                   EVAL      i = i + 1
145200030715     C                   ENDDO
145300070925     C*
145400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
145500080125     C*
145600081020     C                   IF        %trim(VABNZD) = 'I'
145700081020     C                   EVAL      VABNZD = *blanks
145800081020     C                   ENDIF
145900081020     C*
146000081020     C                   EVAL      VABNOT = %trim(%subst(wNOTE:1:35))
146100081020     C                   EVAL      VABNT2 = %trim(%subst(wNOTE:36:35))
146200070925     C*
146300071029     C***  <===  -----------------------------------------
146400070925     C*
146500070925     C* Considerazioni finali su CBO/CAS
146600080125     C                   IF        wFlgCAS = '1'
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
157100080617
157200080617
157300080617
157400080617     C*----------------------------------------------------*
157500080617     C*  CONTROLLO NUMERICITA' CAMPI
157600080617     C*----------------------------------------------------*
157700080617     C     CHKNUM        BEGSR
157800080617     C*
157900080617     C                   IF        PiDecChr = *blanks
158000080617     C                   EVAL      PiDecChr = CharNUM
158100080617     C                   ENDIF
158200080617     C*
158300080617     C                   callp     UBISNUM_Check(PiStr
158400080617     C                                          :PiDecChr
158500080617     C                                          :PiVal
158600080617     C                                          :PiNum
158700080617     C                                          :PiInt)
158800080617     C*
158900080617     C                   ENDSR
159000080617     C***
159100010330
159200010601
159300010601
159400010601
159500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
159600010601     C     repfil        BEGSR
159700010601     C*
159800010601     C                   if        invfil = *zeros and
159900010601     C                             depfil > *zeros and
160000010629     C                             (vinflg = *blanks or
160100010629     C                              vinflg = *zeros)
160200010601     C
160300010601     C                   eval      invfil = depfil
160400010601     C                   endif
160500010601     C*
160600010601     C                   if        depfil <> invfil and
160700010601     C                             invfil > *zeros
160800010601     C                   eval      flgMulti = '1'
160900010601     C                   if        vinflg = *blanks
161000010601     C                   add       1             cntNonEl
161100010601     C                   endif
161200010601     C                   endif
161300010601     C*
161400010601     C                   if        vinflg = '2'
161500010601     C                   eval      flgStato = '2'
161600010601     C                   endif
161700010601     C*
161800010601     C                   ENDSR
161900010601     C***
162000010601
162100010601
162200010601
162300010330
162400040119      /TITLE Invio dei dati al punto operativo.
162500040119     C     invio         BEGSR
162600040119     C*
162700081028     C* 1° invio EDIVAT
162800040119     C                   reset                   dscmz
162900040119     C                   move      vlrpoi        cmzdst
163000081028     C                   eval      cmzfld = 'EDIVATWR'
163100040119     C                   eval      cmzmbd = vlrhdl
163200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
163300040119     C***                if        prmfir = *blanks
163400081028     C                   eval      cmzfla = 'EDIVAT0F'
163500081028     C                   eval      cmzmba = 'EDIVAT0F'
163600040119     C***                else
163700040119     C***                eval      cmzfla = prmfir
163800040119     C***                eval      cmzmba = prmfir
163900040119     C***                endif
164000040119     C                   eval      cmznrr = *zeros
164100040119     C                   move      §ctrokvt      cmznrr
164200040119     C                   eval      cmzlba = vlrfl1
164300040119     C                   call(e)   'TIS711C'
164400040119     C                   parm                    dscmz
164500040119     C                   parm      *blanks       esito
164600040119     C                   if        %error
164700040119     C                             or cmzerr = '1'
164800040119     C                             or esito  = '1'
164900040119     C                   eval      wrkesito = '3'
165000040119     C                   else
165100040119     C*
165200081028     C* 2° invio EDIVAB
165300040119     C                   reset                   dscmz
165400040119     C                   move      vlrpoi        cmzdst
165500040119     C                   eval      cmzfld = vlrfou
165600040119     C                   eval      cmzmbd = vlrhdl
165700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
165800040119     C***                if        prmfir = *blanks
165900081028     C                   eval      cmzfla = 'EDIVAB0F'
166000081028     C                   eval      cmzmba = 'EDIVAB0F'
166100040119     C***                else
166200040119     C***                eval      cmzfla = prmfir
166300040119     C***                eval      cmzmba = prmfir
166400040119     C***                endif
166500040119     C                   eval      cmznrr = *zeros
166600040119     C                   move      §ctrokvb      cmznrr
166700040119     C                   eval      cmzlba = vlrfl1
166800040119     C                   call(e)   'TIS711C'
166900040119     C                   parm                    dscmz
167000040119     C                   parm      *blanks       esito
167100040119     C                   if        %error
167200040119     C                             or cmzerr = '1'
167300040119     C                             or esito  = '1'
167400040119     C                   eval      wrkesito = '3'
167500040119     C                   endif
167600040119     C                   endif
167700040119     C*
167800040119     C                   ENDSR
167900040119     C***
168000010601
168100010601
168200010601
168300010601
168400010601      /TITLE Invio dei dati al punto operativo.
168500010601     C     opeini        BEGSR
168600010601     C*
168700010601     C* Inizializzo flag e contatori operativi
168800010601     C                   movel     '0'           flgGiro           1
168900010601     C                   movel     '0'           flgMulti          1
169000010601     C                   movel     '1'           flgStato          1
169100010615     C                   movel     '0'           flgOk             1
169200010601     C                   z-add     *zeros        cntNonEl         10 0
169300010601     C                   z-add     *zeros        depfil            3 0
169400010601     C                   z-add     *zeros        invfil            3 0
169500010601     C*
169600010601     C                   ENDSR
169700010601     C***
169800070326
169900070326
170000070326
170100070326
170200080916     C     *pssr         BEGSR
170300070329     C*
170400080916     C                   if        %open(tivin00r)
170500080916     C                   close     tivin00r
170600080916     C                   endif
170700081028     C                   if        %open(edivabwr)
170800081028     C                   close     edivabwr
170900080916     C                   endif
171000081028     C                   if        %open(edivatwr)
171100081028     C                   close     edivatwr
171200080916     C                   endif
171300070326     C*
171400070326     C* Effettuo la chiamata al CLLE preposto
171500081028     C                   call(e)   'TITVEVTC'
171600080916     C                   parm                    parccm
171700080916     C                   parm                    parmbr
171800080916     C                   parm      '2'           paropz
171900070326     C*
172000080916     C                   eval      wrkesito = '2'
172100070404     C*
172200080916     C                   seton                                        LR
172300070326     C*
172400080916     C                   ENDSR     '*CANCL'
172500070326     C***
172600070326
172700070326
172800010330
172900010330
173000000613     C     *inzsr        BEGSR
173100990910     C*
173200990910     C     *entry        plist
173300990920     C                   parm                    tivlrds
173400990921     C                   parm      wrkesito      esito
173500000724     C                   parm                    prmlit
173600000710     C                   parm                    prmfir
173700010330     C*
173800010330     C* CALCOLA LA DATA CORRENTE
173900010330     C                   time                    wn14             14 0
174000081028     C                   movel     wn14          oracor            6 0          *ORA
174100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
174200010330     C                   z-add     wn8           g08dat
174300010330     C                   z-add     *zeros        g08inv
174400010330     C                   movel     '0'           g08err
174500010330     C                   call      'XSRDA8'
174600010330     C                   parm                    wlbda8
174700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
174800000613     C*
174900000613     C                   ENDSR
175000000613     C***
175100040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
175200081020FILLE01;FILLE02;FILLER03;W_VABRMN;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;
175300081020VABNZD;W_VABRSD;W_VABIND;W_VABCAD;W_VABLOD;W_VABPRD;W_VABNZD;VABNCL;
175400140512VABPKB;W_VABVLB;W_NOTE;VABTC1;VABCAS;VABTIC;VABFFD;W_VATNOTIJ;W_VATNOT_S;
