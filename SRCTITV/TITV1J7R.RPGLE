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
004700030822     D*------------------
004800030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
004900030822     D*------------------
005000030822     D posDaDft        S              3  0 INZ(*zeros)
005100030822     D posADft         S              3  0 INZ(*zeros)
005200030822     D j               s              3  0 INZ(1)
005300030822     D wGiroDft        s              1  0 INZ(*zeros)
005400010330     D*------------------
005500010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005600010330     D*------------------
005700010330     D WLBDA8          DS                  INZ
005800010330     D  G08DAT                 1      8  0
005900010330     D  G08INV                 9     16  0
006000010330     D  G08ERR                17     17
006100010330     D  G08TGI                18     22  0
006200031201     D*------------------
006300031201     D* Costanti
006400031201     D*------------------
006500031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006600031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006700080617     D*------------------
006800080617     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006900080617     D*------------------
007000080617     D PiStr           S            100A   INZ
007100080617     D PiDecChr        S              1A   INZ
007200080617     D PiVal           S             63S30 INZ
007300080617     D PiInt           S               N   INZ
007400080617     D PiNum           S               N   INZ
007500080617
007600080617     D*------------------
007700080617     D* LINKING A DEFINIZIONI ESTERNE
007800080617     D*------------------
007900080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008000010330
008100010330
008200010330
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500081014     C                   reset                   wrkesito
008600010601     C*
008700010601     C                   exsr      opeini
008800010605     C                   exsr      rwvab
008900081014     C*
009000081014     C                   exsr      endela
009100010601     C*
009200010601     C                   seton                                        lr
009300010601
009400010601
009500010601
009600010601     C*--------------------------------------------------------
009700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009800010601     C*--------------------------------------------------------
009900010601     C     PREELA        BEGSR
010000010601     C*
010100010601     C* SE OCCORRE SPEDIRE IN FILIALE
010200010601     C                   if        invfil <> *zeros and
010300010601     C                             flgGiro = '0'
010400010601     C*
010500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010600010601     C                   eval      flgGiro = '1'
010700010601     C*
010800010601     C                   endif
010900010601     C*
011000010601     C                   ENDSR
011100010601     C***
011200010601
011300010601
011400010601
011500010601
011600010601     C*--------------------------------------------------------
011700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011800010601     C*--------------------------------------------------------
011900010601     C     ENDELA        BEGSR
012000081014     C*
012100081014     C                   if        %open(tivin00r)
012200081014     C                   close     tivin00r
012300081014     C                   endif
012400081028     C                   if        %open(edivabwr)
012500081028     C                   close     edivabwr
012600081014     C                   endif
012700081028     C                   if        %open(edivatwr)
012800081028     C                   close     edivatwr
012900081014     C                   endif
013000040119     C*
013100040119     C* Effettuo la chiamata al CLLE preposto
013200081028     C                   call(e)   'TITVEVTC'
013300040510     C                   parm                    parccm
013400040119     C                   parm                    parmbr
013500040119     C                   parm      '2'           paropz
013600050201     C*
013700050201     C* Effettuo lancio TISI95 solo x chiusura
013800050201     C                   CLEAR                   TISI95DS
013900050201     C                   EVAL      I95TLA = 'C'
014000050201     C                   CALL      'TISI95R'
014100050201     C                   PARM                    TISI95DS
014200000616     C*
014300010601     C                   ENDSR
014400010601     C***
014500000613
014600010601
014700010601
014800010330     C*--------------------------------------------------------
014900081028     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600081028     C                   if        not %open(edivabwr)
015700081028     C                   open      edivabwr
015800010330     C                   endif
015900040119     C*
016000081028     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
016100040119     C                   exsr      prevat
016200040119     C*
016300040119     C                   if        chkcall = '0'
016400040119     C*
016500081028     C                   if        not %open(edivatwr)
016600081028     C                   open      edivatwr
016700040119     C                   endif
016800010330     C*
016900040119     C                   clear                   §CTROKVB          7 0
017000040119     C                   clear                   §CTROKVT          7 0
017100040119     C                   clear                   §CTRMO            7 0
017200040119     C                   clear                   §CTRNO            7 0
017300070823     C*
017400070823     C* Eseguo primo giro x by-pasare la mancanza della 1° riga con le intestazioni
017500070823     C* colonne nel file del cliente (che contiene quindi solo le righe d dati).
017600081016     C***                exsr      inzvar
017700081016     C***                exsr      impvab
017800010330     C*
017900030822     C                   do        *HIVAL
018000010330     C*
018100030822     C                   read      tivin00r                               70
018200010618     C*
018300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
018400010618     C                   if        vinflg = '1'
018500010618     C                   eval      flgOk = '1'
018600010618     C                   endif
018700040510     C*
018800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018900040510     C                   z-add     *zeros        wLenVINDTA        4 0
019000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
019100040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
019200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
019300040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
019400040510     C                   z-add     1             wFlgVINDTA
019500040510     C                   leave
019600040510     C                   endif
019700040510     C                   enddo
019800010618     C*
019900040510     C                   if        vindta > *blanks AND
020000040510     C                             wFlgVINDTA = 1
020100010330     C                   add       1             rrnum
020200010330     C*
020300010601     C                   if        *in70 = *off and
020400010330     C                             (vinflg = *blanks
020500010330     C                              or vinflg = '0'
020600010330     C                              or vinflg = '2')
020700010330     C*
020800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020900010711     C                   if        vinflg = *blanks or vinflg = '0'
021000010711     C                   clear                   vinmsg
021100010711     C                   endif
021200010601     C*
021300070925     C* Aggiungo un byte a blanks all'inizio d ogni record
021400070925     C                   eval      vindta  = ' ' + %trim(vindta)
021500070925     C*
021600010330     C                   exsr      inzvar
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
023600030822     C                   if        wGiro = 2
023700040510     C* Scrivo sempre il VAT
023800040510     C  N31              exsr      wrivat
023900040510     C* Scrivo il VAB solo a rottura d spedizione
024000040119     C                   if        depspe =  *zeros OR
024100040119     C                             depspe <> vabNSP
024200040119     C  N31              exsr      wrivab
024300040119     C  N31              eval      depspe = vabNSP
024400040119     C                   endif
024500040119     C                   endif
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
029400081028     C                   if        %open(edivabwr)
029500081028     C                   close     edivabwr
029600010330     C                   endif
029700081028     C                   if        %open(edivatwr)
029800081028     C                   close     edivatwr
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
030900010330     C*
031000010330     C                   ENDSR
031100010330     C***
031200040119
031300040119
031400040119
031500040119     C*----------------------------------------------------*
031600040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031700040119     C*----------------------------------------------------*
031800040510     C     WRIVAB        BEGSR
031900081028     C*
032000081028     C* VALORIZZO CAMPI RELATIVI AL "CMR"
032100081028     C                   eval      VABCMR = 'AKU ' + %char(datcor)
032200081028     C                   eval      VABDCM = DATCOR
032300081028     C                   eval      VABDTS = DATCOR
032400081028     C                   eval      VABHMS = ORACOR
032500081028     C                   eval      VABCNT = 1
032600040119     C*
032700060331     C* Gestisco l'eventuale rottura x numero spedizione
032800070823     C                   if        VABNSP <> depspe
032900070301     C                   if        wVATNOT_E <> *blanks
033000080125     C                   if        VABCTM = *blanks
033100080125     C                   eval      VABCTM = '7Q'
033200080125     C                   endif
033300070301     C                   endif
033400081028     C                   write     edivab00                                     => scarico il VAB
033500060331     C                   endif
033600040119     C*
033700040119     C                   ENDSR
033800040119
033900040119
034000040119
034100040119     C*----------------------------------------------------*
034200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
034300040119     C*----------------------------------------------------*
034400040510     C     WRIVAT        BEGSR
034500060512     C*
034600060512     C* Inizializzo i campi chiave
034700060512     C                   eval      VATFGS = VABFGS
034800060512     C                   eval      VATAAS = VABAAS
034900060512     C                   eval      VATCCM = VABCCM
035000070823     C                   eval      VATNRS = VABNRS
035100060512     C                   eval      VATNSP = VABNSP
035200060512     C                   eval      VATLNP = VABLNP
035300081028     C*
035400081028     C* Valorizzo i campi relativi al CMR
035500081028     C                   eval      VATCMR = VABCMR
035600081028     C                   eval      VATCNT = VABCNT
035700060331     C*
035800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035900060331     C                   if        wVATNOT_A <> *blanks
036000060331     C                   eval      VATNOT = wVATNOT_A
036100060331     C                   eval      VATTRC = 'A'
036200081028     C                   write     EDIVAT00
036300060331     C                   add       1             §CTROKVT
036400060331     C                   endif
036500060331     C*
036600060331     C                   if        wVATNOT_B <> *blanks
036700060331     C                   eval      VATNOT = wVATNOT_B
036800060331     C                   eval      VATTRC = 'B'
036900081028     C                   write     EDIVAT00
037000060331     C                   add       1             §CTROKVT
037100060331     C                   endif
037200060331     C*
037300080625     C                   if        wVATNOT_E <> *blanks
037400060331     C                   eval      VATNOT = wVATNOT_E
037500060331     C                   eval      VATTRC = 'E'
037600081028     C                   write     EDIVAT00
037700080716     C                   add       1             §CTROKVT
037800080625     C                   endif
037900040119     C*
038000040119     C                   ENDSR
038100040119
038200040119
038300040119
038400040119     C*----------------------------------------------------*
038500081028     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
038600040119     C*----------------------------------------------------*
038700040119     C     PREVAT        BEGSR
038800040119     C*
038900081028     C* Compongo il nome del membro da dare al EDIVATWR
039000040119     C                   eval      parmbr = vlrhdl
039100040119     C                   movel     'M'           parmbr
039200040510     C                   eval      parccm = vlrksc
039300040119     C                   eval      paropz = '1'
039400040119     C* Effettuo la chiamata al CLLE preposto
039500081028     C                   call(e)   'TITVEVTC'
039600040119     C                   parm                    parccm
039700040119     C                   parm                    parmbr
039800040119     C                   parm                    paropz
039900040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
040000040119     C                   if        %error
040100040119     C                   movel     '1'           chkcall
040200040119     C                   else
040300040119     C                   movel     '0'           chkcall
040400040119     C                   endif
040500040119     C*
040600040119     C                   ENDSR
040700010601
040800010601
040900010601
041000010330     C*----------------------------------------------------*
041100030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
041200010330     C*----------------------------------------------------*
041300040119     C     INZVAR        BEGSR
041400010330     C*
041500030822     C* Inizializzo variabili di wrk
041600020204     C                   Z-ADD     *zeros        Num5_0            5 0
041700081020     C                   MOVEL     *blanks       wNOTE            70
041800060331     C                   MOVEL     *blanks       wVATNOT_A
041900060331     C                   MOVEL     *blanks       wVATNOT_B
042000060331     C                   MOVEL     *blanks       wVATNOT_E
042100080617     C                   EVAL      wFlgCAS = *blanks
042200030822     C*
042300030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042400081028     C                   CLEAR                   EDIVAB00
042500081028     C                   CLEAR                   EDIVAT00
042600030822     C                   CLEAR                   SkSplitCSV
042700030822     C                   EVAL      i = 1
042800030822     C                   EVAL      posDa = *zeros
042900030822     C                   EVAL      posA  = *zeros
043000030822     C*
043100030822     C* Reimposto i valori di default
043200030822     C                   EXSR      DEFCAM
043300030822     C*
043400010330     C                   ENDSR
043500010330     C*----------------------------------------------------*
043600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043700010330     C*----------------------------------------------------*
043800010330     C     DEFCAM        BEGSR
043900030822     C*
044000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
044100030715     C* e delimitatore testo.
044200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
044300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044500030822     C*
044600030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
044700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044800030715     C                   SETON                                        50
044900030715     C                   ELSE
045000030715     C                   SETOFF                                       50
045100030715     C                   ENDIF
045200030822     C*
045300030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045400030822     C                   EVAL      posDaDft = 1
045500030822     C                   EVAL      posADft  = 0
045600030822     C                   EVAL      wGiroDft = 0
045700030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
045800030822     C                             posDaDft > 0
045900030822     C*
046000030822     C* Gestisco il 1° giro
046100030822     C                   IF        wGiroDft = 0
046200030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
046300030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046400030822     C* Incremento il contatore dei "giri"
046500030822     C                   EVAL      wGiroDft = 1
046600030822     C                   ELSE
046700030822     C                   EVAL      posDaDft = posADft
046800030822     C                   ENDIF
046900030822     C* Eseguo lo scan x trovare la fine del campo corrente
047000030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
047100030822     C*
047200030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
047300030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047400030822     C                   IF        posDaDft > 0 AND
047500030822     C                             posADft  > 0
047600030822     C* NCL
047700030822     C                   IF        %subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):1:3)
048000030822     C                             = 'NCL'
048100030822     C                   EVAL      PiStr=%trim(%subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048400030822     C                   EXSR      CHKNUM
048500030822     C                   IF        PiInt=*on
048600030822     C                   Z-ADD     PiVal         VABNCL
048700030822     C                   ENDIF
048800030822     C                   ENDIF
048900030822     C* CCM
049000030822     C                   IF        %subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):1:3)
049300030822     C                             = 'CCM'
049400030822     C                   EVAL      PiStr=%trim(%subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049700030822     C                   EXSR      CHKNUM
049800030822     C                   IF        PiInt=*on
049900030822     C                   Z-ADD     PiVal         VABCCM
050000030822     C                   ENDIF
050100030822     C                   ENDIF
050200030822     C* LNP
050300030822     C                   IF        %subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):1:3)
050600030822     C                             = 'LNP'
050700030822     C                   EVAL      PiStr=%trim(%subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051000030822     C                   EXSR      CHKNUM
051100030822     C                   IF        PiInt=*on
051200030822     C                   Z-ADD     PiVal         VABLNP
051300030822     C                   ENDIF
051400030822     C                   ENDIF
051500030822     C* NRS
051600030822     C                   IF        %subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):1:3)
051900030822     C                             = 'NRS'
052000030822     C                   EVAL      PiStr=%trim(%subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052300030822     C                   EXSR      CHKNUM
052400030822     C                   IF        PiInt=*on
052500030822     C                   Z-ADD     PiVal         VABNRS
052600030822     C                   ENDIF
052700030822     C                   ENDIF
052800030822     C* CTR
052900030822     C                   IF        %subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):1:3)
053200030822     C                             = 'CTR'
053300030822     C                   EVAL      PiStr=%trim(%subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053600030822     C                   EXSR      CHKNUM
053700030822     C                   IF        PiInt=*on
053800030822     C                   Z-ADD     PiVal         VABCTR
053900030822     C                   ENDIF
054000030822     C                   ENDIF
054100030822     C* PKB
054200030822     C                   IF        %subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):1:3)
054500030822     C                             = 'PKB'
054600030822     C                   EVAL      PiStr=%trim(%subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054900030822     C                   EXSR      CHKNUM
055000030822     C                   IF        PiNum=*on
055100030822     C                   Z-ADD     PiVal         VABPKB
055200030822     C                   ENDIF
055300030822     C                   ENDIF
055400030822     C* VLB
055500030822     C                   IF        %subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):1:3)
055800030822     C                             = 'VLB'
055900030822     C                   EVAL      PiStr=%trim(%subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056200030822     C                   EXSR      CHKNUM
056300030822     C                   IF        PiNum=*on
056400030822     C                   Z-ADD     PiVal         VABVLB
056500030822     C                   ENDIF
056600030822     C                   ENDIF
056700030822     C* QFT
056800030822     C                   IF        %subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):1:3)
057100030822     C                             = 'QFT'
057200030822     C                   EVAL      PiStr=%trim(%subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057500030822     C                   EXSR      CHKNUM
057600030822     C                   IF        PiNum=*on
057700030822     C                   Z-ADD     PiVal         VABQFT
057800030822     C                   ENDIF
057900030822     C                   ENDIF
058000030822     C* CBO
058100030822     C                   IF        %subst(
058200030822     C                             %subst(vlrppt:posDaDft+1:
058300030822     C                             posADft-posDaDft-1):1:3)
058400030822     C                             = 'CBO'
058500030822     C                   EVAL      VABCBO=%trim(%subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058800030822     C                   ENDIF
058900030822     C* TSP
059000030822     C                   IF        %subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):1:3)
059300030822     C                             = 'TSP'
059400030822     C                   EVAL      VABTSP=%trim(%subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059700030822     C                   ENDIF
059800030822     C* VAS
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'VAS'
060300030822     C                   EVAL      VABVAS=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   ENDIF
060700030822     C* VCA
060800030822     C                   IF        %subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):1:3)
061100030822     C                             = 'VCA'
061200030822     C                   EVAL      VABVCA=%trim(%subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061500030822     C                   ENDIF
061600070925     C* TIC
061700070925     C                   IF        %subst(
061800070925     C                             %subst(vlrppt:posDaDft+1:
061900070925     C                             posADft-posDaDft-1):1:3)
062000070925     C                             = 'TIC'
062100070925     C                   EVAL      VABTIC=%trim(%subst(
062200070925     C                             %subst(vlrppt:posDaDft+1:
062300070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400070925     C                   ENDIF
062500030822     C* GCA
062600030822     C                   IF        %subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):1:3)
062900030822     C                             = 'GCA'
063000030822     C                   EVAL      VABGCA=%trim(%subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063300030822     C                   ENDIF
063400030822     C* CTM
063500030822     C                   IF        %subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):1:3)
063800030822     C                             = 'CTM'
063900030822     C                   EVAL      VABCTM=%trim(%subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064200030822     C                   ENDIF
064300030822     C* FFD
064400030822     C                   IF        %subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):1:3)
064700030822     C                             = 'FFD'
064800030822     C                   EVAL      VABFFD=%trim(%subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065100030822     C                   ENDIF
065200030822     C* VAD
065300030822     C                   IF        %subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):1:3)
065600030822     C                             = 'VAD'
065700030822     C                   EVAL      VABVAD=%trim(%subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066000030822     C                   ENDIF
066100030822     C* GMA
066200030822     C                   IF        %subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):1:3)
066500030822     C                             = 'GMA'
066600030822     C                   EVAL      VABGMA=%trim(%subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066900030822     C                   ENDIF
067000030822     C* GGA
067100030822     C                   IF        %subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):1:3)
067400030822     C                             = 'GGA'
067500030822     C                   EVAL      VABGGA=%trim(%subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067800030822     C                   ENDIF
067900030822     C* GVA
068000030822     C                   IF        %subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):1:3)
068300030822     C                             = 'GVA'
068400030822     C                   EVAL      VABGVA=%trim(%subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068700030822     C                   ENDIF
068800030822     C* TC1
068900030822     C                   IF        %subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):1:3)
069200030822     C                             = 'TC1'
069300030822     C                   EVAL      VABTC1=%trim(%subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069600030822     C                   ENDIF
069700030822     C* TC2
069800030822     C                   IF        %subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):1:3)
070100030822     C                             = 'TC2'
070200030822     C                   EVAL      VABTC2=%trim(%subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070500030822     C                   ENDIF
070600040714     C* VATTRC
070700040714     C                   IF        %subst(
070800040714     C                             %subst(vlrppt:posDaDft+1:
070900040714     C                             posADft-posDaDft-1):1:3)
071000040714     C                             = 'TRC'
071100040714     C                   EVAL      VATTRC=%trim(%subst(
071200040714     C                             %subst(vlrppt:posDaDft+1:
071300040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071400040714     C                   ENDIF
071500030822     C* ...
071600030822     C                   ENDIF
071700030822     C                   ENDDO
071800020204     C*
071900010330     C                   ENDSR
072000010607     C*----------------------------------------------------*
072100081028     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
072200010607     C*----------------------------------------------------*
072300010607     C     IMPVAB        BEGSR
072400010607     C*
072500010607     C                   SETOFF                                       3132
072600030515     C*
072700010607     C                   MOVEL     datcor        VABAAS
072800010607     C                   MOVE      datcor        VABMGS
072900010607     C*
073000030715     C**********
073100030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073200030715     C**********
073300030715     C                   IF        wGiro = *zeros
073400080716     C*
073500080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
073600080716     C                   EVAL      depvinDTA = vinDTA
073700040113     C*
073800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
073900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
074000040510     C                             %trim(intcol(3))<>*blanks
074100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074200040113     C                                      %trim(intcol(3))
074300070823     C     ';':CharCSV   XLATE     vindta        vindta
074400040510     C                   ENDIF
074500080626     C*
074600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074700031201     C     minu:maiu     XLATE     vindta        vindta
074800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074900030715     C                   DOW       posDa <= %len(%trim(vindta))
075000030715     C*
075100030715     C* Gestisco il 1° campo
075200030715     C                   IF        i = 1
075300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075400030715     C                   EVAL      posDa = 1
075500030715     C* Eseguo lo scan x trovare la fine del primo campo
075600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075700030715     C* A questo punto "estraggo" il campo corrente
075800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075900030715     C                                                    (posA-posDa))
076000030715     C* X i campi successivi al 1°
076100030715     C                   ELSE
076200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076300030715     C                   EVAL      posDa = posA + 1
076400030715     C* Eseguo lo scan x trovare la fine del campo corrente
076500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076600030715     C* Gestisco l'ultimo campo
076700030715     C                   IF        posA = *zeros
076800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076900030715     C* A questo punto "estraggo" il campo corrente
077000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077100030715     C                                                    (posA-posDa))
077200030715     C                   LEAVE
077300030715     C                   ELSE
077400030715     C* A questo punto "estraggo" il campo corrente
077500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077600030715     C                                                    (posA-posDa))
077700030715     C                   ENDIF
077800030715     C                   ENDIF
077900030715     C* Incremento il contatore d campo
078000030715     C                   EVAL      i = i +1
078100030715     C                   ENDDO
078200030715     C*
078300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078400030715     C* delimitazione testo
078500030715     C                   EVAL      i = 1
078600030715     C                   DOW       i < %elem(SkSplitFLD)
078700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078900030715     C                   EVAL      i = i + 1
079000030715     C                   ENDDO
079100030715     C                   EVAL      wGiro = 1
079200080716     C* Ripristino la 1° riga originale
079300080716     C                   EVAL      vinDTA = depvinDTA
079400030715     C                   ELSE
079500030715     C                   EVAL      wGiro = 2
079600031204     C**********
079700031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079800031204     C* d separatore campo
079900031204     C**********
080000031204     C                   EVAL      vindta = %trim(vindta)
080100031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
080200031204     C                   EVAL      vindta = %subst(vindta:2)
080300031204     C                   ENDDO
080400031204     C*
080500031204     C                   Z-ADD     *zeros        lunghInput        4 0
080600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080700031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080800031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
081000031204     C                   ENDDO
081100030715     C**********
081200030715     C* Effettuo lo split del campo dati d input
081300030715     C**********
081400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081500030715     C                   DOW       posDa <= %len(%trim(vindta))
081600030715     C*
081700030715     C* Gestisco il 1° campo
081800030715     C                   IF        i = 1
081900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082000030715     C                   EVAL      posDa = 1
082100030715     C* Eseguo lo scan x trovare la fine del primo campo
082200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082300030715     C* A questo punto "estraggo" il campo corrente
082400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082500030715     C                                                    (posA-posDa))
082600030715     C* X i campi successivi al 1°
082700030715     C                   ELSE
082800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082900030715     C                   EVAL      posDa = posA + 1
083000031204     C* Verifico che nn vi sia il campo nn valorizzato
083100031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
083200031204     C* Se campo nn valorizzato skippo al prossimo
083300031204     C                   EVAL      SkSplitCSV(i) = *blanks
083400031204     C                   EVAL      posA  = posA + 1
083500031204     C                   ELSE
083600030715     C* Eseguo lo scan x trovare la fine del campo corrente
083700030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083800030715     C* Gestisco l'ultimo campo
083900030715     C                   IF        posA = *zeros
084000030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
084100030715     C* A questo punto "estraggo" il campo corrente
084200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084300030715     C                                                    (posA-posDa))
084400030715     C                   LEAVE
084500030715     C                   ELSE
084600030715     C* A questo punto "estraggo" il campo corrente
084700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084800030715     C                                                    (posA-posDa))
084900030715     C                   ENDIF
085000031204     C                   ENDIF
085100030715     C                   ENDIF
085200030715     C* Incremento il contatore d campo
085300030715     C                   EVAL      i = i +1
085400030715     C                   ENDDO
085500030715     C*
085600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085700030715     C* delimitazione testo
085800030715     C                   EVAL      i = 1
085900030715     C                   DOW       i < %elem(SkSplitCSV)
086000030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
086100030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
086200030715     C                   EVAL      i = i + 1
086300030715     C                   ENDDO
086400030715     C*
086500030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086600030715     C                   EVAL      i = 1
086700030715     C                   DOW       i < %elem(SkSplitFLD)
086800030715     C*
086900030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
087000030715     C                   IF        SkSplitFLD(i) <> *blanks AND
087100030715     C                             SkSplitCSV(i) <> *blanks
087200030721     C*** ATB
087300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087400030721     C                   EVAL      VABATB = SkSplitCSV(i)
087500030721     C                   ENDIF
087600030715     C*** RSD
087700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087800030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
088000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
088100030715     C* ==
088200030715     C                   ENDIF
088300030721     C*** RD2
088400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088600030721     C                   ENDIF
088700030715     C*** IND
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088900030715     C                   EVAL      VABIND = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** LOD
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030715     C*** PRD
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030721     C*** NZD
090000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
090100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
090200030721     C                   ENDIF
090300030715     C*** RMA
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** NOT
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090900030715     C                   EVAL      VABNOT = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** NT2
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091300030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** VCA
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091700030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030715     C*** VAS
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
092100030715     C                   EVAL      VABVAS = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030715     C*** FFD
092400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092500030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092600030715     C                   ENDIF
092700030721     C*** GC1
092800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092900030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
093000030721     C                   ENDIF
093100030721     C*** GC2
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** TSP
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
093700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
093800030721     C                   ENDIF
093900040510     C*** CBO
094000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
094100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
094200040510     C                   ENDIF
094300030721     C*** NAS
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** TIC
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
094900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** GCA
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** XCO
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
095700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** CTM
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** TCR
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** CTS
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
096900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** FTM
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** VAD
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
097700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** GMA
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GGA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** GVA
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
098900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** TC1
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TC2
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
099700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** SCL
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** RMO
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** NMO
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
100900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*
101200030721     C* Reperisco quindi i campi numerici...
101300040510     C*** NSP
101400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101500040510     C                   EVAL      PiStr=SkSplitCSV(i)
101600040510     C                   EXSR      CHKNUM
101700040510     C                   IF        PiInt=*on
101800040510     C                   Z-ADD     PiVal         VABNSP
101900040510     C                   ELSE
102000040510     C                   SETON                                        31
102100040510     C                   EVAL      VABNSP = *zeros
102200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
102300040510     C                             + ' ' + 'VABNSP'
102400040510     C                   ENDIF
102500040510     C                   ENDIF
102600071106     C*** RMN
102700071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
102800071106     C                   EVAL      PiStr=SkSplitCSV(i)
102900071106     C                   EXSR      CHKNUM
103000071106     C                   IF        PiInt=*on
103100071106     C                   Z-ADD     PiVal         VABRMN
103200071106     C                   ELSE
103300071106     C                   SETON                                        32
103400071106     C                   EVAL      VABRMN = *zeros
103500071106     C                   EVAL      vinmsg = %trimr(vinmsg)
103600071106     C                             + ' ' + 'VABRMN'
103700071106     C                   ENDIF
103800071106     C                   ENDIF
103900030721     C*** CAD
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104100030721     C                   EVAL      PiStr=SkSplitCSV(i)
104200030721     C                   EXSR      CHKNUM
104300030721     C                   IF        PiInt=*on
104400030721     C                   Z-ADD     PiVal         Num5_0
104500030721     C                   MOVEL(p)  Num5_0        VABCAD
104600030721     C                   ELSE
104700030721     C                   SETON                                        32
104800030721     C                   EVAL      VABCAD = *zeros
104900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105000030721     C                             + ' ' + 'VABCAD'
105100030721     C                   ENDIF
105200030721     C                   ENDIF
105300030721     C*** NCL
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
105500030721     C                   EVAL      PiStr=SkSplitCSV(i)
105600030721     C                   EXSR      CHKNUM
105700030721     C                   IF        PiInt=*on
105800030721     C                   Z-ADD     PiVal         VABNCL
105900030721     C                   ELSE
106000030721     C                   SETON                                        32
106100030721     C                   EVAL      VABNCL = *zeros
106200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106300040122     C                             + ' ' + 'VABNCL'
106400030721     C                   ENDIF
106500030721     C                   ENDIF
106600030721     C*** PKB
106700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106800030721     C                   EVAL      PiStr=SkSplitCSV(i)
106900030721     C                   EXSR      CHKNUM
107000030721     C                   IF        PiNum=*on
107100030721     C                   Z-ADD     PiVal         VABPKB
107200030721     C                   ELSE
107300030721     C                   SETON                                        32
107400030721     C                   EVAL      VABPKB = *zeros
107500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107600030721     C                             + ' ' + 'VABPKB'
107700030721     C                   ENDIF
107800030721     C                   ENDIF
107900030721     C*** CAS
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
108100080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
108200080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
108300081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
108400081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
108500080125     C                   EVAL      wFlgCAS = '1'
108600080125     C                   ENDIF
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiNum=*on
109000030721     C                   Z-ADD     PiVal         VABCAS
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABCAS = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500030721     C                             + ' ' + 'VABCAS'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** CCM
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiInt=*on      AND
110300030721     C                             PiVal<=9999999 AND
110400030721     C                             PiVal>*zeros
110500030721     C                   Z-ADD     PiVal         VABCCM
110600030721     C                   ELSE
110700030721     C                   SETON                                        32
110800030721     C                   EVAL      VABCCM = *zeros
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000040510     C                             + ' ' + 'VABCCM'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** LNP
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on    AND
111800030721     C                             PiVal<=999   AND
111900030721     C                             PiVal>*zeros
112000030721     C                   Z-ADD     PiVal         VABLNP
112100030721     C                   ELSE
112200040715     C                   SETON                                        32
112300030721     C                   EVAL      VABLNP = *zeros
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABLNP'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** NRS
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on AND
113300030721     C                             PiVal<=99
113400030721     C                   Z-ADD     PiVal         VABNRS
113500030721     C                   ELSE
113600030721     C                   SETON                                        32
113700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113800030721     C                             + ' ' + 'VABNRS'
113900030721     C                   ENDIF
114000030721     C                   ENDIF
114100030721     C*** LNA
114200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
114300030721     C                   EVAL      PiStr=SkSplitCSV(i)
114400030721     C                   EXSR      CHKNUM
114500030721     C                   IF        PiInt=*on    AND
114600030721     C                             PiVal<=999   AND
114700030721     C                             PiVal>*zeros
114800030721     C                   Z-ADD     PiVal         VABLNA
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      VABLNA = *zeros
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABLNA'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** CTR
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on  AND
116100030721     C                             PiVal<=999
116200030721     C                   Z-ADD     PiVal         VABCTR
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABCTR'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** IAS
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiNum=*on
117400030721     C                   Z-ADD     PiVal         VABIAS
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABIAS'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** VLB
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABVLB
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABVLB'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** QFT
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiNum=*on
119800030721     C                   Z-ADD     PiVal         VABQFT
119900030721     C                   ELSE
120000030721     C                   SETON                                        32
120100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120200030721     C                             + ' ' + 'VABQFT'
120300030721     C                   ENDIF
120400030721     C                   ENDIF
120500030721     C*** NCD
120600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120700030721     C                   EVAL      PiStr=SkSplitCSV(i)
120800030721     C                   EXSR      CHKNUM
120900030721     C                   IF        PiInt=*on      AND
121000030721     C                             PiVal<=9999999
121100030721     C                   Z-ADD     PiVal         VABNCD
121200030721     C                   ELSE
121300030721     C                   SETON                                        32
121400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121500030721     C                             + ' ' + 'VABNCD'
121600030721     C                   ENDIF
121700030721     C                   ENDIF
121800030721     C*** NCA
121900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
122000030721     C                   EVAL      PiStr=SkSplitCSV(i)
122100030721     C                   EXSR      CHKNUM
122200030721     C                   IF        PiInt=*on      AND
122300030721     C                             PiVal<=9999999
122400030721     C                   Z-ADD     PiVal         VABNCA
122500030721     C                   ELSE
122600030721     C                   SETON                                        32
122700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122800030721     C                             + ' ' + 'VABNCA'
122900030721     C                   ENDIF
123000030721     C                   ENDIF
123100030721     C*** ZNC
123200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
123300030721     C                   EVAL      PiStr=SkSplitCSV(i)
123400030721     C                   EXSR      CHKNUM
123500030721     C                   IF        PiInt=*on AND
123600030721     C                             PiVal<=99
123700030721     C                   Z-ADD     PiVal         VABZNC
123800030721     C                   ELSE
123900030721     C                   SETON                                        32
124000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124100030721     C                             + ' ' + 'VABZNC'
124200030721     C                   ENDIF
124300030721     C                   ENDIF
124400030721     C*** DCR
124500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
124600030721     C                   EVAL      PiStr=SkSplitCSV(i)
124700030721     C                   EXSR      CHKNUM
124800030721     C                   IF        PiInt=*on       AND
124900030721     C                             PiVal<=99999999
125000030721     C                   Z-ADD     PiVal         VABDCR
125100030721     C                   ELSE
125200030721     C                   SETON                                        32
125300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125400030721     C                             + ' ' + 'VABDCR'
125500030721     C                   ENDIF
125600030721     C                   ENDIF
125700030721     C*** HCR
125800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125900030721     C                   EVAL      PiStr=SkSplitCSV(i)
126000030721     C                   EXSR      CHKNUM
126100030721     C                   IF        PiInt=*on   AND
126200030721     C                             PiVal<=9999
126300030721     C                   Z-ADD     PiVal         VABHCR
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABHCR'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** VMD
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiNum=*on
127500030721     C                   Z-ADD     PiVal         VABVMD
127600030721     C                   ELSE
127700030721     C                   SETON                                        32
127800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127900030721     C                             + ' ' + 'VABVMD'
128000030721     C                   ENDIF
128100030721     C                   ENDIF
128200030721     C*** ANT
128300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
128400030721     C                   EVAL      PiStr=SkSplitCSV(i)
128500030721     C                   EXSR      CHKNUM
128600030721     C                   IF        PiInt=*on        AND
128700030721     C                             PiVal<=999999999
128800030721     C                   Z-ADD     PiVal         VABANT
128900030721     C                   ELSE
129000030721     C                   SETON                                        32
129100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129200030721     C                             + ' ' + 'VABANT'
129300030721     C                   ENDIF
129400030721     C                   ENDIF
129500030721     C*** CMO
129600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129700030721     C                   EVAL      PiStr=SkSplitCSV(i)
129800030721     C                   EXSR      CHKNUM
129900030721     C                   IF        PiInt=*on
130000030721     C                   Z-ADD     PiVal         Num5_0
130100030721     C                   MOVEL(p)  Num5_0        VABCMO
130200030721     C                   ELSE
130300030721     C                   SETON                                        32
130400030721     C                   EVAL      VABCMO = *zeros
130500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130600030721     C                             + ' ' + 'VABCMO'
130700030721     C                   ENDIF
130800030721     C                   ENDIF
130900040510     C*
131000081028     C* Reperisco i campi del EDIVAT
131100040510     C*** VATNOT
131200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
131300040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
131400040510     C                   ENDIF
131500040510     C*** VATTRC
131600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
131700040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
131800040510     C                   ENDIF
131900060331     C*** VATNOT_A
132000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
132100060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
132200060331     C                   ENDIF
132300060331     C*** VATNOT_B
132400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
132500060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
132600060331     C                   ENDIF
132700060331     C*** VATNOT_E
132800070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
132900060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
133000060331     C                   ENDIF
133100070823     C*
133200071029     C***  ===>  Gestione campi "particolari" dentro ciclo
133300081020     C*
133400081020     C*** RSD
133500081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRSD'
133600081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
133700081020     C                   EVAL      VABRSD = SkSplitCSV(i)
133800081020     C                   ENDIF
133900081020     C                   ENDIF
134000081020     C*** IND
134100081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABIND'
134200081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
134300081020     C                   EVAL      VABIND = SkSplitCSV(i)
134400081020     C                   ENDIF
134500081020     C                   ENDIF
134600081020     C*** LOD
134700081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABLOD'
134800081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
134900081020     C                   EVAL      VABLOD = SkSplitCSV(i)
135000081020     C                   ENDIF
135100081020     C                   ENDIF
135200081020     C*** PRD
135300081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABPRD'
135400081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
135500081020     C                   EVAL      VABPRD = SkSplitCSV(i)
135600081020     C                   ENDIF
135700081020     C                   ENDIF
135800081020     C*** NZD
135900081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNZD'
136000081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136100081020     C                   EVAL      VABNZD = SkSplitCSV(i)
136200081020     C                   ENDIF
136300081020     C                   ENDIF
136400081020     C*** CAD
136500081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAD'
136600081020     C                   IF        %trim(SkSplitCSV(i)) <> *blanks
136700081020     C                   EVAL      PiStr=SkSplitCSV(i)
136800081020     C                   EXSR      CHKNUM
136900081020     C                   IF        PiInt=*on
137000081020     C                   Z-ADD     PiVal         Num5_0
137100081020     C                   MOVEL(p)  Num5_0        VABCAD
137200081020     C                   ELSE
137300081020     C                   SETON                                        32
137400081020     C                   EVAL      VABCAD = *zeros
137500081020     C                   EVAL      vinmsg = %trimr(vinmsg)
137600081020     C                             + ' ' + 'VABCAD'
137700081020     C                   ENDIF
137800081020     C                   ENDIF
137900081020     C                   ENDIF
138000081020     C*** RMN
138100081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
138200081020     C                   EVAL      VABRMA=SkSplitCSV(i)
138300081020     C                   EVAL      PiStr=SkSplitCSV(i)
138400081020     C                   EXSR      CHKNUM
138500081020     C                   IF        PiInt=*on
138600081020     C                   Z-ADD     PiVal         VABRMN
138700081020     C                   Z-ADD     PiVal         VABNSP
138800081020     C                   ELSE
138900081020     C                   SETON                                        32
139000081020     C                   EVAL      VABRMN = *zeros
139100081020     C                   EVAL      vinmsg = %trimr(vinmsg)
139200081020     C                             + ' ' + 'VABRMN VABNSP'
139300081020     C                   ENDIF
139400081020     C                   ENDIF
139500081020     C*** NOT+NT2
139600081020     C                   IF        %trim(SkSplitFLD(i)) = 'W_NOTE'
139700081020     C                   EVAL      wNOTE=SkSplitCSV(i)
139800081028     C                   ENDIF
139900081028     C*** VLB
140000081028     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABVLB'
140100081028     C                   EVAL      PiStr=SkSplitCSV(i)
140200081028     C                   EXSR      CHKNUM
140300081028     C                   IF        PiNum=*on
140400090127     C***                EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
140500081028     C                   Z-ADD(H)  PiVal         VABVLB
140600081105     C                   Z-ADD(H)  PiVal         VABQFT
140700081028     C                   ELSE
140800081028     C                   SETON                                        32
140900081028     C                   EVAL      vinmsg = %trimr(vinmsg)
141000081105     C                             + ' ' + 'VABVLB VABQFT'
141100081028     C                   ENDIF
141200081028     C                   ENDIF
141300070925     C*
141400071029     C***  <===  -----------------------------------------
141500040510     C*
141600030715     C                   ENDIF
141700030715     C                   EVAL      i = i + 1
141800030715     C                   ENDDO
141900070925     C*
142000071029     C***  ===>  Gestione campi "particolari" fuori ciclo
142100080125     C*
142200081020     C                   IF        %trim(VABNZD) = 'I'
142300081020     C                   EVAL      VABNZD = *blanks
142400081020     C                   ENDIF
142500081020     C*
142600081020     C                   EVAL      VABNOT = %trim(%subst(wNOTE:1:35))
142700081020     C                   EVAL      VABNT2 = %trim(%subst(wNOTE:36:35))
142800070925     C*
142900071029     C***  <===  -----------------------------------------
143000070925     C*
143100070925     C* Considerazioni finali su CBO/CAS
143200080125     C                   IF        wFlgCAS = '1'
143300070925     C                   IF        VABCBO = '1'
143400070925     C                   EVAL      VABCBO = '4'
143500070925     C                   ENDIF
143600070925     C                   IF        VABCBO = '2'
143700070925     C                   EVAL      VABCBO = '6'
143800070925     C                   ENDIF
143900070925     C                   ENDIF
144000070925     C***  <===  ----------------------------
144100040510     C*
144200040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
144300040510     C                   IF        VABNSP = *zeros
144400040510     C                   SETON                                        31
144500040510     C                   ENDIF
144600040715     C*
144700040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
144800040715     C                   IF        VABLNP = *zeros
144900040715     C                   SETON                                        31
145000040715     C                   ENDIF
145100040510     C*
145200040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
145300040510     C                   IF        VABRMN = *zeros
145400040510     C                   Z-ADD     VABNSP        VABRMN
145500040510     C                   ENDIF
145600040510     C*
145700040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
145800040510     C                   IF        VABRMA = *blanks
145900040714     C                   MOVEL     VABRMN        VABRMA
146000040510     C                   ENDIF
146100040128     C*
146200040510     C* Se provincia nn valorizzata la reperisco
146300040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146400040510     C                   IF        VABPRD = *blanks
146500040128     C                   CLEAR                   TISI95DS
146600040128     C                   EVAL      I95TCN = '3'
146700040128     C                   Z-ADD     datcor        I95DAT
146800040128     C                   EVAL      I95NAR = VABNZD
146900040128     C                   EVAL      I95CAP = VABCAD
147000040128     C                   EVAL      I95LOC = VABLOD
147100040128     C                   CALL      'TISI95R'
147200040128     C                   PARM                    TISI95DS
147300040128     C                   EVAL      VABPRD = O95PRV
147400040510     C                   ENDIF
147500020204     C*
147600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147700020204     C                   EXSR      CHKIMPDIV
147800030715     C                   ENDIF
147900020204     C*
148000010607     C                   ENDSR
148100010607     C*----------------------------------------------------*
148200010601
148300020204
148400020204     C*----------------------------------------------------*
148500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148600020204     C*----------------------------------------------------*
148700020204     C     CHKIMPDIV     BEGSR
148800020204     C*
148900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
149000020204     C                   Z-ADD     *zeros        wrkDec            9 9
149100020204     C*
149200020204     C* Come prima cosa effettuo considerazioni sulla divisa
149300020204     C                   IF        vabIAS > *zeros
149400020204     C                   IF        vabVAS <> 'EUR'
149500020204     C                   EVAL      vabVAS =  'ITL'
149600020204     C                   ENDIF
149700020204     C                   ENDIF
149800020204     C*
149900020204     C                   IF        vabCAS > *zeros
150000020204     C                   IF        vabVCA <> 'EUR'
150100020204     C                   EVAL      vabVCA =  'ITL'
150200020204     C                   ENDIF
150300020204     C                   ENDIF
150400020204     C*
150500020204     C                   IF        vabVMD > *zeros
150600020204     C                   IF        vabVAD <> 'EUR'
150700020204     C                   EVAL      vabVAD =  'ITL'
150800020204     C                   ENDIF
150900020204     C                   ENDIF
151000020204     C*
151100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
151200020204     C                   Z-ADD     vabIAS        wrkDec
151300020204     C                   IF        wrkDec > *zeros
151400020204     C                   IF        vabVAS = 'ITL'
151500020204     C                   EVAL      vabIAS = *zeros
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
152000020204     C                   Z-ADD     vabCAS        wrkDec
152100020204     C                   IF        wrkDec > *zeros
152200020204     C                   IF        vabVCA = 'ITL'
152300020204     C                   EVAL      vabCAS = *zeros
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152800020204     C                   Z-ADD     vabVMD        wrkDec
152900020204     C                   IF        wrkDec > *zeros
153000020204     C                   IF        vabVAD = 'ITL'
153100020204     C                   EVAL      vabVMD = *zeros
153200020204     C                   ENDIF
153300020204     C                   ENDIF
153400020204     C*
153500020204     C                   ENDSR
153600020204     C***
153700080617
153800080617
153900080617
154000080617     C*----------------------------------------------------*
154100080617     C*  CONTROLLO NUMERICITA' CAMPI
154200080617     C*----------------------------------------------------*
154300080617     C     CHKNUM        BEGSR
154400080617     C*
154500080617     C                   IF        PiDecChr = *blanks
154600080617     C                   EVAL      PiDecChr = CharNUM
154700080617     C                   ENDIF
154800080617     C*
154900080617     C                   callp     UBISNUM_Check(PiStr
155000080617     C                                          :PiDecChr
155100080617     C                                          :PiVal
155200080617     C                                          :PiNum
155300080617     C                                          :PiInt)
155400080617     C*
155500080617     C                   ENDSR
155600080617     C***
155700010330
155800010601
155900010601
156000010601
156100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156200010601     C     repfil        BEGSR
156300010601     C*
156400010601     C                   if        invfil = *zeros and
156500010601     C                             depfil > *zeros and
156600010629     C                             (vinflg = *blanks or
156700010629     C                              vinflg = *zeros)
156800010601     C
156900010601     C                   eval      invfil = depfil
157000010601     C                   endif
157100010601     C*
157200010601     C                   if        depfil <> invfil and
157300010601     C                             invfil > *zeros
157400010601     C                   eval      flgMulti = '1'
157500010601     C                   if        vinflg = *blanks
157600010601     C                   add       1             cntNonEl
157700010601     C                   endif
157800010601     C                   endif
157900010601     C*
158000010601     C                   if        vinflg = '2'
158100010601     C                   eval      flgStato = '2'
158200010601     C                   endif
158300010601     C*
158400010601     C                   ENDSR
158500010601     C***
158600010601
158700010601
158800010601
158900010330
159000040119      /TITLE Invio dei dati al punto operativo.
159100040119     C     invio         BEGSR
159200040119     C*
159300081028     C* 1° invio EDIVAT
159400040119     C                   reset                   dscmz
159500040119     C                   move      vlrpoi        cmzdst
159600081028     C                   eval      cmzfld = 'EDIVATWR'
159700040119     C                   eval      cmzmbd = vlrhdl
159800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159900040119     C***                if        prmfir = *blanks
160000081028     C                   eval      cmzfla = 'EDIVAT0F'
160100081028     C                   eval      cmzmba = 'EDIVAT0F'
160200040119     C***                else
160300040119     C***                eval      cmzfla = prmfir
160400040119     C***                eval      cmzmba = prmfir
160500040119     C***                endif
160600040119     C                   eval      cmznrr = *zeros
160700040119     C                   move      §ctrokvt      cmznrr
160800040119     C                   eval      cmzlba = vlrfl1
160900040119     C                   call(e)   'TIS711C'
161000040119     C                   parm                    dscmz
161100040119     C                   parm      *blanks       esito
161200040119     C                   if        %error
161300040119     C                             or cmzerr = '1'
161400040119     C                             or esito  = '1'
161500040119     C                   eval      wrkesito = '3'
161600040119     C                   else
161700040119     C*
161800081028     C* 2° invio EDIVAB
161900040119     C                   reset                   dscmz
162000040119     C                   move      vlrpoi        cmzdst
162100040119     C                   eval      cmzfld = vlrfou
162200040119     C                   eval      cmzmbd = vlrhdl
162300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162400040119     C***                if        prmfir = *blanks
162500081028     C                   eval      cmzfla = 'EDIVAB0F'
162600081028     C                   eval      cmzmba = 'EDIVAB0F'
162700040119     C***                else
162800040119     C***                eval      cmzfla = prmfir
162900040119     C***                eval      cmzmba = prmfir
163000040119     C***                endif
163100040119     C                   eval      cmznrr = *zeros
163200040119     C                   move      §ctrokvb      cmznrr
163300040119     C                   eval      cmzlba = vlrfl1
163400040119     C                   call(e)   'TIS711C'
163500040119     C                   parm                    dscmz
163600040119     C                   parm      *blanks       esito
163700040119     C                   if        %error
163800040119     C                             or cmzerr = '1'
163900040119     C                             or esito  = '1'
164000040119     C                   eval      wrkesito = '3'
164100040119     C                   endif
164200040119     C                   endif
164300040119     C*
164400040119     C                   ENDSR
164500040119     C***
164600010601
164700010601
164800010601
164900010601
165000010601      /TITLE Invio dei dati al punto operativo.
165100010601     C     opeini        BEGSR
165200010601     C*
165300010601     C* Inizializzo flag e contatori operativi
165400010601     C                   movel     '0'           flgGiro           1
165500010601     C                   movel     '0'           flgMulti          1
165600010601     C                   movel     '1'           flgStato          1
165700010615     C                   movel     '0'           flgOk             1
165800010601     C                   z-add     *zeros        cntNonEl         10 0
165900010601     C                   z-add     *zeros        depfil            3 0
166000010601     C                   z-add     *zeros        invfil            3 0
166100010601     C*
166200010601     C                   ENDSR
166300010601     C***
166400070326
166500070326
166600070326
166700070326
166800080916     C     *pssr         BEGSR
166900070329     C*
167000080916     C                   if        %open(tivin00r)
167100080916     C                   close     tivin00r
167200080916     C                   endif
167300081028     C                   if        %open(edivabwr)
167400081028     C                   close     edivabwr
167500080916     C                   endif
167600081028     C                   if        %open(edivatwr)
167700081028     C                   close     edivatwr
167800080916     C                   endif
167900070326     C*
168000070326     C* Effettuo la chiamata al CLLE preposto
168100081028     C                   call(e)   'TITVEVTC'
168200080916     C                   parm                    parccm
168300080916     C                   parm                    parmbr
168400080916     C                   parm      '2'           paropz
168500070326     C*
168600080916     C                   eval      wrkesito = '2'
168700070404     C*
168800080916     C                   seton                                        LR
168900070326     C*
169000080916     C                   ENDSR     '*CANCL'
169100070326     C***
169200070326
169300070326
169400010330
169500010330
169600000613     C     *inzsr        BEGSR
169700990910     C*
169800990910     C     *entry        plist
169900990920     C                   parm                    tivlrds
170000990921     C                   parm      wrkesito      esito
170100000724     C                   parm                    prmlit
170200000710     C                   parm                    prmfir
170300010330     C*
170400010330     C* CALCOLA LA DATA CORRENTE
170500010330     C                   time                    wn14             14 0
170600081028     C                   movel     wn14          oracor            6 0          *ORA
170700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
170800010330     C                   z-add     wn8           g08dat
170900010330     C                   z-add     *zeros        g08inv
171000010330     C                   movel     '0'           g08err
171100010330     C                   call      'XSRDA8'
171200010330     C                   parm                    wlbda8
171300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
171400000613     C*
171500000613     C                   ENDSR
171600000613     C***
171700040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171800081020FILLE01;FILLE02;FILLER03;W_VABRMN;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;
171900081020VABNZD;W_VABRSD;W_VABIND;W_VABCAD;W_VABLOD;W_VABPRD;W_VABNZD;VABNCL;
172000110713VABPKB;W_VABVLB;W_NOTE;VABTC1;VABCAS;VABTIC;VABFFD;
