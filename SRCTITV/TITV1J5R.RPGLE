000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130121     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600040119     FFIVATwwr  O    E             DISK    usropn
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
012400081014     C                   if        %open(fivabwwr)
012500081014     C                   close     fivabwwr
012600081014     C                   endif
012700081014     C                   if        %open(fivatwwr)
012800081014     C                   close     fivatwwr
012900081014     C                   endif
013000040119     C*
013100040119     C* Effettuo la chiamata al CLLE preposto
013200040119     C                   call(e)   'TITVVTC'
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
014900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
015000010330     C*--------------------------------------------------------
015100010605     C     RWVAB         BEGSR
015200010330     C*
015300010330     C                   if        not %open(tivin00r)
015400010330     C                   open      tivin00r
015500010330     C                   endif
015600021025     C                   if        not %open(fivabwwr)
015700021025     C                   open      fivabwwr
015800010330     C                   endif
015900040119     C*
016000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
016100040119     C                   exsr      prevat
016200040119     C*
016300040119     C                   if        chkcall = '0'
016400040119     C*
016500040119     C                   if        not %open(fivatwwr)
016600040119     C                   open      fivatwwr
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
031900040119     C*
032000060331     C* Gestisco l'eventuale rottura x numero spedizione
032100070823     C                   if        VABNSP <> depspe
032200070301     C                   if        wVATNOT_E <> *blanks
032300080125     C                   if        VABCTM = *blanks
032400080125     C                   eval      VABCTM = '7Q'
032500080125     C                   endif
032600070301     C                   endif
032700040119     C                   write     fivab000                                     => scarico il VAB
032800060331     C                   endif
032900040119     C*
033000040119     C                   ENDSR
033100040119
033200040119
033300040119
033400040119     C*----------------------------------------------------*
033500040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033600040119     C*----------------------------------------------------*
033700040510     C     WRIVAT        BEGSR
033800060512     C*
033900060512     C* Inizializzo i campi chiave
034000060512     C                   eval      VATFGS = VABFGS
034100060512     C                   eval      VATAAS = VABAAS
034200060512     C                   eval      VATCCM = VABCCM
034300070823     C                   eval      VATNRS = VABNRS
034400060512     C                   eval      VATNSP = VABNSP
034500060512     C                   eval      VATLNP = VABLNP
034600060331     C*
034700060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
034800060331     C                   if        wVATNOT_A <> *blanks
034900060331     C                   eval      VATNOT = wVATNOT_A
035000060331     C                   eval      VATTRC = 'A'
035100060331     C                   write     FIVAT000
035200060331     C                   add       1             §CTROKVT
035300060331     C                   endif
035400060331     C*
035500060331     C                   if        wVATNOT_B <> *blanks
035600060331     C                   eval      VATNOT = wVATNOT_B
035700060331     C                   eval      VATTRC = 'B'
035800060331     C                   write     FIVAT000
035900060331     C                   add       1             §CTROKVT
036000060331     C                   endif
036100060331     C*
036200080625     C                   if        wVATNOT_E <> *blanks
036300060331     C                   eval      VATNOT = wVATNOT_E
036400060331     C                   eval      VATTRC = 'E'
036500080716     C                   write     FIVAT000
036600080716     C                   add       1             §CTROKVT
036700080625     C                   endif
036800040119     C*
036900040119     C                   ENDSR
037000040119
037100040119
037200040119
037300040119     C*----------------------------------------------------*
037400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
037500040119     C*----------------------------------------------------*
037600040119     C     PREVAT        BEGSR
037700040119     C*
037800040119     C* Compongo il nome del membro da dare al FIVATWWR
037900040119     C                   eval      parmbr = vlrhdl
038000040119     C                   movel     'M'           parmbr
038100040510     C                   eval      parccm = vlrksc
038200040119     C                   eval      paropz = '1'
038300040119     C* Effettuo la chiamata al CLLE preposto
038400040119     C                   call(e)   'TITVVTC'
038500040119     C                   parm                    parccm
038600040119     C                   parm                    parmbr
038700040119     C                   parm                    paropz
038800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038900040119     C                   if        %error
039000040119     C                   movel     '1'           chkcall
039100040119     C                   else
039200040119     C                   movel     '0'           chkcall
039300040119     C                   endif
039400040119     C*
039500040119     C                   ENDSR
039600010601
039700010601
039800010601
039900010330     C*----------------------------------------------------*
040000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040100010330     C*----------------------------------------------------*
040200040119     C     INZVAR        BEGSR
040300010330     C*
040400030822     C* Inizializzo variabili di wrk
040500020204     C                   Z-ADD     *zeros        Num5_0            5 0
040600060331     C                   MOVEL     *blanks       wVATNOT_A
040700060331     C                   MOVEL     *blanks       wVATNOT_B
040800060331     C                   MOVEL     *blanks       wVATNOT_E
040900080617     C                   EVAL      wFlgCAS = *blanks
041000030822     C*
041100030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
041200030822     C                   CLEAR                   FIVAB000
041300040119     C                   CLEAR                   FIVAT000
041400030822     C                   CLEAR                   SkSplitCSV
041500030822     C                   EVAL      i = 1
041600030822     C                   EVAL      posDa = *zeros
041700030822     C                   EVAL      posA  = *zeros
041800030822     C*
041900030822     C* Reimposto i valori di default
042000030822     C                   EXSR      DEFCAM
042100030822     C*
042200010330     C                   ENDSR
042300010330     C*----------------------------------------------------*
042400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042500010330     C*----------------------------------------------------*
042600010330     C     DEFCAM        BEGSR
042700030822     C*
042800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042900030715     C* e delimitatore testo.
043000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
043100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
043200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043300030822     C*
043400030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043600030715     C                   SETON                                        50
043700030715     C                   ELSE
043800030715     C                   SETOFF                                       50
043900030715     C                   ENDIF
044000030822     C*
044100030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
044200030822     C                   EVAL      posDaDft = 1
044300030822     C                   EVAL      posADft  = 0
044400030822     C                   EVAL      wGiroDft = 0
044500030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044600030822     C                             posDaDft > 0
044700030822     C*
044800030822     C* Gestisco il 1° giro
044900030822     C                   IF        wGiroDft = 0
045000030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
045100030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
045200030822     C* Incremento il contatore dei "giri"
045300030822     C                   EVAL      wGiroDft = 1
045400030822     C                   ELSE
045500030822     C                   EVAL      posDaDft = posADft
045600030822     C                   ENDIF
045700030822     C* Eseguo lo scan x trovare la fine del campo corrente
045800030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045900030822     C*
046000030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
046100030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
046200030822     C                   IF        posDaDft > 0 AND
046300030822     C                             posADft  > 0
046400030822     C* NCL
046500030822     C                   IF        %subst(
046600030822     C                             %subst(vlrppt:posDaDft+1:
046700030822     C                             posADft-posDaDft-1):1:3)
046800030822     C                             = 'NCL'
046900030822     C                   EVAL      PiStr=%trim(%subst(
047000030822     C                             %subst(vlrppt:posDaDft+1:
047100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
047200030822     C                   EXSR      CHKNUM
047300030822     C                   IF        PiInt=*on
047400030822     C                   Z-ADD     PiVal         VABNCL
047500030822     C                   ENDIF
047600030822     C                   ENDIF
047700030822     C* CCM
047800030822     C                   IF        %subst(
047900030822     C                             %subst(vlrppt:posDaDft+1:
048000030822     C                             posADft-posDaDft-1):1:3)
048100030822     C                             = 'CCM'
048200030822     C                   EVAL      PiStr=%trim(%subst(
048300030822     C                             %subst(vlrppt:posDaDft+1:
048400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048500030822     C                   EXSR      CHKNUM
048600030822     C                   IF        PiInt=*on
048700030822     C                   Z-ADD     PiVal         VABCCM
048800030822     C                   ENDIF
048900030822     C                   ENDIF
049000030822     C* LNP
049100030822     C                   IF        %subst(
049200030822     C                             %subst(vlrppt:posDaDft+1:
049300030822     C                             posADft-posDaDft-1):1:3)
049400030822     C                             = 'LNP'
049500030822     C                   EVAL      PiStr=%trim(%subst(
049600030822     C                             %subst(vlrppt:posDaDft+1:
049700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049800030822     C                   EXSR      CHKNUM
049900030822     C                   IF        PiInt=*on
050000030822     C                   Z-ADD     PiVal         VABLNP
050100030822     C                   ENDIF
050200030822     C                   ENDIF
050300030822     C* NRS
050400030822     C                   IF        %subst(
050500030822     C                             %subst(vlrppt:posDaDft+1:
050600030822     C                             posADft-posDaDft-1):1:3)
050700030822     C                             = 'NRS'
050800030822     C                   EVAL      PiStr=%trim(%subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051100030822     C                   EXSR      CHKNUM
051200030822     C                   IF        PiInt=*on
051300030822     C                   Z-ADD     PiVal         VABNRS
051400030822     C                   ENDIF
051500030822     C                   ENDIF
051600030822     C* CTR
051700030822     C                   IF        %subst(
051800030822     C                             %subst(vlrppt:posDaDft+1:
051900030822     C                             posADft-posDaDft-1):1:3)
052000030822     C                             = 'CTR'
052100030822     C                   EVAL      PiStr=%trim(%subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052400030822     C                   EXSR      CHKNUM
052500030822     C                   IF        PiInt=*on
052600030822     C                   Z-ADD     PiVal         VABCTR
052700030822     C                   ENDIF
052800030822     C                   ENDIF
052900030822     C* PKB
053000030822     C                   IF        %subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):1:3)
053300030822     C                             = 'PKB'
053400030822     C                   EVAL      PiStr=%trim(%subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053700030822     C                   EXSR      CHKNUM
053800030822     C                   IF        PiNum=*on
053900030822     C                   Z-ADD     PiVal         VABPKB
054000030822     C                   ENDIF
054100030822     C                   ENDIF
054200030822     C* VLB
054300030822     C                   IF        %subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):1:3)
054600030822     C                             = 'VLB'
054700030822     C                   EVAL      PiStr=%trim(%subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055000030822     C                   EXSR      CHKNUM
055100030822     C                   IF        PiNum=*on
055200030822     C                   Z-ADD     PiVal         VABVLB
055300030822     C                   ENDIF
055400030822     C                   ENDIF
055500030822     C* QFT
055600030822     C                   IF        %subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):1:3)
055900030822     C                             = 'QFT'
056000030822     C                   EVAL      PiStr=%trim(%subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056300030822     C                   EXSR      CHKNUM
056400030822     C                   IF        PiNum=*on
056500030822     C                   Z-ADD     PiVal         VABQFT
056600030822     C                   ENDIF
056700030822     C                   ENDIF
056800030822     C* CBO
056900030822     C                   IF        %subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):1:3)
057200030822     C                             = 'CBO'
057300030822     C                   EVAL      VABCBO=%trim(%subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057600030822     C                   ENDIF
057700030822     C* TSP
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'TSP'
058200030822     C                   EVAL      VABTSP=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   ENDIF
058600030822     C* VAS
058700030822     C                   IF        %subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):1:3)
059000030822     C                             = 'VAS'
059100030822     C                   EVAL      VABVAS=%trim(%subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059400030822     C                   ENDIF
059500030822     C* VCA
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'VCA'
060000030822     C                   EVAL      VABVCA=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   ENDIF
060400070925     C* TIC
060500070925     C                   IF        %subst(
060600070925     C                             %subst(vlrppt:posDaDft+1:
060700070925     C                             posADft-posDaDft-1):1:3)
060800070925     C                             = 'TIC'
060900070925     C                   EVAL      VABTIC=%trim(%subst(
061000070925     C                             %subst(vlrppt:posDaDft+1:
061100070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061200070925     C                   ENDIF
061300030822     C* GCA
061400030822     C                   IF        %subst(
061500030822     C                             %subst(vlrppt:posDaDft+1:
061600030822     C                             posADft-posDaDft-1):1:3)
061700030822     C                             = 'GCA'
061800030822     C                   EVAL      VABGCA=%trim(%subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062100030822     C                   ENDIF
062200030822     C* CTM
062300030822     C                   IF        %subst(
062400030822     C                             %subst(vlrppt:posDaDft+1:
062500030822     C                             posADft-posDaDft-1):1:3)
062600030822     C                             = 'CTM'
062700030822     C                   EVAL      VABCTM=%trim(%subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063000030822     C                   ENDIF
063100030822     C* FFD
063200030822     C                   IF        %subst(
063300030822     C                             %subst(vlrppt:posDaDft+1:
063400030822     C                             posADft-posDaDft-1):1:3)
063500030822     C                             = 'FFD'
063600030822     C                   EVAL      VABFFD=%trim(%subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063900030822     C                   ENDIF
064000030822     C* VAD
064100030822     C                   IF        %subst(
064200030822     C                             %subst(vlrppt:posDaDft+1:
064300030822     C                             posADft-posDaDft-1):1:3)
064400030822     C                             = 'VAD'
064500030822     C                   EVAL      VABVAD=%trim(%subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064800030822     C                   ENDIF
064900030822     C* GMA
065000030822     C                   IF        %subst(
065100030822     C                             %subst(vlrppt:posDaDft+1:
065200030822     C                             posADft-posDaDft-1):1:3)
065300030822     C                             = 'GMA'
065400030822     C                   EVAL      VABGMA=%trim(%subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065700030822     C                   ENDIF
065800030822     C* GGA
065900030822     C                   IF        %subst(
066000030822     C                             %subst(vlrppt:posDaDft+1:
066100030822     C                             posADft-posDaDft-1):1:3)
066200030822     C                             = 'GGA'
066300030822     C                   EVAL      VABGGA=%trim(%subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066600030822     C                   ENDIF
066700030822     C* GVA
066800030822     C                   IF        %subst(
066900030822     C                             %subst(vlrppt:posDaDft+1:
067000030822     C                             posADft-posDaDft-1):1:3)
067100030822     C                             = 'GVA'
067200030822     C                   EVAL      VABGVA=%trim(%subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067500030822     C                   ENDIF
067600030822     C* TC1
067700030822     C                   IF        %subst(
067800030822     C                             %subst(vlrppt:posDaDft+1:
067900030822     C                             posADft-posDaDft-1):1:3)
068000030822     C                             = 'TC1'
068100030822     C                   EVAL      VABTC1=%trim(%subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068400030822     C                   ENDIF
068500030822     C* TC2
068600030822     C                   IF        %subst(
068700030822     C                             %subst(vlrppt:posDaDft+1:
068800030822     C                             posADft-posDaDft-1):1:3)
068900030822     C                             = 'TC2'
069000030822     C                   EVAL      VABTC2=%trim(%subst(
069100030822     C                             %subst(vlrppt:posDaDft+1:
069200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069300030822     C                   ENDIF
069400040714     C* VATTRC
069500040714     C                   IF        %subst(
069600040714     C                             %subst(vlrppt:posDaDft+1:
069700040714     C                             posADft-posDaDft-1):1:3)
069800040714     C                             = 'TRC'
069900040714     C                   EVAL      VATTRC=%trim(%subst(
070000040714     C                             %subst(vlrppt:posDaDft+1:
070100040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070200040714     C                   ENDIF
070300030822     C* ...
070400030822     C                   ENDIF
070500030822     C                   ENDDO
070600020204     C*
070700010330     C                   ENDSR
070800010607     C*----------------------------------------------------*
070900021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
071000010607     C*----------------------------------------------------*
071100010607     C     IMPVAB        BEGSR
071200010607     C*
071300010607     C                   SETOFF                                       3132
071400030515     C*
071500010607     C                   MOVEL     datcor        VABAAS
071600010607     C                   MOVE      datcor        VABMGS
071700010607     C*
071800030715     C**********
071900030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
072000030715     C**********
072100030715     C                   IF        wGiro = *zeros
072200080716     C*
072300080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
072400080716     C                   EVAL      depvinDTA = vinDTA
072500040113     C*
072600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
072700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
072800040510     C                             %trim(intcol(3))<>*blanks
072900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
073000040113     C                                      %trim(intcol(3))
073100070823     C     ';':CharCSV   XLATE     vindta        vindta
073200040510     C                   ENDIF
073300080626     C*
073400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
073500031201     C     minu:maiu     XLATE     vindta        vindta
073600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073700030715     C                   DOW       posDa <= %len(%trim(vindta))
073800030715     C*
073900030715     C* Gestisco il 1° campo
074000030715     C                   IF        i = 1
074100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
074200030715     C                   EVAL      posDa = 1
074300030715     C* Eseguo lo scan x trovare la fine del primo campo
074400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074500030715     C* A questo punto "estraggo" il campo corrente
074600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074700030715     C                                                    (posA-posDa))
074800030715     C* X i campi successivi al 1°
074900030715     C                   ELSE
075000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
075100030715     C                   EVAL      posDa = posA + 1
075200030715     C* Eseguo lo scan x trovare la fine del campo corrente
075300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075400030715     C* Gestisco l'ultimo campo
075500030715     C                   IF        posA = *zeros
075600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
075700030715     C* A questo punto "estraggo" il campo corrente
075800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075900030715     C                                                    (posA-posDa))
076000030715     C                   LEAVE
076100030715     C                   ELSE
076200030715     C* A questo punto "estraggo" il campo corrente
076300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076400030715     C                                                    (posA-posDa))
076500030715     C                   ENDIF
076600030715     C                   ENDIF
076700030715     C* Incremento il contatore d campo
076800030715     C                   EVAL      i = i +1
076900030715     C                   ENDDO
077000030715     C*
077100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
077200030715     C* delimitazione testo
077300030715     C                   EVAL      i = 1
077400030715     C                   DOW       i < %elem(SkSplitFLD)
077500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
077600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
077700030715     C                   EVAL      i = i + 1
077800030715     C                   ENDDO
077900030715     C                   EVAL      wGiro = 1
078000080716     C* Ripristino la 1° riga originale
078100080716     C                   EVAL      vinDTA = depvinDTA
078200030715     C                   ELSE
078300030715     C                   EVAL      wGiro = 2
078400031204     C**********
078500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
078600031204     C* d separatore campo
078700031204     C**********
078800031204     C                   EVAL      vindta = %trim(vindta)
078900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079000031204     C                   EVAL      vindta = %subst(vindta:2)
079100031204     C                   ENDDO
079200031204     C*
079300031204     C                   Z-ADD     *zeros        lunghInput        4 0
079400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
079600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
079700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079800031204     C                   ENDDO
079900030715     C**********
080000030715     C* Effettuo lo split del campo dati d input
080100030715     C**********
080200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
080300030715     C                   DOW       posDa <= %len(%trim(vindta))
080400030715     C*
080500030715     C* Gestisco il 1° campo
080600030715     C                   IF        i = 1
080700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080800030715     C                   EVAL      posDa = 1
080900030715     C* Eseguo lo scan x trovare la fine del primo campo
081000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081100030715     C* A questo punto "estraggo" il campo corrente
081200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
081300030715     C                                                    (posA-posDa))
081400030715     C* X i campi successivi al 1°
081500030715     C                   ELSE
081600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081700030715     C                   EVAL      posDa = posA + 1
081800031204     C* Verifico che nn vi sia il campo nn valorizzato
081900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082000031204     C* Se campo nn valorizzato skippo al prossimo
082100031204     C                   EVAL      SkSplitCSV(i) = *blanks
082200031204     C                   EVAL      posA  = posA + 1
082300031204     C                   ELSE
082400030715     C* Eseguo lo scan x trovare la fine del campo corrente
082500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082600030715     C* Gestisco l'ultimo campo
082700030715     C                   IF        posA = *zeros
082800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C                   LEAVE
083300030715     C                   ELSE
083400030715     C* A questo punto "estraggo" il campo corrente
083500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083600030715     C                                                    (posA-posDa))
083700030715     C                   ENDIF
083800031204     C                   ENDIF
083900030715     C                   ENDIF
084000030715     C* Incremento il contatore d campo
084100030715     C                   EVAL      i = i +1
084200030715     C                   ENDDO
084300030715     C*
084400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084500030715     C* delimitazione testo
084600030715     C                   EVAL      i = 1
084700030715     C                   DOW       i < %elem(SkSplitCSV)
084800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
084900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085000030715     C                   EVAL      i = i + 1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitFLD)
085600030715     C*
085700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
085800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
085900030715     C                             SkSplitCSV(i) <> *blanks
086000030721     C*** ATB
086100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
086200030721     C                   EVAL      VABATB = SkSplitCSV(i)
086300030721     C                   ENDIF
086400030715     C*** RSD
086500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
086600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
086700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
086800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
086900030715     C* ==
087000030715     C                   ENDIF
087100030721     C*** RD2
087200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
087300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
087400030721     C                   ENDIF
087500030715     C*** IND
087600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
087700030715     C                   EVAL      VABIND = SkSplitCSV(i)
087800030715     C                   ENDIF
087900030715     C*** LOD
088000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
088100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
088200030715     C                   ENDIF
088300030715     C*** PRD
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
088500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030721     C*** NZD
088800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
088900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089000030721     C                   ENDIF
089100030715     C*** RMA
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
089300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030715     C*** NOT
089600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
089700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
089800030715     C                   ENDIF
089900030715     C*** NT2
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
090100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** VCA
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
090500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** VAS
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
090900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** FFD
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
091300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030721     C*** GC1
091600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
091700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
091800030721     C                   ENDIF
091900030721     C*** GC2
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
092100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030721     C*** TSP
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
092500030721     C                   EVAL      VABTSP = SkSplitCSV(i)
092600030721     C                   ENDIF
092700040510     C*** CBO
092800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
092900040510     C                   EVAL      VABCBO = SkSplitCSV(i)
093000040510     C                   ENDIF
093100030721     C*** NAS
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
093300030721     C                   EVAL      VABNAS = SkSplitCSV(i)
093400030721     C                   ENDIF
093500030721     C*** TIC
093600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
093700030721     C                   EVAL      VABTIC = SkSplitCSV(i)
093800030721     C                   ENDIF
093900030721     C*** GCA
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
094100030721     C                   EVAL      VABGCA = SkSplitCSV(i)
094200030721     C                   ENDIF
094300030721     C*** XCO
094400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
094500030721     C                   EVAL      VABXCO = SkSplitCSV(i)
094600030721     C                   ENDIF
094700030721     C*** CTM
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
094900030721     C                   EVAL      VABCTM = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TCR
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
095300030721     C                   EVAL      VABTCR = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** CTS
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
095700030721     C                   EVAL      VABCTS = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** FTM
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
096100030721     C                   EVAL      VABFTM = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** VAD
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
096500030721     C                   EVAL      VABVAD = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** GMA
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
096900030721     C                   EVAL      VABGMA = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** GGA
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
097300030721     C                   EVAL      VABGGA = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** GVA
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
097700030721     C                   EVAL      VABGVA = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** TC1
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
098100030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** TC2
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
098500030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** SCL
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
098900030721     C                   EVAL      VABSCL = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** RMO
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
099300030721     C                   EVAL      VABRMO = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** NMO
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
099700030721     C                   EVAL      VABNMO = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*
100000030721     C* Reperisco quindi i campi numerici...
100100040510     C*** NSP
100200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
100300040510     C                   EVAL      PiStr=SkSplitCSV(i)
100400040510     C                   EXSR      CHKNUM
100500040510     C                   IF        PiInt=*on
100600040510     C                   Z-ADD     PiVal         VABNSP
100700040510     C                   ELSE
100800040510     C                   SETON                                        31
100900040510     C                   EVAL      VABNSP = *zeros
101000040510     C                   EVAL      vinmsg = %trimr(vinmsg)
101100040510     C                             + ' ' + 'VABNSP'
101200040510     C                   ENDIF
101300040510     C                   ENDIF
101400071106     C*** RMN
101500071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
101600071106     C                   EVAL      PiStr=SkSplitCSV(i)
101700071106     C                   EXSR      CHKNUM
101800071106     C                   IF        PiInt=*on
101900071106     C                   Z-ADD     PiVal         VABRMN
102000071106     C                   ELSE
102100071106     C                   SETON                                        32
102200071106     C                   EVAL      VABRMN = *zeros
102300071106     C                   EVAL      vinmsg = %trimr(vinmsg)
102400071106     C                             + ' ' + 'VABRMN'
102500071106     C                   ENDIF
102600071106     C                   ENDIF
102700030721     C*** CAD
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
102900030721     C                   EVAL      PiStr=SkSplitCSV(i)
103000030721     C                   EXSR      CHKNUM
103100030721     C                   IF        PiInt=*on
103200030721     C                   Z-ADD     PiVal         Num5_0
103300030721     C                   MOVEL(p)  Num5_0        VABCAD
103400030721     C                   ELSE
103500030721     C                   SETON                                        32
103600030721     C                   EVAL      VABCAD = *zeros
103700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103800030721     C                             + ' ' + 'VABCAD'
103900030721     C                   ENDIF
104000030721     C                   ENDIF
104100030721     C*** NCL
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104300030721     C                   EVAL      PiStr=SkSplitCSV(i)
104400030721     C                   EXSR      CHKNUM
104500030721     C                   IF        PiInt=*on
104600030721     C                   Z-ADD     PiVal         VABNCL
104700030721     C                   ELSE
104800030721     C                   SETON                                        32
104900030721     C                   EVAL      VABNCL = *zeros
105000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105100040122     C                             + ' ' + 'VABNCL'
105200030721     C                   ENDIF
105300030721     C                   ENDIF
105400030721     C*** PKB
105500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
105600030721     C                   EVAL      PiStr=SkSplitCSV(i)
105700030721     C                   EXSR      CHKNUM
105800030721     C                   IF        PiNum=*on
105900030721     C                   Z-ADD     PiVal         VABPKB
106000030721     C                   ELSE
106100030721     C                   SETON                                        32
106200030721     C                   EVAL      VABPKB = *zeros
106300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
106400030721     C                             + ' ' + 'VABPKB'
106500030721     C                   ENDIF
106600030721     C                   ENDIF
106700030721     C*** CAS
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
106900080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
107000080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
107100081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
107200081014     C                             %trim(SkSplitCSV(i)) <> '0,00'
107300080125     C                   EVAL      wFlgCAS = '1'
107400080125     C                   ENDIF
107500030721     C                   EVAL      PiStr=SkSplitCSV(i)
107600030721     C                   EXSR      CHKNUM
107700030721     C                   IF        PiNum=*on
107800030721     C                   Z-ADD     PiVal         VABCAS
107900030721     C                   ELSE
108000030721     C                   SETON                                        32
108100030721     C                   EVAL      VABCAS = *zeros
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300030721     C                             + ' ' + 'VABCAS'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** CCM
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiInt=*on      AND
109100030721     C                             PiVal<=9999999 AND
109200030721     C                             PiVal>*zeros
109300030721     C                   Z-ADD     PiVal         VABCCM
109400030721     C                   ELSE
109500030721     C                   SETON                                        32
109600030721     C                   EVAL      VABCCM = *zeros
109700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109800040510     C                             + ' ' + 'VABCCM'
109900030721     C                   ENDIF
110000030721     C                   ENDIF
110100030721     C*** LNP
110200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110300030721     C                   EVAL      PiStr=SkSplitCSV(i)
110400030721     C                   EXSR      CHKNUM
110500030721     C                   IF        PiInt=*on    AND
110600030721     C                             PiVal<=999   AND
110700030721     C                             PiVal>*zeros
110800030721     C                   Z-ADD     PiVal         VABLNP
110900030721     C                   ELSE
111000040715     C                   SETON                                        32
111100030721     C                   EVAL      VABLNP = *zeros
111200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111300040510     C                             + ' ' + 'VABLNP'
111400030721     C                   ENDIF
111500030721     C                   ENDIF
111600030721     C*** NRS
111700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
111800030721     C                   EVAL      PiStr=SkSplitCSV(i)
111900030721     C                   EXSR      CHKNUM
112000030721     C                   IF        PiInt=*on AND
112100030721     C                             PiVal<=99
112200030721     C                   Z-ADD     PiVal         VABNRS
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112600030721     C                             + ' ' + 'VABNRS'
112700030721     C                   ENDIF
112800030721     C                   ENDIF
112900030721     C*** LNA
113000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113100030721     C                   EVAL      PiStr=SkSplitCSV(i)
113200030721     C                   EXSR      CHKNUM
113300030721     C                   IF        PiInt=*on    AND
113400030721     C                             PiVal<=999   AND
113500030721     C                             PiVal>*zeros
113600030721     C                   Z-ADD     PiVal         VABLNA
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABLNA = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABLNA'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** CTR
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiInt=*on  AND
114900030721     C                             PiVal<=999
115000030721     C                   Z-ADD     PiVal         VABCTR
115100030721     C                   ELSE
115200030721     C                   SETON                                        32
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABCTR'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** IAS
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiNum=*on
116200030721     C                   Z-ADD     PiVal         VABIAS
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116600030721     C                             + ' ' + 'VABIAS'
116700030721     C                   ENDIF
116800030721     C                   ENDIF
116900030721     C*** VLB
117000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117100030721     C                   EVAL      PiStr=SkSplitCSV(i)
117200030721     C                   EXSR      CHKNUM
117300030721     C                   IF        PiNum=*on
117400030721     C                   Z-ADD     PiVal         VABVLB
117500030721     C                   ELSE
117600030721     C                   SETON                                        32
117700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117800030721     C                             + ' ' + 'VABVLB'
117900030721     C                   ENDIF
118000030721     C                   ENDIF
118100030721     C*** QFT
118200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118300030721     C                   EVAL      PiStr=SkSplitCSV(i)
118400030721     C                   EXSR      CHKNUM
118500030721     C                   IF        PiNum=*on
118600030721     C                   Z-ADD     PiVal         VABQFT
118700030721     C                   ELSE
118800030721     C                   SETON                                        32
118900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119000030721     C                             + ' ' + 'VABQFT'
119100030721     C                   ENDIF
119200030721     C                   ENDIF
119300030721     C*** NCD
119400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
119500030721     C                   EVAL      PiStr=SkSplitCSV(i)
119600030721     C                   EXSR      CHKNUM
119700030721     C                   IF        PiInt=*on      AND
119800030721     C                             PiVal<=9999999
119900030721     C                   Z-ADD     PiVal         VABNCD
120000030721     C                   ELSE
120100030721     C                   SETON                                        32
120200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120300030721     C                             + ' ' + 'VABNCD'
120400030721     C                   ENDIF
120500030721     C                   ENDIF
120600030721     C*** NCA
120700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
120800030721     C                   EVAL      PiStr=SkSplitCSV(i)
120900030721     C                   EXSR      CHKNUM
121000030721     C                   IF        PiInt=*on      AND
121100030721     C                             PiVal<=9999999
121200030721     C                   Z-ADD     PiVal         VABNCA
121300030721     C                   ELSE
121400030721     C                   SETON                                        32
121500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121600030721     C                             + ' ' + 'VABNCA'
121700030721     C                   ENDIF
121800030721     C                   ENDIF
121900030721     C*** ZNC
122000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122100030721     C                   EVAL      PiStr=SkSplitCSV(i)
122200030721     C                   EXSR      CHKNUM
122300030721     C                   IF        PiInt=*on AND
122400030721     C                             PiVal<=99
122500030721     C                   Z-ADD     PiVal         VABZNC
122600030721     C                   ELSE
122700030721     C                   SETON                                        32
122800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122900030721     C                             + ' ' + 'VABZNC'
123000030721     C                   ENDIF
123100030721     C                   ENDIF
123200030721     C*** DCR
123300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123400030721     C                   EVAL      PiStr=SkSplitCSV(i)
123500030721     C                   EXSR      CHKNUM
123600030721     C                   IF        PiInt=*on       AND
123700030721     C                             PiVal<=99999999
123800030721     C                   Z-ADD     PiVal         VABDCR
123900030721     C                   ELSE
124000030721     C                   SETON                                        32
124100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124200030721     C                             + ' ' + 'VABDCR'
124300030721     C                   ENDIF
124400030721     C                   ENDIF
124500030721     C*** HCR
124600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
124700030721     C                   EVAL      PiStr=SkSplitCSV(i)
124800030721     C                   EXSR      CHKNUM
124900030721     C                   IF        PiInt=*on   AND
125000030721     C                             PiVal<=9999
125100030721     C                   Z-ADD     PiVal         VABHCR
125200030721     C                   ELSE
125300030721     C                   SETON                                        32
125400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125500030721     C                             + ' ' + 'VABHCR'
125600030721     C                   ENDIF
125700030721     C                   ENDIF
125800030721     C*** VMD
125900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126000030721     C                   EVAL      PiStr=SkSplitCSV(i)
126100030721     C                   EXSR      CHKNUM
126200030721     C                   IF        PiNum=*on
126300030721     C                   Z-ADD     PiVal         VABVMD
126400030721     C                   ELSE
126500030721     C                   SETON                                        32
126600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126700030721     C                             + ' ' + 'VABVMD'
126800030721     C                   ENDIF
126900030721     C                   ENDIF
127000030721     C*** ANT
127100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127200030721     C                   EVAL      PiStr=SkSplitCSV(i)
127300030721     C                   EXSR      CHKNUM
127400030721     C                   IF        PiInt=*on        AND
127500030721     C                             PiVal<=999999999
127600030721     C                   Z-ADD     PiVal         VABANT
127700030721     C                   ELSE
127800030721     C                   SETON                                        32
127900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128000030721     C                             + ' ' + 'VABANT'
128100030721     C                   ENDIF
128200030721     C                   ENDIF
128300030721     C*** CMO
128400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
128500030721     C                   EVAL      PiStr=SkSplitCSV(i)
128600030721     C                   EXSR      CHKNUM
128700030721     C                   IF        PiInt=*on
128800030721     C                   Z-ADD     PiVal         Num5_0
128900030721     C                   MOVEL(p)  Num5_0        VABCMO
129000030721     C                   ELSE
129100030721     C                   SETON                                        32
129200030721     C                   EVAL      VABCMO = *zeros
129300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129400030721     C                             + ' ' + 'VABCMO'
129500030721     C                   ENDIF
129600030721     C                   ENDIF
129700040510     C*
129800040510     C* Reperisco i campi del FIVAT
129900040510     C*** VATNOT
130000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130100040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
130200040510     C                   ENDIF
130300040510     C*** VATTRC
130400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
130500040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
130600040510     C                   ENDIF
130700060331     C*** VATNOT_A
130800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
130900060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
131000060331     C                   ENDIF
131100060331     C*** VATNOT_B
131200060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
131300060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
131400060331     C                   ENDIF
131500060331     C*** VATNOT_E
131600070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
131700060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
131800060331     C                   ENDIF
131900070823     C*
132000071029     C***  ===>  Gestione campi "particolari" dentro ciclo
132100071204     C*
132200070925     C*
132300071029     C***  <===  -----------------------------------------
132400040510     C*
132500030715     C                   ENDIF
132600030715     C                   EVAL      i = i + 1
132700030715     C                   ENDDO
132800070925     C*
132900071029     C***  ===>  Gestione campi "particolari" fuori ciclo
133000080125     C*
133100081016     C                   EVAL      VABNSP = depspe + 1
133200070925     C*
133300071029     C***  <===  -----------------------------------------
133400070925     C*
133500070925     C* Considerazioni finali su CBO/CAS
133600080125     C                   IF        wFlgCAS = '1'
133700070925     C                   IF        VABCBO = '1'
133800070925     C                   EVAL      VABCBO = '4'
133900070925     C                   ENDIF
134000070925     C                   IF        VABCBO = '2'
134100070925     C                   EVAL      VABCBO = '6'
134200070925     C                   ENDIF
134300070925     C                   ENDIF
134400070925     C***  <===  ----------------------------
134500040510     C*
134600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
134700040510     C                   IF        VABNSP = *zeros
134800040510     C                   SETON                                        31
134900040510     C                   ENDIF
135000040715     C*
135100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
135200040715     C                   IF        VABLNP = *zeros
135300040715     C                   SETON                                        31
135400040715     C                   ENDIF
135500040510     C*
135600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
135700040510     C                   IF        VABRMN = *zeros
135800040510     C                   Z-ADD     VABNSP        VABRMN
135900040510     C                   ENDIF
136000040510     C*
136100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
136200040510     C                   IF        VABRMA = *blanks
136300040714     C                   MOVEL     VABRMN        VABRMA
136400040510     C                   ENDIF
136500040128     C*
136600040510     C* Se provincia nn valorizzata la reperisco
136700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
136800040510     C                   IF        VABPRD = *blanks
136900040128     C                   CLEAR                   TISI95DS
137000040128     C                   EVAL      I95TCN = '3'
137100040128     C                   Z-ADD     datcor        I95DAT
137200040128     C                   EVAL      I95NAR = VABNZD
137300040128     C                   EVAL      I95CAP = VABCAD
137400040128     C                   EVAL      I95LOC = VABLOD
137500040128     C                   CALL      'TISI95R'
137600040128     C                   PARM                    TISI95DS
137700040128     C                   EVAL      VABPRD = O95PRV
137800040510     C                   ENDIF
137900020204     C*
138000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
138100020204     C                   EXSR      CHKIMPDIV
138200030715     C                   ENDIF
138300020204     C*
138400010607     C                   ENDSR
138500010607     C*----------------------------------------------------*
138600010601
138700020204
138800020204     C*----------------------------------------------------*
138900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
139000020204     C*----------------------------------------------------*
139100020204     C     CHKIMPDIV     BEGSR
139200020204     C*
139300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
139400020204     C                   Z-ADD     *zeros        wrkDec            9 9
139500020204     C*
139600020204     C* Come prima cosa effettuo considerazioni sulla divisa
139700020204     C                   IF        vabIAS > *zeros
139800020204     C                   IF        vabVAS <> 'EUR'
139900020204     C                   EVAL      vabVAS =  'ITL'
140000020204     C                   ENDIF
140100020204     C                   ENDIF
140200020204     C*
140300020204     C                   IF        vabCAS > *zeros
140400020204     C                   IF        vabVCA <> 'EUR'
140500020204     C                   EVAL      vabVCA =  'ITL'
140600020204     C                   ENDIF
140700020204     C                   ENDIF
140800020204     C*
140900020204     C                   IF        vabVMD > *zeros
141000020204     C                   IF        vabVAD <> 'EUR'
141100020204     C                   EVAL      vabVAD =  'ITL'
141200020204     C                   ENDIF
141300020204     C                   ENDIF
141400020204     C*
141500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141600020204     C                   Z-ADD     vabIAS        wrkDec
141700020204     C                   IF        wrkDec > *zeros
141800020204     C                   IF        vabVAS = 'ITL'
141900020204     C                   EVAL      vabIAS = *zeros
142000020204     C                   ENDIF
142100020204     C                   ENDIF
142200020204     C*
142300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
142400020204     C                   Z-ADD     vabCAS        wrkDec
142500020204     C                   IF        wrkDec > *zeros
142600020204     C                   IF        vabVCA = 'ITL'
142700020204     C                   EVAL      vabCAS = *zeros
142800020204     C                   ENDIF
142900020204     C                   ENDIF
143000020204     C*
143100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
143200020204     C                   Z-ADD     vabVMD        wrkDec
143300020204     C                   IF        wrkDec > *zeros
143400020204     C                   IF        vabVAD = 'ITL'
143500020204     C                   EVAL      vabVMD = *zeros
143600020204     C                   ENDIF
143700020204     C                   ENDIF
143800020204     C*
143900020204     C                   ENDSR
144000020204     C***
144100080617
144200080617
144300080617
144400080617     C*----------------------------------------------------*
144500080617     C*  CONTROLLO NUMERICITA' CAMPI
144600080617     C*----------------------------------------------------*
144700080617     C     CHKNUM        BEGSR
144800080617     C*
144900080617     C                   IF        PiDecChr = *blanks
145000080617     C                   EVAL      PiDecChr = CharNUM
145100080617     C                   ENDIF
145200080617     C*
145300080617     C                   callp     UBISNUM_Check(PiStr
145400080617     C                                          :PiDecChr
145500080617     C                                          :PiVal
145600080617     C                                          :PiNum
145700080617     C                                          :PiInt)
145800080617     C*
145900080617     C                   ENDSR
146000080617     C***
146100010330
146200010601
146300010601
146400010601
146500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146600010601     C     repfil        BEGSR
146700010601     C*
146800010601     C                   if        invfil = *zeros and
146900010601     C                             depfil > *zeros and
147000010629     C                             (vinflg = *blanks or
147100010629     C                              vinflg = *zeros)
147200010601     C
147300010601     C                   eval      invfil = depfil
147400010601     C                   endif
147500010601     C*
147600010601     C                   if        depfil <> invfil and
147700010601     C                             invfil > *zeros
147800010601     C                   eval      flgMulti = '1'
147900010601     C                   if        vinflg = *blanks
148000010601     C                   add       1             cntNonEl
148100010601     C                   endif
148200010601     C                   endif
148300010601     C*
148400010601     C                   if        vinflg = '2'
148500010601     C                   eval      flgStato = '2'
148600010601     C                   endif
148700010601     C*
148800010601     C                   ENDSR
148900010601     C***
149000010601
149100010601
149200010601
149300010330
149400040119      /TITLE Invio dei dati al punto operativo.
149500040119     C     invio         BEGSR
149600040119     C*
149700040119     C* 1° invio FIVAT
149800040119     C                   reset                   dscmz
149900040119     C                   move      vlrpoi        cmzdst
150000040119     C                   eval      cmzfld = 'FIVATWWR'
150100040119     C                   eval      cmzmbd = vlrhdl
150200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
150300040119     C***                if        prmfir = *blanks
150400040119     C                   eval      cmzfla = 'FIVAT00F'
150500040119     C                   eval      cmzmba = 'FIVAT00F'
150600040119     C***                else
150700040119     C***                eval      cmzfla = prmfir
150800040119     C***                eval      cmzmba = prmfir
150900040119     C***                endif
151000040119     C                   eval      cmznrr = *zeros
151100040119     C                   move      §ctrokvt      cmznrr
151200040119     C                   eval      cmzlba = vlrfl1
151300040119     C                   call(e)   'TIS711C'
151400040119     C                   parm                    dscmz
151500040119     C                   parm      *blanks       esito
151600040119     C                   if        %error
151700040119     C                             or cmzerr = '1'
151800040119     C                             or esito  = '1'
151900040119     C                   eval      wrkesito = '3'
152000040119     C                   else
152100040119     C*
152200040119     C* 2° invio FIVAB
152300040119     C                   reset                   dscmz
152400040119     C                   move      vlrpoi        cmzdst
152500040119     C                   eval      cmzfld = vlrfou
152600040119     C                   eval      cmzmbd = vlrhdl
152700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152800040119     C***                if        prmfir = *blanks
152900040119     C                   eval      cmzfla = 'FIVAB00F'
153000040119     C                   eval      cmzmba = 'FIVAB00F'
153100040119     C***                else
153200040119     C***                eval      cmzfla = prmfir
153300040119     C***                eval      cmzmba = prmfir
153400040119     C***                endif
153500040119     C                   eval      cmznrr = *zeros
153600040119     C                   move      §ctrokvb      cmznrr
153700040119     C                   eval      cmzlba = vlrfl1
153800040119     C                   call(e)   'TIS711C'
153900040119     C                   parm                    dscmz
154000040119     C                   parm      *blanks       esito
154100040119     C                   if        %error
154200040119     C                             or cmzerr = '1'
154300040119     C                             or esito  = '1'
154400040119     C                   eval      wrkesito = '3'
154500040119     C                   endif
154600040119     C                   endif
154700040119     C*
154800040119     C                   ENDSR
154900040119     C***
155000010601
155100010601
155200010601
155300010601
155400010601      /TITLE Invio dei dati al punto operativo.
155500010601     C     opeini        BEGSR
155600010601     C*
155700010601     C* Inizializzo flag e contatori operativi
155800010601     C                   movel     '0'           flgGiro           1
155900010601     C                   movel     '0'           flgMulti          1
156000010601     C                   movel     '1'           flgStato          1
156100010615     C                   movel     '0'           flgOk             1
156200010601     C                   z-add     *zeros        cntNonEl         10 0
156300010601     C                   z-add     *zeros        depfil            3 0
156400010601     C                   z-add     *zeros        invfil            3 0
156500010601     C*
156600010601     C                   ENDSR
156700010601     C***
156800070326
156900070326
157000070326
157100070326
157200080916     C     *pssr         BEGSR
157300070329     C*
157400080916     C                   if        %open(tivin00r)
157500080916     C                   close     tivin00r
157600080916     C                   endif
157700080916     C                   if        %open(fivabwwr)
157800080916     C                   close     fivabwwr
157900080916     C                   endif
158000080916     C                   if        %open(fivatwwr)
158100080916     C                   close     fivatwwr
158200080916     C                   endif
158300070326     C*
158400070326     C* Effettuo la chiamata al CLLE preposto
158500080916     C                   call(e)   'TITVVTC'
158600080916     C                   parm                    parccm
158700080916     C                   parm                    parmbr
158800080916     C                   parm      '2'           paropz
158900070326     C*
159000080916     C                   eval      wrkesito = '2'
159100070404     C*
159200080916     C                   seton                                        LR
159300070326     C*
159400080916     C                   ENDSR     '*CANCL'
159500070326     C***
159600070326
159700070326
159800010330
159900010330
160000000613     C     *inzsr        BEGSR
160100990910     C*
160200990910     C     *entry        plist
160300990920     C                   parm                    tivlrds
160400990921     C                   parm      wrkesito      esito
160500000724     C                   parm                    prmlit
160600000710     C                   parm                    prmfir
160700010330     C*
160800010330     C* CALCOLA LA DATA CORRENTE
160900010330     C                   time                    wn14             14 0
161000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
161100010330     C                   z-add     wn8           g08dat
161200010330     C                   z-add     *zeros        g08inv
161300010330     C                   movel     '0'           g08err
161400010330     C                   call      'XSRDA8'
161500010330     C                   parm                    wlbda8
161600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161700000613     C*
161800000613     C                   ENDSR
161900000613     C***
162000040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
162100081024FILLE01;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;FILLER02;
162200110915VABNOT;VABNAS;VATNOT_A;VATNOT_B;
162300080715
