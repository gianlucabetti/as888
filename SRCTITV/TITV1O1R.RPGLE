000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200080617     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
006800080617
006900080617     D*------------------
007000080617     D* LINKING A DEFINIZIONI ESTERNE
007100080617     D*------------------
007200080617     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007300090220     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400010330
007500010330
007600010330
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900081014     C                   reset                   wrkesito
008000010601     C*
008100010601     C                   exsr      opeini
008200010605     C                   exsr      rwvab
008300081014     C*
008400081014     C                   exsr      endela
008500010601     C*
008600010601     C                   seton                                        lr
008700010601
008800010601
008900010601
009000010601     C*--------------------------------------------------------
009100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010601     C*--------------------------------------------------------
009300010601     C     PREELA        BEGSR
009400010601     C*
009500010601     C* SE OCCORRE SPEDIRE IN FILIALE
009600010601     C                   if        invfil <> *zeros and
009700010601     C                             flgGiro = '0'
009800010601     C*
009900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010000010601     C                   eval      flgGiro = '1'
010100010601     C*
010200010601     C                   endif
010300010601     C*
010400010601     C                   ENDSR
010500010601     C***
010600010601
010700010601
010800010601
010900010601
011000010601     C*--------------------------------------------------------
011100010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
011200010601     C*--------------------------------------------------------
011300010601     C     ENDELA        BEGSR
011400081014     C*
011500081014     C                   if        %open(tivin00r)
011600081014     C                   close     tivin00r
011700081014     C                   endif
011800081014     C                   if        %open(fivabwwr)
011900081014     C                   close     fivabwwr
012000081014     C                   endif
012100081014     C                   if        %open(fivatwwr)
012200081014     C                   close     fivatwwr
012300081014     C                   endif
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
013600000613     C*
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
017000081016     C***                exsr      inzvar
017100081016     C***                exsr      impvab
017200010330     C*
017300030822     C                   do        *HIVAL
017400010330     C*
017500030822     C                   read      tivin00r                               70
017600010618     C*
017700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
017800010618     C                   if        vinflg = '1'
017900010618     C                   eval      flgOk = '1'
018000010618     C                   endif
018100040510     C*
018200040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
018300040510     C                   z-add     *zeros        wLenVINDTA        4 0
018400040510     C                   z-add     *zeros        wFlgVINDTA        1 0
018500040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
018600040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
018700040510     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
018800040510     C                   z-add     1             wFlgVINDTA
018900040510     C                   leave
019000040510     C                   endif
019100040510     C                   enddo
019200010618     C*
019300040510     C                   if        vindta > *blanks AND
019400040510     C                             wFlgVINDTA = 1
019500010330     C                   add       1             rrnum
019600010330     C*
019700010601     C                   if        *in70 = *off and
019800010330     C                             (vinflg = *blanks
019900010330     C                              or vinflg = '0'
020000010330     C                              or vinflg = '2')
020100010330     C*
020200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
020300010711     C                   if        vinflg = *blanks or vinflg = '0'
020400010711     C                   clear                   vinmsg
020500010711     C                   endif
020600010601     C*
020700070925     C* Aggiungo un byte a blanks all'inizio d ogni record
020800070925     C                   eval      vindta  = ' ' + %trim(vindta)
020900070925     C*
021000010330     C                   exsr      inzvar
021100010605     C                   exsr      impvab
021200010601     C*
021300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021400010605     C                   eval      depfil = VABLNP
021500010601     C                   exsr      repfil
021600010601     C                   if        depfil = invfil
021700021025     C                   if        vlrpoi = 999
021800030822     C                   move(P)   invfil        VABFGS
021900021025     C                   else
022000030822     C                   move(P)   vlrpoi        VABFGS
022100021025     C                   endif
022200010601     C*
022300010601     C                   exsr      PREELA
022400010601     C*
022500010604     C* Ebbene...
022600010604     C*
022700040119     C  N31              add       1             §CTROKVB
022800040119     C   32              add       1             §CTRMO
022900040119     C   31              add       1             §CTRNO
023000030822     C                   if        wGiro = 2
023100040510     C* Scrivo sempre il VAT
023200040510     C  N31              exsr      wrivat
023300040510     C* Scrivo il VAB solo a rottura d spedizione
023400040119     C                   if        depspe =  *zeros OR
023500040119     C                             depspe <> vabNSP
023600040119     C  N31              exsr      wrivab
023700040119     C  N31              eval      depspe = vabNSP
023800040119     C                   endif
023900040119     C                   endif
024000010604     C*
024100010604     C                   if        *in31 = *off and
024200010604     C                             *in32 = *off
024300010604     C                   eval      vinflg = '1'
024400010604     C                   else
024500010604     C                   eval      vinflg = '2'
024600010604     C                   endif
024700010604     C                   endif
024800010601     C*
024900010604     C                   endif
025000010604     C*
025100010330     C                   else
025200010330     C                   eval      vinflg = '1'
025300010330     C                   endif
025400010601     C*
025500010601     C  N70              update    tivin000
025600010330     C*
025700030822     C  N70              enddo
025800040119     C*
025900040119     C                   endif                                                  (endif - chkcall)
026000010601     C*
026100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026200010601     C                   if        cntNonEl = *zeros or
026300010601     C                             flgMulti = '0'
026400010330     C* Se non ci sono record con errori ...
026500010601     C                   if        §ctrno = 0 and
026600010604     C                             §ctrmo = 0 and
026700010601     C                             flgStato <> '2'
026800010330     C* ... restituisco esito OK.
026900010330     C                   eval      wrkesito = '0'
027000010330     C                   else
027100040119     C                   if        §ctrokvb > 0
027200010330     C                   eval      wrkesito = '1'
027300010330     C                   else
027400010615     C                   if        flgOk = '0'
027500010615     C                   eval      wrkesito = '2'
027600010615     C                   else
027700010615     C                   eval      wrkesito = '6'
027800010615     C                   endif
027900010330     C                   endif
028000010330     C                   endif
028100010601     C                   else
028200010601     C                   eval      wrkesito = '9'
028300010601     C                   endif
028400010330     C*
028500010330     C                   if        %open(tivin00r)
028600010330     C                   close     tivin00r
028700010330     C                   endif
028800021025     C                   if        %open(fivabwwr)
028900021025     C                   close     fivabwwr
029000010330     C                   endif
029100040512     C                   if        %open(fivatwwr)
029200040512     C                   close     fivatwwr
029300040512     C                   endif
029400010601     C*
029500010601     C                   if        vlrpoi <> 999
029600010601     C                   eval      invfil = vlrpoi
029700010601     C                   endif
029800010330     C*
029900040119     C                   if        §ctrokvb > 0
030000010601     C                             and invfil > *zeros
030100010330     C                   exsr      invio
030200010330     C                   endif
030300010330     C*
030400010330     C                   ENDSR
030500010330     C***
030600040119
030700040119
030800040119
030900040119     C*----------------------------------------------------*
031000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031100040119     C*----------------------------------------------------*
031200040510     C     WRIVAB        BEGSR
031300040119     C*
031400060331     C* Gestisco l'eventuale rottura x numero spedizione
031500070823     C                   if        VABNSP <> depspe
031600070301     C                   if        wVATNOT_E <> *blanks
031700080125     C                   if        VABCTM = *blanks
031800080125     C                   eval      VABCTM = '7Q'
031900080125     C                   endif
032000070301     C                   endif
032100040119     C                   write     fivab000                                     => scarico il VAB
032200060331     C                   endif
032300040119     C*
032400040119     C                   ENDSR
032500040119
032600040119
032700040119
032800040119     C*----------------------------------------------------*
032900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033000040119     C*----------------------------------------------------*
033100040510     C     WRIVAT        BEGSR
033200060512     C*
033300060512     C* Inizializzo i campi chiave
033400060512     C                   eval      VATFGS = VABFGS
033500060512     C                   eval      VATAAS = VABAAS
033600060512     C                   eval      VATCCM = VABCCM
033700070823     C                   eval      VATNRS = VABNRS
033800060512     C                   eval      VATNSP = VABNSP
033900060512     C                   eval      VATLNP = VABLNP
034000060331     C*
034100060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
034200060331     C                   if        wVATNOT_A <> *blanks
034300060331     C                   eval      VATNOT = wVATNOT_A
034400060331     C                   eval      VATTRC = 'A'
034500060331     C                   write     FIVAT000
034600060331     C                   add       1             §CTROKVT
034700060331     C                   endif
034800060331     C*
034900060331     C                   if        wVATNOT_B <> *blanks
035000060331     C                   eval      VATNOT = wVATNOT_B
035100060331     C                   eval      VATTRC = 'B'
035200060331     C                   write     FIVAT000
035300060331     C                   add       1             §CTROKVT
035400060331     C                   endif
035500060331     C*
035600080625     C                   if        wVATNOT_E <> *blanks
035700060331     C                   eval      VATNOT = wVATNOT_E
035800060331     C                   eval      VATTRC = 'E'
035900080716     C                   write     FIVAT000
036000080716     C                   add       1             §CTROKVT
036100080625     C                   endif
036200040119     C*
036300040119     C                   ENDSR
036400040119
036500040119
036600040119
036700040119     C*----------------------------------------------------*
036800040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
036900040119     C*----------------------------------------------------*
037000040119     C     PREVAT        BEGSR
037100040119     C*
037200040119     C* Compongo il nome del membro da dare al FIVATWWR
037300040119     C                   eval      parmbr = vlrhdl
037400040119     C                   movel     'M'           parmbr
037500040510     C                   eval      parccm = vlrksc
037600040119     C                   eval      paropz = '1'
037700040119     C* Effettuo la chiamata al CLLE preposto
037800040119     C                   call(e)   'TITVVTC'
037900040119     C                   parm                    parccm
038000040119     C                   parm                    parmbr
038100040119     C                   parm                    paropz
038200040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
038300040119     C                   if        %error
038400040119     C                   movel     '1'           chkcall
038500040119     C                   else
038600040119     C                   movel     '0'           chkcall
038700040119     C                   endif
038800040119     C*
038900040119     C                   ENDSR
039000010601
039100010601
039200010601
039300010330     C*----------------------------------------------------*
039400030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039500010330     C*----------------------------------------------------*
039600040119     C     INZVAR        BEGSR
039700010330     C*
039800030822     C* Inizializzo variabili di wrk
039900020204     C                   Z-ADD     *zeros        Num5_0            5 0
040000060331     C                   MOVEL     *blanks       wVATNOT_A
040100060331     C                   MOVEL     *blanks       wVATNOT_B
040200060331     C                   MOVEL     *blanks       wVATNOT_E
040300090220     C*
040400080617     C                   EVAL      wFlgCAS = *blanks
040500030822     C*
040600030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040700030822     C                   CLEAR                   FIVAB000
040800040119     C                   CLEAR                   FIVAT000
040900030822     C                   CLEAR                   SkSplitCSV
041000030822     C                   EVAL      i = 1
041100030822     C                   EVAL      posDa = *zeros
041200030822     C                   EVAL      posA  = *zeros
041300030822     C*
041400030822     C* Reimposto i valori di default
041500030822     C                   EXSR      DEFCAM
041600030822     C*
041700010330     C                   ENDSR
041800010330     C*----------------------------------------------------*
041900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042000010330     C*----------------------------------------------------*
042100010330     C     DEFCAM        BEGSR
042200030822     C*
042300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042400030715     C* e delimitatore testo.
042500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
042600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
042700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
042800030822     C*
042900030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043100030715     C                   SETON                                        50
043200030715     C                   ELSE
043300030715     C                   SETOFF                                       50
043400030715     C                   ENDIF
043500030822     C*
043600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043700030822     C                   EVAL      posDaDft = 1
043800030822     C                   EVAL      posADft  = 0
043900030822     C                   EVAL      wGiroDft = 0
044000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044100030822     C                             posDaDft > 0
044200030822     C*
044300030822     C* Gestisco il 1° giro
044400030822     C                   IF        wGiroDft = 0
044500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044700030822     C* Incremento il contatore dei "giri"
044800030822     C                   EVAL      wGiroDft = 1
044900030822     C                   ELSE
045000030822     C                   EVAL      posDaDft = posADft
045100030822     C                   ENDIF
045200030822     C* Eseguo lo scan x trovare la fine del campo corrente
045300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045400030822     C*
045500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045700030822     C                   IF        posDaDft > 0 AND
045800030822     C                             posADft  > 0
045900030822     C* NCL
046000030822     C                   IF        %subst(
046100030822     C                             %subst(vlrppt:posDaDft+1:
046200030822     C                             posADft-posDaDft-1):1:3)
046300030822     C                             = 'NCL'
046400030822     C                   EVAL      PiStr=%trim(%subst(
046500030822     C                             %subst(vlrppt:posDaDft+1:
046600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046700030822     C                   EXSR      CHKNUM
046800030822     C                   IF        PiInt=*on
046900030822     C                   Z-ADD     PiVal         VABNCL
047000030822     C                   ENDIF
047100030822     C                   ENDIF
047200030822     C* CCM
047300030822     C                   IF        %subst(
047400030822     C                             %subst(vlrppt:posDaDft+1:
047500030822     C                             posADft-posDaDft-1):1:3)
047600030822     C                             = 'CCM'
047700030822     C                   EVAL      PiStr=%trim(%subst(
047800030822     C                             %subst(vlrppt:posDaDft+1:
047900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048000030822     C                   EXSR      CHKNUM
048100030822     C                   IF        PiInt=*on
048200030822     C                   Z-ADD     PiVal         VABCCM
048300030822     C                   ENDIF
048400030822     C                   ENDIF
048500030822     C* LNP
048600030822     C                   IF        %subst(
048700030822     C                             %subst(vlrppt:posDaDft+1:
048800030822     C                             posADft-posDaDft-1):1:3)
048900030822     C                             = 'LNP'
049000030822     C                   EVAL      PiStr=%trim(%subst(
049100030822     C                             %subst(vlrppt:posDaDft+1:
049200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049300030822     C                   EXSR      CHKNUM
049400030822     C                   IF        PiInt=*on
049500030822     C                   Z-ADD     PiVal         VABLNP
049600030822     C                   ENDIF
049700030822     C                   ENDIF
049800030822     C* NRS
049900030822     C                   IF        %subst(
050000030822     C                             %subst(vlrppt:posDaDft+1:
050100030822     C                             posADft-posDaDft-1):1:3)
050200030822     C                             = 'NRS'
050300030822     C                   EVAL      PiStr=%trim(%subst(
050400030822     C                             %subst(vlrppt:posDaDft+1:
050500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050600030822     C                   EXSR      CHKNUM
050700030822     C                   IF        PiInt=*on
050800030822     C                   Z-ADD     PiVal         VABNRS
050900030822     C                   ENDIF
051000030822     C                   ENDIF
051100030822     C* CTR
051200030822     C                   IF        %subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):1:3)
051500030822     C                             = 'CTR'
051600030822     C                   EVAL      PiStr=%trim(%subst(
051700030822     C                             %subst(vlrppt:posDaDft+1:
051800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051900030822     C                   EXSR      CHKNUM
052000030822     C                   IF        PiInt=*on
052100030822     C                   Z-ADD     PiVal         VABCTR
052200030822     C                   ENDIF
052300030822     C                   ENDIF
052400030822     C* PKB
052500030822     C                   IF        %subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):1:3)
052800030822     C                             = 'PKB'
052900030822     C                   EVAL      PiStr=%trim(%subst(
053000030822     C                             %subst(vlrppt:posDaDft+1:
053100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053200030822     C                   EXSR      CHKNUM
053300030822     C                   IF        PiNum=*on
053400030822     C                   Z-ADD     PiVal         VABPKB
053500030822     C                   ENDIF
053600030822     C                   ENDIF
053700030822     C* VLB
053800030822     C                   IF        %subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):1:3)
054100030822     C                             = 'VLB'
054200030822     C                   EVAL      PiStr=%trim(%subst(
054300030822     C                             %subst(vlrppt:posDaDft+1:
054400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054500030822     C                   EXSR      CHKNUM
054600030822     C                   IF        PiNum=*on
054700030822     C                   Z-ADD     PiVal         VABVLB
054800030822     C                   ENDIF
054900030822     C                   ENDIF
055000030822     C* QFT
055100030822     C                   IF        %subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):1:3)
055400030822     C                             = 'QFT'
055500030822     C                   EVAL      PiStr=%trim(%subst(
055600030822     C                             %subst(vlrppt:posDaDft+1:
055700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055800030822     C                   EXSR      CHKNUM
055900030822     C                   IF        PiNum=*on
056000030822     C                   Z-ADD     PiVal         VABQFT
056100030822     C                   ENDIF
056200030822     C                   ENDIF
056300030822     C* CBO
056400030822     C                   IF        %subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):1:3)
056700030822     C                             = 'CBO'
056800030822     C                   EVAL      VABCBO=%trim(%subst(
056900030822     C                             %subst(vlrppt:posDaDft+1:
057000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057100030822     C                   ENDIF
057200030822     C* TSP
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'TSP'
057700030822     C                   EVAL      VABTSP=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   ENDIF
058100030822     C* VAS
058200030822     C                   IF        %subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):1:3)
058500030822     C                             = 'VAS'
058600030822     C                   EVAL      VABVAS=%trim(%subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058900030822     C                   ENDIF
059000030822     C* VCA
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'VCA'
059500030822     C                   EVAL      VABVCA=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   ENDIF
059900070925     C* TIC
060000070925     C                   IF        %subst(
060100070925     C                             %subst(vlrppt:posDaDft+1:
060200070925     C                             posADft-posDaDft-1):1:3)
060300070925     C                             = 'TIC'
060400070925     C                   EVAL      VABTIC=%trim(%subst(
060500070925     C                             %subst(vlrppt:posDaDft+1:
060600070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060700070925     C                   ENDIF
060800030822     C* GCA
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'GCA'
061300030822     C                   EVAL      VABGCA=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   ENDIF
061700030822     C* CTM
061800030822     C                   IF        %subst(
061900030822     C                             %subst(vlrppt:posDaDft+1:
062000030822     C                             posADft-posDaDft-1):1:3)
062100030822     C                             = 'CTM'
062200030822     C                   EVAL      VABCTM=%trim(%subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062500030822     C                   ENDIF
062600030822     C* FFD
062700030822     C                   IF        %subst(
062800030822     C                             %subst(vlrppt:posDaDft+1:
062900030822     C                             posADft-posDaDft-1):1:3)
063000030822     C                             = 'FFD'
063100030822     C                   EVAL      VABFFD=%trim(%subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063400030822     C                   ENDIF
063500030822     C* VAD
063600030822     C                   IF        %subst(
063700030822     C                             %subst(vlrppt:posDaDft+1:
063800030822     C                             posADft-posDaDft-1):1:3)
063900030822     C                             = 'VAD'
064000030822     C                   EVAL      VABVAD=%trim(%subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064300030822     C                   ENDIF
064400030822     C* GMA
064500030822     C                   IF        %subst(
064600030822     C                             %subst(vlrppt:posDaDft+1:
064700030822     C                             posADft-posDaDft-1):1:3)
064800030822     C                             = 'GMA'
064900030822     C                   EVAL      VABGMA=%trim(%subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065200030822     C                   ENDIF
065300030822     C* GGA
065400030822     C                   IF        %subst(
065500030822     C                             %subst(vlrppt:posDaDft+1:
065600030822     C                             posADft-posDaDft-1):1:3)
065700030822     C                             = 'GGA'
065800030822     C                   EVAL      VABGGA=%trim(%subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066100030822     C                   ENDIF
066200030822     C* GVA
066300030822     C                   IF        %subst(
066400030822     C                             %subst(vlrppt:posDaDft+1:
066500030822     C                             posADft-posDaDft-1):1:3)
066600030822     C                             = 'GVA'
066700030822     C                   EVAL      VABGVA=%trim(%subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067000030822     C                   ENDIF
067100030822     C* TC1
067200030822     C                   IF        %subst(
067300030822     C                             %subst(vlrppt:posDaDft+1:
067400030822     C                             posADft-posDaDft-1):1:3)
067500030822     C                             = 'TC1'
067600030822     C                   EVAL      VABTC1=%trim(%subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067900030822     C                   ENDIF
068000030822     C* TC2
068100030822     C                   IF        %subst(
068200030822     C                             %subst(vlrppt:posDaDft+1:
068300030822     C                             posADft-posDaDft-1):1:3)
068400030822     C                             = 'TC2'
068500030822     C                   EVAL      VABTC2=%trim(%subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068800030822     C                   ENDIF
068900040714     C* VATTRC
069000040714     C                   IF        %subst(
069100040714     C                             %subst(vlrppt:posDaDft+1:
069200040714     C                             posADft-posDaDft-1):1:3)
069300040714     C                             = 'TRC'
069400040714     C                   EVAL      VATTRC=%trim(%subst(
069500040714     C                             %subst(vlrppt:posDaDft+1:
069600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069700040714     C                   ENDIF
069800030822     C* ...
069900030822     C                   ENDIF
070000030822     C                   ENDDO
070100020204     C*
070200010330     C                   ENDSR
070300010607     C*----------------------------------------------------*
070400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070500010607     C*----------------------------------------------------*
070600010607     C     IMPVAB        BEGSR
070700010607     C*
070800010607     C                   SETOFF                                       3132
070900030515     C*
071000010607     C                   MOVEL     datcor        VABAAS
071100010607     C                   MOVE      datcor        VABMGS
071200010607     C*
071300030715     C**********
071400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071500030715     C**********
071600030715     C                   IF        wGiro = *zeros
071700080716     C*
071800080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
071900080716     C                   EVAL      depvinDTA = vinDTA
072000040113     C*
072100040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
072200040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
072300040510     C                             %trim(intcol(3))<>*blanks
072400040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
072500040113     C                                      %trim(intcol(3))
072600070823     C     ';':CharCSV   XLATE     vindta        vindta
072700040510     C                   ENDIF
072800080626     C*
072900031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
073000031201     C     minu:maiu     XLATE     vindta        vindta
073100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
073200030715     C                   DOW       posDa <= %len(%trim(vindta))
073300030715     C*
073400030715     C* Gestisco il 1° campo
073500030715     C                   IF        i = 1
073600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
073700030715     C                   EVAL      posDa = 1
073800030715     C* Eseguo lo scan x trovare la fine del primo campo
073900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074000030715     C* A questo punto "estraggo" il campo corrente
074100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
074200030715     C                                                    (posA-posDa))
074300030715     C* X i campi successivi al 1°
074400030715     C                   ELSE
074500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
074600030715     C                   EVAL      posDa = posA + 1
074700030715     C* Eseguo lo scan x trovare la fine del campo corrente
074800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
074900030715     C* Gestisco l'ultimo campo
075000030715     C                   IF        posA = *zeros
075100030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
075200030715     C* A questo punto "estraggo" il campo corrente
075300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075400030715     C                                                    (posA-posDa))
075500030715     C                   LEAVE
075600030715     C                   ELSE
075700030715     C* A questo punto "estraggo" il campo corrente
075800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075900030715     C                                                    (posA-posDa))
076000030715     C                   ENDIF
076100030715     C                   ENDIF
076200030715     C* Incremento il contatore d campo
076300030715     C                   EVAL      i = i +1
076400030715     C                   ENDDO
076500030715     C*
076600030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
076700030715     C* delimitazione testo
076800030715     C                   EVAL      i = 1
076900030715     C                   DOW       i < %elem(SkSplitFLD)
077000030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
077100030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
077200030715     C                   EVAL      i = i + 1
077300030715     C                   ENDDO
077400030715     C                   EVAL      wGiro = 1
077500080716     C* Ripristino la 1° riga originale
077600080716     C                   EVAL      vinDTA = depvinDTA
077700030715     C                   ELSE
077800030715     C                   EVAL      wGiro = 2
077900031204     C**********
078000031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
078100031204     C* d separatore campo
078200031204     C**********
078300031204     C                   EVAL      vindta = %trim(vindta)
078400031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
078500031204     C                   EVAL      vindta = %subst(vindta:2)
078600031204     C                   ENDDO
078700031204     C*
078800031204     C                   Z-ADD     *zeros        lunghInput        4 0
078900031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079000031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
079100031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
079200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
079300031204     C                   ENDDO
079400030715     C**********
079500030715     C* Effettuo lo split del campo dati d input
079600030715     C**********
079700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079800030715     C                   DOW       posDa <= %len(%trim(vindta))
079900030715     C*
080000030715     C* Gestisco il 1° campo
080100030715     C                   IF        i = 1
080200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
080300030715     C                   EVAL      posDa = 1
080400030715     C* Eseguo lo scan x trovare la fine del primo campo
080500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080600030715     C* A questo punto "estraggo" il campo corrente
080700030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
080800030715     C                                                    (posA-posDa))
080900030715     C* X i campi successivi al 1°
081000030715     C                   ELSE
081100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
081200030715     C                   EVAL      posDa = posA + 1
081300031204     C* Verifico che nn vi sia il campo nn valorizzato
081400031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
081500031204     C* Se campo nn valorizzato skippo al prossimo
081600031204     C                   EVAL      SkSplitCSV(i) = *blanks
081700031204     C                   EVAL      posA  = posA + 1
081800031204     C                   ELSE
081900030715     C* Eseguo lo scan x trovare la fine del campo corrente
082000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082100030715     C* Gestisco l'ultimo campo
082200030715     C                   IF        posA = *zeros
082300030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
082400030715     C* A questo punto "estraggo" il campo corrente
082500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082600030715     C                                                    (posA-posDa))
082700030715     C                   LEAVE
082800030715     C                   ELSE
082900030715     C* A questo punto "estraggo" il campo corrente
083000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083100030715     C                                                    (posA-posDa))
083200030715     C                   ENDIF
083300031204     C                   ENDIF
083400030715     C                   ENDIF
083500030715     C* Incremento il contatore d campo
083600030715     C                   EVAL      i = i +1
083700030715     C                   ENDDO
083800030715     C*
083900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
084000030715     C* delimitazione testo
084100030715     C                   EVAL      i = 1
084200030715     C                   DOW       i < %elem(SkSplitCSV)
084300030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
084400030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
084500030715     C                   EVAL      i = i + 1
084600030715     C                   ENDDO
084700030715     C*
084800030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
084900030715     C                   EVAL      i = 1
085000030715     C                   DOW       i < %elem(SkSplitFLD)
085100030715     C*
085200030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
085300030715     C                   IF        SkSplitFLD(i) <> *blanks AND
085400030715     C                             SkSplitCSV(i) <> *blanks
085500030721     C*** ATB
085600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
085700030721     C                   EVAL      VABATB = SkSplitCSV(i)
085800030721     C                   ENDIF
085900030715     C*** RSD
086000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
086100030715     C                   EVAL      VABRSD = SkSplitCSV(i)
086200030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
086300030715     C     '@':'A'       XLATE     VABRSD        VABRSD
086400030715     C* ==
086500030715     C                   ENDIF
086600030721     C*** RD2
086700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
086800030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
086900030721     C                   ENDIF
087000030715     C*** IND
087100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
087200030715     C                   EVAL      VABIND = SkSplitCSV(i)
087300030715     C                   ENDIF
087400030715     C*** LOD
087500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
087600030715     C                   EVAL      VABLOD = SkSplitCSV(i)
087700030715     C                   ENDIF
087800030715     C*** PRD
087900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
088000030715     C                   EVAL      VABPRD = SkSplitCSV(i)
088100030715     C                   ENDIF
088200030721     C*** NZD
088300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
088400030721     C                   EVAL      VABNZD = SkSplitCSV(i)
088500030721     C                   ENDIF
088600030715     C*** RMA
088700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
088800030715     C                   EVAL      VABRMA = SkSplitCSV(i)
088900030715     C                   ENDIF
089000030715     C*** NOT
089100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
089200030715     C                   EVAL      VABNOT = SkSplitCSV(i)
089300030715     C                   ENDIF
089400030715     C*** NT2
089500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
089600030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
089700030715     C                   ENDIF
089800030715     C*** VCA
089900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
090000030715     C                   EVAL      VABVCA = SkSplitCSV(i)
090100030715     C                   ENDIF
090200030715     C*** VAS
090300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
090400030715     C                   EVAL      VABVAS = SkSplitCSV(i)
090500030715     C                   ENDIF
090600030715     C*** FFD
090700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
090800030715     C                   EVAL      VABFFD = SkSplitCSV(i)
090900030715     C                   ENDIF
091000030721     C*** GC1
091100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
091200030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
091300030721     C                   ENDIF
091400030721     C*** GC2
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
091600030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030721     C*** TSP
091900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
092000030721     C                   EVAL      VABTSP = SkSplitCSV(i)
092100030721     C                   ENDIF
092200040510     C*** CBO
092300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
092400040510     C                   EVAL      VABCBO = SkSplitCSV(i)
092500040510     C                   ENDIF
092600030721     C*** NAS
092700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
092800030721     C                   EVAL      VABNAS = SkSplitCSV(i)
092900030721     C                   ENDIF
093000030721     C*** TIC
093100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
093200030721     C                   EVAL      VABTIC = SkSplitCSV(i)
093300030721     C                   ENDIF
093400030721     C*** GCA
093500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
093600030721     C                   EVAL      VABGCA = SkSplitCSV(i)
093700030721     C                   ENDIF
093800030721     C*** XCO
093900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
094000030721     C                   EVAL      VABXCO = SkSplitCSV(i)
094100030721     C                   ENDIF
094200030721     C*** CTM
094300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
094400030721     C                   EVAL      VABCTM = SkSplitCSV(i)
094500030721     C                   ENDIF
094600030721     C*** TCR
094700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
094800030721     C                   EVAL      VABTCR = SkSplitCSV(i)
094900030721     C                   ENDIF
095000030721     C*** CTS
095100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
095200030721     C                   EVAL      VABCTS = SkSplitCSV(i)
095300030721     C                   ENDIF
095400030721     C*** FTM
095500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
095600030721     C                   EVAL      VABFTM = SkSplitCSV(i)
095700030721     C                   ENDIF
095800030721     C*** VAD
095900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
096000030721     C                   EVAL      VABVAD = SkSplitCSV(i)
096100030721     C                   ENDIF
096200030721     C*** GMA
096300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
096400030721     C                   EVAL      VABGMA = SkSplitCSV(i)
096500030721     C                   ENDIF
096600030721     C*** GGA
096700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
096800030721     C                   EVAL      VABGGA = SkSplitCSV(i)
096900030721     C                   ENDIF
097000030721     C*** GVA
097100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
097200030721     C                   EVAL      VABGVA = SkSplitCSV(i)
097300030721     C                   ENDIF
097400030721     C*** TC1
097500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
097600030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
097700030721     C                   ENDIF
097800030721     C*** TC2
097900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
098000030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
098100030721     C                   ENDIF
098200030721     C*** SCL
098300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
098400030721     C                   EVAL      VABSCL = SkSplitCSV(i)
098500030721     C                   ENDIF
098600030721     C*** RMO
098700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
098800030721     C                   EVAL      VABRMO = SkSplitCSV(i)
098900030721     C                   ENDIF
099000030721     C*** NMO
099100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
099200030721     C                   EVAL      VABNMO = SkSplitCSV(i)
099300030721     C                   ENDIF
099400030721     C*
099500030721     C* Reperisco quindi i campi numerici...
099600040510     C*** NSP
099700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
099800040510     C                   EVAL      PiStr=SkSplitCSV(i)
099900040510     C                   EXSR      CHKNUM
100000040510     C                   IF        PiInt=*on
100100040510     C                   Z-ADD     PiVal         VABNSP
100200040510     C                   ELSE
100300040510     C                   SETON                                        31
100400040510     C                   EVAL      VABNSP = *zeros
100500040510     C                   EVAL      vinmsg = %trimr(vinmsg)
100600040510     C                             + ' ' + 'VABNSP'
100700040510     C                   ENDIF
100800040510     C                   ENDIF
100900071106     C*** RMN
101000071106     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
101100071106     C                   EVAL      PiStr=SkSplitCSV(i)
101200071106     C                   EXSR      CHKNUM
101300071106     C                   IF        PiInt=*on
101400071106     C                   Z-ADD     PiVal         VABRMN
101500071106     C                   ELSE
101600071106     C                   SETON                                        32
101700071106     C                   EVAL      VABRMN = *zeros
101800071106     C                   EVAL      vinmsg = %trimr(vinmsg)
101900071106     C                             + ' ' + 'VABRMN'
102000071106     C                   ENDIF
102100071106     C                   ENDIF
102200030721     C*** CAD
102300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
102400030721     C                   EVAL      PiStr=SkSplitCSV(i)
102500030721     C                   EXSR      CHKNUM
102600030721     C                   IF        PiInt=*on
102700030721     C                   Z-ADD     PiVal         Num5_0
102800030721     C                   MOVEL(p)  Num5_0        VABCAD
102900030721     C                   ELSE
103000030721     C                   SETON                                        32
103100030721     C                   EVAL      VABCAD = *zeros
103200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
103300030721     C                             + ' ' + 'VABCAD'
103400030721     C                   ENDIF
103500030721     C                   ENDIF
103600030721     C*** NCL
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
103800030721     C                   EVAL      PiStr=SkSplitCSV(i)
103900030721     C                   EXSR      CHKNUM
104000030721     C                   IF        PiInt=*on
104100030721     C                   Z-ADD     PiVal         VABNCL
104200030721     C                   ELSE
104300030721     C                   SETON                                        32
104400030721     C                   EVAL      VABNCL = *zeros
104500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
104600040122     C                             + ' ' + 'VABNCL'
104700030721     C                   ENDIF
104800030721     C                   ENDIF
104900030721     C*** PKB
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
105100030721     C                   EVAL      PiStr=SkSplitCSV(i)
105200030721     C                   EXSR      CHKNUM
105300030721     C                   IF        PiNum=*on
105400030721     C                   Z-ADD     PiVal         VABPKB
105500030721     C                   ELSE
105600030721     C                   SETON                                        32
105700030721     C                   EVAL      VABPKB = *zeros
105800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105900030721     C                             + ' ' + 'VABPKB'
106000030721     C                   ENDIF
106100030721     C                   ENDIF
106200030721     C*** CAS
106300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
106400080617     C                   IF        %trim(SkSplitCSV(i)) <> *blanks  AND
106500080716     C                             %trim(SkSplitCSV(i)) <> *zeros   AND
106600081014     C                             %trim(SkSplitCSV(i)) <> '0'      AND
106700110208     C                             %trim(SkSplitCSV(i)) <> '0,00'   AND
106800110208     C                             %trim(SkSplitCSV(i)) <> '-'
106900080125     C                   EVAL      wFlgCAS = '1'
107000080125     C                   ENDIF
107100030721     C                   EVAL      PiStr=SkSplitCSV(i)
107200030721     C                   EXSR      CHKNUM
107300030721     C                   IF        PiNum=*on
107400030721     C                   Z-ADD     PiVal         VABCAS
107500030721     C                   ELSE
107600030721     C                   SETON                                        32
107700030721     C                   EVAL      VABCAS = *zeros
107800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107900030721     C                             + ' ' + 'VABCAS'
108000030721     C                   ENDIF
108100030721     C                   ENDIF
108200030721     C*** CCM
108300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
108400030721     C                   EVAL      PiStr=SkSplitCSV(i)
108500030721     C                   EXSR      CHKNUM
108600030721     C                   IF        PiInt=*on      AND
108700030721     C                             PiVal<=9999999 AND
108800030721     C                             PiVal>*zeros
108900030721     C                   Z-ADD     PiVal         VABCCM
109000030721     C                   ELSE
109100030721     C                   SETON                                        32
109200030721     C                   EVAL      VABCCM = *zeros
109300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109400040510     C                             + ' ' + 'VABCCM'
109500030721     C                   ENDIF
109600030721     C                   ENDIF
109700030721     C*** LNP
109800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
109900030721     C                   EVAL      PiStr=SkSplitCSV(i)
110000030721     C                   EXSR      CHKNUM
110100030721     C                   IF        PiInt=*on    AND
110200030721     C                             PiVal<=999   AND
110300030721     C                             PiVal>*zeros
110400030721     C                   Z-ADD     PiVal         VABLNP
110500030721     C                   ELSE
110600040715     C                   SETON                                        32
110700030721     C                   EVAL      VABLNP = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900040510     C                             + ' ' + 'VABLNP'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** NRS
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on AND
111700030721     C                             PiVal<=99
111800030721     C                   Z-ADD     PiVal         VABNRS
111900030721     C                   ELSE
112000030721     C                   SETON                                        32
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABNRS'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** LNA
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on    AND
113000030721     C                             PiVal<=999   AND
113100030721     C                             PiVal>*zeros
113200030721     C                   Z-ADD     PiVal         VABLNA
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABLNA = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700030721     C                             + ' ' + 'VABLNA'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** CTR
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on  AND
114500030721     C                             PiVal<=999
114600030721     C                   Z-ADD     PiVal         VABCTR
114700030721     C                   ELSE
114800030721     C                   SETON                                        32
114900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115000030721     C                             + ' ' + 'VABCTR'
115100030721     C                   ENDIF
115200030721     C                   ENDIF
115300030721     C*** IAS
115400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
115500030721     C                   EVAL      PiStr=SkSplitCSV(i)
115600030721     C                   EXSR      CHKNUM
115700030721     C                   IF        PiNum=*on
115800030721     C                   Z-ADD     PiVal         VABIAS
115900030721     C                   ELSE
116000030721     C                   SETON                                        32
116100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116200030721     C                             + ' ' + 'VABIAS'
116300030721     C                   ENDIF
116400030721     C                   ENDIF
116500030721     C*** VLB
116600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
116700030721     C                   EVAL      PiStr=SkSplitCSV(i)
116800030721     C                   EXSR      CHKNUM
116900030721     C                   IF        PiNum=*on
117000030721     C                   Z-ADD     PiVal         VABVLB
117100030721     C                   ELSE
117200030721     C                   SETON                                        32
117300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117400030721     C                             + ' ' + 'VABVLB'
117500030721     C                   ENDIF
117600030721     C                   ENDIF
117700030721     C*** QFT
117800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
117900030721     C                   EVAL      PiStr=SkSplitCSV(i)
118000030721     C                   EXSR      CHKNUM
118100030721     C                   IF        PiNum=*on
118200030721     C                   Z-ADD     PiVal         VABQFT
118300030721     C                   ELSE
118400030721     C                   SETON                                        32
118500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118600030721     C                             + ' ' + 'VABQFT'
118700030721     C                   ENDIF
118800030721     C                   ENDIF
118900030721     C*** NCD
119000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
119100030721     C                   EVAL      PiStr=SkSplitCSV(i)
119200030721     C                   EXSR      CHKNUM
119300030721     C                   IF        PiInt=*on      AND
119400030721     C                             PiVal<=9999999
119500030721     C                   Z-ADD     PiVal         VABNCD
119600030721     C                   ELSE
119700030721     C                   SETON                                        32
119800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119900030721     C                             + ' ' + 'VABNCD'
120000030721     C                   ENDIF
120100030721     C                   ENDIF
120200030721     C*** NCA
120300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
120400030721     C                   EVAL      PiStr=SkSplitCSV(i)
120500030721     C                   EXSR      CHKNUM
120600030721     C                   IF        PiInt=*on      AND
120700030721     C                             PiVal<=9999999
120800030721     C                   Z-ADD     PiVal         VABNCA
120900030721     C                   ELSE
121000030721     C                   SETON                                        32
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABNCA'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** ZNC
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on AND
122000030721     C                             PiVal<=99
122100030721     C                   Z-ADD     PiVal         VABZNC
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABZNC'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** DCR
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiInt=*on       AND
123300030721     C                             PiVal<=99999999
123400030721     C                   Z-ADD     PiVal         VABDCR
123500030721     C                   ELSE
123600030721     C                   SETON                                        32
123700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123800030721     C                             + ' ' + 'VABDCR'
123900030721     C                   ENDIF
124000030721     C                   ENDIF
124100030721     C*** HCR
124200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
124300030721     C                   EVAL      PiStr=SkSplitCSV(i)
124400030721     C                   EXSR      CHKNUM
124500030721     C                   IF        PiInt=*on   AND
124600030721     C                             PiVal<=9999
124700030721     C                   Z-ADD     PiVal         VABHCR
124800030721     C                   ELSE
124900030721     C                   SETON                                        32
125000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125100030721     C                             + ' ' + 'VABHCR'
125200030721     C                   ENDIF
125300030721     C                   ENDIF
125400030721     C*** VMD
125500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
125600030721     C                   EVAL      PiStr=SkSplitCSV(i)
125700030721     C                   EXSR      CHKNUM
125800030721     C                   IF        PiNum=*on
125900030721     C                   Z-ADD     PiVal         VABVMD
126000030721     C                   ELSE
126100030721     C                   SETON                                        32
126200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126300030721     C                             + ' ' + 'VABVMD'
126400030721     C                   ENDIF
126500030721     C                   ENDIF
126600030721     C*** ANT
126700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
126800030721     C                   EVAL      PiStr=SkSplitCSV(i)
126900030721     C                   EXSR      CHKNUM
127000030721     C                   IF        PiInt=*on        AND
127100030721     C                             PiVal<=999999999
127200030721     C                   Z-ADD     PiVal         VABANT
127300030721     C                   ELSE
127400030721     C                   SETON                                        32
127500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127600030721     C                             + ' ' + 'VABANT'
127700030721     C                   ENDIF
127800030721     C                   ENDIF
127900030721     C*** CMO
128000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
128100030721     C                   EVAL      PiStr=SkSplitCSV(i)
128200030721     C                   EXSR      CHKNUM
128300030721     C                   IF        PiInt=*on
128400030721     C                   Z-ADD     PiVal         Num5_0
128500030721     C                   MOVEL(p)  Num5_0        VABCMO
128600030721     C                   ELSE
128700030721     C                   SETON                                        32
128800030721     C                   EVAL      VABCMO = *zeros
128900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129000030721     C                             + ' ' + 'VABCMO'
129100030721     C                   ENDIF
129200030721     C                   ENDIF
129300040510     C*
129400040510     C* Reperisco i campi del FIVAT
129500040510     C*** VATNOT
129600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
129700040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
129800040510     C                   ENDIF
129900040510     C*** VATTRC
130000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
130100040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
130200040510     C                   ENDIF
130300060331     C*** VATNOT_A
130400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
130500060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
130600060331     C                   ENDIF
130700060331     C*** VATNOT_B
130800060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
130900060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
131000060331     C                   ENDIF
131100060331     C*** VATNOT_E
131200070823     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
131300060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
131400060331     C                   ENDIF
131500070823     C*
131600071029     C***  ===>  Gestione campi "particolari" dentro ciclo
131700090220     C*
131800070925     C*
131900071029     C***  <===  -----------------------------------------
132000040510     C*
132100030715     C                   ENDIF
132200030715     C                   EVAL      i = i + 1
132300030715     C                   ENDDO
132400070925     C*
132500071029     C***  ===>  Gestione campi "particolari" fuori ciclo
132600080125     C*
132700070925     C*
132800071029     C***  <===  -----------------------------------------
132900070925     C*
133000070925     C* Considerazioni finali su CBO/CAS
133100080125     C                   IF        wFlgCAS = '1'
133200070925     C                   IF        VABCBO = '1'
133300070925     C                   EVAL      VABCBO = '4'
133400070925     C                   ENDIF
133500070925     C                   IF        VABCBO = '2'
133600070925     C                   EVAL      VABCBO = '6'
133700070925     C                   ENDIF
133800070925     C                   ENDIF
133900070925     C***  <===  ----------------------------
134000040510     C*
134100040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
134200040510     C                   IF        VABNSP = *zeros
134300040510     C                   SETON                                        31
134400040510     C                   ENDIF
134500040715     C*
134600040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
134700040715     C                   IF        VABLNP = *zeros
134800040715     C                   SETON                                        31
134900040715     C                   ENDIF
135000040510     C*
135100040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
135200040510     C                   IF        VABRMN = *zeros
135300040510     C                   Z-ADD     VABNSP        VABRMN
135400040510     C                   ENDIF
135500040510     C*
135600040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
135700040510     C                   IF        VABRMA = *blanks
135800040714     C                   MOVEL     VABRMN        VABRMA
135900040510     C                   ENDIF
136000040128     C*
136100040510     C* Se provincia nn valorizzata la reperisco
136200040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
136300040510     C                   IF        VABPRD = *blanks
136400040128     C                   CLEAR                   TISI95DS
136500040128     C                   EVAL      I95TCN = '3'
136600040128     C                   Z-ADD     datcor        I95DAT
136700040128     C                   EVAL      I95NAR = VABNZD
136800040128     C                   EVAL      I95CAP = VABCAD
136900040128     C                   EVAL      I95LOC = VABLOD
137000040128     C                   CALL      'TISI95R'
137100040128     C                   PARM                    TISI95DS
137200040128     C                   EVAL      VABPRD = O95PRV
137300040510     C                   ENDIF
137400020204     C*
137500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
137600020204     C                   EXSR      CHKIMPDIV
137700030715     C                   ENDIF
137800020204     C*
137900010607     C                   ENDSR
138000010607     C*----------------------------------------------------*
138100010601
138200020204
138300020204     C*----------------------------------------------------*
138400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
138500020204     C*----------------------------------------------------*
138600020204     C     CHKIMPDIV     BEGSR
138700020204     C*
138800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
138900020204     C                   Z-ADD     *zeros        wrkDec            9 9
139000020204     C*
139100020204     C* Come prima cosa effettuo considerazioni sulla divisa
139200020204     C                   IF        vabIAS > *zeros
139300020204     C                   IF        vabVAS <> 'EUR'
139400020204     C                   EVAL      vabVAS =  'ITL'
139500020204     C                   ENDIF
139600020204     C                   ENDIF
139700020204     C*
139800020204     C                   IF        vabCAS > *zeros
139900020204     C                   IF        vabVCA <> 'EUR'
140000020204     C                   EVAL      vabVCA =  'ITL'
140100020204     C                   ENDIF
140200020204     C                   ENDIF
140300020204     C*
140400020204     C                   IF        vabVMD > *zeros
140500020204     C                   IF        vabVAD <> 'EUR'
140600020204     C                   EVAL      vabVAD =  'ITL'
140700020204     C                   ENDIF
140800020204     C                   ENDIF
140900020204     C*
141000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
141100020204     C                   Z-ADD     vabIAS        wrkDec
141200020204     C                   IF        wrkDec > *zeros
141300020204     C                   IF        vabVAS = 'ITL'
141400020204     C                   EVAL      vabIAS = *zeros
141500020204     C                   ENDIF
141600020204     C                   ENDIF
141700020204     C*
141800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
141900020204     C                   Z-ADD     vabCAS        wrkDec
142000020204     C                   IF        wrkDec > *zeros
142100020204     C                   IF        vabVCA = 'ITL'
142200020204     C                   EVAL      vabCAS = *zeros
142300020204     C                   ENDIF
142400020204     C                   ENDIF
142500020204     C*
142600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
142700020204     C                   Z-ADD     vabVMD        wrkDec
142800020204     C                   IF        wrkDec > *zeros
142900020204     C                   IF        vabVAD = 'ITL'
143000020204     C                   EVAL      vabVMD = *zeros
143100020204     C                   ENDIF
143200020204     C                   ENDIF
143300020204     C*
143400020204     C                   ENDSR
143500020204     C***
143600080617
143700080617
143800080617
143900080617     C*----------------------------------------------------*
144000080617     C*  CONTROLLO NUMERICITA' CAMPI
144100080617     C*----------------------------------------------------*
144200080617     C     CHKNUM        BEGSR
144300080617     C*
144400080617     C                   IF        PiDecChr = *blanks
144500080617     C                   EVAL      PiDecChr = CharNUM
144600080617     C                   ENDIF
144700080617     C*
144800080617     C                   callp     UBISNUM_Check(PiStr
144900080617     C                                          :PiDecChr
145000080617     C                                          :PiVal
145100080617     C                                          :PiNum
145200080617     C                                          :PiInt)
145300080617     C*
145400080617     C                   ENDSR
145500080617     C***
145600010330
145700010601
145800010601
145900010601
146000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
146100010601     C     repfil        BEGSR
146200010601     C*
146300010601     C                   if        invfil = *zeros and
146400010601     C                             depfil > *zeros and
146500010629     C                             (vinflg = *blanks or
146600010629     C                              vinflg = *zeros)
146700010601     C
146800010601     C                   eval      invfil = depfil
146900010601     C                   endif
147000010601     C*
147100010601     C                   if        depfil <> invfil and
147200010601     C                             invfil > *zeros
147300010601     C                   eval      flgMulti = '1'
147400010601     C                   if        vinflg = *blanks
147500010601     C                   add       1             cntNonEl
147600010601     C                   endif
147700010601     C                   endif
147800010601     C*
147900010601     C                   if        vinflg = '2'
148000010601     C                   eval      flgStato = '2'
148100010601     C                   endif
148200010601     C*
148300010601     C                   ENDSR
148400010601     C***
148500010601
148600010601
148700010601
148800010330
148900040119      /TITLE Invio dei dati al punto operativo.
149000040119     C     invio         BEGSR
149100040119     C*
149200040119     C* 1° invio FIVAT
149300040119     C                   reset                   dscmz
149400040119     C                   move      vlrpoi        cmzdst
149500040119     C                   eval      cmzfld = 'FIVATWWR'
149600040119     C                   eval      cmzmbd = vlrhdl
149700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
149800040119     C***                if        prmfir = *blanks
149900040119     C                   eval      cmzfla = 'FIVAT00F'
150000040119     C                   eval      cmzmba = 'FIVAT00F'
150100040119     C***                else
150200040119     C***                eval      cmzfla = prmfir
150300040119     C***                eval      cmzmba = prmfir
150400040119     C***                endif
150500040119     C                   eval      cmznrr = *zeros
150600040119     C                   move      §ctrokvt      cmznrr
150700040119     C                   eval      cmzlba = vlrfl1
150800040119     C                   call(e)   'TIS711C'
150900040119     C                   parm                    dscmz
151000040119     C                   parm      *blanks       esito
151100040119     C                   if        %error
151200040119     C                             or cmzerr = '1'
151300040119     C                             or esito  = '1'
151400040119     C                   eval      wrkesito = '3'
151500040119     C                   else
151600040119     C*
151700040119     C* 2° invio FIVAB
151800040119     C                   reset                   dscmz
151900040119     C                   move      vlrpoi        cmzdst
152000040119     C                   eval      cmzfld = vlrfou
152100040119     C                   eval      cmzmbd = vlrhdl
152200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152300040119     C***                if        prmfir = *blanks
152400040119     C                   eval      cmzfla = 'FIVAB00F'
152500040119     C                   eval      cmzmba = 'FIVAB00F'
152600040119     C***                else
152700040119     C***                eval      cmzfla = prmfir
152800040119     C***                eval      cmzmba = prmfir
152900040119     C***                endif
153000040119     C                   eval      cmznrr = *zeros
153100040119     C                   move      §ctrokvb      cmznrr
153200040119     C                   eval      cmzlba = vlrfl1
153300040119     C                   call(e)   'TIS711C'
153400040119     C                   parm                    dscmz
153500040119     C                   parm      *blanks       esito
153600040119     C                   if        %error
153700040119     C                             or cmzerr = '1'
153800040119     C                             or esito  = '1'
153900040119     C                   eval      wrkesito = '3'
154000040119     C                   endif
154100040119     C                   endif
154200040119     C*
154300040119     C                   ENDSR
154400040119     C***
154500010601
154600010601
154700010601
154800010601
154900010601      /TITLE Invio dei dati al punto operativo.
155000010601     C     opeini        BEGSR
155100010601     C*
155200010601     C* Inizializzo flag e contatori operativi
155300010601     C                   movel     '0'           flgGiro           1
155400010601     C                   movel     '0'           flgMulti          1
155500010601     C                   movel     '1'           flgStato          1
155600010615     C                   movel     '0'           flgOk             1
155700010601     C                   z-add     *zeros        cntNonEl         10 0
155800010601     C                   z-add     *zeros        depfil            3 0
155900010601     C                   z-add     *zeros        invfil            3 0
156000010601     C*
156100010601     C                   ENDSR
156200010601     C***
156300070326
156400070326
156500070326
156600070326
156700080916     C     *pssr         BEGSR
156800070329     C*
156900080916     C                   if        %open(tivin00r)
157000080916     C                   close     tivin00r
157100080916     C                   endif
157200080916     C                   if        %open(fivabwwr)
157300080916     C                   close     fivabwwr
157400080916     C                   endif
157500080916     C                   if        %open(fivatwwr)
157600080916     C                   close     fivatwwr
157700080916     C                   endif
157800070326     C*
157900070326     C* Effettuo la chiamata al CLLE preposto
158000080916     C                   call(e)   'TITVVTC'
158100080916     C                   parm                    parccm
158200080916     C                   parm                    parmbr
158300080916     C                   parm      '2'           paropz
158400070326     C*
158500080916     C                   eval      wrkesito = '2'
158600070404     C*
158700080916     C                   seton                                        LR
158800070326     C*
158900080916     C                   ENDSR     '*CANCL'
159000070326     C***
159100070326
159200070326
159300010330
159400010330
159500000613     C     *inzsr        BEGSR
159600990910     C*
159700990910     C     *entry        plist
159800990920     C                   parm                    tivlrds
159900990921     C                   parm      wrkesito      esito
160000000724     C                   parm                    prmlit
160100000710     C                   parm                    prmfir
160200010330     C*
160300010330     C* CALCOLA LA DATA CORRENTE
160400010330     C                   time                    wn14             14 0
160500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
160600010330     C                   z-add     wn8           g08dat
160700010330     C                   z-add     *zeros        g08inv
160800010330     C                   movel     '0'           g08err
160900010330     C                   call      'XSRDA8'
161000010330     C                   parm                    wlbda8
161100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
161200000613     C*
161300000613     C                   ENDSR
161400000613     C***
161500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
161600090327VABNSP;FILL01;VABRMA;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VABNCL;VABPKB;
161700090327VABVLB;VABCAS;VABNOT;VABNT2;VATNOT_A;VATNOT_B;VABTC1
161800080715
