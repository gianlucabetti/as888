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
001100090220     D intcol          S            100    DIM(6) CTDATA PERRCD(1)
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
003500090220     D SkSplitFLD      S             10    DIM(300)
003600090220     D SkSplitCSV      S            256    DIM(300)
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
039700090505     C*
039800090505     C                   SETOFF                                       54
039900010330     C*
040000030822     C* Inizializzo variabili di wrk
040100020204     C                   Z-ADD     *zeros        Num5_0            5 0
040200060331     C                   MOVEL     *blanks       wVATNOT_A
040300060331     C                   MOVEL     *blanks       wVATNOT_B
040400060331     C                   MOVEL     *blanks       wVATNOT_E
040500090220     C*
040600080617     C                   EVAL      wFlgCAS = *blanks
040700030822     C*
040800030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
040900030822     C                   CLEAR                   FIVAB000
041000040119     C                   CLEAR                   FIVAT000
041100030822     C                   CLEAR                   SkSplitCSV
041200030822     C                   EVAL      i = 1
041300030822     C                   EVAL      posDa = *zeros
041400030822     C                   EVAL      posA  = *zeros
041500030822     C*
041600030822     C* Reimposto i valori di default
041700030822     C                   EXSR      DEFCAM
041800030822     C*
041900010330     C                   ENDSR
042000010330     C*----------------------------------------------------*
042100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
042200010330     C*----------------------------------------------------*
042300010330     C     DEFCAM        BEGSR
042400030822     C*
042500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
042600030715     C* e delimitatore testo.
042700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
042800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
042900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
043000030822     C*
043100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
043200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
043300030715     C                   SETON                                        50
043400030715     C                   ELSE
043500030715     C                   SETOFF                                       50
043600030715     C                   ENDIF
043700030822     C*
043800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
043900030822     C                   EVAL      posDaDft = 1
044000030822     C                   EVAL      posADft  = 0
044100030822     C                   EVAL      wGiroDft = 0
044200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
044300030822     C                             posDaDft > 0
044400030822     C*
044500030822     C* Gestisco il 1° giro
044600030822     C                   IF        wGiroDft = 0
044700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
044800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
044900030822     C* Incremento il contatore dei "giri"
045000030822     C                   EVAL      wGiroDft = 1
045100030822     C                   ELSE
045200030822     C                   EVAL      posDaDft = posADft
045300030822     C                   ENDIF
045400030822     C* Eseguo lo scan x trovare la fine del campo corrente
045500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
045600030822     C*
045700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
045800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
045900030822     C                   IF        posDaDft > 0 AND
046000030822     C                             posADft  > 0
046100030822     C* NCL
046200030822     C                   IF        %subst(
046300030822     C                             %subst(vlrppt:posDaDft+1:
046400030822     C                             posADft-posDaDft-1):1:3)
046500030822     C                             = 'NCL'
046600030822     C                   EVAL      PiStr=%trim(%subst(
046700030822     C                             %subst(vlrppt:posDaDft+1:
046800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
046900030822     C                   EXSR      CHKNUM
047000030822     C                   IF        PiInt=*on
047100030822     C                   Z-ADD     PiVal         VABNCL
047200030822     C                   ENDIF
047300030822     C                   ENDIF
047400030822     C* CCM
047500030822     C                   IF        %subst(
047600030822     C                             %subst(vlrppt:posDaDft+1:
047700030822     C                             posADft-posDaDft-1):1:3)
047800030822     C                             = 'CCM'
047900030822     C                   EVAL      PiStr=%trim(%subst(
048000030822     C                             %subst(vlrppt:posDaDft+1:
048100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048200030822     C                   EXSR      CHKNUM
048300030822     C                   IF        PiInt=*on
048400030822     C                   Z-ADD     PiVal         VABCCM
048500030822     C                   ENDIF
048600030822     C                   ENDIF
048700030822     C* LNP
048800030822     C                   IF        %subst(
048900030822     C                             %subst(vlrppt:posDaDft+1:
049000030822     C                             posADft-posDaDft-1):1:3)
049100030822     C                             = 'LNP'
049200030822     C                   EVAL      PiStr=%trim(%subst(
049300030822     C                             %subst(vlrppt:posDaDft+1:
049400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
049500030822     C                   EXSR      CHKNUM
049600030822     C                   IF        PiInt=*on
049700030822     C                   Z-ADD     PiVal         VABLNP
049800030822     C                   ENDIF
049900030822     C                   ENDIF
050000030822     C* NRS
050100030822     C                   IF        %subst(
050200030822     C                             %subst(vlrppt:posDaDft+1:
050300030822     C                             posADft-posDaDft-1):1:3)
050400030822     C                             = 'NRS'
050500030822     C                   EVAL      PiStr=%trim(%subst(
050600030822     C                             %subst(vlrppt:posDaDft+1:
050700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050800030822     C                   EXSR      CHKNUM
050900030822     C                   IF        PiInt=*on
051000030822     C                   Z-ADD     PiVal         VABNRS
051100030822     C                   ENDIF
051200030822     C                   ENDIF
051300030822     C* CTR
051400030822     C                   IF        %subst(
051500030822     C                             %subst(vlrppt:posDaDft+1:
051600030822     C                             posADft-posDaDft-1):1:3)
051700030822     C                             = 'CTR'
051800030822     C                   EVAL      PiStr=%trim(%subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052100030822     C                   EXSR      CHKNUM
052200030822     C                   IF        PiInt=*on
052300030822     C                   Z-ADD     PiVal         VABCTR
052400030822     C                   ENDIF
052500030822     C                   ENDIF
052600030822     C* PKB
052700030822     C                   IF        %subst(
052800030822     C                             %subst(vlrppt:posDaDft+1:
052900030822     C                             posADft-posDaDft-1):1:3)
053000030822     C                             = 'PKB'
053100030822     C                   EVAL      PiStr=%trim(%subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053400030822     C                   EXSR      CHKNUM
053500030822     C                   IF        PiNum=*on
053600030822     C                   Z-ADD     PiVal         VABPKB
053700030822     C                   ENDIF
053800030822     C                   ENDIF
053900030822     C* VLB
054000030822     C                   IF        %subst(
054100030822     C                             %subst(vlrppt:posDaDft+1:
054200030822     C                             posADft-posDaDft-1):1:3)
054300030822     C                             = 'VLB'
054400030822     C                   EVAL      PiStr=%trim(%subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054700030822     C                   EXSR      CHKNUM
054800030822     C                   IF        PiNum=*on
054900030822     C                   Z-ADD     PiVal         VABVLB
055000030822     C                   ENDIF
055100030822     C                   ENDIF
055200030822     C* QFT
055300030822     C                   IF        %subst(
055400030822     C                             %subst(vlrppt:posDaDft+1:
055500030822     C                             posADft-posDaDft-1):1:3)
055600030822     C                             = 'QFT'
055700030822     C                   EVAL      PiStr=%trim(%subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056000030822     C                   EXSR      CHKNUM
056100030822     C                   IF        PiNum=*on
056200030822     C                   Z-ADD     PiVal         VABQFT
056300030822     C                   ENDIF
056400030822     C                   ENDIF
056500030822     C* CBO
056600030822     C                   IF        %subst(
056700030822     C                             %subst(vlrppt:posDaDft+1:
056800030822     C                             posADft-posDaDft-1):1:3)
056900030822     C                             = 'CBO'
057000030822     C                   EVAL      VABCBO=%trim(%subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057300030822     C                   ENDIF
057400030822     C* TSP
057500030822     C                   IF        %subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):1:3)
057800030822     C                             = 'TSP'
057900030822     C                   EVAL      VABTSP=%trim(%subst(
058000030822     C                             %subst(vlrppt:posDaDft+1:
058100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058200030822     C                   ENDIF
058300030822     C* VAS
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'VAS'
058800030822     C                   EVAL      VABVAS=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* VCA
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'VCA'
059700030822     C                   EVAL      VABVCA=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100070925     C* TIC
060200070925     C                   IF        %subst(
060300070925     C                             %subst(vlrppt:posDaDft+1:
060400070925     C                             posADft-posDaDft-1):1:3)
060500070925     C                             = 'TIC'
060600070925     C                   EVAL      VABTIC=%trim(%subst(
060700070925     C                             %subst(vlrppt:posDaDft+1:
060800070925     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900070925     C                   ENDIF
061000030822     C* GCA
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'GCA'
061500030822     C                   EVAL      VABGCA=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* CTM
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'CTM'
062400030822     C                   EVAL      VABCTM=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* FFD
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'FFD'
063300030822     C                   EVAL      VABFFD=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* VAD
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'VAD'
064200030822     C                   EVAL      VABVAD=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* GMA
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'GMA'
065100030822     C                   EVAL      VABGMA=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* GGA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'GGA'
066000030822     C                   EVAL      VABGGA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* GVA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'GVA'
066900030822     C                   EVAL      VABGVA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* TC1
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'TC1'
067800030822     C                   EVAL      VABTC1=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TC2
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TC2'
068700030822     C                   EVAL      VABTC2=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100040714     C* VATTRC
069200040714     C                   IF        %subst(
069300040714     C                             %subst(vlrppt:posDaDft+1:
069400040714     C                             posADft-posDaDft-1):1:3)
069500040714     C                             = 'TRC'
069600040714     C                   EVAL      VATTRC=%trim(%subst(
069700040714     C                             %subst(vlrppt:posDaDft+1:
069800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900040714     C                   ENDIF
070000030822     C* ...
070100030822     C                   ENDIF
070200030822     C                   ENDDO
070300020204     C*
070400010330     C                   ENDSR
070500010607     C*----------------------------------------------------*
070600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
070700010607     C*----------------------------------------------------*
070800010607     C     IMPVAB        BEGSR
070900010607     C*
071000010607     C                   SETOFF                                       3132
071100030515     C*
071200010607     C                   MOVEL     datcor        VABAAS
071300010607     C                   MOVE      datcor        VABMGS
071400010607     C*
071500030715     C**********
071600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
071700030715     C**********
071800030715     C                   IF        wGiro = *zeros
071900080716     C*
072000080716     C* Salvo la 1° riga originale x poi sovrapporla con quella avente le ns. intestazioni campo
072100080716     C                   EVAL      depvinDTA = vinDTA
072200040113     C*
072300040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
072400040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
072500090220     C                             %trim(intcol(3))+%trim(intcol(4))+
072600090220     C                             %trim(intcol(5))+%trim(intcol(6))
072700090220     C                             <>*blanks
072800040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
072900090220     C                                      %trim(intcol(3))+%trim(intcol(4))+
073000090220     C                                      %trim(intcol(5))+%trim(intcol(6))
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
132100090220     C*
132200090220     C*** NSP
132300090220     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABNSP'
132400090223     C                   EVAL      PiStr=%trim(SkSplitCSV(i))
132500090223     C                   EVAL      PiStr=%xlate('/':'0':PiStr:1)
132600090220     C                   EXSR      CHKNUM
132700090220     C                   IF        PiInt=*on
132800090220     C                   Z-ADD     PiVal         VABNSP
132900090220     C                   ELSE
133000090220     C                   SETON                                        31
133100090220     C                   EVAL      VABNSP = *zeros
133200090220     C                   EVAL      vinmsg = %trimr(vinmsg)
133300090220     C                             + ' ' + 'VABNSP'
133400090220     C                   ENDIF
133500090220     C                   ENDIF
133600090505     C*** TIC
133700090505     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABTIC'
133800090505     C                   IF        %trim(SkSplitCSV(i)) = 'Y'
133900090505     C                   EVAL      VABTIC = 'BM'
134000090505     C                   SETON                                        54
134100090505     C                   ENDIF
134200090505     C                   ENDIF
134300090505     C*** CAS
134400090505     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAS' AND *IN54
134500090505     C                   EVAL      wFlgCAS = '1'
134600090505     C                   EVAL      PiStr=SkSplitCSV(i)
134700090505     C                   EXSR      CHKNUM
134800090505     C                   IF        PiNum=*on
134900090505     C                   Z-ADD     PiVal         VABCAS
135000090505     C                   ELSE
135100090505     C                   SETON                                        32
135200090505     C                   EVAL      VABCAS = *zeros
135300090505     C                   EVAL      vinmsg = %trimr(vinmsg)
135400090505     C                             + ' ' + 'VABCAS'
135500090505     C                   ENDIF
135600090505     C                   ENDIF
135700070925     C*
135800071029     C***  <===  -----------------------------------------
135900040510     C*
136000030715     C                   ENDIF
136100030715     C                   EVAL      i = i + 1
136200030715     C                   ENDDO
136300070925     C*
136400071029     C***  ===>  Gestione campi "particolari" fuori ciclo
136500080125     C*
136600090223     C                   IF        VABNZD = 'ITA' or
136700090223     C                             VABNZD = 'IT'
136800090223     C                   EVAL      VABNZD = *blanks
136900090223     C                   ENDIF
137000070925     C*
137100071029     C***  <===  -----------------------------------------
137200070925     C*
137300070925     C* Considerazioni finali su CBO/CAS
137400080125     C                   IF        wFlgCAS = '1'
137500070925     C                   IF        VABCBO = '1'
137600070925     C                   EVAL      VABCBO = '4'
137700070925     C                   ENDIF
137800070925     C                   IF        VABCBO = '2'
137900070925     C                   EVAL      VABCBO = '6'
138000070925     C                   ENDIF
138100070925     C                   ENDIF
138200070925     C***  <===  ----------------------------
138300040510     C*
138400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
138500040510     C                   IF        VABNSP = *zeros
138600040510     C                   SETON                                        31
138700040510     C                   ENDIF
138800040715     C*
138900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
139000040715     C                   IF        VABLNP = *zeros
139100040715     C                   SETON                                        31
139200040715     C                   ENDIF
139300040510     C*
139400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
139500040510     C                   IF        VABRMN = *zeros
139600040510     C                   Z-ADD     VABNSP        VABRMN
139700040510     C                   ENDIF
139800040510     C*
139900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
140000040510     C                   IF        VABRMA = *blanks
140100040714     C                   MOVEL     VABRMN        VABRMA
140200040510     C                   ENDIF
140300040128     C*
140400040510     C* Se provincia nn valorizzata la reperisco
140500040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
140600040510     C                   IF        VABPRD = *blanks
140700040128     C                   CLEAR                   TISI95DS
140800040128     C                   EVAL      I95TCN = '3'
140900040128     C                   Z-ADD     datcor        I95DAT
141000040128     C                   EVAL      I95NAR = VABNZD
141100040128     C                   EVAL      I95CAP = VABCAD
141200040128     C                   EVAL      I95LOC = VABLOD
141300040128     C                   CALL      'TISI95R'
141400040128     C                   PARM                    TISI95DS
141500040128     C                   EVAL      VABPRD = O95PRV
141600040510     C                   ENDIF
141700020204     C*
141800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
141900020204     C                   EXSR      CHKIMPDIV
142000030715     C                   ENDIF
142100020204     C*
142200010607     C                   ENDSR
142300010607     C*----------------------------------------------------*
142400010601
142500020204
142600020204     C*----------------------------------------------------*
142700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
142800020204     C*----------------------------------------------------*
142900020204     C     CHKIMPDIV     BEGSR
143000020204     C*
143100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
143200020204     C                   Z-ADD     *zeros        wrkDec            9 9
143300020204     C*
143400020204     C* Come prima cosa effettuo considerazioni sulla divisa
143500020204     C                   IF        vabIAS > *zeros
143600020204     C                   IF        vabVAS <> 'EUR'
143700020204     C                   EVAL      vabVAS =  'ITL'
143800020204     C                   ENDIF
143900020204     C                   ENDIF
144000020204     C*
144100020204     C                   IF        vabCAS > *zeros
144200020204     C                   IF        vabVCA <> 'EUR'
144300020204     C                   EVAL      vabVCA =  'ITL'
144400020204     C                   ENDIF
144500020204     C                   ENDIF
144600020204     C*
144700020204     C                   IF        vabVMD > *zeros
144800020204     C                   IF        vabVAD <> 'EUR'
144900020204     C                   EVAL      vabVAD =  'ITL'
145000020204     C                   ENDIF
145100020204     C                   ENDIF
145200020204     C*
145300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
145400020204     C                   Z-ADD     vabIAS        wrkDec
145500020204     C                   IF        wrkDec > *zeros
145600020204     C                   IF        vabVAS = 'ITL'
145700020204     C                   EVAL      vabIAS = *zeros
145800020204     C                   ENDIF
145900020204     C                   ENDIF
146000020204     C*
146100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
146200020204     C                   Z-ADD     vabCAS        wrkDec
146300020204     C                   IF        wrkDec > *zeros
146400020204     C                   IF        vabVCA = 'ITL'
146500020204     C                   EVAL      vabCAS = *zeros
146600020204     C                   ENDIF
146700020204     C                   ENDIF
146800020204     C*
146900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
147000020204     C                   Z-ADD     vabVMD        wrkDec
147100020204     C                   IF        wrkDec > *zeros
147200020204     C                   IF        vabVAD = 'ITL'
147300020204     C                   EVAL      vabVMD = *zeros
147400020204     C                   ENDIF
147500020204     C                   ENDIF
147600020204     C*
147700020204     C                   ENDSR
147800020204     C***
147900080617
148000080617
148100080617
148200080617     C*----------------------------------------------------*
148300080617     C*  CONTROLLO NUMERICITA' CAMPI
148400080617     C*----------------------------------------------------*
148500080617     C     CHKNUM        BEGSR
148600080617     C*
148700080617     C                   IF        PiDecChr = *blanks
148800080617     C                   EVAL      PiDecChr = CharNUM
148900080617     C                   ENDIF
149000080617     C*
149100080617     C                   callp     UBISNUM_Check(PiStr
149200080617     C                                          :PiDecChr
149300080617     C                                          :PiVal
149400080617     C                                          :PiNum
149500080617     C                                          :PiInt)
149600080617     C*
149700080617     C                   ENDSR
149800080617     C***
149900010330
150000010601
150100010601
150200010601
150300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
150400010601     C     repfil        BEGSR
150500010601     C*
150600010601     C                   if        invfil = *zeros and
150700010601     C                             depfil > *zeros and
150800010629     C                             (vinflg = *blanks or
150900010629     C                              vinflg = *zeros)
151000010601     C
151100010601     C                   eval      invfil = depfil
151200010601     C                   endif
151300010601     C*
151400010601     C                   if        depfil <> invfil and
151500010601     C                             invfil > *zeros
151600010601     C                   eval      flgMulti = '1'
151700010601     C                   if        vinflg = *blanks
151800010601     C                   add       1             cntNonEl
151900010601     C                   endif
152000010601     C                   endif
152100010601     C*
152200010601     C                   if        vinflg = '2'
152300010601     C                   eval      flgStato = '2'
152400010601     C                   endif
152500010601     C*
152600010601     C                   ENDSR
152700010601     C***
152800010601
152900010601
153000010601
153100010330
153200040119      /TITLE Invio dei dati al punto operativo.
153300040119     C     invio         BEGSR
153400040119     C*
153500040119     C* 1° invio FIVAT
153600040119     C                   reset                   dscmz
153700040119     C                   move      vlrpoi        cmzdst
153800040119     C                   eval      cmzfld = 'FIVATWWR'
153900040119     C                   eval      cmzmbd = vlrhdl
154000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154100040119     C***                if        prmfir = *blanks
154200040119     C                   eval      cmzfla = 'FIVAT00F'
154300040119     C                   eval      cmzmba = 'FIVAT00F'
154400040119     C***                else
154500040119     C***                eval      cmzfla = prmfir
154600040119     C***                eval      cmzmba = prmfir
154700040119     C***                endif
154800040119     C                   eval      cmznrr = *zeros
154900040119     C                   move      §ctrokvt      cmznrr
155000040119     C                   eval      cmzlba = vlrfl1
155100040119     C                   call(e)   'TIS711C'
155200040119     C                   parm                    dscmz
155300040119     C                   parm      *blanks       esito
155400040119     C                   if        %error
155500040119     C                             or cmzerr = '1'
155600040119     C                             or esito  = '1'
155700040119     C                   eval      wrkesito = '3'
155800040119     C                   else
155900040119     C*
156000040119     C* 2° invio FIVAB
156100040119     C                   reset                   dscmz
156200040119     C                   move      vlrpoi        cmzdst
156300040119     C                   eval      cmzfld = vlrfou
156400040119     C                   eval      cmzmbd = vlrhdl
156500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
156600040119     C***                if        prmfir = *blanks
156700040119     C                   eval      cmzfla = 'FIVAB00F'
156800040119     C                   eval      cmzmba = 'FIVAB00F'
156900040119     C***                else
157000040119     C***                eval      cmzfla = prmfir
157100040119     C***                eval      cmzmba = prmfir
157200040119     C***                endif
157300040119     C                   eval      cmznrr = *zeros
157400040119     C                   move      §ctrokvb      cmznrr
157500040119     C                   eval      cmzlba = vlrfl1
157600040119     C                   call(e)   'TIS711C'
157700040119     C                   parm                    dscmz
157800040119     C                   parm      *blanks       esito
157900040119     C                   if        %error
158000040119     C                             or cmzerr = '1'
158100040119     C                             or esito  = '1'
158200040119     C                   eval      wrkesito = '3'
158300040119     C                   endif
158400040119     C                   endif
158500040119     C*
158600040119     C                   ENDSR
158700040119     C***
158800010601
158900010601
159000010601
159100010601
159200010601      /TITLE Invio dei dati al punto operativo.
159300010601     C     opeini        BEGSR
159400010601     C*
159500010601     C* Inizializzo flag e contatori operativi
159600010601     C                   movel     '0'           flgGiro           1
159700010601     C                   movel     '0'           flgMulti          1
159800010601     C                   movel     '1'           flgStato          1
159900010615     C                   movel     '0'           flgOk             1
160000010601     C                   z-add     *zeros        cntNonEl         10 0
160100010601     C                   z-add     *zeros        depfil            3 0
160200010601     C                   z-add     *zeros        invfil            3 0
160300010601     C*
160400010601     C                   ENDSR
160500010601     C***
160600070326
160700070326
160800070326
160900070326
161000080916     C     *pssr         BEGSR
161100070329     C*
161200080916     C                   if        %open(tivin00r)
161300080916     C                   close     tivin00r
161400080916     C                   endif
161500080916     C                   if        %open(fivabwwr)
161600080916     C                   close     fivabwwr
161700080916     C                   endif
161800080916     C                   if        %open(fivatwwr)
161900080916     C                   close     fivatwwr
162000080916     C                   endif
162100070326     C*
162200070326     C* Effettuo la chiamata al CLLE preposto
162300080916     C                   call(e)   'TITVVTC'
162400080916     C                   parm                    parccm
162500080916     C                   parm                    parmbr
162600080916     C                   parm      '2'           paropz
162700070326     C*
162800080916     C                   eval      wrkesito = '2'
162900070404     C*
163000080916     C                   seton                                        LR
163100070326     C*
163200080916     C                   ENDSR     '*CANCL'
163300070326     C***
163400070326
163500070326
163600010330
163700010330
163800000613     C     *inzsr        BEGSR
163900990910     C*
164000990910     C     *entry        plist
164100990920     C                   parm                    tivlrds
164200990921     C                   parm      wrkesito      esito
164300000724     C                   parm                    prmlit
164400000710     C                   parm                    prmfir
164500010330     C*
164600010330     C* CALCOLA LA DATA CORRENTE
164700010330     C                   time                    wn14             14 0
164800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
164900010330     C                   z-add     wn8           g08dat
165000010330     C                   z-add     *zeros        g08inv
165100010330     C                   movel     '0'           g08err
165200010330     C                   call      'XSRDA8'
165300010330     C                   parm                    wlbda8
165400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
165500000613     C*
165600000613     C                   ENDSR
165700000613     C***
165800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
165900090512X;X;VABRMA;X;X;X;W_VABNSP;X;X;X;X;W_VABTIC;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
166000090512X;X;X;X;X;X;X;X;X;VABNCL;X;X;X;VABNAS;VABPKB;X;X;X;X;X;X;X;X;VABDCR;X;X;
166100100211X;X;X;VABRSD;VABIND;X;VABCAD;VABLOD;VABPRD;X;X;VABNZD;X;X;VATNOT_B;X;X;
166200090512X;X;X;X;X;X;X;X;X;VABNOT;VABNT2;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;X;
166300090505X;X;W_VABCAS;X;X
166400090220
