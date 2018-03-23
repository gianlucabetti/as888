000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
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
003000030715     D*------------------
003100030715     D* VARIABILI X LO SPLIT DELLA STRINGA CSV IN CAMPI
003200030715     D*------------------
003300030715     D SkSplitFLD      S             10    DIM(100)
003400030715     D SkSplitCSV      S            256    DIM(100)
003500030715     D CharCSV         S              1
003600030715     D CharTXT         S              1
003700030715     D CharNUM         S              1
003800090324     D CharSOS         S              1
003900030715     D posDa           S              3  0 INZ(*zeros)
004000030715     D posA            S              3  0 INZ(*zeros)
004100030715     D i               s              3  0 INZ(1)
004200030715     D wGiro           s              1  0 INZ(*zeros)
004300060331     D wVATNOT_A       s                   LIKE(VATNOT)
004400060331     D wVATNOT_B       s                   LIKE(VATNOT)
004500060331     D wVATNOT_E       s                   LIKE(VATNOT)
004600060331     D wVABNSP         s                   LIKE(VABNSP) INZ
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
006700010330
006800010330
006900010330
007000000913     C                   reset                   rrnum
007100990921     C                   reset                   esito
007200990921     C                   reset                   wrkesito
007300010601     C*
007400010601     C                   exsr      opeini
007500010605     C                   exsr      rwvab
007600010601     C*
007700010601     C                   seton                                        lr
007800010601
007900010601
008000010601
008100010601     C*--------------------------------------------------------
008200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008300010601     C*--------------------------------------------------------
008400010601     C     PREELA        BEGSR
008500010601     C*
008600010601     C* SE OCCORRE SPEDIRE IN FILIALE
008700010601     C                   if        invfil <> *zeros and
008800010601     C                             flgGiro = '0'
008900010601     C*
009000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009100010601     C                   eval      flgGiro = '1'
009200010601     C*
009300010601     C                   endif
009400010601     C*
009500010601     C                   ENDSR
009600010601     C***
009700010601
009800010601
009900010601
010000010601
010100010601     C*--------------------------------------------------------
010200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010300010601     C*--------------------------------------------------------
010400010601     C     ENDELA        BEGSR
010500040119     C*
010600040119     C* Effettuo la chiamata al CLLE preposto
010700040119     C                   call(e)   'TITVVTC'
010800040510     C                   parm                    parccm
010900040119     C                   parm                    parmbr
011000040119     C                   parm      '2'           paropz
011100050201     C*
011200050201     C* Effettuo lancio TISI95 solo x chiusura
011300050201     C                   CLEAR                   TISI95DS
011400050201     C                   EVAL      I95TLA = 'C'
011500050201     C                   CALL      'TISI95R'
011600050201     C                   PARM                    TISI95DS
011700000616     C*
011800010601     C                   ENDSR
011900010601     C***
012000000613
012100010601
012200010601
012300010330     C*--------------------------------------------------------
012400021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012500010330     C*--------------------------------------------------------
012600010605     C     RWVAB         BEGSR
012700010330     C*
012800010330     C                   if        not %open(tivin00r)
012900010330     C                   open      tivin00r
013000010330     C                   endif
013100021025     C                   if        not %open(fivabwwr)
013200021025     C                   open      fivabwwr
013300010330     C                   endif
013400040119     C*
013500040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013600040119     C                   exsr      prevat
013700040119     C*
013800040119     C                   if        chkcall = '0'
013900040119     C*
014000040119     C                   if        not %open(fivatwwr)
014100040119     C                   open      fivatwwr
014200040119     C                   endif
014300010330     C*
014400040119     C                   clear                   §CTROKVB          7 0
014500040119     C                   clear                   §CTROKVT          7 0
014600040119     C                   clear                   §CTRMO            7 0
014700040119     C                   clear                   §CTRNO            7 0
014800010330     C*
014900030822     C                   do        *HIVAL
015000010330     C*
015100030822     C                   read      tivin00r                               70
015200010618     C*
015300010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015400010618     C                   if        vinflg = '1'
015500010618     C                   eval      flgOk = '1'
015600010618     C                   endif
015700040510     C*
015800040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015900040510     C                   z-add     *zeros        wLenVINDTA        4 0
016000040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016100090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
016200040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016300090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016400040510     C                   z-add     1             wFlgVINDTA
016500040510     C                   leave
016600040510     C                   endif
016700040510     C                   enddo
016800010618     C*
016900040510     C                   if        vindta > *blanks AND
017000040510     C                             wFlgVINDTA = 1
017100010330     C                   add       1             rrnum
017200010330     C*
017300010601     C                   if        *in70 = *off and
017400010330     C                             (vinflg = *blanks
017500010330     C                              or vinflg = '0'
017600010330     C                              or vinflg = '2')
017700010330     C*
017800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017900010711     C                   if        vinflg = *blanks or vinflg = '0'
018000010711     C                   clear                   vinmsg
018100010711     C                   endif
018200010601     C*
018300081014     C                   exsr      inzvar
018400090324     C*
018500090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
018600090324     C* separatori campi
018700090324     C                   if        CharTXT <> *blanks
018800090324     C                   z-add     *zeros        wLenVINDTA        4 0
018900090324     C                   z-add     *zeros        wFlgVINDTA        1 0
019000090324     C                   setoff                                       80
019100090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
019200090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
019300090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019400090324     C                   if        not *in80
019500090324     C                   seton                                        80
019600090324     C                   else
019700090324     C                   setoff                                       80
019800090324     C                   endif
019900090324     C                   endif
020000090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
020100090324     C                             and *in80 = *on
020200090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020300090324     C                   endif
020400090324     C                   enddo
020500090324     C                   endif
020600090324     C*
020700010605     C                   exsr      impvab
020800010601     C*
020900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021000010605     C                   eval      depfil = VABLNP
021100010601     C                   exsr      repfil
021200010601     C                   if        depfil = invfil
021300021025     C                   if        vlrpoi = 999
021400030822     C                   move(P)   invfil        VABFGS
021500070704     C                   move(P)   invfil        winvcmz           3
021600021025     C                   else
021700030822     C                   move(P)   vlrpoi        VABFGS
021800070704     C                   move(P)   vlrpoi        winvcmz
021900021025     C                   endif
022000010601     C*
022100010601     C                   exsr      PREELA
022200010601     C*
022300010604     C* Ebbene...
022400010604     C*
022500040119     C  N31              add       1             §CTROKVB
022600040119     C   32              add       1             §CTRMO
022700040119     C   31              add       1             §CTRNO
022800030822     C                   if        wGiro = 2
022900040510     C* Scrivo sempre il VAT
023000040510     C  N31              exsr      wrivat
023100040510     C* Scrivo il VAB solo a rottura d spedizione
023200040119     C                   if        depspe =  *zeros OR
023300040119     C                             depspe <> vabNSP
023400040119     C  N31              exsr      wrivab
023500040119     C  N31              eval      depspe = vabNSP
023600040119     C                   endif
023700040119     C                   endif
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
025700040119     C                   endif                                                  (endif - chkcall)
025800010601     C*
025900010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026000010601     C                   if        cntNonEl = *zeros or
026100010601     C                             flgMulti = '0'
026200010330     C* Se non ci sono record con errori ...
026300010601     C                   if        §ctrno = 0 and
026400010604     C                             §ctrmo = 0 and
026500010601     C                             flgStato <> '2'
026600010330     C* ... restituisco esito OK.
026700010330     C                   eval      wrkesito = '0'
026800010330     C                   else
026900040119     C                   if        §ctrokvb > 0
027000010330     C                   eval      wrkesito = '1'
027100010330     C                   else
027200010615     C                   if        flgOk = '0'
027300010615     C                   eval      wrkesito = '2'
027400010615     C                   else
027500010615     C                   eval      wrkesito = '6'
027600010615     C                   endif
027700010330     C                   endif
027800010330     C                   endif
027900010601     C                   else
028000010601     C                   eval      wrkesito = '9'
028100010601     C                   endif
028200010330     C*
028300010330     C                   if        %open(tivin00r)
028400010330     C                   close     tivin00r
028500010330     C                   endif
028600021025     C                   if        %open(fivabwwr)
028700021025     C                   close     fivabwwr
028800010330     C                   endif
028900040512     C                   if        %open(fivatwwr)
029000040512     C                   close     fivatwwr
029100040512     C                   endif
029200010601     C*
029300010601     C                   if        vlrpoi <> 999
029400010601     C                   eval      invfil = vlrpoi
029500010601     C                   endif
029600010330     C*
029700040119     C                   if        §ctrokvb > 0
029800010601     C                             and invfil > *zeros
029900010330     C                   exsr      invio
030000010330     C                   endif
030100010601     C*
030200010618     C                   if        flgGiro = '1'
030300010601     C                   exsr      endela
030400010618     C                   endif
030500010330     C*
030600010330     C                   ENDSR
030700010330     C***
030800040119
030900040119
031000040119
031100040119     C*----------------------------------------------------*
031200040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031300040119     C*----------------------------------------------------*
031400040510     C     WRIVAB        BEGSR
031500040119     C*
031600060331     C* Gestisco l'eventuale rottura x numero spedizione
031700060331     C                   if        VABNSP <> wVABNSP
031800100928     C                   if        wVATNOT_E <> *blanks AND
031900100928     C                             VABCTM = *blanks
032000070301     C                   eval      VABCTM = '7Q'
032100070301     C                   endif
032200040119     C                   write     fivab000                                     => scarico il VAB
032300060331     C                   eval      wVABNSP = VABNSP
032400060331     C                   endif
032500040119     C*
032600040119     C                   ENDSR
032700040119
032800040119
032900040119
033000040119     C*----------------------------------------------------*
033100040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033200040119     C*----------------------------------------------------*
033300040510     C     WRIVAT        BEGSR
033400060512     C*
033500060512     C* Inizializzo i campi chiave
033600060512     C                   eval      VATFGS = VABFGS
033700060512     C                   eval      VATAAS = VABAAS
033800060512     C                   eval      VATCCM = VABCCM
033900060512     C                   eval      VATNSP = VABNSP
034000060512     C                   eval      VATLNP = VABLNP
034100040714     C*
034200040714     C* Solo se VATNOT valorizzato scarico il buffer
034300040714     C                   if        VATNOT <> *blanks
034400040119     C*
034500040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
034600040510     C                   if        VATTRC = *blanks
034700040510     C                   eval      VATTRC = 'E'
034800040510     C                   endif
034900040510     C                   write     FIVAT000
035000040119     C                   add       1             §CTROKVT
035100040714     C*
035200040714     C                   endif
035300060331     C*
035400060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035500060331     C                   if        wVATNOT_A <> *blanks
035600060331     C                   eval      VATNOT = wVATNOT_A
035700060331     C                   eval      VATTRC = 'A'
035800060331     C                   write     FIVAT000
035900060331     C                   add       1             §CTROKVT
036000060331     C                   endif
036100060331     C*
036200060331     C                   if        wVATNOT_B <> *blanks
036300060331     C                   eval      VATNOT = wVATNOT_B
036400060331     C                   eval      VATTRC = 'B'
036500060331     C                   write     FIVAT000
036600060331     C                   add       1             §CTROKVT
036700060331     C                   endif
036800060331     C*
036900060331     C                   if        wVATNOT_E <> *blanks
037000060331     C                   eval      VATNOT = wVATNOT_E
037100060331     C                   eval      VATTRC = 'E'
037200060331     C                   write     FIVAT000
037300060331     C                   add       1             §CTROKVT
037400060331     C                   endif
037500040119     C*
037600040119     C                   ENDSR
037700040119
037800040119
037900040119
038000040119     C*----------------------------------------------------*
038100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038200040119     C*----------------------------------------------------*
038300040119     C     PREVAT        BEGSR
038400040119     C*
038500040119     C* Compongo il nome del membro da dare al FIVATWWR
038600040119     C                   eval      parmbr = vlrhdl
038700040119     C                   movel     'M'           parmbr
038800040510     C                   eval      parccm = vlrksc
038900040119     C                   eval      paropz = '1'
039000040119     C* Effettuo la chiamata al CLLE preposto
039100040119     C                   call(e)   'TITVVTC'
039200040119     C                   parm                    parccm
039300040119     C                   parm                    parmbr
039400040119     C                   parm                    paropz
039500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039600040119     C                   if        %error
039700040119     C                   movel     '1'           chkcall
039800040119     C                   else
039900040119     C                   movel     '0'           chkcall
040000040119     C                   endif
040100040119     C*
040200040119     C                   ENDSR
040300010601
040400010601
040500010601
040600010330     C*----------------------------------------------------*
040700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040800010330     C*----------------------------------------------------*
040900040119     C     INZVAR        BEGSR
041000010330     C*
041100030822     C* Inizializzo variabili di wrk
041200020204     C                   Z-ADD     *zeros        Num5_0            5 0
041300090403     C                   z-add     *zeros        invfil
041400060331     C                   MOVEL     *blanks       wVATNOT_A
041500060331     C                   MOVEL     *blanks       wVATNOT_B
041600060331     C                   MOVEL     *blanks       wVATNOT_E
041700081014     C*
041800081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
041900081014     C                   CLEAR                   FIVAB000
042000081014     C                   CLEAR                   FIVAT000
042100030822     C*
042200081014     C* Inizializzo la schiera d wrk x i dati
042300030822     C                   CLEAR                   SkSplitCSV
042400030822     C                   EVAL      i = 1
042500030822     C                   EVAL      posDa = *zeros
042600030822     C                   EVAL      posA  = *zeros
042700030822     C*
042800030822     C* Reimposto i valori di default
042900030822     C                   EXSR      DEFCAM
043000030822     C*
043100010330     C                   ENDSR
043200010330     C*----------------------------------------------------*
043300030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043400010330     C*----------------------------------------------------*
043500010330     C     DEFCAM        BEGSR
043600030822     C*
043700030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043800030715     C* e delimitatore testo.
043900030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
044000030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044100030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044200090324     C*
044300090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
044400090324     C                   EVAL      CharSOS = CharNUM
044500030822     C*
044600081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
044700030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044800081007     C                   SETON                                        40
044900030715     C                   ELSE
045000081007     C                   SETOFF                                       40
045100030715     C                   ENDIF
045200081007     C*
045300081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
045400081007     C                   SETOFF                                       50
045500030822     C*
045600030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045700030822     C                   EVAL      posDaDft = 1
045800030822     C                   EVAL      posADft  = 0
045900030822     C                   EVAL      wGiroDft = 0
046000030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
046100030822     C                             posDaDft > 0
046200030822     C*
046300030822     C* Gestisco il 1° giro
046400030822     C                   IF        wGiroDft = 0
046500030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
046600030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046700030822     C* Incremento il contatore dei "giri"
046800030822     C                   EVAL      wGiroDft = 1
046900030822     C                   ELSE
047000030822     C                   EVAL      posDaDft = posADft
047100030822     C                   ENDIF
047200030822     C* Eseguo lo scan x trovare la fine del campo corrente
047300030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
047400030822     C*
047500030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
047600030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047700030822     C                   IF        posDaDft > 0 AND
047800030822     C                             posADft  > 0
047900030822     C* NCL
048000030822     C                   IF        %subst(
048100030822     C                             %subst(vlrppt:posDaDft+1:
048200030822     C                             posADft-posDaDft-1):1:3)
048300030822     C                             = 'NCL'
048400030822     C                   EVAL      PiStr=%trim(%subst(
048500030822     C                             %subst(vlrppt:posDaDft+1:
048600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048700030822     C                   EXSR      CHKNUM
048800030822     C                   IF        PiInt=*on
048900030822     C                   Z-ADD     PiVal         VABNCL
049000030822     C                   ENDIF
049100030822     C                   ENDIF
049200030822     C* CCM
049300030822     C                   IF        %subst(
049400030822     C                             %subst(vlrppt:posDaDft+1:
049500030822     C                             posADft-posDaDft-1):1:3)
049600030822     C                             = 'CCM'
049700030822     C                   EVAL      PiStr=%trim(%subst(
049800030822     C                             %subst(vlrppt:posDaDft+1:
049900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050000030822     C                   EXSR      CHKNUM
050100030822     C                   IF        PiInt=*on
050200030822     C                   Z-ADD     PiVal         VABCCM
050300030822     C                   ENDIF
050400030822     C                   ENDIF
050500030822     C* LNP
050600030822     C                   IF        %subst(
050700030822     C                             %subst(vlrppt:posDaDft+1:
050800030822     C                             posADft-posDaDft-1):1:3)
050900030822     C                             = 'LNP'
051000030822     C                   EVAL      PiStr=%trim(%subst(
051100030822     C                             %subst(vlrppt:posDaDft+1:
051200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051300030822     C                   EXSR      CHKNUM
051400030822     C                   IF        PiInt=*on
051500030822     C                   Z-ADD     PiVal         VABLNP
051600030822     C                   ENDIF
051700030822     C                   ENDIF
051800030822     C* NRS
051900030822     C                   IF        %subst(
052000030822     C                             %subst(vlrppt:posDaDft+1:
052100030822     C                             posADft-posDaDft-1):1:3)
052200030822     C                             = 'NRS'
052300030822     C                   EVAL      PiStr=%trim(%subst(
052400030822     C                             %subst(vlrppt:posDaDft+1:
052500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052600030822     C                   EXSR      CHKNUM
052700030822     C                   IF        PiInt=*on
052800030822     C                   Z-ADD     PiVal         VABNRS
052900030822     C                   ENDIF
053000030822     C                   ENDIF
053100030822     C* CTR
053200030822     C                   IF        %subst(
053300030822     C                             %subst(vlrppt:posDaDft+1:
053400030822     C                             posADft-posDaDft-1):1:3)
053500030822     C                             = 'CTR'
053600030822     C                   EVAL      PiStr=%trim(%subst(
053700030822     C                             %subst(vlrppt:posDaDft+1:
053800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053900030822     C                   EXSR      CHKNUM
054000030822     C                   IF        PiInt=*on
054100030822     C                   Z-ADD     PiVal         VABCTR
054200030822     C                   ENDIF
054300030822     C                   ENDIF
054400030822     C* PKB
054500030822     C                   IF        %subst(
054600030822     C                             %subst(vlrppt:posDaDft+1:
054700030822     C                             posADft-posDaDft-1):1:3)
054800030822     C                             = 'PKB'
054900030822     C                   EVAL      PiStr=%trim(%subst(
055000030822     C                             %subst(vlrppt:posDaDft+1:
055100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055200030822     C                   EXSR      CHKNUM
055300030822     C                   IF        PiNum=*on
055400030822     C                   Z-ADD     PiVal         VABPKB
055500030822     C                   ENDIF
055600030822     C                   ENDIF
055700030822     C* VLB
055800030822     C                   IF        %subst(
055900030822     C                             %subst(vlrppt:posDaDft+1:
056000030822     C                             posADft-posDaDft-1):1:3)
056100030822     C                             = 'VLB'
056200030822     C                   EVAL      PiStr=%trim(%subst(
056300030822     C                             %subst(vlrppt:posDaDft+1:
056400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056500030822     C                   EXSR      CHKNUM
056600030822     C                   IF        PiNum=*on
056700030822     C                   Z-ADD     PiVal         VABVLB
056800030822     C                   ENDIF
056900030822     C                   ENDIF
057000030822     C* QFT
057100030822     C                   IF        %subst(
057200030822     C                             %subst(vlrppt:posDaDft+1:
057300030822     C                             posADft-posDaDft-1):1:3)
057400030822     C                             = 'QFT'
057500030822     C                   EVAL      PiStr=%trim(%subst(
057600030822     C                             %subst(vlrppt:posDaDft+1:
057700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057800030822     C                   EXSR      CHKNUM
057900030822     C                   IF        PiNum=*on
058000030822     C                   Z-ADD     PiVal         VABQFT
058100030822     C                   ENDIF
058200030822     C                   ENDIF
058300030822     C* CBO
058400030822     C                   IF        %subst(
058500030822     C                             %subst(vlrppt:posDaDft+1:
058600030822     C                             posADft-posDaDft-1):1:3)
058700030822     C                             = 'CBO'
058800030822     C                   EVAL      VABCBO=%trim(%subst(
058900030822     C                             %subst(vlrppt:posDaDft+1:
059000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059100030822     C                   ENDIF
059200030822     C* TSP
059300030822     C                   IF        %subst(
059400030822     C                             %subst(vlrppt:posDaDft+1:
059500030822     C                             posADft-posDaDft-1):1:3)
059600030822     C                             = 'TSP'
059700030822     C                   EVAL      VABTSP=%trim(%subst(
059800030822     C                             %subst(vlrppt:posDaDft+1:
059900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060000030822     C                   ENDIF
060100030822     C* VAS
060200030822     C                   IF        %subst(
060300030822     C                             %subst(vlrppt:posDaDft+1:
060400030822     C                             posADft-posDaDft-1):1:3)
060500030822     C                             = 'VAS'
060600030822     C                   EVAL      VABVAS=%trim(%subst(
060700030822     C                             %subst(vlrppt:posDaDft+1:
060800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060900030822     C                   ENDIF
061000030822     C* VCA
061100030822     C                   IF        %subst(
061200030822     C                             %subst(vlrppt:posDaDft+1:
061300030822     C                             posADft-posDaDft-1):1:3)
061400030822     C                             = 'VCA'
061500030822     C                   EVAL      VABVCA=%trim(%subst(
061600030822     C                             %subst(vlrppt:posDaDft+1:
061700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061800030822     C                   ENDIF
061900030822     C* GCA
062000030822     C                   IF        %subst(
062100030822     C                             %subst(vlrppt:posDaDft+1:
062200030822     C                             posADft-posDaDft-1):1:3)
062300030822     C                             = 'GCA'
062400030822     C                   EVAL      VABGCA=%trim(%subst(
062500030822     C                             %subst(vlrppt:posDaDft+1:
062600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062700030822     C                   ENDIF
062800030822     C* CTM
062900030822     C                   IF        %subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):1:3)
063200030822     C                             = 'CTM'
063300030822     C                   EVAL      VABCTM=%trim(%subst(
063400030822     C                             %subst(vlrppt:posDaDft+1:
063500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063600030822     C                   ENDIF
063700030822     C* FFD
063800030822     C                   IF        %subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):1:3)
064100030822     C                             = 'FFD'
064200030822     C                   EVAL      VABFFD=%trim(%subst(
064300030822     C                             %subst(vlrppt:posDaDft+1:
064400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064500030822     C                   ENDIF
064600030822     C* VAD
064700030822     C                   IF        %subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):1:3)
065000030822     C                             = 'VAD'
065100030822     C                   EVAL      VABVAD=%trim(%subst(
065200030822     C                             %subst(vlrppt:posDaDft+1:
065300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065400030822     C                   ENDIF
065500030822     C* GMA
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'GMA'
066000030822     C                   EVAL      VABGMA=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* GGA
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'GGA'
066900030822     C                   EVAL      VABGGA=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* GVA
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'GVA'
067800030822     C                   EVAL      VABGVA=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* TC1
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'TC1'
068700030822     C                   EVAL      VABTC1=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* TC2
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'TC2'
069600030822     C                   EVAL      VABTC2=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000080516     C* TIC
070100080516     C                   IF        %subst(
070200080516     C                             %subst(vlrppt:posDaDft+1:
070300080516     C                             posADft-posDaDft-1):1:3)
070400080516     C                             = 'TIC'
070500080516     C                   EVAL      VABTIC=%trim(%subst(
070600080516     C                             %subst(vlrppt:posDaDft+1:
070700080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800080516     C                   ENDIF
070900040714     C* VATTRC
071000040714     C                   IF        %subst(
071100040714     C                             %subst(vlrppt:posDaDft+1:
071200040714     C                             posADft-posDaDft-1):1:3)
071300040714     C                             = 'TRC'
071400040714     C                   EVAL      VATTRC=%trim(%subst(
071500040714     C                             %subst(vlrppt:posDaDft+1:
071600040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700040714     C                   ENDIF
071800030822     C* ...
071900030822     C                   ENDIF
072000030822     C                   ENDDO
072100020204     C*
072200010330     C                   ENDSR
072300010607     C*----------------------------------------------------*
072400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072500010607     C*----------------------------------------------------*
072600010607     C     IMPVAB        BEGSR
072700010607     C*
072800010607     C                   SETOFF                                       3132
072900030515     C*
073000010607     C                   MOVEL     datcor        VABAAS
073100010607     C                   MOVE      datcor        VABMGS
073200010607     C*
073300030715     C**********
073400030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073500030715     C**********
073600030715     C                   IF        wGiro = *zeros
073700040113     C*
073800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
073900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
074000040510     C                             %trim(intcol(3))<>*blanks
074100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074200040113     C                                      %trim(intcol(3))
074300040510     C                   ENDIF
074400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074500031201     C     minu:maiu     XLATE     vindta        vindta
074600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074700030715     C                   DOW       posDa <= %len(%trim(vindta))
074800030715     C*
074900030715     C* Gestisco il 1° campo
075000030715     C                   IF        i = 1
075100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075200030715     C                   EVAL      posDa = 1
075300030715     C* Eseguo lo scan x trovare la fine del primo campo
075400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075500030715     C* A questo punto "estraggo" il campo corrente
075600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075700030715     C                                                    (posA-posDa))
075800030715     C* X i campi successivi al 1°
075900030715     C                   ELSE
076000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076100030715     C                   EVAL      posDa = posA + 1
076200030715     C* Eseguo lo scan x trovare la fine del campo corrente
076300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076400030715     C* Gestisco l'ultimo campo
076500030715     C                   IF        posA = *zeros
076600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076700030715     C* A questo punto "estraggo" il campo corrente
076800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
076900030715     C                                                    (posA-posDa))
077000030715     C                   LEAVE
077100030715     C                   ELSE
077200030715     C* A questo punto "estraggo" il campo corrente
077300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077400030715     C                                                    (posA-posDa))
077500030715     C                   ENDIF
077600030715     C                   ENDIF
077700030715     C* Incremento il contatore d campo
077800030715     C                   EVAL      i = i +1
077900030715     C                   ENDDO
078000030715     C*
078100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078200030715     C* delimitazione testo
078300030715     C                   EVAL      i = 1
078400030715     C                   DOW       i < %elem(SkSplitFLD)
078500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078700030715     C                   EVAL      i = i + 1
078800030715     C                   ENDDO
078900030715     C                   EVAL      wGiro = 1
079000030715     C                   ELSE
079100030715     C                   EVAL      wGiro = 2
079200031204     C**********
079300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079400031204     C* d separatore campo
079500031204     C**********
079600031204     C                   EVAL      vindta = %trim(vindta)
079700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079800031204     C                   EVAL      vindta = %subst(vindta:2)
079900031204     C                   ENDDO
080000031204     C*
080100031204     C                   Z-ADD     *zeros        lunghInput        4 0
080200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080600031204     C                   ENDDO
080700030715     C**********
080800030715     C* Effettuo lo split del campo dati d input
080900030715     C**********
081000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081100030715     C                   DOW       posDa <= %len(%trim(vindta))
081200030715     C*
081300030715     C* Gestisco il 1° campo
081400030715     C                   IF        i = 1
081500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081600030715     C                   EVAL      posDa = 1
081700030715     C* Eseguo lo scan x trovare la fine del primo campo
081800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081900030715     C* A questo punto "estraggo" il campo corrente
082000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082100030715     C                                                    (posA-posDa))
082200030715     C* X i campi successivi al 1°
082300030715     C                   ELSE
082400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082500030715     C                   EVAL      posDa = posA + 1
082600031204     C* Verifico che nn vi sia il campo nn valorizzato
082700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082800031204     C* Se campo nn valorizzato skippo al prossimo
082900031204     C                   EVAL      SkSplitCSV(i) = *blanks
083000031204     C                   EVAL      posA  = posA + 1
083100031204     C                   ELSE
083200030715     C* Eseguo lo scan x trovare la fine del campo corrente
083300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083400030715     C* Gestisco l'ultimo campo
083500030715     C                   IF        posA = *zeros
083600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083700030715     C* A questo punto "estraggo" il campo corrente
083800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
083900030715     C                                                    (posA-posDa))
084000030715     C                   LEAVE
084100030715     C                   ELSE
084200030715     C* A questo punto "estraggo" il campo corrente
084300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084400030715     C                                                    (posA-posDa))
084500030715     C                   ENDIF
084600031204     C                   ENDIF
084700030715     C                   ENDIF
084800030715     C* Incremento il contatore d campo
084900030715     C                   EVAL      i = i +1
085000030715     C                   ENDDO
085100030715     C*
085200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085300030715     C* delimitazione testo
085400030715     C                   EVAL      i = 1
085500030715     C                   DOW       i < %elem(SkSplitCSV)
085600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085800030715     C                   EVAL      i = i + 1
085900030715     C                   ENDDO
086000030715     C*
086100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086200030715     C                   EVAL      i = 1
086300030715     C                   DOW       i < %elem(SkSplitFLD)
086400030715     C*
086500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086700030715     C                             SkSplitCSV(i) <> *blanks
086800030721     C*** ATB
086900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087000030721     C                   EVAL      VABATB = SkSplitCSV(i)
087100030721     C                   ENDIF
087200030715     C*** RSD
087300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087400030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087500030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087600030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087700030715     C* ==
087800030715     C                   ENDIF
087900030721     C*** RD2
088000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088100030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088200030721     C                   ENDIF
088300030715     C*** IND
088400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088500030715     C                   EVAL      VABIND = SkSplitCSV(i)
088600030715     C                   ENDIF
088700030715     C*** LOD
088800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
088900030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089000030715     C                   ENDIF
089100030715     C*** PRD
089200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089300030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089400030715     C                   ENDIF
089500030721     C*** NZD
089600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089700030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089800030721     C                   ENDIF
089900030715     C*** RMA
090000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090100030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090200030715     C                   ENDIF
090300030715     C*** NOT
090400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090500030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090600030715     C                   ENDIF
090700030715     C*** NT2
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
090900030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** VCA
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091300030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** VAS
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091700030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030715     C*** FFD
092000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092100030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092200030715     C                   ENDIF
092300030721     C*** GC1
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092500030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092600100304     C                   IF        %subst(VABGC1:1) = 'P' or
092700100304     C                             %subst(VABGC1:1) = 'M'
092800100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
092900100304     C                   ENDIF
093000030721     C                   ENDIF
093100030721     C*** GC2
093200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093300030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093400100304     C                   IF        %subst(VABGC2:1) = 'P' or
093500100304     C                             %subst(VABGC2:1) = 'M'
093600100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
093700100304     C                   ENDIF
093800030721     C                   ENDIF
093900030721     C*** TSP
094000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094100030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094200030721     C                   ENDIF
094300040510     C*** CBO
094400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
094500040510     C                   EVAL      VABCBO = SkSplitCSV(i)
094600040510     C                   ENDIF
094700030721     C*** NAS
094800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
094900030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095000030721     C                   ENDIF
095100030721     C*** TIC
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095300030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030721     C*** GCA
095600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095700030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095800030721     C                   ENDIF
095900030721     C*** XCO
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096100030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096200030721     C                   ENDIF
096300030721     C*** CTM
096400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096500030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096600030721     C                   ENDIF
096700030721     C*** TCR
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
096900030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030721     C*** CTS
097200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097300030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097400030721     C                   ENDIF
097500030721     C*** FTM
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097700030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** VAD
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098100030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GMA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098500030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** GGA
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
098900030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** GVA
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099300030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099700030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** TC2
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100100030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** SCL
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100500030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** RMO
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
100900030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** NMO
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101300030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*
101600030721     C* Reperisco quindi i campi numerici...
101700040510     C*** NSP
101800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
101900040510     C                   EVAL      PiStr=SkSplitCSV(i)
102000040510     C                   EXSR      CHKNUM
102100040510     C                   IF        PiInt=*on
102200040510     C                   Z-ADD     PiVal         VABNSP
102300040510     C                   ELSE
102400040510     C                   SETON                                        31
102500040510     C                   EVAL      VABNSP = *zeros
102600040510     C                   EVAL      vinmsg = %trimr(vinmsg)
102700040510     C                             + ' ' + 'VABNSP'
102800040510     C                   ENDIF
102900040510     C                   ENDIF
103000070703     C*** RMN
103100070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
103200070703     C                   EVAL      PiStr=SkSplitCSV(i)
103300070703     C                   EXSR      CHKNUM
103400070703     C                   IF        PiInt=*on
103500070703     C                   Z-ADD     PiVal         VABRMN
103600070703     C                   ELSE
103700070703     C                   SETON                                        32
103800070703     C                   EVAL      vinmsg = %trimr(vinmsg)
103900070703     C                             + ' ' + 'VABRMN'
104000070703     C                   ENDIF
104100070703     C                   ENDIF
104200070705     C*** CAD
104300070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104400070705     C                   EVAL      VABCAD = SkSplitCSV(i)
104500070705     C                   ENDIF
104600030721     C*** NCL
104700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104800030721     C                   EVAL      PiStr=SkSplitCSV(i)
104900030721     C                   EXSR      CHKNUM
105000030721     C                   IF        PiInt=*on
105100081007     C  N40              Z-ADD     PiVal         VABNCL
105200081007     C   40              ADD       PiVal         VABNCL
105300030721     C                   ELSE
105400030721     C                   SETON                                        32
105500030721     C                   EVAL      VABNCL = *zeros
105600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105700040122     C                             + ' ' + 'VABNCL'
105800030721     C                   ENDIF
105900030721     C                   ENDIF
106000030721     C*** PKB
106100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106200030721     C                   EVAL      PiStr=SkSplitCSV(i)
106300030721     C                   EXSR      CHKNUM
106400030721     C                   IF        PiNum=*on
106500081007     C  N40              Z-ADD     PiVal         VABPKB
106600081007     C   40              ADD       PiVal         VABPKB
106700030721     C                   ELSE
106800030721     C                   SETON                                        32
106900030721     C                   EVAL      VABPKB = *zeros
107000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107100030721     C                             + ' ' + 'VABPKB'
107200030721     C                   ENDIF
107300030721     C                   ENDIF
107400030721     C*** CAS
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107600030721     C                   EVAL      PiStr=SkSplitCSV(i)
107700030721     C                   EXSR      CHKNUM
107800030721     C                   IF        PiNum=*on
107900030721     C                   Z-ADD     PiVal         VABCAS
108000030721     C                   ELSE
108100030721     C                   SETON                                        32
108200030721     C                   EVAL      VABCAS = *zeros
108300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108400030721     C                             + ' ' + 'VABCAS'
108500030721     C                   ENDIF
108600030721     C                   ENDIF
108700030721     C*** CCM
108800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
108900030721     C                   EVAL      PiStr=SkSplitCSV(i)
109000030721     C                   EXSR      CHKNUM
109100030721     C                   IF        PiInt=*on      AND
109200030721     C                             PiVal<=9999999 AND
109300030721     C                             PiVal>*zeros
109400030721     C                   Z-ADD     PiVal         VABCCM
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      VABCCM = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040510     C                             + ' ' + 'VABCCM'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** LNP
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiInt=*on    AND
110700030721     C                             PiVal<=999   AND
110800030721     C                             PiVal>*zeros
110900030721     C                   Z-ADD     PiVal         VABLNP
111000030721     C                   ELSE
111100040715     C                   SETON                                        32
111200030721     C                   EVAL      VABLNP = *zeros
111300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111400040510     C                             + ' ' + 'VABLNP'
111500030721     C                   ENDIF
111600030721     C                   ENDIF
111700030721     C*** NRS
111800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
111900030721     C                   EVAL      PiStr=SkSplitCSV(i)
112000030721     C                   EXSR      CHKNUM
112100030721     C                   IF        PiInt=*on AND
112200030721     C                             PiVal<=99
112300030721     C                   Z-ADD     PiVal         VABNRS
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700030721     C                             + ' ' + 'VABNRS'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** LNA
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on    AND
113500030721     C                             PiVal<=999   AND
113600030721     C                             PiVal>*zeros
113700030721     C                   Z-ADD     PiVal         VABLNA
113800030721     C                   ELSE
113900030721     C                   SETON                                        32
114000030721     C                   EVAL      VABLNA = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200030721     C                             + ' ' + 'VABLNA'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** CTR
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on  AND
115000030721     C                             PiVal<=999
115100030721     C                   Z-ADD     PiVal         VABCTR
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABCTR'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** IAS
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiNum=*on
116300030721     C                   Z-ADD     PiVal         VABIAS
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABIAS'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** VLB
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiNum=*on
117500081007     C  N40              Z-ADD     PiVal         VABVLB
117600081007     C   40              ADD       PiVal         VABVLB
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABVLB'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** QFT
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800081007     C  N40              Z-ADD     PiVal         VABQFT
118900081007     C   40              ADD       PiVal         VABQFT
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABQFT'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** NCD
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiInt=*on      AND
120100030721     C                             PiVal<=9999999
120200030721     C                   Z-ADD     PiVal         VABNCD
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABNCD'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** NCA
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiInt=*on      AND
121400030721     C                             PiVal<=9999999
121500030721     C                   Z-ADD     PiVal         VABNCA
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABNCA'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** ZNC
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on AND
122700030721     C                             PiVal<=99
122800030721     C                   Z-ADD     PiVal         VABZNC
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABZNC'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** DCR
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on       AND
124000030721     C                             PiVal<=99999999
124100030721     C                   Z-ADD     PiVal         VABDCR
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABDCR'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** HCR
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on   AND
125300030721     C                             PiVal<=9999
125400030721     C                   Z-ADD     PiVal         VABHCR
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABHCR'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** VMD
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiNum=*on
126600030721     C                   Z-ADD     PiVal         VABVMD
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABVMD'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** ANT
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on        AND
127800030721     C                             PiVal<=999999999
127900030721     C                   Z-ADD     PiVal         VABANT
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABANT'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** CMO
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on
129100030721     C                   Z-ADD     PiVal         Num5_0
129200030721     C                   MOVEL(p)  Num5_0        VABCMO
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      VABCMO = *zeros
129600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129700030721     C                             + ' ' + 'VABCMO'
129800030721     C                   ENDIF
129900030721     C                   ENDIF
130000040510     C*
130100040510     C* Reperisco i campi del FIVAT
130200040510     C*** VATNOT
130300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130400040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
130500040510     C                   ENDIF
130600040510     C*** VATTRC
130700040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
130800040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
130900040510     C                   ENDIF
131000060331     C*** VATNOT_A
131100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
131200060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
131300060331     C                   ENDIF
131400060331     C*** VATNOT_B
131500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
131600060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
131700060331     C                   ENDIF
131800060331     C*** VATNOT_E
131900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
132000060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
132100060331     C                   ENDIF
132200040510     C*
132300100928     C* ==> Campi "personalizzati"
132400100928     C*** "DPD"
132500100928     C                   IF        %trim(SkSplitFLD(i)) = 'DPD'
132600100928     C                   IF        %trim(SkSplitCSV(i)) = 'D'
132700100928     C                   EVAL      VABCTM = '7R'
132800100928     C                   ENDIF
132900100928     C                   ENDIF
133000100928     C* <==
133100030715     C                   ENDIF
133200030715     C                   EVAL      i = i + 1
133300030715     C                   ENDDO
133400040510     C*
133500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
133600040510     C                   IF        VABNSP = *zeros
133700040510     C                   SETON                                        31
133800040510     C                   ENDIF
133900040715     C*
134000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
134100040715     C                   IF        VABLNP = *zeros
134200040715     C                   SETON                                        31
134300040715     C                   ENDIF
134400040510     C*
134500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
134600040510     C                   IF        VABRMN = *zeros
134700040510     C                   Z-ADD     VABNSP        VABRMN
134800040510     C                   ENDIF
134900040510     C*
135000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
135100040510     C                   IF        VABRMA = *blanks
135200040714     C                   MOVEL     VABRMN        VABRMA
135300040510     C                   ENDIF
135400070705     C*
135500070705     C* Considerazioni particolari
135600070705     C*** NZD
135700070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
135800070705     C                   EVAL      VABNZD = *blanks
135900070705     C                   ENDIF
136000070705     C*** CAD
136100070705     C                   IF        VABNZD = *blanks
136200070705     C                   EVAL      PiStr=VABCAD
136300070705     C                   EXSR      CHKNUM
136400070705     C                   IF        PiInt=*on
136500070705     C                   Z-ADD     PiVal         Num5_0
136600070705     C                   MOVEL(p)  Num5_0        VABCAD
136700070705     C                   ELSE
136800070705     C                   SETON                                        32
136900070705     C                   EVAL      VABCAD = *zeros
137000070705     C                   EVAL      vinmsg = %trimr(vinmsg)
137100070705     C                             + ' ' + 'VABCAD'
137200070705     C                   ENDIF
137300070705     C                   ENDIF
137400040128     C*
137500040510     C* Se provincia nn valorizzata la reperisco
137600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
137700040510     C                   IF        VABPRD = *blanks
137800040128     C                   CLEAR                   TISI95DS
137900040128     C                   EVAL      I95TCN = '3'
138000040128     C                   Z-ADD     datcor        I95DAT
138100040128     C                   EVAL      I95NAR = VABNZD
138200040128     C                   EVAL      I95CAP = VABCAD
138300040128     C                   EVAL      I95LOC = VABLOD
138400040128     C                   CALL      'TISI95R'
138500040128     C                   PARM                    TISI95DS
138600040128     C                   EVAL      VABPRD = O95PRV
138700040510     C                   ENDIF
138800040113     C*
138900040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
139000040113     C                   IF        VABCAS > *zeros
139100040119     C                   EVAL      VABVCA = 'EUR'
139200040119     C                   IF        VABCBO = '1'
139300040113     C                   EVAL      VABCBO = '4'
139400040119     C                   ENDIF
139500040119     C                   IF        VABCBO = '2'
139600040119     C                   EVAL      VABCBO = '6'
139700040119     C                   ENDIF
139800040113     C                   ENDIF
139900020204     C*
140000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
140100020204     C                   EXSR      CHKIMPDIV
140200030715     C                   ENDIF
140300020204     C*
140400010607     C                   ENDSR
140500010607     C*----------------------------------------------------*
140600010601
140700020204
140800020204     C*----------------------------------------------------*
140900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
141000020204     C*----------------------------------------------------*
141100020204     C     CHKIMPDIV     BEGSR
141200020204     C*
141300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
141400020204     C                   Z-ADD     *zeros        wrkDec            9 9
141500020204     C*
141600020204     C* Come prima cosa effettuo considerazioni sulla divisa
141700020204     C                   IF        vabIAS > *zeros
141800020204     C                   IF        vabVAS <> 'EUR'
141900020204     C                   EVAL      vabVAS =  'ITL'
142000020204     C                   ENDIF
142100020204     C                   ENDIF
142200020204     C*
142300020204     C                   IF        vabCAS > *zeros
142400020204     C                   IF        vabVCA <> 'EUR'
142500020204     C                   EVAL      vabVCA =  'ITL'
142600020204     C                   ENDIF
142700020204     C                   ENDIF
142800020204     C*
142900020204     C                   IF        vabVMD > *zeros
143000020204     C                   IF        vabVAD <> 'EUR'
143100020204     C                   EVAL      vabVAD =  'ITL'
143200020204     C                   ENDIF
143300020204     C                   ENDIF
143400020204     C*
143500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143600020204     C                   Z-ADD     vabIAS        wrkDec
143700020204     C                   IF        wrkDec > *zeros
143800020204     C                   IF        vabVAS = 'ITL'
143900020204     C                   EVAL      vabIAS = *zeros
144000020204     C                   ENDIF
144100020204     C                   ENDIF
144200020204     C*
144300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
144400020204     C                   Z-ADD     vabCAS        wrkDec
144500020204     C                   IF        wrkDec > *zeros
144600020204     C                   IF        vabVCA = 'ITL'
144700020204     C                   EVAL      vabCAS = *zeros
144800020204     C                   ENDIF
144900020204     C                   ENDIF
145000020204     C*
145100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
145200020204     C                   Z-ADD     vabVMD        wrkDec
145300020204     C                   IF        wrkDec > *zeros
145400020204     C                   IF        vabVAD = 'ITL'
145500020204     C                   EVAL      vabVMD = *zeros
145600020204     C                   ENDIF
145700020204     C                   ENDIF
145800020204     C*
145900020204     C                   ENDSR
146000020204     C***
146100020204
146200010330
146300010330
146400010330     C*----------------------------------------------------*
146500010330     C*  CONTROLLO NUMERICITA' CAMPI
146600010330     C*----------------------------------------------------*
146700010330     C     CHKNUM        BEGSR
146800010330     C*
146900010606     C                   IF        PiDecChr = *blanks
147000030715     C                   EVAL      PiDecChr = CharNUM
147100010606     C                   ENDIF
147200010606     C*
147300010606     C                   CALL(e)   'ISNUMERIC'
147400010330     C                   PARM                    PiStr            30
147500010606     C                   PARM                    PiDecChr          1
147600010330     C                   PARM      *ZEROS        PiVal            30 9
147700010330     C                   PARM      '0'           PiInt             1
147800010330     C                   PARM      '0'           PiNum             1
147900010330     C                   IF        %error
148000010606     C                   EVAL      PiNum=*off
148100010330     C                   ENDIF
148200010330     C*
148300010330     C                   ENDSR
148400010330     C***
148500010330
148600010601
148700010601
148800010601
148900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
149000010601     C     repfil        BEGSR
149100010601     C*
149200010601     C                   if        invfil = *zeros and
149300010601     C                             depfil > *zeros and
149400010629     C                             (vinflg = *blanks or
149500010629     C                              vinflg = *zeros)
149600010601     C
149700010601     C                   eval      invfil = depfil
149800010601     C                   endif
149900010601     C*
150000010601     C                   if        depfil <> invfil and
150100010601     C                             invfil > *zeros
150200010601     C                   eval      flgMulti = '1'
150300010601     C                   if        vinflg = *blanks
150400010601     C                   add       1             cntNonEl
150500010601     C                   endif
150600010601     C                   endif
150700010601     C*
150800010601     C                   if        vinflg = '2'
150900010601     C                   eval      flgStato = '2'
151000010601     C                   endif
151100010601     C*
151200010601     C                   ENDSR
151300010601     C***
151400010601
151500010601
151600010601
151700010330
151800040119      /TITLE Invio dei dati al punto operativo.
151900040119     C     invio         BEGSR
152000040119     C*
152100040119     C* 1° invio FIVAT
152200040119     C                   reset                   dscmz
152300070704     C                   move      winvcmz       cmzdst
152400040119     C                   eval      cmzfld = 'FIVATWWR'
152500040119     C                   eval      cmzmbd = vlrhdl
152600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152700040119     C***                if        prmfir = *blanks
152800040119     C                   eval      cmzfla = 'FIVAT00F'
152900040119     C                   eval      cmzmba = 'FIVAT00F'
153000040119     C***                else
153100040119     C***                eval      cmzfla = prmfir
153200040119     C***                eval      cmzmba = prmfir
153300040119     C***                endif
153400040119     C                   eval      cmznrr = *zeros
153500040119     C                   move      §ctrokvt      cmznrr
153600040119     C                   eval      cmzlba = vlrfl1
153700040119     C                   call(e)   'TIS711C'
153800040119     C                   parm                    dscmz
153900040119     C                   parm      *blanks       esito
154000040119     C                   if        %error
154100040119     C                             or cmzerr = '1'
154200040119     C                             or esito  = '1'
154300040119     C                   eval      wrkesito = '3'
154400040119     C                   else
154500040119     C*
154600040119     C* 2° invio FIVAB
154700040119     C                   reset                   dscmz
154800070704     C                   move      winvcmz       cmzdst
154900040119     C                   eval      cmzfld = vlrfou
155000040119     C                   eval      cmzmbd = vlrhdl
155100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155200040119     C***                if        prmfir = *blanks
155300040119     C                   eval      cmzfla = 'FIVAB00F'
155400040119     C                   eval      cmzmba = 'FIVAB00F'
155500040119     C***                else
155600040119     C***                eval      cmzfla = prmfir
155700040119     C***                eval      cmzmba = prmfir
155800040119     C***                endif
155900040119     C                   eval      cmznrr = *zeros
156000040119     C                   move      §ctrokvb      cmznrr
156100040119     C                   eval      cmzlba = vlrfl1
156200040119     C                   call(e)   'TIS711C'
156300040119     C                   parm                    dscmz
156400040119     C                   parm      *blanks       esito
156500040119     C                   if        %error
156600040119     C                             or cmzerr = '1'
156700040119     C                             or esito  = '1'
156800040119     C                   eval      wrkesito = '3'
156900040119     C                   endif
157000040119     C                   endif
157100040119     C*
157200040119     C                   ENDSR
157300040119     C***
157400010601
157500010601
157600010601
157700010601
157800010601      /TITLE Invio dei dati al punto operativo.
157900010601     C     opeini        BEGSR
158000010601     C*
158100010601     C* Inizializzo flag e contatori operativi
158200010601     C                   movel     '0'           flgGiro           1
158300010601     C                   movel     '0'           flgMulti          1
158400010601     C                   movel     '1'           flgStato          1
158500010615     C                   movel     '0'           flgOk             1
158600010601     C                   z-add     *zeros        cntNonEl         10 0
158700010601     C                   z-add     *zeros        depfil            3 0
158800010601     C                   z-add     *zeros        invfil            3 0
158900010601     C*
159000010601     C                   ENDSR
159100010601     C***
159200070326
159300070326
159400070326
159500070326
159600070326     C     *pssr         BEGSR
159700070329     C*
159800070329     C                   if        %open(tivin00r)
159900070329     C                   close     tivin00r
160000070329     C                   endif
160100070329     C                   if        %open(fivabwwr)
160200070329     C                   close     fivabwwr
160300070329     C                   endif
160400070329     C                   if        %open(fivatwwr)
160500070329     C                   close     fivatwwr
160600070329     C                   endif
160700070326     C*
160800070326     C* Effettuo la chiamata al CLLE preposto
160900070326     C                   call(e)   'TITVVTC'
161000070326     C                   parm                    parccm
161100070326     C                   parm                    parmbr
161200070326     C                   parm      '2'           paropz
161300070326     C*
161400070326     C                   eval      wrkesito = '2'
161500070404     C*
161600070404     C                   seton                                        LR
161700070326     C*
161800070326     C                   ENDSR     '*CANCL'
161900070326     C***
162000070326
162100070326
162200010330
162300010330
162400000613     C     *inzsr        BEGSR
162500990910     C*
162600990910     C     *entry        plist
162700990920     C                   parm                    tivlrds
162800990921     C                   parm      wrkesito      esito
162900000724     C                   parm                    prmlit
163000000710     C                   parm                    prmfir
163100010330     C*
163200010330     C* CALCOLA LA DATA CORRENTE
163300010330     C                   time                    wn14             14 0
163400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
163500010330     C                   z-add     wn8           g08dat
163600010330     C                   z-add     *zeros        g08inv
163700010330     C                   movel     '0'           g08err
163800010330     C                   call      'XSRDA8'
163900010330     C                   parm                    wlbda8
164000010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
164100000613     C*
164200000613     C                   ENDSR
164300000613     C***
164400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
164500040510
164600040122
164700040113
