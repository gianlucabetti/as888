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
031800070301     C                   if        wVATNOT_E <> *blanks
031900110419     C                   if        VABCTM = *blanks
032000070301     C                   eval      VABCTM = '7Q'
032100110419     C                   endif
032200070301     C                   endif
032300040119     C                   write     fivab000                                     => scarico il VAB
032400060331     C                   eval      wVABNSP = VABNSP
032500060331     C                   endif
032600040119     C*
032700040119     C                   ENDSR
032800040119
032900040119
033000040119
033100040119     C*----------------------------------------------------*
033200040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033300040119     C*----------------------------------------------------*
033400040510     C     WRIVAT        BEGSR
033500060512     C*
033600060512     C* Inizializzo i campi chiave
033700060512     C                   eval      VATFGS = VABFGS
033800060512     C                   eval      VATAAS = VABAAS
033900060512     C                   eval      VATCCM = VABCCM
034000060512     C                   eval      VATNSP = VABNSP
034100060512     C                   eval      VATLNP = VABLNP
034200040714     C*
034300040714     C* Solo se VATNOT valorizzato scarico il buffer
034400040714     C                   if        VATNOT <> *blanks
034500040119     C*
034600040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
034700040510     C                   if        VATTRC = *blanks
034800040510     C                   eval      VATTRC = 'E'
034900040510     C                   endif
035000040510     C                   write     FIVAT000
035100040119     C                   add       1             §CTROKVT
035200040714     C*
035300040714     C                   endif
035400060331     C*
035500060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035600060331     C                   if        wVATNOT_A <> *blanks
035700060331     C                   eval      VATNOT = wVATNOT_A
035800060331     C                   eval      VATTRC = 'A'
035900060331     C                   write     FIVAT000
036000060331     C                   add       1             §CTROKVT
036100060331     C                   endif
036200060331     C*
036300060331     C                   if        wVATNOT_B <> *blanks
036400060331     C                   eval      VATNOT = wVATNOT_B
036500060331     C                   eval      VATTRC = 'B'
036600060331     C                   write     FIVAT000
036700060331     C                   add       1             §CTROKVT
036800060331     C                   endif
036900060331     C*
037000060331     C                   if        wVATNOT_E <> *blanks
037100060331     C                   eval      VATNOT = wVATNOT_E
037200060331     C                   eval      VATTRC = 'E'
037300060331     C                   write     FIVAT000
037400060331     C                   add       1             §CTROKVT
037500060331     C                   endif
037600040119     C*
037700040119     C                   ENDSR
037800040119
037900040119
038000040119
038100040119     C*----------------------------------------------------*
038200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
038300040119     C*----------------------------------------------------*
038400040119     C     PREVAT        BEGSR
038500040119     C*
038600040119     C* Compongo il nome del membro da dare al FIVATWWR
038700040119     C                   eval      parmbr = vlrhdl
038800040119     C                   movel     'M'           parmbr
038900040510     C                   eval      parccm = vlrksc
039000040119     C                   eval      paropz = '1'
039100040119     C* Effettuo la chiamata al CLLE preposto
039200040119     C                   call(e)   'TITVVTC'
039300040119     C                   parm                    parccm
039400040119     C                   parm                    parmbr
039500040119     C                   parm                    paropz
039600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
039700040119     C                   if        %error
039800040119     C                   movel     '1'           chkcall
039900040119     C                   else
040000040119     C                   movel     '0'           chkcall
040100040119     C                   endif
040200040119     C*
040300040119     C                   ENDSR
040400010601
040500010601
040600010601
040700010330     C*----------------------------------------------------*
040800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040900010330     C*----------------------------------------------------*
041000040119     C     INZVAR        BEGSR
041100010330     C*
041200030822     C* Inizializzo variabili di wrk
041300020204     C                   Z-ADD     *zeros        Num5_0            5 0
041400090403     C                   z-add     *zeros        invfil
041500060331     C                   MOVEL     *blanks       wVATNOT_A
041600060331     C                   MOVEL     *blanks       wVATNOT_B
041700060331     C                   MOVEL     *blanks       wVATNOT_E
041800081014     C*
041900081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
042000081014     C                   CLEAR                   FIVAB000
042100081014     C                   CLEAR                   FIVAT000
042200030822     C*
042300081014     C* Inizializzo la schiera d wrk x i dati
042400030822     C                   CLEAR                   SkSplitCSV
042500030822     C                   EVAL      i = 1
042600030822     C                   EVAL      posDa = *zeros
042700030822     C                   EVAL      posA  = *zeros
042800030822     C*
042900030822     C* Reimposto i valori di default
043000030822     C                   EXSR      DEFCAM
043100030822     C*
043200010330     C                   ENDSR
043300010330     C*----------------------------------------------------*
043400030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
043500010330     C*----------------------------------------------------*
043600010330     C     DEFCAM        BEGSR
043700030822     C*
043800030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
043900030715     C* e delimitatore testo.
044000030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
044100030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
044200030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
044300090324     C*
044400090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
044500090324     C                   EVAL      CharSOS = CharNUM
044600030822     C*
044700081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
044800030715     C                   IF        %subst(vlrppt:1:1) = 'S'
044900081007     C                   SETON                                        40
045000030715     C                   ELSE
045100081007     C                   SETOFF                                       40
045200030715     C                   ENDIF
045300081007     C*
045400081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
045500081007     C                   SETOFF                                       50
045600030822     C*
045700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
045800030822     C                   EVAL      posDaDft = 1
045900030822     C                   EVAL      posADft  = 0
046000030822     C                   EVAL      wGiroDft = 0
046100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
046200030822     C                             posDaDft > 0
046300030822     C*
046400030822     C* Gestisco il 1° giro
046500030822     C                   IF        wGiroDft = 0
046600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
046700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
046800030822     C* Incremento il contatore dei "giri"
046900030822     C                   EVAL      wGiroDft = 1
047000030822     C                   ELSE
047100030822     C                   EVAL      posDaDft = posADft
047200030822     C                   ENDIF
047300030822     C* Eseguo lo scan x trovare la fine del campo corrente
047400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
047500030822     C*
047600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
047700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
047800030822     C                   IF        posDaDft > 0 AND
047900030822     C                             posADft  > 0
048000030822     C* NCL
048100030822     C                   IF        %subst(
048200030822     C                             %subst(vlrppt:posDaDft+1:
048300030822     C                             posADft-posDaDft-1):1:3)
048400030822     C                             = 'NCL'
048500030822     C                   EVAL      PiStr=%trim(%subst(
048600030822     C                             %subst(vlrppt:posDaDft+1:
048700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
048800030822     C                   EXSR      CHKNUM
048900030822     C                   IF        PiInt=*on
049000030822     C                   Z-ADD     PiVal         VABNCL
049100030822     C                   ENDIF
049200030822     C                   ENDIF
049300030822     C* CCM
049400030822     C                   IF        %subst(
049500030822     C                             %subst(vlrppt:posDaDft+1:
049600030822     C                             posADft-posDaDft-1):1:3)
049700030822     C                             = 'CCM'
049800030822     C                   EVAL      PiStr=%trim(%subst(
049900030822     C                             %subst(vlrppt:posDaDft+1:
050000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
050100030822     C                   EXSR      CHKNUM
050200030822     C                   IF        PiInt=*on
050300030822     C                   Z-ADD     PiVal         VABCCM
050400030822     C                   ENDIF
050500030822     C                   ENDIF
050600030822     C* LNP
050700030822     C                   IF        %subst(
050800030822     C                             %subst(vlrppt:posDaDft+1:
050900030822     C                             posADft-posDaDft-1):1:3)
051000030822     C                             = 'LNP'
051100030822     C                   EVAL      PiStr=%trim(%subst(
051200030822     C                             %subst(vlrppt:posDaDft+1:
051300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051400030822     C                   EXSR      CHKNUM
051500030822     C                   IF        PiInt=*on
051600030822     C                   Z-ADD     PiVal         VABLNP
051700030822     C                   ENDIF
051800030822     C                   ENDIF
051900030822     C* NRS
052000030822     C                   IF        %subst(
052100030822     C                             %subst(vlrppt:posDaDft+1:
052200030822     C                             posADft-posDaDft-1):1:3)
052300030822     C                             = 'NRS'
052400030822     C                   EVAL      PiStr=%trim(%subst(
052500030822     C                             %subst(vlrppt:posDaDft+1:
052600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052700030822     C                   EXSR      CHKNUM
052800030822     C                   IF        PiInt=*on
052900030822     C                   Z-ADD     PiVal         VABNRS
053000030822     C                   ENDIF
053100030822     C                   ENDIF
053200030822     C* CTR
053300030822     C                   IF        %subst(
053400030822     C                             %subst(vlrppt:posDaDft+1:
053500030822     C                             posADft-posDaDft-1):1:3)
053600030822     C                             = 'CTR'
053700030822     C                   EVAL      PiStr=%trim(%subst(
053800030822     C                             %subst(vlrppt:posDaDft+1:
053900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054000030822     C                   EXSR      CHKNUM
054100030822     C                   IF        PiInt=*on
054200030822     C                   Z-ADD     PiVal         VABCTR
054300030822     C                   ENDIF
054400030822     C                   ENDIF
054500030822     C* PKB
054600030822     C                   IF        %subst(
054700030822     C                             %subst(vlrppt:posDaDft+1:
054800030822     C                             posADft-posDaDft-1):1:3)
054900030822     C                             = 'PKB'
055000030822     C                   EVAL      PiStr=%trim(%subst(
055100030822     C                             %subst(vlrppt:posDaDft+1:
055200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055300030822     C                   EXSR      CHKNUM
055400030822     C                   IF        PiNum=*on
055500030822     C                   Z-ADD     PiVal         VABPKB
055600030822     C                   ENDIF
055700030822     C                   ENDIF
055800030822     C* VLB
055900030822     C                   IF        %subst(
056000030822     C                             %subst(vlrppt:posDaDft+1:
056100030822     C                             posADft-posDaDft-1):1:3)
056200030822     C                             = 'VLB'
056300030822     C                   EVAL      PiStr=%trim(%subst(
056400030822     C                             %subst(vlrppt:posDaDft+1:
056500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056600030822     C                   EXSR      CHKNUM
056700030822     C                   IF        PiNum=*on
056800030822     C                   Z-ADD     PiVal         VABVLB
056900030822     C                   ENDIF
057000030822     C                   ENDIF
057100030822     C* QFT
057200030822     C                   IF        %subst(
057300030822     C                             %subst(vlrppt:posDaDft+1:
057400030822     C                             posADft-posDaDft-1):1:3)
057500030822     C                             = 'QFT'
057600030822     C                   EVAL      PiStr=%trim(%subst(
057700030822     C                             %subst(vlrppt:posDaDft+1:
057800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057900030822     C                   EXSR      CHKNUM
058000030822     C                   IF        PiNum=*on
058100030822     C                   Z-ADD     PiVal         VABQFT
058200030822     C                   ENDIF
058300030822     C                   ENDIF
058400030822     C* CBO
058500030822     C                   IF        %subst(
058600030822     C                             %subst(vlrppt:posDaDft+1:
058700030822     C                             posADft-posDaDft-1):1:3)
058800030822     C                             = 'CBO'
058900030822     C                   EVAL      VABCBO=%trim(%subst(
059000030822     C                             %subst(vlrppt:posDaDft+1:
059100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059200030822     C                   ENDIF
059300030822     C* TSP
059400030822     C                   IF        %subst(
059500030822     C                             %subst(vlrppt:posDaDft+1:
059600030822     C                             posADft-posDaDft-1):1:3)
059700030822     C                             = 'TSP'
059800030822     C                   EVAL      VABTSP=%trim(%subst(
059900030822     C                             %subst(vlrppt:posDaDft+1:
060000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060100030822     C                   ENDIF
060200030822     C* VAS
060300030822     C                   IF        %subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):1:3)
060600030822     C                             = 'VAS'
060700030822     C                   EVAL      VABVAS=%trim(%subst(
060800030822     C                             %subst(vlrppt:posDaDft+1:
060900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061000030822     C                   ENDIF
061100030822     C* VCA
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'VCA'
061600030822     C                   EVAL      VABVCA=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   ENDIF
062000030822     C* GCA
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'GCA'
062500030822     C                   EVAL      VABGCA=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   ENDIF
062900030822     C* CTM
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CTM'
063400030822     C                   EVAL      VABCTM=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   ENDIF
063800030822     C* FFD
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'FFD'
064300030822     C                   EVAL      VABFFD=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   ENDIF
064700030822     C* VAD
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'VAD'
065200030822     C                   EVAL      VABVAD=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* GMA
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'GMA'
066100030822     C                   EVAL      VABGMA=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* GGA
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'GGA'
067000030822     C                   EVAL      VABGGA=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* GVA
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'GVA'
067900030822     C                   EVAL      VABGVA=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* TC1
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'TC1'
068800030822     C                   EVAL      VABTC1=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* TC2
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'TC2'
069700030822     C                   EVAL      VABTC2=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100080516     C* TIC
070200080516     C                   IF        %subst(
070300080516     C                             %subst(vlrppt:posDaDft+1:
070400080516     C                             posADft-posDaDft-1):1:3)
070500080516     C                             = 'TIC'
070600080516     C                   EVAL      VABTIC=%trim(%subst(
070700080516     C                             %subst(vlrppt:posDaDft+1:
070800080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900080516     C                   ENDIF
071000040714     C* VATTRC
071100040714     C                   IF        %subst(
071200040714     C                             %subst(vlrppt:posDaDft+1:
071300040714     C                             posADft-posDaDft-1):1:3)
071400040714     C                             = 'TRC'
071500040714     C                   EVAL      VATTRC=%trim(%subst(
071600040714     C                             %subst(vlrppt:posDaDft+1:
071700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800040714     C                   ENDIF
071900030822     C* ...
072000030822     C                   ENDIF
072100030822     C                   ENDDO
072200020204     C*
072300010330     C                   ENDSR
072400010607     C*----------------------------------------------------*
072500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
072600010607     C*----------------------------------------------------*
072700010607     C     IMPVAB        BEGSR
072800010607     C*
072900010607     C                   SETOFF                                       3132
073000030515     C*
073100010607     C                   MOVEL     datcor        VABAAS
073200010607     C                   MOVE      datcor        VABMGS
073300010607     C*
073400030715     C**********
073500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
073600030715     C**********
073700030715     C                   IF        wGiro = *zeros
073800040113     C*
073900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
074000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
074100040510     C                             %trim(intcol(3))<>*blanks
074200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
074300040113     C                                      %trim(intcol(3))
074400040510     C                   ENDIF
074500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
074600031201     C     minu:maiu     XLATE     vindta        vindta
074700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
074800030715     C                   DOW       posDa <= %len(%trim(vindta))
074900030715     C*
075000030715     C* Gestisco il 1° campo
075100030715     C                   IF        i = 1
075200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
075300030715     C                   EVAL      posDa = 1
075400030715     C* Eseguo lo scan x trovare la fine del primo campo
075500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
075600030715     C* A questo punto "estraggo" il campo corrente
075700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
075800030715     C                                                    (posA-posDa))
075900030715     C* X i campi successivi al 1°
076000030715     C                   ELSE
076100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
076200030715     C                   EVAL      posDa = posA + 1
076300030715     C* Eseguo lo scan x trovare la fine del campo corrente
076400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
076500030715     C* Gestisco l'ultimo campo
076600030715     C                   IF        posA = *zeros
076700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
076800030715     C* A questo punto "estraggo" il campo corrente
076900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077000030715     C                                                    (posA-posDa))
077100030715     C                   LEAVE
077200030715     C                   ELSE
077300030715     C* A questo punto "estraggo" il campo corrente
077400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077500030715     C                                                    (posA-posDa))
077600030715     C                   ENDIF
077700030715     C                   ENDIF
077800030715     C* Incremento il contatore d campo
077900030715     C                   EVAL      i = i +1
078000030715     C                   ENDDO
078100030715     C*
078200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
078300030715     C* delimitazione testo
078400030715     C                   EVAL      i = 1
078500030715     C                   DOW       i < %elem(SkSplitFLD)
078600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
078700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
078800030715     C                   EVAL      i = i + 1
078900030715     C                   ENDDO
079000030715     C                   EVAL      wGiro = 1
079100030715     C                   ELSE
079200030715     C                   EVAL      wGiro = 2
079300031204     C**********
079400031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
079500031204     C* d separatore campo
079600031204     C**********
079700031204     C                   EVAL      vindta = %trim(vindta)
079800031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
079900031204     C                   EVAL      vindta = %subst(vindta:2)
080000031204     C                   ENDDO
080100031204     C*
080200031204     C                   Z-ADD     *zeros        lunghInput        4 0
080300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080400031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
080500031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
080600031204     C                   EVAL      lunghInput = %len(%trim(vindta))
080700031204     C                   ENDDO
080800030715     C**********
080900030715     C* Effettuo lo split del campo dati d input
081000030715     C**********
081100030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081200030715     C                   DOW       posDa <= %len(%trim(vindta))
081300030715     C*
081400030715     C* Gestisco il 1° campo
081500030715     C                   IF        i = 1
081600030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
081700030715     C                   EVAL      posDa = 1
081800030715     C* Eseguo lo scan x trovare la fine del primo campo
081900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082000030715     C* A questo punto "estraggo" il campo corrente
082100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
082200030715     C                                                    (posA-posDa))
082300030715     C* X i campi successivi al 1°
082400030715     C                   ELSE
082500030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
082600030715     C                   EVAL      posDa = posA + 1
082700031204     C* Verifico che nn vi sia il campo nn valorizzato
082800031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
082900031204     C* Se campo nn valorizzato skippo al prossimo
083000031204     C                   EVAL      SkSplitCSV(i) = *blanks
083100031204     C                   EVAL      posA  = posA + 1
083200031204     C                   ELSE
083300030715     C* Eseguo lo scan x trovare la fine del campo corrente
083400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083500030715     C* Gestisco l'ultimo campo
083600030715     C                   IF        posA = *zeros
083700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083800030715     C* A questo punto "estraggo" il campo corrente
083900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084000030715     C                                                    (posA-posDa))
084100030715     C                   LEAVE
084200030715     C                   ELSE
084300030715     C* A questo punto "estraggo" il campo corrente
084400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084500030715     C                                                    (posA-posDa))
084600030715     C                   ENDIF
084700031204     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitCSV)
085700030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
085800030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C*
086200030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
086300030715     C                   EVAL      i = 1
086400030715     C                   DOW       i < %elem(SkSplitFLD)
086500030715     C*
086600030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
086700030715     C                   IF        SkSplitFLD(i) <> *blanks AND
086800030715     C                             SkSplitCSV(i) <> *blanks
086900030721     C*** ATB
087000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
087100030721     C                   EVAL      VABATB = SkSplitCSV(i)
087200030721     C                   ENDIF
087300030715     C*** RSD
087400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
087500030715     C                   EVAL      VABRSD = SkSplitCSV(i)
087600030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
087700030715     C     '@':'A'       XLATE     VABRSD        VABRSD
087800030715     C* ==
087900030715     C                   ENDIF
088000030721     C*** RD2
088100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
088200030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
088300030721     C                   ENDIF
088400030715     C*** IND
088500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
088600030715     C                   EVAL      VABIND = SkSplitCSV(i)
088700030715     C                   ENDIF
088800030715     C*** LOD
088900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
089000030715     C                   EVAL      VABLOD = SkSplitCSV(i)
089100030715     C                   ENDIF
089200030715     C*** PRD
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
089400030715     C                   EVAL      VABPRD = SkSplitCSV(i)
089500030715     C                   ENDIF
089600030721     C*** NZD
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
089800030721     C                   EVAL      VABNZD = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030715     C*** RMA
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
090200030715     C                   EVAL      VABRMA = SkSplitCSV(i)
090300030715     C                   ENDIF
090400030715     C*** NOT
090500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
090600030715     C                   EVAL      VABNOT = SkSplitCSV(i)
090700030715     C                   ENDIF
090800030715     C*** NT2
090900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
091000030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
091100030715     C                   ENDIF
091200030715     C*** VCA
091300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
091400030715     C                   EVAL      VABVCA = SkSplitCSV(i)
091500030715     C                   ENDIF
091600030715     C*** VAS
091700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
091800030715     C                   EVAL      VABVAS = SkSplitCSV(i)
091900030715     C                   ENDIF
092000030715     C*** FFD
092100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
092200030715     C                   EVAL      VABFFD = SkSplitCSV(i)
092300030715     C                   ENDIF
092400030721     C*** GC1
092500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
092600030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
092700100304     C                   IF        %subst(VABGC1:1) = 'P' or
092800100304     C                             %subst(VABGC1:1) = 'M'
092900100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
093000100304     C                   ENDIF
093100030721     C                   ENDIF
093200030721     C*** GC2
093300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
093400030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
093500100304     C                   IF        %subst(VABGC2:1) = 'P' or
093600100304     C                             %subst(VABGC2:1) = 'M'
093700100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
093800100304     C                   ENDIF
093900030721     C                   ENDIF
094000030721     C*** TSP
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
094200030721     C                   EVAL      VABTSP = SkSplitCSV(i)
094300030721     C                   ENDIF
094400040510     C*** CBO
094500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
094600040510     C                   EVAL      VABCBO = SkSplitCSV(i)
094700040510     C                   ENDIF
094800030721     C*** NAS
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
095000030721     C                   EVAL      VABNAS = SkSplitCSV(i)
095100030721     C                   ENDIF
095200030721     C*** TIC
095300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
095400030721     C                   EVAL      VABTIC = SkSplitCSV(i)
095500030721     C                   ENDIF
095600030721     C*** GCA
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
095800030721     C                   EVAL      VABGCA = SkSplitCSV(i)
095900030721     C                   ENDIF
096000030721     C*** XCO
096100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
096200030721     C                   EVAL      VABXCO = SkSplitCSV(i)
096300030721     C                   ENDIF
096400030721     C*** CTM
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
096600030721     C                   EVAL      VABCTM = SkSplitCSV(i)
096700030721     C                   ENDIF
096800030721     C*** TCR
096900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
097000030721     C                   EVAL      VABTCR = SkSplitCSV(i)
097100030721     C                   ENDIF
097200030721     C*** CTS
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
097400030721     C                   EVAL      VABCTS = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** FTM
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
097800030721     C                   EVAL      VABFTM = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** VAD
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
098200030721     C                   EVAL      VABVAD = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** GMA
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
098600030721     C                   EVAL      VABGMA = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** GGA
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
099000030721     C                   EVAL      VABGGA = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** GVA
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
099400030721     C                   EVAL      VABGVA = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** TC1
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
099800030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** TC2
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
100200030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** SCL
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
100600030721     C                   EVAL      VABSCL = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** RMO
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
101000030721     C                   EVAL      VABRMO = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** NMO
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
101400030721     C                   EVAL      VABNMO = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*
101700030721     C* Reperisco quindi i campi numerici...
101800040510     C*** NSP
101900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
102000040510     C                   EVAL      PiStr=SkSplitCSV(i)
102100040510     C                   EXSR      CHKNUM
102200040510     C                   IF        PiInt=*on
102300040510     C                   Z-ADD     PiVal         VABNSP
102400040510     C                   ELSE
102500040510     C                   SETON                                        31
102600040510     C                   EVAL      VABNSP = *zeros
102700040510     C                   EVAL      vinmsg = %trimr(vinmsg)
102800040510     C                             + ' ' + 'VABNSP'
102900040510     C                   ENDIF
103000040510     C                   ENDIF
103100070703     C*** RMN
103200070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
103300070703     C                   EVAL      PiStr=SkSplitCSV(i)
103400070703     C                   EXSR      CHKNUM
103500070703     C                   IF        PiInt=*on
103600070703     C                   Z-ADD     PiVal         VABRMN
103700070703     C                   ELSE
103800070703     C                   SETON                                        32
103900070703     C                   EVAL      vinmsg = %trimr(vinmsg)
104000070703     C                             + ' ' + 'VABRMN'
104100070703     C                   ENDIF
104200070703     C                   ENDIF
104300070705     C*** CAD
104400070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
104500070705     C                   EVAL      VABCAD = SkSplitCSV(i)
104600070705     C                   ENDIF
104700030721     C*** NCL
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
104900030721     C                   EVAL      PiStr=SkSplitCSV(i)
105000030721     C                   EXSR      CHKNUM
105100030721     C                   IF        PiInt=*on
105200081007     C  N40              Z-ADD     PiVal         VABNCL
105300081007     C   40              ADD       PiVal         VABNCL
105400030721     C                   ELSE
105500030721     C                   SETON                                        32
105600030721     C                   EVAL      VABNCL = *zeros
105700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
105800040122     C                             + ' ' + 'VABNCL'
105900030721     C                   ENDIF
106000030721     C                   ENDIF
106100030721     C*** PKB
106200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
106300030721     C                   EVAL      PiStr=SkSplitCSV(i)
106400030721     C                   EXSR      CHKNUM
106500030721     C                   IF        PiNum=*on
106600081007     C  N40              Z-ADD     PiVal         VABPKB
106700081007     C   40              ADD       PiVal         VABPKB
106800030721     C                   ELSE
106900030721     C                   SETON                                        32
107000030721     C                   EVAL      VABPKB = *zeros
107100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
107200030721     C                             + ' ' + 'VABPKB'
107300030721     C                   ENDIF
107400030721     C                   ENDIF
107500030721     C*** CAS
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
107700030721     C                   EVAL      PiStr=SkSplitCSV(i)
107800030721     C                   EXSR      CHKNUM
107900030721     C                   IF        PiNum=*on
108000110518     C                   EVAL      PiVal = PiVal / 100
108100110518     C                   Z-ADD(H)  PiVal         VABCAS
108200030721     C                   ELSE
108300030721     C                   SETON                                        32
108400030721     C                   EVAL      VABCAS = *zeros
108500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108600030721     C                             + ' ' + 'VABCAS'
108700030721     C                   ENDIF
108800030721     C                   ENDIF
108900030721     C*** CCM
109000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
109100030721     C                   EVAL      PiStr=SkSplitCSV(i)
109200030721     C                   EXSR      CHKNUM
109300030721     C                   IF        PiInt=*on      AND
109400030721     C                             PiVal<=9999999 AND
109500030721     C                             PiVal>*zeros
109600030721     C                   Z-ADD     PiVal         VABCCM
109700030721     C                   ELSE
109800030721     C                   SETON                                        32
109900030721     C                   EVAL      VABCCM = *zeros
110000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110100040510     C                             + ' ' + 'VABCCM'
110200030721     C                   ENDIF
110300030721     C                   ENDIF
110400030721     C*** LNP
110500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
110600030721     C                   EVAL      PiStr=SkSplitCSV(i)
110700030721     C                   EXSR      CHKNUM
110800030721     C                   IF        PiInt=*on    AND
110900030721     C                             PiVal<=999   AND
111000030721     C                             PiVal>*zeros
111100030721     C                   Z-ADD     PiVal         VABLNP
111200030721     C                   ELSE
111300040715     C                   SETON                                        32
111400030721     C                   EVAL      VABLNP = *zeros
111500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111600040510     C                             + ' ' + 'VABLNP'
111700030721     C                   ENDIF
111800030721     C                   ENDIF
111900030721     C*** NRS
112000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
112100030721     C                   EVAL      PiStr=SkSplitCSV(i)
112200030721     C                   EXSR      CHKNUM
112300030721     C                   IF        PiInt=*on AND
112400030721     C                             PiVal<=99
112500030721     C                   Z-ADD     PiVal         VABNRS
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112900030721     C                             + ' ' + 'VABNRS'
113000030721     C                   ENDIF
113100030721     C                   ENDIF
113200030721     C*** LNA
113300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
113400030721     C                   EVAL      PiStr=SkSplitCSV(i)
113500030721     C                   EXSR      CHKNUM
113600030721     C                   IF        PiInt=*on    AND
113700030721     C                             PiVal<=999   AND
113800030721     C                             PiVal>*zeros
113900030721     C                   Z-ADD     PiVal         VABLNA
114000030721     C                   ELSE
114100030721     C                   SETON                                        32
114200030721     C                   EVAL      VABLNA = *zeros
114300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114400030721     C                             + ' ' + 'VABLNA'
114500030721     C                   ENDIF
114600030721     C                   ENDIF
114700030721     C*** CTR
114800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
114900030721     C                   EVAL      PiStr=SkSplitCSV(i)
115000030721     C                   EXSR      CHKNUM
115100030721     C                   IF        PiInt=*on  AND
115200030721     C                             PiVal<=999
115300030721     C                   Z-ADD     PiVal         VABCTR
115400030721     C                   ELSE
115500030721     C                   SETON                                        32
115600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115700030721     C                             + ' ' + 'VABCTR'
115800030721     C                   ENDIF
115900030721     C                   ENDIF
116000030721     C*** IAS
116100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
116200030721     C                   EVAL      PiStr=SkSplitCSV(i)
116300030721     C                   EXSR      CHKNUM
116400030721     C                   IF        PiNum=*on
116500030721     C                   Z-ADD     PiVal         VABIAS
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900030721     C                             + ' ' + 'VABIAS'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** VLB
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiNum=*on
117700081007     C  N40              Z-ADD     PiVal         VABVLB
117800081007     C   40              ADD       PiVal         VABVLB
117900030721     C                   ELSE
118000030721     C                   SETON                                        32
118100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118200030721     C                             + ' ' + 'VABVLB'
118300030721     C                   ENDIF
118400030721     C                   ENDIF
118500030721     C*** QFT
118600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
118700030721     C                   EVAL      PiStr=SkSplitCSV(i)
118800030721     C                   EXSR      CHKNUM
118900030721     C                   IF        PiNum=*on
119000081007     C  N40              Z-ADD     PiVal         VABQFT
119100081007     C   40              ADD       PiVal         VABQFT
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABQFT'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** NCD
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiInt=*on      AND
120300030721     C                             PiVal<=9999999
120400030721     C                   Z-ADD     PiVal         VABNCD
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABNCD'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** NCA
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiInt=*on      AND
121600030721     C                             PiVal<=9999999
121700030721     C                   Z-ADD     PiVal         VABNCA
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABNCA'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** ZNC
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on AND
122900030721     C                             PiVal<=99
123000030721     C                   Z-ADD     PiVal         VABZNC
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABZNC'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** DCR
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on       AND
124200030721     C                             PiVal<=99999999
124300030721     C                   Z-ADD     PiVal         VABDCR
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABDCR'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** HCR
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on   AND
125500030721     C                             PiVal<=9999
125600030721     C                   Z-ADD     PiVal         VABHCR
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABHCR'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** VMD
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiNum=*on
126800030721     C                   Z-ADD     PiVal         VABVMD
126900030721     C                   ELSE
127000030721     C                   SETON                                        32
127100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127200030721     C                             + ' ' + 'VABVMD'
127300030721     C                   ENDIF
127400030721     C                   ENDIF
127500030721     C*** ANT
127600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
127700030721     C                   EVAL      PiStr=SkSplitCSV(i)
127800030721     C                   EXSR      CHKNUM
127900030721     C                   IF        PiInt=*on        AND
128000030721     C                             PiVal<=999999999
128100030721     C                   Z-ADD     PiVal         VABANT
128200030721     C                   ELSE
128300030721     C                   SETON                                        32
128400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128500030721     C                             + ' ' + 'VABANT'
128600030721     C                   ENDIF
128700030721     C                   ENDIF
128800030721     C*** CMO
128900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
129000030721     C                   EVAL      PiStr=SkSplitCSV(i)
129100030721     C                   EXSR      CHKNUM
129200030721     C                   IF        PiInt=*on
129300030721     C                   Z-ADD     PiVal         Num5_0
129400030721     C                   MOVEL(p)  Num5_0        VABCMO
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      VABCMO = *zeros
129800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129900030721     C                             + ' ' + 'VABCMO'
130000030721     C                   ENDIF
130100030721     C                   ENDIF
130200040510     C*
130300040510     C* Reperisco i campi del FIVAT
130400040510     C*** VATNOT
130500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
130600040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
130700040510     C                   ENDIF
130800040510     C*** VATTRC
130900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
131000040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
131100040510     C                   ENDIF
131200060331     C*** VATNOT_A
131300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
131400060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
131500060331     C                   ENDIF
131600060331     C*** VATNOT_B
131700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
131800060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
131900060331     C                   ENDIF
132000060331     C*** VATNOT_E
132100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
132200060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
132300060331     C                   ENDIF
132400040510     C*
132500030715     C                   ENDIF
132600030715     C                   EVAL      i = i + 1
132700030715     C                   ENDDO
132800040510     C*
132900040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
133000040510     C                   IF        VABNSP = *zeros
133100040510     C                   SETON                                        31
133200040510     C                   ENDIF
133300040715     C*
133400040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
133500040715     C                   IF        VABLNP = *zeros
133600040715     C                   SETON                                        31
133700040715     C                   ENDIF
133800040510     C*
133900040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
134000040510     C                   IF        VABRMN = *zeros
134100040510     C                   Z-ADD     VABNSP        VABRMN
134200040510     C                   ENDIF
134300040510     C*
134400040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
134500040510     C                   IF        VABRMA = *blanks
134600040714     C                   MOVEL     VABRMN        VABRMA
134700040510     C                   ENDIF
134800070705     C*
134900070705     C* Considerazioni particolari
135000070705     C*** NZD
135100070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
135200070705     C                   EVAL      VABNZD = *blanks
135300070705     C                   ENDIF
135400070705     C*** CAD
135500070705     C                   IF        VABNZD = *blanks
135600070705     C                   EVAL      PiStr=VABCAD
135700070705     C                   EXSR      CHKNUM
135800070705     C                   IF        PiInt=*on
135900070705     C                   Z-ADD     PiVal         Num5_0
136000070705     C                   MOVEL(p)  Num5_0        VABCAD
136100070705     C                   ELSE
136200070705     C                   SETON                                        32
136300070705     C                   EVAL      VABCAD = *zeros
136400070705     C                   EVAL      vinmsg = %trimr(vinmsg)
136500070705     C                             + ' ' + 'VABCAD'
136600070705     C                   ENDIF
136700070705     C                   ENDIF
136800040128     C*
136900040510     C* Se provincia nn valorizzata la reperisco
137000040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
137100040510     C                   IF        VABPRD = *blanks
137200040128     C                   CLEAR                   TISI95DS
137300040128     C                   EVAL      I95TCN = '3'
137400040128     C                   Z-ADD     datcor        I95DAT
137500040128     C                   EVAL      I95NAR = VABNZD
137600040128     C                   EVAL      I95CAP = VABCAD
137700040128     C                   EVAL      I95LOC = VABLOD
137800040128     C                   CALL      'TISI95R'
137900040128     C                   PARM                    TISI95DS
138000040128     C                   EVAL      VABPRD = O95PRV
138100040510     C                   ENDIF
138200040113     C*
138300040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
138400040113     C                   IF        VABCAS > *zeros
138500040119     C                   EVAL      VABVCA = 'EUR'
138600040119     C                   IF        VABCBO = '1'
138700040113     C                   EVAL      VABCBO = '4'
138800040119     C                   ENDIF
138900040119     C                   IF        VABCBO = '2'
139000040119     C                   EVAL      VABCBO = '6'
139100040119     C                   ENDIF
139200040113     C                   ENDIF
139300020204     C*
139400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
139500020204     C                   EXSR      CHKIMPDIV
139600030715     C                   ENDIF
139700020204     C*
139800010607     C                   ENDSR
139900010607     C*----------------------------------------------------*
140000010601
140100020204
140200020204     C*----------------------------------------------------*
140300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
140400020204     C*----------------------------------------------------*
140500020204     C     CHKIMPDIV     BEGSR
140600020204     C*
140700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
140800020204     C                   Z-ADD     *zeros        wrkDec            9 9
140900020204     C*
141000020204     C* Come prima cosa effettuo considerazioni sulla divisa
141100020204     C                   IF        vabIAS > *zeros
141200020204     C                   IF        vabVAS <> 'EUR'
141300020204     C                   EVAL      vabVAS =  'ITL'
141400020204     C                   ENDIF
141500020204     C                   ENDIF
141600020204     C*
141700020204     C                   IF        vabCAS > *zeros
141800020204     C                   IF        vabVCA <> 'EUR'
141900020204     C                   EVAL      vabVCA =  'ITL'
142000020204     C                   ENDIF
142100020204     C                   ENDIF
142200020204     C*
142300020204     C                   IF        vabVMD > *zeros
142400020204     C                   IF        vabVAD <> 'EUR'
142500020204     C                   EVAL      vabVAD =  'ITL'
142600020204     C                   ENDIF
142700020204     C                   ENDIF
142800020204     C*
142900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
143000020204     C                   Z-ADD     vabIAS        wrkDec
143100020204     C                   IF        wrkDec > *zeros
143200020204     C                   IF        vabVAS = 'ITL'
143300020204     C                   EVAL      vabIAS = *zeros
143400020204     C                   ENDIF
143500020204     C                   ENDIF
143600020204     C*
143700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
143800020204     C                   Z-ADD     vabCAS        wrkDec
143900020204     C                   IF        wrkDec > *zeros
144000020204     C                   IF        vabVCA = 'ITL'
144100020204     C                   EVAL      vabCAS = *zeros
144200020204     C                   ENDIF
144300020204     C                   ENDIF
144400020204     C*
144500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
144600020204     C                   Z-ADD     vabVMD        wrkDec
144700020204     C                   IF        wrkDec > *zeros
144800020204     C                   IF        vabVAD = 'ITL'
144900020204     C                   EVAL      vabVMD = *zeros
145000020204     C                   ENDIF
145100020204     C                   ENDIF
145200020204     C*
145300020204     C                   ENDSR
145400020204     C***
145500020204
145600010330
145700010330
145800010330     C*----------------------------------------------------*
145900010330     C*  CONTROLLO NUMERICITA' CAMPI
146000010330     C*----------------------------------------------------*
146100010330     C     CHKNUM        BEGSR
146200010330     C*
146300010606     C                   IF        PiDecChr = *blanks
146400030715     C                   EVAL      PiDecChr = CharNUM
146500010606     C                   ENDIF
146600010606     C*
146700010606     C                   CALL(e)   'ISNUMERIC'
146800010330     C                   PARM                    PiStr            30
146900010606     C                   PARM                    PiDecChr          1
147000010330     C                   PARM      *ZEROS        PiVal            30 9
147100010330     C                   PARM      '0'           PiInt             1
147200010330     C                   PARM      '0'           PiNum             1
147300010330     C                   IF        %error
147400010606     C                   EVAL      PiNum=*off
147500010330     C                   ENDIF
147600010330     C*
147700010330     C                   ENDSR
147800010330     C***
147900010330
148000010601
148100010601
148200010601
148300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
148400010601     C     repfil        BEGSR
148500010601     C*
148600010601     C                   if        invfil = *zeros and
148700010601     C                             depfil > *zeros and
148800010629     C                             (vinflg = *blanks or
148900010629     C                              vinflg = *zeros)
149000010601     C
149100010601     C                   eval      invfil = depfil
149200010601     C                   endif
149300010601     C*
149400010601     C                   if        depfil <> invfil and
149500010601     C                             invfil > *zeros
149600010601     C                   eval      flgMulti = '1'
149700010601     C                   if        vinflg = *blanks
149800010601     C                   add       1             cntNonEl
149900010601     C                   endif
150000010601     C                   endif
150100010601     C*
150200010601     C                   if        vinflg = '2'
150300010601     C                   eval      flgStato = '2'
150400010601     C                   endif
150500010601     C*
150600010601     C                   ENDSR
150700010601     C***
150800010601
150900010601
151000010601
151100010330
151200040119      /TITLE Invio dei dati al punto operativo.
151300040119     C     invio         BEGSR
151400040119     C*
151500040119     C* 1° invio FIVAT
151600040119     C                   reset                   dscmz
151700070704     C                   move      winvcmz       cmzdst
151800040119     C                   eval      cmzfld = 'FIVATWWR'
151900040119     C                   eval      cmzmbd = vlrhdl
152000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
152100040119     C***                if        prmfir = *blanks
152200040119     C                   eval      cmzfla = 'FIVAT00F'
152300040119     C                   eval      cmzmba = 'FIVAT00F'
152400040119     C***                else
152500040119     C***                eval      cmzfla = prmfir
152600040119     C***                eval      cmzmba = prmfir
152700040119     C***                endif
152800040119     C                   eval      cmznrr = *zeros
152900040119     C                   move      §ctrokvt      cmznrr
153000040119     C                   eval      cmzlba = vlrfl1
153100040119     C                   call(e)   'TIS711C'
153200040119     C                   parm                    dscmz
153300040119     C                   parm      *blanks       esito
153400040119     C                   if        %error
153500040119     C                             or cmzerr = '1'
153600040119     C                             or esito  = '1'
153700040119     C                   eval      wrkesito = '3'
153800040119     C                   else
153900040119     C*
154000040119     C* 2° invio FIVAB
154100040119     C                   reset                   dscmz
154200070704     C                   move      winvcmz       cmzdst
154300040119     C                   eval      cmzfld = vlrfou
154400040119     C                   eval      cmzmbd = vlrhdl
154500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
154600040119     C***                if        prmfir = *blanks
154700040119     C                   eval      cmzfla = 'FIVAB00F'
154800040119     C                   eval      cmzmba = 'FIVAB00F'
154900040119     C***                else
155000040119     C***                eval      cmzfla = prmfir
155100040119     C***                eval      cmzmba = prmfir
155200040119     C***                endif
155300040119     C                   eval      cmznrr = *zeros
155400040119     C                   move      §ctrokvb      cmznrr
155500040119     C                   eval      cmzlba = vlrfl1
155600040119     C                   call(e)   'TIS711C'
155700040119     C                   parm                    dscmz
155800040119     C                   parm      *blanks       esito
155900040119     C                   if        %error
156000040119     C                             or cmzerr = '1'
156100040119     C                             or esito  = '1'
156200040119     C                   eval      wrkesito = '3'
156300040119     C                   endif
156400040119     C                   endif
156500040119     C*
156600040119     C                   ENDSR
156700040119     C***
156800010601
156900010601
157000010601
157100010601
157200010601      /TITLE Invio dei dati al punto operativo.
157300010601     C     opeini        BEGSR
157400010601     C*
157500010601     C* Inizializzo flag e contatori operativi
157600010601     C                   movel     '0'           flgGiro           1
157700010601     C                   movel     '0'           flgMulti          1
157800010601     C                   movel     '1'           flgStato          1
157900010615     C                   movel     '0'           flgOk             1
158000010601     C                   z-add     *zeros        cntNonEl         10 0
158100010601     C                   z-add     *zeros        depfil            3 0
158200010601     C                   z-add     *zeros        invfil            3 0
158300010601     C*
158400010601     C                   ENDSR
158500010601     C***
158600070326
158700070326
158800070326
158900070326
159000070326     C     *pssr         BEGSR
159100070329     C*
159200070329     C                   if        %open(tivin00r)
159300070329     C                   close     tivin00r
159400070329     C                   endif
159500070329     C                   if        %open(fivabwwr)
159600070329     C                   close     fivabwwr
159700070329     C                   endif
159800070329     C                   if        %open(fivatwwr)
159900070329     C                   close     fivatwwr
160000070329     C                   endif
160100070326     C*
160200070326     C* Effettuo la chiamata al CLLE preposto
160300070326     C                   call(e)   'TITVVTC'
160400070326     C                   parm                    parccm
160500070326     C                   parm                    parmbr
160600070326     C                   parm      '2'           paropz
160700070326     C*
160800070326     C                   eval      wrkesito = '2'
160900070404     C*
161000070404     C                   seton                                        LR
161100070326     C*
161200070326     C                   ENDSR     '*CANCL'
161300070326     C***
161400070326
161500070326
161600010330
161700010330
161800000613     C     *inzsr        BEGSR
161900990910     C*
162000990910     C     *entry        plist
162100990920     C                   parm                    tivlrds
162200990921     C                   parm      wrkesito      esito
162300000724     C                   parm                    prmlit
162400000710     C                   parm                    prmfir
162500010330     C*
162600010330     C* CALCOLA LA DATA CORRENTE
162700010330     C                   time                    wn14             14 0
162800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
162900010330     C                   z-add     wn8           g08dat
163000010330     C                   z-add     *zeros        g08inv
163100010330     C                   movel     '0'           g08err
163200010330     C                   call      'XSRDA8'
163300010330     C                   parm                    wlbda8
163400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
163500000613     C*
163600000613     C                   ENDSR
163700000613     C***
163800040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
163900040510
164000040122
164100040113
