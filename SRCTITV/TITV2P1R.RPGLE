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
003300110622     D SkSplitFLD      S             30    DIM(150)
003400110622     D SkSplitCSV      S            256    DIM(150)
003500030715     D CharCSV         S              1
003600030715     D CharTXT         S              1
003700030715     D CharNUM         S              1
003800090324     D CharSOS         S              1
003900110622     D posDa           S              4  0 INZ(*zeros)
004000110622     D posA            S              4  0 INZ(*zeros)
004100030715     D i               s              3  0 INZ(1)
004200030715     D wGiro           s              1  0 INZ(*zeros)
004300060331     D wVATNOT_A       s                   LIKE(VATNOT)
004400060331     D wVATNOT_B       s                   LIKE(VATNOT)
004500060331     D wVATNOT_E       s                   LIKE(VATNOT)
004600110912     D wVATNOT_I       s                   LIKE(VATNOT)
004700110912     D wVATNOT_J       s                   LIKE(VATNOT)
004800110912     D wVATNOT_4       s                   LIKE(VATNOT)
004900060331     D wVABNSP         s                   LIKE(VABNSP) INZ
005000030822     D*------------------
005100030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005200030822     D*------------------
005300030822     D posDaDft        S              3  0 INZ(*zeros)
005400030822     D posADft         S              3  0 INZ(*zeros)
005500030822     D j               s              3  0 INZ(1)
005600030822     D wGiroDft        s              1  0 INZ(*zeros)
005700010330     D*------------------
005800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
005900010330     D*------------------
006000010330     D WLBDA8          DS                  INZ
006100010330     D  G08DAT                 1      8  0
006200010330     D  G08INV                 9     16  0
006300010330     D  G08ERR                17     17
006400010330     D  G08TGI                18     22  0
006500031201     D*------------------
006600031201     D* Costanti
006700031201     D*------------------
006800031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006900031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007000010330
007100010330
007200010330
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600010601     C*
007700010601     C                   exsr      opeini
007800010605     C                   exsr      rwvab
007900010601     C*
008000010601     C                   seton                                        lr
008100010601
008200010601
008300010601
008400010601     C*--------------------------------------------------------
008500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008600010601     C*--------------------------------------------------------
008700010601     C     PREELA        BEGSR
008800010601     C*
008900010601     C* SE OCCORRE SPEDIRE IN FILIALE
009000010601     C                   if        invfil <> *zeros and
009100010601     C                             flgGiro = '0'
009200010601     C*
009300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009400010601     C                   eval      flgGiro = '1'
009500010601     C*
009600010601     C                   endif
009700010601     C*
009800010601     C                   ENDSR
009900010601     C***
010000010601
010100010601
010200010601
010300010601
010400010601     C*--------------------------------------------------------
010500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010601     C*--------------------------------------------------------
010700010601     C     ENDELA        BEGSR
010800040119     C*
010900040119     C* Effettuo la chiamata al CLLE preposto
011000040119     C                   call(e)   'TITVVTC'
011100040510     C                   parm                    parccm
011200040119     C                   parm                    parmbr
011300040119     C                   parm      '2'           paropz
011400050201     C*
011500050201     C* Effettuo lancio TISI95 solo x chiusura
011600050201     C                   CLEAR                   TISI95DS
011700050201     C                   EVAL      I95TLA = 'C'
011800050201     C                   CALL      'TISI95R'
011900050201     C                   PARM                    TISI95DS
012000000616     C*
012100010601     C                   ENDSR
012200010601     C***
012300000613
012400010601
012500010601
012600010330     C*--------------------------------------------------------
012700021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012800010330     C*--------------------------------------------------------
012900010605     C     RWVAB         BEGSR
013000010330     C*
013100010330     C                   if        not %open(tivin00r)
013200010330     C                   open      tivin00r
013300010330     C                   endif
013400021025     C                   if        not %open(fivabwwr)
013500021025     C                   open      fivabwwr
013600010330     C                   endif
013700040119     C*
013800040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013900040119     C                   exsr      prevat
014000040119     C*
014100040119     C                   if        chkcall = '0'
014200040119     C*
014300040119     C                   if        not %open(fivatwwr)
014400040119     C                   open      fivatwwr
014500040119     C                   endif
014600010330     C*
014700040119     C                   clear                   §CTROKVB          7 0
014800040119     C                   clear                   §CTROKVT          7 0
014900040119     C                   clear                   §CTRMO            7 0
015000040119     C                   clear                   §CTRNO            7 0
015100010330     C*
015200030822     C                   do        *HIVAL
015300010330     C*
015400030822     C                   read      tivin00r                               70
015500010618     C*
015600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015700010618     C                   if        vinflg = '1'
015800010618     C                   eval      flgOk = '1'
015900010618     C                   endif
016000040510     C*
016100040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016200040510     C                   z-add     *zeros        wLenVINDTA        4 0
016300040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016400090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
016500040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016600090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016700040510     C                   z-add     1             wFlgVINDTA
016800040510     C                   leave
016900040510     C                   endif
017000040510     C                   enddo
017100010618     C*
017200040510     C                   if        vindta > *blanks AND
017300040510     C                             wFlgVINDTA = 1
017400010330     C                   add       1             rrnum
017500010330     C*
017600010601     C                   if        *in70 = *off and
017700010330     C                             (vinflg = *blanks
017800010330     C                              or vinflg = '0'
017900010330     C                              or vinflg = '2')
018000010330     C*
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500121107     C*
018600121107     C                   exsr      inzvar
018700090324     C*
018800090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
018900090324     C* separatori campi
019000090324     C                   if        CharTXT <> *blanks
019100090324     C                   z-add     *zeros        wLenVINDTA        4 0
019200090324     C                   z-add     *zeros        wFlgVINDTA        1 0
019300090324     C                   setoff                                       80
019400090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
019500090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
019600090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019700090324     C                   if        not *in80
019800090324     C                   seton                                        80
019900090324     C                   else
020000090324     C                   setoff                                       80
020100090324     C                   endif
020200090324     C                   endif
020300090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
020400090324     C                             and *in80 = *on
020500090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020600090324     C                   endif
020700090324     C                   enddo
020800090324     C                   endif
020900090324     C*
021000010605     C                   exsr      impvab
021100010601     C*
021200010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021300010605     C                   eval      depfil = VABLNP
021400010601     C                   exsr      repfil
021500010601     C                   if        depfil = invfil
021600021025     C                   if        vlrpoi = 999
021700030822     C                   move(P)   invfil        VABFGS
021800070704     C                   move(P)   invfil        winvcmz           3
021900021025     C                   else
022000030822     C                   move(P)   vlrpoi        VABFGS
022100070704     C                   move(P)   vlrpoi        winvcmz
022200021025     C                   endif
022300010601     C*
022400010601     C                   exsr      PREELA
022500010601     C*
022600010604     C* Ebbene...
022700010604     C*
022800040119     C  N31              add       1             §CTROKVB
022900040119     C   32              add       1             §CTRMO
023000040119     C   31              add       1             §CTRNO
023100030822     C                   if        wGiro = 2
023200040510     C* Scrivo sempre il VAT
023300040510     C  N31              exsr      wrivat
023400040510     C* Scrivo il VAB solo a rottura d spedizione
023500040119     C                   if        depspe =  *zeros OR
023600040119     C                             depspe <> vabNSP
023700040119     C  N31              exsr      wrivab
023800040119     C  N31              eval      depspe = vabNSP
023900040119     C                   endif
024000040119     C                   endif
024100010604     C*
024200010604     C                   if        *in31 = *off and
024300010604     C                             *in32 = *off
024400010604     C                   eval      vinflg = '1'
024500010604     C                   else
024600010604     C                   eval      vinflg = '2'
024700010604     C                   endif
024800010604     C                   endif
024900010601     C*
025000010604     C                   endif
025100010604     C*
025200010330     C                   else
025300010330     C                   eval      vinflg = '1'
025400010330     C                   endif
025500010601     C*
025600010601     C  N70              update    tivin000
025700010330     C*
025800030822     C  N70              enddo
025900040119     C*
026000040119     C                   endif                                                  (endif - chkcall)
026100010601     C*
026200010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026300010601     C                   if        cntNonEl = *zeros or
026400010601     C                             flgMulti = '0'
026500010330     C* Se non ci sono record con errori ...
026600010601     C                   if        §ctrno = 0 and
026700010604     C                             §ctrmo = 0 and
026800010601     C                             flgStato <> '2'
026900010330     C* ... restituisco esito OK.
027000010330     C                   eval      wrkesito = '0'
027100010330     C                   else
027200040119     C                   if        §ctrokvb > 0
027300010330     C                   eval      wrkesito = '1'
027400010330     C                   else
027500010615     C                   if        flgOk = '0'
027600010615     C                   eval      wrkesito = '2'
027700010615     C                   else
027800010615     C                   eval      wrkesito = '6'
027900010615     C                   endif
028000010330     C                   endif
028100010330     C                   endif
028200010601     C                   else
028300010601     C                   eval      wrkesito = '9'
028400010601     C                   endif
028500010330     C*
028600010330     C                   if        %open(tivin00r)
028700010330     C                   close     tivin00r
028800010330     C                   endif
028900021025     C                   if        %open(fivabwwr)
029000021025     C                   close     fivabwwr
029100010330     C                   endif
029200040512     C                   if        %open(fivatwwr)
029300040512     C                   close     fivatwwr
029400040512     C                   endif
029500010601     C*
029600010601     C                   if        vlrpoi <> 999
029700010601     C                   eval      invfil = vlrpoi
029800010601     C                   endif
029900010330     C*
030000040119     C                   if        §ctrokvb > 0
030100010601     C                             and invfil > *zeros
030200010330     C                   exsr      invio
030300010330     C                   endif
030400010601     C*
030500010618     C                   if        flgGiro = '1'
030600010601     C                   exsr      endela
030700010618     C                   endif
030800010330     C*
030900010330     C                   ENDSR
031000010330     C***
031100040119
031200040119
031300040119
031400040119     C*----------------------------------------------------*
031500040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031600040119     C*----------------------------------------------------*
031700040510     C     WRIVAB        BEGSR
031800040119     C*
031900060331     C* Gestisco l'eventuale rottura x numero spedizione
032000060331     C                   if        VABNSP <> wVABNSP
032100070301     C                   if        wVATNOT_E <> *blanks
032200110419     C                   if        VABCTM = *blanks
032300070301     C                   eval      VABCTM = '7Q'
032400110419     C                   endif
032500070301     C                   endif
032600040119     C                   write     fivab000                                     => scarico il VAB
032700060331     C                   eval      wVABNSP = VABNSP
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
034300060512     C                   eval      VATNSP = VABNSP
034400060512     C                   eval      VATLNP = VABLNP
034500040714     C*
034600040714     C* Solo se VATNOT valorizzato scarico il buffer
034700040714     C                   if        VATNOT <> *blanks
034800040119     C*
034900040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
035000040510     C                   if        VATTRC = *blanks
035100040510     C                   eval      VATTRC = 'E'
035200040510     C                   endif
035300040510     C                   write     FIVAT000
035400040119     C                   add       1             §CTROKVT
035500040714     C*
035600040714     C                   endif
035700060331     C*
035800060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035900060331     C                   if        wVATNOT_A <> *blanks
036000060331     C                   eval      VATNOT = wVATNOT_A
036100060331     C                   eval      VATTRC = 'A'
036200060331     C                   write     FIVAT000
036300060331     C                   add       1             §CTROKVT
036400060331     C                   endif
036500060331     C*
036600060331     C                   if        wVATNOT_B <> *blanks
036700060331     C                   eval      VATNOT = wVATNOT_B
036800060331     C                   eval      VATTRC = 'B'
036900060331     C                   write     FIVAT000
037000060331     C                   add       1             §CTROKVT
037100060331     C                   endif
037200060331     C*
037300060331     C                   if        wVATNOT_E <> *blanks
037400060331     C                   eval      VATNOT = wVATNOT_E
037500060331     C                   eval      VATTRC = 'E'
037600060331     C                   write     FIVAT000
037700060331     C                   add       1             §CTROKVT
037800060331     C                   endif
037900110912     C*
038000110912     C                   if        wVATNOT_I <> *blanks
038100110912     C                   eval      VATNOT = wVATNOT_I
038200110912     C                   eval      VATTRC = 'I'
038300110912     C                   write     FIVAT000
038400110912     C                   add       1             §CTROKVT
038500110912     C                   endif
038600110912     C*
038700110912     C                   if        wVATNOT_J <> *blanks
038800110912     C                   eval      VATNOT = wVATNOT_J
038900110912     C                   eval      VATTRC = 'J'
039000110912     C                   write     FIVAT000
039100110912     C                   add       1             §CTROKVT
039200110912     C                   endif
039300110912     C*
039400110912     C                   if        wVATNOT_4 <> *blanks
039500110912     C                   eval      VATNOT = wVATNOT_4
039600110912     C                   eval      VATTRC = '4'
039700110912     C                   write     FIVAT000
039800110912     C                   add       1             §CTROKVT
039900110912     C                   endif
040000040119     C*
040100040119     C                   ENDSR
040200040119
040300040119
040400040119
040500040119     C*----------------------------------------------------*
040600040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040700040119     C*----------------------------------------------------*
040800040119     C     PREVAT        BEGSR
040900040119     C*
041000040119     C* Compongo il nome del membro da dare al FIVATWWR
041100040119     C                   eval      parmbr = vlrhdl
041200040119     C                   movel     'M'           parmbr
041300040510     C                   eval      parccm = vlrksc
041400040119     C                   eval      paropz = '1'
041500040119     C* Effettuo la chiamata al CLLE preposto
041600040119     C                   call(e)   'TITVVTC'
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
043800090403     C                   z-add     *zeros        invfil
043900060331     C                   MOVEL     *blanks       wVATNOT_A
044000060331     C                   MOVEL     *blanks       wVATNOT_B
044100060331     C                   MOVEL     *blanks       wVATNOT_E
044200110912     C                   MOVEL     *blanks       wVATNOT_I
044300110912     C                   MOVEL     *blanks       wVATNOT_J
044400110912     C                   MOVEL     *blanks       wVATNOT_4
044500081014     C*
044600081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044700081014     C                   CLEAR                   FIVAB000
044800081014     C                   CLEAR                   FIVAT000
044900030822     C*
045000081014     C* Inizializzo la schiera d wrk x i dati
045100030822     C                   CLEAR                   SkSplitCSV
045200030822     C                   EVAL      i = 1
045300030822     C                   EVAL      posDa = *zeros
045400030822     C                   EVAL      posA  = *zeros
045500030822     C*
045600030822     C* Reimposto i valori di default
045700030822     C                   EXSR      DEFCAM
045800030822     C*
045900010330     C                   ENDSR
046000010330     C*----------------------------------------------------*
046100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
046200010330     C*----------------------------------------------------*
046300010330     C     DEFCAM        BEGSR
046400030822     C*
046500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046600030715     C* e delimitatore testo.
046700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
047000090324     C*
047100090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
047200090324     C                   EVAL      CharSOS = CharNUM
047300030822     C*
047400081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
047500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
047600081007     C                   SETON                                        40
047700030715     C                   ELSE
047800081007     C                   SETOFF                                       40
047900030715     C                   ENDIF
048000081007     C*
048100081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
048200081007     C                   SETOFF                                       50
048300030822     C*
048400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
048500030822     C                   EVAL      posDaDft = 1
048600030822     C                   EVAL      posADft  = 0
048700030822     C                   EVAL      wGiroDft = 0
048800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
048900030822     C                             posDaDft > 0
049000030822     C*
049100030822     C* Gestisco il 1° giro
049200030822     C                   IF        wGiroDft = 0
049300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
049400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
049500030822     C* Incremento il contatore dei "giri"
049600030822     C                   EVAL      wGiroDft = 1
049700030822     C                   ELSE
049800030822     C                   EVAL      posDaDft = posADft
049900030822     C                   ENDIF
050000030822     C* Eseguo lo scan x trovare la fine del campo corrente
050100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
050200030822     C*
050300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
050400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
050500030822     C                   IF        posDaDft > 0 AND
050600030822     C                             posADft  > 0
050700030822     C* NCL
050800030822     C                   IF        %subst(
050900030822     C                             %subst(vlrppt:posDaDft+1:
051000030822     C                             posADft-posDaDft-1):1:3)
051100030822     C                             = 'NCL'
051200030822     C                   EVAL      PiStr=%trim(%subst(
051300030822     C                             %subst(vlrppt:posDaDft+1:
051400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
051500030822     C                   EXSR      CHKNUM
051600030822     C                   IF        PiInt=*on
051700030822     C                   Z-ADD     PiVal         VABNCL
051800030822     C                   ENDIF
051900030822     C                   ENDIF
052000030822     C* CCM
052100030822     C                   IF        %subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):1:3)
052400030822     C                             = 'CCM'
052500030822     C                   EVAL      PiStr=%trim(%subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800030822     C                   EXSR      CHKNUM
052900030822     C                   IF        PiInt=*on
053000030822     C                   Z-ADD     PiVal         VABCCM
053100030822     C                   ENDIF
053200030822     C                   ENDIF
053300030822     C* LNP
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'LNP'
053800030822     C                   EVAL      PiStr=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   EXSR      CHKNUM
054200030822     C                   IF        PiInt=*on
054300030822     C                   Z-ADD     PiVal         VABLNP
054400030822     C                   ENDIF
054500030822     C                   ENDIF
054600030822     C* NRS
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'NRS'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABNRS
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* CTR
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'CTR'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABCTR
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* PKB
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'PKB'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiNum=*on
058200030822     C                   Z-ADD     PiVal         VABPKB
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* VLB
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'VLB'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiNum=*on
059500030822     C                   Z-ADD     PiVal         VABVLB
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* QFT
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'QFT'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiNum=*on
060800030822     C                   Z-ADD     PiVal         VABQFT
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* CBO
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'CBO'
061600030822     C                   EVAL      VABCBO=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   ENDIF
062000030822     C* TSP
062100030822     C                   IF        %subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):1:3)
062400030822     C                             = 'TSP'
062500030822     C                   EVAL      VABTSP=%trim(%subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062800030822     C                   ENDIF
062900030822     C* VAS
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'VAS'
063400030822     C                   EVAL      VABVAS=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   ENDIF
063800030822     C* VCA
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'VCA'
064300030822     C                   EVAL      VABVCA=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   ENDIF
064700030822     C* GCA
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'GCA'
065200030822     C                   EVAL      VABGCA=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* CTM
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'CTM'
066100030822     C                   EVAL      VABCTM=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* FFD
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'FFD'
067000030822     C                   EVAL      VABFFD=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* VAD
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'VAD'
067900030822     C                   EVAL      VABVAD=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* GMA
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'GMA'
068800030822     C                   EVAL      VABGMA=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* GGA
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'GGA'
069700030822     C                   EVAL      VABGGA=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* GVA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'GVA'
070600030822     C                   EVAL      VABGVA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* TC1
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'TC1'
071500030822     C                   EVAL      VABTC1=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* TC2
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'TC2'
072400030822     C                   EVAL      VABTC2=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800080516     C* TIC
072900080516     C                   IF        %subst(
073000080516     C                             %subst(vlrppt:posDaDft+1:
073100080516     C                             posADft-posDaDft-1):1:3)
073200080516     C                             = 'TIC'
073300080516     C                   EVAL      VABTIC=%trim(%subst(
073400080516     C                             %subst(vlrppt:posDaDft+1:
073500080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600080516     C                   ENDIF
073700040714     C* VATTRC
073800040714     C                   IF        %subst(
073900040714     C                             %subst(vlrppt:posDaDft+1:
074000040714     C                             posADft-posDaDft-1):1:3)
074100040714     C                             = 'TRC'
074200040714     C                   EVAL      VATTRC=%trim(%subst(
074300040714     C                             %subst(vlrppt:posDaDft+1:
074400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500040714     C                   ENDIF
074600030822     C* ...
074700030822     C                   ENDIF
074800030822     C                   ENDDO
074900020204     C*
075000010330     C                   ENDSR
075100010607     C*----------------------------------------------------*
075200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
075300010607     C*----------------------------------------------------*
075400010607     C     IMPVAB        BEGSR
075500010607     C*
075600010607     C                   SETOFF                                       3132
075700030515     C*
075800010607     C                   MOVEL     datcor        VABAAS
075900010607     C                   MOVE      datcor        VABMGS
076000010607     C*
076100030715     C**********
076200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
076300030715     C**********
076400030715     C                   IF        wGiro = *zeros
076500040113     C*
076600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
076700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
076800040510     C                             %trim(intcol(3))<>*blanks
076900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
077000040113     C                                      %trim(intcol(3))
077100040510     C                   ENDIF
077200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
077300031201     C     minu:maiu     XLATE     vindta        vindta
077400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
077500030715     C                   DOW       posDa <= %len(%trim(vindta))
077600030715     C*
077700030715     C* Gestisco il 1° campo
077800030715     C                   IF        i = 1
077900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
078000030715     C                   EVAL      posDa = 1
078100030715     C* Eseguo lo scan x trovare la fine del primo campo
078200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078300030715     C* A questo punto "estraggo" il campo corrente
078400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078500030715     C                                                    (posA-posDa))
078600030715     C* X i campi successivi al 1°
078700030715     C                   ELSE
078800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078900030715     C                   EVAL      posDa = posA + 1
079000030715     C* Eseguo lo scan x trovare la fine del campo corrente
079100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079200030715     C* Gestisco l'ultimo campo
079300030715     C                   IF        posA = *zeros
079400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
079500030715     C* A questo punto "estraggo" il campo corrente
079600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079700030715     C                                                    (posA-posDa))
079800030715     C                   LEAVE
079900030715     C                   ELSE
080000030715     C* A questo punto "estraggo" il campo corrente
080100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080200030715     C                                                    (posA-posDa))
080300030715     C                   ENDIF
080400030715     C                   ENDIF
080500030715     C* Incremento il contatore d campo
080600030715     C                   EVAL      i = i +1
080700030715     C                   ENDDO
080800030715     C*
080900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
081000030715     C* delimitazione testo
081100030715     C                   EVAL      i = 1
081200030715     C                   DOW       i < %elem(SkSplitFLD)
081300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
081400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
081500030715     C                   EVAL      i = i + 1
081600030715     C                   ENDDO
081700030715     C                   EVAL      wGiro = 1
081800030715     C                   ELSE
081900030715     C                   EVAL      wGiro = 2
082000031204     C**********
082100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
082200031204     C* d separatore campo
082300031204     C**********
082400031204     C                   EVAL      vindta = %trim(vindta)
082500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
082600031204     C                   EVAL      vindta = %subst(vindta:2)
082700031204     C                   ENDDO
082800031204     C*
082900031204     C                   Z-ADD     *zeros        lunghInput        4 0
083000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
083200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
083300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
083400031204     C                   ENDDO
083500030715     C**********
083600030715     C* Effettuo lo split del campo dati d input
083700030715     C**********
083800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083900030715     C                   DOW       posDa <= %len(%trim(vindta))
084000030715     C*
084100030715     C* Gestisco il 1° campo
084200030715     C                   IF        i = 1
084300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084400030715     C                   EVAL      posDa = 1
084500030715     C* Eseguo lo scan x trovare la fine del primo campo
084600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084700030715     C* A questo punto "estraggo" il campo corrente
084800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084900030715     C                                                    (posA-posDa))
085000030715     C* X i campi successivi al 1°
085100030715     C                   ELSE
085200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085300030715     C                   EVAL      posDa = posA + 1
085400031204     C* Verifico che nn vi sia il campo nn valorizzato
085500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085600031204     C* Se campo nn valorizzato skippo al prossimo
085700031204     C                   EVAL      SkSplitCSV(i) = *blanks
085800031204     C                   EVAL      posA  = posA + 1
085900031204     C                   ELSE
086000030715     C* Eseguo lo scan x trovare la fine del campo corrente
086100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086200030715     C* Gestisco l'ultimo campo
086300030715     C                   IF        posA = *zeros
086400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C                   LEAVE
086900030715     C                   ELSE
087000030715     C* A questo punto "estraggo" il campo corrente
087100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087200030715     C                                                    (posA-posDa))
087300030715     C                   ENDIF
087400031204     C                   ENDIF
087500030715     C                   ENDIF
087600030715     C* Incremento il contatore d campo
087700030715     C                   EVAL      i = i +1
087800030715     C                   ENDDO
087900030715     C*
088000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
088100030715     C* delimitazione testo
088200030715     C                   EVAL      i = 1
088300030715     C                   DOW       i < %elem(SkSplitCSV)
088400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088600030715     C                   EVAL      i = i + 1
088700030715     C                   ENDDO
088800030715     C*
088900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
089000030715     C                   EVAL      i = 1
089100030715     C                   DOW       i < %elem(SkSplitFLD)
089200030715     C*
089300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089500030715     C                             SkSplitCSV(i) <> *blanks
089600030721     C*** ATB
089700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089800030721     C                   EVAL      VABATB = SkSplitCSV(i)
089900030721     C                   ENDIF
090000030715     C*** RSD
090100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090500030715     C* ==
090600030715     C                   ENDIF
090700030721     C*** RD2
090800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
091000030721     C                   ENDIF
091100030715     C*** IND
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091300030715     C                   EVAL      VABIND = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** LOD
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091800030715     C                   ENDIF
091900130430     C*** PRD
092000130430     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
092100130430     C                   EVAL      VABPRD = SkSplitCSV(i)
092200130430     C                   ENDIF
092300030721     C*** NZD
092400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092600030721     C                   ENDIF
092700030715     C*** RMA
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
093000030715     C                   ENDIF
093100030715     C*** NOT
093200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093400030715     C                   ENDIF
093500030715     C*** NT2
093600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093800030715     C                   ENDIF
093900030715     C*** VCA
094000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
094100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094200030715     C                   ENDIF
094300030715     C*** VAS
094400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094600030715     C                   ENDIF
094700030715     C*** FFD
094800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
095000030715     C                   ENDIF
095100030721     C*** GC1
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095400100304     C                   IF        %subst(VABGC1:1) = 'P' or
095500100304     C                             %subst(VABGC1:1) = 'M'
095600100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
095700100304     C                   ENDIF
095800030721     C                   ENDIF
095900030721     C*** GC2
096000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
096100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096200100304     C                   IF        %subst(VABGC2:1) = 'P' or
096300100304     C                             %subst(VABGC2:1) = 'M'
096400100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
096500100304     C                   ENDIF
096600030721     C                   ENDIF
096700030721     C*** TSP
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
097000030721     C                   ENDIF
097100040510     C*** CBO
097200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097400040510     C                   ENDIF
097500030721     C*** NAS
097600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097800030721     C                   ENDIF
097900030721     C*** TIC
098000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
098100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098200030721     C                   ENDIF
098300030721     C*** GCA
098400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098600030721     C                   ENDIF
098700030721     C*** XCO
098800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
099000030721     C                   ENDIF
099100030721     C*** CTM
099200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099400030721     C                   ENDIF
099500030721     C*** TCR
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099800030721     C                   ENDIF
099900030721     C*** CTS
100000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
100100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100200030721     C                   ENDIF
100300030721     C*** FTM
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100600030721     C                   ENDIF
100700030721     C*** VAD
100800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
101000030721     C                   ENDIF
101100030721     C*** GMA
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101400030721     C                   ENDIF
101500030721     C*** GGA
101600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101800030721     C                   ENDIF
101900030721     C*** GVA
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
102100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TC1
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** TC2
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** SCL
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** RMO
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** NMO
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
104100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*
104400030721     C* Reperisco quindi i campi numerici...
104500040510     C*** NSP
104600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104700040510     C                   EVAL      PiStr=SkSplitCSV(i)
104800040510     C                   EXSR      CHKNUM
104900040510     C                   IF        PiInt=*on
105000040510     C                   Z-ADD     PiVal         VABNSP
105100040510     C                   ELSE
105200040510     C                   SETON                                        31
105300040510     C                   EVAL      VABNSP = *zeros
105400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105500040510     C                             + ' ' + 'VABNSP'
105600040510     C                   ENDIF
105700040510     C                   ENDIF
105800070703     C*** RMN
105900070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
106000070703     C                   EVAL      PiStr=SkSplitCSV(i)
106100070703     C                   EXSR      CHKNUM
106200070703     C                   IF        PiInt=*on
106300070703     C                   Z-ADD     PiVal         VABRMN
106400070703     C                   ELSE
106500070703     C                   SETON                                        32
106600070703     C                   EVAL      vinmsg = %trimr(vinmsg)
106700070703     C                             + ' ' + 'VABRMN'
106800070703     C                   ENDIF
106900070703     C                   ENDIF
107000070705     C*** CAD
107100070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107200070705     C                   EVAL      VABCAD = SkSplitCSV(i)
107300070705     C                   ENDIF
107400030721     C*** NCL
107500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107600030721     C                   EVAL      PiStr=SkSplitCSV(i)
107700030721     C                   EXSR      CHKNUM
107800030721     C                   IF        PiInt=*on
107900081007     C  N40              Z-ADD     PiVal         VABNCL
108000081007     C   40              ADD       PiVal         VABNCL
108100030721     C                   ELSE
108200030721     C                   SETON                                        32
108300030721     C                   EVAL      VABNCL = *zeros
108400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108500040122     C                             + ' ' + 'VABNCL'
108600030721     C                   ENDIF
108700030721     C                   ENDIF
108800030721     C*** PKB
108900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
109000030721     C                   EVAL      PiStr=SkSplitCSV(i)
109100030721     C                   EXSR      CHKNUM
109200030721     C                   IF        PiNum=*on
109300081007     C  N40              Z-ADD     PiVal         VABPKB
109400081007     C   40              ADD       PiVal         VABPKB
109500030721     C                   ELSE
109600030721     C                   SETON                                        32
109700030721     C                   EVAL      VABPKB = *zeros
109800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109900030721     C                             + ' ' + 'VABPKB'
110000030721     C                   ENDIF
110100030721     C                   ENDIF
110200030721     C*** CAS
110300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110400030721     C                   EVAL      PiStr=SkSplitCSV(i)
110500030721     C                   EXSR      CHKNUM
110600030721     C                   IF        PiNum=*on
110700030721     C                   Z-ADD     PiVal         VABCAS
110800030721     C                   ELSE
110900030721     C                   SETON                                        32
111000030721     C                   EVAL      VABCAS = *zeros
111100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111200030721     C                             + ' ' + 'VABCAS'
111300030721     C                   ENDIF
111400030721     C                   ENDIF
111500030721     C*** CCM
111600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111700030721     C                   EVAL      PiStr=SkSplitCSV(i)
111800030721     C                   EXSR      CHKNUM
111900030721     C                   IF        PiInt=*on      AND
112000030721     C                             PiVal<=9999999 AND
112100030721     C                             PiVal>*zeros
112200030721     C                   Z-ADD     PiVal         VABCCM
112300030721     C                   ELSE
112400030721     C                   SETON                                        32
112500030721     C                   EVAL      VABCCM = *zeros
112600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112700040510     C                             + ' ' + 'VABCCM'
112800030721     C                   ENDIF
112900030721     C                   ENDIF
113000030721     C*** LNP
113100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113200030721     C                   EVAL      PiStr=SkSplitCSV(i)
113300030721     C                   EXSR      CHKNUM
113400030721     C                   IF        PiInt=*on    AND
113500030721     C                             PiVal<=999   AND
113600030721     C                             PiVal>*zeros
113700030721     C                   Z-ADD     PiVal         VABLNP
113800030721     C                   ELSE
113900040715     C                   SETON                                        32
114000030721     C                   EVAL      VABLNP = *zeros
114100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114200040510     C                             + ' ' + 'VABLNP'
114300030721     C                   ENDIF
114400030721     C                   ENDIF
114500030721     C*** NRS
114600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114700030721     C                   EVAL      PiStr=SkSplitCSV(i)
114800030721     C                   EXSR      CHKNUM
114900030721     C                   IF        PiInt=*on AND
115000030721     C                             PiVal<=99
115100030721     C                   Z-ADD     PiVal         VABNRS
115200030721     C                   ELSE
115300030721     C                   SETON                                        32
115400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115500030721     C                             + ' ' + 'VABNRS'
115600030721     C                   ENDIF
115700030721     C                   ENDIF
115800030721     C*** LNA
115900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
116000030721     C                   EVAL      PiStr=SkSplitCSV(i)
116100030721     C                   EXSR      CHKNUM
116200030721     C                   IF        PiInt=*on    AND
116300030721     C                             PiVal<=999   AND
116400030721     C                             PiVal>*zeros
116500030721     C                   Z-ADD     PiVal         VABLNA
116600030721     C                   ELSE
116700030721     C                   SETON                                        32
116800030721     C                   EVAL      VABLNA = *zeros
116900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117000030721     C                             + ' ' + 'VABLNA'
117100030721     C                   ENDIF
117200030721     C                   ENDIF
117300030721     C*** CTR
117400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117500030721     C                   EVAL      PiStr=SkSplitCSV(i)
117600030721     C                   EXSR      CHKNUM
117700030721     C                   IF        PiInt=*on  AND
117800030721     C                             PiVal<=999
117900030721     C                   Z-ADD     PiVal         VABCTR
118000030721     C                   ELSE
118100030721     C                   SETON                                        32
118200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118300030721     C                             + ' ' + 'VABCTR'
118400030721     C                   ENDIF
118500030721     C                   ENDIF
118600030721     C*** IAS
118700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118800030721     C                   EVAL      PiStr=SkSplitCSV(i)
118900030721     C                   EXSR      CHKNUM
119000030721     C                   IF        PiNum=*on
119100030721     C                   Z-ADD     PiVal         VABIAS
119200030721     C                   ELSE
119300030721     C                   SETON                                        32
119400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119500030721     C                             + ' ' + 'VABIAS'
119600030721     C                   ENDIF
119700030721     C                   ENDIF
119800030721     C*** VLB
119900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
120000030721     C                   EVAL      PiStr=SkSplitCSV(i)
120100030721     C                   EXSR      CHKNUM
120200030721     C                   IF        PiNum=*on
120300081007     C  N40              Z-ADD     PiVal         VABVLB
120400081007     C   40              ADD       PiVal         VABVLB
120500030721     C                   ELSE
120600030721     C                   SETON                                        32
120700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120800030721     C                             + ' ' + 'VABVLB'
120900030721     C                   ENDIF
121000030721     C                   ENDIF
121100030721     C*** QFT
121200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121300030721     C                   EVAL      PiStr=SkSplitCSV(i)
121400030721     C                   EXSR      CHKNUM
121500030721     C                   IF        PiNum=*on
121600081007     C  N40              Z-ADD     PiVal         VABQFT
121700081007     C   40              ADD       PiVal         VABQFT
121800030721     C                   ELSE
121900030721     C                   SETON                                        32
122000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122100030721     C                             + ' ' + 'VABQFT'
122200030721     C                   ENDIF
122300030721     C                   ENDIF
122400030721     C*** NCD
122500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122600030721     C                   EVAL      PiStr=SkSplitCSV(i)
122700030721     C                   EXSR      CHKNUM
122800030721     C                   IF        PiInt=*on      AND
122900030721     C                             PiVal<=9999999
123000030721     C                   Z-ADD     PiVal         VABNCD
123100030721     C                   ELSE
123200030721     C                   SETON                                        32
123300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123400030721     C                             + ' ' + 'VABNCD'
123500030721     C                   ENDIF
123600030721     C                   ENDIF
123700030721     C*** NCA
123800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123900030721     C                   EVAL      PiStr=SkSplitCSV(i)
124000030721     C                   EXSR      CHKNUM
124100030721     C                   IF        PiInt=*on      AND
124200030721     C                             PiVal<=9999999
124300030721     C                   Z-ADD     PiVal         VABNCA
124400030721     C                   ELSE
124500030721     C                   SETON                                        32
124600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124700030721     C                             + ' ' + 'VABNCA'
124800030721     C                   ENDIF
124900030721     C                   ENDIF
125000030721     C*** ZNC
125100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125200030721     C                   EVAL      PiStr=SkSplitCSV(i)
125300030721     C                   EXSR      CHKNUM
125400030721     C                   IF        PiInt=*on AND
125500030721     C                             PiVal<=99
125600030721     C                   Z-ADD     PiVal         VABZNC
125700030721     C                   ELSE
125800030721     C                   SETON                                        32
125900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126000030721     C                             + ' ' + 'VABZNC'
126100030721     C                   ENDIF
126200030721     C                   ENDIF
126300030721     C*** DCR
126400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126500030721     C                   EVAL      PiStr=SkSplitCSV(i)
126600030721     C                   EXSR      CHKNUM
126700030721     C                   IF        PiInt=*on       AND
126800030721     C                             PiVal<=99999999
126900030721     C                   Z-ADD     PiVal         VABDCR
127000030721     C                   ELSE
127100030721     C                   SETON                                        32
127200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127300030721     C                             + ' ' + 'VABDCR'
127400030721     C                   ENDIF
127500030721     C                   ENDIF
127600030721     C*** HCR
127700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127800030721     C                   EVAL      PiStr=SkSplitCSV(i)
127900030721     C                   EXSR      CHKNUM
128000030721     C                   IF        PiInt=*on   AND
128100030721     C                             PiVal<=9999
128200030721     C                   Z-ADD     PiVal         VABHCR
128300030721     C                   ELSE
128400030721     C                   SETON                                        32
128500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128600030721     C                             + ' ' + 'VABHCR'
128700030721     C                   ENDIF
128800030721     C                   ENDIF
128900030721     C*** VMD
129000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
129100030721     C                   EVAL      PiStr=SkSplitCSV(i)
129200030721     C                   EXSR      CHKNUM
129300030721     C                   IF        PiNum=*on
129400030721     C                   Z-ADD     PiVal         VABVMD
129500030721     C                   ELSE
129600030721     C                   SETON                                        32
129700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129800030721     C                             + ' ' + 'VABVMD'
129900030721     C                   ENDIF
130000030721     C                   ENDIF
130100030721     C*** ANT
130200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130300030721     C                   EVAL      PiStr=SkSplitCSV(i)
130400030721     C                   EXSR      CHKNUM
130500030721     C                   IF        PiInt=*on        AND
130600030721     C                             PiVal<=999999999
130700030721     C                   Z-ADD     PiVal         VABANT
130800030721     C                   ELSE
130900030721     C                   SETON                                        32
131000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131100030721     C                             + ' ' + 'VABANT'
131200030721     C                   ENDIF
131300030721     C                   ENDIF
131400030721     C*** CMO
131500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131600030721     C                   EVAL      PiStr=SkSplitCSV(i)
131700030721     C                   EXSR      CHKNUM
131800030721     C                   IF        PiInt=*on
131900030721     C                   Z-ADD     PiVal         Num5_0
132000030721     C                   MOVEL(p)  Num5_0        VABCMO
132100030721     C                   ELSE
132200030721     C                   SETON                                        32
132300030721     C                   EVAL      VABCMO = *zeros
132400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132500030721     C                             + ' ' + 'VABCMO'
132600030721     C                   ENDIF
132700030721     C                   ENDIF
132800040510     C*
132900040510     C* Reperisco i campi del FIVAT
133000040510     C*** VATNOT
133100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133300040510     C                   ENDIF
133400040510     C*** VATTRC
133500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133700040510     C                   ENDIF
133800060331     C*** VATNOT_A
133900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
134000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
134100060331     C                   ENDIF
134200060331     C*** VATNOT_B
134300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134500060331     C                   ENDIF
134600060331     C*** VATNOT_E
134700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134900060331     C                   ENDIF
135000110912     C*** VATNOT_I
135100110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
135200110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
135300110912     C                   ENDIF
135400110912     C*** VATNOT_J
135500110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
135600110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
135700110912     C                   ENDIF
135800110912     C*** VATNOT_4
135900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
136000110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
136100110912     C                   ENDIF
136200130430     C*
136300130430     C********** GESTIONE CAMPI PERSONALIZZATI
136400130430     C*
136500130430     C*** PRD
136600130430     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
136700130430     C                   EVAL      VABPRD = *blanks
136800130430     C                   ENDIF
136900130430     C*
137000130430     C*** VATNOT_E / RMA / RMN / NSP
137100130430     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
137200130523     C                   EVAL      wVATNOT_E = '0'+%trim(SkSplitCSV(i))
137300130523     C                   EVAL      VABRMA = wVATNOT_E
137400130523     C                   EVAL      PiStr  = wVATNOT_E
137500130430     C                   EXSR      CHKNUM
137600130430     C                   IF        PiInt=*on
137700130430     C                   Z-ADD     PiVal         VABNSP
137800130430     C                   Z-ADD     PiVal         VABRMN
137900130430     C                   ELSE
138000130430     C                   SETON                                        32
138100130430     C                   EVAL      vinmsg = %trimr(vinmsg)
138200130430     C                             + ' ' + 'VABNSP VABRMN'
138300130430     C                   ENDIF
138400130430     C                   ENDIF
138500130430     C*
138600130430     C****************************************
138700130430     C*
138800040510     C*
138900030715     C                   ENDIF
139000030715     C                   EVAL      i = i + 1
139100030715     C                   ENDDO
139200040510     C*
139300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
139400040510     C                   IF        VABNSP = *zeros
139500040510     C                   SETON                                        31
139600040510     C                   ENDIF
139700040715     C*
139800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
139900040715     C                   IF        VABLNP = *zeros
140000040715     C                   SETON                                        31
140100040715     C                   ENDIF
140200040510     C*
140300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
140400040510     C                   IF        VABRMN = *zeros
140500040510     C                   Z-ADD     VABNSP        VABRMN
140600040510     C                   ENDIF
140700040510     C*
140800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
140900040510     C                   IF        VABRMA = *blanks
141000040714     C                   MOVEL     VABRMN        VABRMA
141100040510     C                   ENDIF
141200070705     C*
141300070705     C* Considerazioni particolari
141400070705     C*** NZD
141500070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
141600070705     C                   EVAL      VABNZD = *blanks
141700070705     C                   ENDIF
141800070705     C*** CAD
141900070705     C                   IF        VABNZD = *blanks
142000070705     C                   EVAL      PiStr=VABCAD
142100070705     C                   EXSR      CHKNUM
142200070705     C                   IF        PiInt=*on
142300070705     C                   Z-ADD     PiVal         Num5_0
142400070705     C                   MOVEL(p)  Num5_0        VABCAD
142500070705     C                   ELSE
142600070705     C                   SETON                                        32
142700070705     C                   EVAL      VABCAD = *zeros
142800070705     C                   EVAL      vinmsg = %trimr(vinmsg)
142900070705     C                             + ' ' + 'VABCAD'
143000070705     C                   ENDIF
143100070705     C                   ENDIF
143200040128     C*
143300040510     C* Se provincia nn valorizzata la reperisco
143400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
143500040510     C                   IF        VABPRD = *blanks
143600040128     C                   CLEAR                   TISI95DS
143700040128     C                   EVAL      I95TCN = '3'
143800040128     C                   Z-ADD     datcor        I95DAT
143900040128     C                   EVAL      I95NAR = VABNZD
144000040128     C                   EVAL      I95CAP = VABCAD
144100040128     C                   EVAL      I95LOC = VABLOD
144200040128     C                   CALL      'TISI95R'
144300040128     C                   PARM                    TISI95DS
144400040128     C                   EVAL      VABPRD = O95PRV
144500040510     C                   ENDIF
144600040113     C*
144700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
144800040113     C                   IF        VABCAS > *zeros
144900040119     C                   EVAL      VABVCA = 'EUR'
145000040119     C                   IF        VABCBO = '1'
145100040113     C                   EVAL      VABCBO = '4'
145200040119     C                   ENDIF
145300040119     C                   IF        VABCBO = '2'
145400040119     C                   EVAL      VABCBO = '6'
145500040119     C                   ENDIF
145600040113     C                   ENDIF
145700020204     C*
145800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
145900020204     C                   EXSR      CHKIMPDIV
146000030715     C                   ENDIF
146100020204     C*
146200010607     C                   ENDSR
146300010607     C*----------------------------------------------------*
146400010601
146500020204
146600020204     C*----------------------------------------------------*
146700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
146800020204     C*----------------------------------------------------*
146900020204     C     CHKIMPDIV     BEGSR
147000020204     C*
147100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
147200020204     C                   Z-ADD     *zeros        wrkDec            9 9
147300020204     C*
147400020204     C* Come prima cosa effettuo considerazioni sulla divisa
147500020204     C                   IF        vabIAS > *zeros
147600020204     C                   IF        vabVAS <> 'EUR'
147700020204     C                   EVAL      vabVAS =  'ITL'
147800020204     C                   ENDIF
147900020204     C                   ENDIF
148000020204     C*
148100020204     C                   IF        vabCAS > *zeros
148200020204     C                   IF        vabVCA <> 'EUR'
148300020204     C                   EVAL      vabVCA =  'ITL'
148400020204     C                   ENDIF
148500020204     C                   ENDIF
148600020204     C*
148700020204     C                   IF        vabVMD > *zeros
148800020204     C                   IF        vabVAD <> 'EUR'
148900020204     C                   EVAL      vabVAD =  'ITL'
149000020204     C                   ENDIF
149100020204     C                   ENDIF
149200020204     C*
149300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
149400020204     C                   Z-ADD     vabIAS        wrkDec
149500020204     C                   IF        wrkDec > *zeros
149600020204     C                   IF        vabVAS = 'ITL'
149700020204     C                   EVAL      vabIAS = *zeros
149800020204     C                   ENDIF
149900020204     C                   ENDIF
150000020204     C*
150100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
150200020204     C                   Z-ADD     vabCAS        wrkDec
150300020204     C                   IF        wrkDec > *zeros
150400020204     C                   IF        vabVCA = 'ITL'
150500020204     C                   EVAL      vabCAS = *zeros
150600020204     C                   ENDIF
150700020204     C                   ENDIF
150800020204     C*
150900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151000020204     C                   Z-ADD     vabVMD        wrkDec
151100020204     C                   IF        wrkDec > *zeros
151200020204     C                   IF        vabVAD = 'ITL'
151300020204     C                   EVAL      vabVMD = *zeros
151400020204     C                   ENDIF
151500020204     C                   ENDIF
151600020204     C*
151700020204     C                   ENDSR
151800020204     C***
151900020204
152000010330
152100010330
152200010330     C*----------------------------------------------------*
152300010330     C*  CONTROLLO NUMERICITA' CAMPI
152400010330     C*----------------------------------------------------*
152500010330     C     CHKNUM        BEGSR
152600010330     C*
152700010606     C                   IF        PiDecChr = *blanks
152800030715     C                   EVAL      PiDecChr = CharNUM
152900010606     C                   ENDIF
153000010606     C*
153100010606     C                   CALL(e)   'ISNUMERIC'
153200010330     C                   PARM                    PiStr            30
153300010606     C                   PARM                    PiDecChr          1
153400010330     C                   PARM      *ZEROS        PiVal            30 9
153500010330     C                   PARM      '0'           PiInt             1
153600010330     C                   PARM      '0'           PiNum             1
153700010330     C                   IF        %error
153800010606     C                   EVAL      PiNum=*off
153900010330     C                   ENDIF
154000010330     C*
154100010330     C                   ENDSR
154200010330     C***
154300010330
154400010601
154500010601
154600010601
154700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
154800010601     C     repfil        BEGSR
154900010601     C*
155000010601     C                   if        invfil = *zeros and
155100010601     C                             depfil > *zeros and
155200010629     C                             (vinflg = *blanks or
155300010629     C                              vinflg = *zeros)
155400010601     C
155500010601     C                   eval      invfil = depfil
155600010601     C                   endif
155700010601     C*
155800010601     C                   if        depfil <> invfil and
155900010601     C                             invfil > *zeros
156000010601     C                   eval      flgMulti = '1'
156100010601     C                   if        vinflg = *blanks
156200010601     C                   add       1             cntNonEl
156300010601     C                   endif
156400010601     C                   endif
156500010601     C*
156600010601     C                   if        vinflg = '2'
156700010601     C                   eval      flgStato = '2'
156800010601     C                   endif
156900010601     C*
157000010601     C                   ENDSR
157100010601     C***
157200010601
157300010601
157400010601
157500010330
157600040119      /TITLE Invio dei dati al punto operativo.
157700040119     C     invio         BEGSR
157800040119     C*
157900040119     C* 1° invio FIVAT
158000040119     C                   reset                   dscmz
158100070704     C                   move      winvcmz       cmzdst
158200040119     C                   eval      cmzfld = 'FIVATWWR'
158300040119     C                   eval      cmzmbd = vlrhdl
158400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158500040119     C***                if        prmfir = *blanks
158600040119     C                   eval      cmzfla = 'FIVAT00F'
158700040119     C                   eval      cmzmba = 'FIVAT00F'
158800040119     C***                else
158900040119     C***                eval      cmzfla = prmfir
159000040119     C***                eval      cmzmba = prmfir
159100040119     C***                endif
159200040119     C                   eval      cmznrr = *zeros
159300040119     C                   move      §ctrokvt      cmznrr
159400040119     C                   eval      cmzlba = vlrfl1
159500040119     C                   call(e)   'TIS711C'
159600040119     C                   parm                    dscmz
159700040119     C                   parm      *blanks       esito
159800040119     C                   if        %error
159900040119     C                             or cmzerr = '1'
160000040119     C                             or esito  = '1'
160100040119     C                   eval      wrkesito = '3'
160200040119     C                   else
160300040119     C*
160400040119     C* 2° invio FIVAB
160500040119     C                   reset                   dscmz
160600070704     C                   move      winvcmz       cmzdst
160700040119     C                   eval      cmzfld = vlrfou
160800040119     C                   eval      cmzmbd = vlrhdl
160900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161000040119     C***                if        prmfir = *blanks
161100040119     C                   eval      cmzfla = 'FIVAB00F'
161200040119     C                   eval      cmzmba = 'FIVAB00F'
161300040119     C***                else
161400040119     C***                eval      cmzfla = prmfir
161500040119     C***                eval      cmzmba = prmfir
161600040119     C***                endif
161700040119     C                   eval      cmznrr = *zeros
161800040119     C                   move      §ctrokvb      cmznrr
161900040119     C                   eval      cmzlba = vlrfl1
162000040119     C                   call(e)   'TIS711C'
162100040119     C                   parm                    dscmz
162200040119     C                   parm      *blanks       esito
162300040119     C                   if        %error
162400040119     C                             or cmzerr = '1'
162500040119     C                             or esito  = '1'
162600040119     C                   eval      wrkesito = '3'
162700040119     C                   endif
162800040119     C                   endif
162900040119     C*
163000040119     C                   ENDSR
163100040119     C***
163200010601
163300010601
163400010601
163500010601
163600010601      /TITLE Invio dei dati al punto operativo.
163700010601     C     opeini        BEGSR
163800010601     C*
163900010601     C* Inizializzo flag e contatori operativi
164000010601     C                   movel     '0'           flgGiro           1
164100010601     C                   movel     '0'           flgMulti          1
164200010601     C                   movel     '1'           flgStato          1
164300010615     C                   movel     '0'           flgOk             1
164400010601     C                   z-add     *zeros        cntNonEl         10 0
164500010601     C                   z-add     *zeros        depfil            3 0
164600010601     C                   z-add     *zeros        invfil            3 0
164700010601     C*
164800010601     C                   ENDSR
164900010601     C***
165000070326
165100070326
165200070326
165300070326
165400070326     C     *pssr         BEGSR
165500070329     C*
165600070329     C                   if        %open(tivin00r)
165700070329     C                   close     tivin00r
165800070329     C                   endif
165900070329     C                   if        %open(fivabwwr)
166000070329     C                   close     fivabwwr
166100070329     C                   endif
166200070329     C                   if        %open(fivatwwr)
166300070329     C                   close     fivatwwr
166400070329     C                   endif
166500070326     C*
166600070326     C* Effettuo la chiamata al CLLE preposto
166700070326     C                   call(e)   'TITVVTC'
166800070326     C                   parm                    parccm
166900070326     C                   parm                    parmbr
167000070326     C                   parm      '2'           paropz
167100070326     C*
167200070326     C                   eval      wrkesito = '2'
167300070404     C*
167400070404     C                   seton                                        LR
167500070326     C*
167600070326     C                   ENDSR     '*CANCL'
167700070326     C***
167800070326
167900070326
168000010330
168100010330
168200000613     C     *inzsr        BEGSR
168300990910     C*
168400990910     C     *entry        plist
168500990920     C                   parm                    tivlrds
168600990921     C                   parm      wrkesito      esito
168700000724     C                   parm                    prmlit
168800000710     C                   parm                    prmfir
168900010330     C*
169000010330     C* CALCOLA LA DATA CORRENTE
169100010330     C                   time                    wn14             14 0
169200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
169300010330     C                   z-add     wn8           g08dat
169400010330     C                   z-add     *zeros        g08inv
169500010330     C                   movel     '0'           g08err
169600010330     C                   call      'XSRDA8'
169700010330     C                   parm                    wlbda8
169800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
169900000613     C*
170000000613     C                   ENDSR
170100000613     C***
170200040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170300040510
170400040122
170500040113
