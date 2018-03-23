000100040119      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209     F*
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
011900010601
012000010601
012100010330     C*--------------------------------------------------------
012200021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012300010330     C*--------------------------------------------------------
012400010605     C     RWVAB         BEGSR
012500010330     C*
012600010330     C                   if        not %open(tivin00r)
012700010330     C                   open      tivin00r
012800010330     C                   endif
012900021025     C                   if        not %open(fivabwwr)
013000021025     C                   open      fivabwwr
013100010330     C                   endif
013200040119     C*
013300040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013400040119     C                   exsr      prevat
013500040119     C*
013600040119     C                   if        chkcall = '0'
013700040119     C*
013800040119     C                   if        not %open(fivatwwr)
013900040119     C                   open      fivatwwr
014000040119     C                   endif
014100010330     C*
014200040119     C                   clear                   §CTROKVB          7 0
014300040119     C                   clear                   §CTROKVT          7 0
014400040119     C                   clear                   §CTRMO            7 0
014500040119     C                   clear                   §CTRNO            7 0
014600010330     C*
014700030822     C                   do        *HIVAL
014800010330     C*
014900030822     C                   read      tivin00r                               70
015000010618     C*
015100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015200010618     C                   if        vinflg = '1'
015300010618     C                   eval      flgOk = '1'
015400010618     C                   endif
015500040510     C*
015600040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015700040510     C                   z-add     *zeros        wLenVINDTA        4 0
015800040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015900090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
016000040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016100090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016200040510     C                   z-add     1             wFlgVINDTA
016300040510     C                   leave
016400040510     C                   endif
016500040510     C                   enddo
016600010618     C*
016700040510     C                   if        vindta > *blanks AND
016800040510     C                             wFlgVINDTA = 1
016900010330     C                   add       1             rrnum
017000010330     C*
017100010601     C                   if        *in70 = *off and
017200010330     C                             (vinflg = *blanks
017300010330     C                              or vinflg = '0'
017400010330     C                              or vinflg = '2')
017500010330     C*
017600010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017700010711     C                   if        vinflg = *blanks or vinflg = '0'
017800010711     C                   clear                   vinmsg
017900010711     C                   endif
018000010601     C*
018100081014     C                   exsr      inzvar
018200090324     C*
018300090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
018400090324     C* separatori campi
018500090324     C                   if        CharTXT <> *blanks
018600090324     C                   z-add     *zeros        wLenVINDTA        4 0
018700090324     C                   z-add     *zeros        wFlgVINDTA        1 0
018800090324     C                   setoff                                       80
018900090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
019000090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
019100090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019200090324     C                   if        not *in80
019300090324     C                   seton                                        80
019400090324     C                   else
019500090324     C                   setoff                                       80
019600090324     C                   endif
019700090324     C                   endif
019800090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
019900090324     C                             and *in80 = *on
020000090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020100090324     C                   endif
020200090324     C                   enddo
020300090324     C                   endif
020400090324     C*
020500010605     C                   exsr      impvab
020600010601     C*
020700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020800010605     C                   eval      depfil = VABLNP
020900010601     C                   exsr      repfil
021000010601     C                   if        depfil = invfil
021100021025     C                   if        vlrpoi = 999
021200030822     C                   move(P)   invfil        VABFGS
021300070704     C                   move(P)   invfil        winvcmz           3
021400021025     C                   else
021500030822     C                   move(P)   vlrpoi        VABFGS
021600070704     C                   move(P)   vlrpoi        winvcmz
021700021025     C                   endif
021800010601     C*
021900010601     C                   exsr      PREELA
022000010601     C*
022100010604     C* Ebbene...
022200010604     C*
022300040119     C  N31              add       1             §CTROKVB
022400040119     C   32              add       1             §CTRMO
022500040119     C   31              add       1             §CTRNO
022600030822     C                   if        wGiro = 2
022700040510     C* Scrivo sempre il VAT
022800040510     C  N31              exsr      wrivat
022900040510     C* Scrivo il VAB solo a rottura d spedizione
023000040119     C                   if        depspe =  *zeros OR
023100040119     C                             depspe <> vabNSP
023200040119     C  N31              exsr      wrivab
023300040119     C  N31              eval      depspe = vabNSP
023400040119     C                   endif
023500040119     C                   endif
023600010604     C*
023700010604     C                   if        *in31 = *off and
023800010604     C                             *in32 = *off
023900010604     C                   eval      vinflg = '1'
024000010604     C                   else
024100010604     C                   eval      vinflg = '2'
024200010604     C                   endif
024300010604     C                   endif
024400010601     C*
024500010604     C                   endif
024600010604     C*
024700010330     C                   else
024800010330     C                   eval      vinflg = '1'
024900010330     C                   endif
025000010601     C*
025100010601     C  N70              update    tivin000
025200010330     C*
025300030822     C  N70              enddo
025400040119     C*
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
028400021025     C                   if        %open(fivabwwr)
028500021025     C                   close     fivabwwr
028600010330     C                   endif
028700040512     C                   if        %open(fivatwwr)
028800040512     C                   close     fivatwwr
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
030600040119
030700040119
030800040119
030900040119     C*----------------------------------------------------*
031000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031100040119     C*----------------------------------------------------*
031200040510     C     WRIVAB        BEGSR
031300040119     C*
031400060331     C* Gestisco l'eventuale rottura x numero spedizione
031500060331     C                   if        VABNSP <> wVABNSP
031600070301     C                   if        wVATNOT_E <> *blanks
031700110419     C                   if        VABCTM = *blanks
031800070301     C                   eval      VABCTM = '7Q'
031900110419     C                   endif
032000070301     C                   endif
032100040119     C                   write     fivab000                                     => scarico il VAB
032200060331     C                   eval      wVABNSP = VABNSP
032300060331     C                   endif
032400040119     C*
032500040119     C                   ENDSR
032600040119
032700040119
032800040119
032900040119     C*----------------------------------------------------*
033000040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033100040119     C*----------------------------------------------------*
033200040510     C     WRIVAT        BEGSR
033300060512     C*
033400060512     C* Inizializzo i campi chiave
033500060512     C                   eval      VATFGS = VABFGS
033600060512     C                   eval      VATAAS = VABAAS
033700060512     C                   eval      VATCCM = VABCCM
033800060512     C                   eval      VATNSP = VABNSP
033900060512     C                   eval      VATLNP = VABLNP
034000040714     C*
034100040714     C* Solo se VATNOT valorizzato scarico il buffer
034200040714     C                   if        VATNOT <> *blanks
034300040119     C*
034400040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
034500040510     C                   if        VATTRC = *blanks
034600040510     C                   eval      VATTRC = 'E'
034700040510     C                   endif
034800040510     C                   write     FIVAT000
034900040119     C                   add       1             §CTROKVT
035000040714     C*
035100040714     C                   endif
035200060331     C*
035300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035400060331     C                   if        wVATNOT_A <> *blanks
035500060331     C                   eval      VATNOT = wVATNOT_A
035600060331     C                   eval      VATTRC = 'A'
035700060331     C                   write     FIVAT000
035800060331     C                   add       1             §CTROKVT
035900060331     C                   endif
036000060331     C*
036100060331     C                   if        wVATNOT_B <> *blanks
036200060331     C                   eval      VATNOT = wVATNOT_B
036300060331     C                   eval      VATTRC = 'B'
036400060331     C                   write     FIVAT000
036500060331     C                   add       1             §CTROKVT
036600060331     C                   endif
036700060331     C*
036800060331     C                   if        wVATNOT_E <> *blanks
036900060331     C                   eval      VATNOT = wVATNOT_E
037000060331     C                   eval      VATTRC = 'E'
037100060331     C                   write     FIVAT000
037200060331     C                   add       1             §CTROKVT
037300060331     C                   endif
037400110912     C*
037500110912     C                   if        wVATNOT_I <> *blanks
037600110912     C                   eval      VATNOT = wVATNOT_I
037700110912     C                   eval      VATTRC = 'I'
037800110912     C                   write     FIVAT000
037900110912     C                   add       1             §CTROKVT
038000110912     C                   endif
038100110912     C*
038200110912     C                   if        wVATNOT_J <> *blanks
038300110912     C                   eval      VATNOT = wVATNOT_J
038400110912     C                   eval      VATTRC = 'J'
038500110912     C                   write     FIVAT000
038600110912     C                   add       1             §CTROKVT
038700110912     C                   endif
038800110912     C*
038900110912     C                   if        wVATNOT_4 <> *blanks
039000110912     C                   eval      VATNOT = wVATNOT_4
039100110912     C                   eval      VATTRC = '4'
039200110912     C                   write     FIVAT000
039300110912     C                   add       1             §CTROKVT
039400110912     C                   endif
039500040119     C*
039600040119     C                   ENDSR
039700040119
039800040119
039900040119
040000040119     C*----------------------------------------------------*
040100040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
040200040119     C*----------------------------------------------------*
040300040119     C     PREVAT        BEGSR
040400040119     C*
040500040119     C* Compongo il nome del membro da dare al FIVATWWR
040600040119     C                   eval      parmbr = vlrhdl
040700040119     C                   movel     'M'           parmbr
040800040510     C                   eval      parccm = vlrksc
040900040119     C                   eval      paropz = '1'
041000040119     C* Effettuo la chiamata al CLLE preposto
041100040119     C                   call(e)   'TITVVTC'
041200040119     C                   parm                    parccm
041300040119     C                   parm                    parmbr
041400040119     C                   parm                    paropz
041500040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
041600040119     C                   if        %error
041700040119     C                   movel     '1'           chkcall
041800040119     C                   else
041900040119     C                   movel     '0'           chkcall
042000040119     C                   endif
042100040119     C*
042200040119     C                   ENDSR
042300010601
042400010601
042500010601
042600010330     C*----------------------------------------------------*
042700030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042800010330     C*----------------------------------------------------*
042900040119     C     INZVAR        BEGSR
043000010330     C*
043100030822     C* Inizializzo variabili di wrk
043200020204     C                   Z-ADD     *zeros        Num5_0            5 0
043300090403     C                   z-add     *zeros        invfil
043400060331     C                   MOVEL     *blanks       wVATNOT_A
043500060331     C                   MOVEL     *blanks       wVATNOT_B
043600060331     C                   MOVEL     *blanks       wVATNOT_E
043700110912     C                   MOVEL     *blanks       wVATNOT_I
043800110912     C                   MOVEL     *blanks       wVATNOT_J
043900110912     C                   MOVEL     *blanks       wVATNOT_4
044000081014     C*
044100081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
044200081014     C                   CLEAR                   FIVAB000
044300081014     C                   CLEAR                   FIVAT000
044400030822     C*
044500081014     C* Inizializzo la schiera d wrk x i dati
044600030822     C                   CLEAR                   SkSplitCSV
044700030822     C                   EVAL      i = 1
044800030822     C                   EVAL      posDa = *zeros
044900030822     C                   EVAL      posA  = *zeros
045000030822     C*
045100030822     C* Reimposto i valori di default
045200030822     C                   EXSR      DEFCAM
045300030822     C*
045400010330     C                   ENDSR
045500010330     C*----------------------------------------------------*
045600030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
045700010330     C*----------------------------------------------------*
045800010330     C     DEFCAM        BEGSR
045900030822     C*
046000030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
046100030715     C* e delimitatore testo.
046200030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
046300030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
046400030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
046500090324     C*
046600090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
046700090324     C                   EVAL      CharSOS = CharNUM
046800030822     C*
046900081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
047000030715     C                   IF        %subst(vlrppt:1:1) = 'S'
047100081007     C                   SETON                                        40
047200030715     C                   ELSE
047300081007     C                   SETOFF                                       40
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
063900030822     C* GCA
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'GCA'
064400030822     C                   EVAL      VABGCA=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* CTM
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'CTM'
065300030822     C                   EVAL      VABCTM=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* FFD
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'FFD'
066200030822     C                   EVAL      VABFFD=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* VAD
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'VAD'
067100030822     C                   EVAL      VABVAD=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* GMA
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'GMA'
068000030822     C                   EVAL      VABGMA=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* GGA
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'GGA'
068900030822     C                   EVAL      VABGGA=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* GVA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GVA'
069800030822     C                   EVAL      VABGVA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* TC1
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'TC1'
070700030822     C                   EVAL      VABTC1=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* TC2
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'TC2'
071600030822     C                   EVAL      VABTC2=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000080516     C* TIC
072100080516     C                   IF        %subst(
072200080516     C                             %subst(vlrppt:posDaDft+1:
072300080516     C                             posADft-posDaDft-1):1:3)
072400080516     C                             = 'TIC'
072500080516     C                   EVAL      VABTIC=%trim(%subst(
072600080516     C                             %subst(vlrppt:posDaDft+1:
072700080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800080516     C                   ENDIF
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
074400021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
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
075700040113     C*
075800040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
075900040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
076000040510     C                             %trim(intcol(3))<>*blanks
076100040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
076200040113     C                                      %trim(intcol(3))
076300040510     C                   ENDIF
076400031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
076500031201     C     minu:maiu     XLATE     vindta        vindta
076600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
076700030715     C                   DOW       posDa <= %len(%trim(vindta))
076800030715     C*
076900030715     C* Gestisco il 1° campo
077000030715     C                   IF        i = 1
077100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
077200030715     C                   EVAL      posDa = 1
077300030715     C* Eseguo lo scan x trovare la fine del primo campo
077400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
077500030715     C* A questo punto "estraggo" il campo corrente
077600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
077700030715     C                                                    (posA-posDa))
077800030715     C* X i campi successivi al 1°
077900030715     C                   ELSE
078000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
078100030715     C                   EVAL      posDa = posA + 1
078200030715     C* Eseguo lo scan x trovare la fine del campo corrente
078300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
078400030715     C* Gestisco l'ultimo campo
078500030715     C                   IF        posA = *zeros
078600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
078700030715     C* A questo punto "estraggo" il campo corrente
078800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
078900030715     C                                                    (posA-posDa))
079000030715     C                   LEAVE
079100030715     C                   ELSE
079200030715     C* A questo punto "estraggo" il campo corrente
079300030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079400030715     C                                                    (posA-posDa))
079500030715     C                   ENDIF
079600030715     C                   ENDIF
079700030715     C* Incremento il contatore d campo
079800030715     C                   EVAL      i = i +1
079900030715     C                   ENDDO
080000030715     C*
080100030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
080200030715     C* delimitazione testo
080300030715     C                   EVAL      i = 1
080400030715     C                   DOW       i < %elem(SkSplitFLD)
080500030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
080600030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
080700030715     C                   EVAL      i = i + 1
080800030715     C                   ENDDO
080900030715     C                   EVAL      wGiro = 1
081000030715     C                   ELSE
081100030715     C                   EVAL      wGiro = 2
081200031204     C**********
081300031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
081400031204     C* d separatore campo
081500031204     C**********
081600031204     C                   EVAL      vindta = %trim(vindta)
081700031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
081800031204     C                   EVAL      vindta = %subst(vindta:2)
081900031204     C                   ENDDO
082000031204     C*
082100031204     C                   Z-ADD     *zeros        lunghInput        4 0
082200031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082300031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
082400031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
082500031204     C                   EVAL      lunghInput = %len(%trim(vindta))
082600031204     C                   ENDDO
082700030715     C**********
082800030715     C* Effettuo lo split del campo dati d input
082900030715     C**********
083000030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083100030715     C                   DOW       posDa <= %len(%trim(vindta))
083200030715     C*
083300030715     C* Gestisco il 1° campo
083400030715     C                   IF        i = 1
083500030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
083600030715     C                   EVAL      posDa = 1
083700030715     C* Eseguo lo scan x trovare la fine del primo campo
083800030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C* X i campi successivi al 1°
084300030715     C                   ELSE
084400030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
084500030715     C                   EVAL      posDa = posA + 1
084600031204     C* Verifico che nn vi sia il campo nn valorizzato
084700031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
084800031204     C* Se campo nn valorizzato skippo al prossimo
084900031204     C                   EVAL      SkSplitCSV(i) = *blanks
085000031204     C                   EVAL      posA  = posA + 1
085100031204     C                   ELSE
085200030715     C* Eseguo lo scan x trovare la fine del campo corrente
085300030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085400030715     C* Gestisco l'ultimo campo
085500030715     C                   IF        posA = *zeros
085600030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C                   LEAVE
086100030715     C                   ELSE
086200030715     C* A questo punto "estraggo" il campo corrente
086300030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086400030715     C                                                    (posA-posDa))
086500030715     C                   ENDIF
086600031204     C                   ENDIF
086700030715     C                   ENDIF
086800030715     C* Incremento il contatore d campo
086900030715     C                   EVAL      i = i +1
087000030715     C                   ENDDO
087100030715     C*
087200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087300030715     C* delimitazione testo
087400030715     C                   EVAL      i = 1
087500030715     C                   DOW       i < %elem(SkSplitCSV)
087600030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
087700030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
087800030715     C                   EVAL      i = i + 1
087900030715     C                   ENDDO
088000030715     C*
088100030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088200030715     C                   EVAL      i = 1
088300030715     C                   DOW       i < %elem(SkSplitFLD)
088400030715     C*
088500030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
088600030715     C                   IF        SkSplitFLD(i) <> *blanks AND
088700030715     C                             SkSplitCSV(i) <> *blanks
088800030721     C*** ATB
088900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089000030721     C                   EVAL      VABATB = SkSplitCSV(i)
089100030721     C                   ENDIF
089200030715     C*** RSD
089300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
089400120130     C**!!!              EVAL      VABRSD = SkSplitCSV(i)
089500120130     C                   MOVEL     *blanks       wRSD             70
089600120130     C                   EVAL      wRSD=SkSplitCSV(i)
089700120130     C                   EVAL      VABRSD=%subst(wRSD:1:35)
089800120130     C                   EVAL      VABRD2=%subst(wRSD:36:35)
089900030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090000030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090100030715     C* ==
090200030715     C                   ENDIF
090300030721     C*** RD2
090400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090500030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090600030721     C                   ENDIF
090700030715     C*** IND
090800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
090900030715     C                   EVAL      VABIND = SkSplitCSV(i)
091000030715     C                   ENDIF
091100030715     C*** LOD
091200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091300030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091400030715     C                   ENDIF
091500030715     C*** PRD
091600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091700030715     C                   EVAL      VABPRD = SkSplitCSV(i)
091800030715     C                   ENDIF
091900030721     C*** NZD
092000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092100030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092200030721     C                   ENDIF
092300030715     C*** RMA
092400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092500030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092600030715     C                   ENDIF
092700030715     C*** NOT
092800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
092900120208     C                   EVAL      VABNOT = %subst(SkSplitCSV(i):1:35)
093000120208     C                   EVAL      VABNT2 = %subst(SkSplitCSV(i):36:35)
093100030715     C                   ENDIF
093200030715     C*** NT2
093300030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093400030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093500030715     C                   ENDIF
093600030715     C*** VCA
093700030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
093800030715     C                   EVAL      VABVCA = SkSplitCSV(i)
093900030715     C                   ENDIF
094000030715     C*** VAS
094100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094200030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094300030715     C                   ENDIF
094400030715     C*** FFD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094600030715     C                   EVAL      VABFFD = SkSplitCSV(i)
094700030715     C                   ENDIF
094800030721     C*** GC1
094900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095000030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095100100304     C                   IF        %subst(VABGC1:1) = 'P' or
095200100304     C                             %subst(VABGC1:1) = 'M'
095300100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
095400100304     C                   ENDIF
095500030721     C                   ENDIF
095600030721     C*** GC2
095700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095800030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
095900100304     C                   IF        %subst(VABGC2:1) = 'P' or
096000100304     C                             %subst(VABGC2:1) = 'M'
096100100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
096200100304     C                   ENDIF
096300030721     C                   ENDIF
096400030721     C*** TSP
096500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096600030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096700030721     C                   ENDIF
096800040510     C*** CBO
096900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097000040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097100040510     C                   ENDIF
097200030721     C*** NAS
097300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097400030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097500030721     C                   ENDIF
097600030721     C*** TIC
097700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097800030721     C                   EVAL      VABTIC = SkSplitCSV(i)
097900030721     C                   ENDIF
098000030721     C*** GCA
098100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098200030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098300030721     C                   ENDIF
098400030721     C*** XCO
098500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098600030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098700030721     C                   ENDIF
098800030721     C*** CTM
098900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099000030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099100030721     C                   ENDIF
099200030721     C*** TCR
099300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099400030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099500030721     C                   ENDIF
099600030721     C*** CTS
099700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099800030721     C                   EVAL      VABCTS = SkSplitCSV(i)
099900030721     C                   ENDIF
100000030721     C*** FTM
100100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100200030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100300030721     C                   ENDIF
100400030721     C*** VAD
100500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100600030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100700030721     C                   ENDIF
100800030721     C*** GMA
100900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101000030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101100030721     C                   ENDIF
101200030721     C*** GGA
101300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101400030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101500030721     C                   ENDIF
101600030721     C*** GVA
101700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101800030721     C                   EVAL      VABGVA = SkSplitCSV(i)
101900030721     C                   ENDIF
102000030721     C*** TC1
102100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102200030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102300030721     C                   ENDIF
102400030721     C*** TC2
102500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102600030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102700030721     C                   ENDIF
102800030721     C*** SCL
102900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103000030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103100030721     C                   ENDIF
103200030721     C*** RMO
103300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103400030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103500030721     C                   ENDIF
103600030721     C*** NMO
103700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103800030721     C                   EVAL      VABNMO = SkSplitCSV(i)
103900030721     C                   ENDIF
104000030721     C*
104100030721     C* Reperisco quindi i campi numerici...
104200040510     C*** NSP
104300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104400040510     C                   EVAL      PiStr=SkSplitCSV(i)
104500040510     C                   EXSR      CHKNUM
104600040510     C                   IF        PiInt=*on
104700040510     C                   Z-ADD     PiVal         VABNSP
104800040510     C                   ELSE
104900040510     C                   SETON                                        31
105000040510     C                   EVAL      VABNSP = *zeros
105100040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105200040510     C                             + ' ' + 'VABNSP'
105300040510     C                   ENDIF
105400040510     C                   ENDIF
105500070703     C*** RMN
105600070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105700070703     C                   EVAL      PiStr=SkSplitCSV(i)
105800070703     C                   EXSR      CHKNUM
105900070703     C                   IF        PiInt=*on
106000070703     C                   Z-ADD     PiVal         VABRMN
106100070703     C                   ELSE
106200070703     C                   SETON                                        32
106300070703     C                   EVAL      vinmsg = %trimr(vinmsg)
106400070703     C                             + ' ' + 'VABRMN'
106500070703     C                   ENDIF
106600070703     C                   ENDIF
106700070705     C*** CAD
106800070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
106900070705     C                   EVAL      VABCAD = SkSplitCSV(i)
107000070705     C                   ENDIF
107100030721     C*** NCL
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107300030721     C                   EVAL      PiStr=SkSplitCSV(i)
107400030721     C                   EXSR      CHKNUM
107500030721     C                   IF        PiInt=*on
107600081007     C  N40              Z-ADD     PiVal         VABNCL
107700081007     C   40              ADD       PiVal         VABNCL
107800030721     C                   ELSE
107900030721     C                   SETON                                        32
108000030721     C                   EVAL      VABNCL = *zeros
108100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108200040122     C                             + ' ' + 'VABNCL'
108300030721     C                   ENDIF
108400030721     C                   ENDIF
108500030721     C*** PKB
108600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108700030721     C                   EVAL      PiStr=SkSplitCSV(i)
108800030721     C                   EXSR      CHKNUM
108900030721     C                   IF        PiNum=*on
109000081007     C  N40              Z-ADD     PiVal         VABPKB
109100081007     C   40              ADD       PiVal         VABPKB
109200030721     C                   ELSE
109300030721     C                   SETON                                        32
109400030721     C                   EVAL      VABPKB = *zeros
109500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109600030721     C                             + ' ' + 'VABPKB'
109700030721     C                   ENDIF
109800030721     C                   ENDIF
109900030721     C*** CAS
110000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110100030721     C                   EVAL      PiStr=SkSplitCSV(i)
110200030721     C                   EXSR      CHKNUM
110300030721     C                   IF        PiNum=*on
110400030721     C                   Z-ADD     PiVal         VABCAS
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABCAS = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABCAS'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** CCM
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiInt=*on      AND
111700030721     C                             PiVal<=9999999 AND
111800030721     C                             PiVal>*zeros
111900030721     C                   Z-ADD     PiVal         VABCCM
112000030721     C                   ELSE
112100030721     C                   SETON                                        32
112200030721     C                   EVAL      VABCCM = *zeros
112300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112400040510     C                             + ' ' + 'VABCCM'
112500030721     C                   ENDIF
112600030721     C                   ENDIF
112700030721     C*** LNP
112800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
112900030721     C                   EVAL      PiStr=SkSplitCSV(i)
113000030721     C                   EXSR      CHKNUM
113100030721     C                   IF        PiInt=*on    AND
113200030721     C                             PiVal<=999   AND
113300030721     C                             PiVal>*zeros
113400030721     C                   Z-ADD     PiVal         VABLNP
113500030721     C                   ELSE
113600040715     C                   SETON                                        32
113700030721     C                   EVAL      VABLNP = *zeros
113800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113900040510     C                             + ' ' + 'VABLNP'
114000030721     C                   ENDIF
114100030721     C                   ENDIF
114200030721     C*** NRS
114300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114400030721     C                   EVAL      PiStr=SkSplitCSV(i)
114500030721     C                   EXSR      CHKNUM
114600030721     C                   IF        PiInt=*on AND
114700030721     C                             PiVal<=99
114800030721     C                   Z-ADD     PiVal         VABNRS
114900030721     C                   ELSE
115000030721     C                   SETON                                        32
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200030721     C                             + ' ' + 'VABNRS'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** LNA
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on    AND
116000030721     C                             PiVal<=999   AND
116100030721     C                             PiVal>*zeros
116200030721     C                   Z-ADD     PiVal         VABLNA
116300030721     C                   ELSE
116400030721     C                   SETON                                        32
116500030721     C                   EVAL      VABLNA = *zeros
116600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116700030721     C                             + ' ' + 'VABLNA'
116800030721     C                   ENDIF
116900030721     C                   ENDIF
117000030721     C*** CTR
117100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117200030721     C                   EVAL      PiStr=SkSplitCSV(i)
117300030721     C                   EXSR      CHKNUM
117400030721     C                   IF        PiInt=*on  AND
117500030721     C                             PiVal<=999
117600030721     C                   Z-ADD     PiVal         VABCTR
117700030721     C                   ELSE
117800030721     C                   SETON                                        32
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABCTR'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** IAS
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiNum=*on
118800030721     C                   Z-ADD     PiVal         VABIAS
118900030721     C                   ELSE
119000030721     C                   SETON                                        32
119100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119200030721     C                             + ' ' + 'VABIAS'
119300030721     C                   ENDIF
119400030721     C                   ENDIF
119500030721     C*** VLB
119600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119700030721     C                   EVAL      PiStr=SkSplitCSV(i)
119800030721     C                   EXSR      CHKNUM
119900030721     C                   IF        PiNum=*on
120000081007     C  N40              Z-ADD     PiVal         VABVLB
120100081007     C   40              ADD       PiVal         VABVLB
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABVLB'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** QFT
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300081007     C  N40              Z-ADD     PiVal         VABQFT
121400081007     C   40              ADD       PiVal         VABQFT
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABQFT'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** NCD
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiInt=*on      AND
122600030721     C                             PiVal<=9999999
122700030721     C                   Z-ADD     PiVal         VABNCD
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABNCD'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** NCA
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on      AND
123900030721     C                             PiVal<=9999999
124000030721     C                   Z-ADD     PiVal         VABNCA
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABNCA'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** ZNC
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on AND
125200030721     C                             PiVal<=99
125300030721     C                   Z-ADD     PiVal         VABZNC
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABZNC'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** DCR
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on       AND
126500030721     C                             PiVal<=99999999
126600030721     C                   Z-ADD     PiVal         VABDCR
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABDCR'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** HCR
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on   AND
127800030721     C                             PiVal<=9999
127900030721     C                   Z-ADD     PiVal         VABHCR
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABHCR'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** VMD
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiNum=*on
129100030721     C                   Z-ADD     PiVal         VABVMD
129200030721     C                   ELSE
129300030721     C                   SETON                                        32
129400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129500030721     C                             + ' ' + 'VABVMD'
129600030721     C                   ENDIF
129700030721     C                   ENDIF
129800030721     C*** ANT
129900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130000030721     C                   EVAL      PiStr=SkSplitCSV(i)
130100030721     C                   EXSR      CHKNUM
130200030721     C                   IF        PiInt=*on        AND
130300030721     C                             PiVal<=999999999
130400030721     C                   Z-ADD     PiVal         VABANT
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABANT'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** CMO
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on
131600030721     C                   Z-ADD     PiVal         Num5_0
131700030721     C                   MOVEL(p)  Num5_0        VABCMO
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      VABCMO = *zeros
132100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132200030721     C                             + ' ' + 'VABCMO'
132300030721     C                   ENDIF
132400030721     C                   ENDIF
132500040510     C*
132600040510     C* Reperisco i campi del FIVAT
132700040510     C*** VATNOT
132800040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
132900040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133000040510     C                   ENDIF
133100040510     C*** VATTRC
133200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133300040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133400040510     C                   ENDIF
133500060331     C*** VATNOT_A
133600060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133700060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
133800060331     C                   ENDIF
133900060331     C*** VATNOT_B
134000060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134100060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134200060331     C                   ENDIF
134300060331     C*** VATNOT_E
134400060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134500060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134600060331     C                   ENDIF
134700110912     C*** VATNOT_I
134800110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
134900110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
135000110912     C                   ENDIF
135100110912     C*** VATNOT_J
135200110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
135300110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
135400110912     C                   ENDIF
135500110912     C*** VATNOT_4
135600110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
135700110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
135800110912     C                   ENDIF
135900040510     C*
136000030715     C                   ENDIF
136100030715     C                   EVAL      i = i + 1
136200030715     C                   ENDDO
136300040510     C*
136400040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
136500040510     C                   IF        VABNSP = *zeros
136600040510     C                   SETON                                        31
136700040510     C                   ENDIF
136800040715     C*
136900040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
137000040715     C                   IF        VABLNP = *zeros
137100040715     C                   SETON                                        31
137200040715     C                   ENDIF
137300040510     C*
137400040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
137500040510     C                   IF        VABRMN = *zeros
137600040510     C                   Z-ADD     VABNSP        VABRMN
137700040510     C                   ENDIF
137800040510     C*
137900040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
138000040510     C                   IF        VABRMA = *blanks
138100040714     C                   MOVEL     VABRMN        VABRMA
138200040510     C                   ENDIF
138300070705     C*
138400070705     C* Considerazioni particolari
138500070705     C*** NZD
138600070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
138700070705     C                   EVAL      VABNZD = *blanks
138800070705     C                   ENDIF
138900070705     C*** CAD
139000070705     C                   IF        VABNZD = *blanks
139100070705     C                   EVAL      PiStr=VABCAD
139200070705     C                   EXSR      CHKNUM
139300070705     C                   IF        PiInt=*on
139400070705     C                   Z-ADD     PiVal         Num5_0
139500070705     C                   MOVEL(p)  Num5_0        VABCAD
139600070705     C                   ELSE
139700070705     C                   SETON                                        32
139800070705     C                   EVAL      VABCAD = *zeros
139900070705     C                   EVAL      vinmsg = %trimr(vinmsg)
140000070705     C                             + ' ' + 'VABCAD'
140100070705     C                   ENDIF
140200070705     C                   ENDIF
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
141700040113     C*
141800040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
141900040113     C                   IF        VABCAS > *zeros
142000040119     C                   EVAL      VABVCA = 'EUR'
142100040119     C                   IF        VABCBO = '1'
142200040113     C                   EVAL      VABCBO = '4'
142300040119     C                   ENDIF
142400040119     C                   IF        VABCBO = '2'
142500040119     C                   EVAL      VABCBO = '6'
142600040119     C                   ENDIF
142700040113     C                   ENDIF
142800020204     C*
142900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
143000020204     C                   EXSR      CHKIMPDIV
143100030715     C                   ENDIF
143200020204     C*
143300010607     C                   ENDSR
143400010607     C*----------------------------------------------------*
143500010601
143600020204
143700020204     C*----------------------------------------------------*
143800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
143900020204     C*----------------------------------------------------*
144000020204     C     CHKIMPDIV     BEGSR
144100020204     C*
144200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
144300020204     C                   Z-ADD     *zeros        wrkDec            9 9
144400020204     C*
144500020204     C* Come prima cosa effettuo considerazioni sulla divisa
144600020204     C                   IF        vabIAS > *zeros
144700020204     C                   IF        vabVAS <> 'EUR'
144800020204     C                   EVAL      vabVAS =  'ITL'
144900020204     C                   ENDIF
145000020204     C                   ENDIF
145100020204     C*
145200020204     C                   IF        vabCAS > *zeros
145300020204     C                   IF        vabVCA <> 'EUR'
145400020204     C                   EVAL      vabVCA =  'ITL'
145500020204     C                   ENDIF
145600020204     C                   ENDIF
145700020204     C*
145800020204     C                   IF        vabVMD > *zeros
145900020204     C                   IF        vabVAD <> 'EUR'
146000020204     C                   EVAL      vabVAD =  'ITL'
146100020204     C                   ENDIF
146200020204     C                   ENDIF
146300020204     C*
146400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
146500020204     C                   Z-ADD     vabIAS        wrkDec
146600020204     C                   IF        wrkDec > *zeros
146700020204     C                   IF        vabVAS = 'ITL'
146800020204     C                   EVAL      vabIAS = *zeros
146900020204     C                   ENDIF
147000020204     C                   ENDIF
147100020204     C*
147200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
147300020204     C                   Z-ADD     vabCAS        wrkDec
147400020204     C                   IF        wrkDec > *zeros
147500020204     C                   IF        vabVCA = 'ITL'
147600020204     C                   EVAL      vabCAS = *zeros
147700020204     C                   ENDIF
147800020204     C                   ENDIF
147900020204     C*
148000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
148100020204     C                   Z-ADD     vabVMD        wrkDec
148200020204     C                   IF        wrkDec > *zeros
148300020204     C                   IF        vabVAD = 'ITL'
148400020204     C                   EVAL      vabVMD = *zeros
148500020204     C                   ENDIF
148600020204     C                   ENDIF
148700020204     C*
148800020204     C                   ENDSR
148900020204     C***
149000020204
149100010330
149200010330
149300010330     C*----------------------------------------------------*
149400010330     C*  CONTROLLO NUMERICITA' CAMPI
149500010330     C*----------------------------------------------------*
149600010330     C     CHKNUM        BEGSR
149700010330     C*
149800010606     C                   IF        PiDecChr = *blanks
149900030715     C                   EVAL      PiDecChr = CharNUM
150000010606     C                   ENDIF
150100010606     C*
150200010606     C                   CALL(e)   'ISNUMERIC'
150300010330     C                   PARM                    PiStr            30
150400010606     C                   PARM                    PiDecChr          1
150500010330     C                   PARM      *ZEROS        PiVal            30 9
150600010330     C                   PARM      '0'           PiInt             1
150700010330     C                   PARM      '0'           PiNum             1
150800010330     C                   IF        %error
150900010606     C                   EVAL      PiNum=*off
151000010330     C                   ENDIF
151100010330     C*
151200010330     C                   ENDSR
151300010330     C***
151400010330
151500010601
151600010601
151700010601
151800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
151900010601     C     repfil        BEGSR
152000010601     C*
152100010601     C                   if        invfil = *zeros and
152200010601     C                             depfil > *zeros and
152300010629     C                             (vinflg = *blanks or
152400010629     C                              vinflg = *zeros)
152500010601     C
152600010601     C                   eval      invfil = depfil
152700010601     C                   endif
152800010601     C*
152900010601     C                   if        depfil <> invfil and
153000010601     C                             invfil > *zeros
153100010601     C                   eval      flgMulti = '1'
153200010601     C                   if        vinflg = *blanks
153300010601     C                   add       1             cntNonEl
153400010601     C                   endif
153500010601     C                   endif
153600010601     C*
153700010601     C                   if        vinflg = '2'
153800010601     C                   eval      flgStato = '2'
153900010601     C                   endif
154000010601     C*
154100010601     C                   ENDSR
154200010601     C***
154300010601
154400010601
154500010601
154600010330
154700040119      /TITLE Invio dei dati al punto operativo.
154800040119     C     invio         BEGSR
154900040119     C*
155000040119     C* 1° invio FIVAT
155100040119     C                   reset                   dscmz
155200070704     C                   move      winvcmz       cmzdst
155300040119     C                   eval      cmzfld = 'FIVATWWR'
155400040119     C                   eval      cmzmbd = vlrhdl
155500040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
155600040119     C***                if        prmfir = *blanks
155700040119     C                   eval      cmzfla = 'FIVAT00F'
155800040119     C                   eval      cmzmba = 'FIVAT00F'
155900040119     C***                else
156000040119     C***                eval      cmzfla = prmfir
156100040119     C***                eval      cmzmba = prmfir
156200040119     C***                endif
156300040119     C                   eval      cmznrr = *zeros
156400040119     C                   move      §ctrokvt      cmznrr
156500040119     C                   eval      cmzlba = vlrfl1
156600040119     C                   call(e)   'TIS711C'
156700040119     C                   parm                    dscmz
156800040119     C                   parm      *blanks       esito
156900040119     C                   if        %error
157000040119     C                             or cmzerr = '1'
157100040119     C                             or esito  = '1'
157200040119     C                   eval      wrkesito = '3'
157300040119     C                   else
157400040119     C*
157500040119     C* 2° invio FIVAB
157600040119     C                   reset                   dscmz
157700070704     C                   move      winvcmz       cmzdst
157800040119     C                   eval      cmzfld = vlrfou
157900040119     C                   eval      cmzmbd = vlrhdl
158000040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158100040119     C***                if        prmfir = *blanks
158200040119     C                   eval      cmzfla = 'FIVAB00F'
158300040119     C                   eval      cmzmba = 'FIVAB00F'
158400040119     C***                else
158500040119     C***                eval      cmzfla = prmfir
158600040119     C***                eval      cmzmba = prmfir
158700040119     C***                endif
158800040119     C                   eval      cmznrr = *zeros
158900040119     C                   move      §ctrokvb      cmznrr
159000040119     C                   eval      cmzlba = vlrfl1
159100040119     C                   call(e)   'TIS711C'
159200040119     C                   parm                    dscmz
159300040119     C                   parm      *blanks       esito
159400040119     C                   if        %error
159500040119     C                             or cmzerr = '1'
159600040119     C                             or esito  = '1'
159700040119     C                   eval      wrkesito = '3'
159800040119     C                   endif
159900040119     C                   endif
160000040119     C*
160100040119     C                   ENDSR
160200040119     C***
160300010601
160400010601
160500010601
160600010601
160700010601      /TITLE Invio dei dati al punto operativo.
160800010601     C     opeini        BEGSR
160900010601     C*
161000010601     C* Inizializzo flag e contatori operativi
161100010601     C                   movel     '0'           flgGiro           1
161200010601     C                   movel     '0'           flgMulti          1
161300010601     C                   movel     '1'           flgStato          1
161400010615     C                   movel     '0'           flgOk             1
161500010601     C                   z-add     *zeros        cntNonEl         10 0
161600010601     C                   z-add     *zeros        depfil            3 0
161700010601     C                   z-add     *zeros        invfil            3 0
161800010601     C*
161900010601     C                   ENDSR
162000010601     C***
162100070326
162200070326
162300070326
162400070326
162500070326     C     *pssr         BEGSR
162600070329     C*
162700070329     C                   if        %open(tivin00r)
162800070329     C                   close     tivin00r
162900070329     C                   endif
163000070329     C                   if        %open(fivabwwr)
163100070329     C                   close     fivabwwr
163200070329     C                   endif
163300070329     C                   if        %open(fivatwwr)
163400070329     C                   close     fivatwwr
163500070329     C                   endif
163600070326     C*
163700070326     C* Effettuo la chiamata al CLLE preposto
163800070326     C                   call(e)   'TITVVTC'
163900070326     C                   parm                    parccm
164000070326     C                   parm                    parmbr
164100070326     C                   parm      '2'           paropz
164200070326     C*
164300070326     C                   eval      wrkesito = '2'
164400070404     C*
164500070404     C                   seton                                        LR
164600070326     C*
164700070326     C                   ENDSR     '*CANCL'
164800070326     C***
164900070326
165000070326
165100010330
165200010330
165300000613     C     *inzsr        BEGSR
165400990910     C*
165500990910     C     *entry        plist
165600990920     C                   parm                    tivlrds
165700990921     C                   parm      wrkesito      esito
165800000724     C                   parm                    prmlit
165900000710     C                   parm                    prmfir
166000010330     C*
166100010330     C* CALCOLA LA DATA CORRENTE
166200010330     C                   time                    wn14             14 0
166300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
166400010330     C                   z-add     wn8           g08dat
166500010330     C                   z-add     *zeros        g08inv
166600010330     C                   movel     '0'           g08err
166700010330     C                   call      'XSRDA8'
166800010330     C                   parm                    wlbda8
166900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
167000000613     C*
167100000613     C                   ENDSR
167200000613     C***
167300990908
167400040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
167500040510
167600040122
167700040113
