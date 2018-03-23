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
004900131018     D wVATNOT_S       s                   LIKE(VATNOT)
005000141128     D wVATNOT_Y       s                   LIKE(VATNOT)
005100060331     D wVABNSP         s                   LIKE(VABNSP) INZ
005200030822     D*------------------
005300030822     D* VARIABILI X LO SPLIT DEI VALORI DI DEFAULT PROVENIENTI DAI PARAMETRI DEL TRADUTTORE
005400030822     D*------------------
005500030822     D posDaDft        S              3  0 INZ(*zeros)
005600030822     D posADft         S              3  0 INZ(*zeros)
005700030822     D j               s              3  0 INZ(1)
005800030822     D wGiroDft        s              1  0 INZ(*zeros)
005900010330     D*------------------
006000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100010330     D*------------------
006200010330     D WLBDA8          DS                  INZ
006300010330     D  G08DAT                 1      8  0
006400010330     D  G08INV                 9     16  0
006500010330     D  G08ERR                17     17
006600010330     D  G08TGI                18     22  0
006700031201     D*------------------
006800031201     D* Costanti
006900031201     D*------------------
007000031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
007100031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
007200010330
007300010330
007400010330
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800010601     C*
007900010601     C                   exsr      opeini
008000010605     C                   exsr      rwvab
008100010601     C*
008200010601     C                   seton                                        lr
008300010601
008400010601
008500010601
008600010601     C*--------------------------------------------------------
008700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008800010601     C*--------------------------------------------------------
008900010601     C     PREELA        BEGSR
009000010601     C*
009100010601     C* SE OCCORRE SPEDIRE IN FILIALE
009200010601     C                   if        invfil <> *zeros and
009300010601     C                             flgGiro = '0'
009400010601     C*
009500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009600010601     C                   eval      flgGiro = '1'
009700010601     C*
009800010601     C                   endif
009900010601     C*
010000010601     C                   ENDSR
010100010601     C***
010200010601
010300010601
010400010601
010500010601
010600010601     C*--------------------------------------------------------
010700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010601     C*--------------------------------------------------------
010900010601     C     ENDELA        BEGSR
011000040119     C*
011100040119     C* Effettuo la chiamata al CLLE preposto
011200040119     C                   call(e)   'TITVVTC'
011300040510     C                   parm                    parccm
011400040119     C                   parm                    parmbr
011500040119     C                   parm      '2'           paropz
011600050201     C*
011700050201     C* Effettuo lancio TISI95 solo x chiusura
011800050201     C                   CLEAR                   TISI95DS
011900050201     C                   EVAL      I95TLA = 'C'
012000050201     C                   CALL      'TISI95R'
012100050201     C                   PARM                    TISI95DS
012200000616     C*
012300010601     C                   ENDSR
012400010601     C***
012500000613
012600010601
012700010601
012800010330     C*--------------------------------------------------------
012900021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
013000010330     C*--------------------------------------------------------
013100010605     C     RWVAB         BEGSR
013200010330     C*
013300010330     C                   if        not %open(tivin00r)
013400010330     C                   open      tivin00r
013500010330     C                   endif
013600021025     C                   if        not %open(fivabwwr)
013700021025     C                   open      fivabwwr
013800010330     C                   endif
013900040119     C*
014000040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014100040119     C                   exsr      prevat
014200040119     C*
014300040119     C                   if        chkcall = '0'
014400040119     C*
014500040119     C                   if        not %open(fivatwwr)
014600040119     C                   open      fivatwwr
014700040119     C                   endif
014800010330     C*
014900040119     C                   clear                   §CTROKVB          7 0
015000040119     C                   clear                   §CTROKVT          7 0
015100040119     C                   clear                   §CTRMO            7 0
015200040119     C                   clear                   §CTRNO            7 0
015300010330     C*
015400030822     C                   do        *HIVAL
015500010330     C*
015600030822     C                   read      tivin00r                               70
015700010618     C*
015800010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015900010618     C                   if        vinflg = '1'
016000010618     C                   eval      flgOk = '1'
016100010618     C                   endif
016200040510     C*
016300040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
016400040510     C                   z-add     *zeros        wLenVINDTA        4 0
016500040510     C                   z-add     *zeros        wFlgVINDTA        1 0
016600090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
016700040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016800090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016900040510     C                   z-add     1             wFlgVINDTA
017000040510     C                   leave
017100040510     C                   endif
017200040510     C                   enddo
017300010618     C*
017400040510     C                   if        vindta > *blanks AND
017500040510     C                             wFlgVINDTA = 1
017600010330     C                   add       1             rrnum
017700010330     C*
017800010601     C                   if        *in70 = *off and
017900010330     C                             (vinflg = *blanks
018000010330     C                              or vinflg = '0'
018100010330     C                              or vinflg = '2')
018200010330     C*
018300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018400010711     C                   if        vinflg = *blanks or vinflg = '0'
018500010711     C                   clear                   vinmsg
018600010711     C                   endif
018700121107     C*
018800121107     C                   exsr      inzvar
018900090324     C*
019000090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
019100090324     C* separatori campi
019200090324     C                   if        CharTXT <> *blanks
019300090324     C                   z-add     *zeros        wLenVINDTA        4 0
019400090324     C                   z-add     *zeros        wFlgVINDTA        1 0
019500090324     C                   setoff                                       80
019600090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
019700090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
019800090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019900090324     C                   if        not *in80
020000090324     C                   seton                                        80
020100090324     C                   else
020200090324     C                   setoff                                       80
020300090324     C                   endif
020400090324     C                   endif
020500090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
020600090324     C                             and *in80 = *on
020700090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020800090324     C                   endif
020900090324     C                   enddo
021000090324     C                   endif
021100090324     C*
021200010605     C                   exsr      impvab
021300010601     C*
021400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021500010605     C                   eval      depfil = VABLNP
021600010601     C                   exsr      repfil
021700010601     C                   if        depfil = invfil
021800021025     C                   if        vlrpoi = 999
021900030822     C                   move(P)   invfil        VABFGS
022000070704     C                   move(P)   invfil        winvcmz           3
022100021025     C                   else
022200030822     C                   move(P)   vlrpoi        VABFGS
022300070704     C                   move(P)   vlrpoi        winvcmz
022400021025     C                   endif
022500010601     C*
022600010601     C                   exsr      PREELA
022700010601     C*
022800010604     C* Ebbene...
022900010604     C*
023000040119     C  N31              add       1             §CTROKVB
023100040119     C   32              add       1             §CTRMO
023200040119     C   31              add       1             §CTRNO
023300030822     C                   if        wGiro = 2
023400040510     C* Scrivo sempre il VAT
023500040510     C  N31              exsr      wrivat
023600040510     C* Scrivo il VAB solo a rottura d spedizione
023700040119     C                   if        depspe =  *zeros OR
023800040119     C                             depspe <> vabNSP
023900040119     C  N31              exsr      wrivab
024000040119     C  N31              eval      depspe = vabNSP
024100040119     C                   endif
024200040119     C                   endif
024300010604     C*
024400010604     C                   if        *in31 = *off and
024500010604     C                             *in32 = *off
024600010604     C                   eval      vinflg = '1'
024700010604     C                   else
024800010604     C                   eval      vinflg = '2'
024900010604     C                   endif
025000010604     C                   endif
025100010601     C*
025200010604     C                   endif
025300010604     C*
025400010330     C                   else
025500010330     C                   eval      vinflg = '1'
025600010330     C                   endif
025700010601     C*
025800010601     C  N70              update    tivin000
025900010330     C*
026000030822     C  N70              enddo
026100040119     C*
026200040119     C                   endif                                                  (endif - chkcall)
026300010601     C*
026400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
026500010601     C                   if        cntNonEl = *zeros or
026600010601     C                             flgMulti = '0'
026700010330     C* Se non ci sono record con errori ...
026800010601     C                   if        §ctrno = 0 and
026900010604     C                             §ctrmo = 0 and
027000010601     C                             flgStato <> '2'
027100010330     C* ... restituisco esito OK.
027200010330     C                   eval      wrkesito = '0'
027300010330     C                   else
027400040119     C                   if        §ctrokvb > 0
027500010330     C                   eval      wrkesito = '1'
027600010330     C                   else
027700010615     C                   if        flgOk = '0'
027800010615     C                   eval      wrkesito = '2'
027900010615     C                   else
028000010615     C                   eval      wrkesito = '6'
028100010615     C                   endif
028200010330     C                   endif
028300010330     C                   endif
028400010601     C                   else
028500010601     C                   eval      wrkesito = '9'
028600010601     C                   endif
028700010330     C*
028800010330     C                   if        %open(tivin00r)
028900010330     C                   close     tivin00r
029000010330     C                   endif
029100021025     C                   if        %open(fivabwwr)
029200021025     C                   close     fivabwwr
029300010330     C                   endif
029400040512     C                   if        %open(fivatwwr)
029500040512     C                   close     fivatwwr
029600040512     C                   endif
029700010601     C*
029800010601     C                   if        vlrpoi <> 999
029900010601     C                   eval      invfil = vlrpoi
030000010601     C                   endif
030100010330     C*
030200040119     C                   if        §ctrokvb > 0
030300010601     C                             and invfil > *zeros
030400010330     C                   exsr      invio
030500010330     C                   endif
030600010601     C*
030700010618     C                   if        flgGiro = '1'
030800010601     C                   exsr      endela
030900010618     C                   endif
031000010330     C*
031100010330     C                   ENDSR
031200010330     C***
031300040119
031400040119
031500040119
031600040119     C*----------------------------------------------------*
031700040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031800040119     C*----------------------------------------------------*
031900040510     C     WRIVAB        BEGSR
032000040119     C*
032100060331     C* Gestisco l'eventuale rottura x numero spedizione
032200060331     C                   if        VABNSP <> wVABNSP
032300070301     C                   if        wVATNOT_E <> *blanks
032400110419     C                   if        VABCTM = *blanks
032500070301     C                   eval      VABCTM = '7Q'
032600110419     C                   endif
032700070301     C                   endif
032800040119     C                   write     fivab000                                     => scarico il VAB
032900060331     C                   eval      wVABNSP = VABNSP
033000060331     C                   endif
033100040119     C*
033200040119     C                   ENDSR
033300040119
033400040119
033500040119
033600040119     C*----------------------------------------------------*
033700040119     C*  SCARICAMENTO BUFFER RECORDS VAT
033800040119     C*----------------------------------------------------*
033900040510     C     WRIVAT        BEGSR
034000060512     C*
034100060512     C* Inizializzo i campi chiave
034200060512     C                   eval      VATFGS = VABFGS
034300060512     C                   eval      VATAAS = VABAAS
034400060512     C                   eval      VATCCM = VABCCM
034500060512     C                   eval      VATNSP = VABNSP
034600060512     C                   eval      VATLNP = VABLNP
034700040714     C*
034800040714     C* Solo se VATNOT valorizzato scarico il buffer
034900040714     C                   if        VATNOT <> *blanks
035000040119     C*
035100040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
035200040510     C                   if        VATTRC = *blanks
035300040510     C                   eval      VATTRC = 'E'
035400040510     C                   endif
035500040510     C                   write     FIVAT000
035600040119     C                   add       1             §CTROKVT
035700040714     C*
035800040714     C                   endif
035900060331     C*
036000060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
036100060331     C                   if        wVATNOT_A <> *blanks
036200060331     C                   eval      VATNOT = wVATNOT_A
036300060331     C                   eval      VATTRC = 'A'
036400060331     C                   write     FIVAT000
036500060331     C                   add       1             §CTROKVT
036600060331     C                   endif
036700060331     C*
036800060331     C                   if        wVATNOT_B <> *blanks
036900060331     C                   eval      VATNOT = wVATNOT_B
037000060331     C                   eval      VATTRC = 'B'
037100060331     C                   write     FIVAT000
037200060331     C                   add       1             §CTROKVT
037300060331     C                   endif
037400060331     C*
037500060331     C                   if        wVATNOT_E <> *blanks
037600060331     C                   eval      VATNOT = wVATNOT_E
037700060331     C                   eval      VATTRC = 'E'
037800060331     C                   write     FIVAT000
037900060331     C                   add       1             §CTROKVT
038000060331     C                   endif
038100110912     C*
038200110912     C                   if        wVATNOT_I <> *blanks
038300110912     C                   eval      VATNOT = wVATNOT_I
038400110912     C                   eval      VATTRC = 'I'
038500110912     C                   write     FIVAT000
038600110912     C                   add       1             §CTROKVT
038700110912     C                   endif
038800110912     C*
038900110912     C                   if        wVATNOT_J <> *blanks
039000110912     C                   eval      VATNOT = wVATNOT_J
039100110912     C                   eval      VATTRC = 'J'
039200110912     C                   write     FIVAT000
039300110912     C                   add       1             §CTROKVT
039400110912     C                   endif
039500110912     C*
039600110912     C                   if        wVATNOT_4 <> *blanks
039700110912     C                   eval      VATNOT = wVATNOT_4
039800110912     C                   eval      VATTRC = '4'
039900110912     C                   write     FIVAT000
040000110912     C                   add       1             §CTROKVT
040100110912     C                   endif
040200131018     C*
040300131018     C                   if        wVATNOT_S <> *blanks
040400131018     C                   eval      VATNOT = wVATNOT_S
040500131018     C                   eval      VATTRC = 'S'
040600131018     C                   write     FIVAT000
040700131018     C                   add       1             §CTROKVT
040800131018     C                   endif
040900141128     C*
041000141128     C                   if        wVATNOT_Y <> *blanks
041100141128     C                   eval      VATNOT = wVATNOT_Y
041200141128     C                   eval      VATTRC = 'Y'
041300141128     C                   write     FIVAT000
041400141128     C                   add       1             §CTROKVT
041500141128     C                   endif
041600040119     C*
041700040119     C                   ENDSR
041800040119
041900040119
042000040119
042100040119     C*----------------------------------------------------*
042200040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042300040119     C*----------------------------------------------------*
042400040119     C     PREVAT        BEGSR
042500040119     C*
042600040119     C* Compongo il nome del membro da dare al FIVATWWR
042700040119     C                   eval      parmbr = vlrhdl
042800040119     C                   movel     'M'           parmbr
042900040510     C                   eval      parccm = vlrksc
043000040119     C                   eval      paropz = '1'
043100040119     C* Effettuo la chiamata al CLLE preposto
043200040119     C                   call(e)   'TITVVTC'
043300040119     C                   parm                    parccm
043400040119     C                   parm                    parmbr
043500040119     C                   parm                    paropz
043600040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043700040119     C                   if        %error
043800040119     C                   movel     '1'           chkcall
043900040119     C                   else
044000040119     C                   movel     '0'           chkcall
044100040119     C                   endif
044200040119     C*
044300040119     C                   ENDSR
044400010601
044500010601
044600010601
044700010330     C*----------------------------------------------------*
044800030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044900010330     C*----------------------------------------------------*
045000040119     C     INZVAR        BEGSR
045100010330     C*
045200030822     C* Inizializzo variabili di wrk
045300020204     C                   Z-ADD     *zeros        Num5_0            5 0
045400090403     C                   z-add     *zeros        invfil
045500060331     C                   MOVEL     *blanks       wVATNOT_A
045600060331     C                   MOVEL     *blanks       wVATNOT_B
045700060331     C                   MOVEL     *blanks       wVATNOT_E
045800110912     C                   MOVEL     *blanks       wVATNOT_I
045900110912     C                   MOVEL     *blanks       wVATNOT_J
046000110912     C                   MOVEL     *blanks       wVATNOT_4
046100131018     C                   MOVEL     *blanks       wVATNOT_S
046200141128     C                   MOVEL     *blanks       wVATNOT_Y
046300081014     C*
046400081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046500081014     C                   CLEAR                   FIVAB000
046600081014     C                   CLEAR                   FIVAT000
046700030822     C*
046800081014     C* Inizializzo la schiera d wrk x i dati
046900030822     C                   CLEAR                   SkSplitCSV
047000030822     C                   EVAL      i = 1
047100030822     C                   EVAL      posDa = *zeros
047200030822     C                   EVAL      posA  = *zeros
047300030822     C*
047400030822     C* Reimposto i valori di default
047500030822     C                   EXSR      DEFCAM
047600030822     C*
047700010330     C                   ENDSR
047800010330     C*----------------------------------------------------*
047900030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
048000010330     C*----------------------------------------------------*
048100010330     C     DEFCAM        BEGSR
048200030822     C*
048300030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048400030715     C* e delimitatore testo.
048500030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048600030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048700030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048800090324     C*
048900090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
049000090324     C                   EVAL      CharSOS = CharNUM
049100030822     C*
049200081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
049300030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049400081007     C                   SETON                                        40
049500030715     C                   ELSE
049600081007     C                   SETOFF                                       40
049700030715     C                   ENDIF
049800081007     C*
049900081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
050000081007     C                   SETOFF                                       50
050100030822     C*
050200030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
050300030822     C                   EVAL      posDaDft = 1
050400030822     C                   EVAL      posADft  = 0
050500030822     C                   EVAL      wGiroDft = 0
050600030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050700030822     C                             posDaDft > 0
050800030822     C*
050900030822     C* Gestisco il 1° giro
051000030822     C                   IF        wGiroDft = 0
051100030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
051200030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
051300030822     C* Incremento il contatore dei "giri"
051400030822     C                   EVAL      wGiroDft = 1
051500030822     C                   ELSE
051600030822     C                   EVAL      posDaDft = posADft
051700030822     C                   ENDIF
051800030822     C* Eseguo lo scan x trovare la fine del campo corrente
051900030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
052000030822     C*
052100030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
052200030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
052300030822     C                   IF        posDaDft > 0 AND
052400030822     C                             posADft  > 0
052500030822     C* NCL
052600030822     C                   IF        %subst(
052700030822     C                             %subst(vlrppt:posDaDft+1:
052800030822     C                             posADft-posDaDft-1):1:3)
052900030822     C                             = 'NCL'
053000030822     C                   EVAL      PiStr=%trim(%subst(
053100030822     C                             %subst(vlrppt:posDaDft+1:
053200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053300030822     C                   EXSR      CHKNUM
053400030822     C                   IF        PiInt=*on
053500030822     C                   Z-ADD     PiVal         VABNCL
053600030822     C                   ENDIF
053700030822     C                   ENDIF
053800030822     C* CCM
053900030822     C                   IF        %subst(
054000030822     C                             %subst(vlrppt:posDaDft+1:
054100030822     C                             posADft-posDaDft-1):1:3)
054200030822     C                             = 'CCM'
054300030822     C                   EVAL      PiStr=%trim(%subst(
054400030822     C                             %subst(vlrppt:posDaDft+1:
054500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054600030822     C                   EXSR      CHKNUM
054700030822     C                   IF        PiInt=*on
054800030822     C                   Z-ADD     PiVal         VABCCM
054900030822     C                   ENDIF
055000030822     C                   ENDIF
055100030822     C* LNP
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'LNP'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABLNP
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* NRS
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'NRS'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABNRS
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* CTR
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'CTR'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABCTR
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* PKB
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'PKB'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiNum=*on
060000030822     C                   Z-ADD     PiVal         VABPKB
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* VLB
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'VLB'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiNum=*on
061300030822     C                   Z-ADD     PiVal         VABVLB
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* QFT
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'QFT'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiNum=*on
062600030822     C                   Z-ADD     PiVal         VABQFT
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* CBO
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'CBO'
063400030822     C                   EVAL      VABCBO=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   ENDIF
063800030822     C* TSP
063900030822     C                   IF        %subst(
064000030822     C                             %subst(vlrppt:posDaDft+1:
064100030822     C                             posADft-posDaDft-1):1:3)
064200030822     C                             = 'TSP'
064300030822     C                   EVAL      VABTSP=%trim(%subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064600030822     C                   ENDIF
064700030822     C* VAS
064800030822     C                   IF        %subst(
064900030822     C                             %subst(vlrppt:posDaDft+1:
065000030822     C                             posADft-posDaDft-1):1:3)
065100030822     C                             = 'VAS'
065200030822     C                   EVAL      VABVAS=%trim(%subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065500030822     C                   ENDIF
065600030822     C* VCA
065700030822     C                   IF        %subst(
065800030822     C                             %subst(vlrppt:posDaDft+1:
065900030822     C                             posADft-posDaDft-1):1:3)
066000030822     C                             = 'VCA'
066100030822     C                   EVAL      VABVCA=%trim(%subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066400030822     C                   ENDIF
066500030822     C* GCA
066600030822     C                   IF        %subst(
066700030822     C                             %subst(vlrppt:posDaDft+1:
066800030822     C                             posADft-posDaDft-1):1:3)
066900030822     C                             = 'GCA'
067000030822     C                   EVAL      VABGCA=%trim(%subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067300030822     C                   ENDIF
067400030822     C* CTM
067500030822     C                   IF        %subst(
067600030822     C                             %subst(vlrppt:posDaDft+1:
067700030822     C                             posADft-posDaDft-1):1:3)
067800030822     C                             = 'CTM'
067900030822     C                   EVAL      VABCTM=%trim(%subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068200030822     C                   ENDIF
068300030822     C* FFD
068400030822     C                   IF        %subst(
068500030822     C                             %subst(vlrppt:posDaDft+1:
068600030822     C                             posADft-posDaDft-1):1:3)
068700030822     C                             = 'FFD'
068800030822     C                   EVAL      VABFFD=%trim(%subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069100030822     C                   ENDIF
069200030822     C* VAD
069300030822     C                   IF        %subst(
069400030822     C                             %subst(vlrppt:posDaDft+1:
069500030822     C                             posADft-posDaDft-1):1:3)
069600030822     C                             = 'VAD'
069700030822     C                   EVAL      VABVAD=%trim(%subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070000030822     C                   ENDIF
070100030822     C* GMA
070200030822     C                   IF        %subst(
070300030822     C                             %subst(vlrppt:posDaDft+1:
070400030822     C                             posADft-posDaDft-1):1:3)
070500030822     C                             = 'GMA'
070600030822     C                   EVAL      VABGMA=%trim(%subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070900030822     C                   ENDIF
071000030822     C* GGA
071100030822     C                   IF        %subst(
071200030822     C                             %subst(vlrppt:posDaDft+1:
071300030822     C                             posADft-posDaDft-1):1:3)
071400030822     C                             = 'GGA'
071500030822     C                   EVAL      VABGGA=%trim(%subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071800030822     C                   ENDIF
071900030822     C* GVA
072000030822     C                   IF        %subst(
072100030822     C                             %subst(vlrppt:posDaDft+1:
072200030822     C                             posADft-posDaDft-1):1:3)
072300030822     C                             = 'GVA'
072400030822     C                   EVAL      VABGVA=%trim(%subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072700030822     C                   ENDIF
072800030822     C* TC1
072900030822     C                   IF        %subst(
073000030822     C                             %subst(vlrppt:posDaDft+1:
073100030822     C                             posADft-posDaDft-1):1:3)
073200030822     C                             = 'TC1'
073300030822     C                   EVAL      VABTC1=%trim(%subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073600030822     C                   ENDIF
073700030822     C* TC2
073800030822     C                   IF        %subst(
073900030822     C                             %subst(vlrppt:posDaDft+1:
074000030822     C                             posADft-posDaDft-1):1:3)
074100030822     C                             = 'TC2'
074200030822     C                   EVAL      VABTC2=%trim(%subst(
074300030822     C                             %subst(vlrppt:posDaDft+1:
074400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074500030822     C                   ENDIF
074600080516     C* TIC
074700080516     C                   IF        %subst(
074800080516     C                             %subst(vlrppt:posDaDft+1:
074900080516     C                             posADft-posDaDft-1):1:3)
075000080516     C                             = 'TIC'
075100080516     C                   EVAL      VABTIC=%trim(%subst(
075200080516     C                             %subst(vlrppt:posDaDft+1:
075300080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075400080516     C                   ENDIF
075500040714     C* VATTRC
075600040714     C                   IF        %subst(
075700040714     C                             %subst(vlrppt:posDaDft+1:
075800040714     C                             posADft-posDaDft-1):1:3)
075900040714     C                             = 'TRC'
076000040714     C                   EVAL      VATTRC=%trim(%subst(
076100040714     C                             %subst(vlrppt:posDaDft+1:
076200040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076300040714     C                   ENDIF
076400030822     C* ...
076500030822     C                   ENDIF
076600030822     C                   ENDDO
076700020204     C*
076800010330     C                   ENDSR
076900010607     C*----------------------------------------------------*
077000021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
077100010607     C*----------------------------------------------------*
077200010607     C     IMPVAB        BEGSR
077300010607     C*
077400010607     C                   SETOFF                                       3132
077500030515     C*
077600010607     C                   MOVEL     datcor        VABAAS
077700010607     C                   MOVE      datcor        VABMGS
077800010607     C*
077900030715     C**********
078000030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
078100030715     C**********
078200030715     C                   IF        wGiro = *zeros
078300040113     C*
078400040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
078500040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078600040510     C                             %trim(intcol(3))<>*blanks
078700040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078800040113     C                                      %trim(intcol(3))
078900040510     C                   ENDIF
079000031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
079100031201     C     minu:maiu     XLATE     vindta        vindta
079200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
079300030715     C                   DOW       posDa <= %len(%trim(vindta))
079400030715     C*
079500030715     C* Gestisco il 1° campo
079600030715     C                   IF        i = 1
079700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079800030715     C                   EVAL      posDa = 1
079900030715     C* Eseguo lo scan x trovare la fine del primo campo
080000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080100030715     C* A questo punto "estraggo" il campo corrente
080200030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080300030715     C                                                    (posA-posDa))
080400030715     C* X i campi successivi al 1°
080500030715     C                   ELSE
080600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080700030715     C                   EVAL      posDa = posA + 1
080800030715     C* Eseguo lo scan x trovare la fine del campo corrente
080900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
081000030715     C* Gestisco l'ultimo campo
081100030715     C                   IF        posA = *zeros
081200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C                   LEAVE
081700030715     C                   ELSE
081800030715     C* A questo punto "estraggo" il campo corrente
081900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082000030715     C                                                    (posA-posDa))
082100030715     C                   ENDIF
082200030715     C                   ENDIF
082300030715     C* Incremento il contatore d campo
082400030715     C                   EVAL      i = i +1
082500030715     C                   ENDDO
082600030715     C*
082700030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082800030715     C* delimitazione testo
082900030715     C                   EVAL      i = 1
083000030715     C                   DOW       i < %elem(SkSplitFLD)
083100030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
083200030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
083300030715     C                   EVAL      i = i + 1
083400030715     C                   ENDDO
083500030715     C                   EVAL      wGiro = 1
083600030715     C                   ELSE
083700030715     C                   EVAL      wGiro = 2
083800031204     C**********
083900031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
084000031204     C* d separatore campo
084100031204     C**********
084200031204     C                   EVAL      vindta = %trim(vindta)
084300031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
084400031204     C                   EVAL      vindta = %subst(vindta:2)
084500031204     C                   ENDDO
084600031204     C*
084700031204     C                   Z-ADD     *zeros        lunghInput        4 0
084800031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084900031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
085000031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
085100031204     C                   EVAL      lunghInput = %len(%trim(vindta))
085200031204     C                   ENDDO
085300030715     C**********
085400030715     C* Effettuo lo split del campo dati d input
085500030715     C**********
085600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
085700030715     C                   DOW       posDa <= %len(%trim(vindta))
085800030715     C*
085900030715     C* Gestisco il 1° campo
086000030715     C                   IF        i = 1
086100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
086200030715     C                   EVAL      posDa = 1
086300030715     C* Eseguo lo scan x trovare la fine del primo campo
086400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086500030715     C* A questo punto "estraggo" il campo corrente
086600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086700030715     C                                                    (posA-posDa))
086800030715     C* X i campi successivi al 1°
086900030715     C                   ELSE
087000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
087100030715     C                   EVAL      posDa = posA + 1
087200031204     C* Verifico che nn vi sia il campo nn valorizzato
087300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
087400031204     C* Se campo nn valorizzato skippo al prossimo
087500031204     C                   EVAL      SkSplitCSV(i) = *blanks
087600031204     C                   EVAL      posA  = posA + 1
087700031204     C                   ELSE
087800030715     C* Eseguo lo scan x trovare la fine del campo corrente
087900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
088000030715     C* Gestisco l'ultimo campo
088100030715     C                   IF        posA = *zeros
088200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
088300030715     C* A questo punto "estraggo" il campo corrente
088400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088500030715     C                                                    (posA-posDa))
088600030715     C                   LEAVE
088700030715     C                   ELSE
088800030715     C* A questo punto "estraggo" il campo corrente
088900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089000030715     C                                                    (posA-posDa))
089100030715     C                   ENDIF
089200031204     C                   ENDIF
089300030715     C                   ENDIF
089400030715     C* Incremento il contatore d campo
089500030715     C                   EVAL      i = i +1
089600030715     C                   ENDDO
089700030715     C*
089800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089900030715     C* delimitazione testo
090000030715     C                   EVAL      i = 1
090100030715     C                   DOW       i < %elem(SkSplitCSV)
090200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
090300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
090400030715     C                   EVAL      i = i + 1
090500030715     C                   ENDDO
090600030715     C*
090700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090800030715     C                   EVAL      i = 1
090900030715     C                   DOW       i < %elem(SkSplitFLD)
091000030715     C*
091100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
091200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
091300030715     C                             SkSplitCSV(i) <> *blanks
091400030721     C*** ATB
091500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
091600030721     C                   EVAL      VABATB = SkSplitCSV(i)
091700030721     C                   ENDIF
091800030715     C*** RSD
091900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
092000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
092100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
092200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
092300030715     C* ==
092400030715     C                   ENDIF
092500030721     C*** RD2
092600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
092700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092800030721     C                   ENDIF
092900030715     C*** IND
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
093100030715     C                   EVAL      VABIND = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** LOD
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
093500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** PRD
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030721     C*** NZD
094200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
094300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
094400030721     C                   ENDIF
094500030715     C*** RMA
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
094700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** NOT
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
095100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** NT2
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
095500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** VCA
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030715     C*** VAS
096200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
096300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
096400030715     C                   ENDIF
096500030715     C*** FFD
096600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
096700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096800030715     C                   ENDIF
096900030721     C*** GC1
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
097100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
097200100304     C                   IF        %subst(VABGC1:1) = 'P' or
097300100304     C                             %subst(VABGC1:1) = 'M'
097400100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
097500100304     C                   ENDIF
097600030721     C                   ENDIF
097700030721     C*** GC2
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
098000100304     C                   IF        %subst(VABGC2:1) = 'P' or
098100100304     C                             %subst(VABGC2:1) = 'M'
098200100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
098300100304     C                   ENDIF
098400030721     C                   ENDIF
098500030721     C*** TSP
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
098700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098800030721     C                   ENDIF
098900040510     C*** CBO
099000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
099100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
099200040510     C                   ENDIF
099300030721     C*** NAS
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
099500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** TIC
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
099900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** GCA
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
100300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** XCO
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
100700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** CTM
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
101100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** TCR
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
101500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** CTS
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
101900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** FTM
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
102300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** VAD
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
102700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** GMA
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
103100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** GGA
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
103500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** GVA
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
103900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** TC1
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
104300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** TC2
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
104700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** SCL
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
105100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*** RMO
105400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
105500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
105600030721     C                   ENDIF
105700030721     C*** NMO
105800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
105900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
106000030721     C                   ENDIF
106100030721     C*
106200030721     C* Reperisco quindi i campi numerici...
106300040510     C*** NSP
106400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
106500040510     C                   EVAL      PiStr=SkSplitCSV(i)
106600040510     C                   EXSR      CHKNUM
106700040510     C                   IF        PiInt=*on
106800040510     C                   Z-ADD     PiVal         VABNSP
106900040510     C                   ELSE
107000040510     C                   SETON                                        31
107100040510     C                   EVAL      VABNSP = *zeros
107200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
107300040510     C                             + ' ' + 'VABNSP'
107400040510     C                   ENDIF
107500040510     C                   ENDIF
107600070703     C*** RMN
107700070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107800070703     C                   EVAL      PiStr=SkSplitCSV(i)
107900070703     C                   EXSR      CHKNUM
108000070703     C                   IF        PiInt=*on
108100070703     C                   Z-ADD     PiVal         VABRMN
108200070703     C                   ELSE
108300070703     C                   SETON                                        32
108400070703     C                   EVAL      vinmsg = %trimr(vinmsg)
108500070703     C                             + ' ' + 'VABRMN'
108600070703     C                   ENDIF
108700070703     C                   ENDIF
108800070705     C*** CAD
108900070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
109000070705     C                   EVAL      VABCAD = SkSplitCSV(i)
109100070705     C                   ENDIF
109200030721     C*** NCL
109300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
109400030721     C                   EVAL      PiStr=SkSplitCSV(i)
109500030721     C                   EXSR      CHKNUM
109600030721     C                   IF        PiInt=*on
109700081007     C  N40              Z-ADD     PiVal         VABNCL
109800081007     C   40              ADD       PiVal         VABNCL
109900030721     C                   ELSE
110000030721     C                   SETON                                        32
110100030721     C                   EVAL      VABNCL = *zeros
110200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110300040122     C                             + ' ' + 'VABNCL'
110400030721     C                   ENDIF
110500030721     C                   ENDIF
110600030721     C*** PKB
110700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110800030721     C                   EVAL      PiStr=SkSplitCSV(i)
110900030721     C                   EXSR      CHKNUM
111000030721     C                   IF        PiNum=*on
111100081007     C  N40              Z-ADD     PiVal         VABPKB
111200081007     C   40              ADD       PiVal         VABPKB
111300030721     C                   ELSE
111400030721     C                   SETON                                        32
111500030721     C                   EVAL      VABPKB = *zeros
111600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111700030721     C                             + ' ' + 'VABPKB'
111800030721     C                   ENDIF
111900030721     C                   ENDIF
112000030721     C*** CAS
112100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
112200030721     C                   EVAL      PiStr=SkSplitCSV(i)
112300030721     C                   EXSR      CHKNUM
112400030721     C                   IF        PiNum=*on
112500030721     C                   Z-ADD     PiVal         VABCAS
112600030721     C                   ELSE
112700030721     C                   SETON                                        32
112800030721     C                   EVAL      VABCAS = *zeros
112900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113000030721     C                             + ' ' + 'VABCAS'
113100030721     C                   ENDIF
113200030721     C                   ENDIF
113300030721     C*** CCM
113400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
113500030721     C                   EVAL      PiStr=SkSplitCSV(i)
113600030721     C                   EXSR      CHKNUM
113700030721     C                   IF        PiInt=*on      AND
113800030721     C                             PiVal<=9999999 AND
113900030721     C                             PiVal>*zeros
114000030721     C                   Z-ADD     PiVal         VABCCM
114100030721     C                   ELSE
114200030721     C                   SETON                                        32
114300030721     C                   EVAL      VABCCM = *zeros
114400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114500040510     C                             + ' ' + 'VABCCM'
114600030721     C                   ENDIF
114700030721     C                   ENDIF
114800030721     C*** LNP
114900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
115000030721     C                   EVAL      PiStr=SkSplitCSV(i)
115100030721     C                   EXSR      CHKNUM
115200030721     C                   IF        PiInt=*on    AND
115300030721     C                             PiVal<=999   AND
115400030721     C                             PiVal>*zeros
115500030721     C                   Z-ADD     PiVal         VABLNP
115600030721     C                   ELSE
115700040715     C                   SETON                                        32
115800030721     C                   EVAL      VABLNP = *zeros
115900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116000040510     C                             + ' ' + 'VABLNP'
116100030721     C                   ENDIF
116200030721     C                   ENDIF
116300030721     C*** NRS
116400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
116500030721     C                   EVAL      PiStr=SkSplitCSV(i)
116600030721     C                   EXSR      CHKNUM
116700030721     C                   IF        PiInt=*on AND
116800030721     C                             PiVal<=99
116900030721     C                   Z-ADD     PiVal         VABNRS
117000030721     C                   ELSE
117100030721     C                   SETON                                        32
117200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
117300030721     C                             + ' ' + 'VABNRS'
117400030721     C                   ENDIF
117500030721     C                   ENDIF
117600030721     C*** LNA
117700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117800030721     C                   EVAL      PiStr=SkSplitCSV(i)
117900030721     C                   EXSR      CHKNUM
118000030721     C                   IF        PiInt=*on    AND
118100030721     C                             PiVal<=999   AND
118200030721     C                             PiVal>*zeros
118300030721     C                   Z-ADD     PiVal         VABLNA
118400030721     C                   ELSE
118500030721     C                   SETON                                        32
118600030721     C                   EVAL      VABLNA = *zeros
118700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118800030721     C                             + ' ' + 'VABLNA'
118900030721     C                   ENDIF
119000030721     C                   ENDIF
119100030721     C*** CTR
119200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
119300030721     C                   EVAL      PiStr=SkSplitCSV(i)
119400030721     C                   EXSR      CHKNUM
119500030721     C                   IF        PiInt=*on  AND
119600030721     C                             PiVal<=999
119700030721     C                   Z-ADD     PiVal         VABCTR
119800030721     C                   ELSE
119900030721     C                   SETON                                        32
120000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120100030721     C                             + ' ' + 'VABCTR'
120200030721     C                   ENDIF
120300030721     C                   ENDIF
120400030721     C*** IAS
120500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
120600030721     C                   EVAL      PiStr=SkSplitCSV(i)
120700030721     C                   EXSR      CHKNUM
120800030721     C                   IF        PiNum=*on
120900030721     C                   Z-ADD     PiVal         VABIAS
121000030721     C                   ELSE
121100030721     C                   SETON                                        32
121200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121300030721     C                             + ' ' + 'VABIAS'
121400030721     C                   ENDIF
121500030721     C                   ENDIF
121600030721     C*** VLB
121700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121800030721     C                   EVAL      PiStr=SkSplitCSV(i)
121900030721     C                   EXSR      CHKNUM
122000030721     C                   IF        PiNum=*on
122100081007     C  N40              Z-ADD     PiVal         VABVLB
122200081007     C   40              ADD       PiVal         VABVLB
122300030721     C                   ELSE
122400030721     C                   SETON                                        32
122500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122600030721     C                             + ' ' + 'VABVLB'
122700030721     C                   ENDIF
122800030721     C                   ENDIF
122900030721     C*** QFT
123000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
123100030721     C                   EVAL      PiStr=SkSplitCSV(i)
123200030721     C                   EXSR      CHKNUM
123300030721     C                   IF        PiNum=*on
123400081007     C  N40              Z-ADD     PiVal         VABQFT
123500081007     C   40              ADD       PiVal         VABQFT
123600030721     C                   ELSE
123700030721     C                   SETON                                        32
123800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123900030721     C                             + ' ' + 'VABQFT'
124000030721     C                   ENDIF
124100030721     C                   ENDIF
124200030721     C*** NCD
124300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
124400030721     C                   EVAL      PiStr=SkSplitCSV(i)
124500030721     C                   EXSR      CHKNUM
124600030721     C                   IF        PiInt=*on      AND
124700030721     C                             PiVal<=9999999
124800030721     C                   Z-ADD     PiVal         VABNCD
124900030721     C                   ELSE
125000030721     C                   SETON                                        32
125100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125200030721     C                             + ' ' + 'VABNCD'
125300030721     C                   ENDIF
125400030721     C                   ENDIF
125500030721     C*** NCA
125600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
125700030721     C                   EVAL      PiStr=SkSplitCSV(i)
125800030721     C                   EXSR      CHKNUM
125900030721     C                   IF        PiInt=*on      AND
126000030721     C                             PiVal<=9999999
126100030721     C                   Z-ADD     PiVal         VABNCA
126200030721     C                   ELSE
126300030721     C                   SETON                                        32
126400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126500030721     C                             + ' ' + 'VABNCA'
126600030721     C                   ENDIF
126700030721     C                   ENDIF
126800030721     C*** ZNC
126900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
127000030721     C                   EVAL      PiStr=SkSplitCSV(i)
127100030721     C                   EXSR      CHKNUM
127200030721     C                   IF        PiInt=*on AND
127300030721     C                             PiVal<=99
127400030721     C                   Z-ADD     PiVal         VABZNC
127500030721     C                   ELSE
127600030721     C                   SETON                                        32
127700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127800030721     C                             + ' ' + 'VABZNC'
127900030721     C                   ENDIF
128000030721     C                   ENDIF
128100030721     C*** DCR
128200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
128300030721     C                   EVAL      PiStr=SkSplitCSV(i)
128400030721     C                   EXSR      CHKNUM
128500030721     C                   IF        PiInt=*on       AND
128600030721     C                             PiVal<=99999999
128700030721     C                   Z-ADD     PiVal         VABDCR
128800030721     C                   ELSE
128900030721     C                   SETON                                        32
129000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129100030721     C                             + ' ' + 'VABDCR'
129200030721     C                   ENDIF
129300030721     C                   ENDIF
129400030721     C*** HCR
129500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
129600030721     C                   EVAL      PiStr=SkSplitCSV(i)
129700030721     C                   EXSR      CHKNUM
129800030721     C                   IF        PiInt=*on   AND
129900030721     C                             PiVal<=9999
130000030721     C                   Z-ADD     PiVal         VABHCR
130100030721     C                   ELSE
130200030721     C                   SETON                                        32
130300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130400030721     C                             + ' ' + 'VABHCR'
130500030721     C                   ENDIF
130600030721     C                   ENDIF
130700030721     C*** VMD
130800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130900030721     C                   EVAL      PiStr=SkSplitCSV(i)
131000030721     C                   EXSR      CHKNUM
131100030721     C                   IF        PiNum=*on
131200030721     C                   Z-ADD     PiVal         VABVMD
131300030721     C                   ELSE
131400030721     C                   SETON                                        32
131500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131600030721     C                             + ' ' + 'VABVMD'
131700030721     C                   ENDIF
131800030721     C                   ENDIF
131900030721     C*** ANT
132000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
132100030721     C                   EVAL      PiStr=SkSplitCSV(i)
132200030721     C                   EXSR      CHKNUM
132300030721     C                   IF        PiInt=*on        AND
132400030721     C                             PiVal<=999999999
132500030721     C                   Z-ADD     PiVal         VABANT
132600030721     C                   ELSE
132700030721     C                   SETON                                        32
132800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132900030721     C                             + ' ' + 'VABANT'
133000030721     C                   ENDIF
133100030721     C                   ENDIF
133200030721     C*** CMO
133300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
133400030721     C                   EVAL      PiStr=SkSplitCSV(i)
133500030721     C                   EXSR      CHKNUM
133600030721     C                   IF        PiInt=*on
133700030721     C                   Z-ADD     PiVal         Num5_0
133800030721     C                   MOVEL(p)  Num5_0        VABCMO
133900030721     C                   ELSE
134000030721     C                   SETON                                        32
134100030721     C                   EVAL      VABCMO = *zeros
134200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
134300030721     C                             + ' ' + 'VABCMO'
134400030721     C                   ENDIF
134500030721     C                   ENDIF
134600040510     C*
134700040510     C* Reperisco i campi del FIVAT
134800040510     C*** VATNOT
134900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
135000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
135100040510     C                   ENDIF
135200040510     C*** VATTRC
135300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
135400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
135500040510     C                   ENDIF
135600060331     C*** VATNOT_A
135700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
135800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
135900060331     C                   ENDIF
136000060331     C*** VATNOT_B
136100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
136200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
136300060331     C                   ENDIF
136400060331     C*** VATNOT_E
136500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
136600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
136700060331     C                   ENDIF
136800110912     C*** VATNOT_I
136900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
137000110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
137100110912     C                   ENDIF
137200110912     C*** VATNOT_J
137300110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
137400110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
137500110912     C                   ENDIF
137600110912     C*** VATNOT_4
137700110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
137800110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
137900110912     C                   ENDIF
138000131018     C*** VATNOT_S
138100131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
138200131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
138300140507     C                   IF        %trim(%subst(wVATNOT_S:1:16)) = '.' OR
138400140507     C                             %trim(%subst(wVATNOT_S:1:16)) = '0'
138500140507     C                   EVAL      %subst(wVATNOT_S:1:16) = *blanks
138600140507     C                   ENDIF
138700131018     C                   ENDIF
138800141128     C*** VATNOT_Y
138900141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
139000141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
139100141128     C                   ENDIF
139200131018     C*
139300030715     C                   ENDIF
139400030715     C                   EVAL      i = i + 1
139500030715     C                   ENDDO
139600040510     C*
139700040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
139800040510     C                   IF        VABNSP = *zeros
139900040510     C                   SETON                                        31
140000040510     C                   ENDIF
140100040715     C*
140200040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
140300040715     C                   IF        VABLNP = *zeros
140400040715     C                   SETON                                        31
140500040715     C                   ENDIF
140600040510     C*
140700040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
140800040510     C                   IF        VABRMN = *zeros
140900040510     C                   Z-ADD     VABNSP        VABRMN
141000040510     C                   ENDIF
141100040510     C*
141200040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
141300040510     C                   IF        VABRMA = *blanks
141400040714     C                   MOVEL     VABRMN        VABRMA
141500040510     C                   ENDIF
141600070705     C*
141700070705     C* Considerazioni particolari
141800070705     C*** NZD
141900070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
142000070705     C                   EVAL      VABNZD = *blanks
142100070705     C                   ENDIF
142200070705     C*** CAD
142300070705     C                   IF        VABNZD = *blanks
142400070705     C                   EVAL      PiStr=VABCAD
142500070705     C                   EXSR      CHKNUM
142600070705     C                   IF        PiInt=*on
142700070705     C                   Z-ADD     PiVal         Num5_0
142800070705     C                   MOVEL(p)  Num5_0        VABCAD
142900070705     C                   ELSE
143000070705     C                   SETON                                        32
143100070705     C                   EVAL      VABCAD = *zeros
143200070705     C                   EVAL      vinmsg = %trimr(vinmsg)
143300070705     C                             + ' ' + 'VABCAD'
143400070705     C                   ENDIF
143500070705     C                   ENDIF
143600040128     C*
143700040510     C* Se provincia nn valorizzata la reperisco
143800040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
143900040510     C                   IF        VABPRD = *blanks
144000040128     C                   CLEAR                   TISI95DS
144100040128     C                   EVAL      I95TCN = '3'
144200040128     C                   Z-ADD     datcor        I95DAT
144300040128     C                   EVAL      I95NAR = VABNZD
144400040128     C                   EVAL      I95CAP = VABCAD
144500040128     C                   EVAL      I95LOC = VABLOD
144600040128     C                   CALL      'TISI95R'
144700040128     C                   PARM                    TISI95DS
144800040128     C                   EVAL      VABPRD = O95PRV
144900040510     C                   ENDIF
145000040113     C*
145100040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
145200040113     C                   IF        VABCAS > *zeros
145300040119     C                   EVAL      VABVCA = 'EUR'
145400040119     C                   IF        VABCBO = '1'
145500040113     C                   EVAL      VABCBO = '4'
145600040119     C                   ENDIF
145700040119     C                   IF        VABCBO = '2'
145800040119     C                   EVAL      VABCBO = '6'
145900040119     C                   ENDIF
146000040113     C                   ENDIF
146100020204     C*
146200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146300020204     C                   EXSR      CHKIMPDIV
146400030715     C                   ENDIF
146500020204     C*
146600010607     C                   ENDSR
146700010607     C*----------------------------------------------------*
146800010601
146900020204
147000020204     C*----------------------------------------------------*
147100020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147200020204     C*----------------------------------------------------*
147300020204     C     CHKIMPDIV     BEGSR
147400020204     C*
147500020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
147600020204     C                   Z-ADD     *zeros        wrkDec            9 9
147700020204     C*
147800020204     C* Come prima cosa effettuo considerazioni sulla divisa
147900020204     C                   IF        vabIAS > *zeros
148000020204     C                   IF        vabVAS <> 'EUR'
148100020204     C                   EVAL      vabVAS =  'ITL'
148200020204     C                   ENDIF
148300020204     C                   ENDIF
148400020204     C*
148500020204     C                   IF        vabCAS > *zeros
148600020204     C                   IF        vabVCA <> 'EUR'
148700020204     C                   EVAL      vabVCA =  'ITL'
148800020204     C                   ENDIF
148900020204     C                   ENDIF
149000020204     C*
149100020204     C                   IF        vabVMD > *zeros
149200020204     C                   IF        vabVAD <> 'EUR'
149300020204     C                   EVAL      vabVAD =  'ITL'
149400020204     C                   ENDIF
149500020204     C                   ENDIF
149600020204     C*
149700020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
149800020204     C                   Z-ADD     vabIAS        wrkDec
149900020204     C                   IF        wrkDec > *zeros
150000020204     C                   IF        vabVAS = 'ITL'
150100020204     C                   EVAL      vabIAS = *zeros
150200020204     C                   ENDIF
150300020204     C                   ENDIF
150400020204     C*
150500020204     C* Stabilisco se il contrasegno ha decimali valorizzati
150600020204     C                   Z-ADD     vabCAS        wrkDec
150700020204     C                   IF        wrkDec > *zeros
150800020204     C                   IF        vabVCA = 'ITL'
150900020204     C                   EVAL      vabCAS = *zeros
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
151400020204     C                   Z-ADD     vabVMD        wrkDec
151500020204     C                   IF        wrkDec > *zeros
151600020204     C                   IF        vabVAD = 'ITL'
151700020204     C                   EVAL      vabVMD = *zeros
151800020204     C                   ENDIF
151900020204     C                   ENDIF
152000020204     C*
152100020204     C                   ENDSR
152200020204     C***
152300020204
152400010330
152500010330
152600010330     C*----------------------------------------------------*
152700010330     C*  CONTROLLO NUMERICITA' CAMPI
152800010330     C*----------------------------------------------------*
152900010330     C     CHKNUM        BEGSR
153000010330     C*
153100010606     C                   IF        PiDecChr = *blanks
153200030715     C                   EVAL      PiDecChr = CharNUM
153300010606     C                   ENDIF
153400010606     C*
153500010606     C                   CALL(e)   'ISNUMERIC'
153600010330     C                   PARM                    PiStr            30
153700010606     C                   PARM                    PiDecChr          1
153800010330     C                   PARM      *ZEROS        PiVal            30 9
153900010330     C                   PARM      '0'           PiInt             1
154000010330     C                   PARM      '0'           PiNum             1
154100010330     C                   IF        %error
154200010606     C                   EVAL      PiNum=*off
154300010330     C                   ENDIF
154400010330     C*
154500010330     C                   ENDSR
154600010330     C***
154700010330
154800010601
154900010601
155000010601
155100010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155200010601     C     repfil        BEGSR
155300010601     C*
155400010601     C                   if        invfil = *zeros and
155500010601     C                             depfil > *zeros and
155600010629     C                             (vinflg = *blanks or
155700010629     C                              vinflg = *zeros)
155800010601     C
155900010601     C                   eval      invfil = depfil
156000010601     C                   endif
156100010601     C*
156200010601     C                   if        depfil <> invfil and
156300010601     C                             invfil > *zeros
156400010601     C                   eval      flgMulti = '1'
156500010601     C                   if        vinflg = *blanks
156600010601     C                   add       1             cntNonEl
156700010601     C                   endif
156800010601     C                   endif
156900010601     C*
157000010601     C                   if        vinflg = '2'
157100010601     C                   eval      flgStato = '2'
157200010601     C                   endif
157300010601     C*
157400010601     C                   ENDSR
157500010601     C***
157600010601
157700010601
157800010601
157900010330
158000040119      /TITLE Invio dei dati al punto operativo.
158100040119     C     invio         BEGSR
158200040119     C*
158300040119     C* 1° invio FIVAT
158400040119     C                   reset                   dscmz
158500070704     C                   move      winvcmz       cmzdst
158600040119     C                   eval      cmzfld = 'FIVATWWR'
158700040119     C                   eval      cmzmbd = vlrhdl
158800040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
158900040119     C***                if        prmfir = *blanks
159000040119     C                   eval      cmzfla = 'FIVAT00F'
159100040119     C                   eval      cmzmba = 'FIVAT00F'
159200040119     C***                else
159300040119     C***                eval      cmzfla = prmfir
159400040119     C***                eval      cmzmba = prmfir
159500040119     C***                endif
159600040119     C                   eval      cmznrr = *zeros
159700040119     C                   move      §ctrokvt      cmznrr
159800040119     C                   eval      cmzlba = vlrfl1
159900040119     C                   call(e)   'TIS711C'
160000040119     C                   parm                    dscmz
160100040119     C                   parm      *blanks       esito
160200040119     C                   if        %error
160300040119     C                             or cmzerr = '1'
160400040119     C                             or esito  = '1'
160500040119     C                   eval      wrkesito = '3'
160600040119     C                   else
160700040119     C*
160800040119     C* 2° invio FIVAB
160900040119     C                   reset                   dscmz
161000070704     C                   move      winvcmz       cmzdst
161100040119     C                   eval      cmzfld = vlrfou
161200040119     C                   eval      cmzmbd = vlrhdl
161300040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161400040119     C***                if        prmfir = *blanks
161500040119     C                   eval      cmzfla = 'FIVAB00F'
161600040119     C                   eval      cmzmba = 'FIVAB00F'
161700040119     C***                else
161800040119     C***                eval      cmzfla = prmfir
161900040119     C***                eval      cmzmba = prmfir
162000040119     C***                endif
162100040119     C                   eval      cmznrr = *zeros
162200040119     C                   move      §ctrokvb      cmznrr
162300040119     C                   eval      cmzlba = vlrfl1
162400040119     C                   call(e)   'TIS711C'
162500040119     C                   parm                    dscmz
162600040119     C                   parm      *blanks       esito
162700040119     C                   if        %error
162800040119     C                             or cmzerr = '1'
162900040119     C                             or esito  = '1'
163000040119     C                   eval      wrkesito = '3'
163100040119     C                   endif
163200040119     C                   endif
163300040119     C*
163400040119     C                   ENDSR
163500040119     C***
163600010601
163700010601
163800010601
163900010601
164000010601      /TITLE Invio dei dati al punto operativo.
164100010601     C     opeini        BEGSR
164200010601     C*
164300010601     C* Inizializzo flag e contatori operativi
164400010601     C                   movel     '0'           flgGiro           1
164500010601     C                   movel     '0'           flgMulti          1
164600010601     C                   movel     '1'           flgStato          1
164700010615     C                   movel     '0'           flgOk             1
164800010601     C                   z-add     *zeros        cntNonEl         10 0
164900010601     C                   z-add     *zeros        depfil            3 0
165000010601     C                   z-add     *zeros        invfil            3 0
165100010601     C*
165200010601     C                   ENDSR
165300010601     C***
165400070326
165500070326
165600070326
165700070326
165800070326     C     *pssr         BEGSR
165900070329     C*
166000070329     C                   if        %open(tivin00r)
166100070329     C                   close     tivin00r
166200070329     C                   endif
166300070329     C                   if        %open(fivabwwr)
166400070329     C                   close     fivabwwr
166500070329     C                   endif
166600070329     C                   if        %open(fivatwwr)
166700070329     C                   close     fivatwwr
166800070329     C                   endif
166900070326     C*
167000070326     C* Effettuo la chiamata al CLLE preposto
167100070326     C                   call(e)   'TITVVTC'
167200070326     C                   parm                    parccm
167300070326     C                   parm                    parmbr
167400070326     C                   parm      '2'           paropz
167500070326     C*
167600070326     C                   eval      wrkesito = '2'
167700070404     C*
167800070404     C                   seton                                        LR
167900070326     C*
168000070326     C                   ENDSR     '*CANCL'
168100070326     C***
168200070326
168300070326
168400010330
168500010330
168600000613     C     *inzsr        BEGSR
168700990910     C*
168800990910     C     *entry        plist
168900990920     C                   parm                    tivlrds
169000990921     C                   parm      wrkesito      esito
169100000724     C                   parm                    prmlit
169200000710     C                   parm                    prmfir
169300010330     C*
169400010330     C* CALCOLA LA DATA CORRENTE
169500010330     C                   time                    wn14             14 0
169600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
169700010330     C                   z-add     wn8           g08dat
169800010330     C                   z-add     *zeros        g08inv
169900010330     C                   movel     '0'           g08err
170000010330     C                   call      'XSRDA8'
170100010330     C                   parm                    wlbda8
170200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
170300000613     C*
170400000613     C                   ENDSR
170500000613     C***
170600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170700040510
170800040122
170900040113
