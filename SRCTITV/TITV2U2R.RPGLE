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
005900031201     D*------------------
006000031201     D* Costanti
006100031201     D*------------------
006200031201     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
006300031201     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
006400010330
006500010330
006600010330
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000010601     C*
007100010601     C                   exsr      opeini
007200010605     C                   exsr      rwvab
007300010601     C*
007400010601     C                   seton                                        lr
007500010601
007600010601
007700010601
007800010601     C*--------------------------------------------------------
007900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010601     C*--------------------------------------------------------
008100010601     C     PREELA        BEGSR
008200010601     C*
008300010601     C* SE OCCORRE SPEDIRE IN FILIALE
008400010601     C                   if        invfil <> *zeros and
008500010601     C                             flgGiro = '0'
008600010601     C*
008700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010601     C                   eval      flgGiro = '1'
008900010601     C*
009000010601     C                   endif
009100010601     C*
009200010601     C                   ENDSR
009300010601     C***
009400010601
009500010601
009600010601
009700010601
009800010601     C*--------------------------------------------------------
009900010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010000010601     C*--------------------------------------------------------
010100010601     C     ENDELA        BEGSR
010200040119     C*
010300040119     C* Effettuo la chiamata al CLLE preposto
010400040119     C                   call(e)   'TITVVTC'
010500040510     C                   parm                    parccm
010600040119     C                   parm                    parmbr
010700040119     C                   parm      '2'           paropz
010800050201     C*
010900050201     C* Effettuo lancio TISI95 solo x chiusura
011000050201     C                   CLEAR                   TISI95DS
011100050201     C                   EVAL      I95TLA = 'C'
011200050201     C                   CALL      'TISI95R'
011300050201     C                   PARM                    TISI95DS
011400000616     C*
011500010601     C                   ENDSR
011600010601     C***
011700000613
011800010601
011900010601
012000010330     C*--------------------------------------------------------
012100021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
012200010330     C*--------------------------------------------------------
012300010605     C     RWVAB         BEGSR
012400010330     C*
012500010330     C                   if        not %open(tivin00r)
012600010330     C                   open      tivin00r
012700010330     C                   endif
012800021025     C                   if        not %open(fivabwwr)
012900021025     C                   open      fivabwwr
013000010330     C                   endif
013100040119     C*
013200040119     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013300040119     C                   exsr      prevat
013400040119     C*
013500040119     C                   if        chkcall = '0'
013600040119     C*
013700040119     C                   if        not %open(fivatwwr)
013800040119     C                   open      fivatwwr
013900040119     C                   endif
014000010330     C*
014100040119     C                   clear                   §CTROKVB          7 0
014200040119     C                   clear                   §CTROKVT          7 0
014300040119     C                   clear                   §CTRMO            7 0
014400040119     C                   clear                   §CTRNO            7 0
014500010330     C*
014600030822     C                   do        *HIVAL
014700010330     C*
014800030822     C                   read      tivin00r                               70
014900010618     C*
015000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015100010618     C                   if        vinflg = '1'
015200010618     C                   eval      flgOk = '1'
015300010618     C                   endif
015400040510     C*
015500040510     C* Verifico che il record nn contenga unicamente i caratteri d separatore campi
015600040510     C                   z-add     *zeros        wLenVINDTA        4 0
015700040510     C                   z-add     *zeros        wFlgVINDTA        1 0
015800090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
015900040510     C                   eval      wLenVINDTA = wLenVINDTA + 1
016000090403     C                   if        %subst(%trim(vindta):wLenVINDTA:1)<>CharCSV
016100040510     C                   z-add     1             wFlgVINDTA
016200040510     C                   leave
016300040510     C                   endif
016400040510     C                   enddo
016500010618     C*
016600040510     C                   if        vindta > *blanks AND
016700040510     C                             wFlgVINDTA = 1
016800010330     C                   add       1             rrnum
016900010330     C*
017000010601     C                   if        *in70 = *off and
017100010330     C                             (vinflg = *blanks
017200010330     C                              or vinflg = '0'
017300010330     C                              or vinflg = '2')
017400010330     C*
017500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017600010711     C                   if        vinflg = *blanks or vinflg = '0'
017700010711     C                   clear                   vinmsg
017800010711     C                   endif
017900121107     C*
018000121107     C                   exsr      inzvar
018100090324     C*
018200090324     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
018300090324     C* separatori campi
018400090324     C                   if        CharTXT <> *blanks
018500090324     C                   z-add     *zeros        wLenVINDTA        4 0
018600090324     C                   z-add     *zeros        wFlgVINDTA        1 0
018700090324     C                   setoff                                       80
018800090324     C                   dow       wLenVINDTA < %len(%trim(vindta))
018900090324     C                   eval      wLenVINDTA = wLenVINDTA + 1
019000090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019100090324     C                   if        not *in80
019200090324     C                   seton                                        80
019300090324     C                   else
019400090324     C                   setoff                                       80
019500090324     C                   endif
019600090324     C                   endif
019700090324     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
019800090324     C                             and *in80 = *on
019900090324     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020000090324     C                   endif
020100090324     C                   enddo
020200090324     C                   endif
020300090324     C*
020400010605     C                   exsr      impvab
020500010601     C*
020600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020700010605     C                   eval      depfil = VABLNP
020800010601     C                   exsr      repfil
020900010601     C                   if        depfil = invfil
021000021025     C                   if        vlrpoi = 999
021100030822     C                   move(P)   invfil        VABFGS
021200070704     C                   move(P)   invfil        winvcmz           3
021300021025     C                   else
021400030822     C                   move(P)   vlrpoi        VABFGS
021500070704     C                   move(P)   vlrpoi        winvcmz
021600021025     C                   endif
021700010601     C*
021800010601     C                   exsr      PREELA
021900010601     C*
022000010604     C* Ebbene...
022100010604     C*
022200040119     C  N31              add       1             §CTROKVB
022300040119     C   32              add       1             §CTRMO
022400040119     C   31              add       1             §CTRNO
022500030822     C                   if        wGiro = 2
022600040510     C* Scrivo sempre il VAT
022700040510     C  N31              exsr      wrivat
022800040510     C* Scrivo il VAB solo a rottura d spedizione
022900040119     C                   if        depspe =  *zeros OR
023000040119     C                             depspe <> vabNSP
023100040119     C  N31              exsr      wrivab
023200040119     C  N31              eval      depspe = vabNSP
023300040119     C                   endif
023400040119     C                   endif
023500010604     C*
023600010604     C                   if        *in31 = *off and
023700010604     C                             *in32 = *off
023800010604     C                   eval      vinflg = '1'
023900010604     C                   else
024000010604     C                   eval      vinflg = '2'
024100010604     C                   endif
024200010604     C                   endif
024300010601     C*
024400010604     C                   endif
024500010604     C*
024600010330     C                   else
024700010330     C                   eval      vinflg = '1'
024800010330     C                   endif
024900010601     C*
025000010601     C  N70              update    tivin000
025100010330     C*
025200030822     C  N70              enddo
025300040119     C*
025400040119     C                   endif                                                  (endif - chkcall)
025500010601     C*
025600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
025700010601     C                   if        cntNonEl = *zeros or
025800010601     C                             flgMulti = '0'
025900010330     C* Se non ci sono record con errori ...
026000010601     C                   if        §ctrno = 0 and
026100010604     C                             §ctrmo = 0 and
026200010601     C                             flgStato <> '2'
026300010330     C* ... restituisco esito OK.
026400010330     C                   eval      wrkesito = '0'
026500010330     C                   else
026600040119     C                   if        §ctrokvb > 0
026700010330     C                   eval      wrkesito = '1'
026800010330     C                   else
026900010615     C                   if        flgOk = '0'
027000010615     C                   eval      wrkesito = '2'
027100010615     C                   else
027200010615     C                   eval      wrkesito = '6'
027300010615     C                   endif
027400010330     C                   endif
027500010330     C                   endif
027600010601     C                   else
027700010601     C                   eval      wrkesito = '9'
027800010601     C                   endif
027900010330     C*
028000010330     C                   if        %open(tivin00r)
028100010330     C                   close     tivin00r
028200010330     C                   endif
028300021025     C                   if        %open(fivabwwr)
028400021025     C                   close     fivabwwr
028500010330     C                   endif
028600040512     C                   if        %open(fivatwwr)
028700040512     C                   close     fivatwwr
028800040512     C                   endif
028900010601     C*
029000010601     C                   if        vlrpoi <> 999
029100010601     C                   eval      invfil = vlrpoi
029200010601     C                   endif
029300010330     C*
029400040119     C                   if        §ctrokvb > 0
029500010601     C                             and invfil > *zeros
029600010330     C                   exsr      invio
029700010330     C                   endif
029800010601     C*
029900010618     C                   if        flgGiro = '1'
030000010601     C                   exsr      endela
030100010618     C                   endif
030200010330     C*
030300010330     C                   ENDSR
030400010330     C***
030500040119
030600040119
030700040119
030800040119     C*----------------------------------------------------*
030900040119     C*  SCARICAMENTO BUFFER RECORDS VAB
031000040119     C*----------------------------------------------------*
031100040510     C     WRIVAB        BEGSR
031200040119     C*
031300060331     C* Gestisco l'eventuale rottura x numero spedizione
031400060331     C                   if        VABNSP <> wVABNSP
031500070301     C                   if        wVATNOT_E <> *blanks
031600110419     C                   if        VABCTM = *blanks
031700070301     C                   eval      VABCTM = '7Q'
031800110419     C                   endif
031900070301     C                   endif
032000040119     C                   write     fivab000                                     => scarico il VAB
032100060331     C                   eval      wVABNSP = VABNSP
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
033700060512     C                   eval      VATNSP = VABNSP
033800060512     C                   eval      VATLNP = VABLNP
033900040714     C*
034000040714     C* Solo se VATNOT valorizzato scarico il buffer
034100040714     C                   if        VATNOT <> *blanks
034200040119     C*
034300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
034400040510     C                   if        VATTRC = *blanks
034500040510     C                   eval      VATTRC = 'E'
034600040510     C                   endif
034700040510     C                   write     FIVAT000
034800040119     C                   add       1             §CTROKVT
034900040714     C*
035000040714     C                   endif
035100060331     C*
035200060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
035300060331     C                   if        wVATNOT_A <> *blanks
035400060331     C                   eval      VATNOT = wVATNOT_A
035500060331     C                   eval      VATTRC = 'A'
035600060331     C                   write     FIVAT000
035700060331     C                   add       1             §CTROKVT
035800060331     C                   endif
035900060331     C*
036000060331     C                   if        wVATNOT_B <> *blanks
036100060331     C                   eval      VATNOT = wVATNOT_B
036200060331     C                   eval      VATTRC = 'B'
036300060331     C                   write     FIVAT000
036400060331     C                   add       1             §CTROKVT
036500060331     C                   endif
036600060331     C*
036700060331     C                   if        wVATNOT_E <> *blanks
036800060331     C                   eval      VATNOT = wVATNOT_E
036900060331     C                   eval      VATTRC = 'E'
037000060331     C                   write     FIVAT000
037100060331     C                   add       1             §CTROKVT
037200060331     C                   endif
037300110912     C*
037400110912     C                   if        wVATNOT_I <> *blanks
037500110912     C                   eval      VATNOT = wVATNOT_I
037600110912     C                   eval      VATTRC = 'I'
037700110912     C                   write     FIVAT000
037800110912     C                   add       1             §CTROKVT
037900110912     C                   endif
038000110912     C*
038100110912     C                   if        wVATNOT_J <> *blanks
038200110912     C                   eval      VATNOT = wVATNOT_J
038300110912     C                   eval      VATTRC = 'J'
038400110912     C                   write     FIVAT000
038500110912     C                   add       1             §CTROKVT
038600110912     C                   endif
038700110912     C*
038800110912     C                   if        wVATNOT_4 <> *blanks
038900110912     C                   eval      VATNOT = wVATNOT_4
039000110912     C                   eval      VATTRC = '4'
039100110912     C                   write     FIVAT000
039200110912     C                   add       1             §CTROKVT
039300110912     C                   endif
039400131018     C*
039500131018     C                   if        wVATNOT_S <> *blanks
039600131018     C                   eval      VATNOT = wVATNOT_S
039700131018     C                   eval      VATTRC = 'S'
039800131018     C                   write     FIVAT000
039900131018     C                   add       1             §CTROKVT
040000131018     C                   endif
040100141128     C*
040200141128     C                   if        wVATNOT_Y <> *blanks
040300141128     C                   eval      VATNOT = wVATNOT_Y
040400141128     C                   eval      VATTRC = 'Y'
040500141128     C                   write     FIVAT000
040600141128     C                   add       1             §CTROKVT
040700141128     C                   endif
040800040119     C*
040900040119     C                   ENDSR
041000040119
041100040119
041200040119
041300040119     C*----------------------------------------------------*
041400040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
041500040119     C*----------------------------------------------------*
041600040119     C     PREVAT        BEGSR
041700040119     C*
041800040119     C* Compongo il nome del membro da dare al FIVATWWR
041900040119     C                   eval      parmbr = vlrhdl
042000040119     C                   movel     'M'           parmbr
042100040510     C                   eval      parccm = vlrksc
042200040119     C                   eval      paropz = '1'
042300040119     C* Effettuo la chiamata al CLLE preposto
042400040119     C                   call(e)   'TITVVTC'
042500040119     C                   parm                    parccm
042600040119     C                   parm                    parmbr
042700040119     C                   parm                    paropz
042800040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
042900040119     C                   if        %error
043000040119     C                   movel     '1'           chkcall
043100040119     C                   else
043200040119     C                   movel     '0'           chkcall
043300040119     C                   endif
043400040119     C*
043500040119     C                   ENDSR
043600010601
043700010601
043800010601
043900010330     C*----------------------------------------------------*
044000030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044100010330     C*----------------------------------------------------*
044200040119     C     INZVAR        BEGSR
044300010330     C*
044400030822     C* Inizializzo variabili di wrk
044500020204     C                   Z-ADD     *zeros        Num5_0            5 0
044600090403     C                   z-add     *zeros        invfil
044700060331     C                   MOVEL     *blanks       wVATNOT_A
044800060331     C                   MOVEL     *blanks       wVATNOT_B
044900060331     C                   MOVEL     *blanks       wVATNOT_E
045000110912     C                   MOVEL     *blanks       wVATNOT_I
045100110912     C                   MOVEL     *blanks       wVATNOT_J
045200110912     C                   MOVEL     *blanks       wVATNOT_4
045300131018     C                   MOVEL     *blanks       wVATNOT_S
045400141128     C                   MOVEL     *blanks       wVATNOT_Y
045500081014     C*
045600081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
045700081014     C                   CLEAR                   FIVAB000
045800081014     C                   CLEAR                   FIVAT000
045900030822     C*
046000081014     C* Inizializzo la schiera d wrk x i dati
046100030822     C                   CLEAR                   SkSplitCSV
046200030822     C                   EVAL      i = 1
046300030822     C                   EVAL      posDa = *zeros
046400030822     C                   EVAL      posA  = *zeros
046500030822     C*
046600030822     C* Reimposto i valori di default
046700030822     C                   EXSR      DEFCAM
046800030822     C*
046900010330     C                   ENDSR
047000010330     C*----------------------------------------------------*
047100030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047200010330     C*----------------------------------------------------*
047300010330     C     DEFCAM        BEGSR
047400030822     C*
047500030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
047600030715     C* e delimitatore testo.
047700030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
047800030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
047900030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048000090324     C*
048100090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
048200090324     C                   EVAL      CharSOS = CharNUM
048300030822     C*
048400081007     C* Reperisco il parametro che mi indica se tuare o meno la stampa in filiale
048500030715     C                   IF        %subst(vlrppt:1:1) = 'S'
048600081007     C                   SETON                                        40
048700030715     C                   ELSE
048800081007     C                   SETOFF                                       40
048900030715     C                   ENDIF
049000081007     C*
049100081007     C* Imposto sempre e cmq che la stampa NN vada in filiale
049200081007     C                   SETOFF                                       50
049300030822     C*
049400030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049500030822     C                   EVAL      posDaDft = 1
049600030822     C                   EVAL      posADft  = 0
049700030822     C                   EVAL      wGiroDft = 0
049800030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
049900030822     C                             posDaDft > 0
050000030822     C*
050100030822     C* Gestisco il 1° giro
050200030822     C                   IF        wGiroDft = 0
050300030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050400030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050500030822     C* Incremento il contatore dei "giri"
050600030822     C                   EVAL      wGiroDft = 1
050700030822     C                   ELSE
050800030822     C                   EVAL      posDaDft = posADft
050900030822     C                   ENDIF
051000030822     C* Eseguo lo scan x trovare la fine del campo corrente
051100030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051200030822     C*
051300030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051400030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051500030822     C                   IF        posDaDft > 0 AND
051600030822     C                             posADft  > 0
051700030822     C* NCL
051800030822     C                   IF        %subst(
051900030822     C                             %subst(vlrppt:posDaDft+1:
052000030822     C                             posADft-posDaDft-1):1:3)
052100030822     C                             = 'NCL'
052200030822     C                   EVAL      PiStr=%trim(%subst(
052300030822     C                             %subst(vlrppt:posDaDft+1:
052400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052500030822     C                   EXSR      CHKNUM
052600030822     C                   IF        PiInt=*on
052700030822     C                   Z-ADD     PiVal         VABNCL
052800030822     C                   ENDIF
052900030822     C                   ENDIF
053000030822     C* CCM
053100030822     C                   IF        %subst(
053200030822     C                             %subst(vlrppt:posDaDft+1:
053300030822     C                             posADft-posDaDft-1):1:3)
053400030822     C                             = 'CCM'
053500030822     C                   EVAL      PiStr=%trim(%subst(
053600030822     C                             %subst(vlrppt:posDaDft+1:
053700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
053800030822     C                   EXSR      CHKNUM
053900030822     C                   IF        PiInt=*on
054000030822     C                   Z-ADD     PiVal         VABCCM
054100030822     C                   ENDIF
054200030822     C                   ENDIF
054300030822     C* LNP
054400030822     C                   IF        %subst(
054500030822     C                             %subst(vlrppt:posDaDft+1:
054600030822     C                             posADft-posDaDft-1):1:3)
054700030822     C                             = 'LNP'
054800030822     C                   EVAL      PiStr=%trim(%subst(
054900030822     C                             %subst(vlrppt:posDaDft+1:
055000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055100030822     C                   EXSR      CHKNUM
055200030822     C                   IF        PiInt=*on
055300030822     C                   Z-ADD     PiVal         VABLNP
055400030822     C                   ENDIF
055500030822     C                   ENDIF
055600030822     C* NRS
055700030822     C                   IF        %subst(
055800030822     C                             %subst(vlrppt:posDaDft+1:
055900030822     C                             posADft-posDaDft-1):1:3)
056000030822     C                             = 'NRS'
056100030822     C                   EVAL      PiStr=%trim(%subst(
056200030822     C                             %subst(vlrppt:posDaDft+1:
056300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056400030822     C                   EXSR      CHKNUM
056500030822     C                   IF        PiInt=*on
056600030822     C                   Z-ADD     PiVal         VABNRS
056700030822     C                   ENDIF
056800030822     C                   ENDIF
056900030822     C* CTR
057000030822     C                   IF        %subst(
057100030822     C                             %subst(vlrppt:posDaDft+1:
057200030822     C                             posADft-posDaDft-1):1:3)
057300030822     C                             = 'CTR'
057400030822     C                   EVAL      PiStr=%trim(%subst(
057500030822     C                             %subst(vlrppt:posDaDft+1:
057600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057700030822     C                   EXSR      CHKNUM
057800030822     C                   IF        PiInt=*on
057900030822     C                   Z-ADD     PiVal         VABCTR
058000030822     C                   ENDIF
058100030822     C                   ENDIF
058200030822     C* PKB
058300030822     C                   IF        %subst(
058400030822     C                             %subst(vlrppt:posDaDft+1:
058500030822     C                             posADft-posDaDft-1):1:3)
058600030822     C                             = 'PKB'
058700030822     C                   EVAL      PiStr=%trim(%subst(
058800030822     C                             %subst(vlrppt:posDaDft+1:
058900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059000030822     C                   EXSR      CHKNUM
059100030822     C                   IF        PiNum=*on
059200030822     C                   Z-ADD     PiVal         VABPKB
059300030822     C                   ENDIF
059400030822     C                   ENDIF
059500030822     C* VLB
059600030822     C                   IF        %subst(
059700030822     C                             %subst(vlrppt:posDaDft+1:
059800030822     C                             posADft-posDaDft-1):1:3)
059900030822     C                             = 'VLB'
060000030822     C                   EVAL      PiStr=%trim(%subst(
060100030822     C                             %subst(vlrppt:posDaDft+1:
060200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060300030822     C                   EXSR      CHKNUM
060400030822     C                   IF        PiNum=*on
060500030822     C                   Z-ADD     PiVal         VABVLB
060600030822     C                   ENDIF
060700030822     C                   ENDIF
060800030822     C* QFT
060900030822     C                   IF        %subst(
061000030822     C                             %subst(vlrppt:posDaDft+1:
061100030822     C                             posADft-posDaDft-1):1:3)
061200030822     C                             = 'QFT'
061300030822     C                   EVAL      PiStr=%trim(%subst(
061400030822     C                             %subst(vlrppt:posDaDft+1:
061500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061600030822     C                   EXSR      CHKNUM
061700030822     C                   IF        PiNum=*on
061800030822     C                   Z-ADD     PiVal         VABQFT
061900030822     C                   ENDIF
062000030822     C                   ENDIF
062100030822     C* CBO
062200030822     C                   IF        %subst(
062300030822     C                             %subst(vlrppt:posDaDft+1:
062400030822     C                             posADft-posDaDft-1):1:3)
062500030822     C                             = 'CBO'
062600030822     C                   EVAL      VABCBO=%trim(%subst(
062700030822     C                             %subst(vlrppt:posDaDft+1:
062800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062900030822     C                   ENDIF
063000030822     C* TSP
063100030822     C                   IF        %subst(
063200030822     C                             %subst(vlrppt:posDaDft+1:
063300030822     C                             posADft-posDaDft-1):1:3)
063400030822     C                             = 'TSP'
063500030822     C                   EVAL      VABTSP=%trim(%subst(
063600030822     C                             %subst(vlrppt:posDaDft+1:
063700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063800030822     C                   ENDIF
063900030822     C* VAS
064000030822     C                   IF        %subst(
064100030822     C                             %subst(vlrppt:posDaDft+1:
064200030822     C                             posADft-posDaDft-1):1:3)
064300030822     C                             = 'VAS'
064400030822     C                   EVAL      VABVAS=%trim(%subst(
064500030822     C                             %subst(vlrppt:posDaDft+1:
064600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064700030822     C                   ENDIF
064800030822     C* VCA
064900030822     C                   IF        %subst(
065000030822     C                             %subst(vlrppt:posDaDft+1:
065100030822     C                             posADft-posDaDft-1):1:3)
065200030822     C                             = 'VCA'
065300030822     C                   EVAL      VABVCA=%trim(%subst(
065400030822     C                             %subst(vlrppt:posDaDft+1:
065500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065600030822     C                   ENDIF
065700030822     C* GCA
065800030822     C                   IF        %subst(
065900030822     C                             %subst(vlrppt:posDaDft+1:
066000030822     C                             posADft-posDaDft-1):1:3)
066100030822     C                             = 'GCA'
066200030822     C                   EVAL      VABGCA=%trim(%subst(
066300030822     C                             %subst(vlrppt:posDaDft+1:
066400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066500030822     C                   ENDIF
066600030822     C* CTM
066700030822     C                   IF        %subst(
066800030822     C                             %subst(vlrppt:posDaDft+1:
066900030822     C                             posADft-posDaDft-1):1:3)
067000030822     C                             = 'CTM'
067100030822     C                   EVAL      VABCTM=%trim(%subst(
067200030822     C                             %subst(vlrppt:posDaDft+1:
067300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067400030822     C                   ENDIF
067500030822     C* FFD
067600030822     C                   IF        %subst(
067700030822     C                             %subst(vlrppt:posDaDft+1:
067800030822     C                             posADft-posDaDft-1):1:3)
067900030822     C                             = 'FFD'
068000030822     C                   EVAL      VABFFD=%trim(%subst(
068100030822     C                             %subst(vlrppt:posDaDft+1:
068200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068300030822     C                   ENDIF
068400030822     C* VAD
068500030822     C                   IF        %subst(
068600030822     C                             %subst(vlrppt:posDaDft+1:
068700030822     C                             posADft-posDaDft-1):1:3)
068800030822     C                             = 'VAD'
068900030822     C                   EVAL      VABVAD=%trim(%subst(
069000030822     C                             %subst(vlrppt:posDaDft+1:
069100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069200030822     C                   ENDIF
069300030822     C* GMA
069400030822     C                   IF        %subst(
069500030822     C                             %subst(vlrppt:posDaDft+1:
069600030822     C                             posADft-posDaDft-1):1:3)
069700030822     C                             = 'GMA'
069800030822     C                   EVAL      VABGMA=%trim(%subst(
069900030822     C                             %subst(vlrppt:posDaDft+1:
070000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070100030822     C                   ENDIF
070200030822     C* GGA
070300030822     C                   IF        %subst(
070400030822     C                             %subst(vlrppt:posDaDft+1:
070500030822     C                             posADft-posDaDft-1):1:3)
070600030822     C                             = 'GGA'
070700030822     C                   EVAL      VABGGA=%trim(%subst(
070800030822     C                             %subst(vlrppt:posDaDft+1:
070900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071000030822     C                   ENDIF
071100030822     C* GVA
071200030822     C                   IF        %subst(
071300030822     C                             %subst(vlrppt:posDaDft+1:
071400030822     C                             posADft-posDaDft-1):1:3)
071500030822     C                             = 'GVA'
071600030822     C                   EVAL      VABGVA=%trim(%subst(
071700030822     C                             %subst(vlrppt:posDaDft+1:
071800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071900030822     C                   ENDIF
072000030822     C* TC1
072100030822     C                   IF        %subst(
072200030822     C                             %subst(vlrppt:posDaDft+1:
072300030822     C                             posADft-posDaDft-1):1:3)
072400030822     C                             = 'TC1'
072500030822     C                   EVAL      VABTC1=%trim(%subst(
072600030822     C                             %subst(vlrppt:posDaDft+1:
072700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072800030822     C                   ENDIF
072900030822     C* TC2
073000030822     C                   IF        %subst(
073100030822     C                             %subst(vlrppt:posDaDft+1:
073200030822     C                             posADft-posDaDft-1):1:3)
073300030822     C                             = 'TC2'
073400030822     C                   EVAL      VABTC2=%trim(%subst(
073500030822     C                             %subst(vlrppt:posDaDft+1:
073600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073700030822     C                   ENDIF
073800080516     C* TIC
073900080516     C                   IF        %subst(
074000080516     C                             %subst(vlrppt:posDaDft+1:
074100080516     C                             posADft-posDaDft-1):1:3)
074200080516     C                             = 'TIC'
074300080516     C                   EVAL      VABTIC=%trim(%subst(
074400080516     C                             %subst(vlrppt:posDaDft+1:
074500080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074600080516     C                   ENDIF
074700040714     C* VATTRC
074800040714     C                   IF        %subst(
074900040714     C                             %subst(vlrppt:posDaDft+1:
075000040714     C                             posADft-posDaDft-1):1:3)
075100040714     C                             = 'TRC'
075200040714     C                   EVAL      VATTRC=%trim(%subst(
075300040714     C                             %subst(vlrppt:posDaDft+1:
075400040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075500040714     C                   ENDIF
075600030822     C* ...
075700030822     C                   ENDIF
075800030822     C                   ENDDO
075900020204     C*
076000010330     C                   ENDSR
076100010607     C*----------------------------------------------------*
076200021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076300010607     C*----------------------------------------------------*
076400010607     C     IMPVAB        BEGSR
076500010607     C*
076600010607     C                   SETOFF                                       3132
076700030515     C*
076800010607     C                   MOVEL     datcor        VABAAS
076900010607     C                   MOVE      datcor        VABMGS
077000010607     C*
077100030715     C**********
077200030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077300030715     C**********
077400030715     C                   IF        wGiro = *zeros
077500040113     C*
077600040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
077700040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
077800040510     C                             %trim(intcol(3))<>*blanks
077900040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078000040113     C                                      %trim(intcol(3))
078100040510     C                   ENDIF
078200031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078300031201     C     minu:maiu     XLATE     vindta        vindta
078400030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078500030715     C                   DOW       posDa <= %len(%trim(vindta))
078600030715     C*
078700030715     C* Gestisco il 1° campo
078800030715     C                   IF        i = 1
078900030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079000030715     C                   EVAL      posDa = 1
079100030715     C* Eseguo lo scan x trovare la fine del primo campo
079200030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079300030715     C* A questo punto "estraggo" il campo corrente
079400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079500030715     C                                                    (posA-posDa))
079600030715     C* X i campi successivi al 1°
079700030715     C                   ELSE
079800030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
079900030715     C                   EVAL      posDa = posA + 1
080000030715     C* Eseguo lo scan x trovare la fine del campo corrente
080100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080200030715     C* Gestisco l'ultimo campo
080300030715     C                   IF        posA = *zeros
080400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080500030715     C* A questo punto "estraggo" il campo corrente
080600030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
080700030715     C                                                    (posA-posDa))
080800030715     C                   LEAVE
080900030715     C                   ELSE
081000030715     C* A questo punto "estraggo" il campo corrente
081100030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081200030715     C                                                    (posA-posDa))
081300030715     C                   ENDIF
081400030715     C                   ENDIF
081500030715     C* Incremento il contatore d campo
081600030715     C                   EVAL      i = i +1
081700030715     C                   ENDDO
081800030715     C*
081900030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082000030715     C* delimitazione testo
082100030715     C                   EVAL      i = 1
082200030715     C                   DOW       i < %elem(SkSplitFLD)
082300030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082400030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082500030715     C                   EVAL      i = i + 1
082600030715     C                   ENDDO
082700030715     C                   EVAL      wGiro = 1
082800030715     C                   ELSE
082900030715     C                   EVAL      wGiro = 2
083000031204     C**********
083100031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
083200031204     C* d separatore campo
083300031204     C**********
083400031204     C                   EVAL      vindta = %trim(vindta)
083500031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
083600031204     C                   EVAL      vindta = %subst(vindta:2)
083700031204     C                   ENDDO
083800031204     C*
083900031204     C                   Z-ADD     *zeros        lunghInput        4 0
084000031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084100031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
084200031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
084300031204     C                   EVAL      lunghInput = %len(%trim(vindta))
084400031204     C                   ENDDO
084500030715     C**********
084600030715     C* Effettuo lo split del campo dati d input
084700030715     C**********
084800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
084900030715     C                   DOW       posDa <= %len(%trim(vindta))
085000030715     C*
085100030715     C* Gestisco il 1° campo
085200030715     C                   IF        i = 1
085300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
085400030715     C                   EVAL      posDa = 1
085500030715     C* Eseguo lo scan x trovare la fine del primo campo
085600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
085700030715     C* A questo punto "estraggo" il campo corrente
085800030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
085900030715     C                                                    (posA-posDa))
086000030715     C* X i campi successivi al 1°
086100030715     C                   ELSE
086200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
086300030715     C                   EVAL      posDa = posA + 1
086400031204     C* Verifico che nn vi sia il campo nn valorizzato
086500031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
086600031204     C* Se campo nn valorizzato skippo al prossimo
086700031204     C                   EVAL      SkSplitCSV(i) = *blanks
086800031204     C                   EVAL      posA  = posA + 1
086900031204     C                   ELSE
087000030715     C* Eseguo lo scan x trovare la fine del campo corrente
087100030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
087200030715     C* Gestisco l'ultimo campo
087300030715     C                   IF        posA = *zeros
087400030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
087500030715     C* A questo punto "estraggo" il campo corrente
087600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087700030715     C                                                    (posA-posDa))
087800030715     C                   LEAVE
087900030715     C                   ELSE
088000030715     C* A questo punto "estraggo" il campo corrente
088100030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
088200030715     C                                                    (posA-posDa))
088300030715     C                   ENDIF
088400031204     C                   ENDIF
088500030715     C                   ENDIF
088600030715     C* Incremento il contatore d campo
088700030715     C                   EVAL      i = i +1
088800030715     C                   ENDDO
088900030715     C*
089000030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
089100030715     C* delimitazione testo
089200030715     C                   EVAL      i = 1
089300030715     C                   DOW       i < %elem(SkSplitCSV)
089400030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
089500030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
089600030715     C                   EVAL      i = i + 1
089700030715     C                   ENDDO
089800030715     C*
089900030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
090000030715     C                   EVAL      i = 1
090100030715     C                   DOW       i < %elem(SkSplitFLD)
090200030715     C*
090300030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
090400030715     C                   IF        SkSplitFLD(i) <> *blanks AND
090500030715     C                             SkSplitCSV(i) <> *blanks
090600030721     C*** ATB
090700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
090800030721     C                   EVAL      VABATB = SkSplitCSV(i)
090900030721     C                   ENDIF
091000030715     C*** RSD
091100030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
091200030715     C                   EVAL      VABRSD = SkSplitCSV(i)
091300030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
091400030715     C     '@':'A'       XLATE     VABRSD        VABRSD
091500030715     C* ==
091600030715     C                   ENDIF
091700030721     C*** RD2
091800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
091900030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
092000030721     C                   ENDIF
092100030715     C*** IND
092200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
092300030715     C                   EVAL      VABIND = SkSplitCSV(i)
092400030715     C                   ENDIF
092500030715     C*** LOD
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
092700030715     C                   EVAL      VABLOD = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030715     C*** PRD
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
093100030715     C                   EVAL      VABPRD = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030721     C*** NZD
093400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
093500030721     C                   EVAL      VABNZD = SkSplitCSV(i)
093600030721     C                   ENDIF
093700030715     C*** RMA
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
093900030715     C                   EVAL      VABRMA = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** NOT
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
094300030715     C                   EVAL      VABNOT = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** NT2
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
094700030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030715     C*** VCA
095000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
095100030715     C                   EVAL      VABVCA = SkSplitCSV(i)
095200030715     C                   ENDIF
095300030715     C*** VAS
095400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
095500030715     C                   EVAL      VABVAS = SkSplitCSV(i)
095600030715     C                   ENDIF
095700030715     C*** FFD
095800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
095900030715     C                   EVAL      VABFFD = SkSplitCSV(i)
096000030715     C                   ENDIF
096100030721     C*** GC1
096200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
096300030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
096400100304     C                   IF        %subst(VABGC1:1) = 'P' or
096500100304     C                             %subst(VABGC1:1) = 'M'
096600100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
096700100304     C                   ENDIF
096800030721     C                   ENDIF
096900030721     C*** GC2
097000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
097100030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
097200100304     C                   IF        %subst(VABGC2:1) = 'P' or
097300100304     C                             %subst(VABGC2:1) = 'M'
097400100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
097500100304     C                   ENDIF
097600030721     C                   ENDIF
097700030721     C*** TSP
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
097900030721     C                   EVAL      VABTSP = SkSplitCSV(i)
098000030721     C                   ENDIF
098100040510     C*** CBO
098200040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
098300040510     C                   EVAL      VABCBO = SkSplitCSV(i)
098400040510     C                   ENDIF
098500030721     C*** NAS
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
098700030721     C                   EVAL      VABNAS = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** TIC
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
099100030721     C                   EVAL      VABTIC = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** GCA
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
099500030721     C                   EVAL      VABGCA = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** XCO
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
099900030721     C                   EVAL      VABXCO = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** CTM
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
100300030721     C                   EVAL      VABCTM = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** TCR
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
100700030721     C                   EVAL      VABTCR = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** CTS
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
101100030721     C                   EVAL      VABCTS = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** FTM
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
101500030721     C                   EVAL      VABFTM = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** VAD
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
101900030721     C                   EVAL      VABVAD = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** GMA
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
102300030721     C                   EVAL      VABGMA = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** GGA
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
102700030721     C                   EVAL      VABGGA = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** GVA
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
103100030721     C                   EVAL      VABGVA = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** TC1
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
103500030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** TC2
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
103900030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*** SCL
104200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
104300030721     C                   EVAL      VABSCL = SkSplitCSV(i)
104400030721     C                   ENDIF
104500030721     C*** RMO
104600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
104700030721     C                   EVAL      VABRMO = SkSplitCSV(i)
104800030721     C                   ENDIF
104900030721     C*** NMO
105000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
105100030721     C                   EVAL      VABNMO = SkSplitCSV(i)
105200030721     C                   ENDIF
105300030721     C*
105400030721     C* Reperisco quindi i campi numerici...
105500040510     C*** NSP
105600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
105700040510     C                   EVAL      PiStr=SkSplitCSV(i)
105800040510     C                   EXSR      CHKNUM
105900040510     C                   IF        PiInt=*on
106000040510     C                   Z-ADD     PiVal         VABNSP
106100040510     C                   ELSE
106200040510     C                   SETON                                        31
106300040510     C                   EVAL      VABNSP = *zeros
106400040510     C                   EVAL      vinmsg = %trimr(vinmsg)
106500040510     C                             + ' ' + 'VABNSP'
106600040510     C                   ENDIF
106700040510     C                   ENDIF
106800070703     C*** RMN
106900070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
107000070703     C                   EVAL      PiStr=SkSplitCSV(i)
107100070703     C                   EXSR      CHKNUM
107200070703     C                   IF        PiInt=*on
107300070703     C                   Z-ADD     PiVal         VABRMN
107400070703     C                   ELSE
107500070703     C                   SETON                                        32
107600070703     C                   EVAL      vinmsg = %trimr(vinmsg)
107700070703     C                             + ' ' + 'VABRMN'
107800070703     C                   ENDIF
107900070703     C                   ENDIF
108000070705     C*** CAD
108100070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
108200070705     C                   EVAL      VABCAD = SkSplitCSV(i)
108300070705     C                   ENDIF
108400030721     C*** NCL
108500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
108600030721     C                   EVAL      PiStr=SkSplitCSV(i)
108700030721     C                   EXSR      CHKNUM
108800030721     C                   IF        PiInt=*on
108900081007     C  N40              Z-ADD     PiVal         VABNCL
109000081007     C   40              ADD       PiVal         VABNCL
109100030721     C                   ELSE
109200030721     C                   SETON                                        32
109300030721     C                   EVAL      VABNCL = *zeros
109400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109500040122     C                             + ' ' + 'VABNCL'
109600030721     C                   ENDIF
109700030721     C                   ENDIF
109800030721     C*** PKB
109900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
110000030721     C                   EVAL      PiStr=SkSplitCSV(i)
110100030721     C                   EXSR      CHKNUM
110200030721     C                   IF        PiNum=*on
110300081007     C  N40              Z-ADD     PiVal         VABPKB
110400081007     C   40              ADD       PiVal         VABPKB
110500030721     C                   ELSE
110600030721     C                   SETON                                        32
110700030721     C                   EVAL      VABPKB = *zeros
110800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
110900030721     C                             + ' ' + 'VABPKB'
111000030721     C                   ENDIF
111100030721     C                   ENDIF
111200030721     C*** CAS
111300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
111400030721     C                   EVAL      PiStr=SkSplitCSV(i)
111500030721     C                   EXSR      CHKNUM
111600030721     C                   IF        PiNum=*on
111700030721     C                   Z-ADD     PiVal         VABCAS
111800030721     C                   ELSE
111900030721     C                   SETON                                        32
112000030721     C                   EVAL      VABCAS = *zeros
112100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112200030721     C                             + ' ' + 'VABCAS'
112300030721     C                   ENDIF
112400030721     C                   ENDIF
112500030721     C*** CCM
112600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
112700030721     C                   EVAL      PiStr=SkSplitCSV(i)
112800030721     C                   EXSR      CHKNUM
112900030721     C                   IF        PiInt=*on      AND
113000030721     C                             PiVal<=9999999 AND
113100030721     C                             PiVal>*zeros
113200030721     C                   Z-ADD     PiVal         VABCCM
113300030721     C                   ELSE
113400030721     C                   SETON                                        32
113500030721     C                   EVAL      VABCCM = *zeros
113600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
113700040510     C                             + ' ' + 'VABCCM'
113800030721     C                   ENDIF
113900030721     C                   ENDIF
114000030721     C*** LNP
114100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
114200030721     C                   EVAL      PiStr=SkSplitCSV(i)
114300030721     C                   EXSR      CHKNUM
114400030721     C                   IF        PiInt=*on    AND
114500030721     C                             PiVal<=999   AND
114600030721     C                             PiVal>*zeros
114700030721     C                   Z-ADD     PiVal         VABLNP
114800030721     C                   ELSE
114900040715     C                   SETON                                        32
115000030721     C                   EVAL      VABLNP = *zeros
115100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115200040510     C                             + ' ' + 'VABLNP'
115300030721     C                   ENDIF
115400030721     C                   ENDIF
115500030721     C*** NRS
115600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
115700030721     C                   EVAL      PiStr=SkSplitCSV(i)
115800030721     C                   EXSR      CHKNUM
115900030721     C                   IF        PiInt=*on AND
116000030721     C                             PiVal<=99
116100030721     C                   Z-ADD     PiVal         VABNRS
116200030721     C                   ELSE
116300030721     C                   SETON                                        32
116400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116500030721     C                             + ' ' + 'VABNRS'
116600030721     C                   ENDIF
116700030721     C                   ENDIF
116800030721     C*** LNA
116900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
117000030721     C                   EVAL      PiStr=SkSplitCSV(i)
117100030721     C                   EXSR      CHKNUM
117200030721     C                   IF        PiInt=*on    AND
117300030721     C                             PiVal<=999   AND
117400030721     C                             PiVal>*zeros
117500030721     C                   Z-ADD     PiVal         VABLNA
117600030721     C                   ELSE
117700030721     C                   SETON                                        32
117800030721     C                   EVAL      VABLNA = *zeros
117900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118000030721     C                             + ' ' + 'VABLNA'
118100030721     C                   ENDIF
118200030721     C                   ENDIF
118300030721     C*** CTR
118400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
118500030721     C                   EVAL      PiStr=SkSplitCSV(i)
118600030721     C                   EXSR      CHKNUM
118700030721     C                   IF        PiInt=*on  AND
118800030721     C                             PiVal<=999
118900030721     C                   Z-ADD     PiVal         VABCTR
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABCTR'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** IAS
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100030721     C                   Z-ADD     PiVal         VABIAS
120200030721     C                   ELSE
120300030721     C                   SETON                                        32
120400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120500030721     C                             + ' ' + 'VABIAS'
120600030721     C                   ENDIF
120700030721     C                   ENDIF
120800030721     C*** VLB
120900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
121000030721     C                   EVAL      PiStr=SkSplitCSV(i)
121100030721     C                   EXSR      CHKNUM
121200030721     C                   IF        PiNum=*on
121300081007     C  N40              Z-ADD     PiVal         VABVLB
121400081007     C   40              ADD       PiVal         VABVLB
121500030721     C                   ELSE
121600030721     C                   SETON                                        32
121700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121800030721     C                             + ' ' + 'VABVLB'
121900030721     C                   ENDIF
122000030721     C                   ENDIF
122100030721     C*** QFT
122200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
122300030721     C                   EVAL      PiStr=SkSplitCSV(i)
122400030721     C                   EXSR      CHKNUM
122500030721     C                   IF        PiNum=*on
122600081007     C  N40              Z-ADD     PiVal         VABQFT
122700081007     C   40              ADD       PiVal         VABQFT
122800030721     C                   ELSE
122900030721     C                   SETON                                        32
123000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123100030721     C                             + ' ' + 'VABQFT'
123200030721     C                   ENDIF
123300030721     C                   ENDIF
123400030721     C*** NCD
123500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
123600030721     C                   EVAL      PiStr=SkSplitCSV(i)
123700030721     C                   EXSR      CHKNUM
123800030721     C                   IF        PiInt=*on      AND
123900030721     C                             PiVal<=9999999
124000030721     C                   Z-ADD     PiVal         VABNCD
124100030721     C                   ELSE
124200030721     C                   SETON                                        32
124300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124400030721     C                             + ' ' + 'VABNCD'
124500030721     C                   ENDIF
124600030721     C                   ENDIF
124700030721     C*** NCA
124800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
124900030721     C                   EVAL      PiStr=SkSplitCSV(i)
125000030721     C                   EXSR      CHKNUM
125100030721     C                   IF        PiInt=*on      AND
125200030721     C                             PiVal<=9999999
125300030721     C                   Z-ADD     PiVal         VABNCA
125400030721     C                   ELSE
125500030721     C                   SETON                                        32
125600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125700030721     C                             + ' ' + 'VABNCA'
125800030721     C                   ENDIF
125900030721     C                   ENDIF
126000030721     C*** ZNC
126100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
126200030721     C                   EVAL      PiStr=SkSplitCSV(i)
126300030721     C                   EXSR      CHKNUM
126400030721     C                   IF        PiInt=*on AND
126500030721     C                             PiVal<=99
126600030721     C                   Z-ADD     PiVal         VABZNC
126700030721     C                   ELSE
126800030721     C                   SETON                                        32
126900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127000030721     C                             + ' ' + 'VABZNC'
127100030721     C                   ENDIF
127200030721     C                   ENDIF
127300030721     C*** DCR
127400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
127500030721     C                   EVAL      PiStr=SkSplitCSV(i)
127600030721     C                   EXSR      CHKNUM
127700030721     C                   IF        PiInt=*on       AND
127800030721     C                             PiVal<=99999999
127900030721     C                   Z-ADD     PiVal         VABDCR
128000030721     C                   ELSE
128100030721     C                   SETON                                        32
128200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128300030721     C                             + ' ' + 'VABDCR'
128400030721     C                   ENDIF
128500030721     C                   ENDIF
128600030721     C*** HCR
128700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
128800030721     C                   EVAL      PiStr=SkSplitCSV(i)
128900030721     C                   EXSR      CHKNUM
129000030721     C                   IF        PiInt=*on   AND
129100030721     C                             PiVal<=9999
129200030721     C                   Z-ADD     PiVal         VABHCR
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABHCR'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** VMD
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiNum=*on
130400030721     C                   Z-ADD     PiVal         VABVMD
130500030721     C                   ELSE
130600030721     C                   SETON                                        32
130700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130800030721     C                             + ' ' + 'VABVMD'
130900030721     C                   ENDIF
131000030721     C                   ENDIF
131100030721     C*** ANT
131200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
131300030721     C                   EVAL      PiStr=SkSplitCSV(i)
131400030721     C                   EXSR      CHKNUM
131500030721     C                   IF        PiInt=*on        AND
131600030721     C                             PiVal<=999999999
131700030721     C                   Z-ADD     PiVal         VABANT
131800030721     C                   ELSE
131900030721     C                   SETON                                        32
132000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132100030721     C                             + ' ' + 'VABANT'
132200030721     C                   ENDIF
132300030721     C                   ENDIF
132400030721     C*** CMO
132500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
132600030721     C                   EVAL      PiStr=SkSplitCSV(i)
132700030721     C                   EXSR      CHKNUM
132800030721     C                   IF        PiInt=*on
132900030721     C                   Z-ADD     PiVal         Num5_0
133000030721     C                   MOVEL(p)  Num5_0        VABCMO
133100030721     C                   ELSE
133200030721     C                   SETON                                        32
133300030721     C                   EVAL      VABCMO = *zeros
133400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133500030721     C                             + ' ' + 'VABCMO'
133600030721     C                   ENDIF
133700030721     C                   ENDIF
133800040510     C*
133900040510     C* Reperisco i campi del FIVAT
134000040510     C*** VATNOT
134100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
134200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
134300040510     C                   ENDIF
134400040510     C*** VATTRC
134500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
134600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
134700040510     C                   ENDIF
134800060331     C*** VATNOT_A
134900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
135000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
135100060331     C                   ENDIF
135200060331     C*** VATNOT_B
135300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
135400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
135500060331     C                   ENDIF
135600060331     C*** VATNOT_E
135700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
135800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
135900060331     C                   ENDIF
136000110912     C*** VATNOT_I
136100110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
136200110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
136300110912     C                   ENDIF
136400110912     C*** VATNOT_J
136500110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
136600110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
136700110912     C                   ENDIF
136800110912     C*** VATNOT_4
136900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
137000110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
137100110912     C                   ENDIF
137200131018     C*** VATNOT_S
137300131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
137400131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
137500140507     C                   IF        %trim(%subst(wVATNOT_S:1:16)) = '.' OR
137600140507     C                             %trim(%subst(wVATNOT_S:1:16)) = '0'
137700140507     C                   EVAL      %subst(wVATNOT_S:1:16) = *blanks
137800140507     C                   ENDIF
137900131018     C                   ENDIF
138000141128     C*** VATNOT_Y
138100141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
138200141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
138300141128     C                   ENDIF
138400151109     C*
138500151109     C* Forzature e/o Personalizzazioni
138600151109     C*
138700151109     C*** VATNOT_E
138800151109     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138900151109     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
139000151109     C* Elimino sempre ed incondizionatamente il primo carattere
139100151109     C                   EVAL      wVATNOT_E = %subst(wVATNOT_E:2)
139200151109     C                   ENDIF
139300160226     C*
139400160226     C* se verrà scritto VATNOT_B imposto VABTC1=A
139500160226     C                   if        wVATNOT_B <> *blanks
139600160226     C                   eval      VABTC1 = 'A'
139700160226     C                   endif
139800131018     C*
139900030715     C                   ENDIF
140000030715     C                   EVAL      i = i + 1
140100030715     C                   ENDDO
140200040510     C*
140300040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
140400040510     C                   IF        VABNSP = *zeros
140500040510     C                   SETON                                        31
140600040510     C                   ENDIF
140700040715     C*
140800040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
140900040715     C                   IF        VABLNP = *zeros
141000040715     C                   SETON                                        31
141100040715     C                   ENDIF
141200040510     C*
141300040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
141400040510     C                   IF        VABRMN = *zeros
141500040510     C                   Z-ADD     VABNSP        VABRMN
141600040510     C                   ENDIF
141700040510     C*
141800040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
141900040510     C                   IF        VABRMA = *blanks
142000040714     C                   MOVEL     VABRMN        VABRMA
142100040510     C                   ENDIF
142200070705     C*
142300070705     C* Considerazioni particolari
142400070705     C*** NZD
142500070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
142600070705     C                   EVAL      VABNZD = *blanks
142700070705     C                   ENDIF
142800070705     C*** CAD
142900070705     C                   IF        VABNZD = *blanks
143000070705     C                   EVAL      PiStr=VABCAD
143100070705     C                   EXSR      CHKNUM
143200070705     C                   IF        PiInt=*on
143300070705     C                   Z-ADD     PiVal         Num5_0
143400070705     C                   MOVEL(p)  Num5_0        VABCAD
143500070705     C                   ELSE
143600070705     C                   SETON                                        32
143700070705     C                   EVAL      VABCAD = *zeros
143800070705     C                   EVAL      vinmsg = %trimr(vinmsg)
143900070705     C                             + ' ' + 'VABCAD'
144000070705     C                   ENDIF
144100070705     C                   ENDIF
144200040128     C*
144300040510     C* Se provincia nn valorizzata la reperisco
144400040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144500040510     C                   IF        VABPRD = *blanks
144600040128     C                   CLEAR                   TISI95DS
144700040128     C                   EVAL      I95TCN = '3'
144800040128     C                   Z-ADD     datcor        I95DAT
144900040128     C                   EVAL      I95NAR = VABNZD
145000040128     C                   EVAL      I95CAP = VABCAD
145100040128     C                   EVAL      I95LOC = VABLOD
145200040128     C                   CALL      'TISI95R'
145300040128     C                   PARM                    TISI95DS
145400040128     C                   EVAL      VABPRD = O95PRV
145500040510     C                   ENDIF
145600040113     C*
145700040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
145800040113     C                   IF        VABCAS > *zeros
145900040119     C                   EVAL      VABVCA = 'EUR'
146000040119     C                   IF        VABCBO = '1'
146100040113     C                   EVAL      VABCBO = '4'
146200040119     C                   ENDIF
146300040119     C                   IF        VABCBO = '2'
146400040119     C                   EVAL      VABCBO = '6'
146500040119     C                   ENDIF
146600040113     C                   ENDIF
146700020204     C*
146800020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
146900020204     C                   EXSR      CHKIMPDIV
147000030715     C                   ENDIF
147100020204     C*
147200010607     C                   ENDSR
147300010607     C*----------------------------------------------------*
147400010601
147500020204
147600020204     C*----------------------------------------------------*
147700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
147800020204     C*----------------------------------------------------*
147900020204     C     CHKIMPDIV     BEGSR
148000020204     C*
148100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148200020204     C                   Z-ADD     *zeros        wrkDec            9 9
148300020204     C*
148400020204     C* Come prima cosa effettuo considerazioni sulla divisa
148500020204     C                   IF        vabIAS > *zeros
148600020204     C                   IF        vabVAS <> 'EUR'
148700020204     C                   EVAL      vabVAS =  'ITL'
148800020204     C                   ENDIF
148900020204     C                   ENDIF
149000020204     C*
149100020204     C                   IF        vabCAS > *zeros
149200020204     C                   IF        vabVCA <> 'EUR'
149300020204     C                   EVAL      vabVCA =  'ITL'
149400020204     C                   ENDIF
149500020204     C                   ENDIF
149600020204     C*
149700020204     C                   IF        vabVMD > *zeros
149800020204     C                   IF        vabVAD <> 'EUR'
149900020204     C                   EVAL      vabVAD =  'ITL'
150000020204     C                   ENDIF
150100020204     C                   ENDIF
150200020204     C*
150300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150400020204     C                   Z-ADD     vabIAS        wrkDec
150500020204     C                   IF        wrkDec > *zeros
150600020204     C                   IF        vabVAS = 'ITL'
150700020204     C                   EVAL      vabIAS = *zeros
150800020204     C                   ENDIF
150900020204     C                   ENDIF
151000020204     C*
151100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151200020204     C                   Z-ADD     vabCAS        wrkDec
151300020204     C                   IF        wrkDec > *zeros
151400020204     C                   IF        vabVCA = 'ITL'
151500020204     C                   EVAL      vabCAS = *zeros
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152000020204     C                   Z-ADD     vabVMD        wrkDec
152100020204     C                   IF        wrkDec > *zeros
152200020204     C                   IF        vabVAD = 'ITL'
152300020204     C                   EVAL      vabVMD = *zeros
152400020204     C                   ENDIF
152500020204     C                   ENDIF
152600020204     C*
152700020204     C                   ENDSR
152800020204     C***
152900020204
153000010330
153100010330
153200010330     C*----------------------------------------------------*
153300010330     C*  CONTROLLO NUMERICITA' CAMPI
153400010330     C*----------------------------------------------------*
153500010330     C     CHKNUM        BEGSR
153600010330     C*
153700010606     C                   IF        PiDecChr = *blanks
153800030715     C                   EVAL      PiDecChr = CharNUM
153900010606     C                   ENDIF
154000010606     C*
154100010606     C                   CALL(e)   'ISNUMERIC'
154200010330     C                   PARM                    PiStr            30
154300010606     C                   PARM                    PiDecChr          1
154400010330     C                   PARM      *ZEROS        PiVal            30 9
154500010330     C                   PARM      '0'           PiInt             1
154600010330     C                   PARM      '0'           PiNum             1
154700010330     C                   IF        %error
154800010606     C                   EVAL      PiNum=*off
154900010330     C                   ENDIF
155000010330     C*
155100010330     C                   ENDSR
155200010330     C***
155300010330
155400010601
155500010601
155600010601
155700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
155800010601     C     repfil        BEGSR
155900010601     C*
156000010601     C                   if        invfil = *zeros and
156100010601     C                             depfil > *zeros and
156200010629     C                             (vinflg = *blanks or
156300010629     C                              vinflg = *zeros)
156400010601     C
156500010601     C                   eval      invfil = depfil
156600010601     C                   endif
156700010601     C*
156800010601     C                   if        depfil <> invfil and
156900010601     C                             invfil > *zeros
157000010601     C                   eval      flgMulti = '1'
157100010601     C                   if        vinflg = *blanks
157200010601     C                   add       1             cntNonEl
157300010601     C                   endif
157400010601     C                   endif
157500010601     C*
157600010601     C                   if        vinflg = '2'
157700010601     C                   eval      flgStato = '2'
157800010601     C                   endif
157900010601     C*
158000010601     C                   ENDSR
158100010601     C***
158200010601
158300010601
158400010601
158500010330
158600040119      /TITLE Invio dei dati al punto operativo.
158700040119     C     invio         BEGSR
158800040119     C*
158900040119     C* 1° invio FIVAT
159000040119     C                   reset                   dscmz
159100070704     C                   move      winvcmz       cmzdst
159200040119     C                   eval      cmzfld = 'FIVATWWR'
159300040119     C                   eval      cmzmbd = vlrhdl
159400040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159500040119     C***                if        prmfir = *blanks
159600040119     C                   eval      cmzfla = 'FIVAT00F'
159700040119     C                   eval      cmzmba = 'FIVAT00F'
159800040119     C***                else
159900040119     C***                eval      cmzfla = prmfir
160000040119     C***                eval      cmzmba = prmfir
160100040119     C***                endif
160200040119     C                   eval      cmznrr = *zeros
160300040119     C                   move      §ctrokvt      cmznrr
160400040119     C                   eval      cmzlba = vlrfl1
160500040119     C                   call(e)   'TIS711C'
160600040119     C                   parm                    dscmz
160700040119     C                   parm      *blanks       esito
160800040119     C                   if        %error
160900040119     C                             or cmzerr = '1'
161000040119     C                             or esito  = '1'
161100040119     C                   eval      wrkesito = '3'
161200040119     C                   else
161300040119     C*
161400040119     C* 2° invio FIVAB
161500040119     C                   reset                   dscmz
161600070704     C                   move      winvcmz       cmzdst
161700040119     C                   eval      cmzfld = vlrfou
161800040119     C                   eval      cmzmbd = vlrhdl
161900040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162000040119     C***                if        prmfir = *blanks
162100040119     C                   eval      cmzfla = 'FIVAB00F'
162200040119     C                   eval      cmzmba = 'FIVAB00F'
162300040119     C***                else
162400040119     C***                eval      cmzfla = prmfir
162500040119     C***                eval      cmzmba = prmfir
162600040119     C***                endif
162700040119     C                   eval      cmznrr = *zeros
162800040119     C                   move      §ctrokvb      cmznrr
162900040119     C                   eval      cmzlba = vlrfl1
163000040119     C                   call(e)   'TIS711C'
163100040119     C                   parm                    dscmz
163200040119     C                   parm      *blanks       esito
163300040119     C                   if        %error
163400040119     C                             or cmzerr = '1'
163500040119     C                             or esito  = '1'
163600040119     C                   eval      wrkesito = '3'
163700040119     C                   endif
163800040119     C                   endif
163900040119     C*
164000040119     C                   ENDSR
164100040119     C***
164200010601
164300010601
164400010601
164500010601
164600010601      /TITLE Invio dei dati al punto operativo.
164700010601     C     opeini        BEGSR
164800010601     C*
164900010601     C* Inizializzo flag e contatori operativi
165000010601     C                   movel     '0'           flgGiro           1
165100010601     C                   movel     '0'           flgMulti          1
165200010601     C                   movel     '1'           flgStato          1
165300010615     C                   movel     '0'           flgOk             1
165400010601     C                   z-add     *zeros        cntNonEl         10 0
165500010601     C                   z-add     *zeros        depfil            3 0
165600010601     C                   z-add     *zeros        invfil            3 0
165700010601     C*
165800010601     C                   ENDSR
165900010601     C***
166000070326
166100070326
166200070326
166300070326
166400070326     C     *pssr         BEGSR
166500070329     C*
166600070329     C                   if        %open(tivin00r)
166700070329     C                   close     tivin00r
166800070329     C                   endif
166900070329     C                   if        %open(fivabwwr)
167000070329     C                   close     fivabwwr
167100070329     C                   endif
167200070329     C                   if        %open(fivatwwr)
167300070329     C                   close     fivatwwr
167400070329     C                   endif
167500070326     C*
167600070326     C* Effettuo la chiamata al CLLE preposto
167700070326     C                   call(e)   'TITVVTC'
167800070326     C                   parm                    parccm
167900070326     C                   parm                    parmbr
168000070326     C                   parm      '2'           paropz
168100070326     C*
168200070326     C                   eval      wrkesito = '2'
168300070404     C*
168400070404     C                   seton                                        LR
168500070326     C*
168600070326     C                   ENDSR     '*CANCL'
168700070326     C***
168800070326
168900070326
169000010330
169100010330
169200000613     C     *inzsr        BEGSR
169300990910     C*
169400990910     C     *entry        plist
169500990920     C                   parm                    tivlrds
169600990921     C                   parm      wrkesito      esito
169700000724     C                   parm                    prmlit
169800000710     C                   parm                    prmfir
169900010330     C*
170000010330     C* CALCOLA LA DATA CORRENTE
170100151109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
170200151109     C                   eval      datcor = %dec(%date() : *ISO)
170300000613     C*
170400000613     C                   ENDSR
170500000613     C***
170600040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
170700040510
170800040122
170900040113
