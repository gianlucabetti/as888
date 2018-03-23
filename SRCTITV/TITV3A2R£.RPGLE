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
017900170622     C*
018000170622     C* Verifico che il record nn inizi mai con il carattere d separatore campi
018100170622     C                   eval      vindta = %trim(vindta)
018200170622     C                   if        %subst(vindta:1:1) = CharCSV
018300170622     C                   eval      vindta = 'X'+%trim(vindta)
018400170622     C                   endif
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
040000131018     C*
040100131018     C                   if        wVATNOT_S <> *blanks
040200131018     C                   eval      VATNOT = wVATNOT_S
040300131018     C                   eval      VATTRC = 'S'
040400131018     C                   write     FIVAT000
040500131018     C                   add       1             §CTROKVT
040600131018     C                   endif
040700141128     C*
040800141128     C                   if        wVATNOT_Y <> *blanks
040900141128     C                   eval      VATNOT = wVATNOT_Y
041000141128     C                   eval      VATTRC = 'Y'
041100141128     C                   write     FIVAT000
041200141128     C                   add       1             §CTROKVT
041300141128     C                   endif
041400040119     C*
041500040119     C                   ENDSR
041600040119
041700040119
041800040119
041900040119     C*----------------------------------------------------*
042000040119     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100040119     C*----------------------------------------------------*
042200040119     C     PREVAT        BEGSR
042300040119     C*
042400040119     C* Compongo il nome del membro da dare al FIVATWWR
042500040119     C                   eval      parmbr = vlrhdl
042600040119     C                   movel     'M'           parmbr
042700040510     C                   eval      parccm = vlrksc
042800040119     C                   eval      paropz = '1'
042900040119     C* Effettuo la chiamata al CLLE preposto
043000040119     C                   call(e)   'TITVVTC'
043100040119     C                   parm                    parccm
043200040119     C                   parm                    parmbr
043300040119     C                   parm                    paropz
043400040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500040119     C                   if        %error
043600040119     C                   movel     '1'           chkcall
043700040119     C                   else
043800040119     C                   movel     '0'           chkcall
043900040119     C                   endif
044000040119     C*
044100040119     C                   ENDSR
044200010601
044300010601
044400010601
044500010330     C*----------------------------------------------------*
044600030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
044700010330     C*----------------------------------------------------*
044800040119     C     INZVAR        BEGSR
044900010330     C*
045000030822     C* Inizializzo variabili di wrk
045100020204     C                   Z-ADD     *zeros        Num5_0            5 0
045200090403     C                   z-add     *zeros        invfil
045300060331     C                   MOVEL     *blanks       wVATNOT_A
045400060331     C                   MOVEL     *blanks       wVATNOT_B
045500060331     C                   MOVEL     *blanks       wVATNOT_E
045600110912     C                   MOVEL     *blanks       wVATNOT_I
045700110912     C                   MOVEL     *blanks       wVATNOT_J
045800110912     C                   MOVEL     *blanks       wVATNOT_4
045900131018     C                   MOVEL     *blanks       wVATNOT_S
046000141128     C                   MOVEL     *blanks       wVATNOT_Y
046100081014     C*
046200081014     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
046300081014     C                   CLEAR                   FIVAB000
046400081014     C                   CLEAR                   FIVAT000
046500030822     C*
046600081014     C* Inizializzo la schiera d wrk x i dati
046700030822     C                   CLEAR                   SkSplitCSV
046800030822     C                   EVAL      i = 1
046900030822     C                   EVAL      posDa = *zeros
047000030822     C                   EVAL      posA  = *zeros
047100030822     C*
047200030822     C* Reimposto i valori di default
047300030822     C                   EXSR      DEFCAM
047400030822     C*
047500010330     C                   ENDSR
047600010330     C*----------------------------------------------------*
047700030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
047800010330     C*----------------------------------------------------*
047900010330     C     DEFCAM        BEGSR
048000030822     C*
048100030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
048200030715     C* e delimitatore testo.
048300030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
048400030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
048500030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
048600090324     C*
048700090324     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
048800090324     C                   EVAL      CharSOS = CharNUM
048900030822     C*
049000170621     C* Reperisco il parametro che mi indica se sommare o meno i valori di testata
049100030715     C                   IF        %subst(vlrppt:1:1) = 'S'
049200081007     C                   SETON                                        40
049300030715     C                   ELSE
049400081007     C                   SETOFF                                       40
049500030715     C                   ENDIF
049600030822     C*
049700030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
049800030822     C                   EVAL      posDaDft = 1
049900030822     C                   EVAL      posADft  = 0
050000030822     C                   EVAL      wGiroDft = 0
050100030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
050200030822     C                             posDaDft > 0
050300030822     C*
050400030822     C* Gestisco il 1° giro
050500030822     C                   IF        wGiroDft = 0
050600030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
050700030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
050800030822     C* Incremento il contatore dei "giri"
050900030822     C                   EVAL      wGiroDft = 1
051000030822     C                   ELSE
051100030822     C                   EVAL      posDaDft = posADft
051200030822     C                   ENDIF
051300030822     C* Eseguo lo scan x trovare la fine del campo corrente
051400030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
051500030822     C*
051600030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
051700030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
051800030822     C                   IF        posDaDft > 0 AND
051900030822     C                             posADft  > 0
052000030822     C* NCL
052100030822     C                   IF        %subst(
052200030822     C                             %subst(vlrppt:posDaDft+1:
052300030822     C                             posADft-posDaDft-1):1:3)
052400030822     C                             = 'NCL'
052500030822     C                   EVAL      PiStr=%trim(%subst(
052600030822     C                             %subst(vlrppt:posDaDft+1:
052700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
052800030822     C                   EXSR      CHKNUM
052900030822     C                   IF        PiInt=*on
053000030822     C                   Z-ADD     PiVal         VABNCL
053100030822     C                   ENDIF
053200030822     C                   ENDIF
053300030822     C* CCM
053400030822     C                   IF        %subst(
053500030822     C                             %subst(vlrppt:posDaDft+1:
053600030822     C                             posADft-posDaDft-1):1:3)
053700030822     C                             = 'CCM'
053800030822     C                   EVAL      PiStr=%trim(%subst(
053900030822     C                             %subst(vlrppt:posDaDft+1:
054000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
054100030822     C                   EXSR      CHKNUM
054200030822     C                   IF        PiInt=*on
054300030822     C                   Z-ADD     PiVal         VABCCM
054400030822     C                   ENDIF
054500030822     C                   ENDIF
054600030822     C* LNP
054700030822     C                   IF        %subst(
054800030822     C                             %subst(vlrppt:posDaDft+1:
054900030822     C                             posADft-posDaDft-1):1:3)
055000030822     C                             = 'LNP'
055100030822     C                   EVAL      PiStr=%trim(%subst(
055200030822     C                             %subst(vlrppt:posDaDft+1:
055300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055400030822     C                   EXSR      CHKNUM
055500030822     C                   IF        PiInt=*on
055600030822     C                   Z-ADD     PiVal         VABLNP
055700030822     C                   ENDIF
055800030822     C                   ENDIF
055900030822     C* NRS
056000030822     C                   IF        %subst(
056100030822     C                             %subst(vlrppt:posDaDft+1:
056200030822     C                             posADft-posDaDft-1):1:3)
056300030822     C                             = 'NRS'
056400030822     C                   EVAL      PiStr=%trim(%subst(
056500030822     C                             %subst(vlrppt:posDaDft+1:
056600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
056700030822     C                   EXSR      CHKNUM
056800030822     C                   IF        PiInt=*on
056900030822     C                   Z-ADD     PiVal         VABNRS
057000030822     C                   ENDIF
057100030822     C                   ENDIF
057200030822     C* CTR
057300030822     C                   IF        %subst(
057400030822     C                             %subst(vlrppt:posDaDft+1:
057500030822     C                             posADft-posDaDft-1):1:3)
057600030822     C                             = 'CTR'
057700030822     C                   EVAL      PiStr=%trim(%subst(
057800030822     C                             %subst(vlrppt:posDaDft+1:
057900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058000030822     C                   EXSR      CHKNUM
058100030822     C                   IF        PiInt=*on
058200030822     C                   Z-ADD     PiVal         VABCTR
058300030822     C                   ENDIF
058400030822     C                   ENDIF
058500030822     C* PKB
058600030822     C                   IF        %subst(
058700030822     C                             %subst(vlrppt:posDaDft+1:
058800030822     C                             posADft-posDaDft-1):1:3)
058900030822     C                             = 'PKB'
059000030822     C                   EVAL      PiStr=%trim(%subst(
059100030822     C                             %subst(vlrppt:posDaDft+1:
059200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059300030822     C                   EXSR      CHKNUM
059400030822     C                   IF        PiNum=*on
059500030822     C                   Z-ADD     PiVal         VABPKB
059600030822     C                   ENDIF
059700030822     C                   ENDIF
059800030822     C* VLB
059900030822     C                   IF        %subst(
060000030822     C                             %subst(vlrppt:posDaDft+1:
060100030822     C                             posADft-posDaDft-1):1:3)
060200030822     C                             = 'VLB'
060300030822     C                   EVAL      PiStr=%trim(%subst(
060400030822     C                             %subst(vlrppt:posDaDft+1:
060500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
060600030822     C                   EXSR      CHKNUM
060700030822     C                   IF        PiNum=*on
060800030822     C                   Z-ADD     PiVal         VABVLB
060900030822     C                   ENDIF
061000030822     C                   ENDIF
061100030822     C* QFT
061200030822     C                   IF        %subst(
061300030822     C                             %subst(vlrppt:posDaDft+1:
061400030822     C                             posADft-posDaDft-1):1:3)
061500030822     C                             = 'QFT'
061600030822     C                   EVAL      PiStr=%trim(%subst(
061700030822     C                             %subst(vlrppt:posDaDft+1:
061800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061900030822     C                   EXSR      CHKNUM
062000030822     C                   IF        PiNum=*on
062100030822     C                   Z-ADD     PiVal         VABQFT
062200030822     C                   ENDIF
062300030822     C                   ENDIF
062400030822     C* CBO
062500030822     C                   IF        %subst(
062600030822     C                             %subst(vlrppt:posDaDft+1:
062700030822     C                             posADft-posDaDft-1):1:3)
062800030822     C                             = 'CBO'
062900030822     C                   EVAL      VABCBO=%trim(%subst(
063000030822     C                             %subst(vlrppt:posDaDft+1:
063100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063200030822     C                   ENDIF
063300030822     C* TSP
063400030822     C                   IF        %subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):1:3)
063700030822     C                             = 'TSP'
063800030822     C                   EVAL      VABTSP=%trim(%subst(
063900030822     C                             %subst(vlrppt:posDaDft+1:
064000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
064100030822     C                   ENDIF
064200030822     C* VAS
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'VAS'
064700030822     C                   EVAL      VABVAS=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   ENDIF
065100030822     C* VCA
065200030822     C                   IF        %subst(
065300030822     C                             %subst(vlrppt:posDaDft+1:
065400030822     C                             posADft-posDaDft-1):1:3)
065500030822     C                             = 'VCA'
065600030822     C                   EVAL      VABVCA=%trim(%subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065900030822     C                   ENDIF
066000030822     C* GCA
066100030822     C                   IF        %subst(
066200030822     C                             %subst(vlrppt:posDaDft+1:
066300030822     C                             posADft-posDaDft-1):1:3)
066400030822     C                             = 'GCA'
066500030822     C                   EVAL      VABGCA=%trim(%subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066800030822     C                   ENDIF
066900030822     C* CTM
067000030822     C                   IF        %subst(
067100030822     C                             %subst(vlrppt:posDaDft+1:
067200030822     C                             posADft-posDaDft-1):1:3)
067300030822     C                             = 'CTM'
067400030822     C                   EVAL      VABCTM=%trim(%subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067700030822     C                   ENDIF
067800030822     C* FFD
067900030822     C                   IF        %subst(
068000030822     C                             %subst(vlrppt:posDaDft+1:
068100030822     C                             posADft-posDaDft-1):1:3)
068200030822     C                             = 'FFD'
068300030822     C                   EVAL      VABFFD=%trim(%subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068600030822     C                   ENDIF
068700030822     C* VAD
068800030822     C                   IF        %subst(
068900030822     C                             %subst(vlrppt:posDaDft+1:
069000030822     C                             posADft-posDaDft-1):1:3)
069100030822     C                             = 'VAD'
069200030822     C                   EVAL      VABVAD=%trim(%subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069500030822     C                   ENDIF
069600030822     C* GMA
069700030822     C                   IF        %subst(
069800030822     C                             %subst(vlrppt:posDaDft+1:
069900030822     C                             posADft-posDaDft-1):1:3)
070000030822     C                             = 'GMA'
070100030822     C                   EVAL      VABGMA=%trim(%subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070400030822     C                   ENDIF
070500030822     C* GGA
070600030822     C                   IF        %subst(
070700030822     C                             %subst(vlrppt:posDaDft+1:
070800030822     C                             posADft-posDaDft-1):1:3)
070900030822     C                             = 'GGA'
071000030822     C                   EVAL      VABGGA=%trim(%subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071300030822     C                   ENDIF
071400030822     C* GVA
071500030822     C                   IF        %subst(
071600030822     C                             %subst(vlrppt:posDaDft+1:
071700030822     C                             posADft-posDaDft-1):1:3)
071800030822     C                             = 'GVA'
071900030822     C                   EVAL      VABGVA=%trim(%subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072200030822     C                   ENDIF
072300030822     C* TC1
072400030822     C                   IF        %subst(
072500030822     C                             %subst(vlrppt:posDaDft+1:
072600030822     C                             posADft-posDaDft-1):1:3)
072700030822     C                             = 'TC1'
072800030822     C                   EVAL      VABTC1=%trim(%subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073100030822     C                   ENDIF
073200030822     C* TC2
073300030822     C                   IF        %subst(
073400030822     C                             %subst(vlrppt:posDaDft+1:
073500030822     C                             posADft-posDaDft-1):1:3)
073600030822     C                             = 'TC2'
073700030822     C                   EVAL      VABTC2=%trim(%subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074000030822     C                   ENDIF
074100080516     C* TIC
074200080516     C                   IF        %subst(
074300080516     C                             %subst(vlrppt:posDaDft+1:
074400080516     C                             posADft-posDaDft-1):1:3)
074500080516     C                             = 'TIC'
074600080516     C                   EVAL      VABTIC=%trim(%subst(
074700080516     C                             %subst(vlrppt:posDaDft+1:
074800080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074900080516     C                   ENDIF
075000040714     C* VATTRC
075100040714     C                   IF        %subst(
075200040714     C                             %subst(vlrppt:posDaDft+1:
075300040714     C                             posADft-posDaDft-1):1:3)
075400040714     C                             = 'TRC'
075500040714     C                   EVAL      VATTRC=%trim(%subst(
075600040714     C                             %subst(vlrppt:posDaDft+1:
075700040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075800040714     C                   ENDIF
075900030822     C* ...
076000030822     C                   ENDIF
076100030822     C                   ENDDO
076200020204     C*
076300010330     C                   ENDSR
076400010607     C*----------------------------------------------------*
076500021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
076600010607     C*----------------------------------------------------*
076700010607     C     IMPVAB        BEGSR
076800010607     C*
076900010607     C                   SETOFF                                       3132
077000030515     C*
077100010607     C                   MOVEL     datcor        VABAAS
077200010607     C                   MOVE      datcor        VABMGS
077300010607     C*
077400030715     C**********
077500030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
077600030715     C**********
077700030715     C                   IF        wGiro = *zeros
077800040113     C*
077900040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
078000040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
078100040510     C                             %trim(intcol(3))<>*blanks
078200040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
078300040113     C                                      %trim(intcol(3))
078400040510     C                   ENDIF
078500031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
078600031201     C     minu:maiu     XLATE     vindta        vindta
078700030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
078800030715     C                   DOW       posDa <= %len(%trim(vindta))
078900030715     C*
079000030715     C* Gestisco il 1° campo
079100030715     C                   IF        i = 1
079200030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
079300030715     C                   EVAL      posDa = 1
079400030715     C* Eseguo lo scan x trovare la fine del primo campo
079500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
079600030715     C* A questo punto "estraggo" il campo corrente
079700030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
079800030715     C                                                    (posA-posDa))
079900030715     C* X i campi successivi al 1°
080000030715     C                   ELSE
080100030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
080200030715     C                   EVAL      posDa = posA + 1
080300030715     C* Eseguo lo scan x trovare la fine del campo corrente
080400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
080500030715     C* Gestisco l'ultimo campo
080600030715     C                   IF        posA = *zeros
080700030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
080800030715     C* A questo punto "estraggo" il campo corrente
080900030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081000030715     C                                                    (posA-posDa))
081100030715     C                   LEAVE
081200030715     C                   ELSE
081300030715     C* A questo punto "estraggo" il campo corrente
081400030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
081500030715     C                                                    (posA-posDa))
081600030715     C                   ENDIF
081700030715     C                   ENDIF
081800030715     C* Incremento il contatore d campo
081900030715     C                   EVAL      i = i +1
082000030715     C                   ENDDO
082100030715     C*
082200030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
082300030715     C* delimitazione testo
082400030715     C                   EVAL      i = 1
082500030715     C                   DOW       i < %elem(SkSplitFLD)
082600030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
082700030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
082800030715     C                   EVAL      i = i + 1
082900030715     C                   ENDDO
083000030715     C                   EVAL      wGiro = 1
083100030715     C                   ELSE
083200030715     C                   EVAL      wGiro = 2
083300030715     C**********
083400030715     C* Effettuo lo split del campo dati d input
083500030715     C**********
083600030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
083700030715     C                   DOW       posDa <= %len(%trim(vindta))
083800030715     C*
083900030715     C* Gestisco il 1° campo
084000030715     C                   IF        i = 1
084100030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
084200030715     C                   EVAL      posDa = 1
084300030715     C* Eseguo lo scan x trovare la fine del primo campo
084400030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
084500030715     C* A questo punto "estraggo" il campo corrente
084600030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
084700030715     C                                                    (posA-posDa))
084800030715     C* X i campi successivi al 1°
084900030715     C                   ELSE
085000030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
085100030715     C                   EVAL      posDa = posA + 1
085200031204     C* Verifico che nn vi sia il campo nn valorizzato
085300031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
085400031204     C* Se campo nn valorizzato skippo al prossimo
085500031204     C                   EVAL      SkSplitCSV(i) = *blanks
085600031204     C                   EVAL      posA  = posA + 1
085700031204     C                   ELSE
085800030715     C* Eseguo lo scan x trovare la fine del campo corrente
085900030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
086000030715     C* Gestisco l'ultimo campo
086100030715     C                   IF        posA = *zeros
086200030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
086300030715     C* A questo punto "estraggo" il campo corrente
086400030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
086500030715     C                                                    (posA-posDa))
086600030715     C                   LEAVE
086700030715     C                   ELSE
086800030715     C* A questo punto "estraggo" il campo corrente
086900030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
087000030715     C                                                    (posA-posDa))
087100030715     C                   ENDIF
087200031204     C                   ENDIF
087300030715     C                   ENDIF
087400030715     C* Incremento il contatore d campo
087500030715     C                   EVAL      i = i +1
087600030715     C                   ENDDO
087700030715     C*
087800030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
087900030715     C* delimitazione testo
088000030715     C                   EVAL      i = 1
088100030715     C                   DOW       i < %elem(SkSplitCSV)
088200030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
088300030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
088400030715     C                   EVAL      i = i + 1
088500030715     C                   ENDDO
088600030715     C*
088700030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
088800030715     C                   EVAL      i = 1
088900030715     C                   DOW       i < %elem(SkSplitFLD)
089000030715     C*
089100030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
089200030715     C                   IF        SkSplitFLD(i) <> *blanks AND
089300030715     C                             SkSplitCSV(i) <> *blanks
089400030721     C*** ATB
089500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
089600030721     C                   EVAL      VABATB = SkSplitCSV(i)
089700030721     C                   ENDIF
089800030715     C*** RSD
089900030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
090000030715     C                   EVAL      VABRSD = SkSplitCSV(i)
090100030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
090200030715     C     '@':'A'       XLATE     VABRSD        VABRSD
090300030715     C* ==
090400030715     C                   ENDIF
090500030721     C*** RD2
090600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
090700030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
090800030721     C                   ENDIF
090900030715     C*** IND
091000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
091100030715     C                   EVAL      VABIND = SkSplitCSV(i)
091200030715     C                   ENDIF
091300030715     C*** LOD
091400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
091500030715     C                   EVAL      VABLOD = SkSplitCSV(i)
091600030715     C                   ENDIF
091700030715     C*** PRD
091800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
091900030715     C                   EVAL      VABPRD = SkSplitCSV(i)
092000030715     C                   ENDIF
092100030721     C*** NZD
092200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
092300030721     C                   EVAL      VABNZD = SkSplitCSV(i)
092400030721     C                   ENDIF
092500030715     C*** RMA
092600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
092700030715     C                   EVAL      VABRMA = SkSplitCSV(i)
092800030715     C                   ENDIF
092900030715     C*** NOT
093000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
093100030715     C                   EVAL      VABNOT = SkSplitCSV(i)
093200030715     C                   ENDIF
093300030715     C*** NT2
093400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
093500030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
093600030715     C                   ENDIF
093700030715     C*** VCA
093800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
093900030715     C                   EVAL      VABVCA = SkSplitCSV(i)
094000030715     C                   ENDIF
094100030715     C*** VAS
094200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
094300030715     C                   EVAL      VABVAS = SkSplitCSV(i)
094400030715     C                   ENDIF
094500030715     C*** FFD
094600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
094700030715     C                   EVAL      VABFFD = SkSplitCSV(i)
094800030715     C                   ENDIF
094900030721     C*** GC1
095000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
095100030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
095200100304     C                   IF        %subst(VABGC1:1) = 'P' or
095300100304     C                             %subst(VABGC1:1) = 'M'
095400100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
095500100304     C                   ENDIF
095600030721     C                   ENDIF
095700030721     C*** GC2
095800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
095900030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
096000100304     C                   IF        %subst(VABGC2:1) = 'P' or
096100100304     C                             %subst(VABGC2:1) = 'M'
096200100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
096300100304     C                   ENDIF
096400030721     C                   ENDIF
096500030721     C*** TSP
096600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
096700030721     C                   EVAL      VABTSP = SkSplitCSV(i)
096800030721     C                   ENDIF
096900040510     C*** CBO
097000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
097100040510     C                   EVAL      VABCBO = SkSplitCSV(i)
097200040510     C                   ENDIF
097300030721     C*** NAS
097400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
097500030721     C                   EVAL      VABNAS = SkSplitCSV(i)
097600030721     C                   ENDIF
097700030721     C*** TIC
097800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
097900030721     C                   EVAL      VABTIC = SkSplitCSV(i)
098000030721     C                   ENDIF
098100030721     C*** GCA
098200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
098300030721     C                   EVAL      VABGCA = SkSplitCSV(i)
098400030721     C                   ENDIF
098500030721     C*** XCO
098600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
098700030721     C                   EVAL      VABXCO = SkSplitCSV(i)
098800030721     C                   ENDIF
098900030721     C*** CTM
099000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
099100030721     C                   EVAL      VABCTM = SkSplitCSV(i)
099200030721     C                   ENDIF
099300030721     C*** TCR
099400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
099500030721     C                   EVAL      VABTCR = SkSplitCSV(i)
099600030721     C                   ENDIF
099700030721     C*** CTS
099800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
099900030721     C                   EVAL      VABCTS = SkSplitCSV(i)
100000030721     C                   ENDIF
100100030721     C*** FTM
100200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
100300030721     C                   EVAL      VABFTM = SkSplitCSV(i)
100400030721     C                   ENDIF
100500030721     C*** VAD
100600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
100700030721     C                   EVAL      VABVAD = SkSplitCSV(i)
100800030721     C                   ENDIF
100900030721     C*** GMA
101000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
101100030721     C                   EVAL      VABGMA = SkSplitCSV(i)
101200030721     C                   ENDIF
101300030721     C*** GGA
101400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
101500030721     C                   EVAL      VABGGA = SkSplitCSV(i)
101600030721     C                   ENDIF
101700030721     C*** GVA
101800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
101900030721     C                   EVAL      VABGVA = SkSplitCSV(i)
102000030721     C                   ENDIF
102100030721     C*** TC1
102200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
102300030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
102400030721     C                   ENDIF
102500030721     C*** TC2
102600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
102700030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
102800030721     C                   ENDIF
102900030721     C*** SCL
103000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
103100030721     C                   EVAL      VABSCL = SkSplitCSV(i)
103200030721     C                   ENDIF
103300030721     C*** RMO
103400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
103500030721     C                   EVAL      VABRMO = SkSplitCSV(i)
103600030721     C                   ENDIF
103700030721     C*** NMO
103800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
103900030721     C                   EVAL      VABNMO = SkSplitCSV(i)
104000030721     C                   ENDIF
104100030721     C*
104200030721     C* Reperisco quindi i campi numerici...
104300040510     C*** NSP
104400040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
104500040510     C                   EVAL      PiStr=SkSplitCSV(i)
104600040510     C                   EXSR      CHKNUM
104700040510     C                   IF        PiInt=*on
104800040510     C                   Z-ADD     PiVal         VABNSP
104900040510     C                   ELSE
105000040510     C                   SETON                                        31
105100040510     C                   EVAL      VABNSP = *zeros
105200040510     C                   EVAL      vinmsg = %trimr(vinmsg)
105300040510     C                             + ' ' + 'VABNSP'
105400040510     C                   ENDIF
105500040510     C                   ENDIF
105600070703     C*** RMN
105700070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
105800070703     C                   EVAL      PiStr=SkSplitCSV(i)
105900070703     C                   EXSR      CHKNUM
106000070703     C                   IF        PiInt=*on
106100070703     C                   Z-ADD     PiVal         VABRMN
106200070703     C                   ELSE
106300070703     C                   SETON                                        32
106400070703     C                   EVAL      vinmsg = %trimr(vinmsg)
106500070703     C                             + ' ' + 'VABRMN'
106600070703     C                   ENDIF
106700070703     C                   ENDIF
106800070705     C*** CAD
106900070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
107000070705     C                   EVAL      VABCAD = SkSplitCSV(i)
107100070705     C                   ENDIF
107200030721     C*** NCL
107300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
107400030721     C                   EVAL      PiStr=SkSplitCSV(i)
107500030721     C                   EXSR      CHKNUM
107600030721     C                   IF        PiInt=*on
107700081007     C  N40              Z-ADD     PiVal         VABNCL
107800081007     C   40              ADD       PiVal         VABNCL
107900030721     C                   ELSE
108000030721     C                   SETON                                        32
108100030721     C                   EVAL      VABNCL = *zeros
108200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
108300040122     C                             + ' ' + 'VABNCL'
108400030721     C                   ENDIF
108500030721     C                   ENDIF
108600030721     C*** PKB
108700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
108800030721     C                   EVAL      PiStr=SkSplitCSV(i)
108900030721     C                   EXSR      CHKNUM
109000030721     C                   IF        PiNum=*on
109100081007     C  N40              Z-ADD     PiVal         VABPKB
109200081007     C   40              ADD       PiVal         VABPKB
109300030721     C                   ELSE
109400030721     C                   SETON                                        32
109500030721     C                   EVAL      VABPKB = *zeros
109600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
109700030721     C                             + ' ' + 'VABPKB'
109800030721     C                   ENDIF
109900030721     C                   ENDIF
110000030721     C*** CAS
110100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
110200030721     C                   EVAL      PiStr=SkSplitCSV(i)
110300030721     C                   EXSR      CHKNUM
110400030721     C                   IF        PiNum=*on
110500030721     C                   Z-ADD     PiVal         VABCAS
110600030721     C                   ELSE
110700030721     C                   SETON                                        32
110800030721     C                   EVAL      VABCAS = *zeros
110900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
111000030721     C                             + ' ' + 'VABCAS'
111100030721     C                   ENDIF
111200030721     C                   ENDIF
111300030721     C*** CCM
111400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
111500030721     C                   EVAL      PiStr=SkSplitCSV(i)
111600030721     C                   EXSR      CHKNUM
111700030721     C                   IF        PiInt=*on      AND
111800030721     C                             PiVal<=9999999 AND
111900030721     C                             PiVal>*zeros
112000030721     C                   Z-ADD     PiVal         VABCCM
112100030721     C                   ELSE
112200030721     C                   SETON                                        32
112300030721     C                   EVAL      VABCCM = *zeros
112400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112500040510     C                             + ' ' + 'VABCCM'
112600030721     C                   ENDIF
112700030721     C                   ENDIF
112800030721     C*** LNP
112900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
113000030721     C                   EVAL      PiStr=SkSplitCSV(i)
113100030721     C                   EXSR      CHKNUM
113200030721     C                   IF        PiInt=*on    AND
113300030721     C                             PiVal<=999   AND
113400030721     C                             PiVal>*zeros
113500030721     C                   Z-ADD     PiVal         VABLNP
113600030721     C                   ELSE
113700040715     C                   SETON                                        32
113800030721     C                   EVAL      VABLNP = *zeros
113900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114000040510     C                             + ' ' + 'VABLNP'
114100030721     C                   ENDIF
114200030721     C                   ENDIF
114300030721     C*** NRS
114400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
114500030721     C                   EVAL      PiStr=SkSplitCSV(i)
114600030721     C                   EXSR      CHKNUM
114700030721     C                   IF        PiInt=*on AND
114800030721     C                             PiVal<=99
114900030721     C                   Z-ADD     PiVal         VABNRS
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115300030721     C                             + ' ' + 'VABNRS'
115400030721     C                   ENDIF
115500030721     C                   ENDIF
115600030721     C*** LNA
115700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
115800030721     C                   EVAL      PiStr=SkSplitCSV(i)
115900030721     C                   EXSR      CHKNUM
116000030721     C                   IF        PiInt=*on    AND
116100030721     C                             PiVal<=999   AND
116200030721     C                             PiVal>*zeros
116300030721     C                   Z-ADD     PiVal         VABLNA
116400030721     C                   ELSE
116500030721     C                   SETON                                        32
116600030721     C                   EVAL      VABLNA = *zeros
116700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116800030721     C                             + ' ' + 'VABLNA'
116900030721     C                   ENDIF
117000030721     C                   ENDIF
117100030721     C*** CTR
117200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
117300030721     C                   EVAL      PiStr=SkSplitCSV(i)
117400030721     C                   EXSR      CHKNUM
117500030721     C                   IF        PiInt=*on  AND
117600030721     C                             PiVal<=999
117700030721     C                   Z-ADD     PiVal         VABCTR
117800030721     C                   ELSE
117900030721     C                   SETON                                        32
118000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118100030721     C                             + ' ' + 'VABCTR'
118200030721     C                   ENDIF
118300030721     C                   ENDIF
118400030721     C*** IAS
118500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
118600030721     C                   EVAL      PiStr=SkSplitCSV(i)
118700030721     C                   EXSR      CHKNUM
118800030721     C                   IF        PiNum=*on
118900030721     C                   Z-ADD     PiVal         VABIAS
119000030721     C                   ELSE
119100030721     C                   SETON                                        32
119200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119300030721     C                             + ' ' + 'VABIAS'
119400030721     C                   ENDIF
119500030721     C                   ENDIF
119600030721     C*** VLB
119700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
119800030721     C                   EVAL      PiStr=SkSplitCSV(i)
119900030721     C                   EXSR      CHKNUM
120000030721     C                   IF        PiNum=*on
120100081007     C  N40              Z-ADD     PiVal         VABVLB
120200081007     C   40              ADD       PiVal         VABVLB
120300030721     C                   ELSE
120400030721     C                   SETON                                        32
120500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
120600030721     C                             + ' ' + 'VABVLB'
120700030721     C                   ENDIF
120800030721     C                   ENDIF
120900030721     C*** QFT
121000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
121100030721     C                   EVAL      PiStr=SkSplitCSV(i)
121200030721     C                   EXSR      CHKNUM
121300030721     C                   IF        PiNum=*on
121400081007     C  N40              Z-ADD     PiVal         VABQFT
121500081007     C   40              ADD       PiVal         VABQFT
121600030721     C                   ELSE
121700030721     C                   SETON                                        32
121800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121900030721     C                             + ' ' + 'VABQFT'
122000030721     C                   ENDIF
122100030721     C                   ENDIF
122200030721     C*** NCD
122300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
122400030721     C                   EVAL      PiStr=SkSplitCSV(i)
122500030721     C                   EXSR      CHKNUM
122600030721     C                   IF        PiInt=*on      AND
122700030721     C                             PiVal<=9999999
122800030721     C                   Z-ADD     PiVal         VABNCD
122900030721     C                   ELSE
123000030721     C                   SETON                                        32
123100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123200030721     C                             + ' ' + 'VABNCD'
123300030721     C                   ENDIF
123400030721     C                   ENDIF
123500030721     C*** NCA
123600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
123700030721     C                   EVAL      PiStr=SkSplitCSV(i)
123800030721     C                   EXSR      CHKNUM
123900030721     C                   IF        PiInt=*on      AND
124000030721     C                             PiVal<=9999999
124100030721     C                   Z-ADD     PiVal         VABNCA
124200030721     C                   ELSE
124300030721     C                   SETON                                        32
124400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124500030721     C                             + ' ' + 'VABNCA'
124600030721     C                   ENDIF
124700030721     C                   ENDIF
124800030721     C*** ZNC
124900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
125000030721     C                   EVAL      PiStr=SkSplitCSV(i)
125100030721     C                   EXSR      CHKNUM
125200030721     C                   IF        PiInt=*on AND
125300030721     C                             PiVal<=99
125400030721     C                   Z-ADD     PiVal         VABZNC
125500030721     C                   ELSE
125600030721     C                   SETON                                        32
125700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
125800030721     C                             + ' ' + 'VABZNC'
125900030721     C                   ENDIF
126000030721     C                   ENDIF
126100030721     C*** DCR
126200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
126300030721     C                   EVAL      PiStr=SkSplitCSV(i)
126400030721     C                   EXSR      CHKNUM
126500030721     C                   IF        PiInt=*on       AND
126600030721     C                             PiVal<=99999999
126700030721     C                   Z-ADD     PiVal         VABDCR
126800030721     C                   ELSE
126900030721     C                   SETON                                        32
127000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127100030721     C                             + ' ' + 'VABDCR'
127200030721     C                   ENDIF
127300030721     C                   ENDIF
127400030721     C*** HCR
127500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
127600030721     C                   EVAL      PiStr=SkSplitCSV(i)
127700030721     C                   EXSR      CHKNUM
127800030721     C                   IF        PiInt=*on   AND
127900030721     C                             PiVal<=9999
128000030721     C                   Z-ADD     PiVal         VABHCR
128100030721     C                   ELSE
128200030721     C                   SETON                                        32
128300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128400030721     C                             + ' ' + 'VABHCR'
128500030721     C                   ENDIF
128600030721     C                   ENDIF
128700030721     C*** VMD
128800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
128900030721     C                   EVAL      PiStr=SkSplitCSV(i)
129000030721     C                   EXSR      CHKNUM
129100030721     C                   IF        PiNum=*on
129200030721     C                   Z-ADD     PiVal         VABVMD
129300030721     C                   ELSE
129400030721     C                   SETON                                        32
129500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
129600030721     C                             + ' ' + 'VABVMD'
129700030721     C                   ENDIF
129800030721     C                   ENDIF
129900030721     C*** ANT
130000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
130100030721     C                   EVAL      PiStr=SkSplitCSV(i)
130200030721     C                   EXSR      CHKNUM
130300030721     C                   IF        PiInt=*on        AND
130400030721     C                             PiVal<=999999999
130500030721     C                   Z-ADD     PiVal         VABANT
130600030721     C                   ELSE
130700030721     C                   SETON                                        32
130800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130900030721     C                             + ' ' + 'VABANT'
131000030721     C                   ENDIF
131100030721     C                   ENDIF
131200030721     C*** CMO
131300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
131400030721     C                   EVAL      PiStr=SkSplitCSV(i)
131500030721     C                   EXSR      CHKNUM
131600030721     C                   IF        PiInt=*on
131700030721     C                   Z-ADD     PiVal         Num5_0
131800030721     C                   MOVEL(p)  Num5_0        VABCMO
131900030721     C                   ELSE
132000030721     C                   SETON                                        32
132100030721     C                   EVAL      VABCMO = *zeros
132200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132300030721     C                             + ' ' + 'VABCMO'
132400030721     C                   ENDIF
132500030721     C                   ENDIF
132600040510     C*
132700040510     C* Reperisco i campi del FIVAT
132800040510     C*** VATNOT
132900040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
133000040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
133100040510     C                   ENDIF
133200040510     C*** VATTRC
133300040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
133400040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
133500040510     C                   ENDIF
133600060331     C*** VATNOT_A
133700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
133800060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
133900060331     C                   ENDIF
134000060331     C*** VATNOT_B
134100060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
134200060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
134300060331     C                   ENDIF
134400060331     C*** VATNOT_E
134500060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
134600060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
134700060331     C                   ENDIF
134800110912     C*** VATNOT_I
134900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
135000110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
135100110912     C                   ENDIF
135200110912     C*** VATNOT_J
135300110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
135400110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
135500110912     C                   ENDIF
135600110912     C*** VATNOT_4
135700110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
135800110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
135900110912     C                   ENDIF
136000131018     C*** VATNOT_S
136100131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
136200131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
136300140507     C                   IF        %trim(%subst(wVATNOT_S:1:16)) = '.' OR
136400140507     C                             %trim(%subst(wVATNOT_S:1:16)) = '0'
136500140507     C                   EVAL      %subst(wVATNOT_S:1:16) = *blanks
136600140507     C                   ENDIF
136700131018     C                   ENDIF
136800141128     C*** VATNOT_Y
136900141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
137000141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
137100141128     C                   ENDIF
137200151109     C*
137300151109     C* Forzature e/o Personalizzazioni
137400151109     C*
137500170622     C*** CAS
137600170621     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABCAS'
137700170622     C                   EVAL      PiStr='0'+%trim(%subst(SkSplitCSV(i):3))
137800170621     C                   EXSR      CHKNUM
137900170621     C                   IF        PiNum=*on
138000170621     C                   Z-ADD     PiVal         VABCAS
138100170621     C                   ELSE
138200170621     C                   SETON                                        32
138300170621     C                   EVAL      VABCAS = *zeros
138400170621     C                   EVAL      vinmsg = %trimr(vinmsg)
138500170621     C                             + ' ' + 'VABCAS'
138600170621     C                   ENDIF
138700170621     C                   ENDIF
138800170622     C*** RMN / NSP
138900170622     C                   IF        %trim(SkSplitFLD(i)) = 'W_VABRMN'
139000170622     C                   EVAL      PiStr=SkSplitCSV(i)
139100170622     C                   EXSR      CHKNUM
139200170622     C                   IF        PiInt=*on
139300170622     C                   Z-ADD     PiVal         VABRMN
139400170622     C                   Z-ADD     PiVal         VABNSP
139500170622     C                   ELSE
139600170622     C                   SETON                                        31
139700170622     C                   EVAL      vinmsg = %trimr(vinmsg)
139800170622     C                             + ' ' + 'VABRMN VABNSP'
139900170622     C                   ENDIF
140000170622     C                   ENDIF
140100131018     C*
140200030715     C                   ENDIF
140300030715     C                   EVAL      i = i + 1
140400030715     C                   ENDDO
140500040510     C*
140600040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
140700040510     C                   IF        VABNSP = *zeros
140800040510     C                   SETON                                        31
140900040510     C                   ENDIF
141000040715     C*
141100040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
141200040715     C                   IF        VABLNP = *zeros
141300040715     C                   SETON                                        31
141400040715     C                   ENDIF
141500040510     C*
141600040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
141700040510     C                   IF        VABRMN = *zeros
141800040510     C                   Z-ADD     VABNSP        VABRMN
141900040510     C                   ENDIF
142000040510     C*
142100040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
142200040510     C                   IF        VABRMA = *blanks
142300040714     C                   MOVEL     VABRMN        VABRMA
142400040510     C                   ENDIF
142500070705     C*
142600070705     C* Considerazioni particolari
142700070705     C*** NZD
142800070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
142900070705     C                   EVAL      VABNZD = *blanks
143000070705     C                   ENDIF
143100070705     C*** CAD
143200070705     C                   IF        VABNZD = *blanks
143300070705     C                   EVAL      PiStr=VABCAD
143400070705     C                   EXSR      CHKNUM
143500070705     C                   IF        PiInt=*on
143600070705     C                   Z-ADD     PiVal         Num5_0
143700070705     C                   MOVEL(p)  Num5_0        VABCAD
143800070705     C                   ELSE
143900070705     C                   SETON                                        32
144000070705     C                   EVAL      VABCAD = *zeros
144100070705     C                   EVAL      vinmsg = %trimr(vinmsg)
144200070705     C                             + ' ' + 'VABCAD'
144300070705     C                   ENDIF
144400070705     C                   ENDIF
144500040128     C*
144600040510     C* Se provincia nn valorizzata la reperisco
144700040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
144800040510     C                   IF        VABPRD = *blanks
144900040128     C                   CLEAR                   TISI95DS
145000040128     C                   EVAL      I95TCN = '3'
145100040128     C                   Z-ADD     datcor        I95DAT
145200040128     C                   EVAL      I95NAR = VABNZD
145300040128     C                   EVAL      I95CAP = VABCAD
145400040128     C                   EVAL      I95LOC = VABLOD
145500040128     C                   CALL      'TISI95R'
145600040128     C                   PARM                    TISI95DS
145700040128     C                   EVAL      VABPRD = O95PRV
145800040510     C                   ENDIF
145900040113     C*
146000040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
146100040113     C                   IF        VABCAS > *zeros
146200040119     C                   EVAL      VABVCA = 'EUR'
146300040119     C                   IF        VABCBO = '1'
146400040113     C                   EVAL      VABCBO = '4'
146500040119     C                   ENDIF
146600040119     C                   IF        VABCBO = '2'
146700040119     C                   EVAL      VABCBO = '6'
146800040119     C                   ENDIF
146900040113     C                   ENDIF
147000020204     C*
147100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
147200020204     C                   EXSR      CHKIMPDIV
147300030715     C                   ENDIF
147400020204     C*
147500010607     C                   ENDSR
147600010607     C*----------------------------------------------------*
147700010601
147800020204
147900020204     C*----------------------------------------------------*
148000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
148100020204     C*----------------------------------------------------*
148200020204     C     CHKIMPDIV     BEGSR
148300020204     C*
148400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
148500020204     C                   Z-ADD     *zeros        wrkDec            9 9
148600020204     C*
148700020204     C* Come prima cosa effettuo considerazioni sulla divisa
148800020204     C                   IF        vabIAS > *zeros
148900020204     C                   IF        vabVAS <> 'EUR'
149000020204     C                   EVAL      vabVAS =  'ITL'
149100020204     C                   ENDIF
149200020204     C                   ENDIF
149300020204     C*
149400020204     C                   IF        vabCAS > *zeros
149500020204     C                   IF        vabVCA <> 'EUR'
149600020204     C                   EVAL      vabVCA =  'ITL'
149700020204     C                   ENDIF
149800020204     C                   ENDIF
149900020204     C*
150000020204     C                   IF        vabVMD > *zeros
150100020204     C                   IF        vabVAD <> 'EUR'
150200020204     C                   EVAL      vabVAD =  'ITL'
150300020204     C                   ENDIF
150400020204     C                   ENDIF
150500020204     C*
150600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
150700020204     C                   Z-ADD     vabIAS        wrkDec
150800020204     C                   IF        wrkDec > *zeros
150900020204     C                   IF        vabVAS = 'ITL'
151000020204     C                   EVAL      vabIAS = *zeros
151100020204     C                   ENDIF
151200020204     C                   ENDIF
151300020204     C*
151400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
151500020204     C                   Z-ADD     vabCAS        wrkDec
151600020204     C                   IF        wrkDec > *zeros
151700020204     C                   IF        vabVCA = 'ITL'
151800020204     C                   EVAL      vabCAS = *zeros
151900020204     C                   ENDIF
152000020204     C                   ENDIF
152100020204     C*
152200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
152300020204     C                   Z-ADD     vabVMD        wrkDec
152400020204     C                   IF        wrkDec > *zeros
152500020204     C                   IF        vabVAD = 'ITL'
152600020204     C                   EVAL      vabVMD = *zeros
152700020204     C                   ENDIF
152800020204     C                   ENDIF
152900020204     C*
153000020204     C                   ENDSR
153100020204     C***
153200020204
153300010330
153400010330
153500010330     C*----------------------------------------------------*
153600010330     C*  CONTROLLO NUMERICITA' CAMPI
153700010330     C*----------------------------------------------------*
153800010330     C     CHKNUM        BEGSR
153900010330     C*
154000010606     C                   IF        PiDecChr = *blanks
154100030715     C                   EVAL      PiDecChr = CharNUM
154200010606     C                   ENDIF
154300010606     C*
154400010606     C                   CALL(e)   'ISNUMERIC'
154500010330     C                   PARM                    PiStr            30
154600010606     C                   PARM                    PiDecChr          1
154700010330     C                   PARM      *ZEROS        PiVal            30 9
154800010330     C                   PARM      '0'           PiInt             1
154900010330     C                   PARM      '0'           PiNum             1
155000010330     C                   IF        %error
155100010606     C                   EVAL      PiNum=*off
155200010330     C                   ENDIF
155300010330     C*
155400010330     C                   ENDSR
155500010330     C***
155600010330
155700010601
155800010601
155900010601
156000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
156100010601     C     repfil        BEGSR
156200010601     C*
156300010601     C                   if        invfil = *zeros and
156400010601     C                             depfil > *zeros and
156500010629     C                             (vinflg = *blanks or
156600010629     C                              vinflg = *zeros)
156700010601     C
156800010601     C                   eval      invfil = depfil
156900010601     C                   endif
157000010601     C*
157100010601     C                   if        depfil <> invfil and
157200010601     C                             invfil > *zeros
157300010601     C                   eval      flgMulti = '1'
157400010601     C                   if        vinflg = *blanks
157500010601     C                   add       1             cntNonEl
157600010601     C                   endif
157700010601     C                   endif
157800010601     C*
157900010601     C                   if        vinflg = '2'
158000010601     C                   eval      flgStato = '2'
158100010601     C                   endif
158200010601     C*
158300010601     C                   ENDSR
158400010601     C***
158500010601
158600010601
158700010601
158800010330
158900040119      /TITLE Invio dei dati al punto operativo.
159000040119     C     invio         BEGSR
159100040119     C*
159200040119     C* 1° invio FIVAT
159300040119     C                   reset                   dscmz
159400070704     C                   move      winvcmz       cmzdst
159500040119     C                   eval      cmzfld = 'FIVATWWR'
159600040119     C                   eval      cmzmbd = vlrhdl
159700040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
159800040119     C***                if        prmfir = *blanks
159900040119     C                   eval      cmzfla = 'FIVAT00F'
160000040119     C                   eval      cmzmba = 'FIVAT00F'
160100040119     C***                else
160200040119     C***                eval      cmzfla = prmfir
160300040119     C***                eval      cmzmba = prmfir
160400040119     C***                endif
160500040119     C                   eval      cmznrr = *zeros
160600040119     C                   move      §ctrokvt      cmznrr
160700040119     C                   eval      cmzlba = vlrfl1
160800040119     C                   call(e)   'TIS711C'
160900040119     C                   parm                    dscmz
161000040119     C                   parm      *blanks       esito
161100040119     C                   if        %error
161200040119     C                             or cmzerr = '1'
161300040119     C                             or esito  = '1'
161400040119     C                   eval      wrkesito = '3'
161500040119     C                   else
161600040119     C*
161700040119     C* 2° invio FIVAB
161800040119     C                   reset                   dscmz
161900070704     C                   move      winvcmz       cmzdst
162000040119     C                   eval      cmzfld = vlrfou
162100040119     C                   eval      cmzmbd = vlrhdl
162200040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
162300040119     C***                if        prmfir = *blanks
162400040119     C                   eval      cmzfla = 'FIVAB00F'
162500040119     C                   eval      cmzmba = 'FIVAB00F'
162600040119     C***                else
162700040119     C***                eval      cmzfla = prmfir
162800040119     C***                eval      cmzmba = prmfir
162900040119     C***                endif
163000040119     C                   eval      cmznrr = *zeros
163100040119     C                   move      §ctrokvb      cmznrr
163200040119     C                   eval      cmzlba = vlrfl1
163300040119     C                   call(e)   'TIS711C'
163400040119     C                   parm                    dscmz
163500040119     C                   parm      *blanks       esito
163600040119     C                   if        %error
163700040119     C                             or cmzerr = '1'
163800040119     C                             or esito  = '1'
163900040119     C                   eval      wrkesito = '3'
164000040119     C                   endif
164100040119     C                   endif
164200040119     C*
164300040119     C                   ENDSR
164400040119     C***
164500010601
164600010601
164700010601
164800010601
164900010601      /TITLE Invio dei dati al punto operativo.
165000010601     C     opeini        BEGSR
165100010601     C*
165200010601     C* Inizializzo flag e contatori operativi
165300010601     C                   movel     '0'           flgGiro           1
165400010601     C                   movel     '0'           flgMulti          1
165500010601     C                   movel     '1'           flgStato          1
165600010615     C                   movel     '0'           flgOk             1
165700010601     C                   z-add     *zeros        cntNonEl         10 0
165800010601     C                   z-add     *zeros        depfil            3 0
165900010601     C                   z-add     *zeros        invfil            3 0
166000010601     C*
166100010601     C                   ENDSR
166200010601     C***
166300070326
166400070326
166500070326
166600070326
166700070326     C     *pssr         BEGSR
166800070329     C*
166900070329     C                   if        %open(tivin00r)
167000070329     C                   close     tivin00r
167100070329     C                   endif
167200070329     C                   if        %open(fivabwwr)
167300070329     C                   close     fivabwwr
167400070329     C                   endif
167500070329     C                   if        %open(fivatwwr)
167600070329     C                   close     fivatwwr
167700070329     C                   endif
167800070326     C*
167900070326     C* Effettuo la chiamata al CLLE preposto
168000070326     C                   call(e)   'TITVVTC'
168100070326     C                   parm                    parccm
168200070326     C                   parm                    parmbr
168300070326     C                   parm      '2'           paropz
168400070326     C*
168500070326     C                   eval      wrkesito = '2'
168600070404     C*
168700070404     C                   seton                                        LR
168800070326     C*
168900070326     C                   ENDSR     '*CANCL'
169000070326     C***
169100070326
169200070326
169300010330
169400010330
169500000613     C     *inzsr        BEGSR
169600990910     C*
169700990910     C     *entry        plist
169800990920     C                   parm                    tivlrds
169900990921     C                   parm      wrkesito      esito
170000000724     C                   parm                    prmlit
170100000710     C                   parm                    prmfir
170200010330     C*
170300010330     C* CALCOLA LA DATA CORRENTE
170400151109     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
170500151109     C                   eval      datcor = %dec(%date() : *ISO)
170600000613     C*
170700000613     C                   ENDSR
170800000613     C***
170900040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
171000170622X;VABRMA;X;X;VABRSD;VABIND;VABCAD;VABLOD;VABPRD;VATNOT_B;X;
171100170622W_VABRMN;X;VABPKB;VABNCL;X;W_VABCAS;
171200040113
