000100071120      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500071120     FEDIVABwr  O    E             DISK    usropn
000600071120     FEDIVATwr  O    E             DISK    usropn
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
003800090403     D CharSOS         S              1
003900030715     D posDa           S              3  0 INZ(*zeros)
004000030715     D posA            S              3  0 INZ(*zeros)
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
011200071121     C                   call(e)   'TITVEVTC'
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
012900071120     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
013000010330     C*--------------------------------------------------------
013100010605     C     RWVAB         BEGSR
013200010330     C*
013300010330     C                   if        not %open(tivin00r)
013400010330     C                   open      tivin00r
013500010330     C                   endif
013600071120     C                   if        not %open(edivabwr)
013700071120     C                   open      edivabwr
013800010330     C                   endif
013900040119     C*
014000071120     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
014100040119     C                   exsr      prevat
014200040119     C*
014300040119     C                   if        chkcall = '0'
014400040119     C*
014500071120     C                   if        not %open(edivatwr)
014600071120     C                   open      edivatwr
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
016600040510     C                   dow       wLenVINDTA < %len(%trim(vindta))
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
018700010601     C*
018800010330     C                   exsr      inzvar
018900090403     C*
019000090403     C* Se previsti caratteri delimitatori testo provvedo a verificare i caratteri
019100090403     C* separatori campi
019200090403     C                   if        CharTXT <> *blanks
019300090403     C                   z-add     *zeros        wLenVINDTA        4 0
019400090403     C                   z-add     *zeros        wFlgVINDTA        1 0
019500090403     C                   setoff                                       80
019600090403     C                   dow       wLenVINDTA < %len(%trim(vindta))
019700090403     C                   eval      wLenVINDTA = wLenVINDTA + 1
019800090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharTXT
019900090403     C                   if        not *in80
020000090403     C                   seton                                        80
020100090403     C                   else
020200090403     C                   setoff                                       80
020300090403     C                   endif
020400090403     C                   endif
020500090403     C                   if        %subst(vindta:wLenVINDTA:1)=CharCSV
020600090403     C                             and *in80 = *on
020700090403     C                   eval      %subst(vindta:wLenVINDTA:1)=CharSOS
020800090403     C                   endif
020900090403     C                   enddo
021000090403     C                   endif
021100090403     C*
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
029100071120     C                   if        %open(edivabwr)
029200071120     C                   close     edivabwr
029300010330     C                   endif
029400071120     C                   if        %open(edivatwr)
029500071120     C                   close     edivatwr
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
031300071120
031400071120
031500071120
031600071120     C*----------------------------------------------------*
031700071120     C*  VALORIZZAZIONE CAMPI CMR
031800071120     C*----------------------------------------------------*
031900071120     C     VALCMR        BEGSR
032000071120     C*
032100071120     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
032200071120     C                   EVAL      VABCMR = %char(DATCOR)+%char(ORACOR)
032300071120     C                   EVAL      VABDCM = DATCOR
032400071120     C                   EVAL      VABDTS = DATCOR
032500071120     C                   EVAL      VABHMS = ORACOR
032600071120     C                   EVAL      VABCNT = 1
032700071120     C*
032800071120     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
032900071120     C                   EVAL      VATCMR = %char(DATCOR)+%char(ORACOR)
033000071120     C                   EVAL      VATCNT = 1
033100071120     C*
033200071120     C                   ENDSR
033300071120
033400071120
033500071120
033600040119
033700040119
033800040119
033900040119     C*----------------------------------------------------*
034000040119     C*  SCARICAMENTO BUFFER RECORDS VAB
034100040119     C*----------------------------------------------------*
034200040510     C     WRIVAB        BEGSR
034300040119     C*
034400060331     C* Gestisco l'eventuale rottura x numero spedizione
034500060331     C                   if        VABNSP <> wVABNSP
034600070301     C                   if        wVATNOT_E <> *blanks
034700070301     C                   eval      VABCTM = '7Q'
034800070301     C                   endif
034900071120     C                   exsr      VALCMR
035000071120     C                   write     edivab00                                     => scarico il VAB
035100060331     C                   eval      wVABNSP = VABNSP
035200060331     C                   endif
035300040119     C*
035400040119     C                   ENDSR
035500040119
035600040119
035700040119
035800040119     C*----------------------------------------------------*
035900040119     C*  SCARICAMENTO BUFFER RECORDS VAT
036000040119     C*----------------------------------------------------*
036100040510     C     WRIVAT        BEGSR
036200060512     C*
036300060512     C* Inizializzo i campi chiave
036400060512     C                   eval      VATFGS = VABFGS
036500060512     C                   eval      VATAAS = VABAAS
036600060512     C                   eval      VATCCM = VABCCM
036700060512     C                   eval      VATNSP = VABNSP
036800060512     C                   eval      VATLNP = VABLNP
036900040714     C*
037000040714     C* Solo se VATNOT valorizzato scarico il buffer
037100040714     C                   if        VATNOT <> *blanks
037200040119     C*
037300040510     C* Se tipo record nn espressamente indicato assumo x default (e x diffusione d uso) = 'E'
037400040510     C                   if        VATTRC = *blanks
037500040510     C                   eval      VATTRC = 'E'
037600040510     C                   endif
037700071120     C                   exsr      VALCMR
037800071120     C                   write     EDIVAT00
037900040119     C                   add       1             §CTROKVT
038000040714     C*
038100040714     C                   endif
038200060331     C*
038300060331     C* Gestisco estensioni VAT "specificatamente esplicitate"
038400060331     C                   if        wVATNOT_A <> *blanks
038500060331     C                   eval      VATNOT = wVATNOT_A
038600060331     C                   eval      VATTRC = 'A'
038700071120     C                   exsr      VALCMR
038800071120     C                   write     EDIVAT00
038900060331     C                   add       1             §CTROKVT
039000060331     C                   endif
039100060331     C*
039200060331     C                   if        wVATNOT_B <> *blanks
039300060331     C                   eval      VATNOT = wVATNOT_B
039400060331     C                   eval      VATTRC = 'B'
039500071120     C                   exsr      VALCMR
039600071120     C                   write     EDIVAT00
039700060331     C                   add       1             §CTROKVT
039800060331     C                   endif
039900060331     C*
040000060331     C                   if        wVATNOT_E <> *blanks
040100060331     C                   eval      VATNOT = wVATNOT_E
040200060331     C                   eval      VATTRC = 'E'
040300071120     C                   exsr      VALCMR
040400071120     C                   write     EDIVAT00
040500060331     C                   add       1             §CTROKVT
040600060331     C                   endif
040700110912     C*
040800110912     C                   if        wVATNOT_I <> *blanks
040900110912     C                   eval      VATNOT = wVATNOT_I
041000110912     C                   eval      VATTRC = 'I'
041100110912     C                   exsr      VALCMR
041200110912     C                   write     EDIVAT00
041300110912     C                   add       1             §CTROKVT
041400110912     C                   endif
041500110912     C*
041600110912     C                   if        wVATNOT_J <> *blanks
041700110912     C                   eval      VATNOT = wVATNOT_J
041800110912     C                   eval      VATTRC = 'J'
041900110912     C                   exsr      VALCMR
042000110912     C                   write     EDIVAT00
042100110912     C                   add       1             §CTROKVT
042200110912     C                   endif
042300110912     C*
042400110912     C                   if        wVATNOT_4 <> *blanks
042500110912     C                   eval      VATNOT = wVATNOT_4
042600110912     C                   eval      VATTRC = '4'
042700110912     C                   exsr      VALCMR
042800110912     C                   write     EDIVAT00
042900110912     C                   add       1             §CTROKVT
043000110912     C                   endif
043100131018     C*
043200131018     C                   if        wVATNOT_S <> *blanks
043300131018     C                   eval      VATNOT = wVATNOT_S
043400131018     C                   eval      VATTRC = 'S'
043500131018     C                   exsr      VALCMR
043600131018     C                   write     EDIVAT00
043700131018     C                   add       1             §CTROKVT
043800131018     C                   endif
043900141128     C*
044000141128     C                   if        wVATNOT_Y <> *blanks
044100141128     C                   eval      VATNOT = wVATNOT_Y
044200141128     C                   eval      VATTRC = 'Y'
044300141128     C                   exsr      VALCMR
044400141128     C                   write     EDIVAT00
044500141128     C                   add       1             §CTROKVT
044600141128     C                   endif
044700040119     C*
044800040119     C                   ENDSR
044900040119
045000040119
045100040119
045200040119     C*----------------------------------------------------*
045300071120     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
045400040119     C*----------------------------------------------------*
045500040119     C     PREVAT        BEGSR
045600040119     C*
045700071120     C* Compongo il nome del membro da dare al EDIVATWR
045800040119     C                   eval      parmbr = vlrhdl
045900040119     C                   movel     'M'           parmbr
046000040510     C                   eval      parccm = vlrksc
046100040119     C                   eval      paropz = '1'
046200040119     C* Effettuo la chiamata al CLLE preposto
046300071121     C                   call(e)   'TITVEVTC'
046400040119     C                   parm                    parccm
046500040119     C                   parm                    parmbr
046600040119     C                   parm                    paropz
046700040119     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
046800040119     C                   if        %error
046900040119     C                   movel     '1'           chkcall
047000040119     C                   else
047100040119     C                   movel     '0'           chkcall
047200040119     C                   endif
047300040119     C*
047400040119     C                   ENDSR
047500010601
047600010601
047700010601
047800010330     C*----------------------------------------------------*
047900030715     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
048000010330     C*----------------------------------------------------*
048100040119     C     INZVAR        BEGSR
048200010330     C*
048300030822     C* Inizializzo variabili di wrk
048400020204     C                   Z-ADD     *zeros        Num5_0            5 0
048500090403     C                   z-add     *zeros        invfil
048600060331     C                   MOVEL     *blanks       wVATNOT_A
048700060331     C                   MOVEL     *blanks       wVATNOT_B
048800060331     C                   MOVEL     *blanks       wVATNOT_E
048900110912     C                   MOVEL     *blanks       wVATNOT_I
049000110912     C                   MOVEL     *blanks       wVATNOT_J
049100110912     C                   MOVEL     *blanks       wVATNOT_4
049200131018     C                   MOVEL     *blanks       wVATNOT_S
049300141128     C                   MOVEL     *blanks       wVATNOT_Y
049400030822     C*
049500030822     C* Inizializzo il buffer del record da scrivere e la schiera d wrk x i dati
049600071120     C                   CLEAR                   EDIVAB00
049700071120     C                   CLEAR                   EDIVAT00
049800030822     C                   CLEAR                   SkSplitCSV
049900030822     C                   EVAL      i = 1
050000030822     C                   EVAL      posDa = *zeros
050100030822     C                   EVAL      posA  = *zeros
050200030822     C*
050300030822     C* Reimposto i valori di default
050400030822     C                   EXSR      DEFCAM
050500030822     C*
050600010330     C                   ENDSR
050700010330     C*----------------------------------------------------*
050800030822     C*  IMPOSTAZIONE CAMPI COSTANTI/VARIABILI DI DEFAULT
050900010330     C*----------------------------------------------------*
051000010330     C     DEFCAM        BEGSR
051100030822     C*
051200030715     C* Reperisco dai parametri i caratteri che identificano i caratteri d separatore campo
051300030715     C* e delimitatore testo.
051400030715     C                   EVAL      CharCSV = %subst(vlrppt:2:1)
051500030715     C                   EVAL      CharTXT = %subst(vlrppt:3:1)
051600030715     C                   EVAL      CharNUM = %subst(vlrppt:4:1)
051700090403     C*
051800090403     C* Determino il carattere sostituente il separatore decimale in caso d conflitto
051900090403     C                   EVAL      CharSOS = CharNUM
052000030822     C*
052100030715     C* Reperisco il flag che mi indica se effettuare o meno la stampa in filiale
052200030715     C                   IF        %subst(vlrppt:1:1) = 'S'
052300030715     C                   SETON                                        50
052400030715     C                   ELSE
052500030715     C                   SETOFF                                       50
052600030715     C                   ENDIF
052700030822     C*
052800030822     C* Reperisco i parametri relativi ai default dei campi "anagrafici"
052900030822     C                   EVAL      posDaDft = 1
053000030822     C                   EVAL      posADft  = 0
053100030822     C                   EVAL      wGiroDft = 0
053200030822     C                   DOW       posDaDft <= %len(%trim(vlrppt)) AND
053300030822     C                             posDaDft > 0
053400030822     C*
053500030822     C* Gestisco il 1° giro
053600030822     C                   IF        wGiroDft = 0
053700030822     C* Eseguo lo scan x trovare l'inizio del campo corrente
053800030822     C                   EVAL      posDaDft = %scan('/':vlrppt:posADft+1)
053900030822     C* Incremento il contatore dei "giri"
054000030822     C                   EVAL      wGiroDft = 1
054100030822     C                   ELSE
054200030822     C                   EVAL      posDaDft = posADft
054300030822     C                   ENDIF
054400030822     C* Eseguo lo scan x trovare la fine del campo corrente
054500030822     C                   EVAL      posADft = %scan('/':vlrppt:posDaDft+1)
054600030822     C*
054700030822     C* A questo "estraggo" il parametro (campo e valore) corrente...
054800030822     C* ...solo se entrambe le posizini (DA/A) sono > 0
054900030822     C                   IF        posDaDft > 0 AND
055000030822     C                             posADft  > 0
055100030822     C* NCL
055200030822     C                   IF        %subst(
055300030822     C                             %subst(vlrppt:posDaDft+1:
055400030822     C                             posADft-posDaDft-1):1:3)
055500030822     C                             = 'NCL'
055600030822     C                   EVAL      PiStr=%trim(%subst(
055700030822     C                             %subst(vlrppt:posDaDft+1:
055800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
055900030822     C                   EXSR      CHKNUM
056000030822     C                   IF        PiInt=*on
056100030822     C                   Z-ADD     PiVal         VABNCL
056200030822     C                   ENDIF
056300030822     C                   ENDIF
056400030822     C* CCM
056500030822     C                   IF        %subst(
056600030822     C                             %subst(vlrppt:posDaDft+1:
056700030822     C                             posADft-posDaDft-1):1:3)
056800030822     C                             = 'CCM'
056900030822     C                   EVAL      PiStr=%trim(%subst(
057000030822     C                             %subst(vlrppt:posDaDft+1:
057100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
057200030822     C                   EXSR      CHKNUM
057300030822     C                   IF        PiInt=*on
057400030822     C                   Z-ADD     PiVal         VABCCM
057500030822     C                   ENDIF
057600030822     C                   ENDIF
057700030822     C* LNP
057800030822     C                   IF        %subst(
057900030822     C                             %subst(vlrppt:posDaDft+1:
058000030822     C                             posADft-posDaDft-1):1:3)
058100030822     C                             = 'LNP'
058200030822     C                   EVAL      PiStr=%trim(%subst(
058300030822     C                             %subst(vlrppt:posDaDft+1:
058400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
058500030822     C                   EXSR      CHKNUM
058600030822     C                   IF        PiInt=*on
058700030822     C                   Z-ADD     PiVal         VABLNP
058800030822     C                   ENDIF
058900030822     C                   ENDIF
059000030822     C* NRS
059100030822     C                   IF        %subst(
059200030822     C                             %subst(vlrppt:posDaDft+1:
059300030822     C                             posADft-posDaDft-1):1:3)
059400030822     C                             = 'NRS'
059500030822     C                   EVAL      PiStr=%trim(%subst(
059600030822     C                             %subst(vlrppt:posDaDft+1:
059700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
059800030822     C                   EXSR      CHKNUM
059900030822     C                   IF        PiInt=*on
060000030822     C                   Z-ADD     PiVal         VABNRS
060100030822     C                   ENDIF
060200030822     C                   ENDIF
060300030822     C* CTR
060400030822     C                   IF        %subst(
060500030822     C                             %subst(vlrppt:posDaDft+1:
060600030822     C                             posADft-posDaDft-1):1:3)
060700030822     C                             = 'CTR'
060800030822     C                   EVAL      PiStr=%trim(%subst(
060900030822     C                             %subst(vlrppt:posDaDft+1:
061000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
061100030822     C                   EXSR      CHKNUM
061200030822     C                   IF        PiInt=*on
061300030822     C                   Z-ADD     PiVal         VABCTR
061400030822     C                   ENDIF
061500030822     C                   ENDIF
061600030822     C* PKB
061700030822     C                   IF        %subst(
061800030822     C                             %subst(vlrppt:posDaDft+1:
061900030822     C                             posADft-posDaDft-1):1:3)
062000030822     C                             = 'PKB'
062100030822     C                   EVAL      PiStr=%trim(%subst(
062200030822     C                             %subst(vlrppt:posDaDft+1:
062300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
062400030822     C                   EXSR      CHKNUM
062500030822     C                   IF        PiNum=*on
062600030822     C                   Z-ADD     PiVal         VABPKB
062700030822     C                   ENDIF
062800030822     C                   ENDIF
062900030822     C* VLB
063000030822     C                   IF        %subst(
063100030822     C                             %subst(vlrppt:posDaDft+1:
063200030822     C                             posADft-posDaDft-1):1:3)
063300030822     C                             = 'VLB'
063400030822     C                   EVAL      PiStr=%trim(%subst(
063500030822     C                             %subst(vlrppt:posDaDft+1:
063600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
063700030822     C                   EXSR      CHKNUM
063800030822     C                   IF        PiNum=*on
063900030822     C                   Z-ADD     PiVal         VABVLB
064000030822     C                   ENDIF
064100030822     C                   ENDIF
064200030822     C* QFT
064300030822     C                   IF        %subst(
064400030822     C                             %subst(vlrppt:posDaDft+1:
064500030822     C                             posADft-posDaDft-1):1:3)
064600030822     C                             = 'QFT'
064700030822     C                   EVAL      PiStr=%trim(%subst(
064800030822     C                             %subst(vlrppt:posDaDft+1:
064900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
065000030822     C                   EXSR      CHKNUM
065100030822     C                   IF        PiNum=*on
065200030822     C                   Z-ADD     PiVal         VABQFT
065300030822     C                   ENDIF
065400030822     C                   ENDIF
065500030822     C* CBO
065600030822     C                   IF        %subst(
065700030822     C                             %subst(vlrppt:posDaDft+1:
065800030822     C                             posADft-posDaDft-1):1:3)
065900030822     C                             = 'CBO'
066000030822     C                   EVAL      VABCBO=%trim(%subst(
066100030822     C                             %subst(vlrppt:posDaDft+1:
066200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
066300030822     C                   ENDIF
066400030822     C* TSP
066500030822     C                   IF        %subst(
066600030822     C                             %subst(vlrppt:posDaDft+1:
066700030822     C                             posADft-posDaDft-1):1:3)
066800030822     C                             = 'TSP'
066900030822     C                   EVAL      VABTSP=%trim(%subst(
067000030822     C                             %subst(vlrppt:posDaDft+1:
067100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
067200030822     C                   ENDIF
067300030822     C* VAS
067400030822     C                   IF        %subst(
067500030822     C                             %subst(vlrppt:posDaDft+1:
067600030822     C                             posADft-posDaDft-1):1:3)
067700030822     C                             = 'VAS'
067800030822     C                   EVAL      VABVAS=%trim(%subst(
067900030822     C                             %subst(vlrppt:posDaDft+1:
068000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
068100030822     C                   ENDIF
068200030822     C* VCA
068300030822     C                   IF        %subst(
068400030822     C                             %subst(vlrppt:posDaDft+1:
068500030822     C                             posADft-posDaDft-1):1:3)
068600030822     C                             = 'VCA'
068700030822     C                   EVAL      VABVCA=%trim(%subst(
068800030822     C                             %subst(vlrppt:posDaDft+1:
068900030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069000030822     C                   ENDIF
069100030822     C* GCA
069200030822     C                   IF        %subst(
069300030822     C                             %subst(vlrppt:posDaDft+1:
069400030822     C                             posADft-posDaDft-1):1:3)
069500030822     C                             = 'GCA'
069600030822     C                   EVAL      VABGCA=%trim(%subst(
069700030822     C                             %subst(vlrppt:posDaDft+1:
069800030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
069900030822     C                   ENDIF
070000030822     C* CTM
070100030822     C                   IF        %subst(
070200030822     C                             %subst(vlrppt:posDaDft+1:
070300030822     C                             posADft-posDaDft-1):1:3)
070400030822     C                             = 'CTM'
070500030822     C                   EVAL      VABCTM=%trim(%subst(
070600030822     C                             %subst(vlrppt:posDaDft+1:
070700030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
070800030822     C                   ENDIF
070900030822     C* FFD
071000030822     C                   IF        %subst(
071100030822     C                             %subst(vlrppt:posDaDft+1:
071200030822     C                             posADft-posDaDft-1):1:3)
071300030822     C                             = 'FFD'
071400030822     C                   EVAL      VABFFD=%trim(%subst(
071500030822     C                             %subst(vlrppt:posDaDft+1:
071600030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
071700030822     C                   ENDIF
071800030822     C* VAD
071900030822     C                   IF        %subst(
072000030822     C                             %subst(vlrppt:posDaDft+1:
072100030822     C                             posADft-posDaDft-1):1:3)
072200030822     C                             = 'VAD'
072300030822     C                   EVAL      VABVAD=%trim(%subst(
072400030822     C                             %subst(vlrppt:posDaDft+1:
072500030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
072600030822     C                   ENDIF
072700030822     C* GMA
072800030822     C                   IF        %subst(
072900030822     C                             %subst(vlrppt:posDaDft+1:
073000030822     C                             posADft-posDaDft-1):1:3)
073100030822     C                             = 'GMA'
073200030822     C                   EVAL      VABGMA=%trim(%subst(
073300030822     C                             %subst(vlrppt:posDaDft+1:
073400030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
073500030822     C                   ENDIF
073600030822     C* GGA
073700030822     C                   IF        %subst(
073800030822     C                             %subst(vlrppt:posDaDft+1:
073900030822     C                             posADft-posDaDft-1):1:3)
074000030822     C                             = 'GGA'
074100030822     C                   EVAL      VABGGA=%trim(%subst(
074200030822     C                             %subst(vlrppt:posDaDft+1:
074300030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
074400030822     C                   ENDIF
074500030822     C* GVA
074600030822     C                   IF        %subst(
074700030822     C                             %subst(vlrppt:posDaDft+1:
074800030822     C                             posADft-posDaDft-1):1:3)
074900030822     C                             = 'GVA'
075000030822     C                   EVAL      VABGVA=%trim(%subst(
075100030822     C                             %subst(vlrppt:posDaDft+1:
075200030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
075300030822     C                   ENDIF
075400030822     C* TC1
075500030822     C                   IF        %subst(
075600030822     C                             %subst(vlrppt:posDaDft+1:
075700030822     C                             posADft-posDaDft-1):1:3)
075800030822     C                             = 'TC1'
075900030822     C                   EVAL      VABTC1=%trim(%subst(
076000030822     C                             %subst(vlrppt:posDaDft+1:
076100030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
076200030822     C                   ENDIF
076300030822     C* TC2
076400030822     C                   IF        %subst(
076500030822     C                             %subst(vlrppt:posDaDft+1:
076600030822     C                             posADft-posDaDft-1):1:3)
076700030822     C                             = 'TC2'
076800030822     C                   EVAL      VABTC2=%trim(%subst(
076900030822     C                             %subst(vlrppt:posDaDft+1:
077000030822     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
077100030822     C                   ENDIF
077200080516     C* TIC
077300080516     C                   IF        %subst(
077400080516     C                             %subst(vlrppt:posDaDft+1:
077500080516     C                             posADft-posDaDft-1):1:3)
077600080516     C                             = 'TIC'
077700080516     C                   EVAL      VABTIC=%trim(%subst(
077800080516     C                             %subst(vlrppt:posDaDft+1:
077900080516     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078000080516     C                   ENDIF
078100040714     C* VATTRC
078200040714     C                   IF        %subst(
078300040714     C                             %subst(vlrppt:posDaDft+1:
078400040714     C                             posADft-posDaDft-1):1:3)
078500040714     C                             = 'TRC'
078600040714     C                   EVAL      VATTRC=%trim(%subst(
078700040714     C                             %subst(vlrppt:posDaDft+1:
078800040714     C                             posADft-posDaDft-1):4:posADft-posDaDft-1-3))
078900040714     C                   ENDIF
079000030822     C* ...
079100030822     C                   ENDIF
079200030822     C                   ENDDO
079300020204     C*
079400010330     C                   ENDSR
079500010607     C*----------------------------------------------------*
079600071120     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
079700010607     C*----------------------------------------------------*
079800010607     C     IMPVAB        BEGSR
079900010607     C*
080000010607     C                   SETOFF                                       3132
080100030515     C*
080200010607     C                   MOVEL     datcor        VABAAS
080300010607     C                   MOVE      datcor        VABMGS
080400010607     C*
080500030715     C**********
080600030715     C* Effettuo lo split del campo dati d input x il reperimento delle intestazioni colonne cliente
080700030715     C**********
080800030715     C                   IF        wGiro = *zeros
080900040113     C*
081000040510     C* Se presente inserisco qui la forzatura della intesatazione colonne personalizzata
081100040510     C                   IF        %trim(intcol(1))+%trim(intcol(2))+
081200040510     C                             %trim(intcol(3))<>*blanks
081300040113     C                   EVAL      vindta = %trim(intcol(1))+%trim(intcol(2))+
081400040113     C                                      %trim(intcol(3))
081500040510     C                   ENDIF
081600031201     C* Porto eventualmente da minuscolo in maiuscolo i dati da elaborare
081700031201     C     minu:maiu     XLATE     vindta        vindta
081800030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
081900030715     C                   DOW       posDa <= %len(%trim(vindta))
082000030715     C*
082100030715     C* Gestisco il 1° campo
082200030715     C                   IF        i = 1
082300030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
082400030715     C                   EVAL      posDa = 1
082500030715     C* Eseguo lo scan x trovare la fine del primo campo
082600030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
082700030715     C* A questo punto "estraggo" il campo corrente
082800030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
082900030715     C                                                    (posA-posDa))
083000030715     C* X i campi successivi al 1°
083100030715     C                   ELSE
083200030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
083300030715     C                   EVAL      posDa = posA + 1
083400030715     C* Eseguo lo scan x trovare la fine del campo corrente
083500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
083600030715     C* Gestisco l'ultimo campo
083700030715     C                   IF        posA = *zeros
083800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
083900030715     C* A questo punto "estraggo" il campo corrente
084000030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084100030715     C                                                    (posA-posDa))
084200030715     C                   LEAVE
084300030715     C                   ELSE
084400030715     C* A questo punto "estraggo" il campo corrente
084500030715     C                   EVAL      SkSplitFLD(i) = %subst(vindta:posDa:
084600030715     C                                                    (posA-posDa))
084700030715     C                   ENDIF
084800030715     C                   ENDIF
084900030715     C* Incremento il contatore d campo
085000030715     C                   EVAL      i = i +1
085100030715     C                   ENDDO
085200030715     C*
085300030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
085400030715     C* delimitazione testo
085500030715     C                   EVAL      i = 1
085600030715     C                   DOW       i < %elem(SkSplitFLD)
085700030715     C     CharTXT:' '   XLATE     SkSplitFLD(i) SkSplitFLD(i)
085800030715     C                   EVAL      SkSplitFLD(i) = %trim(SkSplitFLD(i))
085900030715     C                   EVAL      i = i + 1
086000030715     C                   ENDDO
086100030715     C                   EVAL      wGiro = 1
086200030715     C                   ELSE
086300030715     C                   EVAL      wGiro = 2
086400031204     C**********
086500031204     C* Normalizzo i dati d input in modo tale che NN inizino e NN finiscano MAI con il carattere
086600031204     C* d separatore campo
086700031204     C**********
086800031204     C                   EVAL      vindta = %trim(vindta)
086900031204     C                   DOW       %subst(vindta:1:1) = CharCSV                 * all'inizio
087000031204     C                   EVAL      vindta = %subst(vindta:2)
087100031204     C                   ENDDO
087200031204     C*
087300031204     C                   Z-ADD     *zeros        lunghInput        4 0
087400031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087500031204     C                   DOW       %subst(%trim(vindta):lunghInput:1) = CharCSV
087600031204     C                   EVAL      vindta = %subst(%trim(vindta):1:lunghInput-1)
087700031204     C                   EVAL      lunghInput = %len(%trim(vindta))
087800031204     C                   ENDDO
087900030715     C**********
088000030715     C* Effettuo lo split del campo dati d input
088100030715     C**********
088200030715     C* Innanzitutto ciclo sulla stringa x splittare "brutalmente" la stringa in campi (siano essi a
088300030715     C                   DOW       posDa <= %len(%trim(vindta))
088400030715     C*
088500030715     C* Gestisco il 1° campo
088600030715     C                   IF        i = 1
088700030715     C* Forzo a 1 la posizione di partenza in quanto trattasi del primo giro (quindi il primo campo)
088800030715     C                   EVAL      posDa = 1
088900030715     C* Eseguo lo scan x trovare la fine del primo campo
089000030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
089100030715     C* A questo punto "estraggo" il campo corrente
089200030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
089300030715     C                                                    (posA-posDa))
089400030715     C* X i campi successivi al 1°
089500030715     C                   ELSE
089600030715     C* Parto a considerare il campo corrente dal precedente carattere d separatore campo in poi
089700030715     C                   EVAL      posDa = posA + 1
089800031204     C* Verifico che nn vi sia il campo nn valorizzato
089900031204     C                   IF        %subst(vindta:posDa:1) = CharCSV
090000031204     C* Se campo nn valorizzato skippo al prossimo
090100031204     C                   EVAL      SkSplitCSV(i) = *blanks
090200031204     C                   EVAL      posA  = posA + 1
090300031204     C                   ELSE
090400030715     C* Eseguo lo scan x trovare la fine del campo corrente
090500030715     C                   EVAL      posA  = %scan(CharCSV:vindta:posDa+1)
090600030715     C* Gestisco l'ultimo campo
090700030715     C                   IF        posA = *zeros
090800030715     C                   EVAL      posA = %len(%trim(vindta)) + 1
090900030715     C* A questo punto "estraggo" il campo corrente
091000030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091100030715     C                                                    (posA-posDa))
091200030715     C                   LEAVE
091300030715     C                   ELSE
091400030715     C* A questo punto "estraggo" il campo corrente
091500030715     C                   EVAL      SkSplitCSV(i) = %subst(vindta:posDa:
091600030715     C                                                    (posA-posDa))
091700030715     C                   ENDIF
091800031204     C                   ENDIF
091900030715     C                   ENDIF
092000030715     C* Incremento il contatore d campo
092100030715     C                   EVAL      i = i +1
092200030715     C                   ENDDO
092300030715     C*
092400030715     C* Dopo aver splittato i vari campi eseguo 1 prima "normalizzazione" x eliminare i caratteri d
092500030715     C* delimitazione testo
092600030715     C                   EVAL      i = 1
092700030715     C                   DOW       i < %elem(SkSplitCSV)
092800030715     C     CharTXT:' '   XLATE     SkSplitCSV(i) SkSplitCSV(i)
092900030715     C                   EVAL      SkSplitCSV(i) = %trim(SkSplitCSV(i))
093000030715     C                   EVAL      i = i + 1
093100030715     C                   ENDDO
093200030715     C*
093300030715     C* A questo punto procedo con le assegnazioni "mirate" ai campi del file tradotto
093400030715     C                   EVAL      i = 1
093500030715     C                   DOW       i < %elem(SkSplitFLD)
093600030715     C*
093700030715     C* Elaboro solo gli elementi valorizzati (con 1 nome campo ed 1 contenuto)
093800030715     C                   IF        SkSplitFLD(i) <> *blanks AND
093900030715     C                             SkSplitCSV(i) <> *blanks
094000030721     C*** ATB
094100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABATB'
094200030721     C                   EVAL      VABATB = SkSplitCSV(i)
094300030721     C                   ENDIF
094400030715     C*** RSD
094500030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRSD'
094600030715     C                   EVAL      VABRSD = SkSplitCSV(i)
094700030715     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
094800030715     C     '@':'A'       XLATE     VABRSD        VABRSD
094900030715     C* ==
095000030715     C                   ENDIF
095100030721     C*** RD2
095200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRD2'
095300030721     C                   EVAL      VABRD2 = SkSplitCSV(i)
095400030721     C                   ENDIF
095500030715     C*** IND
095600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABIND'
095700030715     C                   EVAL      VABIND = SkSplitCSV(i)
095800030715     C                   ENDIF
095900030715     C*** LOD
096000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABLOD'
096100030715     C                   EVAL      VABLOD = SkSplitCSV(i)
096200030715     C                   ENDIF
096300030715     C*** PRD
096400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABPRD'
096500030715     C                   EVAL      VABPRD = SkSplitCSV(i)
096600030715     C                   ENDIF
096700030721     C*** NZD
096800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNZD'
096900030721     C                   EVAL      VABNZD = SkSplitCSV(i)
097000030721     C                   ENDIF
097100030715     C*** RMA
097200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMA'
097300030715     C                   EVAL      VABRMA = SkSplitCSV(i)
097400030715     C                   ENDIF
097500030715     C*** NOT
097600030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNOT'
097700030715     C                   EVAL      VABNOT = SkSplitCSV(i)
097800030715     C                   ENDIF
097900030715     C*** NT2
098000030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABNT2'
098100030715     C                   EVAL      VABNT2 = SkSplitCSV(i)
098200030715     C                   ENDIF
098300030715     C*** VCA
098400030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVCA'
098500030715     C                   EVAL      VABVCA = SkSplitCSV(i)
098600030715     C                   ENDIF
098700030715     C*** VAS
098800030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAS'
098900030715     C                   EVAL      VABVAS = SkSplitCSV(i)
099000030715     C                   ENDIF
099100030715     C*** FFD
099200030715     C                   IF        %trim(SkSplitFLD(i)) = 'VABFFD'
099300030715     C                   EVAL      VABFFD = SkSplitCSV(i)
099400030715     C                   ENDIF
099500030721     C*** GC1
099600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC1'
099700030721     C                   EVAL      VABGC1 = SkSplitCSV(i)
099800100304     C                   IF        %subst(VABGC1:1) = 'P' or
099900100304     C                             %subst(VABGC1:1) = 'M'
100000100304     C                   EVAL      VABGC1 = ' '+%trim(VABGC1)
100100100304     C                   ENDIF
100200030721     C                   ENDIF
100300030721     C*** GC2
100400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGC2'
100500030721     C                   EVAL      VABGC2 = SkSplitCSV(i)
100600100304     C                   IF        %subst(VABGC2:1) = 'P' or
100700100304     C                             %subst(VABGC2:1) = 'M'
100800100304     C                   EVAL      VABGC2 = ' '+%trim(VABGC2)
100900100304     C                   ENDIF
101000030721     C                   ENDIF
101100030721     C*** TSP
101200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTSP'
101300030721     C                   EVAL      VABTSP = SkSplitCSV(i)
101400030721     C                   ENDIF
101500040510     C*** CBO
101600040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABCBO'
101700040510     C                   EVAL      VABCBO = SkSplitCSV(i)
101800040510     C                   ENDIF
101900030721     C*** NAS
102000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNAS'
102100030721     C                   EVAL      VABNAS = SkSplitCSV(i)
102200030721     C                   ENDIF
102300030721     C*** TIC
102400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTIC'
102500030721     C                   EVAL      VABTIC = SkSplitCSV(i)
102600030721     C                   ENDIF
102700030721     C*** GCA
102800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGCA'
102900030721     C                   EVAL      VABGCA = SkSplitCSV(i)
103000030721     C                   ENDIF
103100030721     C*** XCO
103200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABXCO'
103300030721     C                   EVAL      VABXCO = SkSplitCSV(i)
103400030721     C                   ENDIF
103500030721     C*** CTM
103600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTM'
103700030721     C                   EVAL      VABCTM = SkSplitCSV(i)
103800030721     C                   ENDIF
103900030721     C*** TCR
104000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTCR'
104100030721     C                   EVAL      VABTCR = SkSplitCSV(i)
104200030721     C                   ENDIF
104300030721     C*** CTS
104400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTS'
104500030721     C                   EVAL      VABCTS = SkSplitCSV(i)
104600030721     C                   ENDIF
104700030721     C*** FTM
104800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABFTM'
104900030721     C                   EVAL      VABFTM = SkSplitCSV(i)
105000030721     C                   ENDIF
105100030721     C*** VAD
105200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVAD'
105300030721     C                   EVAL      VABVAD = SkSplitCSV(i)
105400030721     C                   ENDIF
105500030721     C*** GMA
105600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGMA'
105700030721     C                   EVAL      VABGMA = SkSplitCSV(i)
105800030721     C                   ENDIF
105900030721     C*** GGA
106000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGGA'
106100030721     C                   EVAL      VABGGA = SkSplitCSV(i)
106200030721     C                   ENDIF
106300030721     C*** GVA
106400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABGVA'
106500030721     C                   EVAL      VABGVA = SkSplitCSV(i)
106600030721     C                   ENDIF
106700030721     C*** TC1
106800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC1'
106900030721     C                   EVAL      VABTC1 = SkSplitCSV(i)
107000030721     C                   ENDIF
107100030721     C*** TC2
107200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABTC2'
107300030721     C                   EVAL      VABTC2 = SkSplitCSV(i)
107400030721     C                   ENDIF
107500030721     C*** SCL
107600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABSCL'
107700030721     C                   EVAL      VABSCL = SkSplitCSV(i)
107800030721     C                   ENDIF
107900030721     C*** RMO
108000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMO'
108100030721     C                   EVAL      VABRMO = SkSplitCSV(i)
108200030721     C                   ENDIF
108300030721     C*** NMO
108400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNMO'
108500030721     C                   EVAL      VABNMO = SkSplitCSV(i)
108600030721     C                   ENDIF
108700030721     C*
108800030721     C* Reperisco quindi i campi numerici...
108900040510     C*** NSP
109000040510     C                   IF        %trim(SkSplitFLD(i)) = 'VABNSP'
109100040510     C                   EVAL      PiStr=SkSplitCSV(i)
109200040510     C                   EXSR      CHKNUM
109300040510     C                   IF        PiInt=*on
109400040510     C                   Z-ADD     PiVal         VABNSP
109500040510     C                   ELSE
109600040510     C                   SETON                                        31
109700040510     C                   EVAL      VABNSP = *zeros
109800040510     C                   EVAL      vinmsg = %trimr(vinmsg)
109900040510     C                             + ' ' + 'VABNSP'
110000040510     C                   ENDIF
110100040510     C                   ENDIF
110200070703     C*** RMN
110300070703     C                   IF        %trim(SkSplitFLD(i)) = 'VABRMN'
110400070703     C                   EVAL      PiStr=SkSplitCSV(i)
110500070703     C                   EXSR      CHKNUM
110600070703     C                   IF        PiInt=*on
110700070703     C                   Z-ADD     PiVal         VABRMN
110800070703     C                   ELSE
110900070703     C                   SETON                                        32
111000070703     C                   EVAL      vinmsg = %trimr(vinmsg)
111100070703     C                             + ' ' + 'VABRMN'
111200070703     C                   ENDIF
111300070703     C                   ENDIF
111400070705     C*** CAD
111500070705     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAD'
111600070705     C                   EVAL      VABCAD = SkSplitCSV(i)
111700070705     C                   ENDIF
111800030721     C*** NCL
111900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCL'
112000030721     C                   EVAL      PiStr=SkSplitCSV(i)
112100030721     C                   EXSR      CHKNUM
112200030721     C                   IF        PiInt=*on
112300030721     C                   Z-ADD     PiVal         VABNCL
112400030721     C                   ELSE
112500030721     C                   SETON                                        32
112600030721     C                   EVAL      VABNCL = *zeros
112700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
112800040122     C                             + ' ' + 'VABNCL'
112900030721     C                   ENDIF
113000030721     C                   ENDIF
113100030721     C*** PKB
113200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABPKB'
113300030721     C                   EVAL      PiStr=SkSplitCSV(i)
113400030721     C                   EXSR      CHKNUM
113500030721     C                   IF        PiNum=*on
113600030721     C                   Z-ADD     PiVal         VABPKB
113700030721     C                   ELSE
113800030721     C                   SETON                                        32
113900030721     C                   EVAL      VABPKB = *zeros
114000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
114100030721     C                             + ' ' + 'VABPKB'
114200030721     C                   ENDIF
114300030721     C                   ENDIF
114400030721     C*** CAS
114500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCAS'
114600030721     C                   EVAL      PiStr=SkSplitCSV(i)
114700030721     C                   EXSR      CHKNUM
114800030721     C                   IF        PiNum=*on
114900030721     C                   Z-ADD     PiVal         VABCAS
115000030721     C                   ELSE
115100030721     C                   SETON                                        32
115200030721     C                   EVAL      VABCAS = *zeros
115300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
115400030721     C                             + ' ' + 'VABCAS'
115500030721     C                   ENDIF
115600030721     C                   ENDIF
115700030721     C*** CCM
115800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCCM'
115900030721     C                   EVAL      PiStr=SkSplitCSV(i)
116000030721     C                   EXSR      CHKNUM
116100030721     C                   IF        PiInt=*on      AND
116200030721     C                             PiVal<=9999999 AND
116300030721     C                             PiVal>*zeros
116400030721     C                   Z-ADD     PiVal         VABCCM
116500030721     C                   ELSE
116600030721     C                   SETON                                        32
116700030721     C                   EVAL      VABCCM = *zeros
116800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
116900040510     C                             + ' ' + 'VABCCM'
117000030721     C                   ENDIF
117100030721     C                   ENDIF
117200030721     C*** LNP
117300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNP'
117400030721     C                   EVAL      PiStr=SkSplitCSV(i)
117500030721     C                   EXSR      CHKNUM
117600030721     C                   IF        PiInt=*on    AND
117700030721     C                             PiVal<=999   AND
117800030721     C                             PiVal>*zeros
117900030721     C                   Z-ADD     PiVal         VABLNP
118000030721     C                   ELSE
118100040715     C                   SETON                                        32
118200030721     C                   EVAL      VABLNP = *zeros
118300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
118400040510     C                             + ' ' + 'VABLNP'
118500030721     C                   ENDIF
118600030721     C                   ENDIF
118700030721     C*** NRS
118800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNRS'
118900030721     C                   EVAL      PiStr=SkSplitCSV(i)
119000030721     C                   EXSR      CHKNUM
119100030721     C                   IF        PiInt=*on AND
119200030721     C                             PiVal<=99
119300030721     C                   Z-ADD     PiVal         VABNRS
119400030721     C                   ELSE
119500030721     C                   SETON                                        32
119600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
119700030721     C                             + ' ' + 'VABNRS'
119800030721     C                   ENDIF
119900030721     C                   ENDIF
120000030721     C*** LNA
120100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABLNA'
120200030721     C                   EVAL      PiStr=SkSplitCSV(i)
120300030721     C                   EXSR      CHKNUM
120400030721     C                   IF        PiInt=*on    AND
120500030721     C                             PiVal<=999   AND
120600030721     C                             PiVal>*zeros
120700030721     C                   Z-ADD     PiVal         VABLNA
120800030721     C                   ELSE
120900030721     C                   SETON                                        32
121000030721     C                   EVAL      VABLNA = *zeros
121100030721     C                   EVAL      vinmsg = %trimr(vinmsg)
121200030721     C                             + ' ' + 'VABLNA'
121300030721     C                   ENDIF
121400030721     C                   ENDIF
121500030721     C*** CTR
121600030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCTR'
121700030721     C                   EVAL      PiStr=SkSplitCSV(i)
121800030721     C                   EXSR      CHKNUM
121900030721     C                   IF        PiInt=*on  AND
122000030721     C                             PiVal<=999
122100030721     C                   Z-ADD     PiVal         VABCTR
122200030721     C                   ELSE
122300030721     C                   SETON                                        32
122400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
122500030721     C                             + ' ' + 'VABCTR'
122600030721     C                   ENDIF
122700030721     C                   ENDIF
122800030721     C*** IAS
122900030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABIAS'
123000030721     C                   EVAL      PiStr=SkSplitCSV(i)
123100030721     C                   EXSR      CHKNUM
123200030721     C                   IF        PiNum=*on
123300030721     C                   Z-ADD     PiVal         VABIAS
123400030721     C                   ELSE
123500030721     C                   SETON                                        32
123600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
123700030721     C                             + ' ' + 'VABIAS'
123800030721     C                   ENDIF
123900030721     C                   ENDIF
124000030721     C*** VLB
124100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVLB'
124200030721     C                   EVAL      PiStr=SkSplitCSV(i)
124300030721     C                   EXSR      CHKNUM
124400030721     C                   IF        PiNum=*on
124500030721     C                   Z-ADD     PiVal         VABVLB
124600030721     C                   ELSE
124700030721     C                   SETON                                        32
124800030721     C                   EVAL      vinmsg = %trimr(vinmsg)
124900030721     C                             + ' ' + 'VABVLB'
125000030721     C                   ENDIF
125100030721     C                   ENDIF
125200030721     C*** QFT
125300030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABQFT'
125400030721     C                   EVAL      PiStr=SkSplitCSV(i)
125500030721     C                   EXSR      CHKNUM
125600030721     C                   IF        PiNum=*on
125700030721     C                   Z-ADD     PiVal         VABQFT
125800030721     C                   ELSE
125900030721     C                   SETON                                        32
126000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
126100030721     C                             + ' ' + 'VABQFT'
126200030721     C                   ENDIF
126300030721     C                   ENDIF
126400030721     C*** NCD
126500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCD'
126600030721     C                   EVAL      PiStr=SkSplitCSV(i)
126700030721     C                   EXSR      CHKNUM
126800030721     C                   IF        PiInt=*on      AND
126900030721     C                             PiVal<=9999999
127000030721     C                   Z-ADD     PiVal         VABNCD
127100030721     C                   ELSE
127200030721     C                   SETON                                        32
127300030721     C                   EVAL      vinmsg = %trimr(vinmsg)
127400030721     C                             + ' ' + 'VABNCD'
127500030721     C                   ENDIF
127600030721     C                   ENDIF
127700030721     C*** NCA
127800030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABNCA'
127900030721     C                   EVAL      PiStr=SkSplitCSV(i)
128000030721     C                   EXSR      CHKNUM
128100030721     C                   IF        PiInt=*on      AND
128200030721     C                             PiVal<=9999999
128300030721     C                   Z-ADD     PiVal         VABNCA
128400030721     C                   ELSE
128500030721     C                   SETON                                        32
128600030721     C                   EVAL      vinmsg = %trimr(vinmsg)
128700030721     C                             + ' ' + 'VABNCA'
128800030721     C                   ENDIF
128900030721     C                   ENDIF
129000030721     C*** ZNC
129100030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABZNC'
129200030721     C                   EVAL      PiStr=SkSplitCSV(i)
129300030721     C                   EXSR      CHKNUM
129400030721     C                   IF        PiInt=*on AND
129500030721     C                             PiVal<=99
129600030721     C                   Z-ADD     PiVal         VABZNC
129700030721     C                   ELSE
129800030721     C                   SETON                                        32
129900030721     C                   EVAL      vinmsg = %trimr(vinmsg)
130000030721     C                             + ' ' + 'VABZNC'
130100030721     C                   ENDIF
130200030721     C                   ENDIF
130300030721     C*** DCR
130400030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABDCR'
130500030721     C                   EVAL      PiStr=SkSplitCSV(i)
130600030721     C                   EXSR      CHKNUM
130700030721     C                   IF        PiInt=*on       AND
130800030721     C                             PiVal<=99999999
130900030721     C                   Z-ADD     PiVal         VABDCR
131000030721     C                   ELSE
131100030721     C                   SETON                                        32
131200030721     C                   EVAL      vinmsg = %trimr(vinmsg)
131300030721     C                             + ' ' + 'VABDCR'
131400030721     C                   ENDIF
131500030721     C                   ENDIF
131600030721     C*** HCR
131700030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABHCR'
131800030721     C                   EVAL      PiStr=SkSplitCSV(i)
131900030721     C                   EXSR      CHKNUM
132000030721     C                   IF        PiInt=*on   AND
132100030721     C                             PiVal<=9999
132200030721     C                   Z-ADD     PiVal         VABHCR
132300030721     C                   ELSE
132400030721     C                   SETON                                        32
132500030721     C                   EVAL      vinmsg = %trimr(vinmsg)
132600030721     C                             + ' ' + 'VABHCR'
132700030721     C                   ENDIF
132800030721     C                   ENDIF
132900030721     C*** VMD
133000030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABVMD'
133100030721     C                   EVAL      PiStr=SkSplitCSV(i)
133200030721     C                   EXSR      CHKNUM
133300030721     C                   IF        PiNum=*on
133400030721     C                   Z-ADD     PiVal         VABVMD
133500030721     C                   ELSE
133600030721     C                   SETON                                        32
133700030721     C                   EVAL      vinmsg = %trimr(vinmsg)
133800030721     C                             + ' ' + 'VABVMD'
133900030721     C                   ENDIF
134000030721     C                   ENDIF
134100030721     C*** ANT
134200030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABANT'
134300030721     C                   EVAL      PiStr=SkSplitCSV(i)
134400030721     C                   EXSR      CHKNUM
134500030721     C                   IF        PiInt=*on        AND
134600030721     C                             PiVal<=999999999
134700030721     C                   Z-ADD     PiVal         VABANT
134800030721     C                   ELSE
134900030721     C                   SETON                                        32
135000030721     C                   EVAL      vinmsg = %trimr(vinmsg)
135100030721     C                             + ' ' + 'VABANT'
135200030721     C                   ENDIF
135300030721     C                   ENDIF
135400030721     C*** CMO
135500030721     C                   IF        %trim(SkSplitFLD(i)) = 'VABCMO'
135600030721     C                   EVAL      PiStr=SkSplitCSV(i)
135700030721     C                   EXSR      CHKNUM
135800030721     C                   IF        PiInt=*on
135900030721     C                   Z-ADD     PiVal         Num5_0
136000030721     C                   MOVEL(p)  Num5_0        VABCMO
136100030721     C                   ELSE
136200030721     C                   SETON                                        32
136300030721     C                   EVAL      VABCMO = *zeros
136400030721     C                   EVAL      vinmsg = %trimr(vinmsg)
136500030721     C                             + ' ' + 'VABCMO'
136600030721     C                   ENDIF
136700030721     C                   ENDIF
136800040510     C*
136900071120     C* Reperisco i campi del EDIVAT
137000040510     C*** VATNOT
137100040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT'
137200040510     C                   EVAL      VATNOT = %trim(SkSplitCSV(i))
137300040510     C                   ENDIF
137400040510     C*** VATTRC
137500040510     C                   IF        %trim(SkSplitFLD(i)) = 'VATTRC'
137600040510     C                   EVAL      VATTRC = %trim(SkSplitCSV(i))
137700040510     C                   ENDIF
137800060331     C*** VATNOT_A
137900060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_A'
138000060331     C                   EVAL      wVATNOT_A = %trim(SkSplitCSV(i))
138100060331     C                   ENDIF
138200060331     C*** VATNOT_B
138300060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_B'
138400060331     C                   EVAL      wVATNOT_B = %trim(SkSplitCSV(i))
138500060331     C                   ENDIF
138600060331     C*** VATNOT_E
138700060331     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
138800060331     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
138900060331     C                   ENDIF
139000110912     C*** VATNOT_I
139100110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_I'
139200110912     C                   EVAL      wVATNOT_I = %trim(SkSplitCSV(i))
139300110912     C                   ENDIF
139400110912     C*** VATNOT_J
139500110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_J'
139600110912     C                   EVAL      wVATNOT_J = %trim(SkSplitCSV(i))
139700110912     C                   ENDIF
139800110912     C*** VATNOT_4
139900110912     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_4'
140000110912     C                   EVAL      wVATNOT_4 = %trim(SkSplitCSV(i))
140100110912     C                   ENDIF
140200131018     C*** VATNOT_S
140300131018     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_S'
140400131018     C                   EVAL      wVATNOT_S = SkSplitCSV(i)
140500131018     C                   ENDIF
140600141128     C*** VATNOT_Y
140700141128     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_Y'
140800141128     C                   EVAL      wVATNOT_Y = %trim(SkSplitCSV(i))
140900141128     C                   ENDIF
141000160513     C*
141100160513     C********* PERSONALIZZAZIONI DENTRO CICLO **********
141200160513     C*
141300160513     C*** VATNOT_E / VABRMA
141400160513     C                   IF        %trim(SkSplitFLD(i)) = 'VATNOT_E'
141500160513     C                   EVAL      wVATNOT_E = %trim(SkSplitCSV(i))
141600160513     C                   EVAL      VABRMA = %subst(wVATNOT_E:3)
141700160513     C                   ENDIF
141800160513     C*
141900160513     C***************************************************
142000040510     C*
142100030715     C                   ENDIF
142200030715     C                   EVAL      i = i + 1
142300030715     C                   ENDDO
142400040510     C*
142500040715     C* Se NSP nn valorizzato considero errore bloccante in traduzione
142600040510     C                   IF        VABNSP = *zeros
142700040510     C                   SETON                                        31
142800040510     C                   ENDIF
142900040715     C*
143000040715     C* Se LNP nn valorizzato considero errore bloccante in traduzione
143100040715     C                   IF        VABLNP = *zeros
143200040715     C                   SETON                                        31
143300040715     C                   ENDIF
143400040510     C*
143500040714     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
143600040510     C                   IF        VABRMN = *zeros
143700040510     C                   Z-ADD     VABNSP        VABRMN
143800040510     C                   ENDIF
143900040510     C*
144000040714     C* Se RMA nn valorizzato lo forzo uguale a VABRMN
144100040510     C                   IF        VABRMA = *blanks
144200040714     C                   MOVEL     VABRMN        VABRMA
144300040510     C                   ENDIF
144400070705     C*
144500070705     C* Considerazioni particolari
144600070705     C*** NZD
144700070705     C                   IF        VABNZD = 'IT' OR VABNZD = 'ITA'
144800070705     C                   EVAL      VABNZD = *blanks
144900070705     C                   ENDIF
145000070705     C*** CAD
145100070705     C                   IF        VABNZD = *blanks
145200070705     C                   EVAL      PiStr=VABCAD
145300070705     C                   EXSR      CHKNUM
145400070705     C                   IF        PiInt=*on
145500070705     C                   Z-ADD     PiVal         Num5_0
145600070705     C                   MOVEL(p)  Num5_0        VABCAD
145700070705     C                   ELSE
145800070705     C                   SETON                                        32
145900070705     C                   EVAL      VABCAD = *zeros
146000070705     C                   EVAL      vinmsg = %trimr(vinmsg)
146100070705     C                             + ' ' + 'VABCAD'
146200070705     C                   ENDIF
146300070705     C                   ENDIF
146400040128     C*
146500040510     C* Se provincia nn valorizzata la reperisco
146600040510     C* tramite TISI95R a seconda dei dati d instradamento presenti
146700040510     C                   IF        VABPRD = *blanks
146800040128     C                   CLEAR                   TISI95DS
146900040128     C                   EVAL      I95TCN = '3'
147000040128     C                   Z-ADD     datcor        I95DAT
147100040128     C                   EVAL      I95NAR = VABNZD
147200040128     C                   EVAL      I95CAP = VABCAD
147300040128     C                   EVAL      I95LOC = VABLOD
147400040128     C                   CALL      'TISI95R'
147500040128     C                   PARM                    TISI95DS
147600040128     C                   EVAL      VABPRD = O95PRV
147700040510     C                   ENDIF
147800040113     C*
147900040113     C* Eseguo considerazioni sul tipo bolla in relaziione al contrassegno
148000040113     C                   IF        VABCAS > *zeros
148100040119     C                   EVAL      VABVCA = 'EUR'
148200040119     C                   IF        VABCBO = '1'
148300040113     C                   EVAL      VABCBO = '4'
148400040119     C                   ENDIF
148500040119     C                   IF        VABCBO = '2'
148600040119     C                   EVAL      VABCBO = '6'
148700040119     C                   ENDIF
148800040113     C                   ENDIF
148900020204     C*
149000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
149100020204     C                   EXSR      CHKIMPDIV
149200030715     C                   ENDIF
149300020204     C*
149400010607     C                   ENDSR
149500010607     C*----------------------------------------------------*
149600010601
149700020204
149800020204     C*----------------------------------------------------*
149900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
150000020204     C*----------------------------------------------------*
150100020204     C     CHKIMPDIV     BEGSR
150200020204     C*
150300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
150400020204     C                   Z-ADD     *zeros        wrkDec            9 9
150500020204     C*
150600020204     C* Come prima cosa effettuo considerazioni sulla divisa
150700020204     C                   IF        vabIAS > *zeros
150800020204     C                   IF        vabVAS <> 'EUR'
150900020204     C                   EVAL      vabVAS =  'ITL'
151000020204     C                   ENDIF
151100020204     C                   ENDIF
151200020204     C*
151300020204     C                   IF        vabCAS > *zeros
151400020204     C                   IF        vabVCA <> 'EUR'
151500020204     C                   EVAL      vabVCA =  'ITL'
151600020204     C                   ENDIF
151700020204     C                   ENDIF
151800020204     C*
151900020204     C                   IF        vabVMD > *zeros
152000020204     C                   IF        vabVAD <> 'EUR'
152100020204     C                   EVAL      vabVAD =  'ITL'
152200020204     C                   ENDIF
152300020204     C                   ENDIF
152400020204     C*
152500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
152600020204     C                   Z-ADD     vabIAS        wrkDec
152700020204     C                   IF        wrkDec > *zeros
152800020204     C                   IF        vabVAS = 'ITL'
152900020204     C                   EVAL      vabIAS = *zeros
153000020204     C                   ENDIF
153100020204     C                   ENDIF
153200020204     C*
153300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
153400020204     C                   Z-ADD     vabCAS        wrkDec
153500020204     C                   IF        wrkDec > *zeros
153600020204     C                   IF        vabVCA = 'ITL'
153700020204     C                   EVAL      vabCAS = *zeros
153800020204     C                   ENDIF
153900020204     C                   ENDIF
154000020204     C*
154100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
154200020204     C                   Z-ADD     vabVMD        wrkDec
154300020204     C                   IF        wrkDec > *zeros
154400020204     C                   IF        vabVAD = 'ITL'
154500020204     C                   EVAL      vabVMD = *zeros
154600020204     C                   ENDIF
154700020204     C                   ENDIF
154800020204     C*
154900020204     C                   ENDSR
155000020204     C***
155100020204
155200010330
155300010330
155400010330     C*----------------------------------------------------*
155500010330     C*  CONTROLLO NUMERICITA' CAMPI
155600010330     C*----------------------------------------------------*
155700010330     C     CHKNUM        BEGSR
155800010330     C*
155900010606     C                   IF        PiDecChr = *blanks
156000030715     C                   EVAL      PiDecChr = CharNUM
156100010606     C                   ENDIF
156200010606     C*
156300010606     C                   CALL(e)   'ISNUMERIC'
156400010330     C                   PARM                    PiStr            30
156500010606     C                   PARM                    PiDecChr          1
156600010330     C                   PARM      *ZEROS        PiVal            30 9
156700010330     C                   PARM      '0'           PiInt             1
156800010330     C                   PARM      '0'           PiNum             1
156900010330     C                   IF        %error
157000010606     C                   EVAL      PiNum=*off
157100010330     C                   ENDIF
157200010330     C*
157300010330     C                   ENDSR
157400010330     C***
157500010330
157600010601
157700010601
157800010601
157900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
158000010601     C     repfil        BEGSR
158100010601     C*
158200010601     C                   if        invfil = *zeros and
158300010601     C                             depfil > *zeros and
158400010629     C                             (vinflg = *blanks or
158500010629     C                              vinflg = *zeros)
158600010601     C
158700010601     C                   eval      invfil = depfil
158800010601     C                   endif
158900010601     C*
159000010601     C                   if        depfil <> invfil and
159100010601     C                             invfil > *zeros
159200010601     C                   eval      flgMulti = '1'
159300010601     C                   if        vinflg = *blanks
159400010601     C                   add       1             cntNonEl
159500010601     C                   endif
159600010601     C                   endif
159700010601     C*
159800010601     C                   if        vinflg = '2'
159900010601     C                   eval      flgStato = '2'
160000010601     C                   endif
160100010601     C*
160200010601     C                   ENDSR
160300010601     C***
160400010601
160500010601
160600010601
160700010330
160800040119      /TITLE Invio dei dati al punto operativo.
160900040119     C     invio         BEGSR
161000040119     C*
161100071120     C* 1° invio EDIVAT
161200040119     C                   reset                   dscmz
161300070704     C                   move      winvcmz       cmzdst
161400071120     C                   eval      cmzfld = 'EDIVATWR'
161500040119     C                   eval      cmzmbd = vlrhdl
161600040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
161700040119     C***                if        prmfir = *blanks
161800071120     C                   eval      cmzfla = 'EDIVAT0F'
161900071120     C                   eval      cmzmba = 'EDIVAT0F'
162000040119     C***                else
162100040119     C***                eval      cmzfla = prmfir
162200040119     C***                eval      cmzmba = prmfir
162300040119     C***                endif
162400040119     C                   eval      cmznrr = *zeros
162500040119     C                   move      §ctrokvt      cmznrr
162600040119     C                   eval      cmzlba = vlrfl1
162700040119     C                   call(e)   'TIS711C'
162800040119     C                   parm                    dscmz
162900040119     C                   parm      *blanks       esito
163000040119     C                   if        %error
163100040119     C                             or cmzerr = '1'
163200040119     C                             or esito  = '1'
163300040119     C                   eval      wrkesito = '3'
163400040119     C                   else
163500040119     C*
163600071120     C* 2° invio EDIVAB
163700040119     C                   reset                   dscmz
163800070704     C                   move      winvcmz       cmzdst
163900040119     C                   eval      cmzfld = vlrfou
164000040119     C                   eval      cmzmbd = vlrhdl
164100040119     C                   eval      %subst(cmzmbd:1:1) = 'M'
164200040119     C***                if        prmfir = *blanks
164300071120     C                   eval      cmzfla = 'EDIVAB0F'
164400071120     C                   eval      cmzmba = 'EDIVAB0F'
164500040119     C***                else
164600040119     C***                eval      cmzfla = prmfir
164700040119     C***                eval      cmzmba = prmfir
164800040119     C***                endif
164900040119     C                   eval      cmznrr = *zeros
165000040119     C                   move      §ctrokvb      cmznrr
165100040119     C                   eval      cmzlba = vlrfl1
165200040119     C                   call(e)   'TIS711C'
165300040119     C                   parm                    dscmz
165400040119     C                   parm      *blanks       esito
165500040119     C                   if        %error
165600040119     C                             or cmzerr = '1'
165700040119     C                             or esito  = '1'
165800040119     C                   eval      wrkesito = '3'
165900040119     C                   endif
166000040119     C                   endif
166100040119     C*
166200040119     C                   ENDSR
166300040119     C***
166400010601
166500010601
166600010601
166700010601
166800010601      /TITLE Invio dei dati al punto operativo.
166900010601     C     opeini        BEGSR
167000010601     C*
167100010601     C* Inizializzo flag e contatori operativi
167200010601     C                   movel     '0'           flgGiro           1
167300010601     C                   movel     '0'           flgMulti          1
167400010601     C                   movel     '1'           flgStato          1
167500010615     C                   movel     '0'           flgOk             1
167600010601     C                   z-add     *zeros        cntNonEl         10 0
167700010601     C                   z-add     *zeros        depfil            3 0
167800010601     C                   z-add     *zeros        invfil            3 0
167900010601     C*
168000010601     C                   ENDSR
168100010601     C***
168200070326
168300070326
168400070326
168500070326
168600070326     C     *pssr         BEGSR
168700070329     C*
168800070329     C                   if        %open(tivin00r)
168900070329     C                   close     tivin00r
169000070329     C                   endif
169100071120     C                   if        %open(edivabwr)
169200071120     C                   close     edivabwr
169300070329     C                   endif
169400071120     C                   if        %open(edivatwr)
169500071120     C                   close     edivatwr
169600070329     C                   endif
169700070326     C*
169800070326     C* Effettuo la chiamata al CLLE preposto
169900071121     C                   call(e)   'TITVEVTC'
170000070326     C                   parm                    parccm
170100070326     C                   parm                    parmbr
170200070326     C                   parm      '2'           paropz
170300070326     C*
170400070326     C                   eval      wrkesito = '2'
170500070404     C*
170600070404     C                   seton                                        LR
170700070326     C*
170800070326     C                   ENDSR     '*CANCL'
170900070326     C***
171000070326
171100070326
171200010330
171300010330
171400000613     C     *inzsr        BEGSR
171500990910     C*
171600990910     C     *entry        plist
171700990920     C                   parm                    tivlrds
171800990921     C                   parm      wrkesito      esito
171900000724     C                   parm                    prmlit
172000000710     C                   parm                    prmfir
172100010330     C*
172200010330     C* CALCOLA LA DATA CORRENTE
172300010330     C                   time                    wn14             14 0
172400010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
172500071120     C                   movel     wn14          oracor            6 0          *ORA
172600010330     C                   z-add     wn8           g08dat
172700010330     C                   z-add     *zeros        g08inv
172800010330     C                   movel     '0'           g08err
172900010330     C                   call      'XSRDA8'
173000010330     C                   parm                    wlbda8
173100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
173200000613     C*
173300000613     C                   ENDSR
173400000613     C***
173500040113** INTCOL - INTESTAZIONE COLONNE PERSONALIZZATA
173600040510
173700040122
173800040113
