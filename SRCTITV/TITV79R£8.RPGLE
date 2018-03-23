000100160107      /TITLE Upload via Internet: traduzione in EDIVAB/VATWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500031121     FEDIVABwr  O    E             DISK    usropn
000600160107     FEDIVATwr  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000160107     D parccm          s              8    INZ(*blanks)
002100160107     D parmbr          s             10    INZ(*blanks)
002200160107     D paropz          s              1    INZ(*blanks)
002300031121     D wNomeFile       s             30    INZ(*blanks)
002400160107     D chkcall         s              1    INZ(*blanks)
002500160107
002600160107     D*-------------------
002700160107     D* COSTANTI
002800160107     D*-------------------
002900160107     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
003000160107     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
003100010330
003200010330
003300010330
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700010601     C*
003800010601     C                   exsr      opeini
003900010605     C                   exsr      rwvab
004000010601     C*
004100010601     C                   seton                                        lr
004200010601
004300010601
004400010601
004500010601
004600010601     C*--------------------------------------------------------
004700010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004800010601     C*--------------------------------------------------------
004900010601     C     PREELA        BEGSR
005000010601     C*
005100010601     C* SE OCCORRE SPEDIRE IN FILIALE
005200010601     C                   if        invfil <> *zeros and
005300010601     C                             flgGiro = '0'
005400010601     C*
005500010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005600010601     C                   eval      flgGiro = '1'
005700010601     C*
005800010601     C                   endif
005900010601     C*
006000010601     C                   ENDSR
006100010601     C***
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601
006800010601
006900010601     C*--------------------------------------------------------
007000010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007100010601     C*--------------------------------------------------------
007200010601     C     ENDELA        BEGSR
007300000616     C*
007400010601     C                   ENDSR
007500010601     C***
007600000613
007700010330
007800010601
007900010601
008000010601
008100010330     C*--------------------------------------------------------
008200160107     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR/VATWR        *
008300010330     C*--------------------------------------------------------
008400010605     C     RWVAB         BEGSR
008500010330     C*
008600010330     C                   if        not %open(tivin00r)
008700010330     C                   open      tivin00r
008800010330     C                   endif
008900031121     C                   if        not %open(edivabwr)
009000031121     C                   open      edivabwr
009100010330     C                   endif
009200010330     C*
009300160107     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
009400160107     C                   exsr      prevat
009500160107     C*
009600160107     C                   if        chkcall = '0'
009700160107     C*
009800160107     C                   if        not %open(EDIVATwr)
009900160107     C                   open      EDIVATwr
010000160107     C                   endif
010100160107     C*
010200010604     C                   clear                   §CTROK
010300010604     C                   clear                   §CTRMO
010400010604     C                   clear                   §CTRNO
010500160107     C                   clear                   §CTROKVT          5 0
010600010330     C*
010700010330     C                   DO        *HIVAL
010800010330     C*
010900010330     C                   READ      tivin00r                               70
011000010618     C*
011100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
011200010618     C                   if        vinflg = '1'
011300010618     C                   eval      flgOk = '1'
011400010618     C                   endif
011500010618     C*
011600010330     C                   if        vindta > *blanks
011700010330     C                   add       1             rrnum
011800010330     C*
011900010601     C                   if        *in70 = *off and
012000010330     C                             (vinflg = *blanks
012100010330     C                              or vinflg = '0'
012200010330     C                              or vinflg = '2')
012300010330     C*
012400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012500010711     C                   if        vinflg = *blanks or vinflg = '0'
012600010711     C                   clear                   vinmsg
012700010711     C                   endif
012800010601     C*
012900010330     C                   exsr      inzvar
013000010330     C                   exsr      defcam
013100010605     C                   exsr      impvab
013200010601     C*
013300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013400010605     C                   eval      depfil = VABLNP
013500010601     C                   exsr      repfil
013600010601     C                   if        depfil = invfil
013700021025     C                   if        vlrpoi = 999
013800021025     C                   MOVE(P)   invfil        VABFGS
013900021025     C                   else
014000021025     C                   MOVE(P)   vlrpoi        VABFGS
014100021025     C                   endif
014200010601     C*
014300010601     C                   exsr      PREELA
014400010601     C*
014500010604     C* Ebbene...
014600010604     C*
014700010604     C  N31              ADD       1             §CTROK            7 0
014800010604     C   32              ADD       1             §CTRMO            7 0
014900010604     C   31              ADD       1             §CTRNO            7 0
015000160108     C*
015100160108     C* Scrivo anche estensione dettaglio bolle - tipo record "B" => TELEFONO DESTINATARIO
015200160108     C  N31              exsr      exevatb
015300160108     C*
015400160108     C* Indirizzo e-mail
015500160108     C  N31              exsr      exevati
015600020205     C*
015700020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
015800031121     C  N31              WRITE     EDIVAB00
015900010604     C*
016000010604     C                   if        *in31 = *off and
016100010604     C                             *in32 = *off
016200010604     C                   eval      vinflg = '1'
016300010604     C                   else
016400010604     C                   eval      vinflg = '2'
016500010604     C                   endif
016600010604     C                   endif
016700010601     C*
016800010604     C                   endif
016900010604     C*
017000010330     C                   else
017100010330     C                   eval      vinflg = '1'
017200010330     C                   endif
017300010601     C*
017400010601     C  N70              update    tivin000
017500010330     C*
017600010330     C  N70              ENDdo
017700160107     C*
017800160107     C                   endif
017900010601     C
018000010601     C*
018100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
018200010601     C                   if        cntNonEl = *zeros or
018300010601     C                             flgMulti = '0'
018400010330     C* Se non ci sono record con errori ...
018500010601     C                   if        §ctrno = 0 and
018600010604     C                             §ctrmo = 0 and
018700010601     C                             flgStato <> '2'
018800010330     C* ... restituisco esito OK.
018900010330     C                   eval      wrkesito = '0'
019000010330     C                   else
019100010330     C                   if        §ctrok > 0
019200010330     C                   eval      wrkesito = '1'
019300010330     C                   else
019400010615     C                   if        flgOk = '0'
019500010615     C                   eval      wrkesito = '2'
019600010615     C                   else
019700010615     C                   eval      wrkesito = '6'
019800010615     C                   endif
019900010330     C                   endif
020000010330     C                   endif
020100010601     C                   else
020200010601     C                   eval      wrkesito = '9'
020300010601     C                   endif
020400010330     C*
020500010330     C                   if        %open(tivin00r)
020600010330     C                   close     tivin00r
020700010330     C                   endif
020800031121     C                   if        %open(edivabwr)
020900031121     C                   close     edivabwr
021000010330     C                   endif
021100160107     C                   if        %open(EDIVATwr)
021200160107     C                   close     EDIVATwr
021300160107     C                   endif
021400010601     C*
021500010601     C                   if        vlrpoi <> 999
021600010601     C                   eval      invfil = vlrpoi
021700010601     C                   endif
021800010330     C*
021900010330     C                   if        §ctrok > 0
022000010601     C                             and invfil > *zeros
022100010330     C                   exsr      invio
022200010330     C                   endif
022300010601     C*
022400010618     C                   if        flgGiro = '1'
022500010601     C                   exsr      endela
022600010618     C                   endif
022700010330     C*
022800010330     C                   ENDSR
022900010330     C***
023000010330
023100010601
023200010601
023300010601
023400010601
023500010330     C*----------------------------------------------------*
023600010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023700010330     C*----------------------------------------------------*
023800010330     C     INZVAR        BEGSR
023900010330     C*
024000020204     C                   Z-ADD     *zeros        Num5_0            5 0
024100020213     C                   Z-ADD     *zeros        Num15_3          15 3
024200020204     C                   MOVEL     *blanks       WrkNote          70
024300020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024400020322     C                   MOVEL     '0'           FlgCAS            1
024500010330     C*
024600010330     C                   ENDSR
024700010330     C*----------------------------------------------------*
024800020322 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
024900010330     C*----------------------------------------------------*
025000010330     C     DEFCAM        BEGSR
025100010330     C*
025200020204     C* Inizializzo il buffer del record da scrivere
025300031121     C                   CLEAR                   EDIVAB00
025400160107     C                   clear                   edivat00
025500020204     C* Imposto i valori di default...
025600020403     C                   EVAL      VABCCM = 0591254
025700020403     C                   EVAL      VABCTM = '7Q'
025800020329     C                   EVAL      VABLNP = 059
025900020322     C                   EVAL      VABCTR = 000
026000160107     C                   EVAL      VATCCM = 0591254
026100160107     C                   EVAL      VATLNP = 059
026200020204     C* ... e poi verifico se sono stati passati come parametri
026300020204     C                   IF        vlrppt > *blanks
026400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026500020204     C                   EXSR      CHKNUM
026600020204     C                   IF        PiInt=*on
026700020204     C                   Z-ADD     PiVal         VABCCM
026800020204     C                   ENDIF
026900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027000020204     C                   EXSR      CHKNUM
027100020204     C                   IF        PiInt=*on
027200020204     C                   Z-ADD     PiVal         VABLNP
027300020204     C                   ENDIF
027400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027500020204     C                   EXSR      CHKNUM
027600020204     C                   IF        PiInt=*on
027700020204     C                   Z-ADD     PiVal         VABCTR
027800020204     C                   ENDIF
027900020204     C                   ENDIF
028000020204     C*
028100010330     C                   ENDSR
028200010607     C*----------------------------------------------------*
028300031121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
028400010607     C*----------------------------------------------------*
028500010607     C     IMPVAB        BEGSR
028600010607     C*
028700010607     C                   SETOFF                                       3132
028800010607     C*
028900010607     C* Reperimento campi ALFA
029000010607     C*
029100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
029200020329     C                   EVAL      VABRSD=%trim(%subst(vindta:186:30))
029300160107     C     minu:maiu     XLATE     VABRSD        VABRSD                         *Minus -> Maiuscolo
029400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029600020117     C* ==
029700020329     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
029800160107     C     minu:maiu     XLATE     VABRD2        VABRD2                         *Minus -> Maiuscolo
029900020329     C                   EVAL      VABIND=%trim(%subst(vindta:246:30))
030000020329     C                   EVAL      VABLOD=%trim(%subst(vindta:323:30))
030100020329     C                   EVAL      VABPRD=%trim(%subst(vindta:353:2))
030200020329     C                   EVAL      VABNZD=%trim(%subst(vindta:355:3))
030300020403     C                   IF        %trim(VABNZD)='IT'
030400020403     C                   EVAL      VABNZD=*BLANKS
030500020403     C                   ENDIF
030600100309     C                   EVAL      VABRMA=%trim(%subst(vindta:358:14))
030700040408     C                   EVAL      VABNOT=%trim(%subst(vindta:901:35))
030800040408     C                   EVAL      VABNT2=%trim(%subst(vindta:901+35:35))
030900020329     C*
031000020329     C* Verifico se nel record nn c'è solo 1 DDT
031100020329     C                   IF        %trim(%subst(vindta:401:5)) > *ZEROS
031200020329     C                   SETON                                        32
031300020329     C                   EVAL      vinmsg = %trimr(vinmsg)
031400020329     C                             + ' ' + '+ DDT SUL RECORD'
031500020329     C                   ENDIF
031600020329     C*
031700020329     C                   MOVEL     DATCOR        VABAAS
031800020329     C                   MOVE      DATCOR        VABMGS
031900010607     C*
032000010607     C* Reperimento campi NUMERICI
032100160127     C                   IF        %subst(vindta:1213:15) <> *blank
032200160107     C                   EVAL      PiStr=
032300160127     C                              %subst(%trim(%subst(vindta:1213:15)):7:4) +
032400160127     C                              %subst(%trim(%subst(vindta:1213:15)):4:2) +
032500160127     C                              %subst(%trim(%subst(vindta:1213:15)):1:2)
032600160107     C                   EXSR      CHKNUM
032700160107     C                   IF        PiInt=*on
032800160107     C                   Z-ADD     PiVal         VABDCR
032900160107     C                   ELSE
033000160107     C                   SETON                                        32
033100160107     C                   Z-ADD     *zeros        VABDCR
033200160107     C                   EVAL      vinmsg = %trimr(vinmsg)
033300160107     C                             + ' ' + 'VABDCR'
033400160107     C                   ENDIF
033500160107     C                   ENDIF
033600160107     C*
033700160408     C***                EVAL      PiStr=%subst(vindta:178:6)+
033800160408     C***                                %subst(vindta:185:1)
033900160408     C***                if        %subst(PiStr:7:1) = *blank
034000160408     C***                eval      %subst(PiStr:7:1) = '0'
034100160408     C***                endif
034200160408     C                   EVAL      PiStr=%subst(vindta:1228:7)
034300020304     C                   EXSR      CHKNUM
034400020304     C                   IF        PiInt=*on
034500020304     C                   Z-ADD     PiVal         VABNSP
034600020304     C                   ELSE
034700020304     C                   SETON                                        32
034800020304     C                   Z-ADD     *zeros        VABNSP
034900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
035000030326     C                             + ' ' + 'VABNSP'
035100020304     C                   ENDIF
035200030326     C*
035300030326     C                   EVAL      PiStr=%trim(%subst(vindta:367:5))
035400030326     C                   EXSR      CHKNUM
035500030326     C                   IF        PiInt=*on
035600030326     C                   Z-ADD     PiVal         VABRMN
035700030326     C                   ELSE
035800030326     C                   SETON                                        32
035900030326     C                   Z-ADD     *zeros        VABRMN
036000030326     C                   EVAL      vinmsg = %trimr(vinmsg)
036100030326     C                             + ' ' + 'VABRMN'
036200030326     C                   ENDIF
036300020304     C*
036400020329     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
036500010607     C                   EXSR      CHKNUM
036600010607     C                   IF        PiInt=*on
036700020204     C                   Z-ADD     PiVal         Num5_0
036800020204     C                   MOVEL(p)  Num5_0        VABCAD
036900010607     C                   ELSE
037000010607     C                   SETON                                        32
037100020204     C                   EVAL      VABCAD = *zeros
037200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037300020204     C                             + ' ' + 'VABCAD'
037400010607     C                   ENDIF
037500010607     C*
037600020329     C                   EVAL      PiStr=%trim(%subst(vindta:868:5))
037700010607     C                   EXSR      CHKNUM
037800010607     C                   IF        PiInt=*on
037900010607     C                   Z-ADD     PiVal         VABNCL
038000010607     C                   ELSE
038100010607     C                   SETON                                        32
038200010607     C                   Z-ADD     *zeros        VABNCL
038300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
038400010607     C                             + ' ' + 'VABNCL'
038500010607     C                   ENDIF
038600020304     C*
038700020329     C                   EVAL      PiStr=%trim(%subst(vindta:873:9))
038800020204     C                   EVAL      PiDecChr = ','
038900010607     C                   EXSR      CHKNUM
039000010607     C                   IF        PiNum=*on
039100020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
039200020329     C* riferisce ad un campo 9,3
039300020329     C                   EVAL(H)   VABPKB = PiVal/1000
039400010607     C                   ELSE
039500010607     C                   SETON                                        32
039600010607     C                   Z-ADD     *zeros        VABPKB
039700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
039800010607     C                             + ' ' + 'VABPKB'
039900010607     C                   ENDIF
040000020304     C*
040100020329     C                   EVAL      PiStr=%trim(%subst(vindta:891:9))
040200020304     C                   EVAL      PiDecChr = ','
040300020304     C                   EXSR      CHKNUM
040400020304     C                   IF        PiNum=*on
040500020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
040600020329     C* riferisce ad un campo 9,3
040700021003     C                   EVAL(H)   VABVLB = PiVal/1000
040800020304     C                   ELSE
040900020304     C                   SETON                                        32
041000020329     C                   Z-ADD     *zeros        VABVLB
041100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
041200020322     C                             + ' ' + 'VABVLB'
041300020304     C                   ENDIF
041400090626     C*
041500090626     C* Forzature valide dal 01/07/2009
041600090626     C                   IF        datcor >= 20090701
041700090626     C                   EVAL      VABCTR = 500
041800090626     C                   EVAL      VABQFT = VABVLB
041900090626     C                   ENDIF
042000151123     C*
042100151123     C* Forzature nuove dal 23/11/2015
042200151123     C                   IF        VABPKB <= 3
042300151123     C                   EVAL      VABCTR = 300
042400151123     C                   EVAL      VABQFT = *zeros
042500151123     C                   ENDIF
042600160107     C*
042700160107     C* Forzature nuove dal 07/01/2016
042800160127     C                   IF        %subst(vindta:1211:1) <> 'S'
042900160107     C                   IF        VABPKB <= 3
043000160107     C                   EVAL      VABCTR = 301
043100160107     C                   ELSE
043200160107     C                   EVAL      VABCTR = 510
043300160107     C                   ENDIF
043400160107     C                   ENDIF
043500160107     C*
043600160107     C* Forzatura in base al numeri di colli
043700160216     C                   IF        VABNCL > 3 AND
043800160107     C                             %scan('GEOX':(VABRSD+VABRD2):1) > *zeros
043900160107     C                   EVAL      VABTC1 = 'A'
044000160107     C                   ENDIF
044100160216     C*
044200160216     C* Forzatura in base al numeri di colli
044300160216     C                   IF        %scan('GEOX':(VABRSD+VABRD2):1) > *zeros
044400160216     C                   EVAL      VABGC1 = ' P'
044500160216     C                   ENDIF
044600151123     C*
044700020322     C* Considerazioni finali su CBO/CAS
044800020322     C                   IF        FlgCAS = '1'
044900020322     C                   EVAL      VABCBO = '4'
045000020322     C                   ELSE
045100020322     C                   EVAL      VABCBO = '1'
045200020322     C                   ENDIF
045300020204     C*
045400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
045500020204     C                   EXSR      CHKIMPDIV
045600031121     C*
045700031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
045800031121     C                   MOVE(P)   vlrMSG        wNomeFile
045900031121     C                   EVAL      VABCMR = %subst(wNomeFile:2:7)
046000031121     C                   EVAL      VABDCM = DATCOR
046100031121     C                   EVAL      VABDTS = DATCOR
046200031121     C                   EVAL      VABCNT = 1
046300020204     C*
046400010607     C                   ENDSR
046500010607     C*----------------------------------------------------*
046600010601
046700020204
046800020204     C*----------------------------------------------------*
046900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047000020204     C*----------------------------------------------------*
047100020204     C     CHKIMPDIV     BEGSR
047200020204     C*
047300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
047400020204     C                   Z-ADD     *zeros        wrkDec            9 9
047500020204     C*
047600020204     C* Come prima cosa effettuo considerazioni sulla divisa
047700020204     C                   IF        vabIAS > *zeros
047800020204     C                   IF        vabVAS <> 'EUR'
047900020204     C                   EVAL      vabVAS =  'ITL'
048000020204     C                   ENDIF
048100020204     C                   ENDIF
048200020204     C*
048300020204     C                   IF        vabCAS > *zeros
048400020204     C                   IF        vabVCA <> 'EUR'
048500020204     C                   EVAL      vabVCA =  'ITL'
048600020204     C                   ENDIF
048700020204     C                   ENDIF
048800020204     C*
048900020204     C                   IF        vabVMD > *zeros
049000020321     C                   IF        vabVAD <> 'EUR'
049100020204     C                   EVAL      vabVAD =  'ITL'
049200020204     C                   ENDIF
049300020204     C                   ENDIF
049400020204     C*
049500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
049600020204     C                   Z-ADD     vabIAS        wrkDec
049700020204     C                   IF        wrkDec > *zeros
049800020204     C                   IF        vabVAS = 'ITL'
049900020204     C                   EVAL      vabIAS = *zeros
050000020204     C                   ENDIF
050100020204     C                   ENDIF
050200020204     C*
050300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
050400020204     C                   Z-ADD     vabCAS        wrkDec
050500020204     C                   IF        wrkDec > *zeros
050600020204     C                   IF        vabVCA = 'ITL'
050700020204     C                   EVAL      vabCAS = *zeros
050800020204     C                   ENDIF
050900020204     C                   ENDIF
051000020204     C*
051100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
051200020204     C                   Z-ADD     vabVMD        wrkDec
051300020204     C                   IF        wrkDec > *zeros
051400020204     C                   IF        vabVAD = 'ITL'
051500020204     C                   EVAL      vabVMD = *zeros
051600020204     C                   ENDIF
051700020204     C                   ENDIF
051800020204     C*
051900020204     C                   ENDSR
052000020204     C***
052100020204
052200160107     C*----------------------------------------------------*
052300160107     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "B"
052400160107     C*----------------------------------------------------*
052500160107     C     EXEVATB       BEGSR
052600160107     C*
052700160107     C                   EVAL      VATTRC='B'
052800160127     C                   EVAL      VATNOT = %trim(%subst(vindta:1151:60))
052900160107     C                   exsr      wrivat                                       => scarico VAT
053000160107     C*
053100160107     C                   ENDSR
053200160107     C*----------------------------------------------------*
053300160107     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
053400160107     C*----------------------------------------------------*
053500160107     C     EXEVATI       BEGSR
053600160107     C*
053700160107     C                   EVAL      VATTRC='I'
053800160127     C                   EVAL      VATNOT = %trim(%subst(vindta:1101:35))
053900160107     C                   exsr      wrivat                                       => scarico VAT
054000160107     C*
054100160107     C                   EVAL      VATTRC='J'
054200160127     C                   EVAL      VATNOT = %trim(%subst(vindta:1101+35:15))
054300160107     C                   exsr      wrivat                                       => scarico VAT
054400160107     C*
054500160107     C                   ENDSR
054600160107     C*----------------------------------------------------*
054700160107     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
054800160107     C*----------------------------------------------------*
054900160107     C     WRIVAT        BEGSR
055000160107     C*
055100160108     C                   EVAL      VATFGS=VABFGS
055200160108     C                   EVAL      VATCCM=VABCCM
055300160108     C                   EVAL      VATLNP=VABLNP
055400160108     C                   EVAL      VATAAS=VABAAS
055500160108     C                   EVAL      VATNRS=VABNRS
055600160108     C                   EVAL      VATNSP=VABNSP
055700160107     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
055800160107     C                   EVAL      VATCMR = %subst(wNomeFile:2:7)
055900160107     C                   EVAL      VATCNT = 1
056000160107     C*
056100160107     C* Scrivo solo se valorizzato qualcosa
056200160107     C                   IF        VATNOT <> *blanks
056300160107     C                   WRITE     EDIVAT00
056400160107     C                   ENDIF
056500160107     C*
056600160107     C                   ENDSR
056700010330
056800160107     C*----------------------------------------------------*
056900160107     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
057000160107     C*----------------------------------------------------*
057100160107     C     PREVAT        BEGSR
057200160107     C*
057300160107     C* Compongo il nome del membro da dare al EDIVATWR
057400160107     C                   eval      parmbr = vlrhdl
057500160107     C                   movel     'M'           parmbr
057600160107     C                   eval      parccm = %subst(vlrKSC:2:7)
057700160107     C                   eval      paropz = '1'
057800160107     C* Effettuo la chiamata al CLLE preposto
057900160107     C                   call(e)   'TITVEVTC'
058000160107     C                   parm                    parccm
058100160107     C                   parm                    parmbr
058200160107     C                   parm                    paropz
058300160107     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058400160107     C                   if        %error
058500160107     C                   movel     '1'           chkcall
058600160107     C                   else
058700160107     C                   movel     '0'           chkcall
058800160107     C                   endif
058900160107     C*
059000160107     C                   ENDSR
059100010330
059200010330     C*----------------------------------------------------*
059300010330     C*  CONTROLLO NUMERICITA' CAMPI
059400010330     C*----------------------------------------------------*
059500010330     C     CHKNUM        BEGSR
059600010330     C*
059700010606     C                   IF        PiDecChr = *blanks
059800010606     C                   EVAL      PiDecChr = ','
059900010606     C                   ENDIF
060000010606     C*
060100010606     C                   CALL(e)   'ISNUMERIC'
060200010330     C                   PARM                    PiStr            30
060300010606     C                   PARM                    PiDecChr          1
060400010330     C                   PARM      *ZEROS        PiVal            30 9
060500010330     C                   PARM      '0'           PiInt             1
060600010330     C                   PARM      '0'           PiNum             1
060700010330     C                   IF        %error
060800010606     C                   EVAL      PiNum=*off
060900010330     C                   ENDIF
061000010330     C*
061100010330     C                   ENDSR
061200010330     C***
061300010330
061400010601
061500010601
061600010601
061700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
061800010601     C     repfil        BEGSR
061900010601     C*
062000010601     C                   if        invfil = *zeros and
062100010601     C                             depfil > *zeros and
062200010629     C                             (vinflg = *blanks or
062300010629     C                              vinflg = *zeros)
062400010601     C
062500010601     C                   eval      invfil = depfil
062600010601     C                   endif
062700010601     C*
062800010601     C                   if        depfil <> invfil and
062900010601     C                             invfil > *zeros
063000010601     C                   eval      flgMulti = '1'
063100010601     C                   if        vinflg = *blanks
063200010601     C                   add       1             cntNonEl
063300010601     C                   endif
063400010601     C                   endif
063500010601     C*
063600010601     C                   if        vinflg = '2'
063700010601     C                   eval      flgStato = '2'
063800010601     C                   endif
063900010601     C*
064000010601     C                   ENDSR
064100010601     C***
064200010601
064300010601
064400010601
064500010330
064600010330
064700010330
064800990920      /TITLE Invio dei dati al punto operativo.
064900000613     C     invio         BEGSR
065000990920     C*
065100160107     C* 1° invio EDIVAT
065200160107     C                   reset                   dscmz
065300160107     C                   move      vlrpoi        cmzdst
065400160107     C                   eval      cmzfld = 'EDIVATWR'
065500160107     C                   eval      cmzmbd = vlrhdl
065600160107     C                   eval      %subst(cmzmbd:1:1) = 'M'
065700160107     C***                if        prmfir = *blanks
065800160107     C                   eval      cmzfla = 'EDIVAT0F'
065900160107     C                   eval      cmzmba = 'EDIVAT0F'
066000160107     C***                else
066100160107     C***                eval      cmzfla = prmfir
066200160107     C***                eval      cmzmba = prmfir
066300160107     C***                endif
066400160107     C                   eval      cmznrr = *zeros
066500160107     C                   move      §ctrokvt      cmznrr
066600160107     C                   eval      cmzlba = vlrfl1
066700160107     C                   call(e)   'TIS711C'
066800160107     C                   parm                    dscmz
066900160107     C                   parm      *blanks       esito
067000160107     C                   if        %error
067100160107     C                             or cmzerr = '1'
067200160107     C                             or esito  = '1'
067300160107     C                   eval      wrkesito = '3'
067400160107     C                   else
067500160107     C*
067600160107     C* 2° invio EDIVAB
067700990920     C                   reset                   dscmz
067800010601     C                   move      invfil        cmzdst
067900990920     C                   eval      cmzfld = vlrfou
068000990920     C                   eval      cmzmbd = vlrhdl
068100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
068200000710     C                   if        prmfir = *blanks
068300031121     C                   eval      cmzfla = 'EDIVAB0F'
068400031121     C                   eval      cmzmba = 'EDIVAB0F'
068500000710     C                   else
068600000710     C                   eval      cmzfla = prmfir
068700000710     C                   eval      cmzmba = prmfir
068800000710     C                   endif
068900990920     C                   eval      cmznrr = *zeros
069000990920     C                   move      §ctrok        cmznrr
069100021018     C                   eval      cmzlba = vlrfl1
069200990920     C                   call(e)   'TIS711C'
069300990920     C                   parm                    dscmz
069400990921     C                   parm      *blanks       esito
069500990920     C                   if        %error
069600990920     C                             or cmzerr = '1'
069700990921     C                             or esito  = '1'
069800000710     C                   eval      wrkesito = '3'
069900990920     C                   endif
070000160107     C                   endif
070100990920     C*
070200000613     C                   ENDSR
070300990910
070400010601
070500010601
070600010601
070700010601
070800010601      /TITLE Invio dei dati al punto operativo.
070900010601     C     opeini        BEGSR
071000010601     C*
071100010601     C* Inizializzo flag e contatori operativi
071200010601     C                   movel     '0'           flgGiro           1
071300010601     C                   movel     '0'           flgMulti          1
071400010601     C                   movel     '1'           flgStato          1
071500010615     C                   movel     '0'           flgOk             1
071600010601     C                   z-add     *zeros        cntNonEl         10 0
071700010601     C                   z-add     *zeros        depfil            3 0
071800010601     C                   z-add     *zeros        invfil            3 0
071900010601     C*
072000010601     C                   ENDSR
072100010601     C***
072200010601
072300010601
072400010601
072500010330
072600010330
072700000613     C     *inzsr        BEGSR
072800990910     C*
072900990910     C     *entry        plist
073000990920     C                   parm                    tivlrds
073100990921     C                   parm      wrkesito      esito
073200000724     C                   parm                    prmlit
073300000710     C                   parm                    prmfir
073400010330     C*
073500010330     C* CALCOLA LA DATA CORRENTE
073600160107     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
073700160107     C                   eval      datcor = %dec(%date() : *ISO)
073800000613     C*
073900000613     C                   ENDSR
074000000613     C***
