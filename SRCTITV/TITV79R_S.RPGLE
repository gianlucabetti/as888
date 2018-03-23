000100031121      /TITLE Upload via Internet: traduzione in EDIVABWR. (x NOTTINTON Cod. 0591254)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500031121     FEDIVABwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400010330     D esito           s              1
001500010330     D prmlit          s             10
001600010330     D prmfir          s             10
001700010330     D wrkesito        s                   like(esito)
001800010330     D rrnum           s              6  0 INZ(*zeros)
001900031121     D wNomeFile       s             30    INZ(*blanks)
002000160107
002100160107     D*-------------------
002200160107     D* COSTANTI
002300160107     D*-------------------
002400160107     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
002500160107     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
002600010330
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500010601     C*
003600010601     C                   seton                                        lr
003700010601
003800010601
003900010601
004000010601
004100010601     C*--------------------------------------------------------
004200010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004300010601     C*--------------------------------------------------------
004400010601     C     PREELA        BEGSR
004500010601     C*
004600010601     C* SE OCCORRE SPEDIRE IN FILIALE
004700010601     C                   if        invfil <> *zeros and
004800010601     C                             flgGiro = '0'
004900010601     C*
005000010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005100010601     C                   eval      flgGiro = '1'
005200010601     C*
005300010601     C                   endif
005400010601     C*
005500010601     C                   ENDSR
005600010601     C***
005700010601
005800010601
005900010601
006000010601
006100010601
006200010601
006300010601
006400010601     C*--------------------------------------------------------
006500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006600010601     C*--------------------------------------------------------
006700010601     C     ENDELA        BEGSR
006800000616     C*
006900010601     C                   ENDSR
007000010601     C***
007100000613
007200010330
007300010601
007400010601
007500010601
007600010330     C*--------------------------------------------------------
007700031121     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
007800010330     C*--------------------------------------------------------
007900010605     C     RWVAB         BEGSR
008000010330     C*
008100010330     C                   if        not %open(tivin00r)
008200010330     C                   open      tivin00r
008300010330     C                   endif
008400031121     C                   if        not %open(edivabwr)
008500031121     C                   open      edivabwr
008600010330     C                   endif
008700010330     C*
008800010604     C                   clear                   §CTROK
008900010604     C                   clear                   §CTRMO
009000010604     C                   clear                   §CTRNO
009100010330     C*
009200010330     C                   DO        *HIVAL
009300010330     C*
009400010330     C                   READ      tivin00r                               70
009500010618     C*
009600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009700010618     C                   if        vinflg = '1'
009800010618     C                   eval      flgOk = '1'
009900010618     C                   endif
010000010618     C*
010100010330     C                   if        vindta > *blanks
010200010330     C                   add       1             rrnum
010300010330     C*
010400010601     C                   if        *in70 = *off and
010500010330     C                             (vinflg = *blanks
010600010330     C                              or vinflg = '0'
010700010330     C                              or vinflg = '2')
010800010330     C*
010900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011000010711     C                   if        vinflg = *blanks or vinflg = '0'
011100010711     C                   clear                   vinmsg
011200010711     C                   endif
011300010601     C*
011400010330     C                   exsr      inzvar
011500010330     C                   exsr      defcam
011600010605     C                   exsr      impvab
011700010601     C*
011800010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011900010605     C                   eval      depfil = VABLNP
012000010601     C                   exsr      repfil
012100010601     C                   if        depfil = invfil
012200021025     C                   if        vlrpoi = 999
012300021025     C                   MOVE(P)   invfil        VABFGS
012400021025     C                   else
012500021025     C                   MOVE(P)   vlrpoi        VABFGS
012600021025     C                   endif
012700010601     C*
012800010601     C                   exsr      PREELA
012900010601     C*
013000010604     C* Ebbene...
013100010604     C*
013200010604     C  N31              ADD       1             §CTROK            7 0
013300010604     C   32              ADD       1             §CTRMO            7 0
013400010604     C   31              ADD       1             §CTRNO            7 0
013500020205     C*
013600020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
013700031121     C  N31              WRITE     EDIVAB00
013800010604     C*
013900010604     C                   if        *in31 = *off and
014000010604     C                             *in32 = *off
014100010604     C                   eval      vinflg = '1'
014200010604     C                   else
014300010604     C                   eval      vinflg = '2'
014400010604     C                   endif
014500010604     C                   endif
014600010601     C*
014700010604     C                   endif
014800010604     C*
014900010330     C                   else
015000010330     C                   eval      vinflg = '1'
015100010330     C                   endif
015200010601     C*
015300010601     C  N70              update    tivin000
015400010330     C*
015500010330     C  N70              ENDdo
015600010601     C
015700010601     C*
015800010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015900010601     C                   if        cntNonEl = *zeros or
016000010601     C                             flgMulti = '0'
016100010330     C* Se non ci sono record con errori ...
016200010601     C                   if        §ctrno = 0 and
016300010604     C                             §ctrmo = 0 and
016400010601     C                             flgStato <> '2'
016500010330     C* ... restituisco esito OK.
016600010330     C                   eval      wrkesito = '0'
016700010330     C                   else
016800010330     C                   if        §ctrok > 0
016900010330     C                   eval      wrkesito = '1'
017000010330     C                   else
017100010615     C                   if        flgOk = '0'
017200010615     C                   eval      wrkesito = '2'
017300010615     C                   else
017400010615     C                   eval      wrkesito = '6'
017500010615     C                   endif
017600010330     C                   endif
017700010330     C                   endif
017800010601     C                   else
017900010601     C                   eval      wrkesito = '9'
018000010601     C                   endif
018100010330     C*
018200010330     C                   if        %open(tivin00r)
018300010330     C                   close     tivin00r
018400010330     C                   endif
018500031121     C                   if        %open(edivabwr)
018600031121     C                   close     edivabwr
018700010330     C                   endif
018800010601     C*
018900010601     C                   if        vlrpoi <> 999
019000010601     C                   eval      invfil = vlrpoi
019100010601     C                   endif
019200010330     C*
019300010330     C                   if        §ctrok > 0
019400010601     C                             and invfil > *zeros
019500010330     C                   exsr      invio
019600010330     C                   endif
019700010601     C*
019800010618     C                   if        flgGiro = '1'
019900010601     C                   exsr      endela
020000010618     C                   endif
020100010330     C*
020200010330     C                   ENDSR
020300010330     C***
020400010330
020500010601
020600010601
020700010601
020800010601
020900010330     C*----------------------------------------------------*
021000010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021100010330     C*----------------------------------------------------*
021200010330     C     INZVAR        BEGSR
021300010330     C*
021400020204     C                   Z-ADD     *zeros        Num5_0            5 0
021500020213     C                   Z-ADD     *zeros        Num15_3          15 3
021600020204     C                   MOVEL     *blanks       WrkNote          70
021700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021800020322     C                   MOVEL     '0'           FlgCAS            1
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200020322 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
022300010330     C*----------------------------------------------------*
022400010330     C     DEFCAM        BEGSR
022500010330     C*
022600020204     C* Inizializzo il buffer del record da scrivere
022700031121     C                   CLEAR                   EDIVAB00
022800020204     C* Imposto i valori di default...
022900020403     C                   EVAL      VABCCM = 0591254
023000020403     C                   EVAL      VABCTM = '7Q'
023100020329     C                   EVAL      VABLNP = 059
023200020322     C                   EVAL      VABCTR = 000
023300020204     C* ... e poi verifico se sono stati passati come parametri
023400020204     C                   IF        vlrppt > *blanks
023500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023600020204     C                   EXSR      CHKNUM
023700020204     C                   IF        PiInt=*on
023800020204     C                   Z-ADD     PiVal         VABCCM
023900020204     C                   ENDIF
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300020204     C                   Z-ADD     PiVal         VABLNP
024400020204     C                   ENDIF
024500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020204     C                   EXSR      CHKNUM
024700020204     C                   IF        PiInt=*on
024800020204     C                   Z-ADD     PiVal         VABCTR
024900020204     C                   ENDIF
025000020204     C                   ENDIF
025100020204     C*
025200010330     C                   ENDSR
025300010607     C*----------------------------------------------------*
025400031121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
025500010607     C*----------------------------------------------------*
025600010607     C     IMPVAB        BEGSR
025700010607     C*
025800010607     C                   SETOFF                                       3132
025900010607     C*
026000010607     C* Reperimento campi ALFA
026100010607     C*
026200010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026300020329     C                   EVAL      VABRSD=%trim(%subst(vindta:186:30))
026400160107     C     minu:maiu     XLATE     VABRSD        VABRSD                         *Minus -> Maiuscolo
026500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026700020117     C* ==
026800020329     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
026900160107     C     minu:maiu     XLATE     VABRD2        VABRD2                         *Minus -> Maiuscolo
027000020329     C                   EVAL      VABIND=%trim(%subst(vindta:246:30))
027100020329     C                   EVAL      VABLOD=%trim(%subst(vindta:323:30))
027200020329     C                   EVAL      VABPRD=%trim(%subst(vindta:353:2))
027300020329     C                   EVAL      VABNZD=%trim(%subst(vindta:355:3))
027400020403     C                   IF        %trim(VABNZD)='IT'
027500020403     C                   EVAL      VABNZD=*BLANKS
027600020403     C                   ENDIF
027700100309     C                   EVAL      VABRMA=%trim(%subst(vindta:358:14))
027800040408     C                   EVAL      VABNOT=%trim(%subst(vindta:901:35))
027900040408     C                   EVAL      VABNT2=%trim(%subst(vindta:901+35:35))
028000020329     C*
028100020329     C* Verifico se nel record nn c'è solo 1 DDT
028200020329     C                   IF        %trim(%subst(vindta:401:5)) > *ZEROS
028300020329     C                   SETON                                        32
028400020329     C                   EVAL      vinmsg = %trimr(vinmsg)
028500020329     C                             + ' ' + '+ DDT SUL RECORD'
028600020329     C                   ENDIF
028700020329     C*
028800020329     C                   MOVEL     DATCOR        VABAAS
028900020329     C                   MOVE      DATCOR        VABMGS
029000010607     C*
029100010607     C* Reperimento campi NUMERICI
029200030326     C                   EVAL      PiStr=%trim(%subst(vindta:178:6)+
029300030326     C                                         %subst(vindta:185:1))
029400020304     C                   EXSR      CHKNUM
029500020304     C                   IF        PiInt=*on
029600020304     C                   Z-ADD     PiVal         VABNSP
029700020304     C                   ELSE
029800020304     C                   SETON                                        32
029900020304     C                   Z-ADD     *zeros        VABNSP
030000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030100030326     C                             + ' ' + 'VABNSP'
030200020304     C                   ENDIF
030300030326     C*
030400030326     C                   EVAL      PiStr=%trim(%subst(vindta:367:5))
030500030326     C                   EXSR      CHKNUM
030600030326     C                   IF        PiInt=*on
030700030326     C                   Z-ADD     PiVal         VABRMN
030800030326     C                   ELSE
030900030326     C                   SETON                                        32
031000030326     C                   Z-ADD     *zeros        VABRMN
031100030326     C                   EVAL      vinmsg = %trimr(vinmsg)
031200030326     C                             + ' ' + 'VABRMN'
031300030326     C                   ENDIF
031400020304     C*
031500020329     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
031600010607     C                   EXSR      CHKNUM
031700010607     C                   IF        PiInt=*on
031800020204     C                   Z-ADD     PiVal         Num5_0
031900020204     C                   MOVEL(p)  Num5_0        VABCAD
032000010607     C                   ELSE
032100010607     C                   SETON                                        32
032200020204     C                   EVAL      VABCAD = *zeros
032300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032400020204     C                             + ' ' + 'VABCAD'
032500010607     C                   ENDIF
032600010607     C*
032700020329     C                   EVAL      PiStr=%trim(%subst(vindta:868:5))
032800010607     C                   EXSR      CHKNUM
032900010607     C                   IF        PiInt=*on
033000010607     C                   Z-ADD     PiVal         VABNCL
033100010607     C                   ELSE
033200010607     C                   SETON                                        32
033300010607     C                   Z-ADD     *zeros        VABNCL
033400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033500010607     C                             + ' ' + 'VABNCL'
033600010607     C                   ENDIF
033700020304     C*
033800020329     C                   EVAL      PiStr=%trim(%subst(vindta:873:9))
033900020204     C                   EVAL      PiDecChr = ','
034000010607     C                   EXSR      CHKNUM
034100010607     C                   IF        PiNum=*on
034200020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
034300020329     C* riferisce ad un campo 9,3
034400020329     C                   EVAL(H)   VABPKB = PiVal/1000
034500010607     C                   ELSE
034600010607     C                   SETON                                        32
034700010607     C                   Z-ADD     *zeros        VABPKB
034800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034900010607     C                             + ' ' + 'VABPKB'
035000010607     C                   ENDIF
035100020304     C*
035200020329     C                   EVAL      PiStr=%trim(%subst(vindta:891:9))
035300020304     C                   EVAL      PiDecChr = ','
035400020304     C                   EXSR      CHKNUM
035500020304     C                   IF        PiNum=*on
035600020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
035700020329     C* riferisce ad un campo 9,3
035800021003     C                   EVAL(H)   VABVLB = PiVal/1000
035900020304     C                   ELSE
036000020304     C                   SETON                                        32
036100020329     C                   Z-ADD     *zeros        VABVLB
036200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036300020322     C                             + ' ' + 'VABVLB'
036400020304     C                   ENDIF
036500090626     C*
036600090626     C* Forzature valide dal 01/07/2009
036700090626     C                   IF        datcor >= 20090701
036800090626     C                   EVAL      VABCTR = 500
036900090626     C                   EVAL      VABQFT = VABVLB
037000090626     C                   ENDIF
037100151123     C*
037200151123     C* Forzature nuove dal 23/11/2015
037300151123     C                   IF        VABPKB <= 3
037400151123     C                   EVAL      VABCTR = 300
037500151123     C                   EVAL      VABQFT = *zeros
037600151123     C                   ENDIF
037700160107     C*
037800160107     C* Forzature nuove dal 07/01/2016
037900160107     C                   IF        %subst(vindta:1241:1) <> 'S'
038000160107     C                   IF        VABPKB <= 3
038100160107     C                   EVAL      VABCTR = 301
038200160107     C                   ELSE
038300160107     C                   EVAL      VABCTR = 510
038400160107     C                   ENDIF
038500160107     C                   ENDIF
038600160107     C*
038700160107     C* Forzatura in base al numeri di colli
038800160107     C                   IF        VABNCL > 10 AND
038900160107     C                             %scan('GEOX':(VABRSD+VABRD2):1) > *zeros
039000160107     C                   EVAL      VABTC1 = 'A'
039100160107     C                   ENDIF
039200151123     C*
039300020322     C* Considerazioni finali su CBO/CAS
039400020322     C                   IF        FlgCAS = '1'
039500020322     C                   EVAL      VABCBO = '4'
039600020322     C                   ELSE
039700020322     C                   EVAL      VABCBO = '1'
039800020322     C                   ENDIF
039900020204     C*
040000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040100020204     C                   EXSR      CHKIMPDIV
040200031121     C*
040300031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
040400031121     C                   MOVE(P)   vlrMSG        wNomeFile
040500031121     C                   EVAL      VABCMR = %subst(wNomeFile:2:7)
040600031121     C                   EVAL      VABDCM = DATCOR
040700031121     C                   EVAL      VABDTS = DATCOR
040800031121     C                   EVAL      VABCNT = 1
040900020204     C*
041000010607     C                   ENDSR
041100010607     C*----------------------------------------------------*
041200010601
041300020204
041400020204     C*----------------------------------------------------*
041500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
041600020204     C*----------------------------------------------------*
041700020204     C     CHKIMPDIV     BEGSR
041800020204     C*
041900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
042000020204     C                   Z-ADD     *zeros        wrkDec            9 9
042100020204     C*
042200020204     C* Come prima cosa effettuo considerazioni sulla divisa
042300020204     C                   IF        vabIAS > *zeros
042400020204     C                   IF        vabVAS <> 'EUR'
042500020204     C                   EVAL      vabVAS =  'ITL'
042600020204     C                   ENDIF
042700020204     C                   ENDIF
042800020204     C*
042900020204     C                   IF        vabCAS > *zeros
043000020204     C                   IF        vabVCA <> 'EUR'
043100020204     C                   EVAL      vabVCA =  'ITL'
043200020204     C                   ENDIF
043300020204     C                   ENDIF
043400020204     C*
043500020204     C                   IF        vabVMD > *zeros
043600020321     C                   IF        vabVAD <> 'EUR'
043700020204     C                   EVAL      vabVAD =  'ITL'
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
044200020204     C                   Z-ADD     vabIAS        wrkDec
044300020204     C                   IF        wrkDec > *zeros
044400020204     C                   IF        vabVAS = 'ITL'
044500020204     C                   EVAL      vabIAS = *zeros
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
045000020204     C                   Z-ADD     vabCAS        wrkDec
045100020204     C                   IF        wrkDec > *zeros
045200020204     C                   IF        vabVCA = 'ITL'
045300020204     C                   EVAL      vabCAS = *zeros
045400020204     C                   ENDIF
045500020204     C                   ENDIF
045600020204     C*
045700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045800020204     C                   Z-ADD     vabVMD        wrkDec
045900020204     C                   IF        wrkDec > *zeros
046000020204     C                   IF        vabVAD = 'ITL'
046100020204     C                   EVAL      vabVMD = *zeros
046200020204     C                   ENDIF
046300020204     C                   ENDIF
046400020204     C*
046500020204     C                   ENDSR
046600020204     C***
046700020204
046800010330
046900010330
047000010330     C*----------------------------------------------------*
047100010330     C*  CONTROLLO NUMERICITA' CAMPI
047200010330     C*----------------------------------------------------*
047300010330     C     CHKNUM        BEGSR
047400010330     C*
047500010606     C                   IF        PiDecChr = *blanks
047600010606     C                   EVAL      PiDecChr = ','
047700010606     C                   ENDIF
047800010606     C*
047900010606     C                   CALL(e)   'ISNUMERIC'
048000010330     C                   PARM                    PiStr            30
048100010606     C                   PARM                    PiDecChr          1
048200010330     C                   PARM      *ZEROS        PiVal            30 9
048300010330     C                   PARM      '0'           PiInt             1
048400010330     C                   PARM      '0'           PiNum             1
048500010330     C                   IF        %error
048600010606     C                   EVAL      PiNum=*off
048700010330     C                   ENDIF
048800010330     C*
048900010330     C                   ENDSR
049000010330     C***
049100010330
049200010601
049300010601
049400010601
049500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
049600010601     C     repfil        BEGSR
049700010601     C*
049800010601     C                   if        invfil = *zeros and
049900010601     C                             depfil > *zeros and
050000010629     C                             (vinflg = *blanks or
050100010629     C                              vinflg = *zeros)
050200010601     C
050300010601     C                   eval      invfil = depfil
050400010601     C                   endif
050500010601     C*
050600010601     C                   if        depfil <> invfil and
050700010601     C                             invfil > *zeros
050800010601     C                   eval      flgMulti = '1'
050900010601     C                   if        vinflg = *blanks
051000010601     C                   add       1             cntNonEl
051100010601     C                   endif
051200010601     C                   endif
051300010601     C*
051400010601     C                   if        vinflg = '2'
051500010601     C                   eval      flgStato = '2'
051600010601     C                   endif
051700010601     C*
051800010601     C                   ENDSR
051900010601     C***
052000010601
052100010601
052200010601
052300010330
052400010330
052500010330
052600990920      /TITLE Invio dei dati al punto operativo.
052700000613     C     invio         BEGSR
052800990920     C*
052900990920     C                   reset                   dscmz
053000010601     C                   move      invfil        cmzdst
053100990920     C                   eval      cmzfld = vlrfou
053200990920     C                   eval      cmzmbd = vlrhdl
053300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
053400000710     C                   if        prmfir = *blanks
053500031121     C                   eval      cmzfla = 'EDIVAB0F'
053600031121     C                   eval      cmzmba = 'EDIVAB0F'
053700000710     C                   else
053800000710     C                   eval      cmzfla = prmfir
053900000710     C                   eval      cmzmba = prmfir
054000000710     C                   endif
054100990920     C                   eval      cmznrr = *zeros
054200990920     C                   move      §ctrok        cmznrr
054300021018     C                   eval      cmzlba = vlrfl1
054400990920     C                   call(e)   'TIS711C'
054500990920     C                   parm                    dscmz
054600990921     C                   parm      *blanks       esito
054700990920     C                   if        %error
054800990920     C                             or cmzerr = '1'
054900990921     C                             or esito  = '1'
055000000710     C                   eval      wrkesito = '3'
055100990920     C                   endif
055200990920     C*
055300000613     C                   ENDSR
055400990910
055500010601
055600010601
055700010601
055800010601
055900010601      /TITLE Invio dei dati al punto operativo.
056000010601     C     opeini        BEGSR
056100010601     C*
056200010601     C* Inizializzo flag e contatori operativi
056300010601     C                   movel     '0'           flgGiro           1
056400010601     C                   movel     '0'           flgMulti          1
056500010601     C                   movel     '1'           flgStato          1
056600010615     C                   movel     '0'           flgOk             1
056700010601     C                   z-add     *zeros        cntNonEl         10 0
056800010601     C                   z-add     *zeros        depfil            3 0
056900010601     C                   z-add     *zeros        invfil            3 0
057000010601     C*
057100010601     C                   ENDSR
057200010601     C***
057300010601
057400010601
057500010601
057600010330
057700010330
057800000613     C     *inzsr        BEGSR
057900990910     C*
058000990910     C     *entry        plist
058100990920     C                   parm                    tivlrds
058200990921     C                   parm      wrkesito      esito
058300000724     C                   parm                    prmlit
058400000710     C                   parm                    prmfir
058500010330     C*
058600010330     C* CALCOLA LA DATA CORRENTE
058700160107     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
058800160107     C                   eval      datcor = %dec(%date() : *ISO)
058900000613     C*
059000000613     C                   ENDSR
059100000613     C***
