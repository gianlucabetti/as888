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
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800000313     D*
002900010330
003000010330
003100010330
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010605     C                   exsr      rwvab
003800010601     C*
003900010601     C                   seton                                        lr
004000010601
004100010601
004200010601
004300010601
004400010601     C*--------------------------------------------------------
004500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004600010601     C*--------------------------------------------------------
004700010601     C     PREELA        BEGSR
004800010601     C*
004900010601     C* SE OCCORRE SPEDIRE IN FILIALE
005000010601     C                   if        invfil <> *zeros and
005100010601     C                             flgGiro = '0'
005200010601     C*
005300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005400010601     C                   eval      flgGiro = '1'
005500010601     C*
005600010601     C                   endif
005700010601     C*
005800010601     C                   ENDSR
005900010601     C***
006000010601
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006900010601     C*--------------------------------------------------------
007000010601     C     ENDELA        BEGSR
007100000616     C*
007200010601     C                   ENDSR
007300010601     C***
007400000613
007500010330
007600010601
007700010601
007800010601
007900010330     C*--------------------------------------------------------
008000031121     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWF              *
008100010330     C*--------------------------------------------------------
008200010605     C     RWVAB         BEGSR
008300010330     C*
008400010330     C                   if        not %open(tivin00r)
008500010330     C                   open      tivin00r
008600010330     C                   endif
008700031121     C                   if        not %open(edivabwr)
008800031121     C                   open      edivabwr
008900010330     C                   endif
009000010330     C*
009100010604     C                   clear                   §CTROK
009200010604     C                   clear                   §CTRMO
009300010604     C                   clear                   §CTRNO
009400010330     C*
009500010330     C                   DO        *HIVAL
009600010330     C*
009700010330     C                   READ      tivin00r                               70
009800010618     C*
009900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010000010618     C                   if        vinflg = '1'
010100010618     C                   eval      flgOk = '1'
010200010618     C                   endif
010300010618     C*
010400010330     C                   if        vindta > *blanks
010500010330     C                   add       1             rrnum
010600010330     C*
010700010601     C                   if        *in70 = *off and
010800010330     C                             (vinflg = *blanks
010900010330     C                              or vinflg = '0'
011000010330     C                              or vinflg = '2')
011100010330     C*
011200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011300010711     C                   if        vinflg = *blanks or vinflg = '0'
011400010711     C                   clear                   vinmsg
011500010711     C                   endif
011600010601     C*
011700010330     C                   exsr      inzvar
011800010330     C                   exsr      defcam
011900010605     C                   exsr      impvab
012000010601     C*
012100010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012200010605     C                   eval      depfil = VABLNP
012300010601     C                   exsr      repfil
012400010601     C                   if        depfil = invfil
012500021025     C                   if        vlrpoi = 999
012600021025     C                   MOVE(P)   invfil        VABFGS
012700021025     C                   else
012800021025     C                   MOVE(P)   vlrpoi        VABFGS
012900021025     C                   endif
013000010601     C*
013100010601     C                   exsr      PREELA
013200010601     C*
013300010604     C* Ebbene...
013400010604     C*
013500010604     C  N31              ADD       1             §CTROK            7 0
013600010604     C   32              ADD       1             §CTRMO            7 0
013700010604     C   31              ADD       1             §CTRNO            7 0
013800020205     C*
013900020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014000031121     C  N31              WRITE     EDIVAB00
014100010604     C*
014200010604     C                   if        *in31 = *off and
014300010604     C                             *in32 = *off
014400010604     C                   eval      vinflg = '1'
014500010604     C                   else
014600010604     C                   eval      vinflg = '2'
014700010604     C                   endif
014800010604     C                   endif
014900010601     C*
015000010604     C                   endif
015100010604     C*
015200010330     C                   else
015300010330     C                   eval      vinflg = '1'
015400010330     C                   endif
015500010601     C*
015600010601     C  N70              update    tivin000
015700010330     C*
015800010330     C  N70              ENDdo
015900010601     C
016000010601     C*
016100010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016200010601     C                   if        cntNonEl = *zeros or
016300010601     C                             flgMulti = '0'
016400010330     C* Se non ci sono record con errori ...
016500010601     C                   if        §ctrno = 0 and
016600010604     C                             §ctrmo = 0 and
016700010601     C                             flgStato <> '2'
016800010330     C* ... restituisco esito OK.
016900010330     C                   eval      wrkesito = '0'
017000010330     C                   else
017100010330     C                   if        §ctrok > 0
017200010330     C                   eval      wrkesito = '1'
017300010330     C                   else
017400010615     C                   if        flgOk = '0'
017500010615     C                   eval      wrkesito = '2'
017600010615     C                   else
017700010615     C                   eval      wrkesito = '6'
017800010615     C                   endif
017900010330     C                   endif
018000010330     C                   endif
018100010601     C                   else
018200010601     C                   eval      wrkesito = '9'
018300010601     C                   endif
018400010330     C*
018500010330     C                   if        %open(tivin00r)
018600010330     C                   close     tivin00r
018700010330     C                   endif
018800031121     C                   if        %open(edivabwr)
018900031121     C                   close     edivabwr
019000010330     C                   endif
019100010601     C*
019200010601     C                   if        vlrpoi <> 999
019300010601     C                   eval      invfil = vlrpoi
019400010601     C                   endif
019500010330     C*
019600010330     C                   if        §ctrok > 0
019700010601     C                             and invfil > *zeros
019800010330     C                   exsr      invio
019900010330     C                   endif
020000010601     C*
020100010618     C                   if        flgGiro = '1'
020200010601     C                   exsr      endela
020300010618     C                   endif
020400010330     C*
020500010330     C                   ENDSR
020600010330     C***
020700010330
020800010601
020900010601
021000010601
021100010601
021200010330     C*----------------------------------------------------*
021300010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021400010330     C*----------------------------------------------------*
021500010330     C     INZVAR        BEGSR
021600010330     C*
021700020204     C                   Z-ADD     *zeros        Num5_0            5 0
021800020213     C                   Z-ADD     *zeros        Num15_3          15 3
021900020204     C                   MOVEL     *blanks       WrkNote          70
022000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022100020322     C                   MOVEL     '0'           FlgCAS            1
022200010330     C*
022300010330     C                   ENDSR
022400010330     C*----------------------------------------------------*
022500020322 xxx C*  IMPOSTAZIONE CAMPI COSTANTI
022600010330     C*----------------------------------------------------*
022700010330     C     DEFCAM        BEGSR
022800010330     C*
022900020204     C* Inizializzo il buffer del record da scrivere
023000031121     C                   CLEAR                   EDIVAB00
023100020204     C* Imposto i valori di default...
023200020403     C                   EVAL      VABCCM = 0591254
023300020403     C                   EVAL      VABCTM = '7Q'
023400020329     C                   EVAL      VABLNP = 059
023500020322     C                   EVAL      VABCTR = 000
023600020204     C* ... e poi verifico se sono stati passati come parametri
023700020204     C                   IF        vlrppt > *blanks
023800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023900020204     C                   EXSR      CHKNUM
024000020204     C                   IF        PiInt=*on
024100020204     C                   Z-ADD     PiVal         VABCCM
024200020204     C                   ENDIF
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABLNP
024700020204     C                   ENDIF
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020204     C                   Z-ADD     PiVal         VABCTR
025200020204     C                   ENDIF
025300020204     C                   ENDIF
025400020204     C*
025500010330     C                   ENDSR
025600010607     C*----------------------------------------------------*
025700031121     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
025800010607     C*----------------------------------------------------*
025900010607     C     IMPVAB        BEGSR
026000010607     C*
026100010607     C                   SETOFF                                       3132
026200010607     C*
026300010607     C* Reperimento campi ALFA
026400010607     C*
026500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026600020329     C                   EVAL      VABRSD=%trim(%subst(vindta:186:30))
026700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026900020117     C* ==
027000020329     C                   EVAL      VABRD2=%trim(%subst(vindta:216:30))
027100020329     C                   EVAL      VABIND=%trim(%subst(vindta:246:30))
027200020329     C                   EVAL      VABLOD=%trim(%subst(vindta:323:30))
027300020329     C                   EVAL      VABPRD=%trim(%subst(vindta:353:2))
027400020329     C                   EVAL      VABNZD=%trim(%subst(vindta:355:3))
027500020403     C                   IF        %trim(VABNZD)='IT'
027600020403     C                   EVAL      VABNZD=*BLANKS
027700020403     C                   ENDIF
027800100309     C                   EVAL      VABRMA=%trim(%subst(vindta:358:14))
027900040408     C                   EVAL      VABNOT=%trim(%subst(vindta:901:35))
028000040408     C                   EVAL      VABNT2=%trim(%subst(vindta:901+35:35))
028100020329     C*
028200020329     C* Verifico se nel record nn c'è solo 1 DDT
028300020329     C                   IF        %trim(%subst(vindta:401:5)) > *ZEROS
028400020329     C                   SETON                                        32
028500020329     C                   EVAL      vinmsg = %trimr(vinmsg)
028600020329     C                             + ' ' + '+ DDT SUL RECORD'
028700020329     C                   ENDIF
028800020329     C*
028900020329     C                   MOVEL     DATCOR        VABAAS
029000020329     C                   MOVE      DATCOR        VABMGS
029100010607     C*
029200010607     C* Reperimento campi NUMERICI
029300030326     C                   EVAL      PiStr=%trim(%subst(vindta:178:6)+
029400030326     C                                         %subst(vindta:185:1))
029500020304     C                   EXSR      CHKNUM
029600020304     C                   IF        PiInt=*on
029700020304     C                   Z-ADD     PiVal         VABNSP
029800020304     C                   ELSE
029900020304     C                   SETON                                        32
030000020304     C                   Z-ADD     *zeros        VABNSP
030100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030200030326     C                             + ' ' + 'VABNSP'
030300020304     C                   ENDIF
030400030326     C*
030500140822     C                   EVAL      PiStr=%trim(%subst(vindta:363:5))
030600030326     C                   EXSR      CHKNUM
030700030326     C                   IF        PiInt=*on
030800030326     C                   Z-ADD     PiVal         VABRMN
030900030326     C                   ELSE
031000030326     C                   SETON                                        32
031100030326     C                   Z-ADD     *zeros        VABRMN
031200030326     C                   EVAL      vinmsg = %trimr(vinmsg)
031300030326     C                             + ' ' + 'VABRMN'
031400030326     C                   ENDIF
031500020304     C*
031600020329     C                   EVAL      PiStr=%trim(%subst(vindta:306:5))
031700010607     C                   EXSR      CHKNUM
031800010607     C                   IF        PiInt=*on
031900020204     C                   Z-ADD     PiVal         Num5_0
032000020204     C                   MOVEL(p)  Num5_0        VABCAD
032100010607     C                   ELSE
032200010607     C                   SETON                                        32
032300020204     C                   EVAL      VABCAD = *zeros
032400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032500020204     C                             + ' ' + 'VABCAD'
032600010607     C                   ENDIF
032700010607     C*
032800020329     C                   EVAL      PiStr=%trim(%subst(vindta:868:5))
032900010607     C                   EXSR      CHKNUM
033000010607     C                   IF        PiInt=*on
033100010607     C                   Z-ADD     PiVal         VABNCL
033200010607     C                   ELSE
033300010607     C                   SETON                                        32
033400010607     C                   Z-ADD     *zeros        VABNCL
033500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033600010607     C                             + ' ' + 'VABNCL'
033700010607     C                   ENDIF
033800020304     C*
033900020329     C                   EVAL      PiStr=%trim(%subst(vindta:873:9))
034000020204     C                   EVAL      PiDecChr = ','
034100010607     C                   EXSR      CHKNUM
034200010607     C                   IF        PiNum=*on
034300020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
034400020329     C* riferisce ad un campo 9,3
034500020329     C                   EVAL(H)   VABPKB = PiVal/1000
034600010607     C                   ELSE
034700010607     C                   SETON                                        32
034800010607     C                   Z-ADD     *zeros        VABPKB
034900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035000010607     C                             + ' ' + 'VABPKB'
035100010607     C                   ENDIF
035200020304     C*
035300020329     C                   EVAL      PiStr=%trim(%subst(vindta:891:9))
035400020304     C                   EVAL      PiDecChr = ','
035500020304     C                   EXSR      CHKNUM
035600020304     C                   IF        PiNum=*on
035700020329     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
035800020329     C* riferisce ad un campo 9,3
035900021003     C                   EVAL(H)   VABVLB = PiVal/1000
036000020304     C                   ELSE
036100020304     C                   SETON                                        32
036200020329     C                   Z-ADD     *zeros        VABVLB
036300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
036400020322     C                             + ' ' + 'VABVLB'
036500020304     C                   ENDIF
036600090626     C*
036700090626     C* Forzature valide dal 01/07/2009
036800090626     C                   IF        datcor >= 20090701
036900090626     C                   EVAL      VABCTR = 500
037000090626     C                   EVAL      VABQFT = VABVLB
037100090626     C                   ENDIF
037200151203     C*
037300151203     C* Forzature valide dal 03/12/2015
037400151203     C                   IF        datcor >= 20151203
037500151203     C                   EVAL      VABCTR = 500
037600151203     C                   EVAL      VABQFT = VABVLB
037700151203     C*
037800151203     C* Forzature nuove dal 23/11/2015
037900151203     C                   IF        VABPKB <= 3
038000151203     C                   EVAL      VABCTR = 300
038100151203     C                   EVAL      VABQFT = *zeros
038200151203     C                   ENDIF
038300151203     C                   ENDIF
038400020322     C*
038500020322     C* Considerazioni finali su CBO/CAS
038600020322     C                   IF        FlgCAS = '1'
038700020322     C                   EVAL      VABCBO = '4'
038800020322     C                   ELSE
038900020322     C                   EVAL      VABCBO = '1'
039000020322     C                   ENDIF
039100020204     C*
039200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039300020204     C                   EXSR      CHKIMPDIV
039400031121     C*
039500031121     C* VALORIZZO CAMPI RELATIVI AL "CMR"
039600031121     C                   MOVE(P)   vlrMSG        wNomeFile
039700031121     C                   EVAL      VABCMR = %subst(wNomeFile:2:7)
039800031121     C                   EVAL      VABDCM = DATCOR
039900031121     C                   EVAL      VABDTS = DATCOR
040000031121     C                   EVAL      VABCNT = 1
040100020204     C*
040200010607     C                   ENDSR
040300010607     C*----------------------------------------------------*
040400010601
040500020204
040600020204     C*----------------------------------------------------*
040700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040800020204     C*----------------------------------------------------*
040900020204     C     CHKIMPDIV     BEGSR
041000020204     C*
041100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041200020204     C                   Z-ADD     *zeros        wrkDec            9 9
041300020204     C*
041400020204     C* Come prima cosa effettuo considerazioni sulla divisa
041500020204     C                   IF        vabIAS > *zeros
041600020204     C                   IF        vabVAS <> 'EUR'
041700020204     C                   EVAL      vabVAS =  'ITL'
041800020204     C                   ENDIF
041900020204     C                   ENDIF
042000020204     C*
042100020204     C                   IF        vabCAS > *zeros
042200020204     C                   IF        vabVCA <> 'EUR'
042300020204     C                   EVAL      vabVCA =  'ITL'
042400020204     C                   ENDIF
042500020204     C                   ENDIF
042600020204     C*
042700020204     C                   IF        vabVMD > *zeros
042800020321     C                   IF        vabVAD <> 'EUR'
042900020204     C                   EVAL      vabVAD =  'ITL'
043000020204     C                   ENDIF
043100020204     C                   ENDIF
043200020204     C*
043300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043400020204     C                   Z-ADD     vabIAS        wrkDec
043500020204     C                   IF        wrkDec > *zeros
043600020204     C                   IF        vabVAS = 'ITL'
043700020204     C                   EVAL      vabIAS = *zeros
043800020204     C                   ENDIF
043900020204     C                   ENDIF
044000020204     C*
044100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044200020204     C                   Z-ADD     vabCAS        wrkDec
044300020204     C                   IF        wrkDec > *zeros
044400020204     C                   IF        vabVCA = 'ITL'
044500020204     C                   EVAL      vabCAS = *zeros
044600020204     C                   ENDIF
044700020204     C                   ENDIF
044800020204     C*
044900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045000020204     C                   Z-ADD     vabVMD        wrkDec
045100020204     C                   IF        wrkDec > *zeros
045200020204     C                   IF        vabVAD = 'ITL'
045300020204     C                   EVAL      vabVMD = *zeros
045400020204     C                   ENDIF
045500020204     C                   ENDIF
045600020204     C*
045700020204     C                   ENDSR
045800020204     C***
045900020204
046000010330
046100010330
046200010330     C*----------------------------------------------------*
046300010330     C*  CONTROLLO NUMERICITA' CAMPI
046400010330     C*----------------------------------------------------*
046500010330     C     CHKNUM        BEGSR
046600010330     C*
046700010606     C                   IF        PiDecChr = *blanks
046800010606     C                   EVAL      PiDecChr = ','
046900010606     C                   ENDIF
047000010606     C*
047100010606     C                   CALL(e)   'ISNUMERIC'
047200010330     C                   PARM                    PiStr            30
047300010606     C                   PARM                    PiDecChr          1
047400010330     C                   PARM      *ZEROS        PiVal            30 9
047500010330     C                   PARM      '0'           PiInt             1
047600010330     C                   PARM      '0'           PiNum             1
047700010330     C                   IF        %error
047800010606     C                   EVAL      PiNum=*off
047900010330     C                   ENDIF
048000010330     C*
048100010330     C                   ENDSR
048200010330     C***
048300010330
048400010601
048500010601
048600010601
048700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048800010601     C     repfil        BEGSR
048900010601     C*
049000010601     C                   if        invfil = *zeros and
049100010601     C                             depfil > *zeros and
049200010629     C                             (vinflg = *blanks or
049300010629     C                              vinflg = *zeros)
049400010601     C
049500010601     C                   eval      invfil = depfil
049600010601     C                   endif
049700010601     C*
049800010601     C                   if        depfil <> invfil and
049900010601     C                             invfil > *zeros
050000010601     C                   eval      flgMulti = '1'
050100010601     C                   if        vinflg = *blanks
050200010601     C                   add       1             cntNonEl
050300010601     C                   endif
050400010601     C                   endif
050500010601     C*
050600010601     C                   if        vinflg = '2'
050700010601     C                   eval      flgStato = '2'
050800010601     C                   endif
050900010601     C*
051000010601     C                   ENDSR
051100010601     C***
051200010601
051300010601
051400010601
051500010330
051600010330
051700010330
051800990920      /TITLE Invio dei dati al punto operativo.
051900000613     C     invio         BEGSR
052000990920     C*
052100990920     C                   reset                   dscmz
052200010601     C                   move      invfil        cmzdst
052300990920     C                   eval      cmzfld = vlrfou
052400990920     C                   eval      cmzmbd = vlrhdl
052500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052600000710     C                   if        prmfir = *blanks
052700031121     C                   eval      cmzfla = 'EDIVAB0F'
052800031121     C                   eval      cmzmba = 'EDIVAB0F'
052900000710     C                   else
053000000710     C                   eval      cmzfla = prmfir
053100000710     C                   eval      cmzmba = prmfir
053200000710     C                   endif
053300990920     C                   eval      cmznrr = *zeros
053400990920     C                   move      §ctrok        cmznrr
053500021018     C                   eval      cmzlba = vlrfl1
053600990920     C                   call(e)   'TIS711C'
053700990920     C                   parm                    dscmz
053800990921     C                   parm      *blanks       esito
053900990920     C                   if        %error
054000990920     C                             or cmzerr = '1'
054100990921     C                             or esito  = '1'
054200000710     C                   eval      wrkesito = '3'
054300990920     C                   endif
054400990920     C*
054500000613     C                   ENDSR
054600990910
054700010601
054800010601
054900010601
055000010601
055100010601      /TITLE Invio dei dati al punto operativo.
055200010601     C     opeini        BEGSR
055300010601     C*
055400010601     C* Inizializzo flag e contatori operativi
055500010601     C                   movel     '0'           flgGiro           1
055600010601     C                   movel     '0'           flgMulti          1
055700010601     C                   movel     '1'           flgStato          1
055800010615     C                   movel     '0'           flgOk             1
055900010601     C                   z-add     *zeros        cntNonEl         10 0
056000010601     C                   z-add     *zeros        depfil            3 0
056100010601     C                   z-add     *zeros        invfil            3 0
056200010601     C*
056300010601     C                   ENDSR
056400010601     C***
056500010601
056600010601
056700010601
056800010330
056900010330
057000000613     C     *inzsr        BEGSR
057100990910     C*
057200990910     C     *entry        plist
057300990920     C                   parm                    tivlrds
057400990921     C                   parm      wrkesito      esito
057500000724     C                   parm                    prmlit
057600000710     C                   parm                    prmfir
057700010330     C*
057800010330     C* CALCOLA LA DATA CORRENTE
057900010330     C                   time                    wn14             14 0
058000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058100010330     C                   z-add     wn8           g08dat
058200010330     C                   z-add     *zeros        g08inv
058300010330     C                   movel     '0'           g08err
058400010330     C                   call      'XSRDA8'
058500010330     C                   parm                    wlbda8
058600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058700000613     C*
058800000613     C                   ENDSR
058900000613     C***
