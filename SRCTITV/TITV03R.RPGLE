000100021025      /TITLE Upload via Internet: traduzione in FIVABWWR. (x STEFANEL Cod. 0591072)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700050314     D*------------
000800050314     D* SCHIERE A RUN-TIME
000900050314     D*------------
001000050314     D* - "SERIE" CLIENTE ALFA
001100050314     D SERALF          S              1    DIM(10) CTDATA PERRCD(10)
001200050314     D* - "SERIE" CLIENTE NUMERICA
001300050314     D SERNUM          S              1    DIM(10) CTDATA PERRCD(10)
001400010330     D*----------------------------------------------------
001500010330     D* DICHIARAZIOINE VARIABILI DI WRK
001600010330     D*----------------------------------------------------
001700010330     D dscmz         e ds                  inz
001800010330     D psds           sds
001900010330     D  procname         *PROC
002000010330     D tivlrds       e ds                  extname(tivlr00f)
002100010330     D esito           s              1
002200010330     D prmlit          s             10
002300010330     D prmfir          s             10
002400010330     D wrkesito        s                   like(esito)
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600010330     D*------------------
002700010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002800010330     D*------------------
002900010330     D WLBDA8          DS                  INZ
003000010330     D  G08DAT                 1      8  0
003100010330     D  G08INV                 9     16  0
003200010330     D  G08ERR                17     17
003300010330     D  G08TGI                18     22  0
003400000313     D*
003500010330
003600010330
003700010330
003800000913     C                   reset                   rrnum
003900990921     C                   reset                   esito
004000990921     C                   reset                   wrkesito
004100010601     C*
004200010601     C                   exsr      opeini
004300010605     C                   exsr      rwvab
004400010601     C*
004500010601     C                   seton                                        lr
004600010601
004700010601
004800010601
004900010601
005000010601     C*--------------------------------------------------------
005100010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005200010601     C*--------------------------------------------------------
005300010601     C     PREELA        BEGSR
005400010601     C*
005500010601     C* SE OCCORRE SPEDIRE IN FILIALE
005600010601     C                   if        invfil <> *zeros and
005700010601     C                             flgGiro = '0'
005800010601     C*
005900010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006000010601     C                   eval      flgGiro = '1'
006100010601     C*
006200010601     C                   endif
006300010601     C*
006400010601     C                   ENDSR
006500010601     C***
006600010601
006700010601
006800010601
006900010601
007000010601
007100010601
007200010601
007300010601     C*--------------------------------------------------------
007400010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007500010601     C*--------------------------------------------------------
007600010601     C     ENDELA        BEGSR
007700000616     C*
007800010601     C                   ENDSR
007900010601     C***
008000000613
008100010330
008200010601
008300010601
008400010601
008500010330     C*--------------------------------------------------------
008600021025     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
008700010330     C*--------------------------------------------------------
008800010605     C     RWVAB         BEGSR
008900010330     C*
009000010330     C                   if        not %open(tivin00r)
009100010330     C                   open      tivin00r
009200010330     C                   endif
009300021025     C                   if        not %open(fivabwwr)
009400021025     C                   open      fivabwwr
009500010330     C                   endif
009600010330     C*
009700010604     C                   clear                   §CTROK
009800010604     C                   clear                   §CTRMO
009900010604     C                   clear                   §CTRNO
010000010330     C*
010100010330     C                   DO        *HIVAL
010200010330     C*
010300010330     C                   READ      tivin00r                               70
010400010618     C*
010500010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010600010618     C                   if        vinflg = '1'
010700010618     C                   eval      flgOk = '1'
010800010618     C                   endif
010900010618     C*
011000010330     C                   if        vindta > *blanks
011100010330     C                   add       1             rrnum
011200010330     C*
011300010601     C                   if        *in70 = *off and
011400010330     C                             (vinflg = *blanks
011500010330     C                              or vinflg = '0'
011600010330     C                              or vinflg = '2')
011700010330     C*
011800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011900010711     C                   if        vinflg = *blanks or vinflg = '0'
012000010711     C                   clear                   vinmsg
012100010711     C                   endif
012200010601     C*
012300010330     C                   exsr      inzvar
012400010330     C                   exsr      defcam
012500010605     C                   exsr      impvab
012600010601     C*
012700010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012800010605     C                   eval      depfil = VABLNP
012900010601     C                   exsr      repfil
013000010601     C                   if        depfil = invfil
013100021025     C                   if        vlrpoi = 999
013200021025     C                   MOVE(P)   invfil        VABFGS
013300021025     C                   else
013400021025     C                   MOVE(P)   vlrpoi        VABFGS
013500021025     C                   endif
013600010601     C*
013700010601     C                   exsr      PREELA
013800010601     C*
013900010604     C* Ebbene...
014000010604     C*
014100010604     C  N31              ADD       1             §CTROK            7 0
014200010604     C   32              ADD       1             §CTRMO            7 0
014300010604     C   31              ADD       1             §CTRNO            7 0
014400020205     C*
014500020205     C* Effettuo test particolare solo x DA ROLD x condizoinare la scrittura
014600021025     C  N31              WRITE     FIVAB000
014700010604     C*
014800010604     C                   if        *in31 = *off and
014900010604     C                             *in32 = *off
015000010604     C                   eval      vinflg = '1'
015100010604     C                   else
015200010604     C                   eval      vinflg = '2'
015300010604     C                   endif
015400010604     C                   endif
015500010601     C*
015600010604     C                   endif
015700010604     C*
015800010330     C                   else
015900010330     C                   eval      vinflg = '1'
016000010330     C                   endif
016100010601     C*
016200010601     C  N70              update    tivin000
016300010330     C*
016400010330     C  N70              ENDdo
016500010601     C
016600010601     C*
016700010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016800010601     C                   if        cntNonEl = *zeros or
016900010601     C                             flgMulti = '0'
017000010330     C* Se non ci sono record con errori ...
017100010601     C                   if        §ctrno = 0 and
017200010604     C                             §ctrmo = 0 and
017300010601     C                             flgStato <> '2'
017400010330     C* ... restituisco esito OK.
017500010330     C                   eval      wrkesito = '0'
017600010330     C                   else
017700010330     C                   if        §ctrok > 0
017800010330     C                   eval      wrkesito = '1'
017900010330     C                   else
018000010615     C                   if        flgOk = '0'
018100010615     C                   eval      wrkesito = '2'
018200010615     C                   else
018300010615     C                   eval      wrkesito = '6'
018400010615     C                   endif
018500010330     C                   endif
018600010330     C                   endif
018700010601     C                   else
018800010601     C                   eval      wrkesito = '9'
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        %open(tivin00r)
019200010330     C                   close     tivin00r
019300010330     C                   endif
019400021025     C                   if        %open(fivabwwr)
019500021025     C                   close     fivabwwr
019600010330     C                   endif
019700010601     C*
019800010601     C                   if        vlrpoi <> 999
019900010601     C                   eval      invfil = vlrpoi
020000010601     C                   endif
020100010330     C*
020200010330     C                   if        §ctrok > 0
020300010601     C                             and invfil > *zeros
020400010330     C                   exsr      invio
020500010330     C                   endif
020600010601     C*
020700010618     C                   if        flgGiro = '1'
020800010601     C                   exsr      endela
020900010618     C                   endif
021000010330     C*
021100010330     C                   ENDSR
021200010330     C***
021300010330
021400010601
021500010601
021600010601
021700010601
021800010330     C*----------------------------------------------------*
021900010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022000010330     C*----------------------------------------------------*
022100010330     C     INZVAR        BEGSR
022200010330     C*
022300020204     C                   Z-ADD     *zeros        Num5_0            5 0
022400020213     C                   Z-ADD     *zeros        Num15_3          15 3
022500020204     C                   MOVEL     *blanks       WrkNote          70
022600010330     C*
022700010330     C                   ENDSR
022800010330     C*----------------------------------------------------*
022900010330     C*  IMPOSTAZIONE CAMPI COSTANTI
023000010330     C*----------------------------------------------------*
023100010330     C     DEFCAM        BEGSR
023200010330     C*
023300020204     C* Inizializzo il buffer del record da scrivere
023400021025     C                   CLEAR                   FIVAB000
023500020204     C* Imposto i valori di default...
023600020304     C                   EVAL      VABCCM = 0591072
023700020304     C                   EVAL      VABLNP = 059
023800110630     C                   EVAL      VABCTR = 300
023900020304     C                   EVAL      VABCTM = '7Q'
024000020204     C* ... e poi verifico se sono stati passati come parametri
024100020204     C                   IF        vlrppt > *blanks
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABCCM
024600020204     C                   ENDIF
024700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024800020204     C                   EXSR      CHKNUM
024900020204     C                   IF        PiInt=*on
025000020204     C                   Z-ADD     PiVal         VABLNP
025100020204     C                   ENDIF
025200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025300020204     C                   EXSR      CHKNUM
025400020204     C                   IF        PiInt=*on
025500020204     C                   Z-ADD     PiVal         VABCTR
025600020204     C                   ENDIF
025700020204     C                   ENDIF
025800020204     C*
025900010330     C                   ENDSR
026000010607     C*----------------------------------------------------*
026100050314     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026200010607     C*----------------------------------------------------*
026300010607     C     IMPVAB        BEGSR
026400010607     C*
026500010607     C                   SETOFF                                       3132
026600010607     C*
026700010607     C* Reperimento campi ALFA
026800010607     C*
026900010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027000021104     C                   EVAL      VABRSD=%trim(%subst(vindta:21:35))
027100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027300020117     C* ==
027400021104     C                   EVAL      VABRD2='Tel. '+%trim(%subst(vindta:165:15))
027500021104     C                   EVAL      VABIND=%trim(%subst(vindta:71:60))
027600021104     C                   EVAL      VABLOD=%trim(%subst(vindta:136:25))
027700021104     C                   IF        %trim(%subst(vindta:161:4))='ROMA'
027800020321     C                   EVAL      VABPRD='RM'
027900020321     C                   ELSE
028000021104     C                   EVAL      VABPRD=%trim(%subst(vindta:161:4))
028100020321     C                   ENDIF
028200090422     C                   IF        %subst(vindta:256:15) = *blanks
028300090422     C                   EVAL      VABRMA=%trim(%subst(vindta:12:1))+
028400090422     C                                    %trim(%subst(vindta:14:7))
028500090422     C                   ELSE
028600090422     C                   EVAL      VABRMA=%trim(%subst(vindta:256:15))
028700090422     C                   ENDIF
028800090422     C                   EVAL      VABNOT=%trim(%subst(vindta:219:31)) + ' ' +
028900090422     C                                    %trim(%subst(vindta:271:50))
029000010607     C*
029100010607     C* Reperimento campi NUMERICI
029200021104     C                   EVAL      PiStr=%trim(%subst(vindta:4:4))
029300020304     C                   EXSR      CHKNUM
029400020304     C                   IF        PiInt=*on
029500020304     C                   Z-ADD     PiVal         VABAAS
029600020304     C                   ELSE
029700020304     C                   SETON                                        32
029800020304     C                   EVAL      VABAAS = *zeros
029900020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030000020304     C                             + ' ' + 'VABAAS'
030100020304     C                   ENDIF
030200020304     C*
030300021104     C                   EVAL      PiStr=%trim(%subst(vindta:8:4))
030400020304     C                   EXSR      CHKNUM
030500020304     C                   IF        PiInt=*on
030600020304     C                   Z-ADD     PiVal         VABMGS
030700020304     C                   ELSE
030800020304     C                   SETON                                        32
030900020304     C                   EVAL      VABMGS = *zeros
031000020304     C                   EVAL      vinmsg = %trimr(vinmsg)
031100020304     C                             + ' ' + 'VABMGS'
031200020304     C                   ENDIF
031300050314     C*
031400050314     C                   MOVEL     *blanks       wSerieAlfa        1
031500050314     C                   MOVEL     *zeros        wSerieNum         1
031600050314     C                   EVAL      wSerieAlfa=%subst(vindta:12:1)
031700050314     C                   Z-ADD     1             wInd              2 0
031800050314     C     wSerieAlfa    LOOKUP    SERALF(wInd)                           55
031900050314     C                   IF        %found
032000050314     C                   EVAL      wSerieNum = SERNUM(wInd)
032100050314     C                   ENDIF
032200050314     C*
032300021104     C                   EVAL      PiStr=%trim(%subst(vindta:14:7))
032400020304     C                   EXSR      CHKNUM
032500020304     C                   IF        PiInt=*on
032600020304     C                   Z-ADD     PiVal         VABNSP
032700050314     C                   MOVEL     wSerieNum     VABNSP
032800020304     C                   Z-ADD     PiVal         VABRMN
032900020304     C                   ELSE
033000020304     C                   SETON                                        32
033100020304     C                   Z-ADD     *zeros        VABNSP
033200020304     C                   EVAL      vinmsg = %trimr(vinmsg)
033300020321     C                             + ' ' + 'VABNSP VABRMN'
033400020304     C                   ENDIF
033500020304     C*
033600021104     C                   EVAL      PiStr=%trim(%subst(vindta:131:5))
033700010607     C                   EXSR      CHKNUM
033800010607     C                   IF        PiInt=*on
033900020204     C                   Z-ADD     PiVal         Num5_0
034000020204     C                   MOVEL(p)  Num5_0        VABCAD
034100010607     C                   ELSE
034200010607     C                   SETON                                        32
034300020204     C                   EVAL      VABCAD = *zeros
034400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034500020204     C                             + ' ' + 'VABCAD'
034600010607     C                   ENDIF
034700010607     C*
034800021104     C                   EVAL      PiStr=%trim(%subst(vindta:180:7))
034900010607     C                   EXSR      CHKNUM
035000010607     C                   IF        PiInt=*on
035100010607     C                   Z-ADD     PiVal         VABNCL
035200010607     C                   ELSE
035300010607     C                   SETON                                        32
035400010607     C                   Z-ADD     *zeros        VABNCL
035500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035600010607     C                             + ' ' + 'VABNCL'
035700010607     C                   ENDIF
035800020304     C*
035900021104     C                   EVAL      PiStr=%trim(%subst(vindta:187:7))
036000020204     C                   EVAL      PiDecChr = ','
036100010607     C                   EXSR      CHKNUM
036200010607     C                   IF        PiNum=*on
036300020304     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
036400020304     C* riferisce ad un campo 7,3
036500020304     C                   EVAL(H)   VABPKB = PiVal/1000
036600010607     C                   ELSE
036700010607     C                   SETON                                        32
036800010607     C                   Z-ADD     *zeros        VABPKB
036900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
037000010607     C                             + ' ' + 'VABPKB'
037100010607     C                   ENDIF
037200020304     C*
037300021104     C                   EVAL      PiStr=%trim(%subst(vindta:194:9))
037400020304     C                   EVAL      PiDecChr = ','
037500020304     C                   EXSR      CHKNUM
037600020304     C                   IF        PiNum=*on
037700020304     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
037800020321     C* riferisce ad un campo 12,2
037900020304     C                   EVAL(H)   VABVLB = PiVal/100
038000020321     C** Divido ulteriormente x 1000 perchè il campo in input si riferisce DM cubi mentre il nostro
038100020321     C** si riferisce ad MC cubi
038200020321     C*                   EVAL(H)   VABVLB = VABVLB/1000
038300020304     C* Una volta determinato il giusto volume lo assegno anche al campo VABQFT x il tipo tariffa
038400110630     C***                Z-ADD     VABVLB        VABQFT
038500020304     C                   ELSE
038600020304     C                   SETON                                        32
038700020304     C                   Z-ADD     *zeros        VABPKB
038800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
038900110630     C***                          + ' ' + 'VABVLB VABQFT'
039000110630     C                             + ' ' + 'VABVLB'
039100020304     C                   ENDIF
039200020204     C*
039300020204     C* Considerazioni finali sui valori sopra ricavati
039400020204     C                   IF        (VABCAS = *zeros)
039500020204     C                   EVAL      VABCBO = '1 '
039600020204     C                   ELSE
039700020204     C                   EVAL      VABCBO = '4 '
039800020204     C                   ENDIF
039900020204     C*
040000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
040100020204     C                   EXSR      CHKIMPDIV
040200020204     C*
040300010607     C                   ENDSR
040400010607     C*----------------------------------------------------*
040500010601
040600020204
040700020204     C*----------------------------------------------------*
040800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040900020204     C*----------------------------------------------------*
041000020204     C     CHKIMPDIV     BEGSR
041100020204     C*
041200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041300020204     C                   Z-ADD     *zeros        wrkDec            9 9
041400020204     C*
041500020204     C* Come prima cosa effettuo considerazioni sulla divisa
041600020204     C                   IF        vabIAS > *zeros
041700020204     C                   IF        vabVAS <> 'EUR'
041800020204     C                   EVAL      vabVAS =  'ITL'
041900020204     C                   ENDIF
042000020204     C                   ENDIF
042100020204     C*
042200020204     C                   IF        vabCAS > *zeros
042300020204     C                   IF        vabVCA <> 'EUR'
042400020204     C                   EVAL      vabVCA =  'ITL'
042500020204     C                   ENDIF
042600020204     C                   ENDIF
042700020204     C*
042800020204     C                   IF        vabVMD > *zeros
042900020321     C                   IF        vabVAD <> 'EUR'
043000020204     C                   EVAL      vabVAD =  'ITL'
043100020204     C                   ENDIF
043200020204     C                   ENDIF
043300020204     C*
043400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043500020204     C                   Z-ADD     vabIAS        wrkDec
043600020204     C                   IF        wrkDec > *zeros
043700020204     C                   IF        vabVAS = 'ITL'
043800020204     C                   EVAL      vabIAS = *zeros
043900020204     C                   ENDIF
044000020204     C                   ENDIF
044100020204     C*
044200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044300020204     C                   Z-ADD     vabCAS        wrkDec
044400020204     C                   IF        wrkDec > *zeros
044500020204     C                   IF        vabVCA = 'ITL'
044600020204     C                   EVAL      vabCAS = *zeros
044700020204     C                   ENDIF
044800020204     C                   ENDIF
044900020204     C*
045000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
045100020204     C                   Z-ADD     vabVMD        wrkDec
045200020204     C                   IF        wrkDec > *zeros
045300020204     C                   IF        vabVAD = 'ITL'
045400020204     C                   EVAL      vabVMD = *zeros
045500020204     C                   ENDIF
045600020204     C                   ENDIF
045700020204     C*
045800020204     C                   ENDSR
045900020204     C***
046000020204
046100010330
046200010330
046300010330     C*----------------------------------------------------*
046400010330     C*  CONTROLLO NUMERICITA' CAMPI
046500010330     C*----------------------------------------------------*
046600010330     C     CHKNUM        BEGSR
046700010330     C*
046800010606     C                   IF        PiDecChr = *blanks
046900010606     C                   EVAL      PiDecChr = ','
047000010606     C                   ENDIF
047100010606     C*
047200010606     C                   CALL(e)   'ISNUMERIC'
047300010330     C                   PARM                    PiStr            30
047400010606     C                   PARM                    PiDecChr          1
047500010330     C                   PARM      *ZEROS        PiVal            30 9
047600010330     C                   PARM      '0'           PiInt             1
047700010330     C                   PARM      '0'           PiNum             1
047800010330     C                   IF        %error
047900010606     C                   EVAL      PiNum=*off
048000010330     C                   ENDIF
048100010330     C*
048200010330     C                   ENDSR
048300010330     C***
048400010330
048500010601
048600010601
048700010601
048800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048900010601     C     repfil        BEGSR
049000010601     C*
049100010601     C                   if        invfil = *zeros and
049200010601     C                             depfil > *zeros and
049300010629     C                             (vinflg = *blanks or
049400010629     C                              vinflg = *zeros)
049500010601     C
049600010601     C                   eval      invfil = depfil
049700010601     C                   endif
049800010601     C*
049900010601     C                   if        depfil <> invfil and
050000010601     C                             invfil > *zeros
050100010601     C                   eval      flgMulti = '1'
050200010601     C                   if        vinflg = *blanks
050300010601     C                   add       1             cntNonEl
050400010601     C                   endif
050500010601     C                   endif
050600010601     C*
050700010601     C                   if        vinflg = '2'
050800010601     C                   eval      flgStato = '2'
050900010601     C                   endif
051000010601     C*
051100010601     C                   ENDSR
051200010601     C***
051300010601
051400010601
051500010601
051600010330
051700010330
051800010330
051900990920      /TITLE Invio dei dati al punto operativo.
052000000613     C     invio         BEGSR
052100990920     C*
052200990920     C                   reset                   dscmz
052300010601     C                   move      invfil        cmzdst
052400990920     C                   eval      cmzfld = vlrfou
052500990920     C                   eval      cmzmbd = vlrhdl
052600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052700000710     C                   if        prmfir = *blanks
052800021025     C                   eval      cmzfla = 'FIVAB00F'
052900021025     C                   eval      cmzmba = 'FIVAB00F'
053000000710     C                   else
053100000710     C                   eval      cmzfla = prmfir
053200000710     C                   eval      cmzmba = prmfir
053300000710     C                   endif
053400990920     C                   eval      cmznrr = *zeros
053500990920     C                   move      §ctrok        cmznrr
053600021018     C                   eval      cmzlba = vlrfl1
053700990920     C                   call(e)   'TIS711C'
053800990920     C                   parm                    dscmz
053900990921     C                   parm      *blanks       esito
054000990920     C                   if        %error
054100990920     C                             or cmzerr = '1'
054200990921     C                             or esito  = '1'
054300000710     C                   eval      wrkesito = '3'
054400990920     C                   endif
054500990920     C*
054600000613     C                   ENDSR
054700990910
054800010601
054900010601
055000010601
055100010601
055200010601      /TITLE Invio dei dati al punto operativo.
055300010601     C     opeini        BEGSR
055400010601     C*
055500010601     C* Inizializzo flag e contatori operativi
055600010601     C                   movel     '0'           flgGiro           1
055700010601     C                   movel     '0'           flgMulti          1
055800010601     C                   movel     '1'           flgStato          1
055900010615     C                   movel     '0'           flgOk             1
056000010601     C                   z-add     *zeros        cntNonEl         10 0
056100010601     C                   z-add     *zeros        depfil            3 0
056200010601     C                   z-add     *zeros        invfil            3 0
056300010601     C*
056400010601     C                   ENDSR
056500010601     C***
056600010601
056700010601
056800010601
056900010330
057000010330
057100000613     C     *inzsr        BEGSR
057200990910     C*
057300990910     C     *entry        plist
057400990920     C                   parm                    tivlrds
057500990921     C                   parm      wrkesito      esito
057600000724     C                   parm                    prmlit
057700000710     C                   parm                    prmfir
057800010330     C*
057900010330     C* CALCOLA LA DATA CORRENTE
058000010330     C                   time                    wn14             14 0
058100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
058200010330     C                   z-add     wn8           g08dat
058300010330     C                   z-add     *zeros        g08inv
058400010330     C                   movel     '0'           g08err
058500010330     C                   call      'XSRDA8'
058600010330     C                   parm                    wlbda8
058700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058800000613     C*
058900000613     C                   ENDSR
059000000613     C***
059100050314** "SERIE" CLIENTE ALFA **
059200050314CF
059300050314** "SERIE" CLIENTE NUMERICA **
05940005031436
