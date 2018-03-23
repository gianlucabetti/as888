000100041203      /TITLE Upload via Internet: traduzione in FIVABWWR. (x PAFFONI Cod. 1060177)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021014     FFIVABwwr  O    E             DISK    usropn
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
001900010330     D*------------------
002000010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002100010330     D*------------------
002200010330     D WLBDA8          DS                  INZ
002300010330     D  G08DAT                 1      8  0
002400010330     D  G08INV                 9     16  0
002500010330     D  G08ERR                17     17
002600010330     D  G08TGI                18     22  0
002700000313     D*
002800010330
002900010330
003000000913     C                   reset                   rrnum
003100990921     C                   reset                   esito
003200990921     C                   reset                   wrkesito
003300010601     C*
003400010601     C                   exsr      opeini
003500010605     C                   exsr      rwvab
003600010601     C*
003700010601     C                   seton                                        lr
003800010601
003900010601
004000010601
004100010601
004200010601     C*--------------------------------------------------------
004300010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004400010601     C*--------------------------------------------------------
004500010601     C     PREELA        BEGSR
004600010601     C*
004700010601     C* SE OCCORRE SPEDIRE IN FILIALE
004800010601     C                   if        invfil <> *zeros and
004900010601     C                             flgGiro = '0'
005000010601     C*
005100010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005200010601     C                   eval      flgGiro = '1'
005300010601     C*
005400010601     C                   endif
005500010601     C*
005600010601     C                   ENDSR
005700010601     C***
005800010601
005900010601
006000010601
006100010601     C*--------------------------------------------------------
006200010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006300010601     C*--------------------------------------------------------
006400010601     C     ENDELA        BEGSR
006500000616     C*
006600010601     C                   ENDSR
006700010601     C***
006800000613
006900010601
007000010601
007100010330     C*--------------------------------------------------------
007200040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007300010330     C*--------------------------------------------------------
007400010605     C     RWVAB         BEGSR
007500010330     C*
007600010330     C                   if        not %open(tivin00r)
007700010330     C                   open      tivin00r
007800010330     C                   endif
007900021014     C                   if        not %open(fivabwwr)
008000021014     C                   open      fivabwwr
008100010330     C                   endif
008200010330     C*
008300010604     C                   clear                   §CTROK
008400010604     C                   clear                   §CTRMO
008500010604     C                   clear                   §CTRNO
008600010330     C*
008700010330     C                   DO        *HIVAL
008800010330     C*
008900010330     C                   READ      tivin00r                               70
009000010618     C*
009100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009200010618     C                   if        vinflg = '1'
009300010618     C                   eval      flgOk = '1'
009400010618     C                   endif
009500010618     C*
009600010330     C                   if        vindta > *blanks
009700010330     C                   add       1             rrnum
009800010330     C*
009900010601     C                   if        *in70 = *off and
010000010330     C                             (vinflg = *blanks
010100010330     C                              or vinflg = '0'
010200010330     C                              or vinflg = '2')
010300010330     C*
010400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010500010711     C                   if        vinflg = *blanks or vinflg = '0'
010600010711     C                   clear                   vinmsg
010700010711     C                   endif
010800010601     C*
010900010330     C                   exsr      inzvar
011000010330     C                   exsr      defcam
011100010605     C                   exsr      impvab
011200010601     C*
011300010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011400010605     C                   eval      depfil = VABLNP
011500010601     C                   exsr      repfil
011600010601     C                   if        depfil = invfil
011700021025     C                   if        vlrpoi = 999
011800021025     C                   MOVE(P)   invfil        VABFGS
011900021025     C                   else
012000021025     C                   MOVE(P)   vlrpoi        VABFGS
012100021025     C                   endif
012200010601     C*
012300010601     C                   exsr      PREELA
012400010601     C*
012500010604     C* Ebbene...
012600010604     C*
012700010604     C  N31              ADD       1             §CTROK            7 0
012800010604     C   32              ADD       1             §CTRMO            7 0
012900010604     C   31              ADD       1             §CTRNO            7 0
013000020205     C*
013100040301     C* Scarico il buffer del FIVAB
013200021014     C  N31              WRITE     FIVAB000
013300010604     C*
013400020722     C                   endif
013500020722     C*
013600010604     C                   if        *in31 = *off and
013700010604     C                             *in32 = *off
013800010604     C                   eval      vinflg = '1'
013900010604     C                   else
014000010604     C                   eval      vinflg = '2'
014100010604     C                   endif
014200010604     C                   endif
014300010604     C*
014400010330     C                   else
014500010330     C                   eval      vinflg = '1'
014600010330     C                   endif
014700010601     C*
014800010601     C  N70              update    tivin000
014900010330     C*
015000010330     C  N70              ENDdo
015100010601     C
015200010601     C*
015300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015400010601     C                   if        cntNonEl = *zeros or
015500010601     C                             flgMulti = '0'
015600010330     C* Se non ci sono record con errori ...
015700010601     C                   if        §ctrno = 0 and
015800010604     C                             §ctrmo = 0 and
015900010601     C                             flgStato <> '2'
016000010330     C* ... restituisco esito OK.
016100010330     C                   eval      wrkesito = '0'
016200010330     C                   else
016300010330     C                   if        §ctrok > 0
016400010330     C                   eval      wrkesito = '1'
016500010330     C                   else
016600010615     C                   if        flgOk = '0'
016700010615     C                   eval      wrkesito = '2'
016800010615     C                   else
016900010615     C                   eval      wrkesito = '6'
017000010615     C                   endif
017100010330     C                   endif
017200010330     C                   endif
017300010601     C                   else
017400010601     C                   eval      wrkesito = '9'
017500010601     C                   endif
017600010330     C*
017700010330     C                   if        %open(tivin00r)
017800010330     C                   close     tivin00r
017900010330     C                   endif
018000021014     C                   if        %open(fivabwwr)
018100021014     C                   close     fivabwwr
018200010330     C                   endif
018300010601     C*
018400010601     C                   if        vlrpoi <> 999
018500010601     C                   eval      invfil = vlrpoi
018600010601     C                   endif
018700010330     C*
018800010330     C                   if        §ctrok > 0
018900010601     C                             and invfil > *zeros
019000010330     C                   exsr      invio
019100010330     C                   endif
019200010601     C*
019300010618     C                   if        flgGiro = '1'
019400010601     C                   exsr      endela
019500010618     C                   endif
019600010330     C*
019700010330     C                   ENDSR
019800010330     C***
019900010330
020000010601
020100010601
020200010601
020300010601
020400010330     C*----------------------------------------------------*
020500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020600010330     C*----------------------------------------------------*
020700010330     C     INZVAR        BEGSR
020800010330     C*
020900020204     C                   Z-ADD     *zeros        Num5_0            5 0
021000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021100020322     C                   MOVEL     '0'           FlgCAS            1
021200010330     C*
021300010330     C                   ENDSR
021400010330     C*----------------------------------------------------*
021500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021600010330     C*----------------------------------------------------*
021700010330     C     DEFCAM        BEGSR
021800010330     C*
021900020204     C* Inizializzo il buffer del record da scrivere
022000021014     C                   CLEAR                   FIVAB000
022100020204     C* Imposto i valori di default...
022200041203     C                   EVAL      VABCCM = 1060177
022300041203     C                   EVAL      VABLNP = 106
022400040713     C                   EVAL      VABCTR = 000
022500070606     C                   EVAL      VABCTM = '7A'
022600041108     C                   EVAL      VABCBO = '1'
022700020204     C* ... e poi verifico se sono stati passati come parametri
022800020204     C                   IF        vlrppt > *blanks
022900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023100020204     C                   EXSR      CHKNUM
023200020204     C                   IF        PiInt=*on
023300020204     C                   Z-ADD     PiVal         VABCCM
023400020204     C                   ENDIF
023500040301     C                   ENDIF
023600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023800020204     C                   EXSR      CHKNUM
023900020204     C                   IF        PiInt=*on
024000020204     C                   Z-ADD     PiVal         VABLNP
024100020204     C                   ENDIF
024200040301     C                   ENDIF
024300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024500020204     C                   EXSR      CHKNUM
024600020204     C                   IF        PiInt=*on
024700020204     C                   Z-ADD     PiVal         VABCTR
024800040301     C                   ENDIF
024900020204     C                   ENDIF
025000150921     C                   IF        %subst(vlrppt:16:2) <> *blanks
025100150921     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
025200150921     C                   ENDIF
025300020204     C                   ENDIF
025400020204     C*
025500010330     C                   ENDSR
025600010607     C*----------------------------------------------------*
025700021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025800010607     C*----------------------------------------------------*
025900010607     C     IMPVAB        BEGSR
026000010607     C*
026100010607     C                   SETOFF                                       3132
026200010607     C*
026300010607     C* Reperimento campi ALFA
026400010607     C*
026500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026600041203     C                   EVAL      VABRSD=%trim(%subst(vindta:156:30))
026700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026900020117     C* ==
027000041203     C                   EVAL      VABRD2=%trim(%subst(vindta:186:40))
027100041203     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
027200041203     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
027300041203     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
027400050124     C                   EVAL      VABRMA=%trim(%subst(vindta:464:35))
027500041203     C                   EVAL      VABNOT=%trim(%subst(vindta:464:35))
027600041203     C                   EVAL      VABNT2=%trim(%subst(vindta:464+35:60-35))
027700180226     C                   SELECT
027800180226     C                   WHEN      %subst(vindta:537) = '1'
027900180226     C                   EVAL      VABCBO='1'
028000180226     C                   WHEN      %subst(vindta:537) = '2'
028100180226     C                   EVAL      VABCBO='2'
028200180226     C                   ENDSL
028300010607     C*
028400010607     C* Reperimento campi NUMERICI
028500020722     C                   MOVEL     DATCOR        VABAAS
028600020722     C                   MOVE      DATCOR        VABMGS
028700040420     C* NSP/RMN
028800041203     C                   EVAL      PiStr=%trim(%subst(vindta:89:6))
028900020304     C                   EXSR      CHKNUM
029000020304     C                   IF        PiInt=*on
029100040301     C                   Z-ADD     PiVal         VABNSP
029200040301     C                   Z-ADD     PiVal         VABRMN
029300020304     C                   ELSE
029400020722     C                   SETON                                        31
029500020304     C                   Z-ADD     *zeros        VABNSP
029600040301     C                   Z-ADD     *zeros        VABRMN
029700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029800040301     C                             + ' ' + 'VABNSP VABRMN'
029900020304     C                   ENDIF
030000040420     C* CAD
030100041203     C                   EVAL      PiStr=%trim(%subst(vindta:316:5))
030200010607     C                   EXSR      CHKNUM
030300010607     C                   IF        PiInt=*on
030400020204     C                   Z-ADD     PiVal         Num5_0
030500020204     C                   MOVEL(p)  Num5_0        VABCAD
030600010607     C                   ELSE
030700010607     C                   SETON                                        32
030800020204     C                   EVAL      VABCAD = *zeros
030900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031000020204     C                             + ' ' + 'VABCAD'
031100010607     C                   ENDIF
031200040420     C* NCL
031300041203     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
031400010607     C                   EXSR      CHKNUM
031500010607     C                   IF        PiInt=*on
031600010607     C                   Z-ADD     PiVal         VABNCL
031700010607     C                   ELSE
031800010607     C                   SETON                                        32
031900010607     C                   Z-ADD     *zeros        VABNCL
032000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032100010607     C                             + ' ' + 'VABNCL'
032200010607     C                   ENDIF
032300040420     C* PKB
032400041203     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiNum=*on
032700041203     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
032800041203     C* riferisce ad un campo 7,2
032900041203     C                   EVAL(H)   VABPKB = PiVal/100
033000010607     C                   ELSE
033100010607     C                   SETON                                        32
033200010607     C                   Z-ADD     *zeros        VABPKB
033300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033400010607     C                             + ' ' + 'VABPKB'
033500010607     C                   ENDIF
033600041103     C* CAS
033700041203     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
033800041103     C                   EXSR      CHKNUM
033900041103     C                   IF        PiNum=*on
034000041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
034100041203     C* riferisce ad un campo 15,3
034200041203     C                   EVAL(H)   VABCAS = PiVal/1000
034300041103     C                   IF        vabcas > *Zeros
034400041103     C                   MOVEL     '1'           FlgCAS
034500041108     C                   EVAL      VABVCA = 'EUR'
034600041103     C                   ENDIF
034700041103     C                   ELSE
034800041103     C                   SETON                                        32
034900041103     C                   Z-ADD     *zeros        VABCAS
035000041103     C                   EVAL      vinmsg = %trimr(vinmsg)
035100041103     C                             + ' ' + 'VABCAS'
035200041103     C                   ENDIF
035300041203     C* VMD
035400041203     C                   EVAL      PiStr=%trim(%subst(vindta:415:15))
035500041203     C                   EXSR      CHKNUM
035600041203     C                   IF        PiNum=*on
035700041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
035800041203     C* riferisce ad un campo 15,3
035900041203     C                   EVAL(H)   VABVMD = PiVal/1000
036000041203     C                   IF        vabvmd > *Zeros
036100041203     C                   EVAL      VABVAD = 'EUR'
036200041203     C                   ENDIF
036300041203     C                   ELSE
036400041203     C                   SETON                                        32
036500041203     C                   Z-ADD     *zeros        VABVMD
036600041203     C                   EVAL      vinmsg = %trimr(vinmsg)
036700041203     C                             + ' ' + 'VABVMD'
036800041203     C                   ENDIF
036900041203     C* IAS
037000041203     C                   EVAL      PiStr=%trim(%subst(vindta:434:15))
037100041203     C                   EXSR      CHKNUM
037200041203     C                   IF        PiNum=*on
037300041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
037400041203     C* riferisce ad un campo 15,3
037500041203     C                   EVAL(H)   VABIAS = PiVal/1000
037600041203     C                   IF        vabias > *Zeros
037700041203     C                   EVAL      VABVAS = 'EUR'
037800041203     C                   ENDIF
037900041203     C                   ELSE
038000041203     C                   SETON                                        32
038100041203     C                   Z-ADD     *zeros        VABIAS
038200041203     C                   EVAL      vinmsg = %trimr(vinmsg)
038300041203     C                             + ' ' + 'VABIAS'
038400041203     C                   ENDIF
038500020322     C*
038600020322     C* Considerazioni finali su CBO/CAS
038700020322     C                   IF        FlgCAS = '1'
038800040713     C                   IF        VABCBO = '1'
038900020322     C                   EVAL      VABCBO = '4'
039000040713     C                   ENDIF
039100040713     C                   IF        VABCBO = '2'
039200040713     C                   EVAL      VABCBO = '6'
039300040713     C                   ENDIF
039400020322     C                   ENDIF
039500020204     C*
039600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039700020204     C                   EXSR      CHKIMPDIV
039800020204     C*
039900010607     C                   ENDSR
040000010607     C*----------------------------------------------------*
040100010601
040200020204
040300020204     C*----------------------------------------------------*
040400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040500020204     C*----------------------------------------------------*
040600020204     C     CHKIMPDIV     BEGSR
040700020204     C*
040800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040900020204     C                   Z-ADD     *zeros        wrkDec            9 9
041000020204     C*
041100020204     C* Come prima cosa effettuo considerazioni sulla divisa
041200020204     C                   IF        vabIAS > *zeros
041300020204     C                   IF        vabVAS <> 'EUR'
041400020204     C                   EVAL      vabVAS =  'ITL'
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   IF        vabCAS > *zeros
041900020204     C                   IF        vabVCA <> 'EUR'
042000020204     C                   EVAL      vabVCA =  'ITL'
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C                   IF        vabVMD > *zeros
042500020321     C                   IF        vabVAD <> 'EUR'
042600020204     C                   EVAL      vabVAD =  'ITL'
042700020204     C                   ENDIF
042800020204     C                   ENDIF
042900020204     C*
043000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043100020204     C                   Z-ADD     vabIAS        wrkDec
043200020204     C                   IF        wrkDec > *zeros
043300020204     C                   IF        vabVAS = 'ITL'
043400020204     C                   EVAL      vabIAS = *zeros
043500020204     C                   ENDIF
043600020204     C                   ENDIF
043700020204     C*
043800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043900020204     C                   Z-ADD     vabCAS        wrkDec
044000020204     C                   IF        wrkDec > *zeros
044100020204     C                   IF        vabVCA = 'ITL'
044200020204     C                   EVAL      vabCAS = *zeros
044300020204     C                   ENDIF
044400020204     C                   ENDIF
044500020204     C*
044600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044700020204     C                   Z-ADD     vabVMD        wrkDec
044800020204     C                   IF        wrkDec > *zeros
044900020204     C                   IF        vabVAD = 'ITL'
045000020204     C                   EVAL      vabVMD = *zeros
045100020204     C                   ENDIF
045200020204     C                   ENDIF
045300020204     C*
045400020204     C                   ENDSR
045500020204     C***
045600020204
045700010330
045800010330
045900010330     C*----------------------------------------------------*
046000010330     C*  CONTROLLO NUMERICITA' CAMPI
046100010330     C*----------------------------------------------------*
046200010330     C     CHKNUM        BEGSR
046300010330     C*
046400010606     C                   IF        PiDecChr = *blanks
046500010606     C                   EVAL      PiDecChr = ','
046600010606     C                   ENDIF
046700010606     C*
046800010606     C                   CALL(e)   'ISNUMERIC'
046900010330     C                   PARM                    PiStr            30
047000010606     C                   PARM                    PiDecChr          1
047100010330     C                   PARM      *ZEROS        PiVal            30 9
047200010330     C                   PARM      '0'           PiInt             1
047300010330     C                   PARM      '0'           PiNum             1
047400010330     C                   IF        %error
047500010606     C                   EVAL      PiNum=*off
047600010330     C                   ENDIF
047700010330     C*
047800010330     C                   ENDSR
047900010330     C***
048000010330
048100010601
048200010601
048300010601
048400010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048500010601     C     repfil        BEGSR
048600010601     C*
048700010601     C                   if        invfil = *zeros and
048800010601     C                             depfil > *zeros and
048900010629     C                             (vinflg = *blanks or
049000010629     C                              vinflg = *zeros)
049100010601     C
049200010601     C                   eval      invfil = depfil
049300010601     C                   endif
049400010601     C*
049500010601     C                   if        depfil <> invfil and
049600010601     C                             invfil > *zeros
049700010601     C                   eval      flgMulti = '1'
049800010601     C                   if        vinflg = *blanks
049900010601     C                   add       1             cntNonEl
050000010601     C                   endif
050100010601     C                   endif
050200010601     C*
050300010601     C                   if        vinflg = '2'
050400010601     C                   eval      flgStato = '2'
050500010601     C                   endif
050600010601     C*
050700010601     C                   ENDSR
050800010601     C***
050900010601
051000010601
051100010601
051200010330
051300010330
051400010330
051500990920      /TITLE Invio dei dati al punto operativo.
051600000613     C     invio         BEGSR
051700990920     C*
051800990920     C                   reset                   dscmz
051900010601     C                   move      invfil        cmzdst
052000990920     C                   eval      cmzfld = vlrfou
052100990920     C                   eval      cmzmbd = vlrhdl
052200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052300000710     C                   if        prmfir = *blanks
052400021014     C                   eval      cmzfla = 'FIVAB00F'
052500021014     C                   eval      cmzmba = 'FIVAB00F'
052600000710     C                   else
052700000710     C                   eval      cmzfla = prmfir
052800000710     C                   eval      cmzmba = prmfir
052900000710     C                   endif
053000990920     C                   eval      cmznrr = *zeros
053100990920     C                   move      §ctrok        cmznrr
053200021018     C                   eval      cmzlba = vlrfl1
053300990920     C                   call(e)   'TIS711C'
053400990920     C                   parm                    dscmz
053500990921     C                   parm      *blanks       esito
053600990920     C                   if        %error
053700990920     C                             or cmzerr = '1'
053800990921     C                             or esito  = '1'
053900000710     C                   eval      wrkesito = '3'
054000990920     C                   endif
054100990920     C*
054200000613     C                   ENDSR
054300990910
054400010601
054500010601
054600010601
054700010601
054800010601      /TITLE Invio dei dati al punto operativo.
054900010601     C     opeini        BEGSR
055000010601     C*
055100010601     C* Inizializzo flag e contatori operativi
055200010601     C                   movel     '0'           flgGiro           1
055300010601     C                   movel     '0'           flgMulti          1
055400010601     C                   movel     '1'           flgStato          1
055500010615     C                   movel     '0'           flgOk             1
055600010601     C                   z-add     *zeros        cntNonEl         10 0
055700010601     C                   z-add     *zeros        depfil            3 0
055800010601     C                   z-add     *zeros        invfil            3 0
055900010601     C*
056000010601     C                   ENDSR
056100010601     C***
056200010601
056300010601
056400010601
056500010330
056600010330
056700000613     C     *inzsr        BEGSR
056800990910     C*
056900990910     C     *entry        plist
057000990920     C                   parm                    tivlrds
057100990921     C                   parm      wrkesito      esito
057200000724     C                   parm                    prmlit
057300000710     C                   parm                    prmfir
057400010330     C*
057500010330     C* CALCOLA LA DATA CORRENTE
057600010330     C                   time                    wn14             14 0
057700010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057800010330     C                   z-add     wn8           g08dat
057900010330     C                   z-add     *zeros        g08inv
058000010330     C                   movel     '0'           g08err
058100010330     C                   call      'XSRDA8'
058200010330     C                   parm                    wlbda8
058300010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058400000613     C*
058500000613     C                   ENDSR
058600000613     C***
