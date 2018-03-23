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
027700010607     C*
027800010607     C* Reperimento campi NUMERICI
027900020722     C                   MOVEL     DATCOR        VABAAS
028000020722     C                   MOVE      DATCOR        VABMGS
028100040420     C* NSP/RMN
028200041203     C                   EVAL      PiStr=%trim(%subst(vindta:89:6))
028300020304     C                   EXSR      CHKNUM
028400020304     C                   IF        PiInt=*on
028500040301     C                   Z-ADD     PiVal         VABNSP
028600040301     C                   Z-ADD     PiVal         VABRMN
028700020304     C                   ELSE
028800020722     C                   SETON                                        31
028900020304     C                   Z-ADD     *zeros        VABNSP
029000040301     C                   Z-ADD     *zeros        VABRMN
029100020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029200040301     C                             + ' ' + 'VABNSP VABRMN'
029300020304     C                   ENDIF
029400040420     C* CAD
029500041203     C                   EVAL      PiStr=%trim(%subst(vindta:316:5))
029600010607     C                   EXSR      CHKNUM
029700010607     C                   IF        PiInt=*on
029800020204     C                   Z-ADD     PiVal         Num5_0
029900020204     C                   MOVEL(p)  Num5_0        VABCAD
030000010607     C                   ELSE
030100010607     C                   SETON                                        32
030200020204     C                   EVAL      VABCAD = *zeros
030300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
030400020204     C                             + ' ' + 'VABCAD'
030500010607     C                   ENDIF
030600040420     C* NCL
030700041203     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
030800010607     C                   EXSR      CHKNUM
030900010607     C                   IF        PiInt=*on
031000010607     C                   Z-ADD     PiVal         VABNCL
031100010607     C                   ELSE
031200010607     C                   SETON                                        32
031300010607     C                   Z-ADD     *zeros        VABNCL
031400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031500010607     C                             + ' ' + 'VABNCL'
031600010607     C                   ENDIF
031700040420     C* PKB
031800041203     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
031900010607     C                   EXSR      CHKNUM
032000010607     C                   IF        PiNum=*on
032100041203     C* Divido x 100 perchè il campo di input non gestisce separatori decimali ma il valore si
032200041203     C* riferisce ad un campo 7,2
032300041203     C                   EVAL(H)   VABPKB = PiVal/100
032400010607     C                   ELSE
032500010607     C                   SETON                                        32
032600010607     C                   Z-ADD     *zeros        VABPKB
032700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032800010607     C                             + ' ' + 'VABPKB'
032900010607     C                   ENDIF
033000041103     C* CAS
033100041203     C                   EVAL      PiStr=%trim(%subst(vindta:396:15))
033200041103     C                   EXSR      CHKNUM
033300041103     C                   IF        PiNum=*on
033400041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
033500041203     C* riferisce ad un campo 15,3
033600041203     C                   EVAL(H)   VABCAS = PiVal/1000
033700041103     C                   IF        vabcas > *Zeros
033800041103     C                   MOVEL     '1'           FlgCAS
033900041108     C                   EVAL      VABVCA = 'EUR'
034000041103     C                   ENDIF
034100041103     C                   ELSE
034200041103     C                   SETON                                        32
034300041103     C                   Z-ADD     *zeros        VABCAS
034400041103     C                   EVAL      vinmsg = %trimr(vinmsg)
034500041103     C                             + ' ' + 'VABCAS'
034600041103     C                   ENDIF
034700041203     C* VMD
034800041203     C                   EVAL      PiStr=%trim(%subst(vindta:415:15))
034900041203     C                   EXSR      CHKNUM
035000041203     C                   IF        PiNum=*on
035100041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
035200041203     C* riferisce ad un campo 15,3
035300041203     C                   EVAL(H)   VABVMD = PiVal/1000
035400041203     C                   IF        vabvmd > *Zeros
035500041203     C                   EVAL      VABVAD = 'EUR'
035600041203     C                   ENDIF
035700041203     C                   ELSE
035800041203     C                   SETON                                        32
035900041203     C                   Z-ADD     *zeros        VABVMD
036000041203     C                   EVAL      vinmsg = %trimr(vinmsg)
036100041203     C                             + ' ' + 'VABVMD'
036200041203     C                   ENDIF
036300041203     C* IAS
036400041203     C                   EVAL      PiStr=%trim(%subst(vindta:434:15))
036500041203     C                   EXSR      CHKNUM
036600041203     C                   IF        PiNum=*on
036700041203     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
036800041203     C* riferisce ad un campo 15,3
036900041203     C                   EVAL(H)   VABIAS = PiVal/1000
037000041203     C                   IF        vabias > *Zeros
037100041203     C                   EVAL      VABVAS = 'EUR'
037200041203     C                   ENDIF
037300041203     C                   ELSE
037400041203     C                   SETON                                        32
037500041203     C                   Z-ADD     *zeros        VABIAS
037600041203     C                   EVAL      vinmsg = %trimr(vinmsg)
037700041203     C                             + ' ' + 'VABIAS'
037800041203     C                   ENDIF
037900020322     C*
038000020322     C* Considerazioni finali su CBO/CAS
038100020322     C                   IF        FlgCAS = '1'
038200040713     C                   IF        VABCBO = '1'
038300020322     C                   EVAL      VABCBO = '4'
038400040713     C                   ENDIF
038500040713     C                   IF        VABCBO = '2'
038600040713     C                   EVAL      VABCBO = '6'
038700040713     C                   ENDIF
038800020322     C                   ENDIF
038900020204     C*
039000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039100020204     C                   EXSR      CHKIMPDIV
039200020204     C*
039300010607     C                   ENDSR
039400010607     C*----------------------------------------------------*
039500010601
039600020204
039700020204     C*----------------------------------------------------*
039800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039900020204     C*----------------------------------------------------*
040000020204     C     CHKIMPDIV     BEGSR
040100020204     C*
040200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040300020204     C                   Z-ADD     *zeros        wrkDec            9 9
040400020204     C*
040500020204     C* Come prima cosa effettuo considerazioni sulla divisa
040600020204     C                   IF        vabIAS > *zeros
040700020204     C                   IF        vabVAS <> 'EUR'
040800020204     C                   EVAL      vabVAS =  'ITL'
040900020204     C                   ENDIF
041000020204     C                   ENDIF
041100020204     C*
041200020204     C                   IF        vabCAS > *zeros
041300020204     C                   IF        vabVCA <> 'EUR'
041400020204     C                   EVAL      vabVCA =  'ITL'
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   IF        vabVMD > *zeros
041900020321     C                   IF        vabVAD <> 'EUR'
042000020204     C                   EVAL      vabVAD =  'ITL'
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042500020204     C                   Z-ADD     vabIAS        wrkDec
042600020204     C                   IF        wrkDec > *zeros
042700020204     C                   IF        vabVAS = 'ITL'
042800020204     C                   EVAL      vabIAS = *zeros
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043300020204     C                   Z-ADD     vabCAS        wrkDec
043400020204     C                   IF        wrkDec > *zeros
043500020204     C                   IF        vabVCA = 'ITL'
043600020204     C                   EVAL      vabCAS = *zeros
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044100020204     C                   Z-ADD     vabVMD        wrkDec
044200020204     C                   IF        wrkDec > *zeros
044300020204     C                   IF        vabVAD = 'ITL'
044400020204     C                   EVAL      vabVMD = *zeros
044500020204     C                   ENDIF
044600020204     C                   ENDIF
044700020204     C*
044800020204     C                   ENDSR
044900020204     C***
045000020204
045100010330
045200010330
045300010330     C*----------------------------------------------------*
045400010330     C*  CONTROLLO NUMERICITA' CAMPI
045500010330     C*----------------------------------------------------*
045600010330     C     CHKNUM        BEGSR
045700010330     C*
045800010606     C                   IF        PiDecChr = *blanks
045900010606     C                   EVAL      PiDecChr = ','
046000010606     C                   ENDIF
046100010606     C*
046200010606     C                   CALL(e)   'ISNUMERIC'
046300010330     C                   PARM                    PiStr            30
046400010606     C                   PARM                    PiDecChr          1
046500010330     C                   PARM      *ZEROS        PiVal            30 9
046600010330     C                   PARM      '0'           PiInt             1
046700010330     C                   PARM      '0'           PiNum             1
046800010330     C                   IF        %error
046900010606     C                   EVAL      PiNum=*off
047000010330     C                   ENDIF
047100010330     C*
047200010330     C                   ENDSR
047300010330     C***
047400010330
047500010601
047600010601
047700010601
047800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047900010601     C     repfil        BEGSR
048000010601     C*
048100010601     C                   if        invfil = *zeros and
048200010601     C                             depfil > *zeros and
048300010629     C                             (vinflg = *blanks or
048400010629     C                              vinflg = *zeros)
048500010601     C
048600010601     C                   eval      invfil = depfil
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        depfil <> invfil and
049000010601     C                             invfil > *zeros
049100010601     C                   eval      flgMulti = '1'
049200010601     C                   if        vinflg = *blanks
049300010601     C                   add       1             cntNonEl
049400010601     C                   endif
049500010601     C                   endif
049600010601     C*
049700010601     C                   if        vinflg = '2'
049800010601     C                   eval      flgStato = '2'
049900010601     C                   endif
050000010601     C*
050100010601     C                   ENDSR
050200010601     C***
050300010601
050400010601
050500010601
050600010330
050700010330
050800010330
050900990920      /TITLE Invio dei dati al punto operativo.
051000000613     C     invio         BEGSR
051100990920     C*
051200990920     C                   reset                   dscmz
051300010601     C                   move      invfil        cmzdst
051400990920     C                   eval      cmzfld = vlrfou
051500990920     C                   eval      cmzmbd = vlrhdl
051600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051700000710     C                   if        prmfir = *blanks
051800021014     C                   eval      cmzfla = 'FIVAB00F'
051900021014     C                   eval      cmzmba = 'FIVAB00F'
052000000710     C                   else
052100000710     C                   eval      cmzfla = prmfir
052200000710     C                   eval      cmzmba = prmfir
052300000710     C                   endif
052400990920     C                   eval      cmznrr = *zeros
052500990920     C                   move      §ctrok        cmznrr
052600021018     C                   eval      cmzlba = vlrfl1
052700990920     C                   call(e)   'TIS711C'
052800990920     C                   parm                    dscmz
052900990921     C                   parm      *blanks       esito
053000990920     C                   if        %error
053100990920     C                             or cmzerr = '1'
053200990921     C                             or esito  = '1'
053300000710     C                   eval      wrkesito = '3'
053400990920     C                   endif
053500990920     C*
053600000613     C                   ENDSR
053700990910
053800010601
053900010601
054000010601
054100010601
054200010601      /TITLE Invio dei dati al punto operativo.
054300010601     C     opeini        BEGSR
054400010601     C*
054500010601     C* Inizializzo flag e contatori operativi
054600010601     C                   movel     '0'           flgGiro           1
054700010601     C                   movel     '0'           flgMulti          1
054800010601     C                   movel     '1'           flgStato          1
054900010615     C                   movel     '0'           flgOk             1
055000010601     C                   z-add     *zeros        cntNonEl         10 0
055100010601     C                   z-add     *zeros        depfil            3 0
055200010601     C                   z-add     *zeros        invfil            3 0
055300010601     C*
055400010601     C                   ENDSR
055500010601     C***
055600010601
055700010601
055800010601
055900010330
056000010330
056100000613     C     *inzsr        BEGSR
056200990910     C*
056300990910     C     *entry        plist
056400990920     C                   parm                    tivlrds
056500990921     C                   parm      wrkesito      esito
056600000724     C                   parm                    prmlit
056700000710     C                   parm                    prmfir
056800010330     C*
056900010330     C* CALCOLA LA DATA CORRENTE
057000010330     C                   time                    wn14             14 0
057100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057200010330     C                   z-add     wn8           g08dat
057300010330     C                   z-add     *zeros        g08inv
057400010330     C                   movel     '0'           g08err
057500010330     C                   call      'XSRDA8'
057600010330     C                   parm                    wlbda8
057700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057800000613     C*
057900000613     C                   ENDSR
058000000613     C***
