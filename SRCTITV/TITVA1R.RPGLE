000100040301      /TITLE Upload via Internet: traduzione in FIVABWWR. (x VISTOSI ITA/EST Cod. 0592266/0592267)
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
003000010330
003100000913     C                   reset                   rrnum
003200990921     C                   reset                   esito
003300990921     C                   reset                   wrkesito
003400010601     C*
003500010601     C                   exsr      opeini
003600010605     C                   exsr      rwvab
003700010601     C*
003800010601     C                   seton                                        lr
003900010601
004000010601
004100010601
004200010601
004300010601     C*--------------------------------------------------------
004400010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004500010601     C*--------------------------------------------------------
004600010601     C     PREELA        BEGSR
004700010601     C*
004800010601     C* SE OCCORRE SPEDIRE IN FILIALE
004900010601     C                   if        invfil <> *zeros and
005000010601     C                             flgGiro = '0'
005100010601     C*
005200010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005300010601     C                   eval      flgGiro = '1'
005400010601     C*
005500010601     C                   endif
005600010601     C*
005700010601     C                   ENDSR
005800010601     C***
005900010601
006000010601
006100010601
006200010601     C*--------------------------------------------------------
006300010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006400010601     C*--------------------------------------------------------
006500010601     C     ENDELA        BEGSR
006600000616     C*
006700010601     C                   ENDSR
006800010601     C***
006900000613
007000010601
007100010601
007200010330     C*--------------------------------------------------------
007300040301     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWF              *
007400010330     C*--------------------------------------------------------
007500010605     C     RWVAB         BEGSR
007600010330     C*
007700010330     C                   if        not %open(tivin00r)
007800010330     C                   open      tivin00r
007900010330     C                   endif
008000021014     C                   if        not %open(fivabwwr)
008100021014     C                   open      fivabwwr
008200010330     C                   endif
008300010330     C*
008400010604     C                   clear                   §CTROK
008500010604     C                   clear                   §CTRMO
008600010604     C                   clear                   §CTRNO
008700010330     C*
008800010330     C                   DO        *HIVAL
008900010330     C*
009000010330     C                   READ      tivin00r                               70
009100010618     C*
009200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009300010618     C                   if        vinflg = '1'
009400010618     C                   eval      flgOk = '1'
009500010618     C                   endif
009600010618     C*
009700010330     C                   if        vindta > *blanks
009800010330     C                   add       1             rrnum
009900010330     C*
010000010601     C                   if        *in70 = *off and
010100010330     C                             (vinflg = *blanks
010200010330     C                              or vinflg = '0'
010300010330     C                              or vinflg = '2')
010400010330     C*
010500010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010600010711     C                   if        vinflg = *blanks or vinflg = '0'
010700010711     C                   clear                   vinmsg
010800010711     C                   endif
010900010601     C*
011000010330     C                   exsr      inzvar
011100010330     C                   exsr      defcam
011200010605     C                   exsr      impvab
011300010601     C*
011400010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011500010605     C                   eval      depfil = VABLNP
011600010601     C                   exsr      repfil
011700010601     C                   if        depfil = invfil
011800021025     C                   if        vlrpoi = 999
011900021025     C                   MOVE(P)   invfil        VABFGS
012000021025     C                   else
012100021025     C                   MOVE(P)   vlrpoi        VABFGS
012200021025     C                   endif
012300010601     C*
012400010601     C                   exsr      PREELA
012500010601     C*
012600010604     C* Ebbene...
012700010604     C*
012800010604     C  N31              ADD       1             §CTROK            7 0
012900010604     C   32              ADD       1             §CTRMO            7 0
013000010604     C   31              ADD       1             §CTRNO            7 0
013100020205     C*
013200040301     C* Scarico il buffer del FIVAB
013300021014     C  N31              WRITE     FIVAB000
013400010604     C*
013500020722     C                   endif
013600020722     C*
013700010604     C                   if        *in31 = *off and
013800010604     C                             *in32 = *off
013900010604     C                   eval      vinflg = '1'
014000010604     C                   else
014100010604     C                   eval      vinflg = '2'
014200010604     C                   endif
014300010604     C                   endif
014400010604     C*
014500010330     C                   else
014600010330     C                   eval      vinflg = '1'
014700010330     C                   endif
014800010601     C*
014900010601     C  N70              update    tivin000
015000010330     C*
015100010330     C  N70              ENDdo
015200010601     C
015300010601     C*
015400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015500010601     C                   if        cntNonEl = *zeros or
015600010601     C                             flgMulti = '0'
015700010330     C* Se non ci sono record con errori ...
015800010601     C                   if        §ctrno = 0 and
015900010604     C                             §ctrmo = 0 and
016000010601     C                             flgStato <> '2'
016100010330     C* ... restituisco esito OK.
016200010330     C                   eval      wrkesito = '0'
016300010330     C                   else
016400010330     C                   if        §ctrok > 0
016500010330     C                   eval      wrkesito = '1'
016600010330     C                   else
016700010615     C                   if        flgOk = '0'
016800010615     C                   eval      wrkesito = '2'
016900010615     C                   else
017000010615     C                   eval      wrkesito = '6'
017100010615     C                   endif
017200010330     C                   endif
017300010330     C                   endif
017400010601     C                   else
017500010601     C                   eval      wrkesito = '9'
017600010601     C                   endif
017700010330     C*
017800010330     C                   if        %open(tivin00r)
017900010330     C                   close     tivin00r
018000010330     C                   endif
018100021014     C                   if        %open(fivabwwr)
018200021014     C                   close     fivabwwr
018300010330     C                   endif
018400010601     C*
018500010601     C                   if        vlrpoi <> 999
018600010601     C                   eval      invfil = vlrpoi
018700010601     C                   endif
018800010330     C*
018900010330     C                   if        §ctrok > 0
019000010601     C                             and invfil > *zeros
019100010330     C                   exsr      invio
019200010330     C                   endif
019300010601     C*
019400010618     C                   if        flgGiro = '1'
019500010601     C                   exsr      endela
019600010618     C                   endif
019700010330     C*
019800010330     C                   ENDSR
019900010330     C***
020000010330
020100010601
020200010601
020300010601
020400010601
020500010330     C*----------------------------------------------------*
020600020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
020700010330     C*----------------------------------------------------*
020800010330     C     INZVAR        BEGSR
020900010330     C*
021000020204     C                   Z-ADD     *zeros        Num5_0            5 0
021100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021200020322     C                   MOVEL     '0'           FlgCAS            1
021300010330     C*
021400010330     C                   ENDSR
021500010330     C*----------------------------------------------------*
021600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021700010330     C*----------------------------------------------------*
021800010330     C     DEFCAM        BEGSR
021900010330     C*
022000020204     C* Inizializzo il buffer del record da scrivere
022100021014     C                   CLEAR                   FIVAB000
022200020204     C* Imposto i valori di default...
022300040301     C                   EVAL      VABCCM = 0592266
022400040301     C                   EVAL      VABLNP = 059
022500021014     C                   EVAL      VABCTR = 500
022600020204     C* ... e poi verifico se sono stati passati come parametri
022700020204     C                   IF        vlrppt > *blanks
022800040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
022900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023000020204     C                   EXSR      CHKNUM
023100020204     C                   IF        PiInt=*on
023200020204     C                   Z-ADD     PiVal         VABCCM
023300020204     C                   ENDIF
023400040301     C                   ENDIF
023500040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023700020204     C                   EXSR      CHKNUM
023800020204     C                   IF        PiInt=*on
023900020204     C                   Z-ADD     PiVal         VABLNP
024000020204     C                   ENDIF
024100040301     C                   ENDIF
024200040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABCTR
024700040301     C                   ENDIF
024800020204     C                   ENDIF
024900020204     C                   ENDIF
025000020204     C*
025100010330     C                   ENDSR
025200010607     C*----------------------------------------------------*
025300021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025400010607     C*----------------------------------------------------*
025500010607     C     IMPVAB        BEGSR
025600010607     C*
025700010607     C                   SETOFF                                       3132
025800010607     C*
025900010607     C* Reperimento campi ALFA
026000010607     C*
026100010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026200040301     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
026300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026500020117     C* ==
026600150603     C                   EVAL      VABRD2=%trim(%subst(vindta:406:30))
026700040301     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
026800040301     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
026900040301     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
027000040301     C                   EVAL      VABRMA=%trim(%subst(vindta:30:10))
027100040301     C                   EVAL      VABNOT=%trim(%subst(vindta:260:30))
027200040524     C                   EVAL      VABNT2=%trim(%subst(vindta:326:30))
027300040802     C*
027400040802     C                   IF        %trim(%subst(vindta:293:3)) = '007'
027500160322     C                   EVAL      VABCBO='2'
027600040802     C                   ELSE
027700040802     C                   EVAL      VABCBO='1'
027800040802     C                   ENDIF
027900010607     C*
028000010607     C* Reperimento campi NUMERICI
028100020722     C                   MOVEL     DATCOR        VABAAS
028200020722     C                   MOVE      DATCOR        VABMGS
028300020722     C*
028400040301     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
028500020304     C                   EXSR      CHKNUM
028600020304     C                   IF        PiInt=*on
028700040301     C                   Z-ADD     PiVal         VABNSP
028800040301     C                   Z-ADD     PiVal         VABRMN
028900020304     C                   ELSE
029000020722     C                   SETON                                        31
029100020304     C                   Z-ADD     *zeros        VABNSP
029200040301     C                   Z-ADD     *zeros        VABRMN
029300020304     C                   EVAL      vinmsg = %trimr(vinmsg)
029400040301     C                             + ' ' + 'VABNSP VABRMN'
029500020304     C                   ENDIF
029600020304     C*
029700040301     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
029800010607     C                   EXSR      CHKNUM
029900010607     C                   IF        PiInt=*on
030000020204     C                   Z-ADD     PiVal         Num5_0
030100020204     C                   MOVEL(p)  Num5_0        VABCAD
030200010607     C                   ELSE
030300010607     C                   SETON                                        32
030400020204     C                   EVAL      VABCAD = *zeros
030500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
030600020204     C                             + ' ' + 'VABCAD'
030700010607     C                   ENDIF
030800010607     C*
030900040301     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
031000010607     C                   EXSR      CHKNUM
031100010607     C                   IF        PiInt=*on
031200010607     C                   Z-ADD     PiVal         VABNCL
031300010607     C                   ELSE
031400010607     C                   SETON                                        32
031500010607     C                   Z-ADD     *zeros        VABNCL
031600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031700010607     C                             + ' ' + 'VABNCL'
031800010607     C                   ENDIF
031900020304     C*
032000040301     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
032100020204     C                   EVAL      PiDecChr = ','
032200010607     C                   EXSR      CHKNUM
032300010607     C                   IF        PiNum=*on
032400040301     C                   EVAL      VABPKB = PiVal/100                           * gestisco 2 dec.
032500010607     C                   ELSE
032600010607     C                   SETON                                        32
032700010607     C                   Z-ADD     *zeros        VABPKB
032800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032900010607     C                             + ' ' + 'VABPKB'
033000010607     C                   ENDIF
033100040301     C*
033200040301     C                   EVAL      PiStr=%trim(%subst(vindta:205:9))
033300040301     C                   EVAL      PiDecChr = ','
033400040301     C                   EXSR      CHKNUM
033500040301     C                   IF        PiNum=*on
033600040301     C                   EVAL      VABIAS = PiVal
033700040301     C                   EVAL      VABVAS = 'EUR'
033800040301     C                   ELSE
033900040301     C                   SETON                                        32
034000040301     C                   Z-ADD     *zeros        VABIAS
034100040301     C                   EVAL      vinmsg = %trimr(vinmsg)
034200040301     C                             + ' ' + 'VABIAS'
034300040301     C                   ENDIF
034400040301     C*
034500040524     C                   IF        %subst(vindta:290:3) = '051'
034600040301     C                   EVAL      FlgCAS = '1'
034700040301     C                   EVAL      PiStr=%trim(%subst(vindta:187:9))
034800040301     C                   EVAL      PiDecChr = ','
034900040301     C                   EXSR      CHKNUM
035000040301     C                   IF        PiNum=*on
035100040524     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 dec.
035200040301     C                   EVAL      VABVCA = 'EUR'
035300040301     C                   ELSE
035400040301     C                   SETON                                        32
035500040301     C                   Z-ADD     *zeros        VABCAS
035600040301     C                   EVAL      vinmsg = %trimr(vinmsg)
035700040301     C                             + ' ' + 'VABCAS'
035800040301     C                   ENDIF
035900040301     C                   ENDIF
036000021014     C*
036100040301     C                   EVAL      PiStr=%trim(%subst(vindta:253:7))
036200021014     C                   EVAL      PiDecChr = ','
036300021014     C                   EXSR      CHKNUM
036400021014     C                   IF        PiNum=*on
036500080715     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
036600080715     C                   Z-ADD(H)  PiVal         VABVLB
036700080715     C                   Z-ADD(H)  PiVal         VABQFT
036800021014     C                   ELSE
036900021014     C                   SETON                                        32
037000021014     C                   Z-ADD     *zeros        VABVLB
037100021023     C                   Z-ADD     *zeros        VABQFT
037200021014     C                   EVAL      vinmsg = %trimr(vinmsg)
037300021023     C                             + ' ' + 'VABVLB VABQFT'
037400021014     C                   ENDIF
037500020322     C*
037600020322     C* Considerazioni finali su CBO/CAS
037700040524     C                   IF        VABCBO = *blanks
037800040524     C                   EVAL      VABCBO = '1'
037900040524     C                   ENDIF
038000020322     C                   IF        FlgCAS = '1'
038100040524     C                   IF        VABCBO = '1'
038200020322     C                   EVAL      VABCBO = '4'
038300040524     C                   ENDIF
038400040524     C                   IF        VABCBO = '2'
038500040524     C                   EVAL      VABCBO = '6'
038600040524     C                   ENDIF
038700020322     C                   ENDIF
038800020204     C*
038900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039000020204     C                   EXSR      CHKIMPDIV
039100020204     C*
039200010607     C                   ENDSR
039300010607     C*----------------------------------------------------*
039400010601
039500020204
039600020204     C*----------------------------------------------------*
039700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039800020204     C*----------------------------------------------------*
039900020204     C     CHKIMPDIV     BEGSR
040000020204     C*
040100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
040200020204     C                   Z-ADD     *zeros        wrkDec            9 9
040300020204     C*
040400020204     C* Come prima cosa effettuo considerazioni sulla divisa
040500020204     C                   IF        vabIAS > *zeros
040600020204     C                   IF        vabVAS <> 'EUR'
040700020204     C                   EVAL      vabVAS =  'ITL'
040800020204     C                   ENDIF
040900020204     C                   ENDIF
041000020204     C*
041100020204     C                   IF        vabCAS > *zeros
041200020204     C                   IF        vabVCA <> 'EUR'
041300020204     C                   EVAL      vabVCA =  'ITL'
041400020204     C                   ENDIF
041500020204     C                   ENDIF
041600020204     C*
041700020204     C                   IF        vabVMD > *zeros
041800020321     C                   IF        vabVAD <> 'EUR'
041900020204     C                   EVAL      vabVAD =  'ITL'
042000020204     C                   ENDIF
042100020204     C                   ENDIF
042200020204     C*
042300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042400020204     C                   Z-ADD     vabIAS        wrkDec
042500020204     C                   IF        wrkDec > *zeros
042600020204     C                   IF        vabVAS = 'ITL'
042700020204     C                   EVAL      vabIAS = *zeros
042800020204     C                   ENDIF
042900020204     C                   ENDIF
043000020204     C*
043100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
043200020204     C                   Z-ADD     vabCAS        wrkDec
043300020204     C                   IF        wrkDec > *zeros
043400020204     C                   IF        vabVCA = 'ITL'
043500020204     C                   EVAL      vabCAS = *zeros
043600020204     C                   ENDIF
043700020204     C                   ENDIF
043800020204     C*
043900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044000020204     C                   Z-ADD     vabVMD        wrkDec
044100020204     C                   IF        wrkDec > *zeros
044200020204     C                   IF        vabVAD = 'ITL'
044300020204     C                   EVAL      vabVMD = *zeros
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C                   ENDSR
044800020204     C***
044900020204
045000010330
045100010330
045200010330     C*----------------------------------------------------*
045300010330     C*  CONTROLLO NUMERICITA' CAMPI
045400010330     C*----------------------------------------------------*
045500010330     C     CHKNUM        BEGSR
045600010330     C*
045700010606     C                   IF        PiDecChr = *blanks
045800010606     C                   EVAL      PiDecChr = ','
045900010606     C                   ENDIF
046000010606     C*
046100010606     C                   CALL(e)   'ISNUMERIC'
046200010330     C                   PARM                    PiStr            30
046300010606     C                   PARM                    PiDecChr          1
046400010330     C                   PARM      *ZEROS        PiVal            30 9
046500010330     C                   PARM      '0'           PiInt             1
046600010330     C                   PARM      '0'           PiNum             1
046700010330     C                   IF        %error
046800010606     C                   EVAL      PiNum=*off
046900010330     C                   ENDIF
047000010330     C*
047100010330     C                   ENDSR
047200010330     C***
047300010330
047400010601
047500010601
047600010601
047700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047800010601     C     repfil        BEGSR
047900010601     C*
048000010601     C                   if        invfil = *zeros and
048100010601     C                             depfil > *zeros and
048200010629     C                             (vinflg = *blanks or
048300010629     C                              vinflg = *zeros)
048400010601     C
048500010601     C                   eval      invfil = depfil
048600010601     C                   endif
048700010601     C*
048800010601     C                   if        depfil <> invfil and
048900010601     C                             invfil > *zeros
049000010601     C                   eval      flgMulti = '1'
049100010601     C                   if        vinflg = *blanks
049200010601     C                   add       1             cntNonEl
049300010601     C                   endif
049400010601     C                   endif
049500010601     C*
049600010601     C                   if        vinflg = '2'
049700010601     C                   eval      flgStato = '2'
049800010601     C                   endif
049900010601     C*
050000010601     C                   ENDSR
050100010601     C***
050200010601
050300010601
050400010601
050500010330
050600010330
050700010330
050800990920      /TITLE Invio dei dati al punto operativo.
050900000613     C     invio         BEGSR
051000990920     C*
051100990920     C                   reset                   dscmz
051200010601     C                   move      invfil        cmzdst
051300990920     C                   eval      cmzfld = vlrfou
051400990920     C                   eval      cmzmbd = vlrhdl
051500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051600000710     C                   if        prmfir = *blanks
051700021014     C                   eval      cmzfla = 'FIVAB00F'
051800021014     C                   eval      cmzmba = 'FIVAB00F'
051900000710     C                   else
052000000710     C                   eval      cmzfla = prmfir
052100000710     C                   eval      cmzmba = prmfir
052200000710     C                   endif
052300990920     C                   eval      cmznrr = *zeros
052400990920     C                   move      §ctrok        cmznrr
052500021018     C                   eval      cmzlba = vlrfl1
052600990920     C                   call(e)   'TIS711C'
052700990920     C                   parm                    dscmz
052800990921     C                   parm      *blanks       esito
052900990920     C                   if        %error
053000990920     C                             or cmzerr = '1'
053100990921     C                             or esito  = '1'
053200000710     C                   eval      wrkesito = '3'
053300990920     C                   endif
053400990920     C*
053500000613     C                   ENDSR
053600990910
053700010601
053800010601
053900010601
054000010601
054100010601      /TITLE Invio dei dati al punto operativo.
054200010601     C     opeini        BEGSR
054300010601     C*
054400010601     C* Inizializzo flag e contatori operativi
054500010601     C                   movel     '0'           flgGiro           1
054600010601     C                   movel     '0'           flgMulti          1
054700010601     C                   movel     '1'           flgStato          1
054800010615     C                   movel     '0'           flgOk             1
054900010601     C                   z-add     *zeros        cntNonEl         10 0
055000010601     C                   z-add     *zeros        depfil            3 0
055100010601     C                   z-add     *zeros        invfil            3 0
055200010601     C*
055300010601     C                   ENDSR
055400010601     C***
055500010601
055600010601
055700010601
055800010330
055900010330
056000000613     C     *inzsr        BEGSR
056100990910     C*
056200990910     C     *entry        plist
056300990920     C                   parm                    tivlrds
056400990921     C                   parm      wrkesito      esito
056500000724     C                   parm                    prmlit
056600000710     C                   parm                    prmfir
056700010330     C*
056800010330     C* CALCOLA LA DATA CORRENTE
056900010330     C                   time                    wn14             14 0
057000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057100010330     C                   z-add     wn8           g08dat
057200010330     C                   z-add     *zeros        g08inv
057300010330     C                   movel     '0'           g08err
057400010330     C                   call      'XSRDA8'
057500010330     C                   parm                    wlbda8
057600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057700000613     C*
057800000613     C                   ENDSR
057900000613     C***
