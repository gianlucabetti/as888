000100061003      /TITLE Upload via Internet: traduzione in FIVABWWR. (x MOTORQUALITY Cod. 2021491)
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
011000060901     C                   exsr      inzvar
011100060901     C                   exsr      defcam
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
022300061003     C                   EVAL      VABCCM = 2021491
022400060901     C                   EVAL      VABLNP = 202
022500060901     C                   EVAL      VABCTR = 000
022600060901     C                   EVAL      VABTSP = 'C'
022700061003     C                   EVAL      VABCBO = '1'
022800020204     C* ... e poi verifico se sono stati passati come parametri
022900020204     C                   IF        vlrppt > *blanks
023000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023200020204     C                   EXSR      CHKNUM
023300020204     C                   IF        PiInt=*on
023400020204     C                   Z-ADD     PiVal         VABCCM
023500020204     C                   ENDIF
023600040301     C                   ENDIF
023700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
023800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
023900020204     C                   EXSR      CHKNUM
024000020204     C                   IF        PiInt=*on
024100020204     C                   Z-ADD     PiVal         VABLNP
024200020204     C                   ENDIF
024300040301     C                   ENDIF
024400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024600020204     C                   EXSR      CHKNUM
024700020204     C                   IF        PiInt=*on
024800020204     C                   Z-ADD     PiVal         VABCTR
024900040301     C                   ENDIF
025000020204     C                   ENDIF
025100020204     C                   ENDIF
025200020204     C*
025300010330     C                   ENDSR
025400010607     C*----------------------------------------------------*
025500021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025600010607     C*----------------------------------------------------*
025700010607     C     IMPVAB        BEGSR
025800010607     C*
025900010607     C                   SETOFF                                       3132
026000060720     C*
026100060720     C* Reperimento campi NUMERICI
026200060720     C                   MOVEL     DATCOR        VABAAS
026300060720     C                   MOVE      DATCOR        VABMGS
026400070110     C                   clear                   PiDecChr
026500010607     C*
026600010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026700061003     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
026800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027000020117     C* ==
027100061003     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
027200061003     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
027300061003     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
027400061003     C                   EVAL      VABNOT=%trim(%subst(vindta:199:35))
027500061003     C                   EVAL      VABNT2=%trim(%subst(vindta:199+35:80-35))
027600061017     C                   EVAL      VABRMA=%trim(%subst(vindta:191:6))
027700061003     C                   IF        %trim(%subst(vindta:197:2)) = '01'
027800061003     C                   EVAL      VABCBO='2'
027900061003     C                   ELSE
028000061003     C                   EVAL      VABCBO='1'
028100061003     C                   ENDIF
028200090408     C* CCM
028300090408     C                   IF        %subst(vindta:1:11) <> *blanks AND
028400090408     C                             %subst(vindta:1:11) <> *all'0'
028500090408     C                   EVAL      PiStr=%trim(%subst(vindta:1:11))
028600090408     C                   EXSR      CHKNUM
028700090408     C                   IF        PiInt=*on
028800090408     C                   Z-ADD     PiVal         VABCCM
028900090408     C                   ELSE
029000090408     C                   SETON                                        31
029100090408     C                   Z-ADD     *zeros        VABCCM
029200090408     C                   EVAL      vinmsg = %trimr(vinmsg)
029300090408     C                             + ' ' + 'VABCCM'
029400090408     C                   ENDIF
029500090408     C                   ENDIF
029600060720     C* CAD
029700061003     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
029800060720     C                   EXSR      CHKNUM
029900060720     C                   IF        PiInt=*on
030000060720     C                   Z-ADD     PiVal         Num5_0
030100060720     C                   MOVEL(p)  Num5_0        VABCAD
030200060720     C                   ELSE
030300060720     C                   SETON                                        32
030400060720     C                   EVAL      VABCAD = *zeros
030500060720     C                   EVAL      vinmsg = %trimr(vinmsg)
030600060720     C                             + ' ' + 'VABCAD'
030700060720     C                   ENDIF
030800040420     C* NSP/RMN
030900061003     C                   EVAL      PiStr=%trim(%subst(vindta:191:6))
031000020304     C                   EXSR      CHKNUM
031100020304     C                   IF        PiInt=*on
031200040301     C                   Z-ADD     PiVal         VABNSP
031300040301     C                   Z-ADD     PiVal         VABRMN
031400020304     C                   ELSE
031500020722     C                   SETON                                        31
031600020304     C                   Z-ADD     *zeros        VABNSP
031700040301     C                   Z-ADD     *zeros        VABRMN
031800020304     C                   EVAL      vinmsg = %trimr(vinmsg)
031900040301     C                             + ' ' + 'VABNSP VABRMN'
032000020304     C                   ENDIF
032100040420     C* NCL
032200061003     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
032300010607     C                   EXSR      CHKNUM
032400010607     C                   IF        PiInt=*on
032500010607     C                   Z-ADD     PiVal         VABNCL
032600010607     C                   ELSE
032700010607     C                   SETON                                        32
032800010607     C                   Z-ADD     *zeros        VABNCL
032900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033000010607     C                             + ' ' + 'VABNCL'
033100010607     C                   ENDIF
033200040420     C* PKB
033300061003     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
033400010607     C                   EXSR      CHKNUM
033500010607     C                   IF        PiNum=*on
033600040420     C                   Z-ADD     PiVal         VABPKB
033700010607     C                   ELSE
033800010607     C                   SETON                                        32
033900010607     C                   Z-ADD     *zeros        VABPKB
034000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034100010607     C                             + ' ' + 'VABPKB'
034200010607     C                   ENDIF
034300060901     C* CAS
034400061003     C                   EVAL      PiStr=%trim(%subst(vindta:279:11))
034500081113     C                   EVAL      PiDecChr = '.'
034600061003     C                   EXSR      CHKNUM
034700060720     C                   IF        PiNum=*on
034800060901     C                   Z-ADD     PiVal         VABCAS
034900061003     C                   IF        VABCAS > 0
035000061003     C                   EVAL      FlgCAS = '1'
035100061003     C                   EVAL      VABVCA =  'EUR'
035200061003     C                   ENDIF
035300060720     C                   ELSE
035400060720     C                   SETON                                        32
035500060901     C                   Z-ADD     *zeros        VABCAS
035600060720     C                   EVAL      vinmsg = %trimr(vinmsg)
035700060901     C                             + ' ' + 'VABCAS'
035800060720     C                   ENDIF
035900061003     C* IAS
036000061003     C                   EVAL      PiStr=%trim(%subst(vindta:292:11))
036100081113     C                   EVAL      PiDecChr = '.'
036200061003     C                   EXSR      CHKNUM
036300061003     C                   IF        PiNum=*on
036400061003     C                   Z-ADD     PiVal         VABIAS
036500061003     C                   IF        VABIAS > 0
036600061003     C                   EVAL      VABVAS =  'EUR'
036700061003     C                   ENDIF
036800061003     C                   ELSE
036900061003     C                   SETON                                        32
037000061003     C                   Z-ADD     *zeros        VABIAS
037100061003     C                   EVAL      vinmsg = %trimr(vinmsg)
037200061003     C                             + ' ' + 'VABIAS'
037300061003     C                   ENDIF
037400020322     C*
037500020322     C* Considerazioni finali su CBO/CAS
037600020322     C                   IF        FlgCAS = '1'
037700040713     C                   IF        VABCBO = '1'
037800020322     C                   EVAL      VABCBO = '4'
037900040713     C                   ENDIF
038000040713     C                   IF        VABCBO = '2'
038100040713     C                   EVAL      VABCBO = '6'
038200040713     C                   ENDIF
038300020322     C                   ENDIF
038400020204     C*
038500020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038600020204     C                   EXSR      CHKIMPDIV
038700020204     C*
038800010607     C                   ENDSR
038900010607     C*----------------------------------------------------*
039000010601
039100020204
039200020204     C*----------------------------------------------------*
039300020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039400020204     C*----------------------------------------------------*
039500020204     C     CHKIMPDIV     BEGSR
039600020204     C*
039700020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039800020204     C                   Z-ADD     *zeros        wrkDec            9 9
039900020204     C*
040000020204     C* Come prima cosa effettuo considerazioni sulla divisa
040100020204     C                   IF        vabIAS > *zeros
040200020204     C                   IF        vabVAS <> 'EUR'
040300020204     C                   EVAL      vabVAS =  'ITL'
040400020204     C                   ENDIF
040500020204     C                   ENDIF
040600020204     C*
040700020204     C                   IF        vabCAS > *zeros
040800020204     C                   IF        vabVCA <> 'EUR'
040900020204     C                   EVAL      vabVCA =  'ITL'
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C                   IF        vabVMD > *zeros
041400020321     C                   IF        vabVAD <> 'EUR'
041500020204     C                   EVAL      vabVAD =  'ITL'
041600020204     C                   ENDIF
041700020204     C                   ENDIF
041800020204     C*
041900020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
042000020204     C                   Z-ADD     vabIAS        wrkDec
042100020204     C                   IF        wrkDec > *zeros
042200020204     C                   IF        vabVAS = 'ITL'
042300020204     C                   EVAL      vabIAS = *zeros
042400020204     C                   ENDIF
042500020204     C                   ENDIF
042600020204     C*
042700020204     C* Stabilisco se il contrasegno ha decimali valorizzati
042800020204     C                   Z-ADD     vabCAS        wrkDec
042900020204     C                   IF        wrkDec > *zeros
043000020204     C                   IF        vabVCA = 'ITL'
043100020204     C                   EVAL      vabCAS = *zeros
043200020204     C                   ENDIF
043300020204     C                   ENDIF
043400020204     C*
043500020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043600020204     C                   Z-ADD     vabVMD        wrkDec
043700020204     C                   IF        wrkDec > *zeros
043800020204     C                   IF        vabVAD = 'ITL'
043900020204     C                   EVAL      vabVMD = *zeros
044000020204     C                   ENDIF
044100020204     C                   ENDIF
044200020204     C*
044300020204     C                   ENDSR
044400020204     C***
044500020204
044600010330
044700010330
044800010330     C*----------------------------------------------------*
044900010330     C*  CONTROLLO NUMERICITA' CAMPI
045000010330     C*----------------------------------------------------*
045100010330     C     CHKNUM        BEGSR
045200010330     C*
045300010606     C                   IF        PiDecChr = *blanks
045400061003     C                   EVAL      PiDecChr = '.'
045500010606     C                   ENDIF
045600010606     C*
045700010606     C                   CALL(e)   'ISNUMERIC'
045800010330     C                   PARM                    PiStr            30
045900010606     C                   PARM                    PiDecChr          1
046000010330     C                   PARM      *ZEROS        PiVal            30 9
046100010330     C                   PARM      '0'           PiInt             1
046200010330     C                   PARM      '0'           PiNum             1
046300010330     C                   IF        %error
046400010606     C                   EVAL      PiNum=*off
046500010330     C                   ENDIF
046600010330     C*
046700010330     C                   ENDSR
046800010330     C***
046900010330
047000010601
047100010601
047200010601
047300010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047400010601     C     repfil        BEGSR
047500010601     C*
047600010601     C                   if        invfil = *zeros and
047700010601     C                             depfil > *zeros and
047800010629     C                             (vinflg = *blanks or
047900010629     C                              vinflg = *zeros)
048000010601     C
048100010601     C                   eval      invfil = depfil
048200010601     C                   endif
048300010601     C*
048400010601     C                   if        depfil <> invfil and
048500010601     C                             invfil > *zeros
048600010601     C                   eval      flgMulti = '1'
048700010601     C                   if        vinflg = *blanks
048800010601     C                   add       1             cntNonEl
048900010601     C                   endif
049000010601     C                   endif
049100010601     C*
049200010601     C                   if        vinflg = '2'
049300010601     C                   eval      flgStato = '2'
049400010601     C                   endif
049500010601     C*
049600010601     C                   ENDSR
049700010601     C***
049800010601
049900010601
050000010601
050100010330
050200010330
050300010330
050400990920      /TITLE Invio dei dati al punto operativo.
050500000613     C     invio         BEGSR
050600990920     C*
050700990920     C                   reset                   dscmz
050800010601     C                   move      invfil        cmzdst
050900990920     C                   eval      cmzfld = vlrfou
051000990920     C                   eval      cmzmbd = vlrhdl
051100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
051200000710     C                   if        prmfir = *blanks
051300021014     C                   eval      cmzfla = 'FIVAB00F'
051400021014     C                   eval      cmzmba = 'FIVAB00F'
051500000710     C                   else
051600000710     C                   eval      cmzfla = prmfir
051700000710     C                   eval      cmzmba = prmfir
051800000710     C                   endif
051900990920     C                   eval      cmznrr = *zeros
052000990920     C                   move      §ctrok        cmznrr
052100021018     C                   eval      cmzlba = vlrfl1
052200990920     C                   call(e)   'TIS711C'
052300990920     C                   parm                    dscmz
052400990921     C                   parm      *blanks       esito
052500990920     C                   if        %error
052600990920     C                             or cmzerr = '1'
052700990921     C                             or esito  = '1'
052800000710     C                   eval      wrkesito = '3'
052900990920     C                   endif
053000990920     C*
053100000613     C                   ENDSR
053200990910
053300010601
053400010601
053500010601
053600010601
053700010601      /TITLE Invio dei dati al punto operativo.
053800010601     C     opeini        BEGSR
053900010601     C*
054000010601     C* Inizializzo flag e contatori operativi
054100010601     C                   movel     '0'           flgGiro           1
054200010601     C                   movel     '0'           flgMulti          1
054300010601     C                   movel     '1'           flgStato          1
054400010615     C                   movel     '0'           flgOk             1
054500010601     C                   z-add     *zeros        cntNonEl         10 0
054600010601     C                   z-add     *zeros        depfil            3 0
054700010601     C                   z-add     *zeros        invfil            3 0
054800010601     C*
054900010601     C                   ENDSR
055000010601     C***
055100010601
055200010601
055300010601
055400010330
055500010330
055600000613     C     *inzsr        BEGSR
055700990910     C*
055800990910     C     *entry        plist
055900990920     C                   parm                    tivlrds
056000990921     C                   parm      wrkesito      esito
056100000724     C                   parm                    prmlit
056200000710     C                   parm                    prmfir
056300010330     C*
056400010330     C* CALCOLA LA DATA CORRENTE
056500010330     C                   time                    wn14             14 0
056600010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056700010330     C                   z-add     wn8           g08dat
056800010330     C                   z-add     *zeros        g08inv
056900010330     C                   movel     '0'           g08err
057000010330     C                   call      'XSRDA8'
057100010330     C                   parm                    wlbda8
057200010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057300000613     C*
057400000613     C                   ENDSR
057500000613     C***
