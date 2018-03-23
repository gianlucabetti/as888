000100051221      /TITLE Upload via Internet: traduzione in FIVABWWR. (x MORGANTI Cod. 2022090)
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
022300051221     C                   IF        datcor < 20060109
022400040526     C                   EVAL      VABCCM = 2490615
022500040526     C                   EVAL      VABLNP = 249
022600051221     C                   ELSE
022700051221     C                   EVAL      VABCCM = 2022090
022800051221     C                   EVAL      VABLNP = 202
022900051221     C                   ENDIF
023000100412     C                   EVAL      VABCTM = '7A'
023100040420     C                   EVAL      VABCTR = 000
023200040526     C                   EVAL      VABCBO = '1'
023300020204     C* ... e poi verifico se sono stati passati come parametri
023400020204     C                   IF        vlrppt > *blanks
023500040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023600020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023700020204     C                   EXSR      CHKNUM
023800020204     C                   IF        PiInt=*on
023900020204     C                   Z-ADD     PiVal         VABCCM
024000020204     C                   ENDIF
024100040301     C                   ENDIF
024200040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024400020204     C                   EXSR      CHKNUM
024500020204     C                   IF        PiInt=*on
024600020204     C                   Z-ADD     PiVal         VABLNP
024700020204     C                   ENDIF
024800040301     C                   ENDIF
024900040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025100020204     C                   EXSR      CHKNUM
025200020204     C                   IF        PiInt=*on
025300020204     C                   Z-ADD     PiVal         VABCTR
025400040301     C                   ENDIF
025500020204     C                   ENDIF
025600020204     C                   ENDIF
025700020204     C*
025800010330     C                   ENDSR
025900010607     C*----------------------------------------------------*
026000021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026100010607     C*----------------------------------------------------*
026200010607     C     IMPVAB        BEGSR
026300010607     C*
026400010607     C                   SETOFF                                       3132
026500010607     C*
026600010607     C* Reperimento campi ALFA
026700010607     C*
026800010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026900040526     C                   EVAL      VABRSD=%trim(%subst(vindta:15:30))
027000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027200020117     C* ==
027300040526     C                   EVAL      VABIND=%trim(%subst(vindta:45:30))
027400040526     C                   EVAL      VABLOD=%trim(%subst(vindta:80:20))
027500040526     C                   EVAL      VABPRD=%trim(%subst(vindta:105:2))
027600060526     C***                EVAL      VABRMA=%trim(%subst(vindta:6:9))
027700060526     C                   EVAL      VABRMA=%trim(%subst(vindta:122:5))
027800010607     C*
027900010607     C* Reperimento campi NUMERICI
028000020722     C                   MOVEL     DATCOR        VABAAS
028100020722     C                   MOVE      DATCOR        VABMGS
028200060714     C* CCM
028300060714     C                   EVAL      PiStr=%trim(%subst(vindta:169:7))
028400060714     C                   EXSR      CHKNUM
028500060714     C                   IF        PiInt=*on
028600060714     C                   Z-ADD     PiVal         VABCCM
028700060714     C                   ELSE
028800060714     C                   SETON                                        31
028900060714     C                   Z-ADD     *zeros        VABCCM
029000060714     C                   EVAL      vinmsg = %trimr(vinmsg)
029100060714     C                             + ' ' + 'VABCCM'
029200060714     C                   ENDIF
029300040420     C* NSP/RMN
029400060526     C***                EVAL      PiStr=%trim(%subst(vindta:6:9))
029500060526     C                   EVAL      PiStr=%trim(%subst(vindta:8:7))
029600020304     C                   EXSR      CHKNUM
029700020304     C                   IF        PiInt=*on
029800040301     C                   Z-ADD     PiVal         VABNSP
029900040301     C                   Z-ADD     PiVal         VABRMN
030000020304     C                   ELSE
030100020722     C                   SETON                                        31
030200020304     C                   Z-ADD     *zeros        VABNSP
030300040301     C                   Z-ADD     *zeros        VABRMN
030400020304     C                   EVAL      vinmsg = %trimr(vinmsg)
030500040301     C                             + ' ' + 'VABNSP VABRMN'
030600020304     C                   ENDIF
030700040420     C* CAD
030800040526     C                   EVAL      PiStr=%trim(%subst(vindta:75:5))
030900010607     C                   EXSR      CHKNUM
031000010607     C                   IF        PiInt=*on
031100020204     C                   Z-ADD     PiVal         Num5_0
031200020204     C                   MOVEL(p)  Num5_0        VABCAD
031300010607     C                   ELSE
031400010607     C                   SETON                                        32
031500020204     C                   EVAL      VABCAD = *zeros
031600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031700020204     C                             + ' ' + 'VABCAD'
031800010607     C                   ENDIF
031900040420     C* NCL
032000060526     C***                EVAL      PiStr=%trim(%subst(vindta:102:3))
032100130301     C***                EVAL      PiStr=%trim(%subst(vindta:100:5))
032200130301     C                   EVAL      PiStr=%trim(%subst(vindta:103:2))
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
033300040526     C                   EVAL      PiStr=%trim(%subst(vindta:111:5))
033400010607     C                   EXSR      CHKNUM
033500010607     C                   IF        PiNum=*on
033600040420     C                   Z-ADD     PiVal         VABPKB
033700010607     C                   ELSE
033800010607     C                   SETON                                        32
033900010607     C                   Z-ADD     *zeros        VABPKB
034000010607     C                   EVAL      vinmsg = %trimr(vinmsg)
034100010607     C                             + ' ' + 'VABPKB'
034200010607     C                   ENDIF
034300060526     C*
034400060526     C* Verifica forzatura "destinazione diversa"
034500060526     C                   IF        %trim(%subst(vindta:129:40)) <> *blanks
034600060526     C                   EVAL      VABIND=%trim(%subst(vindta:129:40))
034700060526     C                   EVAL      VABLOD=*blanks
034800060526     C                   EVAL      VABCAD=*blanks
034900060526     C                   ENDIF
035000020322     C*
035100020322     C* Considerazioni finali su CBO/CAS
035200020322     C                   IF        FlgCAS = '1'
035300040420     C                   IF        VABCBO = '1'
035400020322     C                   EVAL      VABCBO = '4'
035500040420     C                   ELSE
035600040420     C                   EVAL      VABCBO = '6'
035700040420     C                   ENDIF
035800020322     C                   ENDIF
035900020204     C*
036000020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036100020204     C                   EXSR      CHKIMPDIV
036200020204     C*
036300010607     C                   ENDSR
036400010607     C*----------------------------------------------------*
036500010601
036600020204
036700020204     C*----------------------------------------------------*
036800020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
036900020204     C*----------------------------------------------------*
037000020204     C     CHKIMPDIV     BEGSR
037100020204     C*
037200020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
037300020204     C                   Z-ADD     *zeros        wrkDec            9 9
037400020204     C*
037500020204     C* Come prima cosa effettuo considerazioni sulla divisa
037600020204     C                   IF        vabIAS > *zeros
037700020204     C                   IF        vabVAS <> 'EUR'
037800020204     C                   EVAL      vabVAS =  'ITL'
037900020204     C                   ENDIF
038000020204     C                   ENDIF
038100020204     C*
038200020204     C                   IF        vabCAS > *zeros
038300020204     C                   IF        vabVCA <> 'EUR'
038400020204     C                   EVAL      vabVCA =  'ITL'
038500020204     C                   ENDIF
038600020204     C                   ENDIF
038700020204     C*
038800020204     C                   IF        vabVMD > *zeros
038900020321     C                   IF        vabVAD <> 'EUR'
039000020204     C                   EVAL      vabVAD =  'ITL'
039100020204     C                   ENDIF
039200020204     C                   ENDIF
039300020204     C*
039400020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
039500020204     C                   Z-ADD     vabIAS        wrkDec
039600020204     C                   IF        wrkDec > *zeros
039700020204     C                   IF        vabVAS = 'ITL'
039800020204     C                   EVAL      vabIAS = *zeros
039900020204     C                   ENDIF
040000020204     C                   ENDIF
040100020204     C*
040200020204     C* Stabilisco se il contrasegno ha decimali valorizzati
040300020204     C                   Z-ADD     vabCAS        wrkDec
040400020204     C                   IF        wrkDec > *zeros
040500020204     C                   IF        vabVCA = 'ITL'
040600020204     C                   EVAL      vabCAS = *zeros
040700020204     C                   ENDIF
040800020204     C                   ENDIF
040900020204     C*
041000020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041100020204     C                   Z-ADD     vabVMD        wrkDec
041200020204     C                   IF        wrkDec > *zeros
041300020204     C                   IF        vabVAD = 'ITL'
041400020204     C                   EVAL      vabVMD = *zeros
041500020204     C                   ENDIF
041600020204     C                   ENDIF
041700020204     C*
041800020204     C                   ENDSR
041900020204     C***
042000020204
042100010330
042200010330
042300010330     C*----------------------------------------------------*
042400010330     C*  CONTROLLO NUMERICITA' CAMPI
042500010330     C*----------------------------------------------------*
042600010330     C     CHKNUM        BEGSR
042700010330     C*
042800010606     C                   IF        PiDecChr = *blanks
042900010606     C                   EVAL      PiDecChr = ','
043000010606     C                   ENDIF
043100010606     C*
043200010606     C                   CALL(e)   'ISNUMERIC'
043300010330     C                   PARM                    PiStr            30
043400010606     C                   PARM                    PiDecChr          1
043500010330     C                   PARM      *ZEROS        PiVal            30 9
043600010330     C                   PARM      '0'           PiInt             1
043700010330     C                   PARM      '0'           PiNum             1
043800010330     C                   IF        %error
043900010606     C                   EVAL      PiNum=*off
044000010330     C                   ENDIF
044100010330     C*
044200010330     C                   ENDSR
044300010330     C***
044400010330
044500010601
044600010601
044700010601
044800010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044900010601     C     repfil        BEGSR
045000010601     C*
045100010601     C                   if        invfil = *zeros and
045200010601     C                             depfil > *zeros and
045300010629     C                             (vinflg = *blanks or
045400010629     C                              vinflg = *zeros)
045500010601     C
045600010601     C                   eval      invfil = depfil
045700010601     C                   endif
045800010601     C*
045900010601     C                   if        depfil <> invfil and
046000010601     C                             invfil > *zeros
046100010601     C                   eval      flgMulti = '1'
046200010601     C                   if        vinflg = *blanks
046300010601     C                   add       1             cntNonEl
046400010601     C                   endif
046500010601     C                   endif
046600010601     C*
046700010601     C                   if        vinflg = '2'
046800010601     C                   eval      flgStato = '2'
046900010601     C                   endif
047000010601     C*
047100010601     C                   ENDSR
047200010601     C***
047300010601
047400010601
047500010601
047600010330
047700010330
047800010330
047900990920      /TITLE Invio dei dati al punto operativo.
048000000613     C     invio         BEGSR
048100990920     C*
048200990920     C                   reset                   dscmz
048300010601     C                   move      invfil        cmzdst
048400990920     C                   eval      cmzfld = vlrfou
048500990920     C                   eval      cmzmbd = vlrhdl
048600990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048700000710     C                   if        prmfir = *blanks
048800021014     C                   eval      cmzfla = 'FIVAB00F'
048900021014     C                   eval      cmzmba = 'FIVAB00F'
049000000710     C                   else
049100000710     C                   eval      cmzfla = prmfir
049200000710     C                   eval      cmzmba = prmfir
049300000710     C                   endif
049400990920     C                   eval      cmznrr = *zeros
049500990920     C                   move      §ctrok        cmznrr
049600021018     C                   eval      cmzlba = vlrfl1
049700990920     C                   call(e)   'TIS711C'
049800990920     C                   parm                    dscmz
049900990921     C                   parm      *blanks       esito
050000990920     C                   if        %error
050100990920     C                             or cmzerr = '1'
050200990921     C                             or esito  = '1'
050300000710     C                   eval      wrkesito = '3'
050400990920     C                   endif
050500990920     C*
050600000613     C                   ENDSR
050700990910
050800010601
050900010601
051000010601
051100010601
051200010601      /TITLE Invio dei dati al punto operativo.
051300010601     C     opeini        BEGSR
051400010601     C*
051500010601     C* Inizializzo flag e contatori operativi
051600010601     C                   movel     '0'           flgGiro           1
051700010601     C                   movel     '0'           flgMulti          1
051800010601     C                   movel     '1'           flgStato          1
051900010615     C                   movel     '0'           flgOk             1
052000010601     C                   z-add     *zeros        cntNonEl         10 0
052100010601     C                   z-add     *zeros        depfil            3 0
052200010601     C                   z-add     *zeros        invfil            3 0
052300010601     C*
052400010601     C                   ENDSR
052500010601     C***
052600010601
052700010601
052800010601
052900010330
053000010330
053100000613     C     *inzsr        BEGSR
053200990910     C*
053300990910     C     *entry        plist
053400990920     C                   parm                    tivlrds
053500990921     C                   parm      wrkesito      esito
053600000724     C                   parm                    prmlit
053700000710     C                   parm                    prmfir
053800010330     C*
053900010330     C* CALCOLA LA DATA CORRENTE
054000010330     C                   time                    wn14             14 0
054100010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054200010330     C                   z-add     wn8           g08dat
054300010330     C                   z-add     *zeros        g08inv
054400010330     C                   movel     '0'           g08err
054500010330     C                   call      'XSRDA8'
054600010330     C                   parm                    wlbda8
054700010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054800000613     C*
054900000613     C                   ENDSR
055000000613     C***
