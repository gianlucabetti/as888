000100061018      /TITLE Upload via Internet: traduzione in FIVABWWR. (x ROSATI Cod. 2493089)
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
022300061018     C                   EVAL      VABCCM = 2493089
022400061018     C                   EVAL      VABLNP = 249
022500061017     C                   EVAL      VABCTR = 000
022600061003     C                   EVAL      VABCBO = '1'
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
025000020204     C                   ENDIF
025100020204     C*
025200010330     C                   ENDSR
025300010607     C*----------------------------------------------------*
025400021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025500010607     C*----------------------------------------------------*
025600010607     C     IMPVAB        BEGSR
025700010607     C*
025800010607     C                   SETOFF                                       3132
025900060720     C*
026000060720     C* Reperimento campi NUMERICI
026100060720     C                   MOVEL     DATCOR        VABAAS
026200060720     C                   MOVE      DATCOR        VABMGS
026300010607     C*
026400010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026500061018     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
026600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026800020117     C* ==
026900061018     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
027000061018     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
027100061018     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
027200061018     C                   IF        %subst(vindta:207:1) = 'A'
027300061018     C                   EVAL      VABCBO='2'
027400061018     C                   ENDIF
027500090513     C***                EVAL      VABRMA=%trim(%subst(vindta:108:10))
027600090513     C                   EVAL      VABRMA=%trim(%subst(vindta:245:20))
027700061018     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
027800061018     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
027900060720     C* CAD
028000061018     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
028100060720     C                   EXSR      CHKNUM
028200060720     C                   IF        PiInt=*on
028300060720     C                   Z-ADD     PiVal         Num5_0
028400060720     C                   MOVEL(p)  Num5_0        VABCAD
028500060720     C                   ELSE
028600060720     C                   SETON                                        32
028700060720     C                   EVAL      VABCAD = *zeros
028800060720     C                   EVAL      vinmsg = %trimr(vinmsg)
028900060720     C                             + ' ' + 'VABCAD'
029000060720     C                   ENDIF
029100061018     C* NSP/RMN
029200061018     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
029300061017     C                   EXSR      CHKNUM
029400061017     C                   IF        PiInt=*on
029500061017     C                   Z-ADD     PiVal         VABNSP
029600061018     C                   Z-ADD     PiVal         VABRMN
029700061017     C                   ELSE
029800061017     C                   SETON                                        31
029900061017     C                   Z-ADD     *zeros        VABNSP
030000061018     C                   Z-ADD     1             VABRMN
030100061017     C                   EVAL      vinmsg = %trimr(vinmsg)
030200061018     C                             + ' ' + 'VABNSP VABRMN'
030300061017     C                   ENDIF
030400040420     C* NCL
030500061018     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
030600010607     C                   EXSR      CHKNUM
030700010607     C                   IF        PiInt=*on
030800010607     C                   Z-ADD     PiVal         VABNCL
030900010607     C                   ELSE
031000010607     C                   SETON                                        32
031100010607     C                   Z-ADD     *zeros        VABNCL
031200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031300010607     C                             + ' ' + 'VABNCL'
031400010607     C                   ENDIF
031500040420     C* PKB
031600061018     C                   EVAL      PiStr=%trim(%subst(vindta:131:7))
031700061017     C                   EVAL      PiDecChr = ','
031800010607     C                   EXSR      CHKNUM
031900010607     C                   IF        PiNum=*on
032000040420     C                   Z-ADD     PiVal         VABPKB
032100010607     C                   ELSE
032200010607     C                   SETON                                        32
032300010607     C                   Z-ADD     *zeros        VABPKB
032400010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032500010607     C                             + ' ' + 'VABPKB'
032600010607     C                   ENDIF
032700060901     C* CAS
032800061018     C                   IF        %subst(vindta:138:9) <> '000000,00'
032900061018     C                   EVAL      FlgCAS = '1'
033000061018     C                   EVAL      VABVCA =  'EUR'
033100061018     C                   EVAL      PiStr=%trim(%subst(vindta:138:9))
033200061017     C                   EVAL      PiDecChr = ','
033300061003     C                   EXSR      CHKNUM
033400060720     C                   IF        PiNum=*on
033500060901     C                   Z-ADD     PiVal         VABCAS
033600060720     C                   ELSE
033700060720     C                   SETON                                        32
033800060901     C                   Z-ADD     *zeros        VABCAS
033900060720     C                   EVAL      vinmsg = %trimr(vinmsg)
034000060901     C                             + ' ' + 'VABCAS'
034100060720     C                   ENDIF
034200061017     C                   ENDIF
034300020322     C*
034400020322     C* Considerazioni finali su CBO/CAS
034500061017     C                   IF        FlgCAS = '1'
034600061017     C                   IF        VABCBO = '1'
034700061017     C                   EVAL      VABCBO = '4'
034800061017     C                   ENDIF
034900061017     C                   IF        VABCBO = '2'
035000061017     C                   EVAL      VABCBO = '6'
035100061017     C                   ENDIF
035200061017     C                   ENDIF
035300020204     C*
035400020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
035500020204     C                   EXSR      CHKIMPDIV
035600020204     C*
035700010607     C                   ENDSR
035800010607     C*----------------------------------------------------*
035900010601
036000020204
036100020204     C*----------------------------------------------------*
036200020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
036300020204     C*----------------------------------------------------*
036400020204     C     CHKIMPDIV     BEGSR
036500020204     C*
036600020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
036700020204     C                   Z-ADD     *zeros        wrkDec            9 9
036800020204     C*
036900020204     C* Come prima cosa effettuo considerazioni sulla divisa
037000020204     C                   IF        vabIAS > *zeros
037100020204     C                   IF        vabVAS <> 'EUR'
037200020204     C                   EVAL      vabVAS =  'ITL'
037300020204     C                   ENDIF
037400020204     C                   ENDIF
037500020204     C*
037600020204     C                   IF        vabCAS > *zeros
037700020204     C                   IF        vabVCA <> 'EUR'
037800020204     C                   EVAL      vabVCA =  'ITL'
037900020204     C                   ENDIF
038000020204     C                   ENDIF
038100020204     C*
038200020204     C                   IF        vabVMD > *zeros
038300020321     C                   IF        vabVAD <> 'EUR'
038400020204     C                   EVAL      vabVAD =  'ITL'
038500020204     C                   ENDIF
038600020204     C                   ENDIF
038700020204     C*
038800020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
038900020204     C                   Z-ADD     vabIAS        wrkDec
039000020204     C                   IF        wrkDec > *zeros
039100020204     C                   IF        vabVAS = 'ITL'
039200020204     C                   EVAL      vabIAS = *zeros
039300020204     C                   ENDIF
039400020204     C                   ENDIF
039500020204     C*
039600020204     C* Stabilisco se il contrasegno ha decimali valorizzati
039700020204     C                   Z-ADD     vabCAS        wrkDec
039800020204     C                   IF        wrkDec > *zeros
039900020204     C                   IF        vabVCA = 'ITL'
040000020204     C                   EVAL      vabCAS = *zeros
040100020204     C                   ENDIF
040200020204     C                   ENDIF
040300020204     C*
040400020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
040500020204     C                   Z-ADD     vabVMD        wrkDec
040600020204     C                   IF        wrkDec > *zeros
040700020204     C                   IF        vabVAD = 'ITL'
040800020204     C                   EVAL      vabVMD = *zeros
040900020204     C                   ENDIF
041000020204     C                   ENDIF
041100020204     C*
041200020204     C                   ENDSR
041300020204     C***
041400020204
041500010330
041600010330
041700010330     C*----------------------------------------------------*
041800010330     C*  CONTROLLO NUMERICITA' CAMPI
041900010330     C*----------------------------------------------------*
042000010330     C     CHKNUM        BEGSR
042100010330     C*
042200010606     C                   IF        PiDecChr = *blanks
042300061018     C                   EVAL      PiDecChr = ','
042400010606     C                   ENDIF
042500010606     C*
042600010606     C                   CALL(e)   'ISNUMERIC'
042700010330     C                   PARM                    PiStr            30
042800010606     C                   PARM                    PiDecChr          1
042900010330     C                   PARM      *ZEROS        PiVal            30 9
043000010330     C                   PARM      '0'           PiInt             1
043100010330     C                   PARM      '0'           PiNum             1
043200010330     C                   IF        %error
043300010606     C                   EVAL      PiNum=*off
043400010330     C                   ENDIF
043500010330     C*
043600010330     C                   ENDSR
043700010330     C***
043800010330
043900010601
044000010601
044100010601
044200010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
044300010601     C     repfil        BEGSR
044400010601     C*
044500010601     C                   if        invfil = *zeros and
044600010601     C                             depfil > *zeros and
044700010629     C                             (vinflg = *blanks or
044800010629     C                              vinflg = *zeros)
044900010601     C
045000010601     C                   eval      invfil = depfil
045100010601     C                   endif
045200010601     C*
045300010601     C                   if        depfil <> invfil and
045400010601     C                             invfil > *zeros
045500010601     C                   eval      flgMulti = '1'
045600010601     C                   if        vinflg = *blanks
045700010601     C                   add       1             cntNonEl
045800010601     C                   endif
045900010601     C                   endif
046000010601     C*
046100010601     C                   if        vinflg = '2'
046200010601     C                   eval      flgStato = '2'
046300010601     C                   endif
046400010601     C*
046500010601     C                   ENDSR
046600010601     C***
046700010601
046800010601
046900010601
047000010330
047100010330
047200010330
047300990920      /TITLE Invio dei dati al punto operativo.
047400000613     C     invio         BEGSR
047500990920     C*
047600990920     C                   reset                   dscmz
047700010601     C                   move      invfil        cmzdst
047800990920     C                   eval      cmzfld = vlrfou
047900990920     C                   eval      cmzmbd = vlrhdl
048000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
048100000710     C                   if        prmfir = *blanks
048200021014     C                   eval      cmzfla = 'FIVAB00F'
048300021014     C                   eval      cmzmba = 'FIVAB00F'
048400000710     C                   else
048500000710     C                   eval      cmzfla = prmfir
048600000710     C                   eval      cmzmba = prmfir
048700000710     C                   endif
048800990920     C                   eval      cmznrr = *zeros
048900990920     C                   move      §ctrok        cmznrr
049000021018     C                   eval      cmzlba = vlrfl1
049100990920     C                   call(e)   'TIS711C'
049200990920     C                   parm                    dscmz
049300990921     C                   parm      *blanks       esito
049400990920     C                   if        %error
049500990920     C                             or cmzerr = '1'
049600990921     C                             or esito  = '1'
049700000710     C                   eval      wrkesito = '3'
049800990920     C                   endif
049900990920     C*
050000000613     C                   ENDSR
050100990910
050200010601
050300010601
050400010601
050500010601
050600010601      /TITLE Invio dei dati al punto operativo.
050700010601     C     opeini        BEGSR
050800010601     C*
050900010601     C* Inizializzo flag e contatori operativi
051000010601     C                   movel     '0'           flgGiro           1
051100010601     C                   movel     '0'           flgMulti          1
051200010601     C                   movel     '1'           flgStato          1
051300010615     C                   movel     '0'           flgOk             1
051400010601     C                   z-add     *zeros        cntNonEl         10 0
051500010601     C                   z-add     *zeros        depfil            3 0
051600010601     C                   z-add     *zeros        invfil            3 0
051700010601     C*
051800010601     C                   ENDSR
051900010601     C***
052000010601
052100010601
052200010601
052300010330
052400010330
052500000613     C     *inzsr        BEGSR
052600990910     C*
052700990910     C     *entry        plist
052800990920     C                   parm                    tivlrds
052900990921     C                   parm      wrkesito      esito
053000000724     C                   parm                    prmlit
053100000710     C                   parm                    prmfir
053200010330     C*
053300010330     C* CALCOLA LA DATA CORRENTE
053400010330     C                   time                    wn14             14 0
053500010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
053600010330     C                   z-add     wn8           g08dat
053700010330     C                   z-add     *zeros        g08inv
053800010330     C                   movel     '0'           g08err
053900010330     C                   call      'XSRDA8'
054000010330     C                   parm                    wlbda8
054100010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
054200000613     C*
054300000613     C                   ENDSR
054400000613     C***
