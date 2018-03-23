000100061016      /TITLE Upload via Internet: traduzione in FIVABWWR. (x LIMONTA Cod. 2022195)
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
021100061017     C                   Z-ADD     *zeros        Num1_0            1 0
021200020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021300020322     C                   MOVEL     '0'           FlgCAS            1
021400010330     C*
021500010330     C                   ENDSR
021600010330     C*----------------------------------------------------*
021700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021800010330     C*----------------------------------------------------*
021900010330     C     DEFCAM        BEGSR
022000010330     C*
022100020204     C* Inizializzo il buffer del record da scrivere
022200021014     C                   CLEAR                   FIVAB000
022300020204     C* Imposto i valori di default...
022400061017     C                   EVAL      VABCCM = 2022195
022500060901     C                   EVAL      VABLNP = 202
022600141219     C                   EVAL      VABCTR = 000
022700060901     C                   EVAL      VABTSP = 'C'
022800061003     C                   EVAL      VABCBO = '1'
022900061017     C                   EVAL      VABNAS = 'tessili'
023000020204     C* ... e poi verifico se sono stati passati come parametri
023100020204     C                   IF        vlrppt > *blanks
023200040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023300020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023400020204     C                   EXSR      CHKNUM
023500020204     C                   IF        PiInt=*on
023600020204     C                   Z-ADD     PiVal         VABCCM
023700020204     C                   ENDIF
023800040301     C                   ENDIF
023900040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024100020204     C                   EXSR      CHKNUM
024200020204     C                   IF        PiInt=*on
024300020204     C                   Z-ADD     PiVal         VABLNP
024400020204     C                   ENDIF
024500040301     C                   ENDIF
024600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
024700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024800020204     C                   EXSR      CHKNUM
024900020204     C                   IF        PiInt=*on
025000020204     C                   Z-ADD     PiVal         VABCTR
025100040301     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C                   ENDIF
025400020204     C*
025500010330     C                   ENDSR
025600010607     C*----------------------------------------------------*
025700021014     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025800010607     C*----------------------------------------------------*
025900010607     C     IMPVAB        BEGSR
026000010607     C*
026100010607     C                   SETOFF                                       3132
026200060720     C*
026300060720     C* Reperimento campi NUMERICI
026400060720     C                   MOVEL     DATCOR        VABAAS
026500060720     C                   MOVE      DATCOR        VABMGS
026600010607     C*
026700010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026800061017     C                   EVAL      VABRSD=%trim(%subst(vindta:335:35))
026900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027100020117     C* ==
027200061017     C                   EVAL      VABIND=%trim(%subst(vindta:405:35))
027300061017     C                   EVAL      VABLOD=%trim(%subst(vindta:510:20))
027400061017     C                   EVAL      VABPRD=%trim(%subst(vindta:756:2))
027500061017     C                   EVAL      VABRMA=%trim(%subst(vindta:760:5))
027600141219     C                   SELECT
027700141219     C                   WHEN      %trim(%subst(vindta:773:2))='AB'
027800141219     C                   EVAL      VABTIC='BM'
027900141219     C                   WHEN      %trim(%subst(vindta:773:2))='AC'
028000141219     C                   EVAL      VABTIC='CM'
028100141219     C                   ENDSL
028200061017      * CBO
028300061017     C                   IF        %trim(%subst(vindta:758:2)) = '09'
028400061017     C                   EVAL      VABCBO='1'
028500061003     C                   ENDIF
028600061017     C                   IF        %trim(%subst(vindta:758:2)) = '01'
028700061017     C                   EVAL      VABCBO='2'
028800061017     C                   ENDIF
028900061017     C                   IF        %trim(%subst(vindta:758:2)) = '04'
029000061017     C                   EVAL      VABCBO='4'
029100061017     C                   ENDIF
029200060720     C* CAD
029300061017     C                   EVAL      PiStr=%trim(%subst(vindta:530:12))
029400060720     C                   EXSR      CHKNUM
029500060720     C                   IF        PiInt=*on
029600060720     C                   Z-ADD     PiVal         Num5_0
029700060720     C                   MOVEL(p)  Num5_0        VABCAD
029800060720     C                   ELSE
029900060720     C                   SETON                                        32
030000060720     C                   EVAL      VABCAD = *zeros
030100060720     C                   EVAL      vinmsg = %trimr(vinmsg)
030200060720     C                             + ' ' + 'VABCAD'
030300060720     C                   ENDIF
030400061017     C* CCM solo se inserito
030500061017     C                   IF        %trim(%subst(vindta:8:7)) <> '0000000'
030600061017     C                   EVAL      PiStr=%trim(%subst(vindta:8:7))
030700061017     C                   EXSR      CHKNUM
030800061017     C                   IF        PiInt=*on
030900061017     C                   Z-ADD     PiVal         VABCCM
031000061017     C                   ELSE
031100061017     C                   SETON                                        31
031200061017     C                   EVAL      VABCCM = *zeros
031300061017     C                   EVAL      vinmsg = %trimr(vinmsg)
031400061017     C                             + ' ' + 'VABCCM'
031500061017     C                   ENDIF
031600061017     C                   ENDIF
031700040420     C* NSP/RMN
031800061017     C                   EVAL      PiStr=%trim(%subst(vindta:760:5))
031900020304     C                   EXSR      CHKNUM
032000020304     C                   IF        PiInt=*on
032100040301     C                   Z-ADD     PiVal         VABNSP
032200040301     C                   Z-ADD     PiVal         VABRMN
032300020304     C                   ELSE
032400020722     C                   SETON                                        31
032500020304     C                   Z-ADD     *zeros        VABNSP
032600040301     C                   Z-ADD     *zeros        VABRMN
032700020304     C                   EVAL      vinmsg = %trimr(vinmsg)
032800040301     C                             + ' ' + 'VABNSP VABRMN'
032900020304     C                   ENDIF
033000040420     C* NCL
033100061017     C                   EVAL      PiStr=%trim(%subst(vindta:589:4))
033200010607     C                   EXSR      CHKNUM
033300010607     C                   IF        PiInt=*on
033400010607     C                   Z-ADD     PiVal         VABNCL
033500010607     C                   ELSE
033600010607     C                   SETON                                        32
033700010607     C                   Z-ADD     *zeros        VABNCL
033800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033900010607     C                             + ' ' + 'VABNCL'
034000010607     C                   ENDIF
034100061017      * numero decimali di PKB
034200061017     C                   EVAL      PiStr=%trim(%subst(vindta:606:1))
034300061017     C                   EXSR      CHKNUM
034400061017     C                   IF        PiNum=*on
034500061017     C                   Z-ADD     PiVal         Num1_0
034600061017     C                   ELSE
034700061017     C                   Z-ADD     *zeros        Num1_0
034800061017     C                   ENDIF
034900040420     C* PKB
035000061017     C                   EVAL      PiStr=%trim(%subst(vindta:593:10))
035100010607     C                   EXSR      CHKNUM
035200010607     C                   IF        PiNum=*on
035300040420     C                   Z-ADD     PiVal         VABPKB
035400061017     C                   EVAL      VABPKB = VABPKB / 10 ** Num1_0
035500010607     C                   ELSE
035600010607     C                   SETON                                        32
035700010607     C                   Z-ADD     *zeros        VABPKB
035800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035900010607     C                             + ' ' + 'VABPKB'
036000010607     C                   ENDIF
036100061017      * numero decimali di CAS
036200061017     C                   EVAL      PiStr=%trim(%subst(vindta:634:1))
036300061017     C                   EXSR      CHKNUM
036400061017     C                   IF        PiNum=*on
036500061017     C                   Z-ADD     PiVal         Num1_0
036600061017     C                   ELSE
036700061017     C                   Z-ADD     *zeros        Num1_0
036800061017     C                   ENDIF
036900060901     C* CAS
037000141219     C                   EVAL      PiStr=%trim(%subst(vindta:775:11))
037100061003     C                   EXSR      CHKNUM
037200060720     C                   IF        PiNum=*on
037300060901     C                   Z-ADD     PiVal         VABCAS
037400141219     C                   EVAL      VABCAS = VABCAS / 100
037500061003     C                   IF        VABCAS > 0
037600061003     C                   EVAL      FlgCAS = '1'
037700061003     C                   EVAL      VABVCA =  'EUR'
037800061003     C                   ENDIF
037900060720     C                   ELSE
038000060720     C                   SETON                                        32
038100060901     C                   Z-ADD     *zeros        VABCAS
038200060720     C                   EVAL      vinmsg = %trimr(vinmsg)
038300060901     C                             + ' ' + 'VABCAS'
038400060720     C                   ENDIF
038500020322     C*
038600020322     C* Considerazioni finali su CBO/CAS
038700061017      * non le faccio xchè lo passa il cliente
038800061017     C**!!!              IF        FlgCAS = '1'
038900061017     C**!!!              IF        VABCBO = '1'
039000061017     C**!!!              EVAL      VABCBO = '4'
039100061017     C**!!!              ENDIF
039200061017     C**!!!              IF        VABCBO = '2'
039300061017     C**!!!              EVAL      VABCBO = '6'
039400061017     C**!!!              ENDIF
039500061017     C**!!!              ENDIF
039600020204     C*
039700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
039800020204     C                   EXSR      CHKIMPDIV
039900020204     C*
040000010607     C                   ENDSR
040100010607     C*----------------------------------------------------*
040200010601
040300020204
040400020204     C*----------------------------------------------------*
040500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
040600020204     C*----------------------------------------------------*
040700020204     C     CHKIMPDIV     BEGSR
040800020204     C*
040900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
041000020204     C                   Z-ADD     *zeros        wrkDec            9 9
041100020204     C*
041200020204     C* Come prima cosa effettuo considerazioni sulla divisa
041300020204     C                   IF        vabIAS > *zeros
041400020204     C                   IF        vabVAS <> 'EUR'
041500020204     C                   EVAL      vabVAS =  'ITL'
041600020204     C                   ENDIF
041700020204     C                   ENDIF
041800020204     C*
041900020204     C                   IF        vabCAS > *zeros
042000020204     C                   IF        vabVCA <> 'EUR'
042100020204     C                   EVAL      vabVCA =  'ITL'
042200020204     C                   ENDIF
042300020204     C                   ENDIF
042400020204     C*
042500020204     C                   IF        vabVMD > *zeros
042600020321     C                   IF        vabVAD <> 'EUR'
042700020204     C                   EVAL      vabVAD =  'ITL'
042800020204     C                   ENDIF
042900020204     C                   ENDIF
043000020204     C*
043100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
043200020204     C                   Z-ADD     vabIAS        wrkDec
043300020204     C                   IF        wrkDec > *zeros
043400020204     C                   IF        vabVAS = 'ITL'
043500020204     C                   EVAL      vabIAS = *zeros
043600020204     C                   ENDIF
043700020204     C                   ENDIF
043800020204     C*
043900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
044000020204     C                   Z-ADD     vabCAS        wrkDec
044100020204     C                   IF        wrkDec > *zeros
044200020204     C                   IF        vabVCA = 'ITL'
044300020204     C                   EVAL      vabCAS = *zeros
044400020204     C                   ENDIF
044500020204     C                   ENDIF
044600020204     C*
044700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
044800020204     C                   Z-ADD     vabVMD        wrkDec
044900020204     C                   IF        wrkDec > *zeros
045000020204     C                   IF        vabVAD = 'ITL'
045100020204     C                   EVAL      vabVMD = *zeros
045200020204     C                   ENDIF
045300020204     C                   ENDIF
045400020204     C*
045500020204     C                   ENDSR
045600020204     C***
045700020204
045800010330
045900010330
046000010330     C*----------------------------------------------------*
046100010330     C*  CONTROLLO NUMERICITA' CAMPI
046200010330     C*----------------------------------------------------*
046300010330     C     CHKNUM        BEGSR
046400010330     C*
046500010606     C                   IF        PiDecChr = *blanks
046600061003     C                   EVAL      PiDecChr = '.'
046700010606     C                   ENDIF
046800010606     C*
046900010606     C                   CALL(e)   'ISNUMERIC'
047000010330     C                   PARM                    PiStr            30
047100010606     C                   PARM                    PiDecChr          1
047200010330     C                   PARM      *ZEROS        PiVal            30 9
047300010330     C                   PARM      '0'           PiInt             1
047400010330     C                   PARM      '0'           PiNum             1
047500010330     C                   IF        %error
047600010606     C                   EVAL      PiNum=*off
047700010330     C                   ENDIF
047800010330     C*
047900010330     C                   ENDSR
048000010330     C***
048100010330
048200010601
048300010601
048400010601
048500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
048600010601     C     repfil        BEGSR
048700010601     C*
048800010601     C                   if        invfil = *zeros and
048900010601     C                             depfil > *zeros and
049000010629     C                             (vinflg = *blanks or
049100010629     C                              vinflg = *zeros)
049200010601     C
049300010601     C                   eval      invfil = depfil
049400010601     C                   endif
049500010601     C*
049600010601     C                   if        depfil <> invfil and
049700010601     C                             invfil > *zeros
049800010601     C                   eval      flgMulti = '1'
049900010601     C                   if        vinflg = *blanks
050000010601     C                   add       1             cntNonEl
050100010601     C                   endif
050200010601     C                   endif
050300010601     C*
050400010601     C                   if        vinflg = '2'
050500010601     C                   eval      flgStato = '2'
050600010601     C                   endif
050700010601     C*
050800010601     C                   ENDSR
050900010601     C***
051000010601
051100010601
051200010601
051300010330
051400010330
051500010330
051600990920      /TITLE Invio dei dati al punto operativo.
051700000613     C     invio         BEGSR
051800990920     C*
051900990920     C                   reset                   dscmz
052000010601     C                   move      invfil        cmzdst
052100990920     C                   eval      cmzfld = vlrfou
052200990920     C                   eval      cmzmbd = vlrhdl
052300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
052400000710     C                   if        prmfir = *blanks
052500021014     C                   eval      cmzfla = 'FIVAB00F'
052600021014     C                   eval      cmzmba = 'FIVAB00F'
052700000710     C                   else
052800000710     C                   eval      cmzfla = prmfir
052900000710     C                   eval      cmzmba = prmfir
053000000710     C                   endif
053100990920     C                   eval      cmznrr = *zeros
053200990920     C                   move      §ctrok        cmznrr
053300021018     C                   eval      cmzlba = vlrfl1
053400990920     C                   call(e)   'TIS711C'
053500990920     C                   parm                    dscmz
053600990921     C                   parm      *blanks       esito
053700990920     C                   if        %error
053800990920     C                             or cmzerr = '1'
053900990921     C                             or esito  = '1'
054000000710     C                   eval      wrkesito = '3'
054100990920     C                   endif
054200990920     C*
054300000613     C                   ENDSR
054400990910
054500010601
054600010601
054700010601
054800010601
054900010601      /TITLE Invio dei dati al punto operativo.
055000010601     C     opeini        BEGSR
055100010601     C*
055200010601     C* Inizializzo flag e contatori operativi
055300010601     C                   movel     '0'           flgGiro           1
055400010601     C                   movel     '0'           flgMulti          1
055500010601     C                   movel     '1'           flgStato          1
055600010615     C                   movel     '0'           flgOk             1
055700010601     C                   z-add     *zeros        cntNonEl         10 0
055800010601     C                   z-add     *zeros        depfil            3 0
055900010601     C                   z-add     *zeros        invfil            3 0
056000010601     C*
056100010601     C                   ENDSR
056200010601     C***
056300010601
056400010601
056500010601
056600010330
056700010330
056800000613     C     *inzsr        BEGSR
056900990910     C*
057000990910     C     *entry        plist
057100990920     C                   parm                    tivlrds
057200990921     C                   parm      wrkesito      esito
057300000724     C                   parm                    prmlit
057400000710     C                   parm                    prmfir
057500010330     C*
057600010330     C* CALCOLA LA DATA CORRENTE
057700010330     C                   time                    wn14             14 0
057800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
057900010330     C                   z-add     wn8           g08dat
058000010330     C                   z-add     *zeros        g08inv
058100010330     C                   movel     '0'           g08err
058200010330     C                   call      'XSRDA8'
058300010330     C                   parm                    wlbda8
058400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
058500000613     C*
058600000613     C                   ENDSR
058700000613     C***
