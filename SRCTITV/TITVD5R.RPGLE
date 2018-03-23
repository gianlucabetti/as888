000100040713      /TITLE Upload via Internet: traduzione in FIVABWWR. (x EDSLAN Cod. 2492400/2493160)
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
022300040713     C                   MOVE(P)   vlrKSC        VABCCM
022400060223     C                   EVAL      VABLNP = 202
022500040713     C                   EVAL      VABCTR = 000
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
026200040713     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
026300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
026400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
026500020117     C* ==
026600040713     C                   EVAL      VABIND=%trim(%subst(vindta:36:70-36+1))
026700040713     C                   EVAL      VABLOD=%trim(%subst(vindta:71:100-71+1))
026800040713     C                   EVAL      VABPRD=%trim(%subst(vindta:106:107-106+1))
026900040713     C                   EVAL      VABRMA=%trim(%subst(vindta:108:117-108+1))
027000040713     C                   EVAL      VABNOT=%trim(%subst(vindta:147:35))
027100040713     C                   EVAL      VABNT2=%trim(%subst(vindta:181:206-181+1))
027200040713     C                   IF        %subst(vindta:207:1) = 'a'
027300040713     C                   EVAL      VABCBO = '2'
027400040713     C                   ELSE
027500040713     C                   EVAL      VABCBO = '1'
027600040713     C                   ENDIF
027700010607     C*
027800010607     C* Reperimento campi NUMERICI
027900020722     C                   MOVEL     DATCOR        VABAAS
028000020722     C                   MOVE      DATCOR        VABMGS
028100040420     C* NSP/RMN
028200040713     C                   EVAL      PiStr=%trim(%subst(vindta:108:117-108+1))
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
029500040713     C                   EVAL      PiStr=%trim(%subst(vindta:101:105-101+1))
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
030700040713     C                   EVAL      PiStr=%trim(%subst(vindta:124:128-124+1))
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
031800040713     C                   EVAL      PiStr=%trim(%subst(vindta:131:136-131+1))
031900010607     C                   EXSR      CHKNUM
032000010607     C                   IF        PiNum=*on
032100040420     C                   Z-ADD     PiVal         VABPKB
032200010607     C                   ELSE
032300010607     C                   SETON                                        32
032400010607     C                   Z-ADD     *zeros        VABPKB
032500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
032600010607     C                             + ' ' + 'VABPKB'
032700010607     C                   ENDIF
032800060725     C* VLB
032900060721     C                   EVAL      PiStr=%trim(%subst(vindta:208:9))
033000060721     C                   EXSR      CHKNUM
033100060721     C                   IF        PiNum=*on
033200060725     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
033300060725     C                   Z-ADD(H)  PiVal         VABVLB
033400060721     C                   ELSE
033500060721     C                   SETON                                        32
033600060721     C                   Z-ADD     *zeros        VABVLB
033700060721     C                   EVAL      vinmsg = %trimr(vinmsg)
033800060721     C                             + ' ' + 'VABVLB'
033900060721     C                   ENDIF
034000060721     C* CAS
034100040713     C                   IF        %subst(vindta:137:146-137+1) <> *all'0'
034200040713     C                   EVAL      FlgCAS = '1'
034300040713     C                   ENDIF
034400040713     C                   EVAL      PiStr=%trim(%subst(vindta:137:146-137+1))
034500040713     C                   EXSR      CHKNUM
034600040713     C                   IF        PiNum=*on
034700061003     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
034800061003     C                   Z-ADD     PiVal         VABCAS
034900040713     C                   EVAL      VABVCA = 'EUR'
035000040713     C                   ELSE
035100040713     C                   SETON                                        32
035200040713     C                   Z-ADD     *zeros        VABCAS
035300040713     C                   EVAL      vinmsg = %trimr(vinmsg)
035400040713     C                             + ' ' + 'VABCAS'
035500040713     C                   ENDIF
035600020322     C*
035700020322     C* Considerazioni finali su CBO/CAS
035800020322     C                   IF        FlgCAS = '1'
035900040713     C                   IF        VABCBO = '1'
036000020322     C                   EVAL      VABCBO = '4'
036100040713     C                   ENDIF
036200040713     C                   IF        VABCBO = '2'
036300040713     C                   EVAL      VABCBO = '6'
036400040713     C                   ENDIF
036500020322     C                   ENDIF
036600020204     C*
036700020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
036800020204     C                   EXSR      CHKIMPDIV
036900020204     C*
037000010607     C                   ENDSR
037100010607     C*----------------------------------------------------*
037200010601
037300020204
037400020204     C*----------------------------------------------------*
037500020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
037600020204     C*----------------------------------------------------*
037700020204     C     CHKIMPDIV     BEGSR
037800020204     C*
037900020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
038000020204     C                   Z-ADD     *zeros        wrkDec            9 9
038100020204     C*
038200020204     C* Come prima cosa effettuo considerazioni sulla divisa
038300020204     C                   IF        vabIAS > *zeros
038400020204     C                   IF        vabVAS <> 'EUR'
038500020204     C                   EVAL      vabVAS =  'ITL'
038600020204     C                   ENDIF
038700020204     C                   ENDIF
038800020204     C*
038900020204     C                   IF        vabCAS > *zeros
039000020204     C                   IF        vabVCA <> 'EUR'
039100020204     C                   EVAL      vabVCA =  'ITL'
039200020204     C                   ENDIF
039300020204     C                   ENDIF
039400020204     C*
039500020204     C                   IF        vabVMD > *zeros
039600020321     C                   IF        vabVAD <> 'EUR'
039700020204     C                   EVAL      vabVAD =  'ITL'
039800020204     C                   ENDIF
039900020204     C                   ENDIF
040000020204     C*
040100020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
040200020204     C                   Z-ADD     vabIAS        wrkDec
040300020204     C                   IF        wrkDec > *zeros
040400020204     C                   IF        vabVAS = 'ITL'
040500020204     C                   EVAL      vabIAS = *zeros
040600020204     C                   ENDIF
040700020204     C                   ENDIF
040800020204     C*
040900020204     C* Stabilisco se il contrasegno ha decimali valorizzati
041000020204     C                   Z-ADD     vabCAS        wrkDec
041100020204     C                   IF        wrkDec > *zeros
041200020204     C                   IF        vabVCA = 'ITL'
041300020204     C                   EVAL      vabCAS = *zeros
041400020204     C                   ENDIF
041500020204     C                   ENDIF
041600020204     C*
041700020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
041800020204     C                   Z-ADD     vabVMD        wrkDec
041900020204     C                   IF        wrkDec > *zeros
042000020204     C                   IF        vabVAD = 'ITL'
042100020204     C                   EVAL      vabVMD = *zeros
042200020204     C                   ENDIF
042300020204     C                   ENDIF
042400020204     C*
042500020204     C                   ENDSR
042600020204     C***
042700020204
042800010330
042900010330
043000010330     C*----------------------------------------------------*
043100010330     C*  CONTROLLO NUMERICITA' CAMPI
043200010330     C*----------------------------------------------------*
043300010330     C     CHKNUM        BEGSR
043400010330     C*
043500010606     C                   IF        PiDecChr = *blanks
043600010606     C                   EVAL      PiDecChr = ','
043700010606     C                   ENDIF
043800010606     C*
043900010606     C                   CALL(e)   'ISNUMERIC'
044000010330     C                   PARM                    PiStr            30
044100010606     C                   PARM                    PiDecChr          1
044200010330     C                   PARM      *ZEROS        PiVal            30 9
044300010330     C                   PARM      '0'           PiInt             1
044400010330     C                   PARM      '0'           PiNum             1
044500010330     C                   IF        %error
044600010606     C                   EVAL      PiNum=*off
044700010330     C                   ENDIF
044800010330     C*
044900010330     C                   ENDSR
045000010330     C***
045100010330
045200010601
045300010601
045400010601
045500010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
045600010601     C     repfil        BEGSR
045700010601     C*
045800010601     C                   if        invfil = *zeros and
045900010601     C                             depfil > *zeros and
046000010629     C                             (vinflg = *blanks or
046100010629     C                              vinflg = *zeros)
046200010601     C
046300010601     C                   eval      invfil = depfil
046400010601     C                   endif
046500010601     C*
046600010601     C                   if        depfil <> invfil and
046700010601     C                             invfil > *zeros
046800010601     C                   eval      flgMulti = '1'
046900010601     C                   if        vinflg = *blanks
047000010601     C                   add       1             cntNonEl
047100010601     C                   endif
047200010601     C                   endif
047300010601     C*
047400010601     C                   if        vinflg = '2'
047500010601     C                   eval      flgStato = '2'
047600010601     C                   endif
047700010601     C*
047800010601     C                   ENDSR
047900010601     C***
048000010601
048100010601
048200010601
048300010330
048400010330
048500010330
048600990920      /TITLE Invio dei dati al punto operativo.
048700000613     C     invio         BEGSR
048800990920     C*
048900990920     C                   reset                   dscmz
049000010601     C                   move      invfil        cmzdst
049100990920     C                   eval      cmzfld = vlrfou
049200990920     C                   eval      cmzmbd = vlrhdl
049300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
049400000710     C                   if        prmfir = *blanks
049500021014     C                   eval      cmzfla = 'FIVAB00F'
049600021014     C                   eval      cmzmba = 'FIVAB00F'
049700000710     C                   else
049800000710     C                   eval      cmzfla = prmfir
049900000710     C                   eval      cmzmba = prmfir
050000000710     C                   endif
050100990920     C                   eval      cmznrr = *zeros
050200990920     C                   move      §ctrok        cmznrr
050300021018     C                   eval      cmzlba = vlrfl1
050400990920     C                   call(e)   'TIS711C'
050500990920     C                   parm                    dscmz
050600990921     C                   parm      *blanks       esito
050700990920     C                   if        %error
050800990920     C                             or cmzerr = '1'
050900990921     C                             or esito  = '1'
051000000710     C                   eval      wrkesito = '3'
051100990920     C                   endif
051200990920     C*
051300000613     C                   ENDSR
051400990910
051500010601
051600010601
051700010601
051800010601
051900010601      /TITLE Invio dei dati al punto operativo.
052000010601     C     opeini        BEGSR
052100010601     C*
052200010601     C* Inizializzo flag e contatori operativi
052300010601     C                   movel     '0'           flgGiro           1
052400010601     C                   movel     '0'           flgMulti          1
052500010601     C                   movel     '1'           flgStato          1
052600010615     C                   movel     '0'           flgOk             1
052700010601     C                   z-add     *zeros        cntNonEl         10 0
052800010601     C                   z-add     *zeros        depfil            3 0
052900010601     C                   z-add     *zeros        invfil            3 0
053000010601     C*
053100010601     C                   ENDSR
053200010601     C***
053300010601
053400010601
053500010601
053600010330
053700010330
053800000613     C     *inzsr        BEGSR
053900990910     C*
054000990910     C     *entry        plist
054100990920     C                   parm                    tivlrds
054200990921     C                   parm      wrkesito      esito
054300000724     C                   parm                    prmlit
054400000710     C                   parm                    prmfir
054500010330     C*
054600010330     C* CALCOLA LA DATA CORRENTE
054700010330     C                   time                    wn14             14 0
054800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
054900010330     C                   z-add     wn8           g08dat
055000010330     C                   z-add     *zeros        g08inv
055100010330     C                   movel     '0'           g08err
055200010330     C                   call      'XSRDA8'
055300010330     C                   parm                    wlbda8
055400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
055500000613     C*
055600000613     C                   ENDSR
055700000613     C***
