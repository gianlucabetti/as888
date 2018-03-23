000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000010330     D dscmz         e ds                  inz
001100010330     D psds           sds
001200010330     D  procname         *PROC
001300010330     D tivlrds       e ds                  extname(tivlr00f)
001400070502     D tisi95ds      e ds
001500010330     D esito           s              1
001600010330     D prmlit          s             10
001700010330     D prmfir          s             10
001800010330     D wrkesito        s                   like(esito)
001900010330     D rrnum           s              6  0 INZ(*zeros)
002000010330     D*------------------
002100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002200010330     D*------------------
002300010330     D WLBDA8          DS                  INZ
002400010330     D  G08DAT                 1      8  0
002500010330     D  G08INV                 9     16  0
002600010330     D  G08ERR                17     17
002700010330     D  G08TGI                18     22  0
002800010330
002900010330
003000000913     C                   reset                   rrnum
003100990921     C                   reset                   esito
003200990921     C                   reset                   wrkesito
003300010601     C*
003400010601     C                   exsr      opeini
003500010605     C                   exsr      rwvab
003600070502     C*
003700070502     C* Effettuo lancio TISI95 solo x chiusura
003800070502     C                   CLEAR                   TISI95DS
003900070502     C                   EVAL      I95TLA = 'C'
004000070502     C                   CALL      'TISI95R'
004100070502     C                   PARM                    TISI95DS
004200010601     C*
004300010601     C                   seton                                        lr
004400010601
004500010601
004600010601
004700010601
004800010601     C*--------------------------------------------------------
004900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005000010601     C*--------------------------------------------------------
005100010601     C     PREELA        BEGSR
005200010601     C*
005300010601     C* SE OCCORRE SPEDIRE IN FILIALE
005400010601     C                   if        invfil <> *zeros and
005500010601     C                             flgGiro = '0'
005600010601     C*
005700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005800010601     C                   eval      flgGiro = '1'
005900010601     C*
006000010601     C                   endif
006100010601     C*
006200010601     C                   ENDSR
006300010601     C***
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
007500010601
007600010601
007700010330     C*--------------------------------------------------------
007800050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007900010330     C*--------------------------------------------------------
008000010605     C     RWVAB         BEGSR
008100050408     C*
008200010330     C                   if        not %open(tivin00r)
008300010330     C                   open      tivin00r
008400010330     C                   endif
008500050408     C                   if        not %open(fivabwwr)
008600050408     C                   open      fivabwwr
008700010330     C                   endif
008800010330     C*
008900010604     C                   clear                   §CTROK
009000010604     C                   clear                   §CTRMO
009100010604     C                   clear                   §CTRNO
009200010330     C*
009300010330     C                   DO        *HIVAL
009400010330     C*
009500010330     C                   READ      tivin00r                               70
009600010618     C*
009700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009800010618     C                   if        vinflg = '1'
009900010618     C                   eval      flgOk = '1'
010000010618     C                   endif
010100010618     C*
010200010330     C                   if        vindta > *blanks
010300010330     C                   add       1             rrnum
010400010330     C*
010500010601     C                   if        *in70 = *off and
010600010330     C                             (vinflg = *blanks
010700010330     C                              or vinflg = '0'
010800010330     C                              or vinflg = '2')
010900010330     C*
011000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011100010711     C                   if        vinflg = *blanks or vinflg = '0'
011200010711     C                   clear                   vinmsg
011300010711     C                   endif
011400070601     C*
011500070601     C                   exsr      inzvar
011600070601     C                   exsr      defcam
011700070601     C                   exsr      impvab
011800010601     C*
011900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012000010605     C                   eval      depfil = VABLNP
012100010601     C                   exsr      repfil
012200010601     C                   if        depfil = invfil
012300021025     C                   if        vlrpoi = 999
012400021025     C                   MOVE(P)   invfil        VABFGS
012500021025     C                   else
012600021025     C                   MOVE(P)   vlrpoi        VABFGS
012700021025     C                   endif
012800010601     C*
012900010601     C                   exsr      PREELA
013000010601     C*
013100010604     C* Ebbene...
013200010604     C*
013300010604     C  N31              ADD       1             §CTROK            7 0
013400010604     C   32              ADD       1             §CTRMO            7 0
013500010604     C   31              ADD       1             §CTRNO            7 0
013600071003     C                   exsr      wrivab                                       => scrivo VAB
013700010604     C*
013800020722     C                   endif
013900020722     C*
014000010604     C                   if        *in31 = *off and
014100010604     C                             *in32 = *off
014200010604     C                   eval      vinflg = '1'
014300010604     C                   else
014400010604     C                   eval      vinflg = '2'
014500010604     C                   endif
014600010604     C                   endif
014700010604     C*
014800010330     C                   else
014900010330     C                   eval      vinflg = '1'
015000010330     C                   endif
015100010601     C*
015200010601     C  N70              update    tivin000
015300010330     C*
015400010330     C  N70              ENDdo
015500010601     C*
015600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
015700010601     C                   if        cntNonEl = *zeros or
015800010601     C                             flgMulti = '0'
015900010330     C* Se non ci sono record con errori ...
016000010601     C                   if        §ctrno = 0 and
016100010604     C                             §ctrmo = 0 and
016200010601     C                             flgStato <> '2'
016300010330     C* ... restituisco esito OK.
016400010330     C                   eval      wrkesito = '0'
016500010330     C                   else
016600010330     C                   if        §ctrok > 0
016700010330     C                   eval      wrkesito = '1'
016800010330     C                   else
016900010615     C                   if        flgOk = '0'
017000010615     C                   eval      wrkesito = '2'
017100010615     C                   else
017200010615     C                   eval      wrkesito = '6'
017300010615     C                   endif
017400010330     C                   endif
017500010330     C                   endif
017600010601     C                   else
017700010601     C                   eval      wrkesito = '9'
017800010601     C                   endif
017900010330     C*
018000010330     C                   if        %open(tivin00r)
018100010330     C                   close     tivin00r
018200010330     C                   endif
018300050408     C                   if        %open(fivabwwr)
018400050408     C                   close     fivabwwr
018500010330     C                   endif
018600010601     C*
018700010601     C                   if        vlrpoi <> 999
018800010601     C                   eval      invfil = vlrpoi
018900010601     C                   endif
019000010330     C*
019100010330     C                   if        §ctrok > 0
019200010601     C                             and invfil > *zeros
019300010330     C                   exsr      invio
019400010330     C                   endif
019500010601     C*
019600010618     C                   if        flgGiro = '1'
019700010601     C                   exsr      endela
019800010618     C                   endif
019900010330     C*
020000010330     C                   ENDSR
020100010330     C***
020200070601
020300070601     C*----------------------------------------------------*
020400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020500070601     C*----------------------------------------------------*
020600070601     C     WRIVAB        BEGSR
020700070601     C*
020800070601     C  N31              WRITE     FIVAB000
020900070601     C*
021000070601     C                   ENDSR
021100010601
021200010601
021300010601
021400010330     C*----------------------------------------------------*
021500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600010330     C*----------------------------------------------------*
021700010330     C     INZVAR        BEGSR
021800010330     C*
021900020204     C                   Z-ADD     *zeros        Num5_0            5 0
022000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022100020322     C                   MOVEL     '0'           FlgCAS            1
022200010330     C*
022300010330     C                   ENDSR
022400010330     C*----------------------------------------------------*
022500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022600010330     C*----------------------------------------------------*
022700010330     C     DEFCAM        BEGSR
022800010330     C*
022900020204     C* Inizializzo il buffer del record da scrivere
023000050408     C                   CLEAR                   FIVAB000
023100020204     C* Imposto i valori di default...
023200071029     C                   EVAL      VABCCM = 0894032
023300071029     C                   EVAL      VABLNP = 089
023400071029     C                   EVAL      VABCTR = 001
023500120220     C                   EVAL      VABCTM = '7T'
023600070208     C                   EVAL      VABCBO = '1'
023700071029     C                   EVAL      VABTSP = 'C'
023800020204     C* ... e poi verifico se sono stati passati come parametri
023900020204     C                   IF        vlrppt > *blanks
024000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024200020204     C                   EXSR      CHKNUM
024300020204     C                   IF        PiInt=*on
024400020204     C                   Z-ADD     PiVal         VABCCM
024500020204     C                   ENDIF
024600040301     C                   ENDIF
024700040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020204     C                   Z-ADD     PiVal         VABLNP
025200020204     C                   ENDIF
025300040301     C                   ENDIF
025400040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025600020204     C                   EXSR      CHKNUM
025700020204     C                   IF        PiInt=*on
025800020204     C                   Z-ADD     PiVal         VABCTR
025900040301     C                   ENDIF
026000020204     C                   ENDIF
026100020204     C                   ENDIF
026200020204     C*
026300010330     C                   ENDSR
026400010607     C*----------------------------------------------------*
026500050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026600010607     C*----------------------------------------------------*
026700010607     C     IMPVAB        BEGSR
026800010607     C*
026900010607     C                   SETOFF                                       3132
027000010607     C*
027100010607     C* Reperimento campi ALFA
027200010607     C*
027300010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
027400071029     C                   EVAL      VABRSD=%trim(%subst(vindta:208:35))
027500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027700020117     C* ==
027800071029     C                   EVAL      VABRD2=%trim(%subst(vindta:208+35:50-35))
027900071029     C                   EVAL      VABIND=%trim(%subst(vindta:258:50))
028000071029     C                   EVAL      VABLOD=%trim(%subst(vindta:318:50))
028100071029     C                   EVAL      VABPRD=%trim(%subst(vindta:368:2))
028200071029     C                   EVAL      VABRMA=%trim(%subst(vindta:33:10))
028300071029     C                   EVAL      VABNOT=%trim(%subst(vindta:388:35))
028400071029     C                   EVAL      VABNT2=%trim(%subst(vindta:388+35:35))
028500091007     C                   EVAL      VABTIC=%trim(%subst(vindta:530:2))
028600060510     C*
028700060510     C* Reperimento campi NUMERICI
028800060510     C                   MOVEL     DATCOR        VABAAS
028900060510     C                   MOVE      DATCOR        VABMGS
029000060710     C* NSP/RMN
029100071029     C                   EVAL      PiStr=%trim(%subst(vindta:6:7))
029200060510     C                   EXSR      CHKNUM
029300060510     C                   IF        PiInt=*on
029400060510     C                   Z-ADD     PiVal         VABNSP
029500060510     C                   Z-ADD     PiVal         VABRMN
029600060404     C                   ELSE
029700070502     C                   SETON                                        31
029800060510     C                   Z-ADD     *zeros        VABNSP
029900060510     C                   Z-ADD     1             VABRMN
030000060404     C                   EVAL      vinmsg = %trimr(vinmsg)
030100060510     C                             + ' ' + 'VABNSP VABRMN'
030200060404     C                   ENDIF
030300040420     C* CAD
030400071029     C                   EVAL      PiStr=%trim(%subst(vindta:308:5))
030500010607     C                   EXSR      CHKNUM
030600010607     C                   IF        PiInt=*on
030700020204     C                   Z-ADD     PiVal         Num5_0
030800020204     C                   MOVEL(p)  Num5_0        VABCAD
030900010607     C                   ELSE
031000010607     C                   SETON                                        32
031100020204     C                   EVAL      VABCAD = *zeros
031200010607     C                   EVAL      vinmsg = %trimr(vinmsg)
031300020204     C                             + ' ' + 'VABCAD'
031400060316     C                   ENDIF
031500070502     C* Reperisco la provincia dal CAP e dalla località
031600070502     C                   IF        VABCAD <> *blanks AND
031700070502     C                             VABPRD  = *blanks
031800070502     C                   CLEAR                   TISI95DS
031900070502     C                   EVAL      I95TCN = '3'
032000070502     C                   Z-ADD     datcor        I95DAT
032100070502     C                   EVAL      I95CAP = VABCAD
032200070502     C                   EVAL      I95LOC = VABLOD
032300070502     C                   EVAL      I95NAR = VABNZD
032400070502     C                   CALL      'TISI95R'
032500070502     C                   PARM                    TISI95DS
032600070502     C                   EVAL      VABPRD = O95PRV
032700070502     C                   ENDIF
032800040420     C* NCL
032900071029     C                   EVAL      PiStr=%trim(%subst(vindta:373:5))
033000010607     C                   EXSR      CHKNUM
033100010607     C                   IF        PiInt=*on
033200070606     C                   Z-ADD     PiVal         VABNCL
033300010607     C                   ELSE
033400010607     C                   SETON                                        32
033500010607     C                   Z-ADD     *zeros        VABNCL
033600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033700010607     C                             + ' ' + 'VABNCL'
033800010607     C                   ENDIF
033900071029     C* PKB
034000071029     C                   EVAL      PiStr=%trim(%subst(vindta:378:10))
034100071029     C                   EVAL      PiDecChr = '.'
034200010607     C                   EXSR      CHKNUM
034300071029     C                   EVAL      PiDecChr = *blanks
034400010607     C                   IF        PiNum=*on
034500071003     C                   Z-ADD(H)  PiVal         VABPKB
034600010607     C                   ELSE
034700010607     C                   SETON                                        32
034800010607     C                   Z-ADD     *zeros        VABPKB
034900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
035000010607     C                             + ' ' + 'VABPKB'
035100010607     C                   ENDIF
035200071022     C* CAS
035300071029     C                   IF        %trim(%subst(vindta:518:10)) <> *blanks AND
035400071029     C                             %trim(%subst(vindta:518:10)) <> *zeros  AND
035500071029     C                             %trim(%subst(vindta:518:10)) <> '0'
035600071022     C                   EVAL      FlgCAS = '1'
035700071029     C                   EVAL      PiStr=%trim(%subst(vindta:518:10))
035800071029     C                   EVAL      PiDecChr = ','
035900071022     C                   EXSR      CHKNUM
036000071029     C                   EVAL      PiDecChr = *blanks
036100071022     C                   IF        PiNum=*on
036200071022     C                   Z-ADD(H)  PiVal         VABCAS
036300071022     C                   EVAL      VABVCA ='EUR'
036400071022     C                   ELSE
036500071022     C                   SETON                                        32
036600071022     C                   Z-ADD     *zeros        VABCAS
036700071022     C                   EVAL      vinmsg = %trimr(vinmsg)
036800071022     C                             + ' ' + 'VABCAS'
036900071022     C                   ENDIF
037000071022     C                   ENDIF
037100020322     C*
037200020322     C* Considerazioni finali su CBO/CAS
037300050908     C                   IF        FlgCAS = '1'
037400040713     C                   IF        VABCBO = '1'
037500020322     C                   EVAL      VABCBO = '4'
037600040713     C                   ENDIF
037700040713     C                   IF        VABCBO = '2'
037800040713     C                   EVAL      VABCBO = '6'
037900040713     C                   ENDIF
038000020322     C                   ENDIF
038100020204     C*
038200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038300020204     C                   EXSR      CHKIMPDIV
038400020204     C*
038500010607     C                   ENDSR
038600010607     C*----------------------------------------------------*
038700010601
038800020204
038900020204     C*----------------------------------------------------*
039000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
039100020204     C*----------------------------------------------------*
039200020204     C     CHKIMPDIV     BEGSR
039300020204     C*
039400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039500020204     C                   Z-ADD     *zeros        wrkDec            9 9
039600020204     C*
039700020204     C* Come prima cosa effettuo considerazioni sulla divisa
039800020204     C                   IF        vabIAS > *zeros
039900020204     C                   IF        vabVAS <> 'EUR'
040000020204     C                   EVAL      vabVAS =  'ITL'
040100020204     C                   ENDIF
040200020204     C                   ENDIF
040300020204     C*
040400020204     C                   IF        vabCAS > *zeros
040500020204     C                   IF        vabVCA <> 'EUR'
040600020204     C                   EVAL      vabVCA =  'ITL'
040700020204     C                   ENDIF
040800020204     C                   ENDIF
040900020204     C*
041000020204     C                   IF        vabVMD > *zeros
041100020321     C                   IF        vabVAD <> 'EUR'
041200020204     C                   EVAL      vabVAD =  'ITL'
041300020204     C                   ENDIF
041400020204     C                   ENDIF
041500020204     C*
041600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041700020204     C                   Z-ADD     vabIAS        wrkDec
041800020204     C                   IF        wrkDec > *zeros
041900020204     C                   IF        vabVAS = 'ITL'
042000020204     C                   EVAL      vabIAS = *zeros
042100020204     C                   ENDIF
042200020204     C                   ENDIF
042300020204     C*
042400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
042500020204     C                   Z-ADD     vabCAS        wrkDec
042600020204     C                   IF        wrkDec > *zeros
042700020204     C                   IF        vabVCA = 'ITL'
042800020204     C                   EVAL      vabCAS = *zeros
042900020204     C                   ENDIF
043000020204     C                   ENDIF
043100020204     C*
043200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043300020204     C                   Z-ADD     vabVMD        wrkDec
043400020204     C                   IF        wrkDec > *zeros
043500020204     C                   IF        vabVAD = 'ITL'
043600020204     C                   EVAL      vabVMD = *zeros
043700020204     C                   ENDIF
043800020204     C                   ENDIF
043900020204     C*
044000020204     C                   ENDSR
044100020204     C***
044200020204
044300010330
044400010330
044500010330     C*----------------------------------------------------*
044600010330     C*  CONTROLLO NUMERICITA' CAMPI
044700010330     C*----------------------------------------------------*
044800010330     C     CHKNUM        BEGSR
044900010330     C*
045000010606     C                   IF        PiDecChr = *blanks
045100070502     C                   EVAL      PiDecChr = '.'
045200010606     C                   ENDIF
045300010606     C*
045400010606     C                   CALL(e)   'ISNUMERIC'
045500010330     C                   PARM                    PiStr            30
045600010606     C                   PARM                    PiDecChr          1
045700010330     C                   PARM      *ZEROS        PiVal            30 9
045800010330     C                   PARM      '0'           PiInt             1
045900010330     C                   PARM      '0'           PiNum             1
046000010330     C                   IF        %error
046100010606     C                   EVAL      PiNum=*off
046200010330     C                   ENDIF
046300010330     C*
046400010330     C                   ENDSR
046500010330     C***
046600010330
046700010601
046800010601
046900010601
047000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
047100010601     C     repfil        BEGSR
047200010601     C*
047300010601     C                   if        invfil = *zeros and
047400010601     C                             depfil > *zeros and
047500010629     C                             (vinflg = *blanks or
047600010629     C                              vinflg = *zeros)
047700010601     C
047800010601     C                   eval      invfil = depfil
047900010601     C                   endif
048000010601     C*
048100010601     C                   if        depfil <> invfil and
048200010601     C                             invfil > *zeros
048300010601     C                   eval      flgMulti = '1'
048400010601     C                   if        vinflg = *blanks
048500010601     C                   add       1             cntNonEl
048600010601     C                   endif
048700010601     C                   endif
048800010601     C*
048900010601     C                   if        vinflg = '2'
049000010601     C                   eval      flgStato = '2'
049100010601     C                   endif
049200010601     C*
049300010601     C                   ENDSR
049400010601     C***
049500010601
049600010601
049700010601
049800010330
049900010330
050000010330
050100990920      /TITLE Invio dei dati al punto operativo.
050200000613     C     invio         BEGSR
050300990920     C*
050400990920     C                   reset                   dscmz
050500010601     C                   move      invfil        cmzdst
050600990920     C                   eval      cmzfld = vlrfou
050700990920     C                   eval      cmzmbd = vlrhdl
050800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
050900000710     C                   if        prmfir = *blanks
051000050408     C                   eval      cmzfla = 'FIVAB00F'
051100050408     C                   eval      cmzmba = 'FIVAB00F'
051200000710     C                   else
051300000710     C                   eval      cmzfla = prmfir
051400000710     C                   eval      cmzmba = prmfir
051500000710     C                   endif
051600990920     C                   eval      cmznrr = *zeros
051700990920     C                   move      §ctrok        cmznrr
051800021018     C                   eval      cmzlba = vlrfl1
051900990920     C                   call(e)   'TIS711C'
052000990920     C                   parm                    dscmz
052100990921     C                   parm      *blanks       esito
052200990920     C                   if        %error
052300990920     C                             or cmzerr = '1'
052400990921     C                             or esito  = '1'
052500000710     C                   eval      wrkesito = '3'
052600990920     C                   endif
052700990920     C*
052800000613     C                   ENDSR
052900990910
053000010601
053100010601
053200010601
053300010601
053400010601      /TITLE Invio dei dati al punto operativo.
053500010601     C     opeini        BEGSR
053600010601     C*
053700010601     C* Inizializzo flag e contatori operativi
053800010601     C                   movel     '0'           flgGiro           1
053900010601     C                   movel     '0'           flgMulti          1
054000010601     C                   movel     '1'           flgStato          1
054100010615     C                   movel     '0'           flgOk             1
054200010601     C                   z-add     *zeros        cntNonEl         10 0
054300010601     C                   z-add     *zeros        depfil            3 0
054400010601     C                   z-add     *zeros        invfil            3 0
054500010601     C*
054600010601     C                   ENDSR
054700010601     C***
054800010601
054900010601
055000010601
055100010330
055200010330
055300000613     C     *inzsr        BEGSR
055400990910     C*
055500990910     C     *entry        plist
055600990920     C                   parm                    tivlrds
055700990921     C                   parm      wrkesito      esito
055800000724     C                   parm                    prmlit
055900000710     C                   parm                    prmfir
056000010330     C*
056100010330     C* CALCOLA LA DATA CORRENTE
056200010330     C                   time                    wn14             14 0
056300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056400010330     C                   z-add     wn8           g08dat
056500010330     C                   z-add     *zeros        g08inv
056600010330     C                   movel     '0'           g08err
056700010330     C                   call      'XSRDA8'
056800010330     C                   parm                    wlbda8
056900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
057000000613     C*
057100000613     C                   ENDSR
057200000613     C***
