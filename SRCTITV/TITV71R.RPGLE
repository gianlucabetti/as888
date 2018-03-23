000100031017      /TITLE Upload via Internet: traduzione in FIVABWWR. (x TRANS. OCHOA - Cod. 0501586)
000200990908     H dftactgrp(*yes)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500021025     FFIVABwwr  O    E             DISK    usropn
000600000313     D*
000700010330     D*----------------------------------------------------
000800010330     D* DICHIARAZIOINE VARIABILI DI WRK
000900010330     D*----------------------------------------------------
001000031020     D tisi95ds      e ds
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
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
002800000313     D*
002900010330
003000010330
003100010330
003200000913     C                   reset                   rrnum
003300990921     C                   reset                   esito
003400990921     C                   reset                   wrkesito
003500010601     C*
003600010601     C                   exsr      opeini
003700010605     C                   exsr      rwvab
003800010601     C*
003900010601     C                   seton                                        lr
004000010601
004100010601
004200010601
004300010601
004400010601     C*--------------------------------------------------------
004500010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004600010601     C*--------------------------------------------------------
004700010601     C     PREELA        BEGSR
004800010601     C*
004900010601     C* SE OCCORRE SPEDIRE IN FILIALE
005000010601     C                   if        invfil <> *zeros and
005100010601     C                             flgGiro = '0'
005200010601     C*
005300010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005400010601     C                   eval      flgGiro = '1'
005500010601     C*
005600010601     C                   endif
005700010601     C*
005800010601     C                   ENDSR
005900010601     C***
006000010601
006100010601
006200010601
006300010601
006400010601
006500010601
006600010601
006700010601     C*--------------------------------------------------------
006800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006900010601     C*--------------------------------------------------------
007000010601     C     ENDELA        BEGSR
007100050201     C*
007200050201     C* Effettuo lancio TISI95 solo x chiusura
007300050201     C                   CLEAR                   TISI95DS
007400050201     C                   EVAL      I95TLA = 'C'
007500050201     C                   CALL      'TISI95R'
007600050201     C                   PARM                    TISI95DS
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
014400021025     C  N31              WRITE     FIVAB000
014500010604     C*
014600010604     C                   if        *in31 = *off and
014700010604     C                             *in32 = *off
014800010604     C                   eval      vinflg = '1'
014900010604     C                   else
015000010604     C                   eval      vinflg = '2'
015100010604     C                   endif
015200010604     C                   endif
015300010601     C*
015400010604     C                   endif
015500010604     C*
015600010330     C                   else
015700010330     C                   eval      vinflg = '1'
015800010330     C                   endif
015900010601     C*
016000010601     C  N70              update    tivin000
016100010330     C*
016200010330     C  N70              ENDdo
016300010601     C
016400010601     C*
016500010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016600010601     C                   if        cntNonEl = *zeros or
016700010601     C                             flgMulti = '0'
016800010330     C* Se non ci sono record con errori ...
016900010601     C                   if        §ctrno = 0 and
017000010604     C                             §ctrmo = 0 and
017100010601     C                             flgStato <> '2'
017200010330     C* ... restituisco esito OK.
017300010330     C                   eval      wrkesito = '0'
017400010330     C                   else
017500010330     C                   if        §ctrok > 0
017600010330     C                   eval      wrkesito = '1'
017700010330     C                   else
017800010615     C                   if        flgOk = '0'
017900010615     C                   eval      wrkesito = '2'
018000010615     C                   else
018100010615     C                   eval      wrkesito = '6'
018200010615     C                   endif
018300010330     C                   endif
018400010330     C                   endif
018500010601     C                   else
018600010601     C                   eval      wrkesito = '9'
018700010601     C                   endif
018800010330     C*
018900010330     C                   if        %open(tivin00r)
019000010330     C                   close     tivin00r
019100010330     C                   endif
019200021025     C                   if        %open(fivabwwr)
019300021025     C                   close     fivabwwr
019400010330     C                   endif
019500010601     C*
019600010601     C                   if        vlrpoi <> 999
019700010601     C                   eval      invfil = vlrpoi
019800010601     C                   endif
019900010330     C*
020000010330     C                   if        §ctrok > 0
020100010601     C                             and invfil > *zeros
020200010330     C                   exsr      invio
020300010330     C                   endif
020400010601     C*
020500010618     C                   if        flgGiro = '1'
020600010601     C                   exsr      endela
020700010618     C                   endif
020800010330     C*
020900010330     C                   ENDSR
021000010330     C***
021100010601
021200010601
021300010601
021400010330     C*----------------------------------------------------*
021500010330     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021600010330     C*----------------------------------------------------*
021700010330     C     INZVAR        BEGSR
021800010330     C*
021900020204     C                   Z-ADD     *zeros        Num5_0            5 0
022000030606     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022100030606     C                   MOVEL     '0'           FlgCAS            1
022200010330     C*
022300010330     C                   ENDSR
022400010330     C*----------------------------------------------------*
022500010330     C*  IMPOSTAZIONE CAMPI COSTANTI
022600010330     C*----------------------------------------------------*
022700010330     C     DEFCAM        BEGSR
022800010330     C*
022900020204     C* Inizializzo il buffer del record da scrivere
023000021025     C                   CLEAR                   FIVAB000
023100020204     C* Imposto i valori di default...
023200031017     C                   EVAL      VABCCM = 0501586
023300031017     C                   EVAL      VABLNP = 050
023400031013     C                   EVAL      VABCTR = 000
023500020204     C* ... e poi verifico se sono stati passati come parametri
023600020204     C                   IF        vlrppt > *blanks
023700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023800020204     C                   EXSR      CHKNUM
023900020204     C                   IF        PiInt=*on
024000020204     C                   Z-ADD     PiVal         VABCCM
024100020204     C                   ENDIF
024200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024300020204     C                   EXSR      CHKNUM
024400020204     C                   IF        PiInt=*on
024500020204     C                   Z-ADD     PiVal         VABLNP
024600020204     C                   ENDIF
024700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
024800020204     C                   EXSR      CHKNUM
024900020204     C                   IF        PiInt=*on
025000020204     C                   Z-ADD     PiVal         VABCTR
025100020204     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C*
025400010330     C                   ENDSR
025500010607     C*----------------------------------------------------*
025600021025     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
025700010607     C*----------------------------------------------------*
025800010607     C     IMPVAB        BEGSR
025900010607     C*
026000010607     C                   SETOFF                                       3132
026100030515     C*
026200010607     C                   MOVEL     datcor        VABAAS
026300010607     C                   MOVE      datcor        VABMGS
026400010607     C*
026500010607     C* Reperimento campi ALFA
026600010607     C*
026700010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
026800031017     C                   EVAL      VABRSD=%trim(%subst(vindta:280:40))
026900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
027000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
027100020117     C* ==
027200031017     C                   EVAL      VABRD2=%trim(%subst(vindta:320:40))
027300031017     C                   EVAL      VABIND=%trim(%subst(vindta:360:40))
027400031107     C                   EVAL      VABLOD=%trim(%subst(vindta:440:30))
027500031107     C*
027600031107     C* Effettuo alcune considerazioni sulla località x estrapolare la provincia racchiusa tra ()
027700031107     C                   Z-ADD     *zeros        pos               3 0
027800031107     C                   EVAL      pos = %scan('(':VABLOD)
027900031107     C                   IF        pos > *zeros
028000031107     C                   EVAL      VABPRD=%trim(%subst(vindta:(440+pos):2))
028100031107     C                   EVAL      VABLOD=%trim(%subst(vindta:440:(pos-1)))
028200031107     C                   ENDIF
028300031107     C*
028400031017     C                   EVAL      VABNOT='Tel: '+%trim(%subst(vindta:477:15))
028500031017     C                   EVAL      VABRMA=%trim(%subst(vindta:528:12))
028600010607     C*
028700010607     C* Reperimento campi NUMERICI
028800020604     C*
028900030515     C* CAD
029000031017     C                   EVAL      PiStr=%trim(%subst(vindta:470:7))
029100010607     C                   EXSR      CHKNUM
029200010607     C                   IF        PiInt=*on
029300020204     C                   Z-ADD     PiVal         Num5_0
029400020204     C                   MOVEL(p)  Num5_0        VABCAD
029500010607     C                   ELSE
029600010607     C                   SETON                                        32
029700020204     C                   EVAL      VABCAD = *zeros
029800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
029900020204     C                             + ' ' + 'VABCAD'
030000010607     C                   ENDIF
030100031107     C* Se la provincia nn è ancora valorizzata reperisco la provincia dal CAP e dalla località
030200031107     C                   IF        VABPRD =  *blanks
030300031013     C                   IF        VABCAD <> *blanks
030400031013     C                   CLEAR                   TISI95DS
030500031013     C                   EVAL      I95TCN = '3'
030600031013     C                   Z-ADD     datcor        I95DAT
030700031013     C                   EVAL      I95CAP = VABCAD
030800031013     C                   EVAL      I95LOC = VABLOD
030900031013     C                   CALL      'TISI95R'
031000031013     C                   PARM                    TISI95DS
031100031013     C                   EVAL      VABPRD = O95PRV
031200031013     C                   ENDIF
031300031107     C                   ENDIF
031400030515     C* RMN/NSP
031500031017     C                   EVAL      PiStr=%trim(%subst(vindta:528:12))
031600011023     C                   EXSR      CHKNUM
031700011023     C                   IF        PiInt=*on
031800030225     C                   Z-ADD     PiVal         VABRMN
031900031013     C                   MOVE(P)   VABRMN        VABNSP
032000011023     C                   ELSE
032100020205     C                   EVAL      VABRMN = 1
032200011023     C                   ENDIF
032300030515     C* NCL
032400031017     C                   EVAL      PiStr=%trim(%subst(vindta:575:5))
032500010607     C                   EXSR      CHKNUM
032600010607     C                   IF        PiInt=*on
032700010607     C                   Z-ADD     PiVal         VABNCL
032800010607     C                   ELSE
032900010607     C                   SETON                                        32
033000010607     C                   Z-ADD     *zeros        VABNCL
033100010607     C                   EVAL      vinmsg = %trimr(vinmsg)
033200010607     C                             + ' ' + 'VABNCL'
033300010607     C                   ENDIF
033400031013     C* PKB
033500031017     C                   EVAL      PiStr=%trim(%subst(vindta:569:6))
033600031013     C                   EXSR      CHKNUM
033700031017     C                   IF        PiNum=*on
033800031017     C                   EVAL(H)   VABPKB = PiVal/10                            * gestisco 1 dec.
033900031013     C                   ELSE
034000031013     C                   SETON                                        32
034100031013     C                   Z-ADD     *zeros        VABPKB
034200031013     C                   EVAL      vinmsg = %trimr(vinmsg)
034300031013     C                             + ' ' + 'VABPKB'
034400031013     C                   ENDIF
034500031017     C* VLB
034600031017     C                   EVAL      PiStr=%trim(%subst(vindta:580:4))
034700031017     C                   EXSR      CHKNUM
034800031017     C                   IF        PiNum=*on
034900031017     C                   EVAL(H)   VABVLB = PiVal/100                           * gestisco 2 dec.
035000031017     C                   ELSE
035100031017     C                   SETON                                        32
035200031017     C                   Z-ADD     *zeros        VABVLB
035300031017     C                   EVAL      vinmsg = %trimr(vinmsg)
035400031017     C                             + ' ' + 'VABVLB'
035500031017     C                   ENDIF
035600031017     C* CAS
035700031020     C                   IF        %subst(vindta:597:9) <> *zeros
035800031020     C                   EVAL      PiStr=%trim(%subst(vindta:597:9))
035900031017     C                   EVAL      FlgCAS = '1'
036000031017     C                   EXSR      CHKNUM
036100031017     C                   IF        PiNum=*on
036200031017     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 dec.
036300031029     C                   EVAL      VABVCA = 'EUR'
036400031017     C                   ELSE
036500031017     C                   SETON                                        32
036600031017     C                   Z-ADD     *zeros        VABCAS
036700031017     C                   EVAL      vinmsg = %trimr(vinmsg)
036800031017     C                             + ' ' + 'VABCAS'
036900031017     C                   ENDIF
037000031017     C                   ENDIF
037100030929     C*
037200030929     C* Considerazioni finali su CBO/CAS
037300030929     C                   IF        FlgCAS = '1'
037400030929     C                   EVAL      VABCBO = '4'
037500030929     C                   ELSE
037600030929     C                   EVAL      VABCBO = '1'
037700030929     C                   ENDIF
037800020204     C*
037900020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038000020204     C                   EXSR      CHKIMPDIV
038100020204     C*
038200010607     C                   ENDSR
038300010607     C*----------------------------------------------------*
038400010601
038500020204
038600020204     C*----------------------------------------------------*
038700020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
038800020204     C*----------------------------------------------------*
038900020204     C     CHKIMPDIV     BEGSR
039000020204     C*
039100020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
039200020204     C                   Z-ADD     *zeros        wrkDec            9 9
039300020204     C*
039400020204     C* Come prima cosa effettuo considerazioni sulla divisa
039500020204     C                   IF        vabIAS > *zeros
039600020204     C                   IF        vabVAS <> 'EUR'
039700020204     C                   EVAL      vabVAS =  'ITL'
039800020204     C                   ENDIF
039900020204     C                   ENDIF
040000020204     C*
040100020204     C                   IF        vabCAS > *zeros
040200020204     C                   IF        vabVCA <> 'EUR'
040300020204     C                   EVAL      vabVCA =  'ITL'
040400020204     C                   ENDIF
040500020204     C                   ENDIF
040600020204     C*
040700020204     C                   IF        vabVMD > *zeros
040800020204     C                   IF        vabVAD <> 'EUR'
040900020204     C                   EVAL      vabVAD =  'ITL'
041000020204     C                   ENDIF
041100020204     C                   ENDIF
041200020204     C*
041300020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
041400020204     C                   Z-ADD     vabIAS        wrkDec
041500020204     C                   IF        wrkDec > *zeros
041600020204     C                   IF        vabVAS = 'ITL'
041700020204     C                   EVAL      vabIAS = *zeros
041800020204     C                   ENDIF
041900020204     C                   ENDIF
042000020204     C*
042100020204     C* Stabilisco se il contrasegno ha decimali valorizzati
042200020204     C                   Z-ADD     vabCAS        wrkDec
042300020204     C                   IF        wrkDec > *zeros
042400020204     C                   IF        vabVCA = 'ITL'
042500020204     C                   EVAL      vabCAS = *zeros
042600020204     C                   ENDIF
042700020204     C                   ENDIF
042800020204     C*
042900020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
043000020204     C                   Z-ADD     vabVMD        wrkDec
043100020204     C                   IF        wrkDec > *zeros
043200020204     C                   IF        vabVAD = 'ITL'
043300020204     C                   EVAL      vabVMD = *zeros
043400020204     C                   ENDIF
043500020204     C                   ENDIF
043600020204     C*
043700020204     C                   ENDSR
043800020204     C***
043900020204
044000010330
044100010330
044200010330     C*----------------------------------------------------*
044300010330     C*  CONTROLLO NUMERICITA' CAMPI
044400010330     C*----------------------------------------------------*
044500010330     C     CHKNUM        BEGSR
044600010330     C*
044700010606     C                   IF        PiDecChr = *blanks
044800030606     C                   EVAL      PiDecChr = ','
044900010606     C                   ENDIF
045000010606     C*
045100010606     C                   CALL(e)   'ISNUMERIC'
045200010330     C                   PARM                    PiStr            30
045300010606     C                   PARM                    PiDecChr          1
045400010330     C                   PARM      *ZEROS        PiVal            30 9
045500010330     C                   PARM      '0'           PiInt             1
045600010330     C                   PARM      '0'           PiNum             1
045700010330     C                   IF        %error
045800010606     C                   EVAL      PiNum=*off
045900010330     C                   ENDIF
046000010330     C*
046100010330     C                   ENDSR
046200010330     C***
046300010330
046400010601
046500010601
046600010601
046700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
046800010601     C     repfil        BEGSR
046900010601     C*
047000010601     C                   if        invfil = *zeros and
047100010601     C                             depfil > *zeros and
047200010629     C                             (vinflg = *blanks or
047300010629     C                              vinflg = *zeros)
047400010601     C
047500010601     C                   eval      invfil = depfil
047600010601     C                   endif
047700010601     C*
047800010601     C                   if        depfil <> invfil and
047900010601     C                             invfil > *zeros
048000010601     C                   eval      flgMulti = '1'
048100010601     C                   if        vinflg = *blanks
048200010601     C                   add       1             cntNonEl
048300010601     C                   endif
048400010601     C                   endif
048500010601     C*
048600010601     C                   if        vinflg = '2'
048700010601     C                   eval      flgStato = '2'
048800010601     C                   endif
048900010601     C*
049000010601     C                   ENDSR
049100010601     C***
049200010601
049300010601
049400010601
049500010330
049600010330
049700010330
049800990920      /TITLE Invio dei dati al punto operativo.
049900000613     C     invio         BEGSR
050000990920     C*
050100990920     C                   reset                   dscmz
050200010601     C                   move      invfil        cmzdst
050300990920     C                   eval      cmzfld = vlrfou
050400990920     C                   eval      cmzmbd = vlrhdl
050500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
050600000710     C                   if        prmfir = *blanks
050700021025     C                   eval      cmzfla = 'FIVAB00F'
050800021025     C                   eval      cmzmba = 'FIVAB00F'
050900000710     C                   else
051000000710     C                   eval      cmzfla = prmfir
051100000710     C                   eval      cmzmba = prmfir
051200000710     C                   endif
051300990920     C                   eval      cmznrr = *zeros
051400990920     C                   move      §ctrok        cmznrr
051500021018     C                   eval      cmzlba = vlrfl1
051600990920     C                   call(e)   'TIS711C'
051700990920     C                   parm                    dscmz
051800990921     C                   parm      *blanks       esito
051900990920     C                   if        %error
052000990920     C                             or cmzerr = '1'
052100990921     C                             or esito  = '1'
052200000710     C                   eval      wrkesito = '3'
052300990920     C                   endif
052400990920     C*
052500000613     C                   ENDSR
052600990910
052700010601
052800010601
052900010601
053000010601
053100010601      /TITLE Invio dei dati al punto operativo.
053200010601     C     opeini        BEGSR
053300010601     C*
053400010601     C* Inizializzo flag e contatori operativi
053500010601     C                   movel     '0'           flgGiro           1
053600010601     C                   movel     '0'           flgMulti          1
053700010601     C                   movel     '1'           flgStato          1
053800010615     C                   movel     '0'           flgOk             1
053900010601     C                   z-add     *zeros        cntNonEl         10 0
054000010601     C                   z-add     *zeros        depfil            3 0
054100010601     C                   z-add     *zeros        invfil            3 0
054200010601     C*
054300010601     C                   ENDSR
054400010601     C***
054500010601
054600010601
054700010601
054800010330
054900010330
055000000613     C     *inzsr        BEGSR
055100990910     C*
055200990910     C     *entry        plist
055300990920     C                   parm                    tivlrds
055400990921     C                   parm      wrkesito      esito
055500000724     C                   parm                    prmlit
055600000710     C                   parm                    prmfir
055700010330     C*
055800010330     C* CALCOLA LA DATA CORRENTE
055900010330     C                   time                    wn14             14 0
056000010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
056100010330     C                   z-add     wn8           g08dat
056200010330     C                   z-add     *zeros        g08inv
056300010330     C                   movel     '0'           g08err
056400010330     C                   call      'XSRDA8'
056500010330     C                   parm                    wlbda8
056600010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
056700000613     C*
056800000613     C                   ENDSR
056900000613     C***
