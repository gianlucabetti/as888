000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200171006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600000313     D*
000700000613     D*------------
000800000613     D* COMANDI
000900000613     D*------------
001000011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800070502     D tisi95ds      e ds
001900010330     D esito           s              1
002000010330     D prmlit          s             10
002100010330     D prmfir          s             10
002200010330     D wrkesito        s                   like(esito)
002300010330     D wrkdata         s               d
002400010330     D wrkora          s               t
002500010330     D rrnum           s              6  0 INZ(*zeros)
002600011113     D depcmd          s            150    INZ(*blanks)
002700010330     D*------------------
002800010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002900010330     D*------------------
003000010330     D WLBDA8          DS                  INZ
003100010330     D  G08DAT                 1      8  0
003200010330     D  G08INV                 9     16  0
003300010330     D  G08ERR                17     17
003400010330     D  G08TGI                18     22  0
003500010330
003600010330
003700990915     C                   time                    wrkdata
003800990915     C                   time                    wrkora
003900000913     C                   reset                   rrnum
004000990921     C                   reset                   esito
004100990921     C                   reset                   wrkesito
004200010601     C*
004300010601     C                   exsr      opeini
004400010605     C                   exsr      rwvab
004500070502     C*
004600070502     C* Effettuo lancio TISI95 solo x chiusura
004700070502     C                   CLEAR                   TISI95DS
004800070502     C                   EVAL      I95TLA = 'C'
004900070502     C                   CALL      'TISI95R'
005000070502     C                   PARM                    TISI95DS
005100010601     C*
005200010601     C                   seton                                        lr
005300010601
005400010601
005500010601
005600010601
005700010601     C*--------------------------------------------------------
005800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005900010601     C*--------------------------------------------------------
006000010601     C     PREELA        BEGSR
006100010601     C*
006200010601     C* SE OCCORRE SPEDIRE IN FILIALE
006300010601     C                   if        invfil <> *zeros and
006400010601     C                             flgGiro = '0'
006500010601     C*
006600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006700010601     C                   eval      flgGiro = '1'
006800010601     C*
006900010601     C                   endif
007000010601     C*
007100010601     C                   ENDSR
007200010601     C***
007300010601
007400010601
007500010601
007600010601     C*--------------------------------------------------------
007700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007800010601     C*--------------------------------------------------------
007900010601     C     ENDELA        BEGSR
008000990915     C*
008100000616     C*
008200010601     C                   ENDSR
008300010601     C***
008400000613
008500010601
008600010601
008700010330     C*--------------------------------------------------------
008800050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
008900010330     C*--------------------------------------------------------
009000010605     C     RWVAB         BEGSR
009100050408     C*
009200010330     C                   if        not %open(tivin00r)
009300010330     C                   open      tivin00r
009400010330     C                   endif
009500050408     C                   if        not %open(fivabwwr)
009600050408     C                   open      fivabwwr
009700010330     C                   endif
009800010330     C*
009900010604     C                   clear                   §CTROK
010000010604     C                   clear                   §CTRMO
010100010604     C                   clear                   §CTRNO
010200010330     C*
010300010330     C                   DO        *HIVAL
010400010330     C*
010500010330     C                   READ      tivin00r                               70
010600010618     C*
010700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010800010618     C                   if        vinflg = '1'
010900010618     C                   eval      flgOk = '1'
011000010618     C                   endif
011100010618     C*
011200010330     C                   if        vindta > *blanks
011300010330     C                   add       1             rrnum
011400010330     C*
011500010601     C                   if        *in70 = *off and
011600010330     C                             (vinflg = *blanks
011700010330     C                              or vinflg = '0'
011800010330     C                              or vinflg = '2')
011900010330     C*
012000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012100010711     C                   if        vinflg = *blanks or vinflg = '0'
012200010711     C                   clear                   vinmsg
012300010711     C                   endif
012400070601     C*
012500070601     C                   exsr      inzvar
012600070601     C                   exsr      defcam
012700070601     C                   exsr      impvab
012800010601     C*
012900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
013000010605     C                   eval      depfil = VABLNP
013100010601     C                   exsr      repfil
013200010601     C                   if        depfil = invfil
013300021025     C                   if        vlrpoi = 999
013400021025     C                   MOVE(P)   invfil        VABFGS
013500021025     C                   else
013600021025     C                   MOVE(P)   vlrpoi        VABFGS
013700021025     C                   endif
013800010601     C*
013900010601     C                   exsr      PREELA
014000010601     C*
014100010604     C* Ebbene...
014200010604     C*
014300010604     C  N31              ADD       1             §CTROK            7 0
014400010604     C   32              ADD       1             §CTRMO            7 0
014500010604     C   31              ADD       1             §CTRNO            7 0
014600071003     C                   exsr      wrivab                                       => scrivo VAB
014700010604     C*
014800020722     C                   endif
014900020722     C*
015000010604     C                   if        *in31 = *off and
015100010604     C                             *in32 = *off
015200010604     C                   eval      vinflg = '1'
015300010604     C                   else
015400010604     C                   eval      vinflg = '2'
015500010604     C                   endif
015600010604     C                   endif
015700010604     C*
015800010330     C                   else
015900010330     C                   eval      vinflg = '1'
016000010330     C                   endif
016100010601     C*
016200010601     C  N70              update    tivin000
016300010330     C*
016400010330     C  N70              ENDdo
016500010601     C*
016600010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016700010601     C                   if        cntNonEl = *zeros or
016800010601     C                             flgMulti = '0'
016900010330     C* Se non ci sono record con errori ...
017000010601     C                   if        §ctrno = 0 and
017100010604     C                             §ctrmo = 0 and
017200010601     C                             flgStato <> '2'
017300010330     C* ... restituisco esito OK.
017400010330     C                   eval      wrkesito = '0'
017500010330     C                   else
017600010330     C                   if        §ctrok > 0
017700010330     C                   eval      wrkesito = '1'
017800010330     C                   else
017900010615     C                   if        flgOk = '0'
018000010615     C                   eval      wrkesito = '2'
018100010615     C                   else
018200010615     C                   eval      wrkesito = '6'
018300010615     C                   endif
018400010330     C                   endif
018500010330     C                   endif
018600010601     C                   else
018700010601     C                   eval      wrkesito = '9'
018800010601     C                   endif
018900010330     C*
019000010330     C                   if        %open(tivin00r)
019100010330     C                   close     tivin00r
019200010330     C                   endif
019300050408     C                   if        %open(fivabwwr)
019400050408     C                   close     fivabwwr
019500010330     C                   endif
019600010601     C*
019700010601     C                   if        vlrpoi <> 999
019800010601     C                   eval      invfil = vlrpoi
019900010601     C                   endif
020000010330     C*
020100010330     C                   if        §ctrok > 0
020200010601     C                             and invfil > *zeros
020300010330     C                   exsr      invio
020400010330     C                   endif
020500010601     C*
020600010618     C                   if        flgGiro = '1'
020700010601     C                   exsr      endela
020800010618     C                   endif
020900010330     C*
021000010330     C                   ENDSR
021100010330     C***
021200070601
021300070601     C*----------------------------------------------------*
021400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
021500070601     C*----------------------------------------------------*
021600070601     C     WRIVAB        BEGSR
021700080321     C*
021800070601     C  N31              WRITE     FIVAB000
021900070601     C*
022000070601     C                   ENDSR
022100010601
022200010601
022300010601
022400010330     C*----------------------------------------------------*
022500020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022600010330     C*----------------------------------------------------*
022700010330     C     INZVAR        BEGSR
022800010330     C*
022900020204     C                   Z-ADD     *zeros        Num5_0            5 0
023000020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023100020322     C                   MOVEL     '0'           FlgCAS            1
023200010330     C*
023300010330     C                   ENDSR
023400010330     C*----------------------------------------------------*
023500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023600010330     C*----------------------------------------------------*
023700010330     C     DEFCAM        BEGSR
023800010330     C*
023900020204     C* Inizializzo il buffer del record da scrivere
024000050408     C                   CLEAR                   FIVAB000
024100020204     C* Imposto i valori di default...
024200080415     C                   EVAL      VABCCM = 1310416
024300080415     C                   EVAL      VABLNP = 275
024400080415     C                   EVAL      VABCTR = 000
024500070208     C                   EVAL      VABCBO = '1'
024600080410     C                   EVAL      VABCTM = '7Q'
024700020204     C* ... e poi verifico se sono stati passati come parametri
024800020204     C                   IF        vlrppt > *blanks
024900040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025000020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025100020204     C                   EXSR      CHKNUM
025200020204     C                   IF        PiInt=*on
025300020204     C                   Z-ADD     PiVal         VABCCM
025400020204     C                   ENDIF
025500040301     C                   ENDIF
025600040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025800020204     C                   EXSR      CHKNUM
025900020204     C                   IF        PiInt=*on
026000020204     C                   Z-ADD     PiVal         VABLNP
026100020204     C                   ENDIF
026200040301     C                   ENDIF
026300040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026500020204     C                   EXSR      CHKNUM
026600020204     C                   IF        PiInt=*on
026700020204     C                   Z-ADD     PiVal         VABCTR
026800040301     C                   ENDIF
026900020204     C                   ENDIF
027000020204     C                   ENDIF
027100020204     C*
027200010330     C                   ENDSR
027300010607     C*----------------------------------------------------*
027400050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027500010607     C*----------------------------------------------------*
027600010607     C     IMPVAB        BEGSR
027700010607     C*
027800010607     C                   SETOFF                                       3132
027900010607     C*
028000010607     C* Reperimento campi ALFA
028100080415     C                   EVAL      VABRSD=%trim(%subst(vindta:156:35))
028200080415     C                   EVAL      VABIND=%trim(%subst(vindta:226:35))
028300080415     C                   EVAL      VABLOD=%trim(%subst(vindta:261:35))
028400080415     C                   EVAL      VABPRD=%trim(%subst(vindta:323:2))
028500080415     C                   EVAL      VABNZD=%trim(%subst(vindta:325:4))
028600080415     C                   EVAL      VABRMA=%trim(%subst(vindta:69:6))
028700080408     C*
028800080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
028900080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029000080408     C     '@':'A'       XLATE     VABRSD        VABRSD
029100080408     C* ==
029200060510     C*
029300060510     C* Reperimento campi NUMERICI
029400060510     C                   MOVEL     DATCOR        VABAAS
029500060510     C                   MOVE      DATCOR        VABMGS
029600080415     C* NSP
029700080415     C                   EVAL      PiStr=%trim(%subst(vindta:88:6))
029800060510     C                   EXSR      CHKNUM
029900060510     C                   IF        PiInt=*on
030000060510     C                   Z-ADD     PiVal         VABNSP
030100060404     C                   ELSE
030200070502     C                   SETON                                        31
030300060510     C                   Z-ADD     *zeros        VABNSP
030400060404     C                   EVAL      vinmsg = %trimr(vinmsg)
030500080410     C                             + ' ' + 'VABNSP'
030600060404     C                   ENDIF
030700080410     C* RMN
030800080415     C                   EVAL      PiStr=%trim(%subst(vindta:88:6))
030900080410     C                   EXSR      CHKNUM
031000080410     C                   IF        PiInt=*on
031100080410     C                   Z-ADD     PiVal         VABRMN
031200080410     C                   ELSE
031300080410     C                   SETON                                        31
031400080410     C                   Z-ADD     1             VABRMN
031500080410     C                   EVAL      vinmsg = %trimr(vinmsg)
031600080410     C                             + ' ' + 'VABRMN'
031700080410     C                   ENDIF
031800080408     C* CAD
031900080415     C                   EVAL      PiStr=%trim(%subst(vindta:316:7))
032000080408     C                   EXSR      CHKNUM
032100080408     C                   IF        PiInt=*on
032200080408     C                   Z-ADD     PiVal         Num5_0
032300080408     C                   MOVEL(p)  Num5_0        VABCAD
032400080408     C                   ELSE
032500080408     C                   SETON                                        32
032600080408     C                   EVAL      VABCAD = *zeros
032700080408     C                   EVAL      vinmsg = %trimr(vinmsg)
032800080408     C                             + ' ' + 'VABCAD'
032900080408     C                   ENDIF
033000080408     C* Reperisco la provincia dal CAP e dalla località
033100080408     C                   IF        VABCAD <> *blanks AND
033200080408     C                             VABPRD  = *blanks
033300080408     C                   CLEAR                   TISI95DS
033400080408     C                   EVAL      I95TCN = '3'
033500080408     C                   Z-ADD     datcor        I95DAT
033600080408     C                   EVAL      I95CAP = VABCAD
033700080408     C                   EVAL      I95LOC = VABLOD
033800080408     C                   EVAL      I95NAR = VABNZD
033900080408     C                   CALL      'TISI95R'
034000080408     C                   PARM                    TISI95DS
034100080408     C                   EVAL      VABPRD = O95PRV
034200080408     C                   ENDIF
034300080415     C* NCL
034400080415     C                   EVAL      PiStr=%trim(%subst(vindta:355:5))
034500080410     C                   EXSR      CHKNUM
034600080410     C                   IF        PiInt=*on
034700080410     C                   Z-ADD     PiVal         VABNCL
034800080410     C                   ELSE
034900080410     C                   SETON                                        32
035000080410     C                   Z-ADD     *zeros        VABNCL
035100080410     C                   EVAL      vinmsg = %trimr(vinmsg)
035200080410     C                             + ' ' + 'VABNCL'
035300080410     C                   ENDIF
035400071029     C* PKB
035500080415     C                   EVAL      PiStr=%trim(%subst(vindta:360:7))
035600010607     C                   EXSR      CHKNUM
035700010607     C                   IF        PiNum=*on
035800080408     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
035900071003     C                   Z-ADD(H)  PiVal         VABPKB
036000010607     C                   ELSE
036100010607     C                   SETON                                        32
036200010607     C                   Z-ADD     *zeros        VABPKB
036300010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036400010607     C                             + ' ' + 'VABPKB'
036500010607     C                   ENDIF
036600080415     C* VLB
036700080415     C                   EVAL      PiStr=%trim(%subst(vindta:385:7))
036800080415     C                   EXSR      CHKNUM
036900080415     C                   IF        PiNum=*on
037000080415     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
037100080415     C                   Z-ADD(H)  PiVal         VABVLB
037200080415     C                   ELSE
037300080415     C                   SETON                                        32
037400080415     C                   Z-ADD     *zeros        VABVLB
037500080415     C                   EVAL      vinmsg = %trimr(vinmsg)
037600080415     C                             + ' ' + 'VABVLB'
037700080415     C                   ENDIF
037800020322     C*
037900020322     C* Considerazioni finali su CBO/CAS
038000050908     C                   IF        FlgCAS = '1'
038100040713     C                   IF        VABCBO = '1'
038200020322     C                   EVAL      VABCBO = '4'
038300040713     C                   ENDIF
038400040713     C                   IF        VABCBO = '2'
038500040713     C                   EVAL      VABCBO = '6'
038600040713     C                   ENDIF
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
045800080409     C                   EVAL      PiDecChr = '.'
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
051700050408     C                   eval      cmzfla = 'FIVAB00F'
051800050408     C                   eval      cmzmba = 'FIVAB00F'
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
