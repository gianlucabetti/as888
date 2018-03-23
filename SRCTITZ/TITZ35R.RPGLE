000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050408     FFIVABWWR  O    E             DISK    usropn
000600010330     D*----------------------------------------------------
000700010330     D* DICHIARAZIOINE VARIABILI DI WRK
000800010330     D*----------------------------------------------------
000900010330     D dscmz         e ds                  inz
001000010330     D psds           sds
001100010330     D  procname         *PROC
001200010330     D tivlrds       e ds                  extname(tivlr00f)
001300070502     D tisi95ds      e ds
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
002700010330
002800010330
002900000913     C                   reset                   rrnum
003000990921     C                   reset                   esito
003100990921     C                   reset                   wrkesito
003200010601     C*
003300010601     C                   exsr      opeini
003400010605     C                   exsr      rwvab
003500070502     C*
003600070502     C* Effettuo lancio TISI95 solo x chiusura
003700070502     C                   CLEAR                   TISI95DS
003800070502     C                   EVAL      I95TLA = 'C'
003900070502     C                   CALL      'TISI95R'
004000070502     C                   PARM                    TISI95DS
004100010601     C*
004200010601     C                   seton                                        lr
004300010601
004400010601
004500010601
004600010601
004700010601     C*--------------------------------------------------------
004800010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
004900010601     C*--------------------------------------------------------
005000010601     C     PREELA        BEGSR
005100010601     C*
005200010601     C* SE OCCORRE SPEDIRE IN FILIALE
005300010601     C                   if        invfil <> *zeros and
005400010601     C                             flgGiro = '0'
005500010601     C*
005600010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
005700010601     C                   eval      flgGiro = '1'
005800010601     C*
005900010601     C                   endif
006000010601     C*
006100010601     C                   ENDSR
006200010601     C***
006300010601
006400010601
006500010601
006600010601     C*--------------------------------------------------------
006700010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
006800010601     C*--------------------------------------------------------
006900010601     C     ENDELA        BEGSR
007000000616     C*
007100010601     C                   ENDSR
007200010601     C***
007300120525
007400010330     C*--------------------------------------------------------
007500050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
007600010330     C*--------------------------------------------------------
007700010605     C     RWVAB         BEGSR
007800050408     C*
007900010330     C                   if        not %open(tivin00r)
008000010330     C                   open      tivin00r
008100010330     C                   endif
008200050408     C                   if        not %open(fivabwwr)
008300050408     C                   open      fivabwwr
008400010330     C                   endif
008500010330     C*
008600010604     C                   clear                   §CTROK
008700010604     C                   clear                   §CTRMO
008800010604     C                   clear                   §CTRNO
008900010330     C*
009000010330     C                   DO        *HIVAL
009100010330     C*
009200010330     C                   READ      tivin00r                               70
009300010618     C*
009400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009500010618     C                   if        vinflg = '1'
009600010618     C                   eval      flgOk = '1'
009700010618     C                   endif
009800010618     C*
009900010330     C                   if        vindta > *blanks
010000010330     C                   add       1             rrnum
010100010330     C*
010200010601     C                   if        *in70 = *off and
010300010330     C                             (vinflg = *blanks
010400010330     C                              or vinflg = '0'
010500010330     C                              or vinflg = '2')
010600010330     C*
010700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
010800010711     C                   if        vinflg = *blanks or vinflg = '0'
010900010711     C                   clear                   vinmsg
011000010711     C                   endif
011100070601     C*
011200070601     C                   exsr      inzvar
011300070601     C                   exsr      defcam
011400070601     C                   exsr      impvab
011500010601     C*
011600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
011700010605     C                   eval      depfil = VABLNP
011800010601     C                   exsr      repfil
011900010601     C                   if        depfil = invfil
012000021025     C                   if        vlrpoi = 999
012100021025     C                   MOVE(P)   invfil        VABFGS
012200021025     C                   else
012300021025     C                   MOVE(P)   vlrpoi        VABFGS
012400021025     C                   endif
012500010601     C*
012600010601     C                   exsr      PREELA
012700010601     C*
012800010604     C* Ebbene...
012900010604     C*
013000010604     C  N31              ADD       1             §CTROK            7 0
013100010604     C   32              ADD       1             §CTRMO            7 0
013200010604     C   31              ADD       1             §CTRNO            7 0
013300071003     C                   exsr      wrivab                                       => scrivo VAB
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
018000050408     C                   if        %open(fivabwwr)
018100050408     C                   close     fivabwwr
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
019900070601
020000070601     C*----------------------------------------------------*
020100070601     C*  SCARICAMENTO BUFFER RECORDS VAB
020200070601     C*----------------------------------------------------*
020300070601     C     WRIVAB        BEGSR
020400080321     C*
020500070601     C  N31              WRITE     FIVAB000
020600070601     C*
020700070601     C                   ENDSR
020800010601
020900010601
021000010601
021100010330     C*----------------------------------------------------*
021200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
021300010330     C*----------------------------------------------------*
021400010330     C     INZVAR        BEGSR
021500010330     C*
021600020204     C                   Z-ADD     *zeros        Num5_0            5 0
021700020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
021800020322     C                   MOVEL     '0'           FlgCAS            1
021900010330     C*
022000010330     C                   ENDSR
022100010330     C*----------------------------------------------------*
022200020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022300010330     C*----------------------------------------------------*
022400010330     C     DEFCAM        BEGSR
022500010330     C*
022600020204     C* Inizializzo il buffer del record da scrivere
022700050408     C                   CLEAR                   FIVAB000
022800020204     C* Imposto i valori di default...
022900120528     C                   EVAL      VABCCM = 0636036
023000120528     C                   EVAL      VABLNP = 255
023100120528     C                   EVAL      VABCTR = 300
023200070208     C                   EVAL      VABCBO = '1'
023300120607     C                   EVAL      VABCTM = *blank
023400020204     C* ... e poi verifico se sono stati passati come parametri
023500020204     C                   IF        vlrppt > *blanks
023600040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
023700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
023800020204     C                   EXSR      CHKNUM
023900020204     C                   IF        PiInt=*on
024000020204     C                   Z-ADD     PiVal         VABCCM
024100020204     C                   ENDIF
024200040301     C                   ENDIF
024300040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
024400020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
024500020204     C                   EXSR      CHKNUM
024600020204     C                   IF        PiInt=*on
024700020204     C                   Z-ADD     PiVal         VABLNP
024800020204     C                   ENDIF
024900040301     C                   ENDIF
025000040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
025100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
025200020204     C                   EXSR      CHKNUM
025300020204     C                   IF        PiInt=*on
025400020204     C                   Z-ADD     PiVal         VABCTR
025500040301     C                   ENDIF
025600020204     C                   ENDIF
025700020204     C                   ENDIF
025800020204     C*
025900010330     C                   ENDSR
026000010607     C*----------------------------------------------------*
026100050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
026200010607     C*----------------------------------------------------*
026300010607     C     IMPVAB        BEGSR
026400010607     C*
026500010607     C                   SETOFF                                       3132
026600010607     C*
026700010607     C* Reperimento campi ALFA
026800120528     C                   EVAL      VABCBO=%trim(%subst(vindta:35:2))
026900120528     C                   EVAL      VABRSD=%trim(%subst(vindta:41:35))
027000120528     C                   EVAL      VABRD2=%trim(%subst(vindta:76:35))
027100120528     C                   EVAL      VABIND=%trim(%subst(vindta:111:35))
027200120528     C                   EVAL      VABCAD=%trim(%subst(vindta:146:9))
027300120528     C                   EVAL      VABLOD=%trim(%subst(vindta:155:35))
027400120528     C                   EVAL      VABPRD=%trim(%subst(vindta:190:2))
027500120528     C                   EVAL      VABNZD=%trim(%subst(vindta:192:3))
027600120528     C                   EVAL      VABGC1=%trim(%subst(vindta:195:2))
027700120528     C                   EVAL      VABGC2=%trim(%subst(vindta:197:2))
027800120528     C                   EVAL      VABTSP=%trim(%subst(vindta:203:1))
027900120528     C                   EVAL      VABNAS=%trim(%subst(vindta:222:15))
028000120528     C                   EVAL      VABRMA=%trim(%subst(vindta:311:15))
028100120607     C                   EVAL      VABNOT=%trim(%subst(vindta:343:35)) + ' ' +
028200120607     C                                    %trim(%subst(vindta:378:35)) + ' ' +
028300120607     C                                    %trim(%subst(vindta:511:35))
028400120528     C                   EVAL      VABFFD=%trim(%subst(vindta:418:1))
028500120528     C                   EVAL      VABTC1=%trim(%subst(vindta:461:1))
028600120528     C                   EVAL      VABTC2=%trim(%subst(vindta:462:1))
028700120528     C                   EVAL      VABRMO=%trim(%subst(vindta:474:25))
028800120528     C                   EVAL      VABCMO=%trim(%subst(vindta:499:9))
028900120528     C                   EVAL      VABNMO=%trim(%subst(vindta:508:3))
029000120528     C                   EVAL      VABTCR=%trim(%subst(vindta:428:1))
029100120528     C* Reperisco la provincia dal CAP e dalla località
029200120528     C                   IF        VABCAD <> *blanks AND
029300120528     C                             VABPRD  = *blanks
029400120528     C                   CLEAR                   TISI95DS
029500120528     C                   EVAL      I95TCN = '3'
029600120528     C                   Z-ADD     datcor        I95DAT
029700120528     C                   EVAL      I95CAP = VABCAD
029800120528     C                   EVAL      I95LOC = VABLOD
029900120528     C                   EVAL      I95NAR = VABNZD
030000120528     C                   CALL      'TISI95R'
030100120528     C                   PARM                    TISI95DS
030200120528     C                   EVAL      VABPRD = O95PRV
030300120528     C                   ENDIF
030400080408     C*
030500080408     C* Considerazioni sulla ragione sociale del destinatario da indicare
030600080408     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030700080408     C     '@':'A'       XLATE     VABRSD        VABRSD
030800080408     C* ==
030900060510     C*
031000060510     C* Reperimento campi NUMERICI
031100060510     C                   MOVEL     DATCOR        VABAAS
031200060510     C                   MOVE      DATCOR        VABMGS
031300120620     C* RMN
031400120620     C                   EVAL      PiStr=%trim(%subst(vindta:554:12))
031500120620     C                   EXSR      CHKNUM
031600120620     C                   IF        PiInt=*on
031700120620     C                   Z-ADD     PiVal         VABRMN
031800120620     C                   ELSE
031900120620     C                   SETON                                        32
032000120620     C                   Z-ADD     *zeros        VABRMN
032100120620     C                   EVAL      vinmsg = %trimr(vinmsg)
032200120620     C                             + ' ' + 'VABRMN'
032300120620     C                   ENDIF
032400080410     C* NSP
032500120528     C                   EVAL      PiStr=%trim(%subst(vindta:27:7))
032600060510     C                   EXSR      CHKNUM
032700060510     C                   IF        PiInt=*on
032800060510     C                   Z-ADD     PiVal         VABNSP
032900060404     C                   ELSE
033000070502     C                   SETON                                        31
033100060510     C                   Z-ADD     *zeros        VABNSP
033200060404     C                   EVAL      vinmsg = %trimr(vinmsg)
033300080410     C                             + ' ' + 'VABNSP'
033400060404     C                   ENDIF
033500120528     C* IAS
033600120528     C                   IF        %trim(%subst(vindta:204:15))  <> *blank
033700120528     C                   EVAL      PiStr=%trim(%subst(vindta:204:15))
033800120528     C                   EXSR      CHKNUM
033900120528     C                   IF        PiNum=*on
034000120528     C                   Z-ADD(H)  PiVal         VABIAS
034100120528     C                   EVAL      VABVAS = %trim(%subst(vindta:219:3))
034200120528     C                   ELSE
034300120528     C                   SETON                                        32
034400120528     C                   Z-ADD     *zeros        VABIAS
034500120528     C                   EVAL      vinmsg = %trimr(vinmsg)
034600120528     C                             + ' ' + 'VABIAS'
034700120528     C                   ENDIF
034800120528     C                   ENDIF
034900080410     C* NCL
035000120528     C                   EVAL      PiStr=%trim(%subst(vindta:237:6))
035100080410     C                   EXSR      CHKNUM
035200080410     C                   IF        PiInt=*on
035300080410     C                   Z-ADD     PiVal         VABNCL
035400080410     C                   ELSE
035500080410     C                   SETON                                        32
035600080410     C                   Z-ADD     *zeros        VABNCL
035700080410     C                   EVAL      vinmsg = %trimr(vinmsg)
035800080410     C                             + ' ' + 'VABNCL'
035900080410     C                   ENDIF
036000071029     C* PKB
036100120528     C                   EVAL      PiStr=%trim(%subst(vindta:243:9))
036200010607     C                   EXSR      CHKNUM
036300010607     C                   IF        PiNum=*on
036400071003     C                   Z-ADD(H)  PiVal         VABPKB
036500010607     C                   ELSE
036600010607     C                   SETON                                        32
036700010607     C                   Z-ADD     *zeros        VABPKB
036800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
036900010607     C                             + ' ' + 'VABPKB'
037000010607     C                   ENDIF
037100120528     C* VLB
037200120528     C                   EVAL      PiStr=%trim(%subst(vindta:252:7))
037300120528     C                   EXSR      CHKNUM
037400120528     C                   IF        PiNum=*on
037500120528     C                   Z-ADD(H)  PiVal         VABVLB
037600120528     C                   ELSE
037700120528     C                   SETON                                        32
037800120528     C                   Z-ADD     *zeros        VABVLB
037900120528     C                   EVAL      vinmsg = %trimr(vinmsg)
038000120528     C                             + ' ' + 'VABVLB'
038100120528     C                   ENDIF
038200080320     C* CAS
038300120529     C                   EVAL      FlgCAS = '0'
038400120528     C                   IF        %trim(%subst(vindta:274:15)) <> *blank
038500120528     C                   EVAL      PiStr=%trim(%subst(vindta:274:15))
038600080320     C                   EXSR      CHKNUM
038700080320     C                   IF        PiNum=*on
038800120529     C                   IF        PiVal <> 0
038900080410     C                   Z-ADD(H)  PiVal         VABCAS
039000120528     C                   EVAL      FlgCAS = '1'
039100120528     C                   EVAL      VABTIC = %trim(%subst(vindta:289:2))
039200120528     C                   EVAL      VABVCA = %trim(%subst(vindta:291:3))
039300120529     C                   ENDIF
039400120529     C                   ELSE
039500120529     C                   SETON                                        32
039600080320     C                   EVAL      vinmsg = %trimr(vinmsg)
039700080320     C                             + ' ' + 'VABCAS'
039800080408     C                   ENDIF
039900120528     C                   ENDIF
040000120529     C* DCR
040100120529     C* (per ora) si assume che la data sia scritta in aaaammgg
040200120529     C                   IF        %trim(%subst(vindta:419:9)) <> *blank
040300120529     C                   EVAL      PiStr=%trim(%subst(vindta:419:9))
040400120529     C                   EXSR      CHKNUM
040500120529     C                   IF        PiInt=*on       AND
040600120529     C                             PiVal<=99999999
040700120529     C                   Z-ADD     PiVal         VABDCR
040800120529     C                   EVAL      VABTIC = %trim(%subst(vindta:289:2))
040900120529     C                   ELSE
041000120529     C                   SETON                                        32
041100120529     C                   EVAL      vinmsg = %trimr(vinmsg)
041200120529     C                             + ' ' + 'VABDCR'
041300120529     C                   ENDIF
041400120529     C                   ENDIF
041500120528     C* VMD
041600120528     C                   IF        %trim(%subst(vindta:437:15)) <> *blank
041700120528     C                   EVAL      PiStr=%trim(%subst(vindta:437:15))
041800120528     C                   EXSR      CHKNUM
041900120528     C                   IF        PiNum=*on
042000120528     C                   Z-ADD(H)  PiVal         VABVMD
042100120529     C                   IF        PiVal <> 0
042200120528     C                   EVAL      VABVAD = %trim(%subst(vindta:452:3))
042300120529     C                   ENDIF
042400120528     C                   ELSE
042500120528     C                   SETON                                        32
042600120528     C                   EVAL      vinmsg = %trimr(vinmsg)
042700120528     C                             + ' ' + 'VABVMD'
042800120528     C                   ENDIF
042900120528     C                   ENDIF
043000020322     C*
043100020322     C* Considerazioni finali su CBO/CAS
043200050908     C                   IF        FlgCAS = '1'
043300040713     C                   IF        VABCBO = '1'
043400020322     C                   EVAL      VABCBO = '4'
043500040713     C                   ENDIF
043600040713     C                   IF        VABCBO = '2'
043700040713     C                   EVAL      VABCBO = '6'
043800040713     C                   ENDIF
043900020322     C                   ENDIF
044000120528     C*
044100120528     C* Se RMN nn valorizzato lo forzo uguale a VABNSP
044200120528     C                   IF        VABRMN = *zeros
044300120528     C                   Z-ADD     VABNSP        VABRMN
044400120528     C                   ENDIF
044500020204     C*
044600020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
044700020204     C                   EXSR      CHKIMPDIV
044800020204     C*
044900010607     C                   ENDSR
045000010607     C*----------------------------------------------------*
045100010601
045200020204
045300020204     C*----------------------------------------------------*
045400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
045500020204     C*----------------------------------------------------*
045600120528     C     CHKIMPDIV     BEGSR
045700020204     C*
045800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
045900020204     C                   Z-ADD     *zeros        wrkDec            9 9
046000020204     C*
046100020204     C* Come prima cosa effettuo considerazioni sulla divisa
046200020204     C                   IF        vabIAS > *zeros
046300020204     C                   IF        vabVAS <> 'EUR'
046400020204     C                   EVAL      vabVAS =  'ITL'
046500020204     C                   ENDIF
046600020204     C                   ENDIF
046700020204     C*
046800020204     C                   IF        vabCAS > *zeros
046900020204     C                   IF        vabVCA <> 'EUR'
047000020204     C                   EVAL      vabVCA =  'ITL'
047100020204     C                   ENDIF
047200020204     C                   ENDIF
047300020204     C*
047400120607     C* siccome il cliente sembra passi la divisa=' ' per dire EUR, viene aggiunto un test che
047500120607     C* assegna EUR se divisa=' '
047600020204     C                   IF        vabVMD > *zeros
047700120607     C                   IF        vabVAD = *blank
047800120607     C                   EVAL      vabVAD =  'EUR'
047900120607     C                   ENDIF
048000120607     C                   IF        vabVAD <> 'EUR'
048100120607     C                   EVAL      vabVAD =  'ITL'
048200120607     C                   ENDIF
048300020204     C                   ENDIF
048400020204     C*
048500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048600020204     C                   Z-ADD     vabIAS        wrkDec
048700020204     C                   IF        wrkDec > *zeros
048800020204     C                   IF        vabVAS = 'ITL'
048900020204     C                   EVAL      vabIAS = *zeros
049000020204     C                   ENDIF
049100020204     C                   ENDIF
049200020204     C*
049300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
049400020204     C                   Z-ADD     vabCAS        wrkDec
049500020204     C                   IF        wrkDec > *zeros
049600020204     C                   IF        vabVCA = 'ITL'
049700020204     C                   EVAL      vabCAS = *zeros
049800020204     C                   ENDIF
049900020204     C                   ENDIF
050000020204     C*
050100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050200020204     C                   Z-ADD     vabVMD        wrkDec
050300020204     C                   IF        wrkDec > *zeros
050400020204     C                   IF        vabVAD = 'ITL'
050500020204     C                   EVAL      vabVMD = *zeros
050600020204     C                   ENDIF
050700020204     C                   ENDIF
050800020204     C*
050900020204     C                   ENDSR
051000020204     C***
051100020204
051200010330
051300010330
051400010330     C*----------------------------------------------------*
051500010330     C*  CONTROLLO NUMERICITA' CAMPI
051600010330     C*----------------------------------------------------*
051700010330     C     CHKNUM        BEGSR
051800010330     C*
051900010606     C                   IF        PiDecChr = *blanks
052000080409     C                   EVAL      PiDecChr = '.'
052100010606     C                   ENDIF
052200010606     C*
052300010606     C                   CALL(e)   'ISNUMERIC'
052400010330     C                   PARM                    PiStr            30
052500010606     C                   PARM                    PiDecChr          1
052600010330     C                   PARM      *ZEROS        PiVal            30 9
052700010330     C                   PARM      '0'           PiInt             1
052800010330     C                   PARM      '0'           PiNum             1
052900010330     C                   IF        %error
053000010606     C                   EVAL      PiNum=*off
053100010330     C                   ENDIF
053200010330     C*
053300010330     C                   ENDSR
053400010330     C***
053500010330
053600010601
053700010601
053800010601
053900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
054000010601     C     repfil        BEGSR
054100010601     C*
054200010601     C                   if        invfil = *zeros and
054300010601     C                             depfil > *zeros and
054400010629     C                             (vinflg = *blanks or
054500010629     C                              vinflg = *zeros)
054600010601     C
054700010601     C                   eval      invfil = depfil
054800010601     C                   endif
054900010601     C*
055000010601     C                   if        depfil <> invfil and
055100010601     C                             invfil > *zeros
055200010601     C                   eval      flgMulti = '1'
055300010601     C                   if        vinflg = *blanks
055400010601     C                   add       1             cntNonEl
055500010601     C                   endif
055600010601     C                   endif
055700010601     C*
055800010601     C                   if        vinflg = '2'
055900010601     C                   eval      flgStato = '2'
056000010601     C                   endif
056100010601     C*
056200010601     C                   ENDSR
056300010601     C***
056400010601
056500010601
056600010601
056700010330
056800010330
056900010330
057000990920      /TITLE Invio dei dati al punto operativo.
057100000613     C     invio         BEGSR
057200990920     C*
057300990920     C                   reset                   dscmz
057400010601     C                   move      invfil        cmzdst
057500990920     C                   eval      cmzfld = vlrfou
057600990920     C                   eval      cmzmbd = vlrhdl
057700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
057800000710     C                   if        prmfir = *blanks
057900050408     C                   eval      cmzfla = 'FIVAB00F'
058000050408     C                   eval      cmzmba = 'FIVAB00F'
058100000710     C                   else
058200000710     C                   eval      cmzfla = prmfir
058300000710     C                   eval      cmzmba = prmfir
058400000710     C                   endif
058500990920     C                   eval      cmznrr = *zeros
058600990920     C                   move      §ctrok        cmznrr
058700021018     C                   eval      cmzlba = vlrfl1
058800990920     C                   call(e)   'TIS711C'
058900990920     C                   parm                    dscmz
059000990921     C                   parm      *blanks       esito
059100990920     C                   if        %error
059200990920     C                             or cmzerr = '1'
059300990921     C                             or esito  = '1'
059400000710     C                   eval      wrkesito = '3'
059500990920     C                   endif
059600990920     C*
059700000613     C                   ENDSR
059800990910
059900010601
060000010601
060100010601
060200010601
060300010601      /TITLE Invio dei dati al punto operativo.
060400010601     C     opeini        BEGSR
060500010601     C*
060600010601     C* Inizializzo flag e contatori operativi
060700010601     C                   movel     '0'           flgGiro           1
060800010601     C                   movel     '0'           flgMulti          1
060900010601     C                   movel     '1'           flgStato          1
061000010615     C                   movel     '0'           flgOk             1
061100010601     C                   z-add     *zeros        cntNonEl         10 0
061200010601     C                   z-add     *zeros        depfil            3 0
061300010601     C                   z-add     *zeros        invfil            3 0
061400010601     C*
061500010601     C                   ENDSR
061600010601     C***
061700010601
061800010601
061900010601
062000010330
062100010330
062200000613     C     *inzsr        BEGSR
062300990910     C*
062400990910     C     *entry        plist
062500990920     C                   parm                    tivlrds
062600990921     C                   parm      wrkesito      esito
062700000724     C                   parm                    prmlit
062800000710     C                   parm                    prmfir
062900010330     C*
063000010330     C* CALCOLA LA DATA CORRENTE
063100010330     C                   time                    wn14             14 0
063200010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
063300010330     C                   z-add     wn8           g08dat
063400010330     C                   z-add     *zeros        g08inv
063500010330     C                   movel     '0'           g08err
063600010330     C                   call      'XSRDA8'
063700010330     C                   parm                    wlbda8
063800010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063900000613     C*
064000000613     C                   ENDSR
064100000613     C***
