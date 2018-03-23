000100121005      /TITLE Upload via Internet: traduzione in EDIVAWWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400130208     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600121005     FEDIVABWR  O    E             DISK    usropn
000700000313     D*
000800010330     D*----------------------------------------------------
000900010330     D* DICHIARAZIOINE VARIABILI DI WRK
001000010330     D*----------------------------------------------------
001100010330     D dscmz         e ds                  inz
001200010330     D psds           sds
001300010330     D  procname         *PROC
001400010330     D tivlrds       e ds                  extname(tivlr00f)
001500130208     D ds15          e ds
001600070502     D tisi95ds      e ds
001700010330     D esito           s              1
001800010330     D prmlit          s             10
001900010330     D prmfir          s             10
002000010330     D wrkesito        s                   like(esito)
002100010330     D rrnum           s              6  0 INZ(*zeros)
002200130208     D jNAZ            s              5  0 INZ(*zeros)
002300010330     D*------------------
002400010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
002500010330     D*------------------
002600010330     D WLBDA8          DS                  INZ
002700010330     D  G08DAT                 1      8  0
002800010330     D  G08INV                 9     16  0
002900010330     D  G08ERR                17     17
003000010330     D  G08TGI                18     22  0
003100130208     D*------------
003200130208     D skNAZISO        S              3    DIM(1000)
003300130208     D skNAZBAR        S              3    DIM(1000)
003400010330
003500010330
003600000913     C                   reset                   rrnum
003700990921     C                   reset                   esito
003800990921     C                   reset                   wrkesito
003900010601     C*
004000130208     C                   EXSR      CARTAB                                       CARICA TABELLE
004100010601     C                   exsr      opeini
004200010605     C                   exsr      rwvab
004300070502     C*
004400070502     C* Effettuo lancio TISI95 solo x chiusura
004500070502     C                   CLEAR                   TISI95DS
004600070502     C                   EVAL      I95TLA = 'C'
004700070502     C                   CALL      'TISI95R'
004800070502     C                   PARM                    TISI95DS
004900010601     C*
005000010601     C                   seton                                        lr
005100010601
005200010601
005300010601
005400010601
005500010601     C*--------------------------------------------------------
005600010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
005700010601     C*--------------------------------------------------------
005800010601     C     PREELA        BEGSR
005900010601     C*
006000010601     C* SE OCCORRE SPEDIRE IN FILIALE
006100010601     C                   if        invfil <> *zeros and
006200010601     C                             flgGiro = '0'
006300010601     C*
006400010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
006500010601     C                   eval      flgGiro = '1'
006600010601     C*
006700010601     C                   endif
006800010601     C*
006900010601     C                   ENDSR
007000010601     C***
007100010601
007200010601
007300010601
007400010601     C*--------------------------------------------------------
007500010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
007600010601     C*--------------------------------------------------------
007700010601     C     ENDELA        BEGSR
007800000616     C*
007900010601     C                   ENDSR
008000010601     C***
008100000613
008200010601
008300010601
008400010330     C*--------------------------------------------------------
008500121005     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVAWWR              *
008600010330     C*--------------------------------------------------------
008700010605     C     RWVAB         BEGSR
008800050408     C*
008900010330     C                   if        not %open(tivin00r)
009000010330     C                   open      tivin00r
009100010330     C                   endif
009200121005     C                   if        not %open(edivabwr)
009300121005     C                   open      edivabwr
009400010330     C                   endif
009500010330     C*
009600010604     C                   clear                   §CTROK
009700010604     C                   clear                   §CTRMO
009800010604     C                   clear                   §CTRNO
009900010330     C*
010000010330     C                   DO        *HIVAL
010100010330     C*
010200010330     C                   READ      tivin00r                               70
010300010618     C*
010400010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010500010618     C                   if        vinflg = '1'
010600010618     C                   eval      flgOk = '1'
010700010618     C                   endif
010800010618     C*
010900010330     C                   if        vindta > *blanks
011000010330     C                   add       1             rrnum
011100010330     C*
011200010601     C                   if        *in70 = *off and
011300010330     C                             (vinflg = *blanks
011400010330     C                              or vinflg = '0'
011500010330     C                              or vinflg = '2')
011600010330     C*
011700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011800010711     C                   if        vinflg = *blanks or vinflg = '0'
011900010711     C                   clear                   vinmsg
012000010711     C                   endif
012100070601     C*
012200070601     C                   exsr      inzvar
012300070601     C                   exsr      defcam
012400070601     C                   exsr      impvab
012500010601     C*
012600010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
012700010605     C                   eval      depfil = VABLNP
012800010601     C                   exsr      repfil
012900010601     C                   if        depfil = invfil
013000021025     C                   if        vlrpoi = 999
013100021025     C                   MOVE(P)   invfil        VABFGS
013200021025     C                   else
013300021025     C                   MOVE(P)   vlrpoi        VABFGS
013400021025     C                   endif
013500010601     C*
013600010601     C                   exsr      PREELA
013700010601     C*
013800010604     C* Ebbene...
013900010604     C*
014000010604     C  N31              ADD       1             §CTROK            7 0
014100010604     C   32              ADD       1             §CTRMO            7 0
014200010604     C   31              ADD       1             §CTRNO            7 0
014300010604     C*
014400071120     C                   exsr      wrivab                                       => scrivo VAB
014500071120     C*
014600020722     C                   endif
014700020722     C*
014800010604     C                   if        *in31 = *off and
014900010604     C                             *in32 = *off
015000010604     C                   eval      vinflg = '1'
015100010604     C                   else
015200010604     C                   eval      vinflg = '2'
015300010604     C                   endif
015400010604     C                   endif
015500010604     C*
015600010330     C                   else
015700010330     C                   eval      vinflg = '1'
015800010330     C                   endif
015900010601     C*
016000010601     C  N70              update    tivin000
016100010330     C*
016200010330     C  N70              ENDdo
016300010601     C*
016400010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
016500010601     C                   if        cntNonEl = *zeros or
016600010601     C                             flgMulti = '0'
016700010330     C* Se non ci sono record con errori ...
016800010601     C                   if        §ctrno = 0 and
016900010604     C                             §ctrmo = 0 and
017000010601     C                             flgStato <> '2'
017100010330     C* ... restituisco esito OK.
017200010330     C                   eval      wrkesito = '0'
017300010330     C                   else
017400010330     C                   if        §ctrok > 0
017500010330     C                   eval      wrkesito = '1'
017600010330     C                   else
017700010615     C                   if        flgOk = '0'
017800010615     C                   eval      wrkesito = '2'
017900010615     C                   else
018000010615     C                   eval      wrkesito = '6'
018100010615     C                   endif
018200010330     C                   endif
018300010330     C                   endif
018400010601     C                   else
018500010601     C                   eval      wrkesito = '9'
018600010601     C                   endif
018700010330     C*
018800010330     C                   if        %open(tivin00r)
018900010330     C                   close     tivin00r
019000010330     C                   endif
019100121005     C                   if        %open(edivabwr)
019200121005     C                   close     edivabwr
019300010330     C                   endif
019400010601     C*
019500010601     C                   if        vlrpoi <> 999
019600010601     C                   eval      invfil = vlrpoi
019700010601     C                   endif
019800010330     C*
019900010330     C                   if        §ctrok > 0
020000010601     C                             and invfil > *zeros
020100010330     C                   exsr      invio
020200010330     C                   endif
020300010601     C*
020400010618     C                   if        flgGiro = '1'
020500010601     C                   exsr      endela
020600010618     C                   endif
020700010330     C*
020800010330     C                   ENDSR
020900010330     C***
021000070601
021100070601     C*----------------------------------------------------*
021200070601     C*  SCARICAMENTO BUFFER RECORDS VAB
021300070601     C*----------------------------------------------------*
021400070601     C     WRIVAB        BEGSR
021500121005     C*
021600121005     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
021700121015     C                   EVAL      VABCMR = %char(datcor)
021800121005     C                   EVAL      VABDCM = datcor
021900121005     C                   EVAL      VABDTS = datcor
022000121005     C                   EVAL      VABHMS = oracor
022100121005     C                   EVAL      VABCNT = 1
022200070601     C*
022300121005     C  N31              WRITE     EDIVAB00
022400070601     C*
022500070601     C                   ENDSR
022600010601
022700010601
022800010601
022900010330     C*----------------------------------------------------*
023000020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
023100010330     C*----------------------------------------------------*
023200010330     C     INZVAR        BEGSR
023300010330     C*
023400020204     C                   Z-ADD     *zeros        Num5_0            5 0
023500020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
023600020322     C                   MOVEL     '0'           FlgCAS            1
023700010330     C*
023800010330     C                   ENDSR
023900010330     C*----------------------------------------------------*
024000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
024100010330     C*----------------------------------------------------*
024200010330     C     DEFCAM        BEGSR
024300010330     C*
024400020204     C* Inizializzo il buffer del record da scrivere
024500121005     C                   CLEAR                   EDIVAB00
024600020204     C* Imposto i valori di default...
024700080311     C                   EVAL      VABCCM = 0661040
024800080311     C                   EVAL      VABLNP = 001
024900080311     C                   EVAL      VABCTR = 100
025000080311     C                   EVAL      VABCTM = '7Q'
025100070208     C                   EVAL      VABCBO = '1'
025200020204     C* ... e poi verifico se sono stati passati come parametri
025300020204     C                   IF        vlrppt > *blanks
025400040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
025500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
025600020204     C                   EXSR      CHKNUM
025700020204     C                   IF        PiInt=*on
025800020204     C                   Z-ADD     PiVal         VABCCM
025900020204     C                   ENDIF
026000040301     C                   ENDIF
026100040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
026200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
026300020204     C                   EXSR      CHKNUM
026400020204     C                   IF        PiInt=*on
026500020204     C                   Z-ADD     PiVal         VABLNP
026600020204     C                   ENDIF
026700040301     C                   ENDIF
026800040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
027000020204     C                   EXSR      CHKNUM
027100020204     C                   IF        PiInt=*on
027200020204     C                   Z-ADD     PiVal         VABCTR
027300040301     C                   ENDIF
027400020204     C                   ENDIF
027500020204     C                   ENDIF
027600020204     C*
027700010330     C                   ENDSR
027800010607     C*----------------------------------------------------*
027900121005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
028000010607     C*----------------------------------------------------*
028100010607     C     IMPVAB        BEGSR
028200010607     C*
028300010607     C                   SETOFF                                       3132
028400010607     C*
028500010607     C* Reperimento campi ALFA
028600010607     C*
028700010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
028800070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
028900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
029000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
029100020117     C* ==
029200080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
029300070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
029400070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
029500070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
029600130208     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
029700130208     C                   Z-ADD     1             jNAZ
029800130208     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
029900130208     C                   IF        %found
030000130208     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
030100130208     C                   ENDIF
030200070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
030300080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
030400080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
030500080311     C                   IF        %subst(vindta:210:1) = 'M'
030600080311     C                   EVAL      VABTIC='BM'
030700080311     C                   ENDIF
030800080311     C                   IF        %subst(vindta:210:1) = 'N'
030900080311     C                   EVAL      VABTIC='TM'
031000080311     C                   ENDIF
031100100527     C                   IF        %subst(vindta:210:1) = 'R'
031200100527     C                   EVAL      VABTIC='CM'
031300080311     C                   ENDIF
031400080311     C                   IF        %subst(vindta:227:4) = *blanks
031500080311     C                   EVAL      VABVCA='EUR'
031600080311     C                   ENDIF
031700080311     C                   IF        %subst(vindta:231:1) = 'I'
031800080311     C                   EVAL      VABTC1='A'
031900080311     C                   ENDIF
032000080311     C                   IF        %subst(vindta:231:1) = '2'
032100080311     C                   EVAL      VABTC2='P'
032200080311     C                   ENDIF
032300080311     C                   IF        %subst(vindta:231:1) = '1'
032400080311     C                   EVAL      VABTC2='S'
032500080311     C                   ENDIF
032600060510     C*
032700060510     C* Reperimento campi NUMERICI
032800060510     C                   MOVEL     DATCOR        VABAAS
032900060510     C                   MOVE      DATCOR        VABMGS
033000080311     C* CCM
033100080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC300'
033200080311     C                   EVAL      VABCCM=0661040
033300080311     C                   ENDIF
033400080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC3000'
033500080311     C                   EVAL      VABCCM=0661069
033600080311     C                   ENDIF
033700150519     C                   IF        %trim(%subst(vindta:23:6)) = 'RE5775' AND
033800150519     C                             VABCCM<>2200910
033900120920     C                   EVAL      VABCCM=2494836
034000120920     C                   ENDIF
034100121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD000'
034200121218     C                   EVAL      VABCCM=0661890
034300121218     C                   ENDIF
034400121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD0000'
034500121218     C                   EVAL      VABCCM=0661889
034600121218     C                   ENDIF
034700121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES070'
034800121218     C                   EVAL      VABCCM=0661892
034900121218     C                   ENDIF
035000121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0700'
035100121218     C                   EVAL      VABCCM=0661891
035200121218     C                   ENDIF
035300121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES380'
035400121218     C                   EVAL      VABCCM=0661895
035500121218     C                   ENDIF
035600121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES3800'
035700121218     C                   EVAL      VABCCM=0661894
035800121218     C                   ENDIF
035900121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES160'
036000121218     C                   EVAL      VABCCM=0661896
036100121218     C                   ENDIF
036200121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES1600'
036300121218     C                   EVAL      VABCCM=0661893
036400121218     C                   ENDIF
036500121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT000'
036600121218     C                   EVAL      VABCCM=0661888
036700121218     C                   ENDIF
036800121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT0000'
036900121218     C                   EVAL      VABCCM=0661887
037000121218     C                   ENDIF
037100121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES000'
037200121218     C                   EVAL      VABCCM=0661886
037300121218     C                   ENDIF
037400121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0000'
037500121218     C                   EVAL      VABCCM=0661885
037600121218     C                   ENDIF
037700080311     C* DCR
037800080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
037900080311     C                   EXSR      CHKNUM
038000080311     C                   IF        PiInt=*on
038100080311     C                   Z-ADD     PiVal         VABDCR
038200080311     C                   ELSE
038300100527     C                   Z-ADD     *zeros        VABDCR
038400080311     C                   EVAL      vinmsg = %trimr(vinmsg)
038500080311     C                             + ' ' + 'VABDCR'
038600080311     C                   ENDIF
038700060710     C* NSP/RMN
038800080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
038900060510     C                   EXSR      CHKNUM
039000060510     C                   IF        PiInt=*on
039100060510     C                   Z-ADD     PiVal         VABNSP
039200060510     C                   Z-ADD     PiVal         VABRMN
039300060404     C                   ELSE
039400070502     C                   SETON                                        31
039500060510     C                   Z-ADD     *zeros        VABNSP
039600060510     C                   Z-ADD     1             VABRMN
039700060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039800060510     C                             + ' ' + 'VABNSP VABRMN'
039900060404     C                   ENDIF
040000040420     C* CAD
040100130208     C                   EVAL      VABCAD=%trim(%subst(vindta:145:9))
040200130208     C****               EVAL      PiStr=%trim(%subst(vindta:145:9))
040300130208     C****               EXSR      CHKNUM
040400130208     C****               IF        PiInt=*on
040500130208     C****               Z-ADD     PiVal         Num5_0
040600130208     C****               MOVEL(p)  Num5_0        VABCAD
040700130208     C****               ELSE
040800130208     C****               SETON                                        32
040900130208     C****               EVAL      VABCAD = *zeros
041000130208     C****               EVAL      vinmsg = %trimr(vinmsg)
041100130208     C****                         + ' ' + 'VABCAD'
041200130208     C****               ENDIF
041300070502     C* Reperisco la provincia dal CAP e dalla località
041400170223     C                   IF        VABNZD  = *blanks AND
041500170223     C                             VABCAD <> *blanks AND
041600070502     C                             VABPRD  = *blanks
041700070502     C                   CLEAR                   TISI95DS
041800070502     C                   EVAL      I95TCN = '3'
041900070502     C                   Z-ADD     datcor        I95DAT
042000070502     C                   EVAL      I95CAP = VABCAD
042100070502     C                   EVAL      I95LOC = VABLOD
042200070502     C                   EVAL      I95NAR = VABNZD
042300070502     C                   CALL      'TISI95R'
042400070502     C                   PARM                    TISI95DS
042500070502     C                   EVAL      VABPRD = O95PRV
042600070502     C                   ENDIF
042700040420     C* NCL
042800070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
042900010607     C                   EXSR      CHKNUM
043000010607     C                   IF        PiInt=*on
043100070606     C                   Z-ADD     PiVal         VABNCL
043200010607     C                   ELSE
043300010607     C                   SETON                                        32
043400010607     C                   Z-ADD     *zeros        VABNCL
043500010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043600010607     C                             + ' ' + 'VABNCL'
043700010607     C                   ENDIF
043800040420     C* PKB
043900070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
044000070208     C                   EVAL      PiDecChr = '.'
044100010607     C                   EXSR      CHKNUM
044200010607     C                   IF        PiNum=*on
044300070925     C                   Z-ADD(H)  PiVal         VABPKB
044400010607     C                   ELSE
044500010607     C                   SETON                                        32
044600010607     C                   Z-ADD     *zeros        VABPKB
044700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
044800010607     C                             + ' ' + 'VABPKB'
044900010607     C                   ENDIF
045000070925     C* VLB
045100070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
045200070925     C                   EVAL      PiDecChr = '.'
045300070925     C                   EXSR      CHKNUM
045400070925     C                   IF        PiNum=*on
045500070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
045600070925     C                   Z-ADD     PiVal         VABVLB
045700070925     C                   ELSE
045800070925     C                   SETON                                        32
045900070925     C                   Z-ADD     *zeros        VABVLB
046000070925     C                   EVAL      vinmsg = %trimr(vinmsg)
046100070925     C                             + ' ' + 'VABVLB'
046200070925     C                   ENDIF
046300080311     C* CAS
046400080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
046500080311     C                             %subst(vindta:196:14) <> *zeros
046600080311     C                   EVAL      FlgCAS = '1'
046700080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
046800080311     C                   EVAL      PiDecChr = '.'
046900080311     C                   EXSR      CHKNUM
047000080311     C                   IF        PiNum=*on
047100080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
047200080311     C                   Z-ADD     PiVal         VABCAS
047300080311     C                   ELSE
047400080311     C                   SETON                                        32
047500120123     C                   Z-ADD     *zeros        VABCAS
047600080311     C                   EVAL      vinmsg = %trimr(vinmsg)
047700080311     C                             + ' ' + 'VABCAS'
047800080311     C                   ENDIF
047900080311     C                   ENDIF
048000020322     C*
048100020322     C* Considerazioni finali su CBO/CAS
048200050908     C                   IF        FlgCAS = '1'
048300040713     C                   IF        VABCBO = '1'
048400020322     C                   EVAL      VABCBO = '4'
048500040713     C                   ENDIF
048600040713     C                   IF        VABCBO = '2'
048700040713     C                   EVAL      VABCBO = '6'
048800040713     C                   ENDIF
048900020322     C                   ENDIF
049000020204     C*
049100020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049200020204     C                   EXSR      CHKIMPDIV
049300020204     C*
049400010607     C                   ENDSR
049500010607     C*----------------------------------------------------*
049600010601
049700020204
049800020204     C*----------------------------------------------------*
049900020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
050000020204     C*----------------------------------------------------*
050100020204     C     CHKIMPDIV     BEGSR
050200020204     C*
050300020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
050400020204     C                   Z-ADD     *zeros        wrkDec            9 9
050500020204     C*
050600020204     C* Come prima cosa effettuo considerazioni sulla divisa
050700020204     C                   IF        vabIAS > *zeros
050800020204     C                   IF        vabVAS <> 'EUR'
050900020204     C                   EVAL      vabVAS =  'ITL'
051000020204     C                   ENDIF
051100020204     C                   ENDIF
051200020204     C*
051300020204     C                   IF        vabCAS > *zeros
051400020204     C                   IF        vabVCA <> 'EUR'
051500020204     C                   EVAL      vabVCA =  'ITL'
051600020204     C                   ENDIF
051700020204     C                   ENDIF
051800020204     C*
051900020204     C                   IF        vabVMD > *zeros
052000020321     C                   IF        vabVAD <> 'EUR'
052100020204     C                   EVAL      vabVAD =  'ITL'
052200020204     C                   ENDIF
052300020204     C                   ENDIF
052400020204     C*
052500020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
052600020204     C                   Z-ADD     vabIAS        wrkDec
052700020204     C                   IF        wrkDec > *zeros
052800020204     C                   IF        vabVAS = 'ITL'
052900020204     C                   EVAL      vabIAS = *zeros
053000020204     C                   ENDIF
053100020204     C                   ENDIF
053200020204     C*
053300020204     C* Stabilisco se il contrasegno ha decimali valorizzati
053400020204     C                   Z-ADD     vabCAS        wrkDec
053500020204     C                   IF        wrkDec > *zeros
053600020204     C                   IF        vabVCA = 'ITL'
053700020204     C                   EVAL      vabCAS = *zeros
053800020204     C                   ENDIF
053900020204     C                   ENDIF
054000020204     C*
054100020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
054200020204     C                   Z-ADD     vabVMD        wrkDec
054300020204     C                   IF        wrkDec > *zeros
054400020204     C                   IF        vabVAD = 'ITL'
054500020204     C                   EVAL      vabVMD = *zeros
054600020204     C                   ENDIF
054700020204     C                   ENDIF
054800020204     C*
054900020204     C                   ENDSR
055000020204     C***
055100020204
055200010330
055300010330
055400010330     C*----------------------------------------------------*
055500010330     C*  CONTROLLO NUMERICITA' CAMPI
055600010330     C*----------------------------------------------------*
055700010330     C     CHKNUM        BEGSR
055800010330     C*
055900010606     C                   IF        PiDecChr = *blanks
056000070502     C                   EVAL      PiDecChr = '.'
056100010606     C                   ENDIF
056200010606     C*
056300010606     C                   CALL(e)   'ISNUMERIC'
056400010330     C                   PARM                    PiStr            30
056500010606     C                   PARM                    PiDecChr          1
056600010330     C                   PARM      *ZEROS        PiVal            30 9
056700010330     C                   PARM      '0'           PiInt             1
056800010330     C                   PARM      '0'           PiNum             1
056900010330     C                   IF        %error
057000010606     C                   EVAL      PiNum=*off
057100010330     C                   ENDIF
057200010330     C*
057300010330     C                   ENDSR
057400010330     C***
057500010330
057600010601
057700010601
057800010601
057900010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
058000010601     C     repfil        BEGSR
058100010601     C*
058200010601     C                   if        invfil = *zeros and
058300010601     C                             depfil > *zeros and
058400010629     C                             (vinflg = *blanks or
058500010629     C                              vinflg = *zeros)
058600010601     C
058700010601     C                   eval      invfil = depfil
058800010601     C                   endif
058900010601     C*
059000010601     C                   if        depfil <> invfil and
059100010601     C                             invfil > *zeros
059200010601     C                   eval      flgMulti = '1'
059300010601     C                   if        vinflg = *blanks
059400010601     C                   add       1             cntNonEl
059500010601     C                   endif
059600010601     C                   endif
059700010601     C*
059800010601     C                   if        vinflg = '2'
059900010601     C                   eval      flgStato = '2'
060000010601     C                   endif
060100010601     C*
060200010601     C                   ENDSR
060300010601     C***
060400010601
060500010601
060600010601
060700010330
060800010330
060900010330
061000990920      /TITLE Invio dei dati al punto operativo.
061100000613     C     invio         BEGSR
061200990920     C*
061300990920     C                   reset                   dscmz
061400010601     C                   move      invfil        cmzdst
061500990920     C                   eval      cmzfld = vlrfou
061600990920     C                   eval      cmzmbd = vlrhdl
061700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
061800000710     C                   if        prmfir = *blanks
061900121005     C                   eval      cmzfla = 'EDIVAB0F'
062000121005     C                   eval      cmzmba = 'EDIVAB0F'
062100000710     C                   else
062200000710     C                   eval      cmzfla = prmfir
062300000710     C                   eval      cmzmba = prmfir
062400000710     C                   endif
062500990920     C                   eval      cmznrr = *zeros
062600990920     C                   move      §ctrok        cmznrr
062700021018     C                   eval      cmzlba = vlrfl1
062800990920     C                   call(e)   'TIS711C'
062900990920     C                   parm                    dscmz
063000990921     C                   parm      *blanks       esito
063100990920     C                   if        %error
063200990920     C                             or cmzerr = '1'
063300990921     C                             or esito  = '1'
063400000710     C                   eval      wrkesito = '3'
063500990920     C                   endif
063600990920     C*
063700000613     C                   ENDSR
063800130208
063900130208     C*--------------------------------------------------------
064000130208     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
064100130208     C*--------------------------------------------------------
064200130208     C     CARTAB        BEGSR
064300130208     C*
064400130208     C* TABELLA '15' - NAZIONI
064500130208     C                   clear                   skNAZISO
064600130208     C                   clear                   skNAZBAR
064700130208     C                   eval      tblKUT = 1
064800130208     C                   eval      tblCOD = '15'
064900130208     C     KEYtabP       setll     tabel00f
065000130208     C     KEYtabP       reade     tabel00f
065100130208     C                   dow       not %eof(tabel00f)
065200130208     C                   if        tblFLG = *blanks
065300130208     C                   movel(p)  tblUNI        ds15
065400130208     C                   add       1             jNAZ
065500130208     C                   eval      skNAZBAR(jNAZ) = tblKEY
065600130208     C                   eval      skNAZISO(jNAZ) = §15COD
065700130208     C                   endif
065800130208     C     KEYtabP       reade     tabel00f
065900130208     C                   enddo
066000130208     C*
066100130208     C                   ENDSR
066200130208     C***
066300990910
066400010601
066500010601
066600010601
066700010601
066800010601      /TITLE Invio dei dati al punto operativo.
066900010601     C     opeini        BEGSR
067000010601     C*
067100010601     C* Inizializzo flag e contatori operativi
067200010601     C                   movel     '0'           flgGiro           1
067300010601     C                   movel     '0'           flgMulti          1
067400010601     C                   movel     '1'           flgStato          1
067500010615     C                   movel     '0'           flgOk             1
067600010601     C                   z-add     *zeros        cntNonEl         10 0
067700010601     C                   z-add     *zeros        depfil            3 0
067800010601     C                   z-add     *zeros        invfil            3 0
067900010601     C*
068000010601     C                   ENDSR
068100010601     C***
068200010601
068300010601
068400010601
068500010330
068600010330
068700000613     C     *inzsr        BEGSR
068800990910     C*
068900990910     C     *entry        plist
069000990920     C                   parm                    tivlrds
069100990921     C                   parm      wrkesito      esito
069200000724     C                   parm                    prmlit
069300000710     C                   parm                    prmfir
069400010330     C*
069500010330     C* CALCOLA LA DATA CORRENTE
069600010330     C                   time                    wn14             14 0
069700121005     C                   movel     wn14          oracor            6 0          *ORA
069800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
069900010330     C                   z-add     wn8           g08dat
070000010330     C                   z-add     *zeros        g08inv
070100010330     C                   movel     '0'           g08err
070200010330     C                   call      'XSRDA8'
070300010330     C                   parm                    wlbda8
070400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
070500130208     C*
070600130208     C* Chiave su TABEL00F - parziale
070700130208     C     KEYtabP       KLIST
070800130208     C                   KFLD                    tblKUT
070900130208     C                   KFLD                    tblCOD
071000000613     C*
071100000613     C                   ENDSR
071200000613     C***
