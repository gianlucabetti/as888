000100051219      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400130208     Ftabel00f  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050408     FFIVABWWR  O    E             DISK    usropn
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
008500050408     C* RWVAB   LEGGE TIVIN00R E SCRIVE FIVABWWR              *
008600010330     C*--------------------------------------------------------
008700010605     C     RWVAB         BEGSR
008800050408     C*
008900010330     C                   if        not %open(tivin00r)
009000010330     C                   open      tivin00r
009100010330     C                   endif
009200050408     C                   if        not %open(fivabwwr)
009300050408     C                   open      fivabwwr
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
019100050408     C                   if        %open(fivabwwr)
019200050408     C                   close     fivabwwr
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
021500070601     C*
021600070601     C  N31              WRITE     FIVAB000
021700070601     C*
021800070601     C                   ENDSR
021900010601
022000010601
022100010601
022200010330     C*----------------------------------------------------*
022300020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
022400010330     C*----------------------------------------------------*
022500010330     C     INZVAR        BEGSR
022600010330     C*
022700020204     C                   Z-ADD     *zeros        Num5_0            5 0
022800020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
022900020322     C                   MOVEL     '0'           FlgCAS            1
023000010330     C*
023100010330     C                   ENDSR
023200010330     C*----------------------------------------------------*
023300020722     C*  IMPOSTAZIONE CAMPI COSTANTI
023400010330     C*----------------------------------------------------*
023500010330     C     DEFCAM        BEGSR
023600010330     C*
023700020204     C* Inizializzo il buffer del record da scrivere
023800050408     C                   CLEAR                   FIVAB000
023900020204     C* Imposto i valori di default...
024000130118     C                   EVAL      VABCCM = 0661908
024100080311     C                   EVAL      VABLNP = 001
024200080311     C                   EVAL      VABCTR = 100
024300080311     C                   EVAL      VABCTM = '7Q'
024400070208     C                   EVAL      VABCBO = '1'
024500020204     C* ... e poi verifico se sono stati passati come parametri
024600020204     C                   IF        vlrppt > *blanks
024700040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
024800020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
024900020204     C                   EXSR      CHKNUM
025000020204     C                   IF        PiInt=*on
025100020204     C                   Z-ADD     PiVal         VABCCM
025200020204     C                   ENDIF
025300040301     C                   ENDIF
025400040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
025500020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
025600020204     C                   EXSR      CHKNUM
025700020204     C                   IF        PiInt=*on
025800020204     C                   Z-ADD     PiVal         VABLNP
025900020204     C                   ENDIF
026000040301     C                   ENDIF
026100040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
026200020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
026300020204     C                   EXSR      CHKNUM
026400020204     C                   IF        PiInt=*on
026500020204     C                   Z-ADD     PiVal         VABCTR
026600040301     C                   ENDIF
026700020204     C                   ENDIF
026800020204     C                   ENDIF
026900020204     C*
027000010330     C                   ENDSR
027100010607     C*----------------------------------------------------*
027200050408     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
027300010607     C*----------------------------------------------------*
027400010607     C     IMPVAB        BEGSR
027500010607     C*
027600010607     C                   SETOFF                                       3132
027700010607     C*
027800010607     C* Reperimento campi ALFA
027900010607     C*
028000010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
028100070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
028200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
028300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
028400020117     C* ==
028500080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
028600070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
028700070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
028800070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
028900130208     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
029000130208     C                   Z-ADD     1             jNAZ
029100130208     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
029200130208     C                   IF        %found
029300130208     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
029400130208     C                   ENDIF
029500070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
029600080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
029700080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
029800080311     C                   IF        %subst(vindta:210:1) = 'M'
029900080311     C                   EVAL      VABTIC='BM'
030000080311     C                   ENDIF
030100080311     C                   IF        %subst(vindta:210:1) = 'N'
030200080311     C                   EVAL      VABTIC='TM'
030300080311     C                   ENDIF
030400100527     C                   IF        %subst(vindta:210:1) = 'R'
030500100527     C                   EVAL      VABTIC='CM'
030600080311     C                   ENDIF
030700080311     C                   IF        %subst(vindta:227:4) = *blanks
030800080311     C                   EVAL      VABVCA='EUR'
030900080311     C                   ENDIF
031000080311     C                   IF        %subst(vindta:231:1) = 'I'
031100080311     C                   EVAL      VABTC1='A'
031200080311     C                   ENDIF
031300080311     C                   IF        %subst(vindta:231:1) = '2'
031400080311     C                   EVAL      VABTC2='P'
031500080311     C                   ENDIF
031600080311     C                   IF        %subst(vindta:231:1) = '1'
031700080311     C                   EVAL      VABTC2='S'
031800080311     C                   ENDIF
031900060510     C*
032000060510     C* Reperimento campi NUMERICI
032100060510     C                   MOVEL     DATCOR        VABAAS
032200060510     C                   MOVE      DATCOR        VABMGS
032300080311     C* CCM
032400080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC300'
032500130118     C                   EVAL      VABCCM=0661908
032600080311     C                   ENDIF
032700080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC3000'
032800130118     C                   EVAL      VABCCM=0661909
032900080311     C                   ENDIF
033000120920     C                   IF        %trim(%subst(vindta:23:6)) = 'RE5775'
033100120920     C                   EVAL      VABCCM=2494836
033200120920     C                   ENDIF
033300121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD000'
033400130118     C                   EVAL      VABCCM=0661912
033500121218     C                   ENDIF
033600121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD0000'
033700130118     C                   EVAL      VABCCM=0661913
033800121218     C                   ENDIF
033900121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES070'
034000130118     C                   EVAL      VABCCM=0661914
034100121218     C                   ENDIF
034200121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0700'
034300130118     C                   EVAL      VABCCM=0661915
034400121218     C                   ENDIF
034500121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES380'
034600130118     C                   EVAL      VABCCM=0661916
034700121218     C                   ENDIF
034800121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES3800'
034900130118     C                   EVAL      VABCCM=0661917
035000121218     C                   ENDIF
035100121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES160'
035200130118     C                   EVAL      VABCCM=0661918
035300121218     C                   ENDIF
035400121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES1600'
035500130118     C                   EVAL      VABCCM=0661919
035600121218     C                   ENDIF
035700121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT000'
035800130118     C                   EVAL      VABCCM=0661920
035900121218     C                   ENDIF
036000121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT0000'
036100130118     C                   EVAL      VABCCM=0661921
036200121218     C                   ENDIF
036300121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES000'
036400130118     C                   EVAL      VABCCM=0661922
036500121218     C                   ENDIF
036600121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0000'
036700130118     C                   EVAL      VABCCM=0661923
036800121218     C                   ENDIF
036900080311     C* DCR
037000080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
037100080311     C                   EXSR      CHKNUM
037200080311     C                   IF        PiInt=*on
037300080311     C                   Z-ADD     PiVal         VABDCR
037400080311     C                   ELSE
037500100527     C                   Z-ADD     *zeros        VABDCR
037600080311     C                   EVAL      vinmsg = %trimr(vinmsg)
037700080311     C                             + ' ' + 'VABDCR'
037800080311     C                   ENDIF
037900060710     C* NSP/RMN
038000080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
038100060510     C                   EXSR      CHKNUM
038200060510     C                   IF        PiInt=*on
038300060510     C                   Z-ADD     PiVal         VABNSP
038400060510     C                   Z-ADD     PiVal         VABRMN
038500060404     C                   ELSE
038600070502     C                   SETON                                        31
038700060510     C                   Z-ADD     *zeros        VABNSP
038800060510     C                   Z-ADD     1             VABRMN
038900060404     C                   EVAL      vinmsg = %trimr(vinmsg)
039000060510     C                             + ' ' + 'VABNSP VABRMN'
039100060404     C                   ENDIF
039200040420     C* CAD
039300130208     C                   EVAL      VABCAD=%trim(%subst(vindta:145:9))
039400130208     C***                EVAL      PiStr=%trim(%subst(vindta:145:9))
039500130208     C***                EXSR      CHKNUM
039600130208     C***                IF        PiInt=*on
039700130208     C***                Z-ADD     PiVal         Num5_0
039800130208     C***                MOVEL(p)  Num5_0        VABCAD
039900130208     C***                ELSE
040000130208     C***                SETON                                        32
040100130208     C***                EVAL      VABCAD = *zeros
040200130208     C***                EVAL      vinmsg = %trimr(vinmsg)
040300130208     C***                          + ' ' + 'VABCAD'
040400130208     C***                ENDIF
040500070502     C* Reperisco la provincia dal CAP e dalla località
040600070502     C                   IF        VABCAD <> *blanks AND
040700070502     C                             VABPRD  = *blanks
040800070502     C                   CLEAR                   TISI95DS
040900070502     C                   EVAL      I95TCN = '3'
041000070502     C                   Z-ADD     datcor        I95DAT
041100070502     C                   EVAL      I95CAP = VABCAD
041200070502     C                   EVAL      I95LOC = VABLOD
041300070502     C                   EVAL      I95NAR = VABNZD
041400070502     C                   CALL      'TISI95R'
041500070502     C                   PARM                    TISI95DS
041600070502     C                   EVAL      VABPRD = O95PRV
041700070502     C                   ENDIF
041800040420     C* NCL
041900070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
042000010607     C                   EXSR      CHKNUM
042100010607     C                   IF        PiInt=*on
042200070606     C                   Z-ADD     PiVal         VABNCL
042300010607     C                   ELSE
042400010607     C                   SETON                                        32
042500010607     C                   Z-ADD     *zeros        VABNCL
042600010607     C                   EVAL      vinmsg = %trimr(vinmsg)
042700010607     C                             + ' ' + 'VABNCL'
042800010607     C                   ENDIF
042900040420     C* PKB
043000070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
043100070208     C                   EVAL      PiDecChr = '.'
043200010607     C                   EXSR      CHKNUM
043300010607     C                   IF        PiNum=*on
043400070925     C                   Z-ADD(H)  PiVal         VABPKB
043500010607     C                   ELSE
043600010607     C                   SETON                                        32
043700010607     C                   Z-ADD     *zeros        VABPKB
043800010607     C                   EVAL      vinmsg = %trimr(vinmsg)
043900010607     C                             + ' ' + 'VABPKB'
044000010607     C                   ENDIF
044100070925     C* VLB
044200070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
044300070925     C                   EVAL      PiDecChr = '.'
044400070925     C                   EXSR      CHKNUM
044500070925     C                   IF        PiNum=*on
044600070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
044700070925     C                   Z-ADD     PiVal         VABVLB
044800070925     C                   ELSE
044900070925     C                   SETON                                        32
045000070925     C                   Z-ADD     *zeros        VABVLB
045100070925     C                   EVAL      vinmsg = %trimr(vinmsg)
045200070925     C                             + ' ' + 'VABVLB'
045300070925     C                   ENDIF
045400080311     C* CAS
045500080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
045600080311     C                             %subst(vindta:196:14) <> *zeros
045700080311     C                   EVAL      FlgCAS = '1'
045800080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
045900080311     C                   EVAL      PiDecChr = '.'
046000080311     C                   EXSR      CHKNUM
046100080311     C                   IF        PiNum=*on
046200080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
046300080311     C                   Z-ADD     PiVal         VABCAS
046400080311     C                   ELSE
046500080311     C                   SETON                                        32
046600120123     C                   Z-ADD     *zeros        VABCAS
046700080311     C                   EVAL      vinmsg = %trimr(vinmsg)
046800080311     C                             + ' ' + 'VABCAS'
046900080311     C                   ENDIF
047000080311     C                   ENDIF
047100020322     C*
047200020322     C* Considerazioni finali su CBO/CAS
047300050908     C                   IF        FlgCAS = '1'
047400040713     C                   IF        VABCBO = '1'
047500020322     C                   EVAL      VABCBO = '4'
047600040713     C                   ENDIF
047700040713     C                   IF        VABCBO = '2'
047800040713     C                   EVAL      VABCBO = '6'
047900040713     C                   ENDIF
048000020322     C                   ENDIF
048100020204     C*
048200020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048300020204     C                   EXSR      CHKIMPDIV
048400020204     C*
048500010607     C                   ENDSR
048600010607     C*----------------------------------------------------*
048700010601
048800020204
048900020204     C*----------------------------------------------------*
049000020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
049100020204     C*----------------------------------------------------*
049200020204     C     CHKIMPDIV     BEGSR
049300020204     C*
049400020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
049500020204     C                   Z-ADD     *zeros        wrkDec            9 9
049600020204     C*
049700020204     C* Come prima cosa effettuo considerazioni sulla divisa
049800020204     C                   IF        vabIAS > *zeros
049900020204     C                   IF        vabVAS <> 'EUR'
050000020204     C                   EVAL      vabVAS =  'ITL'
050100020204     C                   ENDIF
050200020204     C                   ENDIF
050300020204     C*
050400020204     C                   IF        vabCAS > *zeros
050500020204     C                   IF        vabVCA <> 'EUR'
050600020204     C                   EVAL      vabVCA =  'ITL'
050700020204     C                   ENDIF
050800020204     C                   ENDIF
050900020204     C*
051000020204     C                   IF        vabVMD > *zeros
051100020321     C                   IF        vabVAD <> 'EUR'
051200020204     C                   EVAL      vabVAD =  'ITL'
051300020204     C                   ENDIF
051400020204     C                   ENDIF
051500020204     C*
051600020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
051700020204     C                   Z-ADD     vabIAS        wrkDec
051800020204     C                   IF        wrkDec > *zeros
051900020204     C                   IF        vabVAS = 'ITL'
052000020204     C                   EVAL      vabIAS = *zeros
052100020204     C                   ENDIF
052200020204     C                   ENDIF
052300020204     C*
052400020204     C* Stabilisco se il contrasegno ha decimali valorizzati
052500020204     C                   Z-ADD     vabCAS        wrkDec
052600020204     C                   IF        wrkDec > *zeros
052700020204     C                   IF        vabVCA = 'ITL'
052800020204     C                   EVAL      vabCAS = *zeros
052900020204     C                   ENDIF
053000020204     C                   ENDIF
053100020204     C*
053200020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
053300020204     C                   Z-ADD     vabVMD        wrkDec
053400020204     C                   IF        wrkDec > *zeros
053500020204     C                   IF        vabVAD = 'ITL'
053600020204     C                   EVAL      vabVMD = *zeros
053700020204     C                   ENDIF
053800020204     C                   ENDIF
053900020204     C*
054000020204     C                   ENDSR
054100020204     C***
054200020204
054300010330
054400010330
054500010330     C*----------------------------------------------------*
054600010330     C*  CONTROLLO NUMERICITA' CAMPI
054700010330     C*----------------------------------------------------*
054800010330     C     CHKNUM        BEGSR
054900010330     C*
055000010606     C                   IF        PiDecChr = *blanks
055100070502     C                   EVAL      PiDecChr = '.'
055200010606     C                   ENDIF
055300010606     C*
055400010606     C                   CALL(e)   'ISNUMERIC'
055500010330     C                   PARM                    PiStr            30
055600010606     C                   PARM                    PiDecChr          1
055700010330     C                   PARM      *ZEROS        PiVal            30 9
055800010330     C                   PARM      '0'           PiInt             1
055900010330     C                   PARM      '0'           PiNum             1
056000010330     C                   IF        %error
056100010606     C                   EVAL      PiNum=*off
056200010330     C                   ENDIF
056300010330     C*
056400010330     C                   ENDSR
056500010330     C***
056600010330
056700010601
056800010601
056900010601
057000010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
057100010601     C     repfil        BEGSR
057200010601     C*
057300010601     C                   if        invfil = *zeros and
057400010601     C                             depfil > *zeros and
057500010629     C                             (vinflg = *blanks or
057600010629     C                              vinflg = *zeros)
057700010601     C
057800010601     C                   eval      invfil = depfil
057900010601     C                   endif
058000010601     C*
058100010601     C                   if        depfil <> invfil and
058200010601     C                             invfil > *zeros
058300010601     C                   eval      flgMulti = '1'
058400010601     C                   if        vinflg = *blanks
058500010601     C                   add       1             cntNonEl
058600010601     C                   endif
058700010601     C                   endif
058800010601     C*
058900010601     C                   if        vinflg = '2'
059000010601     C                   eval      flgStato = '2'
059100010601     C                   endif
059200010601     C*
059300010601     C                   ENDSR
059400010601     C***
059500010601
059600010601
059700010601
059800010330
059900010330
060000010330
060100990920      /TITLE Invio dei dati al punto operativo.
060200000613     C     invio         BEGSR
060300990920     C*
060400990920     C                   reset                   dscmz
060500010601     C                   move      invfil        cmzdst
060600990920     C                   eval      cmzfld = vlrfou
060700990920     C                   eval      cmzmbd = vlrhdl
060800990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
060900000710     C                   if        prmfir = *blanks
061000050408     C                   eval      cmzfla = 'FIVAB00F'
061100050408     C                   eval      cmzmba = 'FIVAB00F'
061200000710     C                   else
061300000710     C                   eval      cmzfla = prmfir
061400000710     C                   eval      cmzmba = prmfir
061500000710     C                   endif
061600990920     C                   eval      cmznrr = *zeros
061700990920     C                   move      §ctrok        cmznrr
061800021018     C                   eval      cmzlba = vlrfl1
061900990920     C                   call(e)   'TIS711C'
062000990920     C                   parm                    dscmz
062100990921     C                   parm      *blanks       esito
062200990920     C                   if        %error
062300990920     C                             or cmzerr = '1'
062400990921     C                             or esito  = '1'
062500000710     C                   eval      wrkesito = '3'
062600990920     C                   endif
062700990920     C*
062800000613     C                   ENDSR
062900130208
063000130208     C*--------------------------------------------------------
063100130208     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
063200130208     C*--------------------------------------------------------
063300130208     C     CARTAB        BEGSR
063400130208     C*
063500130208     C* TABELLA '15' - NAZIONI
063600130208     C                   clear                   skNAZISO
063700130208     C                   clear                   skNAZBAR
063800130208     C                   eval      tblKUT = 1
063900130208     C                   eval      tblCOD = '15'
064000130208     C     KEYtabP       setll     tabel00f
064100130208     C     KEYtabP       reade     tabel00f
064200130208     C                   dow       not %eof(tabel00f)
064300130208     C                   if        tblFLG = *blanks
064400130208     C                   movel(p)  tblUNI        ds15
064500130208     C                   add       1             jNAZ
064600130208     C                   eval      skNAZBAR(jNAZ) = tblKEY
064700130208     C                   eval      skNAZISO(jNAZ) = §15COD
064800130208     C                   endif
064900130208     C     KEYtabP       reade     tabel00f
065000130208     C                   enddo
065100130208     C*
065200130208     C                   ENDSR
065300130208     C***
065400990910
065500010601
065600010601
065700010601
065800010601
065900010601      /TITLE Invio dei dati al punto operativo.
066000010601     C     opeini        BEGSR
066100010601     C*
066200010601     C* Inizializzo flag e contatori operativi
066300010601     C                   movel     '0'           flgGiro           1
066400010601     C                   movel     '0'           flgMulti          1
066500010601     C                   movel     '1'           flgStato          1
066600010615     C                   movel     '0'           flgOk             1
066700010601     C                   z-add     *zeros        cntNonEl         10 0
066800010601     C                   z-add     *zeros        depfil            3 0
066900010601     C                   z-add     *zeros        invfil            3 0
067000010601     C*
067100010601     C                   ENDSR
067200010601     C***
067300010601
067400010601
067500010601
067600010330
067700010330
067800000613     C     *inzsr        BEGSR
067900990910     C*
068000990910     C     *entry        plist
068100990920     C                   parm                    tivlrds
068200990921     C                   parm      wrkesito      esito
068300000724     C                   parm                    prmlit
068400000710     C                   parm                    prmfir
068500010330     C*
068600010330     C* CALCOLA LA DATA CORRENTE
068700010330     C                   time                    wn14             14 0
068800010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
068900010330     C                   z-add     wn8           g08dat
069000010330     C                   z-add     *zeros        g08inv
069100010330     C                   movel     '0'           g08err
069200010330     C                   call      'XSRDA8'
069300010330     C                   parm                    wlbda8
069400010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
069500130208     C*
069600130208     C* Chiave su TABEL00F - parziale
069700130208     C     KEYtabP       KLIST
069800130208     C                   KFLD                    tblKUT
069900130208     C                   KFLD                    tblCOD
070000000613     C*
070100000613     C                   ENDSR
070200000613     C***
