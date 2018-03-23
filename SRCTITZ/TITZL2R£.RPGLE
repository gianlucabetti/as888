000100171003      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300171003      *
000400171003      * PARTICOLARITA':
000500171003      *
000600130208     Ftabel00f  if   e           k disk
000700990910     Ftivin00r  uF   E             DISK    usropn
000800121005     FEDIVABWR  O    E             DISK    usropn
000900171003     FEDIVATwr  O    E             DISK    usropn
001000000313     D*
001100010330     D*----------------------------------------------------
001200010330     D* DICHIARAZIOINE VARIABILI DI WRK
001300010330     D*----------------------------------------------------
001400010330     D dscmz         e ds                  inz
001500010330     D psds           sds
001600010330     D  procname         *PROC
001700010330     D tivlrds       e ds                  extname(tivlr00f)
001800130208     D ds15          e ds
001900070502     D tisi95ds      e ds
002000010330     D esito           s              1
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D rrnum           s              6  0 INZ(*zeros)
002500130208     D jNAZ            s              5  0 INZ(*zeros)
002600171003     D parccm          s              8    INZ(*blanks)
002700171003     D parmbr          s             10    INZ(*blanks)
002800171003     D paropz          s              1    INZ(*blanks)
002900171003     D chkcall         s              1    INZ(*blanks)
003000010330     D*------------------
003100010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200010330     D*------------------
003300010330     D WLBDA8          DS                  INZ
003400010330     D  G08DAT                 1      8  0
003500010330     D  G08INV                 9     16  0
003600010330     D  G08ERR                17     17
003700010330     D  G08TGI                18     22  0
003800130208     D*------------
003900130208     D skNAZISO        S              3    DIM(1000)
004000130208     D skNAZBAR        S              3    DIM(1000)
004100010330
004200010330
004300000913     C                   reset                   rrnum
004400990921     C                   reset                   esito
004500990921     C                   reset                   wrkesito
004600010601     C*
004700130208     C                   EXSR      CARTAB                                       CARICA TABELLE
004800010601     C                   exsr      opeini
004900010605     C                   exsr      rwvab
005000171003     C*
005100171003     C* Effettuo la chiamata al CLLE preposto
005200171003     C                   call(e)   'TITVEVTC'
005300171003     C                   parm                    parccm
005400171003     C                   parm                    parmbr
005500171003     C                   parm      '2'           paropz
005600070502     C*
005700070502     C* Effettuo lancio TISI95 solo x chiusura
005800070502     C                   CLEAR                   TISI95DS
005900070502     C                   EVAL      I95TLA = 'C'
006000070502     C                   CALL      'TISI95R'
006100070502     C                   PARM                    TISI95DS
006200010601     C*
006300010601     C                   seton                                        lr
006400010601
006500010601
006600010601
006700010601
006800010601     C*--------------------------------------------------------
006900010601     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007000010601     C*--------------------------------------------------------
007100010601     C     PREELA        BEGSR
007200010601     C*
007300010601     C* SE OCCORRE SPEDIRE IN FILIALE
007400010601     C                   if        invfil <> *zeros and
007500010601     C                             flgGiro = '0'
007600010601     C*
007700010601     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
007800010601     C                   eval      flgGiro = '1'
007900010601     C*
008000010601     C                   endif
008100010601     C*
008200010601     C                   ENDSR
008300010601     C***
008400010601
008500010601
008600010601
008700010601     C*--------------------------------------------------------
008800010601     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
008900010601     C*--------------------------------------------------------
009000010601     C     ENDELA        BEGSR
009100000616     C*
009200010601     C                   ENDSR
009300010601     C***
009400000613
009500010601
009600010601
009700010330     C*--------------------------------------------------------
009800171003     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR/EDIVATWR     *
009900010330     C*--------------------------------------------------------
010000010605     C     RWVAB         BEGSR
010100050408     C*
010200010330     C                   if        not %open(tivin00r)
010300010330     C                   open      tivin00r
010400010330     C                   endif
010500121005     C                   if        not %open(edivabwr)
010600121005     C                   open      edivabwr
010700010330     C                   endif
010800171003     C*
010900171003     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
011000171003     C                   exsr      prevat
011100171003     C*
011200171003     C                   if        chkcall = '0'
011300171003     C*
011400171003     C                   if        not %open(edivatwr)
011500171003     C                   open      edivatwr
011600171003     C                   endif
011700010330     C*
011800171003     C                   clear                   §CTROKVB
011900171003     C                   clear                   §CTROKVT          5 0
012000010604     C                   clear                   §CTRMO
012100010604     C                   clear                   §CTRNO
012200010330     C*
012300010330     C                   DO        *HIVAL
012400010330     C*
012500010330     C                   READ      tivin00r                               70
012600010618     C*
012700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012800010618     C                   if        vinflg = '1'
012900010618     C                   eval      flgOk = '1'
013000010618     C                   endif
013100010618     C*
013200010330     C                   if        vindta > *blanks
013300010330     C                   add       1             rrnum
013400010330     C*
013500010601     C                   if        *in70 = *off and
013600010330     C                             (vinflg = *blanks
013700010330     C                              or vinflg = '0'
013800010330     C                              or vinflg = '2')
013900010330     C*
014000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014100010711     C                   if        vinflg = *blanks or vinflg = '0'
014200010711     C                   clear                   vinmsg
014300010711     C                   endif
014400070601     C*
014500070601     C                   exsr      inzvar
014600070601     C                   exsr      defcam
014700070601     C                   exsr      impvab
014800010601     C*
014900010601     C* Effettuo considerazioni x elaborazioni "multi-filiale"
015000010605     C                   eval      depfil = VABLNP
015100010601     C                   exsr      repfil
015200010601     C                   if        depfil = invfil
015300021025     C                   if        vlrpoi = 999
015400021025     C                   MOVE(P)   invfil        VABFGS
015500021025     C                   else
015600021025     C                   MOVE(P)   vlrpoi        VABFGS
015700021025     C                   endif
015800171003     C                   EVAL      VATFGS = VABFGS
015900171003     C*
016000171003     C* Scrivo sempre le estensioni del record corrente
016100171003     C                   exsr      exevati                                      => write VAT-E
016200010601     C*
016300010601     C                   exsr      PREELA
016400010601     C*
016500010604     C* Ebbene...
016600010604     C*
016700171003     C  N31              ADD       1             §CTROKVB          7 0
016800010604     C   32              ADD       1             §CTRMO            7 0
016900010604     C   31              ADD       1             §CTRNO            7 0
017000010604     C*
017100071120     C                   exsr      wrivab                                       => scrivo VAB
017200071120     C*
017300020722     C                   endif
017400020722     C*
017500010604     C                   if        *in31 = *off and
017600010604     C                             *in32 = *off
017700010604     C                   eval      vinflg = '1'
017800010604     C                   else
017900010604     C                   eval      vinflg = '2'
018000010604     C                   endif
018100010604     C                   endif
018200010604     C*
018300010330     C                   else
018400010330     C                   eval      vinflg = '1'
018500010330     C                   endif
018600010601     C*
018700010601     C  N70              update    tivin000
018800010330     C*
018900010330     C  N70              ENDdo
019000171003     C*
019100171003     C                   endif
019200010601     C*
019300010601     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
019400010601     C                   if        cntNonEl = *zeros or
019500010601     C                             flgMulti = '0'
019600010330     C* Se non ci sono record con errori ...
019700010601     C                   if        §ctrno = 0 and
019800010604     C                             §ctrmo = 0 and
019900010601     C                             flgStato <> '2'
020000010330     C* ... restituisco esito OK.
020100010330     C                   eval      wrkesito = '0'
020200010330     C                   else
020300171003     C                   if        §ctrokvb > 0
020400010330     C                   eval      wrkesito = '1'
020500010330     C                   else
020600010615     C                   if        flgOk = '0'
020700010615     C                   eval      wrkesito = '2'
020800010615     C                   else
020900010615     C                   eval      wrkesito = '6'
021000010615     C                   endif
021100010330     C                   endif
021200010330     C                   endif
021300010601     C                   else
021400010601     C                   eval      wrkesito = '9'
021500010601     C                   endif
021600010330     C*
021700010330     C                   if        %open(tivin00r)
021800010330     C                   close     tivin00r
021900010330     C                   endif
022000121005     C                   if        %open(edivabwr)
022100121005     C                   close     edivabwr
022200010330     C                   endif
022300171003     C                   if        %open(edivatwr)
022400171003     C                   close     edivatwr
022500171003     C                   endif
022600010601     C*
022700010601     C                   if        vlrpoi <> 999
022800010601     C                   eval      invfil = vlrpoi
022900010601     C                   endif
023000010330     C*
023100171003     C                   if        §ctrokvb > 0
023200010601     C                             and invfil > *zeros
023300010330     C                   exsr      invio
023400010330     C                   endif
023500010601     C*
023600010618     C                   if        flgGiro = '1'
023700010601     C                   exsr      endela
023800010618     C                   endif
023900010330     C*
024000010330     C                   ENDSR
024100010330     C***
024200070601
024300070601     C*----------------------------------------------------*
024400070601     C*  SCARICAMENTO BUFFER RECORDS VAB
024500070601     C*----------------------------------------------------*
024600070601     C     WRIVAB        BEGSR
024700171003     C*
024800121005     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
024900121015     C                   EVAL      VABCMR = %char(datcor)
025000121005     C                   EVAL      VABDCM = datcor
025100121005     C                   EVAL      VABDTS = datcor
025200121005     C                   EVAL      VABHMS = oracor
025300121005     C                   EVAL      VABCNT = 1
025400070601     C*
025500121005     C  N31              WRITE     EDIVAB00
025600070601     C*
025700070601     C                   ENDSR
025800010601
025900010601
026000010601
026100010330     C*----------------------------------------------------*
026200020722     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026300010330     C*----------------------------------------------------*
026400010330     C     INZVAR        BEGSR
026500171003     C*
026600171003     C* Inizializzo il buffer del record da scrivere
026700171003     C                   CLEAR                   EDIVAB00
026800171003     C                   CLEAR                   EDIVAT00
026900010330     C*
027000020204     C                   Z-ADD     *zeros        Num5_0            5 0
027100020322     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027200020322     C                   MOVEL     '0'           FlgCAS            1
027300010330     C*
027400010330     C                   ENDSR
027500010330     C*----------------------------------------------------*
027600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
027700010330     C*----------------------------------------------------*
027800010330     C     DEFCAM        BEGSR
027900010330     C*
028000020204     C* Imposto i valori di default...
028100080311     C                   EVAL      VABCCM = 0661040
028200171003     C                   EVAL      VATCCM = 0661040
028300080311     C                   EVAL      VABLNP = 001
028400171003     C                   EVAL      VATLNP = 001
028500080311     C                   EVAL      VABCTR = 100
028600080311     C                   EVAL      VABCTM = '7Q'
028700070208     C                   EVAL      VABCBO = '1'
028800020204     C* ... e poi verifico se sono stati passati come parametri
028900020204     C                   IF        vlrppt > *blanks
029000040301     C                   IF        %trim(%subst(vlrppt:1:7)) <> *blanks
029100020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029200020204     C                   EXSR      CHKNUM
029300020204     C                   IF        PiInt=*on
029400020204     C                   Z-ADD     PiVal         VABCCM
029500171003     C                   Z-ADD     PiVal         VATCCM
029600020204     C                   ENDIF
029700040301     C                   ENDIF
029800040301     C                   IF        %trim(%subst(vlrppt:8:3)) <> *blanks
029900020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030000020204     C                   EXSR      CHKNUM
030100020204     C                   IF        PiInt=*on
030200020204     C                   Z-ADD     PiVal         VABLNP
030300171003     C                   Z-ADD     PiVal         VATLNP
030400020204     C                   ENDIF
030500040301     C                   ENDIF
030600040301     C                   IF        %trim(%subst(vlrppt:11:3)) <> *blanks
030700020204     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
030800020204     C                   EXSR      CHKNUM
030900020204     C                   IF        PiInt=*on
031000020204     C                   Z-ADD     PiVal         VABCTR
031100040301     C                   ENDIF
031200020204     C                   ENDIF
031300020204     C                   ENDIF
031400020204     C*
031500010330     C                   ENDSR
031600010607     C*----------------------------------------------------*
031700121005     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
031800010607     C*----------------------------------------------------*
031900010607     C     IMPVAB        BEGSR
032000010607     C*
032100010607     C                   SETOFF                                       3132
032200010607     C*
032300010607     C* Reperimento campi ALFA
032400010607     C*
032500010607     C* Considerazioni sulla ragione sociale del destinatario da indicare
032600070925     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
032700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032900020117     C* ==
033000080311     C                   EVAL      VABRD2=%trim(%subst(vindta:407:20))
033100070925     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
033200070925     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
033300070925     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
033400130208     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
033500130208     C                   Z-ADD     1             jNAZ
033600130208     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
033700130208     C                   IF        %found
033800130208     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
033900130208     C                   ENDIF
034000070925     C                   EVAL      VABRMA=%trim(%subst(vindta:35:6))
034100080311     C                   EVAL      VABNOT=%trim(%subst(vindta:306:35))
034200080311     C                   EVAL      VABNT2=%trim(%subst(vindta:427:35))
034300080311     C                   IF        %subst(vindta:210:1) = 'M'
034400080311     C                   EVAL      VABTIC='BM'
034500080311     C                   ENDIF
034600080311     C                   IF        %subst(vindta:210:1) = 'N'
034700080311     C                   EVAL      VABTIC='TM'
034800080311     C                   ENDIF
034900100527     C                   IF        %subst(vindta:210:1) = 'R'
035000100527     C                   EVAL      VABTIC='CM'
035100080311     C                   ENDIF
035200080311     C                   IF        %subst(vindta:227:4) = *blanks
035300080311     C                   EVAL      VABVCA='EUR'
035400080311     C                   ENDIF
035500080311     C                   IF        %subst(vindta:231:1) = 'I'
035600080311     C                   EVAL      VABTC1='A'
035700080311     C                   ENDIF
035800080311     C                   IF        %subst(vindta:231:1) = '2'
035900080311     C                   EVAL      VABTC2='P'
036000080311     C                   ENDIF
036100080311     C                   IF        %subst(vindta:231:1) = '1'
036200080311     C                   EVAL      VABTC2='S'
036300080311     C                   ENDIF
036400060510     C*
036500060510     C* Reperimento campi NUMERICI
036600060510     C                   MOVEL     DATCOR        VABAAS
036700060510     C                   MOVE      DATCOR        VABMGS
036800171003     C                   MOVEL     DATCOR        VATAAS
036900080311     C* CCM
037000080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC300'
037100080311     C                   EVAL      VABCCM=0661040
037200080311     C                   ENDIF
037300080311     C                   IF        %trim(%subst(vindta:23:6)) = 'MC3000'
037400080311     C                   EVAL      VABCCM=0661069
037500080311     C                   ENDIF
037600150519     C                   IF        %trim(%subst(vindta:23:6)) = 'RE5775' AND
037700150519     C                             VABCCM<>2200910
037800120920     C                   EVAL      VABCCM=2494836
037900120920     C                   ENDIF
038000121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD000'
038100121218     C                   EVAL      VABCCM=0661890
038200121218     C                   ENDIF
038300121218     C                   IF        %trim(%subst(vindta:23:6)) = 'AD0000'
038400121218     C                   EVAL      VABCCM=0661889
038500121218     C                   ENDIF
038600121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES070'
038700121218     C                   EVAL      VABCCM=0661892
038800121218     C                   ENDIF
038900121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0700'
039000121218     C                   EVAL      VABCCM=0661891
039100121218     C                   ENDIF
039200121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES380'
039300121218     C                   EVAL      VABCCM=0661895
039400121218     C                   ENDIF
039500121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES3800'
039600121218     C                   EVAL      VABCCM=0661894
039700121218     C                   ENDIF
039800121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES160'
039900121218     C                   EVAL      VABCCM=0661896
040000121218     C                   ENDIF
040100121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES1600'
040200121218     C                   EVAL      VABCCM=0661893
040300121218     C                   ENDIF
040400121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT000'
040500121218     C                   EVAL      VABCCM=0661888
040600121218     C                   ENDIF
040700121218     C                   IF        %trim(%subst(vindta:23:6)) = 'PT0000'
040800121218     C                   EVAL      VABCCM=0661887
040900121218     C                   ENDIF
041000121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES000'
041100121218     C                   EVAL      VABCCM=0661886
041200121218     C                   ENDIF
041300121218     C                   IF        %trim(%subst(vindta:23:6)) = 'ES0000'
041400121218     C                   EVAL      VABCCM=0661885
041500121218     C                   ENDIF
041600171003     C                   EVAL      VATCCM=VABCCM
041700080311     C* DCR
041800080311     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
041900080311     C                   EXSR      CHKNUM
042000080311     C                   IF        PiInt=*on
042100080311     C                   Z-ADD     PiVal         VABDCR
042200080311     C                   ELSE
042300100527     C                   Z-ADD     *zeros        VABDCR
042400080311     C                   EVAL      vinmsg = %trimr(vinmsg)
042500080311     C                             + ' ' + 'VABDCR'
042600080311     C                   ENDIF
042700060710     C* NSP/RMN
042800080311     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
042900060510     C                   EXSR      CHKNUM
043000060510     C                   IF        PiInt=*on
043100060510     C                   Z-ADD     PiVal         VABNSP
043200171003     C                   Z-ADD     PiVal         VATNSP
043300060510     C                   Z-ADD     PiVal         VABRMN
043400060404     C                   ELSE
043500070502     C                   SETON                                        31
043600060510     C                   Z-ADD     *zeros        VABNSP
043700171003     C                   Z-ADD     *zeros        VATNSP
043800060510     C                   Z-ADD     1             VABRMN
043900060404     C                   EVAL      vinmsg = %trimr(vinmsg)
044000060510     C                             + ' ' + 'VABNSP VABRMN'
044100060404     C                   ENDIF
044200040420     C* CAD
044300130208     C                   EVAL      VABCAD=%trim(%subst(vindta:145:9))
044400130208     C****               EVAL      PiStr=%trim(%subst(vindta:145:9))
044500130208     C****               EXSR      CHKNUM
044600130208     C****               IF        PiInt=*on
044700130208     C****               Z-ADD     PiVal         Num5_0
044800130208     C****               MOVEL(p)  Num5_0        VABCAD
044900130208     C****               ELSE
045000130208     C****               SETON                                        32
045100130208     C****               EVAL      VABCAD = *zeros
045200130208     C****               EVAL      vinmsg = %trimr(vinmsg)
045300130208     C****                         + ' ' + 'VABCAD'
045400130208     C****               ENDIF
045500070502     C* Reperisco la provincia dal CAP e dalla località
045600170223     C                   IF        VABNZD  = *blanks AND
045700170223     C                             VABCAD <> *blanks AND
045800070502     C                             VABPRD  = *blanks
045900070502     C                   CLEAR                   TISI95DS
046000070502     C                   EVAL      I95TCN = '3'
046100070502     C                   Z-ADD     datcor        I95DAT
046200070502     C                   EVAL      I95CAP = VABCAD
046300070502     C                   EVAL      I95LOC = VABLOD
046400070502     C                   EVAL      I95NAR = VABNZD
046500070502     C                   CALL      'TISI95R'
046600070502     C                   PARM                    TISI95DS
046700070502     C                   EVAL      VABPRD = O95PRV
046800070502     C                   ENDIF
046900040420     C* NCL
047000070925     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
047100010607     C                   EXSR      CHKNUM
047200010607     C                   IF        PiInt=*on
047300070606     C                   Z-ADD     PiVal         VABNCL
047400010607     C                   ELSE
047500010607     C                   SETON                                        32
047600010607     C                   Z-ADD     *zeros        VABNCL
047700010607     C                   EVAL      vinmsg = %trimr(vinmsg)
047800010607     C                             + ' ' + 'VABNCL'
047900010607     C                   ENDIF
048000040420     C* PKB
048100070925     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
048200070208     C                   EVAL      PiDecChr = '.'
048300010607     C                   EXSR      CHKNUM
048400010607     C                   IF        PiNum=*on
048500070925     C                   Z-ADD(H)  PiVal         VABPKB
048600010607     C                   ELSE
048700010607     C                   SETON                                        32
048800010607     C                   Z-ADD     *zeros        VABPKB
048900010607     C                   EVAL      vinmsg = %trimr(vinmsg)
049000010607     C                             + ' ' + 'VABPKB'
049100010607     C                   ENDIF
049200070925     C* VLB
049300070925     C                   EVAL      PiStr=%trim(%subst(vindta:184:6))
049400070925     C                   EVAL      PiDecChr = '.'
049500070925     C                   EXSR      CHKNUM
049600070925     C                   IF        PiNum=*on
049700070925     C                   EVAL(H)   PiVal = PiVal / 1000                         * gest. 3 decimali
049800070925     C                   Z-ADD     PiVal         VABVLB
049900070925     C                   ELSE
050000070925     C                   SETON                                        32
050100070925     C                   Z-ADD     *zeros        VABVLB
050200070925     C                   EVAL      vinmsg = %trimr(vinmsg)
050300070925     C                             + ' ' + 'VABVLB'
050400070925     C                   ENDIF
050500080311     C* CAS
050600080311     C                   IF        %subst(vindta:196:14) <> *blanks AND
050700080311     C                             %subst(vindta:196:14) <> *zeros
050800080311     C                   EVAL      FlgCAS = '1'
050900080311     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
051000080311     C                   EVAL      PiDecChr = '.'
051100080311     C                   EXSR      CHKNUM
051200080311     C                   IF        PiNum=*on
051300080311     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
051400080311     C                   Z-ADD     PiVal         VABCAS
051500080311     C                   ELSE
051600080311     C                   SETON                                        32
051700120123     C                   Z-ADD     *zeros        VABCAS
051800080311     C                   EVAL      vinmsg = %trimr(vinmsg)
051900080311     C                             + ' ' + 'VABCAS'
052000080311     C                   ENDIF
052100080311     C                   ENDIF
052200020322     C*
052300020322     C* Considerazioni finali su CBO/CAS
052400050908     C                   IF        FlgCAS = '1'
052500040713     C                   IF        VABCBO = '1'
052600020322     C                   EVAL      VABCBO = '4'
052700040713     C                   ENDIF
052800040713     C                   IF        VABCBO = '2'
052900040713     C                   EVAL      VABCBO = '6'
053000040713     C                   ENDIF
053100020322     C                   ENDIF
053200020204     C*
053300020204     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053400020204     C                   EXSR      CHKIMPDIV
053500020204     C*
053600010607     C                   ENDSR
053700010607     C*----------------------------------------------------*
053800171003     C*----------------------------------------------------*
053900171003     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT) - TIPO RECORD "I" e "J"
054000171003     C*----------------------------------------------------*
054100171003     C     EXEVATI       BEGSR
054200171003     C*
054300171003     C                   IF        %subst(vindta:502:35) <> *blank
054400171003     C                   EVAL      VATTRC='I'
054500171003     C                   EVAL      VATNOT = %trim(%subst(vindta:502:35))
054600171003     C                   exsr      wrivat                                       => scarico VAT
054700171003     C                   endif
054800171003     C*
054900171003     C                   IF        %subst(vindta:502+35:15) <> *blank
055000171005     C                   EVAL      VATTRC='J'
055100171003     C                   EVAL      VATNOT = %trim(%subst(vindta:502+35:15))
055200171003     C                   exsr      wrivat                                       => scarico VAT
055300171003     C                   endif
055400171003     C*
055500171003     C                   ENDSR
055600171003     C*----------------------------------------------------*
055700171003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT)
055800171003     C*----------------------------------------------------*
055900171003     C     WRIVAT        BEGSR
056000171003     C*
056100171003     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
056200171003     C                   EVAL      VATCMR = %char(datcor)
056300171003     C                   EVAL      VATCNT = 1
056400171003     C*
056500171003     C* Scrivo solo se valorizzato qualcosa
056600171003     C                   IF        VATNOT <> *blanks
056700171003     C                   WRITE     EDIVAT00
056800171003     C                   ENDIF
056900171003     C*
057000171003     C                   ENDSR
057100010601
057200020204
057300020204     C*----------------------------------------------------*
057400020204     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
057500020204     C*----------------------------------------------------*
057600020204     C     CHKIMPDIV     BEGSR
057700020204     C*
057800020204     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
057900020204     C                   Z-ADD     *zeros        wrkDec            9 9
058000020204     C*
058100020204     C* Come prima cosa effettuo considerazioni sulla divisa
058200020204     C                   IF        vabIAS > *zeros
058300020204     C                   IF        vabVAS <> 'EUR'
058400020204     C                   EVAL      vabVAS =  'ITL'
058500020204     C                   ENDIF
058600020204     C                   ENDIF
058700020204     C*
058800020204     C                   IF        vabCAS > *zeros
058900020204     C                   IF        vabVCA <> 'EUR'
059000020204     C                   EVAL      vabVCA =  'ITL'
059100020204     C                   ENDIF
059200020204     C                   ENDIF
059300020204     C*
059400020204     C                   IF        vabVMD > *zeros
059500020321     C                   IF        vabVAD <> 'EUR'
059600020204     C                   EVAL      vabVAD =  'ITL'
059700020204     C                   ENDIF
059800020204     C                   ENDIF
059900020204     C*
060000020204     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
060100020204     C                   Z-ADD     vabIAS        wrkDec
060200020204     C                   IF        wrkDec > *zeros
060300020204     C                   IF        vabVAS = 'ITL'
060400020204     C                   EVAL      vabIAS = *zeros
060500020204     C                   ENDIF
060600020204     C                   ENDIF
060700020204     C*
060800020204     C* Stabilisco se il contrasegno ha decimali valorizzati
060900020204     C                   Z-ADD     vabCAS        wrkDec
061000020204     C                   IF        wrkDec > *zeros
061100020204     C                   IF        vabVCA = 'ITL'
061200020204     C                   EVAL      vabCAS = *zeros
061300020204     C                   ENDIF
061400020204     C                   ENDIF
061500020204     C*
061600020204     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
061700020204     C                   Z-ADD     vabVMD        wrkDec
061800020204     C                   IF        wrkDec > *zeros
061900020204     C                   IF        vabVAD = 'ITL'
062000020204     C                   EVAL      vabVMD = *zeros
062100020204     C                   ENDIF
062200020204     C                   ENDIF
062300020204     C*
062400020204     C                   ENDSR
062500020204     C***
062600020204
062700010330
062800010330
062900171003     C*----------------------------------------------------*
063000171003     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
063100171003     C*----------------------------------------------------*
063200171003     C     PREVAT        BEGSR
063300171003     C*
063400171003     C* Compongo il nome del membro da dare al EDIVATWR
063500171003     C                   eval      parmbr = vlrhdl
063600171003     C                   movel     'M'           parmbr
063700171003     C                   eval      parccm = %subst(vlrKSC:2:7)
063800171003     C                   eval      paropz = '1'
063900171003     C* Effettuo la chiamata al CLLE preposto
064000171003     C                   call(e)   'TITVEVTC'
064100171003     C                   parm                    parccm
064200171003     C                   parm                    parmbr
064300171003     C                   parm                    paropz
064400171003     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064500171003     C                   if        %error
064600171003     C                   movel     '1'           chkcall
064700171003     C                   else
064800171003     C                   movel     '0'           chkcall
064900171003     C                   endif
065000171003     C*
065100171003     C                   ENDSR
065200010330     C*----------------------------------------------------*
065300010330     C*  CONTROLLO NUMERICITA' CAMPI
065400010330     C*----------------------------------------------------*
065500010330     C     CHKNUM        BEGSR
065600010330     C*
065700010606     C                   IF        PiDecChr = *blanks
065800070502     C                   EVAL      PiDecChr = '.'
065900010606     C                   ENDIF
066000010606     C*
066100010606     C                   CALL(e)   'ISNUMERIC'
066200010330     C                   PARM                    PiStr            30
066300010606     C                   PARM                    PiDecChr          1
066400010330     C                   PARM      *ZEROS        PiVal            30 9
066500010330     C                   PARM      '0'           PiInt             1
066600010330     C                   PARM      '0'           PiNum             1
066700010330     C                   IF        %error
066800010606     C                   EVAL      PiNum=*off
066900010330     C                   ENDIF
067000010330     C*
067100010330     C                   ENDSR
067200010330     C***
067300010330
067400010601
067500010601
067600010601
067700010601      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
067800010601     C     repfil        BEGSR
067900010601     C*
068000010601     C                   if        invfil = *zeros and
068100010601     C                             depfil > *zeros and
068200010629     C                             (vinflg = *blanks or
068300010629     C                              vinflg = *zeros)
068400010601     C
068500010601     C                   eval      invfil = depfil
068600010601     C                   endif
068700010601     C*
068800010601     C                   if        depfil <> invfil and
068900010601     C                             invfil > *zeros
069000010601     C                   eval      flgMulti = '1'
069100010601     C                   if        vinflg = *blanks
069200010601     C                   add       1             cntNonEl
069300010601     C                   endif
069400010601     C                   endif
069500010601     C*
069600010601     C                   if        vinflg = '2'
069700010601     C                   eval      flgStato = '2'
069800010601     C                   endif
069900010601     C*
070000010601     C                   ENDSR
070100010601     C***
070200010601
070300010601
070400010601
070500010330
070600010330
070700010330
070800990920      /TITLE Invio dei dati al punto operativo.
070900000613     C     invio         BEGSR
071000171003     C*
071100171003     C* 1° invio EDIVAT
071200171003     C                   reset                   dscmz
071300171003     C                   move      vlrpoi        cmzdst
071400171003     C                   eval      cmzfld = 'EDIVATWR'
071500171003     C                   eval      cmzmbd = vlrhdl
071600171003     C                   eval      %subst(cmzmbd:1:1) = 'M'
071700171003     C***                if        prmfir = *blanks
071800171003     C                   eval      cmzfla = 'EDIVAT0F'
071900171003     C                   eval      cmzmba = 'EDIVAT0F'
072000171003     C***                else
072100171003     C***                eval      cmzfla = prmfir
072200171003     C***                eval      cmzmba = prmfir
072300171003     C***                endif
072400171003     C                   eval      cmznrr = *zeros
072500171003     C                   move      §ctrokvt      cmznrr
072600171003     C                   eval      cmzlba = vlrfl1
072700171003     C                   call(e)   'TIS711C'
072800171003     C                   parm                    dscmz
072900171003     C                   parm      *blanks       esito
073000171003     C                   if        %error
073100171003     C                             or cmzerr = '1'
073200171003     C                             or esito  = '1'
073300171003     C                   eval      wrkesito = '3'
073400171003     C                   else
073500171003     C*
073600171003     C* 2° invio EDIVAB
073700990920     C                   reset                   dscmz
073800010601     C                   move      invfil        cmzdst
073900990920     C                   eval      cmzfld = vlrfou
074000990920     C                   eval      cmzmbd = vlrhdl
074100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
074200171003     C***                if        prmfir = *blanks
074300121005     C                   eval      cmzfla = 'EDIVAB0F'
074400121005     C                   eval      cmzmba = 'EDIVAB0F'
074500171003     C***                else
074600171003     C***                eval      cmzfla = prmfir
074700171003     C***                eval      cmzmba = prmfir
074800171003     C***                endif
074900990920     C                   eval      cmznrr = *zeros
075000171003     C                   move      §ctrokvb      cmznrr
075100021018     C                   eval      cmzlba = vlrfl1
075200990920     C                   call(e)   'TIS711C'
075300990920     C                   parm                    dscmz
075400990921     C                   parm      *blanks       esito
075500990920     C                   if        %error
075600990920     C                             or cmzerr = '1'
075700990921     C                             or esito  = '1'
075800000710     C                   eval      wrkesito = '3'
075900990920     C                   endif
076000171003     C                   endif
076100990920     C*
076200000613     C                   ENDSR
076300130208
076400130208     C*--------------------------------------------------------
076500130208     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
076600130208     C*--------------------------------------------------------
076700130208     C     CARTAB        BEGSR
076800130208     C*
076900130208     C* TABELLA '15' - NAZIONI
077000130208     C                   clear                   skNAZISO
077100130208     C                   clear                   skNAZBAR
077200130208     C                   eval      tblKUT = 1
077300130208     C                   eval      tblCOD = '15'
077400130208     C     KEYtabP       setll     tabel00f
077500130208     C     KEYtabP       reade     tabel00f
077600130208     C                   dow       not %eof(tabel00f)
077700130208     C                   if        tblFLG = *blanks
077800130208     C                   movel(p)  tblUNI        ds15
077900130208     C                   add       1             jNAZ
078000130208     C                   eval      skNAZBAR(jNAZ) = tblKEY
078100130208     C                   eval      skNAZISO(jNAZ) = §15COD
078200130208     C                   endif
078300130208     C     KEYtabP       reade     tabel00f
078400130208     C                   enddo
078500130208     C*
078600130208     C                   ENDSR
078700130208     C***
078800990910
078900010601
079000010601
079100010601
079200010601
079300010601      /TITLE Invio dei dati al punto operativo.
079400010601     C     opeini        BEGSR
079500010601     C*
079600010601     C* Inizializzo flag e contatori operativi
079700010601     C                   movel     '0'           flgGiro           1
079800010601     C                   movel     '0'           flgMulti          1
079900010601     C                   movel     '1'           flgStato          1
080000010615     C                   movel     '0'           flgOk             1
080100010601     C                   z-add     *zeros        cntNonEl         10 0
080200010601     C                   z-add     *zeros        depfil            3 0
080300010601     C                   z-add     *zeros        invfil            3 0
080400010601     C*
080500010601     C                   ENDSR
080600010601     C***
080700010601
080800010601
080900010601
081000010330
081100010330
081200000613     C     *inzsr        BEGSR
081300990910     C*
081400990910     C     *entry        plist
081500990920     C                   parm                    tivlrds
081600990921     C                   parm      wrkesito      esito
081700000724     C                   parm                    prmlit
081800000710     C                   parm                    prmfir
081900010330     C*
082000010330     C* CALCOLA LA DATA CORRENTE
082100010330     C                   time                    wn14             14 0
082200121005     C                   movel     wn14          oracor            6 0          *ORA
082300010330     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
082400010330     C                   z-add     wn8           g08dat
082500010330     C                   z-add     *zeros        g08inv
082600010330     C                   movel     '0'           g08err
082700010330     C                   call      'XSRDA8'
082800010330     C                   parm                    wlbda8
082900010330     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
083000130208     C*
083100130208     C* Chiave su TABEL00F - parziale
083200130208     C     KEYtabP       KLIST
083300130208     C                   KFLD                    tblKUT
083400130208     C                   KFLD                    tblCOD
083500000613     C*
083600000613     C                   ENDSR
083700000613     C***
