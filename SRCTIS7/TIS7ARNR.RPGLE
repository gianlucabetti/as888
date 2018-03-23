000100100301      /TITLE Upload via Internet: traduzione in EDIVAWWR.
000200171018     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500100301     FEDIVABwr  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800000512     D* COMANDI
000900000512     D*------------
001000011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001100000801     D*----------------------------------------------------
001200010215     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200990915     D wrkdata         s               d
002300990915     D wrkora          s               t
002400000613     D rrnum           s              6  0 INZ(*zeros)
002500011113     D depcmd          s            150    INZ(*blanks)
002600000801
002700010213
002800000801     D  Num5_0         s              5  0
002900990908
003000010201
003100010201
003200990915     C                   time                    wrkdata
003300990915     C                   time                    wrkora
003400000913     C                   reset                   rrnum
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700000613     C*
003800010215     C                   EXSR      RWVAB                                        LETT/SCR. VAB
003900000616     C*
004000000801     C
004100010201     C                   seton                                        LR
004200000613
004300000801
004400910830     C*--------------------------------------------------------
004500100301     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWR              *
004600910830     C*--------------------------------------------------------
004700010215     C     RWVAB         BEGSR
004800990910     C*
004900990914     C                   if        not %open(tivin00r)
005000990908     C                   open      tivin00r
005100990914     C                   endif
005200100301     C                   if        not %open(edivabwr)
005300100301     C                   open      edivabwr
005400990914     C                   endif
005500990910     C*
005600010215     C                   clear                   §CTROK            5 0
005700000801     C                   clear                   §CTRMO            5 0
005800000801     C                   clear                   §CTRNO            5 0
005900990910     C*
006000921023     C                   DO        *HIVAL
006100990913     C*
006200990915     C                   READ      tivin00r                               70
006300000905     C                   if        vindta > *blanks
006400000613     C                   add       1             rrnum
006500000801     C*
006600000801     C                   if        *in70 = *off
006700000801     C                             and
006800000801     C                             (vinflg = *blanks
006900000801     C                              or vinflg = '0'
007000000801     C                              or vinflg = '2')
007100000801     C*
007200000801     C                   clear                   vinmsg
007300000801     C                   eval      vinflg = '1'
007400010201     C*
007500000801     C                   exsr      inzvar
007600000801     C                   exsr      defcam
007700010201     C                   exsr      impvab
007800100301     C*
007900100301     C* Imposto i campi x il CMR
008000100301     C                   eval      VABCMR = 'MONDADORI'
008100100301     C                   eval      VABDCM = datcor
008200100301     C                   eval      VABDTS = datcor
008300100301     C                   eval      VABHMS = oracor
008400100301     C                   eval      VABCNT = 1
008500100301     C*
008600100301     C                   write     edivab00
008700010201     C                   endif
008800000905     C*
008900000905     C                   else
009000000905     C                   eval      vinflg = '1'
009100000905     C                   endif
009200000905     C*
009300000905     C  N70              update    tivin000
009400000905     C*
009500991022     C  N70              ENDdo
009600010202     C*
009700990910
009800990910     C* Se non ci sono record con errori ...
009900000710     C                   if        §ctrno = 0
010000990910     C* ... restituisco esito OK.
010100990921     C                   eval      wrkesito = '0'
010200990910     C                   else
010300010215     C                   if        §ctrok   > 0
010400990921     C                   eval      wrkesito = '1'
010500000710     C                   else
010600000710     C                   eval      wrkesito = '2'
010700990910     C                   endif
010800000710     C                   endif
010900990910     C*
011000990914     C                   if        %open(tivin00r)
011100990908     C                   close     tivin00r
011200990914     C                   endif
011300100301     C                   if        %open(edivabwr)
011400100301     C                   close     edivabwr
011500990914     C                   endif
011600990910     C*
011700010215     C                   if        §ctrok > 0
011800000724     C                             and vlrpoi <> *zeros
011900010202     C                   exsr      invio
012000990920     C                   endif
012100990920     C*
012200910830     C                   ENDSR
012300000613     C***
012400990920
012500000801     C*----------------------------------------------------*
012600010322     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
012700000801     C*----------------------------------------------------*
012800010201     C     INZVAR        BEGSR
012900000801     C*
013000010322     C                   Z-ADD     *zeros        Num5_0
013100000801     C*
013200000801     C                   ENDSR
013300000801     C*----------------------------------------------------*
013400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
013500000801     C*----------------------------------------------------*
013600000801     C     DEFCAM        BEGSR
013700000801     C*
013800050103     C* Inizializzo il buffer del record da scrivere
013900100301     C                   CLEAR                   EDIVAB00
014000050103     C* Imposto i valori di default...
014100050103     C                   Z-ADD     0430001       VABCCM
014200010322     C                   Z-ADD     043           VABLNP
014300090930     C                   Z-ADD     301           VABCTR
014400010402     C                   MOVEL     '7Q'          VABCTM
014500050103     C* ... e poi verifico se sono stati passati come parametri
014600050103     C                   IF        vlrppt > *blanks
014700050103     C                   IF        %subst(vlrppt:1:7) <> *blanks
014800050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
014900050103     C                   EXSR      CHKNUM
015000050103     C                   IF        PiInt=*on
015100050103     C                   Z-ADD     PiVal         VABCCM
015200050103     C                   ENDIF
015300050103     C                   ENDIF
015400050103     C                   IF        %subst(vlrppt:8:3) <> *blanks
015500050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
015600050103     C                   EXSR      CHKNUM
015700050103     C                   IF        PiInt=*on
015800050103     C                   Z-ADD     PiVal         VABLNP
015900050103     C                   ENDIF
016000050103     C                   ENDIF
016100050103     C                   IF        %subst(vlrppt:11:3) <> *blanks
016200050103     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
016300050103     C                   EXSR      CHKNUM
016400050103     C                   IF        PiInt=*on
016500050103     C                   Z-ADD     PiVal         VABCTR
016600050103     C                   ENDIF
016700050103     C                   ENDIF
016800050103     C                   ENDIF
016900000801     C*
017000000801     C                   ENDSR
017100000801     C*----------------------------------------------------*
017200100301     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
017300000801     C*----------------------------------------------------*
017400010201     C     IMPVAB        BEGSR
017500000801     C*
017600000801     C                   Z-ADD     *zeros        errore            1 0
017700000830     C                   MOVEL     datcor        VABAAS
017800000830     C                   MOVE      datcor        VABMGS
017900021113     C                   MOVE(P)   vlrpoi        VABFGS
018000010215     C*
018100010215     C* Reperimento campi ALFA
018200010322     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
018300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
018400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
018500020117     C* ==
018600010322     C                   EVAL      VABIND=%trim(%subst(vindta:57:50))
018700010322     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
018800010322     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
018900020104     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
019000020104     C                   EVAL      VABNT2=%trim(%subst(vindta:232:35))
019100010322     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
019200010322     C                   EVAL      VABRMA=%trim(%subst(vindta:189:6))
019300010319     C*
019400010322     C                   EVAL      PiStr=%trim(%subst(vindta:189:6))
019500010319     C                   EXSR      CHKNUM
019600010319     C                   IF        PiInt=*on
019700010319     C                   Z-ADD     PiVal         VABRMN
019800010322     C                   Z-ADD     PiVal         VABNSP
019900010319     C                   ELSE
020000010319     C                   ADD       1             errore
020100010319     C                   EVAL      vinmsg = %trimr(vinmsg)
020200010322     C                             + ' ' + 'VABRMN/VABNSP'
020300010319     C                   ENDIF
020400010319     C*
020500010322     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
020600010319     C                   EXSR      CHKNUM
020700010319     C                   IF        PiInt=*on
020800010319     C                   Z-ADD     PiVal         Num5_0
020900010319     C                   MOVEL(p)  Num5_0        VABCAD
021000010319     C                   ELSE
021100010319     C                   ADD       1             errore
021200010319     C                   EVAL      vinmsg = %trimr(vinmsg)
021300010319     C                             + ' ' + 'VABCAD'
021400010319     C                   ENDIF
021500010319     C*
021600010322     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
021700010201     C                   EXSR      CHKNUM
021800010201     C                   IF        PiInt=*on
021900010322     C                   Z-ADD     PiVal         VABNCL
022000010201     C                   ENDIF
022100010215     C*
022200010322     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
022300010201     C                   EXSR      CHKNUM
022400010201     C                   IF        PiNum=*on
022500010322     C                   EVAL      PiVal = PiVal / 1000
022600010322     C                   Z-ADD(H)  PiVal         VABPKB
022700010201     C                   ENDIF
022800010215     C*
022900020104     C                   EVAL      PiStr=%trim(%subst(vindta:277:12))
023000020104     C                   EXSR      CHKNUM
023100020104     C                   IF        PiNum=*on
023200020104     C                   Z-ADD     PiVal         VABCAS
023300020104     C                   ENDIF
023400171018     C* DCR
023500171018     C                   IF        %subst(vindta:400:8)<>*blank
023600171018     C                   EVAL      PiStr=%trim(%subst(vindta:400:8))
023700171018     C                   EXSR      CHKNUM
023800171018     C                   IF        PiInt=*on
023900171018     C                   Z-ADD     PiVal         VABDCR
024000171018     C* nel caso passassero un dato errato faccio andare avanti la spedizione senza scrivere la DCR
024100171018     C*                  ELSE
024200171018     C*                  ADD       1             errore
024300171018     C*                  EVAL      vinmsg = %trimr(vinmsg)
024400171018     C*                            + ' ' + 'VABDCR'
024500171018     C                   ENDIF
024600171018     C                   ENDIF
024700010205     C*
024800010322     C                   EVAL      VABCBO = '1 '
024900010322     C*
025000010322     C                   IF        VABCAS > *zeros
025100020104     C                   EVAL      VABVCA=%trim(%subst(vindta:309:3))
025200010322     C                   EVAL      VABCBO = '4 '
025300110414     C                   EVAL      VABTIC = 'BM'
025400010322     C                   ENDIF
025500010202     C*
025600000801     C* Ebbene...
025700000801     C                   ADD       1             §CTRMO
025800010201     C                   IF        errore <> *zeros
025900000801     C                   ADD       1             §CTRNO
026000000801     C                   EVAL      vinflg = '2'
026100000801     C                   ELSE
026200010215     C                   ADD       1             §CTROK
026300000801     C                   ENDIF
026400000801     C*
026500000801     C                   ENDSR
026600000801     C*----------------------------------------------------*
026700000801     C*  CONTROLLO NUMERICITA' CAMPI
026800000801     C*----------------------------------------------------*
026900000801     C     CHKNUM        BEGSR
027000000801     C*
027100000801     C                   call(e)   'ISNUMERIC'
027200000801     C                   PARM                    PiStr            30
027300000801     C                   PARM      ','           PiDecChr          1
027400000801     C                   PARM      *ZEROS        PiVal            30 9
027500000801     C                   PARM      '0'           PiInt             1
027600000801     C                   PARM      '0'           PiNum             1
027700000801     C                   IF        %error
027800000801     C                   EVAL      PiInt=*off
027900000801     C                   ENDIF
028000000801     C*
028100000801     C                   ENDSR
028200000801     C***
028300000801
028400000801
028500000801
028600990920      /TITLE Invio dei dati al punto operativo.
028700010202     C     invio         BEGSR
028800010201     C*
028900100301     C* Invio EDIVAB
029000010201     C                   reset                   dscmz
029100010201     C                   move      vlrpoi        cmzdst
029200010201     C                   eval      cmzfld = vlrfou
029300010201     C                   eval      cmzmbd = vlrhdl
029400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
029500010201     C                   if        prmfir = *blanks
029600100301     C                   eval      cmzfla = 'EDIVAB0F'
029700100301     C                   eval      cmzmba = 'EDIVAB0F'
029800010201     C                   else
029900010201     C                   eval      cmzfla = prmfir
030000010201     C                   eval      cmzmba = prmfir
030100010201     C                   endif
030200010201     C                   eval      cmznrr = *zeros
030300010215     C                   move      §ctrok        cmznrr
030400021025     C                   eval      cmzlba = vlrfl1
030500010201     C                   call(e)   'TIS711C'
030600010201     C                   parm                    dscmz
030700010201     C                   parm      *blanks       esito
030800010201     C                   if        %error
030900010201     C                             or cmzerr = '1'
031000010201     C                             or esito  = '1'
031100010201     C                   eval      wrkesito = '3'
031200010205     C                   endif
031300990920     C*
031400000613     C                   ENDSR
031500000613     C***
031600990910
031700000613     C     *inzsr        BEGSR
031800990910     C*
031900990910     C     *entry        plist
032000990920     C                   parm                    tivlrds
032100990921     C                   parm      wrkesito      esito
032200000724     C                   parm                    prmlit
032300000710     C                   parm                    prmfir
032400000613     C*
032500000830     C* CALCOLA LA DATA CORRENTE
032600000830     C                   time                    wn14             14 0
032700100301     C                   movel     wn14          oracor            6 0          *ORA
032800100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
032900100301     C                   eval      datcor = %dec(%date() : *ISO)
033000000830     C*
033100000613     C                   ENDSR
033200000613     C***
