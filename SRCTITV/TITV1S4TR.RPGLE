000100091022      /TITLE Upload via Internet: traduzione in EDIVATWR
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500091012     FTITV1S41L uF A E           k DISK    rename(TITV1S400:TITV1S401)
000600091012     F                                     prefix(VAB_)
000700091012     FTITV1S42L uF A E           k DISK    rename(TITV1S400:TITV1S402)
000800091012     F                                     prefix(VAT_)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000091022     D edivat_IN     e ds                  extname(edivatwr)
002100091022     D edivat_O_VAT  e ds                  extname(edivatwr) prefix(VAT_)
002200091022     D edivat_O_VAB  e ds                  extname(edivatwr) prefix(VAB_)
002300091012     D wTV1S4KEY       s                   like(VAT_TV1S4KEY)
002400091012     D wTV1S4FLGD      s                   like(VAT_TV1S4FLGD)
002500091015     D wVABPKB         s                   like(VAT_VABPKB)
002600010201
002700091006
002800091006     D*------------------
002900091006     D* LINKING A DEFINIZIONI ESTERNE
003000091006     D*------------------
003100091006     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
003200091006     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
003300091006
003400010201
003500990921     C                   reset                   esito
003600990921     C                   reset                   wrkesito
003700000613     C*
003800091012     C                   EXSR      RWFILE                                       LETT/SCR. VAT
003900000616     C*
004000010201     C                   seton                                        LR
004100990908
004200000801
004300910830     C*--------------------------------------------------------
004400091022     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVATWR
004500910830     C*--------------------------------------------------------
004600040526     C     RWFILE        BEGSR
004700990910     C*
004800990914     C                   if        not %open(tivin00r)
004900990908     C                   open      tivin00r
005000990914     C                   endif
005100091012     C*
005200091012     C                   clear                   §CTROKVT          5 0
005300091012     C                   clear                   §CTRMO            5 0
005400091012     C                   clear                   §CTRNO            5 0
005500990910     C*
005600921023     C                   DO        *HIVAL
005700990913     C*
005800990915     C                   READ      tivin00r                               70
005900050627     C                   if        vindta > *blanks
006000000801     C*
006100000801     C                   if        *in70 = *off
006200000801     C                             and
006300000801     C                             (vinflg = *blanks
006400000801     C                              or vinflg = '0'
006500000801     C                              or vinflg = '2')
006600000801     C*
006700000801     C                   clear                   vinmsg
006800000801     C                   eval      vinflg = '1'
006900010305     C*
007000091022     C                   clear                   edivat_in
007100091015     C                   clear                   wVABPKB
007200091012     C                   exsr      impvat                                       => carico VAT
007300091012     C                   exsr      wrivat                                       => scarico VAT
007400000905     C*
007500000905     C                   else
007600000905     C                   eval      vinflg = '1'
007700050628     C                   endif
007800000905     C                   endif
007900000905     C*
008000000905     C  N70              update    tivin000
008100000905     C*
008200991022     C  N70              ENDdo
008300990910
008400091012
008500990910     C* Se non ci sono record con errori ...
008600000710     C                   if        §ctrno = 0
008700990910     C* ... restituisco esito OK.
008800990921     C                   eval      wrkesito = '0'
008900990910     C                   else
009000091012     C                   if        §ctrokvt > 0
009100990921     C                   eval      wrkesito = '1'
009200000710     C                   else
009300000710     C                   eval      wrkesito = '2'
009400990910     C                   endif
009500000710     C                   endif
009600990910     C*
009700990914     C                   if        %open(tivin00r)
009800990908     C                   close     tivin00r
009900990914     C                   endif
010000990920     C*
010100910830     C                   ENDSR
010200000613     C***
010300010305
010400010305     C*----------------------------------------------------*
010500091012     C*  SCARICAMENTO BUFFER RECORDS VAT
010600010305     C*----------------------------------------------------*
010700091012     C     WRIVAT        BEGSR
010800091015     C*
010900091015     C                   SETOFF                                       55
011000091015     C                   IF        wVABPKB <> *zeros
011100091015     C                   SETON                                        55
011200091015     C                   ENDIF
011300091012     C*
011400100312     C                   EVAL      wTV1S4KEY  = '0'+%editc(wCCM1:'X')+'_BOLLE'
011500091012     C                   EVAL      wTV1S4FLGD = %trim(%subst(vindta:94:1))
011600091012     C*
011700091022     C* Verifico se già esistono i record EDIVAB correlati
011800091014     C     KEY1s4_1_P    CHAIN     TITV1S41L
011900091012     C*
012000091012     C                   IF        %found(TITV1S41L)
012100091012     C                   IF        VAB_TV1S4VAT = *blanks
012200091012     C                   DELETE    TITV1S401
012300091022     C                   EVAL      edivat_o_vab = edivat_in
012400091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
012500091012     C                   EVAL      VAB_TV1S4FLGD = wTV1S4FLGD
012600091012     C                   EVAL      VAB_TV1S4VAT  = 'X'
012700091015     C   55              EVAL      VAB_VABPKB    = wVABPKB
012800091012     C                   WRITE     TITV1S401
012900091013     C                   ELSE
013000091013     C                   UNLOCK    TITV1S41L
013100091012     C                   ENDIF
013200091012     C                   ENDIF
013300091012     C*
013400091012     C     KEY1s4_2      CHAIN     TITV1S42L
013500091012     C*
013600091012     C                   IF        %found(TITV1S42L)
013700091022     C                   EVAL      edivat_o_vat = edivat_in
013800091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
013900091012     C                   EVAL      VAT_TV1S4FLGD = wTV1S4FLGD
014000091012     C                   EVAL      VAT_TV1S4VAT  = 'X'
014100091015     C   55              EVAL      VAT_VABPKB    = wVABPKB
014200091012     C                   UPDATE    TITV1S402
014300091012     C*
014400091012     C                   ELSE
014500091012     C*
014600091022     C                   EVAL      edivat_o_vat = edivat_in
014700091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
014800091012     C                   EVAL      VAT_TV1S4FLGD = wTV1S4FLGD
014900091012     C                   EVAL      VAT_TV1S4VAT  = 'X'
015000091015     C   55              EVAL      VAT_VABPKB    = wVABPKB
015100091012     C                   WRITE     TITV1S402
015200091012     C                   ENDIF
015300010305     C*
015400010305     C                   ENDSR
015500990920
015600000801     C*----------------------------------------------------*
015700091012     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
015800000801     C*----------------------------------------------------*
015900010201     C     INZVAR        BEGSR
016000000801     C*
016100040802     C                   Z-ADD     *zeros        Num5_0            5 0
016200040802     C                   MOVEL     '0'           FlgCAS            1
016300000801     C*
016400000801     C                   ENDSR
016500000801     C*----------------------------------------------------*
016600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
016700000801     C*----------------------------------------------------*
016800000801     C     DEFCAM        BEGSR
016900000801     C*
017000020619     C* Imposto i valori di default...
017100091012     C                   EVAL      VATTRC = 'E'
017200020619     C* ... e poi verifico se sono stati passati come parametri
017300100312     C                   IF        vlrppt > *blanks
017400100312     C                   IF        %subst(vlrppt:1:7) <> *blanks
017500100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
017600100312     C                   EXSR      CHKNUM
017700100312     C                   IF        PiInt=*on
017800100312     C                   Z-ADD     PiVal         wCCM1             7 0
017900100312     C                   ENDIF
018000100312     C                   ENDIF
018100100312     C                   IF        %subst(vlrppt:8:7) <> *blanks
018200100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:7))
018300100312     C                   EXSR      CHKNUM
018400100312     C                   IF        PiInt=*on
018500100312     C                   Z-ADD     PiVal         wCCM2             7 0
018600100312     C                   ENDIF
018700100312     C                   ENDIF
018800100312     C                   IF        %subst(vlrppt:15:7) <> *blanks
018900100312     C                   EVAL      PiStr=%trim(%subst(vlrppt:15:7))
019000100312     C                   EXSR      CHKNUM
019100100312     C                   IF        PiInt=*on
019200100312     C                   Z-ADD     PiVal         wCCM3             7 0
019300100312     C                   ENDIF
019400100312     C                   ENDIF
019500100312     C                   IF        %subst(vlrppt:22:3) <> *blanks
019600100312     C                   ENDIF
019700100312     C                   IF        %subst(vlrppt:25:3) <> *blanks
019800100312     C                   ENDIF
019900100312     C                   IF        %subst(vlrppt:28:2) = 'VB'
020000100312     C                   SETON                                        50
020100100312     C                   ELSE
020200100312     C                   SETOFF                                       50
020300100312     C                   ENDIF
020400100312     C                   IF        %subst(vlrppt:30:1) = 'L'
020500100312     C                   SETON                                        51
020600100312     C                   ELSE
020700100312     C                   SETOFF                                       51
020800100312     C                   ENDIF
020900100312     C                   IF        %subst(vlrppt:31:8) <> *blanks
021000100312     C                   MOVEL     *blanks       wCMR              8
021100100312     C                   EVAL      wCMR=%trim(%subst(vlrppt:31:8))
021200100312     C                   ENDIF
021300100312     C                   IF        %subst(vlrppt:39:8) <> *blanks
021400100312     C                   MOVEL     *blanks       wEMAIL            8
021500100312     C                   EVAL      wEMAIL=%trim(%subst(vlrppt:39:8))
021600100312     C                   ENDIF
021700100312     C                   ENDIF
021800000801     C*
021900000801     C                   ENDSR
022000000801     C*----------------------------------------------------*
022100091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
022200000801     C*----------------------------------------------------*
022300091012     C     IMPVAT        BEGSR
022400040823     C*
022500020305     C                   EXSR      INZVAR
022600020305     C                   EXSR      DEFCAM
022700010305     C*
022800000801     C                   Z-ADD     *zeros        errore            1 0
022900091012     C                   MOVE(P)   vlrpoi        VATFGS
023000091012     C*
023100091012     C                   EVAL      VATNOT=%trim(%subst(vindta:74:20))
023200091012     C* LNP
023300091012     C                   EVAL      PiStr=%trim(%subst(vindta:10:4))
023400091012     C                   EXSR      CHKNUM
023500091012     C                   IF        PiInt=*on
023600091012     C                   Z-ADD     PiVal         VATLNP
023700091012     C                   ELSE
023800091012     C                   ADD       1             errore
023900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
024000091012     C                             + ' ' + 'VATLNP'
024100091012     C                   ENDIF
024200091012     C* AAS
024300091012     C                   EVAL      PiStr=%trim(%subst(vindta:14:5))
024400091012     C                   EXSR      CHKNUM
024500091012     C                   IF        PiInt=*on
024600091012     C                   Z-ADD     PiVal         VATAAS
024700091012     C                   ELSE
024800091012     C                   ADD       1             errore
024900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
025000091012     C                             + ' ' + 'VATAAS'
025100091012     C                   ENDIF
025200091012     C* NRS
025300091012     C                   EVAL      PiStr=%trim(%subst(vindta:19:3))
025400091012     C                   EXSR      CHKNUM
025500091012     C                   IF        PiInt=*on
025600091012     C                   Z-ADD     PiVal         VATNRS
025700091012     C                   ELSE
025800091012     C                   ADD       1             errore
025900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
026000091012     C                             + ' ' + 'VATNRS'
026100091012     C                   ENDIF
026200091012     C* NSP
026300091012     C                   EVAL      PiStr=%trim(%subst(vindta:22:8))
026400091012     C                   EXSR      CHKNUM
026500091012     C                   IF        PiInt=*on
026600091012     C                   Z-ADD     PiVal         VATNSP
026700091012     C                   ELSE
026800091012     C                   ADD       1             errore
026900091012     C                   EVAL      vinmsg = %trimr(vinmsg)
027000091012     C                             + ' ' + 'VATNSP'
027100091012     C                   ENDIF
027200091015     C* PKB
027300091015     C                   IF        %subst(vindta:134:6) <> *all'0'
027400091015     C                   EVAL      PiStr=%trim(%subst(vindta:134:6))
027500091015     C                   EXSR      CHKNUM
027600091015     C                   IF        PiNum=*on
027700091015     C                   EVAL      PiVal = PiVal / 10                           * gestisco 1 dec.
027800091015     C                   Z-ADD(H)  PiVal         wVABPKB
027900091015     C                   ELSE
028000091015     C                   ADD       1             errore
028100091015     C                   EVAL      vinmsg = %trimr(vinmsg)
028200091015     C                             + ' ' + 'VABPKB'
028300091015     C                   ENDIF
028400091015     C                   ENDIF
028500010202     C*
028600000801     C* Ebbene...
028700091012     C                   ADD       1             §CTRMO
028800010201     C                   IF        errore <> *zeros
028900091012     C                   ADD       1             §CTRNO
029000000801     C                   EVAL      vinflg = '2'
029100091012     C                   ELSE
029200091012     C                   ADD       1             §CTROKVT
029300091012     C                   ENDIF
029400000801     C*
029500000801     C                   ENDSR
029600000801     C*----------------------------------------------------*
029700000801     C*  CONTROLLO NUMERICITA' CAMPI
029800000801     C*----------------------------------------------------*
029900091006     C     CHKNUM        BEGSR
030000091006     C*
030100091006     C                   IF        PiDecChr = *blanks
030200091006     C                   EVAL      PiDecChr = ','
030300091006     C                   ENDIF
030400091006     C*
030500091006     C                   callp(e)  UBISNUM_Check(PiStr
030600091006     C                                          :PiDecChr
030700091006     C                                          :PiVal
030800091006     C                                          :PiNum
030900091006     C                                          :PiInt)
031000091006     C*
031100091006     C                   IF        %error
031200091006     C                   EVAL      PiInt=*off
031300091006     C                   ENDIF
031400000801     C*
031500000801     C                   ENDSR
031600000801     C***
031700070411
031800091012
031900091012
032000070411     C     *pssr         BEGSR
032100070411     C*
032200070411     C                   if        %open(tivin00r)
032300070411     C                   close     tivin00r
032400070411     C                   endif
032500070411     C*
032600070411     C                   eval      wrkesito = '2'
032700070411     C*
032800070411     C                   seton                                        LR
032900070411     C*
033000070411     C                   ENDSR     '*CANCL'
033100070411     C***
033200070411
033300990910
033400000613     C     *inzsr        BEGSR
033500990910     C*
033600990910     C     *entry        plist
033700990920     C                   parm                    tivlrds
033800990921     C                   parm      wrkesito      esito
033900000724     C                   parm                    prmlit
034000000710     C                   parm                    prmfir
034100091012     C*
034200091012     C* Definizione chiavi
034300091014     C* TITV1S41L - Parziale
034400091014     C     KEY1s4_1_P    klist
034500091012     C                   kfld                    wTV1S4KEY
034600091012     C                   kfld                    VATAAS
034700091012     C                   kfld                    VATLNP
034800091012     C                   kfld                    VATNRS
034900091012     C                   kfld                    VATNSP
035000091012     C* TITV1S42L - Completa
035100091012     C     KEY1s4_2      klist
035200091012     C                   kfld                    wTV1S4KEY
035300091012     C                   kfld                    VATAAS
035400091012     C                   kfld                    VATLNP
035500091012     C                   kfld                    VATNRS
035600091012     C                   kfld                    VATNSP
035700091012     C                   kfld                    VATTRC
035800091012     C                   kfld                    VATNOT
035900000830     C*
036000000613     C                   ENDSR
036100000613     C***
