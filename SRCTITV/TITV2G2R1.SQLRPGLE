000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130129     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400170523     Ftabel00f  if   e           k disk
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800990908
000900000801     D*----------------------------------------------------
001000000801     D* DICHIARAZIOINE VARIABILI DI WRK
001100000801     D*----------------------------------------------------
001200990920     D dscmz         e ds                  inz
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600070730     D tisi95ds      e ds
001700170523     D ds15          e ds
001800990910     D esito           s              1
001900000724     D prmlit          s             10
002000000710     D prmfir          s             10
002100990921     D wrkesito        s                   like(esito)
002200000613     D rrnum           s              6  0 INZ(*zeros)
002300130506     D depspe          s             16    INZ(*blanks)
002400130506     D curspe          s             16    INZ(*blanks)
002500010202     D parccm          s              8    INZ(*blanks)
002600010202     D parmbr          s             10    INZ(*blanks)
002700010202     D paropz          s              1    INZ(*blanks)
002800010202     D chkcall         s              1    INZ(*blanks)
002900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003000070530     D Num5_0          s              5  0
003100140203     D wNomeFile       s             30    INZ(*blanks)
003200170523
003300170523     D*------------
003400170523     D jNAZ            s              5  0 INZ(*zeros)
003500170523     D skNAZISO        S              3    DIM(1000)
003600170523     D skNAZBAR        S              3    DIM(1000)
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500130506     D  VINSPE                       16
004600101007     D  VINRRN                        8  0
004700020725     D*
004800080923     D*------------------
004900080923     D* DS REPERIMENTO NUMERATORE
005000080923     D*------------------
005100080923     D trul33ds      e ds                  inz
005200080923     D*------------------
005300080923     D* DS ARCHITETTURA
005400080923     D*------------------
005500080923     D kpjba         e ds                  inz
005600080923     D*------------------
005700081113
005800081113     D*------------------
005900081113     D* LINKING A DEFINIZIONI ESTERNE
006000081113     D*------------------
006100100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006200081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006300081113
006400990908
006500010201
006600010201
006700080117     C*
006800080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006900080117     C
007000080117     C/EXEC SQL
007100080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007200080117     C/END-EXEC
007300080117     C
007400000913     C                   reset                   rrnum
007500990921     C                   reset                   esito
007600990921     C                   reset                   wrkesito
007700000613     C*
007800070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007900000613     C*
008000010202     C* Effettuo la chiamata al CLLE preposto
008100050128     C                   call(e)   'TITVVTC'
008200010202     C                   parm                    parccm
008300010202     C                   parm                    parmbr
008400010202     C                   parm      '2'           paropz
008500070730     C*
008600070730     C* Effettuo lancio TISI95 solo x chiusura
008700070730     C                   CLEAR                   TISI95DS
008800070730     C                   EVAL      I95TLA = 'C'
008900070730     C                   CALL      'TISI95R'
009000070730     C                   PARM                    TISI95DS
009100000616     C*
009200000801     C
009300010201     C                   seton                                        LR
009400000801
009500910830     C*--------------------------------------------------------
009600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009700910830     C*--------------------------------------------------------
009800070530     C     RWFILE        BEGSR
009900990910     C*
010000990914     C                   if        not %open(tivin00r)
010100990908     C                   open      tivin00r
010200990914     C                   endif
010300021113     C                   if        not %open(fivabwwr)
010400021113     C                   open      fivabwwr
010500990914     C                   endif
010600070530     C*
010700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010800020305     C                   exsr      prevat
010900010201     C*
011000010202     C                   if        chkcall = '0'
011100010202     C*
011200021113     C                   if        not %open(fivatwwr)
011300021113     C                   open      fivatwwr
011400010201     C                   endif
011500080117     C*
011600080117     C                   EXSR      INZVAR
011700080117     C                   EXSR      DEFCAM
011800990910     C*
011900010201     C                   clear                   §CTROKVB          5 0
012000020305     C                   clear                   §CTROKVT          5 0
012100000801     C                   clear                   §CTRMO            5 0
012200000801     C                   clear                   §CTRNO            5 0
012300990910     C*
012400020725     C/EXEC SQL
012500020725     C+ declare C1 cursor for select
012600130506     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
012700060223     C+ from tivin00r
012800060223     C+ order by sped
012900020725     C+ for read only
013000020725     C/END-EXEC
013100020725     C
013200020725     C/EXEC SQL
013300020725     C+ open C1
013400020725     C/END-EXEC
013500020725     C
013600020725     C/EXEC SQL
013700070530     C+ Fetch C1 into :INPUT_DS
013800020725     C/END-EXEC
013900020725     C*
014000020725     C                   dow       sqlcod = *zeros
014100990913     C*
014200020725     C                   if        vindta > *blanks
014300000613     C                   add       1             rrnum
014400000801     C*
014500020725     C                   if        vinflg = *blanks
014600020725     C                             or vinflg = '0'
014700020725     C                             or vinflg = '2'
014800000801     C*
014900020725     C                   clear                   x_vinmsg
015000020725     C                   eval      x_vinflg = '1'
015100010305     C*
015200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015300130506     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015400130506     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
015500020305     C                   MOVEL(p)  PiStr         curspe
015600010305     C*
015700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015800080923     C                   if        *in50 = *off
015900071003     C                   exsr      impvab
016000071003     C                   exsr      wrivab
016100071003     C                   exsr      wrivat_b                                     => carico VAT
016200071003     C                   else
016300080923     C*
016400071009     C                   if        wDISK = 'D'
016500071009     C                   exsr      impvab
016600071009     C                   exsr      wrivab
016700071009     C                   exsr      wrivat_b                                     => carico VAT
016800071009     C                   exsr      wrivat_e                                     => carico VAT
016900071009     C                   else
017000071009     C*
017100171011     C                   if        depspe = *blanks OR                          => 1° giro
017200171011     C                             %subst(vindta:412:3) <> *blanks
017300010305     C                   eval      depspe = curspe                              => memorizz. spediz
017400080117     C                   clear                   tivinds
017500020305     C                   exsr      impvab
017600171011     C   55              exsr      wrivab
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800071003     C   50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200070928     C                   exsr      wrivab
018300080117     C                   clear                   tivinds
018400020305     C                   exsr      impvab
018500071003     C                   exsr      wrivat_b                                     => carico VAT
018600071003     C   50              exsr      wrivat_e                                     => carico VAT
018700020305     C                   else                                                   => x stessa spediz
018800090204     C***                exsr      impvab
018900071003     C                   exsr      wrivat_b                                     => carico VAT
019000071003     C   50              exsr      wrivat_e                                     => carico VAT
019100010305     C                   endif
019200010305     C                   endif
019300010305     C                   endif
019400071003     C                   endif
019500071009     C                   endif
019600000905     C*
019700000905     C                   else
019800020725     C                   eval      x_vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100020725     C     VINRRN        chain     tivin000
020200020725     C                   if        %found(tivin00r)
020300020725     C                   eval      y_vinflg = x_vinflg
020400020725     C                   eval      y_vinmsg = x_vinmsg
020500020725     C                   update    tivin000
020600020725     C                   endif
020700020725     C*
020800020725     C/EXEC SQL
020900070530     C+ Fetch C1 into :INPUT_DS
021000020725     C/END-EXEC
021100020725     C*
021200020725     C                   enddo
021300020725     C*
021400020725     C/EXEC SQL
021500020725     C+ close C1
021600020725     C/END-EXEC
021700000905     C*
021800020305     C* Scarico i VAB rimasti "in sospeso"
021900071009     C                   if        wDISK = 'C'
022000171011     C  N55              exsr      wrivab
022100071009     C                   endif
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000021113     C                   if        %open(fivabwwr)
024100021113     C                   close     fivabwwr
024200990914     C                   endif
024300021113     C                   if        %open(fivatwwr)
024400021113     C                   close     fivatwwr
024500010201     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800000724     C                             and vlrpoi <> *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400010305
025500010305     C*----------------------------------------------------*
025600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025700010305     C*----------------------------------------------------*
025800020305     C     WRIVAB        BEGSR
025900070730     C*
026000071003     C* Considerazioni finali
026100071003     C                   if        VABRMA = *blanks
026200071003     C                   movel(P)  VABRMN        VABRMA
026300071003     C                   endif
026400071003     C*
026500021113     C                   write     fivab000                                     => scarico il VAB
026600010305     C*
026700010305     C                   ENDSR
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400010201     C                   Z-ADD     *zeros        Num5_0
027500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027600020725     C                   MOVEL     '0'           FlgCAS            1
027700000801     C*
027800000801     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028100000801     C*----------------------------------------------------*
028200020904     C     DEFCAM        BEGSR
028300080923     C*
028400080923     C                   SETOFF                                       5051
028500000801     C*
028600021113     C                   CLEAR                   FIVAB000
028700021113     C                   CLEAR                   FIVAT000
028800070730     C* Imposto i valori di default...
028900130506     C                   EVAL      VABCCM = 2101878
029000130506     C                   EVAL      VATCCM = 2101878
029100130506     C                   EVAL      VABLNP = 210
029200130506     C                   EVAL      VATLNP = 210
029300130518     C                   EVAL      VABCTR = 301
029400070730     C                   EVAL      VABCBO = '1'
029500070222     C* ... e poi verifico se sono stati passati come parametri
029600070222     C                   IF        vlrppt > *blanks
029700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029900070222     C                   EXSR      CHKNUM
030000070222     C                   IF        PiInt=*on
030100070222     C                   Z-ADD     PiVal         VABCCM
030200070222     C                   Z-ADD     PiVal         VATCCM
030300070222     C                   ENDIF
030400070222     C                   ENDIF
030500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030700070222     C                   EXSR      CHKNUM
030800070222     C                   IF        PiInt=*on
030900070222     C                   Z-ADD     PiVal         VABLNP
031000070222     C                   Z-ADD     PiVal         VATLNP
031100070222     C                   ENDIF
031200070222     C                   ENDIF
031300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABCTR
031800070222     C                   ENDIF
031900070928     C                   ENDIF
032000071009     C                   MOVEL     *blanks       wDISK             1
032100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032200071009     C                   IF        wDISK <> *blanks
032300070928     C                   SETON                                        50
032400070222     C                   ENDIF
032500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032600080923     C                   SETON                                        51
032700080923     C                   ENDIF
032800070222     C                   ENDIF
032900071009     C*
033000071009     C   50              EVAL      VABCTM = '7Q'
033100000801     C*
033200000801     C                   ENDSR
033300000801     C*----------------------------------------------------*
033400021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033500000801     C*----------------------------------------------------*
033600010201     C     IMPVAB        BEGSR
033700070530     C*
033800070530     C                   SETOFF                                       3132
033900070928     C*
034000070928     C                   EXSR      INZVAR
034100070928     C                   EXSR      DEFCAM
034200070928     C*
034300070928     C                   MOVE(P)   vlrpoi        VABFGS
034400070928     C                   MOVE(P)   vlrpoi        VATFGS
034500070928     C*
034600070928     C                   MOVEL     datcor        VABAAS
034700070928     C                   MOVEL     datcor        VATAAS
034800070928     C                   MOVE      datcor        VABMGS
034900070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
035000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035200070928     C* ==
035300070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
035400070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
035500070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
035600110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
035700110622     C***                EVAL      VABRMA=%trim(%subst(vindta:331:10))
035800070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
035900080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
036000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
036100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
036200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
036300130506     C                   IF        %trim(%subst(vindta:394:1)) = 'F'
036400130506     C                   EVAL      VABCBO='1'
036500130506     C                   ENDIF
036600130506     C                   IF        %trim(%subst(vindta:394:1)) = 'A'
036700130506     C                   EVAL      VABCBO='2'
036800130506     C                   ENDIF
036900130607     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
037000130607     C***                EVAL      VABTSP='E'
037100130607     C***                ENDIF
037200130607     C                   EVAL      VABTSP=%subst(vindta:12:1)
037300130506     C                   IF        %trim(%subst(vindta:16:1)) = '1'
037400130506     C                   EVAL      VABFFD='S'
037500130506     C                   ENDIF
037600130506     C                   IF        %trim(%subst(vindta:16:1)) = '2'
037700130506     C                   EVAL      VABTC1='A'
037800130506     C                   ENDIF
037900170523     C* NZD
038000170523     C                   EVAL      VABNZD=%trim(%subst(vindta:412:3))
038100171011     C                   SETOFF                                       55
038200170523     C                   IF        VABNZD <> *blanks
038300171011     C                   SETON                                        55
038400170523     C                   Z-ADD     1             jNAZ
038500170523     C     VABNZD        LOOKUP    skNAZISO(jNAZ)                         13
038600170523     C                   IF        %found
038700170523     C                   EVAL      VABNZD = skNAZBAR(jNAZ)
038800170523     C                   ENDIF
038900170523     C                   ENDIF
039000130607     C* CCM
039100130607     C                   EVAL      PiStr=%trim(%subst(vindta:1:11))
039200130607     C                   EXSR      CHKNUM
039300130607     C                   IF        PiInt=*on
039400130607     C                   Z-ADD     PiVal         VABCCM
039500130607     C                   Z-ADD     PiVal         VATCCM
039600130607     C                   ELSE
039700130607     C                   SETON                                        32
039800130607     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039900130607     C                             + ' ' + 'VABCCM VATCCM'
040000130607     C                   ENDIF
040100171011     C   55              IF        %trim(%subst(vindta:1:11)) = '2102965'
040200171011     C                   EVAL      VABCTM = '7R'
040300171011     C                   ENDIF
040400071003     C* CAD
040500170530     C                   EVAL      VABCAD=%trim(%subst(vindta:107:5))
040600170530     C***                EVAL      PiStr=%trim(%subst(vindta:107:5))
040700170530     C***                EXSR      CHKNUM
040800170530     C***                IF        PiInt=*on
040900170530     C***                Z-ADD     PiVal         Num5_0
041000170530     C***                MOVEL     Num5_0        VABCAD
041100170530     C***                ELSE
041200170530     C***                SETON                                        32
041300170530     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
041400170530     C***                          + ' ' + 'VABCAD'
041500170530     C***                ENDIF
041600080923     C* RMN
041700110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
041800130506     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
041900130506     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
042000101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
042100080923     C                   EXSR      CHKNUM
042200080923     C                   IF        PiInt=*on
042300080923     C                   Z-ADD     PiVal         VABRMN
042400080923     C                   ELSE
042500080923     C                   SETON                                        32
042600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700080923     C                             + ' ' + 'VABRMN'
042800080923     C                   ENDIF
042900080923     C* NSP
043000171016     C                   IF        *in51 = *on OR *in55 = *on
043100080923     C* NSP => Stacco un numeratore da AZNUM
043200080923     C                   clear                   TRUL33DS
043300080923     C                   eval      I33OPE = *zeros
043400080923     C                   eval      I33CNU = 302
043500080923     C                   eval      I33NUM = 1
043600080923     C                   movel     TRUL33DS      KPJBU
043700080923     C                   call      'TRUL33R'
043800080923     C                   parm                    KPJBA
043900080923     C                   movel     KPJBU         TRUL33DS
044000080923     C                   if        O33ERR = *zeros
044100080923     C                   z-add     O33NRF        VABNSP
044200080923     C                   z-add     O33NRF        VATNSP
044300080923     C                   else
044400080923     C                   SETON                                        31
044500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600080923     C                             + ' ' + 'VABNSP VATNSP'
044700080923     C                   endif
044800080923     C                   ELSE
044900130506     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
045000080923     C                   EXSR      CHKNUM
045100080923     C                   IF        PiInt=*on
045200080923     C                   Z-ADD     PiVal         VABNSP
045300080923     C                   Z-ADD     PiVal         VATNSP
045400080923     C                   ELSE
045500080923     C                   SETON                                        31
045600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045700080923     C                             + ' ' + 'VABNSP VATNSP'
045800080923     C                   ENDIF
045900080923     C                   ENDIF
046000071003     C* NCL
046100070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
046200171011     C   55              Z-ADD     1             VABNCL
046300171011     C  N55              EXSR      CHKNUM
046400171011     C  N55              IF        PiInt=*on
046500070928     C                   Z-ADD     PiVal         VABNCL
046600070928     C                   ELSE
046700070928     C                   SETON                                        32
046800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900070928     C                             + ' ' + 'VABNCL'
047000070928     C                   ENDIF
047100071003     C* PKB
047200110520     C                   IF        %subst(vindta:170:8) <> *blanks
047300070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
047400070928     C                   EXSR      CHKNUM
047500070928     C                   IF        PiNum=*on
047600070928     C                   Z-ADD     PiVal         VABPKB
047700070928     C                   ELSE
047800070928     C                   SETON                                        32
047900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000070928     C                             + ' ' + 'VABPKB'
048100070928     C                   ENDIF
048200110520     C                   ELSE
048300110520     C                   ENDIF
048400080923     C* CAS
048500080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
048600080923     C                             AND
048700080923     C                             %trim(%subst(vindta:280:12))<>*zeros
048800080923     C                             AND
048900080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
049000080923     C                             AND
049100080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
049200080923     C                             AND
049300080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
049400070928     C                   MOVEL     '1'           FlgCAS
049500070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
049600070928     C                   EXSR      CHKNUM
049700070928     C                   IF        PiNum=*on
049800070928     C                   Z-ADD     PiVal         VABCAS
049900070928     C                   ELSE
050000070928     C                   SETON                                        32
050100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200070928     C                             + ' ' + 'VABCAS'
050300070928     C                   ENDIF
050400070928     C                   ENDIF
050500071003     C* IAS
050600080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
050700080923     C                             AND
050800080923     C                             %trim(%subst(vindta:295:12))<>*zeros
050900080923     C                             AND
051000080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
051100080923     C                             AND
051200080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
051300080923     C                             AND
051400080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
051500080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
051600080923     C                   EXSR      CHKNUM
051700080923     C                   IF        PiNum=*on
051800080923     C                   Z-ADD     PiVal         VABIAS
051900080923     C                   ELSE
052000080923     C                   SETON                                        32
052100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052200080923     C                             + ' ' + 'VABIAS'
052300080923     C                   ENDIF
052400071003     C                   ENDIF
052500070730     C*
052600070730     C* Considerazioni finali su CBO/CAS
052700070730     C                   IF        FlgCAS = '1'
052800070730     C                   IF        VABCBO = '1'
052900070730     C                   EVAL      VABCBO = '4'
053000070730     C                   ENDIF
053100070730     C                   IF        VABCBO = '2'
053200070730     C                   EVAL      VABCBO = '6'
053300070730     C                   ENDIF
053400070730     C                   ENDIF
053500020305     C*
053600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053700011113     C                   EXSR      CHKIMPDIV
053800010202     C*
053900000801     C* Ebbene...
054000000801     C                   ADD       1             §CTRMO
054100070530     C                   IF        *in31 <> *zeros OR
054200070530     C                             *in32 <> *zeros
054300000801     C                   ADD       1             §CTRNO
054400020725     C                   EVAL      x_vinflg = '2'
054500000801     C                   ELSE
054600010201     C                   ADD       1             §CTROKVB
054700000801     C                   ENDIF
054800000801     C*
054900000801     C                   ENDSR
055000010201     C*----------------------------------------------------*
055100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
055200010201     C*----------------------------------------------------*
055300071003     C     WRIVAT_B      BEGSR
055400090204     C*
055500090204     C                   eval      vatAAS = vabAAS
055600090204     C                   eval      vatLNP = vabLNP
055700090204     C                   eval      vatNRS = vabNRS
055800090204     C                   eval      vatNSP = vabNSP
055900010201     C*
056000101007     C* Valorizzo il buffer di scrittura del FIVAT
056100070928     C* - TIPO RECORD "B"
056200070928     C                   if        %subst(vindta:92:15) <> *blanks
056300070928     C                   eval      VATTRC = 'B'
056400070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
056500021113     C                   write     FIVAT000
056600020725     C                   endif
056700010201     C*
056800010201     C                   ENDSR
056900071003     C*----------------------------------------------------*
057000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057100071003     C*----------------------------------------------------*
057200071003     C     WRIVAT_E      BEGSR
057300090204     C*
057400090204     C                   eval      vatAAS = vabAAS
057500090204     C                   eval      vatLNP = vabLNP
057600090204     C                   eval      vatNRS = vabNRS
057700090204     C                   eval      vatNSP = vabNSP
057800071003     C*
057900101007     C* Valorizzo il buffer di scrittura del FIVAT
058000071003     C* - TIPO RECORD "E"
058100071003     C                   if        %subst(vindta:331:26) <> *blanks
058200071003     C                   eval      VATTRC = 'E'
058300071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
058400071003     C                   write     FIVAT000
058500071003     C                   endif
058600071003     C*
058700071003     C                   ENDSR
058800020904
058900020904
059000010202     C*----------------------------------------------------*
059100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
059200010202     C*----------------------------------------------------*
059300020305     C     PREVAT        BEGSR
059400010202     C*
059500021113     C* Compongo il nome del membro da dare al FIVATWWR
059600010202     C                   eval      parmbr = vlrhdl
059700010202     C                   movel     'M'           parmbr
059800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
059900010202     C                   eval      paropz = '1'
060000010202     C* Effettuo la chiamata al CLLE preposto
060100050128     C                   call(e)   'TITVVTC'
060200010202     C                   parm                    parccm
060300010202     C                   parm                    parmbr
060400010202     C                   parm                    paropz
060500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
060600010202     C                   if        %error
060700010202     C                   movel     '1'           chkcall
060800010202     C                   else
060900010202     C                   movel     '0'           chkcall
061000010202     C                   endif
061100010202     C*
061200010202     C                   ENDSR
061300000801     C*----------------------------------------------------*
061400000801     C*  CONTROLLO NUMERICITA' CAMPI
061500000801     C*----------------------------------------------------*
061600000801     C     CHKNUM        BEGSR
061700081113     C*
061800081113     C                   IF        PiDecChr = *blanks
061900110524     C                   EVAL      PiDecChr = '.'
062000081113     C                   ENDIF
062100081113     C*
062200081113     C                   callp(e)  UBISNUM_Check(PiStr
062300081113     C                                          :PiDecChr
062400081113     C                                          :PiVal
062500081113     C                                          :PiNum
062600081113     C                                          :PiInt)
062700081113     C*
062800000801     C                   IF        %error
062900000801     C                   EVAL      PiInt=*off
063000000801     C                   ENDIF
063100000801     C*
063200000801     C                   ENDSR
063300000801     C***
063400000801
063500011113
063600011113     C*----------------------------------------------------*
063700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
063800011113     C*----------------------------------------------------*
063900011113     C     CHKIMPDIV     BEGSR
064000011113     C*
064100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
064200011113     C                   Z-ADD     *zeros        wrkDec            9 9
064300011113     C*
064400011113     C* Come prima cosa effettuo considerazioni sulla divisa
064500011113     C                   IF        vabIAS > *zeros
064600011113     C                   IF        vabVAS <> 'EUR'
064700011113     C                   EVAL      vabVAS =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C                   IF        vabCAS > *zeros
065200011113     C                   IF        vabVCA <> 'EUR'
065300011113     C                   EVAL      vabVCA =  'ITL'
065400011113     C                   ENDIF
065500011113     C                   ENDIF
065600011113     C*
065700011113     C                   IF        vabVMD > *zeros
065800020305     C                   IF        vabVAD <> 'EUR'
065900011113     C                   EVAL      vabVAD =  'ITL'
066000011113     C                   ENDIF
066100011113     C                   ENDIF
066200011113     C*
066300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
066400011113     C                   Z-ADD     vabIAS        wrkDec
066500011113     C                   IF        wrkDec > *zeros
066600011113     C                   IF        vabVAS = 'ITL'
066700011113     C                   EVAL      vabIAS = *zeros
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
067200011113     C                   Z-ADD     vabCAS        wrkDec
067300011113     C                   IF        wrkDec > *zeros
067400011113     C                   IF        vabVCA = 'ITL'
067500011113     C                   EVAL      vabCAS = *zeros
067600011113     C                   ENDIF
067700011113     C                   ENDIF
067800011113     C*
067900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068000011113     C                   Z-ADD     vabVMD        wrkDec
068100011113     C                   IF        wrkDec > *zeros
068200011113     C                   IF        vabVAD = 'ITL'
068300011113     C                   EVAL      vabVMD = *zeros
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C                   ENDSR
068800011113     C***
068900011113
069000011113
069100000801
069200000801
069300990920      /TITLE Invio dei dati al punto operativo.
069400010202     C     invio         BEGSR
069500990920     C*
069600021113     C* 1° invio FIVAT
069700010201     C                   reset                   dscmz
069800010201     C                   move      vlrpoi        cmzdst
069900021113     C                   eval      cmzfld = 'FIVATWWR'
070000010201     C                   eval      cmzmbd = vlrhdl
070100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070200021009     C***                if        prmfir = *blanks
070300021113     C                   eval      cmzfla = 'FIVAT00F'
070400021113     C                   eval      cmzmba = 'FIVAT00F'
070500021009     C***                else
070600021009     C***                eval      cmzfla = prmfir
070700021009     C***                eval      cmzmba = prmfir
070800021009     C***                endif
070900010201     C                   eval      cmznrr = *zeros
071000020305     C                   move      §ctrokvt      cmznrr
071100021018     C                   eval      cmzlba = vlrfl1
071200010201     C                   call(e)   'TIS711C'
071300010201     C                   parm                    dscmz
071400010201     C                   parm      *blanks       esito
071500010205     C                   if        %error
071600010205     C                             or cmzerr = '1'
071700010205     C                             or esito  = '1'
071800010205     C                   eval      wrkesito = '3'
071900010205     C                   else
072000010201     C*
072100021113     C* 2° invio FIVAB
072200010201     C                   reset                   dscmz
072300010201     C                   move      vlrpoi        cmzdst
072400010201     C                   eval      cmzfld = vlrfou
072500010201     C                   eval      cmzmbd = vlrhdl
072600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072700021009     C***                if        prmfir = *blanks
072800021113     C                   eval      cmzfla = 'FIVAB00F'
072900021113     C                   eval      cmzmba = 'FIVAB00F'
073000021009     C***                else
073100021009     C***                eval      cmzfla = prmfir
073200021009     C***                eval      cmzmba = prmfir
073300021009     C***                endif
073400010201     C                   eval      cmznrr = *zeros
073500010201     C                   move      §ctrokvb      cmznrr
073600021018     C                   eval      cmzlba = vlrfl1
073700010201     C                   call(e)   'TIS711C'
073800010201     C                   parm                    dscmz
073900010201     C                   parm      *blanks       esito
074000010201     C                   if        %error
074100010201     C                             or cmzerr = '1'
074200010201     C                             or esito  = '1'
074300010201     C                   eval      wrkesito = '3'
074400010201     C                   endif
074500010205     C                   endif
074600990920     C*
074700000613     C                   ENDSR
074800000613     C***
074900070411
075000070411     C     *pssr         BEGSR
075100070411     C*
075200070411     C                   if        %open(tivin00r)
075300070411     C                   close     tivin00r
075400070411     C                   endif
075500070411     C                   if        %open(fivabwwr)
075600070411     C                   close     fivabwwr
075700070411     C                   endif
075800070411     C                   if        %open(fivatwwr)
075900070411     C                   close     fivatwwr
076000070411     C                   endif
076100070411     C*
076200070411     C* Effettuo la chiamata al CLLE preposto
076300070411     C                   call(e)   'TITVVTC'
076400070411     C                   parm                    parccm
076500070411     C                   parm                    parmbr
076600070411     C                   parm      '2'           paropz
076700070411     C*
076800070411     C                   eval      wrkesito = '2'
076900070411     C*
077000070411     C                   seton                                        LR
077100070411     C*
077200070411     C                   ENDSR     '*CANCL'
077300070411     C***
077400170523
077500170523
077600170523
077700170523     C*--------------------------------------------------------
077800170523     C* CARTAB - CARICAMENTO DATI TABELLARI                   *
077900170523     C*--------------------------------------------------------
078000170523     C     CARTAB        BEGSR
078100170523     C*
078200170523     C* TABELLA '15' - NAZIONI
078300170523     C                   clear                   skNAZISO
078400170523     C                   clear                   skNAZBAR
078500170523     C                   eval      tblKUT = 1
078600170523     C                   eval      tblCOD = '15'
078700170523     C     KEYtabP       setll     tabel00f
078800170523     C     KEYtabP       reade     tabel00f
078900170523     C                   dow       not %eof(tabel00f)
079000170523     C                   if        tblFLG = *blanks
079100170523     C                   movel(p)  tblUNI        ds15
079200170523     C                   add       1             jNAZ
079300170523     C                   eval      skNAZBAR(jNAZ) = tblKEY
079400170523     C                   eval      skNAZISO(jNAZ) = §15ISO
079500170523     C                   endif
079600170523     C     KEYtabP       reade     tabel00f
079700170523     C                   enddo
079800170523     C*
079900170523     C                   ENDSR
080000170523     C***
080100170523
080200070411
080300990910
080400000613     C     *inzsr        BEGSR
080500990910     C*
080600990910     C     *entry        plist
080700990920     C                   parm                    tivlrds
080800990921     C                   parm      wrkesito      esito
080900000724     C                   parm                    prmlit
081000000710     C                   parm                    prmfir
081100000613     C*
081200000830     C* CALCOLA LA DATA CORRENTE
081300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081400100324     C                   eval      datcor = %dec(%date() : *ISO)
081500170523     C*
081600170523     C* Chiave su TABEL00F - parziale
081700170523     C     KEYtabP       KLIST
081800170523     C                   KFLD                    tblKUT
081900170523     C                   KFLD                    tblCOD
082000140203     C*
082100140203     C* REPERISCO SUBITO IL NOME DEL FILE "UPLOADATO" CHE PILOTA LA FILIALE A CUI INVIARE I DATI
082200140203     C                   MOVE(P)   vlrMSG        wNomeFile
082300140203     C                   SELECT
082400140203     C                   WHEN      %subst(wNomeFile:8:6) = 'BISEDE' OR
082500140203     C                             %subst(wNomeFile:8:6) = 'DEP006'
082600140203     C                   EVAL      vlrpoi = 210
082700140203     C                   WHEN      %subst(wNomeFile:8:6) = 'DEP007'
082800140203     C                   EVAL      vlrpoi = 036
082900170313     C***                WHEN      %subst(wNomeFile:8:6) = 'DEP010'
083000170313     C***                EVAL      vlrpoi = 056
083100170313     C                   WHEN      %subst(wNomeFile:8:6) = 'DEP010'
083200170313     C                   EVAL      vlrpoi = 210
083300140203     C                   ENDSL
083400000830     C*
083500000613     C                   ENDSR
083600000613     C***
