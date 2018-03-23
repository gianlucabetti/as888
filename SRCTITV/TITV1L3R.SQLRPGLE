000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500021113     FFIVABwwr  O    E             DISK    usropn
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000000613     D rrnum           s              6  0 INZ(*zeros)
002100090203     D depspe          s             10    INZ(*blanks)
002200090203     D curspe          s             10    INZ(*blanks)
002300010202     D parccm          s              8    INZ(*blanks)
002400010202     D parmbr          s             10    INZ(*blanks)
002500010202     D paropz          s              1    INZ(*blanks)
002600010202     D chkcall         s              1    INZ(*blanks)
002700080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
002800070530     D Num5_0          s              5  0
002900000830
003000000830     D*------------------
003100000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
003200000830     D*------------------
003300000830     D WLBDA8          DS                  INZ
003400000830     D  G08DAT                 1      8  0
003500000830     D  G08INV                 9     16  0
003600000830     D  G08ERR                17     17
003700000830     D  G08TGI                18     22  0
003800000830     D*
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400020725     D  VINDTA                 1   2048
004500020725     D  VINFLG              2049   2049
004600020725     D  VINSPE              2050   2059
004700020725     D  VINRRN              2060   2067  0
004800020725     D*
004900080923     D*------------------
005000080923     D* DS REPERIMENTO NUMERATORE
005100080923     D*------------------
005200080923     D trul33ds      e ds                  inz
005300080923     D*------------------
005400080923     D* DS ARCHITETTURA
005500080923     D*------------------
005600080923     D kpjba         e ds                  inz
005700080923     D*------------------
005800081113
005900081113     D*------------------
006000081113     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
006100081113     D*------------------
006200081113     D PiStr           S            100A   INZ
006300081113     D PiDecChr        S              1A   INZ
006400081113     D PiVal           S             63S30 INZ
006500081113     D PiInt           S               N   INZ
006600081113     D PiNum           S               N   INZ
006700081113
006800081113     D*------------------
006900081113     D* LINKING A DEFINIZIONI ESTERNE
007000081113     D*------------------
007100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007200081113
007300990908
007400010201
007500010201
007600080117     C*
007700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007800080117     C
007900080117     C/EXEC SQL
008000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008100080117     C/END-EXEC
008200080117     C
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000613     C*
008700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008800000613     C*
008900010202     C* Effettuo la chiamata al CLLE preposto
009000050128     C                   call(e)   'TITVVTC'
009100010202     C                   parm                    parccm
009200010202     C                   parm                    parmbr
009300010202     C                   parm      '2'           paropz
009400070730     C*
009500070730     C* Effettuo lancio TISI95 solo x chiusura
009600070730     C                   CLEAR                   TISI95DS
009700070730     C                   EVAL      I95TLA = 'C'
009800070730     C                   CALL      'TISI95R'
009900070730     C                   PARM                    TISI95DS
010000000616     C*
010100000801     C
010200010201     C                   seton                                        LR
010300990908
010400000801
010500910830     C*--------------------------------------------------------
010600070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010700910830     C*--------------------------------------------------------
010800070530     C     RWFILE        BEGSR
010900990910     C*
011000990914     C                   if        not %open(tivin00r)
011100990908     C                   open      tivin00r
011200990914     C                   endif
011300021113     C                   if        not %open(fivabwwr)
011400021113     C                   open      fivabwwr
011500990914     C                   endif
011600070530     C*
011700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011800020305     C                   exsr      prevat
011900010201     C*
012000010202     C                   if        chkcall = '0'
012100010202     C*
012200021113     C                   if        not %open(fivatwwr)
012300021113     C                   open      fivatwwr
012400010201     C                   endif
012500080117     C*
012600080117     C                   EXSR      INZVAR
012700080117     C                   EXSR      DEFCAM
012800990910     C*
012900010201     C                   clear                   §CTROKVB          5 0
013000020305     C                   clear                   §CTROKVT          5 0
013100000801     C                   clear                   §CTRMO            5 0
013200000801     C                   clear                   §CTRNO            5 0
013300990910     C*
013400020725     C/EXEC SQL
013500020725     C+ declare C1 cursor for select
013600090203     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
013700060223     C+ from tivin00r
013800060223     C+ order by sped
013900020725     C+ for read only
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300020725     C+ open C1
014400020725     C/END-EXEC
014500020725     C
014600020725     C/EXEC SQL
014700070530     C+ Fetch C1 into :INPUT_DS
014800020725     C/END-EXEC
014900020725     C*
015000020725     C                   dow       sqlcod = *zeros
015100990913     C*
015200020725     C                   if        vindta > *blanks
015300000613     C                   add       1             rrnum
015400000801     C*
015500020725     C                   if        vinflg = *blanks
015600020725     C                             or vinflg = '0'
015700020725     C                             or vinflg = '2'
015800000801     C*
015900020725     C                   clear                   x_vinmsg
016000020725     C                   eval      x_vinflg = '1'
016100010305     C*
016200010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016300090203     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
016400020305     C                   MOVEL(p)  PiStr         curspe
016500010305     C*
016600071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016700080923     C                   if        *in50 = *off
016800071003     C                   exsr      impvab
016900071003     C                   exsr      wrivab
017000071003     C                   exsr      wrivat_b                                     => carico VAT
017100071003     C                   else
017200080923     C*
017300071009     C                   if        wDISK = 'D'
017400071009     C                   exsr      impvab
017500071009     C                   exsr      wrivab
017600071009     C                   exsr      wrivat_b                                     => carico VAT
017700071009     C                   exsr      wrivat_e                                     => carico VAT
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300020305     C                   exsr      impvab
018400071003     C                   exsr      wrivat_b                                     => carico VAT
018500071003     C   50              exsr      wrivat_e                                     => carico VAT
018600010305     C                   else
018700020725     C                   if        depspe <> curspe                             => rottura di spediz
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900070928     C                   exsr      wrivab
019000080117     C                   clear                   tivinds
019100020305     C                   exsr      impvab
019200071003     C                   exsr      wrivat_b                                     => carico VAT
019300071003     C   50              exsr      wrivat_e                                     => carico VAT
019400020305     C                   else                                                   => x stessa spediz
019500090204     C***                exsr      impvab
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   endif
019900010305     C                   endif
020000010305     C                   endif
020100071003     C                   endif
020200071009     C                   endif
020300000905     C*
020400000905     C                   else
020500020725     C                   eval      x_vinflg = '1'
020600000905     C                   endif
020700000905     C*
020800020725     C     VINRRN        chain     tivin000
020900020725     C                   if        %found(tivin00r)
021000020725     C                   eval      y_vinflg = x_vinflg
021100020725     C                   eval      y_vinmsg = x_vinmsg
021200020725     C                   update    tivin000
021300020725     C                   endif
021400020725     C*
021500020725     C/EXEC SQL
021600070530     C+ Fetch C1 into :INPUT_DS
021700020725     C/END-EXEC
021800020725     C*
021900020725     C                   enddo
022000020725     C*
022100020725     C/EXEC SQL
022200020725     C+ close C1
022300020725     C/END-EXEC
022400000905     C*
022500020305     C* Scarico i VAB rimasti "in sospeso"
022600071009     C                   if        wDISK = 'C'
022700071009     C                   exsr      wrivab
022800071009     C                   endif
022900010202     C*
023000010202     C                   endif
023100990910
023200990910     C* Se non ci sono record con errori ...
023300000710     C                   if        §ctrno = 0
023400990910     C* ... restituisco esito OK.
023500990921     C                   eval      wrkesito = '0'
023600990910     C                   else
023700010201     C                   if        §ctrokvb > 0
023800990921     C                   eval      wrkesito = '1'
023900000710     C                   else
024000000710     C                   eval      wrkesito = '2'
024100990910     C                   endif
024200000710     C                   endif
024300990910     C*
024400990914     C                   if        %open(tivin00r)
024500990908     C                   close     tivin00r
024600990914     C                   endif
024700021113     C                   if        %open(fivabwwr)
024800021113     C                   close     fivabwwr
024900990914     C                   endif
025000021113     C                   if        %open(fivatwwr)
025100021113     C                   close     fivatwwr
025200010201     C                   endif
025300990910     C*
025400010201     C                   if        §ctrokvb > 0
025500000724     C                             and vlrpoi <> *zeros
025600010202     C                   exsr      invio
025700990920     C                   endif
025800990920     C*
025900910830     C                   ENDSR
026000000613     C***
026100010305
026200010305     C*----------------------------------------------------*
026300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026400010305     C*----------------------------------------------------*
026500020305     C     WRIVAB        BEGSR
026600070730     C*
026700071003     C* Considerazioni finali
026800071003     C                   if        VABRMA = *blanks
026900071003     C                   movel(P)  VABRMN        VABRMA
027000071003     C                   endif
027100071003     C*
027200021113     C                   write     fivab000                                     => scarico il VAB
027300010305     C*
027400010305     C                   ENDSR
027500990920
027600000801     C*----------------------------------------------------*
027700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027800000801     C*----------------------------------------------------*
027900010201     C     INZVAR        BEGSR
028000000801     C*
028100010201     C                   Z-ADD     *zeros        Num5_0
028200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028300020725     C                   MOVEL     '0'           FlgCAS            1
028400000801     C*
028500000801     C                   ENDSR
028600000801     C*----------------------------------------------------*
028700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028800000801     C*----------------------------------------------------*
028900020904     C     DEFCAM        BEGSR
029000080923     C*
029100080923     C                   SETOFF                                       5051
029200000801     C*
029300021113     C                   CLEAR                   FIVAB000
029400021113     C                   CLEAR                   FIVAT000
029500070730     C* Imposto i valori di default...
029600090202     C                   EVAL      VABCCM = 0230745
029700090202     C                   EVAL      VATCCM = 0230745
029800090202     C                   EVAL      VABLNP = 023
029900090202     C                   EVAL      VATLNP = 023
030000070928     C                   EVAL      VABCTR = 000
030100070730     C                   EVAL      VABCBO = '1'
030200070222     C* ... e poi verifico se sono stati passati come parametri
030300070222     C                   IF        vlrppt > *blanks
030400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030600070222     C                   EXSR      CHKNUM
030700070222     C                   IF        PiInt=*on
030800070222     C                   Z-ADD     PiVal         VABCCM
030900070222     C                   Z-ADD     PiVal         VATCCM
031000070222     C                   ENDIF
031100070222     C                   ENDIF
031200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031400070222     C                   EXSR      CHKNUM
031500070222     C                   IF        PiInt=*on
031600070222     C                   Z-ADD     PiVal         VABLNP
031700070222     C                   Z-ADD     PiVal         VATLNP
031800070222     C                   ENDIF
031900070222     C                   ENDIF
032000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032200070222     C                   EXSR      CHKNUM
032300070222     C                   IF        PiInt=*on
032400070222     C                   Z-ADD     PiVal         VABCTR
032500070222     C                   ENDIF
032600070928     C                   ENDIF
032700071009     C                   MOVEL     *blanks       wDISK             1
032800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032900071009     C                   IF        wDISK <> *blanks
033000070928     C                   SETON                                        50
033100070222     C                   ENDIF
033200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033300080923     C                   SETON                                        51
033400080923     C                   ENDIF
033500070222     C                   ENDIF
033600071009     C*
033700071009     C   50              EVAL      VABCTM = '7Q'
033800000801     C*
033900000801     C                   ENDSR
034000000801     C*----------------------------------------------------*
034100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034200000801     C*----------------------------------------------------*
034300010201     C     IMPVAB        BEGSR
034400070530     C*
034500070530     C                   SETOFF                                       3132
034600070928     C*
034700070928     C                   EXSR      INZVAR
034800070928     C                   EXSR      DEFCAM
034900070928     C*
035000070928     C                   MOVE(P)   vlrpoi        VABFGS
035100070928     C                   MOVE(P)   vlrpoi        VATFGS
035200070928     C*
035300070928     C                   MOVEL     datcor        VABAAS
035400070928     C                   MOVEL     datcor        VATAAS
035500070928     C                   MOVE      datcor        VABMGS
035600070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
035700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035900070928     C* ==
036000070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
036100070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
036200070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
036300081218     C***                EVAL      VABRMA=%trim(%subst(vindta:315:16))
036400090203     C***                EVAL      VABRMA=%trim(%subst(vindta:187:11))
036500090203     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
036600070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
036700080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
036800070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
036900070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
037000070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
037100090211     C                   IF        %trim(%subst(vindta:394:1)) = 'F'
037200080226     C                   EVAL      VABCBO='1'
037300080226     C                   ENDIF
037400090211     C                   IF        %trim(%subst(vindta:394:1)) = 'A'
037500080226     C                   EVAL      VABCBO='2'
037600080226     C                   ENDIF
037700080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
037800080226     C                   EVAL      VABTSP='E'
037900080226     C                   ENDIF
038000080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
038100080226     C                   EVAL      VABFFD='S'
038200080226     C                   ENDIF
038300081113     C                   IF        %trim(%subst(vindta:16:1)) = '2'
038400080328     C                   EVAL      VABTC1='A'
038500080226     C                   ENDIF
038600071003     C* CAD
038700070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
038800070928     C                   EXSR      CHKNUM
038900070928     C                   IF        PiInt=*on
039000070928     C                   Z-ADD     PiVal         Num5_0
039100070928     C                   MOVEL     Num5_0        VABCAD
039200070928     C                   ELSE
039300070928     C                   SETON                                        32
039400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039500070928     C                             + ' ' + 'VABCAD'
039600070928     C                   ENDIF
039700080923     C* RMN
039800090202     C                   EVAL      PiStr=%trim(%subst(vindta:189:7))
039900080923     C                   EXSR      CHKNUM
040000080923     C                   IF        PiInt=*on
040100080923     C                   Z-ADD     PiVal         VABRMN
040200080923     C                   ELSE
040300080923     C                   SETON                                        32
040400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040500080923     C                             + ' ' + 'VABRMN'
040600080923     C                   ENDIF
040700080923     C* NSP
040800080923     C                   IF        *in51 = *on
040900080923     C* NSP => Stacco un numeratore da AZNUM
041000080923     C                   clear                   TRUL33DS
041100080923     C                   eval      I33OPE = *zeros
041200080923     C                   eval      I33CNU = 302
041300080923     C                   eval      I33NUM = 1
041400080923     C                   movel     TRUL33DS      KPJBU
041500080923     C                   call      'TRUL33R'
041600080923     C                   parm                    KPJBA
041700080923     C                   movel     KPJBU         TRUL33DS
041800080923     C                   if        O33ERR = *zeros
041900080923     C                   z-add     O33NRF        VABNSP
042000080923     C                   z-add     O33NRF        VATNSP
042100080923     C                   else
042200080923     C                   SETON                                        31
042300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400080923     C                             + ' ' + 'VABNSP VATNSP'
042500080923     C                   endif
042600080923     C                   ELSE
042700080923     C                   EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
042800080923     C                   EXSR      CHKNUM
042900080923     C                   IF        PiInt=*on
043000080923     C                   Z-ADD     PiVal         VABNSP
043100080923     C                   Z-ADD     PiVal         VATNSP
043200080923     C                   ELSE
043300080923     C                   SETON                                        31
043400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500080923     C                             + ' ' + 'VABNSP VATNSP'
043600080923     C                   ENDIF
043700080923     C                   ENDIF
043800071003     C* NCL
043900070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
044000070928     C                   EXSR      CHKNUM
044100070928     C                   IF        PiInt=*on
044200070928     C                   Z-ADD     PiVal         VABNCL
044300070928     C                   ELSE
044400070928     C                   SETON                                        32
044500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044600070928     C                             + ' ' + 'VABNCL'
044700070928     C                   ENDIF
044800071003     C* PKB
044900070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
045000070928     C                   EXSR      CHKNUM
045100070928     C                   IF        PiNum=*on
045200070928     C                   Z-ADD     PiVal         VABPKB
045300070928     C                   ELSE
045400070928     C                   SETON                                        32
045500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600070928     C                             + ' ' + 'VABPKB'
045700070928     C                   ENDIF
045800080923     C* CAS
045900080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
046000080923     C                             AND
046100080923     C                             %trim(%subst(vindta:280:12))<>*zeros
046200080923     C                             AND
046300080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
046400080923     C                             AND
046500080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
046600080923     C                             AND
046700080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
046800070928     C                   MOVEL     '1'           FlgCAS
046900070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
047000070928     C                   EXSR      CHKNUM
047100070928     C                   IF        PiNum=*on
047200070928     C                   Z-ADD     PiVal         VABCAS
047300070928     C                   ELSE
047400070928     C                   SETON                                        32
047500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600070928     C                             + ' ' + 'VABCAS'
047700070928     C                   ENDIF
047800070928     C                   ENDIF
047900071003     C* IAS
048000080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
048100080923     C                             AND
048200080923     C                             %trim(%subst(vindta:295:12))<>*zeros
048300080923     C                             AND
048400080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
048500080923     C                             AND
048600080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
048700080923     C                             AND
048800080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
048900080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
049000080923     C                   EXSR      CHKNUM
049100080923     C                   IF        PiNum=*on
049200080923     C                   Z-ADD     PiVal         VABIAS
049300080923     C                   ELSE
049400080923     C                   SETON                                        32
049500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600080923     C                             + ' ' + 'VABIAS'
049700080923     C                   ENDIF
049800071003     C                   ENDIF
049900070730     C*
050000070730     C* Considerazioni finali su CBO/CAS
050100070730     C                   IF        FlgCAS = '1'
050200070730     C                   IF        VABCBO = '1'
050300070730     C                   EVAL      VABCBO = '4'
050400070730     C                   ENDIF
050500070730     C                   IF        VABCBO = '2'
050600070730     C                   EVAL      VABCBO = '6'
050700070730     C                   ENDIF
050800070730     C                   ENDIF
050900020305     C*
051000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051100011113     C                   EXSR      CHKIMPDIV
051200010202     C*
051300000801     C* Ebbene...
051400000801     C                   ADD       1             §CTRMO
051500070530     C                   IF        *in31 <> *zeros OR
051600070530     C                             *in32 <> *zeros
051700000801     C                   ADD       1             §CTRNO
051800020725     C                   EVAL      x_vinflg = '2'
051900000801     C                   ELSE
052000010201     C                   ADD       1             §CTROKVB
052100000801     C                   ENDIF
052200000801     C*
052300000801     C                   ENDSR
052400010201     C*----------------------------------------------------*
052500071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052600010201     C*----------------------------------------------------*
052700071003     C     WRIVAT_B      BEGSR
052800090204     C*
052900090204     C                   eval      vatAAS = vabAAS
053000090204     C                   eval      vatLNP = vabLNP
053100090204     C                   eval      vatNRS = vabNRS
053200090204     C                   eval      vatNSP = vabNSP
053300010201     C*
053400021113     C* Valorizzo l buffer di scrittura del FIVAT
053500070928     C* - TIPO RECORD "B"
053600070928     C                   if        %subst(vindta:92:15) <> *blanks
053700070928     C                   eval      VATTRC = 'B'
053800070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
053900021113     C                   write     FIVAT000
054000020725     C                   endif
054100010201     C*
054200010201     C                   ENDSR
054300071003     C*----------------------------------------------------*
054400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054500071003     C*----------------------------------------------------*
054600071003     C     WRIVAT_E      BEGSR
054700090204     C*
054800090204     C                   eval      vatAAS = vabAAS
054900090204     C                   eval      vatLNP = vabLNP
055000090204     C                   eval      vatNRS = vabNRS
055100090204     C                   eval      vatNSP = vabNSP
055200071003     C*
055300071003     C* Valorizzo l buffer di scrittura del FIVAT
055400071003     C* - TIPO RECORD "E"
055500071003     C                   if        %subst(vindta:331:26) <> *blanks
055600071003     C                   eval      VATTRC = 'E'
055700071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
055800071003     C                   write     FIVAT000
055900071003     C                   endif
056000071003     C*
056100071003     C                   ENDSR
056200020904
056300020904
056400010202     C*----------------------------------------------------*
056500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056600010202     C*----------------------------------------------------*
056700020305     C     PREVAT        BEGSR
056800010202     C*
056900021113     C* Compongo il nome del membro da dare al FIVATWWR
057000010202     C                   eval      parmbr = vlrhdl
057100010202     C                   movel     'M'           parmbr
057200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057300010202     C                   eval      paropz = '1'
057400010202     C* Effettuo la chiamata al CLLE preposto
057500050128     C                   call(e)   'TITVVTC'
057600010202     C                   parm                    parccm
057700010202     C                   parm                    parmbr
057800010202     C                   parm                    paropz
057900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058000010202     C                   if        %error
058100010202     C                   movel     '1'           chkcall
058200010202     C                   else
058300010202     C                   movel     '0'           chkcall
058400010202     C                   endif
058500010202     C*
058600010202     C                   ENDSR
058700000801     C*----------------------------------------------------*
058800000801     C*  CONTROLLO NUMERICITA' CAMPI
058900000801     C*----------------------------------------------------*
059000000801     C     CHKNUM        BEGSR
059100081113     C*
059200081113     C                   IF        PiDecChr = *blanks
059300081218     C                   EVAL      PiDecChr = ','
059400081113     C                   ENDIF
059500081113     C*
059600081113     C                   callp(e)  UBISNUM_Check(PiStr
059700081113     C                                          :PiDecChr
059800081113     C                                          :PiVal
059900081113     C                                          :PiNum
060000081113     C                                          :PiInt)
060100081113     C*
060200000801     C                   IF        %error
060300000801     C                   EVAL      PiInt=*off
060400000801     C                   ENDIF
060500000801     C*
060600000801     C                   ENDSR
060700000801     C***
060800000801
060900011113
061000011113     C*----------------------------------------------------*
061100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061200011113     C*----------------------------------------------------*
061300011113     C     CHKIMPDIV     BEGSR
061400011113     C*
061500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061600011113     C                   Z-ADD     *zeros        wrkDec            9 9
061700011113     C*
061800011113     C* Come prima cosa effettuo considerazioni sulla divisa
061900011113     C                   IF        vabIAS > *zeros
062000011113     C                   IF        vabVAS <> 'EUR'
062100011113     C                   EVAL      vabVAS =  'ITL'
062200011113     C                   ENDIF
062300011113     C                   ENDIF
062400011113     C*
062500011113     C                   IF        vabCAS > *zeros
062600011113     C                   IF        vabVCA <> 'EUR'
062700011113     C                   EVAL      vabVCA =  'ITL'
062800011113     C                   ENDIF
062900011113     C                   ENDIF
063000011113     C*
063100011113     C                   IF        vabVMD > *zeros
063200020305     C                   IF        vabVAD <> 'EUR'
063300011113     C                   EVAL      vabVAD =  'ITL'
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
063800011113     C                   Z-ADD     vabIAS        wrkDec
063900011113     C                   IF        wrkDec > *zeros
064000011113     C                   IF        vabVAS = 'ITL'
064100011113     C                   EVAL      vabIAS = *zeros
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064600011113     C                   Z-ADD     vabCAS        wrkDec
064700011113     C                   IF        wrkDec > *zeros
064800011113     C                   IF        vabVCA = 'ITL'
064900011113     C                   EVAL      vabCAS = *zeros
065000011113     C                   ENDIF
065100011113     C                   ENDIF
065200011113     C*
065300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065400011113     C                   Z-ADD     vabVMD        wrkDec
065500011113     C                   IF        wrkDec > *zeros
065600011113     C                   IF        vabVAD = 'ITL'
065700011113     C                   EVAL      vabVMD = *zeros
065800011113     C                   ENDIF
065900011113     C                   ENDIF
066000011113     C*
066100011113     C                   ENDSR
066200011113     C***
066300011113
066400011113
066500000801
066600000801
066700990920      /TITLE Invio dei dati al punto operativo.
066800010202     C     invio         BEGSR
066900990920     C*
067000021113     C* 1° invio FIVAT
067100010201     C                   reset                   dscmz
067200010201     C                   move      vlrpoi        cmzdst
067300021113     C                   eval      cmzfld = 'FIVATWWR'
067400010201     C                   eval      cmzmbd = vlrhdl
067500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067600021009     C***                if        prmfir = *blanks
067700021113     C                   eval      cmzfla = 'FIVAT00F'
067800021113     C                   eval      cmzmba = 'FIVAT00F'
067900021009     C***                else
068000021009     C***                eval      cmzfla = prmfir
068100021009     C***                eval      cmzmba = prmfir
068200021009     C***                endif
068300010201     C                   eval      cmznrr = *zeros
068400020305     C                   move      §ctrokvt      cmznrr
068500021018     C                   eval      cmzlba = vlrfl1
068600010201     C                   call(e)   'TIS711C'
068700010201     C                   parm                    dscmz
068800010201     C                   parm      *blanks       esito
068900010205     C                   if        %error
069000010205     C                             or cmzerr = '1'
069100010205     C                             or esito  = '1'
069200010205     C                   eval      wrkesito = '3'
069300010205     C                   else
069400010201     C*
069500021113     C* 2° invio FIVAB
069600010201     C                   reset                   dscmz
069700010201     C                   move      vlrpoi        cmzdst
069800010201     C                   eval      cmzfld = vlrfou
069900010201     C                   eval      cmzmbd = vlrhdl
070000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070100021009     C***                if        prmfir = *blanks
070200021113     C                   eval      cmzfla = 'FIVAB00F'
070300021113     C                   eval      cmzmba = 'FIVAB00F'
070400021009     C***                else
070500021009     C***                eval      cmzfla = prmfir
070600021009     C***                eval      cmzmba = prmfir
070700021009     C***                endif
070800010201     C                   eval      cmznrr = *zeros
070900010201     C                   move      §ctrokvb      cmznrr
071000021018     C                   eval      cmzlba = vlrfl1
071100010201     C                   call(e)   'TIS711C'
071200010201     C                   parm                    dscmz
071300010201     C                   parm      *blanks       esito
071400010201     C                   if        %error
071500010201     C                             or cmzerr = '1'
071600010201     C                             or esito  = '1'
071700010201     C                   eval      wrkesito = '3'
071800010201     C                   endif
071900010205     C                   endif
072000990920     C*
072100000613     C                   ENDSR
072200000613     C***
072300070411
072400070411     C     *pssr         BEGSR
072500070411     C*
072600070411     C                   if        %open(tivin00r)
072700070411     C                   close     tivin00r
072800070411     C                   endif
072900070411     C                   if        %open(fivabwwr)
073000070411     C                   close     fivabwwr
073100070411     C                   endif
073200070411     C                   if        %open(fivatwwr)
073300070411     C                   close     fivatwwr
073400070411     C                   endif
073500070411     C*
073600070411     C* Effettuo la chiamata al CLLE preposto
073700070411     C                   call(e)   'TITVVTC'
073800070411     C                   parm                    parccm
073900070411     C                   parm                    parmbr
074000070411     C                   parm      '2'           paropz
074100070411     C*
074200070411     C                   eval      wrkesito = '2'
074300070411     C*
074400070411     C                   seton                                        LR
074500070411     C*
074600070411     C                   ENDSR     '*CANCL'
074700070411     C***
074800070411
074900990910
075000000613     C     *inzsr        BEGSR
075100990910     C*
075200990910     C     *entry        plist
075300990920     C                   parm                    tivlrds
075400990921     C                   parm      wrkesito      esito
075500000724     C                   parm                    prmlit
075600000710     C                   parm                    prmfir
075700000613     C*
075800000830     C* CALCOLA LA DATA CORRENTE
075900000830     C                   time                    wn14             14 0
076000000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
076100000830     C                   z-add     wn8           g08dat
076200000830     C                   z-add     *zeros        g08inv
076300000830     C                   movel     '0'           g08err
076400000830     C                   call      'XSRDA8'
076500000830     C                   parm                    wlbda8
076600000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
076700000830     C*
076800000613     C                   ENDSR
076900000613     C***
