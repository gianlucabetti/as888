000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000600120706     FEDIVATwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600120706     D fivabds       e ds                  extname(EDIVAB0f)
001700120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
001800120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400120706     D depspe          s             10    INZ(*blanks)
002500120706     D curspe          s             10    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200000830
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700070530     D INPUT_DS        DS                  INZ
003800101007     D  VINDTA                     2048
003900101007     D  VINFLG                        1
004000110524     D  VINSPE                        8
004100101007     D  VINRRN                        8  0
004200020725     D*
004300080923     D*------------------
004400080923     D* DS REPERIMENTO NUMERATORE
004500080923     D*------------------
004600080923     D trul33ds      e ds                  inz
004700080923     D*------------------
004800080923     D* DS ARCHITETTURA
004900080923     D*------------------
005000080923     D kpjba         e ds                  inz
005100080923     D*------------------
005200081113
005300081113     D*------------------
005400081113     D* LINKING A DEFINIZIONI ESTERNE
005500081113     D*------------------
005600100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005800081113
005900990908
006000010201
006100010201
006200080117     C*
006300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400080117     C
006500080117     C/EXEC SQL
006600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700080117     C/END-EXEC
006800080117     C
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000613     C*
007300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007400000613     C*
007500010202     C* Effettuo la chiamata al CLLE preposto
007600120706     C                   call(e)   'TITVEVTC'
007700010202     C                   parm                    parccm
007800010202     C                   parm                    parmbr
007900010202     C                   parm      '2'           paropz
008000070730     C*
008100070730     C* Effettuo lancio TISI95 solo x chiusura
008200070730     C                   CLEAR                   TISI95DS
008300070730     C                   EVAL      I95TLA = 'C'
008400070730     C                   CALL      'TISI95R'
008500070730     C                   PARM                    TISI95DS
008600000616     C*
008700000801     C
008800010201     C                   seton                                        LR
008900990908
009000000801
009100910830     C*--------------------------------------------------------
009200120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009300910830     C*--------------------------------------------------------
009400070530     C     RWFILE        BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900120706     C                   if        not %open(edivabwr)
010000120706     C                   open      edivabwr
010100990914     C                   endif
010200070530     C*
010300120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010400020305     C                   exsr      prevat
010500010201     C*
010600010202     C                   if        chkcall = '0'
010700010202     C*
010800120706     C                   if        not %open(edivatwr)
010900120706     C                   open      edivatwr
011000010201     C                   endif
011100080117     C*
011200080117     C                   EXSR      INZVAR
011300080117     C                   EXSR      DEFCAM
011400990910     C*
011500010201     C                   clear                   §CTROKVB          5 0
011600020305     C                   clear                   §CTROKVT          5 0
011700000801     C                   clear                   §CTRMO            5 0
011800000801     C                   clear                   §CTRNO            5 0
011900990910     C*
012000020725     C/EXEC SQL
012100020725     C+ declare C1 cursor for select
012200120706     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
012300060223     C+ from tivin00r
012400060223     C+ order by sped
012500020725     C+ for read only
012600020725     C/END-EXEC
012700020725     C
012800020725     C/EXEC SQL
012900020725     C+ open C1
013000020725     C/END-EXEC
013100020725     C
013200020725     C/EXEC SQL
013300070530     C+ Fetch C1 into :INPUT_DS
013400020725     C/END-EXEC
013500020725     C*
013600020725     C                   dow       sqlcod = *zeros
013700990913     C*
013800020725     C                   if        vindta > *blanks
013900000613     C                   add       1             rrnum
014000000801     C*
014100020725     C                   if        vinflg = *blanks
014200020725     C                             or vinflg = '0'
014300020725     C                             or vinflg = '2'
014400000801     C*
014500020725     C                   clear                   x_vinmsg
014600020725     C                   eval      x_vinflg = '1'
014700010305     C*
014800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014900120706     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
015000020305     C                   MOVEL(p)  PiStr         curspe
015100010305     C*
015200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400071003     C                   exsr      impvab
015500111130     C  N31              eval      fivabds_ok = fivabds
015600111130     C  N31              exsr      wrivab
015700071003     C                   exsr      wrivat_b                                     => carico VAT
015800111125     C                   exsr      inzvar
015900071003     C                   else
016000080923     C*
016100071009     C                   if        wDISK = 'D'
016200071009     C                   exsr      impvab
016300111130     C  N31              eval      fivabds_ok = fivabds
016400111130     C  N31              exsr      wrivab
016500071009     C                   exsr      wrivat_b                                     => carico VAT
016600071009     C                   exsr      wrivat_e                                     => carico VAT
016700111125     C                   exsr      inzvar
016800071009     C                   else
016900071009     C*
017000010305     C                   if        depspe = *blanks                             => 1° giro
017100010305     C                   eval      depspe = curspe                              => memorizz. spediz
017200080117     C                   clear                   tivinds
017300020305     C                   exsr      impvab
017400111130     C                   z-add     VABNCL        sav_VABNCL
017500111130     C                   z-add     VABPKB        sav_VABPKB
017600111130     C                   z-add     VABVLB        sav_VABVLB
017700071003     C                   exsr      wrivat_b                                     => carico VAT
017800071003     C   50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100111130     C                   z-add     sav_VABNCL    VABNCL
018200111130     C                   z-add     sav_VABPKB    VABPKB
018300111130     C                   z-add     sav_VABVLB    VABVLB
018400111130     C  N31              eval      fivabds_ok = fivabds
018500111125     C  N31              exsr      wrivab
018600111125     C                   exsr      inzvar
018700111125     C                   eval      depspe = curspe                              => memorizz. spediz
018800080117     C                   clear                   tivinds
018900020305     C                   exsr      impvab
019000111130     C                   if        wCntRecSpe = *zeros
019100111130     C                   z-add     VABNCL        sav_VABNCL
019200111130     C                   z-add     VABPKB        sav_VABPKB
019300111130     C                   z-add     VABVLB        sav_VABVLB
019400111130     C                   endif
019500111130     C                   add       1             wCntRecSpe
019600071003     C                   exsr      wrivat_b                                     => carico VAT
019700071003     C   50              exsr      wrivat_e                                     => carico VAT
019800020305     C                   else                                                   => x stessa spediz
019900111130     C                   exsr      impvab
020000120706     C   51              add       VABNCL        sav_VABNCL
020100120706     C** 51              z-add     VABNCL        sav_VABNCL
020200111125     C   51              add       VABPKB        sav_VABPKB
020300111125     C   51              add       VABVLB        sav_VABVLB
020400111125     C   52              z-add     VABNCL        sav_VABNCL
020500111125     C   52              z-add     VABPKB        sav_VABPKB
020600111125     C   52              z-add     VABVLB        sav_VABVLB
020700111125     C   53              add       VABNCL        sav_VABNCL
020800111125     C   53              z-add     VABPKB        sav_VABPKB
020900111125     C   53              z-add     VABVLB        sav_VABVLB
021000111125     C*
021100071003     C                   exsr      wrivat_b                                     => carico VAT
021200071003     C   50              exsr      wrivat_e                                     => carico VAT
021300010305     C                   endif
021400010305     C                   endif
021500010305     C                   endif
021600071003     C                   endif
021700071009     C                   endif
021800000905     C*
021900000905     C                   else
022000020725     C                   eval      x_vinflg = '1'
022100000905     C                   endif
022200000905     C*
022300020725     C     VINRRN        chain     tivin000
022400020725     C                   if        %found(tivin00r)
022500020725     C                   eval      y_vinflg = x_vinflg
022600020725     C                   eval      y_vinmsg = x_vinmsg
022700020725     C                   update    tivin000
022800020725     C                   endif
022900020725     C*
023000020725     C/EXEC SQL
023100070530     C+ Fetch C1 into :INPUT_DS
023200020725     C/END-EXEC
023300020725     C*
023400020725     C                   enddo
023500020725     C*
023600020725     C/EXEC SQL
023700020725     C+ close C1
023800020725     C/END-EXEC
023900000905     C*
024000020305     C* Scarico i VAB rimasti "in sospeso"
024100071009     C                   if        wDISK = 'C'
024200111130     C  N31              eval      fivabds_ok = fivabds
024300111130     C  N31              exsr      wrivab
024400071009     C                   endif
024500010202     C*
024600010202     C                   endif
024700990910
024800990910     C* Se non ci sono record con errori ...
024900000710     C                   if        §ctrno = 0
025000990910     C* ... restituisco esito OK.
025100990921     C                   eval      wrkesito = '0'
025200990910     C                   else
025300010201     C                   if        §ctrokvb > 0
025400990921     C                   eval      wrkesito = '1'
025500000710     C                   else
025600000710     C                   eval      wrkesito = '2'
025700990910     C                   endif
025800000710     C                   endif
025900990910     C*
026000990914     C                   if        %open(tivin00r)
026100990908     C                   close     tivin00r
026200990914     C                   endif
026300120706     C                   if        %open(edivabwr)
026400120706     C                   close     edivabwr
026500120706     C                   endif
026600120706     C                   if        %open(edivatwr)
026700120706     C                   close     edivatwr
026800010201     C                   endif
026900990910     C*
027000111031     C                   if        §ctrokvb > 0 or
027100111031     C                             §ctrokvt > 0
027200111031     C                             and wPOI <> *zeros
027300010202     C                   exsr      invio
027400990920     C                   endif
027500990920     C*
027600910830     C                   ENDSR
027700000613     C***
027800010305
027900010305     C*----------------------------------------------------*
028000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028100010305     C*----------------------------------------------------*
028200020305     C     WRIVAB        BEGSR
028300070730     C*
028400071003     C* Considerazioni finali
028500111125     C                   if        ok_VABRMA = *blanks
028600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
028700071003     C                   endif
028800120706     C*
028900120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
029000120709     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
029100120709     C                   EVAL      ok_VABDCM = datcor
029200120709     C                   EVAL      ok_VABDTS = datcor
029300120709     C                   EVAL      ok_VABHMS = oracor
029400120709     C                   EVAL      ok_VABCNT = 1
029500071003     C*
029600120706     C                   write     edivab00                                     => scarico il VAB
029700010305     C*
029800010305     C                   ENDSR
029900990920
030000000801     C*----------------------------------------------------*
030100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030200000801     C*----------------------------------------------------*
030300010201     C     INZVAR        BEGSR
030400000801     C*
030500010201     C                   Z-ADD     *zeros        Num5_0
030600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030700020725     C                   MOVEL     '0'           FlgCAS            1
030800111125     C*
030900111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
031000111130     C*
031100111125     C                   CLEAR                   FIVABDS
031200111130     C                   CLEAR                   FIVABDS_OK
031300111130     C                   CLEAR                   FIVABDS_SAV
031400120706     C                   CLEAR                   EDIVAB00
031500120706     C                   CLEAR                   EDIVAT00
031600000801     C*
031700000801     C                   ENDSR
031800000801     C*----------------------------------------------------*
031900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032000000801     C*----------------------------------------------------*
032100020904     C     DEFCAM        BEGSR
032200080923     C*
032300111125     C                   SETOFF                                       50
032400111125     C                   SETOFF                                       515253
032500070730     C* Imposto i valori di default...
032600120706     C                   EVAL      VABCCM = 1111006
032700120706     C                   EVAL      VATCCM = 1111006
032800120706     C                   EVAL      VABLNP = 111
032900120706     C                   EVAL      VATLNP = 111
033000101007     C                   EVAL      VABCTR = 000
033100070730     C                   EVAL      VABCBO = '1'
033200110520     C                   EVAL      VABTSP = 'C'
033300070222     C* ... e poi verifico se sono stati passati come parametri
033400070222     C                   IF        vlrppt > *blanks
033500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033700070222     C                   EXSR      CHKNUM
033800070222     C                   IF        PiInt=*on
033900070222     C                   Z-ADD     PiVal         VABCCM
034000070222     C                   Z-ADD     PiVal         VATCCM
034100070222     C                   ENDIF
034200070222     C                   ENDIF
034300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034500070222     C                   EXSR      CHKNUM
034600070222     C                   IF        PiInt=*on
034700070222     C                   Z-ADD     PiVal         VABLNP
034800070222     C                   Z-ADD     PiVal         VATLNP
034900070222     C                   ENDIF
035000070222     C                   ENDIF
035100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035300070222     C                   EXSR      CHKNUM
035400070222     C                   IF        PiInt=*on
035500070222     C                   Z-ADD     PiVal         VABCTR
035600070222     C                   ENDIF
035700070928     C                   ENDIF
035800071009     C                   MOVEL     *blanks       wDISK             1
035900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036000071009     C                   IF        wDISK <> *blanks
036100070928     C                   SETON                                        50
036200070222     C                   ENDIF
036300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
036400111125     C                   SETON                                        41
036500080923     C                   ENDIF
036600070222     C                   ENDIF
036700120706     C*
036800120706     C* valido per nr.colli/peso/volume
036900120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
037000120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
037100120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
037200120706     C                   SELECT
037300120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
037400120706     C                   SETON                                        51
037500120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
037600120706     C                   SETON                                        52
037700120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
037800120706     C                   SETON                                        53
037900120706     C                   ENDSL
038000120706     C*
038100071009     C   50              EVAL      VABCTM = '7Q'
038200000801     C*
038300000801     C                   ENDSR
038400000801     C*----------------------------------------------------*
038500120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
038600000801     C*----------------------------------------------------*
038700010201     C     IMPVAB        BEGSR
038800070530     C*
038900070530     C                   SETOFF                                       3132
039000070928     C*
039100070928     C                   EXSR      DEFCAM
039200070928     C*
039300111031     C                   IF        vlrpoi <> 999
039400070928     C                   MOVE(P)   vlrpoi        VABFGS
039500070928     C                   MOVE(P)   vlrpoi        VATFGS
039600111031     C                   Z-ADD     vlrpoi        wPOI              3 0
039700111031     C                   ENDIF
039800070928     C*
039900070928     C                   MOVEL     datcor        VABAAS
040000070928     C                   MOVEL     datcor        VATAAS
040100070928     C                   MOVE      datcor        VABMGS
040200070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
040300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
040400070928     C     '@':'A'       XLATE     VABRSD        VABRSD
040500070928     C* ==
040600070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
040700070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
040800070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
040900110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
041000110524     C**+                EVAL      VABRMA=%trim(%subst(vindta:331:10))
041100070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
041200080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
041300070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
041400070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
041500070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
041600120706     C                   IF        %trim(%subst(vindta:394:1)) = 'F'
041700120706     C                   EVAL      VABCBO='1'
041800120706     C                   ENDIF
041900120706     C                   IF        %trim(%subst(vindta:394:1)) = 'A'
042000120706     C                   EVAL      VABCBO='2'
042100120706     C                   ENDIF
042200121017     C* VABTSP N=C T=E D=H
042300121017     C                   IF        %trim(%subst(vindta:12:1)) = 'N'
042400121017     C                   EVAL      VABTSP='C'
042500121017     C                   ENDIF
042600121017     C                   IF        %trim(%subst(vindta:12:1)) = 'T'
042700121017     C                   EVAL      VABTSP='E'
042800121017     C                   ENDIF
042900121017     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
043000121017     C                   EVAL      VABTSP='H'
043100121017     C                   ENDIF
043200120706     C***                IF        %trim(%subst(vindta:16:1)) = '1'
043300120706     C***                EVAL      VABFFD='S'
043400120706     C***                ENDIF
043500120706     C***                IF        %trim(%subst(vindta:16:1)) = '2'
043600120706     C***                EVAL      VABTC1='A'
043700120706     C***                ENDIF
043800111028     C* CCM
043900120706     C***                SETOFF                                       51
044000120706     C***                EVAL      PiStr=%trim(%subst(vindta:1:7))
044100120706     C***                EXSR      CHKNUM
044200120706     C***                IF        PiInt=*on
044300120706     C***                Z-ADD     PiVal         VABCCM
044400120706     C***                Z-ADD     PiVal         VATCCM
044500120706     C***                ELSE
044600120706     C***                SETON                                        32
044700120706     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
044800120706     C***                          + ' ' + 'VABCCM VATCCM'
044900120706     C***                ENDIF
045000120706     C***                IF        VABCCM = 0240227
045100120706     C***                SETON                                        51
045200120706     C***                ENDIF
045300111109     C* CTR
045400120706     C***                IF        VABCCM = 0240227
045500120706     C***                EVAL      VABCTR = 100
045600120706     C***                ENDIF
045700111028     C* LNP
045800120706     C***                EVAL      PiStr=%trim(%subst(vindta:14:3))
045900120706     C***                EXSR      CHKNUM
046000120706     C***                IF        PiInt=*on
046100120706     C***                Z-ADD     PiVal         VABLNP
046200120706     C***                Z-ADD     PiVal         VABFGS
046300120706     C***                Z-ADD     PiVal         VATLNP
046400120706     C***                Z-ADD     PiVal         VATFGS
046500120706     C***
046600120706     C***                IF        vlrpoi = 999
046700120706     C***                Z-ADD     VABFGS        wPOI
046800120706     C***                ENDIF
046900120706     C***
047000120706     C***                ELSE
047100120706     C***                SETON                                        32
047200120706     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
047300120706     C***                          + ' ' + 'VABLNP VATLNP'
047400120706     C***                ENDIF
047500071003     C* CAD
047600070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
047700070928     C                   EXSR      CHKNUM
047800070928     C                   IF        PiInt=*on
047900070928     C                   Z-ADD     PiVal         Num5_0
048000070928     C                   MOVEL     Num5_0        VABCAD
048100070928     C                   ELSE
048200070928     C                   SETON                                        32
048300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048400070928     C                             + ' ' + 'VABCAD'
048500070928     C                   ENDIF
048600080923     C* RMN
048700120706     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
048800110524     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
048900120706     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
049000101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
049100080923     C                   EXSR      CHKNUM
049200080923     C                   IF        PiInt=*on
049300080923     C                   Z-ADD     PiVal         VABRMN
049400080923     C                   ELSE
049500080923     C                   SETON                                        32
049600080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700080923     C                             + ' ' + 'VABRMN'
049800080923     C                   ENDIF
049900080923     C* NSP
050000111125     C                   IF        *in41 = *on
050100080923     C* NSP => Stacco un numeratore da AZNUM
050200080923     C                   clear                   TRUL33DS
050300080923     C                   eval      I33OPE = *zeros
050400080923     C                   eval      I33CNU = 302
050500080923     C                   eval      I33NUM = 1
050600080923     C                   movel     TRUL33DS      KPJBU
050700080923     C                   call      'TRUL33R'
050800080923     C                   parm                    KPJBA
050900080923     C                   movel     KPJBU         TRUL33DS
051000080923     C                   if        O33ERR = *zeros
051100080923     C                   z-add     O33NRF        VABNSP
051200080923     C                   z-add     O33NRF        VATNSP
051300080923     C                   else
051400080923     C                   SETON                                        31
051500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051600080923     C                             + ' ' + 'VABNSP VATNSP'
051700080923     C                   endif
051800080923     C                   ELSE
051900120706     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
052000110525     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
052100120706     C                   EVAL      PiStr=%trim(%subst(vindta:187+3:7))
052200080923     C                   EXSR      CHKNUM
052300080923     C                   IF        PiInt=*on
052400080923     C                   Z-ADD     PiVal         VABNSP
052500080923     C                   Z-ADD     PiVal         VATNSP
052600080923     C                   ELSE
052700080923     C                   SETON                                        31
052800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900080923     C                             + ' ' + 'VABNSP VATNSP'
053000080923     C                   ENDIF
053100080923     C                   ENDIF
053200071003     C* NCL
053300070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
053400070928     C                   EXSR      CHKNUM
053500070928     C                   IF        PiInt=*on
053600070928     C                   Z-ADD     PiVal         VABNCL
053700070928     C                   ELSE
053800070928     C                   SETON                                        32
053900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054000070928     C                             + ' ' + 'VABNCL'
054100070928     C                   ENDIF
054200071003     C* PKB
054300120709     C                   IF        %subst(vindta:170:8) <> *blanks
054400120709     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
054500070928     C                   EXSR      CHKNUM
054600070928     C                   IF        PiNum=*on
054700111125     C                   Z-ADD     PiVal         VABPKB
054800070928     C                   ELSE
054900070928     C                   SETON                                        32
055000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055100070928     C                             + ' ' + 'VABPKB'
055200070928     C                   ENDIF
055300110520     C                   ENDIF
055400130211     C* valorizzare il peso a 1 kg quando il cliente non lo valorizza
055500130211     C                   IF        VABPkb = 0
055600130211     C                   EVAL      VABPkb = 1
055700130211     C                   ENDIF
055800120706     C* VLB
055900120706     C                   IF        %subst(vindta:308:7) <> *blanks
056000120706     C                   EVAL      PiStr=%trim(%subst(vindta:308:7))
056100120706     C                   EXSR      CHKNUM
056200120706     C                   IF        PiNum=*on
056300120709     C                   Z-ADD(H)  PiVal         VABVLB
056400120706     C                   ELSE
056500120706     C                   SETON                                        32
056600120706     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056700120706     C                             + ' ' + 'VABVLB'
056800120706     C                   ENDIF
056900120706     C                   ENDIF
057000080923     C* CAS
057100080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
057200080923     C                             AND
057300080923     C                             %trim(%subst(vindta:280:12))<>*zeros
057400080923     C                             AND
057500080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
057600080923     C                             AND
057700080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
057800080923     C                             AND
057900080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
058000070928     C                   MOVEL     '1'           FlgCAS
058100070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
058200070928     C                   EXSR      CHKNUM
058300070928     C                   IF        PiNum=*on
058400070928     C                   Z-ADD     PiVal         VABCAS
058500070928     C                   ELSE
058600070928     C                   SETON                                        32
058700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058800070928     C                             + ' ' + 'VABCAS'
058900070928     C                   ENDIF
059000070928     C                   ENDIF
059100071003     C* IAS
059200080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
059300080923     C                             AND
059400080923     C                             %trim(%subst(vindta:295:12))<>*zeros
059500080923     C                             AND
059600080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
059700080923     C                             AND
059800080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
059900080923     C                             AND
060000080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
060100080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
060200080923     C                   EXSR      CHKNUM
060300080923     C                   IF        PiNum=*on
060400080923     C                   Z-ADD     PiVal         VABIAS
060500080923     C                   ELSE
060600080923     C                   SETON                                        32
060700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060800080923     C                             + ' ' + 'VABIAS'
060900080923     C                   ENDIF
061000071003     C                   ENDIF
061100070730     C*
061200070730     C* Considerazioni finali su CBO/CAS
061300070730     C                   IF        FlgCAS = '1'
061400070730     C                   IF        VABCBO = '1'
061500070730     C                   EVAL      VABCBO = '4'
061600070730     C                   ENDIF
061700070730     C                   IF        VABCBO = '2'
061800070730     C                   EVAL      VABCBO = '6'
061900070730     C                   ENDIF
062000070730     C                   ENDIF
062100020305     C*
062200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
062300011113     C                   EXSR      CHKIMPDIV
062400010202     C*
062500000801     C* Ebbene...
062600000801     C                   ADD       1             §CTRMO
062700070530     C                   IF        *in31 <> *zeros OR
062800070530     C                             *in32 <> *zeros
062900000801     C                   ADD       1             §CTRNO
063000020725     C                   EVAL      x_vinflg = '2'
063100000801     C                   ELSE
063200010201     C                   ADD       1             §CTROKVB
063300000801     C                   ENDIF
063400000801     C*
063500000801     C                   ENDSR
063600010201     C*----------------------------------------------------*
063700120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
063800010201     C*----------------------------------------------------*
063900071003     C     WRIVAT_B      BEGSR
064000090204     C*
064100090204     C                   eval      vatAAS = vabAAS
064200090204     C                   eval      vatLNP = vabLNP
064300090204     C                   eval      vatNRS = vabNRS
064400090204     C                   eval      vatNSP = vabNSP
064500010201     C*
064600120706     C* Valorizzo il buffer di scrittura del EDIVAT
064700070928     C* - TIPO RECORD "B"
064800070928     C                   if        %subst(vindta:92:15) <> *blanks
064900120706     C*
065000120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
065100120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
065200120706     C                   EVAL      VATCNT = 1
065300120706     C*
065400070928     C                   eval      VATTRC = 'B'
065500070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
065600120706     C                   write     EDIVAT00
065700111031     C                   add       1             §ctrokvt
065800020725     C                   endif
065900010201     C*
066000010201     C                   ENDSR
066100071003     C*----------------------------------------------------*
066200120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
066300071003     C*----------------------------------------------------*
066400071003     C     WRIVAT_E      BEGSR
066500090204     C*
066600090204     C                   eval      vatAAS = vabAAS
066700090204     C                   eval      vatLNP = vabLNP
066800090204     C                   eval      vatNRS = vabNRS
066900090204     C                   eval      vatNSP = vabNSP
067000071003     C*
067100120706     C* Valorizzo il buffer di scrittura del EDIVAT
067200071003     C* - TIPO RECORD "E"
067300071003     C                   if        %subst(vindta:331:26) <> *blanks
067400120706     C*
067500120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
067600120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
067700120706     C                   EVAL      VATCNT = 1
067800120706     C*
067900071003     C                   eval      VATTRC = 'E'
068000071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
068100120706     C                   write     EDIVAT00
068200111031     C                   add       1             §ctrokvt
068300071003     C                   endif
068400071003     C*
068500071003     C                   ENDSR
068600020904
068700020904
068800010202     C*----------------------------------------------------*
068900120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
069000010202     C*----------------------------------------------------*
069100020305     C     PREVAT        BEGSR
069200010202     C*
069300120706     C* Compongo il nome del membro da dare al EDIVATWR
069400010202     C                   eval      parmbr = vlrhdl
069500010202     C                   movel     'M'           parmbr
069600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069700010202     C                   eval      paropz = '1'
069800010202     C* Effettuo la chiamata al CLLE preposto
069900120706     C                   call(e)   'TITVEVTC'
070000010202     C                   parm                    parccm
070100010202     C                   parm                    parmbr
070200010202     C                   parm                    paropz
070300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070400010202     C                   if        %error
070500010202     C                   movel     '1'           chkcall
070600010202     C                   else
070700010202     C                   movel     '0'           chkcall
070800010202     C                   endif
070900010202     C*
071000010202     C                   ENDSR
071100000801     C*----------------------------------------------------*
071200000801     C*  CONTROLLO NUMERICITA' CAMPI
071300000801     C*----------------------------------------------------*
071400000801     C     CHKNUM        BEGSR
071500081113     C*
071600081113     C                   IF        PiDecChr = *blanks
071700110524     C                   EVAL      PiDecChr = '.'
071800081113     C                   ENDIF
071900081113     C*
072000081113     C                   callp(e)  UBISNUM_Check(PiStr
072100081113     C                                          :PiDecChr
072200081113     C                                          :PiVal
072300081113     C                                          :PiNum
072400081113     C                                          :PiInt)
072500081113     C*
072600000801     C                   IF        %error
072700000801     C                   EVAL      PiInt=*off
072800000801     C                   ENDIF
072900000801     C*
073000000801     C                   ENDSR
073100000801     C***
073200000801
073300011113
073400011113     C*----------------------------------------------------*
073500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
073600011113     C*----------------------------------------------------*
073700011113     C     CHKIMPDIV     BEGSR
073800011113     C*
073900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
074000011113     C                   Z-ADD     *zeros        wrkDec            9 9
074100011113     C*
074200011113     C* Come prima cosa effettuo considerazioni sulla divisa
074300011113     C                   IF        vabIAS > *zeros
074400011113     C                   IF        vabVAS <> 'EUR'
074500011113     C                   EVAL      vabVAS =  'ITL'
074600011113     C                   ENDIF
074700011113     C                   ENDIF
074800011113     C*
074900011113     C                   IF        vabCAS > *zeros
075000011113     C                   IF        vabVCA <> 'EUR'
075100011113     C                   EVAL      vabVCA =  'ITL'
075200011113     C                   ENDIF
075300011113     C                   ENDIF
075400011113     C*
075500011113     C                   IF        vabVMD > *zeros
075600020305     C                   IF        vabVAD <> 'EUR'
075700011113     C                   EVAL      vabVAD =  'ITL'
075800011113     C                   ENDIF
075900011113     C                   ENDIF
076000011113     C*
076100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
076200011113     C                   Z-ADD     vabIAS        wrkDec
076300011113     C                   IF        wrkDec > *zeros
076400011113     C                   IF        vabVAS = 'ITL'
076500011113     C                   EVAL      vabIAS = *zeros
076600011113     C                   ENDIF
076700011113     C                   ENDIF
076800011113     C*
076900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
077000011113     C                   Z-ADD     vabCAS        wrkDec
077100011113     C                   IF        wrkDec > *zeros
077200011113     C                   IF        vabVCA = 'ITL'
077300011113     C                   EVAL      vabCAS = *zeros
077400011113     C                   ENDIF
077500011113     C                   ENDIF
077600011113     C*
077700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
077800011113     C                   Z-ADD     vabVMD        wrkDec
077900011113     C                   IF        wrkDec > *zeros
078000011113     C                   IF        vabVAD = 'ITL'
078100011113     C                   EVAL      vabVMD = *zeros
078200011113     C                   ENDIF
078300011113     C                   ENDIF
078400011113     C*
078500011113     C                   ENDSR
078600011113     C***
078700011113
078800011113
078900000801
079000000801
079100990920      /TITLE Invio dei dati al punto operativo.
079200010202     C     invio         BEGSR
079300990920     C*
079400120706     C* 1° invio EDIVAT
079500010201     C                   reset                   dscmz
079600111031     C                   move      wPOI          cmzdst
079700120706     C                   eval      cmzfld = 'EDIVATWR'
079800010201     C                   eval      cmzmbd = vlrhdl
079900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080000021009     C***                if        prmfir = *blanks
080100120706     C                   eval      cmzfla = 'EDIVAT0F'
080200120706     C                   eval      cmzmba = 'EDIVAT0F'
080300021009     C***                else
080400021009     C***                eval      cmzfla = prmfir
080500021009     C***                eval      cmzmba = prmfir
080600021009     C***                endif
080700010201     C                   eval      cmznrr = *zeros
080800020305     C                   move      §ctrokvt      cmznrr
080900021018     C                   eval      cmzlba = vlrfl1
081000010201     C                   call(e)   'TIS711C'
081100010201     C                   parm                    dscmz
081200010201     C                   parm      *blanks       esito
081300010205     C                   if        %error
081400010205     C                             or cmzerr = '1'
081500010205     C                             or esito  = '1'
081600010205     C                   eval      wrkesito = '3'
081700010205     C                   else
081800010201     C*
081900120706     C* 2° invio EDIVAB
082000010201     C                   reset                   dscmz
082100111031     C                   move      wPOI          cmzdst
082200010201     C                   eval      cmzfld = vlrfou
082300010201     C                   eval      cmzmbd = vlrhdl
082400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082500021009     C***                if        prmfir = *blanks
082600120712     C                   eval      cmzfla = 'EDIVAB0F'
082700120712     C                   eval      cmzmba = 'EDIVAB0F'
082800021009     C***                else
082900021009     C***                eval      cmzfla = prmfir
083000021009     C***                eval      cmzmba = prmfir
083100021009     C***                endif
083200010201     C                   eval      cmznrr = *zeros
083300010201     C                   move      §ctrokvb      cmznrr
083400021018     C                   eval      cmzlba = vlrfl1
083500010201     C                   call(e)   'TIS711C'
083600010201     C                   parm                    dscmz
083700010201     C                   parm      *blanks       esito
083800010201     C                   if        %error
083900010201     C                             or cmzerr = '1'
084000010201     C                             or esito  = '1'
084100010201     C                   eval      wrkesito = '3'
084200010201     C                   endif
084300010205     C                   endif
084400990920     C*
084500000613     C                   ENDSR
084600000613     C***
084700070411
084800070411     C     *pssr         BEGSR
084900070411     C*
085000070411     C                   if        %open(tivin00r)
085100070411     C                   close     tivin00r
085200070411     C                   endif
085300120706     C                   if        %open(edivabwr)
085400120706     C                   close     edivabwr
085500120706     C                   endif
085600120706     C                   if        %open(edivatwr)
085700120706     C                   close     edivatwr
085800070411     C                   endif
085900070411     C*
086000070411     C* Effettuo la chiamata al CLLE preposto
086100120706     C                   call(e)   'TITVEVTC'
086200070411     C                   parm                    parccm
086300070411     C                   parm                    parmbr
086400070411     C                   parm      '2'           paropz
086500070411     C*
086600070411     C                   eval      wrkesito = '2'
086700070411     C*
086800070411     C                   seton                                        LR
086900070411     C*
087000070411     C                   ENDSR     '*CANCL'
087100070411     C***
087200070411
087300990910
087400000613     C     *inzsr        BEGSR
087500990910     C*
087600990910     C     *entry        plist
087700990920     C                   parm                    tivlrds
087800990921     C                   parm      wrkesito      esito
087900000724     C                   parm                    prmlit
088000000710     C                   parm                    prmfir
088100000613     C*
088200000830     C* CALCOLA LA DATA CORRENTE
088300120706     C                   time                    wn14             14 0
088400120706     C                   movel     wn14          oracor            6 0          *ORA
088500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088600100324     C                   eval      datcor = %dec(%date() : *ISO)
088700000830     C*
088800000613     C                   ENDSR
088900000613     C***
