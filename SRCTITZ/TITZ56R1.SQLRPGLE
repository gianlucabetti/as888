000100120706      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400121113     C*  per questo cliente NON c'è il tipo tracciato B per EDIVAT
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800120709     FEDIVABwr  O    E             DISK    usropn prefix(ok_)
000900120706     FEDIVATwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900120706     D fivabds       e ds                  extname(EDIVAB0f)
002000120706     D fivabds_sav   e ds                  extname(EDIVAB0f) prefix(sav_)
002100120706     D fivabds_ok    e ds                  extname(EDIVAB0f) prefix(ok_)
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700121113     D depspe          s              7    INZ(*blanks)
002800121113     D curspe          s              7    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500120903     D wDivPKB         s                   like(VABNCL)
003600000830
003700020725
003800020725     D*------------------
003900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004000020725     D*------------------
004100070530     D INPUT_DS        DS                  INZ
004200101007     D  VINDTA                     2048
004300101007     D  VINFLG                        1
004400121113     D  VINSPE                        7
004500101007     D  VINRRN                        8  0
004600020725     D*
004700080923     D*------------------
004800080923     D* DS REPERIMENTO NUMERATORE
004900080923     D*------------------
005000080923     D trul33ds      e ds                  inz
005100080923     D*------------------
005200080923     D* DS ARCHITETTURA
005300080923     D*------------------
005400080923     D kpjba         e ds                  inz
005500080923     D*------------------
005600121113     D  Num7_0         s              7  0
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
008100120706     C                   call(e)   'TITVEVTC'
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
009400990908
009500000801
009600910830     C*--------------------------------------------------------
009700120706     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
009800910830     C*--------------------------------------------------------
009900070530     C     RWFILE        BEGSR
010000990910     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400120706     C                   if        not %open(edivabwr)
010500120706     C                   open      edivabwr
010600990914     C                   endif
010700070530     C*
010800120706     C* Eseguo operazioni di aggiunta nuovo membro in EDIVATWR
010900020305     C                   exsr      prevat
011000010201     C*
011100010202     C                   if        chkcall = '0'
011200010202     C*
011300120706     C                   if        not %open(edivatwr)
011400120706     C                   open      edivatwr
011500010201     C                   endif
011600080117     C*
011700080117     C                   EXSR      INZVAR
011800080117     C                   EXSR      DEFCAM
011900990910     C*
012000010201     C                   clear                   §CTROKVB          5 0
012100020305     C                   clear                   §CTROKVT          5 0
012200000801     C                   clear                   §CTRMO            5 0
012300000801     C                   clear                   §CTRNO            5 0
012400990910     C*
012500020725     C/EXEC SQL
012600020725     C+ declare C1 cursor for select
012700121113     C+ vindta, vinflg, substr(vindta, 311, 7) as sped, rrn(tivin00r)
012800060223     C+ from tivin00r
012900060223     C+ order by sped
013000020725     C+ for read only
013100020725     C/END-EXEC
013200020725     C
013300020725     C/EXEC SQL
013400020725     C+ open C1
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800070530     C+ Fetch C1 into :INPUT_DS
013900020725     C/END-EXEC
014000020725     C*
014100020725     C                   dow       sqlcod = *zeros
014200990913     C*
014300020725     C                   if        vindta > *blanks
014400000613     C                   add       1             rrnum
014500000801     C*
014600020725     C                   if        vinflg = *blanks
014700020725     C                             or vinflg = '0'
014800020725     C                             or vinflg = '2'
014900000801     C*
015000020725     C                   clear                   x_vinmsg
015100020725     C                   eval      x_vinflg = '1'
015200010305     C*
015300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015400121113     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
015500020305     C                   MOVEL(p)  PiStr         curspe
015600010305     C*
015700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015800080923     C                   if        *in50 = *off
015900071003     C                   exsr      impvab
016000111130     C  N31              eval      fivabds_ok = fivabds
016100111130     C  N31              exsr      wrivab
016200071003     C                   exsr      wrivat_b                                     => carico VAT
016300111125     C                   exsr      inzvar
016400071003     C                   else
016500080923     C*
016600071009     C                   if        wDISK = 'D'
016700071009     C                   exsr      impvab
016800111130     C  N31              eval      fivabds_ok = fivabds
016900111130     C  N31              exsr      wrivab
017000071009     C                   exsr      wrivat_b                                     => carico VAT
017100071009     C                   exsr      wrivat_e                                     => carico VAT
017200111125     C                   exsr      inzvar
017300071009     C                   else
017400071009     C*
017500010305     C                   if        depspe = *blanks                             => 1° giro
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700080117     C                   clear                   tivinds
017800020305     C                   exsr      impvab
017900111130     C                   z-add     VABNCL        sav_VABNCL
018000111130     C                   z-add     VABPKB        sav_VABPKB
018100111130     C                   z-add     VABVLB        sav_VABVLB
018200071003     C                   exsr      wrivat_b                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400010305     C                   else
018500020725     C                   if        depspe <> curspe                             => rottura di spediz
018600111130     C                   z-add     sav_VABNCL    VABNCL
018700111130     C                   z-add     sav_VABPKB    VABPKB
018800111130     C                   z-add     sav_VABVLB    VABVLB
018900111130     C  N31              eval      fivabds_ok = fivabds
019000111125     C  N31              exsr      wrivab
019100111125     C                   exsr      inzvar
019200111125     C                   eval      depspe = curspe                              => memorizz. spediz
019300080117     C                   clear                   tivinds
019400020305     C                   exsr      impvab
019500111130     C                   if        wCntRecSpe = *zeros
019600111130     C                   z-add     VABNCL        sav_VABNCL
019700111130     C                   z-add     VABPKB        sav_VABPKB
019800111130     C                   z-add     VABVLB        sav_VABVLB
019900111130     C                   endif
020000111130     C                   add       1             wCntRecSpe
020100071003     C                   exsr      wrivat_b                                     => carico VAT
020200071003     C   50              exsr      wrivat_e                                     => carico VAT
020300020305     C                   else                                                   => x stessa spediz
020400111130     C                   exsr      impvab
020500120706     C   51              add       VABNCL        sav_VABNCL
020600111125     C   51              add       VABPKB        sav_VABPKB
020700111125     C   51              add       VABVLB        sav_VABVLB
020800111125     C   52              z-add     VABNCL        sav_VABNCL
020900111125     C   52              z-add     VABPKB        sav_VABPKB
021000111125     C   52              z-add     VABVLB        sav_VABVLB
021100111125     C   53              add       VABNCL        sav_VABNCL
021200111125     C   53              z-add     VABPKB        sav_VABPKB
021300111125     C   53              z-add     VABVLB        sav_VABVLB
021400111125     C*
021500071003     C                   exsr      wrivat_b                                     => carico VAT
021600071003     C   50              exsr      wrivat_e                                     => carico VAT
021700010305     C                   endif
021800010305     C                   endif
021900010305     C                   endif
022000071003     C                   endif
022100071009     C                   endif
022200000905     C*
022300000905     C                   else
022400020725     C                   eval      x_vinflg = '1'
022500000905     C                   endif
022600000905     C*
022700020725     C     VINRRN        chain     tivin000
022800020725     C                   if        %found(tivin00r)
022900020725     C                   eval      y_vinflg = x_vinflg
023000020725     C                   eval      y_vinmsg = x_vinmsg
023100020725     C                   update    tivin000
023200020725     C                   endif
023300020725     C*
023400020725     C/EXEC SQL
023500070530     C+ Fetch C1 into :INPUT_DS
023600020725     C/END-EXEC
023700020725     C*
023800020725     C                   enddo
023900020725     C*
024000020725     C/EXEC SQL
024100020725     C+ close C1
024200020725     C/END-EXEC
024300000905     C*
024400020305     C* Scarico i VAB rimasti "in sospeso"
024500071009     C                   if        wDISK = 'C'
024600111130     C  N31              eval      fivabds_ok = fivabds
024700111130     C  N31              exsr      wrivab
024800071009     C                   endif
024900010202     C*
025000010202     C                   endif
025100990910
025200990910     C* Se non ci sono record con errori ...
025300000710     C                   if        §ctrno = 0
025400990910     C* ... restituisco esito OK.
025500990921     C                   eval      wrkesito = '0'
025600990910     C                   else
025700010201     C                   if        §ctrokvb > 0
025800990921     C                   eval      wrkesito = '1'
025900000710     C                   else
026000000710     C                   eval      wrkesito = '2'
026100990910     C                   endif
026200000710     C                   endif
026300990910     C*
026400990914     C                   if        %open(tivin00r)
026500990908     C                   close     tivin00r
026600990914     C                   endif
026700120706     C                   if        %open(edivabwr)
026800120706     C                   close     edivabwr
026900120706     C                   endif
027000120706     C                   if        %open(edivatwr)
027100120706     C                   close     edivatwr
027200010201     C                   endif
027300990910     C*
027400111031     C                   if        §ctrokvb > 0 or
027500111031     C                             §ctrokvt > 0
027600111031     C                             and wPOI <> *zeros
027700010202     C                   exsr      invio
027800990920     C                   endif
027900990920     C*
028000910830     C                   ENDSR
028100000613     C***
028200010305
028300010305     C*----------------------------------------------------*
028400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
028500010305     C*----------------------------------------------------*
028600020305     C     WRIVAB        BEGSR
028700070730     C*
028800071003     C* Considerazioni finali
028900111125     C                   if        ok_VABRMA = *blanks
029000111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
029100071003     C                   endif
029200120706     C*
029300120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
029400120709     C                   EVAL      ok_VABCMR = %char(datcor)+'-'+%char(oracor)
029500120709     C                   EVAL      ok_VABDCM = datcor
029600120709     C                   EVAL      ok_VABDTS = datcor
029700120709     C                   EVAL      ok_VABHMS = oracor
029800120709     C                   EVAL      ok_VABCNT = 1
029900071003     C*
030000120706     C                   write     edivab00                                     => scarico il VAB
030100010305     C*
030200010305     C                   ENDSR
030300990920
030400000801     C*----------------------------------------------------*
030500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030600000801     C*----------------------------------------------------*
030700010201     C     INZVAR        BEGSR
030800000801     C*
030900010201     C                   Z-ADD     *zeros        Num5_0
031000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031100020725     C                   MOVEL     '0'           FlgCAS            1
031200111125     C*
031300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
031400111130     C*
031500111125     C                   CLEAR                   FIVABDS
031600111130     C                   CLEAR                   FIVABDS_OK
031700111130     C                   CLEAR                   FIVABDS_SAV
031800120706     C                   CLEAR                   EDIVAB00
031900120706     C                   CLEAR                   EDIVAT00
032000000801     C*
032100000801     C                   ENDSR
032200000801     C*----------------------------------------------------*
032300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032400000801     C*----------------------------------------------------*
032500020904     C     DEFCAM        BEGSR
032600080923     C*
032700111125     C                   SETOFF                                       50
032800111125     C                   SETOFF                                       515253
032900070730     C* Imposto i valori di default...
033000121113     C                   EVAL      VABCCM = 1661663
033100121113     C                   EVAL      VATCCM = 1661663
033200121113     C                   EVAL      VABLNP = 166
033300121113     C                   EVAL      VATLNP = 166
033400121113     C                   EVAL      VABCTR = 000
033500120903     C                   EVAL      VABCBO = '1'
033600070222     C* ... e poi verifico se sono stati passati come parametri
033700070222     C                   IF        vlrppt > *blanks
033800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034000070222     C                   EXSR      CHKNUM
034100070222     C                   IF        PiInt=*on
034200070222     C                   Z-ADD     PiVal         VABCCM
034300070222     C                   Z-ADD     PiVal         VATCCM
034400070222     C                   ENDIF
034500070222     C                   ENDIF
034600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034800070222     C                   EXSR      CHKNUM
034900070222     C                   IF        PiInt=*on
035000070222     C                   Z-ADD     PiVal         VABLNP
035100070222     C                   Z-ADD     PiVal         VATLNP
035200070222     C                   ENDIF
035300070222     C                   ENDIF
035400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035600070222     C                   EXSR      CHKNUM
035700070222     C                   IF        PiInt=*on
035800070222     C                   Z-ADD     PiVal         VABCTR
035900070222     C                   ENDIF
036000070928     C                   ENDIF
036100071009     C                   MOVEL     *blanks       wDISK             1
036200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036300071009     C                   IF        wDISK <> *blanks
036400070928     C                   SETON                                        50
036500070222     C                   ENDIF
036600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
036700111125     C                   SETON                                        41
036800080923     C                   ENDIF
036900070222     C                   ENDIF
037000120706     C*
037100120706     C* valido per nr.colli/peso/volume
037200120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
037300120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
037400120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
037500120706     C                   SELECT
037600120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
037700120706     C                   SETON                                        51
037800120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
037900120706     C                   SETON                                        52
038000120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
038100120706     C                   SETON                                        53
038200120706     C                   ENDSL
038300120706     C*
038400071009     C   50              EVAL      VABCTM = '7Q'
038500000801     C*
038600000801     C                   ENDSR
038700000801     C*----------------------------------------------------*
038800120706     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
038900000801     C*----------------------------------------------------*
039000010201     C     IMPVAB        BEGSR
039100070530     C*
039200070530     C                   SETOFF                                       3132
039300070928     C*
039400070928     C                   EXSR      DEFCAM
039500070928     C*
039600111031     C                   IF        vlrpoi <> 999
039700070928     C                   MOVE(P)   vlrpoi        VABFGS
039800070928     C                   MOVE(P)   vlrpoi        VATFGS
039900111031     C                   Z-ADD     vlrpoi        wPOI              3 0
040000111031     C                   ENDIF
040100070928     C*
040200070928     C                   MOVEL     datcor        VABAAS
040300070928     C                   MOVEL     datcor        VATAAS
040400070928     C                   MOVE      datcor        VABMGS
040500121113     C                   EVAL      VABRSD=%trim(%subst(vindta:53:35))
040600121113     C                   EVAL      VABRD2=%trim(%subst(vindta:88:35))
040700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
040800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
040900070928     C* ==
041000121113     C                   EVAL      VABIND=%trim(%subst(vindta:123:35))
041100121113     C                   EVAL      VABLOD=%trim(%subst(vindta:167:25))
041200121113     C                   EVAL      VABPRD=%trim(%subst(vindta:202:2))
041300121113     C                   EVAL      VABRMA=%trim(%subst(vindta:324:15))
041400121113     C                   EVAL      VABNAS=%trim(%subst(vindta:234:15))
041500121113     C                   EVAL      VABTIC=%trim(%subst(vindta:301:2))
041600121113     C                   IF        %trim(%subst(vindta:47:2)) = '2'
041700121113     C                   EVAL      VABCBO='2'
041800121113     C                   ENDIF
041900151113     C* però se il codice cliente è 1662191
042000151113     C                   IF        VABCCM = 1662191
042100151113     C* viene forzato porto franco
042200151113     C                   EVAL      VABCBO='1'
042300151113     C                   ENDIF
042400131129     C                   EVAL      VABTC1=%trim(%subst(vindta:306:1))
042500131129     C                   EVAL      VABTC2=%trim(%subst(vindta:307:1))
042600071003     C* CAD
042700121113     C                   EVAL      PiStr=%trim(%subst(vindta:158:9))
042800070928     C                   EXSR      CHKNUM
042900070928     C                   IF        PiInt=*on
043000070928     C                   Z-ADD     PiVal         Num5_0
043100070928     C                   MOVEL     Num5_0        VABCAD
043200070928     C                   ELSE
043300070928     C                   SETON                                        32
043400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500070928     C                             + ' ' + 'VABCAD'
043600070928     C                   ENDIF
043700121113     C*
043800121113     C* Se provincia nn valorizzata la reperisco
043900121113     C* tramite TISI95R a seconda dei dati d instradamento presenti
044000121113     C                   IF        VABPRD = *blanks
044100121113     C                   CLEAR                   TISI95DS
044200121113     C                   EVAL      I95TCN = '3'
044300121113     C                   Z-ADD     datcor        I95DAT
044400121113     C                   EVAL      I95NAR = VABNZD
044500121113     C                   EVAL      I95CAP = VABCAD
044600121113     C                   EVAL      I95LOC = VABLOD
044700121113     C                   CALL      'TISI95R'
044800121113     C                   PARM                    TISI95DS
044900121113     C                   EVAL      VABPRD = O95PRV
045000121113     C                   ENDIF
045100121113     C* CCM
045200121113     C                   IF        %subst(vindta:3:7) <> *all'0' AND
045300121113     C                             %subst(vindta:3:7) <> *blanks
045400121113     C                   EVAL      PiStr=%trim(%subst(vindta:3:7))
045500121113     C                   EXSR      CHKNUM
045600121113     C                   IF        PiInt=*on
045700121113     C                   Z-ADD     PiVal         VABCCM
045800121113     C                   Z-ADD     PiVal         VATCCM
045900121113     C                   ELSE
046000121113     C                   SETON                                        31
046100121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200121113     C                             + ' ' + 'VABCCM VATCCM'
046300121113     C                   ENDIF
046400121113     C                   ENDIF
046500131204     C*
046600131204     C* Valorizzo le note dopo il codice cliente
046700131210     C***                IF        VABCCM = 1662191
046800131210     C***                EVAL      VABNOT=%trim(%subst(vindta:308:16))
046900131210     C***                ELSE
047000131210     C                   EVAL      VABNOT=%trim(%subst(vindta:380:35))
047100131210     C                   EVAL      VABNT2=%trim(%subst(vindta:415:35))
047200131210     C***                ENDIF
047300121113     C* LNP
047400121113     C                   IF        %subst(vindta:11:3) <> *all'0' AND
047500121113     C                             %subst(vindta:11:3) <> *blanks
047600121113     C                   EVAL      PiStr=%trim(%subst(vindta:11:3))
047700121113     C                   EXSR      CHKNUM
047800121113     C                   IF        PiInt=*on
047900121113     C                   Z-ADD     PiVal         VABLNP
048000121113     C                   Z-ADD     PiVal         VATLNP
048100121113     C                   ELSE
048200121113     C                   SETON                                        31
048300121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048400121113     C                             + ' ' + 'VABLNP VATLNP'
048500121113     C                   ENDIF
048600121113     C                   ENDIF
048700121113     C* RMN
048800121113     C                   EVAL      PiStr=%trim(%subst(vindta:308:16))
048900121113     C                   EXSR      CHKNUM
049000121113     C                   IF        PiInt=*on
049100121113     C                   Z-ADD     PiVal         VABRMN
049200121113     C                   ELSE
049300121113     C                   SETON                                        32
049400121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500121113     C                             + ' ' + 'VABRMN'
049600121113     C                   ENDIF
049700121113     C* NSP
049800121113     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
049900121113     C                   EXSR      CHKNUM
050000121113     C                   IF        PiInt=*on
050100121113     C                   Z-ADD     PiVal         Num7_0
050200121113     C                   MOVE(P)   Num7_0        VABNSP
050300121113     C                   MOVE(P)   Num7_0        VATNSP
050400121113     C                   ELSE
050500121113     C                   SETON                                        31
050600121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700121113     C                             + ' ' + 'VABNSP VATNSP'
050800121113     C                   ENDIF
050900121113     C* NCL
051000121113     C                   EVAL      PiStr=%trim(%subst(vindta:249:6))
051100121113     C                   EXSR      CHKNUM
051200121113     C                   IF        PiInt=*on
051300121113     C                   Z-ADD     PiVal         VABNCL
051400121113     C                   ELSE
051500121113     C                   SETON                                        32
051600121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051700121113     C                             + ' ' + 'VABNCL'
051800121113     C                   ENDIF
051900121113     C* PKB
052000121113     C                   EVAL      PiStr=%trim(%subst(vindta:255:9))
052100121113     C                   EXSR      CHKNUM
052200121113     C                   IF        PiNum=*on
052300121113     C***                EVAL      PiVal = PiVal / 1000                         * da grammi a Kg.
052400121113     C                   Z-ADD     PiVal         VABPKB
052500151113     C* Peso minimo non a zero ...
052600151113     C                   IF        PiVal < 0,1
052700151113     C                   Z-ADD     0,1           VABPKB
052800151113     C                   ENDIF
052900121113     C                   ELSE
053000121113     C                   SETON                                        32
053100121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053200121113     C                             + ' ' + 'VABPKB'
053300121113     C                   ENDIF
053400121113     C* IAS
053500121113     C                   IF        %subst(vindta:216:15) <> *blanks
053600121113     C                   EVAL      PiStr=%trim(%subst(vindta:216:15))
053700121113     C                   EXSR      CHKNUM
053800121113     C                   IF        PiNum=*on
053900121113     C                   Z-ADD     PiVal         VABIAS
054000121113     C                   IF        VABIAS > *zeros
054100121113     C                   EVAL      VABVAS = 'EUR'
054200121113     C                   ENDIF
054300121113     C                   ELSE
054400121113     C                   SETON                                        32
054500121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600121113     C                             + ' ' + 'VABIAS'
054700121113     C                   ENDIF
054800121113     C                   ENDIF
054900121113     C* CAS
055000121113     C                   IF        %subst(vindta:286:15) <> *blanks
055100121113     C                   MOVEL     '1'           FlgCAS
055200121113     C                   EVAL      PiStr=%trim(%subst(vindta:286:15))
055300121113     C                   EVAL      VABVCA=%trim(%subst(vindta:303:3))
055400121113     C                   EXSR      CHKNUM
055500121113     C                   IF        PiNum=*on
055600121113     C                   Z-ADD     PiVal         VABCAS
055700121113     C                   ELSE
055800121113     C                   SETON                                        32
055900121113     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056000121113     C                             + ' ' + 'VABCAS'
056100121113     C                   ENDIF
056200121113     C                   ENDIF
056300070730     C*
056400070730     C* Considerazioni finali su CBO/CAS
056500121113     C                   IF        FlgCAS <> '0'
056600070730     C                   IF        VABCBO = '1'
056700070730     C                   EVAL      VABCBO = '4'
056800070730     C                   ENDIF
056900070730     C                   IF        VABCBO = '2'
057000070730     C                   EVAL      VABCBO = '6'
057100070730     C                   ENDIF
057200070730     C                   ENDIF
057300020305     C*
057400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057500011113     C                   EXSR      CHKIMPDIV
057600010202     C*
057700000801     C* Ebbene...
057800000801     C                   ADD       1             §CTRMO
057900070530     C                   IF        *in31 <> *zeros OR
058000070530     C                             *in32 <> *zeros
058100000801     C                   ADD       1             §CTRNO
058200020725     C                   EVAL      x_vinflg = '2'
058300000801     C                   ELSE
058400010201     C                   ADD       1             §CTROKVB
058500000801     C                   ENDIF
058600000801     C*
058700000801     C                   ENDSR
058800010201     C*----------------------------------------------------*
058900120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "B"
059000121113     C*  per questo cliente NON c'è il tipo tracciato B
059100010201     C*----------------------------------------------------*
059200071003     C     WRIVAT_B      BEGSR
059300090204     C*
059400090204     C                   eval      vatAAS = vabAAS
059500090204     C                   eval      vatLNP = vabLNP
059600090204     C                   eval      vatNRS = vabNRS
059700090204     C                   eval      vatNSP = vabNSP
059800010201     C*
059900120706     C* Valorizzo il buffer di scrittura del EDIVAT
060000070928     C* - TIPO RECORD "B"
060100121113     C***                if        %subst(vindta:104:20) <> *blanks
060200120706     C*
060300120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
060400120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
060500120706     C                   EVAL      VATCNT = 1
060600120706     C*
060700070928     C                   eval      VATTRC = 'B'
060800121113     C***                eval      VATNOT = %trim(%subst(vindta:104:20))
060900121113     C***                write     EDIVAT00
061000111031     C                   add       1             §ctrokvt
061100121113     C***                endif
061200010201     C*
061300010201     C                   ENDSR
061400071003     C*----------------------------------------------------*
061500120706     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X EDIVAT) - TRC "E"
061600071003     C*----------------------------------------------------*
061700071003     C     WRIVAT_E      BEGSR
061800090204     C*
061900090204     C                   eval      vatAAS = vabAAS
062000090204     C                   eval      vatLNP = vabLNP
062100090204     C                   eval      vatNRS = vabNRS
062200090204     C                   eval      vatNSP = vabNSP
062300071003     C*
062400120706     C* Valorizzo il buffer di scrittura del EDIVAT
062500071003     C* - TIPO RECORD "E"
062600121113     C                   if        %subst(vindta:28:19) <> *blanks
062700120706     C*
062800120706     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
062900120706     C                   EVAL      VATCMR = %char(datcor)+'-'+%char(oracor)
063000120706     C                   EVAL      VATCNT = 1
063100120706     C*
063200071003     C                   eval      VATTRC = 'E'
063300121113     C                   eval      vatNOT = %trim(%subst(vindta:28:19))
063400120706     C                   write     EDIVAT00
063500111031     C                   add       1             §ctrokvt
063600071003     C                   endif
063700071003     C*
063800071003     C                   ENDSR
063900020904
064000020904
064100010202     C*----------------------------------------------------*
064200120706     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE EDIVATWR
064300010202     C*----------------------------------------------------*
064400020305     C     PREVAT        BEGSR
064500010202     C*
064600120706     C* Compongo il nome del membro da dare al EDIVATWR
064700010202     C                   eval      parmbr = vlrhdl
064800010202     C                   movel     'M'           parmbr
064900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065000010202     C                   eval      paropz = '1'
065100010202     C* Effettuo la chiamata al CLLE preposto
065200120706     C                   call(e)   'TITVEVTC'
065300010202     C                   parm                    parccm
065400010202     C                   parm                    parmbr
065500010202     C                   parm                    paropz
065600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065700010202     C                   if        %error
065800010202     C                   movel     '1'           chkcall
065900010202     C                   else
066000010202     C                   movel     '0'           chkcall
066100010202     C                   endif
066200010202     C*
066300010202     C                   ENDSR
066400000801     C*----------------------------------------------------*
066500000801     C*  CONTROLLO NUMERICITA' CAMPI
066600000801     C*----------------------------------------------------*
066700000801     C     CHKNUM        BEGSR
066800081113     C*
066900081113     C                   IF        PiDecChr = *blanks
067000121113     C                   EVAL      PiDecChr = '.'
067100081113     C                   ENDIF
067200081113     C*
067300081113     C                   callp(e)  UBISNUM_Check(PiStr
067400081113     C                                          :PiDecChr
067500081113     C                                          :PiVal
067600081113     C                                          :PiNum
067700081113     C                                          :PiInt)
067800081113     C*
067900000801     C                   IF        %error
068000000801     C                   EVAL      PiInt=*off
068100000801     C                   ENDIF
068200000801     C*
068300000801     C                   ENDSR
068400000801     C***
068500000801
068600011113
068700011113     C*----------------------------------------------------*
068800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
068900011113     C*----------------------------------------------------*
069000011113     C     CHKIMPDIV     BEGSR
069100011113     C*
069200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
069300011113     C                   Z-ADD     *zeros        wrkDec            9 9
069400011113     C*
069500011113     C* Come prima cosa effettuo considerazioni sulla divisa
069600011113     C                   IF        vabIAS > *zeros
069700011113     C                   IF        vabVAS <> 'EUR'
069800011113     C                   EVAL      vabVAS =  'ITL'
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C                   IF        vabCAS > *zeros
070300011113     C                   IF        vabVCA <> 'EUR'
070400011113     C                   EVAL      vabVCA =  'ITL'
070500011113     C                   ENDIF
070600011113     C                   ENDIF
070700011113     C*
070800011113     C                   IF        vabVMD > *zeros
070900020305     C                   IF        vabVAD <> 'EUR'
071000011113     C                   EVAL      vabVAD =  'ITL'
071100011113     C                   ENDIF
071200011113     C                   ENDIF
071300011113     C*
071400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
071500011113     C                   Z-ADD     vabIAS        wrkDec
071600011113     C                   IF        wrkDec > *zeros
071700011113     C                   IF        vabVAS = 'ITL'
071800011113     C                   EVAL      vabIAS = *zeros
071900011113     C                   ENDIF
072000011113     C                   ENDIF
072100011113     C*
072200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
072300011113     C                   Z-ADD     vabCAS        wrkDec
072400011113     C                   IF        wrkDec > *zeros
072500011113     C                   IF        vabVCA = 'ITL'
072600011113     C                   EVAL      vabCAS = *zeros
072700011113     C                   ENDIF
072800011113     C                   ENDIF
072900011113     C*
073000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073100011113     C                   Z-ADD     vabVMD        wrkDec
073200011113     C                   IF        wrkDec > *zeros
073300011113     C                   IF        vabVAD = 'ITL'
073400011113     C                   EVAL      vabVMD = *zeros
073500011113     C                   ENDIF
073600011113     C                   ENDIF
073700011113     C*
073800011113     C                   ENDSR
073900011113     C***
074000011113
074100011113
074200000801
074300000801
074400990920      /TITLE Invio dei dati al punto operativo.
074500010202     C     invio         BEGSR
074600990920     C*
074700120706     C* 1° invio EDIVAT
074800010201     C                   reset                   dscmz
074900111031     C                   move      wPOI          cmzdst
075000120706     C                   eval      cmzfld = 'EDIVATWR'
075100010201     C                   eval      cmzmbd = vlrhdl
075200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075300021009     C***                if        prmfir = *blanks
075400120706     C                   eval      cmzfla = 'EDIVAT0F'
075500120706     C                   eval      cmzmba = 'EDIVAT0F'
075600021009     C***                else
075700021009     C***                eval      cmzfla = prmfir
075800021009     C***                eval      cmzmba = prmfir
075900021009     C***                endif
076000010201     C                   eval      cmznrr = *zeros
076100020305     C                   move      §ctrokvt      cmznrr
076200021018     C                   eval      cmzlba = vlrfl1
076300010201     C                   call(e)   'TIS711C'
076400010201     C                   parm                    dscmz
076500010201     C                   parm      *blanks       esito
076600010205     C                   if        %error
076700010205     C                             or cmzerr = '1'
076800010205     C                             or esito  = '1'
076900010205     C                   eval      wrkesito = '3'
077000010205     C                   else
077100010201     C*
077200120706     C* 2° invio EDIVAB
077300010201     C                   reset                   dscmz
077400111031     C                   move      wPOI          cmzdst
077500010201     C                   eval      cmzfld = vlrfou
077600010201     C                   eval      cmzmbd = vlrhdl
077700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077800021009     C***                if        prmfir = *blanks
077900120712     C                   eval      cmzfla = 'EDIVAB0F'
078000120712     C                   eval      cmzmba = 'EDIVAB0F'
078100021009     C***                else
078200021009     C***                eval      cmzfla = prmfir
078300021009     C***                eval      cmzmba = prmfir
078400021009     C***                endif
078500010201     C                   eval      cmznrr = *zeros
078600010201     C                   move      §ctrokvb      cmznrr
078700021018     C                   eval      cmzlba = vlrfl1
078800010201     C                   call(e)   'TIS711C'
078900010201     C                   parm                    dscmz
079000010201     C                   parm      *blanks       esito
079100010201     C                   if        %error
079200010201     C                             or cmzerr = '1'
079300010201     C                             or esito  = '1'
079400010201     C                   eval      wrkesito = '3'
079500010201     C                   endif
079600010205     C                   endif
079700990920     C*
079800000613     C                   ENDSR
079900000613     C***
080000070411
080100070411     C     *pssr         BEGSR
080200070411     C*
080300070411     C                   if        %open(tivin00r)
080400070411     C                   close     tivin00r
080500070411     C                   endif
080600120706     C                   if        %open(edivabwr)
080700120706     C                   close     edivabwr
080800120706     C                   endif
080900120706     C                   if        %open(edivatwr)
081000120706     C                   close     edivatwr
081100070411     C                   endif
081200070411     C*
081300070411     C* Effettuo la chiamata al CLLE preposto
081400120706     C                   call(e)   'TITVEVTC'
081500070411     C                   parm                    parccm
081600070411     C                   parm                    parmbr
081700070411     C                   parm      '2'           paropz
081800070411     C*
081900070411     C                   eval      wrkesito = '2'
082000070411     C*
082100070411     C                   seton                                        LR
082200070411     C*
082300070411     C                   ENDSR     '*CANCL'
082400070411     C***
082500070411
082600990910
082700000613     C     *inzsr        BEGSR
082800990910     C*
082900990910     C     *entry        plist
083000990920     C                   parm                    tivlrds
083100990921     C                   parm      wrkesito      esito
083200000724     C                   parm                    prmlit
083300000710     C                   parm                    prmfir
083400000613     C*
083500000830     C* CALCOLA LA DATA CORRENTE
083600120706     C                   time                    wn14             14 0
083700120706     C                   movel     wn14          oracor            6 0          *ORA
083800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
083900100324     C                   eval      datcor = %dec(%date() : *ISO)
084000000830     C*
084100000613     C                   ENDSR
084200000613     C***
