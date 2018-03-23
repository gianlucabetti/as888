000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400160304      *
000500170315      * disk A
000600170315      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
001100131016     FFIVATWWr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100131016     D fivabds       e ds                  extname(FIVAB00f)
002200131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
002300131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900160304     D depspe          s              6    INZ(*blanks)
003000160304     D curspe          s              6    INZ(*blanks)
003100010202     D parccm          s              8    INZ(*blanks)
003200010202     D parmbr          s             10    INZ(*blanks)
003300010202     D paropz          s              1    INZ(*blanks)
003400010202     D chkcall         s              1    INZ(*blanks)
003500080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003600070530     D Num5_0          s              5  0
003700131016     D w70             s             70
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600160304     D  VINSPE                        6
004700101007     D  VINRRN                        8  0
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
005800121113     D  Num7_0         s              7  0
005900081113
006000081113     D*------------------
006100081113     D* LINKING A DEFINIZIONI ESTERNE
006200081113     D*------------------
006300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500081113
006600990908
006700010201
006800010201
006900080117     C*
007000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100080117     C
007200080117     C/EXEC SQL
007300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400080117     C/END-EXEC
007500080117     C
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000613     C*
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300131017     C                   call(e)   'TITVVTC'
008400010202     C                   parm                    parccm
008500010202     C                   parm                    parmbr
008600010202     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600990908
009700000801
009800910830     C*--------------------------------------------------------
009900131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600131016     C                   if        not %open(FIVABWWr)
010700131016     C                   open      FIVABWWr
010800990914     C                   endif
010900070530     C*
011000131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011100020305     C                   exsr      prevat
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500131016     C                   if        not %open(FIVATWWr)
011600131016     C                   open      FIVATWWr
011700010201     C                   endif
011800080117     C*
011900080117     C                   EXSR      INZVAR
012000080117     C                   EXSR      DEFCAM
012100990910     C*
012200010201     C                   clear                   §CTROKVB          5 0
012300020305     C                   clear                   §CTROKVT          5 0
012400000801     C                   clear                   §CTRMO            5 0
012500000801     C                   clear                   §CTRNO            5 0
012600990910     C*
012700020725     C/EXEC SQL
012800020725     C+ declare C1 cursor for select
012900170315     C+ vindta, vinflg, substr(vindta, 111, 6) as sped, rrn(tivin00r)
013000060223     C+ from tivin00r
013100060223     C+ order by sped
013200020725     C+ for read only
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600020725     C+ open C1
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000070530     C+ Fetch C1 into :INPUT_DS
014100020725     C/END-EXEC
014200020725     C*
014300020725     C                   dow       sqlcod = *zeros
014400990913     C*
014500020725     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700000801     C*
014800020725     C                   if        vinflg = *blanks
014900020725     C                             or vinflg = '0'
015000020725     C                             or vinflg = '2'
015100000801     C*
015200020725     C                   clear                   x_vinmsg
015300020725     C                   eval      x_vinflg = '1'
015400010305     C*
015500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015600160304     C* (uso RMN)
015700160304     C                   EVAL      PiStr=%trim(%subst(vindta:15:5))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200071003     C                   exsr      impvab
016300111130     C  N31              eval      fivabds_ok = fivabds
016400111130     C  N31              exsr      wrivab
016500160304     C***                exsr      wrivat_a                                     => carico VAT
016600170316     C                   exsr      wrivat_b                                     => carico VAT
016700170316     C                   exsr      wrivat_ij                                    => carico VAT
016800160304     C***                exsr      wrivat_s                                     => carico VAT
016900111125     C                   exsr      inzvar
017000071003     C                   else
017100080923     C*
017200071009     C                   if        wDISK = 'D'
017300170316     C***                exsr      repNSP
017400071009     C                   exsr      impvab
017500111130     C  N31              eval      fivabds_ok = fivabds
017600111130     C  N31              exsr      wrivab
017700170316     C***                exsr      wrivat_a                                     => carico VAT
017800170316     C                   exsr      wrivat_b                                     => carico VAT
017900170316     C                   exsr      wrivat_ij                                    => carico VAT
018000160304     C***                exsr      wrivat_s                                     => carico VAT
018100170316     C***                exsr      wrivat_e                                     => carico VAT
018200111125     C                   exsr      inzvar
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800170316     C***                exsr      repNSP
018900020305     C                   exsr      impvab
019000111130     C                   z-add     VABNCL        sav_VABNCL
019100111130     C                   z-add     VABPKB        sav_VABPKB
019200111130     C                   z-add     VABVLB        sav_VABVLB
019300160304     C***                exsr      wrivat_a                                     => carico VAT
019400170316     C                   exsr      wrivat_b                                     => carico VAT
019500170316     C                   exsr      wrivat_ij                                    => carico VAT
019600170316     C***                exsr      wrivat_s                                     => carico VAT
019700170316     C***50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   else
019900020725     C                   if        depspe <> curspe                             => rottura di spediz
020000111130     C                   z-add     sav_VABNCL    VABNCL
020100111130     C                   z-add     sav_VABPKB    VABPKB
020200111130     C                   z-add     sav_VABVLB    VABVLB
020300111130     C  N31              eval      fivabds_ok = fivabds
020400111125     C  N31              exsr      wrivab
020500111125     C                   exsr      inzvar
020600111125     C                   eval      depspe = curspe                              => memorizz. spediz
020700080117     C                   clear                   tivinds
020800170316     C***                exsr      repNSP
020900020305     C                   exsr      impvab
021000111130     C                   if        wCntRecSpe = *zeros
021100111130     C                   z-add     VABNCL        sav_VABNCL
021200111130     C                   z-add     VABPKB        sav_VABPKB
021300111130     C                   z-add     VABVLB        sav_VABVLB
021400111130     C                   endif
021500111130     C                   add       1             wCntRecSpe
021600160304     C***                exsr      wrivat_a                                     => carico VAT
021700170316     C                   exsr      wrivat_b                                     => carico VAT
021800170316     C                   exsr      wrivat_ij                                    => carico VAT
021900160304     C***                exsr      wrivat_s                                     => carico VAT
022000170316     C***50              exsr      wrivat_e                                     => carico VAT
022100020305     C                   else                                                   => x stessa spediz
022200111130     C                   exsr      impvab
022300120706     C   51              add       VABNCL        sav_VABNCL
022400111125     C   51              add       VABPKB        sav_VABPKB
022500111125     C   51              add       VABVLB        sav_VABVLB
022600111125     C   52              z-add     VABNCL        sav_VABNCL
022700111125     C   52              z-add     VABPKB        sav_VABPKB
022800111125     C   52              z-add     VABVLB        sav_VABVLB
022900111125     C   53              add       VABNCL        sav_VABNCL
023000111125     C   53              z-add     VABPKB        sav_VABPKB
023100111125     C   53              z-add     VABVLB        sav_VABVLB
023200131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023300131016     C   54              ADD       VABPKB        sav_VABPKB
023400131016     C   54              ADD       VABVLB        sav_VABVLB
023500111125     C*
023600160304     C***                exsr      wrivat_a                                     => carico VAT
023700170316     C                   exsr      wrivat_b                                     => carico VAT
023800170316     C                   exsr      wrivat_ij                                    => carico VAT
023900160304     C***                exsr      wrivat_s                                     => carico VAT
024000170316     C***50              exsr      wrivat_e                                     => carico VAT
024100010305     C                   endif
024200010305     C                   endif
024300010305     C                   endif
024400071003     C                   endif
024500071009     C                   endif
024600000905     C*
024700000905     C                   else
024800020725     C                   eval      x_vinflg = '1'
024900000905     C                   endif
025000000905     C*
025100020725     C     VINRRN        chain     tivin000
025200020725     C                   if        %found(tivin00r)
025300020725     C                   eval      y_vinflg = x_vinflg
025400020725     C                   eval      y_vinmsg = x_vinmsg
025500020725     C                   update    tivin000
025600020725     C                   endif
025700020725     C*
025800020725     C/EXEC SQL
025900070530     C+ Fetch C1 into :INPUT_DS
026000020725     C/END-EXEC
026100020725     C*
026200020725     C                   enddo
026300020725     C*
026400020725     C/EXEC SQL
026500020725     C+ close C1
026600020725     C/END-EXEC
026700000905     C*
026800020305     C* Scarico i VAB rimasti "in sospeso"
026900071009     C                   if        wDISK = 'C'
027000160304     C                   z-add     sav_VABNCL    VABNCL
027100160304     C                   z-add     sav_VABPKB    VABPKB
027200160304     C                   z-add     sav_VABVLB    VABVLB
027300111130     C  N31              eval      fivabds_ok = fivabds
027400111130     C  N31              exsr      wrivab
027500071009     C                   endif
027600010202     C*
027700010202     C                   endif
027800990910
027900990910     C* Se non ci sono record con errori ...
028000000710     C                   if        §ctrno = 0
028100990910     C* ... restituisco esito OK.
028200990921     C                   eval      wrkesito = '0'
028300990910     C                   else
028400010201     C                   if        §ctrokvb > 0
028500990921     C                   eval      wrkesito = '1'
028600000710     C                   else
028700000710     C                   eval      wrkesito = '2'
028800990910     C                   endif
028900000710     C                   endif
029000990910     C*
029100990914     C                   if        %open(tivin00r)
029200990908     C                   close     tivin00r
029300990914     C                   endif
029400131016     C                   if        %open(FIVABWWr)
029500131016     C                   close     FIVABWWr
029600120706     C                   endif
029700131016     C                   if        %open(FIVATWWr)
029800131016     C                   close     FIVATWWr
029900010201     C                   endif
030000990910     C*
030100111031     C                   if        §ctrokvb > 0 or
030200111031     C                             §ctrokvt > 0
030300111031     C                             and wPOI <> *zeros
030400010202     C                   exsr      invio
030500990920     C                   endif
030600990920     C*
030700910830     C                   ENDSR
030800000613     C***
030900010305
031000010305     C*----------------------------------------------------*
031100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031200010305     C*----------------------------------------------------*
031300020305     C     WRIVAB        BEGSR
031400070730     C*
031500071003     C* Considerazioni finali
031600111125     C                   if        ok_VABRMA = *blanks
031700111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031800071003     C                   endif
031900071003     C*
032000131016     C                   write     FIVAB000                                     => scarico il VAB
032100010305     C*
032200010305     C                   ENDSR
032300990920
032400000801     C*----------------------------------------------------*
032500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032600000801     C*----------------------------------------------------*
032700010201     C     INZVAR        BEGSR
032800000801     C*
032900010201     C                   Z-ADD     *zeros        Num5_0
033000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033100020725     C                   MOVEL     '0'           FlgCAS            1
033200111125     C*
033300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033400111130     C*
033500111125     C                   CLEAR                   FIVABDS
033600111130     C                   CLEAR                   FIVABDS_OK
033700111130     C                   CLEAR                   FIVABDS_SAV
033800131016     C                   CLEAR                   FIVAB000
033900131016     C                   CLEAR                   FIVAT000
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034400000801     C*----------------------------------------------------*
034500020904     C     DEFCAM        BEGSR
034600080923     C*
034700111125     C                   SETOFF                                       50
034800111125     C                   SETOFF                                       515253
034900131016     C                   SETOFF                                       54
035000070730     C* Imposto i valori di default...
035100170316     C                   EVAL      VABCCM = 0281964
035200170316     C                   EVAL      VATCCM = 0281964
035300170316     C                   EVAL      VABLNP = 028
035400170316     C                   EVAL      VATLNP = 028
035500170316     C                   EVAL      VABCTR = 100
035600160304     C                   EVAL      VABCBO = '1'
035700160304     C                   EVAL      VABCTM = '7Q'
035800070222     C* ... e poi verifico se sono stati passati come parametri
035900070222     C                   IF        vlrppt > *blanks
036000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCCM
036500070222     C                   Z-ADD     PiVal         VATCCM
036600070222     C                   ENDIF
036700070222     C                   ENDIF
036800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037000070222     C                   EXSR      CHKNUM
037100070222     C                   IF        PiInt=*on
037200070222     C                   Z-ADD     PiVal         VABLNP
037300070222     C                   Z-ADD     PiVal         VATLNP
037400070222     C                   ENDIF
037500070222     C                   ENDIF
037600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037800070222     C                   EXSR      CHKNUM
037900070222     C                   IF        PiInt=*on
038000070222     C                   Z-ADD     PiVal         VABCTR
038100070222     C                   ENDIF
038200070928     C                   ENDIF
038300071009     C                   MOVEL     *blanks       wDISK             1
038400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038500071009     C                   IF        wDISK <> *blanks
038600070928     C                   SETON                                        50
038700070222     C                   ENDIF
038800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038900111125     C                   SETON                                        41
039000080923     C                   ENDIF
039100070222     C                   ENDIF
039200120706     C*
039300120706     C* valido per nr.colli/peso/volume
039400120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039500120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039600120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039700131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039800120706     C                   SELECT
039900120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
040000120706     C                   SETON                                        51
040100120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040200120706     C                   SETON                                        52
040300120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040400120706     C                   SETON                                        53
040500131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040600131016     C                   SETON                                        54
040700120706     C                   ENDSL
040800120706     C*
040900071009     C   50              EVAL      VABCTM = '7Q'
041000000801     C*
041100000801     C                   ENDSR
041200131016     C*----------------------------------------------------*
041300131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041400131016     C*----------------------------------------------------*
041500131016     C     REPNSP        BEGSR
041600131016     C*
041700131016     C                   EXSR      INZVAR
041800131016     C                   EXSR      DEFCAM
041900131016     C*
042000131016     C                   SETON                                        60
042100131016     C*
042200131016     C* NSP => Stacco un numeratore da AZNUM
042300131016     C                   clear                   TRUL33DS
042400131016     C                   eval      I33OPE = *zeros
042500131016     C                   eval      I33CNU = 302
042600131016     C                   eval      I33NUM = 1
042700131016     C                   movel     TRUL33DS      KPJBU
042800131016     C                   call      'TRUL33R'
042900131016     C                   parm                    KPJBA
043000131016     C                   movel     KPJBU         TRUL33DS
043100131016     C                   if        O33ERR = *zeros
043200131016     C                   z-add     O33NRF        VABNSP
043300131016     C                   z-add     O33NRF        VATNSP
043400131016     C                   else
043500131016     C                   SETON                                        31
043600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043700160304     C                             + ' ' + 'VABNSP VATNSP'
043800131016     C                   endif
043900131016     C*
044000131016     C                   ENDSR
044100000801     C*----------------------------------------------------*
044200131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044300000801     C*----------------------------------------------------*
044400010201     C     IMPVAB        BEGSR
044500070530     C*
044600070530     C                   SETOFF                                       3132
044700070928     C*
044800070928     C                   EXSR      DEFCAM
044900070928     C*
045000111031     C                   IF        vlrpoi <> 999
045100070928     C                   MOVE(P)   vlrpoi        VABFGS
045200070928     C                   MOVE(P)   vlrpoi        VATFGS
045300111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045400111031     C                   ENDIF
045500070928     C*
045600070928     C                   MOVEL     datcor        VABAAS
045700070928     C                   MOVEL     datcor        VATAAS
045800070928     C                   MOVE      datcor        VABMGS
045900170315     C                   EVAL      w70   =%trim(%subst(vindta:1:38))
046000160304     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100160304     C     '@':'A'       XLATE     w70           w70
046200160304     C* ==
046300160304     C                   EVAL      VABRSD=%subst(w70:1:35)
046400160304     C                   EVAL      VABRD2=%subst(w70:36:5)
046500170315     C                   EVAL      VABIND=%trim(%subst(vindta:40:30))
046600170315     C                   EVAL      VABLOD=%trim(%subst(vindta:77:30))
046700170315     C                   EVAL      VABPRD=%trim(%subst(vindta:108:2))
046800170317     C                   EVAL      VABRMA=%trim(%subst(vindta:202:15))
046900170316     C* queste posizioni paiono eliminate con nuovo tracciato
047000170316     C***                EVAL      VABNOT=%trim(%subst(vindta:191:35))
047100170316     C***                EVAL      VABNT2=%trim(%subst(vindta:191+35:70-35))
047200170316     C***                MOVEL     *blanks       wDCR_Alfa         8
047300170316     C***                EVAL      wDCR_Alfa = %subst(vindta:262:4)+
047400170316     C***                                      %subst(vindta:266:2)+
047500170316     C***                                      %subst(vindta:268:2)
047600170316     C***                MOVE(P)   wDCR_Alfa     VABDCR
047700160304     C*
047800170315     C* Reperimento campi NUMERICI
047900170315     C*
048000170315     C* NSP/RMN
048100170315     C                   EVAL      PiStr=%trim(%subst(vindta:111:6))
048200170315     C                   EXSR      CHKNUM
048300170315     C                   IF        PiInt=*on
048400170315     C                   Z-ADD     PiVal         VABRMN
048500170315     C                   Z-ADD     PiVal         VABNSP
048600170315     C                   ELSE
048700170315     C                   SETON                                        31
048800170315     C                   Z-ADD     *zeros        VABRMN
048900170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049000170315     C                             + ' ' + 'VABRMN VABNSP'
049100170315     C                   ENDIF
049200170315     C* CAD
049300170315     C                   EVAL      PiStr=%trim(%subst(vindta:71:5))
049400170315     C                   EXSR      CHKNUM
049500170315     C                   IF        PiInt=*on
049600170315     C                   Z-ADD     PiVal         Num5_0
049700170315     C                   MOVEL(p)  Num5_0        VABCAD
049800170315     C                   ELSE
049900170315     C                   SETON                                        32
050000170315     C                   EVAL      VABCAD = *zeros
050100170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200170315     C                             + ' ' + 'VABCAD'
050300170315     C                   ENDIF
050400170315     C* NCL
050500170315     C                   EVAL      PiStr=%trim(%subst(vindta:118:6))
050600170315     C                   EXSR      CHKNUM
050700170315     C                   IF        PiInt=*on
050800170315     C                   Z-ADD     PiVal         VABNCL
050900170315     C                   ELSE
051000170315     C                   SETON                                        32
051100170315     C                   Z-ADD     *zeros        VABNCL
051200170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300170315     C                             + ' ' + 'VABNCL'
051400170315     C                   ENDIF
051500170315     C* PKB
051600170315     C                   EVAL      PiStr=%trim(%subst(vindta:125:6))
051700170315     C                   EXSR      CHKNUM
051800170315     C                   IF        PiNum=*on
051900170315     C                   Z-ADD(H)  PiVal         VABPKB
052000170315     C                   ELSE
052100170315     C                   SETON                                        32
052200170315     C                   Z-ADD     *zeros        VABPKB
052300170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052400170315     C                             + ' ' + 'VABPKB'
052500170315     C                   ENDIF
052600170315     C* CAS
052700170315     C                   IF        %trim(%subst(vindta:178:10))<>'0000000000'
052800170315     C                   EVAL      PiStr=%trim(%subst(vindta:178:10))
052900170315     C                   EXSR      CHKNUM
053000170315     C                   IF        PiNum=*on
053100170315     C                   EVAL      VABCAS = PiVal
053200170315     C                   EVAL      FlgCAS = '1'
053300170315     C                   EVAL      VABVCA =  'EUR'
053400170315     C                   ELSE
053500170315     C                   SETON                                        32
053600170315     C                   Z-ADD     *zeros        VABCAS
053700170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053800170315     C                             + ' ' + 'VABCAS'
053900170315     C                   ENDIF
054000170315     C                   ENDIF
054100170315     C* IAS
054200170315     C                   IF        %trim(%subst(vindta:167:10))<>'0000000000'
054300170315     C                   EVAL      PiStr=%trim(%subst(vindta:167:10))
054400170315     C                   EXSR      CHKNUM
054500170315     C                   IF        PiNum=*on
054600170315     C                   EVAL      VABIAS = PiVal
054700170315     C                   EVAL      VABVAS =  'EUR'
054800170315     C                   ELSE
054900170315     C                   SETON                                        32
055000170315     C                   Z-ADD     *zeros        VABIAS
055100170315     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055200170315     C                             + ' ' + 'VABIAS'
055300170315     C                   ENDIF
055400170315     C                   ENDIF
055500070730     C*
055600070730     C* Considerazioni finali su CBO/CAS
055700121113     C                   IF        FlgCAS <> '0'
055800070730     C                   IF        VABCBO = '1'
055900070730     C                   EVAL      VABCBO = '4'
056000070730     C                   ENDIF
056100070730     C                   IF        VABCBO = '2'
056200070730     C                   EVAL      VABCBO = '6'
056300070730     C                   ENDIF
056400070730     C                   ENDIF
056500160304     C*
056600160304     C* Se provincia nn valorizzata la reperisco
056700160304     C* tramite TISI95R a seconda dei dati d instradamento presenti
056800160304     C                   IF        VABPRD =  *blanks AND
056900160304     C                             VABNZD =  *blanks AND
057000160304     C                             VABCAD <> *blanks AND
057100160304     C                             VABLOD <> *blanks
057200160304     C                   CLEAR                   TISI95DS
057300160304     C                   EVAL      I95TCN = '3'
057400160304     C                   Z-ADD     datcor        I95DAT
057500160304     C                   EVAL      I95NAR = VABNZD
057600160304     C                   EVAL      I95CAP = VABCAD
057700160304     C                   EVAL      I95LOC = VABLOD
057800160304     C                   CALL      'TISI95R'
057900160304     C                   PARM                    TISI95DS
058000160304     C                   EVAL      VABPRD = O95PRV
058100160304     C                   ENDIF
058200020305     C*
058300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058400011113     C                   EXSR      CHKIMPDIV
058500010202     C*
058600000801     C* Ebbene...
058700000801     C                   ADD       1             §CTRMO
058800070530     C                   IF        *in31 <> *zeros OR
058900070530     C                             *in32 <> *zeros
059000000801     C                   ADD       1             §CTRNO
059100020725     C                   EVAL      x_vinflg = '2'
059200000801     C                   ELSE
059300010201     C                   ADD       1             §CTROKVB
059400000801     C                   ENDIF
059500000801     C*
059600000801     C                   ENDSR
059700131016     C*----------------------------------------------------*
059800131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059900131016     C*----------------------------------------------------*
060000131016     C     WRIVAT_A      BEGSR
060100131016     C*
060200131016     C                   eval      vatAAS = vabAAS
060300131016     C                   eval      vatLNP = vabLNP
060400131016     C                   eval      vatNRS = vabNRS
060500131016     C                   eval      vatNSP = vabNSP
060600131016     C*
060700131016     C* Valorizzo il buffer di scrittura del FIVAT
060800131016     C* - TIPO RECORD "A"
060900131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
061000131016     C                   eval      VATTRC = 'A'
061100131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
061200131016     C                   write     FIVAT000
061300131016     C                   endif
061400131016     C*
061500131016     C                   ENDSR
061600131016     C*----------------------------------------------------*
061700131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061800131016     C*----------------------------------------------------*
061900131016     C     WRIVAT_B      BEGSR
062000131016     C*
062100131016     C                   eval      vatAAS = vabAAS
062200131016     C                   eval      vatLNP = vabLNP
062300131016     C                   eval      vatNRS = vabNRS
062400131016     C                   eval      vatNSP = vabNSP
062500131016     C*
062600131016     C* Valorizzo il buffer di scrittura del FIVAT
062700131016     C* - TIPO RECORD "B"
062800170317     C                   if        %subst(vindta:279:30) <> *blanks
062900131016     C                   eval      VATTRC = 'B'
063000170317     C                   eval      VATNOT = %trim(%subst(vindta:279:30))
063100131016     C                   write     FIVAT000
063200131016     C                   endif
063300131016     C*
063400131016     C                   ENDSR
063500170316     C*----------------------------------------------------*
063600170316     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
063700170316     C*----------------------------------------------------*
063800170316     C     WRIVAT_IJ     BEGSR
063900170316     C*
064000170316     C                   eval      vatAAS = vabAAS
064100170316     C                   eval      vatLNP = vabLNP
064200170316     C                   eval      vatNRS = vabNRS
064300170316     C                   eval      vatNSP = vabNSP
064400170316     C*
064500170316     C* Valorizzo il buffer di scrittura del FIVAT
064600170316     C* - TIPO RECORD "I"
064700170317     C                   eval      w70 = %subst(vindta:218:60)
064800170316
064900170316     C                   if        %subst(w70:1:35) <> *blanks
065000170316     C                   eval      VATTRC = 'I'
065100170316     C                   eval      VATNOT = %trim(%subst(w70:1:35))
065200170316     C                   write     FIVAT000
065300170316     C                   endif
065400170316
065500170316     C                   if        %subst(w70:36:35) <> *blanks
065600170316     C                   eval      VATTRC = 'J'
065700170316     C                   eval      VATNOT = %trim(%subst(w70:36:35))
065800170316     C                   write     FIVAT000
065900170316     C                   endif
066000170316     C*
066100170316     C                   ENDSR
066200131016     C*----------------------------------------------------*
066300131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
066400131016     C*----------------------------------------------------*
066500131016     C     WRIVAT_E      BEGSR
066600131016     C*
066700131016     C                   eval      vatAAS = vabAAS
066800131016     C                   eval      vatLNP = vabLNP
066900131016     C                   eval      vatNRS = vabNRS
067000131016     C                   eval      vatNSP = vabNSP
067100131016     C*
067200131016     C* Valorizzo il buffer di scrittura del FIVAT
067300131016     C* - TIPO RECORD "E"
067400160304     C                   if        %subst(vindta:35:20) <> *blanks
067500131016     C                   eval      VATTRC = 'E'
067600160304     C                   eval      VATNOT = %trim(%subst(vindta:35:20))
067700131016     C                   write     FIVAT000
067800131016     C                   endif
067900131016     C*
068000131016     C                   ENDSR
068100140708     C*----------------------------------------------------*
068200140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
068300140708     C*----------------------------------------------------*
068400140708     C     WRIVAT_S      BEGSR
068500140708     C*
068600140708     C                   eval      vatAAS = vabAAS
068700140708     C                   eval      vatLNP = vabLNP
068800140708     C                   eval      vatNRS = vabNRS
068900140708     C                   eval      vatNSP = vabNSP
069000140708     C*
069100140708     C* Valorizzo il buffer di scrittura del FIVAT
069200140708     C* - TIPO RECORD "B"
069300140708     C                   if        %subst(vindta:113:015) <> *blanks
069400140708     C                   eval      VATTRC = 'S'
069500140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
069600140708     C                   write     FIVAT000
069700140708     C                   endif
069800140708     C*
069900140708     C                   ENDSR
070000020904
070100020904
070200010202     C*----------------------------------------------------*
070300131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
070400010202     C*----------------------------------------------------*
070500020305     C     PREVAT        BEGSR
070600010202     C*
070700131016     C* Compongo il nome del membro da dare al FIVATWWR
070800010202     C                   eval      parmbr = vlrhdl
070900010202     C                   movel     'M'           parmbr
071000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
071100010202     C                   eval      paropz = '1'
071200010202     C* Effettuo la chiamata al CLLE preposto
071300131017     C                   call(e)   'TITVVTC'
071400010202     C                   parm                    parccm
071500010202     C                   parm                    parmbr
071600010202     C                   parm                    paropz
071700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071800010202     C                   if        %error
071900010202     C                   movel     '1'           chkcall
072000010202     C                   else
072100010202     C                   movel     '0'           chkcall
072200010202     C                   endif
072300010202     C*
072400010202     C                   ENDSR
072500000801     C*----------------------------------------------------*
072600000801     C*  CONTROLLO NUMERICITA' CAMPI
072700000801     C*----------------------------------------------------*
072800000801     C     CHKNUM        BEGSR
072900081113     C*
073000081113     C                   IF        PiDecChr = *blanks
073100160304     C                   EVAL      PiDecChr = ','
073200081113     C                   ENDIF
073300081113     C*
073400081113     C                   callp(e)  UBISNUM_Check(PiStr
073500081113     C                                          :PiDecChr
073600081113     C                                          :PiVal
073700081113     C                                          :PiNum
073800081113     C                                          :PiInt)
073900081113     C*
074000000801     C                   IF        %error
074100000801     C                   EVAL      PiInt=*off
074200000801     C                   ENDIF
074300000801     C*
074400000801     C                   ENDSR
074500000801     C***
074600000801
074700011113
074800011113     C*----------------------------------------------------*
074900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
075000011113     C*----------------------------------------------------*
075100011113     C     CHKIMPDIV     BEGSR
075200011113     C*
075300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
075400011113     C                   Z-ADD     *zeros        wrkDec            9 9
075500011113     C*
075600011113     C* Come prima cosa effettuo considerazioni sulla divisa
075700011113     C                   IF        vabIAS > *zeros
075800011113     C                   IF        vabVAS <> 'EUR'
075900011113     C                   EVAL      vabVAS =  'ITL'
076000011113     C                   ENDIF
076100011113     C                   ENDIF
076200011113     C*
076300011113     C                   IF        vabCAS > *zeros
076400011113     C                   IF        vabVCA <> 'EUR'
076500011113     C                   EVAL      vabVCA =  'ITL'
076600011113     C                   ENDIF
076700011113     C                   ENDIF
076800011113     C*
076900011113     C                   IF        vabVMD > *zeros
077000020305     C                   IF        vabVAD <> 'EUR'
077100011113     C                   EVAL      vabVAD =  'ITL'
077200011113     C                   ENDIF
077300011113     C                   ENDIF
077400011113     C*
077500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
077600011113     C                   Z-ADD     vabIAS        wrkDec
077700011113     C                   IF        wrkDec > *zeros
077800011113     C                   IF        vabVAS = 'ITL'
077900011113     C                   EVAL      vabIAS = *zeros
078000011113     C                   ENDIF
078100011113     C                   ENDIF
078200011113     C*
078300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
078400011113     C                   Z-ADD     vabCAS        wrkDec
078500011113     C                   IF        wrkDec > *zeros
078600011113     C                   IF        vabVCA = 'ITL'
078700011113     C                   EVAL      vabCAS = *zeros
078800011113     C                   ENDIF
078900011113     C                   ENDIF
079000011113     C*
079100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
079200011113     C                   Z-ADD     vabVMD        wrkDec
079300011113     C                   IF        wrkDec > *zeros
079400011113     C                   IF        vabVAD = 'ITL'
079500011113     C                   EVAL      vabVMD = *zeros
079600011113     C                   ENDIF
079700011113     C                   ENDIF
079800011113     C*
079900011113     C                   ENDSR
080000011113     C***
080100011113
080200011113
080300000801
080400000801
080500990920      /TITLE Invio dei dati al punto operativo.
080600010202     C     invio         BEGSR
080700990920     C*
080800131016     C* 1° invio FIVAT
080900010201     C                   reset                   dscmz
081000111031     C                   move      wPOI          cmzdst
081100131016     C                   eval      cmzfld = 'FIVATWWR'
081200010201     C                   eval      cmzmbd = vlrhdl
081300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081400021009     C***                if        prmfir = *blanks
081500131016     C                   eval      cmzfla = 'FIVAT00F'
081600131016     C                   eval      cmzmba = 'FIVAT00F'
081700021009     C***                else
081800021009     C***                eval      cmzfla = prmfir
081900021009     C***                eval      cmzmba = prmfir
082000021009     C***                endif
082100010201     C                   eval      cmznrr = *zeros
082200020305     C                   move      §ctrokvt      cmznrr
082300021018     C                   eval      cmzlba = vlrfl1
082400010201     C                   call(e)   'TIS711C'
082500010201     C                   parm                    dscmz
082600010201     C                   parm      *blanks       esito
082700010205     C                   if        %error
082800010205     C                             or cmzerr = '1'
082900010205     C                             or esito  = '1'
083000010205     C                   eval      wrkesito = '3'
083100010205     C                   else
083200010201     C*
083300131016     C* 2° invio FIVAB
083400010201     C                   reset                   dscmz
083500111031     C                   move      wPOI          cmzdst
083600010201     C                   eval      cmzfld = vlrfou
083700010201     C                   eval      cmzmbd = vlrhdl
083800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083900021009     C***                if        prmfir = *blanks
084000131016     C                   eval      cmzfla = 'FIVAB00F'
084100131016     C                   eval      cmzmba = 'FIVAB00F'
084200021009     C***                else
084300021009     C***                eval      cmzfla = prmfir
084400021009     C***                eval      cmzmba = prmfir
084500021009     C***                endif
084600010201     C                   eval      cmznrr = *zeros
084700010201     C                   move      §ctrokvb      cmznrr
084800021018     C                   eval      cmzlba = vlrfl1
084900010201     C                   call(e)   'TIS711C'
085000010201     C                   parm                    dscmz
085100010201     C                   parm      *blanks       esito
085200010201     C                   if        %error
085300010201     C                             or cmzerr = '1'
085400010201     C                             or esito  = '1'
085500010201     C                   eval      wrkesito = '3'
085600010201     C                   endif
085700010205     C                   endif
085800990920     C*
085900000613     C                   ENDSR
086000000613     C***
086100070411
086200070411     C     *pssr         BEGSR
086300070411     C*
086400070411     C                   if        %open(tivin00r)
086500070411     C                   close     tivin00r
086600070411     C                   endif
086700131016     C                   if        %open(FIVABWWr)
086800131016     C                   close     FIVABWWr
086900120706     C                   endif
087000131016     C                   if        %open(FIVATWWr)
087100131016     C                   close     FIVATWWr
087200070411     C                   endif
087300070411     C*
087400070411     C* Effettuo la chiamata al CLLE preposto
087500131017     C                   call(e)   'TITVVTC'
087600070411     C                   parm                    parccm
087700070411     C                   parm                    parmbr
087800070411     C                   parm      '2'           paropz
087900070411     C*
088000070411     C                   eval      wrkesito = '2'
088100070411     C*
088200070411     C                   seton                                        LR
088300070411     C*
088400070411     C                   ENDSR     '*CANCL'
088500070411     C***
088600070411
088700990910
088800000613     C     *inzsr        BEGSR
088900990910     C*
089000990910     C     *entry        plist
089100990920     C                   parm                    tivlrds
089200990921     C                   parm      wrkesito      esito
089300000724     C                   parm                    prmlit
089400000710     C                   parm                    prmfir
089500000613     C*
089600000830     C* CALCOLA LA DATA CORRENTE
089700120706     C                   time                    wn14             14 0
089800120706     C                   movel     wn14          oracor            6 0          *ORA
089900100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
090000100324     C                   eval      datcor = %dec(%date() : *ISO)
090100000830     C*
090200000613     C                   ENDSR
090300000613     C***
