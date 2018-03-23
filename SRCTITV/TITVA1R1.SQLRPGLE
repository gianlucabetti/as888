000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400160304      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
000900131016     FFIVATWWr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900131016     D fivabds       e ds                  extname(FIVAB00f)
002000131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
002100131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700160331     D depspe          s             10    INZ(*blanks)
002800160331     D curspe          s             10    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500131016     D w70             s             70
003600000830
003700020725
003800020725     D*------------------
003900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004000020725     D*------------------
004100070530     D INPUT_DS        DS                  INZ
004200101007     D  VINDTA                     2048
004300101007     D  VINFLG                        1
004400160331     D  VINSPE                       10
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
008100131017     C                   call(e)   'TITVVTC'
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
009700131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009800910830     C*--------------------------------------------------------
009900070530     C     RWFILE        BEGSR
010000990910     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400131016     C                   if        not %open(FIVABWWr)
010500131016     C                   open      FIVABWWr
010600990914     C                   endif
010700070530     C*
010800131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010900020305     C                   exsr      prevat
011000010201     C*
011100010202     C                   if        chkcall = '0'
011200010202     C*
011300131016     C                   if        not %open(FIVATWWr)
011400131016     C                   open      FIVATWWr
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
012700160331     C+ vindta, vinflg, substr(vindta, 30, 10) as sped, rrn(tivin00r)
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
015400160331     C* (uso NSP/RMN)
015500160331     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300160331     C                   exsr      wrivat_a                                     => carico VAT
016400160331     C                   exsr      wrivat_b                                     => carico VAT
016500160304     C***                exsr      wrivat_s                                     => carico VAT
016600111125     C                   exsr      inzvar
016700071003     C                   else
016800080923     C*
016900071009     C                   if        wDISK = 'D'
017000160331     C***                exsr      repNSP
017100071009     C                   exsr      impvab
017200111130     C  N31              eval      fivabds_ok = fivabds
017300111130     C  N31              exsr      wrivab
017400160331     C                   exsr      wrivat_a                                     => carico VAT
017500160331     C                   exsr      wrivat_b                                     => carico VAT
017600160304     C***                exsr      wrivat_s                                     => carico VAT
017700160331     C***                exsr      wrivat_e                                     => carico VAT
017800111125     C                   exsr      inzvar
017900071009     C                   else
018000071009     C*
018100010305     C                   if        depspe = *blanks                             => 1° giro
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300080117     C                   clear                   tivinds
018400160331     C***                exsr      repNSP
018500020305     C                   exsr      impvab
018600111130     C                   z-add     VABNCL        sav_VABNCL
018700111130     C                   z-add     VABPKB        sav_VABPKB
018800111130     C                   z-add     VABVLB        sav_VABVLB
018900160331     C                   exsr      wrivat_a                                     => carico VAT
019000160331     C                   exsr      wrivat_b                                     => carico VAT
019100160304     C****               exsr      wrivat_s                                     => carico VAT
019200160331     C***50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   else
019400020725     C                   if        depspe <> curspe                             => rottura di spediz
019500111130     C                   z-add     sav_VABNCL    VABNCL
019600111130     C                   z-add     sav_VABPKB    VABPKB
019700111130     C                   z-add     sav_VABVLB    VABVLB
019800111130     C  N31              eval      fivabds_ok = fivabds
019900111125     C  N31              exsr      wrivab
020000111125     C                   exsr      inzvar
020100111125     C                   eval      depspe = curspe                              => memorizz. spediz
020200080117     C                   clear                   tivinds
020300160331     C***                exsr      repNSP
020400020305     C                   exsr      impvab
020500111130     C                   if        wCntRecSpe = *zeros
020600111130     C                   z-add     VABNCL        sav_VABNCL
020700111130     C                   z-add     VABPKB        sav_VABPKB
020800111130     C                   z-add     VABVLB        sav_VABVLB
020900111130     C                   endif
021000111130     C                   add       1             wCntRecSpe
021100160331     C                   exsr      wrivat_a                                     => carico VAT
021200160331     C                   exsr      wrivat_b                                     => carico VAT
021300160304     C***                exsr      wrivat_s                                     => carico VAT
021400160331     C***50              exsr      wrivat_e                                     => carico VAT
021500020305     C                   else                                                   => x stessa spediz
021600111130     C                   exsr      impvab
021700120706     C   51              add       VABNCL        sav_VABNCL
021800111125     C   51              add       VABPKB        sav_VABPKB
021900111125     C   51              add       VABVLB        sav_VABVLB
022000111125     C   52              z-add     VABNCL        sav_VABNCL
022100111125     C   52              z-add     VABPKB        sav_VABPKB
022200111125     C   52              z-add     VABVLB        sav_VABVLB
022300111125     C   53              add       VABNCL        sav_VABNCL
022400111125     C   53              z-add     VABPKB        sav_VABPKB
022500111125     C   53              z-add     VABVLB        sav_VABVLB
022600131016     C   54              Z-ADD     VABNCL        sav_VABNCL
022700131016     C   54              ADD       VABPKB        sav_VABPKB
022800131016     C   54              ADD       VABVLB        sav_VABVLB
022900111125     C*
023000160331     C                   exsr      wrivat_a                                     => carico VAT
023100160331     C                   exsr      wrivat_b                                     => carico VAT
023200160304     C***                exsr      wrivat_s                                     => carico VAT
023300160331     C***50              exsr      wrivat_e                                     => carico VAT
023400010305     C                   endif
023500010305     C                   endif
023600010305     C                   endif
023700071003     C                   endif
023800071009     C                   endif
023900000905     C*
024000000905     C                   else
024100020725     C                   eval      x_vinflg = '1'
024200000905     C                   endif
024300000905     C*
024400020725     C     VINRRN        chain     tivin000
024500020725     C                   if        %found(tivin00r)
024600020725     C                   eval      y_vinflg = x_vinflg
024700020725     C                   eval      y_vinmsg = x_vinmsg
024800020725     C                   update    tivin000
024900020725     C                   endif
025000020725     C*
025100020725     C/EXEC SQL
025200070530     C+ Fetch C1 into :INPUT_DS
025300020725     C/END-EXEC
025400020725     C*
025500020725     C                   enddo
025600020725     C*
025700020725     C/EXEC SQL
025800020725     C+ close C1
025900020725     C/END-EXEC
026000000905     C*
026100020305     C* Scarico i VAB rimasti "in sospeso"
026200071009     C                   if        wDISK = 'C'
026300160304     C                   z-add     sav_VABNCL    VABNCL
026400160304     C                   z-add     sav_VABPKB    VABPKB
026500160304     C                   z-add     sav_VABVLB    VABVLB
026600111130     C  N31              eval      fivabds_ok = fivabds
026700111130     C  N31              exsr      wrivab
026800071009     C                   endif
026900010202     C*
027000010202     C                   endif
027100990910
027200990910     C* Se non ci sono record con errori ...
027300000710     C                   if        §ctrno = 0
027400990910     C* ... restituisco esito OK.
027500990921     C                   eval      wrkesito = '0'
027600990910     C                   else
027700010201     C                   if        §ctrokvb > 0
027800990921     C                   eval      wrkesito = '1'
027900000710     C                   else
028000000710     C                   eval      wrkesito = '2'
028100990910     C                   endif
028200000710     C                   endif
028300990910     C*
028400990914     C                   if        %open(tivin00r)
028500990908     C                   close     tivin00r
028600990914     C                   endif
028700131016     C                   if        %open(FIVABWWr)
028800131016     C                   close     FIVABWWr
028900120706     C                   endif
029000131016     C                   if        %open(FIVATWWr)
029100131016     C                   close     FIVATWWr
029200010201     C                   endif
029300990910     C*
029400111031     C                   if        §ctrokvb > 0 or
029500111031     C                             §ctrokvt > 0
029600111031     C                             and wPOI <> *zeros
029700010202     C                   exsr      invio
029800990920     C                   endif
029900990920     C*
030000910830     C                   ENDSR
030100000613     C***
030200010305
030300010305     C*----------------------------------------------------*
030400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030500010305     C*----------------------------------------------------*
030600020305     C     WRIVAB        BEGSR
030700070730     C*
030800071003     C* Considerazioni finali
030900111125     C                   if        ok_VABRMA = *blanks
031000111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031100071003     C                   endif
031200071003     C*
031300131016     C                   write     FIVAB000                                     => scarico il VAB
031400010305     C*
031500010305     C                   ENDSR
031600990920
031700000801     C*----------------------------------------------------*
031800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031900000801     C*----------------------------------------------------*
032000010201     C     INZVAR        BEGSR
032100000801     C*
032200010201     C                   Z-ADD     *zeros        Num5_0
032300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032400020725     C                   MOVEL     '0'           FlgCAS            1
032500111125     C*
032600111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032700111130     C*
032800111125     C                   CLEAR                   FIVABDS
032900111130     C                   CLEAR                   FIVABDS_OK
033000111130     C                   CLEAR                   FIVABDS_SAV
033100131016     C                   CLEAR                   FIVAB000
033200131016     C                   CLEAR                   FIVAT000
033300000801     C*
033400000801     C                   ENDSR
033500000801     C*----------------------------------------------------*
033600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033700000801     C*----------------------------------------------------*
033800020904     C     DEFCAM        BEGSR
033900080923     C*
034000111125     C                   SETOFF                                       50
034100111125     C                   SETOFF                                       515253
034200131016     C                   SETOFF                                       54
034300070730     C* Imposto i valori di default...
034400160331     C                   EVAL      VABCCM = 0592266
034500160331     C                   EVAL      VATCCM = 0592266
034600160331     C                   EVAL      VABLNP = 059
034700160331     C                   EVAL      VATLNP = 059
034800160331     C                   EVAL      VABCTR = 500
034900070222     C* ... e poi verifico se sono stati passati come parametri
035000070222     C                   IF        vlrppt > *blanks
035100070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035300070222     C                   EXSR      CHKNUM
035400070222     C                   IF        PiInt=*on
035500070222     C                   Z-ADD     PiVal         VABCCM
035600070222     C                   Z-ADD     PiVal         VATCCM
035700070222     C                   ENDIF
035800070222     C                   ENDIF
035900070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036100070222     C                   EXSR      CHKNUM
036200070222     C                   IF        PiInt=*on
036300070222     C                   Z-ADD     PiVal         VABLNP
036400070222     C                   Z-ADD     PiVal         VATLNP
036500070222     C                   ENDIF
036600070222     C                   ENDIF
036700070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036900070222     C                   EXSR      CHKNUM
037000070222     C                   IF        PiInt=*on
037100070222     C                   Z-ADD     PiVal         VABCTR
037200070222     C                   ENDIF
037300070928     C                   ENDIF
037400071009     C                   MOVEL     *blanks       wDISK             1
037500071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037600071009     C                   IF        wDISK <> *blanks
037700070928     C                   SETON                                        50
037800070222     C                   ENDIF
037900080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038000111125     C                   SETON                                        41
038100080923     C                   ENDIF
038200070222     C                   ENDIF
038300120706     C*
038400120706     C* valido per nr.colli/peso/volume
038500120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038600120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038700120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038800131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038900120706     C                   SELECT
039000120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039100120706     C                   SETON                                        51
039200120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039300120706     C                   SETON                                        52
039400120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039500120706     C                   SETON                                        53
039600131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039700131016     C                   SETON                                        54
039800120706     C                   ENDSL
039900120706     C*
040000071009     C   50              EVAL      VABCTM = '7Q'
040100000801     C*
040200000801     C                   ENDSR
040300131016     C*----------------------------------------------------*
040400131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040500131016     C*----------------------------------------------------*
040600131016     C     REPNSP        BEGSR
040700131016     C*
040800131016     C                   EXSR      INZVAR
040900131016     C                   EXSR      DEFCAM
041000131016     C*
041100131016     C                   SETON                                        60
041200131016     C*
041300131016     C* NSP => Stacco un numeratore da AZNUM
041400131016     C                   clear                   TRUL33DS
041500131016     C                   eval      I33OPE = *zeros
041600131016     C                   eval      I33CNU = 302
041700131016     C                   eval      I33NUM = 1
041800131016     C                   movel     TRUL33DS      KPJBU
041900131016     C                   call      'TRUL33R'
042000131016     C                   parm                    KPJBA
042100131016     C                   movel     KPJBU         TRUL33DS
042200131016     C                   if        O33ERR = *zeros
042300131016     C                   z-add     O33NRF        VABNSP
042400131016     C                   z-add     O33NRF        VATNSP
042500131016     C                   else
042600131016     C                   SETON                                        31
042700131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042800160304     C                             + ' ' + 'VABNSP VATNSP'
042900131016     C                   endif
043000131016     C*
043100131016     C                   ENDSR
043200000801     C*----------------------------------------------------*
043300131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043400000801     C*----------------------------------------------------*
043500010201     C     IMPVAB        BEGSR
043600070530     C*
043700070530     C                   SETOFF                                       3132
043800070928     C*
043900070928     C                   EXSR      DEFCAM
044000070928     C*
044100111031     C                   IF        vlrpoi <> 999
044200070928     C                   MOVE(P)   vlrpoi        VABFGS
044300070928     C                   MOVE(P)   vlrpoi        VATFGS
044400111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044500111031     C                   ENDIF
044600070928     C*
044700070928     C                   MOVEL     datcor        VABAAS
044800070928     C                   MOVEL     datcor        VATAAS
044900070928     C                   MOVE      datcor        VABMGS
045000160331     C                   EVAL      VABRSD=%trim(%subst(vindta:51:30))
045100160304     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045200160331     C     '@':'A'       XLATE     VABRSD        VABRSD
045300160304     C* ==
045400160331     C                   EVAL      VABRD2=%trim(%subst(vindta:406:30))
045500160331     C                   EVAL      VABIND=%trim(%subst(vindta:81:30))
045600160331     C                   EVAL      VABLOD=%trim(%subst(vindta:111:30))
045700160331     C                   EVAL      VABPRD=%trim(%subst(vindta:141:2))
045800160331     C                   EVAL      VABRMA=%trim(%subst(vindta:30:10))
045900160331     C                   EVAL      VABNOT=%trim(%subst(vindta:260:30))
046000160331     C                   EVAL      VABNT2=%trim(%subst(vindta:326:30))
046100160331     C                   SELECT
046200160331     C                   WHEN      %subst(vindta:436:1)='B'
046300160331     C                   EVAL      VABTIC='BM'
046400160331     C                   WHEN      %subst(vindta:436:1)='C'
046500160331     C                   EVAL      VABTIC=*blank
046600160331     C                   ENDSL
046700160331     C                   SELECT
046800160331     C                   WHEN      %subst(vindta:445:1)='T'
046900160331     C                   EVAL      VABTC1='A'
047000160331     C                   ENDSL
047100160331     C*
047200160331     C                   IF        %trim(%subst(vindta:293:3)) = '002'
047300160331     C                   EVAL      VABCBO='2'
047400160331     C                   ELSE
047500160331     C                   EVAL      VABCBO='1'
047600160331     C                   ENDIF
047700160331     C*
047800160331     C* Reperimento campi NUMERICI
047900160331     C*
048000160331     C* NSP - RMN
048100160331     C                   EVAL      PiStr=%trim(%subst(vindta:30:10))
048200160331     C                   EXSR      CHKNUM
048300160331     C                   IF        PiInt=*on
048400160331     C                   Z-ADD     PiVal         VABNSP
048500160331     C                   Z-ADD     PiVal         VATNSP
048600160331     C                   Z-ADD     PiVal         VABRMN
048700160331     C                   ELSE
048800160331     C                   SETON                                        31
048900160331     C                   Z-ADD     *zeros        VABNSP
049000160331     C                   Z-ADD     *zeros        VATNSP
049100160331     C                   Z-ADD     *zeros        VABRMN
049200160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300160331     C                             + ' ' + 'VABNSP VATNSP VABRMN'
049400160331     C                   ENDIF
049500160331     C*
049600160331     C* CAD
049700160331     C                   EVAL      PiStr=%trim(%subst(vindta:143:5))
049800160331     C                   EXSR      CHKNUM
049900160331     C                   IF        PiInt=*on
050000160331     C                   Z-ADD     PiVal         Num5_0
050100160331     C                   MOVEL(p)  Num5_0        VABCAD
050200160331     C                   ELSE
050300160331     C                   SETON                                        32
050400160331     C                   EVAL      VABCAD = *zeros
050500160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050600160331     C                             + ' ' + 'VABCAD'
050700160331     C                   ENDIF
050800160331     C*
050900160331     C* NCL
051000160331     C                   EVAL      PiStr=%trim(%subst(vindta:148:5))
051100160331     C                   EXSR      CHKNUM
051200160331     C                   IF        PiInt=*on
051300160331     C                   Z-ADD     PiVal         VABNCL
051400160331     C                   ELSE
051500160331     C                   SETON                                        32
051600160331     C                   Z-ADD     *zeros        VABNCL
051700160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800160331     C                             + ' ' + 'VABNCL'
051900160331     C                   ENDIF
052000160331     C*
052100160331     C* PKB
052200160331     C                   EVAL      PiStr=%trim(%subst(vindta:153:7))
052300160331     C                   EVAL      PiDecChr = ','
052400160331     C                   EXSR      CHKNUM
052500160331     C                   IF        PiNum=*on
052600160331     C                   EVAL      VABPKB = PiVal/100                           * gestisco 2 dec.
052700160331     C                   ELSE
052800160331     C                   SETON                                        32
052900160331     C                   Z-ADD     *zeros        VABPKB
053000160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053100160331     C                             + ' ' + 'VABPKB'
053200160331     C                   ENDIF
053300160331     C*
053400160331     C* IAS
053500160331     C                   EVAL      PiStr=%trim(%subst(vindta:205:9))
053600160331     C                   EVAL      PiDecChr = ','
053700160331     C                   EXSR      CHKNUM
053800160331     C                   IF        PiNum=*on
053900160331     C                   EVAL      VABIAS = PiVal
054000160331     C                   EVAL      VABVAS = 'EUR'
054100160331     C                   ELSE
054200160331     C                   SETON                                        32
054300160331     C                   Z-ADD     *zeros        VABIAS
054400160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500160331     C                             + ' ' + 'VABIAS'
054600160331     C                   ENDIF
054700160331     C*
054800160331     C* CAS
054900160331     C                   IF        %subst(vindta:290:3) = '051'
055000160331     C                   EVAL      FlgCAS = '1'
055100160331     C                   EVAL      PiStr=%trim(%subst(vindta:187:9))
055200160331     C                   EVAL      PiDecChr = ','
055300160331     C                   EXSR      CHKNUM
055400160331     C                   IF        PiNum=*on
055500160331     C                   EVAL      VABCAS = PiVal/100                           * gestisco 2 dec.
055600160331     C                   EVAL      VABVCA = 'EUR'
055700160331     C                   ELSE
055800160331     C                   SETON                                        32
055900160331     C                   Z-ADD     *zeros        VABCAS
056000160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056100160331     C                             + ' ' + 'VABCAS'
056200160331     C                   ENDIF
056300160331     C                   ENDIF
056400160331     C*
056500160331     C* VLB - QFT
056600160331     C                   EVAL      PiStr=%trim(%subst(vindta:253:7))
056700160331     C                   EVAL      PiDecChr = ','
056800160331     C                   EXSR      CHKNUM
056900160331     C                   IF        PiNum=*on
057000160331     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
057100160331     C                   Z-ADD(H)  PiVal         VABVLB
057200160331     C                   Z-ADD(H)  PiVal         VABQFT
057300160331     C                   ELSE
057400160331     C                   SETON                                        32
057500160331     C                   Z-ADD     *zeros        VABVLB
057600160331     C                   Z-ADD     *zeros        VABQFT
057700160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057800160331     C                             + ' ' + 'VABVLB VABQFT'
057900160331     C                   ENDIF
058000160331     C*
058100160331     C* DCM
058200160331     C                   IF        %trim(%subst(vindta:437:8)) <> *zeros AND
058300160331     C                             %trim(%subst(vindta:437:8)) <> *blanks
058400160331     C                   EVAL      PiStr=%trim(%subst(vindta:437:8))
058500160331     C                   EXSR      CHKNUM
058600160331     C                   IF        PiInt=*on
058700160331     C                   Z-ADD     PiVal         VABDCR
058800160331     C                   ELSE
058900160331     C                   SETON                                        32
059000160331     C                   Z-ADD     *zeros        VABDCR
059100160331     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059200160331     C                             + ' ' + 'VABDCR'
059300160331     C                   ENDIF
059400160331     C                   ENDIF
059500070730     C*
059600070730     C* Considerazioni finali su CBO/CAS
059700121113     C                   IF        FlgCAS <> '0'
059800070730     C                   IF        VABCBO = '1'
059900070730     C                   EVAL      VABCBO = '4'
060000070730     C                   ENDIF
060100070730     C                   IF        VABCBO = '2'
060200070730     C                   EVAL      VABCBO = '6'
060300070730     C                   ENDIF
060400070730     C                   ENDIF
060500160304     C*
060600160304     C* Se provincia nn valorizzata la reperisco
060700160304     C* tramite TISI95R a seconda dei dati d instradamento presenti
060800160304     C                   IF        VABPRD =  *blanks AND
060900160304     C                             VABNZD =  *blanks AND
061000160304     C                             VABCAD <> *blanks AND
061100160304     C                             VABLOD <> *blanks
061200160304     C                   CLEAR                   TISI95DS
061300160304     C                   EVAL      I95TCN = '3'
061400160304     C                   Z-ADD     datcor        I95DAT
061500160304     C                   EVAL      I95NAR = VABNZD
061600160304     C                   EVAL      I95CAP = VABCAD
061700160304     C                   EVAL      I95LOC = VABLOD
061800160304     C                   CALL      'TISI95R'
061900160304     C                   PARM                    TISI95DS
062000160304     C                   EVAL      VABPRD = O95PRV
062100160304     C                   ENDIF
062200020305     C*
062300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
062400011113     C                   EXSR      CHKIMPDIV
062500010202     C*
062600000801     C* Ebbene...
062700000801     C                   ADD       1             §CTRMO
062800070530     C                   IF        *in31 <> *zeros OR
062900070530     C                             *in32 <> *zeros
063000000801     C                   ADD       1             §CTRNO
063100020725     C                   EVAL      x_vinflg = '2'
063200000801     C                   ELSE
063300010201     C                   ADD       1             §CTROKVB
063400000801     C                   ENDIF
063500000801     C*
063600000801     C                   ENDSR
063700131016     C*----------------------------------------------------*
063800131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
063900131016     C*----------------------------------------------------*
064000131016     C     WRIVAT_A      BEGSR
064100131016     C*
064200131016     C                   eval      vatAAS = vabAAS
064300131016     C                   eval      vatLNP = vabLNP
064400131016     C                   eval      vatNRS = vabNRS
064500131016     C                   eval      vatNSP = vabNSP
064600131016     C*
064700131016     C* Valorizzo il buffer di scrittura del FIVAT
064800131016     C* - TIPO RECORD "A"
064900160331     C                   if        %trim(%subst(vindta:446:30)) <> *blanks
065000131016     C                   eval      VATTRC = 'A'
065100160331     C                   eval      VATNOT = %trim(%subst(vindta:446:30))
065200131016     C                   write     FIVAT000
065300131016     C                   endif
065400131016     C*
065500131016     C                   ENDSR
065600131016     C*----------------------------------------------------*
065700131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065800131016     C*----------------------------------------------------*
065900131016     C     WRIVAT_B      BEGSR
066000131016     C*
066100131016     C                   eval      vatAAS = vabAAS
066200131016     C                   eval      vatLNP = vabLNP
066300131016     C                   eval      vatNRS = vabNRS
066400131016     C                   eval      vatNSP = vabNSP
066500131016     C*
066600131016     C* Valorizzo il buffer di scrittura del FIVAT
066700131016     C* - TIPO RECORD "B"
066800160331     C                   if        %subst(vindta:476:30) <> *blanks
066900131016     C                   eval      VATTRC = 'B'
067000160331     C                   eval      VATNOT =
067100160331     C                             %scanrpl(' ':'':
067200160331     C                              %trim(%subst(vindta:476:30)))
067300131016     C                   write     FIVAT000
067400131016     C                   endif
067500131016     C*
067600131016     C                   ENDSR
067700131016     C*----------------------------------------------------*
067800131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
067900131016     C*----------------------------------------------------*
068000131016     C     WRIVAT_E      BEGSR
068100131016     C*
068200131016     C                   eval      vatAAS = vabAAS
068300131016     C                   eval      vatLNP = vabLNP
068400131016     C                   eval      vatNRS = vabNRS
068500131016     C                   eval      vatNSP = vabNSP
068600131016     C*
068700131016     C* Valorizzo il buffer di scrittura del FIVAT
068800131016     C* - TIPO RECORD "E"
068900160304     C                   if        %subst(vindta:35:20) <> *blanks
069000131016     C                   eval      VATTRC = 'E'
069100160304     C                   eval      VATNOT = %trim(%subst(vindta:35:20))
069200131016     C                   write     FIVAT000
069300131016     C                   endif
069400131016     C*
069500131016     C                   ENDSR
069600140708     C*----------------------------------------------------*
069700140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
069800140708     C*----------------------------------------------------*
069900140708     C     WRIVAT_S      BEGSR
070000140708     C*
070100140708     C                   eval      vatAAS = vabAAS
070200140708     C                   eval      vatLNP = vabLNP
070300140708     C                   eval      vatNRS = vabNRS
070400140708     C                   eval      vatNSP = vabNSP
070500140708     C*
070600140708     C* Valorizzo il buffer di scrittura del FIVAT
070700140708     C* - TIPO RECORD "B"
070800140708     C                   if        %subst(vindta:113:015) <> *blanks
070900140708     C                   eval      VATTRC = 'S'
071000140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
071100140708     C                   write     FIVAT000
071200140708     C                   endif
071300140708     C*
071400140708     C                   ENDSR
071500020904
071600020904
071700010202     C*----------------------------------------------------*
071800131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
071900010202     C*----------------------------------------------------*
072000020305     C     PREVAT        BEGSR
072100010202     C*
072200131016     C* Compongo il nome del membro da dare al FIVATWWR
072300010202     C                   eval      parmbr = vlrhdl
072400010202     C                   movel     'M'           parmbr
072500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072600010202     C                   eval      paropz = '1'
072700010202     C* Effettuo la chiamata al CLLE preposto
072800131017     C                   call(e)   'TITVVTC'
072900010202     C                   parm                    parccm
073000010202     C                   parm                    parmbr
073100010202     C                   parm                    paropz
073200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073300010202     C                   if        %error
073400010202     C                   movel     '1'           chkcall
073500010202     C                   else
073600010202     C                   movel     '0'           chkcall
073700010202     C                   endif
073800010202     C*
073900010202     C                   ENDSR
074000000801     C*----------------------------------------------------*
074100000801     C*  CONTROLLO NUMERICITA' CAMPI
074200000801     C*----------------------------------------------------*
074300000801     C     CHKNUM        BEGSR
074400081113     C*
074500081113     C                   IF        PiDecChr = *blanks
074600160304     C                   EVAL      PiDecChr = ','
074700081113     C                   ENDIF
074800081113     C*
074900081113     C                   callp(e)  UBISNUM_Check(PiStr
075000081113     C                                          :PiDecChr
075100081113     C                                          :PiVal
075200081113     C                                          :PiNum
075300081113     C                                          :PiInt)
075400081113     C*
075500000801     C                   IF        %error
075600000801     C                   EVAL      PiInt=*off
075700000801     C                   ENDIF
075800000801     C*
075900000801     C                   ENDSR
076000000801     C***
076100000801
076200011113
076300011113     C*----------------------------------------------------*
076400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
076500011113     C*----------------------------------------------------*
076600011113     C     CHKIMPDIV     BEGSR
076700011113     C*
076800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
076900011113     C                   Z-ADD     *zeros        wrkDec            9 9
077000011113     C*
077100011113     C* Come prima cosa effettuo considerazioni sulla divisa
077200011113     C                   IF        vabIAS > *zeros
077300011113     C                   IF        vabVAS <> 'EUR'
077400011113     C                   EVAL      vabVAS =  'ITL'
077500011113     C                   ENDIF
077600011113     C                   ENDIF
077700011113     C*
077800011113     C                   IF        vabCAS > *zeros
077900011113     C                   IF        vabVCA <> 'EUR'
078000011113     C                   EVAL      vabVCA =  'ITL'
078100011113     C                   ENDIF
078200011113     C                   ENDIF
078300011113     C*
078400011113     C                   IF        vabVMD > *zeros
078500020305     C                   IF        vabVAD <> 'EUR'
078600011113     C                   EVAL      vabVAD =  'ITL'
078700011113     C                   ENDIF
078800011113     C                   ENDIF
078900011113     C*
079000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
079100011113     C                   Z-ADD     vabIAS        wrkDec
079200011113     C                   IF        wrkDec > *zeros
079300011113     C                   IF        vabVAS = 'ITL'
079400011113     C                   EVAL      vabIAS = *zeros
079500011113     C                   ENDIF
079600011113     C                   ENDIF
079700011113     C*
079800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
079900011113     C                   Z-ADD     vabCAS        wrkDec
080000011113     C                   IF        wrkDec > *zeros
080100011113     C                   IF        vabVCA = 'ITL'
080200011113     C                   EVAL      vabCAS = *zeros
080300011113     C                   ENDIF
080400011113     C                   ENDIF
080500011113     C*
080600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
080700011113     C                   Z-ADD     vabVMD        wrkDec
080800011113     C                   IF        wrkDec > *zeros
080900011113     C                   IF        vabVAD = 'ITL'
081000011113     C                   EVAL      vabVMD = *zeros
081100011113     C                   ENDIF
081200011113     C                   ENDIF
081300011113     C*
081400011113     C                   ENDSR
081500011113     C***
081600011113
081700011113
081800000801
081900000801
082000990920      /TITLE Invio dei dati al punto operativo.
082100010202     C     invio         BEGSR
082200990920     C*
082300131016     C* 1° invio FIVAT
082400010201     C                   reset                   dscmz
082500111031     C                   move      wPOI          cmzdst
082600131016     C                   eval      cmzfld = 'FIVATWWR'
082700010201     C                   eval      cmzmbd = vlrhdl
082800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082900021009     C***                if        prmfir = *blanks
083000131016     C                   eval      cmzfla = 'FIVAT00F'
083100131016     C                   eval      cmzmba = 'FIVAT00F'
083200021009     C***                else
083300021009     C***                eval      cmzfla = prmfir
083400021009     C***                eval      cmzmba = prmfir
083500021009     C***                endif
083600010201     C                   eval      cmznrr = *zeros
083700020305     C                   move      §ctrokvt      cmznrr
083800021018     C                   eval      cmzlba = vlrfl1
083900010201     C                   call(e)   'TIS711C'
084000010201     C                   parm                    dscmz
084100010201     C                   parm      *blanks       esito
084200010205     C                   if        %error
084300010205     C                             or cmzerr = '1'
084400010205     C                             or esito  = '1'
084500010205     C                   eval      wrkesito = '3'
084600010205     C                   else
084700010201     C*
084800131016     C* 2° invio FIVAB
084900010201     C                   reset                   dscmz
085000111031     C                   move      wPOI          cmzdst
085100010201     C                   eval      cmzfld = vlrfou
085200010201     C                   eval      cmzmbd = vlrhdl
085300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085400021009     C***                if        prmfir = *blanks
085500131016     C                   eval      cmzfla = 'FIVAB00F'
085600131016     C                   eval      cmzmba = 'FIVAB00F'
085700021009     C***                else
085800021009     C***                eval      cmzfla = prmfir
085900021009     C***                eval      cmzmba = prmfir
086000021009     C***                endif
086100010201     C                   eval      cmznrr = *zeros
086200010201     C                   move      §ctrokvb      cmznrr
086300021018     C                   eval      cmzlba = vlrfl1
086400010201     C                   call(e)   'TIS711C'
086500010201     C                   parm                    dscmz
086600010201     C                   parm      *blanks       esito
086700010201     C                   if        %error
086800010201     C                             or cmzerr = '1'
086900010201     C                             or esito  = '1'
087000010201     C                   eval      wrkesito = '3'
087100010201     C                   endif
087200010205     C                   endif
087300990920     C*
087400000613     C                   ENDSR
087500000613     C***
087600070411
087700070411     C     *pssr         BEGSR
087800070411     C*
087900070411     C                   if        %open(tivin00r)
088000070411     C                   close     tivin00r
088100070411     C                   endif
088200131016     C                   if        %open(FIVABWWr)
088300131016     C                   close     FIVABWWr
088400120706     C                   endif
088500131016     C                   if        %open(FIVATWWr)
088600131016     C                   close     FIVATWWr
088700070411     C                   endif
088800070411     C*
088900070411     C* Effettuo la chiamata al CLLE preposto
089000131017     C                   call(e)   'TITVVTC'
089100070411     C                   parm                    parccm
089200070411     C                   parm                    parmbr
089300070411     C                   parm      '2'           paropz
089400070411     C*
089500070411     C                   eval      wrkesito = '2'
089600070411     C*
089700070411     C                   seton                                        LR
089800070411     C*
089900070411     C                   ENDSR     '*CANCL'
090000070411     C***
090100070411
090200990910
090300000613     C     *inzsr        BEGSR
090400990910     C*
090500990910     C     *entry        plist
090600990920     C                   parm                    tivlrds
090700990921     C                   parm      wrkesito      esito
090800000724     C                   parm                    prmlit
090900000710     C                   parm                    prmfir
091000000613     C*
091100000830     C* CALCOLA LA DATA CORRENTE
091200120706     C                   time                    wn14             14 0
091300120706     C                   movel     wn14          oracor            6 0          *ORA
091400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
091500100324     C                   eval      datcor = %dec(%date() : *ISO)
091600000830     C*
091700000613     C                   ENDSR
091800000613     C***
