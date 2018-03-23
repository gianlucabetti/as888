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
002700160304     D depspe          s              6    INZ(*blanks)
002800160304     D curspe          s              6    INZ(*blanks)
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
004400160304     D  VINSPE                        6
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
012700160304     C+ vindta, vinflg, substr(vindta, 3, 6) as sped, rrn(tivin00r)
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
015400160304     C* (uso RMN)
015500160304     C                   EVAL      PiStr=%trim(%subst(vindta:15:5))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300160304     C***                exsr      wrivat_a                                     => carico VAT
016400160304     C***                exsr      wrivat_b                                     => carico VAT
016500160304     C***                exsr      wrivat_s                                     => carico VAT
016600111125     C                   exsr      inzvar
016700071003     C                   else
016800080923     C*
016900071009     C                   if        wDISK = 'D'
017000160304     C                   exsr      repNSP
017100071009     C                   exsr      impvab
017200111130     C  N31              eval      fivabds_ok = fivabds
017300111130     C  N31              exsr      wrivab
017400160304     C***                exsr      wrivat_a                                     => carico VAT
017500160304     C***                exsr      wrivat_b                                     => carico VAT
017600160304     C***                exsr      wrivat_s                                     => carico VAT
017700131016     C                   exsr      wrivat_e                                     => carico VAT
017800111125     C                   exsr      inzvar
017900071009     C                   else
018000071009     C*
018100010305     C                   if        depspe = *blanks                             => 1° giro
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300080117     C                   clear                   tivinds
018400160304     C                   exsr      repNSP
018500020305     C                   exsr      impvab
018600111130     C                   z-add     VABNCL        sav_VABNCL
018700111130     C                   z-add     VABPKB        sav_VABPKB
018800111130     C                   z-add     VABVLB        sav_VABVLB
018900160304     C***                exsr      wrivat_a                                     => carico VAT
019000160304     C***                exsr      wrivat_b                                     => carico VAT
019100160304     C****               exsr      wrivat_s                                     => carico VAT
019200071003     C   50              exsr      wrivat_e                                     => carico VAT
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
020300160304     C                   exsr      repNSP
020400020305     C                   exsr      impvab
020500111130     C                   if        wCntRecSpe = *zeros
020600111130     C                   z-add     VABNCL        sav_VABNCL
020700111130     C                   z-add     VABPKB        sav_VABPKB
020800111130     C                   z-add     VABVLB        sav_VABVLB
020900111130     C                   endif
021000111130     C                   add       1             wCntRecSpe
021100160304     C***                exsr      wrivat_a                                     => carico VAT
021200160304     C***                exsr      wrivat_b                                     => carico VAT
021300160304     C***                exsr      wrivat_s                                     => carico VAT
021400071003     C   50              exsr      wrivat_e                                     => carico VAT
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
023000160304     C***                exsr      wrivat_a                                     => carico VAT
023100160304     C***                exsr      wrivat_b                                     => carico VAT
023200160304     C***                exsr      wrivat_s                                     => carico VAT
023300071003     C   50              exsr      wrivat_e                                     => carico VAT
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
034400160304     C                   EVAL      VABCCM = 0024659
034500160304     C                   EVAL      VATCCM = 0024659
034600160304     C                   EVAL      VABLNP = 002
034700160304     C                   EVAL      VATLNP = 002
034800160304     C                   EVAL      VABCTR = 000
034900160304     C                   EVAL      VABCBO = '1'
035000160304     C                   EVAL      VABCTM = '7Q'
035100070222     C* ... e poi verifico se sono stati passati come parametri
035200070222     C                   IF        vlrppt > *blanks
035300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035500070222     C                   EXSR      CHKNUM
035600070222     C                   IF        PiInt=*on
035700070222     C                   Z-ADD     PiVal         VABCCM
035800070222     C                   Z-ADD     PiVal         VATCCM
035900070222     C                   ENDIF
036000070222     C                   ENDIF
036100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036300070222     C                   EXSR      CHKNUM
036400070222     C                   IF        PiInt=*on
036500070222     C                   Z-ADD     PiVal         VABLNP
036600070222     C                   Z-ADD     PiVal         VATLNP
036700070222     C                   ENDIF
036800070222     C                   ENDIF
036900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037100070222     C                   EXSR      CHKNUM
037200070222     C                   IF        PiInt=*on
037300070222     C                   Z-ADD     PiVal         VABCTR
037400070222     C                   ENDIF
037500070928     C                   ENDIF
037600071009     C                   MOVEL     *blanks       wDISK             1
037700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037800071009     C                   IF        wDISK <> *blanks
037900070928     C                   SETON                                        50
038000070222     C                   ENDIF
038100080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038200111125     C                   SETON                                        41
038300080923     C                   ENDIF
038400070222     C                   ENDIF
038500120706     C*
038600120706     C* valido per nr.colli/peso/volume
038700120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038800120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038900120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039000131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039100120706     C                   SELECT
039200120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039300120706     C                   SETON                                        51
039400120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039500120706     C                   SETON                                        52
039600120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039700120706     C                   SETON                                        53
039800131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039900131016     C                   SETON                                        54
040000120706     C                   ENDSL
040100120706     C*
040200071009     C   50              EVAL      VABCTM = '7Q'
040300000801     C*
040400000801     C                   ENDSR
040500131016     C*----------------------------------------------------*
040600131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040700131016     C*----------------------------------------------------*
040800131016     C     REPNSP        BEGSR
040900131016     C*
041000131016     C                   EXSR      INZVAR
041100131016     C                   EXSR      DEFCAM
041200131016     C*
041300131016     C                   SETON                                        60
041400131016     C*
041500131016     C* NSP => Stacco un numeratore da AZNUM
041600131016     C                   clear                   TRUL33DS
041700131016     C                   eval      I33OPE = *zeros
041800131016     C                   eval      I33CNU = 302
041900131016     C                   eval      I33NUM = 1
042000131016     C                   movel     TRUL33DS      KPJBU
042100131016     C                   call      'TRUL33R'
042200131016     C                   parm                    KPJBA
042300131016     C                   movel     KPJBU         TRUL33DS
042400131016     C                   if        O33ERR = *zeros
042500131016     C                   z-add     O33NRF        VABNSP
042600131016     C                   z-add     O33NRF        VATNSP
042700131016     C                   else
042800131016     C                   SETON                                        31
042900131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043000160304     C                             + ' ' + 'VABNSP VATNSP'
043100131016     C                   endif
043200131016     C*
043300131016     C                   ENDSR
043400000801     C*----------------------------------------------------*
043500131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043600000801     C*----------------------------------------------------*
043700010201     C     IMPVAB        BEGSR
043800070530     C*
043900070530     C                   SETOFF                                       3132
044000070928     C*
044100070928     C                   EXSR      DEFCAM
044200070928     C*
044300111031     C                   IF        vlrpoi <> 999
044400070928     C                   MOVE(P)   vlrpoi        VABFGS
044500070928     C                   MOVE(P)   vlrpoi        VATFGS
044600111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044700111031     C                   ENDIF
044800070928     C*
044900070928     C                   MOVEL     datcor        VABAAS
045000070928     C                   MOVEL     datcor        VATAAS
045100070928     C                   MOVE      datcor        VABMGS
045200160304     C                   EVAL      w70   =%trim(%subst(vindta:79:40))
045300160304     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045400160304     C     '@':'A'       XLATE     w70           w70
045500160304     C* ==
045600160304     C                   EVAL      VABRSD=%subst(w70:1:35)
045700160304     C                   EVAL      VABRD2=%subst(w70:36:5)
045800160304     C                   EVAL      VABIND=%trim(%subst(vindta:119:30))
045900160304     C                   EVAL      VABLOD=%trim(%subst(vindta:154:30))
046000160304     C                   EVAL      VABPRD=%trim(%subst(vindta:184:2))
046100160304     C                   EVAL      VABRMA=%trim(%subst(vindta:20:6))
046200160304     C                   IF        %trim(%subst(vindta:186:3)) ='T11'
046300160304     C                   EVAL      VABCBO='2'
046400160304     C                   ENDIF
046500160304     C                   EVAL      w70   =%trim(%subst(vindta:189:40))
046600160304     C                   EVAL      VABNOT=%subst(w70:1:35)
046700160304     C                   EVAL      VABNT2=%subst(w70:36:5)
046800160304     C*
046900160304     C* CAP
047000160304     C                   EVAL      PiStr=%trim(%subst(vindta:149:5))
047100160304     C                   EXSR      CHKNUM
047200160304     C                   IF        PiInt=*on
047300160304     C                   Z-ADD     PiVal         Num5_0
047400160304     C                   MOVEL(P)  Num5_0        VABCAD
047500160304     C                   ELSE
047600160304     C                   SETON                                        32
047700160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047800160304     C                             + ' ' + 'VABCAD'
047900160304     C                   ENDIF
048000160304     C* RMN
048100160304     C                   EVAL      PiStr=%trim(%subst(vindta:15:5))
048200160304     C                   EXSR      CHKNUM
048300160304     C                   IF        PiInt=*on
048400160304     C                   Z-ADD     PiVal         VABRMN
048500160304     C                   ELSE
048600160304     C                   SETON                                        32
048700160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800160304     C                             + ' ' + 'VABRMN'
048900160304     C                   ENDIF
049000160304     C* NCL
049100160304     C                   EVAL      PiStr=%trim(%subst(vindta:64:5))
049200160304     C                   EXSR      CHKNUM
049300160304     C                   IF        PiInt=*on
049400160304     C                   Z-ADD     PiVal         VABNCL
049500160304     C                   ELSE
049600160304     C                   SETON                                        32
049700160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800160304     C                             + ' ' + 'VABNCL'
049900160304     C                   ENDIF
050000160304     C* PKB
050100160304     C                   EVAL      PiStr=%trim(%subst(vindta:55:9))
050200160304     C                   EXSR      CHKNUM
050300160304     C                   IF        PiNum=*on
050400160304     C* il dato ha 3 decimali senza separatore
050500160304     C                   EVAL(H)   VABPKB = PiVal/1000
050600160304     C                   ELSE
050700160304     C                   SETON                                        32
050800160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900160304     C                             + ' ' + 'VABPKB'
051000160304     C                   ENDIF
051100160304     C* CAS
051200160304     C                   IF        %trim(%subst(vindta:69:10)) <> *blanks AND
051300160304     C                             %trim(%subst(vindta:69:10)) <> *zeros
051400160304     C                   MOVEL     '1'           FlgCAS
051500160304     C                   EVAL      VABVCA = 'EUR'
051600160304     C                   EVAL      PiStr=%trim(%subst(vindta:69:10))
051700160304     C                   EXSR      CHKNUM
051800160304     C                   IF        PiNum=*on
051900160304     C                   EVAL      PiVal = PiVal
052000160304     C                   Z-ADD     PiVal         VABCAS
052100160304     C                   ELSE
052200160304     C                   SETON                                        32
052300160304     C                   Z-ADD     *zeros        VABCAS
052400160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500160304     C                             + ' ' + 'VABCAS'
052600160304     C                   ENDIF
052700160304     C                   ENDIF
052800070730     C*
052900070730     C* Considerazioni finali su CBO/CAS
053000121113     C                   IF        FlgCAS <> '0'
053100070730     C                   IF        VABCBO = '1'
053200070730     C                   EVAL      VABCBO = '4'
053300070730     C                   ENDIF
053400070730     C                   IF        VABCBO = '2'
053500070730     C                   EVAL      VABCBO = '6'
053600070730     C                   ENDIF
053700070730     C                   ENDIF
053800160304     C*
053900160304     C* Se provincia nn valorizzata la reperisco
054000160304     C* tramite TISI95R a seconda dei dati d instradamento presenti
054100160304     C                   IF        VABPRD =  *blanks AND
054200160304     C                             VABNZD =  *blanks AND
054300160304     C                             VABCAD <> *blanks AND
054400160304     C                             VABLOD <> *blanks
054500160304     C                   CLEAR                   TISI95DS
054600160304     C                   EVAL      I95TCN = '3'
054700160304     C                   Z-ADD     datcor        I95DAT
054800160304     C                   EVAL      I95NAR = VABNZD
054900160304     C                   EVAL      I95CAP = VABCAD
055000160304     C                   EVAL      I95LOC = VABLOD
055100160304     C                   CALL      'TISI95R'
055200160304     C                   PARM                    TISI95DS
055300160304     C                   EVAL      VABPRD = O95PRV
055400160304     C                   ENDIF
055500020305     C*
055600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055700011113     C                   EXSR      CHKIMPDIV
055800010202     C*
055900000801     C* Ebbene...
056000000801     C                   ADD       1             §CTRMO
056100070530     C                   IF        *in31 <> *zeros OR
056200070530     C                             *in32 <> *zeros
056300000801     C                   ADD       1             §CTRNO
056400020725     C                   EVAL      x_vinflg = '2'
056500000801     C                   ELSE
056600010201     C                   ADD       1             §CTROKVB
056700000801     C                   ENDIF
056800000801     C*
056900000801     C                   ENDSR
057000131016     C*----------------------------------------------------*
057100131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057200131016     C*----------------------------------------------------*
057300131016     C     WRIVAT_A      BEGSR
057400131016     C*
057500131016     C                   eval      vatAAS = vabAAS
057600131016     C                   eval      vatLNP = vabLNP
057700131016     C                   eval      vatNRS = vabNRS
057800131016     C                   eval      vatNSP = vabNSP
057900131016     C*
058000131016     C* Valorizzo il buffer di scrittura del FIVAT
058100131016     C* - TIPO RECORD "A"
058200131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
058300131016     C                   eval      VATTRC = 'A'
058400131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
058500131016     C                   write     FIVAT000
058600131016     C                   endif
058700131016     C*
058800131016     C                   ENDSR
058900131016     C*----------------------------------------------------*
059000131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059100131016     C*----------------------------------------------------*
059200131016     C     WRIVAT_B      BEGSR
059300131016     C*
059400131016     C                   eval      vatAAS = vabAAS
059500131016     C                   eval      vatLNP = vabLNP
059600131016     C                   eval      vatNRS = vabNRS
059700131016     C                   eval      vatNSP = vabNSP
059800131016     C*
059900131016     C* Valorizzo il buffer di scrittura del FIVAT
060000131016     C* - TIPO RECORD "B"
060100140708     C                   if        %subst(vindta:113:015) <> *blanks
060200131016     C                   eval      VATTRC = 'B'
060300140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
060400131016     C                   write     FIVAT000
060500131016     C                   endif
060600131016     C*
060700131016     C                   ENDSR
060800131016     C*----------------------------------------------------*
060900131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
061000131016     C*----------------------------------------------------*
061100131016     C     WRIVAT_E      BEGSR
061200131016     C*
061300131016     C                   eval      vatAAS = vabAAS
061400131016     C                   eval      vatLNP = vabLNP
061500131016     C                   eval      vatNRS = vabNRS
061600131016     C                   eval      vatNSP = vabNSP
061700131016     C*
061800131016     C* Valorizzo il buffer di scrittura del FIVAT
061900131016     C* - TIPO RECORD "E"
062000160304     C                   if        %subst(vindta:35:20) <> *blanks
062100131016     C                   eval      VATTRC = 'E'
062200160304     C                   eval      VATNOT = %trim(%subst(vindta:35:20))
062300131016     C                   write     FIVAT000
062400131016     C                   endif
062500131016     C*
062600131016     C                   ENDSR
062700140708     C*----------------------------------------------------*
062800140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
062900140708     C*----------------------------------------------------*
063000140708     C     WRIVAT_S      BEGSR
063100140708     C*
063200140708     C                   eval      vatAAS = vabAAS
063300140708     C                   eval      vatLNP = vabLNP
063400140708     C                   eval      vatNRS = vabNRS
063500140708     C                   eval      vatNSP = vabNSP
063600140708     C*
063700140708     C* Valorizzo il buffer di scrittura del FIVAT
063800140708     C* - TIPO RECORD "B"
063900140708     C                   if        %subst(vindta:113:015) <> *blanks
064000140708     C                   eval      VATTRC = 'S'
064100140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
064200140708     C                   write     FIVAT000
064300140708     C                   endif
064400140708     C*
064500140708     C                   ENDSR
064600020904
064700020904
064800010202     C*----------------------------------------------------*
064900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065000010202     C*----------------------------------------------------*
065100020305     C     PREVAT        BEGSR
065200010202     C*
065300131016     C* Compongo il nome del membro da dare al FIVATWWR
065400010202     C                   eval      parmbr = vlrhdl
065500010202     C                   movel     'M'           parmbr
065600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065700010202     C                   eval      paropz = '1'
065800010202     C* Effettuo la chiamata al CLLE preposto
065900131017     C                   call(e)   'TITVVTC'
066000010202     C                   parm                    parccm
066100010202     C                   parm                    parmbr
066200010202     C                   parm                    paropz
066300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066400010202     C                   if        %error
066500010202     C                   movel     '1'           chkcall
066600010202     C                   else
066700010202     C                   movel     '0'           chkcall
066800010202     C                   endif
066900010202     C*
067000010202     C                   ENDSR
067100000801     C*----------------------------------------------------*
067200000801     C*  CONTROLLO NUMERICITA' CAMPI
067300000801     C*----------------------------------------------------*
067400000801     C     CHKNUM        BEGSR
067500081113     C*
067600081113     C                   IF        PiDecChr = *blanks
067700160304     C                   EVAL      PiDecChr = ','
067800081113     C                   ENDIF
067900081113     C*
068000081113     C                   callp(e)  UBISNUM_Check(PiStr
068100081113     C                                          :PiDecChr
068200081113     C                                          :PiVal
068300081113     C                                          :PiNum
068400081113     C                                          :PiInt)
068500081113     C*
068600000801     C                   IF        %error
068700000801     C                   EVAL      PiInt=*off
068800000801     C                   ENDIF
068900000801     C*
069000000801     C                   ENDSR
069100000801     C***
069200000801
069300011113
069400011113     C*----------------------------------------------------*
069500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069600011113     C*----------------------------------------------------*
069700011113     C     CHKIMPDIV     BEGSR
069800011113     C*
069900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070000011113     C                   Z-ADD     *zeros        wrkDec            9 9
070100011113     C*
070200011113     C* Come prima cosa effettuo considerazioni sulla divisa
070300011113     C                   IF        vabIAS > *zeros
070400011113     C                   IF        vabVAS <> 'EUR'
070500011113     C                   EVAL      vabVAS =  'ITL'
070600011113     C                   ENDIF
070700011113     C                   ENDIF
070800011113     C*
070900011113     C                   IF        vabCAS > *zeros
071000011113     C                   IF        vabVCA <> 'EUR'
071100011113     C                   EVAL      vabVCA =  'ITL'
071200011113     C                   ENDIF
071300011113     C                   ENDIF
071400011113     C*
071500011113     C                   IF        vabVMD > *zeros
071600020305     C                   IF        vabVAD <> 'EUR'
071700011113     C                   EVAL      vabVAD =  'ITL'
071800011113     C                   ENDIF
071900011113     C                   ENDIF
072000011113     C*
072100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072200011113     C                   Z-ADD     vabIAS        wrkDec
072300011113     C                   IF        wrkDec > *zeros
072400011113     C                   IF        vabVAS = 'ITL'
072500011113     C                   EVAL      vabIAS = *zeros
072600011113     C                   ENDIF
072700011113     C                   ENDIF
072800011113     C*
072900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073000011113     C                   Z-ADD     vabCAS        wrkDec
073100011113     C                   IF        wrkDec > *zeros
073200011113     C                   IF        vabVCA = 'ITL'
073300011113     C                   EVAL      vabCAS = *zeros
073400011113     C                   ENDIF
073500011113     C                   ENDIF
073600011113     C*
073700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073800011113     C                   Z-ADD     vabVMD        wrkDec
073900011113     C                   IF        wrkDec > *zeros
074000011113     C                   IF        vabVAD = 'ITL'
074100011113     C                   EVAL      vabVMD = *zeros
074200011113     C                   ENDIF
074300011113     C                   ENDIF
074400011113     C*
074500011113     C                   ENDSR
074600011113     C***
074700011113
074800011113
074900000801
075000000801
075100990920      /TITLE Invio dei dati al punto operativo.
075200010202     C     invio         BEGSR
075300990920     C*
075400131016     C* 1° invio FIVAT
075500010201     C                   reset                   dscmz
075600111031     C                   move      wPOI          cmzdst
075700131016     C                   eval      cmzfld = 'FIVATWWR'
075800010201     C                   eval      cmzmbd = vlrhdl
075900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076000021009     C***                if        prmfir = *blanks
076100131016     C                   eval      cmzfla = 'FIVAT00F'
076200131016     C                   eval      cmzmba = 'FIVAT00F'
076300021009     C***                else
076400021009     C***                eval      cmzfla = prmfir
076500021009     C***                eval      cmzmba = prmfir
076600021009     C***                endif
076700010201     C                   eval      cmznrr = *zeros
076800020305     C                   move      §ctrokvt      cmznrr
076900021018     C                   eval      cmzlba = vlrfl1
077000010201     C                   call(e)   'TIS711C'
077100010201     C                   parm                    dscmz
077200010201     C                   parm      *blanks       esito
077300010205     C                   if        %error
077400010205     C                             or cmzerr = '1'
077500010205     C                             or esito  = '1'
077600010205     C                   eval      wrkesito = '3'
077700010205     C                   else
077800010201     C*
077900131016     C* 2° invio FIVAB
078000010201     C                   reset                   dscmz
078100111031     C                   move      wPOI          cmzdst
078200010201     C                   eval      cmzfld = vlrfou
078300010201     C                   eval      cmzmbd = vlrhdl
078400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078500021009     C***                if        prmfir = *blanks
078600131016     C                   eval      cmzfla = 'FIVAB00F'
078700131016     C                   eval      cmzmba = 'FIVAB00F'
078800021009     C***                else
078900021009     C***                eval      cmzfla = prmfir
079000021009     C***                eval      cmzmba = prmfir
079100021009     C***                endif
079200010201     C                   eval      cmznrr = *zeros
079300010201     C                   move      §ctrokvb      cmznrr
079400021018     C                   eval      cmzlba = vlrfl1
079500010201     C                   call(e)   'TIS711C'
079600010201     C                   parm                    dscmz
079700010201     C                   parm      *blanks       esito
079800010201     C                   if        %error
079900010201     C                             or cmzerr = '1'
080000010201     C                             or esito  = '1'
080100010201     C                   eval      wrkesito = '3'
080200010201     C                   endif
080300010205     C                   endif
080400990920     C*
080500000613     C                   ENDSR
080600000613     C***
080700070411
080800070411     C     *pssr         BEGSR
080900070411     C*
081000070411     C                   if        %open(tivin00r)
081100070411     C                   close     tivin00r
081200070411     C                   endif
081300131016     C                   if        %open(FIVABWWr)
081400131016     C                   close     FIVABWWr
081500120706     C                   endif
081600131016     C                   if        %open(FIVATWWr)
081700131016     C                   close     FIVATWWr
081800070411     C                   endif
081900070411     C*
082000070411     C* Effettuo la chiamata al CLLE preposto
082100131017     C                   call(e)   'TITVVTC'
082200070411     C                   parm                    parccm
082300070411     C                   parm                    parmbr
082400070411     C                   parm      '2'           paropz
082500070411     C*
082600070411     C                   eval      wrkesito = '2'
082700070411     C*
082800070411     C                   seton                                        LR
082900070411     C*
083000070411     C                   ENDSR     '*CANCL'
083100070411     C***
083200070411
083300990910
083400000613     C     *inzsr        BEGSR
083500990910     C*
083600990910     C     *entry        plist
083700990920     C                   parm                    tivlrds
083800990921     C                   parm      wrkesito      esito
083900000724     C                   parm                    prmlit
084000000710     C                   parm                    prmfir
084100000613     C*
084200000830     C* CALCOLA LA DATA CORRENTE
084300120706     C                   time                    wn14             14 0
084400120706     C                   movel     wn14          oracor            6 0          *ORA
084500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084600100324     C                   eval      datcor = %dec(%date() : *ISO)
084700000830     C*
084800000613     C                   ENDSR
084900000613     C***
