000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400160304      * vanno sommati solo i colli
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
015400160304     C* (uso RMN/NSP)
015500160304     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
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
017000160304     C***                exsr      repNSP
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
018400160304     C***                exsr      repNSP
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
020300160304     C***                exsr      repNSP
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
034400160304     C                   EVAL      VABCCM = 0204070
034500160304     C                   EVAL      VATCCM = 0204070
034600160304     C                   EVAL      VABLNP = 020
034700160304     C                   EVAL      VATLNP = 020
034800160304     C                   EVAL      VABCTR = 001
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
042700131016     C                   z-add     O33NRF        VABRMN
042800131016     C                   else
042900131016     C                   SETON                                        31
043000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
043200131016     C                   endif
043300131016     C*
043400131016     C                   ENDSR
043500000801     C*----------------------------------------------------*
043600131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043700000801     C*----------------------------------------------------*
043800010201     C     IMPVAB        BEGSR
043900070530     C*
044000070530     C                   SETOFF                                       3132
044100070928     C*
044200070928     C                   EXSR      DEFCAM
044300070928     C*
044400111031     C                   IF        vlrpoi <> 999
044500070928     C                   MOVE(P)   vlrpoi        VABFGS
044600070928     C                   MOVE(P)   vlrpoi        VATFGS
044700111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044800111031     C                   ENDIF
044900070928     C*
045000070928     C                   MOVEL     datcor        VABAAS
045100070928     C                   MOVEL     datcor        VATAAS
045200070928     C                   MOVE      datcor        VABMGS
045300160304     C                   EVAL      VABRSD=%trim(%subst(vindta:42:30))
045400160304     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045500160304     C     '@':'A'       XLATE     VABRSD        VABRSD
045600160304     C* ==
045700160304     C                   EVAL      VABIND=%trim(%subst(vindta:72:30))
045800160304     C                   EVAL      VABLOD=%trim(%subst(vindta:132:30))
045900160304     C                   EVAL      VABPRD=%trim(%subst(vindta:1:2))
046000160304     C                   EVAL      VABRMA=%trim(%subst(vindta:205:12))
046100160304     C                   IF        %scan('assegnato':%subst(vindta:9:15)) > 0
046200160304     C                   EVAL      VABCBO='2'
046300160304     C                   ENDIF
046400160304     C*
046500160304     C* CAP
046600160304     C                   EVAL      PiStr=%trim(%subst(vindta:102:30))
046700160304     C                   EXSR      CHKNUM
046800160304     C                   IF        PiInt=*on
046900160304     C                   Z-ADD     PiVal         Num5_0
047000160304     C                   MOVEL(P)  Num5_0        VABCAD
047100160304     C                   ELSE
047200160304     C                   SETON                                        32
047300160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400160304     C                             + ' ' + 'VABCAD'
047500160304     C                   ENDIF
047600160304     C* NSP/RMN
047700160304     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
047800160304     C                   EXSR      CHKNUM
047900160304     C                   IF        PiInt=*on
048000160304     C                   Z-ADD     PiVal         VABRMN
048100160304     C                   Z-ADD     PiVal         VABNSP
048200160304     C                   Z-ADD     PiVal         VATNSP
048300160304     C                   ELSE
048400160304     C                   SETON                                        32
048500160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048600160304     C                             + ' ' + 'VABRMN VABNSP VATNSP'
048700160304     C                   ENDIF
048800160304     C* NCL
048900160304     C                   EVAL      PiStr=%trim(%subst(vindta:170:6))
049000160304     C                   EXSR      CHKNUM
049100160304     C                   IF        PiInt=*on
049200160304     C                   Z-ADD     PiVal         VABNCL
049300160304     C                   ELSE
049400160304     C                   SETON                                        32
049500160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600160304     C                             + ' ' + 'VABNCL'
049700160304     C                   ENDIF
049800160304     C* PKB
049900160304     C                   EVAL      PiStr=%trim(%subst(vindta:176:8))
050000160304     C                   EXSR      CHKNUM
050100160304     C                   IF        PiNum=*on
050200160304     C* il dato ha 1 decimali senza separatore
050300160304     C                   EVAL(H)   VABPKB = PiVal/100
050400160304     C                   ELSE
050500160304     C                   SETON                                        32
050600160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700160304     C                             + ' ' + 'VABPKB'
050800160304     C                   ENDIF
050900160304     C* VLB
051000160304     C                   EVAL      PiStr=%trim(%subst(vindta:162:8))
051100160304     C                   EXSR      CHKNUM
051200160304     C                   IF        PiNum=*on
051300160304     C* il dato ha 2 decimali senza separatore
051400160304     C                   EVAL(H)   VABVLB = PiVal/100000
051500160304     C                   ELSE
051600160304     C                   SETON                                        32
051700160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800160304     C                             + ' ' + 'VABVLB'
051900160304     C                   ENDIF
052000160304     C* CAS
052100160304     C                   IF        %trim(%subst(vindta:184:9)) <> *blanks AND
052200160304     C                             %trim(%subst(vindta:184:9)) <> *zeros  AND
052300160304     C                             %trim(%subst(vindta:184:9)) <> '000000,00'
052400160304     C                   MOVEL     '1'           FlgCAS
052500160304     C                   EVAL      VABVCA = 'EUR'
052600160304     C                   EVAL      PiStr=%trim(%subst(vindta:184:9))
052700160304     C                   EXSR      CHKNUM
052800160304     C                   IF        PiNum=*on
052900160304     C                   EVAL      PiVal = PiVal
053000160304     C                   Z-ADD     PiVal         VABCAS
053100160304     C                   ELSE
053200160304     C                   SETON                                        32
053300160304     C                   Z-ADD     *zeros        VABCAS
053400160304     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053500160304     C                             + ' ' + 'VABCAS'
053600160304     C                   ENDIF
053700160304     C                   ENDIF
053800070730     C*
053900070730     C* Considerazioni finali su CBO/CAS
054000121113     C                   IF        FlgCAS <> '0'
054100070730     C                   IF        VABCBO = '1'
054200070730     C                   EVAL      VABCBO = '4'
054300070730     C                   ENDIF
054400070730     C                   IF        VABCBO = '2'
054500070730     C                   EVAL      VABCBO = '6'
054600070730     C                   ENDIF
054700070730     C                   ENDIF
054800160304     C*
054900160304     C* Se provincia nn valorizzata la reperisco
055000160304     C* tramite TISI95R a seconda dei dati d instradamento presenti
055100160304     C                   IF        VABPRD =  *blanks AND
055200160304     C                             VABNZD =  *blanks AND
055300160304     C                             VABCAD <> *blanks AND
055400160304     C                             VABLOD <> *blanks
055500160304     C                   CLEAR                   TISI95DS
055600160304     C                   EVAL      I95TCN = '3'
055700160304     C                   Z-ADD     datcor        I95DAT
055800160304     C                   EVAL      I95NAR = VABNZD
055900160304     C                   EVAL      I95CAP = VABCAD
056000160304     C                   EVAL      I95LOC = VABLOD
056100160304     C                   CALL      'TISI95R'
056200160304     C                   PARM                    TISI95DS
056300160304     C                   EVAL      VABPRD = O95PRV
056400160304     C                   ENDIF
056500020305     C*
056600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
056700011113     C                   EXSR      CHKIMPDIV
056800010202     C*
056900000801     C* Ebbene...
057000000801     C                   ADD       1             §CTRMO
057100070530     C                   IF        *in31 <> *zeros OR
057200070530     C                             *in32 <> *zeros
057300000801     C                   ADD       1             §CTRNO
057400020725     C                   EVAL      x_vinflg = '2'
057500000801     C                   ELSE
057600010201     C                   ADD       1             §CTROKVB
057700000801     C                   ENDIF
057800000801     C*
057900000801     C                   ENDSR
058000131016     C*----------------------------------------------------*
058100131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
058200131016     C*----------------------------------------------------*
058300131016     C     WRIVAT_A      BEGSR
058400131016     C*
058500131016     C                   eval      vatAAS = vabAAS
058600131016     C                   eval      vatLNP = vabLNP
058700131016     C                   eval      vatNRS = vabNRS
058800131016     C                   eval      vatNSP = vabNSP
058900131016     C*
059000131016     C* Valorizzo il buffer di scrittura del FIVAT
059100131016     C* - TIPO RECORD "A"
059200131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
059300131016     C                   eval      VATTRC = 'A'
059400131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
059500131016     C                   write     FIVAT000
059600131016     C                   endif
059700131016     C*
059800131016     C                   ENDSR
059900131016     C*----------------------------------------------------*
060000131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060100131016     C*----------------------------------------------------*
060200131016     C     WRIVAT_B      BEGSR
060300131016     C*
060400131016     C                   eval      vatAAS = vabAAS
060500131016     C                   eval      vatLNP = vabLNP
060600131016     C                   eval      vatNRS = vabNRS
060700131016     C                   eval      vatNSP = vabNSP
060800131016     C*
060900131016     C* Valorizzo il buffer di scrittura del FIVAT
061000131016     C* - TIPO RECORD "B"
061100140708     C                   if        %subst(vindta:113:015) <> *blanks
061200131016     C                   eval      VATTRC = 'B'
061300140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
061400131016     C                   write     FIVAT000
061500131016     C                   endif
061600131016     C*
061700131016     C                   ENDSR
061800131016     C*----------------------------------------------------*
061900131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062000131016     C*----------------------------------------------------*
062100131016     C     WRIVAT_E      BEGSR
062200131016     C*
062300131016     C                   eval      vatAAS = vabAAS
062400131016     C                   eval      vatLNP = vabLNP
062500131016     C                   eval      vatNRS = vabNRS
062600131016     C                   eval      vatNSP = vabNSP
062700131016     C*
062800131016     C* Valorizzo il buffer di scrittura del FIVAT
062900131016     C* - TIPO RECORD "E"
063000160304     C                   if        %subst(vindta:193:12) <> *blanks
063100131016     C                   eval      VATTRC = 'E'
063200160304     C                   eval      VATNOT = %trim(%subst(vindta:193:12))
063300131016     C                   write     FIVAT000
063400131016     C                   endif
063500131016     C*
063600131016     C                   ENDSR
063700140708     C*----------------------------------------------------*
063800140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
063900140708     C*----------------------------------------------------*
064000140708     C     WRIVAT_S      BEGSR
064100140708     C*
064200140708     C                   eval      vatAAS = vabAAS
064300140708     C                   eval      vatLNP = vabLNP
064400140708     C                   eval      vatNRS = vabNRS
064500140708     C                   eval      vatNSP = vabNSP
064600140708     C*
064700140708     C* Valorizzo il buffer di scrittura del FIVAT
064800140708     C* - TIPO RECORD "B"
064900140708     C                   if        %subst(vindta:113:015) <> *blanks
065000140708     C                   eval      VATTRC = 'S'
065100140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
065200140708     C                   write     FIVAT000
065300140708     C                   endif
065400140708     C*
065500140708     C                   ENDSR
065600020904
065700020904
065800010202     C*----------------------------------------------------*
065900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
066000010202     C*----------------------------------------------------*
066100020305     C     PREVAT        BEGSR
066200010202     C*
066300131016     C* Compongo il nome del membro da dare al FIVATWWR
066400010202     C                   eval      parmbr = vlrhdl
066500010202     C                   movel     'M'           parmbr
066600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066700010202     C                   eval      paropz = '1'
066800010202     C* Effettuo la chiamata al CLLE preposto
066900131017     C                   call(e)   'TITVVTC'
067000010202     C                   parm                    parccm
067100010202     C                   parm                    parmbr
067200010202     C                   parm                    paropz
067300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067400010202     C                   if        %error
067500010202     C                   movel     '1'           chkcall
067600010202     C                   else
067700010202     C                   movel     '0'           chkcall
067800010202     C                   endif
067900010202     C*
068000010202     C                   ENDSR
068100000801     C*----------------------------------------------------*
068200000801     C*  CONTROLLO NUMERICITA' CAMPI
068300000801     C*----------------------------------------------------*
068400000801     C     CHKNUM        BEGSR
068500081113     C*
068600081113     C                   IF        PiDecChr = *blanks
068700160304     C                   EVAL      PiDecChr = ','
068800081113     C                   ENDIF
068900081113     C*
069000081113     C                   callp(e)  UBISNUM_Check(PiStr
069100081113     C                                          :PiDecChr
069200081113     C                                          :PiVal
069300081113     C                                          :PiNum
069400081113     C                                          :PiInt)
069500081113     C*
069600000801     C                   IF        %error
069700000801     C                   EVAL      PiInt=*off
069800000801     C                   ENDIF
069900000801     C*
070000000801     C                   ENDSR
070100000801     C***
070200000801
070300011113
070400011113     C*----------------------------------------------------*
070500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070600011113     C*----------------------------------------------------*
070700011113     C     CHKIMPDIV     BEGSR
070800011113     C*
070900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
071000011113     C                   Z-ADD     *zeros        wrkDec            9 9
071100011113     C*
071200011113     C* Come prima cosa effettuo considerazioni sulla divisa
071300011113     C                   IF        vabIAS > *zeros
071400011113     C                   IF        vabVAS <> 'EUR'
071500011113     C                   EVAL      vabVAS =  'ITL'
071600011113     C                   ENDIF
071700011113     C                   ENDIF
071800011113     C*
071900011113     C                   IF        vabCAS > *zeros
072000011113     C                   IF        vabVCA <> 'EUR'
072100011113     C                   EVAL      vabVCA =  'ITL'
072200011113     C                   ENDIF
072300011113     C                   ENDIF
072400011113     C*
072500011113     C                   IF        vabVMD > *zeros
072600020305     C                   IF        vabVAD <> 'EUR'
072700011113     C                   EVAL      vabVAD =  'ITL'
072800011113     C                   ENDIF
072900011113     C                   ENDIF
073000011113     C*
073100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
073200011113     C                   Z-ADD     vabIAS        wrkDec
073300011113     C                   IF        wrkDec > *zeros
073400011113     C                   IF        vabVAS = 'ITL'
073500011113     C                   EVAL      vabIAS = *zeros
073600011113     C                   ENDIF
073700011113     C                   ENDIF
073800011113     C*
073900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
074000011113     C                   Z-ADD     vabCAS        wrkDec
074100011113     C                   IF        wrkDec > *zeros
074200011113     C                   IF        vabVCA = 'ITL'
074300011113     C                   EVAL      vabCAS = *zeros
074400011113     C                   ENDIF
074500011113     C                   ENDIF
074600011113     C*
074700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074800011113     C                   Z-ADD     vabVMD        wrkDec
074900011113     C                   IF        wrkDec > *zeros
075000011113     C                   IF        vabVAD = 'ITL'
075100011113     C                   EVAL      vabVMD = *zeros
075200011113     C                   ENDIF
075300011113     C                   ENDIF
075400011113     C*
075500011113     C                   ENDSR
075600011113     C***
075700011113
075800011113
075900000801
076000000801
076100990920      /TITLE Invio dei dati al punto operativo.
076200010202     C     invio         BEGSR
076300990920     C*
076400131016     C* 1° invio FIVAT
076500010201     C                   reset                   dscmz
076600111031     C                   move      wPOI          cmzdst
076700131016     C                   eval      cmzfld = 'FIVATWWR'
076800010201     C                   eval      cmzmbd = vlrhdl
076900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077000021009     C***                if        prmfir = *blanks
077100131016     C                   eval      cmzfla = 'FIVAT00F'
077200131016     C                   eval      cmzmba = 'FIVAT00F'
077300021009     C***                else
077400021009     C***                eval      cmzfla = prmfir
077500021009     C***                eval      cmzmba = prmfir
077600021009     C***                endif
077700010201     C                   eval      cmznrr = *zeros
077800020305     C                   move      §ctrokvt      cmznrr
077900021018     C                   eval      cmzlba = vlrfl1
078000010201     C                   call(e)   'TIS711C'
078100010201     C                   parm                    dscmz
078200010201     C                   parm      *blanks       esito
078300010205     C                   if        %error
078400010205     C                             or cmzerr = '1'
078500010205     C                             or esito  = '1'
078600010205     C                   eval      wrkesito = '3'
078700010205     C                   else
078800010201     C*
078900131016     C* 2° invio FIVAB
079000010201     C                   reset                   dscmz
079100111031     C                   move      wPOI          cmzdst
079200010201     C                   eval      cmzfld = vlrfou
079300010201     C                   eval      cmzmbd = vlrhdl
079400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079500021009     C***                if        prmfir = *blanks
079600131016     C                   eval      cmzfla = 'FIVAB00F'
079700131016     C                   eval      cmzmba = 'FIVAB00F'
079800021009     C***                else
079900021009     C***                eval      cmzfla = prmfir
080000021009     C***                eval      cmzmba = prmfir
080100021009     C***                endif
080200010201     C                   eval      cmznrr = *zeros
080300010201     C                   move      §ctrokvb      cmznrr
080400021018     C                   eval      cmzlba = vlrfl1
080500010201     C                   call(e)   'TIS711C'
080600010201     C                   parm                    dscmz
080700010201     C                   parm      *blanks       esito
080800010201     C                   if        %error
080900010201     C                             or cmzerr = '1'
081000010201     C                             or esito  = '1'
081100010201     C                   eval      wrkesito = '3'
081200010201     C                   endif
081300010205     C                   endif
081400990920     C*
081500000613     C                   ENDSR
081600000613     C***
081700070411
081800070411     C     *pssr         BEGSR
081900070411     C*
082000070411     C                   if        %open(tivin00r)
082100070411     C                   close     tivin00r
082200070411     C                   endif
082300131016     C                   if        %open(FIVABWWr)
082400131016     C                   close     FIVABWWr
082500120706     C                   endif
082600131016     C                   if        %open(FIVATWWr)
082700131016     C                   close     FIVATWWr
082800070411     C                   endif
082900070411     C*
083000070411     C* Effettuo la chiamata al CLLE preposto
083100131017     C                   call(e)   'TITVVTC'
083200070411     C                   parm                    parccm
083300070411     C                   parm                    parmbr
083400070411     C                   parm      '2'           paropz
083500070411     C*
083600070411     C                   eval      wrkesito = '2'
083700070411     C*
083800070411     C                   seton                                        LR
083900070411     C*
084000070411     C                   ENDSR     '*CANCL'
084100070411     C***
084200070411
084300990910
084400000613     C     *inzsr        BEGSR
084500990910     C*
084600990910     C     *entry        plist
084700990920     C                   parm                    tivlrds
084800990921     C                   parm      wrkesito      esito
084900000724     C                   parm                    prmlit
085000000710     C                   parm                    prmfir
085100000613     C*
085200000830     C* CALCOLA LA DATA CORRENTE
085300120706     C                   time                    wn14             14 0
085400120706     C                   movel     wn14          oracor            6 0          *ORA
085500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085600100324     C                   eval      datcor = %dec(%date() : *ISO)
085700000830     C*
085800000613     C                   ENDSR
085900000613     C***
