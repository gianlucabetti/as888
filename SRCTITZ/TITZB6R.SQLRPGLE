000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140120      *
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
002700140306     D depspe          s             10    INZ(*blanks)
002800140306     D curspe          s             10    INZ(*blanks)
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
004400140306     D  VINSPE                       10
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
012700140306     C+ vindta, vinflg, substr(vindta, 33, 10) as sped, rrn(tivin00r)
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
015400140306     C* (uso NSP)
015500140306     C                   EVAL      PiStr=%trim(%subst(vindta:33:10))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300140120     C***                exsr      wrivat_a                                     => carico VAT
016400140120     C***                exsr      wrivat_b                                     => carico VAT
016500111125     C                   exsr      inzvar
016600071003     C                   else
016700080923     C*
016800071009     C                   if        wDISK = 'D'
016900140120     C***                exsr      repNSP
017000071009     C                   exsr      impvab
017100111130     C  N31              eval      fivabds_ok = fivabds
017200111130     C  N31              exsr      wrivab
017300140120     C***                exsr      wrivat_a                                     => carico VAT
017400140120     C***                exsr      wrivat_b                                     => carico VAT
017500131016     C                   exsr      wrivat_e                                     => carico VAT
017600111125     C                   exsr      inzvar
017700071009     C                   else
017800071009     C*
017900010305     C                   if        depspe = *blanks                             => 1° giro
018000010305     C                   eval      depspe = curspe                              => memorizz. spediz
018100080117     C                   clear                   tivinds
018200140120     C***                exsr      repNSP
018300020305     C                   exsr      impvab
018400111130     C                   z-add     VABNCL        sav_VABNCL
018500111130     C                   z-add     VABPKB        sav_VABPKB
018600111130     C                   z-add     VABVLB        sav_VABVLB
018700140120     C***                exsr      wrivat_a                                     => carico VAT
018800140120     C***                exsr      wrivat_b                                     => carico VAT
018900071003     C   50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200111130     C                   z-add     sav_VABNCL    VABNCL
019300111130     C                   z-add     sav_VABPKB    VABPKB
019400111130     C                   z-add     sav_VABVLB    VABVLB
019500111130     C  N31              eval      fivabds_ok = fivabds
019600111125     C  N31              exsr      wrivab
019700111125     C                   exsr      inzvar
019800111125     C                   eval      depspe = curspe                              => memorizz. spediz
019900080117     C                   clear                   tivinds
020000140120     C***                exsr      repNSP
020100020305     C                   exsr      impvab
020200111130     C                   if        wCntRecSpe = *zeros
020300111130     C                   z-add     VABNCL        sav_VABNCL
020400111130     C                   z-add     VABPKB        sav_VABPKB
020500111130     C                   z-add     VABVLB        sav_VABVLB
020600111130     C                   endif
020700111130     C                   add       1             wCntRecSpe
020800140120     C***                exsr      wrivat_a                                     => carico VAT
020900140120     C***                exsr      wrivat_b                                     => carico VAT
021000071003     C   50              exsr      wrivat_e                                     => carico VAT
021100020305     C                   else                                                   => x stessa spediz
021200111130     C                   exsr      impvab
021300120706     C   51              add       VABNCL        sav_VABNCL
021400111125     C   51              add       VABPKB        sav_VABPKB
021500111125     C   51              add       VABVLB        sav_VABVLB
021600111125     C   52              z-add     VABNCL        sav_VABNCL
021700111125     C   52              z-add     VABPKB        sav_VABPKB
021800111125     C   52              z-add     VABVLB        sav_VABVLB
021900111125     C   53              add       VABNCL        sav_VABNCL
022000111125     C   53              z-add     VABPKB        sav_VABPKB
022100111125     C   53              z-add     VABVLB        sav_VABVLB
022200131016     C   54              Z-ADD     VABNCL        sav_VABNCL
022300131016     C   54              ADD       VABPKB        sav_VABPKB
022400131016     C   54              ADD       VABVLB        sav_VABVLB
022500111125     C*
022600140120     C***                exsr      wrivat_a                                     => carico VAT
022700140120     C***                exsr      wrivat_b                                     => carico VAT
022800071003     C   50              exsr      wrivat_e                                     => carico VAT
022900010305     C                   endif
023000010305     C                   endif
023100010305     C                   endif
023200071003     C                   endif
023300071009     C                   endif
023400000905     C*
023500000905     C                   else
023600020725     C                   eval      x_vinflg = '1'
023700000905     C                   endif
023800000905     C*
023900020725     C     VINRRN        chain     tivin000
024000020725     C                   if        %found(tivin00r)
024100020725     C                   eval      y_vinflg = x_vinflg
024200020725     C                   eval      y_vinmsg = x_vinmsg
024300020725     C                   update    tivin000
024400020725     C                   endif
024500020725     C*
024600020725     C/EXEC SQL
024700070530     C+ Fetch C1 into :INPUT_DS
024800020725     C/END-EXEC
024900020725     C*
025000020725     C                   enddo
025100020725     C*
025200020725     C/EXEC SQL
025300020725     C+ close C1
025400020725     C/END-EXEC
025500000905     C*
025600020305     C* Scarico i VAB rimasti "in sospeso"
025700071009     C                   if        wDISK = 'C'
025800140307     C                   z-add     sav_VABNCL    VABNCL
025900140307     C                   z-add     sav_VABPKB    VABPKB
026000140307     C                   z-add     sav_VABVLB    VABVLB
026100111130     C  N31              eval      fivabds_ok = fivabds
026200111130     C  N31              exsr      wrivab
026300071009     C                   endif
026400010202     C*
026500010202     C                   endif
026600990910
026700990910     C* Se non ci sono record con errori ...
026800000710     C                   if        §ctrno = 0
026900990910     C* ... restituisco esito OK.
027000990921     C                   eval      wrkesito = '0'
027100990910     C                   else
027200010201     C                   if        §ctrokvb > 0
027300990921     C                   eval      wrkesito = '1'
027400000710     C                   else
027500000710     C                   eval      wrkesito = '2'
027600990910     C                   endif
027700000710     C                   endif
027800990910     C*
027900990914     C                   if        %open(tivin00r)
028000990908     C                   close     tivin00r
028100990914     C                   endif
028200131016     C                   if        %open(FIVABWWr)
028300131016     C                   close     FIVABWWr
028400120706     C                   endif
028500131016     C                   if        %open(FIVATWWr)
028600131016     C                   close     FIVATWWr
028700010201     C                   endif
028800990910     C*
028900111031     C                   if        §ctrokvb > 0 or
029000111031     C                             §ctrokvt > 0
029100111031     C                             and wPOI <> *zeros
029200010202     C                   exsr      invio
029300990920     C                   endif
029400990920     C*
029500910830     C                   ENDSR
029600000613     C***
029700010305
029800010305     C*----------------------------------------------------*
029900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030000010305     C*----------------------------------------------------*
030100020305     C     WRIVAB        BEGSR
030200070730     C*
030300071003     C* Considerazioni finali
030400111125     C                   if        ok_VABRMA = *blanks
030500111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030600071003     C                   endif
030700071003     C*
030800131016     C                   write     FIVAB000                                     => scarico il VAB
030900010305     C*
031000010305     C                   ENDSR
031100990920
031200000801     C*----------------------------------------------------*
031300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031400000801     C*----------------------------------------------------*
031500010201     C     INZVAR        BEGSR
031600000801     C*
031700010201     C                   Z-ADD     *zeros        Num5_0
031800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031900020725     C                   MOVEL     '0'           FlgCAS            1
032000111125     C*
032100111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032200111130     C*
032300111125     C                   CLEAR                   FIVABDS
032400111130     C                   CLEAR                   FIVABDS_OK
032500111130     C                   CLEAR                   FIVABDS_SAV
032600131016     C                   CLEAR                   FIVAB000
032700131016     C                   CLEAR                   FIVAT000
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033200000801     C*----------------------------------------------------*
033300020904     C     DEFCAM        BEGSR
033400080923     C*
033500111125     C                   SETOFF                                       50
033600111125     C                   SETOFF                                       515253
033700131016     C                   SETOFF                                       54
033800070730     C* Imposto i valori di default...
033900140306     C                   EVAL      VABCCM = 2495120
034000140306     C                   EVAL      VATCCM = 2495120
034100140306     C                   EVAL      VABLNP = 249
034200140306     C                   EVAL      VATLNP = 249
034300140306     C                   EVAL      VABCTR = 100
034400131016     C                   EVAL      VABCBO = '1'
034500140306     C                   EVAL      VABTSP = 'C'
034600070222     C* ... e poi verifico se sono stati passati come parametri
034700070222     C                   IF        vlrppt > *blanks
034800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035000070222     C                   EXSR      CHKNUM
035100070222     C                   IF        PiInt=*on
035200070222     C                   Z-ADD     PiVal         VABCCM
035300070222     C                   Z-ADD     PiVal         VATCCM
035400070222     C                   ENDIF
035500070222     C                   ENDIF
035600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035800070222     C                   EXSR      CHKNUM
035900070222     C                   IF        PiInt=*on
036000070222     C                   Z-ADD     PiVal         VABLNP
036100070222     C                   Z-ADD     PiVal         VATLNP
036200070222     C                   ENDIF
036300070222     C                   ENDIF
036400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036600070222     C                   EXSR      CHKNUM
036700070222     C                   IF        PiInt=*on
036800070222     C                   Z-ADD     PiVal         VABCTR
036900070222     C                   ENDIF
037000070928     C                   ENDIF
037100071009     C                   MOVEL     *blanks       wDISK             1
037200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037300071009     C                   IF        wDISK <> *blanks
037400070928     C                   SETON                                        50
037500070222     C                   ENDIF
037600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037700111125     C                   SETON                                        41
037800080923     C                   ENDIF
037900070222     C                   ENDIF
038000120706     C*
038100120706     C* valido per nr.colli/peso/volume
038200120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038300120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038400120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038500131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038600120706     C                   SELECT
038700120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
038800120706     C                   SETON                                        51
038900120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039000120706     C                   SETON                                        52
039100120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039200120706     C                   SETON                                        53
039300131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039400131016     C                   SETON                                        54
039500120706     C                   ENDSL
039600120706     C*
039700071009     C   50              EVAL      VABCTM = '7Q'
039800000801     C*
039900000801     C                   ENDSR
040000131016     C*----------------------------------------------------*
040100131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040200131016     C*----------------------------------------------------*
040300131016     C     REPNSP        BEGSR
040400131016     C*
040500131016     C                   EXSR      INZVAR
040600131016     C                   EXSR      DEFCAM
040700131016     C*
040800131016     C                   SETON                                        60
040900131016     C*
041000131016     C* NSP => Stacco un numeratore da AZNUM
041100131016     C                   clear                   TRUL33DS
041200131016     C                   eval      I33OPE = *zeros
041300131016     C                   eval      I33CNU = 302
041400131016     C                   eval      I33NUM = 1
041500131016     C                   movel     TRUL33DS      KPJBU
041600131016     C                   call      'TRUL33R'
041700131016     C                   parm                    KPJBA
041800131016     C                   movel     KPJBU         TRUL33DS
041900131016     C                   if        O33ERR = *zeros
042000131016     C                   z-add     O33NRF        VABNSP
042100131016     C                   z-add     O33NRF        VATNSP
042200131016     C                   z-add     O33NRF        VABRMN
042300131016     C                   else
042400131016     C                   SETON                                        31
042500131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042600131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042700131016     C                   endif
042800131016     C*
042900131016     C                   ENDSR
043000000801     C*----------------------------------------------------*
043100131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043200000801     C*----------------------------------------------------*
043300010201     C     IMPVAB        BEGSR
043400070530     C*
043500070530     C                   SETOFF                                       3132
043600070928     C*
043700070928     C                   EXSR      DEFCAM
043800070928     C*
043900111031     C                   IF        vlrpoi <> 999
044000070928     C                   MOVE(P)   vlrpoi        VABFGS
044100070928     C                   MOVE(P)   vlrpoi        VATFGS
044200111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044300111031     C                   ENDIF
044400070928     C*
044500070928     C                   MOVEL     datcor        VABAAS
044600070928     C                   MOVEL     datcor        VATAAS
044700070928     C                   MOVE      datcor        VABMGS
044800140306     C                   EVAL      VABRSD=%trim(%subst(vindta:44:35))
044900131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045000140306     C     '@':'A'       XLATE     VABRSD        VABRSD
045100131016     C* ==
045200140306     C                   EVAL      VABRD2=%trim(%subst(vindta:80:35))
045300140306     C                   EVAL      VABIND=%trim(%subst(vindta:116:35))
045400140306     C                   EVAL      VABLOD=%trim(%subst(vindta:163:35))
045500140306     C                   EVAL      VABRMA=%trim(%subst(vindta:33:10))
045600140306     C*
045700140306     C* Reperisco la provincia dal CAP e dalla località
045800140306     C                   IF        VABCAD <> *blanks AND
045900140306     C                             VABPRD  = *blanks AND
046000140306     C                             VABNZD  = *blanks
046100140306     C                   CLEAR                   TISI95DS
046200140306     C                   EVAL      I95TCN = '3'
046300140306     C                   Z-ADD     datcor        I95DAT
046400140306     C                   EVAL      I95CAP = VABCAD
046500140306     C                   EVAL      I95LOC = VABLOD
046600140306     C                   CALL      'TISI95R'
046700140306     C                   PARM                    TISI95DS
046800140306     C                   EVAL      VABPRD = O95PRV
046900140306     C                   ENDIF
047000131016     C*
047100131016     C* CAD
047200140306     C                   EVAL      PiStr=%trim(%subst(vindta:152:10))
047300131016     C                   EXSR      CHKNUM
047400131016     C                   IF        PiInt=*on
047500131016     C                   Z-ADD     PiVal         Num5_0
047600131016     C                   MOVEL     Num5_0        VABCAD
047700131016     C                   ELSE
047800131016     C                   SETON                                        32
047900131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000131016     C                             + ' ' + 'VABCAD'
048100131016     C                   ENDIF
048200140306     C* RMN
048300140306     C                   EVAL      PiStr=%trim(%subst(vindta:22:10))
048400131016     C                   EXSR      CHKNUM
048500131016     C                   IF        PiInt=*on
048600140120     C                   Z-ADD     PiVal         VABNSP
048700140120     C                   Z-ADD     PiVal         VABRMN
048800131016     C                   ELSE
048900131016     C                   SETON                                        32
049000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100140306     C                             + ' ' + 'VABRMN'
049200131016     C                   ENDIF
049300140306     C* NSP
049400140306     C                   EVAL      PiStr=%trim(%subst(vindta:33:10))
049500140306     C                   EXSR      CHKNUM
049600140306     C                   IF        PiInt=*on
049700140306     C                   Z-ADD     PiVal         VABNSP
049800140306     C                   Z-ADD     PiVal         VATNSP
049900140306     C                   ELSE
050000140306     C                   SETON                                        32
050100140306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200140306     C                             + ' ' + 'VABNSP VATNSP VABRMA'
050300140306     C                   ENDIF
050400140120     C* NCL
050500140306     C                   EVAL      PiStr=%trim(%subst(vindta:264:5))
050600140120     C                   EXSR      CHKNUM
050700140120     C                   IF        PiInt=*on
050800140120     C                   Z-ADD     PiVal         VABNCL
050900140120     C                   ELSE
051000140120     C                   SETON                                        32
051100140120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200140120     C                             + ' ' + 'VABNCL'
051300140120     C                   ENDIF
051400131016     C* PKB
051500140306     C                   EVAL      PiStr=%trim(%subst(vindta:228:8))
051600131016     C                   EXSR      CHKNUM
051700131016     C                   IF        PiNum=*on
051800131016     C* il dato ha la virgola che separa i decimali
051900131016     C                   EVAL(H)   VABPKB = PiVal
052000131016     C                   ELSE
052100131016     C                   SETON                                        32
052200131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052300131016     C                             + ' ' + 'VABPKB'
052400131016     C                   ENDIF
052500140306     C* VLB
052600140306     C                   EVAL      PiStr=%trim(%subst(vindta:252:6))
052700140120     C                   EXSR      CHKNUM
052800140120     C                   IF        PiNum=*on
052900140120     C* il dato ha il punto che separa i decimali
053000140306     C                   EVAL(H)   VABVLB = PiVal
053100140120     C                   ELSE
053200140120     C                   SETON                                        32
053300140120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053400140306     C                             + ' ' + 'VABVLB'
053500140120     C                   ENDIF
053600070730     C*
053700070730     C* Considerazioni finali su CBO/CAS
053800121113     C                   IF        FlgCAS <> '0'
053900070730     C                   IF        VABCBO = '1'
054000070730     C                   EVAL      VABCBO = '4'
054100070730     C                   ENDIF
054200070730     C                   IF        VABCBO = '2'
054300070730     C                   EVAL      VABCBO = '6'
054400070730     C                   ENDIF
054500070730     C                   ENDIF
054600020305     C*
054700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054800011113     C                   EXSR      CHKIMPDIV
054900010202     C*
055000000801     C* Ebbene...
055100000801     C                   ADD       1             §CTRMO
055200070530     C                   IF        *in31 <> *zeros OR
055300070530     C                             *in32 <> *zeros
055400000801     C                   ADD       1             §CTRNO
055500020725     C                   EVAL      x_vinflg = '2'
055600000801     C                   ELSE
055700010201     C                   ADD       1             §CTROKVB
055800000801     C                   ENDIF
055900000801     C*
056000000801     C                   ENDSR
056100131016     C*----------------------------------------------------*
056200131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
056300131016     C*----------------------------------------------------*
056400131016     C     WRIVAT_A      BEGSR
056500131016     C*
056600131016     C                   eval      vatAAS = vabAAS
056700131016     C                   eval      vatLNP = vabLNP
056800131016     C                   eval      vatNRS = vabNRS
056900131016     C                   eval      vatNSP = vabNSP
057000131016     C*
057100131016     C* Valorizzo il buffer di scrittura del FIVAT
057200131016     C* - TIPO RECORD "A"
057300131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
057400131016     C                   eval      VATTRC = 'A'
057500131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
057600131016     C                   write     FIVAT000
057700131016     C                   endif
057800131016     C*
057900131016     C                   ENDSR
058000131016     C*----------------------------------------------------*
058100131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
058200131016     C*----------------------------------------------------*
058300131016     C     WRIVAT_B      BEGSR
058400131016     C*
058500131016     C                   eval      vatAAS = vabAAS
058600131016     C                   eval      vatLNP = vabLNP
058700131016     C                   eval      vatNRS = vabNRS
058800131016     C                   eval      vatNSP = vabNSP
058900131016     C*
059000131016     C* Valorizzo il buffer di scrittura del FIVAT
059100131016     C* - TIPO RECORD "B"
059200131016     C                   EVAL      w70=%trim(%subst(vindta:113:015))
059300131016     C                   if        %subst(w70:001:15) <> *blanks
059400131016     C                   eval      VATTRC = 'B'
059500131016     C                   eval      VATNOT = %trim(%subst(w70:1:15))
059600131016     C                   write     FIVAT000
059700131016     C                   endif
059800131016     C*
059900131016     C                   ENDSR
060000131016     C*----------------------------------------------------*
060100131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
060200131016     C*----------------------------------------------------*
060300131016     C     WRIVAT_E      BEGSR
060400131016     C*
060500131016     C                   eval      vatAAS = vabAAS
060600131016     C                   eval      vatLNP = vabLNP
060700131016     C                   eval      vatNRS = vabNRS
060800131016     C                   eval      vatNSP = vabNSP
060900131016     C*
061000131016     C* Valorizzo il buffer di scrittura del FIVAT
061100131016     C* - TIPO RECORD "E"
061200140306     C                   if        %trim(%subst(vindta:203:20)) <> *blanks
061300131016     C                   eval      VATTRC = 'E'
061400140306     C                   eval      VATNOT = %trim(%subst(vindta:203:20))
061500131016     C                   write     FIVAT000
061600131016     C                   endif
061700131016     C*
061800131016     C                   ENDSR
061900020904
062000020904
062100010202     C*----------------------------------------------------*
062200131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
062300010202     C*----------------------------------------------------*
062400020305     C     PREVAT        BEGSR
062500010202     C*
062600131016     C* Compongo il nome del membro da dare al FIVATWWR
062700010202     C                   eval      parmbr = vlrhdl
062800010202     C                   movel     'M'           parmbr
062900070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063000010202     C                   eval      paropz = '1'
063100010202     C* Effettuo la chiamata al CLLE preposto
063200131017     C                   call(e)   'TITVVTC'
063300010202     C                   parm                    parccm
063400010202     C                   parm                    parmbr
063500010202     C                   parm                    paropz
063600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063700010202     C                   if        %error
063800010202     C                   movel     '1'           chkcall
063900010202     C                   else
064000010202     C                   movel     '0'           chkcall
064100010202     C                   endif
064200010202     C*
064300010202     C                   ENDSR
064400000801     C*----------------------------------------------------*
064500000801     C*  CONTROLLO NUMERICITA' CAMPI
064600000801     C*----------------------------------------------------*
064700000801     C     CHKNUM        BEGSR
064800081113     C*
064900081113     C                   IF        PiDecChr = *blanks
065000121113     C                   EVAL      PiDecChr = '.'
065100081113     C                   ENDIF
065200081113     C*
065300081113     C                   callp(e)  UBISNUM_Check(PiStr
065400081113     C                                          :PiDecChr
065500081113     C                                          :PiVal
065600081113     C                                          :PiNum
065700081113     C                                          :PiInt)
065800081113     C*
065900000801     C                   IF        %error
066000000801     C                   EVAL      PiInt=*off
066100000801     C                   ENDIF
066200000801     C*
066300000801     C                   ENDSR
066400000801     C***
066500000801
066600011113
066700011113     C*----------------------------------------------------*
066800011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066900011113     C*----------------------------------------------------*
067000011113     C     CHKIMPDIV     BEGSR
067100011113     C*
067200011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067300011113     C                   Z-ADD     *zeros        wrkDec            9 9
067400011113     C*
067500011113     C* Come prima cosa effettuo considerazioni sulla divisa
067600011113     C                   IF        vabIAS > *zeros
067700011113     C                   IF        vabVAS <> 'EUR'
067800011113     C                   EVAL      vabVAS =  'ITL'
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C                   IF        vabCAS > *zeros
068300011113     C                   IF        vabVCA <> 'EUR'
068400011113     C                   EVAL      vabVCA =  'ITL'
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C                   IF        vabVMD > *zeros
068900020305     C                   IF        vabVAD <> 'EUR'
069000011113     C                   EVAL      vabVAD =  'ITL'
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069500011113     C                   Z-ADD     vabIAS        wrkDec
069600011113     C                   IF        wrkDec > *zeros
069700011113     C                   IF        vabVAS = 'ITL'
069800011113     C                   EVAL      vabIAS = *zeros
069900011113     C                   ENDIF
070000011113     C                   ENDIF
070100011113     C*
070200011113     C* Stabilisco se il contrasegno ha decimali valorizzati
070300011113     C                   Z-ADD     vabCAS        wrkDec
070400011113     C                   IF        wrkDec > *zeros
070500011113     C                   IF        vabVCA = 'ITL'
070600011113     C                   EVAL      vabCAS = *zeros
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071100011113     C                   Z-ADD     vabVMD        wrkDec
071200011113     C                   IF        wrkDec > *zeros
071300011113     C                   IF        vabVAD = 'ITL'
071400011113     C                   EVAL      vabVMD = *zeros
071500011113     C                   ENDIF
071600011113     C                   ENDIF
071700011113     C*
071800011113     C                   ENDSR
071900011113     C***
072000011113
072100011113
072200000801
072300000801
072400990920      /TITLE Invio dei dati al punto operativo.
072500010202     C     invio         BEGSR
072600990920     C*
072700131016     C* 1° invio FIVAT
072800010201     C                   reset                   dscmz
072900111031     C                   move      wPOI          cmzdst
073000131016     C                   eval      cmzfld = 'FIVATWWR'
073100010201     C                   eval      cmzmbd = vlrhdl
073200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073300021009     C***                if        prmfir = *blanks
073400131016     C                   eval      cmzfla = 'FIVAT00F'
073500131016     C                   eval      cmzmba = 'FIVAT00F'
073600021009     C***                else
073700021009     C***                eval      cmzfla = prmfir
073800021009     C***                eval      cmzmba = prmfir
073900021009     C***                endif
074000010201     C                   eval      cmznrr = *zeros
074100020305     C                   move      §ctrokvt      cmznrr
074200021018     C                   eval      cmzlba = vlrfl1
074300010201     C                   call(e)   'TIS711C'
074400010201     C                   parm                    dscmz
074500010201     C                   parm      *blanks       esito
074600010205     C                   if        %error
074700010205     C                             or cmzerr = '1'
074800010205     C                             or esito  = '1'
074900010205     C                   eval      wrkesito = '3'
075000010205     C                   else
075100010201     C*
075200131016     C* 2° invio FIVAB
075300010201     C                   reset                   dscmz
075400111031     C                   move      wPOI          cmzdst
075500010201     C                   eval      cmzfld = vlrfou
075600010201     C                   eval      cmzmbd = vlrhdl
075700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075800021009     C***                if        prmfir = *blanks
075900131016     C                   eval      cmzfla = 'FIVAB00F'
076000131016     C                   eval      cmzmba = 'FIVAB00F'
076100021009     C***                else
076200021009     C***                eval      cmzfla = prmfir
076300021009     C***                eval      cmzmba = prmfir
076400021009     C***                endif
076500010201     C                   eval      cmznrr = *zeros
076600010201     C                   move      §ctrokvb      cmznrr
076700021018     C                   eval      cmzlba = vlrfl1
076800010201     C                   call(e)   'TIS711C'
076900010201     C                   parm                    dscmz
077000010201     C                   parm      *blanks       esito
077100010201     C                   if        %error
077200010201     C                             or cmzerr = '1'
077300010201     C                             or esito  = '1'
077400010201     C                   eval      wrkesito = '3'
077500010201     C                   endif
077600010205     C                   endif
077700990920     C*
077800000613     C                   ENDSR
077900000613     C***
078000070411
078100070411     C     *pssr         BEGSR
078200070411     C*
078300070411     C                   if        %open(tivin00r)
078400070411     C                   close     tivin00r
078500070411     C                   endif
078600131016     C                   if        %open(FIVABWWr)
078700131016     C                   close     FIVABWWr
078800120706     C                   endif
078900131016     C                   if        %open(FIVATWWr)
079000131016     C                   close     FIVATWWr
079100070411     C                   endif
079200070411     C*
079300070411     C* Effettuo la chiamata al CLLE preposto
079400131017     C                   call(e)   'TITVVTC'
079500070411     C                   parm                    parccm
079600070411     C                   parm                    parmbr
079700070411     C                   parm      '2'           paropz
079800070411     C*
079900070411     C                   eval      wrkesito = '2'
080000070411     C*
080100070411     C                   seton                                        LR
080200070411     C*
080300070411     C                   ENDSR     '*CANCL'
080400070411     C***
080500070411
080600990910
080700000613     C     *inzsr        BEGSR
080800990910     C*
080900990910     C     *entry        plist
081000990920     C                   parm                    tivlrds
081100990921     C                   parm      wrkesito      esito
081200000724     C                   parm                    prmlit
081300000710     C                   parm                    prmfir
081400000613     C*
081500000830     C* CALCOLA LA DATA CORRENTE
081600120706     C                   time                    wn14             14 0
081700120706     C                   movel     wn14          oracor            6 0          *ORA
081800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081900100324     C                   eval      datcor = %dec(%date() : *ISO)
082000000830     C*
082100000613     C                   ENDSR
082200000613     C***
