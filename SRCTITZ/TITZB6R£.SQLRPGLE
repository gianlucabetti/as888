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
002700140120     D depspe          s              9    INZ(*blanks)
002800140120     D curspe          s              9    INZ(*blanks)
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
004400140120     D  VINSPE                        9
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
012700140120     C+ vindta, vinflg, substr(vindta, 123, 9) as sped, rrn(tivin00r)
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
015400140120     C* (uso NSP/RMN)
015500140120     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
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
025800111130     C  N31              eval      fivabds_ok = fivabds
025900111130     C  N31              exsr      wrivab
026000071009     C                   endif
026100010202     C*
026200010202     C                   endif
026300990910
026400990910     C* Se non ci sono record con errori ...
026500000710     C                   if        §ctrno = 0
026600990910     C* ... restituisco esito OK.
026700990921     C                   eval      wrkesito = '0'
026800990910     C                   else
026900010201     C                   if        §ctrokvb > 0
027000990921     C                   eval      wrkesito = '1'
027100000710     C                   else
027200000710     C                   eval      wrkesito = '2'
027300990910     C                   endif
027400000710     C                   endif
027500990910     C*
027600990914     C                   if        %open(tivin00r)
027700990908     C                   close     tivin00r
027800990914     C                   endif
027900131016     C                   if        %open(FIVABWWr)
028000131016     C                   close     FIVABWWr
028100120706     C                   endif
028200131016     C                   if        %open(FIVATWWr)
028300131016     C                   close     FIVATWWr
028400010201     C                   endif
028500990910     C*
028600111031     C                   if        §ctrokvb > 0 or
028700111031     C                             §ctrokvt > 0
028800111031     C                             and wPOI <> *zeros
028900010202     C                   exsr      invio
029000990920     C                   endif
029100990920     C*
029200910830     C                   ENDSR
029300000613     C***
029400010305
029500010305     C*----------------------------------------------------*
029600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029700010305     C*----------------------------------------------------*
029800020305     C     WRIVAB        BEGSR
029900070730     C*
030000071003     C* Considerazioni finali
030100111125     C                   if        ok_VABRMA = *blanks
030200111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030300071003     C                   endif
030400071003     C*
030500131016     C                   write     FIVAB000                                     => scarico il VAB
030600010305     C*
030700010305     C                   ENDSR
030800990920
030900000801     C*----------------------------------------------------*
031000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031100000801     C*----------------------------------------------------*
031200010201     C     INZVAR        BEGSR
031300000801     C*
031400010201     C                   Z-ADD     *zeros        Num5_0
031500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031600020725     C                   MOVEL     '0'           FlgCAS            1
031700111125     C*
031800111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
031900111130     C*
032000111125     C                   CLEAR                   FIVABDS
032100111130     C                   CLEAR                   FIVABDS_OK
032200111130     C                   CLEAR                   FIVABDS_SAV
032300131016     C                   CLEAR                   FIVAB000
032400131016     C                   CLEAR                   FIVAT000
032500000801     C*
032600000801     C                   ENDSR
032700000801     C*----------------------------------------------------*
032800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032900000801     C*----------------------------------------------------*
033000020904     C     DEFCAM        BEGSR
033100080923     C*
033200111125     C                   SETOFF                                       50
033300111125     C                   SETOFF                                       515253
033400131016     C                   SETOFF                                       54
033500070730     C* Imposto i valori di default...
033600140120     C                   EVAL      VABCCM = 1662245
033700140120     C                   EVAL      VATCCM = 1662245
033800140120     C                   EVAL      VABLNP = 166
033900140120     C                   EVAL      VATLNP = 166
034000140120     C***                EVAL      VABCTR = 000
034100131016     C                   EVAL      VABCBO = '1'
034200070222     C* ... e poi verifico se sono stati passati come parametri
034300070222     C                   IF        vlrppt > *blanks
034400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABCCM
034900070222     C                   Z-ADD     PiVal         VATCCM
035000070222     C                   ENDIF
035100070222     C                   ENDIF
035200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABLNP
035700070222     C                   Z-ADD     PiVal         VATLNP
035800070222     C                   ENDIF
035900070222     C                   ENDIF
036000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036200070222     C                   EXSR      CHKNUM
036300070222     C                   IF        PiInt=*on
036400070222     C                   Z-ADD     PiVal         VABCTR
036500070222     C                   ENDIF
036600070928     C                   ENDIF
036700071009     C                   MOVEL     *blanks       wDISK             1
036800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036900071009     C                   IF        wDISK <> *blanks
037000070928     C                   SETON                                        50
037100070222     C                   ENDIF
037200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037300111125     C                   SETON                                        41
037400080923     C                   ENDIF
037500070222     C                   ENDIF
037600120706     C*
037700120706     C* valido per nr.colli/peso/volume
037800120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
037900120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038000120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038100131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038200120706     C                   SELECT
038300120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
038400120706     C                   SETON                                        51
038500120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
038600120706     C                   SETON                                        52
038700120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
038800120706     C                   SETON                                        53
038900131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039000131016     C                   SETON                                        54
039100120706     C                   ENDSL
039200120706     C*
039300071009     C   50              EVAL      VABCTM = '7Q'
039400000801     C*
039500000801     C                   ENDSR
039600131016     C*----------------------------------------------------*
039700131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039800131016     C*----------------------------------------------------*
039900131016     C     REPNSP        BEGSR
040000131016     C*
040100131016     C                   EXSR      INZVAR
040200131016     C                   EXSR      DEFCAM
040300131016     C*
040400131016     C                   SETON                                        60
040500131016     C*
040600131016     C* NSP => Stacco un numeratore da AZNUM
040700131016     C                   clear                   TRUL33DS
040800131016     C                   eval      I33OPE = *zeros
040900131016     C                   eval      I33CNU = 302
041000131016     C                   eval      I33NUM = 1
041100131016     C                   movel     TRUL33DS      KPJBU
041200131016     C                   call      'TRUL33R'
041300131016     C                   parm                    KPJBA
041400131016     C                   movel     KPJBU         TRUL33DS
041500131016     C                   if        O33ERR = *zeros
041600131016     C                   z-add     O33NRF        VABNSP
041700131016     C                   z-add     O33NRF        VATNSP
041800131016     C                   z-add     O33NRF        VABRMN
041900131016     C                   else
042000131016     C                   SETON                                        31
042100131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042200131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042300131016     C                   endif
042400131016     C*
042500131016     C                   ENDSR
042600000801     C*----------------------------------------------------*
042700131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042800000801     C*----------------------------------------------------*
042900010201     C     IMPVAB        BEGSR
043000070530     C*
043100070530     C                   SETOFF                                       3132
043200070928     C*
043300070928     C                   EXSR      DEFCAM
043400070928     C*
043500111031     C                   IF        vlrpoi <> 999
043600070928     C                   MOVE(P)   vlrpoi        VABFGS
043700070928     C                   MOVE(P)   vlrpoi        VATFGS
043800111031     C                   Z-ADD     vlrpoi        wPOI              3 0
043900111031     C                   ENDIF
044000070928     C*
044100070928     C                   MOVEL     datcor        VABAAS
044200070928     C                   MOVEL     datcor        VATAAS
044300070928     C                   MOVE      datcor        VABMGS
044400131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044500131016     C     '@':'A'       XLATE     w70           w70
044600140120     C                   EVAL      VABRSD=%trim(%subst(vindta:25:30))
044700131016     C* ==
044800140120     C                   EVAL      VABIND=%trim(%subst(vindta:55:30))
044900140120     C                   EVAL      VABLOD=%trim(%subst(vindta:90:20))
045000140120     C                   EVAL      VABPRD=%trim(%subst(vindta:110:2))
045100140120     C                   EVAL      VABRMA=%trim(%subst(vindta:132:8))
045200131016     C*
045300131016     C* CAD
045400140120     C                   EVAL      PiStr=%trim(%subst(vindta:85:5))
045500131016     C                   EXSR      CHKNUM
045600131016     C                   IF        PiInt=*on
045700131016     C                   Z-ADD     PiVal         Num5_0
045800131016     C                   MOVEL     Num5_0        VABCAD
045900131016     C                   ELSE
046000131016     C                   SETON                                        32
046100131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200131016     C                             + ' ' + 'VABCAD'
046300131016     C                   ENDIF
046400140120     C* NSP/RMN
046500140120     C                   EVAL      PiStr=%trim(%subst(vindta:123:9))
046600131016     C                   EXSR      CHKNUM
046700131016     C                   IF        PiInt=*on
046800140120     C                   Z-ADD     PiVal         VABNSP
046900140120     C                   Z-ADD     PiVal         VABRMN
047000131016     C                   ELSE
047100131016     C                   SETON                                        32
047200131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047300140120     C                             + ' ' + 'VABNSP VABRMN'
047400131016     C                   ENDIF
047500140120     C* NCL
047600140120     C                   EVAL      PiStr=%trim(%subst(vindta:16:3))
047700140120     C                   EXSR      CHKNUM
047800140120     C                   IF        PiInt=*on
047900140120     C                   Z-ADD     PiVal         VABNCL
048000140120     C                   ELSE
048100140120     C                   SETON                                        32
048200140120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300140120     C                             + ' ' + 'VABNCL'
048400140120     C                   ENDIF
048500131016     C* PKB
048600140120     C                   EVAL      PiStr=%trim(%subst(vindta:161:10))
048700131016     C                   EXSR      CHKNUM
048800131016     C                   IF        PiNum=*on
048900131016     C* il dato ha la virgola che separa i decimali
049000131016     C                   EVAL(H)   VABPKB = PiVal
049100131016     C                   ELSE
049200131016     C                   SETON                                        32
049300131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049400131016     C                             + ' ' + 'VABPKB'
049500131016     C                   ENDIF
049600140120     C* CAS
049700140120     C                   IF        %trim(%subst(vindta:140:1))='X'
049800140120     C                   EVAL      VABVCA = 'EUR'
049900140120     C                   EVAL      VABTIC = *blank
050000140120     C                   EVAL      FlgCAS = '1'
050100140120     C                   EVAL      PiStr=%trim(%subst(vindta:151:10))
050200140120     C                   EXSR      CHKNUM
050300140120     C                   IF        PiNum=*on
050400140120     C* il dato ha il punto che separa i decimali
050500140120     C                   EVAL(H)   VABCAS = PiVal
050600140120     C                   ELSE
050700140120     C                   SETON                                        32
050800140120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900140120     C                             + ' ' + 'VABCAS'
051000140120     C                   ENDIF
051100140120     C                   ENDIF
051200070730     C*
051300070730     C* Considerazioni finali su CBO/CAS
051400121113     C                   IF        FlgCAS <> '0'
051500070730     C                   IF        VABCBO = '1'
051600070730     C                   EVAL      VABCBO = '4'
051700070730     C                   ENDIF
051800070730     C                   IF        VABCBO = '2'
051900070730     C                   EVAL      VABCBO = '6'
052000070730     C                   ENDIF
052100070730     C                   ENDIF
052200020305     C*
052300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052400011113     C                   EXSR      CHKIMPDIV
052500010202     C*
052600000801     C* Ebbene...
052700000801     C                   ADD       1             §CTRMO
052800070530     C                   IF        *in31 <> *zeros OR
052900070530     C                             *in32 <> *zeros
053000000801     C                   ADD       1             §CTRNO
053100020725     C                   EVAL      x_vinflg = '2'
053200000801     C                   ELSE
053300010201     C                   ADD       1             §CTROKVB
053400000801     C                   ENDIF
053500000801     C*
053600000801     C                   ENDSR
053700131016     C*----------------------------------------------------*
053800131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
053900131016     C*----------------------------------------------------*
054000131016     C     WRIVAT_A      BEGSR
054100131016     C*
054200131016     C                   eval      vatAAS = vabAAS
054300131016     C                   eval      vatLNP = vabLNP
054400131016     C                   eval      vatNRS = vabNRS
054500131016     C                   eval      vatNSP = vabNSP
054600131016     C*
054700131016     C* Valorizzo il buffer di scrittura del FIVAT
054800131016     C* - TIPO RECORD "A"
054900131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
055000131016     C                   eval      VATTRC = 'A'
055100131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
055200131016     C                   write     FIVAT000
055300131016     C                   endif
055400131016     C*
055500131016     C                   ENDSR
055600131016     C*----------------------------------------------------*
055700131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
055800131016     C*----------------------------------------------------*
055900131016     C     WRIVAT_B      BEGSR
056000131016     C*
056100131016     C                   eval      vatAAS = vabAAS
056200131016     C                   eval      vatLNP = vabLNP
056300131016     C                   eval      vatNRS = vabNRS
056400131016     C                   eval      vatNSP = vabNSP
056500131016     C*
056600131016     C* Valorizzo il buffer di scrittura del FIVAT
056700131016     C* - TIPO RECORD "B"
056800131016     C                   EVAL      w70=%trim(%subst(vindta:113:015))
056900131016     C                   if        %subst(w70:001:15) <> *blanks
057000131016     C                   eval      VATTRC = 'B'
057100131016     C                   eval      VATNOT = %trim(%subst(w70:1:15))
057200131016     C                   write     FIVAT000
057300131016     C                   endif
057400131016     C*
057500131016     C                   ENDSR
057600131016     C*----------------------------------------------------*
057700131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057800131016     C*----------------------------------------------------*
057900131016     C     WRIVAT_E      BEGSR
058000131016     C*
058100131016     C                   eval      vatAAS = vabAAS
058200131016     C                   eval      vatLNP = vabLNP
058300131016     C                   eval      vatNRS = vabNRS
058400131016     C                   eval      vatNSP = vabNSP
058500131016     C*
058600131016     C* Valorizzo il buffer di scrittura del FIVAT
058700131016     C* - TIPO RECORD "E"
058800140120     C                   if        %trim(%subst(vindta:141:10)) <> *blanks
058900131016     C                   eval      VATTRC = 'E'
059000140120     C                   eval      VATNOT = 'M' + %trim(%subst(vindta:141:10))
059100131016     C                   write     FIVAT000
059200131016     C                   endif
059300131016     C*
059400131016     C                   ENDSR
059500020904
059600020904
059700010202     C*----------------------------------------------------*
059800131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
059900010202     C*----------------------------------------------------*
060000020305     C     PREVAT        BEGSR
060100010202     C*
060200131016     C* Compongo il nome del membro da dare al FIVATWWR
060300010202     C                   eval      parmbr = vlrhdl
060400010202     C                   movel     'M'           parmbr
060500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
060600010202     C                   eval      paropz = '1'
060700010202     C* Effettuo la chiamata al CLLE preposto
060800131017     C                   call(e)   'TITVVTC'
060900010202     C                   parm                    parccm
061000010202     C                   parm                    parmbr
061100010202     C                   parm                    paropz
061200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061300010202     C                   if        %error
061400010202     C                   movel     '1'           chkcall
061500010202     C                   else
061600010202     C                   movel     '0'           chkcall
061700010202     C                   endif
061800010202     C*
061900010202     C                   ENDSR
062000000801     C*----------------------------------------------------*
062100000801     C*  CONTROLLO NUMERICITA' CAMPI
062200000801     C*----------------------------------------------------*
062300000801     C     CHKNUM        BEGSR
062400081113     C*
062500081113     C                   IF        PiDecChr = *blanks
062600121113     C                   EVAL      PiDecChr = '.'
062700081113     C                   ENDIF
062800081113     C*
062900081113     C                   callp(e)  UBISNUM_Check(PiStr
063000081113     C                                          :PiDecChr
063100081113     C                                          :PiVal
063200081113     C                                          :PiNum
063300081113     C                                          :PiInt)
063400081113     C*
063500000801     C                   IF        %error
063600000801     C                   EVAL      PiInt=*off
063700000801     C                   ENDIF
063800000801     C*
063900000801     C                   ENDSR
064000000801     C***
064100000801
064200011113
064300011113     C*----------------------------------------------------*
064400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064500011113     C*----------------------------------------------------*
064600011113     C     CHKIMPDIV     BEGSR
064700011113     C*
064800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
064900011113     C                   Z-ADD     *zeros        wrkDec            9 9
065000011113     C*
065100011113     C* Come prima cosa effettuo considerazioni sulla divisa
065200011113     C                   IF        vabIAS > *zeros
065300011113     C                   IF        vabVAS <> 'EUR'
065400011113     C                   EVAL      vabVAS =  'ITL'
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C                   IF        vabCAS > *zeros
065900011113     C                   IF        vabVCA <> 'EUR'
066000011113     C                   EVAL      vabVCA =  'ITL'
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C                   IF        vabVMD > *zeros
066500020305     C                   IF        vabVAD <> 'EUR'
066600011113     C                   EVAL      vabVAD =  'ITL'
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067100011113     C                   Z-ADD     vabIAS        wrkDec
067200011113     C                   IF        wrkDec > *zeros
067300011113     C                   IF        vabVAS = 'ITL'
067400011113     C                   EVAL      vabIAS = *zeros
067500011113     C                   ENDIF
067600011113     C                   ENDIF
067700011113     C*
067800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
067900011113     C                   Z-ADD     vabCAS        wrkDec
068000011113     C                   IF        wrkDec > *zeros
068100011113     C                   IF        vabVCA = 'ITL'
068200011113     C                   EVAL      vabCAS = *zeros
068300011113     C                   ENDIF
068400011113     C                   ENDIF
068500011113     C*
068600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068700011113     C                   Z-ADD     vabVMD        wrkDec
068800011113     C                   IF        wrkDec > *zeros
068900011113     C                   IF        vabVAD = 'ITL'
069000011113     C                   EVAL      vabVMD = *zeros
069100011113     C                   ENDIF
069200011113     C                   ENDIF
069300011113     C*
069400011113     C                   ENDSR
069500011113     C***
069600011113
069700011113
069800000801
069900000801
070000990920      /TITLE Invio dei dati al punto operativo.
070100010202     C     invio         BEGSR
070200990920     C*
070300131016     C* 1° invio FIVAT
070400010201     C                   reset                   dscmz
070500111031     C                   move      wPOI          cmzdst
070600131016     C                   eval      cmzfld = 'FIVATWWR'
070700010201     C                   eval      cmzmbd = vlrhdl
070800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070900021009     C***                if        prmfir = *blanks
071000131016     C                   eval      cmzfla = 'FIVAT00F'
071100131016     C                   eval      cmzmba = 'FIVAT00F'
071200021009     C***                else
071300021009     C***                eval      cmzfla = prmfir
071400021009     C***                eval      cmzmba = prmfir
071500021009     C***                endif
071600010201     C                   eval      cmznrr = *zeros
071700020305     C                   move      §ctrokvt      cmznrr
071800021018     C                   eval      cmzlba = vlrfl1
071900010201     C                   call(e)   'TIS711C'
072000010201     C                   parm                    dscmz
072100010201     C                   parm      *blanks       esito
072200010205     C                   if        %error
072300010205     C                             or cmzerr = '1'
072400010205     C                             or esito  = '1'
072500010205     C                   eval      wrkesito = '3'
072600010205     C                   else
072700010201     C*
072800131016     C* 2° invio FIVAB
072900010201     C                   reset                   dscmz
073000111031     C                   move      wPOI          cmzdst
073100010201     C                   eval      cmzfld = vlrfou
073200010201     C                   eval      cmzmbd = vlrhdl
073300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073400021009     C***                if        prmfir = *blanks
073500131016     C                   eval      cmzfla = 'FIVAB00F'
073600131016     C                   eval      cmzmba = 'FIVAB00F'
073700021009     C***                else
073800021009     C***                eval      cmzfla = prmfir
073900021009     C***                eval      cmzmba = prmfir
074000021009     C***                endif
074100010201     C                   eval      cmznrr = *zeros
074200010201     C                   move      §ctrokvb      cmznrr
074300021018     C                   eval      cmzlba = vlrfl1
074400010201     C                   call(e)   'TIS711C'
074500010201     C                   parm                    dscmz
074600010201     C                   parm      *blanks       esito
074700010201     C                   if        %error
074800010201     C                             or cmzerr = '1'
074900010201     C                             or esito  = '1'
075000010201     C                   eval      wrkesito = '3'
075100010201     C                   endif
075200010205     C                   endif
075300990920     C*
075400000613     C                   ENDSR
075500000613     C***
075600070411
075700070411     C     *pssr         BEGSR
075800070411     C*
075900070411     C                   if        %open(tivin00r)
076000070411     C                   close     tivin00r
076100070411     C                   endif
076200131016     C                   if        %open(FIVABWWr)
076300131016     C                   close     FIVABWWr
076400120706     C                   endif
076500131016     C                   if        %open(FIVATWWr)
076600131016     C                   close     FIVATWWr
076700070411     C                   endif
076800070411     C*
076900070411     C* Effettuo la chiamata al CLLE preposto
077000131017     C                   call(e)   'TITVVTC'
077100070411     C                   parm                    parccm
077200070411     C                   parm                    parmbr
077300070411     C                   parm      '2'           paropz
077400070411     C*
077500070411     C                   eval      wrkesito = '2'
077600070411     C*
077700070411     C                   seton                                        LR
077800070411     C*
077900070411     C                   ENDSR     '*CANCL'
078000070411     C***
078100070411
078200990910
078300000613     C     *inzsr        BEGSR
078400990910     C*
078500990910     C     *entry        plist
078600990920     C                   parm                    tivlrds
078700990921     C                   parm      wrkesito      esito
078800000724     C                   parm                    prmlit
078900000710     C                   parm                    prmfir
079000000613     C*
079100000830     C* CALCOLA LA DATA CORRENTE
079200120706     C                   time                    wn14             14 0
079300120706     C                   movel     wn14          oracor            6 0          *ORA
079400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079500100324     C                   eval      datcor = %dec(%date() : *ISO)
079600000830     C*
079700000613     C                   ENDSR
079800000613     C***
