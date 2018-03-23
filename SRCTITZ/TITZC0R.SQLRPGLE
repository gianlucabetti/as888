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
002700140328     D depspe          s             15    INZ(*blanks)
002800140328     D curspe          s             15    INZ(*blanks)
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
004400140328     D  VINSPE                       15
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
012700140328     C+ vindta, vinflg, substr(vindta, 35, 15) as sped, rrn(tivin00r)
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
015400140328     C* (uso NSP/RMN/RMA)
015500140328     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300140120     C***                exsr      wrivat_a                                     => carico VAT
016400140328     C                   exsr      wrivat_b                                     => carico VAT
016500111125     C                   exsr      inzvar
016600071003     C                   else
016700080923     C*
016800071009     C                   if        wDISK = 'D'
016900140120     C***                exsr      repNSP
017000071009     C                   exsr      impvab
017100111130     C  N31              eval      fivabds_ok = fivabds
017200111130     C  N31              exsr      wrivab
017300140120     C***                exsr      wrivat_a                                     => carico VAT
017400140328     C                   exsr      wrivat_b                                     => carico VAT
017500140328     C***                exsr      wrivat_e                                     => carico VAT
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
018800140328     C                   exsr      wrivat_b                                     => carico VAT
018900140328     C***50              exsr      wrivat_e                                     => carico VAT
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
020900140328     C                   exsr      wrivat_b                                     => carico VAT
021000140328     C***50              exsr      wrivat_e                                     => carico VAT
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
022700140328     C                   exsr      wrivat_b                                     => carico VAT
022800140328     C***50              exsr      wrivat_e                                     => carico VAT
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
033900140328     C                   EVAL      VABCCM = 0897557
034000140328     C                   EVAL      VATCCM = 0897557
034100140328     C                   EVAL      VABLNP = 089
034200140328     C                   EVAL      VATLNP = 089
034300140328     C                   EVAL      VABCTR = 000
034400131016     C                   EVAL      VABCBO = '1'
034500070222     C* ... e poi verifico se sono stati passati come parametri
034600070222     C                   IF        vlrppt > *blanks
034700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
034800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
034900070222     C                   EXSR      CHKNUM
035000070222     C                   IF        PiInt=*on
035100070222     C                   Z-ADD     PiVal         VABCCM
035200070222     C                   Z-ADD     PiVal         VATCCM
035300070222     C                   ENDIF
035400070222     C                   ENDIF
035500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABLNP
036000070222     C                   Z-ADD     PiVal         VATLNP
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABCTR
036800070222     C                   ENDIF
036900070928     C                   ENDIF
037000071009     C                   MOVEL     *blanks       wDISK             1
037100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037200071009     C                   IF        wDISK <> *blanks
037300070928     C                   SETON                                        50
037400070222     C                   ENDIF
037500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037600111125     C                   SETON                                        41
037700080923     C                   ENDIF
037800070222     C                   ENDIF
037900120706     C*
038000120706     C* valido per nr.colli/peso/volume
038100120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038200120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038300120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038400131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038500120706     C                   SELECT
038600120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
038700120706     C                   SETON                                        51
038800120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
038900120706     C                   SETON                                        52
039000120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039100120706     C                   SETON                                        53
039200131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039300131016     C                   SETON                                        54
039400120706     C                   ENDSL
039500120706     C*
039600071009     C   50              EVAL      VABCTM = '7Q'
039700000801     C*
039800000801     C                   ENDSR
039900131016     C*----------------------------------------------------*
040000131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040100131016     C*----------------------------------------------------*
040200131016     C     REPNSP        BEGSR
040300131016     C*
040400131016     C                   EXSR      INZVAR
040500131016     C                   EXSR      DEFCAM
040600131016     C*
040700131016     C                   SETON                                        60
040800131016     C*
040900131016     C* NSP => Stacco un numeratore da AZNUM
041000131016     C                   clear                   TRUL33DS
041100131016     C                   eval      I33OPE = *zeros
041200131016     C                   eval      I33CNU = 302
041300131016     C                   eval      I33NUM = 1
041400131016     C                   movel     TRUL33DS      KPJBU
041500131016     C                   call      'TRUL33R'
041600131016     C                   parm                    KPJBA
041700131016     C                   movel     KPJBU         TRUL33DS
041800131016     C                   if        O33ERR = *zeros
041900131016     C                   z-add     O33NRF        VABNSP
042000131016     C                   z-add     O33NRF        VATNSP
042100131016     C                   z-add     O33NRF        VABRMN
042200131016     C                   else
042300131016     C                   SETON                                        31
042400131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042500131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042600131016     C                   endif
042700131016     C*
042800131016     C                   ENDSR
042900000801     C*----------------------------------------------------*
043000131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043100000801     C*----------------------------------------------------*
043200010201     C     IMPVAB        BEGSR
043300070530     C*
043400070530     C                   SETOFF                                       3132
043500070928     C*
043600070928     C                   EXSR      DEFCAM
043700070928     C*
043800111031     C                   IF        vlrpoi <> 999
043900070928     C                   MOVE(P)   vlrpoi        VABFGS
044000070928     C                   MOVE(P)   vlrpoi        VATFGS
044100111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044200111031     C                   ENDIF
044300070928     C*
044400070928     C                   MOVEL     datcor        VABAAS
044500070928     C                   MOVEL     datcor        VATAAS
044600070928     C                   MOVE      datcor        VABMGS
044700140328     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
044800131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
044900140306     C     '@':'A'       XLATE     VABRSD        VABRSD
045000131016     C* ==
045100140328     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
045200140328     C                   EVAL      VABLOD=%trim(%subst(vindta:120:25))
045300140328     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
045400140328     C                   EVAL      VABNZD=%trim(%subst(vindta:156:3))
045500140328     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
045600140328     C                   SELECT
045700140328     C                   WHEN      %subst(vindta:210:1) = 'C'
045800140328     C                   EVAL      VABTIC=*blank
045900140328     C                   WHEN      %subst(vindta:210:1) = 'M'
046000140328     C                   EVAL      VABTIC='BM'
046100140328     C                   WHEN      %subst(vindta:210:1) = 'R'
046200140328     C                   EVAL      VABTIC='CM'
046300171110     C                   WHEN      %subst(vindta:210:1) = 'T'
046400171110     C                   EVAL      VABTIC='TM'
046500140328     C                   ENDSL
046600140328     C                   EVAL      w70=%trim(%subst(vindta:306:50))
046700140328     C                   EVAL      VABNOT=%subst(w70:1:35)
046800140328     C                   EVAL      VABNT2=%subst(w70:36:35)
046900140328     C                   IF        %trim(%subst(vindta:407:20)) <> *blank
047000140328     C                   eval      VABTC1 = 'A'
047100140328     C                   endif
047200140306     C*
047300140306     C* Reperisco la provincia dal CAP e dalla località
047400140306     C                   IF        VABCAD <> *blanks AND
047500140306     C                             VABPRD  = *blanks AND
047600140306     C                             VABNZD  = *blanks
047700140306     C                   CLEAR                   TISI95DS
047800140306     C                   EVAL      I95TCN = '3'
047900140306     C                   Z-ADD     datcor        I95DAT
048000140306     C                   EVAL      I95CAP = VABCAD
048100140306     C                   EVAL      I95LOC = VABLOD
048200140306     C                   CALL      'TISI95R'
048300140306     C                   PARM                    TISI95DS
048400140306     C                   EVAL      VABPRD = O95PRV
048500140306     C                   ENDIF
048600131016     C*
048700131016     C* CAD
048800140328     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
048900131016     C                   EXSR      CHKNUM
049000131016     C                   IF        PiInt=*on
049100131016     C                   Z-ADD     PiVal         Num5_0
049200131016     C                   MOVEL     Num5_0        VABCAD
049300131016     C                   ELSE
049400131016     C                   SETON                                        32
049500131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600131016     C                             + ' ' + 'VABCAD'
049700131016     C                   ENDIF
049800140306     C* RMN
049900140328     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
050000131016     C                   EXSR      CHKNUM
050100131016     C                   IF        PiInt=*on
050200140120     C                   Z-ADD     PiVal         VABNSP
050300140120     C                   Z-ADD     PiVal         VABRMN
050400131016     C                   ELSE
050500131016     C                   SETON                                        32
050600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700140306     C                             + ' ' + 'VABRMN'
050800131016     C                   ENDIF
050900140306     C* NSP
051000140328     C                   EVAL      PiStr=%trim(%subst(vindta:35:15))
051100140306     C                   EXSR      CHKNUM
051200140306     C                   IF        PiInt=*on
051300140306     C                   Z-ADD     PiVal         VABNSP
051400140306     C                   Z-ADD     PiVal         VATNSP
051500140306     C                   ELSE
051600140306     C                   SETON                                        32
051700140306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051800140306     C                             + ' ' + 'VABNSP VATNSP VABRMA'
051900140306     C                   ENDIF
052000140120     C* NCL
052100140328     C                   EVAL      PiStr=%trim(%subst(vindta:191:5))
052200140120     C                   EXSR      CHKNUM
052300140120     C                   IF        PiInt=*on
052400140120     C                   Z-ADD     PiVal         VABNCL
052500140120     C                   ELSE
052600140120     C                   SETON                                        32
052700140120     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052800140120     C                             + ' ' + 'VABNCL'
052900140120     C                   ENDIF
053000131016     C* PKB
053100140328     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
053200131016     C                   EXSR      CHKNUM
053300131016     C                   IF        PiNum=*on
053400140328     C* il dato NON ha decimali
053500140328     C                   EVAL      VABPKB = PiVal
053600131016     C                   ELSE
053700131016     C                   SETON                                        32
053800131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053900131016     C                             + ' ' + 'VABPKB'
054000131016     C                   ENDIF
054100140328     C* DCR
054200140328     C                   EVAL      PiStr=%trim(%subst(vindta:11:8))
054300140328     C                   EXSR      CHKNUM
054400140328     C                   IF        PiInt=*on
054500140328     C                   Z-ADD     PiVal         VABDCR
054600140328     C                   ELSE
054700140328     C                   SETON                                        32
054800140328     C                   Z-ADD     *zeros        VABDCR
054900140328     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055000140328     C                             + ' ' + 'VABDCR'
055100140328     C                   ENDIF
055200140328     C* CAS
055300140328     C                   IF        %subst(vindta:232:14)<> *blanks    AND
055400140328     C                             %subst(vindta:232:14)<> *zeros     AND
055500140328     C                             %subst(vindta:232:14)<> '00000000000,00'
055600140328     C                   EVAL      FlgCAS = '1'
055700140328     C                   EVAL      VABVCA = 'EUR'
055800140328     C                   EVAL      PiStr=%trim(%subst(vindta:232:14))
055900140328     C                   EXSR      CHKNUM
056000140328     C                   IF        PiNum=*on
056100140328     C                   Z-ADD(H)  PiVal         VABCAS
056200140328     C                   ELSE
056300140328     C  N51              SETON                                        32
056400140328     C  N51              Z-ADD     *zeros        VABCAS
056500140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
056600140328     C                             + ' ' + 'VABCAS'
056700140328     C                   ENDIF
056800140328     C                   ELSE
056900140328     C                   EVAL      FlgCAS = '0'
057000140328     C                   ENDIF
057100070730     C*
057200070730     C* Considerazioni finali su CBO/CAS
057300121113     C                   IF        FlgCAS <> '0'
057400070730     C                   IF        VABCBO = '1'
057500070730     C                   EVAL      VABCBO = '4'
057600070730     C                   ENDIF
057700070730     C                   IF        VABCBO = '2'
057800070730     C                   EVAL      VABCBO = '6'
057900070730     C                   ENDIF
058000070730     C                   ENDIF
058100020305     C*
058200011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058300011113     C                   EXSR      CHKIMPDIV
058400010202     C*
058500000801     C* Ebbene...
058600000801     C                   ADD       1             §CTRMO
058700070530     C                   IF        *in31 <> *zeros OR
058800070530     C                             *in32 <> *zeros
058900000801     C                   ADD       1             §CTRNO
059000020725     C                   EVAL      x_vinflg = '2'
059100000801     C                   ELSE
059200010201     C                   ADD       1             §CTROKVB
059300000801     C                   ENDIF
059400000801     C*
059500000801     C                   ENDSR
059600131016     C*----------------------------------------------------*
059700131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059800131016     C*----------------------------------------------------*
059900131016     C     WRIVAT_A      BEGSR
060000131016     C*
060100131016     C                   eval      vatAAS = vabAAS
060200131016     C                   eval      vatLNP = vabLNP
060300131016     C                   eval      vatNRS = vabNRS
060400131016     C                   eval      vatNSP = vabNSP
060500131016     C*
060600131016     C* Valorizzo il buffer di scrittura del FIVAT
060700131016     C* - TIPO RECORD "A"
060800131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
060900131016     C                   eval      VATTRC = 'A'
061000131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
061100131016     C                   write     FIVAT000
061200131016     C                   endif
061300131016     C*
061400131016     C                   ENDSR
061500131016     C*----------------------------------------------------*
061600131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061700131016     C*----------------------------------------------------*
061800131016     C     WRIVAT_B      BEGSR
061900131016     C*
062000131016     C                   eval      vatAAS = vabAAS
062100131016     C                   eval      vatLNP = vabLNP
062200131016     C                   eval      vatNRS = vabNRS
062300131016     C                   eval      vatNSP = vabNSP
062400131016     C*
062500131016     C* Valorizzo il buffer di scrittura del FIVAT
062600131016     C* - TIPO RECORD "B"
062700140328     C                   IF        %trim(%subst(vindta:407:20)) <> *blank
062800131016     C                   eval      VATTRC = 'B'
062900140328     C                   eval      VATNOT = %trim(%subst(vindta:407:20))
063000131016     C                   write     FIVAT000
063100131016     C                   endif
063200131016     C*
063300131016     C                   ENDSR
063400131016     C*----------------------------------------------------*
063500131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063600131016     C*----------------------------------------------------*
063700131016     C     WRIVAT_E      BEGSR
063800131016     C*
063900131016     C                   eval      vatAAS = vabAAS
064000131016     C                   eval      vatLNP = vabLNP
064100131016     C                   eval      vatNRS = vabNRS
064200131016     C                   eval      vatNSP = vabNSP
064300131016     C*
064400131016     C* Valorizzo il buffer di scrittura del FIVAT
064500131016     C* - TIPO RECORD "E"
064600140306     C                   if        %trim(%subst(vindta:203:20)) <> *blanks
064700131016     C                   eval      VATTRC = 'E'
064800140306     C                   eval      VATNOT = %trim(%subst(vindta:203:20))
064900131016     C                   write     FIVAT000
065000131016     C                   endif
065100131016     C*
065200131016     C                   ENDSR
065300020904
065400020904
065500010202     C*----------------------------------------------------*
065600131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065700010202     C*----------------------------------------------------*
065800020305     C     PREVAT        BEGSR
065900010202     C*
066000131016     C* Compongo il nome del membro da dare al FIVATWWR
066100010202     C                   eval      parmbr = vlrhdl
066200010202     C                   movel     'M'           parmbr
066300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
066400010202     C                   eval      paropz = '1'
066500010202     C* Effettuo la chiamata al CLLE preposto
066600131017     C                   call(e)   'TITVVTC'
066700010202     C                   parm                    parccm
066800010202     C                   parm                    parmbr
066900010202     C                   parm                    paropz
067000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
067100010202     C                   if        %error
067200010202     C                   movel     '1'           chkcall
067300010202     C                   else
067400010202     C                   movel     '0'           chkcall
067500010202     C                   endif
067600010202     C*
067700010202     C                   ENDSR
067800000801     C*----------------------------------------------------*
067900000801     C*  CONTROLLO NUMERICITA' CAMPI
068000000801     C*----------------------------------------------------*
068100000801     C     CHKNUM        BEGSR
068200081113     C*
068300081113     C                   IF        PiDecChr = *blanks
068400121113     C                   EVAL      PiDecChr = '.'
068500081113     C                   ENDIF
068600081113     C*
068700081113     C                   callp(e)  UBISNUM_Check(PiStr
068800081113     C                                          :PiDecChr
068900081113     C                                          :PiVal
069000081113     C                                          :PiNum
069100081113     C                                          :PiInt)
069200081113     C*
069300000801     C                   IF        %error
069400000801     C                   EVAL      PiInt=*off
069500000801     C                   ENDIF
069600000801     C*
069700000801     C                   ENDSR
069800000801     C***
069900000801
070000011113
070100011113     C*----------------------------------------------------*
070200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070300011113     C*----------------------------------------------------*
070400011113     C     CHKIMPDIV     BEGSR
070500011113     C*
070600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070700011113     C                   Z-ADD     *zeros        wrkDec            9 9
070800011113     C*
070900011113     C* Come prima cosa effettuo considerazioni sulla divisa
071000011113     C                   IF        vabIAS > *zeros
071100011113     C                   IF        vabVAS <> 'EUR'
071200011113     C                   EVAL      vabVAS =  'ITL'
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C                   IF        vabCAS > *zeros
071700011113     C                   IF        vabVCA <> 'EUR'
071800011113     C                   EVAL      vabVCA =  'ITL'
071900011113     C                   ENDIF
072000011113     C                   ENDIF
072100011113     C*
072200011113     C                   IF        vabVMD > *zeros
072300020305     C                   IF        vabVAD <> 'EUR'
072400011113     C                   EVAL      vabVAD =  'ITL'
072500011113     C                   ENDIF
072600011113     C                   ENDIF
072700011113     C*
072800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072900011113     C                   Z-ADD     vabIAS        wrkDec
073000011113     C                   IF        wrkDec > *zeros
073100011113     C                   IF        vabVAS = 'ITL'
073200011113     C                   EVAL      vabIAS = *zeros
073300011113     C                   ENDIF
073400011113     C                   ENDIF
073500011113     C*
073600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073700011113     C                   Z-ADD     vabCAS        wrkDec
073800011113     C                   IF        wrkDec > *zeros
073900011113     C                   IF        vabVCA = 'ITL'
074000011113     C                   EVAL      vabCAS = *zeros
074100011113     C                   ENDIF
074200011113     C                   ENDIF
074300011113     C*
074400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074500011113     C                   Z-ADD     vabVMD        wrkDec
074600011113     C                   IF        wrkDec > *zeros
074700011113     C                   IF        vabVAD = 'ITL'
074800011113     C                   EVAL      vabVMD = *zeros
074900011113     C                   ENDIF
075000011113     C                   ENDIF
075100011113     C*
075200011113     C                   ENDSR
075300011113     C***
075400011113
075500011113
075600000801
075700000801
075800990920      /TITLE Invio dei dati al punto operativo.
075900010202     C     invio         BEGSR
076000990920     C*
076100131016     C* 1° invio FIVAT
076200010201     C                   reset                   dscmz
076300111031     C                   move      wPOI          cmzdst
076400131016     C                   eval      cmzfld = 'FIVATWWR'
076500010201     C                   eval      cmzmbd = vlrhdl
076600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076700021009     C***                if        prmfir = *blanks
076800131016     C                   eval      cmzfla = 'FIVAT00F'
076900131016     C                   eval      cmzmba = 'FIVAT00F'
077000021009     C***                else
077100021009     C***                eval      cmzfla = prmfir
077200021009     C***                eval      cmzmba = prmfir
077300021009     C***                endif
077400010201     C                   eval      cmznrr = *zeros
077500020305     C                   move      §ctrokvt      cmznrr
077600021018     C                   eval      cmzlba = vlrfl1
077700010201     C                   call(e)   'TIS711C'
077800010201     C                   parm                    dscmz
077900010201     C                   parm      *blanks       esito
078000010205     C                   if        %error
078100010205     C                             or cmzerr = '1'
078200010205     C                             or esito  = '1'
078300010205     C                   eval      wrkesito = '3'
078400010205     C                   else
078500010201     C*
078600131016     C* 2° invio FIVAB
078700010201     C                   reset                   dscmz
078800111031     C                   move      wPOI          cmzdst
078900010201     C                   eval      cmzfld = vlrfou
079000010201     C                   eval      cmzmbd = vlrhdl
079100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
079200021009     C***                if        prmfir = *blanks
079300131016     C                   eval      cmzfla = 'FIVAB00F'
079400131016     C                   eval      cmzmba = 'FIVAB00F'
079500021009     C***                else
079600021009     C***                eval      cmzfla = prmfir
079700021009     C***                eval      cmzmba = prmfir
079800021009     C***                endif
079900010201     C                   eval      cmznrr = *zeros
080000010201     C                   move      §ctrokvb      cmznrr
080100021018     C                   eval      cmzlba = vlrfl1
080200010201     C                   call(e)   'TIS711C'
080300010201     C                   parm                    dscmz
080400010201     C                   parm      *blanks       esito
080500010201     C                   if        %error
080600010201     C                             or cmzerr = '1'
080700010201     C                             or esito  = '1'
080800010201     C                   eval      wrkesito = '3'
080900010201     C                   endif
081000010205     C                   endif
081100990920     C*
081200000613     C                   ENDSR
081300000613     C***
081400070411
081500070411     C     *pssr         BEGSR
081600070411     C*
081700070411     C                   if        %open(tivin00r)
081800070411     C                   close     tivin00r
081900070411     C                   endif
082000131016     C                   if        %open(FIVABWWr)
082100131016     C                   close     FIVABWWr
082200120706     C                   endif
082300131016     C                   if        %open(FIVATWWr)
082400131016     C                   close     FIVATWWr
082500070411     C                   endif
082600070411     C*
082700070411     C* Effettuo la chiamata al CLLE preposto
082800131017     C                   call(e)   'TITVVTC'
082900070411     C                   parm                    parccm
083000070411     C                   parm                    parmbr
083100070411     C                   parm      '2'           paropz
083200070411     C*
083300070411     C                   eval      wrkesito = '2'
083400070411     C*
083500070411     C                   seton                                        LR
083600070411     C*
083700070411     C                   ENDSR     '*CANCL'
083800070411     C***
083900070411
084000990910
084100000613     C     *inzsr        BEGSR
084200990910     C*
084300990910     C     *entry        plist
084400990920     C                   parm                    tivlrds
084500990921     C                   parm      wrkesito      esito
084600000724     C                   parm                    prmlit
084700000710     C                   parm                    prmfir
084800000613     C*
084900000830     C* CALCOLA LA DATA CORRENTE
085000120706     C                   time                    wn14             14 0
085100120706     C                   movel     wn14          oracor            6 0          *ORA
085200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
085300100324     C                   eval      datcor = %dec(%date() : *ISO)
085400000830     C*
085500000613     C                   ENDSR
085600000613     C***
