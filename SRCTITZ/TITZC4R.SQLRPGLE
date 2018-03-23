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
002700140411     D depspe          s              7    INZ(*blanks)
002800140411     D curspe          s              7    INZ(*blanks)
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
004400140411     D  VINSPE                        7
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
012700140411     C+ vindta, vinflg, substr(vindta, 20, 7) as sped, rrn(tivin00r)
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
015400140411     C* (uso NSP)
015500140411     C                   EVAL      PiStr=%trim(%subst(vindta:20:7))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300140411     C                   exsr      wrivat_a                                     => carico VAT
016400140328     C                   exsr      wrivat_b                                     => carico VAT
016500140411     C                   exsr      wrivat_i                                     => carico VAT
016600140411     C                   exsr      wrivat_s                                     => carico VAT
016700111125     C                   exsr      inzvar
016800071003     C                   else
016900080923     C*
017000071009     C                   if        wDISK = 'D'
017100140120     C***                exsr      repNSP
017200071009     C                   exsr      impvab
017300111130     C  N31              eval      fivabds_ok = fivabds
017400111130     C  N31              exsr      wrivab
017500140411     C                   exsr      wrivat_a                                     => carico VAT
017600140328     C                   exsr      wrivat_b                                     => carico VAT
017700140411     C                   exsr      wrivat_i                                     => carico VAT
017800140411     C                   exsr      wrivat_s                                     => carico VAT
017900140411     C                   exsr      wrivat_e                                     => carico VAT
018000111125     C                   exsr      inzvar
018100071009     C                   else
018200071009     C*
018300010305     C                   if        depspe = *blanks                             => 1° giro
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500080117     C                   clear                   tivinds
018600140120     C***                exsr      repNSP
018700020305     C                   exsr      impvab
018800111130     C                   z-add     VABNCL        sav_VABNCL
018900111130     C                   z-add     VABPKB        sav_VABPKB
019000111130     C                   z-add     VABVLB        sav_VABVLB
019100140411     C                   exsr      wrivat_a                                     => carico VAT
019200140328     C                   exsr      wrivat_b                                     => carico VAT
019300140411     C                   exsr      wrivat_i                                     => carico VAT
019400140411     C                   exsr      wrivat_s                                     => carico VAT
019500140411     C   50              exsr      wrivat_e                                     => carico VAT
019600010305     C                   else
019700020725     C                   if        depspe <> curspe                             => rottura di spediz
019800111130     C                   z-add     sav_VABNCL    VABNCL
019900111130     C                   z-add     sav_VABPKB    VABPKB
020000111130     C                   z-add     sav_VABVLB    VABVLB
020100111130     C  N31              eval      fivabds_ok = fivabds
020200111125     C  N31              exsr      wrivab
020300111125     C                   exsr      inzvar
020400111125     C                   eval      depspe = curspe                              => memorizz. spediz
020500080117     C                   clear                   tivinds
020600140120     C***                exsr      repNSP
020700020305     C                   exsr      impvab
020800111130     C                   if        wCntRecSpe = *zeros
020900111130     C                   z-add     VABNCL        sav_VABNCL
021000111130     C                   z-add     VABPKB        sav_VABPKB
021100111130     C                   z-add     VABVLB        sav_VABVLB
021200111130     C                   endif
021300111130     C                   add       1             wCntRecSpe
021400140411     C                   exsr      wrivat_a                                     => carico VAT
021500140328     C                   exsr      wrivat_b                                     => carico VAT
021600140411     C                   exsr      wrivat_i                                     => carico VAT
021700140411     C                   exsr      wrivat_s                                     => carico VAT
021800140411     C   50              exsr      wrivat_e                                     => carico VAT
021900020305     C                   else                                                   => x stessa spediz
022000111130     C                   exsr      impvab
022100120706     C   51              add       VABNCL        sav_VABNCL
022200111125     C   51              add       VABPKB        sav_VABPKB
022300111125     C   51              add       VABVLB        sav_VABVLB
022400111125     C   52              z-add     VABNCL        sav_VABNCL
022500111125     C   52              z-add     VABPKB        sav_VABPKB
022600111125     C   52              z-add     VABVLB        sav_VABVLB
022700111125     C   53              add       VABNCL        sav_VABNCL
022800111125     C   53              z-add     VABPKB        sav_VABPKB
022900111125     C   53              z-add     VABVLB        sav_VABVLB
023000131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023100131016     C   54              ADD       VABPKB        sav_VABPKB
023200131016     C   54              ADD       VABVLB        sav_VABVLB
023300111125     C*
023400140411     C                   exsr      wrivat_a                                     => carico VAT
023500140328     C                   exsr      wrivat_b                                     => carico VAT
023600140411     C                   exsr      wrivat_i                                     => carico VAT
023700140411     C                   exsr      wrivat_s                                     => carico VAT
023800140411     C   50              exsr      wrivat_e                                     => carico VAT
023900010305     C                   endif
024000010305     C                   endif
024100010305     C                   endif
024200071003     C                   endif
024300071009     C                   endif
024400000905     C*
024500000905     C                   else
024600020725     C                   eval      x_vinflg = '1'
024700000905     C                   endif
024800000905     C*
024900020725     C     VINRRN        chain     tivin000
025000020725     C                   if        %found(tivin00r)
025100020725     C                   eval      y_vinflg = x_vinflg
025200020725     C                   eval      y_vinmsg = x_vinmsg
025300020725     C                   update    tivin000
025400020725     C                   endif
025500020725     C*
025600020725     C/EXEC SQL
025700070530     C+ Fetch C1 into :INPUT_DS
025800020725     C/END-EXEC
025900020725     C*
026000020725     C                   enddo
026100020725     C*
026200020725     C/EXEC SQL
026300020725     C+ close C1
026400020725     C/END-EXEC
026500000905     C*
026600020305     C* Scarico i VAB rimasti "in sospeso"
026700071009     C                   if        wDISK = 'C'
026800140307     C                   z-add     sav_VABNCL    VABNCL
026900140307     C                   z-add     sav_VABPKB    VABPKB
027000140307     C                   z-add     sav_VABVLB    VABVLB
027100111130     C  N31              eval      fivabds_ok = fivabds
027200111130     C  N31              exsr      wrivab
027300071009     C                   endif
027400010202     C*
027500010202     C                   endif
027600990910
027700990910     C* Se non ci sono record con errori ...
027800000710     C                   if        §ctrno = 0
027900990910     C* ... restituisco esito OK.
028000990921     C                   eval      wrkesito = '0'
028100990910     C                   else
028200010201     C                   if        §ctrokvb > 0
028300990921     C                   eval      wrkesito = '1'
028400000710     C                   else
028500000710     C                   eval      wrkesito = '2'
028600990910     C                   endif
028700000710     C                   endif
028800990910     C*
028900990914     C                   if        %open(tivin00r)
029000990908     C                   close     tivin00r
029100990914     C                   endif
029200131016     C                   if        %open(FIVABWWr)
029300131016     C                   close     FIVABWWr
029400120706     C                   endif
029500131016     C                   if        %open(FIVATWWr)
029600131016     C                   close     FIVATWWr
029700010201     C                   endif
029800990910     C*
029900111031     C                   if        §ctrokvb > 0 or
030000111031     C                             §ctrokvt > 0
030100111031     C                             and wPOI <> *zeros
030200010202     C                   exsr      invio
030300990920     C                   endif
030400990920     C*
030500910830     C                   ENDSR
030600000613     C***
030700010305
030800010305     C*----------------------------------------------------*
030900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031000010305     C*----------------------------------------------------*
031100020305     C     WRIVAB        BEGSR
031200070730     C*
031300071003     C* Considerazioni finali
031400111125     C                   if        ok_VABRMA = *blanks
031500111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031600071003     C                   endif
031700071003     C*
031800131016     C                   write     FIVAB000                                     => scarico il VAB
031900010305     C*
032000010305     C                   ENDSR
032100990920
032200000801     C*----------------------------------------------------*
032300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032400000801     C*----------------------------------------------------*
032500010201     C     INZVAR        BEGSR
032600000801     C*
032700010201     C                   Z-ADD     *zeros        Num5_0
032800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032900020725     C                   MOVEL     '0'           FlgCAS            1
033000111125     C*
033100111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033200111130     C*
033300111125     C                   CLEAR                   FIVABDS
033400111130     C                   CLEAR                   FIVABDS_OK
033500111130     C                   CLEAR                   FIVABDS_SAV
033600131016     C                   CLEAR                   FIVAB000
033700131016     C                   CLEAR                   FIVAT000
033800000801     C*
033900000801     C                   ENDSR
034000000801     C*----------------------------------------------------*
034100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034200000801     C*----------------------------------------------------*
034300020904     C     DEFCAM        BEGSR
034400080923     C*
034500111125     C                   SETOFF                                       50
034600111125     C                   SETOFF                                       515253
034700131016     C                   SETOFF                                       54
034800070730     C* Imposto i valori di default...
034900140411     C                   EVAL      VABCCM = 0880988
035000140411     C                   EVAL      VATCCM = 0880988
035100140411     C                   EVAL      VABLNP = 088
035200140411     C                   EVAL      VATLNP = 088
035300140328     C                   EVAL      VABCTR = 000
035400131016     C                   EVAL      VABCBO = '1'
035500070222     C* ... e poi verifico se sono stati passati come parametri
035600070222     C                   IF        vlrppt > *blanks
035700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABCCM
036200070222     C                   Z-ADD     PiVal         VATCCM
036300070222     C                   ENDIF
036400070222     C                   ENDIF
036500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036700070222     C                   EXSR      CHKNUM
036800070222     C                   IF        PiInt=*on
036900070222     C                   Z-ADD     PiVal         VABLNP
037000070222     C                   Z-ADD     PiVal         VATLNP
037100070222     C                   ENDIF
037200070222     C                   ENDIF
037300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037500070222     C                   EXSR      CHKNUM
037600070222     C                   IF        PiInt=*on
037700070222     C                   Z-ADD     PiVal         VABCTR
037800070222     C                   ENDIF
037900070928     C                   ENDIF
038000071009     C                   MOVEL     *blanks       wDISK             1
038100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038200071009     C                   IF        wDISK <> *blanks
038300070928     C                   SETON                                        50
038400070222     C                   ENDIF
038500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038600111125     C                   SETON                                        41
038700080923     C                   ENDIF
038800070222     C                   ENDIF
038900120706     C*
039000120706     C* valido per nr.colli/peso/volume
039100120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039200120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039300120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039400131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039500120706     C                   SELECT
039600120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039700120706     C                   SETON                                        51
039800120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039900120706     C                   SETON                                        52
040000120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040100120706     C                   SETON                                        53
040200131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040300131016     C                   SETON                                        54
040400120706     C                   ENDSL
040500120706     C*
040600071009     C   50              EVAL      VABCTM = '7Q'
040700000801     C*
040800000801     C                   ENDSR
040900131016     C*----------------------------------------------------*
041000131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041100131016     C*----------------------------------------------------*
041200131016     C     REPNSP        BEGSR
041300131016     C*
041400131016     C                   EXSR      INZVAR
041500131016     C                   EXSR      DEFCAM
041600131016     C*
041700131016     C                   SETON                                        60
041800131016     C*
041900131016     C* NSP => Stacco un numeratore da AZNUM
042000131016     C                   clear                   TRUL33DS
042100131016     C                   eval      I33OPE = *zeros
042200131016     C                   eval      I33CNU = 302
042300131016     C                   eval      I33NUM = 1
042400131016     C                   movel     TRUL33DS      KPJBU
042500131016     C                   call      'TRUL33R'
042600131016     C                   parm                    KPJBA
042700131016     C                   movel     KPJBU         TRUL33DS
042800131016     C                   if        O33ERR = *zeros
042900131016     C                   z-add     O33NRF        VABNSP
043000131016     C                   z-add     O33NRF        VATNSP
043100131016     C                   z-add     O33NRF        VABRMN
043200131016     C                   else
043300131016     C                   SETON                                        31
043400131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
043600131016     C                   endif
043700131016     C*
043800131016     C                   ENDSR
043900000801     C*----------------------------------------------------*
044000131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044100000801     C*----------------------------------------------------*
044200010201     C     IMPVAB        BEGSR
044300070530     C*
044400070530     C                   SETOFF                                       3132
044500070928     C*
044600070928     C                   EXSR      DEFCAM
044700070928     C*
044800111031     C                   IF        vlrpoi <> 999
044900070928     C                   MOVE(P)   vlrpoi        VABFGS
045000070928     C                   MOVE(P)   vlrpoi        VATFGS
045100111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045200111031     C                   ENDIF
045300070928     C*
045400070928     C                   MOVEL     datcor        VABAAS
045500070928     C                   MOVEL     datcor        VATAAS
045600070928     C                   MOVE      datcor        VABMGS
045700140411     C                   EVAL      w70=%trim(%subst(vindta:29:70))
045800140411     C                   EVAL      VABRSD=%subst(w70:1:35)
045900140411     C                   EVAL      VABRD2=%subst(w70:36:35)
046000131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100140306     C     '@':'A'       XLATE     VABRSD        VABRSD
046200131016     C* ==
046300140411     C                   EVAL      VABCBO=%trim(%subst(vindta:27:2))
046400140411     C                   EVAL      VABIND=%trim(%subst(vindta:99:35))
046500140411     C                   EVAL      VABLOD=%trim(%subst(vindta:143:35))
046600140411     C                   EVAL      VABPRD=%trim(%subst(vindta:178:2))
046700140411     C                   EVAL      VABNZD=%trim(%subst(vindta:180:3))
046800140411     C                   EVAL      VABGC1=%trim(%subst(vindta:183:2))
046900140411     C                   EVAL      VABGC2=%trim(%subst(vindta:185:2))
047000140411     C                   EVAL      VABTSP=%trim(%subst(vindta:191:1))
047100140411     C                   EVAL      VABNAS=%trim(%subst(vindta:210:15))
047200140411     C                   EVAL      VABRMA=%trim(%subst(vindta:255:2))
047300140411     C                   EVAL      VABRMA=%trim(%subst(vindta:261:15))
047400140411     C                   EVAL      w70=%trim(%subst(vindta:311:70))
047500140328     C                   EVAL      VABNOT=%subst(w70:1:35)
047600140328     C                   EVAL      VABNT2=%subst(w70:36:35)
047700140704     C                   EVAL      VABTC1=%trim(%subst(vindta:520:1))
047800151027     C                   EVAL      VABCAD=%trim(%subst(vindta:134:9))
047900151104     C                   EVAL      VABFFD=%trim(%subst(vindta:527:1))
048000151104     C                   EVAL      VABTCR=%trim(%subst(vindta:536:1))
048100131016     C*
048200151104     C* DCR
048300151104     C                   IF        %subst(vindta:528:8) <> *blank and
048400151104     C                             %subst(vindta:528:8) <> *zeros
048500151104     C                   EVAL      PiStr=%trim(%subst(vindta:528:8))
048600151104     C                   EXSR      CHKNUM
048700151104     C                   IF        PiInt=*on
048800151104     C                   Z-ADD     PiVal         VABDCR
048900151104     C                   ELSE
049000151104     C                   SETON                                        32
049100151104     C                   Z-ADD     *zeros        VABDCR
049200151104     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300151104     C                             + ' ' + 'VABDCR'
049400151104     C                   ENDIF
049500151104     C                   ENDIF
049600131016     C* CAD
049700151027     C***                EVAL      PiStr=%trim(%subst(vindta:134:9))
049800151027     C***                EXSR      CHKNUM
049900151027     C***                IF        PiInt=*on
050000151027     C***                Z-ADD     PiVal         Num5_0
050100151027     C***                MOVEL     Num5_0        VABCAD
050200151027     C***                ELSE
050300151027     C***                SETON                                        32
050400151027     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
050500151027     C***                          + ' ' + 'VABCAD'
050600151027     C***                ENDIF
050700140411     C* IAS
050800140411     C                   IF        %subst(vindta:193:14)<> *blanks    AND
050900140411     C                             %subst(vindta:193:14)<> *zeros     AND
051000140411     C                             %subst(vindta:193:14)<> '0000000000,000'
051100140411     C                   EVAL      VABVAS = %trim(%subst(vindta:207:3))
051200140411     C                   EVAL      PiStr=%trim(%subst(vindta:193:14))
051300140411     C                   EXSR      CHKNUM
051400140411     C                   IF        PiNum=*on
051500140411     C                   Z-ADD(H)  PiVal         VABIAS
051600140411     C                   ELSE
051700140411     C  N51              SETON                                        32
051800140411     C  N51              Z-ADD     *zeros        VABIAS
051900140411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052000140411     C                             + ' ' + 'VABIAS'
052100140411     C                   ENDIF
052200140411     C                   ENDIF
052300140328     C* CAS
052400140411     C                   IF        %subst(vindta:241:14)<> *blanks    AND
052500140411     C                             %subst(vindta:241:14)<> *zeros     AND
052600140411     C                             %subst(vindta:241:14)<> '0000000000,000'
052700140328     C                   EVAL      FlgCAS = '1'
052800140328     C                   EVAL      VABVCA = 'EUR'
052900140411     C                   EVAL      PiStr=%trim(%subst(vindta:241:14))
053000140328     C                   EXSR      CHKNUM
053100140328     C                   IF        PiNum=*on
053200140328     C                   Z-ADD(H)  PiVal         VABCAS
053300140328     C                   ELSE
053400140328     C  N51              SETON                                        32
053500140328     C  N51              Z-ADD     *zeros        VABCAS
053600140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053700140328     C                             + ' ' + 'VABCAS'
053800140328     C                   ENDIF
053900140328     C                   ELSE
054000140328     C                   EVAL      FlgCAS = '0'
054100140328     C                   ENDIF
054200140411     C* RMN
054300140411     C                   EVAL      PiStr=%trim(%subst(vindta:261:15))
054400140411     C                   EXSR      CHKNUM
054500140411     C                   IF        PiInt=*on
054600140411     C                   Z-ADD     PiVal         VABRMN
054700140411     C                   ELSE
054800140411     C                   SETON                                        32
054900140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055000140411     C                             + ' ' + 'VABRMN'
055100140411     C                   ENDIF
055200140411     C* NSP
055300140411     C                   EVAL      PiStr=%trim(%subst(vindta:20:7))
055400140411     C                   EXSR      CHKNUM
055500140411     C                   IF        PiInt=*on
055600140411     C                   Z-ADD     PiVal         VABNSP
055700140411     C                   Z-ADD     PiVal         VATNSP
055800140411     C                   ELSE
055900140411     C                   SETON                                        32
056000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056100140411     C                             + ' ' + 'VABNSP VATNSP       '
056200140411     C                   ENDIF
056300140411     C* NCL
056400140411     C                   EVAL      PiStr=%trim(%subst(vindta:226:5))
056500140411     C                   EXSR      CHKNUM
056600140411     C                   IF        PiInt=*on
056700140411     C                   Z-ADD     PiVal         VABNCL
056800140411     C                   ELSE
056900140411     C                   SETON                                        32
057000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057100140411     C                             + ' ' + 'VABNCL'
057200140411     C                   ENDIF
057300140411     C* PKB
057400140411     C                   EVAL      PiStr=%trim(%subst(vindta:232:8))
057500140411     C                   EXSR      CHKNUM
057600140411     C                   IF        PiNum=*on
057700140411     C                   EVAL      VABPKB = PiVal
057800140411     C                   ELSE
057900140411     C                   SETON                                        32
058000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058100140411     C                             + ' ' + 'VABPKB'
058200140411     C                   ENDIF
058300140902     C* VLB
058400140902     C                   IF        %subst(vindta:521:6) <> *blanks
058500140902     C                   EVAL      PiStr=%trim(%subst(vindta:521:6))
058600140902     C                   EXSR      CHKNUM
058700140902     C                   IF        PiNum=*on
058800140902     C                   EVAL      VABVLB = PiVal
058900140902     C                   ELSE
059000140902     C                   SETON                                        32
059100140902     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059200140902     C                             + ' ' + 'VABVLB'
059300140902     C                   ENDIF
059400140902     C                   ENDIF
059500140411     C*
059600140411     C* Reperisco la provincia dal CAP e dalla località
059700140411     C                   IF        VABCAD <> *blanks AND
059800140411     C                             VABPRD  = *blanks AND
059900140411     C                             VABNZD  = *blanks
060000140411     C                   CLEAR                   TISI95DS
060100140411     C                   EVAL      I95TCN = '3'
060200140411     C                   Z-ADD     datcor        I95DAT
060300140411     C                   EVAL      I95CAP = VABCAD
060400140411     C                   EVAL      I95LOC = VABLOD
060500140411     C                   CALL      'TISI95R'
060600140411     C                   PARM                    TISI95DS
060700140411     C                   EVAL      VABPRD = O95PRV
060800140411     C                   ENDIF
060900070730     C*
061000070730     C* Considerazioni finali su CBO/CAS
061100121113     C                   IF        FlgCAS <> '0'
061200070730     C                   IF        VABCBO = '1'
061300070730     C                   EVAL      VABCBO = '4'
061400070730     C                   ENDIF
061500070730     C                   IF        VABCBO = '2'
061600070730     C                   EVAL      VABCBO = '6'
061700070730     C                   ENDIF
061800070730     C                   ENDIF
061900020305     C*
062000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
062100011113     C                   EXSR      CHKIMPDIV
062200010202     C*
062300000801     C* Ebbene...
062400000801     C                   ADD       1             §CTRMO
062500070530     C                   IF        *in31 <> *zeros OR
062600070530     C                             *in32 <> *zeros
062700000801     C                   ADD       1             §CTRNO
062800020725     C                   EVAL      x_vinflg = '2'
062900000801     C                   ELSE
063000010201     C                   ADD       1             §CTROKVB
063100000801     C                   ENDIF
063200000801     C*
063300000801     C                   ENDSR
063400140411     C*----------------------------------------------------*
063500140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
063600140411     C*----------------------------------------------------*
063700140411     C     WRIVAT_A      BEGSR
063800140411     C*
063900140411     C                   eval      vatAAS = vabAAS
064000140411     C                   eval      vatLNP = vabLNP
064100140411     C                   eval      vatNRS = vabNRS
064200140411     C                   eval      vatNSP = vabNSP
064300140411     C*
064400140411     C* Valorizzo il buffer di scrittura del FIVAT
064500140411     C* - TIPO RECORD "A"
064600140411     C                   if        %trim(%subst(vindta:399:35)) <> *blanks
064700140411     C                   eval      VATTRC = 'A'
064800140411     C                   eval      VATNOT = %trim(%subst(vindta:399:35))
064900140411     C                   write     FIVAT000
065000140411     C                   endif
065100140411     C*
065200140411     C                   ENDSR
065300140411     C*----------------------------------------------------*
065400140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065500140411     C*----------------------------------------------------*
065600140411     C     WRIVAT_B      BEGSR
065700140411     C*
065800140411     C                   eval      vatAAS = vabAAS
065900140411     C                   eval      vatLNP = vabLNP
066000140411     C                   eval      vatNRS = vabNRS
066100140411     C                   eval      vatNSP = vabNSP
066200140411     C*
066300140411     C* Valorizzo il buffer di scrittura del FIVAT
066400140411     C* - TIPO RECORD "B"
066500140411     C                   IF        %trim(%subst(vindta:383:16)) <> *blank
066600140411     C                   eval      VATTRC = 'B'
066700140411     C                   eval      VATNOT = %trim(%subst(vindta:383:16))
066800140411     C                   write     FIVAT000
066900140411     C                   endif
067000140411     C*
067100140411     C                   ENDSR
067200140411     C*----------------------------------------------------*
067300140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
067400140411     C*----------------------------------------------------*
067500140411     C     WRIVAT_I      BEGSR
067600140411     C*
067700140411     C                   eval      vatAAS = vabAAS
067800140411     C                   eval      vatLNP = vabLNP
067900140411     C                   eval      vatNRS = vabNRS
068000140411     C                   eval      vatNSP = vabNSP
068100140411     C*
068200140411     C* Valorizzo il buffer di scrittura del FIVAT
068300140411     C* - TIPO RECORD "I"
068400140411     C                   EVAL      w70=%trim(%subst(vindta:434:70))
068500140411     C                   if        %subst(w70:1:35) <> *blanks
068600140411     C                   eval      VATTRC = 'I'
068700140411     C                   eval      VATNOT = %trim(%subst(w70:1:35))
068800140411     C                   write     FIVAT000
068900140411     C                   endif
069000140411     C                   if        %subst(w70:36:35) <> *blanks
069100140411     C                   eval      VATTRC = 'J'
069200140411     C                   eval      VATNOT = %trim(%subst(w70:36:35))
069300140411     C                   write     FIVAT000
069400140411     C                   endif
069500140411     C*
069600140411     C                   ENDSR
069700140411     C*----------------------------------------------------*
069800140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
069900140411     C*----------------------------------------------------*
070000140411     C     WRIVAT_S      BEGSR
070100140411     C*
070200140411     C                   eval      vatAAS = vabAAS
070300140411     C                   eval      vatLNP = vabLNP
070400140411     C                   eval      vatNRS = vabNRS
070500140411     C                   eval      vatNSP = vabNSP
070600140411     C*
070700140411     C* Valorizzo il buffer di scrittura del FIVAT
070800140411     C* - TIPO RECORD "S"
070900140411     C                   IF        %trim(%subst(vindta:504:16)) <> *blank
071000140411     C                   eval      VATTRC = 'S'
071100140411     C                   eval      VATNOT = %trim(%subst(vindta:504:16))
071200140411     C                   EVAL      %subst(VATNOT:17:2) = 'SS'
071300140411     C                   write     FIVAT000
071400140411     C                   endif
071500140411     C*
071600140411     C                   ENDSR
071700140411     C*----------------------------------------------------*
071800140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
071900140411     C*----------------------------------------------------*
072000140411     C     WRIVAT_E      BEGSR
072100140411     C*
072200140411     C                   eval      vatAAS = vabAAS
072300140411     C                   eval      vatLNP = vabLNP
072400140411     C                   eval      vatNRS = vabNRS
072500140411     C                   eval      vatNSP = vabNSP
072600140411     C*
072700140411     C* Valorizzo il buffer di scrittura del FIVAT
072800140411     C* - TIPO RECORD "E"
072900140411     C                   if        %trim(%subst(vindta:276:35)) <> *blanks
073000140411     C                   eval      VATTRC = 'E'
073100140411     C                   EVAL      VATNOT = %trim(%subst(vindta:276:35))
073200140411     C                   write     FIVAT000
073300140411     C                   endif
073400140411     C*
073500140411     C                   ENDSR
073600020904
073700020904
073800010202     C*----------------------------------------------------*
073900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
074000010202     C*----------------------------------------------------*
074100020305     C     PREVAT        BEGSR
074200010202     C*
074300131016     C* Compongo il nome del membro da dare al FIVATWWR
074400010202     C                   eval      parmbr = vlrhdl
074500010202     C                   movel     'M'           parmbr
074600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
074700010202     C                   eval      paropz = '1'
074800010202     C* Effettuo la chiamata al CLLE preposto
074900131017     C                   call(e)   'TITVVTC'
075000010202     C                   parm                    parccm
075100010202     C                   parm                    parmbr
075200010202     C                   parm                    paropz
075300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
075400010202     C                   if        %error
075500010202     C                   movel     '1'           chkcall
075600010202     C                   else
075700010202     C                   movel     '0'           chkcall
075800010202     C                   endif
075900010202     C*
076000010202     C                   ENDSR
076100000801     C*----------------------------------------------------*
076200000801     C*  CONTROLLO NUMERICITA' CAMPI
076300000801     C*----------------------------------------------------*
076400000801     C     CHKNUM        BEGSR
076500081113     C*
076600081113     C                   IF        PiDecChr = *blanks
076700140411     C                   EVAL      PiDecChr = ','
076800081113     C                   ENDIF
076900081113     C*
077000081113     C                   callp(e)  UBISNUM_Check(PiStr
077100081113     C                                          :PiDecChr
077200081113     C                                          :PiVal
077300081113     C                                          :PiNum
077400081113     C                                          :PiInt)
077500081113     C*
077600000801     C                   IF        %error
077700000801     C                   EVAL      PiInt=*off
077800000801     C                   ENDIF
077900000801     C*
078000000801     C                   ENDSR
078100000801     C***
078200000801
078300011113
078400011113     C*----------------------------------------------------*
078500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
078600011113     C*----------------------------------------------------*
078700011113     C     CHKIMPDIV     BEGSR
078800011113     C*
078900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
079000011113     C                   Z-ADD     *zeros        wrkDec            9 9
079100011113     C*
079200011113     C* Come prima cosa effettuo considerazioni sulla divisa
079300011113     C                   IF        vabIAS > *zeros
079400011113     C                   IF        vabVAS <> 'EUR'
079500011113     C                   EVAL      vabVAS =  'ITL'
079600011113     C                   ENDIF
079700011113     C                   ENDIF
079800011113     C*
079900011113     C                   IF        vabCAS > *zeros
080000011113     C                   IF        vabVCA <> 'EUR'
080100011113     C                   EVAL      vabVCA =  'ITL'
080200011113     C                   ENDIF
080300011113     C                   ENDIF
080400011113     C*
080500011113     C                   IF        vabVMD > *zeros
080600020305     C                   IF        vabVAD <> 'EUR'
080700011113     C                   EVAL      vabVAD =  'ITL'
080800011113     C                   ENDIF
080900011113     C                   ENDIF
081000011113     C*
081100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
081200011113     C                   Z-ADD     vabIAS        wrkDec
081300011113     C                   IF        wrkDec > *zeros
081400011113     C                   IF        vabVAS = 'ITL'
081500011113     C                   EVAL      vabIAS = *zeros
081600011113     C                   ENDIF
081700011113     C                   ENDIF
081800011113     C*
081900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
082000011113     C                   Z-ADD     vabCAS        wrkDec
082100011113     C                   IF        wrkDec > *zeros
082200011113     C                   IF        vabVCA = 'ITL'
082300011113     C                   EVAL      vabCAS = *zeros
082400011113     C                   ENDIF
082500011113     C                   ENDIF
082600011113     C*
082700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
082800011113     C                   Z-ADD     vabVMD        wrkDec
082900011113     C                   IF        wrkDec > *zeros
083000011113     C                   IF        vabVAD = 'ITL'
083100011113     C                   EVAL      vabVMD = *zeros
083200011113     C                   ENDIF
083300011113     C                   ENDIF
083400011113     C*
083500011113     C                   ENDSR
083600011113     C***
083700011113
083800011113
083900000801
084000000801
084100990920      /TITLE Invio dei dati al punto operativo.
084200010202     C     invio         BEGSR
084300990920     C*
084400131016     C* 1° invio FIVAT
084500010201     C                   reset                   dscmz
084600111031     C                   move      wPOI          cmzdst
084700131016     C                   eval      cmzfld = 'FIVATWWR'
084800010201     C                   eval      cmzmbd = vlrhdl
084900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085000021009     C***                if        prmfir = *blanks
085100131016     C                   eval      cmzfla = 'FIVAT00F'
085200131016     C                   eval      cmzmba = 'FIVAT00F'
085300021009     C***                else
085400021009     C***                eval      cmzfla = prmfir
085500021009     C***                eval      cmzmba = prmfir
085600021009     C***                endif
085700010201     C                   eval      cmznrr = *zeros
085800020305     C                   move      §ctrokvt      cmznrr
085900021018     C                   eval      cmzlba = vlrfl1
086000010201     C                   call(e)   'TIS711C'
086100010201     C                   parm                    dscmz
086200010201     C                   parm      *blanks       esito
086300010205     C                   if        %error
086400010205     C                             or cmzerr = '1'
086500010205     C                             or esito  = '1'
086600010205     C                   eval      wrkesito = '3'
086700010205     C                   else
086800010201     C*
086900131016     C* 2° invio FIVAB
087000010201     C                   reset                   dscmz
087100111031     C                   move      wPOI          cmzdst
087200010201     C                   eval      cmzfld = vlrfou
087300010201     C                   eval      cmzmbd = vlrhdl
087400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
087500021009     C***                if        prmfir = *blanks
087600131016     C                   eval      cmzfla = 'FIVAB00F'
087700131016     C                   eval      cmzmba = 'FIVAB00F'
087800021009     C***                else
087900021009     C***                eval      cmzfla = prmfir
088000021009     C***                eval      cmzmba = prmfir
088100021009     C***                endif
088200010201     C                   eval      cmznrr = *zeros
088300010201     C                   move      §ctrokvb      cmznrr
088400021018     C                   eval      cmzlba = vlrfl1
088500010201     C                   call(e)   'TIS711C'
088600010201     C                   parm                    dscmz
088700010201     C                   parm      *blanks       esito
088800010201     C                   if        %error
088900010201     C                             or cmzerr = '1'
089000010201     C                             or esito  = '1'
089100010201     C                   eval      wrkesito = '3'
089200010201     C                   endif
089300010205     C                   endif
089400990920     C*
089500000613     C                   ENDSR
089600000613     C***
089700070411
089800070411     C     *pssr         BEGSR
089900070411     C*
090000070411     C                   if        %open(tivin00r)
090100070411     C                   close     tivin00r
090200070411     C                   endif
090300131016     C                   if        %open(FIVABWWr)
090400131016     C                   close     FIVABWWr
090500120706     C                   endif
090600131016     C                   if        %open(FIVATWWr)
090700131016     C                   close     FIVATWWr
090800070411     C                   endif
090900070411     C*
091000070411     C* Effettuo la chiamata al CLLE preposto
091100131017     C                   call(e)   'TITVVTC'
091200070411     C                   parm                    parccm
091300070411     C                   parm                    parmbr
091400070411     C                   parm      '2'           paropz
091500070411     C*
091600070411     C                   eval      wrkesito = '2'
091700070411     C*
091800070411     C                   seton                                        LR
091900070411     C*
092000070411     C                   ENDSR     '*CANCL'
092100070411     C***
092200070411
092300990910
092400000613     C     *inzsr        BEGSR
092500990910     C*
092600990910     C     *entry        plist
092700990920     C                   parm                    tivlrds
092800990921     C                   parm      wrkesito      esito
092900000724     C                   parm                    prmlit
093000000710     C                   parm                    prmfir
093100000613     C*
093200000830     C* CALCOLA LA DATA CORRENTE
093300120706     C                   time                    wn14             14 0
093400120706     C                   movel     wn14          oracor            6 0          *ORA
093500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
093600100324     C                   eval      datcor = %dec(%date() : *ISO)
093700000830     C*
093800000613     C                   ENDSR
093900000613     C***
