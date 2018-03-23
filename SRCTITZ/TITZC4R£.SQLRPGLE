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
047900131016     C*
048000131016     C* CAD
048100151027     C***                EVAL      PiStr=%trim(%subst(vindta:134:9))
048200151027     C***                EXSR      CHKNUM
048300151027     C***                IF        PiInt=*on
048400151027     C***                Z-ADD     PiVal         Num5_0
048500151027     C***                MOVEL     Num5_0        VABCAD
048600151027     C***                ELSE
048700151027     C***                SETON                                        32
048800151027     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
048900151027     C***                          + ' ' + 'VABCAD'
049000151027     C***                ENDIF
049100140411     C* IAS
049200140411     C                   IF        %subst(vindta:193:14)<> *blanks    AND
049300140411     C                             %subst(vindta:193:14)<> *zeros     AND
049400140411     C                             %subst(vindta:193:14)<> '0000000000,000'
049500140411     C                   EVAL      VABVAS = %trim(%subst(vindta:207:3))
049600140411     C                   EVAL      PiStr=%trim(%subst(vindta:193:14))
049700140411     C                   EXSR      CHKNUM
049800140411     C                   IF        PiNum=*on
049900140411     C                   Z-ADD(H)  PiVal         VABIAS
050000140411     C                   ELSE
050100140411     C  N51              SETON                                        32
050200140411     C  N51              Z-ADD     *zeros        VABIAS
050300140411     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
050400140411     C                             + ' ' + 'VABIAS'
050500140411     C                   ENDIF
050600140411     C                   ENDIF
050700140328     C* CAS
050800140411     C                   IF        %subst(vindta:241:14)<> *blanks    AND
050900140411     C                             %subst(vindta:241:14)<> *zeros     AND
051000140411     C                             %subst(vindta:241:14)<> '0000000000,000'
051100140328     C                   EVAL      FlgCAS = '1'
051200140328     C                   EVAL      VABVCA = 'EUR'
051300140411     C                   EVAL      PiStr=%trim(%subst(vindta:241:14))
051400140328     C                   EXSR      CHKNUM
051500140328     C                   IF        PiNum=*on
051600140328     C                   Z-ADD(H)  PiVal         VABCAS
051700140328     C                   ELSE
051800140328     C  N51              SETON                                        32
051900140328     C  N51              Z-ADD     *zeros        VABCAS
052000140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
052100140328     C                             + ' ' + 'VABCAS'
052200140328     C                   ENDIF
052300140328     C                   ELSE
052400140328     C                   EVAL      FlgCAS = '0'
052500140328     C                   ENDIF
052600140411     C* RMN
052700140411     C                   EVAL      PiStr=%trim(%subst(vindta:261:15))
052800140411     C                   EXSR      CHKNUM
052900140411     C                   IF        PiInt=*on
053000140411     C                   Z-ADD     PiVal         VABRMN
053100140411     C                   ELSE
053200140411     C                   SETON                                        32
053300140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053400140411     C                             + ' ' + 'VABRMN'
053500140411     C                   ENDIF
053600140411     C* NSP
053700140411     C                   EVAL      PiStr=%trim(%subst(vindta:20:7))
053800140411     C                   EXSR      CHKNUM
053900140411     C                   IF        PiInt=*on
054000140411     C                   Z-ADD     PiVal         VABNSP
054100140411     C                   Z-ADD     PiVal         VATNSP
054200140411     C                   ELSE
054300140411     C                   SETON                                        32
054400140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500140411     C                             + ' ' + 'VABNSP VATNSP       '
054600140411     C                   ENDIF
054700140411     C* NCL
054800140411     C                   EVAL      PiStr=%trim(%subst(vindta:226:5))
054900140411     C                   EXSR      CHKNUM
055000140411     C                   IF        PiInt=*on
055100140411     C                   Z-ADD     PiVal         VABNCL
055200140411     C                   ELSE
055300140411     C                   SETON                                        32
055400140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055500140411     C                             + ' ' + 'VABNCL'
055600140411     C                   ENDIF
055700140411     C* PKB
055800140411     C                   EVAL      PiStr=%trim(%subst(vindta:232:8))
055900140411     C                   EXSR      CHKNUM
056000140411     C                   IF        PiNum=*on
056100140411     C                   EVAL      VABPKB = PiVal
056200140411     C                   ELSE
056300140411     C                   SETON                                        32
056400140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056500140411     C                             + ' ' + 'VABPKB'
056600140411     C                   ENDIF
056700140902     C* VLB
056800140902     C                   IF        %subst(vindta:521:6) <> *blanks
056900140902     C                   EVAL      PiStr=%trim(%subst(vindta:521:6))
057000140902     C                   EXSR      CHKNUM
057100140902     C                   IF        PiNum=*on
057200140902     C                   EVAL      VABVLB = PiVal
057300140902     C                   ELSE
057400140902     C                   SETON                                        32
057500140902     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057600140902     C                             + ' ' + 'VABVLB'
057700140902     C                   ENDIF
057800140902     C                   ENDIF
057900140411     C*
058000140411     C* Reperisco la provincia dal CAP e dalla località
058100140411     C                   IF        VABCAD <> *blanks AND
058200140411     C                             VABPRD  = *blanks AND
058300140411     C                             VABNZD  = *blanks
058400140411     C                   CLEAR                   TISI95DS
058500140411     C                   EVAL      I95TCN = '3'
058600140411     C                   Z-ADD     datcor        I95DAT
058700140411     C                   EVAL      I95CAP = VABCAD
058800140411     C                   EVAL      I95LOC = VABLOD
058900140411     C                   CALL      'TISI95R'
059000140411     C                   PARM                    TISI95DS
059100140411     C                   EVAL      VABPRD = O95PRV
059200140411     C                   ENDIF
059300070730     C*
059400070730     C* Considerazioni finali su CBO/CAS
059500121113     C                   IF        FlgCAS <> '0'
059600070730     C                   IF        VABCBO = '1'
059700070730     C                   EVAL      VABCBO = '4'
059800070730     C                   ENDIF
059900070730     C                   IF        VABCBO = '2'
060000070730     C                   EVAL      VABCBO = '6'
060100070730     C                   ENDIF
060200070730     C                   ENDIF
060300020305     C*
060400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
060500011113     C                   EXSR      CHKIMPDIV
060600010202     C*
060700000801     C* Ebbene...
060800000801     C                   ADD       1             §CTRMO
060900070530     C                   IF        *in31 <> *zeros OR
061000070530     C                             *in32 <> *zeros
061100000801     C                   ADD       1             §CTRNO
061200020725     C                   EVAL      x_vinflg = '2'
061300000801     C                   ELSE
061400010201     C                   ADD       1             §CTROKVB
061500000801     C                   ENDIF
061600000801     C*
061700000801     C                   ENDSR
061800140411     C*----------------------------------------------------*
061900140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
062000140411     C*----------------------------------------------------*
062100140411     C     WRIVAT_A      BEGSR
062200140411     C*
062300140411     C                   eval      vatAAS = vabAAS
062400140411     C                   eval      vatLNP = vabLNP
062500140411     C                   eval      vatNRS = vabNRS
062600140411     C                   eval      vatNSP = vabNSP
062700140411     C*
062800140411     C* Valorizzo il buffer di scrittura del FIVAT
062900140411     C* - TIPO RECORD "A"
063000140411     C                   if        %trim(%subst(vindta:399:35)) <> *blanks
063100140411     C                   eval      VATTRC = 'A'
063200140411     C                   eval      VATNOT = %trim(%subst(vindta:399:35))
063300140411     C                   write     FIVAT000
063400140411     C                   endif
063500140411     C*
063600140411     C                   ENDSR
063700140411     C*----------------------------------------------------*
063800140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
063900140411     C*----------------------------------------------------*
064000140411     C     WRIVAT_B      BEGSR
064100140411     C*
064200140411     C                   eval      vatAAS = vabAAS
064300140411     C                   eval      vatLNP = vabLNP
064400140411     C                   eval      vatNRS = vabNRS
064500140411     C                   eval      vatNSP = vabNSP
064600140411     C*
064700140411     C* Valorizzo il buffer di scrittura del FIVAT
064800140411     C* - TIPO RECORD "B"
064900140411     C                   IF        %trim(%subst(vindta:383:16)) <> *blank
065000140411     C                   eval      VATTRC = 'B'
065100140411     C                   eval      VATNOT = %trim(%subst(vindta:383:16))
065200140411     C                   write     FIVAT000
065300140411     C                   endif
065400140411     C*
065500140411     C                   ENDSR
065600140411     C*----------------------------------------------------*
065700140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
065800140411     C*----------------------------------------------------*
065900140411     C     WRIVAT_I      BEGSR
066000140411     C*
066100140411     C                   eval      vatAAS = vabAAS
066200140411     C                   eval      vatLNP = vabLNP
066300140411     C                   eval      vatNRS = vabNRS
066400140411     C                   eval      vatNSP = vabNSP
066500140411     C*
066600140411     C* Valorizzo il buffer di scrittura del FIVAT
066700140411     C* - TIPO RECORD "I"
066800140411     C                   EVAL      w70=%trim(%subst(vindta:434:70))
066900140411     C                   if        %subst(w70:1:35) <> *blanks
067000140411     C                   eval      VATTRC = 'I'
067100140411     C                   eval      VATNOT = %trim(%subst(w70:1:35))
067200140411     C                   write     FIVAT000
067300140411     C                   endif
067400140411     C                   if        %subst(w70:36:35) <> *blanks
067500140411     C                   eval      VATTRC = 'J'
067600140411     C                   eval      VATNOT = %trim(%subst(w70:36:35))
067700140411     C                   write     FIVAT000
067800140411     C                   endif
067900140411     C*
068000140411     C                   ENDSR
068100140411     C*----------------------------------------------------*
068200140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
068300140411     C*----------------------------------------------------*
068400140411     C     WRIVAT_S      BEGSR
068500140411     C*
068600140411     C                   eval      vatAAS = vabAAS
068700140411     C                   eval      vatLNP = vabLNP
068800140411     C                   eval      vatNRS = vabNRS
068900140411     C                   eval      vatNSP = vabNSP
069000140411     C*
069100140411     C* Valorizzo il buffer di scrittura del FIVAT
069200140411     C* - TIPO RECORD "S"
069300140411     C                   IF        %trim(%subst(vindta:504:16)) <> *blank
069400140411     C                   eval      VATTRC = 'S'
069500140411     C                   eval      VATNOT = %trim(%subst(vindta:504:16))
069600140411     C                   EVAL      %subst(VATNOT:17:2) = 'SS'
069700140411     C                   write     FIVAT000
069800140411     C                   endif
069900140411     C*
070000140411     C                   ENDSR
070100140411     C*----------------------------------------------------*
070200140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
070300140411     C*----------------------------------------------------*
070400140411     C     WRIVAT_E      BEGSR
070500140411     C*
070600140411     C                   eval      vatAAS = vabAAS
070700140411     C                   eval      vatLNP = vabLNP
070800140411     C                   eval      vatNRS = vabNRS
070900140411     C                   eval      vatNSP = vabNSP
071000140411     C*
071100140411     C* Valorizzo il buffer di scrittura del FIVAT
071200140411     C* - TIPO RECORD "E"
071300140411     C                   if        %trim(%subst(vindta:276:35)) <> *blanks
071400140411     C                   eval      VATTRC = 'E'
071500140411     C                   EVAL      VATNOT = %trim(%subst(vindta:276:35))
071600140411     C                   write     FIVAT000
071700140411     C                   endif
071800140411     C*
071900140411     C                   ENDSR
072000020904
072100020904
072200010202     C*----------------------------------------------------*
072300131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
072400010202     C*----------------------------------------------------*
072500020305     C     PREVAT        BEGSR
072600010202     C*
072700131016     C* Compongo il nome del membro da dare al FIVATWWR
072800010202     C                   eval      parmbr = vlrhdl
072900010202     C                   movel     'M'           parmbr
073000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
073100010202     C                   eval      paropz = '1'
073200010202     C* Effettuo la chiamata al CLLE preposto
073300131017     C                   call(e)   'TITVVTC'
073400010202     C                   parm                    parccm
073500010202     C                   parm                    parmbr
073600010202     C                   parm                    paropz
073700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073800010202     C                   if        %error
073900010202     C                   movel     '1'           chkcall
074000010202     C                   else
074100010202     C                   movel     '0'           chkcall
074200010202     C                   endif
074300010202     C*
074400010202     C                   ENDSR
074500000801     C*----------------------------------------------------*
074600000801     C*  CONTROLLO NUMERICITA' CAMPI
074700000801     C*----------------------------------------------------*
074800000801     C     CHKNUM        BEGSR
074900081113     C*
075000081113     C                   IF        PiDecChr = *blanks
075100140411     C                   EVAL      PiDecChr = ','
075200081113     C                   ENDIF
075300081113     C*
075400081113     C                   callp(e)  UBISNUM_Check(PiStr
075500081113     C                                          :PiDecChr
075600081113     C                                          :PiVal
075700081113     C                                          :PiNum
075800081113     C                                          :PiInt)
075900081113     C*
076000000801     C                   IF        %error
076100000801     C                   EVAL      PiInt=*off
076200000801     C                   ENDIF
076300000801     C*
076400000801     C                   ENDSR
076500000801     C***
076600000801
076700011113
076800011113     C*----------------------------------------------------*
076900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
077000011113     C*----------------------------------------------------*
077100011113     C     CHKIMPDIV     BEGSR
077200011113     C*
077300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
077400011113     C                   Z-ADD     *zeros        wrkDec            9 9
077500011113     C*
077600011113     C* Come prima cosa effettuo considerazioni sulla divisa
077700011113     C                   IF        vabIAS > *zeros
077800011113     C                   IF        vabVAS <> 'EUR'
077900011113     C                   EVAL      vabVAS =  'ITL'
078000011113     C                   ENDIF
078100011113     C                   ENDIF
078200011113     C*
078300011113     C                   IF        vabCAS > *zeros
078400011113     C                   IF        vabVCA <> 'EUR'
078500011113     C                   EVAL      vabVCA =  'ITL'
078600011113     C                   ENDIF
078700011113     C                   ENDIF
078800011113     C*
078900011113     C                   IF        vabVMD > *zeros
079000020305     C                   IF        vabVAD <> 'EUR'
079100011113     C                   EVAL      vabVAD =  'ITL'
079200011113     C                   ENDIF
079300011113     C                   ENDIF
079400011113     C*
079500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
079600011113     C                   Z-ADD     vabIAS        wrkDec
079700011113     C                   IF        wrkDec > *zeros
079800011113     C                   IF        vabVAS = 'ITL'
079900011113     C                   EVAL      vabIAS = *zeros
080000011113     C                   ENDIF
080100011113     C                   ENDIF
080200011113     C*
080300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
080400011113     C                   Z-ADD     vabCAS        wrkDec
080500011113     C                   IF        wrkDec > *zeros
080600011113     C                   IF        vabVCA = 'ITL'
080700011113     C                   EVAL      vabCAS = *zeros
080800011113     C                   ENDIF
080900011113     C                   ENDIF
081000011113     C*
081100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
081200011113     C                   Z-ADD     vabVMD        wrkDec
081300011113     C                   IF        wrkDec > *zeros
081400011113     C                   IF        vabVAD = 'ITL'
081500011113     C                   EVAL      vabVMD = *zeros
081600011113     C                   ENDIF
081700011113     C                   ENDIF
081800011113     C*
081900011113     C                   ENDSR
082000011113     C***
082100011113
082200011113
082300000801
082400000801
082500990920      /TITLE Invio dei dati al punto operativo.
082600010202     C     invio         BEGSR
082700990920     C*
082800131016     C* 1° invio FIVAT
082900010201     C                   reset                   dscmz
083000111031     C                   move      wPOI          cmzdst
083100131016     C                   eval      cmzfld = 'FIVATWWR'
083200010201     C                   eval      cmzmbd = vlrhdl
083300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083400021009     C***                if        prmfir = *blanks
083500131016     C                   eval      cmzfla = 'FIVAT00F'
083600131016     C                   eval      cmzmba = 'FIVAT00F'
083700021009     C***                else
083800021009     C***                eval      cmzfla = prmfir
083900021009     C***                eval      cmzmba = prmfir
084000021009     C***                endif
084100010201     C                   eval      cmznrr = *zeros
084200020305     C                   move      §ctrokvt      cmznrr
084300021018     C                   eval      cmzlba = vlrfl1
084400010201     C                   call(e)   'TIS711C'
084500010201     C                   parm                    dscmz
084600010201     C                   parm      *blanks       esito
084700010205     C                   if        %error
084800010205     C                             or cmzerr = '1'
084900010205     C                             or esito  = '1'
085000010205     C                   eval      wrkesito = '3'
085100010205     C                   else
085200010201     C*
085300131016     C* 2° invio FIVAB
085400010201     C                   reset                   dscmz
085500111031     C                   move      wPOI          cmzdst
085600010201     C                   eval      cmzfld = vlrfou
085700010201     C                   eval      cmzmbd = vlrhdl
085800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085900021009     C***                if        prmfir = *blanks
086000131016     C                   eval      cmzfla = 'FIVAB00F'
086100131016     C                   eval      cmzmba = 'FIVAB00F'
086200021009     C***                else
086300021009     C***                eval      cmzfla = prmfir
086400021009     C***                eval      cmzmba = prmfir
086500021009     C***                endif
086600010201     C                   eval      cmznrr = *zeros
086700010201     C                   move      §ctrokvb      cmznrr
086800021018     C                   eval      cmzlba = vlrfl1
086900010201     C                   call(e)   'TIS711C'
087000010201     C                   parm                    dscmz
087100010201     C                   parm      *blanks       esito
087200010201     C                   if        %error
087300010201     C                             or cmzerr = '1'
087400010201     C                             or esito  = '1'
087500010201     C                   eval      wrkesito = '3'
087600010201     C                   endif
087700010205     C                   endif
087800990920     C*
087900000613     C                   ENDSR
088000000613     C***
088100070411
088200070411     C     *pssr         BEGSR
088300070411     C*
088400070411     C                   if        %open(tivin00r)
088500070411     C                   close     tivin00r
088600070411     C                   endif
088700131016     C                   if        %open(FIVABWWr)
088800131016     C                   close     FIVABWWr
088900120706     C                   endif
089000131016     C                   if        %open(FIVATWWr)
089100131016     C                   close     FIVATWWr
089200070411     C                   endif
089300070411     C*
089400070411     C* Effettuo la chiamata al CLLE preposto
089500131017     C                   call(e)   'TITVVTC'
089600070411     C                   parm                    parccm
089700070411     C                   parm                    parmbr
089800070411     C                   parm      '2'           paropz
089900070411     C*
090000070411     C                   eval      wrkesito = '2'
090100070411     C*
090200070411     C                   seton                                        LR
090300070411     C*
090400070411     C                   ENDSR     '*CANCL'
090500070411     C***
090600070411
090700990910
090800000613     C     *inzsr        BEGSR
090900990910     C*
091000990910     C     *entry        plist
091100990920     C                   parm                    tivlrds
091200990921     C                   parm      wrkesito      esito
091300000724     C                   parm                    prmlit
091400000710     C                   parm                    prmfir
091500000613     C*
091600000830     C* CALCOLA LA DATA CORRENTE
091700120706     C                   time                    wn14             14 0
091800120706     C                   movel     wn14          oracor            6 0          *ORA
091900100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
092000100324     C                   eval      datcor = %dec(%date() : *ISO)
092100000830     C*
092200000613     C                   ENDSR
092300000613     C***
