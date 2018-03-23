000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400131016     C* ppt:18='P'=vanno sommati solo peso e volume, per i colli c'è l'intera spedizione
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
002700160905     D depspe          s              7    INZ(*blanks)
002800160905     D curspe          s              7    INZ(*blanks)
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
004400160905     D  VINSPE                        7
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
015400131017     C* (uso l'RMA)
015500160905     C                   EVAL      PiStr=%trim(%subst(vindta:311:7))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000160810     C                   exsr      repNSP
016100071003     C                   exsr      impvab
016200111130     C  N31              eval      fivabds_ok = fivabds
016300111130     C  N31              exsr      wrivab
016400131016     C                   exsr      wrivat_a                                     => carico VAT
016500071003     C                   exsr      wrivat_b                                     => carico VAT
016600140708     C                   exsr      wrivat_s                                     => carico VAT
016700111125     C                   exsr      inzvar
016800071003     C                   else
016900080923     C*
017000071009     C                   if        wDISK = 'D'
017100131016     C                   exsr      repNSP
017200071009     C                   exsr      impvab
017300111130     C  N31              eval      fivabds_ok = fivabds
017400111130     C  N31              exsr      wrivab
017500131016     C                   exsr      wrivat_a                                     => carico VAT
017600131016     C                   exsr      wrivat_b                                     => carico VAT
017700140708     C                   exsr      wrivat_s                                     => carico VAT
017800131016     C                   exsr      wrivat_e                                     => carico VAT
017900111125     C                   exsr      inzvar
018000071009     C                   else
018100071009     C*
018200010305     C                   if        depspe = *blanks                             => 1° giro
018300010305     C                   eval      depspe = curspe                              => memorizz. spediz
018400080117     C                   clear                   tivinds
018500131016     C                   exsr      repNSP
018600020305     C                   exsr      impvab
018700111130     C                   z-add     VABNCL        sav_VABNCL
018800111130     C                   z-add     VABPKB        sav_VABPKB
018900111130     C                   z-add     VABVLB        sav_VABVLB
019000131016     C                   exsr      wrivat_a                                     => carico VAT
019100071003     C                   exsr      wrivat_b                                     => carico VAT
019200140708     C                   exsr      wrivat_s                                     => carico VAT
019300071003     C   50              exsr      wrivat_e                                     => carico VAT
019400010305     C                   else
019500020725     C                   if        depspe <> curspe                             => rottura di spediz
019600111130     C                   z-add     sav_VABNCL    VABNCL
019700111130     C                   z-add     sav_VABPKB    VABPKB
019800111130     C                   z-add     sav_VABVLB    VABVLB
019900111130     C  N31              eval      fivabds_ok = fivabds
020000111125     C  N31              exsr      wrivab
020100111125     C                   exsr      inzvar
020200111125     C                   eval      depspe = curspe                              => memorizz. spediz
020300080117     C                   clear                   tivinds
020400131016     C                   exsr      repNSP
020500020305     C                   exsr      impvab
020600111130     C                   if        wCntRecSpe = *zeros
020700111130     C                   z-add     VABNCL        sav_VABNCL
020800111130     C                   z-add     VABPKB        sav_VABPKB
020900111130     C                   z-add     VABVLB        sav_VABVLB
021000111130     C                   endif
021100111130     C                   add       1             wCntRecSpe
021200131016     C                   exsr      wrivat_a                                     => carico VAT
021300071003     C                   exsr      wrivat_b                                     => carico VAT
021400140708     C                   exsr      wrivat_s                                     => carico VAT
021500071003     C   50              exsr      wrivat_e                                     => carico VAT
021600020305     C                   else                                                   => x stessa spediz
021700111130     C                   exsr      impvab
021800120706     C   51              add       VABNCL        sav_VABNCL
021900111125     C   51              add       VABPKB        sav_VABPKB
022000111125     C   51              add       VABVLB        sav_VABVLB
022100111125     C   52              z-add     VABNCL        sav_VABNCL
022200111125     C   52              z-add     VABPKB        sav_VABPKB
022300111125     C   52              z-add     VABVLB        sav_VABVLB
022400111125     C   53              add       VABNCL        sav_VABNCL
022500111125     C   53              z-add     VABPKB        sav_VABPKB
022600111125     C   53              z-add     VABVLB        sav_VABVLB
022700131016     C   54              Z-ADD     VABNCL        sav_VABNCL
022800131016     C   54              ADD       VABPKB        sav_VABPKB
022900131016     C   54              ADD       VABVLB        sav_VABVLB
023000111125     C*
023100131016     C                   exsr      wrivat_a                                     => carico VAT
023200071003     C                   exsr      wrivat_b                                     => carico VAT
023300140708     C                   exsr      wrivat_s                                     => carico VAT
023400071003     C   50              exsr      wrivat_e                                     => carico VAT
023500010305     C                   endif
023600010305     C                   endif
023700010305     C                   endif
023800071003     C                   endif
023900071009     C                   endif
024000000905     C*
024100000905     C                   else
024200020725     C                   eval      x_vinflg = '1'
024300000905     C                   endif
024400000905     C*
024500020725     C     VINRRN        chain     tivin000
024600020725     C                   if        %found(tivin00r)
024700020725     C                   eval      y_vinflg = x_vinflg
024800020725     C                   eval      y_vinmsg = x_vinmsg
024900020725     C                   update    tivin000
025000020725     C                   endif
025100020725     C*
025200020725     C/EXEC SQL
025300070530     C+ Fetch C1 into :INPUT_DS
025400020725     C/END-EXEC
025500020725     C*
025600020725     C                   enddo
025700020725     C*
025800020725     C/EXEC SQL
025900020725     C+ close C1
026000020725     C/END-EXEC
026100000905     C*
026200020305     C* Scarico i VAB rimasti "in sospeso"
026300071009     C                   if        wDISK = 'C'
026400111130     C  N31              eval      fivabds_ok = fivabds
026500111130     C  N31              exsr      wrivab
026600071009     C                   endif
026700010202     C*
026800010202     C                   endif
026900990910
027000990910     C* Se non ci sono record con errori ...
027100000710     C                   if        §ctrno = 0
027200990910     C* ... restituisco esito OK.
027300990921     C                   eval      wrkesito = '0'
027400990910     C                   else
027500010201     C                   if        §ctrokvb > 0
027600990921     C                   eval      wrkesito = '1'
027700000710     C                   else
027800000710     C                   eval      wrkesito = '2'
027900990910     C                   endif
028000000710     C                   endif
028100990910     C*
028200990914     C                   if        %open(tivin00r)
028300990908     C                   close     tivin00r
028400990914     C                   endif
028500131016     C                   if        %open(FIVABWWr)
028600131016     C                   close     FIVABWWr
028700120706     C                   endif
028800131016     C                   if        %open(FIVATWWr)
028900131016     C                   close     FIVATWWr
029000010201     C                   endif
029100990910     C*
029200111031     C                   if        §ctrokvb > 0 or
029300111031     C                             §ctrokvt > 0
029400111031     C                             and wPOI <> *zeros
029500010202     C                   exsr      invio
029600990920     C                   endif
029700990920     C*
029800910830     C                   ENDSR
029900000613     C***
030000010305
030100010305     C*----------------------------------------------------*
030200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030300010305     C*----------------------------------------------------*
030400020305     C     WRIVAB        BEGSR
030500070730     C*
030600071003     C* Considerazioni finali
030700111125     C                   if        ok_VABRMA = *blanks
030800111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030900071003     C                   endif
031000071003     C*
031100131016     C                   write     FIVAB000                                     => scarico il VAB
031200010305     C*
031300010305     C                   ENDSR
031400990920
031500000801     C*----------------------------------------------------*
031600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031700000801     C*----------------------------------------------------*
031800010201     C     INZVAR        BEGSR
031900000801     C*
032000010201     C                   Z-ADD     *zeros        Num5_0
032100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032200020725     C                   MOVEL     '0'           FlgCAS            1
032300111125     C*
032400111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032500111130     C*
032600111125     C                   CLEAR                   FIVABDS
032700111130     C                   CLEAR                   FIVABDS_OK
032800111130     C                   CLEAR                   FIVABDS_SAV
032900131016     C                   CLEAR                   FIVAB000
033000131016     C                   CLEAR                   FIVAT000
033100000801     C*
033200000801     C                   ENDSR
033300000801     C*----------------------------------------------------*
033400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033500000801     C*----------------------------------------------------*
033600020904     C     DEFCAM        BEGSR
033700080923     C*
033800111125     C                   SETOFF                                       50
033900111125     C                   SETOFF                                       515253
034000131016     C                   SETOFF                                       54
034100070730     C* Imposto i valori di default...
034200131016     C                   EVAL      VABCCM = 0431139
034300131016     C                   EVAL      VATCCM = 0431139
034400131016     C                   EVAL      VABLNP = 043
034500131016     C                   EVAL      VATLNP = 043
034600131016     C                   EVAL      VABCTR = 000
034700131016     C                   EVAL      VABCBO = '1'
034800070222     C* ... e poi verifico se sono stati passati come parametri
034900070222     C                   IF        vlrppt > *blanks
035000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035200070222     C                   EXSR      CHKNUM
035300070222     C                   IF        PiInt=*on
035400070222     C                   Z-ADD     PiVal         VABCCM
035500070222     C                   Z-ADD     PiVal         VATCCM
035600070222     C                   ENDIF
035700070222     C                   ENDIF
035800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036000070222     C                   EXSR      CHKNUM
036100070222     C                   IF        PiInt=*on
036200070222     C                   Z-ADD     PiVal         VABLNP
036300070222     C                   Z-ADD     PiVal         VATLNP
036400070222     C                   ENDIF
036500070222     C                   ENDIF
036600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036800070222     C                   EXSR      CHKNUM
036900070222     C                   IF        PiInt=*on
037000070222     C                   Z-ADD     PiVal         VABCTR
037100070222     C                   ENDIF
037200070928     C                   ENDIF
037300071009     C                   MOVEL     *blanks       wDISK             1
037400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037500071009     C                   IF        wDISK <> *blanks
037600070928     C                   SETON                                        50
037700070222     C                   ENDIF
037800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037900111125     C                   SETON                                        41
038000080923     C                   ENDIF
038100070222     C                   ENDIF
038200120706     C*
038300120706     C* valido per nr.colli/peso/volume
038400120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038500120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038600120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038700131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038800120706     C                   SELECT
038900120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039000120706     C                   SETON                                        51
039100120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039200120706     C                   SETON                                        52
039300120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039400120706     C                   SETON                                        53
039500131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039600131016     C                   SETON                                        54
039700120706     C                   ENDSL
039800120706     C*
039900071009     C   50              EVAL      VABCTM = '7Q'
040000000801     C*
040100000801     C                   ENDSR
040200131016     C*----------------------------------------------------*
040300131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040400131016     C*----------------------------------------------------*
040500131016     C     REPNSP        BEGSR
040600131016     C*
040700131016     C                   EXSR      INZVAR
040800131016     C                   EXSR      DEFCAM
040900131016     C*
041000131016     C                   SETON                                        60
041100131016     C*
041200131016     C* NSP => Stacco un numeratore da AZNUM
041300131016     C                   clear                   TRUL33DS
041400131016     C                   eval      I33OPE = *zeros
041500131016     C                   eval      I33CNU = 302
041600131016     C                   eval      I33NUM = 1
041700131016     C                   movel     TRUL33DS      KPJBU
041800131016     C                   call      'TRUL33R'
041900131016     C                   parm                    KPJBA
042000131016     C                   movel     KPJBU         TRUL33DS
042100131016     C                   if        O33ERR = *zeros
042200131016     C                   z-add     O33NRF        VABNSP
042300131016     C                   z-add     O33NRF        VATNSP
042400131016     C                   z-add     O33NRF        VABRMN
042500131016     C                   else
042600131016     C                   SETON                                        31
042700131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042800131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
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
045000140708     C                   EVAL      w70=%trim(%subst(vindta:001:30))
045100131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045200131016     C     '@':'A'       XLATE     w70           w70
045300140708     C                   EVAL      VABRSD=%subst(w70:001:30)
045400131016     C* ==
045500131016     C                   EVAL      VABIND=%trim(%subst(vindta:031:30))
045600131016     C                   EVAL      VABLOD=%trim(%subst(vindta:066:30))
045700131016     C                   EVAL      VABPRD=%trim(%subst(vindta:096:02))
045800131016     C                   EVAL      VABRMA=%trim(%subst(vindta:128:12))
045900131016     C                   IF        %trim(%subst(vindta:031:30))='FERMO DEPOSITO'
046000131016     C                   EVAL      VABFFD='S'
046100131016     C                   ENDIF
046200131016     C*
046300131016     C* CAD
046400131016     C                   EVAL      PiStr=%trim(%subst(vindta:061:05))
046500131016     C                   EXSR      CHKNUM
046600131016     C                   IF        PiInt=*on
046700131016     C                   Z-ADD     PiVal         Num5_0
046800131016     C                   MOVEL     Num5_0        VABCAD
046900131016     C                   ELSE
047000131016     C                   SETON                                        32
047100131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047200131016     C                             + ' ' + 'VABCAD'
047300131016     C                   ENDIF
047400131016     C* NCL
047500131016     C                   EVAL      PiStr=%trim(%subst(vindta:145:2))
047600131016     C                   EXSR      CHKNUM
047700131016     C                   IF        PiInt=*on
047800131016     C                   Z-ADD     PiVal         VABNCL
047900131016     C                   ELSE
048000131016     C                   SETON                                        32
048100131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048200131016     C                             + ' ' + 'VABNCL'
048300131016     C                   ENDIF
048400131016     C* PKB
048500131016     C                   EVAL      PiStr=%trim(%subst(vindta:147:7))
048600131016     C                   EXSR      CHKNUM
048700131016     C                   IF        PiNum=*on
048800131016     C* il dato ha la virgola che separa i decimali
048900131016     C                   EVAL(H)   VABPKB = PiVal
049000131016     C                   ELSE
049100131016     C                   SETON                                        32
049200131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049300131016     C                             + ' ' + 'VABPKB'
049400131016     C                   ENDIF
049500070730     C*
049600070730     C* Considerazioni finali su CBO/CAS
049700121113     C                   IF        FlgCAS <> '0'
049800070730     C                   IF        VABCBO = '1'
049900070730     C                   EVAL      VABCBO = '4'
050000070730     C                   ENDIF
050100070730     C                   IF        VABCBO = '2'
050200070730     C                   EVAL      VABCBO = '6'
050300070730     C                   ENDIF
050400070730     C                   ENDIF
050500020305     C*
050600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050700011113     C                   EXSR      CHKIMPDIV
050800010202     C*
050900000801     C* Ebbene...
051000000801     C                   ADD       1             §CTRMO
051100070530     C                   IF        *in31 <> *zeros OR
051200070530     C                             *in32 <> *zeros
051300000801     C                   ADD       1             §CTRNO
051400020725     C                   EVAL      x_vinflg = '2'
051500000801     C                   ELSE
051600010201     C                   ADD       1             §CTROKVB
051700000801     C                   ENDIF
051800000801     C*
051900000801     C                   ENDSR
052000131016     C*----------------------------------------------------*
052100131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
052200131016     C*----------------------------------------------------*
052300131016     C     WRIVAT_A      BEGSR
052400131016     C*
052500131016     C                   eval      vatAAS = vabAAS
052600131016     C                   eval      vatLNP = vabLNP
052700131016     C                   eval      vatNRS = vabNRS
052800131016     C                   eval      vatNSP = vabNSP
052900131016     C*
053000131016     C* Valorizzo il buffer di scrittura del FIVAT
053100131016     C* - TIPO RECORD "A"
053200131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
053300131016     C                   eval      VATTRC = 'A'
053400131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
053500131016     C                   write     FIVAT000
053600131016     C                   endif
053700131016     C*
053800131016     C                   ENDSR
053900131016     C*----------------------------------------------------*
054000131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054100131016     C*----------------------------------------------------*
054200131016     C     WRIVAT_B      BEGSR
054300131016     C*
054400131016     C                   eval      vatAAS = vabAAS
054500131016     C                   eval      vatLNP = vabLNP
054600131016     C                   eval      vatNRS = vabNRS
054700131016     C                   eval      vatNSP = vabNSP
054800131016     C*
054900131016     C* Valorizzo il buffer di scrittura del FIVAT
055000131016     C* - TIPO RECORD "B"
055100140708     C                   if        %subst(vindta:113:015) <> *blanks
055200131016     C                   eval      VATTRC = 'B'
055300140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
055400131016     C                   write     FIVAT000
055500131016     C                   endif
055600131016     C*
055700131016     C                   ENDSR
055800131016     C*----------------------------------------------------*
055900131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
056000131016     C*----------------------------------------------------*
056100131016     C     WRIVAT_E      BEGSR
056200131016     C*
056300131016     C                   eval      vatAAS = vabAAS
056400131016     C                   eval      vatLNP = vabLNP
056500131016     C                   eval      vatNRS = vabNRS
056600131016     C                   eval      vatNSP = vabNSP
056700131016     C*
056800131016     C* Valorizzo il buffer di scrittura del FIVAT
056900131016     C* - TIPO RECORD "E"
057000131016     C                   if        %trim(%subst(vindta:154:11)) <> *blanks
057100131016     C                   eval      VATTRC = 'E'
057200131016     C                   eval      VATNOT = %trim(%subst(vindta:154:011))
057300131016     C                   write     FIVAT000
057400131016     C                   endif
057500131016     C*
057600131016     C                   ENDSR
057700140708     C*----------------------------------------------------*
057800140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
057900140708     C*----------------------------------------------------*
058000140708     C     WRIVAT_S      BEGSR
058100140708     C*
058200140708     C                   eval      vatAAS = vabAAS
058300140708     C                   eval      vatLNP = vabLNP
058400140708     C                   eval      vatNRS = vabNRS
058500140708     C                   eval      vatNSP = vabNSP
058600140708     C*
058700140708     C* Valorizzo il buffer di scrittura del FIVAT
058800140708     C* - TIPO RECORD "B"
058900140708     C                   if        %subst(vindta:113:015) <> *blanks
059000140708     C                   eval      VATTRC = 'S'
059100140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
059200140708     C                   write     FIVAT000
059300140708     C                   endif
059400140708     C*
059500140708     C                   ENDSR
059600020904
059700020904
059800010202     C*----------------------------------------------------*
059900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060000010202     C*----------------------------------------------------*
060100020305     C     PREVAT        BEGSR
060200010202     C*
060300131016     C* Compongo il nome del membro da dare al FIVATWWR
060400010202     C                   eval      parmbr = vlrhdl
060500010202     C                   movel     'M'           parmbr
060600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
060700010202     C                   eval      paropz = '1'
060800010202     C* Effettuo la chiamata al CLLE preposto
060900131017     C                   call(e)   'TITVVTC'
061000010202     C                   parm                    parccm
061100010202     C                   parm                    parmbr
061200010202     C                   parm                    paropz
061300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061400010202     C                   if        %error
061500010202     C                   movel     '1'           chkcall
061600010202     C                   else
061700010202     C                   movel     '0'           chkcall
061800010202     C                   endif
061900010202     C*
062000010202     C                   ENDSR
062100000801     C*----------------------------------------------------*
062200000801     C*  CONTROLLO NUMERICITA' CAMPI
062300000801     C*----------------------------------------------------*
062400000801     C     CHKNUM        BEGSR
062500081113     C*
062600081113     C                   IF        PiDecChr = *blanks
062700121113     C                   EVAL      PiDecChr = '.'
062800081113     C                   ENDIF
062900081113     C*
063000081113     C                   callp(e)  UBISNUM_Check(PiStr
063100081113     C                                          :PiDecChr
063200081113     C                                          :PiVal
063300081113     C                                          :PiNum
063400081113     C                                          :PiInt)
063500081113     C*
063600000801     C                   IF        %error
063700000801     C                   EVAL      PiInt=*off
063800000801     C                   ENDIF
063900000801     C*
064000000801     C                   ENDSR
064100000801     C***
064200000801
064300011113
064400011113     C*----------------------------------------------------*
064500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
064600011113     C*----------------------------------------------------*
064700011113     C     CHKIMPDIV     BEGSR
064800011113     C*
064900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065000011113     C                   Z-ADD     *zeros        wrkDec            9 9
065100011113     C*
065200011113     C* Come prima cosa effettuo considerazioni sulla divisa
065300011113     C                   IF        vabIAS > *zeros
065400011113     C                   IF        vabVAS <> 'EUR'
065500011113     C                   EVAL      vabVAS =  'ITL'
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C                   IF        vabCAS > *zeros
066000011113     C                   IF        vabVCA <> 'EUR'
066100011113     C                   EVAL      vabVCA =  'ITL'
066200011113     C                   ENDIF
066300011113     C                   ENDIF
066400011113     C*
066500011113     C                   IF        vabVMD > *zeros
066600020305     C                   IF        vabVAD <> 'EUR'
066700011113     C                   EVAL      vabVAD =  'ITL'
066800011113     C                   ENDIF
066900011113     C                   ENDIF
067000011113     C*
067100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067200011113     C                   Z-ADD     vabIAS        wrkDec
067300011113     C                   IF        wrkDec > *zeros
067400011113     C                   IF        vabVAS = 'ITL'
067500011113     C                   EVAL      vabIAS = *zeros
067600011113     C                   ENDIF
067700011113     C                   ENDIF
067800011113     C*
067900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068000011113     C                   Z-ADD     vabCAS        wrkDec
068100011113     C                   IF        wrkDec > *zeros
068200011113     C                   IF        vabVCA = 'ITL'
068300011113     C                   EVAL      vabCAS = *zeros
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
068800011113     C                   Z-ADD     vabVMD        wrkDec
068900011113     C                   IF        wrkDec > *zeros
069000011113     C                   IF        vabVAD = 'ITL'
069100011113     C                   EVAL      vabVMD = *zeros
069200011113     C                   ENDIF
069300011113     C                   ENDIF
069400011113     C*
069500011113     C                   ENDSR
069600011113     C***
069700011113
069800011113
069900000801
070000000801
070100990920      /TITLE Invio dei dati al punto operativo.
070200010202     C     invio         BEGSR
070300990920     C*
070400131016     C* 1° invio FIVAT
070500010201     C                   reset                   dscmz
070600111031     C                   move      wPOI          cmzdst
070700131016     C                   eval      cmzfld = 'FIVATWWR'
070800010201     C                   eval      cmzmbd = vlrhdl
070900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071000021009     C***                if        prmfir = *blanks
071100131016     C                   eval      cmzfla = 'FIVAT00F'
071200131016     C                   eval      cmzmba = 'FIVAT00F'
071300021009     C***                else
071400021009     C***                eval      cmzfla = prmfir
071500021009     C***                eval      cmzmba = prmfir
071600021009     C***                endif
071700010201     C                   eval      cmznrr = *zeros
071800020305     C                   move      §ctrokvt      cmznrr
071900021018     C                   eval      cmzlba = vlrfl1
072000010201     C                   call(e)   'TIS711C'
072100010201     C                   parm                    dscmz
072200010201     C                   parm      *blanks       esito
072300010205     C                   if        %error
072400010205     C                             or cmzerr = '1'
072500010205     C                             or esito  = '1'
072600010205     C                   eval      wrkesito = '3'
072700010205     C                   else
072800010201     C*
072900131016     C* 2° invio FIVAB
073000010201     C                   reset                   dscmz
073100111031     C                   move      wPOI          cmzdst
073200010201     C                   eval      cmzfld = vlrfou
073300010201     C                   eval      cmzmbd = vlrhdl
073400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073500021009     C***                if        prmfir = *blanks
073600131016     C                   eval      cmzfla = 'FIVAB00F'
073700131016     C                   eval      cmzmba = 'FIVAB00F'
073800021009     C***                else
073900021009     C***                eval      cmzfla = prmfir
074000021009     C***                eval      cmzmba = prmfir
074100021009     C***                endif
074200010201     C                   eval      cmznrr = *zeros
074300010201     C                   move      §ctrokvb      cmznrr
074400021018     C                   eval      cmzlba = vlrfl1
074500010201     C                   call(e)   'TIS711C'
074600010201     C                   parm                    dscmz
074700010201     C                   parm      *blanks       esito
074800010201     C                   if        %error
074900010201     C                             or cmzerr = '1'
075000010201     C                             or esito  = '1'
075100010201     C                   eval      wrkesito = '3'
075200010201     C                   endif
075300010205     C                   endif
075400990920     C*
075500000613     C                   ENDSR
075600000613     C***
075700070411
075800070411     C     *pssr         BEGSR
075900070411     C*
076000070411     C                   if        %open(tivin00r)
076100070411     C                   close     tivin00r
076200070411     C                   endif
076300131016     C                   if        %open(FIVABWWr)
076400131016     C                   close     FIVABWWr
076500120706     C                   endif
076600131016     C                   if        %open(FIVATWWr)
076700131016     C                   close     FIVATWWr
076800070411     C                   endif
076900070411     C*
077000070411     C* Effettuo la chiamata al CLLE preposto
077100131017     C                   call(e)   'TITVVTC'
077200070411     C                   parm                    parccm
077300070411     C                   parm                    parmbr
077400070411     C                   parm      '2'           paropz
077500070411     C*
077600070411     C                   eval      wrkesito = '2'
077700070411     C*
077800070411     C                   seton                                        LR
077900070411     C*
078000070411     C                   ENDSR     '*CANCL'
078100070411     C***
078200070411
078300990910
078400000613     C     *inzsr        BEGSR
078500990910     C*
078600990910     C     *entry        plist
078700990920     C                   parm                    tivlrds
078800990921     C                   parm      wrkesito      esito
078900000724     C                   parm                    prmlit
079000000710     C                   parm                    prmfir
079100000613     C*
079200000830     C* CALCOLA LA DATA CORRENTE
079300120706     C                   time                    wn14             14 0
079400120706     C                   movel     wn14          oracor            6 0          *ORA
079500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079600100324     C                   eval      datcor = %dec(%date() : *ISO)
079700000830     C*
079800000613     C                   ENDSR
079900000613     C***
