000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140319     C* ppt:18='V'=va sommato solo volume, per colli e peso c'è l'intera spedizione
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
002700140319     D depspe          s             25    INZ(*blanks)
002800140319     D curspe          s             25    INZ(*blanks)
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
004400140319     D  VINSPE                       25
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
012700140319     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
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
015400140319     C                   EVAL      PiStr=%trim(%subst(vindta:46:25))
015500020305     C                   MOVEL(p)  PiStr         curspe
015600010305     C*
015700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015800080923     C                   if        *in50 = *off
015900071003     C                   exsr      impvab
016000111130     C  N31              eval      fivabds_ok = fivabds
016100111130     C  N31              exsr      wrivab
016200131016     C                   exsr      wrivat_a                                     => carico VAT
016300071003     C                   exsr      wrivat_b                                     => carico VAT
016400160219     C                   exsr      wrivat_s                                     => carico VAT
016500111125     C                   exsr      inzvar
016600071003     C                   else
016700080923     C*
016800071009     C                   if        wDISK = 'D'
016900131016     C                   exsr      repNSP
017000071009     C                   exsr      impvab
017100111130     C  N31              eval      fivabds_ok = fivabds
017200111130     C  N31              exsr      wrivab
017300131016     C                   exsr      wrivat_a                                     => carico VAT
017400131016     C                   exsr      wrivat_b                                     => carico VAT
017500160219     C                   exsr      wrivat_s                                     => carico VAT
017600131016     C                   exsr      wrivat_e                                     => carico VAT
017700111125     C                   exsr      inzvar
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300131016     C                   exsr      repNSP
018400020305     C                   exsr      impvab
018500111130     C                   z-add     VABNCL        sav_VABNCL
018600111130     C                   z-add     VABPKB        sav_VABPKB
018700111130     C                   z-add     VABVLB        sav_VABVLB
018800131016     C                   exsr      wrivat_a                                     => carico VAT
018900071003     C                   exsr      wrivat_b                                     => carico VAT
019000160219     C                   exsr      wrivat_s                                     => carico VAT
019100071003     C   50              exsr      wrivat_e                                     => carico VAT
019200010305     C                   else
019300020725     C                   if        depspe <> curspe                             => rottura di spediz
019400111130     C                   z-add     sav_VABNCL    VABNCL
019500111130     C                   z-add     sav_VABPKB    VABPKB
019600111130     C                   z-add     sav_VABVLB    VABVLB
019700111130     C  N31              eval      fivabds_ok = fivabds
019800111125     C  N31              exsr      wrivab
019900111125     C                   exsr      inzvar
020000111125     C                   eval      depspe = curspe                              => memorizz. spediz
020100080117     C                   clear                   tivinds
020200131016     C                   exsr      repNSP
020300020305     C                   exsr      impvab
020400111130     C                   if        wCntRecSpe = *zeros
020500111130     C                   z-add     VABNCL        sav_VABNCL
020600111130     C                   z-add     VABPKB        sav_VABPKB
020700111130     C                   z-add     VABVLB        sav_VABVLB
020800111130     C                   endif
020900111130     C                   add       1             wCntRecSpe
021000131016     C                   exsr      wrivat_a                                     => carico VAT
021100071003     C                   exsr      wrivat_b                                     => carico VAT
021200160219     C                   exsr      wrivat_s                                     => carico VAT
021300071003     C   50              exsr      wrivat_e                                     => carico VAT
021400020305     C                   else                                                   => x stessa spediz
021500111130     C                   exsr      impvab
021600120706     C   51              add       VABNCL        sav_VABNCL
021700111125     C   51              add       VABPKB        sav_VABPKB
021800111125     C   51              add       VABVLB        sav_VABVLB
021900111125     C   52              z-add     VABNCL        sav_VABNCL
022000111125     C   52              z-add     VABPKB        sav_VABPKB
022100111125     C   52              z-add     VABVLB        sav_VABVLB
022200111125     C   53              add       VABNCL        sav_VABNCL
022300111125     C   53              z-add     VABPKB        sav_VABPKB
022400111125     C   53              z-add     VABVLB        sav_VABVLB
022500131016     C   54              Z-ADD     VABNCL        sav_VABNCL
022600131016     C   54              ADD       VABPKB        sav_VABPKB
022700131016     C   54              ADD       VABVLB        sav_VABVLB
022800140319     C   55              Z-ADD     VABNCL        sav_VABNCL
022900140319     C   55              Z-ADD     VABPKB        sav_VABPKB
023000140319     C   55              ADD       VABVLB        sav_VABVLB
023100111125     C*
023200131016     C                   exsr      wrivat_a                                     => carico VAT
023300071003     C                   exsr      wrivat_b                                     => carico VAT
023400160219     C                   exsr      wrivat_s                                     => carico VAT
023500071003     C   50              exsr      wrivat_e                                     => carico VAT
023600010305     C                   endif
023700010305     C                   endif
023800010305     C                   endif
023900071003     C                   endif
024000071009     C                   endif
024100000905     C*
024200000905     C                   else
024300020725     C                   eval      x_vinflg = '1'
024400000905     C                   endif
024500000905     C*
024600020725     C     VINRRN        chain     tivin000
024700020725     C                   if        %found(tivin00r)
024800020725     C                   eval      y_vinflg = x_vinflg
024900020725     C                   eval      y_vinmsg = x_vinmsg
025000020725     C                   update    tivin000
025100020725     C                   endif
025200020725     C*
025300020725     C/EXEC SQL
025400070530     C+ Fetch C1 into :INPUT_DS
025500020725     C/END-EXEC
025600020725     C*
025700020725     C                   enddo
025800020725     C*
025900020725     C/EXEC SQL
026000020725     C+ close C1
026100020725     C/END-EXEC
026200000905     C*
026300020305     C* Scarico i VAB rimasti "in sospeso"
026400071009     C                   if        wDISK = 'C'
026500140321     C                   z-add     sav_VABNCL    VABNCL
026600140321     C                   z-add     sav_VABPKB    VABPKB
026700140321     C                   z-add     sav_VABVLB    VABVLB
026800111130     C  N31              eval      fivabds_ok = fivabds
026900111130     C  N31              exsr      wrivab
027000071009     C                   endif
027100010202     C*
027200010202     C                   endif
027300990910
027400990910     C* Se non ci sono record con errori ...
027500000710     C                   if        §ctrno = 0
027600990910     C* ... restituisco esito OK.
027700990921     C                   eval      wrkesito = '0'
027800990910     C                   else
027900010201     C                   if        §ctrokvb > 0
028000990921     C                   eval      wrkesito = '1'
028100000710     C                   else
028200000710     C                   eval      wrkesito = '2'
028300990910     C                   endif
028400000710     C                   endif
028500990910     C*
028600990914     C                   if        %open(tivin00r)
028700990908     C                   close     tivin00r
028800990914     C                   endif
028900131016     C                   if        %open(FIVABWWr)
029000131016     C                   close     FIVABWWr
029100120706     C                   endif
029200131016     C                   if        %open(FIVATWWr)
029300131016     C                   close     FIVATWWr
029400010201     C                   endif
029500990910     C*
029600111031     C                   if        §ctrokvb > 0 or
029700111031     C                             §ctrokvt > 0
029800111031     C                             and wPOI <> *zeros
029900010202     C                   exsr      invio
030000990920     C                   endif
030100990920     C*
030200910830     C                   ENDSR
030300000613     C***
030400010305
030500010305     C*----------------------------------------------------*
030600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030700010305     C*----------------------------------------------------*
030800020305     C     WRIVAB        BEGSR
030900070730     C*
031000071003     C* Considerazioni finali
031100111125     C                   if        ok_VABRMA = *blanks
031200111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031300071003     C                   endif
031400071003     C*
031500131016     C                   write     FIVAB000                                     => scarico il VAB
031600010305     C*
031700010305     C                   ENDSR
031800990920
031900000801     C*----------------------------------------------------*
032000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032100000801     C*----------------------------------------------------*
032200010201     C     INZVAR        BEGSR
032300000801     C*
032400010201     C                   Z-ADD     *zeros        Num5_0
032500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032600020725     C                   MOVEL     '0'           FlgCAS            1
032700111125     C*
032800111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032900111130     C*
033000111125     C                   CLEAR                   FIVABDS
033100111130     C                   CLEAR                   FIVABDS_OK
033200111130     C                   CLEAR                   FIVABDS_SAV
033300131016     C                   CLEAR                   FIVAB000
033400131016     C                   CLEAR                   FIVAT000
033500000801     C*
033600000801     C                   ENDSR
033700000801     C*----------------------------------------------------*
033800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033900000801     C*----------------------------------------------------*
034000020904     C     DEFCAM        BEGSR
034100080923     C*
034200111125     C                   SETOFF                                       50
034300111125     C                   SETOFF                                       515253
034400140319     C                   SETOFF                                       5455
034500070730     C* Imposto i valori di default...
034600140319     C                   Z-ADD     0674040       VABCCM
034700140319     C                   Z-ADD     0674040       VATCCM
034800140319     C                   Z-ADD     067           VABLNP
034900140319     C                   Z-ADD     067           VATLNP
035000140319     C                   Z-ADD     000           VABCTR
035100140319     C                   MOVEL     '1'           VABCBO
035200140319     C                   MOVEL     '7Q'          VABCTM
035300070222     C* ... e poi verifico se sono stati passati come parametri
035400070222     C                   IF        vlrppt > *blanks
035500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035700070222     C                   EXSR      CHKNUM
035800070222     C                   IF        PiInt=*on
035900070222     C                   Z-ADD     PiVal         VABCCM
036000070222     C                   Z-ADD     PiVal         VATCCM
036100070222     C                   ENDIF
036200070222     C                   ENDIF
036300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABLNP
036800070222     C                   Z-ADD     PiVal         VATLNP
036900070222     C                   ENDIF
037000070222     C                   ENDIF
037100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037300070222     C                   EXSR      CHKNUM
037400070222     C                   IF        PiInt=*on
037500070222     C                   Z-ADD     PiVal         VABCTR
037600070222     C                   ENDIF
037700070928     C                   ENDIF
037800071009     C                   MOVEL     *blanks       wDISK             1
037900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038000071009     C                   IF        wDISK <> *blanks
038100070928     C                   SETON                                        50
038200070222     C                   ENDIF
038300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038400111125     C                   SETON                                        41
038500080923     C                   ENDIF
038600070222     C                   ENDIF
038700120706     C*
038800120706     C* valido per nr.colli/peso/volume
038900120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039000120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039100120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039200131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039300140321     C* 'V' = va sommato solo il volume, per colli e peso c'è il valore dell'intera spedizione
039400120706     C                   SELECT
039500120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039600120706     C                   SETON                                        51
039700120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039800120706     C                   SETON                                        52
039900120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040000120706     C                   SETON                                        53
040100131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040200131016     C                   SETON                                        54
040300140319     C                   WHEN      %subst(vlrppt:18:1) = 'V'
040400140319     C                   SETON                                        55
040500120706     C                   ENDSL
040600120706     C*
040700071009     C   50              EVAL      VABCTM = '7Q'
040800000801     C*
040900000801     C                   ENDSR
041000131016     C*----------------------------------------------------*
041100131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041200131016     C*----------------------------------------------------*
041300131016     C     REPNSP        BEGSR
041400131016     C*
041500131016     C                   EXSR      INZVAR
041600131016     C                   EXSR      DEFCAM
041700131016     C*
041800131016     C                   SETON                                        60
041900131016     C*
042000131016     C* NSP => Stacco un numeratore da AZNUM
042100131016     C                   clear                   TRUL33DS
042200131016     C                   eval      I33OPE = *zeros
042300131016     C                   eval      I33CNU = 302
042400131016     C                   eval      I33NUM = 1
042500131016     C                   movel     TRUL33DS      KPJBU
042600131016     C                   call      'TRUL33R'
042700131016     C                   parm                    KPJBA
042800131016     C                   movel     KPJBU         TRUL33DS
042900131016     C                   if        O33ERR = *zeros
043000131016     C                   z-add     O33NRF        VABNSP
043100131016     C                   z-add     O33NRF        VATNSP
043200131016     C                   else
043300131016     C                   SETON                                        31
043400131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500140319     C                             + ' ' + 'VABNSP VATNSP'
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
045700140319     C*
045800140319     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
045900140319     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046000140319     C     '@':'A'       XLATE     VABRSD        VABRSD
046100140319     C* ==
046200140319     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
046300140319     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
046400140319     C                                    %trim(%subst(vindta:341:30))+' '+
046500140319     C                                    %trim(%subst(vindta:371:5)))
046600140319     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
046700140319     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
046800140319     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
046900140319     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
047000140319     C                   IF        %subst(vindta:43:1) = 'N'
047100140319     C                   EVAL      VABCBO='1'
047200140319     C                   ENDIF
047300140319     C                   IF        %subst(vindta:43:1) = 'P'
047400140319     C                   EVAL      VABCBO='2'
047500140319     C                   ENDIF
047600140319     C                   IF        %subst(vindta:511:3) = 'ABM'
047700140319     C                   EVAL      VABTIC='BM'
047800140319     C                   ENDIF
047900140319     C                   IF        %subst(vindta:511:3) = 'ABS'
048000140319     C                   EVAL      VABTIC='B'
048100140319     C                   ENDIF
048200140319     C                   IF        %subst(vindta:511:3) = 'CON'
048300140319     C                   EVAL      VABTIC=*blanks
048400140319     C                   ENDIF
048500140319     C                   IF        %subst(vindta:511:3) = 'ACM'
048600140319     C                   EVAL      VABTIC='CM'
048700140319     C                   ENDIF
048800160219     C                   IF        %trim(%subst(vindta:511:3)) = 'TM'
048900160219     C                   EVAL      VABTIC='TM'
049000160219     C                   ENDIF
049100140319     C* CAP
049200140319     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
049300140319     C                   EXSR      CHKNUM
049400140319     C                   IF        PiInt=*on
049500140319     C                   Z-ADD     PiVal         Num5_0
049600140319     C                   MOVEL(P)  Num5_0        VABCAD
049700140319     C                   ELSE
049800140321     C                   SETON                                        32
049900140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000140319     C                             + ' ' + 'VABCAD'
050100140319     C                   ENDIF
050200140319     C* Reperisco la provincia dal CAP e dalla località
050300140319     C                   IF        VABCAD <> *blanks AND
050400140319     C                             VABPRD  = *blanks
050500140319     C                   CLEAR                   TISI95DS
050600140319     C                   EVAL      I95TCN = '3'
050700140319     C                   Z-ADD     datcor        I95DAT
050800140319     C                   EVAL      I95CAP = VABCAD
050900140319     C                   EVAL      I95LOC = VABLOD
051000140319     C                   EVAL      I95NAR = VABNZD
051100140319     C                   CALL      'TISI95R'
051200140319     C                   PARM                    TISI95DS
051300140319     C                   EVAL      VABPRD = O95PRV
051400140319     C                   ENDIF
051500140319     C* NCL
051600140319     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
051700140319     C                   EXSR      CHKNUM
051800140319     C                   IF        PiInt=*on
051900140321     C                   Z-ADD     PiVal         VABNCL
052000140319     C                   ELSE
052100140321     C                   SETON                                        32
052200140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052300140319     C                             + ' ' + 'VABNCL'
052400140319     C                   ENDIF
052500140319     C* PKB
052600140319     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
052700140319     C                   EXSR      CHKNUM
052800140319     C                   IF        PiNum=*on
052900140319     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
053000140319     C                   Z-ADD     PiVal         VABPKB
053100140319     C                   ELSE
053200140321     C                   SETON                                        32
053300140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053400140319     C                             + ' ' + 'VABPKB'
053500140319     C                   ENDIF
053600140319     C* VLB
053700150722     C                   IF        %trim(%subst(vindta:461:6)) <> *blanks
053800140319     C                   EVAL      PiStr=%trim(%subst(vindta:461:6))
053900140319     C                   EXSR      CHKNUM
054000140319     C                   IF        PiNum=*on
054100140319     C                   EVAL      PiVal=PiVal/1000                             da decm3 a m3
054200140319     C                   Z-ADD     PiVal         VABVLB
054300140319     C                   ELSE
054400140321     C                   SETON                                        32
054500140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600140319     C                             + ' ' + 'VABVLB'
054700140319     C                   ENDIF
054800150722     C                   ENDIF
054900140319     C* RMN
055000140319     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
055100140319     C                   EVAL      PiStr=%trim(%subst(vindta:47:24))
055200140319     C                   EXSR      CHKNUM
055300140319     C                   IF        PiInt=*on
055400140319     C                   Z-ADD     PiVal         VABRMN
055500140319     C                   ELSE
055600140321     C                   SETON                                        32
055700140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800140319     C                             + ' ' + 'VABRMN'
055900140319     C                   ENDIF
056000140319     C* CTR
056100140319     C                   EVAL      PiStr=%trim(%subst(vindta:457:3))
056200140319     C                   EXSR      CHKNUM
056300140319     C                   IF        PiInt=*on
056400140319     C                   Z-ADD     PiVal         VABCTR
056500140319     C                   ELSE
056600140321     C                   SETON                                        32
056700140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056800140319     C                             + ' ' + 'VABCTR'
056900140319     C                   ENDIF
057000140319     C* CAS
057100140319     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
057200140319     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
057300140319     C                             %trim(%subst(vindta:502:9))<>*zeros
057400140319     C                   EVAL      VABVCA = 'EUR'
057500160219     C***                EVAL      VABTIC='OM'
057600140319     C                   EVAL      FlgCAS = '1'
057700140319     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
057800140319     C                   EXSR      CHKNUM
057900140319     C                   IF        PiNum=*on
058000140319     C                   Z-ADD     PiVal         VABCAS
058100140319     C                   ELSE
058200140321     C                   SETON                                        32
058300140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058400140319     C                             + ' ' + 'VABCAS'
058500140319     C                   ENDIF
058600140319     C                   ENDIF
058700140319     C* IAS
058800140319     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
058900140319     C                   EVAL      VABVAS = 'EUR'
059000140319     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
059100140319     C                   EXSR      CHKNUM
059200140319     C                   IF        PiNum=*on
059300140319     C                   Z-ADD     PiVal         VABIAS
059400140319     C                   ELSE
059500140321     C                   SETON                                        32
059600140321     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059700140319     C                             + ' ' + 'VABIAS'
059800140319     C                   ENDIF
059900140319     C                   ENDIF
060000070730     C*
060100070730     C* Considerazioni finali su CBO/CAS
060200121113     C                   IF        FlgCAS <> '0'
060300070730     C                   IF        VABCBO = '1'
060400070730     C                   EVAL      VABCBO = '4'
060500070730     C                   ENDIF
060600070730     C                   IF        VABCBO = '2'
060700070730     C                   EVAL      VABCBO = '6'
060800070730     C                   ENDIF
060900070730     C                   ENDIF
061000020305     C*
061100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
061200011113     C                   EXSR      CHKIMPDIV
061300010202     C*
061400000801     C* Ebbene...
061500000801     C                   ADD       1             §CTRMO
061600070530     C                   IF        *in31 <> *zeros OR
061700070530     C                             *in32 <> *zeros
061800000801     C                   ADD       1             §CTRNO
061900020725     C                   EVAL      x_vinflg = '2'
062000000801     C                   ELSE
062100010201     C                   ADD       1             §CTROKVB
062200000801     C                   ENDIF
062300000801     C*
062400000801     C                   ENDSR
062500131016     C*----------------------------------------------------*
062600131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
062700131016     C*----------------------------------------------------*
062800131016     C     WRIVAT_A      BEGSR
062900131016     C*
063000131016     C                   eval      vatAAS = vabAAS
063100131016     C                   eval      vatLNP = vabLNP
063200131016     C                   eval      vatNRS = vabNRS
063300131016     C                   eval      vatNSP = vabNSP
063400131016     C*
063500131016     C* Valorizzo il buffer di scrittura del FIVAT
063600131016     C* - TIPO RECORD "A"
063700140319     C                   if        %trim(%subst(vindta:581:30)) <> *blanks
063800131016     C                   eval      VATTRC = 'A'
063900140319     C                   eval      VATNOT = %trim(%subst(vindta:581:30))
064000131016     C                   write     FIVAT000
064100131016     C                   endif
064200131016     C*
064300131016     C                   ENDSR
064400131016     C*----------------------------------------------------*
064500131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
064600131016     C*----------------------------------------------------*
064700131016     C     WRIVAT_B      BEGSR
064800131016     C*
064900131016     C                   eval      vatAAS = vabAAS
065000131016     C                   eval      vatLNP = vabLNP
065100131016     C                   eval      vatNRS = vabNRS
065200131016     C                   eval      vatNSP = vabNSP
065300131016     C*
065400131016     C* Valorizzo il buffer di scrittura del FIVAT
065500131016     C* - TIPO RECORD "B"
065600140319     C                   if        %trim(%subst(vindta:376:15)) <> *blanks
065700131016     C                   eval      VATTRC = 'B'
065800140319     C                   EVAL      VATNOT = %trim(%subst(vindta:376:15))
065900131016     C                   write     FIVAT000
066000131016     C                   endif
066100131016     C*
066200131016     C                   ENDSR
066300160219     C*----------------------------------------------------*
066400160219     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
066500160219     C*----------------------------------------------------*
066600160219     C     WRIVAT_S      BEGSR
066700160219     C*
066800160219     C                   eval      vatAAS = vabAAS
066900160219     C                   eval      vatLNP = vabLNP
067000160219     C                   eval      vatNRS = vabNRS
067100160219     C                   eval      vatNSP = vabNSP
067200160219     C*
067300160219     C* Valorizzo il buffer di scrittura del FIVAT
067400160219     C* - TIPO RECORD "S"
067500160219     C                   if        %trim(%subst(vindta:717:10)) <> *blanks
067600160219     C                   eval      VATTRC = 'S'
067700160219     C                   EVAL      VATNOT = %trim(%subst(vindta:717:10))
067800160219     C* i 2 flag devono essere blank
067900160219     C                   EVAL      %subst(VATNOT:17:1)=*blank
068000160219     C                   EVAL      %subst(VATNOT:18:1)=*blank
068100160219     C                   write     FIVAT000
068200160219     C                   endif
068300160219     C*
068400160219     C                   ENDSR
068500131016     C*----------------------------------------------------*
068600131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
068700131016     C*----------------------------------------------------*
068800131016     C     WRIVAT_E      BEGSR
068900131016     C*
069000131016     C                   eval      vatAAS = vabAAS
069100131016     C                   eval      vatLNP = vabLNP
069200131016     C                   eval      vatNRS = vabNRS
069300131016     C                   eval      vatNSP = vabNSP
069400131016     C*
069500131016     C* Valorizzo il buffer di scrittura del FIVAT
069600131016     C* - TIPO RECORD "E"
069700140319     C                   if        %trim(%subst(vindta:554:25)) <> *blanks
069800131016     C                   eval      VATTRC = 'E'
069900150902     C***                eval      VATNOT = %trim(%subst(vindta:554:25))
070000150902     C                   eval      VATNOT = %trim(%char(%dec(
070100150902     C                                      %subst(vindta:554:25):25:0)))
070200131016     C                   write     FIVAT000
070300131016     C                   endif
070400131016     C*
070500131016     C                   ENDSR
070600020904
070700020904
070800010202     C*----------------------------------------------------*
070900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
071000010202     C*----------------------------------------------------*
071100020305     C     PREVAT        BEGSR
071200010202     C*
071300131016     C* Compongo il nome del membro da dare al FIVATWWR
071400010202     C                   eval      parmbr = vlrhdl
071500010202     C                   movel     'M'           parmbr
071600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
071700010202     C                   eval      paropz = '1'
071800010202     C* Effettuo la chiamata al CLLE preposto
071900131017     C                   call(e)   'TITVVTC'
072000010202     C                   parm                    parccm
072100010202     C                   parm                    parmbr
072200010202     C                   parm                    paropz
072300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
072400010202     C                   if        %error
072500010202     C                   movel     '1'           chkcall
072600010202     C                   else
072700010202     C                   movel     '0'           chkcall
072800010202     C                   endif
072900010202     C*
073000010202     C                   ENDSR
073100000801     C*----------------------------------------------------*
073200000801     C*  CONTROLLO NUMERICITA' CAMPI
073300000801     C*----------------------------------------------------*
073400000801     C     CHKNUM        BEGSR
073500081113     C*
073600081113     C                   IF        PiDecChr = *blanks
073700121113     C                   EVAL      PiDecChr = '.'
073800081113     C                   ENDIF
073900081113     C*
074000081113     C                   callp(e)  UBISNUM_Check(PiStr
074100081113     C                                          :PiDecChr
074200081113     C                                          :PiVal
074300081113     C                                          :PiNum
074400081113     C                                          :PiInt)
074500081113     C*
074600000801     C                   IF        %error
074700000801     C                   EVAL      PiInt=*off
074800000801     C                   ENDIF
074900000801     C*
075000000801     C                   ENDSR
075100000801     C***
075200000801
075300011113
075400011113     C*----------------------------------------------------*
075500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
075600011113     C*----------------------------------------------------*
075700011113     C     CHKIMPDIV     BEGSR
075800011113     C*
075900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
076000011113     C                   Z-ADD     *zeros        wrkDec            9 9
076100011113     C*
076200011113     C* Come prima cosa effettuo considerazioni sulla divisa
076300011113     C                   IF        vabIAS > *zeros
076400011113     C                   IF        vabVAS <> 'EUR'
076500011113     C                   EVAL      vabVAS =  'ITL'
076600011113     C                   ENDIF
076700011113     C                   ENDIF
076800011113     C*
076900011113     C                   IF        vabCAS > *zeros
077000011113     C                   IF        vabVCA <> 'EUR'
077100011113     C                   EVAL      vabVCA =  'ITL'
077200011113     C                   ENDIF
077300011113     C                   ENDIF
077400011113     C*
077500011113     C                   IF        vabVMD > *zeros
077600020305     C                   IF        vabVAD <> 'EUR'
077700011113     C                   EVAL      vabVAD =  'ITL'
077800011113     C                   ENDIF
077900011113     C                   ENDIF
078000011113     C*
078100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
078200011113     C                   Z-ADD     vabIAS        wrkDec
078300011113     C                   IF        wrkDec > *zeros
078400011113     C                   IF        vabVAS = 'ITL'
078500011113     C                   EVAL      vabIAS = *zeros
078600011113     C                   ENDIF
078700011113     C                   ENDIF
078800011113     C*
078900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
079000011113     C                   Z-ADD     vabCAS        wrkDec
079100011113     C                   IF        wrkDec > *zeros
079200011113     C                   IF        vabVCA = 'ITL'
079300011113     C                   EVAL      vabCAS = *zeros
079400011113     C                   ENDIF
079500011113     C                   ENDIF
079600011113     C*
079700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
079800011113     C                   Z-ADD     vabVMD        wrkDec
079900011113     C                   IF        wrkDec > *zeros
080000011113     C                   IF        vabVAD = 'ITL'
080100011113     C                   EVAL      vabVMD = *zeros
080200011113     C                   ENDIF
080300011113     C                   ENDIF
080400011113     C*
080500011113     C                   ENDSR
080600011113     C***
080700011113
080800011113
080900000801
081000000801
081100990920      /TITLE Invio dei dati al punto operativo.
081200010202     C     invio         BEGSR
081300990920     C*
081400131016     C* 1° invio FIVAT
081500010201     C                   reset                   dscmz
081600111031     C                   move      wPOI          cmzdst
081700131016     C                   eval      cmzfld = 'FIVATWWR'
081800010201     C                   eval      cmzmbd = vlrhdl
081900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082000021009     C***                if        prmfir = *blanks
082100131016     C                   eval      cmzfla = 'FIVAT00F'
082200131016     C                   eval      cmzmba = 'FIVAT00F'
082300021009     C***                else
082400021009     C***                eval      cmzfla = prmfir
082500021009     C***                eval      cmzmba = prmfir
082600021009     C***                endif
082700010201     C                   eval      cmznrr = *zeros
082800020305     C                   move      §ctrokvt      cmznrr
082900021018     C                   eval      cmzlba = vlrfl1
083000010201     C                   call(e)   'TIS711C'
083100010201     C                   parm                    dscmz
083200010201     C                   parm      *blanks       esito
083300010205     C                   if        %error
083400010205     C                             or cmzerr = '1'
083500010205     C                             or esito  = '1'
083600010205     C                   eval      wrkesito = '3'
083700010205     C                   else
083800010201     C*
083900131016     C* 2° invio FIVAB
084000010201     C                   reset                   dscmz
084100111031     C                   move      wPOI          cmzdst
084200010201     C                   eval      cmzfld = vlrfou
084300010201     C                   eval      cmzmbd = vlrhdl
084400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
084500021009     C***                if        prmfir = *blanks
084600131016     C                   eval      cmzfla = 'FIVAB00F'
084700131016     C                   eval      cmzmba = 'FIVAB00F'
084800021009     C***                else
084900021009     C***                eval      cmzfla = prmfir
085000021009     C***                eval      cmzmba = prmfir
085100021009     C***                endif
085200010201     C                   eval      cmznrr = *zeros
085300010201     C                   move      §ctrokvb      cmznrr
085400021018     C                   eval      cmzlba = vlrfl1
085500010201     C                   call(e)   'TIS711C'
085600010201     C                   parm                    dscmz
085700010201     C                   parm      *blanks       esito
085800010201     C                   if        %error
085900010201     C                             or cmzerr = '1'
086000010201     C                             or esito  = '1'
086100010201     C                   eval      wrkesito = '3'
086200010201     C                   endif
086300010205     C                   endif
086400990920     C*
086500000613     C                   ENDSR
086600000613     C***
086700070411
086800070411     C     *pssr         BEGSR
086900070411     C*
087000070411     C                   if        %open(tivin00r)
087100070411     C                   close     tivin00r
087200070411     C                   endif
087300131016     C                   if        %open(FIVABWWr)
087400131016     C                   close     FIVABWWr
087500120706     C                   endif
087600131016     C                   if        %open(FIVATWWr)
087700131016     C                   close     FIVATWWr
087800070411     C                   endif
087900070411     C*
088000070411     C* Effettuo la chiamata al CLLE preposto
088100131017     C                   call(e)   'TITVVTC'
088200070411     C                   parm                    parccm
088300070411     C                   parm                    parmbr
088400070411     C                   parm      '2'           paropz
088500070411     C*
088600070411     C                   eval      wrkesito = '2'
088700070411     C*
088800070411     C                   seton                                        LR
088900070411     C*
089000070411     C                   ENDSR     '*CANCL'
089100070411     C***
089200070411
089300990910
089400140319     C     *inzsr        BEGSR
089500990910     C*
089600990910     C     *entry        plist
089700990920     C                   parm                    tivlrds
089800990921     C                   parm      wrkesito      esito
089900000724     C                   parm                    prmlit
090000000710     C                   parm                    prmfir
090100000613     C*
090200000830     C* CALCOLA LA DATA CORRENTE
090300120706     C                   time                    wn14             14 0
090400120706     C                   movel     wn14          oracor            6 0          *ORA
090500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
090600100324     C                   eval      datcor = %dec(%date() : *ISO)
090700000830     C*
090800000613     C                   ENDSR
090900000613     C***
