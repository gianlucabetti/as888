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
001100180308     D*----------------------------------------------------
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
002700180309     D depspe          s              6    INZ(*blanks)
002800180309     D curspe          s              6    INZ(*blanks)
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
004400180309     D  VINSPE                        6
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
015500180309     C                   EVAL      PiStr=%trim(%subst(vindta:165:6))
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
016700180309     C                   exsr      wrivat_ij                                    => carico VAT
016800111125     C                   exsr      inzvar
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200131016     C                   exsr      repNSP
017300071009     C                   exsr      impvab
017400111130     C  N31              eval      fivabds_ok = fivabds
017500111130     C  N31              exsr      wrivab
017600131016     C                   exsr      wrivat_a                                     => carico VAT
017700131016     C                   exsr      wrivat_b                                     => carico VAT
017800140708     C                   exsr      wrivat_s                                     => carico VAT
017900180309     C                   exsr      wrivat_ij                                    => carico VAT
018000131016     C                   exsr      wrivat_e                                     => carico VAT
018100111125     C                   exsr      inzvar
018200071009     C                   else
018300071009     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600080117     C                   clear                   tivinds
018700131016     C                   exsr      repNSP
018800020305     C                   exsr      impvab
018900111130     C                   z-add     VABNCL        sav_VABNCL
019000111130     C                   z-add     VABPKB        sav_VABPKB
019100111130     C                   z-add     VABVLB        sav_VABVLB
019200131016     C                   exsr      wrivat_a                                     => carico VAT
019300071003     C                   exsr      wrivat_b                                     => carico VAT
019400140708     C                   exsr      wrivat_s                                     => carico VAT
019500180309     C                   exsr      wrivat_ij                                    => carico VAT
019600071003     C   50              exsr      wrivat_e                                     => carico VAT
019700010305     C                   else
019800020725     C                   if        depspe <> curspe                             => rottura di spediz
019900111130     C                   z-add     sav_VABNCL    VABNCL
020000111130     C                   z-add     sav_VABPKB    VABPKB
020100111130     C                   z-add     sav_VABVLB    VABVLB
020200111130     C  N31              eval      fivabds_ok = fivabds
020300111125     C  N31              exsr      wrivab
020400111125     C                   exsr      inzvar
020500111125     C                   eval      depspe = curspe                              => memorizz. spediz
020600080117     C                   clear                   tivinds
020700131016     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900111130     C                   if        wCntRecSpe = *zeros
021000111130     C                   z-add     VABNCL        sav_VABNCL
021100111130     C                   z-add     VABPKB        sav_VABPKB
021200111130     C                   z-add     VABVLB        sav_VABVLB
021300111130     C                   endif
021400111130     C                   add       1             wCntRecSpe
021500131016     C                   exsr      wrivat_a                                     => carico VAT
021600071003     C                   exsr      wrivat_b                                     => carico VAT
021700140708     C                   exsr      wrivat_s                                     => carico VAT
021800180309     C                   exsr      wrivat_ij                                    => carico VAT
021900071003     C   50              exsr      wrivat_e                                     => carico VAT
022000020305     C                   else                                                   => x stessa spediz
022100111130     C                   exsr      impvab
022200120706     C   51              add       VABNCL        sav_VABNCL
022300111125     C   51              add       VABPKB        sav_VABPKB
022400111125     C   51              add       VABVLB        sav_VABVLB
022500111125     C   52              z-add     VABNCL        sav_VABNCL
022600111125     C   52              z-add     VABPKB        sav_VABPKB
022700111125     C   52              z-add     VABVLB        sav_VABVLB
022800111125     C   53              add       VABNCL        sav_VABNCL
022900111125     C   53              z-add     VABPKB        sav_VABPKB
023000111125     C   53              z-add     VABVLB        sav_VABVLB
023100131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023200131016     C   54              ADD       VABPKB        sav_VABPKB
023300131016     C   54              ADD       VABVLB        sav_VABVLB
023400111125     C*
023500131016     C                   exsr      wrivat_a                                     => carico VAT
023600071003     C                   exsr      wrivat_b                                     => carico VAT
023700140708     C                   exsr      wrivat_s                                     => carico VAT
023800180309     C                   exsr      wrivat_ij                                    => carico VAT
023900071003     C   50              exsr      wrivat_e                                     => carico VAT
024000010305     C                   endif
024100010305     C                   endif
024200010305     C                   endif
024300071003     C                   endif
024400071009     C                   endif
024500000905     C*
024600000905     C                   else
024700020725     C                   eval      x_vinflg = '1'
024800000905     C                   endif
024900000905     C*
025000020725     C     VINRRN        chain     tivin000
025100020725     C                   if        %found(tivin00r)
025200020725     C                   eval      y_vinflg = x_vinflg
025300020725     C                   eval      y_vinmsg = x_vinmsg
025400020725     C                   update    tivin000
025500020725     C                   endif
025600020725     C*
025700020725     C/EXEC SQL
025800070530     C+ Fetch C1 into :INPUT_DS
025900020725     C/END-EXEC
026000020725     C*
026100020725     C                   enddo
026200020725     C*
026300020725     C/EXEC SQL
026400020725     C+ close C1
026500020725     C/END-EXEC
026600000905     C*
026700020305     C* Scarico i VAB rimasti "in sospeso"
026800071009     C                   if        wDISK = 'C'
026900111130     C  N31              eval      fivabds_ok = fivabds
027000111130     C  N31              exsr      wrivab
027100071009     C                   endif
027200010202     C*
027300010202     C                   endif
027400990910
027500990910     C* Se non ci sono record con errori ...
027600000710     C                   if        §ctrno = 0
027700990910     C* ... restituisco esito OK.
027800990921     C                   eval      wrkesito = '0'
027900990910     C                   else
028000010201     C                   if        §ctrokvb > 0
028100990921     C                   eval      wrkesito = '1'
028200000710     C                   else
028300000710     C                   eval      wrkesito = '2'
028400990910     C                   endif
028500000710     C                   endif
028600990910     C*
028700990914     C                   if        %open(tivin00r)
028800990908     C                   close     tivin00r
028900990914     C                   endif
029000131016     C                   if        %open(FIVABWWr)
029100131016     C                   close     FIVABWWr
029200120706     C                   endif
029300131016     C                   if        %open(FIVATWWr)
029400131016     C                   close     FIVATWWr
029500010201     C                   endif
029600990910     C*
029700111031     C                   if        §ctrokvb > 0 or
029800111031     C                             §ctrokvt > 0
029900111031     C                             and wPOI <> *zeros
030000010202     C                   exsr      invio
030100990920     C                   endif
030200990920     C*
030300910830     C                   ENDSR
030400000613     C***
030500010305
030600010305     C*----------------------------------------------------*
030700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030800010305     C*----------------------------------------------------*
030900020305     C     WRIVAB        BEGSR
031000070730     C*
031100071003     C* Considerazioni finali
031200111125     C                   if        ok_VABRMA = *blanks
031300111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031400071003     C                   endif
031500071003     C*
031600131016     C                   write     FIVAB000                                     => scarico il VAB
031700010305     C*
031800010305     C                   ENDSR
031900990920
032000000801     C*----------------------------------------------------*
032100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032200000801     C*----------------------------------------------------*
032300010201     C     INZVAR        BEGSR
032400000801     C*
032500010201     C                   Z-ADD     *zeros        Num5_0
032600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032700020725     C                   MOVEL     '0'           FlgCAS            1
032800111125     C*
032900111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033000111130     C*
033100111125     C                   CLEAR                   FIVABDS
033200111130     C                   CLEAR                   FIVABDS_OK
033300111130     C                   CLEAR                   FIVABDS_SAV
033400131016     C                   CLEAR                   FIVAB000
033500131016     C                   CLEAR                   FIVAT000
033600000801     C*
033700000801     C                   ENDSR
033800000801     C*----------------------------------------------------*
033900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034000000801     C*----------------------------------------------------*
034100020904     C     DEFCAM        BEGSR
034200080923     C*
034300111125     C                   SETOFF                                       50
034400111125     C                   SETOFF                                       515253
034500131016     C                   SETOFF                                       54
034600070730     C* Imposto i valori di default...
034700131016     C                   EVAL      VABCCM = 0431139
034800131016     C                   EVAL      VATCCM = 0431139
034900131016     C                   EVAL      VABLNP = 043
035000131016     C                   EVAL      VATLNP = 043
035100131016     C                   EVAL      VABCTR = 000
035200131016     C                   EVAL      VABCBO = '1'
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
039300120706     C                   SELECT
039400120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039500120706     C                   SETON                                        51
039600120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039700120706     C                   SETON                                        52
039800120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039900120706     C                   SETON                                        53
040000131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040100131016     C                   SETON                                        54
040200120706     C                   ENDSL
040300120706     C*
040400071009     C   50              EVAL      VABCTM = '7Q'
040500000801     C*
040600000801     C                   ENDSR
040700131016     C*----------------------------------------------------*
040800131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040900131016     C*----------------------------------------------------*
041000131016     C     REPNSP        BEGSR
041100131016     C*
041200131016     C                   EXSR      INZVAR
041300131016     C                   EXSR      DEFCAM
041400131016     C*
041500131016     C                   SETON                                        60
041600131016     C*
041700131016     C* NSP => Stacco un numeratore da AZNUM
041800131016     C                   clear                   TRUL33DS
041900131016     C                   eval      I33OPE = *zeros
042000131016     C                   eval      I33CNU = 302
042100131016     C                   eval      I33NUM = 1
042200131016     C                   movel     TRUL33DS      KPJBU
042300131016     C                   call      'TRUL33R'
042400131016     C                   parm                    KPJBA
042500131016     C                   movel     KPJBU         TRUL33DS
042600131016     C                   if        O33ERR = *zeros
042700131016     C                   z-add     O33NRF        VABNSP
042800131016     C                   z-add     O33NRF        VATNSP
042900131016     C                   z-add     O33NRF        VABRMN
043000131016     C                   else
043100131016     C                   SETON                                        31
043200131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043300131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
043400131016     C                   endif
043500131016     C*
043600131016     C                   ENDSR
043700000801     C*----------------------------------------------------*
043800131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043900000801     C*----------------------------------------------------*
044000010201     C     IMPVAB        BEGSR
044100070530     C*
044200070530     C                   SETOFF                                       3132
044300070928     C*
044400070928     C                   EXSR      DEFCAM
044500070928     C*
044600111031     C                   IF        vlrpoi <> 999
044700070928     C                   MOVE(P)   vlrpoi        VABFGS
044800070928     C                   MOVE(P)   vlrpoi        VATFGS
044900111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045000111031     C                   ENDIF
045100070928     C*
045200070928     C                   MOVEL     datcor        VABAAS
045300070928     C                   MOVEL     datcor        VATAAS
045400070928     C                   MOVE      datcor        VABMGS
045500140708     C                   EVAL      w70=%trim(%subst(vindta:001:30))
045600131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045700131016     C     '@':'A'       XLATE     w70           w70
045800140708     C                   EVAL      VABRSD=%subst(w70:001:30)
045900131016     C* ==
046000131016     C                   EVAL      VABIND=%trim(%subst(vindta:031:30))
046100131016     C                   EVAL      VABLOD=%trim(%subst(vindta:066:30))
046200131016     C                   EVAL      VABPRD=%trim(%subst(vindta:096:02))
046300131016     C                   EVAL      VABRMA=%trim(%subst(vindta:128:12))
046400131016     C                   IF        %trim(%subst(vindta:031:30))='FERMO DEPOSITO'
046500131016     C                   EVAL      VABFFD='S'
046600131016     C                   ENDIF
046700131016     C*
046800131016     C* CAD
046900131016     C                   EVAL      PiStr=%trim(%subst(vindta:061:05))
047000131016     C                   EXSR      CHKNUM
047100131016     C                   IF        PiInt=*on
047200131016     C                   Z-ADD     PiVal         Num5_0
047300131016     C                   MOVEL     Num5_0        VABCAD
047400131016     C                   ELSE
047500131016     C                   SETON                                        32
047600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700131016     C                             + ' ' + 'VABCAD'
047800131016     C                   ENDIF
047900131016     C* NCL
048000131016     C                   EVAL      PiStr=%trim(%subst(vindta:145:2))
048100131016     C                   EXSR      CHKNUM
048200131016     C                   IF        PiInt=*on
048300131016     C                   Z-ADD     PiVal         VABNCL
048400131016     C                   ELSE
048500131016     C                   SETON                                        32
048600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700131016     C                             + ' ' + 'VABNCL'
048800131016     C                   ENDIF
048900131016     C* PKB
049000131016     C                   EVAL      PiStr=%trim(%subst(vindta:147:7))
049100131016     C                   EXSR      CHKNUM
049200131016     C                   IF        PiNum=*on
049300131016     C* il dato ha la virgola che separa i decimali
049400131016     C                   EVAL(H)   VABPKB = PiVal
049500131016     C                   ELSE
049600131016     C                   SETON                                        32
049700131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800131016     C                             + ' ' + 'VABPKB'
049900131016     C                   ENDIF
050000070730     C*
050100070730     C* Considerazioni finali su CBO/CAS
050200121113     C                   IF        FlgCAS <> '0'
050300070730     C                   IF        VABCBO = '1'
050400070730     C                   EVAL      VABCBO = '4'
050500070730     C                   ENDIF
050600070730     C                   IF        VABCBO = '2'
050700070730     C                   EVAL      VABCBO = '6'
050800070730     C                   ENDIF
050900070730     C                   ENDIF
051000020305     C*
051100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051200011113     C                   EXSR      CHKIMPDIV
051300010202     C*
051400000801     C* Ebbene...
051500000801     C                   ADD       1             §CTRMO
051600070530     C                   IF        *in31 <> *zeros OR
051700070530     C                             *in32 <> *zeros
051800000801     C                   ADD       1             §CTRNO
051900020725     C                   EVAL      x_vinflg = '2'
052000000801     C                   ELSE
052100010201     C                   ADD       1             §CTROKVB
052200000801     C                   ENDIF
052300000801     C*
052400000801     C                   ENDSR
052500131016     C*----------------------------------------------------*
052600131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
052700131016     C*----------------------------------------------------*
052800131016     C     WRIVAT_A      BEGSR
052900131016     C*
053000131016     C                   eval      vatAAS = vabAAS
053100131016     C                   eval      vatLNP = vabLNP
053200131016     C                   eval      vatNRS = vabNRS
053300131016     C                   eval      vatNSP = vabNSP
053400131016     C*
053500131016     C* Valorizzo il buffer di scrittura del FIVAT
053600131016     C* - TIPO RECORD "A"
053700131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
053800131016     C                   eval      VATTRC = 'A'
053900131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
054000131016     C                   write     FIVAT000
054100131016     C                   endif
054200131016     C*
054300131016     C                   ENDSR
054400131016     C*----------------------------------------------------*
054500131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054600131016     C*----------------------------------------------------*
054700131016     C     WRIVAT_B      BEGSR
054800131016     C*
054900131016     C                   eval      vatAAS = vabAAS
055000131016     C                   eval      vatLNP = vabLNP
055100131016     C                   eval      vatNRS = vabNRS
055200131016     C                   eval      vatNSP = vabNSP
055300131016     C*
055400131016     C* Valorizzo il buffer di scrittura del FIVAT
055500131016     C* - TIPO RECORD "B"
055600140708     C                   if        %subst(vindta:113:015) <> *blanks
055700131016     C                   eval      VATTRC = 'B'
055800140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
055900131016     C                   write     FIVAT000
056000131016     C                   endif
056100131016     C*
056200131016     C                   ENDSR
056300131016     C*----------------------------------------------------*
056400131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
056500131016     C*----------------------------------------------------*
056600131016     C     WRIVAT_E      BEGSR
056700131016     C*
056800131016     C                   eval      vatAAS = vabAAS
056900131016     C                   eval      vatLNP = vabLNP
057000131016     C                   eval      vatNRS = vabNRS
057100131016     C                   eval      vatNSP = vabNSP
057200131016     C*
057300131016     C* Valorizzo il buffer di scrittura del FIVAT
057400131016     C* - TIPO RECORD "E"
057500131016     C                   if        %trim(%subst(vindta:154:11)) <> *blanks
057600131016     C                   eval      VATTRC = 'E'
057700131016     C                   eval      VATNOT = %trim(%subst(vindta:154:011))
057800131016     C                   write     FIVAT000
057900131016     C                   endif
058000131016     C*
058100131016     C                   ENDSR
058200180309     C*----------------------------------------------------*
058300180309     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
058400180309     C*----------------------------------------------------*
058500180309     C     WRIVAT_IJ     BEGSR
058600180309     C*
058700180309     C                   eval      vatAAS = vabAAS
058800180309     C                   eval      vatLNP = vabLNP
058900180309     C                   eval      vatNRS = vabNRS
059000180309     C                   eval      vatNSP = vabNSP
059100180309     C*
059200180309     C* Valorizzo il buffer di scrittura del FIVAT
059300180309     C* - TIPO RECORD "I" e "J"
059400180309     C                   if        %subst(vindta:171:50) <> *blanks
059500180309     C                   eval      VATTRC = 'I'
059600180309     C                   eval      w70 = %trim(%subst(vindta:171:50))
059700180309     C                   eval      VATNOT = %trim(%subst(w70:1:35))
059800180309     C                   write     FIVAT000
059900180309     C                   if        %subst(w70:36:35) <> *blanks
060000180309     C                   eval      VATTRC = 'J'
060100180309     C                   eval      VATNOT = %trim(%subst(w70:36:35))
060200180309     C                   write     FIVAT000
060300180309     C                   endif
060400180309     C                   endif
060500180309     C*
060600180309     C                   ENDSR
060700140708     C*----------------------------------------------------*
060800140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
060900140708     C*----------------------------------------------------*
061000140708     C     WRIVAT_S      BEGSR
061100140708     C*
061200140708     C                   eval      vatAAS = vabAAS
061300140708     C                   eval      vatLNP = vabLNP
061400140708     C                   eval      vatNRS = vabNRS
061500140708     C                   eval      vatNSP = vabNSP
061600140708     C*
061700140708     C* Valorizzo il buffer di scrittura del FIVAT
061800140708     C* - TIPO RECORD "B"
061900140708     C                   if        %subst(vindta:113:015) <> *blanks
062000140708     C                   eval      VATTRC = 'S'
062100140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
062200140708     C                   write     FIVAT000
062300140708     C                   endif
062400140708     C*
062500140708     C                   ENDSR
062600020904
062700020904
062800010202     C*----------------------------------------------------*
062900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
063000010202     C*----------------------------------------------------*
063100020305     C     PREVAT        BEGSR
063200010202     C*
063300131016     C* Compongo il nome del membro da dare al FIVATWWR
063400010202     C                   eval      parmbr = vlrhdl
063500010202     C                   movel     'M'           parmbr
063600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
063700010202     C                   eval      paropz = '1'
063800010202     C* Effettuo la chiamata al CLLE preposto
063900131017     C                   call(e)   'TITVVTC'
064000010202     C                   parm                    parccm
064100010202     C                   parm                    parmbr
064200010202     C                   parm                    paropz
064300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064400010202     C                   if        %error
064500010202     C                   movel     '1'           chkcall
064600010202     C                   else
064700010202     C                   movel     '0'           chkcall
064800010202     C                   endif
064900010202     C*
065000010202     C                   ENDSR
065100000801     C*----------------------------------------------------*
065200000801     C*  CONTROLLO NUMERICITA' CAMPI
065300000801     C*----------------------------------------------------*
065400000801     C     CHKNUM        BEGSR
065500081113     C*
065600081113     C                   IF        PiDecChr = *blanks
065700121113     C                   EVAL      PiDecChr = '.'
065800081113     C                   ENDIF
065900081113     C*
066000081113     C                   callp(e)  UBISNUM_Check(PiStr
066100081113     C                                          :PiDecChr
066200081113     C                                          :PiVal
066300081113     C                                          :PiNum
066400081113     C                                          :PiInt)
066500081113     C*
066600000801     C                   IF        %error
066700000801     C                   EVAL      PiInt=*off
066800000801     C                   ENDIF
066900000801     C*
067000000801     C                   ENDSR
067100000801     C***
067200000801
067300011113
067400011113     C*----------------------------------------------------*
067500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
067600011113     C*----------------------------------------------------*
067700011113     C     CHKIMPDIV     BEGSR
067800011113     C*
067900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
068000011113     C                   Z-ADD     *zeros        wrkDec            9 9
068100011113     C*
068200011113     C* Come prima cosa effettuo considerazioni sulla divisa
068300011113     C                   IF        vabIAS > *zeros
068400011113     C                   IF        vabVAS <> 'EUR'
068500011113     C                   EVAL      vabVAS =  'ITL'
068600011113     C                   ENDIF
068700011113     C                   ENDIF
068800011113     C*
068900011113     C                   IF        vabCAS > *zeros
069000011113     C                   IF        vabVCA <> 'EUR'
069100011113     C                   EVAL      vabVCA =  'ITL'
069200011113     C                   ENDIF
069300011113     C                   ENDIF
069400011113     C*
069500011113     C                   IF        vabVMD > *zeros
069600020305     C                   IF        vabVAD <> 'EUR'
069700011113     C                   EVAL      vabVAD =  'ITL'
069800011113     C                   ENDIF
069900011113     C                   ENDIF
070000011113     C*
070100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
070200011113     C                   Z-ADD     vabIAS        wrkDec
070300011113     C                   IF        wrkDec > *zeros
070400011113     C                   IF        vabVAS = 'ITL'
070500011113     C                   EVAL      vabIAS = *zeros
070600011113     C                   ENDIF
070700011113     C                   ENDIF
070800011113     C*
070900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
071000011113     C                   Z-ADD     vabCAS        wrkDec
071100011113     C                   IF        wrkDec > *zeros
071200011113     C                   IF        vabVCA = 'ITL'
071300011113     C                   EVAL      vabCAS = *zeros
071400011113     C                   ENDIF
071500011113     C                   ENDIF
071600011113     C*
071700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071800011113     C                   Z-ADD     vabVMD        wrkDec
071900011113     C                   IF        wrkDec > *zeros
072000011113     C                   IF        vabVAD = 'ITL'
072100011113     C                   EVAL      vabVMD = *zeros
072200011113     C                   ENDIF
072300011113     C                   ENDIF
072400011113     C*
072500011113     C                   ENDSR
072600011113     C***
072700011113
072800011113
072900000801
073000000801
073100990920      /TITLE Invio dei dati al punto operativo.
073200010202     C     invio         BEGSR
073300990920     C*
073400131016     C* 1° invio FIVAT
073500010201     C                   reset                   dscmz
073600111031     C                   move      wPOI          cmzdst
073700131016     C                   eval      cmzfld = 'FIVATWWR'
073800010201     C                   eval      cmzmbd = vlrhdl
073900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074000021009     C***                if        prmfir = *blanks
074100131016     C                   eval      cmzfla = 'FIVAT00F'
074200131016     C                   eval      cmzmba = 'FIVAT00F'
074300021009     C***                else
074400021009     C***                eval      cmzfla = prmfir
074500021009     C***                eval      cmzmba = prmfir
074600021009     C***                endif
074700010201     C                   eval      cmznrr = *zeros
074800020305     C                   move      §ctrokvt      cmznrr
074900021018     C                   eval      cmzlba = vlrfl1
075000010201     C                   call(e)   'TIS711C'
075100010201     C                   parm                    dscmz
075200010201     C                   parm      *blanks       esito
075300010205     C                   if        %error
075400010205     C                             or cmzerr = '1'
075500010205     C                             or esito  = '1'
075600010205     C                   eval      wrkesito = '3'
075700010205     C                   else
075800010201     C*
075900131016     C* 2° invio FIVAB
076000010201     C                   reset                   dscmz
076100111031     C                   move      wPOI          cmzdst
076200010201     C                   eval      cmzfld = vlrfou
076300010201     C                   eval      cmzmbd = vlrhdl
076400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076500021009     C***                if        prmfir = *blanks
076600131016     C                   eval      cmzfla = 'FIVAB00F'
076700131016     C                   eval      cmzmba = 'FIVAB00F'
076800021009     C***                else
076900021009     C***                eval      cmzfla = prmfir
077000021009     C***                eval      cmzmba = prmfir
077100021009     C***                endif
077200010201     C                   eval      cmznrr = *zeros
077300010201     C                   move      §ctrokvb      cmznrr
077400021018     C                   eval      cmzlba = vlrfl1
077500010201     C                   call(e)   'TIS711C'
077600010201     C                   parm                    dscmz
077700010201     C                   parm      *blanks       esito
077800010201     C                   if        %error
077900010201     C                             or cmzerr = '1'
078000010201     C                             or esito  = '1'
078100010201     C                   eval      wrkesito = '3'
078200010201     C                   endif
078300010205     C                   endif
078400990920     C*
078500000613     C                   ENDSR
078600000613     C***
078700070411
078800070411     C     *pssr         BEGSR
078900070411     C*
079000070411     C                   if        %open(tivin00r)
079100070411     C                   close     tivin00r
079200070411     C                   endif
079300131016     C                   if        %open(FIVABWWr)
079400131016     C                   close     FIVABWWr
079500120706     C                   endif
079600131016     C                   if        %open(FIVATWWr)
079700131016     C                   close     FIVATWWr
079800070411     C                   endif
079900070411     C*
080000070411     C* Effettuo la chiamata al CLLE preposto
080100131017     C                   call(e)   'TITVVTC'
080200070411     C                   parm                    parccm
080300070411     C                   parm                    parmbr
080400070411     C                   parm      '2'           paropz
080500070411     C*
080600070411     C                   eval      wrkesito = '2'
080700070411     C*
080800070411     C                   seton                                        LR
080900070411     C*
081000070411     C                   ENDSR     '*CANCL'
081100070411     C***
081200070411
081300990910
081400000613     C     *inzsr        BEGSR
081500990910     C*
081600990910     C     *entry        plist
081700990920     C                   parm                    tivlrds
081800990921     C                   parm      wrkesito      esito
081900000724     C                   parm                    prmlit
082000000710     C                   parm                    prmfir
082100000613     C*
082200000830     C* CALCOLA LA DATA CORRENTE
082300120706     C                   time                    wn14             14 0
082400120706     C                   movel     wn14          oracor            6 0          *ORA
082500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
082600100324     C                   eval      datcor = %dec(%date() : *ISO)
082700000830     C*
082800000613     C                   ENDSR
082900000613     C***
