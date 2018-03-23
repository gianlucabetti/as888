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
002700131017     D depspe          s             12    INZ(*blanks)
002800131017     D curspe          s             12    INZ(*blanks)
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
004400131017     D  VINSPE                       12
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
015500131017     C                   EVAL      PiStr=%trim(%subst(vindta:128:12))
015600020305     C                   MOVEL(p)  PiStr         curspe
015700010305     C*
015800071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015900080923     C                   if        *in50 = *off
016000071003     C                   exsr      impvab
016100111130     C  N31              eval      fivabds_ok = fivabds
016200111130     C  N31              exsr      wrivab
016300131016     C                   exsr      wrivat_a                                     => carico VAT
016400071003     C                   exsr      wrivat_b                                     => carico VAT
016500140708     C                   exsr      wrivat_s                                     => carico VAT
016600111125     C                   exsr      inzvar
016700071003     C                   else
016800080923     C*
016900071009     C                   if        wDISK = 'D'
017000131016     C                   exsr      repNSP
017100071009     C                   exsr      impvab
017200111130     C  N31              eval      fivabds_ok = fivabds
017300111130     C  N31              exsr      wrivab
017400131016     C                   exsr      wrivat_a                                     => carico VAT
017500131016     C                   exsr      wrivat_b                                     => carico VAT
017600140708     C                   exsr      wrivat_s                                     => carico VAT
017700131016     C                   exsr      wrivat_e                                     => carico VAT
017800111125     C                   exsr      inzvar
017900071009     C                   else
018000071009     C*
018100010305     C                   if        depspe = *blanks                             => 1° giro
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300080117     C                   clear                   tivinds
018400131016     C                   exsr      repNSP
018500020305     C                   exsr      impvab
018600111130     C                   z-add     VABNCL        sav_VABNCL
018700111130     C                   z-add     VABPKB        sav_VABPKB
018800111130     C                   z-add     VABVLB        sav_VABVLB
018900131016     C                   exsr      wrivat_a                                     => carico VAT
019000071003     C                   exsr      wrivat_b                                     => carico VAT
019100140708     C                   exsr      wrivat_s                                     => carico VAT
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
020300131016     C                   exsr      repNSP
020400020305     C                   exsr      impvab
020500111130     C                   if        wCntRecSpe = *zeros
020600111130     C                   z-add     VABNCL        sav_VABNCL
020700111130     C                   z-add     VABPKB        sav_VABPKB
020800111130     C                   z-add     VABVLB        sav_VABVLB
020900111130     C                   endif
021000111130     C                   add       1             wCntRecSpe
021100131016     C                   exsr      wrivat_a                                     => carico VAT
021200071003     C                   exsr      wrivat_b                                     => carico VAT
021300140708     C                   exsr      wrivat_s                                     => carico VAT
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
023000131016     C                   exsr      wrivat_a                                     => carico VAT
023100071003     C                   exsr      wrivat_b                                     => carico VAT
023200140708     C                   exsr      wrivat_s                                     => carico VAT
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
026300111130     C  N31              eval      fivabds_ok = fivabds
026400111130     C  N31              exsr      wrivab
026500071009     C                   endif
026600010202     C*
026700010202     C                   endif
026800990910
026900990910     C* Se non ci sono record con errori ...
027000000710     C                   if        §ctrno = 0
027100990910     C* ... restituisco esito OK.
027200990921     C                   eval      wrkesito = '0'
027300990910     C                   else
027400010201     C                   if        §ctrokvb > 0
027500990921     C                   eval      wrkesito = '1'
027600000710     C                   else
027700000710     C                   eval      wrkesito = '2'
027800990910     C                   endif
027900000710     C                   endif
028000990910     C*
028100990914     C                   if        %open(tivin00r)
028200990908     C                   close     tivin00r
028300990914     C                   endif
028400131016     C                   if        %open(FIVABWWr)
028500131016     C                   close     FIVABWWr
028600120706     C                   endif
028700131016     C                   if        %open(FIVATWWr)
028800131016     C                   close     FIVATWWr
028900010201     C                   endif
029000990910     C*
029100111031     C                   if        §ctrokvb > 0 or
029200111031     C                             §ctrokvt > 0
029300111031     C                             and wPOI <> *zeros
029400010202     C                   exsr      invio
029500990920     C                   endif
029600990920     C*
029700910830     C                   ENDSR
029800000613     C***
029900010305
030000010305     C*----------------------------------------------------*
030100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030200010305     C*----------------------------------------------------*
030300020305     C     WRIVAB        BEGSR
030400070730     C*
030500071003     C* Considerazioni finali
030600111125     C                   if        ok_VABRMA = *blanks
030700111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030800071003     C                   endif
030900071003     C*
031000131016     C                   write     FIVAB000                                     => scarico il VAB
031100010305     C*
031200010305     C                   ENDSR
031300990920
031400000801     C*----------------------------------------------------*
031500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031600000801     C*----------------------------------------------------*
031700010201     C     INZVAR        BEGSR
031800000801     C*
031900010201     C                   Z-ADD     *zeros        Num5_0
032000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032100020725     C                   MOVEL     '0'           FlgCAS            1
032200111125     C*
032300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032400111130     C*
032500111125     C                   CLEAR                   FIVABDS
032600111130     C                   CLEAR                   FIVABDS_OK
032700111130     C                   CLEAR                   FIVABDS_SAV
032800131016     C                   CLEAR                   FIVAB000
032900131016     C                   CLEAR                   FIVAT000
033000000801     C*
033100000801     C                   ENDSR
033200000801     C*----------------------------------------------------*
033300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033400000801     C*----------------------------------------------------*
033500020904     C     DEFCAM        BEGSR
033600080923     C*
033700111125     C                   SETOFF                                       50
033800111125     C                   SETOFF                                       515253
033900131016     C                   SETOFF                                       54
034000070730     C* Imposto i valori di default...
034100131016     C                   EVAL      VABCCM = 0431139
034200131016     C                   EVAL      VATCCM = 0431139
034300131016     C                   EVAL      VABLNP = 043
034400131016     C                   EVAL      VATLNP = 043
034500131016     C                   EVAL      VABCTR = 000
034600131016     C                   EVAL      VABCBO = '1'
034700070222     C* ... e poi verifico se sono stati passati come parametri
034800070222     C                   IF        vlrppt > *blanks
034900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
035100070222     C                   EXSR      CHKNUM
035200070222     C                   IF        PiInt=*on
035300070222     C                   Z-ADD     PiVal         VABCCM
035400070222     C                   Z-ADD     PiVal         VATCCM
035500070222     C                   ENDIF
035600070222     C                   ENDIF
035700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
035800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
035900070222     C                   EXSR      CHKNUM
036000070222     C                   IF        PiInt=*on
036100070222     C                   Z-ADD     PiVal         VABLNP
036200070222     C                   Z-ADD     PiVal         VATLNP
036300070222     C                   ENDIF
036400070222     C                   ENDIF
036500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
036600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
036700070222     C                   EXSR      CHKNUM
036800070222     C                   IF        PiInt=*on
036900070222     C                   Z-ADD     PiVal         VABCTR
037000070222     C                   ENDIF
037100070928     C                   ENDIF
037200071009     C                   MOVEL     *blanks       wDISK             1
037300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
037400071009     C                   IF        wDISK <> *blanks
037500070928     C                   SETON                                        50
037600070222     C                   ENDIF
037700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
037800111125     C                   SETON                                        41
037900080923     C                   ENDIF
038000070222     C                   ENDIF
038100120706     C*
038200120706     C* valido per nr.colli/peso/volume
038300120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
038400120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
038500120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
038600131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
038700120706     C                   SELECT
038800120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
038900120706     C                   SETON                                        51
039000120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
039100120706     C                   SETON                                        52
039200120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
039300120706     C                   SETON                                        53
039400131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
039500131016     C                   SETON                                        54
039600120706     C                   ENDSL
039700120706     C*
039800071009     C   50              EVAL      VABCTM = '7Q'
039900000801     C*
040000000801     C                   ENDSR
040100131016     C*----------------------------------------------------*
040200131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
040300131016     C*----------------------------------------------------*
040400131016     C     REPNSP        BEGSR
040500131016     C*
040600131016     C                   EXSR      INZVAR
040700131016     C                   EXSR      DEFCAM
040800131016     C*
040900131016     C                   SETON                                        60
041000131016     C*
041100131016     C* NSP => Stacco un numeratore da AZNUM
041200131016     C                   clear                   TRUL33DS
041300131016     C                   eval      I33OPE = *zeros
041400131016     C                   eval      I33CNU = 302
041500131016     C                   eval      I33NUM = 1
041600131016     C                   movel     TRUL33DS      KPJBU
041700131016     C                   call      'TRUL33R'
041800131016     C                   parm                    KPJBA
041900131016     C                   movel     KPJBU         TRUL33DS
042000131016     C                   if        O33ERR = *zeros
042100131016     C                   z-add     O33NRF        VABNSP
042200131016     C                   z-add     O33NRF        VATNSP
042300131016     C                   z-add     O33NRF        VABRMN
042400131016     C                   else
042500131016     C                   SETON                                        31
042600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042700131016     C                             + ' ' + 'VABNSP VATNSP VABRMN'
042800131016     C                   endif
042900131016     C*
043000131016     C                   ENDSR
043100000801     C*----------------------------------------------------*
043200131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043300000801     C*----------------------------------------------------*
043400010201     C     IMPVAB        BEGSR
043500070530     C*
043600070530     C                   SETOFF                                       3132
043700070928     C*
043800070928     C                   EXSR      DEFCAM
043900070928     C*
044000111031     C                   IF        vlrpoi <> 999
044100070928     C                   MOVE(P)   vlrpoi        VABFGS
044200070928     C                   MOVE(P)   vlrpoi        VATFGS
044300111031     C                   Z-ADD     vlrpoi        wPOI              3 0
044400111031     C                   ENDIF
044500070928     C*
044600070928     C                   MOVEL     datcor        VABAAS
044700070928     C                   MOVEL     datcor        VATAAS
044800070928     C                   MOVE      datcor        VABMGS
044900140708     C                   EVAL      w70=%trim(%subst(vindta:001:30))
045000131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045100131016     C     '@':'A'       XLATE     w70           w70
045200140708     C                   EVAL      VABRSD=%subst(w70:001:30)
045300131016     C* ==
045400131016     C                   EVAL      VABIND=%trim(%subst(vindta:031:30))
045500131016     C                   EVAL      VABLOD=%trim(%subst(vindta:066:30))
045600131016     C                   EVAL      VABPRD=%trim(%subst(vindta:096:02))
045700131016     C                   EVAL      VABRMA=%trim(%subst(vindta:128:12))
045800131016     C                   IF        %trim(%subst(vindta:031:30))='FERMO DEPOSITO'
045900131016     C                   EVAL      VABFFD='S'
046000131016     C                   ENDIF
046100131016     C*
046200131016     C* CAD
046300131016     C                   EVAL      PiStr=%trim(%subst(vindta:061:05))
046400131016     C                   EXSR      CHKNUM
046500131016     C                   IF        PiInt=*on
046600131016     C                   Z-ADD     PiVal         Num5_0
046700131016     C                   MOVEL     Num5_0        VABCAD
046800131016     C                   ELSE
046900131016     C                   SETON                                        32
047000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100131016     C                             + ' ' + 'VABCAD'
047200131016     C                   ENDIF
047300131016     C* NCL
047400131016     C                   EVAL      PiStr=%trim(%subst(vindta:145:2))
047500131016     C                   EXSR      CHKNUM
047600131016     C                   IF        PiInt=*on
047700131016     C                   Z-ADD     PiVal         VABNCL
047800131016     C                   ELSE
047900131016     C                   SETON                                        32
048000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100131016     C                             + ' ' + 'VABNCL'
048200131016     C                   ENDIF
048300131016     C* PKB
048400131016     C                   EVAL      PiStr=%trim(%subst(vindta:147:7))
048500131016     C                   EXSR      CHKNUM
048600131016     C                   IF        PiNum=*on
048700131016     C* il dato ha la virgola che separa i decimali
048800131016     C                   EVAL(H)   VABPKB = PiVal
048900131016     C                   ELSE
049000131016     C                   SETON                                        32
049100131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200131016     C                             + ' ' + 'VABPKB'
049300131016     C                   ENDIF
049400070730     C*
049500070730     C* Considerazioni finali su CBO/CAS
049600121113     C                   IF        FlgCAS <> '0'
049700070730     C                   IF        VABCBO = '1'
049800070730     C                   EVAL      VABCBO = '4'
049900070730     C                   ENDIF
050000070730     C                   IF        VABCBO = '2'
050100070730     C                   EVAL      VABCBO = '6'
050200070730     C                   ENDIF
050300070730     C                   ENDIF
050400020305     C*
050500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050600011113     C                   EXSR      CHKIMPDIV
050700010202     C*
050800000801     C* Ebbene...
050900000801     C                   ADD       1             §CTRMO
051000070530     C                   IF        *in31 <> *zeros OR
051100070530     C                             *in32 <> *zeros
051200000801     C                   ADD       1             §CTRNO
051300020725     C                   EVAL      x_vinflg = '2'
051400000801     C                   ELSE
051500010201     C                   ADD       1             §CTROKVB
051600000801     C                   ENDIF
051700000801     C*
051800000801     C                   ENDSR
051900131016     C*----------------------------------------------------*
052000131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
052100131016     C*----------------------------------------------------*
052200131016     C     WRIVAT_A      BEGSR
052300131016     C*
052400131016     C                   eval      vatAAS = vabAAS
052500131016     C                   eval      vatLNP = vabLNP
052600131016     C                   eval      vatNRS = vabNRS
052700131016     C                   eval      vatNSP = vabNSP
052800131016     C*
052900131016     C* Valorizzo il buffer di scrittura del FIVAT
053000131016     C* - TIPO RECORD "A"
053100131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
053200131016     C                   eval      VATTRC = 'A'
053300131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
053400131016     C                   write     FIVAT000
053500131016     C                   endif
053600131016     C*
053700131016     C                   ENDSR
053800131016     C*----------------------------------------------------*
053900131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054000131016     C*----------------------------------------------------*
054100131016     C     WRIVAT_B      BEGSR
054200131016     C*
054300131016     C                   eval      vatAAS = vabAAS
054400131016     C                   eval      vatLNP = vabLNP
054500131016     C                   eval      vatNRS = vabNRS
054600131016     C                   eval      vatNSP = vabNSP
054700131016     C*
054800131016     C* Valorizzo il buffer di scrittura del FIVAT
054900131016     C* - TIPO RECORD "B"
055000140708     C                   if        %subst(vindta:113:015) <> *blanks
055100131016     C                   eval      VATTRC = 'B'
055200140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
055300131016     C                   write     FIVAT000
055400131016     C                   endif
055500131016     C*
055600131016     C                   ENDSR
055700131016     C*----------------------------------------------------*
055800131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
055900131016     C*----------------------------------------------------*
056000131016     C     WRIVAT_E      BEGSR
056100131016     C*
056200131016     C                   eval      vatAAS = vabAAS
056300131016     C                   eval      vatLNP = vabLNP
056400131016     C                   eval      vatNRS = vabNRS
056500131016     C                   eval      vatNSP = vabNSP
056600131016     C*
056700131016     C* Valorizzo il buffer di scrittura del FIVAT
056800131016     C* - TIPO RECORD "E"
056900131016     C                   if        %trim(%subst(vindta:154:11)) <> *blanks
057000131016     C                   eval      VATTRC = 'E'
057100131016     C                   eval      VATNOT = %trim(%subst(vindta:154:011))
057200131016     C                   write     FIVAT000
057300131016     C                   endif
057400131016     C*
057500131016     C                   ENDSR
057600140708     C*----------------------------------------------------*
057700140708     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
057800140708     C*----------------------------------------------------*
057900140708     C     WRIVAT_S      BEGSR
058000140708     C*
058100140708     C                   eval      vatAAS = vabAAS
058200140708     C                   eval      vatLNP = vabLNP
058300140708     C                   eval      vatNRS = vabNRS
058400140708     C                   eval      vatNSP = vabNSP
058500140708     C*
058600140708     C* Valorizzo il buffer di scrittura del FIVAT
058700140708     C* - TIPO RECORD "B"
058800140708     C                   if        %subst(vindta:113:015) <> *blanks
058900140708     C                   eval      VATTRC = 'S'
059000140708     C                   eval      VATNOT = %trim(%subst(vindta:113:015))
059100140708     C                   write     FIVAT000
059200140708     C                   endif
059300140708     C*
059400140708     C                   ENDSR
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
