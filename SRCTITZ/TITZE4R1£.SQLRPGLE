000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140723      *
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
002700141017     D depspe          s              7    INZ(*blanks)
002800141017     D curspe          s              7    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500131016     D w70             s             70
003600140930     D w128            s            128
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500150410     D  VINSPE                        7
004600101007     D  VINRRN                        8  0
004700020725     D*
004800080923     D*------------------
004900080923     D* DS REPERIMENTO NUMERATORE
005000080923     D*------------------
005100080923     D trul33ds      e ds                  inz
005200080923     D*------------------
005300080923     D* DS ARCHITETTURA
005400080923     D*------------------
005500080923     D kpjba         e ds                  inz
005600080923     D*------------------
005700121113     D  Num7_0         s              7  0
005800081113
005900081113     D*------------------
006000081113     D* LINKING A DEFINIZIONI ESTERNE
006100081113     D*------------------
006200100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006300081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006400081113
006500990908
006600010201
006700010201
006800080117     C*
006900080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007000080117     C
007100080117     C/EXEC SQL
007200080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007300080117     C/END-EXEC
007400080117     C
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800000613     C*
007900070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008000000613     C*
008100010202     C* Effettuo la chiamata al CLLE preposto
008200131017     C                   call(e)   'TITVVTC'
008300010202     C                   parm                    parccm
008400010202     C                   parm                    parmbr
008500010202     C                   parm      '2'           paropz
008600070730     C*
008700070730     C* Effettuo lancio TISI95 solo x chiusura
008800070730     C                   CLEAR                   TISI95DS
008900070730     C                   EVAL      I95TLA = 'C'
009000070730     C                   CALL      'TISI95R'
009100070730     C                   PARM                    TISI95DS
009200000616     C*
009300000801     C
009400010201     C                   seton                                        LR
009500990908
009600000801
009700910830     C*--------------------------------------------------------
009800131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009900910830     C*--------------------------------------------------------
010000070530     C     RWFILE        BEGSR
010100990910     C*
010200990914     C                   if        not %open(tivin00r)
010300990908     C                   open      tivin00r
010400990914     C                   endif
010500131016     C                   if        not %open(FIVABWWr)
010600131016     C                   open      FIVABWWr
010700990914     C                   endif
010800070530     C*
010900131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000020305     C                   exsr      prevat
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400131016     C                   if        not %open(FIVATWWr)
011500131016     C                   open      FIVATWWr
011600010201     C                   endif
011700080117     C*
011800080117     C                   EXSR      INZVAR
011900080117     C                   EXSR      DEFCAM
012000990910     C*
012100010201     C                   clear                   §CTROKVB          5 0
012200020305     C                   clear                   §CTROKVT          5 0
012300000801     C                   clear                   §CTRMO            5 0
012400000801     C                   clear                   §CTRNO            5 0
012500990910     C*
012600020725     C/EXEC SQL
012700020725     C+ declare C1 cursor for select
012800141222     C+ vindta, vinflg, substr(vindta, 248, 6) as sped, rrn(tivin00r)
012900060223     C+ from tivin00r
013000060223     C+ order by sped
013100020725     C+ for read only
013200020725     C/END-EXEC
013300020725     C
013400020725     C/EXEC SQL
013500020725     C+ open C1
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900070530     C+ Fetch C1 into :INPUT_DS
014000020725     C/END-EXEC
014100020725     C*
014200020725     C                   dow       sqlcod = *zeros
014300990913     C*
014400020725     C                   if        vindta > *blanks
014500000613     C                   add       1             rrnum
014600000801     C*
014700020725     C                   if        vinflg = *blanks
014800020725     C                             or vinflg = '0'
014900020725     C                             or vinflg = '2'
015000000801     C*
015100020725     C                   clear                   x_vinmsg
015200020725     C                   eval      x_vinflg = '1'
015300010305     C*
015400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015500141219     C* (uso NSP/RMN)
015600150410     C                   EVAL      PiStr=%trim(%subst(vindta:248:7))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100140930     C***                exsr      repNSP
016200071003     C                   exsr      impvab
016300111130     C  N31              eval      fivabds_ok = fivabds
016400111130     C  N31              exsr      wrivab
016500141219     C                   exsr      wrivat_a                                     => carico VAT
016600141219     C                   exsr      wrivat_b                                     => carico VAT
016700140411     C                   exsr      wrivat_i                                     => carico VAT
016800140411     C                   exsr      wrivat_s                                     => carico VAT
016900111125     C                   exsr      inzvar
017000071003     C                   else
017100080923     C*
017200071009     C                   if        wDISK = 'D'
017300140930     C***                exsr      repNSP
017400071009     C                   exsr      impvab
017500111130     C  N31              eval      fivabds_ok = fivabds
017600111130     C  N31              exsr      wrivab
017700140411     C                   exsr      wrivat_a                                     => carico VAT
017800140328     C                   exsr      wrivat_b                                     => carico VAT
017900140411     C                   exsr      wrivat_i                                     => carico VAT
018000140411     C                   exsr      wrivat_s                                     => carico VAT
018100140411     C                   exsr      wrivat_e                                     => carico VAT
018200111125     C                   exsr      inzvar
018300071009     C                   else
018400071009     C*
018500010305     C                   if        depspe = *blanks                             => 1° giro
018600010305     C                   eval      depspe = curspe                              => memorizz. spediz
018700080117     C                   clear                   tivinds
018800140930     C***                exsr      repNSP
018900020305     C                   exsr      impvab
019000111130     C                   z-add     VABNCL        sav_VABNCL
019100111130     C                   z-add     VABPKB        sav_VABPKB
019200111130     C                   z-add     VABVLB        sav_VABVLB
019300140411     C                   exsr      wrivat_a                                     => carico VAT
019400140328     C                   exsr      wrivat_b                                     => carico VAT
019500140411     C                   exsr      wrivat_i                                     => carico VAT
019600140411     C                   exsr      wrivat_s                                     => carico VAT
019700140411     C   50              exsr      wrivat_e                                     => carico VAT
019800010305     C                   else
019900020725     C                   if        depspe <> curspe                             => rottura di spediz
020000111130     C                   z-add     sav_VABNCL    VABNCL
020100111130     C                   z-add     sav_VABPKB    VABPKB
020200111130     C                   z-add     sav_VABVLB    VABVLB
020300111130     C  N31              eval      fivabds_ok = fivabds
020400111125     C  N31              exsr      wrivab
020500111125     C                   exsr      inzvar
020600111125     C                   eval      depspe = curspe                              => memorizz. spediz
020700080117     C                   clear                   tivinds
020800140930     C***                exsr      repNSP
020900020305     C                   exsr      impvab
021000111130     C                   if        wCntRecSpe = *zeros
021100111130     C                   z-add     VABNCL        sav_VABNCL
021200111130     C                   z-add     VABPKB        sav_VABPKB
021300111130     C                   z-add     VABVLB        sav_VABVLB
021400111130     C                   endif
021500111130     C                   add       1             wCntRecSpe
021600140411     C                   exsr      wrivat_a                                     => carico VAT
021700140328     C                   exsr      wrivat_b                                     => carico VAT
021800140411     C                   exsr      wrivat_i                                     => carico VAT
021900140411     C                   exsr      wrivat_s                                     => carico VAT
022000140411     C   50              exsr      wrivat_e                                     => carico VAT
022100020305     C                   else                                                   => x stessa spediz
022200111130     C                   exsr      impvab
022300120706     C   51              add       VABNCL        sav_VABNCL
022400111125     C   51              add       VABPKB        sav_VABPKB
022500111125     C   51              add       VABVLB        sav_VABVLB
022600111125     C   52              z-add     VABNCL        sav_VABNCL
022700111125     C   52              z-add     VABPKB        sav_VABPKB
022800111125     C   52              z-add     VABVLB        sav_VABVLB
022900111125     C   53              add       VABNCL        sav_VABNCL
023000111125     C   53              z-add     VABPKB        sav_VABPKB
023100111125     C   53              z-add     VABVLB        sav_VABVLB
023200131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023300131016     C   54              ADD       VABPKB        sav_VABPKB
023400131016     C   54              ADD       VABVLB        sav_VABVLB
023500111125     C*
023600140411     C                   exsr      wrivat_a                                     => carico VAT
023700140328     C                   exsr      wrivat_b                                     => carico VAT
023800140411     C                   exsr      wrivat_i                                     => carico VAT
023900140411     C                   exsr      wrivat_s                                     => carico VAT
024000140411     C   50              exsr      wrivat_e                                     => carico VAT
024100010305     C                   endif
024200010305     C                   endif
024300010305     C                   endif
024400071003     C                   endif
024500071009     C                   endif
024600000905     C*
024700000905     C                   else
024800020725     C                   eval      x_vinflg = '1'
024900000905     C                   endif
025000000905     C*
025100020725     C     VINRRN        chain     tivin000
025200020725     C                   if        %found(tivin00r)
025300020725     C                   eval      y_vinflg = x_vinflg
025400020725     C                   eval      y_vinmsg = x_vinmsg
025500020725     C                   update    tivin000
025600020725     C                   endif
025700020725     C*
025800020725     C/EXEC SQL
025900070530     C+ Fetch C1 into :INPUT_DS
026000020725     C/END-EXEC
026100020725     C*
026200020725     C                   enddo
026300020725     C*
026400020725     C/EXEC SQL
026500020725     C+ close C1
026600020725     C/END-EXEC
026700000905     C*
026800020305     C* Scarico i VAB rimasti "in sospeso"
026900071009     C                   if        wDISK = 'C'
027000140307     C                   z-add     sav_VABNCL    VABNCL
027100140307     C                   z-add     sav_VABPKB    VABPKB
027200140307     C                   z-add     sav_VABVLB    VABVLB
027300111130     C  N31              eval      fivabds_ok = fivabds
027400111130     C  N31              exsr      wrivab
027500071009     C                   endif
027600010202     C*
027700010202     C                   endif
027800990910
027900990910     C* Se non ci sono record con errori ...
028000000710     C                   if        §ctrno = 0
028100990910     C* ... restituisco esito OK.
028200990921     C                   eval      wrkesito = '0'
028300990910     C                   else
028400010201     C                   if        §ctrokvb > 0
028500990921     C                   eval      wrkesito = '1'
028600000710     C                   else
028700000710     C                   eval      wrkesito = '2'
028800990910     C                   endif
028900000710     C                   endif
029000990910     C*
029100990914     C                   if        %open(tivin00r)
029200990908     C                   close     tivin00r
029300990914     C                   endif
029400131016     C                   if        %open(FIVABWWr)
029500131016     C                   close     FIVABWWr
029600120706     C                   endif
029700131016     C                   if        %open(FIVATWWr)
029800131016     C                   close     FIVATWWr
029900010201     C                   endif
030000990910     C*
030100111031     C                   if        §ctrokvb > 0 or
030200111031     C                             §ctrokvt > 0
030300111031     C                             and wPOI <> *zeros
030400010202     C                   exsr      invio
030500990920     C                   endif
030600990920     C*
030700910830     C                   ENDSR
030800000613     C***
030900010305
031000010305     C*----------------------------------------------------*
031100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031200010305     C*----------------------------------------------------*
031300020305     C     WRIVAB        BEGSR
031400070730     C*
031500071003     C* Considerazioni finali
031600111125     C                   if        ok_VABRMA = *blanks
031700111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031800071003     C                   endif
031900071003     C*
032000131016     C                   write     FIVAB000                                     => scarico il VAB
032100010305     C*
032200010305     C                   ENDSR
032300990920
032400000801     C*----------------------------------------------------*
032500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032600000801     C*----------------------------------------------------*
032700010201     C     INZVAR        BEGSR
032800000801     C*
032900010201     C                   Z-ADD     *zeros        Num5_0
033000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033100020725     C                   MOVEL     '0'           FlgCAS            1
033200111125     C*
033300111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033400111130     C*
033500111125     C                   CLEAR                   FIVABDS
033600111130     C                   CLEAR                   FIVABDS_OK
033700111130     C                   CLEAR                   FIVABDS_SAV
033800131016     C                   CLEAR                   FIVAB000
033900131016     C                   CLEAR                   FIVAT000
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034400000801     C*----------------------------------------------------*
034500020904     C     DEFCAM        BEGSR
034600080923     C*
034700111125     C                   SETOFF                                       50
034800111125     C                   SETOFF                                       515253
034900131016     C                   SETOFF                                       54
035000070730     C* Imposto i valori di default...
035100141219     C                   EVAL      VABCCM = 0017124
035200141219     C                   EVAL      VATCCM = 0017124
035300141017     C                   EVAL      VABLNP = 001
035400141017     C                   EVAL      VATLNP = 001
035500140919     C                   EVAL      VABCTR = 000
035600131016     C                   EVAL      VABCBO = '1'
035700070222     C* ... e poi verifico se sono stati passati come parametri
035800070222     C                   IF        vlrppt > *blanks
035900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036100070222     C                   EXSR      CHKNUM
036200070222     C                   IF        PiInt=*on
036300070222     C                   Z-ADD     PiVal         VABCCM
036400070222     C                   Z-ADD     PiVal         VATCCM
036500070222     C                   ENDIF
036600070222     C                   ENDIF
036700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036900070222     C                   EXSR      CHKNUM
037000070222     C                   IF        PiInt=*on
037100070222     C                   Z-ADD     PiVal         VABLNP
037200070222     C                   Z-ADD     PiVal         VATLNP
037300070222     C                   ENDIF
037400070222     C                   ENDIF
037500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037700070222     C                   EXSR      CHKNUM
037800070222     C                   IF        PiInt=*on
037900070222     C                   Z-ADD     PiVal         VABCTR
038000070222     C                   ENDIF
038100070928     C                   ENDIF
038200071009     C                   MOVEL     *blanks       wDISK             1
038300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038400071009     C                   IF        wDISK <> *blanks
038500070928     C                   SETON                                        50
038600070222     C                   ENDIF
038700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038800111125     C                   SETON                                        41
038900080923     C                   ENDIF
039000070222     C                   ENDIF
039100120706     C*
039200120706     C* valido per nr.colli/peso/volume
039300120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039400120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039500120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039600131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039700120706     C                   SELECT
039800120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039900120706     C                   SETON                                        51
040000120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040100120706     C                   SETON                                        52
040200120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040300120706     C                   SETON                                        53
040400131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040500131016     C                   SETON                                        54
040600120706     C                   ENDSL
040700120706     C*
040800071009     C   50              EVAL      VABCTM = '7Q'
040900000801     C*
041000000801     C                   ENDSR
041100131016     C*----------------------------------------------------*
041200131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041300131016     C*----------------------------------------------------*
041400131016     C     REPNSP        BEGSR
041500131016     C*
041600131016     C                   EXSR      INZVAR
041700131016     C                   EXSR      DEFCAM
041800131016     C*
041900131016     C                   SETON                                        60
042000131016     C*
042100131016     C* NSP => Stacco un numeratore da AZNUM
042200131016     C                   clear                   TRUL33DS
042300131016     C                   eval      I33OPE = *zeros
042400131016     C                   eval      I33CNU = 302
042500131016     C                   eval      I33NUM = 1
042600131016     C                   movel     TRUL33DS      KPJBU
042700131016     C                   call      'TRUL33R'
042800131016     C                   parm                    KPJBA
042900131016     C                   movel     KPJBU         TRUL33DS
043000131016     C                   if        O33ERR = *zeros
043100131016     C                   z-add     O33NRF        VABNSP
043200131016     C                   z-add     O33NRF        VATNSP
043300140905     C***                z-add     O33NRF        VABRMN
043400131016     C                   else
043500131016     C                   SETON                                        31
043600131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043700140905     C                             + ' ' + 'VABNSP VATNSP'
043800131016     C                   endif
043900131016     C*
044000131016     C                   ENDSR
044100000801     C*----------------------------------------------------*
044200131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044300000801     C*----------------------------------------------------*
044400010201     C     IMPVAB        BEGSR
044500070530     C*
044600070530     C                   SETOFF                                       3132
044700070928     C*
044800070928     C                   EXSR      DEFCAM
044900070928     C*
045000111031     C                   IF        vlrpoi <> 999
045100070928     C                   MOVE(P)   vlrpoi        VABFGS
045200070928     C                   MOVE(P)   vlrpoi        VATFGS
045300111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045400111031     C                   ENDIF
045500070928     C*
045600070928     C                   MOVEL     datcor        VABAAS
045700070928     C                   MOVEL     datcor        VATAAS
045800070928     C                   MOVE      datcor        VABMGS
045900141017     C                   EVAL      VABRSD=%trim(%subst(vindta:15:30))
046000131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100140306     C     '@':'A'       XLATE     VABRSD        VABRSD
046200131016     C* ==
046300141017     C                   EVAL      VABIND=%trim(%subst(vindta:45:30))
046400141017     C                   EVAL      VABLOD=%trim(%subst(vindta:75:20))
046500141017     C                   EVAL      VABPRD=%trim(%subst(vindta:95:2))
046600141219     C                   EVAL      VABTIC=%trim(%subst(vindta:137:2))
046700141017     C                   EVAL      w70=%trim(%subst(vindta:139:70))
046800141017     C                   EVAL      VABNOT = %trim(%subst(w70:1:35))
046900141017     C                   EVAL      VABNT2 = %trim(%subst(w70:36:35))
047000140919
047100131016     C*
047200131016     C* CAD
047300141017     C                   EVAL      PiStr=%trim(%subst(vindta:97:5))
047400131016     C                   EXSR      CHKNUM
047500131016     C                   IF        PiInt=*on
047600131016     C                   Z-ADD     PiVal         Num5_0
047700131016     C                   MOVEL     Num5_0        VABCAD
047800131016     C                   ELSE
047900131016     C                   SETON                                        32
048000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100131016     C                             + ' ' + 'VABCAD'
048200131016     C                   ENDIF
048300140930     C* RMN / NSP
048400150410     C                   EVAL      PiStr=%trim(%subst(vindta:248:7))
048500140411     C                   EXSR      CHKNUM
048600140411     C                   IF        PiInt=*on
048700140411     C                   Z-ADD     PiVal         VABRMN
048800140930     C                   Z-ADD     PiVal         VABNSP
048900140411     C                   ELSE
049000140411     C                   SETON                                        32
049100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200140930     C                             + ' ' + 'VABRMN VABNSP'
049300140411     C                   ENDIF
049400140411     C* NCL
049500141017     C                   EVAL      PiStr=%trim(%subst(vindta:102:4))
049600140411     C                   EXSR      CHKNUM
049700140411     C                   IF        PiInt=*on
049800140411     C                   Z-ADD     PiVal         VABNCL
049900140411     C                   ELSE
050000140411     C                   SETON                                        32
050100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200140411     C                             + ' ' + 'VABNCL'
050300140411     C                   ENDIF
050400140411     C* PKB
050500141017     C                   EVAL      PiStr=%trim(%subst(vindta:106:6))
050600140411     C                   EXSR      CHKNUM
050700140411     C                   IF        PiNum=*on
050800141017     C                   EVAL      VABPKB = PiVal/100
050900140411     C                   ELSE
051000140411     C                   SETON                                        32
051100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051200140411     C                             + ' ' + 'VABPKB'
051300140411     C                   ENDIF
051400141219     C* CAS
051500141219     C                   IF        %subst(vindta:355:20) <> *blank
051600141219     C                   EVAL      vabVCA = 'EUR'
051700141219     C                   MOVEL     '1'           FlgCAS
051800141219     C                   EVAL      PiStr=
051900141219     C                              %trim(%subst(vindta:355:17)) +
052000141219     C                              %trim(%subst(vindta:355+18:2))
052100141219     C                   EXSR      CHKNUM
052200141219     C                   IF        PiNum=*on
052300141219     C                   EVAL      VABCAS = PiVal/100
052400141219     C                   ELSE
052500141219     C                   SETON                                        32
052600141219     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052700141219     C                             + ' ' + 'VABCAS'
052800141219     C                   ENDIF
052900141219     C                   ENDIF
053000140411     C*
053100140411     C* Reperisco la provincia dal CAP e dalla località
053200140411     C                   IF        VABCAD <> *blanks AND
053300140411     C                             VABPRD  = *blanks AND
053400140411     C                             VABNZD  = *blanks
053500140411     C                   CLEAR                   TISI95DS
053600140411     C                   EVAL      I95TCN = '3'
053700140411     C                   Z-ADD     datcor        I95DAT
053800140411     C                   EVAL      I95CAP = VABCAD
053900140411     C                   EVAL      I95LOC = VABLOD
054000140411     C                   CALL      'TISI95R'
054100140411     C                   PARM                    TISI95DS
054200140411     C                   EVAL      VABPRD = O95PRV
054300140411     C                   ENDIF
054400070730     C*
054500070730     C* Considerazioni finali su CBO/CAS
054600121113     C                   IF        FlgCAS <> '0'
054700070730     C                   IF        VABCBO = '1'
054800070730     C                   EVAL      VABCBO = '4'
054900070730     C                   ENDIF
055000070730     C                   IF        VABCBO = '2'
055100070730     C                   EVAL      VABCBO = '6'
055200070730     C                   ENDIF
055300070730     C                   ENDIF
055400020305     C*
055500011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055600011113     C                   EXSR      CHKIMPDIV
055700010202     C*
055800000801     C* Ebbene...
055900000801     C                   ADD       1             §CTRMO
056000070530     C                   IF        *in31 <> *zeros OR
056100070530     C                             *in32 <> *zeros
056200000801     C                   ADD       1             §CTRNO
056300020725     C                   EVAL      x_vinflg = '2'
056400000801     C                   ELSE
056500010201     C                   ADD       1             §CTROKVB
056600000801     C                   ENDIF
056700000801     C*
056800000801     C                   ENDSR
056900140411     C*----------------------------------------------------*
057000140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057100140930     C* nonostante questa routine non faccia pressoché nulla, devo non commentare la WRITE perché
057200140930     C* così i campi di VAT sono definiti.
057300140411     C*----------------------------------------------------*
057400140411     C     WRIVAT_A      BEGSR
057500140411     C*
057600140411     C                   eval      vatAAS = vabAAS
057700140411     C                   eval      vatLNP = vabLNP
057800140411     C                   eval      vatNRS = vabNRS
057900140411     C                   eval      vatNSP = vabNSP
058000140411     C*
058100140411     C* Valorizzo il buffer di scrittura del FIVAT
058200140411     C* - TIPO RECORD "A"
058300140930     C                   if        1 <> 1
058400140919     C***                if        %subst(w100:71:30) <> *blanks
058500140919     C***                eval      VATTRC = 'A'
058600140919     C***                eval      VATNOT = %trim(%subst(w100:71:30))
058700141222     C***                write     FIVAT000
058800140930     C                   endif
058900140411     C*
059000140411     C                   ENDSR
059100140411     C*----------------------------------------------------*
059200140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059300140411     C*----------------------------------------------------*
059400140411     C     WRIVAT_B      BEGSR
059500140930     C*
059600140930     C***                eval      vatAAS = vabAAS
059700140930     C***                eval      vatLNP = vabLNP
059800140930     C***                eval      vatNRS = vabNRS
059900140930     C***                eval      vatNSP = vabNSP
060000140930     C***
060100140930     C**Valorizzo il buffer di scrittura del FIVAT
060200140930     C**- TIPO RECORD "B"
060300140930     C***                IF        %trim(%subst(vindta:407:20)) <> *blank
060400140930     C***                eval      VATTRC = 'B'
060500140930     C***                eval      VATNOT = %trim(%subst(vindta:407:20))
060600140930     C***                write     FIVAT000
060700140930     C***                endif
060800140411     C*
060900140411     C                   ENDSR
061000140411     C*----------------------------------------------------*
061100140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
061200140411     C*----------------------------------------------------*
061300140411     C     WRIVAT_I      BEGSR
061400140411     C*
061500140411     C                   eval      vatAAS = vabAAS
061600140411     C                   eval      vatLNP = vabLNP
061700140411     C                   eval      vatNRS = vabNRS
061800140411     C                   eval      vatNSP = vabNSP
061900140411     C*
062000140411     C* Valorizzo il buffer di scrittura del FIVAT
062100140411     C* - TIPO RECORD "I"
062200150410     C                   EVAL      w70=%trim(%subst(vindta:255:70))
062300141017     C                   if        %subst(w70:1:35) <> *blanks
062400141017     C                   eval      VATTRC = 'I'
062500141017     C                   eval      VATNOT = %trim(%subst(w70:1:35))
062600141017     C                   write     FIVAT000
062700141017     C                   endif
062800141017     C                   if        %subst(w70:36:35) <> *blanks
062900141017     C                   eval      VATTRC = 'J'
063000141017     C                   eval      VATNOT = %trim(%subst(w70:36:35))
063100141017     C                   write     FIVAT000
063200141017     C                   endif
063300140411     C*
063400140411     C                   ENDSR
063500140411     C*----------------------------------------------------*
063600140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
063700140411     C*----------------------------------------------------*
063800140411     C     WRIVAT_S      BEGSR
063900140411     C*
064000140411     C                   eval      vatAAS = vabAAS
064100140411     C                   eval      vatLNP = vabLNP
064200140411     C                   eval      vatNRS = vabNRS
064300140411     C                   eval      vatNSP = vabNSP
064400140411     C*
064500140411     C* Valorizzo il buffer di scrittura del FIVAT
064600140411     C* - TIPO RECORD "S"
064700140723     C***                IF        %trim(%subst(vindta:504:16)) <> *blank
064800140723     C***                eval      VATTRC = 'S'
064900140723     C***                eval      VATNOT = %trim(%subst(vindta:504:16))
065000140723     C***                EVAL      %subst(VATNOT:17:2) = 'SS'
065100140723     C***                write     FIVAT000
065200140723     C***                endif
065300140411     C*
065400140411     C                   ENDSR
065500140411     C*----------------------------------------------------*
065600140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
065700140411     C*----------------------------------------------------*
065800140411     C     WRIVAT_E      BEGSR
065900140411     C*
066000140411     C                   eval      vatAAS = vabAAS
066100140411     C                   eval      vatLNP = vabLNP
066200140411     C                   eval      vatNRS = vabNRS
066300140411     C                   eval      vatNSP = vabNSP
066400140411     C*
066500140411     C* Valorizzo il buffer di scrittura del FIVAT
066600140411     C* - TIPO RECORD "E"
066700141017     C                   eval      VATTRC = 'E'
066800141222     C                   eval      VATNOT = %trim(%subst(vindta:232:16))
066900141017     C                   write     FIVAT000
067000140411     C*
067100140411     C                   ENDSR
067200020904
067300020904
067400010202     C*----------------------------------------------------*
067500131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
067600010202     C*----------------------------------------------------*
067700020305     C     PREVAT        BEGSR
067800010202     C*
067900131016     C* Compongo il nome del membro da dare al FIVATWWR
068000010202     C                   eval      parmbr = vlrhdl
068100010202     C                   movel     'M'           parmbr
068200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068300010202     C                   eval      paropz = '1'
068400010202     C* Effettuo la chiamata al CLLE preposto
068500131017     C                   call(e)   'TITVVTC'
068600010202     C                   parm                    parccm
068700010202     C                   parm                    parmbr
068800010202     C                   parm                    paropz
068900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069000010202     C                   if        %error
069100010202     C                   movel     '1'           chkcall
069200010202     C                   else
069300010202     C                   movel     '0'           chkcall
069400010202     C                   endif
069500010202     C*
069600010202     C                   ENDSR
069700000801     C*----------------------------------------------------*
069800000801     C*  CONTROLLO NUMERICITA' CAMPI
069900000801     C*----------------------------------------------------*
070000000801     C     CHKNUM        BEGSR
070100081113     C*
070200081113     C                   IF        PiDecChr = *blanks
070300140411     C                   EVAL      PiDecChr = ','
070400081113     C                   ENDIF
070500081113     C*
070600081113     C                   callp(e)  UBISNUM_Check(PiStr
070700081113     C                                          :PiDecChr
070800081113     C                                          :PiVal
070900081113     C                                          :PiNum
071000081113     C                                          :PiInt)
071100081113     C*
071200000801     C                   IF        %error
071300000801     C                   EVAL      PiInt=*off
071400000801     C                   ENDIF
071500000801     C*
071600000801     C                   ENDSR
071700000801     C***
071800000801
071900011113
072000011113     C*----------------------------------------------------*
072100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
072200011113     C*----------------------------------------------------*
072300011113     C     CHKIMPDIV     BEGSR
072400011113     C*
072500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
072600011113     C                   Z-ADD     *zeros        wrkDec            9 9
072700011113     C*
072800011113     C* Come prima cosa effettuo considerazioni sulla divisa
072900011113     C                   IF        vabIAS > *zeros
073000011113     C                   IF        vabVAS <> 'EUR'
073100011113     C                   EVAL      vabVAS =  'ITL'
073200011113     C                   ENDIF
073300011113     C                   ENDIF
073400011113     C*
073500011113     C                   IF        vabCAS > *zeros
073600011113     C                   IF        vabVCA <> 'EUR'
073700011113     C                   EVAL      vabVCA =  'ITL'
073800011113     C                   ENDIF
073900011113     C                   ENDIF
074000011113     C*
074100011113     C                   IF        vabVMD > *zeros
074200020305     C                   IF        vabVAD <> 'EUR'
074300011113     C                   EVAL      vabVAD =  'ITL'
074400011113     C                   ENDIF
074500011113     C                   ENDIF
074600011113     C*
074700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
074800011113     C                   Z-ADD     vabIAS        wrkDec
074900011113     C                   IF        wrkDec > *zeros
075000011113     C                   IF        vabVAS = 'ITL'
075100011113     C                   EVAL      vabIAS = *zeros
075200011113     C                   ENDIF
075300011113     C                   ENDIF
075400011113     C*
075500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
075600011113     C                   Z-ADD     vabCAS        wrkDec
075700011113     C                   IF        wrkDec > *zeros
075800011113     C                   IF        vabVCA = 'ITL'
075900011113     C                   EVAL      vabCAS = *zeros
076000011113     C                   ENDIF
076100011113     C                   ENDIF
076200011113     C*
076300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
076400011113     C                   Z-ADD     vabVMD        wrkDec
076500011113     C                   IF        wrkDec > *zeros
076600011113     C                   IF        vabVAD = 'ITL'
076700011113     C                   EVAL      vabVMD = *zeros
076800011113     C                   ENDIF
076900011113     C                   ENDIF
077000011113     C*
077100011113     C                   ENDSR
077200011113     C***
077300011113
077400011113
077500000801
077600000801
077700990920      /TITLE Invio dei dati al punto operativo.
077800010202     C     invio         BEGSR
077900990920     C*
078000131016     C* 1° invio FIVAT
078100010201     C                   reset                   dscmz
078200111031     C                   move      wPOI          cmzdst
078300131016     C                   eval      cmzfld = 'FIVATWWR'
078400010201     C                   eval      cmzmbd = vlrhdl
078500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078600021009     C***                if        prmfir = *blanks
078700131016     C                   eval      cmzfla = 'FIVAT00F'
078800131016     C                   eval      cmzmba = 'FIVAT00F'
078900021009     C***                else
079000021009     C***                eval      cmzfla = prmfir
079100021009     C***                eval      cmzmba = prmfir
079200021009     C***                endif
079300010201     C                   eval      cmznrr = *zeros
079400020305     C                   move      §ctrokvt      cmznrr
079500021018     C                   eval      cmzlba = vlrfl1
079600010201     C                   call(e)   'TIS711C'
079700010201     C                   parm                    dscmz
079800010201     C                   parm      *blanks       esito
079900010205     C                   if        %error
080000010205     C                             or cmzerr = '1'
080100010205     C                             or esito  = '1'
080200010205     C                   eval      wrkesito = '3'
080300010205     C                   else
080400010201     C*
080500131016     C* 2° invio FIVAB
080600010201     C                   reset                   dscmz
080700111031     C                   move      wPOI          cmzdst
080800010201     C                   eval      cmzfld = vlrfou
080900010201     C                   eval      cmzmbd = vlrhdl
081000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081100021009     C***                if        prmfir = *blanks
081200131016     C                   eval      cmzfla = 'FIVAB00F'
081300131016     C                   eval      cmzmba = 'FIVAB00F'
081400021009     C***                else
081500021009     C***                eval      cmzfla = prmfir
081600021009     C***                eval      cmzmba = prmfir
081700021009     C***                endif
081800010201     C                   eval      cmznrr = *zeros
081900010201     C                   move      §ctrokvb      cmznrr
082000021018     C                   eval      cmzlba = vlrfl1
082100010201     C                   call(e)   'TIS711C'
082200010201     C                   parm                    dscmz
082300010201     C                   parm      *blanks       esito
082400010201     C                   if        %error
082500010201     C                             or cmzerr = '1'
082600010201     C                             or esito  = '1'
082700010201     C                   eval      wrkesito = '3'
082800010201     C                   endif
082900010205     C                   endif
083000990920     C*
083100000613     C                   ENDSR
083200000613     C***
083300070411
083400070411     C     *pssr         BEGSR
083500070411     C*
083600070411     C                   if        %open(tivin00r)
083700070411     C                   close     tivin00r
083800070411     C                   endif
083900131016     C                   if        %open(FIVABWWr)
084000131016     C                   close     FIVABWWr
084100120706     C                   endif
084200131016     C                   if        %open(FIVATWWr)
084300131016     C                   close     FIVATWWr
084400070411     C                   endif
084500070411     C*
084600070411     C* Effettuo la chiamata al CLLE preposto
084700131017     C                   call(e)   'TITVVTC'
084800070411     C                   parm                    parccm
084900070411     C                   parm                    parmbr
085000070411     C                   parm      '2'           paropz
085100070411     C*
085200070411     C                   eval      wrkesito = '2'
085300070411     C*
085400070411     C                   seton                                        LR
085500070411     C*
085600070411     C                   ENDSR     '*CANCL'
085700070411     C***
085800070411
085900990910
086000000613     C     *inzsr        BEGSR
086100990910     C*
086200990910     C     *entry        plist
086300990920     C                   parm                    tivlrds
086400990921     C                   parm      wrkesito      esito
086500000724     C                   parm                    prmlit
086600000710     C                   parm                    prmfir
086700000613     C*
086800000830     C* CALCOLA LA DATA CORRENTE
086900120706     C                   time                    wn14             14 0
087000120706     C                   movel     wn14          oracor            6 0          *ORA
087100100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087200100324     C                   eval      datcor = %dec(%date() : *ISO)
087300000830     C*
087400000613     C                   ENDSR
087500000613     C***
