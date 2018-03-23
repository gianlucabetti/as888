000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140919      * - disk A
000500140930      * - RSD/IND/LOD/CADPRD/NZD hanno 2 posizioni per essere presi, una preferenziale
000600140930      *   solo per RSD, la non preferenziale viene messa su RD2 se esiste la preferenziale
000700140723      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900990908
001000020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001100131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
001200131016     FFIVATWWr  O    E             DISK    usropn
001300990908
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100070730     D tisi95ds      e ds
002200131016     D fivabds       e ds                  extname(FIVAB00f)
002300131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
002400131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
002500990910     D esito           s              1
002600000724     D prmlit          s             10
002700000710     D prmfir          s             10
002800990921     D wrkesito        s                   like(esito)
002900000613     D rrnum           s              6  0 INZ(*zeros)
003000140930     D depspe          s             16    INZ(*blanks)
003100140930     D curspe          s             16    INZ(*blanks)
003200010202     D parccm          s              8    INZ(*blanks)
003300010202     D parmbr          s             10    INZ(*blanks)
003400010202     D paropz          s              1    INZ(*blanks)
003500010202     D chkcall         s              1    INZ(*blanks)
003600080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003700070530     D Num5_0          s              5  0
003800131016     D w70             s             70
003900140930     D w128            s            128
004000000830
004100020725
004200020725     D*------------------
004300020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004400020725     D*------------------
004500070530     D INPUT_DS        DS                  INZ
004600101007     D  VINDTA                     2048
004700101007     D  VINFLG                        1
004800140930     D  VINSPE                       16
004900101007     D  VINRRN                        8  0
005000020725     D*
005100080923     D*------------------
005200080923     D* DS REPERIMENTO NUMERATORE
005300080923     D*------------------
005400080923     D trul33ds      e ds                  inz
005500080923     D*------------------
005600080923     D* DS ARCHITETTURA
005700080923     D*------------------
005800080923     D kpjba         e ds                  inz
005900080923     D*------------------
006000121113     D  Num7_0         s              7  0
006100081113
006200081113     D*------------------
006300081113     D* LINKING A DEFINIZIONI ESTERNE
006400081113     D*------------------
006500100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006700081113
006800990908
006900010201
007000010201
007100080117     C*
007200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007300080117     C
007400080117     C/EXEC SQL
007500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007600080117     C/END-EXEC
007700080117     C
007800000913     C                   reset                   rrnum
007900990921     C                   reset                   esito
008000990921     C                   reset                   wrkesito
008100000613     C*
008200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008300000613     C*
008400010202     C* Effettuo la chiamata al CLLE preposto
008500131017     C                   call(e)   'TITVVTC'
008600010202     C                   parm                    parccm
008700010202     C                   parm                    parmbr
008800010202     C                   parm      '2'           paropz
008900070730     C*
009000070730     C* Effettuo lancio TISI95 solo x chiusura
009100070730     C                   CLEAR                   TISI95DS
009200070730     C                   EVAL      I95TLA = 'C'
009300070730     C                   CALL      'TISI95R'
009400070730     C                   PARM                    TISI95DS
009500000616     C*
009600000801     C
009700010201     C                   seton                                        LR
009800990908
009900000801
010000910830     C*--------------------------------------------------------
010100131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010200910830     C*--------------------------------------------------------
010300070530     C     RWFILE        BEGSR
010400990910     C*
010500990914     C                   if        not %open(tivin00r)
010600990908     C                   open      tivin00r
010700990914     C                   endif
010800131016     C                   if        not %open(FIVABWWr)
010900131016     C                   open      FIVABWWr
011000990914     C                   endif
011100070530     C*
011200131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011300020305     C                   exsr      prevat
011400010201     C*
011500010202     C                   if        chkcall = '0'
011600010202     C*
011700131016     C                   if        not %open(FIVATWWr)
011800131016     C                   open      FIVATWWr
011900010201     C                   endif
012000080117     C*
012100080117     C                   EXSR      INZVAR
012200080117     C                   EXSR      DEFCAM
012300990910     C*
012400010201     C                   clear                   §CTROKVB          5 0
012500020305     C                   clear                   §CTROKVT          5 0
012600000801     C                   clear                   §CTRMO            5 0
012700000801     C                   clear                   §CTRNO            5 0
012800990910     C*
012900020725     C/EXEC SQL
013000020725     C+ declare C1 cursor for select
013100140930     C+ vindta, vinflg, substr(vindta, 1, 16) as sped, rrn(tivin00r)
013200060223     C+ from tivin00r
013300060223     C+ order by sped
013400020725     C+ for read only
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800020725     C+ open C1
013900020725     C/END-EXEC
014000020725     C
014100020725     C/EXEC SQL
014200070530     C+ Fetch C1 into :INPUT_DS
014300020725     C/END-EXEC
014400020725     C*
014500020725     C                   dow       sqlcod = *zeros
014600990913     C*
014700020725     C                   if        vindta > *blanks
014800000613     C                   add       1             rrnum
014900000801     C*
015000020725     C                   if        vinflg = *blanks
015100020725     C                             or vinflg = '0'
015200020725     C                             or vinflg = '2'
015300000801     C*
015400020725     C                   clear                   x_vinmsg
015500020725     C                   eval      x_vinflg = '1'
015600010305     C*
015700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015800140930     C* (uso NSP)
015900140930     C                   EVAL      PiStr=%trim(%subst(vindta:1:16))
016000020305     C                   MOVEL(p)  PiStr         curspe
016100010305     C*
016200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016300080923     C                   if        *in50 = *off
016400140930     C***                exsr      repNSP
016500071003     C                   exsr      impvab
016600111130     C  N31              eval      fivabds_ok = fivabds
016700111130     C  N31              exsr      wrivab
016800140411     C                   exsr      wrivat_a                                     => carico VAT
016900140328     C                   exsr      wrivat_b                                     => carico VAT
017000140411     C                   exsr      wrivat_i                                     => carico VAT
017100140411     C                   exsr      wrivat_s                                     => carico VAT
017200111125     C                   exsr      inzvar
017300071003     C                   else
017400080923     C*
017500071009     C                   if        wDISK = 'D'
017600140930     C***                exsr      repNSP
017700071009     C                   exsr      impvab
017800111130     C  N31              eval      fivabds_ok = fivabds
017900111130     C  N31              exsr      wrivab
018000140411     C                   exsr      wrivat_a                                     => carico VAT
018100140328     C                   exsr      wrivat_b                                     => carico VAT
018200140411     C                   exsr      wrivat_i                                     => carico VAT
018300140411     C                   exsr      wrivat_s                                     => carico VAT
018400140411     C                   exsr      wrivat_e                                     => carico VAT
018500111125     C                   exsr      inzvar
018600071009     C                   else
018700071009     C*
018800010305     C                   if        depspe = *blanks                             => 1° giro
018900010305     C                   eval      depspe = curspe                              => memorizz. spediz
019000080117     C                   clear                   tivinds
019100140930     C***                exsr      repNSP
019200020305     C                   exsr      impvab
019300111130     C                   z-add     VABNCL        sav_VABNCL
019400111130     C                   z-add     VABPKB        sav_VABPKB
019500111130     C                   z-add     VABVLB        sav_VABVLB
019600140411     C                   exsr      wrivat_a                                     => carico VAT
019700140328     C                   exsr      wrivat_b                                     => carico VAT
019800140411     C                   exsr      wrivat_i                                     => carico VAT
019900140411     C                   exsr      wrivat_s                                     => carico VAT
020000140411     C   50              exsr      wrivat_e                                     => carico VAT
020100010305     C                   else
020200020725     C                   if        depspe <> curspe                             => rottura di spediz
020300111130     C                   z-add     sav_VABNCL    VABNCL
020400111130     C                   z-add     sav_VABPKB    VABPKB
020500111130     C                   z-add     sav_VABVLB    VABVLB
020600111130     C  N31              eval      fivabds_ok = fivabds
020700111125     C  N31              exsr      wrivab
020800111125     C                   exsr      inzvar
020900111125     C                   eval      depspe = curspe                              => memorizz. spediz
021000080117     C                   clear                   tivinds
021100140930     C***                exsr      repNSP
021200020305     C                   exsr      impvab
021300111130     C                   if        wCntRecSpe = *zeros
021400111130     C                   z-add     VABNCL        sav_VABNCL
021500111130     C                   z-add     VABPKB        sav_VABPKB
021600111130     C                   z-add     VABVLB        sav_VABVLB
021700111130     C                   endif
021800111130     C                   add       1             wCntRecSpe
021900140411     C                   exsr      wrivat_a                                     => carico VAT
022000140328     C                   exsr      wrivat_b                                     => carico VAT
022100140411     C                   exsr      wrivat_i                                     => carico VAT
022200140411     C                   exsr      wrivat_s                                     => carico VAT
022300140411     C   50              exsr      wrivat_e                                     => carico VAT
022400020305     C                   else                                                   => x stessa spediz
022500111130     C                   exsr      impvab
022600120706     C   51              add       VABNCL        sav_VABNCL
022700111125     C   51              add       VABPKB        sav_VABPKB
022800111125     C   51              add       VABVLB        sav_VABVLB
022900111125     C   52              z-add     VABNCL        sav_VABNCL
023000111125     C   52              z-add     VABPKB        sav_VABPKB
023100111125     C   52              z-add     VABVLB        sav_VABVLB
023200111125     C   53              add       VABNCL        sav_VABNCL
023300111125     C   53              z-add     VABPKB        sav_VABPKB
023400111125     C   53              z-add     VABVLB        sav_VABVLB
023500131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023600131016     C   54              ADD       VABPKB        sav_VABPKB
023700131016     C   54              ADD       VABVLB        sav_VABVLB
023800111125     C*
023900140411     C                   exsr      wrivat_a                                     => carico VAT
024000140328     C                   exsr      wrivat_b                                     => carico VAT
024100140411     C                   exsr      wrivat_i                                     => carico VAT
024200140411     C                   exsr      wrivat_s                                     => carico VAT
024300140411     C   50              exsr      wrivat_e                                     => carico VAT
024400010305     C                   endif
024500010305     C                   endif
024600010305     C                   endif
024700071003     C                   endif
024800071009     C                   endif
024900000905     C*
025000000905     C                   else
025100020725     C                   eval      x_vinflg = '1'
025200000905     C                   endif
025300000905     C*
025400020725     C     VINRRN        chain     tivin000
025500020725     C                   if        %found(tivin00r)
025600020725     C                   eval      y_vinflg = x_vinflg
025700020725     C                   eval      y_vinmsg = x_vinmsg
025800020725     C                   update    tivin000
025900020725     C                   endif
026000020725     C*
026100020725     C/EXEC SQL
026200070530     C+ Fetch C1 into :INPUT_DS
026300020725     C/END-EXEC
026400020725     C*
026500020725     C                   enddo
026600020725     C*
026700020725     C/EXEC SQL
026800020725     C+ close C1
026900020725     C/END-EXEC
027000000905     C*
027100020305     C* Scarico i VAB rimasti "in sospeso"
027200071009     C                   if        wDISK = 'C'
027300140307     C                   z-add     sav_VABNCL    VABNCL
027400140307     C                   z-add     sav_VABPKB    VABPKB
027500140307     C                   z-add     sav_VABVLB    VABVLB
027600111130     C  N31              eval      fivabds_ok = fivabds
027700111130     C  N31              exsr      wrivab
027800071009     C                   endif
027900010202     C*
028000010202     C                   endif
028100990910
028200990910     C* Se non ci sono record con errori ...
028300000710     C                   if        §ctrno = 0
028400990910     C* ... restituisco esito OK.
028500990921     C                   eval      wrkesito = '0'
028600990910     C                   else
028700010201     C                   if        §ctrokvb > 0
028800990921     C                   eval      wrkesito = '1'
028900000710     C                   else
029000000710     C                   eval      wrkesito = '2'
029100990910     C                   endif
029200000710     C                   endif
029300990910     C*
029400990914     C                   if        %open(tivin00r)
029500990908     C                   close     tivin00r
029600990914     C                   endif
029700131016     C                   if        %open(FIVABWWr)
029800131016     C                   close     FIVABWWr
029900120706     C                   endif
030000131016     C                   if        %open(FIVATWWr)
030100131016     C                   close     FIVATWWr
030200010201     C                   endif
030300990910     C*
030400111031     C                   if        §ctrokvb > 0 or
030500111031     C                             §ctrokvt > 0
030600111031     C                             and wPOI <> *zeros
030700010202     C                   exsr      invio
030800990920     C                   endif
030900990920     C*
031000910830     C                   ENDSR
031100000613     C***
031200010305
031300010305     C*----------------------------------------------------*
031400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031500010305     C*----------------------------------------------------*
031600020305     C     WRIVAB        BEGSR
031700070730     C*
031800071003     C* Considerazioni finali
031900111125     C                   if        ok_VABRMA = *blanks
032000111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
032100071003     C                   endif
032200071003     C*
032300131016     C                   write     FIVAB000                                     => scarico il VAB
032400010305     C*
032500010305     C                   ENDSR
032600990920
032700000801     C*----------------------------------------------------*
032800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032900000801     C*----------------------------------------------------*
033000010201     C     INZVAR        BEGSR
033100000801     C*
033200010201     C                   Z-ADD     *zeros        Num5_0
033300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033400020725     C                   MOVEL     '0'           FlgCAS            1
033500111125     C*
033600111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033700111130     C*
033800111125     C                   CLEAR                   FIVABDS
033900111130     C                   CLEAR                   FIVABDS_OK
034000111130     C                   CLEAR                   FIVABDS_SAV
034100131016     C                   CLEAR                   FIVAB000
034200131016     C                   CLEAR                   FIVAT000
034300000801     C*
034400000801     C                   ENDSR
034500000801     C*----------------------------------------------------*
034600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034700000801     C*----------------------------------------------------*
034800020904     C     DEFCAM        BEGSR
034900080923     C*
035000111125     C                   SETOFF                                       50
035100111125     C                   SETOFF                                       515253
035200131016     C                   SETOFF                                       54
035300070730     C* Imposto i valori di default...
035400140930     C                   EVAL      VABCCM = 0770567
035500140930     C                   EVAL      VATCCM = 0770567
035600140930     C                   EVAL      VABLNP = 077
035700140930     C                   EVAL      VATLNP = 077
035800140919     C                   EVAL      VABCTR = 000
035900131016     C                   EVAL      VABCBO = '1'
036000070222     C* ... e poi verifico se sono stati passati come parametri
036100070222     C                   IF        vlrppt > *blanks
036200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036400070222     C                   EXSR      CHKNUM
036500070222     C                   IF        PiInt=*on
036600070222     C                   Z-ADD     PiVal         VABCCM
036700070222     C                   Z-ADD     PiVal         VATCCM
036800070222     C                   ENDIF
036900070222     C                   ENDIF
037000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037200070222     C                   EXSR      CHKNUM
037300070222     C                   IF        PiInt=*on
037400070222     C                   Z-ADD     PiVal         VABLNP
037500070222     C                   Z-ADD     PiVal         VATLNP
037600070222     C                   ENDIF
037700070222     C                   ENDIF
037800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038000070222     C                   EXSR      CHKNUM
038100070222     C                   IF        PiInt=*on
038200070222     C                   Z-ADD     PiVal         VABCTR
038300070222     C                   ENDIF
038400070928     C                   ENDIF
038500071009     C                   MOVEL     *blanks       wDISK             1
038600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038700071009     C                   IF        wDISK <> *blanks
038800070928     C                   SETON                                        50
038900070222     C                   ENDIF
039000080923     C                   IF        %subst(vlrppt:15:1) = 'S'
039100111125     C                   SETON                                        41
039200080923     C                   ENDIF
039300070222     C                   ENDIF
039400120706     C*
039500120706     C* valido per nr.colli/peso/volume
039600120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039700120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039800120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039900131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
040000120706     C                   SELECT
040100120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
040200120706     C                   SETON                                        51
040300120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040400120706     C                   SETON                                        52
040500120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040600120706     C                   SETON                                        53
040700131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040800131016     C                   SETON                                        54
040900120706     C                   ENDSL
041000120706     C*
041100071009     C   50              EVAL      VABCTM = '7Q'
041200000801     C*
041300000801     C                   ENDSR
041400131016     C*----------------------------------------------------*
041500131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041600131016     C*----------------------------------------------------*
041700131016     C     REPNSP        BEGSR
041800131016     C*
041900131016     C                   EXSR      INZVAR
042000131016     C                   EXSR      DEFCAM
042100131016     C*
042200131016     C                   SETON                                        60
042300131016     C*
042400131016     C* NSP => Stacco un numeratore da AZNUM
042500131016     C                   clear                   TRUL33DS
042600131016     C                   eval      I33OPE = *zeros
042700131016     C                   eval      I33CNU = 302
042800131016     C                   eval      I33NUM = 1
042900131016     C                   movel     TRUL33DS      KPJBU
043000131016     C                   call      'TRUL33R'
043100131016     C                   parm                    KPJBA
043200131016     C                   movel     KPJBU         TRUL33DS
043300131016     C                   if        O33ERR = *zeros
043400131016     C                   z-add     O33NRF        VABNSP
043500131016     C                   z-add     O33NRF        VATNSP
043600140905     C***                z-add     O33NRF        VABRMN
043700131016     C                   else
043800131016     C                   SETON                                        31
043900131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044000140905     C                             + ' ' + 'VABNSP VATNSP'
044100131016     C                   endif
044200131016     C*
044300131016     C                   ENDSR
044400000801     C*----------------------------------------------------*
044500131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044600000801     C*----------------------------------------------------*
044700010201     C     IMPVAB        BEGSR
044800070530     C*
044900070530     C                   SETOFF                                       3132
045000070928     C*
045100070928     C                   EXSR      DEFCAM
045200070928     C*
045300111031     C                   IF        vlrpoi <> 999
045400070928     C                   MOVE(P)   vlrpoi        VABFGS
045500070928     C                   MOVE(P)   vlrpoi        VATFGS
045600111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045700111031     C                   ENDIF
045800070928     C*
045900070928     C                   MOVEL     datcor        VABAAS
046000070928     C                   MOVEL     datcor        VATAAS
046100070928     C                   MOVE      datcor        VABMGS
046200140930     C                   SELECT
046300140930     C                   WHEN      %subst(vindta:476:36) <> *BLANK
046400140930     C                   EVAL      VABRSD=%trim(%subst(vindta:476:36))
046500140930     C                   EVAL      VABRD2=%trim(%subst(vindta:339:36))
046600140930     C                   WHEN      %subst(vindta:339:36) <> *BLANK
046700140930     C                   EVAL      VABRSD=%trim(%subst(vindta:339:36))
046800140930     C                   ENDSL
046900131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
047000140306     C     '@':'A'       XLATE     VABRSD        VABRSD
047100131016     C* ==
047200140930     C                   SELECT
047300140930     C                   WHEN      %subst(vindta:512:36) <> *BLANK
047400140930     C                   EVAL      VABIND=%trim(%subst(vindta:512:36))
047500140930     C                   WHEN      %subst(vindta:375:36) <> *BLANK
047600140930     C                   EVAL      VABIND=%trim(%subst(vindta:375:36))
047700140930     C                   ENDSL
047800140930     C                   SELECT
047900140930     C                   WHEN      %subst(vindta:548:36) <> *BLANK
048000140930     C                   EVAL      VABLOD=%trim(%subst(vindta:548:36))
048100140930     C                   WHEN      %subst(vindta:411:36) <> *BLANK
048200140930     C                   EVAL      VABLOD=%trim(%subst(vindta:411:36))
048300140930     C                   ENDSL
048400140930     C                   SELECT
048500140930     C                   WHEN      %subst(vindta:593:2) <> *BLANK
048600140930     C                   EVAL      VABPRD=%trim(%subst(vindta:593:2))
048700140930     C                   WHEN      %subst(vindta:456:2) <> *BLANK
048800140930     C                   EVAL      VABPRD=%trim(%subst(vindta:456:2))
048900140930     C                   ENDSL
049000140930     C                   SELECT
049100140930     C                   WHEN      %subst(vindta:595:2) <> *BLANK
049200140930     C                   EVAL      VABNZD=%trim(%subst(vindta:595:2))
049300140930     C                   WHEN      %subst(vindta:458:2) <> *BLANK
049400140930     C                   EVAL      VABNZD=%trim(%subst(vindta:458:2))
049500140930     C                   ENDSL
049600140930     C                   IF        VABNZD = 'IT'
049700140930     C                   EVAL      VABNZD = *blank
049800140930     c                   ENDIF
049900140930     C                   EVAL      VABRMA=%trim(%subst(vindta:1:16))
050000140930     C                   EVAL      w128=%trim(%subst(vindta:657:128))
050100140930     C                   EVAL      VABNOT = %trim(%subst(w128:1:35))
050200140930     C                   EVAL      VABNT2 = %trim(%subst(w128:36:35))
050300140930     C                   EVAL      VABNAS=%trim(%subst(vindta:796:4))
050400140930     C                   IF        %subst(vindta:804:1) <> *blank
050500140930     C                   EVAL      VABFFD = 'S'
050600140930     c                   ENDIF
050700140919
050800131016     C*
050900131016     C* CAD
051000140930     C                   SELECT
051100140930     C                   WHEN      %subst(vindta:584:9) <> *BLANK
051200140930     C                   EVAL      PiStr=%trim(%subst(vindta:584:9))
051300140930     C                   WHEN      %subst(vindta:447:9) <> *BLANK
051400140930     C                   EVAL      PiStr=%trim(%subst(vindta:447:9))
051500140930     C                   ENDSL
051600131016     C                   EXSR      CHKNUM
051700131016     C                   IF        PiInt=*on
051800131016     C                   Z-ADD     PiVal         Num5_0
051900131016     C                   MOVEL     Num5_0        VABCAD
052000131016     C                   ELSE
052100131016     C                   SETON                                        32
052200131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052300131016     C                             + ' ' + 'VABCAD'
052400131016     C                   ENDIF
052500140328     C* CAS
052600140930     C                   IF        %subst(vindta:630:10)<> *blanks    AND
052700140930     C                             %subst(vindta:630:10)<> *zeros
052800140328     C                   EVAL      FlgCAS = '1'
052900140930     C                   EVAL      VABVCA =%trim(%subst(vindta:640:4))
053000140930     C                   EVAL      PiStr=%trim(%subst(vindta:630:10))
053100140328     C                   EXSR      CHKNUM
053200140328     C                   IF        PiNum=*on
053300141003     C                   EVAL      VABCAS = PiVal/100
053400140328     C                   ELSE
053500140328     C  N51              SETON                                        32
053600140328     C  N51              Z-ADD     *zeros        VABCAS
053700140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053800140328     C                             + ' ' + 'VABCAS'
053900140328     C                   ENDIF
054000140328     C                   ELSE
054100140328     C                   EVAL      FlgCAS = '0'
054200140328     C                   ENDIF
054300140930     C* RMN / NSP
054400140930     C                   EVAL      PiStr=%trim(%subst(vindta:1:16))
054500140411     C                   EXSR      CHKNUM
054600140411     C                   IF        PiInt=*on
054700140411     C                   Z-ADD     PiVal         VABRMN
054800140930     C                   Z-ADD     PiVal         VABNSP
054900140411     C                   ELSE
055000140411     C                   SETON                                        32
055100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055200140930     C                             + ' ' + 'VABRMN VABNSP'
055300140411     C                   ENDIF
055400140411     C* NCL
055500140930     C                   EVAL      PiStr=%trim(%subst(vindta:597:6))
055600140411     C                   EXSR      CHKNUM
055700140411     C                   IF        PiInt=*on
055800140411     C                   Z-ADD     PiVal         VABNCL
055900140411     C                   ELSE
056000140411     C                   SETON                                        32
056100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056200140411     C                             + ' ' + 'VABNCL'
056300140411     C                   ENDIF
056400140411     C* PKB
056500140930     C                   EVAL      PiStr=%trim(%subst(vindta:603:6))
056600140411     C                   EXSR      CHKNUM
056700140411     C                   IF        PiNum=*on
056800140919     C                   EVAL      VABPKB = PiVal
056900140411     C                   ELSE
057000140411     C                   SETON                                        32
057100140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057200140411     C                             + ' ' + 'VABPKB'
057300140411     C                   ENDIF
057400140930     C* DCR
057500140930     C                   IF        %subst(vindta:645:8) <> *blank
057600140930     C                   EVAL      PiStr=%trim(%subst(vindta:645:8))
057700140930     C                   EXSR      CHKNUM
057800140930     C                   IF        PiNum=*on
057900140930     C                   EVAL      VABDCR = PiVal
058000140930     C                   ELSE
058100140930     C                   SETON                                        32
058200140930     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058300140930     C                             + ' ' + 'VABDCR'
058400140930     C                   ENDIF
058500140930     C                   ENDIF
058600140930     C* HCR
058700140930     C                   IF        %subst(vindta:653:4) <> *blank
058800140930     C                   EVAL      PiStr=%trim(%subst(vindta:653:4))
058900140930     C                   EXSR      CHKNUM
059000140930     C                   IF        PiNum=*on
059100140930     C                   EVAL      VABHCR = PiVal
059200140930     C                   ELSE
059300140930     C                   SETON                                        32
059400140930     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059500140930     C                             + ' ' + 'VABHCR'
059600140930     C                   ENDIF
059700140930     C                   ENDIF
059800140411     C*
059900140411     C* Reperisco la provincia dal CAP e dalla località
060000140411     C                   IF        VABCAD <> *blanks AND
060100140411     C                             VABPRD  = *blanks AND
060200140411     C                             VABNZD  = *blanks
060300140411     C                   CLEAR                   TISI95DS
060400140411     C                   EVAL      I95TCN = '3'
060500140411     C                   Z-ADD     datcor        I95DAT
060600140411     C                   EVAL      I95CAP = VABCAD
060700140411     C                   EVAL      I95LOC = VABLOD
060800140411     C                   CALL      'TISI95R'
060900140411     C                   PARM                    TISI95DS
061000140411     C                   EVAL      VABPRD = O95PRV
061100140411     C                   ENDIF
061200070730     C*
061300070730     C* Considerazioni finali su CBO/CAS
061400121113     C                   IF        FlgCAS <> '0'
061500070730     C                   IF        VABCBO = '1'
061600070730     C                   EVAL      VABCBO = '4'
061700070730     C                   ENDIF
061800070730     C                   IF        VABCBO = '2'
061900070730     C                   EVAL      VABCBO = '6'
062000070730     C                   ENDIF
062100070730     C                   ENDIF
062200020305     C*
062300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
062400011113     C                   EXSR      CHKIMPDIV
062500010202     C*
062600000801     C* Ebbene...
062700000801     C                   ADD       1             §CTRMO
062800070530     C                   IF        *in31 <> *zeros OR
062900070530     C                             *in32 <> *zeros
063000000801     C                   ADD       1             §CTRNO
063100020725     C                   EVAL      x_vinflg = '2'
063200000801     C                   ELSE
063300010201     C                   ADD       1             §CTROKVB
063400000801     C                   ENDIF
063500000801     C*
063600000801     C                   ENDSR
063700140411     C*----------------------------------------------------*
063800140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
063900140930     C* nonostante questa routine non faccia pressoché nulla, devo non commentare la WRITE perché
064000140930     C* così i campi di VAT sono definiti.
064100140411     C*----------------------------------------------------*
064200140411     C     WRIVAT_A      BEGSR
064300140411     C*
064400140411     C                   eval      vatAAS = vabAAS
064500140411     C                   eval      vatLNP = vabLNP
064600140411     C                   eval      vatNRS = vabNRS
064700140411     C                   eval      vatNSP = vabNSP
064800140411     C*
064900140411     C* Valorizzo il buffer di scrittura del FIVAT
065000140411     C* - TIPO RECORD "A"
065100140930     C                   if        1 <> 1
065200140919     C***                if        %subst(w100:71:30) <> *blanks
065300140919     C***                eval      VATTRC = 'A'
065400140919     C***                eval      VATNOT = %trim(%subst(w100:71:30))
065500140930     C                   write     FIVAT000
065600140930     C                   endif
065700140411     C*
065800140411     C                   ENDSR
065900140411     C*----------------------------------------------------*
066000140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066100140411     C*----------------------------------------------------*
066200140411     C     WRIVAT_B      BEGSR
066300140930     C*
066400140930     C***                eval      vatAAS = vabAAS
066500140930     C***                eval      vatLNP = vabLNP
066600140930     C***                eval      vatNRS = vabNRS
066700140930     C***                eval      vatNSP = vabNSP
066800140930     C***
066900140930     C**Valorizzo il buffer di scrittura del FIVAT
067000140930     C**- TIPO RECORD "B"
067100140930     C***                IF        %trim(%subst(vindta:407:20)) <> *blank
067200140930     C***                eval      VATTRC = 'B'
067300140930     C***                eval      VATNOT = %trim(%subst(vindta:407:20))
067400140930     C***                write     FIVAT000
067500140930     C***                endif
067600140411     C*
067700140411     C                   ENDSR
067800140411     C*----------------------------------------------------*
067900140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
068000140411     C*----------------------------------------------------*
068100140411     C     WRIVAT_I      BEGSR
068200140411     C*
068300140411     C                   eval      vatAAS = vabAAS
068400140411     C                   eval      vatLNP = vabLNP
068500140411     C                   eval      vatNRS = vabNRS
068600140411     C                   eval      vatNSP = vabNSP
068700140411     C*
068800140411     C* Valorizzo il buffer di scrittura del FIVAT
068900140411     C* - TIPO RECORD "I"
069000140723     C***                EVAL      w70=%trim(%subst(vindta:434:70))
069100140723     C***                if        %subst(w70:1:35) <> *blanks
069200140723     C***                eval      VATTRC = 'I'
069300140723     C***                eval      VATNOT = %trim(%subst(w70:1:35))
069400140723     C***                write     FIVAT000
069500140723     C***                endif
069600140723     C***                if        %subst(w70:36:35) <> *blanks
069700140723     C***                eval      VATTRC = 'J'
069800140723     C***                eval      VATNOT = %trim(%subst(w70:36:35))
069900140723     C***                write     FIVAT000
070000140723     C***                endif
070100140411     C*
070200140411     C                   ENDSR
070300140411     C*----------------------------------------------------*
070400140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
070500140411     C*----------------------------------------------------*
070600140411     C     WRIVAT_S      BEGSR
070700140411     C*
070800140411     C                   eval      vatAAS = vabAAS
070900140411     C                   eval      vatLNP = vabLNP
071000140411     C                   eval      vatNRS = vabNRS
071100140411     C                   eval      vatNSP = vabNSP
071200140411     C*
071300140411     C* Valorizzo il buffer di scrittura del FIVAT
071400140411     C* - TIPO RECORD "S"
071500140723     C***                IF        %trim(%subst(vindta:504:16)) <> *blank
071600140723     C***                eval      VATTRC = 'S'
071700140723     C***                eval      VATNOT = %trim(%subst(vindta:504:16))
071800140723     C***                EVAL      %subst(VATNOT:17:2) = 'SS'
071900140723     C***                write     FIVAT000
072000140723     C***                endif
072100140411     C*
072200140411     C                   ENDSR
072300140411     C*----------------------------------------------------*
072400140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
072500140411     C*----------------------------------------------------*
072600140411     C     WRIVAT_E      BEGSR
072700140411     C*
072800140411     C                   eval      vatAAS = vabAAS
072900140411     C                   eval      vatLNP = vabLNP
073000140411     C                   eval      vatNRS = vabNRS
073100140411     C                   eval      vatNSP = vabNSP
073200140411     C*
073300140411     C* Valorizzo il buffer di scrittura del FIVAT
073400140411     C* - TIPO RECORD "E"
073500140919     C***                if        %trim(%subst(vindta:427:9)) <> *blanks
073600140919     C***                eval      VATTRC = 'E'
073700140919     C***                EVAL      VATNOT = %trim(%editc(
073800140919     C***                                   %dec(%subst(vindta:427:9) : 9 : 0)
073900140919     C***                                   : '4'))
074000140919     C***                write     FIVAT000
074100140919     C***                endif
074200140411     C*
074300140411     C                   ENDSR
074400020904
074500020904
074600010202     C*----------------------------------------------------*
074700131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
074800010202     C*----------------------------------------------------*
074900020305     C     PREVAT        BEGSR
075000010202     C*
075100131016     C* Compongo il nome del membro da dare al FIVATWWR
075200010202     C                   eval      parmbr = vlrhdl
075300010202     C                   movel     'M'           parmbr
075400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
075500010202     C                   eval      paropz = '1'
075600010202     C* Effettuo la chiamata al CLLE preposto
075700131017     C                   call(e)   'TITVVTC'
075800010202     C                   parm                    parccm
075900010202     C                   parm                    parmbr
076000010202     C                   parm                    paropz
076100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
076200010202     C                   if        %error
076300010202     C                   movel     '1'           chkcall
076400010202     C                   else
076500010202     C                   movel     '0'           chkcall
076600010202     C                   endif
076700010202     C*
076800010202     C                   ENDSR
076900000801     C*----------------------------------------------------*
077000000801     C*  CONTROLLO NUMERICITA' CAMPI
077100000801     C*----------------------------------------------------*
077200000801     C     CHKNUM        BEGSR
077300081113     C*
077400081113     C                   IF        PiDecChr = *blanks
077500140411     C                   EVAL      PiDecChr = ','
077600081113     C                   ENDIF
077700081113     C*
077800081113     C                   callp(e)  UBISNUM_Check(PiStr
077900081113     C                                          :PiDecChr
078000081113     C                                          :PiVal
078100081113     C                                          :PiNum
078200081113     C                                          :PiInt)
078300081113     C*
078400000801     C                   IF        %error
078500000801     C                   EVAL      PiInt=*off
078600000801     C                   ENDIF
078700000801     C*
078800000801     C                   ENDSR
078900000801     C***
079000000801
079100011113
079200011113     C*----------------------------------------------------*
079300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
079400011113     C*----------------------------------------------------*
079500011113     C     CHKIMPDIV     BEGSR
079600011113     C*
079700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
079800011113     C                   Z-ADD     *zeros        wrkDec            9 9
079900011113     C*
080000011113     C* Come prima cosa effettuo considerazioni sulla divisa
080100011113     C                   IF        vabIAS > *zeros
080200011113     C                   IF        vabVAS <> 'EUR'
080300011113     C                   EVAL      vabVAS =  'ITL'
080400011113     C                   ENDIF
080500011113     C                   ENDIF
080600011113     C*
080700011113     C                   IF        vabCAS > *zeros
080800011113     C                   IF        vabVCA <> 'EUR'
080900011113     C                   EVAL      vabVCA =  'ITL'
081000011113     C                   ENDIF
081100011113     C                   ENDIF
081200011113     C*
081300011113     C                   IF        vabVMD > *zeros
081400020305     C                   IF        vabVAD <> 'EUR'
081500011113     C                   EVAL      vabVAD =  'ITL'
081600011113     C                   ENDIF
081700011113     C                   ENDIF
081800011113     C*
081900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
082000011113     C                   Z-ADD     vabIAS        wrkDec
082100011113     C                   IF        wrkDec > *zeros
082200011113     C                   IF        vabVAS = 'ITL'
082300011113     C                   EVAL      vabIAS = *zeros
082400011113     C                   ENDIF
082500011113     C                   ENDIF
082600011113     C*
082700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
082800011113     C                   Z-ADD     vabCAS        wrkDec
082900011113     C                   IF        wrkDec > *zeros
083000011113     C                   IF        vabVCA = 'ITL'
083100011113     C                   EVAL      vabCAS = *zeros
083200011113     C                   ENDIF
083300011113     C                   ENDIF
083400011113     C*
083500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
083600011113     C                   Z-ADD     vabVMD        wrkDec
083700011113     C                   IF        wrkDec > *zeros
083800011113     C                   IF        vabVAD = 'ITL'
083900011113     C                   EVAL      vabVMD = *zeros
084000011113     C                   ENDIF
084100011113     C                   ENDIF
084200011113     C*
084300011113     C                   ENDSR
084400011113     C***
084500011113
084600011113
084700000801
084800000801
084900990920      /TITLE Invio dei dati al punto operativo.
085000010202     C     invio         BEGSR
085100990920     C*
085200131016     C* 1° invio FIVAT
085300010201     C                   reset                   dscmz
085400111031     C                   move      wPOI          cmzdst
085500131016     C                   eval      cmzfld = 'FIVATWWR'
085600010201     C                   eval      cmzmbd = vlrhdl
085700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085800021009     C***                if        prmfir = *blanks
085900131016     C                   eval      cmzfla = 'FIVAT00F'
086000131016     C                   eval      cmzmba = 'FIVAT00F'
086100021009     C***                else
086200021009     C***                eval      cmzfla = prmfir
086300021009     C***                eval      cmzmba = prmfir
086400021009     C***                endif
086500010201     C                   eval      cmznrr = *zeros
086600020305     C                   move      §ctrokvt      cmznrr
086700021018     C                   eval      cmzlba = vlrfl1
086800010201     C                   call(e)   'TIS711C'
086900010201     C                   parm                    dscmz
087000010201     C                   parm      *blanks       esito
087100010205     C                   if        %error
087200010205     C                             or cmzerr = '1'
087300010205     C                             or esito  = '1'
087400010205     C                   eval      wrkesito = '3'
087500010205     C                   else
087600010201     C*
087700131016     C* 2° invio FIVAB
087800010201     C                   reset                   dscmz
087900111031     C                   move      wPOI          cmzdst
088000010201     C                   eval      cmzfld = vlrfou
088100010201     C                   eval      cmzmbd = vlrhdl
088200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
088300021009     C***                if        prmfir = *blanks
088400131016     C                   eval      cmzfla = 'FIVAB00F'
088500131016     C                   eval      cmzmba = 'FIVAB00F'
088600021009     C***                else
088700021009     C***                eval      cmzfla = prmfir
088800021009     C***                eval      cmzmba = prmfir
088900021009     C***                endif
089000010201     C                   eval      cmznrr = *zeros
089100010201     C                   move      §ctrokvb      cmznrr
089200021018     C                   eval      cmzlba = vlrfl1
089300010201     C                   call(e)   'TIS711C'
089400010201     C                   parm                    dscmz
089500010201     C                   parm      *blanks       esito
089600010201     C                   if        %error
089700010201     C                             or cmzerr = '1'
089800010201     C                             or esito  = '1'
089900010201     C                   eval      wrkesito = '3'
090000010201     C                   endif
090100010205     C                   endif
090200990920     C*
090300000613     C                   ENDSR
090400000613     C***
090500070411
090600070411     C     *pssr         BEGSR
090700070411     C*
090800070411     C                   if        %open(tivin00r)
090900070411     C                   close     tivin00r
091000070411     C                   endif
091100131016     C                   if        %open(FIVABWWr)
091200131016     C                   close     FIVABWWr
091300120706     C                   endif
091400131016     C                   if        %open(FIVATWWr)
091500131016     C                   close     FIVATWWr
091600070411     C                   endif
091700070411     C*
091800070411     C* Effettuo la chiamata al CLLE preposto
091900131017     C                   call(e)   'TITVVTC'
092000070411     C                   parm                    parccm
092100070411     C                   parm                    parmbr
092200070411     C                   parm      '2'           paropz
092300070411     C*
092400070411     C                   eval      wrkesito = '2'
092500070411     C*
092600070411     C                   seton                                        LR
092700070411     C*
092800070411     C                   ENDSR     '*CANCL'
092900070411     C***
093000070411
093100990910
093200000613     C     *inzsr        BEGSR
093300990910     C*
093400990910     C     *entry        plist
093500990920     C                   parm                    tivlrds
093600990921     C                   parm      wrkesito      esito
093700000724     C                   parm                    prmlit
093800000710     C                   parm                    prmfir
093900000613     C*
094000000830     C* CALCOLA LA DATA CORRENTE
094100120706     C                   time                    wn14             14 0
094200120706     C                   movel     wn14          oracor            6 0          *ORA
094300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
094400100324     C                   eval      datcor = %dec(%date() : *ISO)
094500000830     C*
094600000613     C                   ENDSR
094700000613     C***
