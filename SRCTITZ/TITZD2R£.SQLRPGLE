000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140919      * - disk A
000500140919      * - aggiunto la routine RepNSP per gestione disk A
000600140723      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
001100131016     FFIVATWWr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100131016     D fivabds       e ds                  extname(FIVAB00f)
002200131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
002300131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
002400990910     D esito           s              1
002500000724     D prmlit          s             10
002600000710     D prmfir          s             10
002700990921     D wrkesito        s                   like(esito)
002800000613     D rrnum           s              6  0 INZ(*zeros)
002900140919     D depspe          s              6    INZ(*blanks)
003000140919     D curspe          s              6    INZ(*blanks)
003100010202     D parccm          s              8    INZ(*blanks)
003200010202     D parmbr          s             10    INZ(*blanks)
003300010202     D paropz          s              1    INZ(*blanks)
003400010202     D chkcall         s              1    INZ(*blanks)
003500080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003600070530     D Num5_0          s              5  0
003700131016     D w70             s             70
003800140723     D w100            s            100
003900000830
004000020725
004100020725     D*------------------
004200020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004300020725     D*------------------
004400070530     D INPUT_DS        DS                  INZ
004500101007     D  VINDTA                     2048
004600101007     D  VINFLG                        1
004700140919     D  VINSPE                        6
004800101007     D  VINRRN                        8  0
004900020725     D*
005000080923     D*------------------
005100080923     D* DS REPERIMENTO NUMERATORE
005200080923     D*------------------
005300080923     D trul33ds      e ds                  inz
005400080923     D*------------------
005500080923     D* DS ARCHITETTURA
005600080923     D*------------------
005700080923     D kpjba         e ds                  inz
005800080923     D*------------------
005900121113     D  Num7_0         s              7  0
006000081113
006100081113     D*------------------
006200081113     D* LINKING A DEFINIZIONI ESTERNE
006300081113     D*------------------
006400100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006500081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006600081113
006700990908
006800010201
006900010201
007000080117     C*
007100080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007200080117     C
007300080117     C/EXEC SQL
007400080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007500080117     C/END-EXEC
007600080117     C
007700000913     C                   reset                   rrnum
007800990921     C                   reset                   esito
007900990921     C                   reset                   wrkesito
008000000613     C*
008100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400131017     C                   call(e)   'TITVVTC'
008500010202     C                   parm                    parccm
008600010202     C                   parm                    parmbr
008700010202     C                   parm      '2'           paropz
008800070730     C*
008900070730     C* Effettuo lancio TISI95 solo x chiusura
009000070730     C                   CLEAR                   TISI95DS
009100070730     C                   EVAL      I95TLA = 'C'
009200070730     C                   CALL      'TISI95R'
009300070730     C                   PARM                    TISI95DS
009400000616     C*
009500000801     C
009600010201     C                   seton                                        LR
009700990908
009800000801
009900910830     C*--------------------------------------------------------
010000131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010100910830     C*--------------------------------------------------------
010200070530     C     RWFILE        BEGSR
010300990910     C*
010400990914     C                   if        not %open(tivin00r)
010500990908     C                   open      tivin00r
010600990914     C                   endif
010700131016     C                   if        not %open(FIVABWWr)
010800131016     C                   open      FIVABWWr
010900990914     C                   endif
011000070530     C*
011100131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011200020305     C                   exsr      prevat
011300010201     C*
011400010202     C                   if        chkcall = '0'
011500010202     C*
011600131016     C                   if        not %open(FIVATWWr)
011700131016     C                   open      FIVATWWr
011800010201     C                   endif
011900080117     C*
012000080117     C                   EXSR      INZVAR
012100080117     C                   EXSR      DEFCAM
012200990910     C*
012300010201     C                   clear                   §CTROKVB          5 0
012400020305     C                   clear                   §CTROKVT          5 0
012500000801     C                   clear                   §CTRMO            5 0
012600000801     C                   clear                   §CTRNO            5 0
012700990910     C*
012800020725     C/EXEC SQL
012900020725     C+ declare C1 cursor for select
013000140919     C+ vindta, vinflg, substr(vindta, 35, 6) as sped, rrn(tivin00r)
013100060223     C+ from tivin00r
013200060223     C+ order by sped
013300020725     C+ for read only
013400020725     C/END-EXEC
013500020725     C
013600020725     C/EXEC SQL
013700020725     C+ open C1
013800020725     C/END-EXEC
013900020725     C
014000020725     C/EXEC SQL
014100070530     C+ Fetch C1 into :INPUT_DS
014200020725     C/END-EXEC
014300020725     C*
014400020725     C                   dow       sqlcod = *zeros
014500990913     C*
014600020725     C                   if        vindta > *blanks
014700000613     C                   add       1             rrnum
014800000801     C*
014900020725     C                   if        vinflg = *blanks
015000020725     C                             or vinflg = '0'
015100020725     C                             or vinflg = '2'
015200000801     C*
015300020725     C                   clear                   x_vinmsg
015400020725     C                   eval      x_vinflg = '1'
015500010305     C*
015600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015700140723     C* (uso RMN)
015800140919     C                   EVAL      PiStr=%trim(%subst(vindta:35:6))
015900020305     C                   MOVEL(p)  PiStr         curspe
016000010305     C*
016100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016200080923     C                   if        *in50 = *off
016300140919     C                   exsr      repNSP
016400071003     C                   exsr      impvab
016500111130     C  N31              eval      fivabds_ok = fivabds
016600111130     C  N31              exsr      wrivab
016700140411     C                   exsr      wrivat_a                                     => carico VAT
016800140328     C                   exsr      wrivat_b                                     => carico VAT
016900140411     C                   exsr      wrivat_i                                     => carico VAT
017000140411     C                   exsr      wrivat_s                                     => carico VAT
017100111125     C                   exsr      inzvar
017200071003     C                   else
017300080923     C*
017400071009     C                   if        wDISK = 'D'
017500140723     C                   exsr      repNSP
017600071009     C                   exsr      impvab
017700111130     C  N31              eval      fivabds_ok = fivabds
017800111130     C  N31              exsr      wrivab
017900140411     C                   exsr      wrivat_a                                     => carico VAT
018000140328     C                   exsr      wrivat_b                                     => carico VAT
018100140411     C                   exsr      wrivat_i                                     => carico VAT
018200140411     C                   exsr      wrivat_s                                     => carico VAT
018300140411     C                   exsr      wrivat_e                                     => carico VAT
018400111125     C                   exsr      inzvar
018500071009     C                   else
018600071009     C*
018700010305     C                   if        depspe = *blanks                             => 1° giro
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900080117     C                   clear                   tivinds
019000140723     C                   exsr      repNSP
019100020305     C                   exsr      impvab
019200111130     C                   z-add     VABNCL        sav_VABNCL
019300111130     C                   z-add     VABPKB        sav_VABPKB
019400111130     C                   z-add     VABVLB        sav_VABVLB
019500140411     C                   exsr      wrivat_a                                     => carico VAT
019600140328     C                   exsr      wrivat_b                                     => carico VAT
019700140411     C                   exsr      wrivat_i                                     => carico VAT
019800140411     C                   exsr      wrivat_s                                     => carico VAT
019900140411     C   50              exsr      wrivat_e                                     => carico VAT
020000010305     C                   else
020100020725     C                   if        depspe <> curspe                             => rottura di spediz
020200111130     C                   z-add     sav_VABNCL    VABNCL
020300111130     C                   z-add     sav_VABPKB    VABPKB
020400111130     C                   z-add     sav_VABVLB    VABVLB
020500111130     C  N31              eval      fivabds_ok = fivabds
020600111125     C  N31              exsr      wrivab
020700111125     C                   exsr      inzvar
020800111125     C                   eval      depspe = curspe                              => memorizz. spediz
020900080117     C                   clear                   tivinds
021000140723     C                   exsr      repNSP
021100020305     C                   exsr      impvab
021200111130     C                   if        wCntRecSpe = *zeros
021300111130     C                   z-add     VABNCL        sav_VABNCL
021400111130     C                   z-add     VABPKB        sav_VABPKB
021500111130     C                   z-add     VABVLB        sav_VABVLB
021600111130     C                   endif
021700111130     C                   add       1             wCntRecSpe
021800140411     C                   exsr      wrivat_a                                     => carico VAT
021900140328     C                   exsr      wrivat_b                                     => carico VAT
022000140411     C                   exsr      wrivat_i                                     => carico VAT
022100140411     C                   exsr      wrivat_s                                     => carico VAT
022200140411     C   50              exsr      wrivat_e                                     => carico VAT
022300020305     C                   else                                                   => x stessa spediz
022400111130     C                   exsr      impvab
022500120706     C   51              add       VABNCL        sav_VABNCL
022600111125     C   51              add       VABPKB        sav_VABPKB
022700111125     C   51              add       VABVLB        sav_VABVLB
022800111125     C   52              z-add     VABNCL        sav_VABNCL
022900111125     C   52              z-add     VABPKB        sav_VABPKB
023000111125     C   52              z-add     VABVLB        sav_VABVLB
023100111125     C   53              add       VABNCL        sav_VABNCL
023200111125     C   53              z-add     VABPKB        sav_VABPKB
023300111125     C   53              z-add     VABVLB        sav_VABVLB
023400131016     C   54              Z-ADD     VABNCL        sav_VABNCL
023500131016     C   54              ADD       VABPKB        sav_VABPKB
023600131016     C   54              ADD       VABVLB        sav_VABVLB
023700111125     C*
023800140411     C                   exsr      wrivat_a                                     => carico VAT
023900140328     C                   exsr      wrivat_b                                     => carico VAT
024000140411     C                   exsr      wrivat_i                                     => carico VAT
024100140411     C                   exsr      wrivat_s                                     => carico VAT
024200140411     C   50              exsr      wrivat_e                                     => carico VAT
024300010305     C                   endif
024400010305     C                   endif
024500010305     C                   endif
024600071003     C                   endif
024700071009     C                   endif
024800000905     C*
024900000905     C                   else
025000020725     C                   eval      x_vinflg = '1'
025100000905     C                   endif
025200000905     C*
025300020725     C     VINRRN        chain     tivin000
025400020725     C                   if        %found(tivin00r)
025500020725     C                   eval      y_vinflg = x_vinflg
025600020725     C                   eval      y_vinmsg = x_vinmsg
025700020725     C                   update    tivin000
025800020725     C                   endif
025900020725     C*
026000020725     C/EXEC SQL
026100070530     C+ Fetch C1 into :INPUT_DS
026200020725     C/END-EXEC
026300020725     C*
026400020725     C                   enddo
026500020725     C*
026600020725     C/EXEC SQL
026700020725     C+ close C1
026800020725     C/END-EXEC
026900000905     C*
027000020305     C* Scarico i VAB rimasti "in sospeso"
027100071009     C                   if        wDISK = 'C'
027200140307     C                   z-add     sav_VABNCL    VABNCL
027300140307     C                   z-add     sav_VABPKB    VABPKB
027400140307     C                   z-add     sav_VABVLB    VABVLB
027500111130     C  N31              eval      fivabds_ok = fivabds
027600111130     C  N31              exsr      wrivab
027700071009     C                   endif
027800010202     C*
027900010202     C                   endif
028000990910
028100990910     C* Se non ci sono record con errori ...
028200000710     C                   if        §ctrno = 0
028300990910     C* ... restituisco esito OK.
028400990921     C                   eval      wrkesito = '0'
028500990910     C                   else
028600010201     C                   if        §ctrokvb > 0
028700990921     C                   eval      wrkesito = '1'
028800000710     C                   else
028900000710     C                   eval      wrkesito = '2'
029000990910     C                   endif
029100000710     C                   endif
029200990910     C*
029300990914     C                   if        %open(tivin00r)
029400990908     C                   close     tivin00r
029500990914     C                   endif
029600131016     C                   if        %open(FIVABWWr)
029700131016     C                   close     FIVABWWr
029800120706     C                   endif
029900131016     C                   if        %open(FIVATWWr)
030000131016     C                   close     FIVATWWr
030100010201     C                   endif
030200990910     C*
030300111031     C                   if        §ctrokvb > 0 or
030400111031     C                             §ctrokvt > 0
030500111031     C                             and wPOI <> *zeros
030600010202     C                   exsr      invio
030700990920     C                   endif
030800990920     C*
030900910830     C                   ENDSR
031000000613     C***
031100010305
031200010305     C*----------------------------------------------------*
031300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031400010305     C*----------------------------------------------------*
031500020305     C     WRIVAB        BEGSR
031600070730     C*
031700071003     C* Considerazioni finali
031800111125     C                   if        ok_VABRMA = *blanks
031900111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
032000071003     C                   endif
032100071003     C*
032200131016     C                   write     FIVAB000                                     => scarico il VAB
032300010305     C*
032400010305     C                   ENDSR
032500990920
032600000801     C*----------------------------------------------------*
032700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032800000801     C*----------------------------------------------------*
032900010201     C     INZVAR        BEGSR
033000000801     C*
033100010201     C                   Z-ADD     *zeros        Num5_0
033200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033300020725     C                   MOVEL     '0'           FlgCAS            1
033400111125     C*
033500111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033600111130     C*
033700111125     C                   CLEAR                   FIVABDS
033800111130     C                   CLEAR                   FIVABDS_OK
033900111130     C                   CLEAR                   FIVABDS_SAV
034000131016     C                   CLEAR                   FIVAB000
034100131016     C                   CLEAR                   FIVAT000
034200000801     C*
034300000801     C                   ENDSR
034400000801     C*----------------------------------------------------*
034500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034600000801     C*----------------------------------------------------*
034700020904     C     DEFCAM        BEGSR
034800080923     C*
034900111125     C                   SETOFF                                       50
035000111125     C                   SETOFF                                       515253
035100131016     C                   SETOFF                                       54
035200070730     C* Imposto i valori di default...
035300140919     C                   EVAL      VABCCM = 0935828
035400140919     C                   EVAL      VATCCM = 0935828
035500140919     C                   EVAL      VABLNP = 094
035600140919     C                   EVAL      VATLNP = 094
035700140919     C                   EVAL      VABCTR = 000
035800131016     C                   EVAL      VABCBO = '1'
035900140919     C                   EVAL      VABTSP = 'C'
036000140919     C                   EVAL      VABCTM = '7'
036100070222     C* ... e poi verifico se sono stati passati come parametri
036200070222     C                   IF        vlrppt > *blanks
036300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
036400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036500070222     C                   EXSR      CHKNUM
036600070222     C                   IF        PiInt=*on
036700070222     C                   Z-ADD     PiVal         VABCCM
036800070222     C                   Z-ADD     PiVal         VATCCM
036900070222     C                   ENDIF
037000070222     C                   ENDIF
037100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037300070222     C                   EXSR      CHKNUM
037400070222     C                   IF        PiInt=*on
037500070222     C                   Z-ADD     PiVal         VABLNP
037600070222     C                   Z-ADD     PiVal         VATLNP
037700070222     C                   ENDIF
037800070222     C                   ENDIF
037900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038100070222     C                   EXSR      CHKNUM
038200070222     C                   IF        PiInt=*on
038300070222     C                   Z-ADD     PiVal         VABCTR
038400070222     C                   ENDIF
038500070928     C                   ENDIF
038600071009     C                   MOVEL     *blanks       wDISK             1
038700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038800071009     C                   IF        wDISK <> *blanks
038900070928     C                   SETON                                        50
039000070222     C                   ENDIF
039100080923     C                   IF        %subst(vlrppt:15:1) = 'S'
039200111125     C                   SETON                                        41
039300080923     C                   ENDIF
039400070222     C                   ENDIF
039500120706     C*
039600120706     C* valido per nr.colli/peso/volume
039700120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039800120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039900120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
040000131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
040100120706     C                   SELECT
040200120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
040300120706     C                   SETON                                        51
040400120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040500120706     C                   SETON                                        52
040600120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040700120706     C                   SETON                                        53
040800131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040900131016     C                   SETON                                        54
041000120706     C                   ENDSL
041100120706     C*
041200071009     C   50              EVAL      VABCTM = '7Q'
041300000801     C*
041400000801     C                   ENDSR
041500131016     C*----------------------------------------------------*
041600131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041700131016     C*----------------------------------------------------*
041800131016     C     REPNSP        BEGSR
041900131016     C*
042000131016     C                   EXSR      INZVAR
042100131016     C                   EXSR      DEFCAM
042200131016     C*
042300131016     C                   SETON                                        60
042400131016     C*
042500131016     C* NSP => Stacco un numeratore da AZNUM
042600131016     C                   clear                   TRUL33DS
042700131016     C                   eval      I33OPE = *zeros
042800131016     C                   eval      I33CNU = 302
042900131016     C                   eval      I33NUM = 1
043000131016     C                   movel     TRUL33DS      KPJBU
043100131016     C                   call      'TRUL33R'
043200131016     C                   parm                    KPJBA
043300131016     C                   movel     KPJBU         TRUL33DS
043400131016     C                   if        O33ERR = *zeros
043500131016     C                   z-add     O33NRF        VABNSP
043600131016     C                   z-add     O33NRF        VATNSP
043700140905     C***                z-add     O33NRF        VABRMN
043800131016     C                   else
043900131016     C                   SETON                                        31
044000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044100140905     C                             + ' ' + 'VABNSP VATNSP'
044200131016     C                   endif
044300131016     C*
044400131016     C                   ENDSR
044500000801     C*----------------------------------------------------*
044600131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044700000801     C*----------------------------------------------------*
044800010201     C     IMPVAB        BEGSR
044900070530     C*
045000070530     C                   SETOFF                                       3132
045100070928     C*
045200070928     C                   EXSR      DEFCAM
045300070928     C*
045400111031     C                   IF        vlrpoi <> 999
045500070928     C                   MOVE(P)   vlrpoi        VABFGS
045600070928     C                   MOVE(P)   vlrpoi        VATFGS
045700111031     C                   Z-ADD     vlrpoi        wPOI              3 0
045800111031     C                   ENDIF
045900070928     C*
046000070928     C                   MOVEL     datcor        VABAAS
046100070928     C                   MOVEL     datcor        VATAAS
046200070928     C                   MOVE      datcor        VABMGS
046300140919     C                   EVAL      VABRSD=%trim(%subst(vindta:50:35))
046400131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046500140306     C     '@':'A'       XLATE     VABRSD        VABRSD
046600131016     C* ==
046700140919     C* imposto cliente e tariffa
046800140919     C                   SELECT
046900140919     C                   WHEN      %trim(%subst(vindta:23:2))='17'
047000140919     C                   EVAL      VABCCM=0935828
047100140919     C                   EVAL      VATCCM=0935828
047200140919     C                   EVAL      VABCTR=100
047300140919     C                   WHEN      %trim(%subst(vindta:23:2))='16'
047400140919     C                   EVAL      VABCCM=0935829
047500140919     C                   EVAL      VATCCM=0935829
047600140919     C                   EVAL      VABCTR=100
047700140919     C                   WHEN      %trim(%subst(vindta:23:2))='74'
047800140919     C                   EVAL      VABCCM=0935829
047900140919     C                   EVAL      VATCCM=0935829
048000140919     C                   EVAL      VABCTR=001
048100140919     C                   ENDSL
048200140919
048300140919     C                   EVAL      VABIND=%trim(%subst(vindta:85:35))
048400140919     C                   EVAL      VABLOD=%trim(%subst(vindta:120:20))
048500140919     C                   EVAL      VABPRD=%trim(%subst(vindta:154:2))
048600140919     C                   EVAL      VABRMA=%trim(%subst(vindta:35:15))
048700140919     C* imposto tipo incasso
048800140919     C                   SELECT
048900140919     C                   WHEN      %trim(%subst(vindta:210:1))='A'
049000140919     C                   EVAL      VABTIC='BM'
049100140919     C                   WHEN      %trim(%subst(vindta:210:1))='M'
049200140919     C                   EVAL      VABTIC='BM'
049300140919     C                   WHEN      %trim(%subst(vindta:210:1))='C'
049400140919     C                   EVAL      VABTIC=*blank
049500140919     C                   WHEN      %trim(%subst(vindta:210:1))='I'
049600140919     C                   EVAL      VABTIC='CM'
049700140919     C                   WHEN      %trim(%subst(vindta:210:1))='R'
049800140919     C                   EVAL      VABTIC='CM'
049900140919     C                   WHEN      %trim(%subst(vindta:210:1))='E'
050000140919     C                   EVAL      VABTIC='CA'
050100140919     C                   ENDSL
050200140919     C* imposto porto
050300140919     C                   SELECT
050400140919     C                   WHEN      %trim(%subst(vindta:569:1))='F'
050500140919     C                   EVAL      VABCBO='1'
050600140919     C                   WHEN      %trim(%subst(vindta:569:1))='A'
050700140919     C                   EVAL      VABCBO='2'
050800140919     C                   ENDSL
050900140919     C                   EVAL      w100=%trim(%subst(vindta:306:50))
051000140919     C                   EVAL      VABNOT = %trim(%subst(w100:1:35))
051100140919     C                   EVAL      VABNT2 = %trim(%subst(w100:36:35))
051200140919
051300131016     C*
051400131016     C* CAD
051500140919     C                   EVAL      PiStr=%trim(%subst(vindta:145:9))
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
052600140919     C                   IF        %subst(vindta:196:14)<> *blanks    AND
052700140919     C                             %subst(vindta:196:14)<> *zeros
052800140328     C                   EVAL      FlgCAS = '1'
052900140919     C                   EVAL      VABVCA ='EUR'
053000140919     C                   EVAL      PiStr=%trim(%subst(vindta:196:14))
053100140328     C                   EXSR      CHKNUM
053200140328     C                   IF        PiNum=*on
053300140723     C                   EVAL      VABCAS = PiVal/100
053400140328     C                   ELSE
053500140328     C  N51              SETON                                        32
053600140328     C  N51              Z-ADD     *zeros        VABCAS
053700140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
053800140328     C                             + ' ' + 'VABCAS'
053900140328     C                   ENDIF
054000140328     C                   ELSE
054100140328     C                   EVAL      FlgCAS = '0'
054200140328     C                   ENDIF
054300140411     C* RMN
054400140919     C                   EVAL      PiStr=%trim(%subst(vindta:35:6))
054500140411     C                   EXSR      CHKNUM
054600140411     C                   IF        PiInt=*on
054700140411     C                   Z-ADD     PiVal         VABRMN
054800140411     C                   ELSE
054900140411     C                   SETON                                        32
055000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055100140411     C                             + ' ' + 'VABRMN'
055200140411     C                   ENDIF
055300140411     C* NCL
055400140919     C                   EVAL      PiStr=%trim(%subst(vindta:164:5))
055500140411     C                   EXSR      CHKNUM
055600140411     C                   IF        PiInt=*on
055700140411     C                   Z-ADD     PiVal         VABNCL
055800140411     C                   ELSE
055900140411     C                   SETON                                        32
056000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056100140411     C                             + ' ' + 'VABNCL'
056200140411     C                   ENDIF
056300140411     C* PKB
056400140919     C                   EVAL      PiStr=%trim(%subst(vindta:159:5))
056500140411     C                   EXSR      CHKNUM
056600140411     C                   IF        PiNum=*on
056700140919     C                   EVAL      VABPKB = PiVal
056800140411     C                   ELSE
056900140411     C                   SETON                                        32
057000140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057100140411     C                             + ' ' + 'VABPKB'
057200140411     C                   ENDIF
057300140411     C*
057400140411     C* Reperisco la provincia dal CAP e dalla località
057500140411     C                   IF        VABCAD <> *blanks AND
057600140411     C                             VABPRD  = *blanks AND
057700140411     C                             VABNZD  = *blanks
057800140411     C                   CLEAR                   TISI95DS
057900140411     C                   EVAL      I95TCN = '3'
058000140411     C                   Z-ADD     datcor        I95DAT
058100140411     C                   EVAL      I95CAP = VABCAD
058200140411     C                   EVAL      I95LOC = VABLOD
058300140411     C                   CALL      'TISI95R'
058400140411     C                   PARM                    TISI95DS
058500140411     C                   EVAL      VABPRD = O95PRV
058600140411     C                   ENDIF
058700070730     C*
058800070730     C* Considerazioni finali su CBO/CAS
058900121113     C                   IF        FlgCAS <> '0'
059000070730     C                   IF        VABCBO = '1'
059100070730     C                   EVAL      VABCBO = '4'
059200070730     C                   ENDIF
059300070730     C                   IF        VABCBO = '2'
059400070730     C                   EVAL      VABCBO = '6'
059500070730     C                   ENDIF
059600070730     C                   ENDIF
059700020305     C*
059800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
059900011113     C                   EXSR      CHKIMPDIV
060000010202     C*
060100000801     C* Ebbene...
060200000801     C                   ADD       1             §CTRMO
060300070530     C                   IF        *in31 <> *zeros OR
060400070530     C                             *in32 <> *zeros
060500000801     C                   ADD       1             §CTRNO
060600020725     C                   EVAL      x_vinflg = '2'
060700000801     C                   ELSE
060800010201     C                   ADD       1             §CTROKVB
060900000801     C                   ENDIF
061000000801     C*
061100000801     C                   ENDSR
061200140411     C*----------------------------------------------------*
061300140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
061400140411     C*----------------------------------------------------*
061500140411     C     WRIVAT_A      BEGSR
061600140411     C*
061700140411     C                   eval      vatAAS = vabAAS
061800140411     C                   eval      vatLNP = vabLNP
061900140411     C                   eval      vatNRS = vabNRS
062000140411     C                   eval      vatNSP = vabNSP
062100140411     C*
062200140411     C* Valorizzo il buffer di scrittura del FIVAT
062300140411     C* - TIPO RECORD "A"
062400140919     C***                if        %subst(w100:71:30) <> *blanks
062500140919     C***                eval      VATTRC = 'A'
062600140919     C***                eval      VATNOT = %trim(%subst(w100:71:30))
062700140919     C***                write     FIVAT000
062800140919     C***                endif
062900140411     C*
063000140411     C                   ENDSR
063100140411     C*----------------------------------------------------*
063200140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
063300140411     C*----------------------------------------------------*
063400140411     C     WRIVAT_B      BEGSR
063500140411     C*
063600140411     C                   eval      vatAAS = vabAAS
063700140411     C                   eval      vatLNP = vabLNP
063800140411     C                   eval      vatNRS = vabNRS
063900140411     C                   eval      vatNSP = vabNSP
064000140411     C*
064100140411     C* Valorizzo il buffer di scrittura del FIVAT
064200140411     C* - TIPO RECORD "B"
064300140919     C                   IF        %trim(%subst(vindta:407:20)) <> *blank
064400140919     C                   eval      VATTRC = 'B'
064500140919     C                   eval      VATNOT = %trim(%subst(vindta:407:20))
064600140919     C                   write     FIVAT000
064700140919     C                   endif
064800140411     C*
064900140411     C                   ENDSR
065000140411     C*----------------------------------------------------*
065100140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
065200140411     C*----------------------------------------------------*
065300140411     C     WRIVAT_I      BEGSR
065400140411     C*
065500140411     C                   eval      vatAAS = vabAAS
065600140411     C                   eval      vatLNP = vabLNP
065700140411     C                   eval      vatNRS = vabNRS
065800140411     C                   eval      vatNSP = vabNSP
065900140411     C*
066000140411     C* Valorizzo il buffer di scrittura del FIVAT
066100140411     C* - TIPO RECORD "I"
066200140723     C***                EVAL      w70=%trim(%subst(vindta:434:70))
066300140723     C***                if        %subst(w70:1:35) <> *blanks
066400140723     C***                eval      VATTRC = 'I'
066500140723     C***                eval      VATNOT = %trim(%subst(w70:1:35))
066600140723     C***                write     FIVAT000
066700140723     C***                endif
066800140723     C***                if        %subst(w70:36:35) <> *blanks
066900140723     C***                eval      VATTRC = 'J'
067000140723     C***                eval      VATNOT = %trim(%subst(w70:36:35))
067100140723     C***                write     FIVAT000
067200140723     C***                endif
067300140411     C*
067400140411     C                   ENDSR
067500140411     C*----------------------------------------------------*
067600140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
067700140411     C*----------------------------------------------------*
067800140411     C     WRIVAT_S      BEGSR
067900140411     C*
068000140411     C                   eval      vatAAS = vabAAS
068100140411     C                   eval      vatLNP = vabLNP
068200140411     C                   eval      vatNRS = vabNRS
068300140411     C                   eval      vatNSP = vabNSP
068400140411     C*
068500140411     C* Valorizzo il buffer di scrittura del FIVAT
068600140411     C* - TIPO RECORD "S"
068700140723     C***                IF        %trim(%subst(vindta:504:16)) <> *blank
068800140723     C***                eval      VATTRC = 'S'
068900140723     C***                eval      VATNOT = %trim(%subst(vindta:504:16))
069000140723     C***                EVAL      %subst(VATNOT:17:2) = 'SS'
069100140723     C***                write     FIVAT000
069200140723     C***                endif
069300140411     C*
069400140411     C                   ENDSR
069500140411     C*----------------------------------------------------*
069600140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
069700140411     C*----------------------------------------------------*
069800140411     C     WRIVAT_E      BEGSR
069900140411     C*
070000140411     C                   eval      vatAAS = vabAAS
070100140411     C                   eval      vatLNP = vabLNP
070200140411     C                   eval      vatNRS = vabNRS
070300140411     C                   eval      vatNSP = vabNSP
070400140411     C*
070500140411     C* Valorizzo il buffer di scrittura del FIVAT
070600140411     C* - TIPO RECORD "E"
070700140919     C***                if        %trim(%subst(vindta:427:9)) <> *blanks
070800140919     C***                eval      VATTRC = 'E'
070900140919     C***                EVAL      VATNOT = %trim(%editc(
071000140919     C***                                   %dec(%subst(vindta:427:9) : 9 : 0)
071100140919     C***                                   : '4'))
071200140919     C***                write     FIVAT000
071300140919     C***                endif
071400140411     C*
071500140411     C                   ENDSR
071600020904
071700020904
071800010202     C*----------------------------------------------------*
071900131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
072000010202     C*----------------------------------------------------*
072100020305     C     PREVAT        BEGSR
072200010202     C*
072300131016     C* Compongo il nome del membro da dare al FIVATWWR
072400010202     C                   eval      parmbr = vlrhdl
072500010202     C                   movel     'M'           parmbr
072600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072700010202     C                   eval      paropz = '1'
072800010202     C* Effettuo la chiamata al CLLE preposto
072900131017     C                   call(e)   'TITVVTC'
073000010202     C                   parm                    parccm
073100010202     C                   parm                    parmbr
073200010202     C                   parm                    paropz
073300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073400010202     C                   if        %error
073500010202     C                   movel     '1'           chkcall
073600010202     C                   else
073700010202     C                   movel     '0'           chkcall
073800010202     C                   endif
073900010202     C*
074000010202     C                   ENDSR
074100000801     C*----------------------------------------------------*
074200000801     C*  CONTROLLO NUMERICITA' CAMPI
074300000801     C*----------------------------------------------------*
074400000801     C     CHKNUM        BEGSR
074500081113     C*
074600081113     C                   IF        PiDecChr = *blanks
074700140411     C                   EVAL      PiDecChr = ','
074800081113     C                   ENDIF
074900081113     C*
075000081113     C                   callp(e)  UBISNUM_Check(PiStr
075100081113     C                                          :PiDecChr
075200081113     C                                          :PiVal
075300081113     C                                          :PiNum
075400081113     C                                          :PiInt)
075500081113     C*
075600000801     C                   IF        %error
075700000801     C                   EVAL      PiInt=*off
075800000801     C                   ENDIF
075900000801     C*
076000000801     C                   ENDSR
076100000801     C***
076200000801
076300011113
076400011113     C*----------------------------------------------------*
076500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
076600011113     C*----------------------------------------------------*
076700011113     C     CHKIMPDIV     BEGSR
076800011113     C*
076900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
077000011113     C                   Z-ADD     *zeros        wrkDec            9 9
077100011113     C*
077200011113     C* Come prima cosa effettuo considerazioni sulla divisa
077300011113     C                   IF        vabIAS > *zeros
077400011113     C                   IF        vabVAS <> 'EUR'
077500011113     C                   EVAL      vabVAS =  'ITL'
077600011113     C                   ENDIF
077700011113     C                   ENDIF
077800011113     C*
077900011113     C                   IF        vabCAS > *zeros
078000011113     C                   IF        vabVCA <> 'EUR'
078100011113     C                   EVAL      vabVCA =  'ITL'
078200011113     C                   ENDIF
078300011113     C                   ENDIF
078400011113     C*
078500011113     C                   IF        vabVMD > *zeros
078600020305     C                   IF        vabVAD <> 'EUR'
078700011113     C                   EVAL      vabVAD =  'ITL'
078800011113     C                   ENDIF
078900011113     C                   ENDIF
079000011113     C*
079100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
079200011113     C                   Z-ADD     vabIAS        wrkDec
079300011113     C                   IF        wrkDec > *zeros
079400011113     C                   IF        vabVAS = 'ITL'
079500011113     C                   EVAL      vabIAS = *zeros
079600011113     C                   ENDIF
079700011113     C                   ENDIF
079800011113     C*
079900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
080000011113     C                   Z-ADD     vabCAS        wrkDec
080100011113     C                   IF        wrkDec > *zeros
080200011113     C                   IF        vabVCA = 'ITL'
080300011113     C                   EVAL      vabCAS = *zeros
080400011113     C                   ENDIF
080500011113     C                   ENDIF
080600011113     C*
080700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
080800011113     C                   Z-ADD     vabVMD        wrkDec
080900011113     C                   IF        wrkDec > *zeros
081000011113     C                   IF        vabVAD = 'ITL'
081100011113     C                   EVAL      vabVMD = *zeros
081200011113     C                   ENDIF
081300011113     C                   ENDIF
081400011113     C*
081500011113     C                   ENDSR
081600011113     C***
081700011113
081800011113
081900000801
082000000801
082100990920      /TITLE Invio dei dati al punto operativo.
082200010202     C     invio         BEGSR
082300990920     C*
082400131016     C* 1° invio FIVAT
082500010201     C                   reset                   dscmz
082600111031     C                   move      wPOI          cmzdst
082700131016     C                   eval      cmzfld = 'FIVATWWR'
082800010201     C                   eval      cmzmbd = vlrhdl
082900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
083000021009     C***                if        prmfir = *blanks
083100131016     C                   eval      cmzfla = 'FIVAT00F'
083200131016     C                   eval      cmzmba = 'FIVAT00F'
083300021009     C***                else
083400021009     C***                eval      cmzfla = prmfir
083500021009     C***                eval      cmzmba = prmfir
083600021009     C***                endif
083700010201     C                   eval      cmznrr = *zeros
083800020305     C                   move      §ctrokvt      cmznrr
083900021018     C                   eval      cmzlba = vlrfl1
084000010201     C                   call(e)   'TIS711C'
084100010201     C                   parm                    dscmz
084200010201     C                   parm      *blanks       esito
084300010205     C                   if        %error
084400010205     C                             or cmzerr = '1'
084500010205     C                             or esito  = '1'
084600010205     C                   eval      wrkesito = '3'
084700010205     C                   else
084800010201     C*
084900131016     C* 2° invio FIVAB
085000010201     C                   reset                   dscmz
085100111031     C                   move      wPOI          cmzdst
085200010201     C                   eval      cmzfld = vlrfou
085300010201     C                   eval      cmzmbd = vlrhdl
085400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085500021009     C***                if        prmfir = *blanks
085600131016     C                   eval      cmzfla = 'FIVAB00F'
085700131016     C                   eval      cmzmba = 'FIVAB00F'
085800021009     C***                else
085900021009     C***                eval      cmzfla = prmfir
086000021009     C***                eval      cmzmba = prmfir
086100021009     C***                endif
086200010201     C                   eval      cmznrr = *zeros
086300010201     C                   move      §ctrokvb      cmznrr
086400021018     C                   eval      cmzlba = vlrfl1
086500010201     C                   call(e)   'TIS711C'
086600010201     C                   parm                    dscmz
086700010201     C                   parm      *blanks       esito
086800010201     C                   if        %error
086900010201     C                             or cmzerr = '1'
087000010201     C                             or esito  = '1'
087100010201     C                   eval      wrkesito = '3'
087200010201     C                   endif
087300010205     C                   endif
087400990920     C*
087500000613     C                   ENDSR
087600000613     C***
087700070411
087800070411     C     *pssr         BEGSR
087900070411     C*
088000070411     C                   if        %open(tivin00r)
088100070411     C                   close     tivin00r
088200070411     C                   endif
088300131016     C                   if        %open(FIVABWWr)
088400131016     C                   close     FIVABWWr
088500120706     C                   endif
088600131016     C                   if        %open(FIVATWWr)
088700131016     C                   close     FIVATWWr
088800070411     C                   endif
088900070411     C*
089000070411     C* Effettuo la chiamata al CLLE preposto
089100131017     C                   call(e)   'TITVVTC'
089200070411     C                   parm                    parccm
089300070411     C                   parm                    parmbr
089400070411     C                   parm      '2'           paropz
089500070411     C*
089600070411     C                   eval      wrkesito = '2'
089700070411     C*
089800070411     C                   seton                                        LR
089900070411     C*
090000070411     C                   ENDSR     '*CANCL'
090100070411     C***
090200070411
090300990910
090400000613     C     *inzsr        BEGSR
090500990910     C*
090600990910     C     *entry        plist
090700990920     C                   parm                    tivlrds
090800990921     C                   parm      wrkesito      esito
090900000724     C                   parm                    prmlit
091000000710     C                   parm                    prmfir
091100000613     C*
091200000830     C* CALCOLA LA DATA CORRENTE
091300120706     C                   time                    wn14             14 0
091400120706     C                   movel     wn14          oracor            6 0          *ORA
091500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
091600100324     C                   eval      datcor = %dec(%date() : *ISO)
091700000830     C*
091800000613     C                   ENDSR
091900000613     C***
