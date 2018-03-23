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
002700140723     D depspe          s             20    INZ(*blanks)
002800140723     D curspe          s             20    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500131016     D w70             s             70
003600140723     D w100            s            100
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500140723     D  VINSPE                       20
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
012800140723     C+ vindta, vinflg, substr(vindta, 1, 20) as sped, rrn(tivin00r)
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
015500140723     C* (uso RMN)
015600140723     C                   EVAL      PiStr=%trim(%subst(vindta:1:20))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200111130     C  N31              eval      fivabds_ok = fivabds
016300111130     C  N31              exsr      wrivab
016400140411     C                   exsr      wrivat_a                                     => carico VAT
016500140328     C                   exsr      wrivat_b                                     => carico VAT
016600140411     C                   exsr      wrivat_i                                     => carico VAT
016700140411     C                   exsr      wrivat_s                                     => carico VAT
016800111125     C                   exsr      inzvar
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200140723     C                   exsr      repNSP
017300071009     C                   exsr      impvab
017400111130     C  N31              eval      fivabds_ok = fivabds
017500111130     C  N31              exsr      wrivab
017600140411     C                   exsr      wrivat_a                                     => carico VAT
017700140328     C                   exsr      wrivat_b                                     => carico VAT
017800140411     C                   exsr      wrivat_i                                     => carico VAT
017900140411     C                   exsr      wrivat_s                                     => carico VAT
018000140411     C                   exsr      wrivat_e                                     => carico VAT
018100111125     C                   exsr      inzvar
018200071009     C                   else
018300071009     C*
018400010305     C                   if        depspe = *blanks                             => 1° giro
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600080117     C                   clear                   tivinds
018700140723     C                   exsr      repNSP
018800020305     C                   exsr      impvab
018900111130     C                   z-add     VABNCL        sav_VABNCL
019000111130     C                   z-add     VABPKB        sav_VABPKB
019100111130     C                   z-add     VABVLB        sav_VABVLB
019200140411     C                   exsr      wrivat_a                                     => carico VAT
019300140328     C                   exsr      wrivat_b                                     => carico VAT
019400140411     C                   exsr      wrivat_i                                     => carico VAT
019500140411     C                   exsr      wrivat_s                                     => carico VAT
019600140411     C   50              exsr      wrivat_e                                     => carico VAT
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
020700140723     C                   exsr      repNSP
020800020305     C                   exsr      impvab
020900111130     C                   if        wCntRecSpe = *zeros
021000111130     C                   z-add     VABNCL        sav_VABNCL
021100111130     C                   z-add     VABPKB        sav_VABPKB
021200111130     C                   z-add     VABVLB        sav_VABVLB
021300111130     C                   endif
021400111130     C                   add       1             wCntRecSpe
021500140411     C                   exsr      wrivat_a                                     => carico VAT
021600140328     C                   exsr      wrivat_b                                     => carico VAT
021700140411     C                   exsr      wrivat_i                                     => carico VAT
021800140411     C                   exsr      wrivat_s                                     => carico VAT
021900140411     C   50              exsr      wrivat_e                                     => carico VAT
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
023500140411     C                   exsr      wrivat_a                                     => carico VAT
023600140328     C                   exsr      wrivat_b                                     => carico VAT
023700140411     C                   exsr      wrivat_i                                     => carico VAT
023800140411     C                   exsr      wrivat_s                                     => carico VAT
023900140411     C   50              exsr      wrivat_e                                     => carico VAT
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
026900140307     C                   z-add     sav_VABNCL    VABNCL
027000140307     C                   z-add     sav_VABPKB    VABPKB
027100140307     C                   z-add     sav_VABVLB    VABVLB
027200111130     C  N31              eval      fivabds_ok = fivabds
027300111130     C  N31              exsr      wrivab
027400071009     C                   endif
027500010202     C*
027600010202     C                   endif
027700990910
027800990910     C* Se non ci sono record con errori ...
027900000710     C                   if        §ctrno = 0
028000990910     C* ... restituisco esito OK.
028100990921     C                   eval      wrkesito = '0'
028200990910     C                   else
028300010201     C                   if        §ctrokvb > 0
028400990921     C                   eval      wrkesito = '1'
028500000710     C                   else
028600000710     C                   eval      wrkesito = '2'
028700990910     C                   endif
028800000710     C                   endif
028900990910     C*
029000990914     C                   if        %open(tivin00r)
029100990908     C                   close     tivin00r
029200990914     C                   endif
029300131016     C                   if        %open(FIVABWWr)
029400131016     C                   close     FIVABWWr
029500120706     C                   endif
029600131016     C                   if        %open(FIVATWWr)
029700131016     C                   close     FIVATWWr
029800010201     C                   endif
029900990910     C*
030000111031     C                   if        §ctrokvb > 0 or
030100111031     C                             §ctrokvt > 0
030200111031     C                             and wPOI <> *zeros
030300010202     C                   exsr      invio
030400990920     C                   endif
030500990920     C*
030600910830     C                   ENDSR
030700000613     C***
030800010305
030900010305     C*----------------------------------------------------*
031000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
031100010305     C*----------------------------------------------------*
031200020305     C     WRIVAB        BEGSR
031300070730     C*
031400071003     C* Considerazioni finali
031500111125     C                   if        ok_VABRMA = *blanks
031600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
031700071003     C                   endif
031800071003     C*
031900131016     C                   write     FIVAB000                                     => scarico il VAB
032000010305     C*
032100010305     C                   ENDSR
032200990920
032300000801     C*----------------------------------------------------*
032400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
032500000801     C*----------------------------------------------------*
032600010201     C     INZVAR        BEGSR
032700000801     C*
032800010201     C                   Z-ADD     *zeros        Num5_0
032900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
033000020725     C                   MOVEL     '0'           FlgCAS            1
033100111125     C*
033200111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
033300111130     C*
033400111125     C                   CLEAR                   FIVABDS
033500111130     C                   CLEAR                   FIVABDS_OK
033600111130     C                   CLEAR                   FIVABDS_SAV
033700131016     C                   CLEAR                   FIVAB000
033800131016     C                   CLEAR                   FIVAT000
033900000801     C*
034000000801     C                   ENDSR
034100000801     C*----------------------------------------------------*
034200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
034300000801     C*----------------------------------------------------*
034400020904     C     DEFCAM        BEGSR
034500080923     C*
034600111125     C                   SETOFF                                       50
034700111125     C                   SETOFF                                       515253
034800131016     C                   SETOFF                                       54
034900070730     C* Imposto i valori di default...
035000140723     C                   EVAL      VABCCM = 1202828
035100140723     C                   EVAL      VATCCM = 1202828
035200140723     C                   EVAL      VABLNP = 005
035300140723     C                   EVAL      VATLNP = 005
035400140328     C                   EVAL      VABCTR = 000
035500131016     C                   EVAL      VABCBO = '1'
035600070222     C* ... e poi verifico se sono stati passati come parametri
035700070222     C                   IF        vlrppt > *blanks
035800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
035900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
036000070222     C                   EXSR      CHKNUM
036100070222     C                   IF        PiInt=*on
036200070222     C                   Z-ADD     PiVal         VABCCM
036300070222     C                   Z-ADD     PiVal         VATCCM
036400070222     C                   ENDIF
036500070222     C                   ENDIF
036600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
036700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
036800070222     C                   EXSR      CHKNUM
036900070222     C                   IF        PiInt=*on
037000070222     C                   Z-ADD     PiVal         VABLNP
037100070222     C                   Z-ADD     PiVal         VATLNP
037200070222     C                   ENDIF
037300070222     C                   ENDIF
037400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
037500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
037600070222     C                   EXSR      CHKNUM
037700070222     C                   IF        PiInt=*on
037800070222     C                   Z-ADD     PiVal         VABCTR
037900070222     C                   ENDIF
038000070928     C                   ENDIF
038100071009     C                   MOVEL     *blanks       wDISK             1
038200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
038300071009     C                   IF        wDISK <> *blanks
038400070928     C                   SETON                                        50
038500070222     C                   ENDIF
038600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
038700111125     C                   SETON                                        41
038800080923     C                   ENDIF
038900070222     C                   ENDIF
039000120706     C*
039100120706     C* valido per nr.colli/peso/volume
039200120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
039300120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
039400120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
039500131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
039600120706     C                   SELECT
039700120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
039800120706     C                   SETON                                        51
039900120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
040000120706     C                   SETON                                        52
040100120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
040200120706     C                   SETON                                        53
040300131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
040400131016     C                   SETON                                        54
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
043500140905     C                             + ' ' + 'VABNSP VATNSP'
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
045700140723     C                   EVAL      w100=%trim(%subst(vindta:89:100))
045800140723     C                   EVAL      VABRSD=%subst(w100:1:35)
045900140723     C                   EVAL      VABRD2=%subst(w100:36:35)
046000131016     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100140306     C     '@':'A'       XLATE     VABRSD        VABRSD
046200131016     C* ==
046300140723     C                   EVAL      VABIND=%trim(%subst(vindta:189:100))
046400140723     C                   EVAL      VABLOD=%trim(%subst(vindta:289:50))
046500140723     C                   EVAL      VABPRD=%trim(%subst(vindta:359:2))
046600140723     C                   EVAL      VABNZD=%trim(%subst(vindta:361:50))
046700140723     C                   IF        VABNZD = 'ITA'
046800140723     C                   EVAL      VABNZD = *blank
046900140723     C                   ENDIF
047000161124     C                   EVAL      VABRMA=%trim(%subst(vindta:1:20))
047100131016     C*
047200131016     C* CAD
047300140723     C                   EVAL      PiStr=%trim(%subst(vindta:339:20))
047400131016     C                   EXSR      CHKNUM
047500131016     C                   IF        PiInt=*on
047600131016     C                   Z-ADD     PiVal         Num5_0
047700131016     C                   MOVEL     Num5_0        VABCAD
047800131016     C                   ELSE
047900131016     C                   SETON                                        32
048000131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048100131016     C                             + ' ' + 'VABCAD'
048200131016     C                   ENDIF
048300140328     C* CAS
048400140723     C                   IF        %subst(vindta:419:8)<> *blanks    AND
048500140723     C                             %subst(vindta:419:8)<> *zeros
048600140328     C                   EVAL      FlgCAS = '1'
048700140723     C                   EVAL      VABVCA =%trim(%subst(vindta:416:3))
048800140723     C                   EVAL      PiStr=%trim(%subst(vindta:419:8))
048900140328     C                   EXSR      CHKNUM
049000140328     C                   IF        PiNum=*on
049100141002     C                   EVAL      VABCAS = PiVal/100
049200140328     C                   ELSE
049300140328     C  N51              SETON                                        32
049400140328     C  N51              Z-ADD     *zeros        VABCAS
049500140328     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
049600140328     C                             + ' ' + 'VABCAS'
049700140328     C                   ENDIF
049800140328     C                   ELSE
049900140328     C                   EVAL      FlgCAS = '0'
050000140328     C                   ENDIF
050100140411     C* RMN
050200161124     C                   EVAL      PiStr=%trim(%subst(vindta:61:17))
050300161124     C                   EXSR      CHKNUM
050400161124     C                   IF        PiInt=*on
050500161124     C                   Z-ADD     PiVal         VABRMN
050600161124     C                   ELSE
050700161124     C                   SETON                                        32
050800161124     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050900161124     C                             + ' ' + 'VABRMN'
051000161124     C                   ENDIF
051100140411     C* NCL
051200140723     C                   EVAL      PiStr=%trim(%subst(vindta:411:5))
051300140411     C                   EXSR      CHKNUM
051400140411     C                   IF        PiInt=*on
051500140411     C                   Z-ADD     PiVal         VABNCL
051600140411     C                   ELSE
051700140411     C                   SETON                                        32
051800140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051900140411     C                             + ' ' + 'VABNCL'
052000140411     C                   ENDIF
052100140411     C* PKB
052200140723     C                   EVAL      PiStr=%trim(%subst(vindta:444:8))
052300140411     C                   EXSR      CHKNUM
052400140411     C                   IF        PiNum=*on
052500141002     C                   EVAL      VABPKB = PiVal/1000
052600140411     C                   ELSE
052700140411     C                   SETON                                        32
052800140411     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900140411     C                             + ' ' + 'VABPKB'
053000140411     C                   ENDIF
053100140411     C*
053200140411     C* Reperisco la provincia dal CAP e dalla località
053300140411     C                   IF        VABCAD <> *blanks AND
053400140411     C                             VABPRD  = *blanks AND
053500140411     C                             VABNZD  = *blanks
053600140411     C                   CLEAR                   TISI95DS
053700140411     C                   EVAL      I95TCN = '3'
053800140411     C                   Z-ADD     datcor        I95DAT
053900140411     C                   EVAL      I95CAP = VABCAD
054000140411     C                   EVAL      I95LOC = VABLOD
054100140411     C                   CALL      'TISI95R'
054200140411     C                   PARM                    TISI95DS
054300140411     C                   EVAL      VABPRD = O95PRV
054400140411     C                   ENDIF
054500070730     C*
054600070730     C* Considerazioni finali su CBO/CAS
054700121113     C                   IF        FlgCAS <> '0'
054800070730     C                   IF        VABCBO = '1'
054900070730     C                   EVAL      VABCBO = '4'
055000070730     C                   ENDIF
055100070730     C                   IF        VABCBO = '2'
055200070730     C                   EVAL      VABCBO = '6'
055300070730     C                   ENDIF
055400070730     C                   ENDIF
055500020305     C*
055600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055700011113     C                   EXSR      CHKIMPDIV
055800010202     C*
055900000801     C* Ebbene...
056000000801     C                   ADD       1             §CTRMO
056100070530     C                   IF        *in31 <> *zeros OR
056200070530     C                             *in32 <> *zeros
056300000801     C                   ADD       1             §CTRNO
056400020725     C                   EVAL      x_vinflg = '2'
056500000801     C                   ELSE
056600010201     C                   ADD       1             §CTROKVB
056700000801     C                   ENDIF
056800000801     C*
056900000801     C                   ENDSR
057000140411     C*----------------------------------------------------*
057100140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
057200140411     C*----------------------------------------------------*
057300140411     C     WRIVAT_A      BEGSR
057400140411     C*
057500140411     C                   eval      vatAAS = vabAAS
057600140411     C                   eval      vatLNP = vabLNP
057700140411     C                   eval      vatNRS = vabNRS
057800140411     C                   eval      vatNSP = vabNSP
057900140411     C*
058000140411     C* Valorizzo il buffer di scrittura del FIVAT
058100140411     C* - TIPO RECORD "A"
058200140723     C                   if        %subst(w100:71:30) <> *blanks
058300140411     C                   eval      VATTRC = 'A'
058400140723     C                   eval      VATNOT = %trim(%subst(w100:71:30))
058500140411     C                   write     FIVAT000
058600140411     C                   endif
058700140411     C*
058800140411     C                   ENDSR
058900140411     C*----------------------------------------------------*
059000140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
059100140411     C*----------------------------------------------------*
059200140411     C     WRIVAT_B      BEGSR
059300140411     C*
059400140411     C                   eval      vatAAS = vabAAS
059500140411     C                   eval      vatLNP = vabLNP
059600140411     C                   eval      vatNRS = vabNRS
059700140411     C                   eval      vatNSP = vabNSP
059800140411     C*
059900140411     C* Valorizzo il buffer di scrittura del FIVAT
060000140411     C* - TIPO RECORD "B"
060100140723     C***                IF        %trim(%subst(vindta:383:16)) <> *blank
060200140723     C***                eval      VATTRC = 'B'
060300140723     C***                eval      VATNOT = %trim(%subst(vindta:383:16))
060400140723     C***                write     FIVAT000
060500140723     C***                endif
060600140411     C*
060700140411     C                   ENDSR
060800140411     C*----------------------------------------------------*
060900140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
061000140411     C*----------------------------------------------------*
061100140411     C     WRIVAT_I      BEGSR
061200140411     C*
061300140411     C                   eval      vatAAS = vabAAS
061400140411     C                   eval      vatLNP = vabLNP
061500140411     C                   eval      vatNRS = vabNRS
061600140411     C                   eval      vatNSP = vabNSP
061700140411     C*
061800140411     C* Valorizzo il buffer di scrittura del FIVAT
061900140411     C* - TIPO RECORD "I"
062000140723     C***                EVAL      w70=%trim(%subst(vindta:434:70))
062100140723     C***                if        %subst(w70:1:35) <> *blanks
062200140723     C***                eval      VATTRC = 'I'
062300140723     C***                eval      VATNOT = %trim(%subst(w70:1:35))
062400140723     C***                write     FIVAT000
062500140723     C***                endif
062600140723     C***                if        %subst(w70:36:35) <> *blanks
062700140723     C***                eval      VATTRC = 'J'
062800140723     C***                eval      VATNOT = %trim(%subst(w70:36:35))
062900140723     C***                write     FIVAT000
063000140723     C***                endif
063100140411     C*
063200140411     C                   ENDSR
063300140411     C*----------------------------------------------------*
063400140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "S"
063500140411     C*----------------------------------------------------*
063600140411     C     WRIVAT_S      BEGSR
063700140411     C*
063800140411     C                   eval      vatAAS = vabAAS
063900140411     C                   eval      vatLNP = vabLNP
064000140411     C                   eval      vatNRS = vabNRS
064100140411     C                   eval      vatNSP = vabNSP
064200140411     C*
064300140411     C* Valorizzo il buffer di scrittura del FIVAT
064400140411     C* - TIPO RECORD "S"
064500140723     C***                IF        %trim(%subst(vindta:504:16)) <> *blank
064600140723     C***                eval      VATTRC = 'S'
064700140723     C***                eval      VATNOT = %trim(%subst(vindta:504:16))
064800140723     C***                EVAL      %subst(VATNOT:17:2) = 'SS'
064900140723     C***                write     FIVAT000
065000140723     C***                endif
065100140411     C*
065200140411     C                   ENDSR
065300140411     C*----------------------------------------------------*
065400140411     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
065500140411     C*----------------------------------------------------*
065600140411     C     WRIVAT_E      BEGSR
065700140411     C*
065800140411     C                   eval      vatAAS = vabAAS
065900140411     C                   eval      vatLNP = vabLNP
066000140411     C                   eval      vatNRS = vabNRS
066100140411     C                   eval      vatNSP = vabNSP
066200140411     C*
066300140411     C* Valorizzo il buffer di scrittura del FIVAT
066400140411     C* - TIPO RECORD "E"
066500140723     C                   if        %trim(%subst(vindta:427:9)) <> *blanks
066600140411     C                   eval      VATTRC = 'E'
066700140723     C                   EVAL      VATNOT = %trim(%editc(
066800140723     C                                      %dec(%subst(vindta:427:9) : 9 : 0)
066900140723     C                                      : '4'))
067000140411     C                   write     FIVAT000
067100140411     C                   endif
067200140411     C*
067300140411     C                   ENDSR
067400020904
067500020904
067600010202     C*----------------------------------------------------*
067700131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
067800010202     C*----------------------------------------------------*
067900020305     C     PREVAT        BEGSR
068000010202     C*
068100131016     C* Compongo il nome del membro da dare al FIVATWWR
068200010202     C                   eval      parmbr = vlrhdl
068300010202     C                   movel     'M'           parmbr
068400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
068500010202     C                   eval      paropz = '1'
068600010202     C* Effettuo la chiamata al CLLE preposto
068700131017     C                   call(e)   'TITVVTC'
068800010202     C                   parm                    parccm
068900010202     C                   parm                    parmbr
069000010202     C                   parm                    paropz
069100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
069200010202     C                   if        %error
069300010202     C                   movel     '1'           chkcall
069400010202     C                   else
069500010202     C                   movel     '0'           chkcall
069600010202     C                   endif
069700010202     C*
069800010202     C                   ENDSR
069900000801     C*----------------------------------------------------*
070000000801     C*  CONTROLLO NUMERICITA' CAMPI
070100000801     C*----------------------------------------------------*
070200000801     C     CHKNUM        BEGSR
070300081113     C*
070400081113     C                   IF        PiDecChr = *blanks
070500140411     C                   EVAL      PiDecChr = ','
070600081113     C                   ENDIF
070700081113     C*
070800081113     C                   callp(e)  UBISNUM_Check(PiStr
070900081113     C                                          :PiDecChr
071000081113     C                                          :PiVal
071100081113     C                                          :PiNum
071200081113     C                                          :PiInt)
071300081113     C*
071400000801     C                   IF        %error
071500000801     C                   EVAL      PiInt=*off
071600000801     C                   ENDIF
071700000801     C*
071800000801     C                   ENDSR
071900000801     C***
072000000801
072100011113
072200011113     C*----------------------------------------------------*
072300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
072400011113     C*----------------------------------------------------*
072500011113     C     CHKIMPDIV     BEGSR
072600011113     C*
072700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
072800011113     C                   Z-ADD     *zeros        wrkDec            9 9
072900011113     C*
073000011113     C* Come prima cosa effettuo considerazioni sulla divisa
073100011113     C                   IF        vabIAS > *zeros
073200011113     C                   IF        vabVAS <> 'EUR'
073300011113     C                   EVAL      vabVAS =  'ITL'
073400011113     C                   ENDIF
073500011113     C                   ENDIF
073600011113     C*
073700011113     C                   IF        vabCAS > *zeros
073800011113     C                   IF        vabVCA <> 'EUR'
073900011113     C                   EVAL      vabVCA =  'ITL'
074000011113     C                   ENDIF
074100011113     C                   ENDIF
074200011113     C*
074300011113     C                   IF        vabVMD > *zeros
074400020305     C                   IF        vabVAD <> 'EUR'
074500011113     C                   EVAL      vabVAD =  'ITL'
074600011113     C                   ENDIF
074700011113     C                   ENDIF
074800011113     C*
074900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
075000011113     C                   Z-ADD     vabIAS        wrkDec
075100011113     C                   IF        wrkDec > *zeros
075200011113     C                   IF        vabVAS = 'ITL'
075300011113     C                   EVAL      vabIAS = *zeros
075400011113     C                   ENDIF
075500011113     C                   ENDIF
075600011113     C*
075700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
075800011113     C                   Z-ADD     vabCAS        wrkDec
075900011113     C                   IF        wrkDec > *zeros
076000011113     C                   IF        vabVCA = 'ITL'
076100011113     C                   EVAL      vabCAS = *zeros
076200011113     C                   ENDIF
076300011113     C                   ENDIF
076400011113     C*
076500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
076600011113     C                   Z-ADD     vabVMD        wrkDec
076700011113     C                   IF        wrkDec > *zeros
076800011113     C                   IF        vabVAD = 'ITL'
076900011113     C                   EVAL      vabVMD = *zeros
077000011113     C                   ENDIF
077100011113     C                   ENDIF
077200011113     C*
077300011113     C                   ENDSR
077400011113     C***
077500011113
077600011113
077700000801
077800000801
077900990920      /TITLE Invio dei dati al punto operativo.
078000010202     C     invio         BEGSR
078100990920     C*
078200131016     C* 1° invio FIVAT
078300010201     C                   reset                   dscmz
078400111031     C                   move      wPOI          cmzdst
078500131016     C                   eval      cmzfld = 'FIVATWWR'
078600010201     C                   eval      cmzmbd = vlrhdl
078700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078800021009     C***                if        prmfir = *blanks
078900131016     C                   eval      cmzfla = 'FIVAT00F'
079000131016     C                   eval      cmzmba = 'FIVAT00F'
079100021009     C***                else
079200021009     C***                eval      cmzfla = prmfir
079300021009     C***                eval      cmzmba = prmfir
079400021009     C***                endif
079500010201     C                   eval      cmznrr = *zeros
079600020305     C                   move      §ctrokvt      cmznrr
079700021018     C                   eval      cmzlba = vlrfl1
079800010201     C                   call(e)   'TIS711C'
079900010201     C                   parm                    dscmz
080000010201     C                   parm      *blanks       esito
080100010205     C                   if        %error
080200010205     C                             or cmzerr = '1'
080300010205     C                             or esito  = '1'
080400010205     C                   eval      wrkesito = '3'
080500010205     C                   else
080600010201     C*
080700131016     C* 2° invio FIVAB
080800010201     C                   reset                   dscmz
080900111031     C                   move      wPOI          cmzdst
081000010201     C                   eval      cmzfld = vlrfou
081100010201     C                   eval      cmzmbd = vlrhdl
081200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
081300021009     C***                if        prmfir = *blanks
081400131016     C                   eval      cmzfla = 'FIVAB00F'
081500131016     C                   eval      cmzmba = 'FIVAB00F'
081600021009     C***                else
081700021009     C***                eval      cmzfla = prmfir
081800021009     C***                eval      cmzmba = prmfir
081900021009     C***                endif
082000010201     C                   eval      cmznrr = *zeros
082100010201     C                   move      §ctrokvb      cmznrr
082200021018     C                   eval      cmzlba = vlrfl1
082300010201     C                   call(e)   'TIS711C'
082400010201     C                   parm                    dscmz
082500010201     C                   parm      *blanks       esito
082600010201     C                   if        %error
082700010201     C                             or cmzerr = '1'
082800010201     C                             or esito  = '1'
082900010201     C                   eval      wrkesito = '3'
083000010201     C                   endif
083100010205     C                   endif
083200990920     C*
083300000613     C                   ENDSR
083400000613     C***
083500070411
083600070411     C     *pssr         BEGSR
083700070411     C*
083800070411     C                   if        %open(tivin00r)
083900070411     C                   close     tivin00r
084000070411     C                   endif
084100131016     C                   if        %open(FIVABWWr)
084200131016     C                   close     FIVABWWr
084300120706     C                   endif
084400131016     C                   if        %open(FIVATWWr)
084500131016     C                   close     FIVATWWr
084600070411     C                   endif
084700070411     C*
084800070411     C* Effettuo la chiamata al CLLE preposto
084900131017     C                   call(e)   'TITVVTC'
085000070411     C                   parm                    parccm
085100070411     C                   parm                    parmbr
085200070411     C                   parm      '2'           paropz
085300070411     C*
085400070411     C                   eval      wrkesito = '2'
085500070411     C*
085600070411     C                   seton                                        LR
085700070411     C*
085800070411     C                   ENDSR     '*CANCL'
085900070411     C***
086000070411
086100990910
086200000613     C     *inzsr        BEGSR
086300990910     C*
086400990910     C     *entry        plist
086500990920     C                   parm                    tivlrds
086600990921     C                   parm      wrkesito      esito
086700000724     C                   parm                    prmlit
086800000710     C                   parm                    prmfir
086900000613     C*
087000000830     C* CALCOLA LA DATA CORRENTE
087100120706     C                   time                    wn14             14 0
087200120706     C                   movel     wn14          oracor            6 0          *ORA
087300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
087400100324     C                   eval      datcor = %dec(%date() : *ISO)
087500000830     C*
087600000613     C                   ENDSR
087700000613     C***
