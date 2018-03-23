000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400150608      * - nella routine CHKNUM il separalore decimale da punto (.) diventa virgola (,)
000500150608      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700990908
000800020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000900131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
001000131016     FFIVATWWr  O    E             DISK    usropn
001100990908
001200000801     D*----------------------------------------------------
001300000801     D* DICHIARAZIOINE VARIABILI DI WRK
001400000801     D*----------------------------------------------------
001500990920     D dscmz         e ds                  inz
001600990910     D psds           sds
001700990910     D  procname         *PROC
001800990920     D tivlrds       e ds                  extname(tivlr00f)
001900070730     D tisi95ds      e ds
002000131016     D fivabds       e ds                  extname(FIVAB00f)
002100131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
002200131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800150605     D depspe          s              8    INZ(*blanks)
002900150605     D curspe          s              8    INZ(*blanks)
003000010202     D parccm          s              8    INZ(*blanks)
003100010202     D parmbr          s             10    INZ(*blanks)
003200010202     D paropz          s              1    INZ(*blanks)
003300010202     D chkcall         s              1    INZ(*blanks)
003400080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003500070530     D Num5_0          s              5  0
003600131016     D w70             s             70
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500150605     D  VINSPE                        8
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
012800140306     C+ vindta, vinflg, substr(vindta, 33, 10) as sped, rrn(tivin00r)
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
015500140306     C* (uso NSP)
015600150605     C                   EVAL      PiStr=%trim(%subst(vindta:192:8))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200111130     C  N31              eval      fivabds_ok = fivabds
016300111130     C  N31              exsr      wrivab
016400140120     C***                exsr      wrivat_a                                     => carico VAT
016500140120     C***                exsr      wrivat_b                                     => carico VAT
016600111125     C                   exsr      inzvar
016700071003     C                   else
016800080923     C*
016900071009     C                   if        wDISK = 'D'
017000140120     C***                exsr      repNSP
017100071009     C                   exsr      impvab
017200111130     C  N31              eval      fivabds_ok = fivabds
017300111130     C  N31              exsr      wrivab
017400140120     C***                exsr      wrivat_a                                     => carico VAT
017500140120     C***                exsr      wrivat_b                                     => carico VAT
017600131016     C                   exsr      wrivat_e                                     => carico VAT
017700111125     C                   exsr      inzvar
017800071009     C                   else
017900071009     C*
018000010305     C                   if        depspe = *blanks                             => 1° giro
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200080117     C                   clear                   tivinds
018300140120     C***                exsr      repNSP
018400020305     C                   exsr      impvab
018500111130     C                   z-add     VABNCL        sav_VABNCL
018600111130     C                   z-add     VABPKB        sav_VABPKB
018700111130     C                   z-add     VABVLB        sav_VABVLB
018800140120     C***                exsr      wrivat_a                                     => carico VAT
018900140120     C***                exsr      wrivat_b                                     => carico VAT
019000071003     C   50              exsr      wrivat_e                                     => carico VAT
019100010305     C                   else
019200020725     C                   if        depspe <> curspe                             => rottura di spediz
019300111130     C                   z-add     sav_VABNCL    VABNCL
019400111130     C                   z-add     sav_VABPKB    VABPKB
019500111130     C                   z-add     sav_VABVLB    VABVLB
019600111130     C  N31              eval      fivabds_ok = fivabds
019700111125     C  N31              exsr      wrivab
019800111125     C                   exsr      inzvar
019900111125     C                   eval      depspe = curspe                              => memorizz. spediz
020000080117     C                   clear                   tivinds
020100140120     C***                exsr      repNSP
020200020305     C                   exsr      impvab
020300111130     C                   if        wCntRecSpe = *zeros
020400111130     C                   z-add     VABNCL        sav_VABNCL
020500111130     C                   z-add     VABPKB        sav_VABPKB
020600111130     C                   z-add     VABVLB        sav_VABVLB
020700111130     C                   endif
020800111130     C                   add       1             wCntRecSpe
020900140120     C***                exsr      wrivat_a                                     => carico VAT
021000140120     C***                exsr      wrivat_b                                     => carico VAT
021100071003     C   50              exsr      wrivat_e                                     => carico VAT
021200020305     C                   else                                                   => x stessa spediz
021300111130     C                   exsr      impvab
021400120706     C   51              add       VABNCL        sav_VABNCL
021500111125     C   51              add       VABPKB        sav_VABPKB
021600111125     C   51              add       VABVLB        sav_VABVLB
021700111125     C   52              z-add     VABNCL        sav_VABNCL
021800111125     C   52              z-add     VABPKB        sav_VABPKB
021900111125     C   52              z-add     VABVLB        sav_VABVLB
022000111125     C   53              add       VABNCL        sav_VABNCL
022100111125     C   53              z-add     VABPKB        sav_VABPKB
022200111125     C   53              z-add     VABVLB        sav_VABVLB
022300131016     C   54              Z-ADD     VABNCL        sav_VABNCL
022400131016     C   54              ADD       VABPKB        sav_VABPKB
022500131016     C   54              ADD       VABVLB        sav_VABVLB
022600111125     C*
022700140120     C***                exsr      wrivat_a                                     => carico VAT
022800140120     C***                exsr      wrivat_b                                     => carico VAT
022900071003     C   50              exsr      wrivat_e                                     => carico VAT
023000010305     C                   endif
023100010305     C                   endif
023200010305     C                   endif
023300071003     C                   endif
023400071009     C                   endif
023500000905     C*
023600000905     C                   else
023700020725     C                   eval      x_vinflg = '1'
023800000905     C                   endif
023900000905     C*
024000020725     C     VINRRN        chain     tivin000
024100020725     C                   if        %found(tivin00r)
024200020725     C                   eval      y_vinflg = x_vinflg
024300020725     C                   eval      y_vinmsg = x_vinmsg
024400020725     C                   update    tivin000
024500020725     C                   endif
024600020725     C*
024700020725     C/EXEC SQL
024800070530     C+ Fetch C1 into :INPUT_DS
024900020725     C/END-EXEC
025000020725     C*
025100020725     C                   enddo
025200020725     C*
025300020725     C/EXEC SQL
025400020725     C+ close C1
025500020725     C/END-EXEC
025600000905     C*
025700020305     C* Scarico i VAB rimasti "in sospeso"
025800071009     C                   if        wDISK = 'C'
025900140307     C                   z-add     sav_VABNCL    VABNCL
026000140307     C                   z-add     sav_VABPKB    VABPKB
026100140307     C                   z-add     sav_VABVLB    VABVLB
026200111130     C  N31              eval      fivabds_ok = fivabds
026300111130     C  N31              exsr      wrivab
026400071009     C                   endif
026500010202     C*
026600010202     C                   endif
026700990910
026800990910     C* Se non ci sono record con errori ...
026900000710     C                   if        §ctrno = 0
027000990910     C* ... restituisco esito OK.
027100990921     C                   eval      wrkesito = '0'
027200990910     C                   else
027300010201     C                   if        §ctrokvb > 0
027400990921     C                   eval      wrkesito = '1'
027500000710     C                   else
027600000710     C                   eval      wrkesito = '2'
027700990910     C                   endif
027800000710     C                   endif
027900990910     C*
028000990914     C                   if        %open(tivin00r)
028100990908     C                   close     tivin00r
028200990914     C                   endif
028300131016     C                   if        %open(FIVABWWr)
028400131016     C                   close     FIVABWWr
028500120706     C                   endif
028600131016     C                   if        %open(FIVATWWr)
028700131016     C                   close     FIVATWWr
028800010201     C                   endif
028900990910     C*
029000111031     C                   if        §ctrokvb > 0 or
029100111031     C                             §ctrokvt > 0
029200111031     C                             and wPOI <> *zeros
029300010202     C                   exsr      invio
029400990920     C                   endif
029500990920     C*
029600910830     C                   ENDSR
029700000613     C***
029800010305
029900010305     C*----------------------------------------------------*
030000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
030100010305     C*----------------------------------------------------*
030200020305     C     WRIVAB        BEGSR
030300070730     C*
030400071003     C* Considerazioni finali
030500111125     C                   if        ok_VABRMA = *blanks
030600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
030700071003     C                   endif
030800071003     C*
030900131016     C                   write     FIVAB000                                     => scarico il VAB
031000010305     C*
031100010305     C                   ENDSR
031200990920
031300000801     C*----------------------------------------------------*
031400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
031500000801     C*----------------------------------------------------*
031600010201     C     INZVAR        BEGSR
031700000801     C*
031800010201     C                   Z-ADD     *zeros        Num5_0
031900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
032000020725     C                   MOVEL     '0'           FlgCAS            1
032100111125     C*
032200111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
032300111130     C*
032400111125     C                   CLEAR                   FIVABDS
032500111130     C                   CLEAR                   FIVABDS_OK
032600111130     C                   CLEAR                   FIVABDS_SAV
032700131016     C                   CLEAR                   FIVAB000
032800131016     C                   CLEAR                   FIVAT000
032900000801     C*
033000000801     C                   ENDSR
033100000801     C*----------------------------------------------------*
033200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
033300000801     C*----------------------------------------------------*
033400020904     C     DEFCAM        BEGSR
033500080923     C*
033600111125     C                   SETOFF                                       50
033700111125     C                   SETOFF                                       515253
033800131016     C                   SETOFF                                       54
033900070730     C* Imposto i valori di default...
034000150605     C                   Z-ADD     2020790       VABCCM
034100150605     C                   Z-ADD     2020790       VATCCM
034200150605     C                   Z-ADD     202           VABLNP
034300150605     C                   Z-ADD     202           VATLNP
034400150605     C                   Z-ADD     000           VABCTR
034500150605     C                   EVAL      VABTSP = 'D'
034600150605     C                   EVAL      VABCTM = '7Q'
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
044900150605     C* Considerazioni sulla ragione sociale del destinatario da indicare
045000150605     C                   EVAL      VABRSD=%trim(%subst(vindta:3:35))
045100150605     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045200150605     C     '@':'A'       XLATE     VABRSD        VABRSD
045300150605     C* ==
045400150605     C                   EVAL      VABRD2=%trim(%subst(vindta:38:35))
045500150605     C                   EVAL      VABIND=%trim(%subst(vindta:73:35))
045600150605     C                   EVAL      VABLOD=%trim(%subst(vindta:117:35))
045700150605     C                   EVAL      VABPRD=%trim(%subst(vindta:152:2))
045800150605     C                   EVAL      VABRMA=%trim(%subst(vindta:192:8))
045900150605     C                   EVAL      VABCBO=%trim(%subst(vindta:2:1))
046000150605     C                   EVAL      VABTIC=%trim(%subst(vindta:183:2))
046100150605     C                   EVAL      VABNOT=%trim(%subst(vindta:200:35))
046200150605     C                   EVAL      VABNT2=%trim(%subst(vindta:235:35))
046300150605     C*
046400150605     C* Reperimento campi NUMERICI
046500150605     C                   MOVEL     DATCOR        VABAAS
046600150605     C                   MOVE      DATCOR        VABMGS
046700150605     C* RMN/NSP
046800150605     C                   EVAL      PiStr=%trim(%subst(vindta:192:8))
046900150605     C                   EXSR      CHKNUM
047000150605     C                   IF        PiInt=*on
047100150605     C                   Z-ADD     PiVal         VABRMN
047200150605     C                   Z-ADD     PiVal         VABNSP
047300150605     C                   ELSE
047400150605     C                   SETON                                        31
047500150605     C                   Z-ADD     *zeros        VABRMN
047600150605     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047700150605     C                             + ' ' + 'VABRMN VABNSP'
047800150605     C                   ENDIF
047900150605     C* CAD
048000150605     C                   EVAL      PiStr=%trim(%subst(vindta:108:5))
048100150605     C                   EXSR      CHKNUM
048200150605     C                   IF        PiInt=*on
048300150605     C                   Z-ADD     PiVal         Num5_0
048400150605     C                   MOVEL(p)  Num5_0        VABCAD
048500150605     C                   ELSE
048600150605     C                   SETON                                        32
048700150605     C                   EVAL      VABCAD = *zeros
048800150605     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900150605     C                             + ' ' + 'VABCAD'
049000150605     C                   ENDIF
049100150605     C* NCL
049200150605     C                   EVAL      PiStr=%trim(%subst(vindta:154:5))
049300150605     C                   EXSR      CHKNUM
049400150605     C                   IF        PiInt=*on
049500150605     C                   Z-ADD     PiVal         VABNCL
049600150605     C                   ELSE
049700150605     C                   SETON                                        32
049800150605     C                   Z-ADD     *zeros        VABNCL
049900150605     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000150605     C                             + ' ' + 'VABNCL'
050100150605     C                   ENDIF
050200150605     C* PKB
050300150605     C                   EVAL      PiStr=%trim(%subst(vindta:159:9))
050400150605     C                   EXSR      CHKNUM
050500150605     C                   IF        PiNum=*on
050600150605     C                   Z-ADD(H)  PiVal         VABPKB
050700150605     C                   ELSE
050800150605     C                   SETON                                        32
050900150605     C                   Z-ADD     *zeros        VABPKB
051000150605     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051100150605     C                             + ' ' + 'VABPKB'
051200150605     C                   ENDIF
051300150605     C* CAS
051400150605     C                   IF        %trim(%subst(vindta:173:10))<>*zeros
051500150605     C                   EVAL      PiStr=%trim(%subst(vindta:173:10))
051600150605     C                   EVAL      VABVCA='EUR'
051700150605     C                   EVAL      FlgCAS = '1'
051800150605     C                   EXSR      CHKNUM
051900150605     C                   IF        PiNum=*on
052000150605     C                   Z-ADD     PiVal         VABCAS
052100150605     C                   ELSE
052200150605     C                   SETON                                        32
052300150605     C                   Z-ADD     *zeros        VABCAS
052400150605     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500150605     C                             + ' ' + 'VABCAS'
052600150605     C                   ENDIF
052700150605     C                   ENDIF
052800070730     C*
052900070730     C* Considerazioni finali su CBO/CAS
053000121113     C                   IF        FlgCAS <> '0'
053100070730     C                   IF        VABCBO = '1'
053200070730     C                   EVAL      VABCBO = '4'
053300070730     C                   ENDIF
053400070730     C                   IF        VABCBO = '2'
053500070730     C                   EVAL      VABCBO = '6'
053600070730     C                   ENDIF
053700070730     C                   ENDIF
053800020305     C*
053900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054000011113     C                   EXSR      CHKIMPDIV
054100010202     C*
054200000801     C* Ebbene...
054300000801     C                   ADD       1             §CTRMO
054400070530     C                   IF        *in31 <> *zeros OR
054500070530     C                             *in32 <> *zeros
054600000801     C                   ADD       1             §CTRNO
054700020725     C                   EVAL      x_vinflg = '2'
054800000801     C                   ELSE
054900010201     C                   ADD       1             §CTROKVB
055000000801     C                   ENDIF
055100000801     C*
055200000801     C                   ENDSR
055300131016     C*----------------------------------------------------*
055400131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
055500131016     C*----------------------------------------------------*
055600131016     C     WRIVAT_A      BEGSR
055700131016     C*
055800131016     C                   eval      vatAAS = vabAAS
055900131016     C                   eval      vatLNP = vabLNP
056000131016     C                   eval      vatNRS = vabNRS
056100131016     C                   eval      vatNSP = vabNSP
056200131016     C*
056300131016     C* Valorizzo il buffer di scrittura del FIVAT
056400131016     C* - TIPO RECORD "A"
056500131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
056600131016     C                   eval      VATTRC = 'A'
056700131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
056800131016     C                   write     FIVAT000
056900131016     C                   endif
057000131016     C*
057100131016     C                   ENDSR
057200131016     C*----------------------------------------------------*
057300131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
057400131016     C*----------------------------------------------------*
057500131016     C     WRIVAT_B      BEGSR
057600131016     C*
057700131016     C                   eval      vatAAS = vabAAS
057800131016     C                   eval      vatLNP = vabLNP
057900131016     C                   eval      vatNRS = vabNRS
058000131016     C                   eval      vatNSP = vabNSP
058100131016     C*
058200131016     C* Valorizzo il buffer di scrittura del FIVAT
058300131016     C* - TIPO RECORD "B"
058400131016     C                   EVAL      w70=%trim(%subst(vindta:113:015))
058500131016     C                   if        %subst(w70:001:15) <> *blanks
058600131016     C                   eval      VATTRC = 'B'
058700131016     C                   eval      VATNOT = %trim(%subst(w70:1:15))
058800131016     C                   write     FIVAT000
058900131016     C                   endif
059000131016     C*
059100131016     C                   ENDSR
059200131016     C*----------------------------------------------------*
059300131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059400131016     C*----------------------------------------------------*
059500131016     C     WRIVAT_E      BEGSR
059600131016     C*
059700131016     C                   eval      vatAAS = vabAAS
059800131016     C                   eval      vatLNP = vabLNP
059900131016     C                   eval      vatNRS = vabNRS
060000131016     C                   eval      vatNSP = vabNSP
060100131016     C*
060200131016     C* Valorizzo il buffer di scrittura del FIVAT
060300131016     C* - TIPO RECORD "E"
060400150904     C                   if        %trim(%subst(vindta:280:20)) <> *blanks
060500131016     C                   eval      VATTRC = 'E'
060600150904     C                   eval      VATNOT = %trim(%subst(vindta:280:20))
060700131016     C                   write     FIVAT000
060800131016     C                   endif
060900131016     C*
061000131016     C                   ENDSR
061100020904
061200020904
061300010202     C*----------------------------------------------------*
061400131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061500010202     C*----------------------------------------------------*
061600020305     C     PREVAT        BEGSR
061700010202     C*
061800131016     C* Compongo il nome del membro da dare al FIVATWWR
061900010202     C                   eval      parmbr = vlrhdl
062000010202     C                   movel     'M'           parmbr
062100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062200010202     C                   eval      paropz = '1'
062300010202     C* Effettuo la chiamata al CLLE preposto
062400131017     C                   call(e)   'TITVVTC'
062500010202     C                   parm                    parccm
062600010202     C                   parm                    parmbr
062700010202     C                   parm                    paropz
062800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062900010202     C                   if        %error
063000010202     C                   movel     '1'           chkcall
063100010202     C                   else
063200010202     C                   movel     '0'           chkcall
063300010202     C                   endif
063400010202     C*
063500010202     C                   ENDSR
063600000801     C*----------------------------------------------------*
063700000801     C*  CONTROLLO NUMERICITA' CAMPI
063800000801     C*----------------------------------------------------*
063900000801     C     CHKNUM        BEGSR
064000081113     C*
064100081113     C                   IF        PiDecChr = *blanks
064200150608     C***                EVAL      PiDecChr = '.'
064300150608     C                   EVAL      PiDecChr = ','
064400081113     C                   ENDIF
064500081113     C*
064600081113     C                   callp(e)  UBISNUM_Check(PiStr
064700081113     C                                          :PiDecChr
064800081113     C                                          :PiVal
064900081113     C                                          :PiNum
065000081113     C                                          :PiInt)
065100081113     C*
065200000801     C                   IF        %error
065300000801     C                   EVAL      PiInt=*off
065400000801     C                   ENDIF
065500000801     C*
065600000801     C                   ENDSR
065700000801     C***
065800000801
065900011113
066000011113     C*----------------------------------------------------*
066100011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066200011113     C*----------------------------------------------------*
066300011113     C     CHKIMPDIV     BEGSR
066400011113     C*
066500011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066600011113     C                   Z-ADD     *zeros        wrkDec            9 9
066700011113     C*
066800011113     C* Come prima cosa effettuo considerazioni sulla divisa
066900011113     C                   IF        vabIAS > *zeros
067000011113     C                   IF        vabVAS <> 'EUR'
067100011113     C                   EVAL      vabVAS =  'ITL'
067200011113     C                   ENDIF
067300011113     C                   ENDIF
067400011113     C*
067500011113     C                   IF        vabCAS > *zeros
067600011113     C                   IF        vabVCA <> 'EUR'
067700011113     C                   EVAL      vabVCA =  'ITL'
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   IF        vabVMD > *zeros
068200020305     C                   IF        vabVAD <> 'EUR'
068300011113     C                   EVAL      vabVAD =  'ITL'
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068800011113     C                   Z-ADD     vabIAS        wrkDec
068900011113     C                   IF        wrkDec > *zeros
069000011113     C                   IF        vabVAS = 'ITL'
069100011113     C                   EVAL      vabIAS = *zeros
069200011113     C                   ENDIF
069300011113     C                   ENDIF
069400011113     C*
069500011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069600011113     C                   Z-ADD     vabCAS        wrkDec
069700011113     C                   IF        wrkDec > *zeros
069800011113     C                   IF        vabVCA = 'ITL'
069900011113     C                   EVAL      vabCAS = *zeros
070000011113     C                   ENDIF
070100011113     C                   ENDIF
070200011113     C*
070300011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070400011113     C                   Z-ADD     vabVMD        wrkDec
070500011113     C                   IF        wrkDec > *zeros
070600011113     C                   IF        vabVAD = 'ITL'
070700011113     C                   EVAL      vabVMD = *zeros
070800011113     C                   ENDIF
070900011113     C                   ENDIF
071000011113     C*
071100011113     C                   ENDSR
071200011113     C***
071300011113
071400011113
071500000801
071600000801
071700990920      /TITLE Invio dei dati al punto operativo.
071800010202     C     invio         BEGSR
071900990920     C*
072000131016     C* 1° invio FIVAT
072100010201     C                   reset                   dscmz
072200111031     C                   move      wPOI          cmzdst
072300131016     C                   eval      cmzfld = 'FIVATWWR'
072400010201     C                   eval      cmzmbd = vlrhdl
072500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072600021009     C***                if        prmfir = *blanks
072700131016     C                   eval      cmzfla = 'FIVAT00F'
072800131016     C                   eval      cmzmba = 'FIVAT00F'
072900021009     C***                else
073000021009     C***                eval      cmzfla = prmfir
073100021009     C***                eval      cmzmba = prmfir
073200021009     C***                endif
073300010201     C                   eval      cmznrr = *zeros
073400020305     C                   move      §ctrokvt      cmznrr
073500021018     C                   eval      cmzlba = vlrfl1
073600010201     C                   call(e)   'TIS711C'
073700010201     C                   parm                    dscmz
073800010201     C                   parm      *blanks       esito
073900010205     C                   if        %error
074000010205     C                             or cmzerr = '1'
074100010205     C                             or esito  = '1'
074200010205     C                   eval      wrkesito = '3'
074300010205     C                   else
074400010201     C*
074500131016     C* 2° invio FIVAB
074600010201     C                   reset                   dscmz
074700111031     C                   move      wPOI          cmzdst
074800010201     C                   eval      cmzfld = vlrfou
074900010201     C                   eval      cmzmbd = vlrhdl
075000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075100021009     C***                if        prmfir = *blanks
075200131016     C                   eval      cmzfla = 'FIVAB00F'
075300131016     C                   eval      cmzmba = 'FIVAB00F'
075400021009     C***                else
075500021009     C***                eval      cmzfla = prmfir
075600021009     C***                eval      cmzmba = prmfir
075700021009     C***                endif
075800010201     C                   eval      cmznrr = *zeros
075900010201     C                   move      §ctrokvb      cmznrr
076000021018     C                   eval      cmzlba = vlrfl1
076100010201     C                   call(e)   'TIS711C'
076200010201     C                   parm                    dscmz
076300010201     C                   parm      *blanks       esito
076400010201     C                   if        %error
076500010201     C                             or cmzerr = '1'
076600010201     C                             or esito  = '1'
076700010201     C                   eval      wrkesito = '3'
076800010201     C                   endif
076900010205     C                   endif
077000990920     C*
077100000613     C                   ENDSR
077200000613     C***
077300070411
077400070411     C     *pssr         BEGSR
077500070411     C*
077600070411     C                   if        %open(tivin00r)
077700070411     C                   close     tivin00r
077800070411     C                   endif
077900131016     C                   if        %open(FIVABWWr)
078000131016     C                   close     FIVABWWr
078100120706     C                   endif
078200131016     C                   if        %open(FIVATWWr)
078300131016     C                   close     FIVATWWr
078400070411     C                   endif
078500070411     C*
078600070411     C* Effettuo la chiamata al CLLE preposto
078700131017     C                   call(e)   'TITVVTC'
078800070411     C                   parm                    parccm
078900070411     C                   parm                    parmbr
079000070411     C                   parm      '2'           paropz
079100070411     C*
079200070411     C                   eval      wrkesito = '2'
079300070411     C*
079400070411     C                   seton                                        LR
079500070411     C*
079600070411     C                   ENDSR     '*CANCL'
079700070411     C***
079800070411
079900990910
080000000613     C     *inzsr        BEGSR
080100990910     C*
080200990910     C     *entry        plist
080300990920     C                   parm                    tivlrds
080400990921     C                   parm      wrkesito      esito
080500000724     C                   parm                    prmlit
080600000710     C                   parm                    prmfir
080700000613     C*
080800000830     C* CALCOLA LA DATA CORRENTE
080900120706     C                   time                    wn14             14 0
081000120706     C                   movel     wn14          oracor            6 0          *ORA
081100100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081200100324     C                   eval      datcor = %dec(%date() : *ISO)
081300000830     C*
081400000613     C                   ENDSR
081500000613     C***
