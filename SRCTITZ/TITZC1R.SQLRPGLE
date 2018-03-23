000100131016      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200120903      *
000300120903      * PARTICOLARITA':
000400140415      * è un Disk A che però stacca il numeratore spedizione (REPNSP) e ha il tracciato E su VAT
000500140120      *
000600121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000700990908
000800020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000900131016     FFIVABWWr  O    E             DISK    usropn prefix(ok_)
001000131016     FFIVATWWr  O    E             DISK    usropn
001100990908
001200140401     D*------------------
001300140401     D TRUL28DSX     e ds                  inz
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800140401     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001900140401     D* 10+3
002000140401     D                 DS
002100140401     D  WS15                   1      1
002200140401     D  WI15                   2     11
002300140401     D  WV15                  12     12
002400140401     D  WD15                  13     15
002500140401     D  WASC15                 1     15
002600140401     D*  6+1
002700140401     D                 DS
002800140401     D  WS09                   1      1
002900140401     D  WI09                   2      7
003000140401     D  WV09                   8      8
003100140401     D  WD09                   9      9
003200140401     D  WASC09                 1      9
003300140401     D*  2+3
003400140401     D                 DS
003500140401     D  WS07                   1      1
003600140401     D  WI07                   2      3
003700140401     D  WV07                   4      4
003800140401     D  WD07                   5      7
003900140401     D  WASC07                 1      7
004000990910     D psds           sds
004100990910     D  procname         *PROC
004200990920     D tivlrds       e ds                  extname(tivlr00f)
004300070730     D tisi95ds      e ds
004400131016     D fivabds       e ds                  extname(FIVAB00f)
004500131016     D fivabds_sav   e ds                  extname(FIVAB00f) prefix(sav_)
004600131016     D fivabds_ok    e ds                  extname(FIVAB00f) prefix(ok_)
004700140401     D fnvabaar      e ds                  inz
004800990910     D esito           s              1
004900000724     D prmlit          s             10
005000000710     D prmfir          s             10
005100990921     D wrkesito        s                   like(esito)
005200000613     D rrnum           s              6  0 INZ(*zeros)
005300140401     D depspe          s              8    INZ(*blanks)
005400140401     D curspe          s              8    INZ(*blanks)
005500010202     D parccm          s              8    INZ(*blanks)
005600010202     D parmbr          s             10    INZ(*blanks)
005700010202     D paropz          s              1    INZ(*blanks)
005800010202     D chkcall         s              1    INZ(*blanks)
005900080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
006000070530     D Num5_0          s              5  0
006100131016     D w70             s             70
006200140401     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
006300000830
006400020725
006500020725     D*------------------
006600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
006700020725     D*------------------
006800070530     D INPUT_DS        DS                  INZ
006900101007     D  VINDTA                     2048
007000101007     D  VINFLG                        1
007100140401     D  VINSPE                        8
007200101007     D  VINRRN                        8  0
007300020725     D*
007400080923     D*------------------
007500080923     D* DS REPERIMENTO NUMERATORE
007600080923     D*------------------
007700080923     D trul33ds      e ds                  inz
007800080923     D*------------------
007900080923     D* DS ARCHITETTURA
008000080923     D*------------------
008100080923     D kpjba         e ds                  inz
008200080923     D*------------------
008300121113     D  Num7_0         s              7  0
008400081113
008500081113     D*------------------
008600081113     D* LINKING A DEFINIZIONI ESTERNE
008700081113     D*------------------
008800100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
008900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
009000081113
009100990908
009200010201
009300010201
009400080117     C*
009500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009600080117     C
009700080117     C/EXEC SQL
009800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009900080117     C/END-EXEC
010000080117     C
010100000913     C                   reset                   rrnum
010200990921     C                   reset                   esito
010300990921     C                   reset                   wrkesito
010400000613     C*
010500070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
010600000613     C*
010700010202     C* Effettuo la chiamata al CLLE preposto
010800131017     C                   call(e)   'TITVVTC'
010900010202     C                   parm                    parccm
011000010202     C                   parm                    parmbr
011100010202     C                   parm      '2'           paropz
011200070730     C*
011300070730     C* Effettuo lancio TISI95 solo x chiusura
011400070730     C                   CLEAR                   TISI95DS
011500070730     C                   EVAL      I95TLA = 'C'
011600070730     C                   CALL      'TISI95R'
011700070730     C                   PARM                    TISI95DS
011800000616     C*
011900000801     C
012000010201     C                   seton                                        LR
012100990908
012200000801
012300910830     C*--------------------------------------------------------
012400131016     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
012500910830     C*--------------------------------------------------------
012600070530     C     RWFILE        BEGSR
012700990910     C*
012800990914     C                   if        not %open(tivin00r)
012900990908     C                   open      tivin00r
013000990914     C                   endif
013100131016     C                   if        not %open(FIVABWWr)
013200131016     C                   open      FIVABWWr
013300990914     C                   endif
013400070530     C*
013500131016     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013600020305     C                   exsr      prevat
013700010201     C*
013800010202     C                   if        chkcall = '0'
013900010202     C*
014000131016     C                   if        not %open(FIVATWWr)
014100131016     C                   open      FIVATWWr
014200010201     C                   endif
014300080117     C*
014400080117     C                   EXSR      INZVAR
014500080117     C                   EXSR      DEFCAM
014600990910     C*
014700010201     C                   clear                   §CTROKVB          5 0
014800020305     C                   clear                   §CTROKVT          5 0
014900000801     C                   clear                   §CTRMO            5 0
015000000801     C                   clear                   §CTRNO            5 0
015100990910     C*
015200020725     C/EXEC SQL
015300020725     C+ declare C1 cursor for select
015400140401     C+ vindta, vinflg, substr(vindta, 27, 8) as sped, rrn(tivin00r)
015500060223     C+ from tivin00r
015600060223     C+ order by sped
015700020725     C+ for read only
015800020725     C/END-EXEC
015900020725     C
016000020725     C/EXEC SQL
016100020725     C+ open C1
016200020725     C/END-EXEC
016300020725     C
016400020725     C/EXEC SQL
016500070530     C+ Fetch C1 into :INPUT_DS
016600020725     C/END-EXEC
016700020725     C*
016800020725     C                   dow       sqlcod = *zeros
016900990913     C*
017000020725     C                   if        vindta > *blanks
017100000613     C                   add       1             rrnum
017200000801     C*
017300020725     C                   if        vinflg = *blanks
017400020725     C                             or vinflg = '0'
017500020725     C                             or vinflg = '2'
017600000801     C*
017700020725     C                   clear                   x_vinmsg
017800020725     C                   eval      x_vinflg = '1'
017900010305     C*
018000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018100140328     C* (uso NSP/RMN/RMA)
018200140415     C                   EVAL      PiStr=%trim(%subst(vindta:1:34))
018300020305     C                   MOVEL(p)  PiStr         curspe
018400010305     C*
018500071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
018600080923     C                   if        *in50 = *off
018700140415     C* personalizzazione per il cliente
018800140415     C                   exsr      repNSP
018900071003     C                   exsr      impvab
019000111130     C  N31              eval      fivabds_ok = fivabds
019100111130     C  N31              exsr      wrivab
019200140120     C***                exsr      wrivat_a                                     => carico VAT
019300140401     C***                exsr      wrivat_b                                     => carico VAT
019400140415     C* personalizzazione per il cliente
019500140415     C                   exsr      wrivat_e                                     => carico VAT
019600111125     C                   exsr      inzvar
019700071003     C                   else
019800080923     C*
019900071009     C                   if        wDISK = 'D'
020000140120     C***                exsr      repNSP
020100071009     C                   exsr      impvab
020200111130     C  N31              eval      fivabds_ok = fivabds
020300111130     C  N31              exsr      wrivab
020400140120     C***                exsr      wrivat_a                                     => carico VAT
020500140401     C***                exsr      wrivat_b                                     => carico VAT
020600140401     C                   exsr      wrivat_e                                     => carico VAT
020700111125     C                   exsr      inzvar
020800071009     C                   else
020900071009     C*
021000010305     C                   if        depspe = *blanks                             => 1° giro
021100010305     C                   eval      depspe = curspe                              => memorizz. spediz
021200080117     C                   clear                   tivinds
021300140120     C***                exsr      repNSP
021400020305     C                   exsr      impvab
021500111130     C                   z-add     VABNCL        sav_VABNCL
021600111130     C                   z-add     VABPKB        sav_VABPKB
021700111130     C                   z-add     VABVLB        sav_VABVLB
021800140120     C***                exsr      wrivat_a                                     => carico VAT
021900140401     C***                exsr      wrivat_b                                     => carico VAT
022000140401     C   50              exsr      wrivat_e                                     => carico VAT
022100010305     C                   else
022200020725     C                   if        depspe <> curspe                             => rottura di spediz
022300111130     C                   z-add     sav_VABNCL    VABNCL
022400111130     C                   z-add     sav_VABPKB    VABPKB
022500111130     C                   z-add     sav_VABVLB    VABVLB
022600111130     C  N31              eval      fivabds_ok = fivabds
022700111125     C  N31              exsr      wrivab
022800111125     C                   exsr      inzvar
022900111125     C                   eval      depspe = curspe                              => memorizz. spediz
023000080117     C                   clear                   tivinds
023100140120     C***                exsr      repNSP
023200020305     C                   exsr      impvab
023300111130     C                   if        wCntRecSpe = *zeros
023400111130     C                   z-add     VABNCL        sav_VABNCL
023500111130     C                   z-add     VABPKB        sav_VABPKB
023600111130     C                   z-add     VABVLB        sav_VABVLB
023700111130     C                   endif
023800111130     C                   add       1             wCntRecSpe
023900140120     C***                exsr      wrivat_a                                     => carico VAT
024000140401     C***                exsr      wrivat_b                                     => carico VAT
024100140401     C   50              exsr      wrivat_e                                     => carico VAT
024200020305     C                   else                                                   => x stessa spediz
024300111130     C                   exsr      impvab
024400120706     C   51              add       VABNCL        sav_VABNCL
024500111125     C   51              add       VABPKB        sav_VABPKB
024600111125     C   51              add       VABVLB        sav_VABVLB
024700111125     C   52              z-add     VABNCL        sav_VABNCL
024800111125     C   52              z-add     VABPKB        sav_VABPKB
024900111125     C   52              z-add     VABVLB        sav_VABVLB
025000111125     C   53              add       VABNCL        sav_VABNCL
025100111125     C   53              z-add     VABPKB        sav_VABPKB
025200111125     C   53              z-add     VABVLB        sav_VABVLB
025300131016     C   54              Z-ADD     VABNCL        sav_VABNCL
025400131016     C   54              ADD       VABPKB        sav_VABPKB
025500131016     C   54              ADD       VABVLB        sav_VABVLB
025600111125     C*
025700140120     C***                exsr      wrivat_a                                     => carico VAT
025800140401     C***                exsr      wrivat_b                                     => carico VAT
025900140401     C   50              exsr      wrivat_e                                     => carico VAT
026000010305     C                   endif
026100010305     C                   endif
026200010305     C                   endif
026300071003     C                   endif
026400071009     C                   endif
026500000905     C*
026600000905     C                   else
026700020725     C                   eval      x_vinflg = '1'
026800000905     C                   endif
026900000905     C*
027000020725     C     VINRRN        chain     tivin000
027100020725     C                   if        %found(tivin00r)
027200020725     C                   eval      y_vinflg = x_vinflg
027300020725     C                   eval      y_vinmsg = x_vinmsg
027400020725     C                   update    tivin000
027500020725     C                   endif
027600020725     C*
027700020725     C/EXEC SQL
027800070530     C+ Fetch C1 into :INPUT_DS
027900020725     C/END-EXEC
028000020725     C*
028100020725     C                   enddo
028200020725     C*
028300020725     C/EXEC SQL
028400020725     C+ close C1
028500020725     C/END-EXEC
028600000905     C*
028700020305     C* Scarico i VAB rimasti "in sospeso"
028800071009     C                   if        wDISK = 'C'
028900140307     C                   z-add     sav_VABNCL    VABNCL
029000140307     C                   z-add     sav_VABPKB    VABPKB
029100140307     C                   z-add     sav_VABVLB    VABVLB
029200111130     C  N31              eval      fivabds_ok = fivabds
029300111130     C  N31              exsr      wrivab
029400071009     C                   endif
029500010202     C*
029600010202     C                   endif
029700990910
029800990910     C* Se non ci sono record con errori ...
029900000710     C                   if        §ctrno = 0
030000990910     C* ... restituisco esito OK.
030100990921     C                   eval      wrkesito = '0'
030200990910     C                   else
030300010201     C                   if        §ctrokvb > 0
030400990921     C                   eval      wrkesito = '1'
030500000710     C                   else
030600000710     C                   eval      wrkesito = '2'
030700990910     C                   endif
030800000710     C                   endif
030900990910     C*
031000990914     C                   if        %open(tivin00r)
031100990908     C                   close     tivin00r
031200990914     C                   endif
031300131016     C                   if        %open(FIVABWWr)
031400131016     C                   close     FIVABWWr
031500120706     C                   endif
031600131016     C                   if        %open(FIVATWWr)
031700131016     C                   close     FIVATWWr
031800010201     C                   endif
031900990910     C*
032000111031     C                   if        §ctrokvb > 0 or
032100111031     C                             §ctrokvt > 0
032200111031     C                             and wPOI <> *zeros
032300010202     C                   exsr      invio
032400990920     C                   endif
032500990920     C*
032600910830     C                   ENDSR
032700000613     C***
032800010305
032900010305     C*----------------------------------------------------*
033000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033100010305     C*----------------------------------------------------*
033200020305     C     WRIVAB        BEGSR
033300140429     C*
033400140429     C* Considerazioni sul contrassegno / tipo bolla
033500140429     C                   if        ok_VABCAS > *zeros
033600140429     C                   select
033700140429     C                   when      ok_VABCBO ='1'
033800140429     C                   eval      ok_VABCBO = '4'
033900140429     C                   when      ok_VABCBO ='2'
034000140429     C                   eval      ok_VABCBO = '6'
034100140429     C                   endsl
034200140429     C                   endif
034300070730     C*
034400071003     C* Considerazioni finali
034500111125     C                   if        ok_VABRMA = *blanks
034600111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
034700071003     C                   endif
034800071003     C*
034900131016     C                   write     FIVAB000                                     => scarico il VAB
035000010305     C*
035100010305     C                   ENDSR
035200990920
035300000801     C*----------------------------------------------------*
035400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035500000801     C*----------------------------------------------------*
035600010201     C     INZVAR        BEGSR
035700000801     C*
035800010201     C                   Z-ADD     *zeros        Num5_0
035900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036000020725     C                   MOVEL     '0'           FlgCAS            1
036100111125     C*
036200111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
036300111130     C*
036400111125     C                   CLEAR                   FIVABDS
036500111130     C                   CLEAR                   FIVABDS_OK
036600111130     C                   CLEAR                   FIVABDS_SAV
036700131016     C                   CLEAR                   FIVAB000
036800131016     C                   CLEAR                   FIVAT000
036900000801     C*
037000000801     C                   ENDSR
037100000801     C*----------------------------------------------------*
037200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037300000801     C*----------------------------------------------------*
037400020904     C     DEFCAM        BEGSR
037500080923     C*
037600111125     C                   SETOFF                                       50
037700111125     C                   SETOFF                                       515253
037800131016     C                   SETOFF                                       54
037900070730     C* Imposto i valori di default...
038000140401     C                   EVAL      VABCCM = 2552902
038100140401     C                   EVAL      VATCCM = 2552902
038200140401     C                   EVAL      VABLNP = 255
038300140401     C                   EVAL      VATLNP = 255
038400140328     C                   EVAL      VABCTR = 000
038500131016     C                   EVAL      VABCBO = '1'
038600070222     C* ... e poi verifico se sono stati passati come parametri
038700070222     C                   IF        vlrppt > *blanks
038800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039000070222     C                   EXSR      CHKNUM
039100070222     C                   IF        PiInt=*on
039200070222     C                   Z-ADD     PiVal         VABCCM
039300070222     C                   Z-ADD     PiVal         VATCCM
039400070222     C                   ENDIF
039500070222     C                   ENDIF
039600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039800070222     C                   EXSR      CHKNUM
039900070222     C                   IF        PiInt=*on
040000070222     C                   Z-ADD     PiVal         VABLNP
040100070222     C                   Z-ADD     PiVal         VATLNP
040200070222     C                   ENDIF
040300070222     C                   ENDIF
040400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040600070222     C                   EXSR      CHKNUM
040700070222     C                   IF        PiInt=*on
040800070222     C                   Z-ADD     PiVal         VABCTR
040900070222     C                   ENDIF
041000070928     C                   ENDIF
041100071009     C                   MOVEL     *blanks       wDISK             1
041200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
041300071009     C                   IF        wDISK <> *blanks
041400070928     C                   SETON                                        50
041500070222     C                   ENDIF
041600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
041700111125     C                   SETON                                        41
041800080923     C                   ENDIF
041900070222     C                   ENDIF
042000120706     C*
042100120706     C* valido per nr.colli/peso/volume
042200120706     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
042300120706     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
042400120706     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
042500131016     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
042600120706     C                   SELECT
042700120903     C                   WHEN      %subst(vlrppt:18:1) = 'A'
042800120706     C                   SETON                                        51
042900120903     C                   WHEN      %subst(vlrppt:18:1) = *blanks
043000120706     C                   SETON                                        52
043100120903     C                   WHEN      %subst(vlrppt:18:1) = 'C'
043200120706     C                   SETON                                        53
043300131016     C                   WHEN      %subst(vlrppt:18:1) = 'P'
043400131016     C                   SETON                                        54
043500120706     C                   ENDSL
043600120706     C*
043700071009     C   50              EVAL      VABCTM = '7Q'
043800000801     C*
043900000801     C                   ENDSR
044000131016     C*----------------------------------------------------*
044100131016     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044200131016     C*----------------------------------------------------*
044300131016     C     REPNSP        BEGSR
044400131016     C*
044500131016     C                   EXSR      INZVAR
044600131016     C                   EXSR      DEFCAM
044700131016     C*
044800131016     C                   SETON                                        60
044900131016     C*
045000131016     C* NSP => Stacco un numeratore da AZNUM
045100131016     C                   clear                   TRUL33DS
045200131016     C                   eval      I33OPE = *zeros
045300131016     C                   eval      I33CNU = 302
045400131016     C                   eval      I33NUM = 1
045500131016     C                   movel     TRUL33DS      KPJBU
045600131016     C                   call      'TRUL33R'
045700131016     C                   parm                    KPJBA
045800131016     C                   movel     KPJBU         TRUL33DS
045900131016     C                   if        O33ERR = *zeros
046000131016     C                   z-add     O33NRF        VABNSP
046100131016     C                   z-add     O33NRF        VATNSP
046200131016     C                   z-add     O33NRF        VABRMN
046300131016     C                   else
046400131016     C                   SETON                                        31
046500131016     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046600140415     C                             + ' ' + 'VABNSP VATNSP'
046700131016     C                   endif
046800131016     C*
046900131016     C                   ENDSR
047000000801     C*----------------------------------------------------*
047100131016     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
047200000801     C*----------------------------------------------------*
047300010201     C     IMPVAB        BEGSR
047400070530     C*
047500140401     C                   SETOFF                                       303132
047600070928     C*
047700070928     C                   EXSR      DEFCAM
047800070928     C*
047900111031     C                   IF        vlrpoi <> 999
048000070928     C                   MOVE(P)   vlrpoi        VABFGS
048100070928     C                   MOVE(P)   vlrpoi        VATFGS
048200111031     C                   Z-ADD     vlrpoi        wPOI              3 0
048300111031     C                   ENDIF
048400070928     C*
048500140401     C                   eval      fnvabaar = vindta
048600140401     C*
048700140401     C* valorizzo FIVABDS da FNVABAAR
048800140401     C                   EXSR      ValVAB
048900070730     C*
049000070730     C* Considerazioni finali su CBO/CAS
049100121113     C                   IF        FlgCAS <> '0'
049200070730     C                   IF        VABCBO = '1'
049300070730     C                   EVAL      VABCBO = '4'
049400070730     C                   ENDIF
049500070730     C                   IF        VABCBO = '2'
049600070730     C                   EVAL      VABCBO = '6'
049700070730     C                   ENDIF
049800070730     C                   ENDIF
049900020305     C*
050000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
050100011113     C                   EXSR      CHKIMPDIV
050200010202     C*
050300000801     C* Ebbene...
050400000801     C                   ADD       1             §CTRMO
050500070530     C                   IF        *in31 <> *zeros OR
050600070530     C                             *in32 <> *zeros
050700000801     C                   ADD       1             §CTRNO
050800020725     C                   EVAL      x_vinflg = '2'
050900000801     C                   ELSE
051000010201     C                   ADD       1             §CTROKVB
051100000801     C                   ENDIF
051200000801     C*
051300000801     C                   ENDSR
051400131016     C*----------------------------------------------------*
051500140401     C*  Valorizzazione FIVABDS da FNVABAAR
051600131016     C*----------------------------------------------------*
051700140401     C     ValVAB        BEGSR
051800131016     C*
051900140401     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
052000140401     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
052100140401     C*    OPPURE TUTTI BLANKS
052200140401     C*
052300140401     C                   TESTN                   VHBLNP               30
052400140401     C  N30              TESTN                   VHBLNP                 30
052500140401     C  N30              TESTN                   VHBLNP                   30
052600140401     C                   MOVE      VHBLNP        W001A             1
052700140401     C     *IN30         IFEQ      *OFF
052800140401     C     W001A         ORLT      '0'
052900140401     C                   SETON                                        31
053000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
053100140401     C                             + ' ' + 'VHBLNP'
053200140401     C                   ELSE
053300140401     C                   MOVE      VHBLNP        VABLNP
053400140401     C     VABLNP        IFLE      *ZEROS
053500140401     C                   SETON                                        31
053600140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
053700140401     C                             + ' ' + 'VHBLNP'
053800140401     C                   ENDIF
053900140401     C                   ENDIF
054000140401     C*
054100140401     C                   TESTN                   VHBCCM               30
054200140401     C  N30              TESTN                   VHBCCM                 30
054300140401     C  N30              TESTN                   VHBCCM                   30
054400140401     C                   MOVE      VHBCCM        W001A             1
054500140401     C     *IN30         IFEQ      *OFF
054600140401     C     W001A         ORLT      '0'
054700140401     C                   SETON                                        31
054800140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
054900140401     C                             + ' ' + 'VHBCCM'
055000140401     C                   ELSE
055100140401     C                   MOVE      VHBCCM        VABCCM
055200140401     C     VABCCM        IFLE      *ZEROS
055300140401     C                   SETON                                        31
055400140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
055500140401     C                             + ' ' + 'VHBCCM'
055600140401     C                   ENDIF
055700140401     C                   ENDIF
055800140401     C*
055900140401     C                   TESTN                   VHBAAS               30
056000140401     C  N30              TESTN                   VHBAAS                 30
056100140401     C  N30              TESTN                   VHBAAS                   30
056200140401     C                   MOVE      VHBAAS        W001A             1
056300140401     C     *IN30         IFEQ      *OFF
056400140401     C     W001A         ORLT      '0'
056500140401     C     W001A         ANDNE     *BLANK
056600140401     C                   SETON                                        32
056700140401     C                   MOVEL     datcor        VABAAS
056800140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
056900140401     C                             + ' ' + 'VHBAAS'
057000140401     C                   ELSE
057100140401     C                   MOVE      VHBAAS        VABAAS
057200140401     C                   ENDIF
057300140401     C*
057400140401     C                   TESTN                   VHBMGS               30
057500140401     C  N30              TESTN                   VHBMGS                 30
057600140401     C  N30              TESTN                   VHBMGS                   30
057700140401     C                   MOVE      VHBMGS        W001A             1
057800140401     C     *IN30         IFEQ      *OFF
057900140401     C     W001A         ORLT      '0'
058000140401     C     W001A         ANDNE     *BLANK
058100140401     C                   SETON                                        32
058200140401     C                   MOVE      '0101'        VABMGS
058300140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
058400140401     C                             + ' ' + 'VHBMGS'
058500140401     C                   ELSE
058600140401     C                   MOVE      VHBMGS        VABMGS
058700140401     C                   ENDIF
058800140401     C*
058900140401     C                   TESTN                   VHBNRS               30
059000140401     C  N30              TESTN                   VHBNRS                 30
059100140401     C  N30              TESTN                   VHBNRS                   30
059200140401     C                   MOVE      VHBNRS        W001A             1
059300140401     C     *IN30         IFEQ      *OFF
059400140401     C     W001A         ORLT      '0'
059500140401     C     W001A         ANDNE     *BLANK
059600140401     C                   SETON                                        32
059700140401     C                   MOVE      *ZERO         VABNRS
059800140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
059900140401     C                             + ' ' + 'VHBNRS'
060000140401     C                   ELSE
060100140401     C                   MOVE      VHBNRS        VABNRS
060200140401     C                   ENDIF
060300140415     C***
060400140415     C***                TESTN                   VHBNSP               30
060500140415     C**N30              TESTN                   VHBNSP                 30
060600140415     C**N30              TESTN                   VHBNSP                   30
060700140415     C***                MOVE      VHBNSP        W001A             1
060800140415     C***  *IN30         IFEQ      *OFF
060900140415     C***  W001A         ORLT      '0'
061000140415     C***  W001A         ANDNE     *BLANK
061100140415     C***                SETON                                        32
061200140415     C***                MOVE      *ZERO         VABNSP
061300140415     C***                eval      x_vinmsg = %trimr(x_vinmsg)
061400140415     C***                          + ' ' + 'VHBNSP'
061500140415     C***                ELSE
061600140415     C***                MOVE      VHBNSP        VABNSP
061700140415     C***                ENDIF
061800140401     C*
061900140401     C                   TESTN                   VHBLNA               30
062000140401     C  N30              TESTN                   VHBLNA                 30
062100140401     C  N30              TESTN                   VHBLNA                   30
062200140401     C                   MOVE      VHBLNA        W001A             1
062300140401     C     *IN30         IFEQ      *OFF
062400140401     C     W001A         ORLT      '0'
062500140401     C     W001A         ANDNE     *BLANK
062600140401     C                   SETON                                        32
062700140401     C                   MOVE      *ZERO         VABLNA
062800140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
062900140401     C                             + ' ' + 'VHBLNA'
063000140401     C                   ELSE
063100140401     C                   MOVE      VHBLNA        VABLNA
063200140401     C                   ENDIF
063300140401     C*
063400140401     C                   TESTN                   VHBCTR               30
063500140401     C  N30              TESTN                   VHBCTR                 30
063600140401     C  N30              TESTN                   VHBCTR                   30
063700140401     C                   MOVE      VHBCTR        W001A             1
063800140401     C     *IN30         IFEQ      *OFF
063900140401     C     W001A         ORLT      '0'
064000140401     C     W001A         ANDNE     *BLANK
064100140401     C                   SETON                                        32
064200140401     C                   MOVE      *ZERO         VABCTR
064300140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
064400140401     C                             + ' ' + 'VHBCTR'
064500140401     C                   ELSE
064600140401     C                   MOVE      VHBCTR        VABCTR
064700140401     C                   ENDIF
064800140401     C*
064900140401     C                   MOVE      *ZERO         VABIAS
065000140401     C                   MOVE      VHBIAS        WASC15
065100140401     C                   TESTN                   WI15                 30
065200140401     C  N30              TESTN                   WI15                   30
065300140401     C  N30              TESTN                   WI15                     30
065400140401     C                   MOVE      WI15          W001A             1
065500140401     C     *IN30         IFEQ      *OFF
065600140401     C     W001A         ORLT      '0'
065700140401     C     W001A         ANDNE     *BLANK
065800140401     C                   SETON                                        32
065900140401     C                   MOVE      *ZERO         VABIAS
066000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
066100140401     C                             + ' ' + 'VHBIAS'
066200140401     C                   ELSE
066300140401     C                   MOVEL     WI15          VABIAS
066400140401     C                   TESTN                   WD15                 30
066500140401     C  N30              TESTN                   WD15                   30
066600140401     C  N30              TESTN                   WD15                     30
066700140401     C                   MOVE      WD15          W001A             1
066800140401     C     *IN30         IFEQ      *OFF
066900140401     C     W001A         ORLT      '0'
067000140401     C     W001A         ANDNE     *BLANK
067100140401     C                   SETON                                        32
067200140401     C                   MOVE      *ZERO         VABIAS
067300140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
067400140401     C                             + ' ' + 'VHBIAS'
067500140401     C                   ELSE
067600140401     C                   MOVE      WD15          VABIAS
067700140401     C                   ENDIF
067800140401     C                   ENDIF
067900140401     C*
068000140401     C                   TESTN                   VHBNCL               30
068100140401     C  N30              TESTN                   VHBNCL                 30
068200140401     C  N30              TESTN                   VHBNCL                   30
068300140401     C                   MOVE      VHBNCL        W001A             1
068400140401     C     *IN30         IFEQ      *OFF
068500140401     C     W001A         ORLT      '0'
068600140401     C                   SETON                                        32
068700140401     C                   MOVE      *ZERO         VABNCL
068800140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
068900140401     C                             + ' ' + 'VHBNCL'
069000140401     C                   ELSE
069100140401     C                   MOVE      VHBNCL        VABNCL
069200140401     C                   ENDIF
069300140401     C*
069400140401     C                   MOVE      *ZERO         VABPKB
069500140401     C                   MOVE      VHBPKB        WASC09
069600140401     C                   IF        WI09 = *blanks
069700140401     C                   EVAL      WI09 = *zeros
069800140401     C                   ENDIF
069900140401     C                   TESTN                   WI09                 30
070000140401     C  N30              TESTN                   WI09                   30
070100140401     C  N30              TESTN                   WI09                     30
070200140401     C                   MOVE      WI09          W001A             1
070300140401     C     *IN30         IFEQ      *OFF
070400140401     C     W001A         ORLT      '0'
070500140401     C                   SETON                                        32
070600140401     C                   MOVE      *ZERO         VABPKB
070700140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
070800140401     C                             + ' ' + 'VHBPKB'
070900140401     C                   ELSE
071000140401     C                   MOVEL     WI09          VABPKB
071100140401     C                   IF        WD09 = *blanks
071200140401     C                   EVAL      WD09 = *zeros
071300140401     C                   ENDIF
071400140401     C                   TESTN                   WD09                 30
071500140401     C  N30              TESTN                   WD09                     30
071600140401     C                   MOVE      WD09          W001A             1
071700140401     C     *IN30         IFEQ      *OFF
071800140401     C     W001A         ORLT      '0'
071900140401     C     W001A         ANDNE     *BLANK
072000140401     C                   SETON                                        32
072100140401     C                   MOVE      *ZERO         VABPKB
072200140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
072300140401     C                             + ' ' + 'VHBPKB'
072400140401     C                   ELSE
072500140401     C                   MOVE      WD09          VABPKB
072600140401     C                   ENDIF
072700140401     C                   ENDIF
072800140401     C*
072900140401     C                   MOVE      *ZERO         VABVLB
073000140401     C                   MOVE      VHBVLB        WASC07
073100140401     C                   IF        WI07 = *blanks
073200140401     C                   EVAL      WI07 = *zeros
073300140401     C                   ENDIF
073400140401     C                   TESTN                   WI07                 30
073500140401     C  N30              TESTN                   WI07                   30
073600140401     C  N30              TESTN                   WI07                     30
073700140401     C                   MOVE      WI07          W001A             1
073800140401     C     *IN30         IFEQ      *OFF
073900140401     C     W001A         ORLT      '0'
074000140401     C     W001A         ANDNE     *BLANK
074100140401     C                   SETON                                        32
074200140401     C                   MOVE      *ZERO         VABVLB
074300140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
074400140401     C                             + ' ' + 'VHBVLB'
074500140401     C                   ELSE
074600140401     C                   MOVEL     WI07          VABVLB
074700140401     C                   IF        WD07 = *blanks
074800140401     C                   EVAL      WD07 = *zeros
074900140401     C                   ENDIF
075000140401     C                   TESTN                   WD07                 30
075100140401     C  N30              TESTN                   WD07                   30
075200140401     C  N30              TESTN                   WD07                     30
075300140401     C                   MOVE      WD07          W001A             1
075400140401     C     *IN30         IFEQ      *OFF
075500140401     C     W001A         ORLT      '0'
075600140401     C     W001A         ANDNE     *BLANK
075700140401     C                   SETON                                        32
075800140401     C                   MOVE      *ZERO         VABVLB
075900140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
076000140401     C                             + ' ' + 'VHBVLB'
076100140401     C                   ELSE
076200140401     C                   MOVE      WD07          VABVLB
076300140401     C                   ENDIF
076400140401     C                   ENDIF
076500140401     C*
076600140401     C                   MOVE      *ZERO         VABQFT
076700140401     C                   MOVE      VHBQFT        WASC15
076800140401     C                   IF        WI15 = *blanks
076900140401     C                   EVAL      WI15 = *zeros
077000140401     C                   ENDIF
077100140401     C                   TESTN                   WI15                 30
077200140401     C  N30              TESTN                   WI15                   30
077300140401     C  N30              TESTN                   WI15                     30
077400140401     C                   MOVE      WI15          W001A             1
077500140401     C     *IN30         IFEQ      *OFF
077600140401     C     W001A         ORLT      '0'
077700140401     C     W001A         ANDNE     *BLANK
077800140401     C                   SETON                                        32
077900140401     C                   MOVE      *ZERO         VABQFT
078000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
078100140401     C                             + ' ' + 'VHBQFT'
078200140401     C                   ELSE
078300140401     C                   MOVEL     WI15          VABQFT
078400140401     C                   IF        WD15 = *blanks
078500140401     C                   EVAL      WD15 = *zeros
078600140401     C                   ENDIF
078700140401     C                   TESTN                   WD15                 30
078800140401     C  N30              TESTN                   WD15                   30
078900140401     C  N30              TESTN                   WD15                     30
079000140401     C                   MOVE      WD15          W001A             1
079100140401     C     *IN30         IFEQ      *OFF
079200140401     C     W001A         ORLT      '0'
079300140401     C     W001A         ANDNE     *BLANK
079400140401     C                   SETON                                        32
079500140401     C                   MOVE      *ZERO         VABQFT
079600140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
079700140401     C                             + ' ' + 'VHBQFT'
079800140401     C                   ELSE
079900140401     C                   MOVE      WD15          VABQFT
080000140401     C                   ENDIF
080100140401     C                   ENDIF
080200140401     C*
080300140401     C                   MOVE      *ZERO         VABCAS
080400140401     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
080500140401     C                   EVAL      wVHBCAS = VHBCAS
080600140424     C                   IF        %subst(wVHBCAS:12:1) = ','
080700140424     C                   EVAL      %subst(wVHBCAS:12:1) = '.'
080800140401     C                   ENDIF
080900140401     C*
081000140401     C                   MOVEL(P)  wVHBCAS       PiStr
081100140401     C                   EXSR      CHKNUM
081200140401     C                   IF        PiNum=*on
081300140401     C                   Z-ADD     PiVal         VABCAS
081400140424     C***                EVAL      VABVCA ='EUR'
081500140401     C                   ELSE
081600140401     C                   SETON                                        32
081700140401     C                   Z-ADD     *zeros        VABCAS
081800140401     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
081900140401     C                             + ' ' + 'VHBCAS'
082000140401     C                   ENDIF
082100140401     C*
082200140401     C                   TESTN                   VHBRMN               30
082300140401     C  N30              TESTN                   VHBRMN                 30
082400140401     C  N30              TESTN                   VHBRMN                   30
082500140401     C                   MOVE      VHBRMN        W001A             1
082600140401     C     *IN30         IFEQ      *OFF
082700140401     C     W001A         ORLT      '0'
082800140401     C                   SETON                                        32
082900140401     C                   MOVE      *ZERO         VABRMN
083000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
083100140401     C                             + ' ' + 'VHBRMN'
083200140401     C                   ELSE
083300140401     C                   MOVE      VHBRMN        VABRMN
083400140401     C                   ENDIF
083500140401     C*
083600140401     C                   TESTN                   VHBNCD               30
083700140401     C  N30              TESTN                   VHBNCD                 30
083800140401     C  N30              TESTN                   VHBNCD                   30
083900140401     C                   MOVE      VHBNCD        W001A             1
084000140401     C     *IN30         IFEQ      *OFF
084100140401     C     W001A         ORLT      '0'
084200140401     C     W001A         ANDNE     *BLANK
084300140401     C                   SETON                                        32
084400140401     C                   MOVE      *ZERO         VABNCD
084500140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
084600140401     C                             + ' ' + 'VHBNCD'
084700140401     C                   ELSE
084800140401     C                   MOVE      VHBNCD        VABNCD
084900140401     C                   ENDIF
085000140401     C*
085100140401     C                   TESTN                   VHBNCA               30
085200140401     C  N30              TESTN                   VHBNCA                 30
085300140401     C  N30              TESTN                   VHBNCA                   30
085400140401     C                   MOVE      VHBNCA        W001A             1
085500140401     C     *IN30         IFEQ      *OFF
085600140401     C     W001A         ORLT      '0'
085700140401     C     W001A         ANDNE     *BLANK
085800140401     C                   SETON                                        32
085900140401     C                   MOVE      *ZERO         VABNCA
086000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
086100140401     C                             + ' ' + 'VHBNCA'
086200140401     C                   ELSE
086300140401     C                   MOVE      VHBNCA        VABNCA
086400140401     C                   ENDIF
086500140401     C*
086600140401     C                   TESTN                   VHBZNC               30
086700140401     C  N30              TESTN                   VHBZNC                 30
086800140401     C  N30              TESTN                   VHBZNC                   30
086900140401     C                   MOVE      VHBZNC        W001A             1
087000140401     C     *IN30         IFEQ      *OFF
087100140401     C     W001A         ORLT      '0'
087200140401     C     W001A         ANDNE     *BLANK
087300140401     C                   SETON                                        32
087400140401     C                   MOVE      *ZERO         VABZNC
087500140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
087600140401     C                             + ' ' + 'VHBZNC'
087700140401     C                   ELSE
087800140401     C                   MOVE      VHBZNC        VABZNC
087900140401     C                   ENDIF
088000140401     C*
088100140401     C                   TESTN                   VHBDCR               30
088200140401     C  N30              TESTN                   VHBDCR                 30
088300140401     C  N30              TESTN                   VHBDCR                   30
088400140401     C                   MOVE      VHBDCR        W001A             1
088500140401     C     *IN30         IFEQ      *OFF
088600140401     C     W001A         ORLT      '0'
088700140401     C     W001A         ANDNE     *BLANK
088800140401     C                   SETON                                        32
088900140401     C                   MOVE      *ZERO         VABDCR
089000140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
089100140401     C                             + ' ' + 'VHBDCR'
089200140401     C                   ELSE
089300140401     C                   MOVE      VHBDCR        VABDCR
089400140401     C                   ENDIF
089500140401     C*
089600140401     C                   TESTN                   VHBHCR               30
089700140401     C  N30              TESTN                   VHBHCR                 30
089800140401     C  N30              TESTN                   VHBHCR                   30
089900140401     C                   MOVE      VHBHCR        W001A             1
090000140401     C     *IN30         IFEQ      *OFF
090100140401     C     W001A         ORLT      '0'
090200140401     C     W001A         ANDNE     *BLANK
090300140401     C                   SETON                                        32
090400140401     C                   MOVE      *ZERO         VABHCR
090500140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
090600140401     C                             + ' ' + 'VHBHCR'
090700140401     C                   ELSE
090800140401     C                   MOVE      VHBHCR        VABHCR
090900140401     C                   ENDIF
091000140401     C*
091100140401     C                   MOVE      *ZERO         VABVMD
091200140401     C                   MOVE      VHBVMD        WASC15
091300140401     C                   IF        WI15 = *blanks
091400140401     C                   EVAL      WI15 = *zeros
091500140401     C                   ENDIF
091600140401     C                   TESTN                   WI15                 30
091700140401     C  N30              TESTN                   WI15                   30
091800140401     C  N30              TESTN                   WI15                     30
091900140401     C                   MOVE      WI15          W001A             1
092000140401     C     *IN30         IFEQ      *OFF
092100140401     C     W001A         ORLT      '0'
092200140401     C     W001A         ANDNE     *BLANK
092300140401     C                   SETON                                        32
092400140401     C                   MOVE      *ZERO         VABVMD
092500140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
092600140401     C                             + ' ' + 'VHBVMD'
092700140401     C                   ELSE
092800140401     C                   MOVEL     WI15          VABVMD
092900140401     C                   IF        WD15 = *blanks
093000140401     C                   EVAL      WD15 = *zeros
093100140401     C                   ENDIF
093200140401     C                   TESTN                   WD15                 30
093300140401     C  N30              TESTN                   WD15                   30
093400140401     C  N30              TESTN                   WD15                     30
093500140401     C                   MOVE      WD15          W001A             1
093600140401     C     *IN30         IFEQ      *OFF
093700140401     C     W001A         ORLT      '0'
093800140401     C     W001A         ANDNE     *BLANK
093900140401     C                   SETON                                        32
094000140401     C                   MOVE      *ZERO         VABVMD
094100140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
094200140401     C                             + ' ' + 'VHBVMD'
094300140401     C                   ELSE
094400140401     C                   MOVE      WD15          VABVMD
094500140401     C                   ENDIF
094600140401     C                   ENDIF
094700140401     C*
094800140401     C                   TESTN                   VHBANT               30
094900140401     C  N30              TESTN                   VHBANT                 30
095000140401     C  N30              TESTN                   VHBANT                   30
095100140401     C                   MOVE      VHBANT        W001A             1
095200140401     C     *IN30         IFEQ      *OFF
095300140401     C     W001A         ORLT      '0'
095400140401     C     W001A         ANDNE     *BLANK
095500140401     C                   SETON                                        32
095600140401     C                   MOVE      *ZERO         VABANT
095700140401     C                   eval      x_vinmsg = %trimr(x_vinmsg)
095800140401     C                             + ' ' + 'VHBANT'
095900140401     C                   ELSE
096000140401     C                   MOVE      VHBANT        VABANT
096100140401     C                   ENDIF
096200140401     C*
096300140401     C* CAMPI ALFABETICI:
096400140401     C                   MOVE      VHBATB        VABATB
096500140401     C                   MOVE      VHBCBO        VABCBO
096600140401     C                   MOVE      VHBRSD        VABRSD
096700140401     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
096800140401     C     '@':'A'       XLATE     VABRSD        VABRSD
096900140401     C* ==
097000140401     C                   MOVE      VHBRD2        VABRD2
097100140401     C                   MOVE      VHBIND        VABIND
097200140401     C                   MOVE      VHBCAD        VABCAD
097300140401     C                   MOVE      VHBLOD        VABLOD
097400140401     C                   MOVE      VHBPRD        VABPRD
097500140401     C                   MOVE      VHBNZD        VABNZD
097600140401     C                   MOVE      VHBGC1        VABGC1
097700140401     C                   MOVE      VHBGC2        VABGC2
097800140401     C                   MOVE      VHBTSP        VABTSP
097900140401     C                   MOVE      VHBVAS        VABVAS
098000140401     C                   MOVE      VHBNAS        VABNAS
098100140401     C                   MOVE      VHBTIC        VABTIC
098200140401     C                   MOVE      VHBVCA        VABVCA
098300140401     C                   MOVE      VHBGCA        VABGCA
098400140401     C                   MOVE      VHBRMA        VABRMA
098500140401     C                   MOVE      VHBXCO        VABXCO
098600140401     C                   MOVE      VHBNOT        VABNOT
098700140401     C                   MOVE      VHBNT2        VABNT2
098800140401     C                   MOVE      VHBCTM        VABCTM
098900140401     C                   MOVE      VHBFFD        VABFFD
099000140401     C                   MOVE      VHBTCR        VABTCR
099100140401     C                   MOVE      VHBCTS        VABCTS
099200140401     C                   MOVE      VHBFTM        VABFTM
099300140401     C                   MOVE      VHBVAD        VABVAD
099400140401     C                   MOVE      VHBGMA        VABGMA
099500140401     C                   MOVE      VHBGGA        VABGGA
099600140401     C                   MOVE      VHBGVA        VABGVA
099700140401     C                   MOVE      VHBTC1        VABTC1
099800140401     C                   MOVE      VHBTC2        VABTC2
099900140401     C                   MOVE      VHBSCL        VABSCL
100000140401     C                   MOVE      VHBRMO        VABRMO
100100140401     C                   MOVE      VHBCMO        VABCMO
100200140401     C                   MOVE      VHBNMO        VABNMO
100300140401     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
100400140401     C* NELLA NAZIONE MITTENTE ORIGINALE
100500140401     C                   IF        %trim(VHBNMO) < 'A'
100600140401     C                   MOVE      *BLANKS       VABNMO
100700140401     C                   ENDIF
100800140401     C*
100900140401     C                   ENDSR
101000140401     C*----------------------------------------------------*
101100140401     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
101200140401     C*----------------------------------------------------*
101300140401     C     WRIVAT_A      BEGSR
101400140401     C*
101500131016     C                   eval      vatAAS = vabAAS
101600131016     C                   eval      vatLNP = vabLNP
101700131016     C                   eval      vatNRS = vabNRS
101800131016     C                   eval      vatNSP = vabNSP
101900131016     C*
102000131016     C* Valorizzo il buffer di scrittura del FIVAT
102100131016     C* - TIPO RECORD "A"
102200131016     C                   if        %trim(%subst(vindta:098:15)) <> *blanks
102300131016     C                   eval      VATTRC = 'A'
102400131016     C                   eval      VATNOT = %trim(%subst(vindta:098:15))
102500131016     C                   write     FIVAT000
102600131016     C                   endif
102700131016     C*
102800131016     C                   ENDSR
102900131016     C*----------------------------------------------------*
103000131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
103100131016     C*----------------------------------------------------*
103200131016     C     WRIVAT_B      BEGSR
103300131016     C*
103400131016     C                   eval      vatAAS = vabAAS
103500131016     C                   eval      vatLNP = vabLNP
103600131016     C                   eval      vatNRS = vabNRS
103700131016     C                   eval      vatNSP = vabNSP
103800131016     C*
103900131016     C* Valorizzo il buffer di scrittura del FIVAT
104000131016     C* - TIPO RECORD "B"
104100140328     C                   IF        %trim(%subst(vindta:407:20)) <> *blank
104200131016     C                   eval      VATTRC = 'B'
104300140328     C                   eval      VATNOT = %trim(%subst(vindta:407:20))
104400131016     C                   write     FIVAT000
104500131016     C                   endif
104600131016     C*
104700131016     C                   ENDSR
104800131016     C*----------------------------------------------------*
104900131016     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
105000131016     C*----------------------------------------------------*
105100131016     C     WRIVAT_E      BEGSR
105200131016     C*
105300131016     C                   eval      vatAAS = vabAAS
105400131016     C                   eval      vatLNP = vabLNP
105500131016     C                   eval      vatNRS = vabNRS
105600131016     C                   eval      vatNSP = vabNSP
105700131016     C*
105800131016     C* Valorizzo il buffer di scrittura del FIVAT
105900131016     C* - TIPO RECORD "E"
106000140401     C                   if        %trim(%subst(vindta:514:12)) <> *blanks
106100131016     C                   eval      VATTRC = 'E'
106200140401     C*
106300140401     C* Sviluppati i "CHI SONO" in relazione al numero colli
106400140401     C*
106500140401     C     1             DO        VABNCL        wNumCollo         3 0
106600140401     C                   EVAL      VATNOT = %editc(
106700140418     C                                %dec(%trimr(%subst(vindta:514:12)) :12:0)
106800140418     C                                + wNumCollo - 1 :'X')
106900140418     C*
107000140401     C* recupero il check digit
107100140401     C                   clear                   TRUL28DSX
107200140401     C                   eval      I284STD = 'E10'
107300140401     C                   eval      I284COD = VATNOT
107400140401     C                   call      'TRUL28R4'
107500140401     C                   parm                    TRUL28DSX
107600140401     C* Se tutto ok scrivo il "chi sono" corrente col check digit
107700140401     C                   IF        O284err = *blanks
107800140418     C                   EVAL      VATNOT = %subst(vindta:512:2)+
107900140418     C                                      %trim(VATNOT) + %trim(O284CkD)
108000140401     C                   write     FIVAT000
108100140401     C                   add       1             §CTROKVT
108200140401     C                   ELSE
108300140401     C                   SETON                                        31
108400140401     C                   EVAL      VATNOT = *blank
108500140401     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
108600140401     C                             + ' ' + 'VATNOT'
108700140401     C                   ENDIF
108800140401     C                   ENDDO
108900140401     C*
109000131016     C                   endif
109100131016     C*
109200131016     C                   ENDSR
109300020904
109400020904
109500010202     C*----------------------------------------------------*
109600131016     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
109700010202     C*----------------------------------------------------*
109800020305     C     PREVAT        BEGSR
109900010202     C*
110000131016     C* Compongo il nome del membro da dare al FIVATWWR
110100010202     C                   eval      parmbr = vlrhdl
110200010202     C                   movel     'M'           parmbr
110300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
110400010202     C                   eval      paropz = '1'
110500010202     C* Effettuo la chiamata al CLLE preposto
110600131017     C                   call(e)   'TITVVTC'
110700010202     C                   parm                    parccm
110800010202     C                   parm                    parmbr
110900010202     C                   parm                    paropz
111000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
111100010202     C                   if        %error
111200010202     C                   movel     '1'           chkcall
111300010202     C                   else
111400010202     C                   movel     '0'           chkcall
111500010202     C                   endif
111600010202     C*
111700010202     C                   ENDSR
111800000801     C*----------------------------------------------------*
111900000801     C*  CONTROLLO NUMERICITA' CAMPI
112000000801     C*----------------------------------------------------*
112100000801     C     CHKNUM        BEGSR
112200081113     C*
112300081113     C                   IF        PiDecChr = *blanks
112400121113     C                   EVAL      PiDecChr = '.'
112500081113     C                   ENDIF
112600081113     C*
112700081113     C                   callp(e)  UBISNUM_Check(PiStr
112800081113     C                                          :PiDecChr
112900081113     C                                          :PiVal
113000081113     C                                          :PiNum
113100081113     C                                          :PiInt)
113200081113     C*
113300000801     C                   IF        %error
113400000801     C                   EVAL      PiInt=*off
113500000801     C                   ENDIF
113600000801     C*
113700000801     C                   ENDSR
113800000801     C***
113900000801
114000011113
114100011113     C*----------------------------------------------------*
114200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
114300011113     C*----------------------------------------------------*
114400011113     C     CHKIMPDIV     BEGSR
114500011113     C*
114600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
114700011113     C                   Z-ADD     *zeros        wrkDec            9 9
114800011113     C*
114900011113     C* Come prima cosa effettuo considerazioni sulla divisa
115000011113     C                   IF        vabIAS > *zeros
115100011113     C                   IF        vabVAS <> 'EUR'
115200011113     C                   EVAL      vabVAS =  'ITL'
115300011113     C                   ENDIF
115400011113     C                   ENDIF
115500011113     C*
115600011113     C                   IF        vabCAS > *zeros
115700011113     C                   IF        vabVCA <> 'EUR'
115800011113     C                   EVAL      vabVCA =  'ITL'
115900011113     C                   ENDIF
116000011113     C                   ENDIF
116100011113     C*
116200011113     C                   IF        vabVMD > *zeros
116300020305     C                   IF        vabVAD <> 'EUR'
116400011113     C                   EVAL      vabVAD =  'ITL'
116500011113     C                   ENDIF
116600011113     C                   ENDIF
116700011113     C*
116800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
116900011113     C                   Z-ADD     vabIAS        wrkDec
117000011113     C                   IF        wrkDec > *zeros
117100011113     C                   IF        vabVAS = 'ITL'
117200011113     C                   EVAL      vabIAS = *zeros
117300011113     C                   ENDIF
117400011113     C                   ENDIF
117500011113     C*
117600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
117700011113     C                   Z-ADD     vabCAS        wrkDec
117800011113     C                   IF        wrkDec > *zeros
117900011113     C                   IF        vabVCA = 'ITL'
118000011113     C                   EVAL      vabCAS = *zeros
118100011113     C                   ENDIF
118200011113     C                   ENDIF
118300011113     C*
118400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
118500011113     C                   Z-ADD     vabVMD        wrkDec
118600011113     C                   IF        wrkDec > *zeros
118700011113     C                   IF        vabVAD = 'ITL'
118800011113     C                   EVAL      vabVMD = *zeros
118900011113     C                   ENDIF
119000011113     C                   ENDIF
119100011113     C*
119200011113     C                   ENDSR
119300011113     C***
119400011113
119500011113
119600000801
119700000801
119800990920      /TITLE Invio dei dati al punto operativo.
119900010202     C     invio         BEGSR
120000990920     C*
120100131016     C* 1° invio FIVAT
120200010201     C                   reset                   dscmz
120300111031     C                   move      wPOI          cmzdst
120400131016     C                   eval      cmzfld = 'FIVATWWR'
120500010201     C                   eval      cmzmbd = vlrhdl
120600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
120700021009     C***                if        prmfir = *blanks
120800131016     C                   eval      cmzfla = 'FIVAT00F'
120900131016     C                   eval      cmzmba = 'FIVAT00F'
121000021009     C***                else
121100021009     C***                eval      cmzfla = prmfir
121200021009     C***                eval      cmzmba = prmfir
121300021009     C***                endif
121400010201     C                   eval      cmznrr = *zeros
121500020305     C                   move      §ctrokvt      cmznrr
121600021018     C                   eval      cmzlba = vlrfl1
121700010201     C                   call(e)   'TIS711C'
121800010201     C                   parm                    dscmz
121900010201     C                   parm      *blanks       esito
122000010205     C                   if        %error
122100010205     C                             or cmzerr = '1'
122200010205     C                             or esito  = '1'
122300010205     C                   eval      wrkesito = '3'
122400010205     C                   else
122500010201     C*
122600131016     C* 2° invio FIVAB
122700010201     C                   reset                   dscmz
122800111031     C                   move      wPOI          cmzdst
122900010201     C                   eval      cmzfld = vlrfou
123000010201     C                   eval      cmzmbd = vlrhdl
123100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
123200021009     C***                if        prmfir = *blanks
123300131016     C                   eval      cmzfla = 'FIVAB00F'
123400131016     C                   eval      cmzmba = 'FIVAB00F'
123500021009     C***                else
123600021009     C***                eval      cmzfla = prmfir
123700021009     C***                eval      cmzmba = prmfir
123800021009     C***                endif
123900010201     C                   eval      cmznrr = *zeros
124000010201     C                   move      §ctrokvb      cmznrr
124100021018     C                   eval      cmzlba = vlrfl1
124200010201     C                   call(e)   'TIS711C'
124300010201     C                   parm                    dscmz
124400010201     C                   parm      *blanks       esito
124500010201     C                   if        %error
124600010201     C                             or cmzerr = '1'
124700010201     C                             or esito  = '1'
124800010201     C                   eval      wrkesito = '3'
124900010201     C                   endif
125000010205     C                   endif
125100990920     C*
125200000613     C                   ENDSR
125300000613     C***
125400070411
125500070411     C     *pssr         BEGSR
125600070411     C*
125700070411     C                   if        %open(tivin00r)
125800070411     C                   close     tivin00r
125900070411     C                   endif
126000131016     C                   if        %open(FIVABWWr)
126100131016     C                   close     FIVABWWr
126200120706     C                   endif
126300131016     C                   if        %open(FIVATWWr)
126400131016     C                   close     FIVATWWr
126500070411     C                   endif
126600070411     C*
126700070411     C* Effettuo la chiamata al CLLE preposto
126800131017     C                   call(e)   'TITVVTC'
126900070411     C                   parm                    parccm
127000070411     C                   parm                    parmbr
127100070411     C                   parm      '2'           paropz
127200070411     C*
127300070411     C                   eval      wrkesito = '2'
127400070411     C*
127500070411     C                   seton                                        LR
127600070411     C*
127700070411     C                   ENDSR     '*CANCL'
127800070411     C***
127900070411
128000990910
128100000613     C     *inzsr        BEGSR
128200990910     C*
128300990910     C     *entry        plist
128400990920     C                   parm                    tivlrds
128500990921     C                   parm      wrkesito      esito
128600000724     C                   parm                    prmlit
128700000710     C                   parm                    prmfir
128800000613     C*
128900000830     C* CALCOLA LA DATA CORRENTE
129000120706     C                   time                    wn14             14 0
129100120706     C                   movel     wn14          oracor            6 0          *ORA
129200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
129300100324     C                   eval      datcor = %dec(%date() : *ISO)
129400000830     C*
129500000613     C                   ENDSR
129600000613     C***
