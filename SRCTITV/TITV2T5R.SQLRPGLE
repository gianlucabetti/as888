000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200130315     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000500111125     FFIVABwwr  O    E             DISK    usropn prefix(ok_)
000600021113     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000801     D*----------------------------------------------------
000900000801     D* DICHIARAZIOINE VARIABILI DI WRK
001000000801     D*----------------------------------------------------
001100990920     D dscmz         e ds                  inz
001200990910     D psds           sds
001300990910     D  procname         *PROC
001400990920     D tivlrds       e ds                  extname(tivlr00f)
001500070730     D tisi95ds      e ds
001600111125     D fivabds       e ds                  extname(fivab00f)
001700111125     D fivabds_sav   e ds                  extname(fivab00f) prefix(sav_)
001800111125     D fivabds_ok    e ds                  extname(fivab00f) prefix(ok_)
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400150126     D depspe          s             10    INZ(*blanks)
002500150126     D curspe          s             10    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200000830
003300020725
003400020725     D*------------------
003500020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003600020725     D*------------------
003700070530     D INPUT_DS        DS                  INZ
003800101007     D  VINDTA                     2048
003900101007     D  VINFLG                        1
004000150126     D  VINSPE                       10
004100101007     D  VINRRN                        8  0
004200020725     D*
004300080923     D*------------------
004400080923     D* DS REPERIMENTO NUMERATORE
004500080923     D*------------------
004600080923     D trul33ds      e ds                  inz
004700080923     D*------------------
004800080923     D* DS ARCHITETTURA
004900080923     D*------------------
005000080923     D kpjba         e ds                  inz
005100080923     D*------------------
005200081113
005300081113     D*------------------
005400081113     D* LINKING A DEFINIZIONI ESTERNE
005500081113     D*------------------
005600100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
005700081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
005800081113
005900990908
006000010201
006100010201
006200080117     C*
006300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006400080117     C
006500080117     C/EXEC SQL
006600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006700080117     C/END-EXEC
006800080117     C
006900000913     C                   reset                   rrnum
007000990921     C                   reset                   esito
007100990921     C                   reset                   wrkesito
007200000613     C*
007300070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007400000613     C*
007500010202     C* Effettuo la chiamata al CLLE preposto
007600050128     C                   call(e)   'TITVVTC'
007700010202     C                   parm                    parccm
007800010202     C                   parm                    parmbr
007900010202     C                   parm      '2'           paropz
008000070730     C*
008100070730     C* Effettuo lancio TISI95 solo x chiusura
008200070730     C                   CLEAR                   TISI95DS
008300070730     C                   EVAL      I95TLA = 'C'
008400070730     C                   CALL      'TISI95R'
008500070730     C                   PARM                    TISI95DS
008600000616     C*
008700000801     C
008800010201     C                   seton                                        LR
008900990908
009000000801
009100910830     C*--------------------------------------------------------
009200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009300910830     C*--------------------------------------------------------
009400070530     C     RWFILE        BEGSR
009500990910     C*
009600990914     C                   if        not %open(tivin00r)
009700990908     C                   open      tivin00r
009800990914     C                   endif
009900021113     C                   if        not %open(fivabwwr)
010000021113     C                   open      fivabwwr
010100990914     C                   endif
010200070530     C*
010300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010400020305     C                   exsr      prevat
010500010201     C*
010600010202     C                   if        chkcall = '0'
010700010202     C*
010800021113     C                   if        not %open(fivatwwr)
010900021113     C                   open      fivatwwr
011000010201     C                   endif
011100080117     C*
011200080117     C                   EXSR      INZVAR
011300080117     C                   EXSR      DEFCAM
011400990910     C*
011500010201     C                   clear                   §CTROKVB          5 0
011600020305     C                   clear                   §CTROKVT          5 0
011700000801     C                   clear                   §CTRMO            5 0
011800000801     C                   clear                   §CTRNO            5 0
011900990910     C*
012000020725     C/EXEC SQL
012100020725     C+ declare C1 cursor for select
012200150126     C+ vindta, vinflg, substr(vindta, 522, 10) as sped, rrn(tivin00r)
012300060223     C+ from tivin00r
012400060223     C+ order by sped
012500020725     C+ for read only
012600020725     C/END-EXEC
012700020725     C
012800020725     C/EXEC SQL
012900020725     C+ open C1
013000020725     C/END-EXEC
013100020725     C
013200020725     C/EXEC SQL
013300070530     C+ Fetch C1 into :INPUT_DS
013400020725     C/END-EXEC
013500020725     C*
013600020725     C                   dow       sqlcod = *zeros
013700990913     C*
013800020725     C                   if        vindta > *blanks
013900000613     C                   add       1             rrnum
014000000801     C*
014100020725     C                   if        vinflg = *blanks
014200020725     C                             or vinflg = '0'
014300020725     C                             or vinflg = '2'
014400000801     C*
014500020725     C                   clear                   x_vinmsg
014600020725     C                   eval      x_vinflg = '1'
014700010305     C*
014800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
014900150122     C                   EVAL      PiStr=%trim(%subst(vindta:522:10))
015000020305     C                   MOVEL(p)  PiStr         curspe
015100010305     C*
015200071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015300080923     C                   if        *in50 = *off
015400071003     C                   exsr      impvab
015500111130     C  N31              eval      fivabds_ok = fivabds
015600111130     C  N31              exsr      wrivab
015700071003     C                   exsr      wrivat_b                                     => carico VAT
015800150122     C                   exsr      wrivat_i                                     => carico VAT
015900111125     C                   exsr      inzvar
016000071003     C                   else
016100080923     C*
016200071009     C                   if        wDISK = 'D'
016300150122     C                   exsr      repNSP
016400071009     C                   exsr      impvab
016500111130     C  N31              eval      fivabds_ok = fivabds
016600111130     C  N31              exsr      wrivab
016700071009     C                   exsr      wrivat_b                                     => carico VAT
016800150122     C                   exsr      wrivat_i                                     => carico VAT
016900071009     C                   exsr      wrivat_e                                     => carico VAT
017000111125     C                   exsr      inzvar
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600150122     C                   exsr      repNSP
017700020305     C                   exsr      impvab
017800111130     C                   z-add     VABNCL        sav_VABNCL
017900111130     C                   z-add     VABPKB        sav_VABPKB
018000111130     C                   z-add     VABVLB        sav_VABVLB
018100071003     C                   exsr      wrivat_b                                     => carico VAT
018200150122     C                   exsr      wrivat_i                                     => carico VAT
018300071003     C   50              exsr      wrivat_e                                     => carico VAT
018400010305     C                   else
018500020725     C                   if        depspe <> curspe                             => rottura di spediz
018600111130     C                   z-add     sav_VABNCL    VABNCL
018700111130     C                   z-add     sav_VABPKB    VABPKB
018800111130     C                   z-add     sav_VABVLB    VABVLB
018900111130     C  N31              eval      fivabds_ok = fivabds
019000111125     C  N31              exsr      wrivab
019100111125     C                   exsr      inzvar
019200111125     C                   eval      depspe = curspe                              => memorizz. spediz
019300080117     C                   clear                   tivinds
019400150122     C                   exsr      repNSP
019500020305     C                   exsr      impvab
019600111130     C                   if        wCntRecSpe = *zeros
019700111130     C                   z-add     VABNCL        sav_VABNCL
019800111130     C                   z-add     VABPKB        sav_VABPKB
019900111130     C                   z-add     VABVLB        sav_VABVLB
020000111130     C                   endif
020100111130     C                   add       1             wCntRecSpe
020200071003     C                   exsr      wrivat_b                                     => carico VAT
020300150122     C                   exsr      wrivat_i                                     => carico VAT
020400071003     C   50              exsr      wrivat_e                                     => carico VAT
020500020305     C                   else                                                   => x stessa spediz
020600111130     C                   exsr      impvab
020700150113     C   51              add       1             sav_VABNCL
020800111125     C** 51              add       VABNCL        sav_VABNCL
020900150113     C** 51              z-add     VABNCL        sav_VABNCL
021000111125     C   51              add       VABPKB        sav_VABPKB
021100111125     C   51              add       VABVLB        sav_VABVLB
021200111125     C   52              z-add     VABNCL        sav_VABNCL
021300111125     C   52              z-add     VABPKB        sav_VABPKB
021400111125     C   52              z-add     VABVLB        sav_VABVLB
021500111125     C   53              add       VABNCL        sav_VABNCL
021600111125     C   53              z-add     VABPKB        sav_VABPKB
021700111125     C   53              z-add     VABVLB        sav_VABVLB
021800111125     C*
021900071003     C                   exsr      wrivat_b                                     => carico VAT
022000150122     C                   exsr      wrivat_i                                     => carico VAT
022100071003     C   50              exsr      wrivat_e                                     => carico VAT
022200010305     C                   endif
022300010305     C                   endif
022400010305     C                   endif
022500071003     C                   endif
022600071009     C                   endif
022700000905     C*
022800000905     C                   else
022900020725     C                   eval      x_vinflg = '1'
023000000905     C                   endif
023100000905     C*
023200020725     C     VINRRN        chain     tivin000
023300020725     C                   if        %found(tivin00r)
023400020725     C                   eval      y_vinflg = x_vinflg
023500020725     C                   eval      y_vinmsg = x_vinmsg
023600020725     C                   update    tivin000
023700020725     C                   endif
023800020725     C*
023900020725     C/EXEC SQL
024000070530     C+ Fetch C1 into :INPUT_DS
024100020725     C/END-EXEC
024200020725     C*
024300020725     C                   enddo
024400020725     C*
024500020725     C/EXEC SQL
024600020725     C+ close C1
024700020725     C/END-EXEC
024800000905     C*
024900020305     C* Scarico i VAB rimasti "in sospeso"
025000071009     C                   if        wDISK = 'C'
025100111130     C  N31              eval      fivabds_ok = fivabds
025200111130     C  N31              exsr      wrivab
025300071009     C                   endif
025400010202     C*
025500010202     C                   endif
025600990910
025700990910     C* Se non ci sono record con errori ...
025800000710     C                   if        §ctrno = 0
025900990910     C* ... restituisco esito OK.
026000990921     C                   eval      wrkesito = '0'
026100990910     C                   else
026200010201     C                   if        §ctrokvb > 0
026300990921     C                   eval      wrkesito = '1'
026400000710     C                   else
026500000710     C                   eval      wrkesito = '2'
026600990910     C                   endif
026700000710     C                   endif
026800990910     C*
026900990914     C                   if        %open(tivin00r)
027000990908     C                   close     tivin00r
027100990914     C                   endif
027200021113     C                   if        %open(fivabwwr)
027300021113     C                   close     fivabwwr
027400990914     C                   endif
027500021113     C                   if        %open(fivatwwr)
027600021113     C                   close     fivatwwr
027700010201     C                   endif
027800990910     C*
027900111031     C                   if        §ctrokvb > 0 or
028000111031     C                             §ctrokvt > 0
028100111031     C                             and wPOI <> *zeros
028200010202     C                   exsr      invio
028300990920     C                   endif
028400990920     C*
028500910830     C                   ENDSR
028600000613     C***
028700010305
028800010305     C*----------------------------------------------------*
028900020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029000010305     C*----------------------------------------------------*
029100020305     C     WRIVAB        BEGSR
029200070730     C*
029300071003     C* Considerazioni finali
029400111125     C                   if        ok_VABRMA = *blanks
029500111125     C                   movel(P)  ok_VABRMN     ok_VABRMA
029600071003     C                   endif
029700071003     C*
029800021113     C                   write     fivab000                                     => scarico il VAB
029900010305     C*
030000010305     C                   ENDSR
030100990920
030200000801     C*----------------------------------------------------*
030300000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030400000801     C*----------------------------------------------------*
030500010201     C     INZVAR        BEGSR
030600000801     C*
030700010201     C                   Z-ADD     *zeros        Num5_0
030800020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
030900020725     C                   MOVEL     '0'           FlgCAS            1
031000111125     C*
031100111130     C                   Z-ADD     *zeros        wCntRecSpe        6 0
031200111130     C*
031300111125     C                   CLEAR                   FIVABDS
031400111130     C                   CLEAR                   FIVABDS_OK
031500111130     C                   CLEAR                   FIVABDS_SAV
031600111125     C                   CLEAR                   FIVAT000
031700000801     C*
031800000801     C                   ENDSR
031900000801     C*----------------------------------------------------*
032000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032100000801     C*----------------------------------------------------*
032200020904     C     DEFCAM        BEGSR
032300080923     C*
032400111125     C                   SETOFF                                       50
032500111125     C                   SETOFF                                       515253
032600070730     C* Imposto i valori di default...
032700150113     C                   EVAL      VABCCM = 0506414
032800150113     C                   EVAL      VATCCM = 0506414
032900150113     C                   EVAL      VABLNP = 050
033000150113     C                   EVAL      VATLNP = 050
033100101007     C                   EVAL      VABCTR = 000
033200070730     C                   EVAL      VABCBO = '1'
033300110520     C                   EVAL      VABTSP = 'C'
033400150122     C                   EVAL      VABNCL = 1
033500070222     C* ... e poi verifico se sono stati passati come parametri
033600070222     C                   IF        vlrppt > *blanks
033700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033900070222     C                   EXSR      CHKNUM
034000070222     C                   IF        PiInt=*on
034100070222     C                   Z-ADD     PiVal         VABCCM
034200070222     C                   Z-ADD     PiVal         VATCCM
034300070222     C                   ENDIF
034400070222     C                   ENDIF
034500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034700070222     C                   EXSR      CHKNUM
034800070222     C                   IF        PiInt=*on
034900070222     C                   Z-ADD     PiVal         VABLNP
035000070222     C                   Z-ADD     PiVal         VATLNP
035100070222     C                   ENDIF
035200070222     C                   ENDIF
035300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035500070222     C                   EXSR      CHKNUM
035600070222     C                   IF        PiInt=*on
035700070222     C                   Z-ADD     PiVal         VABCTR
035800070222     C                   ENDIF
035900070928     C                   ENDIF
036000071009     C                   MOVEL     *blanks       wDISK             1
036100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
036200071009     C                   IF        wDISK <> *blanks
036300070928     C                   SETON                                        50
036400070222     C                   ENDIF
036500080923     C                   IF        %subst(vlrppt:15:1) = 'S'
036600111125     C                   SETON                                        41
036700080923     C                   ENDIF
036800070222     C                   ENDIF
036900071009     C*
037000150122     C* valido per nr.colli/peso/volume
037100150122     C* ' ' = in ogni riga c'è il valore dell'intera spedizione
037200150122     C* 'A' = in ogni riga c'è il valore del collo, per cui va sommato all'interno della spedizione
037300150122     C* 'C' = vanno sommati solo i colli, per peso e volume c'è il valore dell'intera spedizione
037400150122     C* 'P' = vanno sommati solo peso e volume, per i colli c'è il valore dell'intera spedizione
037500150122     C                   SELECT
037600150122     C                   WHEN      %subst(vlrppt:18:1) = 'A'
037700150122     C                   SETON                                        51
037800150122     C                   WHEN      %subst(vlrppt:18:1) = *blanks
037900150122     C                   SETON                                        52
038000150122     C                   WHEN      %subst(vlrppt:18:1) = 'C'
038100150122     C                   SETON                                        53
038200150122     C                   WHEN      %subst(vlrppt:18:1) = 'P'
038300150122     C                   SETON                                        54
038400150122     C                   ENDSL
038500150122     C*
038600071009     C   50              EVAL      VABCTM = '7Q'
038700000801     C*
038800000801     C                   ENDSR
038900150122     C*----------------------------------------------------*
039000150122     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
039100150122     C*----------------------------------------------------*
039200150122     C     REPNSP        BEGSR
039300150122     C*
039400150122     C                   EXSR      INZVAR
039500150122     C                   EXSR      DEFCAM
039600150122     C*
039700150122     C                   SETON                                        60
039800150122     C*
039900150122     C* NSP => Stacco un numeratore da AZNUM
040000150122     C                   clear                   TRUL33DS
040100150122     C                   eval      I33OPE = *zeros
040200150122     C                   eval      I33CNU = 302
040300150122     C                   eval      I33NUM = 1
040400150122     C                   movel     TRUL33DS      KPJBU
040500150122     C                   call      'TRUL33R'
040600150122     C                   parm                    KPJBA
040700150122     C                   movel     KPJBU         TRUL33DS
040800150122     C                   if        O33ERR = *zeros
040900150122     C                   z-add     O33NRF        VABNSP
041000150122     C                   z-add     O33NRF        VATNSP
041100150122     C                   else
041200150122     C                   SETON                                        31
041300150122     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041400150122     C                             + ' ' + 'VABNSP VATNSP'
041500150122     C                   endif
041600150122     C*
041700150122     C                   ENDSR
041800000801     C*----------------------------------------------------*
041900021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042000000801     C*----------------------------------------------------*
042100010201     C     IMPVAB        BEGSR
042200070530     C*
042300070530     C                   SETOFF                                       3132
042400070928     C*
042500070928     C                   EXSR      DEFCAM
042600070928     C*
042700111031     C                   IF        vlrpoi <> 999
042800070928     C                   MOVE(P)   vlrpoi        VABFGS
042900070928     C                   MOVE(P)   vlrpoi        VATFGS
043000111031     C                   Z-ADD     vlrpoi        wPOI              3 0
043100111031     C                   ENDIF
043200070928     C*
043300070928     C                   MOVEL     datcor        VABAAS
043400070928     C                   MOVEL     datcor        VATAAS
043500070928     C                   MOVE      datcor        VABMGS
043600150122     C                   EVAL      VABRSD=%trim(%subst(vindta:217:32))
043700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
043900070928     C* ==
044000150122     C                   EVAL      VABRD2=%trim(%subst(vindta:249:32))
044100150122     C                   EVAL      VABIND=%trim(%subst(vindta:281:32))
044200150122     C                   EVAL      VABLOD=%trim(%subst(vindta:323:17))
044300150122     C                   EVAL      VABRMA=%trim(%subst(vindta:58:15))
044400071003     C* CAD
044500150122     C                   EVAL      PiStr=%trim(%subst(vindta:313:10))
044600070928     C                   EXSR      CHKNUM
044700070928     C                   IF        PiInt=*on
044800070928     C                   Z-ADD     PiVal         Num5_0
044900070928     C                   MOVEL     Num5_0        VABCAD
045000070928     C                   ELSE
045100070928     C                   SETON                                        32
045200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045300070928     C                             + ' ' + 'VABCAD'
045400070928     C                   ENDIF
045500080923     C* RMN
045600150122     C                   EVAL      PiStr=%trim(%subst(vindta:522:10))
045700080923     C                   EXSR      CHKNUM
045800080923     C                   IF        PiInt=*on
045900080923     C                   Z-ADD     PiVal         VABRMN
046000080923     C                   ELSE
046100080923     C                   SETON                                        32
046200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046300080923     C                             + ' ' + 'VABRMN'
046400080923     C                   ENDIF
046500071003     C* PKB
046600150122     C                   IF        %subst(vindta:52:6) <> *blanks
046700150122     C                   EVAL      PiStr=%trim(%subst(vindta:52:6))
046800070928     C                   EXSR      CHKNUM
046900070928     C                   IF        PiNum=*on
047000150122     C                   EVAL      VABPKB = PiVal
047100070928     C                   ELSE
047200070928     C                   SETON                                        32
047300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400070928     C                             + ' ' + 'VABPKB'
047500070928     C                   ENDIF
047600110520     C                   ENDIF
047700080923     C* CAS
047800150122     C***                IF        %trim(%subst(vindta:487:12))<>*blanks
047900150122     C***                          AND
048000150122     C***                          %trim(%subst(vindta:487:12))<>'000000000,00'
048100150122     C***                          AND
048200150122     C***                          %trim(%subst(vindta:487:12))<>'000000000.00'
048300150122     C***                MOVEL     '1'           FlgCAS
048400150122     C***                EVAL      VABVCA='EUR'
048500150122     C***                EVAL      PiStr=%trim(%subst(vindta:487:12))
048600150122     C***                                %trim(%subst(vindta:497:2))
048700150122     C***                EXSR      CHKNUM
048800150122     C***                IF        PiNum=*on
048900150122     C***                EVAL      VABCAS = PiVal
049000150122     C***                ELSE
049100150122     C***                SETON                                        32
049200150122     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
049300150122     C***                          + ' ' + 'VABCAS'
049400150122     C***                ENDIF
049500150122     C***                ENDIF
049600070730     C*
049700070730     C* Considerazioni finali su CBO/CAS
049800070730     C                   IF        FlgCAS = '1'
049900070730     C                   IF        VABCBO = '1'
050000070730     C                   EVAL      VABCBO = '4'
050100070730     C                   ENDIF
050200070730     C                   IF        VABCBO = '2'
050300070730     C                   EVAL      VABCBO = '6'
050400070730     C                   ENDIF
050500070730     C                   ENDIF
050600150122     C*
050700150122     C* Se provincia nn valorizzata la reperisco
050800150122     C* tramite TISI95R a seconda dei dati d instradamento presenti
050900150122     C                   IF        VABPRD =  *blanks AND
051000150122     C                             VABNZD =  *blanks AND
051100150122     C                             VABCAD <> *blanks AND
051200150122     C                             VABLOD <> *blanks
051300150122     C                   CLEAR                   TISI95DS
051400150122     C                   EVAL      I95TCN = '3'
051500150122     C                   Z-ADD     datcor        I95DAT
051600150122     C                   EVAL      I95NAR = VABNZD
051700150122     C                   EVAL      I95CAP = VABCAD
051800150122     C                   EVAL      I95LOC = VABLOD
051900150122     C                   CALL      'TISI95R'
052000150122     C                   PARM                    TISI95DS
052100150122     C                   EVAL      VABPRD = O95PRV
052200150122     C                   ENDIF
052300020305     C*
052400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
052500011113     C                   EXSR      CHKIMPDIV
052600010202     C*
052700000801     C* Ebbene...
052800000801     C                   ADD       1             §CTRMO
052900070530     C                   IF        *in31 <> *zeros OR
053000070530     C                             *in32 <> *zeros
053100000801     C                   ADD       1             §CTRNO
053200020725     C                   EVAL      x_vinflg = '2'
053300000801     C                   ELSE
053400010201     C                   ADD       1             §CTROKVB
053500000801     C                   ENDIF
053600000801     C*
053700000801     C                   ENDSR
053800010201     C*----------------------------------------------------*
053900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
054000010201     C*----------------------------------------------------*
054100071003     C     WRIVAT_B      BEGSR
054200090204     C*
054300090204     C                   eval      vatAAS = vabAAS
054400090204     C                   eval      vatLNP = vabLNP
054500090204     C                   eval      vatNRS = vabNRS
054600090204     C                   eval      vatNSP = vabNSP
054700010201     C*
054800101007     C* Valorizzo il buffer di scrittura del FIVAT
054900070928     C* - TIPO RECORD "B"
055000150122     C                   if        %subst(vindta:458:27) <> *blanks
055100070928     C                   eval      VATTRC = 'B'
055200150122     C                   eval      VATNOT = %trim(%subst(vindta:458:27))
055300021113     C                   write     FIVAT000
055400111031     C                   add       1             §ctrokvt
055500020725     C                   endif
055600010201     C*
055700010201     C                   ENDSR
055800150122     C*----------------------------------------------------*
055900150122     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
056000150122     C*----------------------------------------------------*
056100150122     C     WRIVAT_I      BEGSR
056200150122     C*
056300150122     C***                eval      vatAAS = vabAAS
056400150122     C***                eval      vatLNP = vabLNP
056500150122     C***                eval      vatNRS = vabNRS
056600150122     C***                eval      vatNSP = vabNSP
056700150122     C***
056800150122     C**Valorizzo il buffer di scrittura del FIVAT
056900150122     C**- TIPO RECORD "I"
057000150122     C***                EVAL      w70=%trim(%subst(vindta:352:64))
057100150122     C***                if        %subst(w70:1:35) <> *blanks
057200150122     C***                eval      VATTRC = 'I'
057300150122     C***                eval      VATNOT = %trim(%subst(w70:1:35))
057400150122     C***                write     FIVAT000
057500150122     C***                endif
057600150122     C***                if        %subst(w70:36:35) <> *blanks
057700150122     C***                eval      VATTRC = 'J'
057800150122     C***                eval      VATNOT = %trim(%subst(w70:36:35))
057900150122     C***                write     FIVAT000
058000150122     C***                endif
058100150122     C*
058200150122     C                   ENDSR
058300071003     C*----------------------------------------------------*
058400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058500071003     C*----------------------------------------------------*
058600071003     C     WRIVAT_E      BEGSR
058700090204     C*
058800090204     C                   eval      vatAAS = vabAAS
058900090204     C                   eval      vatLNP = vabLNP
059000090204     C                   eval      vatNRS = vabNRS
059100090204     C                   eval      vatNSP = vabNSP
059200071003     C*
059300101007     C* Valorizzo il buffer di scrittura del FIVAT
059400071003     C* - TIPO RECORD "E"
059500150122     C                   if        %subst(vindta:20:32) <> *blanks
059600071003     C                   eval      VATTRC = 'E'
059700150122     C                   eval      VATNOT = %trim(%subst(vindta:20:32))
059800071003     C                   write     FIVAT000
059900111031     C                   add       1             §ctrokvt
060000071003     C                   endif
060100071003     C*
060200071003     C                   ENDSR
060300020904
060400020904
060500010202     C*----------------------------------------------------*
060600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060700010202     C*----------------------------------------------------*
060800020305     C     PREVAT        BEGSR
060900010202     C*
061000021113     C* Compongo il nome del membro da dare al FIVATWWR
061100010202     C                   eval      parmbr = vlrhdl
061200010202     C                   movel     'M'           parmbr
061300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061400010202     C                   eval      paropz = '1'
061500010202     C* Effettuo la chiamata al CLLE preposto
061600050128     C                   call(e)   'TITVVTC'
061700010202     C                   parm                    parccm
061800010202     C                   parm                    parmbr
061900010202     C                   parm                    paropz
062000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062100010202     C                   if        %error
062200010202     C                   movel     '1'           chkcall
062300010202     C                   else
062400010202     C                   movel     '0'           chkcall
062500010202     C                   endif
062600010202     C*
062700010202     C                   ENDSR
062800000801     C*----------------------------------------------------*
062900000801     C*  CONTROLLO NUMERICITA' CAMPI
063000000801     C*----------------------------------------------------*
063100000801     C     CHKNUM        BEGSR
063200081113     C*
063300081113     C                   IF        PiDecChr = *blanks
063400110524     C                   EVAL      PiDecChr = '.'
063500081113     C                   ENDIF
063600081113     C*
063700081113     C                   callp(e)  UBISNUM_Check(PiStr
063800081113     C                                          :PiDecChr
063900081113     C                                          :PiVal
064000081113     C                                          :PiNum
064100081113     C                                          :PiInt)
064200081113     C*
064300000801     C                   IF        %error
064400000801     C                   EVAL      PiInt=*off
064500000801     C                   ENDIF
064600000801     C*
064700000801     C                   ENDSR
064800000801     C***
064900000801
065000011113
065100011113     C*----------------------------------------------------*
065200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065300011113     C*----------------------------------------------------*
065400011113     C     CHKIMPDIV     BEGSR
065500011113     C*
065600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065700011113     C                   Z-ADD     *zeros        wrkDec            9 9
065800011113     C*
065900011113     C* Come prima cosa effettuo considerazioni sulla divisa
066000011113     C                   IF        vabIAS > *zeros
066100011113     C                   IF        vabVAS <> 'EUR'
066200011113     C                   EVAL      vabVAS =  'ITL'
066300011113     C                   ENDIF
066400011113     C                   ENDIF
066500011113     C*
066600011113     C                   IF        vabCAS > *zeros
066700130327     C* siccome il cliente passa, a volte, questo dato vuoto per intendere EUR,
066800130327     C* se il c/assegno è valorizzato ma la divisa è vuota, la forzo a EUR
066900130327     C                   IF        vabVCA = *blank
067000130327     C                   EVAL      vabVCA =  'EUR'
067100130327     C                   ENDIF
067200011113     C                   IF        vabVCA <> 'EUR'
067300011113     C                   EVAL      vabVCA =  'ITL'
067400011113     C                   ENDIF
067500011113     C                   ENDIF
067600011113     C*
067700011113     C                   IF        vabVMD > *zeros
067800020305     C                   IF        vabVAD <> 'EUR'
067900011113     C                   EVAL      vabVAD =  'ITL'
068000011113     C                   ENDIF
068100011113     C                   ENDIF
068200011113     C*
068300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068400011113     C                   Z-ADD     vabIAS        wrkDec
068500011113     C                   IF        wrkDec > *zeros
068600011113     C                   IF        vabVAS = 'ITL'
068700011113     C                   EVAL      vabIAS = *zeros
068800011113     C                   ENDIF
068900011113     C                   ENDIF
069000011113     C*
069100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069200011113     C                   Z-ADD     vabCAS        wrkDec
069300011113     C                   IF        wrkDec > *zeros
069400011113     C                   IF        vabVCA = 'ITL'
069500011113     C                   EVAL      vabCAS = *zeros
069600011113     C                   ENDIF
069700011113     C                   ENDIF
069800011113     C*
069900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070000011113     C                   Z-ADD     vabVMD        wrkDec
070100011113     C                   IF        wrkDec > *zeros
070200011113     C                   IF        vabVAD = 'ITL'
070300011113     C                   EVAL      vabVMD = *zeros
070400011113     C                   ENDIF
070500011113     C                   ENDIF
070600011113     C*
070700011113     C                   ENDSR
070800011113     C***
070900011113
071000011113
071100000801
071200000801
071300990920      /TITLE Invio dei dati al punto operativo.
071400010202     C     invio         BEGSR
071500990920     C*
071600021113     C* 1° invio FIVAT
071700010201     C                   reset                   dscmz
071800111031     C                   move      wPOI          cmzdst
071900021113     C                   eval      cmzfld = 'FIVATWWR'
072000010201     C                   eval      cmzmbd = vlrhdl
072100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072200021009     C***                if        prmfir = *blanks
072300021113     C                   eval      cmzfla = 'FIVAT00F'
072400021113     C                   eval      cmzmba = 'FIVAT00F'
072500021009     C***                else
072600021009     C***                eval      cmzfla = prmfir
072700021009     C***                eval      cmzmba = prmfir
072800021009     C***                endif
072900010201     C                   eval      cmznrr = *zeros
073000020305     C                   move      §ctrokvt      cmznrr
073100021018     C                   eval      cmzlba = vlrfl1
073200010201     C                   call(e)   'TIS711C'
073300010201     C                   parm                    dscmz
073400010201     C                   parm      *blanks       esito
073500010205     C                   if        %error
073600010205     C                             or cmzerr = '1'
073700010205     C                             or esito  = '1'
073800010205     C                   eval      wrkesito = '3'
073900010205     C                   else
074000010201     C*
074100021113     C* 2° invio FIVAB
074200010201     C                   reset                   dscmz
074300111031     C                   move      wPOI          cmzdst
074400010201     C                   eval      cmzfld = vlrfou
074500010201     C                   eval      cmzmbd = vlrhdl
074600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074700021009     C***                if        prmfir = *blanks
074800021113     C                   eval      cmzfla = 'FIVAB00F'
074900021113     C                   eval      cmzmba = 'FIVAB00F'
075000021009     C***                else
075100021009     C***                eval      cmzfla = prmfir
075200021009     C***                eval      cmzmba = prmfir
075300021009     C***                endif
075400010201     C                   eval      cmznrr = *zeros
075500010201     C                   move      §ctrokvb      cmznrr
075600021018     C                   eval      cmzlba = vlrfl1
075700010201     C                   call(e)   'TIS711C'
075800010201     C                   parm                    dscmz
075900010201     C                   parm      *blanks       esito
076000010201     C                   if        %error
076100010201     C                             or cmzerr = '1'
076200010201     C                             or esito  = '1'
076300010201     C                   eval      wrkesito = '3'
076400010201     C                   endif
076500010205     C                   endif
076600990920     C*
076700000613     C                   ENDSR
076800000613     C***
076900070411
077000070411     C     *pssr         BEGSR
077100070411     C*
077200070411     C                   if        %open(tivin00r)
077300070411     C                   close     tivin00r
077400070411     C                   endif
077500070411     C                   if        %open(fivabwwr)
077600070411     C                   close     fivabwwr
077700070411     C                   endif
077800070411     C                   if        %open(fivatwwr)
077900070411     C                   close     fivatwwr
078000070411     C                   endif
078100070411     C*
078200070411     C* Effettuo la chiamata al CLLE preposto
078300070411     C                   call(e)   'TITVVTC'
078400070411     C                   parm                    parccm
078500070411     C                   parm                    parmbr
078600070411     C                   parm      '2'           paropz
078700070411     C*
078800070411     C                   eval      wrkesito = '2'
078900070411     C*
079000070411     C                   seton                                        LR
079100070411     C*
079200070411     C                   ENDSR     '*CANCL'
079300070411     C***
079400070411
079500990910
079600000613     C     *inzsr        BEGSR
079700990910     C*
079800990910     C     *entry        plist
079900990920     C                   parm                    tivlrds
080000990921     C                   parm      wrkesito      esito
080100000724     C                   parm                    prmlit
080200000710     C                   parm                    prmfir
080300000613     C*
080400000830     C* CALCOLA LA DATA CORRENTE
080500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080600100324     C                   eval      datcor = %dec(%date() : *ISO)
080700000830     C*
080800000613     C                   ENDSR
080900000613     C***
