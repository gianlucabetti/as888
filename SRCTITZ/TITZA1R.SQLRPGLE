000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130306      *
000500121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000600990908
000700020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000800021113     FFIVABwwr  O    E             DISK    usropn
000900021113     FFIVATwwr  O    E             DISK    usropn
001000990908
001100000801     D*----------------------------------------------------
001200000801     D* DICHIARAZIOINE VARIABILI DI WRK
001300000801     D*----------------------------------------------------
001400990920     D dscmz         e ds                  inz
001500990910     D psds           sds
001600990910     D  procname         *PROC
001700990920     D tivlrds       e ds                  extname(tivlr00f)
001800070730     D tisi95ds      e ds
001900990910     D esito           s              1
002000000724     D prmlit          s             10
002100000710     D prmfir          s             10
002200990921     D wrkesito        s                   like(esito)
002300000613     D rrnum           s              6  0 INZ(*zeros)
002400131009     D depspe          s             16    INZ(*blanks)
002500131009     D curspe          s             16    INZ(*blanks)
002600010202     D parccm          s              8    INZ(*blanks)
002700010202     D parmbr          s             10    INZ(*blanks)
002800010202     D paropz          s              1    INZ(*blanks)
002900010202     D chkcall         s              1    INZ(*blanks)
003000080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003100070530     D Num5_0          s              5  0
003200121011     D w70             s             70
003300000830
003400020725
003500020725     D*------------------
003600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003700020725     D*------------------
003800070530     D INPUT_DS        DS                  INZ
003900101007     D  VINDTA                     2048
004000101007     D  VINFLG                        1
004100131009     D  VINSPE                       16
004200101007     D  VINRRN                        8  0
004300020725     D*
004400080923     D*------------------
004500080923     D* DS REPERIMENTO NUMERATORE
004600080923     D*------------------
004700080923     D trul33ds      e ds                  inz
004800080923     D*------------------
004900080923     D* DS ARCHITETTURA
005000080923     D*------------------
005100080923     D kpjba         e ds                  inz
005200080923     D*------------------
005300121126     D BarcodeDS       DS           770    INZ
005400121126     D  wBarcode                     14    dim(55)
005500121109     D*------------------
005600121109     D TRUL28DSX     e ds                  inz
005700121109     D*------------------
005800121109     D wX              s              3s 0
005900081113
006000081113     D*------------------
006100081113     D* LINKING A DEFINIZIONI ESTERNE
006200081113     D*------------------
006300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006500081113
006600990908
006700010201
006800010201
006900080117     C*
007000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007100080117     C
007200080117     C/EXEC SQL
007300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007400080117     C/END-EXEC
007500080117     C
007600000913     C                   reset                   rrnum
007700990921     C                   reset                   esito
007800990921     C                   reset                   wrkesito
007900000613     C*
008000131009     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
008100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008200000613     C*
008300010202     C* Effettuo la chiamata al CLLE preposto
008400050128     C                   call(e)   'TITVVTC'
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
009700000801
009800910830     C*--------------------------------------------------------
009900070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010000910830     C*--------------------------------------------------------
010100070530     C     RWFILE        BEGSR
010200990910     C*
010300990914     C                   if        not %open(tivin00r)
010400990908     C                   open      tivin00r
010500990914     C                   endif
010600021113     C                   if        not %open(fivabwwr)
010700021113     C                   open      fivabwwr
010800990914     C                   endif
010900070530     C*
011000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011100020305     C                   exsr      prevat
011200010201     C*
011300010202     C                   if        chkcall = '0'
011400010202     C*
011500021113     C                   if        not %open(fivatwwr)
011600021113     C                   open      fivatwwr
011700010201     C                   endif
011800080117     C*
011900080117     C                   EXSR      INZVAR
012000080117     C                   EXSR      DEFCAM
012100990910     C*
012200010201     C                   clear                   §CTROKVB          5 0
012300020305     C                   clear                   §CTROKVT          5 0
012400000801     C                   clear                   §CTRMO            5 0
012500000801     C                   clear                   §CTRNO            5 0
012600990910     C*
012700020725     C/EXEC SQL
012800020725     C+ declare C1 cursor for select
012900131009     C+ vindta, vinflg, substr(vindta, 1, 16) as sped, rrn(tivin00r)
013000060223     C+ from tivin00r
013100060223     C+ order by sped
013200020725     C+ for read only
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600020725     C+ open C1
013700020725     C/END-EXEC
013800020725     C
013900020725     C/EXEC SQL
014000070530     C+ Fetch C1 into :INPUT_DS
014100020725     C/END-EXEC
014200020725     C*
014300020725     C                   dow       sqlcod = *zeros
014400990913     C*
014500020725     C                   if        vindta > *blanks
014600000613     C                   add       1             rrnum
014700000801     C*
014800020725     C                   if        vinflg = *blanks
014900020725     C                             or vinflg = '0'
015000020725     C                             or vinflg = '2'
015100000801     C*
015200020725     C                   clear                   x_vinmsg
015300020725     C                   eval      x_vinflg = '1'
015400010305     C*
015500121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015600121109     C* (uso l'RMN)
015700131009     C                   EVAL      PiStr=%trim(%subst(vindta:001:16))
015800020305     C                   MOVEL(p)  PiStr         curspe
015900010305     C*
016000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016100080923     C                   if        *in50 = *off
016200131009     C                   exsr      inzvar
016300131009     C                   exsr      defcam
016400071003     C                   exsr      impvab
016500071003     C                   exsr      wrivab
016600131009     C                   exsr      wrivat_a                                     => carico VAT
016700131009     C                   exsr      wrivat_i                                     => carico VAT
016800131009     C                   exsr      wrivat_j                                     => carico VAT
016900071003     C                   else
017000080923     C*
017100071009     C                   if        wDISK = 'D'
017200131009     C                   exsr      repNSP
017300071009     C                   exsr      impvab
017400071009     C                   exsr      wrivab
017500131009     C                   exsr      wrivat_a                                     => carico VAT
017600131009     C                   exsr      wrivat_i                                     => carico VAT
017700131009     C                   exsr      wrivat_j                                     => carico VAT
017800131009     C                   exsr      wrivat_e                                     => carico VAT
017900071009     C                   else
018000071009     C*
018100010305     C                   if        depspe = *blanks                             => 1° giro
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300080117     C                   clear                   tivinds
018400131009     C                   exsr      repNSP
018500020305     C                   exsr      impvab
018600131009     C                   exsr      wrivat_a                                     => carico VAT
018700131009     C                   exsr      wrivat_i                                     => carico VAT
018800131009     C                   exsr      wrivat_j                                     => carico VAT
018900131009     C   50              exsr      wrivat_e                                     => carico VAT
019000010305     C                   else
019100020725     C                   if        depspe <> curspe                             => rottura di spediz
019200010305     C                   eval      depspe = curspe                              => memorizz. spediz
019300070928     C                   exsr      wrivab
019400080117     C                   clear                   tivinds
019500131009     C                   exsr      repNSP
019600020305     C                   exsr      impvab
019700131009     C                   exsr      wrivat_a                                     => carico VAT
019800131009     C                   exsr      wrivat_i                                     => carico VAT
019900131009     C                   exsr      wrivat_j                                     => carico VAT
020000131009     C   50              exsr      wrivat_e                                     => carico VAT
020100020305     C                   else                                                   => x stessa spediz
020200090204     C***                exsr      impvab
020300131028     C                   exsr      wrivat_a                                     => carico VAT
020400131028     C                   exsr      wrivat_i                                     => carico VAT
020500131028     C                   exsr      wrivat_j                                     => carico VAT
020600131028     C   50              exsr      wrivat_e                                     => carico VAT
020700010305     C                   endif
020800010305     C                   endif
020900010305     C                   endif
021000071003     C                   endif
021100071009     C                   endif
021200000905     C*
021300000905     C                   else
021400020725     C                   eval      x_vinflg = '1'
021500000905     C                   endif
021600000905     C*
021700020725     C     VINRRN        chain     tivin000
021800020725     C                   if        %found(tivin00r)
021900020725     C                   eval      y_vinflg = x_vinflg
022000020725     C                   eval      y_vinmsg = x_vinmsg
022100020725     C                   update    tivin000
022200020725     C                   endif
022300020725     C*
022400020725     C/EXEC SQL
022500070530     C+ Fetch C1 into :INPUT_DS
022600020725     C/END-EXEC
022700020725     C*
022800020725     C                   enddo
022900020725     C*
023000020725     C/EXEC SQL
023100020725     C+ close C1
023200020725     C/END-EXEC
023300000905     C*
023400020305     C* Scarico i VAB rimasti "in sospeso"
023500071009     C                   if        wDISK = 'C'
023600071009     C                   exsr      wrivab
023700071009     C                   endif
023800010202     C*
023900010202     C                   endif
024000990910
024100990910     C* Se non ci sono record con errori ...
024200000710     C                   if        §ctrno = 0
024300990910     C* ... restituisco esito OK.
024400990921     C                   eval      wrkesito = '0'
024500990910     C                   else
024600010201     C                   if        §ctrokvb > 0
024700990921     C                   eval      wrkesito = '1'
024800000710     C                   else
024900000710     C                   eval      wrkesito = '2'
025000990910     C                   endif
025100000710     C                   endif
025200990910     C*
025300990914     C                   if        %open(tivin00r)
025400990908     C                   close     tivin00r
025500990914     C                   endif
025600021113     C                   if        %open(fivabwwr)
025700021113     C                   close     fivabwwr
025800990914     C                   endif
025900021113     C                   if        %open(fivatwwr)
026000021113     C                   close     fivatwwr
026100010201     C                   endif
026200990910     C*
026300010201     C                   if        §ctrokvb > 0
026400000724     C                             and vlrpoi <> *zeros
026500010202     C                   exsr      invio
026600990920     C                   endif
026700990920     C*
026800910830     C                   ENDSR
026900000613     C***
027000010305
027100010305     C*----------------------------------------------------*
027200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027300010305     C*----------------------------------------------------*
027400020305     C     WRIVAB        BEGSR
027500070730     C*
027600071003     C* Considerazioni finali
027700071003     C                   if        VABRMA = *blanks
027800071003     C                   movel(P)  VABRMN        VABRMA
027900071003     C                   endif
028000071003     C*
028100021113     C                   write     fivab000                                     => scarico il VAB
028200010305     C*
028300010305     C                   ENDSR
028400990920
028500000801     C*----------------------------------------------------*
028600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
028700000801     C*----------------------------------------------------*
028800010201     C     INZVAR        BEGSR
028900000801     C*
029000010201     C                   Z-ADD     *zeros        Num5_0
029100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029200020725     C                   MOVEL     '0'           FlgCAS            1
029300000801     C*
029400000801     C                   ENDSR
029500000801     C*----------------------------------------------------*
029600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
029700000801     C*----------------------------------------------------*
029800020904     C     DEFCAM        BEGSR
029900080923     C*
030000080923     C                   SETOFF                                       5051
030100000801     C*
030200021113     C                   CLEAR                   FIVAB000
030300021113     C                   CLEAR                   FIVAT000
030400070730     C* Imposto i valori di default...
030500131009     C                   EVAL      VABCCM = 2230139
030600131009     C                   EVAL      VATCCM = 2230139
030700131009     C                   EVAL      VABLNP = 223
030800131009     C                   EVAL      VATLNP = 223
030900130306     C                   EVAL      VABCTR = 000
031000130723     C                   EVAL      VABCBO = '1'
031100070222     C* ... e poi verifico se sono stati passati come parametri
031200070222     C                   IF        vlrppt > *blanks
031300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
031500070222     C                   EXSR      CHKNUM
031600070222     C                   IF        PiInt=*on
031700070222     C                   Z-ADD     PiVal         VABCCM
031800070222     C                   Z-ADD     PiVal         VATCCM
031900070222     C                   ENDIF
032000070222     C                   ENDIF
032100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032300070222     C                   EXSR      CHKNUM
032400070222     C                   IF        PiInt=*on
032500070222     C                   Z-ADD     PiVal         VABLNP
032600070222     C                   Z-ADD     PiVal         VATLNP
032700070222     C                   ENDIF
032800070222     C                   ENDIF
032900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
033000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033100070222     C                   EXSR      CHKNUM
033200070222     C                   IF        PiInt=*on
033300070222     C                   Z-ADD     PiVal         VABCTR
033400070222     C                   ENDIF
033500070928     C                   ENDIF
033600071009     C                   MOVEL     *blanks       wDISK             1
033700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
033800071009     C                   IF        wDISK <> *blanks
033900070928     C                   SETON                                        50
034000070222     C                   ENDIF
034100080923     C                   IF        %subst(vlrppt:15:1) = 'S'
034200080923     C                   SETON                                        51
034300080923     C                   ENDIF
034400130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
034500130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
034600130507     C                   ENDIF
034700070222     C                   ENDIF
034800071009     C*
034900071009     C   50              EVAL      VABCTM = '7Q'
035000000801     C*
035100000801     C                   ENDSR
035200131009     C*----------------------------------------------------*
035300131009     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
035400131009     C*----------------------------------------------------*
035500131009     C     REPNSP        BEGSR
035600131009     C*
035700131009     C                   EXSR      INZVAR
035800131009     C                   EXSR      DEFCAM
035900131009     C*
036000131009     C                   SETON                                        60
036100131009     C*
036200131009     C* NSP => Stacco un numeratore da AZNUM
036300131009     C                   clear                   TRUL33DS
036400131009     C                   eval      I33OPE = *zeros
036500131009     C                   eval      I33CNU = 302
036600131009     C                   eval      I33NUM = 1
036700131009     C                   movel     TRUL33DS      KPJBU
036800131009     C                   call      'TRUL33R'
036900131009     C                   parm                    KPJBA
037000131009     C                   movel     KPJBU         TRUL33DS
037100131009     C                   if        O33ERR = *zeros
037200131009     C                   z-add     O33NRF        VABNSP
037300131009     C                   z-add     O33NRF        VATNSP
037400131009     C                   else
037500131009     C                   SETON                                        31
037600131009     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037700131009     C                             + ' ' + 'VABNSP VATNSP'
037800131009     C                   endif
037900131009     C*
038000131009     C                   ENDSR
038100000801     C*----------------------------------------------------*
038200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
038300000801     C*----------------------------------------------------*
038400010201     C     IMPVAB        BEGSR
038500070530     C*
038600070530     C                   SETOFF                                       3132
038700070928     C*
038800070928     C                   MOVE(P)   vlrpoi        VABFGS
038900070928     C                   MOVE(P)   vlrpoi        VATFGS
039000130723     C                   MOVEL     datcor        VABAAS
039100070928     C                   MOVE      datcor        VABMGS
039200131009     C                   EVAL      w70=%trim(%subst(vindta:072:40))
039300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
039400131009     C     '@':'A'       XLATE     w70           w70
039500131009     C                   EVAL      VABRSD=%subst(w70:01:35)
039600131009     C                   EVAL      VABRD2=%subst(w70:36:05)
039700070928     C* ==
039800131009     C                   IF        %trim(%subst(vindta:071:01))='D'
039900131009     C                   EVAL      VABTSP='H'
040000131009     C                   ENDIF
040100131009     C                   EVAL      VABIND=%trim(%subst(vindta:112:40))
040200131009     C                   EVAL      VABLOD=%trim(%subst(vindta:162:30))
040300131009     C                   EVAL      VABPRD=%trim(%subst(vindta:192:02))
040400131009     C                   EVAL      VABRMA=%trim(%subst(vindta:027:10))
040500131009     C                   EVAL      w70=%trim(%subst(vindta:306:60))
040600130723     C                   EVAL      VABNOT=%subst(w70:1:35)
040700130723     C                   EVAL      VABNT2=%subst(w70:36:35)
040800131009     C                   IF        %trim(%subst(vindta:366:01))='Y'
040900131009     C                   EVAL      VABFFD='S'
041000131009     C                   ENDIF
041100131009     C                   IF        %trim(%subst(vindta:387:01))='A'
041200131009     C                   EVAL      VABCBO='2'
041300131009     C                   ENDIF
041400120717     C*
041500071003     C* CAD
041600131009     C                   EVAL      PiStr=%trim(%subst(vindta:152:10))
041700121001     C                   EXSR      CHKNUM
041800121001     C                   IF        PiInt=*on
041900121001     C                   Z-ADD     PiVal         Num5_0
042000121001     C                   MOVEL     Num5_0        VABCAD
042100121001     C                   ELSE
042200121001     C                   SETON                                        32
042300121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042400121001     C                             + ' ' + 'VABCAD'
042500121001     C                   ENDIF
042600080923     C* RMN
042700131009     C                   EVAL      PiStr=%trim(%subst(vindta:001:16))
042800121001     C                   EXSR      CHKNUM
042900121001     C                   IF        PiInt=*on
043000121001     C                   Z-ADD     PiVal         VABRMN
043100121001     C                   ELSE
043200121001     C                   SETON                                        32
043300121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400121001     C                             + ' ' + 'VABRMN'
043500121001     C                   ENDIF
043600071003     C* NCL
043700131009     C                   EVAL      PiStr=%trim(%subst(vindta:197:5))
043800070928     C                   EXSR      CHKNUM
043900070928     C                   IF        PiInt=*on
044000070928     C                   Z-ADD     PiVal         VABNCL
044100070928     C                   ELSE
044200070928     C                   SETON                                        32
044300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400070928     C                             + ' ' + 'VABNCL'
044500070928     C                   ENDIF
044600071003     C* PKB
044700140306     C                   EVAL      PiStr=%trim(%subst(vindta:202:8))
044800070928     C                   EXSR      CHKNUM
044900070928     C                   IF        PiNum=*on
045000131009     C* il dato ha la virgola che separa i decimali
045100131009     C                   EVAL(H)   VABPKB = PiVal
045200070928     C                   ELSE
045300070928     C                   SETON                                        32
045400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045500070928     C                             + ' ' + 'VABPKB'
045600070928     C                   ENDIF
045700131009     C* CAS
045800131009     C                   IF        %trim(%subst(vindta:367:01))='Y'
045900131009     C                   EVAL      VABVCA = 'EUR'
046000131122     C                   EVAL      VABTIC = 'BM'
046100131009     C                   EVAL      PiStr=%trim(%subst(vindta:368:9))
046200131009     C                   EXSR      CHKNUM
046300131009     C                   IF        PiNum=*on
046400131009     C* il dato ha la virgola che separa i decimali
046500131009     C                   EVAL(H)   VABCAS = PiVal
046600131009     C                   ELSE
046700131009     C                   SETON                                        32
046800131009     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046900131009     C                             + ' ' + 'VABCAS'
047000131009     C                   ENDIF
047100131009     C                   ENDIF
047200131009     C* IAS
047300131009     C                   IF        %trim(%subst(vindta:377:01))='Y'
047400131009     C                   EVAL      VABVAS = 'EUR'
047500131009     C                   EVAL      PiStr=%trim(%subst(vindta:378:9))
047600131009     C                   EXSR      CHKNUM
047700131009     C                   IF        PiNum=*on
047800131009     C* il dato ha la virgola che separa i decimali
047900131009     C                   EVAL(H)   VABIAS = PiVal
048000131009     C                   ELSE
048100131009     C                   SETON                                        32
048200131009     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048300131009     C                             + ' ' + 'VABIAS'
048400131009     C                   ENDIF
048500131009     C                   ENDIF
048600070730     C*
048700121109     C* Considerazioni finali su CBO/CAS
048800121106     C*
048900121106      * se c/assegno impostato
049000121106     C                   IF        VABCAS > 0
049100121109     C                   IF        VABCBO = '1'
049200121109     C                   EVAL      VABCBO = '4'
049300121109     C                   ENDIF
049400121109     C                   IF        VABCBO = '2'
049500121109     C                   EVAL      VABCBO = '6'
049600121109     C                   ENDIF
049700121106     C                   ENDIF
049800130306     C*
049900130306     C* Se provincia nn valorizzata la reperisco
050000130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
050100130306     C                   IF        VABPRD =  *blanks AND
050200130306     C                             VABNZD =  *blanks AND
050300130306     C                             VABCAD <> *blanks AND
050400130306     C                             VABLOD <> *blanks
050500130306     C                   CLEAR                   TISI95DS
050600130306     C                   EVAL      I95TCN = '3'
050700130306     C                   Z-ADD     datcor        I95DAT
050800130306     C                   EVAL      I95NAR = VABNZD
050900130306     C                   EVAL      I95CAP = VABCAD
051000130306     C                   EVAL      I95LOC = VABLOD
051100130306     C                   CALL      'TISI95R'
051200130306     C                   PARM                    TISI95DS
051300130306     C                   EVAL      VABPRD = O95PRV
051400130306     C                   ENDIF
051500020305     C*
051600011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051700011113     C                   EXSR      CHKIMPDIV
051800010202     C*
051900000801     C* Ebbene...
052000000801     C                   ADD       1             §CTRMO
052100070530     C                   IF        *in31 <> *zeros OR
052200070530     C                             *in32 <> *zeros
052300000801     C                   ADD       1             §CTRNO
052400020725     C                   EVAL      x_vinflg = '2'
052500000801     C                   ELSE
052600010201     C                   ADD       1             §CTROKVB
052700000801     C                   ENDIF
052800000801     C*
052900000801     C                   ENDSR
053000010201     C*----------------------------------------------------*
053100131009     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
053200010201     C*----------------------------------------------------*
053300131009     C     WRIVAT_A      BEGSR
053400090204     C*
053500090204     C                   eval      vatAAS = vabAAS
053600090204     C                   eval      vatLNP = vabLNP
053700090204     C                   eval      vatNRS = vabNRS
053800090204     C                   eval      vatNSP = vabNSP
053900010201     C*
054000101007     C* Valorizzo il buffer di scrittura del FIVAT
054100131009     C* - TIPO RECORD "A"
054200131009     C                   if        %trim(%subst(vindta:246:30)) <> *blanks
054300131009     C                   eval      VATTRC = 'A'
054400131009     C                   eval      VATNOT = %trim(%subst(vindta:246:30))
054500021113     C                   write     FIVAT000
054600020725     C                   endif
054700010201     C*
054800010201     C                   ENDSR
054900131009     C*----------------------------------------------------*
055000131009     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
055100131009     C*----------------------------------------------------*
055200131009     C     WRIVAT_I      BEGSR
055300131009     C*
055400131009     C                   eval      vatAAS = vabAAS
055500131009     C                   eval      vatLNP = vabLNP
055600131009     C                   eval      vatNRS = vabNRS
055700131009     C                   eval      vatNSP = vabNSP
055800131009     C*
055900131009     C* Valorizzo il buffer di scrittura del FIVAT
056000131009     C* - TIPO RECORD "I"
056100131009     C                   EVAL      w70=%trim(%subst(vindta:276:030))
056200131009     C                   if        %subst(w70:001:35) <> *blanks
056300131009     C                   eval      VATTRC = 'I'
056400131009     C                   eval      VATNOT = %trim(%subst(w70:1:35))
056500131009     C                   write     FIVAT000
056600131009     C                   endif
056700131009     C*
056800131009     C                   ENDSR
056900131009     C*----------------------------------------------------*
057000131009     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "J"
057100131009     C*----------------------------------------------------*
057200131009     C     WRIVAT_J      BEGSR
057300131009     C*
057400131009     C                   eval      vatAAS = vabAAS
057500131009     C                   eval      vatLNP = vabLNP
057600131009     C                   eval      vatNRS = vabNRS
057700131009     C                   eval      vatNSP = vabNSP
057800131009     C*
057900131009     C* Valorizzo il buffer di scrittura del FIVAT
058000131009     C* - TIPO RECORD "J"
058100131009     C                   if        %subst(w70:36:35) <> *blanks
058200131009     C                   eval      VATTRC = 'J'
058300131009     C                   eval      VATNOT = %trim(%subst(w70:36:35))
058400131009     C                   write     FIVAT000
058500131009     C                   endif
058600131009     C*
058700131009     C                   ENDSR
058800071003     C*----------------------------------------------------*
058900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059000071003     C*----------------------------------------------------*
059100071003     C     WRIVAT_E      BEGSR
059200090204     C*
059300090204     C                   eval      vatAAS = vabAAS
059400090204     C                   eval      vatLNP = vabLNP
059500090204     C                   eval      vatNRS = vabNRS
059600090204     C                   eval      vatNSP = vabNSP
059700071003     C*
059800101007     C* Valorizzo il buffer di scrittura del FIVAT
059900071003     C* - TIPO RECORD "E"
060000131009     C                   if        %trim(%subst(vindta:017:10)) <> *blanks
060100130306     C                   eval      VATTRC = 'E'
060200131009     C                   eval      VATNOT = %trim(%subst(vindta:017:010))
060300071003     C                   write     FIVAT000
060400131009     C                   endif
060500071003     C*
060600071003     C                   ENDSR
060700020904
060800020904
060900010202     C*----------------------------------------------------*
061000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061100010202     C*----------------------------------------------------*
061200020305     C     PREVAT        BEGSR
061300010202     C*
061400021113     C* Compongo il nome del membro da dare al FIVATWWR
061500010202     C                   eval      parmbr = vlrhdl
061600010202     C                   movel     'M'           parmbr
061700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061800010202     C                   eval      paropz = '1'
061900010202     C* Effettuo la chiamata al CLLE preposto
062000050128     C                   call(e)   'TITVVTC'
062100010202     C                   parm                    parccm
062200010202     C                   parm                    parmbr
062300010202     C                   parm                    paropz
062400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062500010202     C                   if        %error
062600010202     C                   movel     '1'           chkcall
062700010202     C                   else
062800010202     C                   movel     '0'           chkcall
062900010202     C                   endif
063000010202     C*
063100010202     C                   ENDSR
063200000801     C*----------------------------------------------------*
063300000801     C*  CONTROLLO NUMERICITA' CAMPI
063400000801     C*----------------------------------------------------*
063500000801     C     CHKNUM        BEGSR
063600081113     C*
063700081113     C                   IF        PiDecChr = *blanks
063800121109     C                   EVAL      PiDecChr = ','
063900081113     C                   ENDIF
064000081113     C*
064100081113     C                   callp(e)  UBISNUM_Check(PiStr
064200081113     C                                          :PiDecChr
064300081113     C                                          :PiVal
064400081113     C                                          :PiNum
064500081113     C                                          :PiInt)
064600081113     C*
064700000801     C                   IF        %error
064800000801     C                   EVAL      PiInt=*off
064900000801     C                   ENDIF
065000000801     C*
065100000801     C                   ENDSR
065200000801     C***
065300000801
065400011113
065500011113     C*----------------------------------------------------*
065600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065700011113     C*----------------------------------------------------*
065800011113     C     CHKIMPDIV     BEGSR
065900011113     C*
066000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066100011113     C                   Z-ADD     *zeros        wrkDec            9 9
066200011113     C*
066300011113     C* Come prima cosa effettuo considerazioni sulla divisa
066400011113     C                   IF        vabIAS > *zeros
066500011113     C                   IF        vabVAS <> 'EUR'
066600011113     C                   EVAL      vabVAS =  'ITL'
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C                   IF        vabCAS > *zeros
067100011113     C                   IF        vabVCA <> 'EUR'
067200011113     C                   EVAL      vabVCA =  'ITL'
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C                   IF        vabVMD > *zeros
067700020305     C                   IF        vabVAD <> 'EUR'
067800011113     C                   EVAL      vabVAD =  'ITL'
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068300011113     C                   Z-ADD     vabIAS        wrkDec
068400011113     C                   IF        wrkDec > *zeros
068500011113     C                   IF        vabVAS = 'ITL'
068600011113     C                   EVAL      vabIAS = *zeros
068700011113     C                   ENDIF
068800011113     C                   ENDIF
068900011113     C*
069000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069100011113     C                   Z-ADD     vabCAS        wrkDec
069200011113     C                   IF        wrkDec > *zeros
069300011113     C                   IF        vabVCA = 'ITL'
069400011113     C                   EVAL      vabCAS = *zeros
069500011113     C                   ENDIF
069600011113     C                   ENDIF
069700011113     C*
069800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069900011113     C                   Z-ADD     vabVMD        wrkDec
070000011113     C                   IF        wrkDec > *zeros
070100011113     C                   IF        vabVAD = 'ITL'
070200011113     C                   EVAL      vabVMD = *zeros
070300011113     C                   ENDIF
070400011113     C                   ENDIF
070500011113     C*
070600011113     C                   ENDSR
070700011113     C***
070800011113
070900011113
071000000801
071100000801
071200990920      /TITLE Invio dei dati al punto operativo.
071300010202     C     invio         BEGSR
071400990920     C*
071500021113     C* 1° invio FIVAT
071600010201     C                   reset                   dscmz
071700010201     C                   move      vlrpoi        cmzdst
071800021113     C                   eval      cmzfld = 'FIVATWWR'
071900010201     C                   eval      cmzmbd = vlrhdl
072000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072100021009     C***                if        prmfir = *blanks
072200021113     C                   eval      cmzfla = 'FIVAT00F'
072300021113     C                   eval      cmzmba = 'FIVAT00F'
072400021009     C***                else
072500021009     C***                eval      cmzfla = prmfir
072600021009     C***                eval      cmzmba = prmfir
072700021009     C***                endif
072800010201     C                   eval      cmznrr = *zeros
072900020305     C                   move      §ctrokvt      cmznrr
073000021018     C                   eval      cmzlba = vlrfl1
073100010201     C                   call(e)   'TIS711C'
073200010201     C                   parm                    dscmz
073300010201     C                   parm      *blanks       esito
073400010205     C                   if        %error
073500010205     C                             or cmzerr = '1'
073600010205     C                             or esito  = '1'
073700010205     C                   eval      wrkesito = '3'
073800010205     C                   else
073900010201     C*
074000021113     C* 2° invio FIVAB
074100010201     C                   reset                   dscmz
074200010201     C                   move      vlrpoi        cmzdst
074300010201     C                   eval      cmzfld = vlrfou
074400010201     C                   eval      cmzmbd = vlrhdl
074500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074600021009     C***                if        prmfir = *blanks
074700021113     C                   eval      cmzfla = 'FIVAB00F'
074800021113     C                   eval      cmzmba = 'FIVAB00F'
074900021009     C***                else
075000021009     C***                eval      cmzfla = prmfir
075100021009     C***                eval      cmzmba = prmfir
075200021009     C***                endif
075300010201     C                   eval      cmznrr = *zeros
075400010201     C                   move      §ctrokvb      cmznrr
075500021018     C                   eval      cmzlba = vlrfl1
075600010201     C                   call(e)   'TIS711C'
075700010201     C                   parm                    dscmz
075800010201     C                   parm      *blanks       esito
075900010201     C                   if        %error
076000010201     C                             or cmzerr = '1'
076100010201     C                             or esito  = '1'
076200010201     C                   eval      wrkesito = '3'
076300010201     C                   endif
076400010205     C                   endif
076500990920     C*
076600000613     C                   ENDSR
076700000613     C***
076800070411
076900070411     C     *pssr         BEGSR
077000070411     C*
077100070411     C                   if        %open(tivin00r)
077200070411     C                   close     tivin00r
077300070411     C                   endif
077400070411     C                   if        %open(fivabwwr)
077500070411     C                   close     fivabwwr
077600070411     C                   endif
077700070411     C                   if        %open(fivatwwr)
077800070411     C                   close     fivatwwr
077900070411     C                   endif
078000070411     C*
078100070411     C* Effettuo la chiamata al CLLE preposto
078200070411     C                   call(e)   'TITVVTC'
078300070411     C                   parm                    parccm
078400070411     C                   parm                    parmbr
078500070411     C                   parm      '2'           paropz
078600070411     C*
078700070411     C                   eval      wrkesito = '2'
078800070411     C*
078900070411     C                   seton                                        LR
079000070411     C*
079100070411     C                   ENDSR     '*CANCL'
079200070411     C***
079300070411
079400990910
079500000613     C     *inzsr        BEGSR
079600990910     C*
079700990910     C     *entry        plist
079800990920     C                   parm                    tivlrds
079900990921     C                   parm      wrkesito      esito
080000000724     C                   parm                    prmlit
080100000710     C                   parm                    prmfir
080200000613     C*
080300000830     C* CALCOLA LA DATA CORRENTE
080400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
080500100324     C                   eval      datcor = %dec(%date() : *ISO)
080600000830     C*
080700000613     C                   ENDSR
080800000613     C***
