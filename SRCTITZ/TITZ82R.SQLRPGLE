000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130416      * TSP di default 'C', se il cliente passa qualcosa che non sia ' ', va preso questo dato.
000500130416      * TC1 di default ' ', se il cliente passa ' ' o 'A' vanno salvati questi dati.
000600130416      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001000021113     FFIVABwwr  O    E             DISK    usropn
001100021113     FFIVATwwr  O    E             DISK    usropn
001200990908
001300000801     D*----------------------------------------------------
001400000801     D* DICHIARAZIOINE VARIABILI DI WRK
001500000801     D*----------------------------------------------------
001600990920     D dscmz         e ds                  inz
001700990910     D psds           sds
001800990910     D  procname         *PROC
001900990920     D tivlrds       e ds                  extname(tivlr00f)
002000070730     D tisi95ds      e ds
002100990910     D esito           s              1
002200000724     D prmlit          s             10
002300000710     D prmfir          s             10
002400990921     D wrkesito        s                   like(esito)
002500000613     D rrnum           s              6  0 INZ(*zeros)
002600130405     D depspe          s             15    INZ(*blanks)
002700130405     D curspe          s             15    INZ(*blanks)
002800010202     D parccm          s              8    INZ(*blanks)
002900010202     D parmbr          s             10    INZ(*blanks)
003000010202     D paropz          s              1    INZ(*blanks)
003100010202     D chkcall         s              1    INZ(*blanks)
003200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003300070530     D Num5_0          s              5  0
003400121011     D w70             s             70
003500000830
003600020725
003700020725     D*------------------
003800020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
003900020725     D*------------------
004000070530     D INPUT_DS        DS                  INZ
004100101007     D  VINDTA                     2048
004200101007     D  VINFLG                        1
004300130405     D  VINSPE                       15
004400101007     D  VINRRN                        8  0
004500020725     D*
004600080923     D*------------------
004700080923     D* DS REPERIMENTO NUMERATORE
004800080923     D*------------------
004900080923     D trul33ds      e ds                  inz
005000080923     D*------------------
005100080923     D* DS ARCHITETTURA
005200080923     D*------------------
005300080923     D kpjba         e ds                  inz
005400080923     D*------------------
005500121126     D BarcodeDS       DS           770    INZ
005600121126     D  wBarcode                     14    dim(55)
005700121109     D*------------------
005800121109     D TRUL28DSX     e ds                  inz
005900121109     D*------------------
006000130405     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100130405     D*------------------
006200130405     D WLBDA8          DS                  INZ
006300130405     D  G08DAT                 1      8  0
006400130405     D  G08INV                 9     16  0
006500130405     D  G08ERR                17     17
006600130405     D  G08TGI                18     22  0
006700130405     D*
006800121109     D wX              s              3s 0
006900081113
007000081113     D*------------------
007100081113     D* LINKING A DEFINIZIONI ESTERNE
007200081113     D*------------------
007300100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007400081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007500081113
007600990908
007700010201
007800010201
007900080117     C*
008000080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008100080117     C
008200080117     C/EXEC SQL
008300080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008400080117     C/END-EXEC
008500080117     C
008600000913     C                   reset                   rrnum
008700990921     C                   reset                   esito
008800990921     C                   reset                   wrkesito
008900000613     C*
009000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
009100000613     C*
009200010202     C* Effettuo la chiamata al CLLE preposto
009300050128     C                   call(e)   'TITVVTC'
009400010202     C                   parm                    parccm
009500010202     C                   parm                    parmbr
009600010202     C                   parm      '2'           paropz
009700070730     C*
009800070730     C* Effettuo lancio TISI95 solo x chiusura
009900070730     C                   CLEAR                   TISI95DS
010000070730     C                   EVAL      I95TLA = 'C'
010100070730     C                   CALL      'TISI95R'
010200070730     C                   PARM                    TISI95DS
010300000616     C*
010400000801     C
010500010201     C                   seton                                        LR
010600000801
010700910830     C*--------------------------------------------------------
010800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010900910830     C*--------------------------------------------------------
011000070530     C     RWFILE        BEGSR
011100990910     C*
011200990914     C                   if        not %open(tivin00r)
011300990908     C                   open      tivin00r
011400990914     C                   endif
011500021113     C                   if        not %open(fivabwwr)
011600021113     C                   open      fivabwwr
011700990914     C                   endif
011800070530     C*
011900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
012000020305     C                   exsr      prevat
012100010201     C*
012200010202     C                   if        chkcall = '0'
012300010202     C*
012400021113     C                   if        not %open(fivatwwr)
012500021113     C                   open      fivatwwr
012600010201     C                   endif
012700080117     C*
012800080117     C                   EXSR      INZVAR
012900080117     C                   EXSR      DEFCAM
013000990910     C*
013100010201     C                   clear                   §CTROKVB          5 0
013200020305     C                   clear                   §CTROKVT          5 0
013300000801     C                   clear                   §CTRMO            5 0
013400000801     C                   clear                   §CTRNO            5 0
013500990910     C*
013600020725     C/EXEC SQL
013700020725     C+ declare C1 cursor for select
013800130405     C+ vindta, vinflg, substr(vindta, 24, 15) as sped, rrn(tivin00r)
013900060223     C+ from tivin00r
014000060223     C+ order by sped
014100020725     C+ for read only
014200020725     C/END-EXEC
014300020725     C
014400020725     C/EXEC SQL
014500020725     C+ open C1
014600020725     C/END-EXEC
014700020725     C
014800020725     C/EXEC SQL
014900070530     C+ Fetch C1 into :INPUT_DS
015000020725     C/END-EXEC
015100020725     C*
015200020725     C                   dow       sqlcod = *zeros
015300990913     C*
015400020725     C                   if        vindta > *blanks
015500000613     C                   add       1             rrnum
015600000801     C*
015700020725     C                   if        vinflg = *blanks
015800020725     C                             or vinflg = '0'
015900020725     C                             or vinflg = '2'
016000000801     C*
016100020725     C                   clear                   x_vinmsg
016200020725     C                   eval      x_vinflg = '1'
016300010305     C*
016400121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
016500121109     C* (uso l'RMN)
016600130415     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
016700020305     C                   MOVEL(p)  PiStr         curspe
016800010305     C*
016900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
017000080923     C                   if        *in50 = *off
017100071003     C                   exsr      impvab
017200071003     C                   exsr      wrivab
017300130306     C                   exsr      wrivat_b                                     => carico VAT
017400130415     C                   exsr      wrivat_i                                     => carico VAT
017500130415     C                   exsr      wrivat_j                                     => carico VAT
017600071003     C                   else
017700080923     C*
017800071009     C                   if        wDISK = 'D'
017900071009     C                   exsr      impvab
018000071009     C                   exsr      wrivab
018100130306     C                   exsr      wrivat_b                                     => carico VAT
018200130306     C***                exsr      wrivat_e                                     => carico VAT
018300130415     C                   exsr      wrivat_i                                     => carico VAT
018400130415     C                   exsr      wrivat_j                                     => carico VAT
018500071009     C                   else
018600071009     C*
018700010305     C                   if        depspe = *blanks                             => 1° giro
018800010305     C                   eval      depspe = curspe                              => memorizz. spediz
018900080117     C                   clear                   tivinds
019000020305     C                   exsr      impvab
019100130306     C                   exsr      wrivat_b                                     => carico VAT
019200130306     C***50              exsr      wrivat_e                                     => carico VAT
019300130415     C                   exsr      wrivat_i                                     => carico VAT
019400130415     C                   exsr      wrivat_j                                     => carico VAT
019500010305     C                   else
019600020725     C                   if        depspe <> curspe                             => rottura di spediz
019700010305     C                   eval      depspe = curspe                              => memorizz. spediz
019800070928     C                   exsr      wrivab
019900080117     C                   clear                   tivinds
020000020305     C                   exsr      impvab
020100130306     C                   exsr      wrivat_b                                     => carico VAT
020200130306     C***50              exsr      wrivat_e                                     => carico VAT
020300130415     C                   exsr      wrivat_i                                     => carico VAT
020400130415     C                   exsr      wrivat_j                                     => carico VAT
020500020305     C                   else                                                   => x stessa spediz
020600090204     C***                exsr      impvab
020700130306     C                   exsr      wrivat_b                                     => carico VAT
020800130306     C***50              exsr      wrivat_e                                     => carico VAT
020900130415     C                   exsr      wrivat_i                                     => carico VAT
021000130415     C                   exsr      wrivat_j                                     => carico VAT
021100010305     C                   endif
021200010305     C                   endif
021300010305     C                   endif
021400071003     C                   endif
021500071009     C                   endif
021600000905     C*
021700000905     C                   else
021800020725     C                   eval      x_vinflg = '1'
021900000905     C                   endif
022000000905     C*
022100020725     C     VINRRN        chain     tivin000
022200020725     C                   if        %found(tivin00r)
022300020725     C                   eval      y_vinflg = x_vinflg
022400020725     C                   eval      y_vinmsg = x_vinmsg
022500020725     C                   update    tivin000
022600020725     C                   endif
022700020725     C*
022800020725     C/EXEC SQL
022900070530     C+ Fetch C1 into :INPUT_DS
023000020725     C/END-EXEC
023100020725     C*
023200020725     C                   enddo
023300020725     C*
023400020725     C/EXEC SQL
023500020725     C+ close C1
023600020725     C/END-EXEC
023700000905     C*
023800020305     C* Scarico i VAB rimasti "in sospeso"
023900071009     C                   if        wDISK = 'C'
024000071009     C                   exsr      wrivab
024100071009     C                   endif
024200010202     C*
024300010202     C                   endif
024400990910
024500990910     C* Se non ci sono record con errori ...
024600000710     C                   if        §ctrno = 0
024700990910     C* ... restituisco esito OK.
024800990921     C                   eval      wrkesito = '0'
024900990910     C                   else
025000010201     C                   if        §ctrokvb > 0
025100990921     C                   eval      wrkesito = '1'
025200000710     C                   else
025300000710     C                   eval      wrkesito = '2'
025400990910     C                   endif
025500000710     C                   endif
025600990910     C*
025700990914     C                   if        %open(tivin00r)
025800990908     C                   close     tivin00r
025900990914     C                   endif
026000021113     C                   if        %open(fivabwwr)
026100021113     C                   close     fivabwwr
026200990914     C                   endif
026300021113     C                   if        %open(fivatwwr)
026400021113     C                   close     fivatwwr
026500010201     C                   endif
026600990910     C*
026700010201     C                   if        §ctrokvb > 0
026800000724     C                             and vlrpoi <> *zeros
026900010202     C                   exsr      invio
027000990920     C                   endif
027100990920     C*
027200910830     C                   ENDSR
027300000613     C***
027400010305
027500010305     C*----------------------------------------------------*
027600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
027700010305     C*----------------------------------------------------*
027800020305     C     WRIVAB        BEGSR
027900070730     C*
028000071003     C* Considerazioni finali
028100071003     C                   if        VABRMA = *blanks
028200071003     C                   movel(P)  VABRMN        VABRMA
028300071003     C                   endif
028400071003     C*
028500021113     C                   write     fivab000                                     => scarico il VAB
028600010305     C*
028700010305     C                   ENDSR
028800990920
028900000801     C*----------------------------------------------------*
029000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
029100000801     C*----------------------------------------------------*
029200010201     C     INZVAR        BEGSR
029300000801     C*
029400010201     C                   Z-ADD     *zeros        Num5_0
029500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
029600020725     C                   MOVEL     '0'           FlgCAS            1
029700000801     C*
029800000801     C                   ENDSR
029900000801     C*----------------------------------------------------*
030000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
030100000801     C*----------------------------------------------------*
030200020904     C     DEFCAM        BEGSR
030300080923     C*
030400080923     C                   SETOFF                                       5051
030500000801     C*
030600021113     C                   CLEAR                   FIVAB000
030700021113     C                   CLEAR                   FIVAT000
030800070730     C* Imposto i valori di default...
030900130415     C                   EVAL      VABCCM = 2023133
031000130415     C                   EVAL      VATCCM = 2023133
031100130415     C                   EVAL      VABLNP = 202
031200130415     C                   EVAL      VATLNP = 202
031300130415     C                   EVAL      VABCTR = 001
031400130405     C                   EVAL      VABCBO = '1'
031500130415     C                   EVAL      VABTSP = 'C'
031600070222     C* ... e poi verifico se sono stati passati come parametri
031700070222     C                   IF        vlrppt > *blanks
031800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
031900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
032000070222     C                   EXSR      CHKNUM
032100070222     C                   IF        PiInt=*on
032200070222     C                   Z-ADD     PiVal         VABCCM
032300070222     C                   Z-ADD     PiVal         VATCCM
032400070222     C                   ENDIF
032500070222     C                   ENDIF
032600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
032700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
032800070222     C                   EXSR      CHKNUM
032900070222     C                   IF        PiInt=*on
033000070222     C                   Z-ADD     PiVal         VABLNP
033100070222     C                   Z-ADD     PiVal         VATLNP
033200070222     C                   ENDIF
033300070222     C                   ENDIF
033400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
033500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
033600070222     C                   EXSR      CHKNUM
033700070222     C                   IF        PiInt=*on
033800070222     C                   Z-ADD     PiVal         VABCTR
033900070222     C                   ENDIF
034000070928     C                   ENDIF
034100071009     C                   MOVEL     *blanks       wDISK             1
034200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
034300071009     C                   IF        wDISK <> *blanks
034400070928     C                   SETON                                        50
034500070222     C                   ENDIF
034600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
034700080923     C                   SETON                                        51
034800080923     C                   ENDIF
034900070222     C                   ENDIF
035000071009     C*
035100071009     C   50              EVAL      VABCTM = '7Q'
035200000801     C*
035300000801     C                   ENDSR
035400000801     C*----------------------------------------------------*
035500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
035600000801     C*----------------------------------------------------*
035700010201     C     IMPVAB        BEGSR
035800070530     C*
035900070530     C                   SETOFF                                       3132
036000070928     C*
036100070928     C                   EXSR      INZVAR
036200070928     C                   EXSR      DEFCAM
036300070928     C*
036400070928     C                   MOVE(P)   vlrpoi        VABFGS
036500070928     C                   MOVE(P)   vlrpoi        VATFGS
036600130404     C                   MOVEL     datcor        VABAAS
036700130404     C                   MOVE      datcor        VABMGS
036800130415     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
036900070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
037000070928     C     '@':'A'       XLATE     VABRSD        VABRSD
037100070928     C* ==
037200130415     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
037300130415     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
037400130415     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
037500130415     C                   EVAL      VABRMA=%trim(%subst(vindta:108:10))
037600130415     C                   IF        %trim(%subst(vindta:365:1)) <> *blank
037700130415     C                   EVAL      VABTSP=%trim(%subst(vindta:365:1))
037800130415     C                   ENDIF
037900130415     C                   IF        %trim(%subst(vindta:366:1)) = *blank or
038000130415     C                             %trim(%subst(vindta:366:1)) = 'A'
038100130415     C                   EVAL      VABTC1=%trim(%subst(vindta:366:1))
038200130415     C                   ENDIF
038300130415     C                   EVAL      w70=%trim(%subst(vindta:245:100))
038400120717     C*
038500071003     C* CAD
038600130415     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
038700121001     C                   EXSR      CHKNUM
038800121001     C                   IF        PiInt=*on
038900121001     C                   Z-ADD     PiVal         Num5_0
039000121001     C                   MOVEL     Num5_0        VABCAD
039100121001     C                   ELSE
039200121001     C                   SETON                                        32
039300121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039400121001     C                             + ' ' + 'VABCAD'
039500121001     C                   ENDIF
039600080923     C* RMN
039700130415     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
039800121001     C                   EXSR      CHKNUM
039900121001     C                   IF        PiInt=*on
040000121001     C                   Z-ADD     PiVal         VABRMN
040100121001     C                   ELSE
040200121001     C                   SETON                                        32
040300121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040400121001     C                             + ' ' + 'VABRMN'
040500121001     C                   ENDIF
040600080923     C* NSP
040700080923     C                   IF        *in51 = *on
040800080923     C* NSP => Stacco un numeratore da AZNUM
040900080923     C                   clear                   TRUL33DS
041000080923     C                   eval      I33OPE = *zeros
041100080923     C                   eval      I33CNU = 302
041200080923     C                   eval      I33NUM = 1
041300080923     C                   movel     TRUL33DS      KPJBU
041400080923     C                   call      'TRUL33R'
041500080923     C                   parm                    KPJBA
041600080923     C                   movel     KPJBU         TRUL33DS
041700080923     C                   if        O33ERR = *zeros
041800080923     C                   z-add     O33NRF        VABNSP
041900080923     C                   z-add     O33NRF        VATNSP
042000080923     C                   else
042100080923     C                   SETON                                        31
042200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042300080923     C                             + ' ' + 'VABNSP VATNSP'
042400080923     C                   endif
042500080923     C                   ELSE
042600130404     C                   EVAL      PiStr=%trim(%subst(vindta:24:15))
042700130306     C                   EXSR      CHKNUM
042800130306     C                   IF        PiInt=*on
042900130306     C                   Z-ADD     PiVal         VABNSP
043000130306     C                   Z-ADD     PiVal         VATNSP
043100130306     C                   ELSE
043200130306     C                   SETON                                        31
043300130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400130306     C                             + ' ' + 'VABNSP VATNSP'
043500130306     C                   ENDIF
043600080923     C                   ENDIF
043700071003     C* NCL
043800130415     C                   EVAL      PiStr=%trim(%subst(vindta:124:2))
043900070928     C                   EXSR      CHKNUM
044000070928     C                   IF        PiInt=*on
044100070928     C                   Z-ADD     PiVal         VABNCL
044200070928     C                   ELSE
044300070928     C                   SETON                                        32
044400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044500070928     C                             + ' ' + 'VABNCL'
044600070928     C                   ENDIF
044700071003     C* PKB
044800130415     C                   EVAL      PiStr=%trim(%subst(vindta:131:5))
044900070928     C                   EXSR      CHKNUM
045000070928     C                   IF        PiNum=*on
045100130415     C* il dato ha 1 decimale, ma è    separato da (,)
045200130415     C                   EVAL(H)   VABPKB = PiVal
045300070928     C                   ELSE
045400070928     C                   SETON                                        32
045500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600070928     C                             + ' ' + 'VABPKB'
045700070928     C                   ENDIF
045800080923     C* CAS
045900130415     C* l'importo contrassegno ha 2 decimali e viene scritto con separatore decimale (,)
046000130415     C                   IF        %trim(%subst(vindta:137:10))<>*blanks
046100080923     C                             AND
046200130415     C                             %trim(%subst(vindta:137:10))<>*zeros
046300130415     C                             AND
046400130415     C                             %trim(%subst(vindta:137:10))<>'0,00'
046500130415     C                             AND
046600130415     C                             %trim(%subst(vindta:137:10))<>'0000000,00'
046700121011     C                   EVAL      VABVCA = 'EUR'
046800070928     C                   MOVEL     '1'           FlgCAS
046900130415     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
047000070928     C                   EXSR      CHKNUM
047100070928     C                   IF        PiNum=*on
047200130415     C                   EVAL(H)   VABCAS = PiVal
047300070928     C                   ELSE
047400070928     C                   SETON                                        32
047500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600070928     C                             + ' ' + 'VABCAS'
047700070928     C                   ENDIF
047800070928     C                   ENDIF
047900070730     C*
048000121109     C* Considerazioni finali su CBO/CAS
048100121106     C*
048200121106      * se c/assegno impostato
048300121106     C                   IF        VABCAS > 0
048400121109     C                   IF        VABCBO = '1'
048500121109     C                   EVAL      VABCBO = '4'
048600121109     C                   ENDIF
048700121109     C                   IF        VABCBO = '2'
048800121109     C                   EVAL      VABCBO = '6'
048900121109     C                   ENDIF
049000121106     C                   ENDIF
049100130306     C*
049200130306     C* Se provincia nn valorizzata la reperisco
049300130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
049400130306     C                   IF        VABPRD =  *blanks AND
049500130306     C                             VABNZD =  *blanks AND
049600130306     C                             VABCAD <> *blanks AND
049700130306     C                             VABLOD <> *blanks
049800130306     C                   CLEAR                   TISI95DS
049900130306     C                   EVAL      I95TCN = '3'
050000130306     C                   Z-ADD     datcor        I95DAT
050100130306     C                   EVAL      I95NAR = VABNZD
050200130306     C                   EVAL      I95CAP = VABCAD
050300130306     C                   EVAL      I95LOC = VABLOD
050400130306     C                   CALL      'TISI95R'
050500130306     C                   PARM                    TISI95DS
050600130306     C                   EVAL      VABPRD = O95PRV
050700130306     C                   ENDIF
050800020305     C*
050900011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051000011113     C                   EXSR      CHKIMPDIV
051100010202     C*
051200000801     C* Ebbene...
051300000801     C                   ADD       1             §CTRMO
051400070530     C                   IF        *in31 <> *zeros OR
051500070530     C                             *in32 <> *zeros
051600000801     C                   ADD       1             §CTRNO
051700020725     C                   EVAL      x_vinflg = '2'
051800000801     C                   ELSE
051900010201     C                   ADD       1             §CTROKVB
052000000801     C                   ENDIF
052100000801     C*
052200000801     C                   ENDSR
052300010201     C*----------------------------------------------------*
052400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052500010201     C*----------------------------------------------------*
052600071003     C     WRIVAT_B      BEGSR
052700090204     C*
052800090204     C                   eval      vatAAS = vabAAS
052900090204     C                   eval      vatLNP = vabLNP
053000090204     C                   eval      vatNRS = vabNRS
053100090204     C                   eval      vatNSP = vabNSP
053200010201     C*
053300101007     C* Valorizzo il buffer di scrittura del FIVAT
053400070928     C* - TIPO RECORD "B"
053500130415     C                   if        %subst(vindta:345:20) <> *blanks
053600070928     C                   eval      VATTRC = 'B'
053700130415     C                   eval      VATNOT = %trim(%subst(vindta:345:20))
053800021113     C                   write     FIVAT000
053900020725     C                   endif
054000010201     C*
054100010201     C                   ENDSR
054200071003     C*----------------------------------------------------*
054300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054400071003     C*----------------------------------------------------*
054500071003     C     WRIVAT_E      BEGSR
054600090204     C*
054700090204     C                   eval      vatAAS = vabAAS
054800090204     C                   eval      vatLNP = vabLNP
054900090204     C                   eval      vatNRS = vabNRS
055000090204     C                   eval      vatNSP = vabNSP
055100071003     C*
055200101007     C* Valorizzo il buffer di scrittura del FIVAT
055300071003     C* - TIPO RECORD "E"
055400130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
055500130306     C                   eval      VATTRC = 'E'
055600121109     C*
055700130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
055800071003     C                   write     FIVAT000
055900121109     C*
056000130306     C***                endif
056100071003     C*
056200071003     C                   ENDSR
056300130415     C*----------------------------------------------------*
056400130415     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
056500130415     C*----------------------------------------------------*
056600130415     C     WRIVAT_I      BEGSR
056700130415     C*
056800130415     C                   eval      vatAAS = vabAAS
056900130415     C                   eval      vatLNP = vabLNP
057000130415     C                   eval      vatNRS = vabNRS
057100130415     C                   eval      vatNSP = vabNSP
057200130415     C*
057300130415     C* Valorizzo il buffer di scrittura del FIVAT
057400130415     C* - TIPO RECORD "I"
057500130415     C                   if        %subst(w70:1:35) <> *blanks
057600130415     C                   eval      VATTRC = 'I'
057700130415     C                   eval      VATNOT = %trim(%subst(w70:1:35))
057800130415     C                   write     FIVAT000
057900130415     C                   endif
058000130415     C*
058100130415     C                   ENDSR
058200130415     C*----------------------------------------------------*
058300130415     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "J"
058400130415     C*----------------------------------------------------*
058500130415     C     WRIVAT_J      BEGSR
058600130415     C*
058700130415     C                   eval      vatAAS = vabAAS
058800130415     C                   eval      vatLNP = vabLNP
058900130415     C                   eval      vatNRS = vabNRS
059000130415     C                   eval      vatNSP = vabNSP
059100130415     C*
059200130415     C* Valorizzo il buffer di scrittura del FIVAT
059300130415     C* - TIPO RECORD "J"
059400130415     C                   if        %subst(w70:36:35) <> *blanks
059500130415     C                   eval      VATTRC = 'J'
059600130415     C                   eval      VATNOT = %trim(%subst(w70:36:35))
059700130415     C                   write     FIVAT000
059800130415     C                   endif
059900130415     C*
060000130415     C                   ENDSR
060100020904
060200020904
060300010202     C*----------------------------------------------------*
060400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060500010202     C*----------------------------------------------------*
060600020305     C     PREVAT        BEGSR
060700010202     C*
060800021113     C* Compongo il nome del membro da dare al FIVATWWR
060900010202     C                   eval      parmbr = vlrhdl
061000010202     C                   movel     'M'           parmbr
061100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061200010202     C                   eval      paropz = '1'
061300010202     C* Effettuo la chiamata al CLLE preposto
061400050128     C                   call(e)   'TITVVTC'
061500010202     C                   parm                    parccm
061600010202     C                   parm                    parmbr
061700010202     C                   parm                    paropz
061800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061900010202     C                   if        %error
062000010202     C                   movel     '1'           chkcall
062100010202     C                   else
062200010202     C                   movel     '0'           chkcall
062300010202     C                   endif
062400010202     C*
062500010202     C                   ENDSR
062600000801     C*----------------------------------------------------*
062700000801     C*  CONTROLLO NUMERICITA' CAMPI
062800000801     C*----------------------------------------------------*
062900000801     C     CHKNUM        BEGSR
063000081113     C*
063100081113     C                   IF        PiDecChr = *blanks
063200121109     C                   EVAL      PiDecChr = ','
063300081113     C                   ENDIF
063400081113     C*
063500081113     C                   callp(e)  UBISNUM_Check(PiStr
063600081113     C                                          :PiDecChr
063700081113     C                                          :PiVal
063800081113     C                                          :PiNum
063900081113     C                                          :PiInt)
064000081113     C*
064100000801     C                   IF        %error
064200000801     C                   EVAL      PiInt=*off
064300000801     C                   ENDIF
064400000801     C*
064500000801     C                   ENDSR
064600000801     C***
064700000801
064800011113
064900011113     C*----------------------------------------------------*
065000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065100011113     C*----------------------------------------------------*
065200011113     C     CHKIMPDIV     BEGSR
065300011113     C*
065400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065500011113     C                   Z-ADD     *zeros        wrkDec            9 9
065600011113     C*
065700011113     C* Come prima cosa effettuo considerazioni sulla divisa
065800011113     C                   IF        vabIAS > *zeros
065900011113     C                   IF        vabVAS <> 'EUR'
066000011113     C                   EVAL      vabVAS =  'ITL'
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C                   IF        vabCAS > *zeros
066500011113     C                   IF        vabVCA <> 'EUR'
066600011113     C                   EVAL      vabVCA =  'ITL'
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C                   IF        vabVMD > *zeros
067100020305     C                   IF        vabVAD <> 'EUR'
067200011113     C                   EVAL      vabVAD =  'ITL'
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067700011113     C                   Z-ADD     vabIAS        wrkDec
067800011113     C                   IF        wrkDec > *zeros
067900011113     C                   IF        vabVAS = 'ITL'
068000011113     C                   EVAL      vabIAS = *zeros
068100011113     C                   ENDIF
068200011113     C                   ENDIF
068300011113     C*
068400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068500011113     C                   Z-ADD     vabCAS        wrkDec
068600011113     C                   IF        wrkDec > *zeros
068700011113     C                   IF        vabVCA = 'ITL'
068800011113     C                   EVAL      vabCAS = *zeros
068900011113     C                   ENDIF
069000011113     C                   ENDIF
069100011113     C*
069200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069300011113     C                   Z-ADD     vabVMD        wrkDec
069400011113     C                   IF        wrkDec > *zeros
069500011113     C                   IF        vabVAD = 'ITL'
069600011113     C                   EVAL      vabVMD = *zeros
069700011113     C                   ENDIF
069800011113     C                   ENDIF
069900011113     C*
070000011113     C                   ENDSR
070100011113     C***
070200011113
070300011113
070400000801
070500000801
070600990920      /TITLE Invio dei dati al punto operativo.
070700010202     C     invio         BEGSR
070800990920     C*
070900021113     C* 1° invio FIVAT
071000010201     C                   reset                   dscmz
071100010201     C                   move      vlrpoi        cmzdst
071200021113     C                   eval      cmzfld = 'FIVATWWR'
071300010201     C                   eval      cmzmbd = vlrhdl
071400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500021009     C***                if        prmfir = *blanks
071600021113     C                   eval      cmzfla = 'FIVAT00F'
071700021113     C                   eval      cmzmba = 'FIVAT00F'
071800021009     C***                else
071900021009     C***                eval      cmzfla = prmfir
072000021009     C***                eval      cmzmba = prmfir
072100021009     C***                endif
072200010201     C                   eval      cmznrr = *zeros
072300020305     C                   move      §ctrokvt      cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500010201     C                   call(e)   'TIS711C'
072600010201     C                   parm                    dscmz
072700010201     C                   parm      *blanks       esito
072800010205     C                   if        %error
072900010205     C                             or cmzerr = '1'
073000010205     C                             or esito  = '1'
073100010205     C                   eval      wrkesito = '3'
073200010205     C                   else
073300010201     C*
073400021113     C* 2° invio FIVAB
073500010201     C                   reset                   dscmz
073600010201     C                   move      vlrpoi        cmzdst
073700010201     C                   eval      cmzfld = vlrfou
073800010201     C                   eval      cmzmbd = vlrhdl
073900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074000021009     C***                if        prmfir = *blanks
074100021113     C                   eval      cmzfla = 'FIVAB00F'
074200021113     C                   eval      cmzmba = 'FIVAB00F'
074300021009     C***                else
074400021009     C***                eval      cmzfla = prmfir
074500021009     C***                eval      cmzmba = prmfir
074600021009     C***                endif
074700010201     C                   eval      cmznrr = *zeros
074800010201     C                   move      §ctrokvb      cmznrr
074900021018     C                   eval      cmzlba = vlrfl1
075000010201     C                   call(e)   'TIS711C'
075100010201     C                   parm                    dscmz
075200010201     C                   parm      *blanks       esito
075300010201     C                   if        %error
075400010201     C                             or cmzerr = '1'
075500010201     C                             or esito  = '1'
075600010201     C                   eval      wrkesito = '3'
075700010201     C                   endif
075800010205     C                   endif
075900990920     C*
076000000613     C                   ENDSR
076100000613     C***
076200070411
076300070411     C     *pssr         BEGSR
076400070411     C*
076500070411     C                   if        %open(tivin00r)
076600070411     C                   close     tivin00r
076700070411     C                   endif
076800070411     C                   if        %open(fivabwwr)
076900070411     C                   close     fivabwwr
077000070411     C                   endif
077100070411     C                   if        %open(fivatwwr)
077200070411     C                   close     fivatwwr
077300070411     C                   endif
077400070411     C*
077500070411     C* Effettuo la chiamata al CLLE preposto
077600070411     C                   call(e)   'TITVVTC'
077700070411     C                   parm                    parccm
077800070411     C                   parm                    parmbr
077900070411     C                   parm      '2'           paropz
078000070411     C*
078100070411     C                   eval      wrkesito = '2'
078200070411     C*
078300070411     C                   seton                                        LR
078400070411     C*
078500070411     C                   ENDSR     '*CANCL'
078600070411     C***
078700070411
078800990910
078900000613     C     *inzsr        BEGSR
079000990910     C*
079100990910     C     *entry        plist
079200990920     C                   parm                    tivlrds
079300990921     C                   parm      wrkesito      esito
079400000724     C                   parm                    prmlit
079500000710     C                   parm                    prmfir
079600000613     C*
079700000830     C* CALCOLA LA DATA CORRENTE
079800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079900100324     C                   eval      datcor = %dec(%date() : *ISO)
080000000830     C*
080100000613     C                   ENDSR
080200000613     C***
