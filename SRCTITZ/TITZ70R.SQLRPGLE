000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121011      *
000300121001      * PARTICOLARITA':
000400130306      * per VABRMN prendere solo parte numerica - ovvero da posizione 134 x 8 byteslunghi 14;
000500130306      * per VABRMA prendere tutto
000600130306      * CBO = 01 = PORTO FRANCO   = 1
000700130306      *       02= PORTO ASSEGNATO = 2
000800130306      *
000900121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
001000990908
001100020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001200021113     FFIVABwwr  O    E             DISK    usropn
001300021113     FFIVATwwr  O    E             DISK    usropn
001400990908
001500000801     D*----------------------------------------------------
001600000801     D* DICHIARAZIOINE VARIABILI DI WRK
001700000801     D*----------------------------------------------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100990920     D tivlrds       e ds                  extname(tivlr00f)
002200070730     D tisi95ds      e ds
002300990910     D esito           s              1
002400000724     D prmlit          s             10
002500000710     D prmfir          s             10
002600990921     D wrkesito        s                   like(esito)
002700000613     D rrnum           s              6  0 INZ(*zeros)
002800130306     D depspe          s              8    INZ(*blanks)
002900130306     D curspe          s              8    INZ(*blanks)
003000010202     D parccm          s              8    INZ(*blanks)
003100010202     D parmbr          s             10    INZ(*blanks)
003200010202     D paropz          s              1    INZ(*blanks)
003300010202     D chkcall         s              1    INZ(*blanks)
003400080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003500070530     D Num5_0          s              5  0
003600121011     D w70             s             70
003700000830
003800020725
003900020725     D*------------------
004000020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004100020725     D*------------------
004200070530     D INPUT_DS        DS                  INZ
004300101007     D  VINDTA                     2048
004400101007     D  VINFLG                        1
004500130306     D  VINSPE                        8
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
005700121126     D BarcodeDS       DS           770    INZ
005800121126     D  wBarcode                     14    dim(55)
005900121109     D*------------------
006000121109     D TRUL28DSX     e ds                  inz
006100121109     D*------------------
006200121109     D wX              s              3s 0
006300081113
006400081113     D*------------------
006500081113     D* LINKING A DEFINIZIONI ESTERNE
006600081113     D*------------------
006700100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900081113
007000990908
007100010201
007200010201
007300080117     C*
007400080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007500080117     C
007600080117     C/EXEC SQL
007700080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007800080117     C/END-EXEC
007900080117     C
008000000913     C                   reset                   rrnum
008100990921     C                   reset                   esito
008200990921     C                   reset                   wrkesito
008300000613     C*
008400070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008500000613     C*
008600010202     C* Effettuo la chiamata al CLLE preposto
008700050128     C                   call(e)   'TITVVTC'
008800010202     C                   parm                    parccm
008900010202     C                   parm                    parmbr
009000010202     C                   parm      '2'           paropz
009100070730     C*
009200070730     C* Effettuo lancio TISI95 solo x chiusura
009300070730     C                   CLEAR                   TISI95DS
009400070730     C                   EVAL      I95TLA = 'C'
009500070730     C                   CALL      'TISI95R'
009600070730     C                   PARM                    TISI95DS
009700000616     C*
009800000801     C
009900010201     C                   seton                                        LR
010000000801
010100910830     C*--------------------------------------------------------
010200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
010300910830     C*--------------------------------------------------------
010400070530     C     RWFILE        BEGSR
010500990910     C*
010600990914     C                   if        not %open(tivin00r)
010700990908     C                   open      tivin00r
010800990914     C                   endif
010900021113     C                   if        not %open(fivabwwr)
011000021113     C                   open      fivabwwr
011100990914     C                   endif
011200070530     C*
011300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011400020305     C                   exsr      prevat
011500010201     C*
011600010202     C                   if        chkcall = '0'
011700010202     C*
011800021113     C                   if        not %open(fivatwwr)
011900021113     C                   open      fivatwwr
012000010201     C                   endif
012100080117     C*
012200080117     C                   EXSR      INZVAR
012300080117     C                   EXSR      DEFCAM
012400990910     C*
012500010201     C                   clear                   §CTROKVB          5 0
012600020305     C                   clear                   §CTROKVT          5 0
012700000801     C                   clear                   §CTRMO            5 0
012800000801     C                   clear                   §CTRNO            5 0
012900990910     C*
013000020725     C/EXEC SQL
013100020725     C+ declare C1 cursor for select
013200130306     C+ vindta, vinflg, substr(vindta, 134, 8) as sped, rrn(tivin00r)
013300060223     C+ from tivin00r
013400060223     C+ order by sped
013500020725     C+ for read only
013600020725     C/END-EXEC
013700020725     C
013800020725     C/EXEC SQL
013900020725     C+ open C1
014000020725     C/END-EXEC
014100020725     C
014200020725     C/EXEC SQL
014300070530     C+ Fetch C1 into :INPUT_DS
014400020725     C/END-EXEC
014500020725     C*
014600020725     C                   dow       sqlcod = *zeros
014700990913     C*
014800020725     C                   if        vindta > *blanks
014900000613     C                   add       1             rrnum
015000000801     C*
015100020725     C                   if        vinflg = *blanks
015200020725     C                             or vinflg = '0'
015300020725     C                             or vinflg = '2'
015400000801     C*
015500020725     C                   clear                   x_vinmsg
015600020725     C                   eval      x_vinflg = '1'
015700010305     C*
015800121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015900121109     C* (uso l'RMN)
016000121109     C                   EVAL      PiStr=%trim(%subst(vindta:108:10))
016100020305     C                   MOVEL(p)  PiStr         curspe
016200010305     C*
016300071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016400080923     C                   if        *in50 = *off
016500071003     C                   exsr      impvab
016600071003     C                   exsr      wrivab
016700130306     C                   exsr      wrivat_b                                     => carico VAT
016800071003     C                   else
016900080923     C*
017000071009     C                   if        wDISK = 'D'
017100071009     C                   exsr      impvab
017200071009     C                   exsr      wrivab
017300130306     C                   exsr      wrivat_b                                     => carico VAT
017400130306     C***                exsr      wrivat_e                                     => carico VAT
017500071009     C                   else
017600071009     C*
017700010305     C                   if        depspe = *blanks                             => 1° giro
017800010305     C                   eval      depspe = curspe                              => memorizz. spediz
017900080117     C                   clear                   tivinds
018000020305     C                   exsr      impvab
018100130306     C                   exsr      wrivat_b                                     => carico VAT
018200130306     C***50              exsr      wrivat_e                                     => carico VAT
018300010305     C                   else
018400020725     C                   if        depspe <> curspe                             => rottura di spediz
018500010305     C                   eval      depspe = curspe                              => memorizz. spediz
018600070928     C                   exsr      wrivab
018700080117     C                   clear                   tivinds
018800020305     C                   exsr      impvab
018900130306     C                   exsr      wrivat_b                                     => carico VAT
019000130306     C***50              exsr      wrivat_e                                     => carico VAT
019100020305     C                   else                                                   => x stessa spediz
019200090204     C***                exsr      impvab
019300130306     C                   exsr      wrivat_b                                     => carico VAT
019400130306     C***50              exsr      wrivat_e                                     => carico VAT
019500010305     C                   endif
019600010305     C                   endif
019700010305     C                   endif
019800071003     C                   endif
019900071009     C                   endif
020000000905     C*
020100000905     C                   else
020200020725     C                   eval      x_vinflg = '1'
020300000905     C                   endif
020400000905     C*
020500020725     C     VINRRN        chain     tivin000
020600020725     C                   if        %found(tivin00r)
020700020725     C                   eval      y_vinflg = x_vinflg
020800020725     C                   eval      y_vinmsg = x_vinmsg
020900020725     C                   update    tivin000
021000020725     C                   endif
021100020725     C*
021200020725     C/EXEC SQL
021300070530     C+ Fetch C1 into :INPUT_DS
021400020725     C/END-EXEC
021500020725     C*
021600020725     C                   enddo
021700020725     C*
021800020725     C/EXEC SQL
021900020725     C+ close C1
022000020725     C/END-EXEC
022100000905     C*
022200020305     C* Scarico i VAB rimasti "in sospeso"
022300071009     C                   if        wDISK = 'C'
022400071009     C                   exsr      wrivab
022500071009     C                   endif
022600010202     C*
022700010202     C                   endif
022800990910
022900990910     C* Se non ci sono record con errori ...
023000000710     C                   if        §ctrno = 0
023100990910     C* ... restituisco esito OK.
023200990921     C                   eval      wrkesito = '0'
023300990910     C                   else
023400010201     C                   if        §ctrokvb > 0
023500990921     C                   eval      wrkesito = '1'
023600000710     C                   else
023700000710     C                   eval      wrkesito = '2'
023800990910     C                   endif
023900000710     C                   endif
024000990910     C*
024100990914     C                   if        %open(tivin00r)
024200990908     C                   close     tivin00r
024300990914     C                   endif
024400021113     C                   if        %open(fivabwwr)
024500021113     C                   close     fivabwwr
024600990914     C                   endif
024700021113     C                   if        %open(fivatwwr)
024800021113     C                   close     fivatwwr
024900010201     C                   endif
025000990910     C*
025100010201     C                   if        §ctrokvb > 0
025200000724     C                             and vlrpoi <> *zeros
025300010202     C                   exsr      invio
025400990920     C                   endif
025500990920     C*
025600910830     C                   ENDSR
025700000613     C***
025800010305
025900010305     C*----------------------------------------------------*
026000020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026100010305     C*----------------------------------------------------*
026200020305     C     WRIVAB        BEGSR
026300070730     C*
026400071003     C* Considerazioni finali
026500071003     C                   if        VABRMA = *blanks
026600071003     C                   movel(P)  VABRMN        VABRMA
026700071003     C                   endif
026800071003     C*
026900021113     C                   write     fivab000                                     => scarico il VAB
027000010305     C*
027100010305     C                   ENDSR
027200990920
027300000801     C*----------------------------------------------------*
027400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027500000801     C*----------------------------------------------------*
027600010201     C     INZVAR        BEGSR
027700000801     C*
027800010201     C                   Z-ADD     *zeros        Num5_0
027900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028000020725     C                   MOVEL     '0'           FlgCAS            1
028100000801     C*
028200000801     C                   ENDSR
028300000801     C*----------------------------------------------------*
028400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028500000801     C*----------------------------------------------------*
028600020904     C     DEFCAM        BEGSR
028700080923     C*
028800080923     C                   SETOFF                                       5051
028900000801     C*
029000021113     C                   CLEAR                   FIVAB000
029100021113     C                   CLEAR                   FIVAT000
029200070730     C* Imposto i valori di default...
029300130306     C                   EVAL      VABCCM = 0505772
029400130306     C                   EVAL      VATCCM = 0505772
029500130306     C                   EVAL      VABLNP = 050
029600130306     C                   EVAL      VATLNP = 050
029700130306     C                   EVAL      VABCTR = 000
029800070222     C* ... e poi verifico se sono stati passati come parametri
029900070222     C                   IF        vlrppt > *blanks
030000070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030200070222     C                   EXSR      CHKNUM
030300070222     C                   IF        PiInt=*on
030400070222     C                   Z-ADD     PiVal         VABCCM
030500070222     C                   Z-ADD     PiVal         VATCCM
030600070222     C                   ENDIF
030700070222     C                   ENDIF
030800070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031000070222     C                   EXSR      CHKNUM
031100070222     C                   IF        PiInt=*on
031200070222     C                   Z-ADD     PiVal         VABLNP
031300070222     C                   Z-ADD     PiVal         VATLNP
031400070222     C                   ENDIF
031500070222     C                   ENDIF
031600070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031800070222     C                   EXSR      CHKNUM
031900070222     C                   IF        PiInt=*on
032000070222     C                   Z-ADD     PiVal         VABCTR
032100070222     C                   ENDIF
032200070928     C                   ENDIF
032300071009     C                   MOVEL     *blanks       wDISK             1
032400071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032500071009     C                   IF        wDISK <> *blanks
032600070928     C                   SETON                                        50
032700070222     C                   ENDIF
032800080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032900080923     C                   SETON                                        51
033000080923     C                   ENDIF
033100130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033200130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033300130507     C                   ENDIF
033400070222     C                   ENDIF
033500071009     C*
033600071009     C   50              EVAL      VABCTM = '7Q'
033700000801     C*
033800000801     C                   ENDSR
033900000801     C*----------------------------------------------------*
034000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034100000801     C*----------------------------------------------------*
034200010201     C     IMPVAB        BEGSR
034300070530     C*
034400070530     C                   SETOFF                                       3132
034500070928     C*
034600070928     C                   EXSR      INZVAR
034700070928     C                   EXSR      DEFCAM
034800070928     C*
034900070928     C                   MOVE(P)   vlrpoi        VABFGS
035000070928     C                   MOVE(P)   vlrpoi        VATFGS
035100070928     C*
035200130306     C* AAS (deve essere preso da loro)
035300130306     C                   EVAL      PiStr=%trim(%subst(vindta:142:4))
035400130306     C                   EXSR      CHKNUM
035500130306     C                   IF        PiInt=*on
035600130306     C                   Z-ADD     PiVal         VABAAS
035700130306     C                   Z-ADD     PiVal         VATAAS
035800130306     C                   ELSE
035900130306     C                   SETON                                        31
036000130306     C                   Z-ADD     *zeros        VATAAS
036100130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
036200130306     C                             + ' ' + 'VABAAS VATAAS'
036300130306     C                   ENDIF
036400070928     C                   MOVE      datcor        VABMGS
036500130306     C                   EVAL      VABRSD=%trim(%subst(vindta:3:30))
036600070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
036700070928     C     '@':'A'       XLATE     VABRSD        VABRSD
036800070928     C* ==
036900130306     C                   EVAL      VABIND=%trim(%subst(vindta:33:30))
037000130306     C                   EVAL      VABLOD=%trim(%subst(vindta:63:30))
037100130306     C                   EVAL      VABRMA=%trim(%subst(vindta:132:10))
037200130306     C                   IF        %trim(%subst(vindta:1:2)) = '01'
037300130306     C                   EVAL      VABCBO='1'
037400121109     C                   ENDIF
037500130306     C                   IF        %trim(%subst(vindta:1:2)) = '02'
037600130306     C                   EVAL      VABCBO='2'
037700130306     C                   ENDIF
037800130306     C                   EVAL      w70=%trim(%subst(vindta:311:70))
037900130306     C                   EVAL      VABNOT=%subst(w70:1:35)
038000130306     C                   EVAL      VABNT2=%subst(w70:36:35)
038100120717     C*
038200071003     C* CAD
038300130306     C                   EVAL      PiStr=%trim(%subst(vindta:93:5))
038400121001     C                   EXSR      CHKNUM
038500121001     C                   IF        PiInt=*on
038600121001     C                   Z-ADD     PiVal         Num5_0
038700121001     C                   MOVEL     Num5_0        VABCAD
038800121001     C                   ELSE
038900121001     C                   SETON                                        32
039000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039100121001     C                             + ' ' + 'VABCAD'
039200121001     C                   ENDIF
039300080923     C* RMN
039400130306     C                   EVAL      PiStr=%trim(%subst(vindta:134:8))
039500121001     C                   EXSR      CHKNUM
039600121001     C                   IF        PiInt=*on
039700121001     C                   Z-ADD     PiVal         VABRMN
039800121001     C                   ELSE
039900121001     C                   SETON                                        32
040000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040100121001     C                             + ' ' + 'VABRMN'
040200121001     C                   ENDIF
040300080923     C* NSP
040400080923     C                   IF        *in51 = *on
040500080923     C* NSP => Stacco un numeratore da AZNUM
040600080923     C                   clear                   TRUL33DS
040700080923     C                   eval      I33OPE = *zeros
040800080923     C                   eval      I33CNU = 302
040900080923     C                   eval      I33NUM = 1
041000080923     C                   movel     TRUL33DS      KPJBU
041100080923     C                   call      'TRUL33R'
041200080923     C                   parm                    KPJBA
041300080923     C                   movel     KPJBU         TRUL33DS
041400080923     C                   if        O33ERR = *zeros
041500080923     C                   z-add     O33NRF        VABNSP
041600080923     C                   z-add     O33NRF        VATNSP
041700080923     C                   else
041800080923     C                   SETON                                        31
041900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042000080923     C                             + ' ' + 'VABNSP VATNSP'
042100080923     C                   endif
042200080923     C                   ELSE
042300130306     C                   EVAL      PiStr=%trim(%subst(vindta:134:8))
042400130306     C                   EXSR      CHKNUM
042500130306     C                   IF        PiInt=*on
042600130306     C                   Z-ADD     PiVal         VABNSP
042700130306     C                   Z-ADD     PiVal         VATNSP
042800130306     C                   ELSE
042900130306     C                   SETON                                        31
043000130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100130306     C                             + ' ' + 'VABNSP VATNSP'
043200130306     C                   ENDIF
043300080923     C                   ENDIF
043400071003     C* NCL
043500130306     C                   EVAL      PiStr=%trim(%subst(vindta:100:4))
043600070928     C                   EXSR      CHKNUM
043700070928     C                   IF        PiInt=*on
043800070928     C                   Z-ADD     PiVal         VABNCL
043900070928     C                   ELSE
044000070928     C                   SETON                                        32
044100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200070928     C                             + ' ' + 'VABNCL'
044300070928     C                   ENDIF
044400071003     C* PKB
044500130306     C                   EVAL      PiStr=%trim(%subst(vindta:112:8))
044600070928     C                   EXSR      CHKNUM
044700070928     C                   IF        PiNum=*on
044800130306     C* il dato ha 2 decimali
044900130306     C                   EVAL(H)   VABPKB = PiVal/100
045000070928     C                   ELSE
045100070928     C                   SETON                                        32
045200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045300070928     C                             + ' ' + 'VABPKB'
045400070928     C                   ENDIF
045500130306     C* VLB
045600130306     C                   EVAL      PiStr=%trim(%subst(vindta:104:8))
045700130306     C                   EXSR      CHKNUM
045800130306     C                   IF        PiNum=*on
045900130306     C* il dato ha 2 decimali
046000130306     C                   EVAL(H)   VABVLB = PiVal/100
046100130306     C                   ELSE
046200130306     C                   SETON                                        32
046300130306     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046400130306     C                             + ' ' + 'VABVLB'
046500130306     C                   ENDIF
046600080923     C* CAS
046700130306     C* l'importo contrassegno ha 2 decimali e NON viene scritto con separatore decimale
046800130306     C                   IF        %trim(%subst(vindta:200:10))<>*blanks
046900080923     C                             AND
047000130306     C                             %trim(%subst(vindta:200:10))<>*zeros
047100121011     C                   EVAL      VABVCA = 'EUR'
047200070928     C                   MOVEL     '1'           FlgCAS
047300130306     C                   EVAL      PiStr=%trim(%subst(vindta:200:10))
047400070928     C                   EXSR      CHKNUM
047500070928     C                   IF        PiNum=*on
047600130306     C                   EVAL(H)   VABCAS = PiVal/100
047700070928     C                   ELSE
047800070928     C                   SETON                                        32
047900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048000070928     C                             + ' ' + 'VABCAS'
048100070928     C                   ENDIF
048200070928     C                   ENDIF
048300070730     C*
048400121109     C* Considerazioni finali su CBO/CAS
048500121106     C*
048600121106      * se c/assegno impostato
048700121106     C                   IF        VABCAS > 0
048800121109     C                   IF        VABCBO = '1'
048900121109     C                   EVAL      VABCBO = '4'
049000121109     C                   ENDIF
049100121109     C                   IF        VABCBO = '2'
049200121109     C                   EVAL      VABCBO = '6'
049300121109     C                   ENDIF
049400121106     C                   ENDIF
049500130306     C*
049600130306     C* Se provincia nn valorizzata la reperisco
049700130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
049800130306     C                   IF        VABPRD =  *blanks AND
049900130306     C                             VABNZD =  *blanks AND
050000130306     C                             VABCAD <> *blanks AND
050100130306     C                             VABLOD <> *blanks
050200130306     C                   CLEAR                   TISI95DS
050300130306     C                   EVAL      I95TCN = '3'
050400130306     C                   Z-ADD     datcor        I95DAT
050500130306     C                   EVAL      I95NAR = VABNZD
050600130306     C                   EVAL      I95CAP = VABCAD
050700130306     C                   EVAL      I95LOC = VABLOD
050800130306     C                   CALL      'TISI95R'
050900130306     C                   PARM                    TISI95DS
051000130306     C                   EVAL      VABPRD = O95PRV
051100130306     C                   ENDIF
051200020305     C*
051300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
051400011113     C                   EXSR      CHKIMPDIV
051500010202     C*
051600000801     C* Ebbene...
051700000801     C                   ADD       1             §CTRMO
051800070530     C                   IF        *in31 <> *zeros OR
051900070530     C                             *in32 <> *zeros
052000000801     C                   ADD       1             §CTRNO
052100020725     C                   EVAL      x_vinflg = '2'
052200000801     C                   ELSE
052300010201     C                   ADD       1             §CTROKVB
052400000801     C                   ENDIF
052500000801     C*
052600000801     C                   ENDSR
052700010201     C*----------------------------------------------------*
052800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
052900010201     C*----------------------------------------------------*
053000071003     C     WRIVAT_B      BEGSR
053100090204     C*
053200090204     C                   eval      vatAAS = vabAAS
053300090204     C                   eval      vatLNP = vabLNP
053400090204     C                   eval      vatNRS = vabNRS
053500090204     C                   eval      vatNSP = vabNSP
053600010201     C*
053700101007     C* Valorizzo il buffer di scrittura del FIVAT
053800070928     C* - TIPO RECORD "B"
053900130306     C                   if        %subst(vindta:260:13) <> *blanks
054000070928     C                   eval      VATTRC = 'B'
054100130306     C                   eval      VATNOT = %trim(%subst(vindta:260:13))
054200021113     C                   write     FIVAT000
054300020725     C                   endif
054400010201     C*
054500010201     C                   ENDSR
054600071003     C*----------------------------------------------------*
054700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
054800071003     C*----------------------------------------------------*
054900071003     C     WRIVAT_E      BEGSR
055000090204     C*
055100090204     C                   eval      vatAAS = vabAAS
055200090204     C                   eval      vatLNP = vabLNP
055300090204     C                   eval      vatNRS = vabNRS
055400090204     C                   eval      vatNSP = vabNSP
055500071003     C*
055600101007     C* Valorizzo il buffer di scrittura del FIVAT
055700071003     C* - TIPO RECORD "E"
055800130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
055900130306     C                   eval      VATTRC = 'E'
056000121109     C*
056100130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
056200071003     C                   write     FIVAT000
056300121109     C*
056400130306     C***                endif
056500071003     C*
056600071003     C                   ENDSR
056700020904
056800020904
056900010202     C*----------------------------------------------------*
057000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
057100010202     C*----------------------------------------------------*
057200020305     C     PREVAT        BEGSR
057300010202     C*
057400021113     C* Compongo il nome del membro da dare al FIVATWWR
057500010202     C                   eval      parmbr = vlrhdl
057600010202     C                   movel     'M'           parmbr
057700070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057800010202     C                   eval      paropz = '1'
057900010202     C* Effettuo la chiamata al CLLE preposto
058000050128     C                   call(e)   'TITVVTC'
058100010202     C                   parm                    parccm
058200010202     C                   parm                    parmbr
058300010202     C                   parm                    paropz
058400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058500010202     C                   if        %error
058600010202     C                   movel     '1'           chkcall
058700010202     C                   else
058800010202     C                   movel     '0'           chkcall
058900010202     C                   endif
059000010202     C*
059100010202     C                   ENDSR
059200000801     C*----------------------------------------------------*
059300000801     C*  CONTROLLO NUMERICITA' CAMPI
059400000801     C*----------------------------------------------------*
059500000801     C     CHKNUM        BEGSR
059600081113     C*
059700081113     C                   IF        PiDecChr = *blanks
059800121109     C                   EVAL      PiDecChr = ','
059900081113     C                   ENDIF
060000081113     C*
060100081113     C                   callp(e)  UBISNUM_Check(PiStr
060200081113     C                                          :PiDecChr
060300081113     C                                          :PiVal
060400081113     C                                          :PiNum
060500081113     C                                          :PiInt)
060600081113     C*
060700000801     C                   IF        %error
060800000801     C                   EVAL      PiInt=*off
060900000801     C                   ENDIF
061000000801     C*
061100000801     C                   ENDSR
061200000801     C***
061300000801
061400011113
061500011113     C*----------------------------------------------------*
061600011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061700011113     C*----------------------------------------------------*
061800011113     C     CHKIMPDIV     BEGSR
061900011113     C*
062000011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
062100011113     C                   Z-ADD     *zeros        wrkDec            9 9
062200011113     C*
062300011113     C* Come prima cosa effettuo considerazioni sulla divisa
062400011113     C                   IF        vabIAS > *zeros
062500011113     C                   IF        vabVAS <> 'EUR'
062600011113     C                   EVAL      vabVAS =  'ITL'
062700011113     C                   ENDIF
062800011113     C                   ENDIF
062900011113     C*
063000011113     C                   IF        vabCAS > *zeros
063100011113     C                   IF        vabVCA <> 'EUR'
063200011113     C                   EVAL      vabVCA =  'ITL'
063300011113     C                   ENDIF
063400011113     C                   ENDIF
063500011113     C*
063600011113     C                   IF        vabVMD > *zeros
063700020305     C                   IF        vabVAD <> 'EUR'
063800011113     C                   EVAL      vabVAD =  'ITL'
063900011113     C                   ENDIF
064000011113     C                   ENDIF
064100011113     C*
064200011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064300011113     C                   Z-ADD     vabIAS        wrkDec
064400011113     C                   IF        wrkDec > *zeros
064500011113     C                   IF        vabVAS = 'ITL'
064600011113     C                   EVAL      vabIAS = *zeros
064700011113     C                   ENDIF
064800011113     C                   ENDIF
064900011113     C*
065000011113     C* Stabilisco se il contrasegno ha decimali valorizzati
065100011113     C                   Z-ADD     vabCAS        wrkDec
065200011113     C                   IF        wrkDec > *zeros
065300011113     C                   IF        vabVCA = 'ITL'
065400011113     C                   EVAL      vabCAS = *zeros
065500011113     C                   ENDIF
065600011113     C                   ENDIF
065700011113     C*
065800011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065900011113     C                   Z-ADD     vabVMD        wrkDec
066000011113     C                   IF        wrkDec > *zeros
066100011113     C                   IF        vabVAD = 'ITL'
066200011113     C                   EVAL      vabVMD = *zeros
066300011113     C                   ENDIF
066400011113     C                   ENDIF
066500011113     C*
066600011113     C                   ENDSR
066700011113     C***
066800011113
066900011113
067000000801
067100000801
067200990920      /TITLE Invio dei dati al punto operativo.
067300010202     C     invio         BEGSR
067400990920     C*
067500021113     C* 1° invio FIVAT
067600010201     C                   reset                   dscmz
067700010201     C                   move      vlrpoi        cmzdst
067800021113     C                   eval      cmzfld = 'FIVATWWR'
067900010201     C                   eval      cmzmbd = vlrhdl
068000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
068100021009     C***                if        prmfir = *blanks
068200021113     C                   eval      cmzfla = 'FIVAT00F'
068300021113     C                   eval      cmzmba = 'FIVAT00F'
068400021009     C***                else
068500021009     C***                eval      cmzfla = prmfir
068600021009     C***                eval      cmzmba = prmfir
068700021009     C***                endif
068800010201     C                   eval      cmznrr = *zeros
068900020305     C                   move      §ctrokvt      cmznrr
069000021018     C                   eval      cmzlba = vlrfl1
069100010201     C                   call(e)   'TIS711C'
069200010201     C                   parm                    dscmz
069300010201     C                   parm      *blanks       esito
069400010205     C                   if        %error
069500010205     C                             or cmzerr = '1'
069600010205     C                             or esito  = '1'
069700010205     C                   eval      wrkesito = '3'
069800010205     C                   else
069900010201     C*
070000021113     C* 2° invio FIVAB
070100010201     C                   reset                   dscmz
070200010201     C                   move      vlrpoi        cmzdst
070300010201     C                   eval      cmzfld = vlrfou
070400010201     C                   eval      cmzmbd = vlrhdl
070500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070600021009     C***                if        prmfir = *blanks
070700021113     C                   eval      cmzfla = 'FIVAB00F'
070800021113     C                   eval      cmzmba = 'FIVAB00F'
070900021009     C***                else
071000021009     C***                eval      cmzfla = prmfir
071100021009     C***                eval      cmzmba = prmfir
071200021009     C***                endif
071300010201     C                   eval      cmznrr = *zeros
071400010201     C                   move      §ctrokvb      cmznrr
071500021018     C                   eval      cmzlba = vlrfl1
071600010201     C                   call(e)   'TIS711C'
071700010201     C                   parm                    dscmz
071800010201     C                   parm      *blanks       esito
071900010201     C                   if        %error
072000010201     C                             or cmzerr = '1'
072100010201     C                             or esito  = '1'
072200010201     C                   eval      wrkesito = '3'
072300010201     C                   endif
072400010205     C                   endif
072500990920     C*
072600000613     C                   ENDSR
072700000613     C***
072800070411
072900070411     C     *pssr         BEGSR
073000070411     C*
073100070411     C                   if        %open(tivin00r)
073200070411     C                   close     tivin00r
073300070411     C                   endif
073400070411     C                   if        %open(fivabwwr)
073500070411     C                   close     fivabwwr
073600070411     C                   endif
073700070411     C                   if        %open(fivatwwr)
073800070411     C                   close     fivatwwr
073900070411     C                   endif
074000070411     C*
074100070411     C* Effettuo la chiamata al CLLE preposto
074200070411     C                   call(e)   'TITVVTC'
074300070411     C                   parm                    parccm
074400070411     C                   parm                    parmbr
074500070411     C                   parm      '2'           paropz
074600070411     C*
074700070411     C                   eval      wrkesito = '2'
074800070411     C*
074900070411     C                   seton                                        LR
075000070411     C*
075100070411     C                   ENDSR     '*CANCL'
075200070411     C***
075300070411
075400990910
075500000613     C     *inzsr        BEGSR
075600990910     C*
075700990910     C     *entry        plist
075800990920     C                   parm                    tivlrds
075900990921     C                   parm      wrkesito      esito
076000000724     C                   parm                    prmlit
076100000710     C                   parm                    prmfir
076200000613     C*
076300000830     C* CALCOLA LA DATA CORRENTE
076400100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076500100324     C                   eval      datcor = %dec(%date() : *ISO)
076600000830     C*
076700000613     C                   ENDSR
076800000613     C***
