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
002400150812     D depspe          s             10    INZ(*blanks)
002500150812     D curspe          s             10    INZ(*blanks)
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
004100150812     D  VINSPE                       10
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
008000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
008100000613     C*
008200010202     C* Effettuo la chiamata al CLLE preposto
008300050128     C                   call(e)   'TITVVTC'
008400010202     C                   parm                    parccm
008500010202     C                   parm                    parmbr
008600010202     C                   parm      '2'           paropz
008700070730     C*
008800070730     C* Effettuo lancio TISI95 solo x chiusura
008900070730     C                   CLEAR                   TISI95DS
009000070730     C                   EVAL      I95TLA = 'C'
009100070730     C                   CALL      'TISI95R'
009200070730     C                   PARM                    TISI95DS
009300000616     C*
009400000801     C
009500010201     C                   seton                                        LR
009600000801
009700910830     C*--------------------------------------------------------
009800070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009900910830     C*--------------------------------------------------------
010000070530     C     RWFILE        BEGSR
010100990910     C*
010200990914     C                   if        not %open(tivin00r)
010300990908     C                   open      tivin00r
010400990914     C                   endif
010500021113     C                   if        not %open(fivabwwr)
010600021113     C                   open      fivabwwr
010700990914     C                   endif
010800070530     C*
010900021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
011000020305     C                   exsr      prevat
011100010201     C*
011200010202     C                   if        chkcall = '0'
011300010202     C*
011400021113     C                   if        not %open(fivatwwr)
011500021113     C                   open      fivatwwr
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
012800150812     C+ vindta, vinflg, substr(vindta, 2, 10) as sped, rrn(tivin00r)
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
015400121109     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015500121109     C* (uso l'RMN)
015600150812     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200071003     C                   exsr      wrivab
016300150812     C                   exsr      wrivat_b                                     => carico VAT
016400150812     C                   exsr      wrivat_ij                                    => carico VAT
016500071003     C                   else
016600080923     C*
016700071009     C                   if        wDISK = 'D'
016800071009     C                   exsr      impvab
016900071009     C                   exsr      wrivab
017000150812     C                   exsr      wrivat_b                                     => carico VAT
017100150812     C                   exsr      wrivat_ij                                    => carico VAT
017200130306     C***                exsr      wrivat_e                                     => carico VAT
017300071009     C                   else
017400071009     C*
017500010305     C                   if        depspe = *blanks                             => 1° giro
017600010305     C                   eval      depspe = curspe                              => memorizz. spediz
017700080117     C                   clear                   tivinds
017800020305     C                   exsr      impvab
017900150812     C                   exsr      wrivat_b                                     => carico VAT
018000150812     C                   exsr      wrivat_ij                                    => carico VAT
018100130306     C***50              exsr      wrivat_e                                     => carico VAT
018200010305     C                   else
018300020725     C                   if        depspe <> curspe                             => rottura di spediz
018400010305     C                   eval      depspe = curspe                              => memorizz. spediz
018500070928     C                   exsr      wrivab
018600080117     C                   clear                   tivinds
018700020305     C                   exsr      impvab
018800150812     C                   exsr      wrivat_b                                     => carico VAT
018900150812     C                   exsr      wrivat_ij                                    => carico VAT
019000130306     C***50              exsr      wrivat_e                                     => carico VAT
019100020305     C                   else                                                   => x stessa spediz
019200090204     C***                exsr      impvab
019300130723     C***                exsr      wrivat_b                                     => carico VAT
019400150812     C***                exsr      wrivat_ij                                    => carico VAT
019500130306     C***50              exsr      wrivat_e                                     => carico VAT
019600010305     C                   endif
019700010305     C                   endif
019800010305     C                   endif
019900071003     C                   endif
020000071009     C                   endif
020100000905     C*
020200000905     C                   else
020300020725     C                   eval      x_vinflg = '1'
020400000905     C                   endif
020500000905     C*
020600020725     C     VINRRN        chain     tivin000
020700020725     C                   if        %found(tivin00r)
020800020725     C                   eval      y_vinflg = x_vinflg
020900020725     C                   eval      y_vinmsg = x_vinmsg
021000020725     C                   update    tivin000
021100020725     C                   endif
021200020725     C*
021300020725     C/EXEC SQL
021400070530     C+ Fetch C1 into :INPUT_DS
021500020725     C/END-EXEC
021600020725     C*
021700020725     C                   enddo
021800020725     C*
021900020725     C/EXEC SQL
022000020725     C+ close C1
022100020725     C/END-EXEC
022200000905     C*
022300020305     C* Scarico i VAB rimasti "in sospeso"
022400071009     C                   if        wDISK = 'C'
022500071009     C                   exsr      wrivab
022600071009     C                   endif
022700010202     C*
022800010202     C                   endif
022900990910
023000990910     C* Se non ci sono record con errori ...
023100000710     C                   if        §ctrno = 0
023200990910     C* ... restituisco esito OK.
023300990921     C                   eval      wrkesito = '0'
023400990910     C                   else
023500010201     C                   if        §ctrokvb > 0
023600990921     C                   eval      wrkesito = '1'
023700000710     C                   else
023800000710     C                   eval      wrkesito = '2'
023900990910     C                   endif
024000000710     C                   endif
024100990910     C*
024200990914     C                   if        %open(tivin00r)
024300990908     C                   close     tivin00r
024400990914     C                   endif
024500021113     C                   if        %open(fivabwwr)
024600021113     C                   close     fivabwwr
024700990914     C                   endif
024800021113     C                   if        %open(fivatwwr)
024900021113     C                   close     fivatwwr
025000010201     C                   endif
025100990910     C*
025200010201     C                   if        §ctrokvb > 0
025300000724     C                             and vlrpoi <> *zeros
025400010202     C                   exsr      invio
025500990920     C                   endif
025600990920     C*
025700910830     C                   ENDSR
025800000613     C***
025900010305
026000010305     C*----------------------------------------------------*
026100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
026200010305     C*----------------------------------------------------*
026300020305     C     WRIVAB        BEGSR
026400070730     C*
026500071003     C* Considerazioni finali
026600071003     C                   if        VABRMA = *blanks
026700071003     C                   movel(P)  VABRMN        VABRMA
026800071003     C                   endif
026900071003     C*
027000021113     C                   write     fivab000                                     => scarico il VAB
027100010305     C*
027200010305     C                   ENDSR
027300990920
027400000801     C*----------------------------------------------------*
027500000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027600000801     C*----------------------------------------------------*
027700010201     C     INZVAR        BEGSR
027800000801     C*
027900010201     C                   Z-ADD     *zeros        Num5_0
028000020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
028100020725     C                   MOVEL     '0'           FlgCAS            1
028200000801     C*
028300000801     C                   ENDSR
028400000801     C*----------------------------------------------------*
028500000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028600000801     C*----------------------------------------------------*
028700020904     C     DEFCAM        BEGSR
028800080923     C*
028900080923     C                   SETOFF                                       5051
029000000801     C*
029100021113     C                   CLEAR                   FIVAB000
029200021113     C                   CLEAR                   FIVAT000
029300070730     C* Imposto i valori di default...
029400150812     C                   EVAL      VABCCM = 2140997
029500150812     C                   EVAL      VATCCM = 2140997
029600150812     C                   EVAL      VABLNP = 214
029700150812     C                   EVAL      VATLNP = 214
029800130306     C                   EVAL      VABCTR = 000
029900130723     C                   EVAL      VABCBO = '1'
030000150508     C                   EVAL      VABCTM = '7Q'
030100070222     C* ... e poi verifico se sono stati passati come parametri
030200070222     C                   IF        vlrppt > *blanks
030300070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
030400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030500070222     C                   EXSR      CHKNUM
030600070222     C                   IF        PiInt=*on
030700070222     C                   Z-ADD     PiVal         VABCCM
030800070222     C                   Z-ADD     PiVal         VATCCM
030900070222     C                   ENDIF
031000070222     C                   ENDIF
031100070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
031200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
031300070222     C                   EXSR      CHKNUM
031400070222     C                   IF        PiInt=*on
031500070222     C                   Z-ADD     PiVal         VABLNP
031600070222     C                   Z-ADD     PiVal         VATLNP
031700070222     C                   ENDIF
031800070222     C                   ENDIF
031900070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
032000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
032100070222     C                   EXSR      CHKNUM
032200070222     C                   IF        PiInt=*on
032300070222     C                   Z-ADD     PiVal         VABCTR
032400070222     C                   ENDIF
032500070928     C                   ENDIF
032600071009     C                   MOVEL     *blanks       wDISK             1
032700071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032800071009     C                   IF        wDISK <> *blanks
032900070928     C                   SETON                                        50
033000070222     C                   ENDIF
033100080923     C                   IF        %subst(vlrppt:15:1) = 'S'
033200080923     C                   SETON                                        51
033300080923     C                   ENDIF
033400130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033500130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033600130507     C                   ENDIF
033700070222     C                   ENDIF
033800071009     C*
033900071009     C   50              EVAL      VABCTM = '7Q'
034000000801     C*
034100000801     C                   ENDSR
034200000801     C*----------------------------------------------------*
034300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
034400000801     C*----------------------------------------------------*
034500010201     C     IMPVAB        BEGSR
034600070530     C*
034700070530     C                   SETOFF                                       3132
034800070928     C*
034900070928     C                   EXSR      INZVAR
035000070928     C                   EXSR      DEFCAM
035100070928     C*
035200070928     C                   MOVE(P)   vlrpoi        VABFGS
035300070928     C                   MOVE(P)   vlrpoi        VATFGS
035400130723     C                   MOVEL     datcor        VABAAS
035500070928     C                   MOVE      datcor        VABMGS
035600150812     C                   EVAL      VABRSD=%trim(%subst(vindta:21:35))
035700150508     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035800150810     C     '@':'A'       XLATE     VABRSD        VABRSD
035900070928     C* ==
036000150812     C                   EVAL      VABRD2=%trim(%subst(vindta:56:35))
036100150812     C                   EVAL      VABIND=%trim(%subst(vindta:91:35))
036200150812     C                   EVAL      VABLOD=%trim(%subst(vindta:126:35))
036300150812     C                   EVAL      VABCAD=%trim(%subst(vindta:161:10))
036400150812     C                   EVAL      VABPRD=%trim(%subst(vindta:171:3))
036500150812     C                   EVAL      VABNZD=%trim(%subst(vindta:174:3))
036600150810     C                   IF        %trim(VABNZD) = 'I' or
036700150810     C                             %trim(VABNZD) = 'IT' or
036800150810     C                             %trim(VABNZD) = 'ITA'
036900150508     C                   EVAL      VABNZD = *blanks
037000150508     C                   ENDIF
037100150812     C                   EVAL      VABCBO='1'
037200150812     C                   SELECT
037300150812     C                   WHEN      %subst(vindta:228:1)='A'
037400150812     C                   EVAL      VABCBO='2'
037500150812     C                   ENDSL
037600150812     C                   EVAL      w70=%trim(%subst(vindta:229:225))
037700150810     C                   EVAL      VABNOT=%subst(w70:1:35)
037800150810     C                   EVAL      VABNT2=%subst(w70:36:35)
037900150508     C*
038000150812     C* AAS+MGS
038100150812     C                   EVAL      PiStr=%trim(%subst(vindta:12:8))
038200150812     C                   EXSR      CHKNUM
038300150812     C                   IF        PiInt=*on
038400150812     C                   EVAL      VABAAS = %dec(%subst(%char(PiVal):1:4):4:0)
038500150812     C                   EVAL      VABMGS = %dec(%subst(%char(PiVal):5:4):4:0)
038600150812     C***                EVAL      VABAAS = PiVal/10000
038700150812     C***                EVAL      VABMGS = PiVal
038800150812     C***                MOVEL     PiVal         VABAAS
038900150812     C***                MOVE      PiVal         VABMGS
039000150812     C                   ELSE
039100150812     C                   SETON                                        32
039200150812     C                   Z-ADD     8888          VABAAS
039300150812     C                   Z-ADD     9999          VABMGS
039400150812     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039500150812     C                             + ' ' + 'VABAAS VABMGS'
039600150812     C                   ENDIF
039700080923     C* RMN
039800150812     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
039900121001     C                   EXSR      CHKNUM
040000121001     C                   IF        PiInt=*on
040100121001     C                   Z-ADD     PiVal         VABRMN
040200121001     C                   ELSE
040300121001     C                   SETON                                        32
040400121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040500121001     C                             + ' ' + 'VABRMN'
040600121001     C                   ENDIF
040700130723     C* NSP
040800150812     C                   EVAL      PiStr=%trim(%subst(vindta:2:10))
040900150810     C                   EXSR      CHKNUM
041000150810     C                   IF        PiInt=*on
041100150810     C                   Z-ADD     PiVal         VABNSP
041200150810     C                   ELSE
041300150810     C                   SETON                                        32
041400150810     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041500150810     C                             + ' ' + 'VABNSP'
041600150810     C                   ENDIF
041700071003     C* NCL
041800150812     C                   EVAL      PiStr=%trim(%subst(vindta:193:5))
041900070928     C                   EXSR      CHKNUM
042000070928     C                   IF        PiInt=*on
042100070928     C                   Z-ADD     PiVal         VABNCL
042200070928     C                   ELSE
042300070928     C                   SETON                                        32
042400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042500070928     C                             + ' ' + 'VABNCL'
042600070928     C                   ENDIF
042700071003     C* PKB
042800150812     C                   EVAL      PiStr=%trim(%subst(vindta:198:9))
042900070928     C                   EXSR      CHKNUM
043000070928     C                   IF        PiNum=*on
043100150812     C* il dato ha 3 decimali con   separatore
043200150812     C                   EVAL      VABPKB = PiVal
043300070928     C                   ELSE
043400070928     C                   SETON                                        32
043500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043600070928     C                             + ' ' + 'VABPKB'
043700070928     C                   ENDIF
043800150812     C* VLB
043900150812     C                   EVAL      PiStr=%trim(%subst(vindta:207:9))
044000150812     C                   EXSR      CHKNUM
044100150812     C                   IF        PiNum=*on
044200150812     C* il dato ha 3 decimali con   separatore
044300150812     C                   EVAL      VABVLB = PiVal
044400150812     C                   ELSE
044500150812     C                   SETON                                        32
044600150812     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044700150812     C                             + ' ' + 'VABVLB'
044800150812     C                   ENDIF
044900150508     C* CAS
045000150812     C                   IF        %trim(%subst(vindta:219:9)) <> *blanks AND
045100150812     C                             %trim(%subst(vindta:219:9)) <> *zeros  AND
045200150812     C                             %trim(%subst(vindta:219:9)) <> '000000,00'
045300150508     C                   MOVEL     '1'           FlgCAS
045400150508     C                   EVAL      VABVCA = 'EUR'
045500150812     C                   EVAL      PiStr=%trim(%subst(vindta:219:9))
045600150508     C                   EXSR      CHKNUM
045700150508     C                   IF        PiNum=*on
045800150812     C* il dato ha 2 decimali con   separatore
045900150508     C                   Z-ADD     PiVal         VABCAS
046000150508     C                   ELSE
046100150508     C                   SETON                                        32
046200150508     C                   Z-ADD     *zeros        VABCAS
046300150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046400150508     C                             + ' ' + 'VABCAS'
046500150508     C                   ENDIF
046600150508     C                   ENDIF
046700070730     C*
046800121109     C* Considerazioni finali su CBO/CAS
046900121106     C*
047000121106      * se c/assegno impostato
047100121106     C                   IF        VABCAS > 0
047200121109     C                   IF        VABCBO = '1'
047300121109     C                   EVAL      VABCBO = '4'
047400121109     C                   ENDIF
047500121109     C                   IF        VABCBO = '2'
047600121109     C                   EVAL      VABCBO = '6'
047700121109     C                   ENDIF
047800121106     C                   ENDIF
047900130306     C*
048000130306     C* Se provincia nn valorizzata la reperisco
048100130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
048200130306     C                   IF        VABPRD =  *blanks AND
048300130306     C                             VABNZD =  *blanks AND
048400130306     C                             VABCAD <> *blanks AND
048500130306     C                             VABLOD <> *blanks
048600130306     C                   CLEAR                   TISI95DS
048700130306     C                   EVAL      I95TCN = '3'
048800130306     C                   Z-ADD     datcor        I95DAT
048900130306     C                   EVAL      I95NAR = VABNZD
049000130306     C                   EVAL      I95CAP = VABCAD
049100130306     C                   EVAL      I95LOC = VABLOD
049200130306     C                   CALL      'TISI95R'
049300130306     C                   PARM                    TISI95DS
049400130306     C                   EVAL      VABPRD = O95PRV
049500130306     C                   ENDIF
049600020305     C*
049700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
049800011113     C                   EXSR      CHKIMPDIV
049900010202     C*
050000000801     C* Ebbene...
050100000801     C                   ADD       1             §CTRMO
050200070530     C                   IF        *in31 <> *zeros OR
050300070530     C                             *in32 <> *zeros
050400000801     C                   ADD       1             §CTRNO
050500020725     C                   EVAL      x_vinflg = '2'
050600000801     C                   ELSE
050700010201     C                   ADD       1             §CTROKVB
050800000801     C                   ENDIF
050900000801     C*
051000000801     C                   ENDSR
051100010201     C*----------------------------------------------------*
051200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
051300010201     C*----------------------------------------------------*
051400071003     C     WRIVAT_B      BEGSR
051500090204     C*
051600090204     C                   eval      vatAAS = vabAAS
051700090204     C                   eval      vatLNP = vabLNP
051800090204     C                   eval      vatNRS = vabNRS
051900090204     C                   eval      vatNSP = vabNSP
052000010201     C*
052100101007     C* Valorizzo il buffer di scrittura del FIVAT
052200070928     C* - TIPO RECORD "B"
052300130306     C                   if        %subst(vindta:260:13) <> *blanks
052400070928     C                   eval      VATTRC = 'B'
052500150812     C                   eval      VATNOT = %trim(%subst(vindta:177:16))
052600021113     C                   write     FIVAT000
052700020725     C                   endif
052800010201     C*
052900010201     C                   ENDSR
053000150812     C*----------------------------------------------------*
053100150812     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I" e "J"
053200150812     C*----------------------------------------------------*
053300150812     C     WRIVAT_IJ     BEGSR
053400150812     C*
053500150812     C                   eval      vatAAS = vabAAS
053600150812     C                   eval      vatLNP = vabLNP
053700150812     C                   eval      vatNRS = vabNRS
053800150812     C                   eval      vatNSP = vabNSP
053900150812     C*
054000150812     C* Valorizzo il buffer di scrittura del FIVAT
054100150812     C* - TIPO RECORD "I"
054200150812     C                   if        %subst(vindta:484:241) <> *blanks
054300150812     C                   eval      w70 = %trim(%subst(vindta:484:241))
054400150812     C                   eval      VATTRC = 'I'
054500150812     C                   eval      VATNOT = %subst(w70:1:35)
054600150812     C                   write     FIVAT000
054700150812     C                   IF        %subst(w70:36:35) <> *blank
054800150812     C                   eval      VATTRC = 'J'
054900150812     C                   eval      VATNOT = %subst(w70:36:35)
055000150812     C                   write     FIVAT000
055100150812     C                   endif
055200150812     C                   endif
055300150812     C*
055400150812     C                   ENDSR
055500071003     C*----------------------------------------------------*
055600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
055700071003     C*----------------------------------------------------*
055800071003     C     WRIVAT_E      BEGSR
055900090204     C*
056000090204     C                   eval      vatAAS = vabAAS
056100090204     C                   eval      vatLNP = vabLNP
056200090204     C                   eval      vatNRS = vabNRS
056300090204     C                   eval      vatNSP = vabNSP
056400071003     C*
056500101007     C* Valorizzo il buffer di scrittura del FIVAT
056600071003     C* - TIPO RECORD "E"
056700130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
056800130306     C                   eval      VATTRC = 'E'
056900121109     C*
057000130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
057100071003     C                   write     FIVAT000
057200121109     C*
057300130306     C***                endif
057400071003     C*
057500071003     C                   ENDSR
057600020904
057700020904
057800010202     C*----------------------------------------------------*
057900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
058000010202     C*----------------------------------------------------*
058100020305     C     PREVAT        BEGSR
058200010202     C*
058300021113     C* Compongo il nome del membro da dare al FIVATWWR
058400010202     C                   eval      parmbr = vlrhdl
058500010202     C                   movel     'M'           parmbr
058600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
058700010202     C                   eval      paropz = '1'
058800010202     C* Effettuo la chiamata al CLLE preposto
058900050128     C                   call(e)   'TITVVTC'
059000010202     C                   parm                    parccm
059100010202     C                   parm                    parmbr
059200010202     C                   parm                    paropz
059300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
059400010202     C                   if        %error
059500010202     C                   movel     '1'           chkcall
059600010202     C                   else
059700010202     C                   movel     '0'           chkcall
059800010202     C                   endif
059900010202     C*
060000010202     C                   ENDSR
060100000801     C*----------------------------------------------------*
060200000801     C*  CONTROLLO NUMERICITA' CAMPI
060300000801     C*----------------------------------------------------*
060400000801     C     CHKNUM        BEGSR
060500081113     C*
060600081113     C                   IF        PiDecChr = *blanks
060700121109     C                   EVAL      PiDecChr = ','
060800081113     C                   ENDIF
060900081113     C*
061000081113     C                   callp(e)  UBISNUM_Check(PiStr
061100081113     C                                          :PiDecChr
061200081113     C                                          :PiVal
061300081113     C                                          :PiNum
061400081113     C                                          :PiInt)
061500081113     C*
061600000801     C                   IF        %error
061700000801     C                   EVAL      PiInt=*off
061800000801     C                   ENDIF
061900000801     C*
062000000801     C                   ENDSR
062100000801     C***
062200000801
062300011113
062400011113     C*----------------------------------------------------*
062500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
062600011113     C*----------------------------------------------------*
062700011113     C     CHKIMPDIV     BEGSR
062800011113     C*
062900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
063000011113     C                   Z-ADD     *zeros        wrkDec            9 9
063100011113     C*
063200011113     C* Come prima cosa effettuo considerazioni sulla divisa
063300011113     C                   IF        vabIAS > *zeros
063400011113     C                   IF        vabVAS <> 'EUR'
063500011113     C                   EVAL      vabVAS =  'ITL'
063600011113     C                   ENDIF
063700011113     C                   ENDIF
063800011113     C*
063900011113     C                   IF        vabCAS > *zeros
064000011113     C                   IF        vabVCA <> 'EUR'
064100011113     C                   EVAL      vabVCA =  'ITL'
064200011113     C                   ENDIF
064300011113     C                   ENDIF
064400011113     C*
064500011113     C                   IF        vabVMD > *zeros
064600020305     C                   IF        vabVAD <> 'EUR'
064700011113     C                   EVAL      vabVAD =  'ITL'
064800011113     C                   ENDIF
064900011113     C                   ENDIF
065000011113     C*
065100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
065200011113     C                   Z-ADD     vabIAS        wrkDec
065300011113     C                   IF        wrkDec > *zeros
065400011113     C                   IF        vabVAS = 'ITL'
065500011113     C                   EVAL      vabIAS = *zeros
065600011113     C                   ENDIF
065700011113     C                   ENDIF
065800011113     C*
065900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
066000011113     C                   Z-ADD     vabCAS        wrkDec
066100011113     C                   IF        wrkDec > *zeros
066200011113     C                   IF        vabVCA = 'ITL'
066300011113     C                   EVAL      vabCAS = *zeros
066400011113     C                   ENDIF
066500011113     C                   ENDIF
066600011113     C*
066700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
066800011113     C                   Z-ADD     vabVMD        wrkDec
066900011113     C                   IF        wrkDec > *zeros
067000011113     C                   IF        vabVAD = 'ITL'
067100011113     C                   EVAL      vabVMD = *zeros
067200011113     C                   ENDIF
067300011113     C                   ENDIF
067400011113     C*
067500011113     C                   ENDSR
067600011113     C***
067700011113
067800011113
067900000801
068000000801
068100990920      /TITLE Invio dei dati al punto operativo.
068200010202     C     invio         BEGSR
068300990920     C*
068400021113     C* 1° invio FIVAT
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700021113     C                   eval      cmzfld = 'FIVATWWR'
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAT00F'
069200021113     C                   eval      cmzmba = 'FIVAT00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800020305     C                   move      §ctrokvt      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010205     C                   if        %error
070400010205     C                             or cmzerr = '1'
070500010205     C                             or esito  = '1'
070600010205     C                   eval      wrkesito = '3'
070700010205     C                   else
070800010201     C*
070900021113     C* 2° invio FIVAB
071000010201     C                   reset                   dscmz
071100010201     C                   move      vlrpoi        cmzdst
071200010201     C                   eval      cmzfld = vlrfou
071300010201     C                   eval      cmzmbd = vlrhdl
071400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500021009     C***                if        prmfir = *blanks
071600021113     C                   eval      cmzfla = 'FIVAB00F'
071700021113     C                   eval      cmzmba = 'FIVAB00F'
071800021009     C***                else
071900021009     C***                eval      cmzfla = prmfir
072000021009     C***                eval      cmzmba = prmfir
072100021009     C***                endif
072200010201     C                   eval      cmznrr = *zeros
072300010201     C                   move      §ctrokvb      cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500010201     C                   call(e)   'TIS711C'
072600010201     C                   parm                    dscmz
072700010201     C                   parm      *blanks       esito
072800010201     C                   if        %error
072900010201     C                             or cmzerr = '1'
073000010201     C                             or esito  = '1'
073100010201     C                   eval      wrkesito = '3'
073200010201     C                   endif
073300010205     C                   endif
073400990920     C*
073500000613     C                   ENDSR
073600000613     C***
073700070411
073800070411     C     *pssr         BEGSR
073900070411     C*
074000070411     C                   if        %open(tivin00r)
074100070411     C                   close     tivin00r
074200070411     C                   endif
074300070411     C                   if        %open(fivabwwr)
074400070411     C                   close     fivabwwr
074500070411     C                   endif
074600070411     C                   if        %open(fivatwwr)
074700070411     C                   close     fivatwwr
074800070411     C                   endif
074900070411     C*
075000070411     C* Effettuo la chiamata al CLLE preposto
075100070411     C                   call(e)   'TITVVTC'
075200070411     C                   parm                    parccm
075300070411     C                   parm                    parmbr
075400070411     C                   parm      '2'           paropz
075500070411     C*
075600070411     C                   eval      wrkesito = '2'
075700070411     C*
075800070411     C                   seton                                        LR
075900070411     C*
076000070411     C                   ENDSR     '*CANCL'
076100070411     C***
076200070411
076300990910
076400000613     C     *inzsr        BEGSR
076500990910     C*
076600990910     C     *entry        plist
076700990920     C                   parm                    tivlrds
076800990921     C                   parm      wrkesito      esito
076900000724     C                   parm                    prmlit
077000000710     C                   parm                    prmfir
077100000613     C*
077200000830     C* CALCOLA LA DATA CORRENTE
077300100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
077400100324     C                   eval      datcor = %dec(%date() : *ISO)
077500000830     C*
077600000613     C                   ENDSR
077700000613     C***
