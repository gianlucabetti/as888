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
002400150508     D depspe          s             15    INZ(*blanks)
002500150508     D curspe          s             15    INZ(*blanks)
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
004100150508     D  VINSPE                       15
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
012800150508     C+ vindta, vinflg, substr(vindta, 106, 15) as sped, rrn(tivin00r)
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
015600150508     C                   EVAL      PiStr=%trim(%subst(vindta:106:15))
015700020305     C                   MOVEL(p)  PiStr         curspe
015800010305     C*
015900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
016000080923     C                   if        *in50 = *off
016100071003     C                   exsr      impvab
016200071003     C                   exsr      wrivab
016300130723     C***                exsr      wrivat_b                                     => carico VAT
016400071003     C                   else
016500080923     C*
016600071009     C                   if        wDISK = 'D'
016700071009     C                   exsr      impvab
016800071009     C                   exsr      wrivab
016900130723     C***                exsr      wrivat_b                                     => carico VAT
017000130306     C***                exsr      wrivat_e                                     => carico VAT
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600020305     C                   exsr      impvab
017700130723     C***                exsr      wrivat_b                                     => carico VAT
017800130306     C***50              exsr      wrivat_e                                     => carico VAT
017900010305     C                   else
018000020725     C                   if        depspe <> curspe                             => rottura di spediz
018100010305     C                   eval      depspe = curspe                              => memorizz. spediz
018200070928     C                   exsr      wrivab
018300080117     C                   clear                   tivinds
018400020305     C                   exsr      impvab
018500130723     C***                exsr      wrivat_b                                     => carico VAT
018600130306     C***50              exsr      wrivat_e                                     => carico VAT
018700020305     C                   else                                                   => x stessa spediz
018800090204     C***                exsr      impvab
018900130723     C***                exsr      wrivat_b                                     => carico VAT
019000130306     C***50              exsr      wrivat_e                                     => carico VAT
019100010305     C                   endif
019200010305     C                   endif
019300010305     C                   endif
019400071003     C                   endif
019500071009     C                   endif
019600000905     C*
019700000905     C                   else
019800020725     C                   eval      x_vinflg = '1'
019900000905     C                   endif
020000000905     C*
020100020725     C     VINRRN        chain     tivin000
020200020725     C                   if        %found(tivin00r)
020300020725     C                   eval      y_vinflg = x_vinflg
020400020725     C                   eval      y_vinmsg = x_vinmsg
020500020725     C                   update    tivin000
020600020725     C                   endif
020700020725     C*
020800020725     C/EXEC SQL
020900070530     C+ Fetch C1 into :INPUT_DS
021000020725     C/END-EXEC
021100020725     C*
021200020725     C                   enddo
021300020725     C*
021400020725     C/EXEC SQL
021500020725     C+ close C1
021600020725     C/END-EXEC
021700000905     C*
021800020305     C* Scarico i VAB rimasti "in sospeso"
021900071009     C                   if        wDISK = 'C'
022000071009     C                   exsr      wrivab
022100071009     C                   endif
022200010202     C*
022300010202     C                   endif
022400990910
022500990910     C* Se non ci sono record con errori ...
022600000710     C                   if        §ctrno = 0
022700990910     C* ... restituisco esito OK.
022800990921     C                   eval      wrkesito = '0'
022900990910     C                   else
023000010201     C                   if        §ctrokvb > 0
023100990921     C                   eval      wrkesito = '1'
023200000710     C                   else
023300000710     C                   eval      wrkesito = '2'
023400990910     C                   endif
023500000710     C                   endif
023600990910     C*
023700990914     C                   if        %open(tivin00r)
023800990908     C                   close     tivin00r
023900990914     C                   endif
024000021113     C                   if        %open(fivabwwr)
024100021113     C                   close     fivabwwr
024200990914     C                   endif
024300021113     C                   if        %open(fivatwwr)
024400021113     C                   close     fivatwwr
024500010201     C                   endif
024600990910     C*
024700010201     C                   if        §ctrokvb > 0
024800000724     C                             and vlrpoi <> *zeros
024900010202     C                   exsr      invio
025000990920     C                   endif
025100990920     C*
025200910830     C                   ENDSR
025300000613     C***
025400010305
025500010305     C*----------------------------------------------------*
025600020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025700010305     C*----------------------------------------------------*
025800020305     C     WRIVAB        BEGSR
025900070730     C*
026000071003     C* Considerazioni finali
026100071003     C                   if        VABRMA = *blanks
026200071003     C                   movel(P)  VABRMN        VABRMA
026300071003     C                   endif
026400071003     C*
026500021113     C                   write     fivab000                                     => scarico il VAB
026600010305     C*
026700010305     C                   ENDSR
026800990920
026900000801     C*----------------------------------------------------*
027000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027100000801     C*----------------------------------------------------*
027200010201     C     INZVAR        BEGSR
027300000801     C*
027400010201     C                   Z-ADD     *zeros        Num5_0
027500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027600020725     C                   MOVEL     '0'           FlgCAS            1
027700000801     C*
027800000801     C                   ENDSR
027900000801     C*----------------------------------------------------*
028000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028100000801     C*----------------------------------------------------*
028200020904     C     DEFCAM        BEGSR
028300080923     C*
028400080923     C                   SETOFF                                       5051
028500000801     C*
028600021113     C                   CLEAR                   FIVAB000
028700021113     C                   CLEAR                   FIVAT000
028800070730     C* Imposto i valori di default...
028900150508     C                   EVAL      VABCCM = 1050042
029000150508     C                   EVAL      VATCCM = 1050042
029100150508     C                   EVAL      VABLNP = 105
029200150508     C                   EVAL      VATLNP = 105
029300130306     C                   EVAL      VABCTR = 000
029400130723     C                   EVAL      VABCBO = '1'
029500150508     C                   EVAL      VABCTM = '7Q'
029600070222     C* ... e poi verifico se sono stati passati come parametri
029700070222     C                   IF        vlrppt > *blanks
029800070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
030000070222     C                   EXSR      CHKNUM
030100070222     C                   IF        PiInt=*on
030200070222     C                   Z-ADD     PiVal         VABCCM
030300070222     C                   Z-ADD     PiVal         VATCCM
030400070222     C                   ENDIF
030500070222     C                   ENDIF
030600070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030800070222     C                   EXSR      CHKNUM
030900070222     C                   IF        PiInt=*on
031000070222     C                   Z-ADD     PiVal         VABLNP
031100070222     C                   Z-ADD     PiVal         VATLNP
031200070222     C                   ENDIF
031300070222     C                   ENDIF
031400070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031600070222     C                   EXSR      CHKNUM
031700070222     C                   IF        PiInt=*on
031800070222     C                   Z-ADD     PiVal         VABCTR
031900070222     C                   ENDIF
032000070928     C                   ENDIF
032100071009     C                   MOVEL     *blanks       wDISK             1
032200071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
032300071009     C                   IF        wDISK <> *blanks
032400070928     C                   SETON                                        50
032500070222     C                   ENDIF
032600080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032700080923     C                   SETON                                        51
032800080923     C                   ENDIF
032900130507     C                   IF        %subst(vlrppt:16:2) <> *blanks
033000130507     C                   EVAL      VABCTM = %subst(vlrppt:16:2)
033100130507     C                   ENDIF
033200070222     C                   ENDIF
033300071009     C*
033400071009     C   50              EVAL      VABCTM = '7Q'
033500000801     C*
033600000801     C                   ENDSR
033700000801     C*----------------------------------------------------*
033800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033900000801     C*----------------------------------------------------*
034000010201     C     IMPVAB        BEGSR
034100070530     C*
034200070530     C                   SETOFF                                       3132
034300070928     C*
034400070928     C                   EXSR      INZVAR
034500070928     C                   EXSR      DEFCAM
034600070928     C*
034700070928     C                   MOVE(P)   vlrpoi        VABFGS
034800070928     C                   MOVE(P)   vlrpoi        VATFGS
034900130723     C                   MOVEL     datcor        VABAAS
035000070928     C                   MOVE      datcor        VABMGS
035100150508     C                   EVAL      w70=%trim(%subst(vindta:1:70))
035200150508     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035300150508     C     '@':'A'       XLATE     w70           w70
035400150508     C                   EVAL      VABRSD=%trim(%subst(w70:1:35))
035500150508     C                   EVAL      VABRD2=%trim(%subst(w70:36:35))
035600070928     C* ==
035700150508     C                   EVAL      VABIND=%trim(%subst(vindta:71:35))
035800150508     C                   EVAL      VABLOD=%trim(%subst(vindta:130:35))
035900150508     C                   EVAL      VABPRD=%trim(%subst(vindta:165:2))
036000150508     C                   EVAL      VABNZD=%trim(%subst(vindta:167:2))
036100150508     C                   IF        %trim(VABNZD) = 'IT'
036200150508     C                   EVAL      VABNZD = *blanks
036300150508     C                   ENDIF
036400150508     C                   EVAL      w70=%trim(%subst(vindta:182:70))
036500130723     C                   EVAL      VABNOT=%subst(w70:1:35)
036600130723     C                   EVAL      VABNT2=%subst(w70:36:35)
036700150508     C                   EVAL      VABCBO=%trim(%subst(vindta:255:2))
036800150508     C                   EVAL      VABTIC=%trim(%subst(vindta:283:2))
036900150508     C* siccome il cliente spedisce all'estero e lì ci possono essere dei CAP alfanumerici,
037000150508     C* prendo alfanumerico
037100150508     C                   EVAL      VABCAD=%trim(%subst(vindta:121:9))
037200150508     C*
037300080923     C* RMN
037400150508     C                   EVAL      PiStr=%trim(%subst(vindta:106:15))
037500121001     C                   EXSR      CHKNUM
037600121001     C                   IF        PiInt=*on
037700121001     C                   Z-ADD     PiVal         VABRMN
037800121001     C                   ELSE
037900121001     C                   SETON                                        32
038000121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038100121001     C                             + ' ' + 'VABRMN'
038200121001     C                   ENDIF
038300130723     C* NSP
038400150508     C* ultimi 7 char di RMN
038500150508     C                   Z-ADD     VABRMN        VABNSP
038600071003     C* NCL
038700150508     C                   EVAL      PiStr=%trim(%subst(vindta:170:5))
038800070928     C                   EXSR      CHKNUM
038900070928     C                   IF        PiInt=*on
039000070928     C                   Z-ADD     PiVal         VABNCL
039100070928     C                   ELSE
039200070928     C                   SETON                                        32
039300070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039400070928     C                             + ' ' + 'VABNCL'
039500070928     C                   ENDIF
039600071003     C* PKB
039700150508     C                   EVAL      PiStr=%trim(%subst(vindta:175:7))
039800070928     C                   EXSR      CHKNUM
039900070928     C                   IF        PiNum=*on
040000150508     C* il dato ha 1 decimali senza separatore
040100150508     C                   EVAL(H)   VABPKB = PiVal/10
040200070928     C                   ELSE
040300070928     C                   SETON                                        32
040400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040500070928     C                             + ' ' + 'VABPKB'
040600070928     C                   ENDIF
040700150508     C* CTR
040800150508     C                   EVAL      PiStr=%trim(%subst(vindta:252:3))
040900150508     C                   EXSR      CHKNUM
041000150508     C                   IF        PiInt=*on
041100150508     C                   EVAL      VABCTR = PiVal
041200150521     C* se VABCTR = 309 allora VABCTM=7R
041300150521     C                   IF        VABCTR = 309
041400150521     C                   EVAL      VABCTM = '7R'
041500150521     C                   ENDIF
041600150508     C                   ELSE
041700150508     C                   SETON                                        32
041800150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041900150508     C                             + ' ' + 'VABCTR'
042000150508     C                   ENDIF
042100150508     C* CAS
042200150508     C                   IF        %trim(%subst(vindta:257:13)) <> *blanks AND
042300150508     C                             %trim(%subst(vindta:257:13)) <> *zeros
042400150508     C                   MOVEL     '1'           FlgCAS
042500150508     C                   EVAL      VABVCA = 'EUR'
042600150508     C                   EVAL      PiStr=%trim(%subst(vindta:257:13))
042700150508     C                   EXSR      CHKNUM
042800150508     C                   IF        PiNum=*on
042900150508     C                   EVAL      PiVal = PiVal / 1000                         * gest. 3 decimali
043000150508     C                   Z-ADD     PiVal         VABCAS
043100150508     C                   ELSE
043200150508     C                   SETON                                        32
043300150508     C                   Z-ADD     *zeros        VABCAS
043400150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043500150508     C                             + ' ' + 'VABCAS'
043600150508     C                   ENDIF
043700150508     C                   ENDIF
043800150508     C* IAS
043900150508     C                   IF        %trim(%subst(vindta:270:13)) <> *blanks AND
044000150508     C                             %trim(%subst(vindta:270:13)) <> *zeros
044100150508     C                   EVAL      VABVAS = 'EUR'
044200150508     C                   EVAL      PiStr=%trim(%subst(vindta:270:13))
044300150508     C                   EXSR      CHKNUM
044400150508     C                   IF        PiNum=*on
044500150508     C                   EVAL      PiVal = PiVal / 1000                         * gest. 3 decimali
044600150508     C                   Z-ADD     PiVal         VABIAS
044700150508     C                   ELSE
044800150508     C                   SETON                                        32
044900150508     C                   Z-ADD     *zeros        VABIAS
045000150508     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045100150508     C                             + ' ' + 'VABIAS'
045200150508     C                   ENDIF
045300150508     C                   ENDIF
045400070730     C*
045500121109     C* Considerazioni finali su CBO/CAS
045600121106     C*
045700121106      * se c/assegno impostato
045800121106     C                   IF        VABCAS > 0
045900121109     C                   IF        VABCBO = '1'
046000121109     C                   EVAL      VABCBO = '4'
046100121109     C                   ENDIF
046200121109     C                   IF        VABCBO = '2'
046300121109     C                   EVAL      VABCBO = '6'
046400121109     C                   ENDIF
046500121106     C                   ENDIF
046600130306     C*
046700130306     C* Se provincia nn valorizzata la reperisco
046800130306     C* tramite TISI95R a seconda dei dati d instradamento presenti
046900130306     C                   IF        VABPRD =  *blanks AND
047000130306     C                             VABNZD =  *blanks AND
047100130306     C                             VABCAD <> *blanks AND
047200130306     C                             VABLOD <> *blanks
047300130306     C                   CLEAR                   TISI95DS
047400130306     C                   EVAL      I95TCN = '3'
047500130306     C                   Z-ADD     datcor        I95DAT
047600130306     C                   EVAL      I95NAR = VABNZD
047700130306     C                   EVAL      I95CAP = VABCAD
047800130306     C                   EVAL      I95LOC = VABLOD
047900130306     C                   CALL      'TISI95R'
048000130306     C                   PARM                    TISI95DS
048100130306     C                   EVAL      VABPRD = O95PRV
048200130306     C                   ENDIF
048300020305     C*
048400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048500011113     C                   EXSR      CHKIMPDIV
048600010202     C*
048700000801     C* Ebbene...
048800000801     C                   ADD       1             §CTRMO
048900070530     C                   IF        *in31 <> *zeros OR
049000070530     C                             *in32 <> *zeros
049100000801     C                   ADD       1             §CTRNO
049200020725     C                   EVAL      x_vinflg = '2'
049300000801     C                   ELSE
049400010201     C                   ADD       1             §CTROKVB
049500000801     C                   ENDIF
049600000801     C*
049700000801     C                   ENDSR
049800010201     C*----------------------------------------------------*
049900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
050000010201     C*----------------------------------------------------*
050100071003     C     WRIVAT_B      BEGSR
050200090204     C*
050300090204     C                   eval      vatAAS = vabAAS
050400090204     C                   eval      vatLNP = vabLNP
050500090204     C                   eval      vatNRS = vabNRS
050600090204     C                   eval      vatNSP = vabNSP
050700010201     C*
050800101007     C* Valorizzo il buffer di scrittura del FIVAT
050900070928     C* - TIPO RECORD "B"
051000130306     C                   if        %subst(vindta:260:13) <> *blanks
051100070928     C                   eval      VATTRC = 'B'
051200130306     C                   eval      VATNOT = %trim(%subst(vindta:260:13))
051300021113     C                   write     FIVAT000
051400020725     C                   endif
051500010201     C*
051600010201     C                   ENDSR
051700071003     C*----------------------------------------------------*
051800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
051900071003     C*----------------------------------------------------*
052000071003     C     WRIVAT_E      BEGSR
052100090204     C*
052200090204     C                   eval      vatAAS = vabAAS
052300090204     C                   eval      vatLNP = vabLNP
052400090204     C                   eval      vatNRS = vabNRS
052500090204     C                   eval      vatNSP = vabNSP
052600071003     C*
052700101007     C* Valorizzo il buffer di scrittura del FIVAT
052800071003     C* - TIPO RECORD "E"
052900130306     C***                if        %subst(vindta:xxx:yyy) <> *blanks
053000130306     C                   eval      VATTRC = 'E'
053100121109     C*
053200130306     C***                eval      VATNOT = %trim(%subst(vindta:xxx:yyy))
053300071003     C                   write     FIVAT000
053400121109     C*
053500130306     C***                endif
053600071003     C*
053700071003     C                   ENDSR
053800020904
053900020904
054000010202     C*----------------------------------------------------*
054100021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
054200010202     C*----------------------------------------------------*
054300020305     C     PREVAT        BEGSR
054400010202     C*
054500021113     C* Compongo il nome del membro da dare al FIVATWWR
054600010202     C                   eval      parmbr = vlrhdl
054700010202     C                   movel     'M'           parmbr
054800070530     C                   eval      parccm = %subst(vlrKSC:2:7)
054900010202     C                   eval      paropz = '1'
055000010202     C* Effettuo la chiamata al CLLE preposto
055100050128     C                   call(e)   'TITVVTC'
055200010202     C                   parm                    parccm
055300010202     C                   parm                    parmbr
055400010202     C                   parm                    paropz
055500010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
055600010202     C                   if        %error
055700010202     C                   movel     '1'           chkcall
055800010202     C                   else
055900010202     C                   movel     '0'           chkcall
056000010202     C                   endif
056100010202     C*
056200010202     C                   ENDSR
056300000801     C*----------------------------------------------------*
056400000801     C*  CONTROLLO NUMERICITA' CAMPI
056500000801     C*----------------------------------------------------*
056600000801     C     CHKNUM        BEGSR
056700081113     C*
056800081113     C                   IF        PiDecChr = *blanks
056900121109     C                   EVAL      PiDecChr = ','
057000081113     C                   ENDIF
057100081113     C*
057200081113     C                   callp(e)  UBISNUM_Check(PiStr
057300081113     C                                          :PiDecChr
057400081113     C                                          :PiVal
057500081113     C                                          :PiNum
057600081113     C                                          :PiInt)
057700081113     C*
057800000801     C                   IF        %error
057900000801     C                   EVAL      PiInt=*off
058000000801     C                   ENDIF
058100000801     C*
058200000801     C                   ENDSR
058300000801     C***
058400000801
058500011113
058600011113     C*----------------------------------------------------*
058700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
058800011113     C*----------------------------------------------------*
058900011113     C     CHKIMPDIV     BEGSR
059000011113     C*
059100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
059200011113     C                   Z-ADD     *zeros        wrkDec            9 9
059300011113     C*
059400011113     C* Come prima cosa effettuo considerazioni sulla divisa
059500011113     C                   IF        vabIAS > *zeros
059600011113     C                   IF        vabVAS <> 'EUR'
059700011113     C                   EVAL      vabVAS =  'ITL'
059800011113     C                   ENDIF
059900011113     C                   ENDIF
060000011113     C*
060100011113     C                   IF        vabCAS > *zeros
060200011113     C                   IF        vabVCA <> 'EUR'
060300011113     C                   EVAL      vabVCA =  'ITL'
060400011113     C                   ENDIF
060500011113     C                   ENDIF
060600011113     C*
060700011113     C                   IF        vabVMD > *zeros
060800020305     C                   IF        vabVAD <> 'EUR'
060900011113     C                   EVAL      vabVAD =  'ITL'
061000011113     C                   ENDIF
061100011113     C                   ENDIF
061200011113     C*
061300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
061400011113     C                   Z-ADD     vabIAS        wrkDec
061500011113     C                   IF        wrkDec > *zeros
061600011113     C                   IF        vabVAS = 'ITL'
061700011113     C                   EVAL      vabIAS = *zeros
061800011113     C                   ENDIF
061900011113     C                   ENDIF
062000011113     C*
062100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
062200011113     C                   Z-ADD     vabCAS        wrkDec
062300011113     C                   IF        wrkDec > *zeros
062400011113     C                   IF        vabVCA = 'ITL'
062500011113     C                   EVAL      vabCAS = *zeros
062600011113     C                   ENDIF
062700011113     C                   ENDIF
062800011113     C*
062900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
063000011113     C                   Z-ADD     vabVMD        wrkDec
063100011113     C                   IF        wrkDec > *zeros
063200011113     C                   IF        vabVAD = 'ITL'
063300011113     C                   EVAL      vabVMD = *zeros
063400011113     C                   ENDIF
063500011113     C                   ENDIF
063600011113     C*
063700011113     C                   ENDSR
063800011113     C***
063900011113
064000011113
064100000801
064200000801
064300990920      /TITLE Invio dei dati al punto operativo.
064400010202     C     invio         BEGSR
064500990920     C*
064600021113     C* 1° invio FIVAT
064700010201     C                   reset                   dscmz
064800010201     C                   move      vlrpoi        cmzdst
064900021113     C                   eval      cmzfld = 'FIVATWWR'
065000010201     C                   eval      cmzmbd = vlrhdl
065100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
065200021009     C***                if        prmfir = *blanks
065300021113     C                   eval      cmzfla = 'FIVAT00F'
065400021113     C                   eval      cmzmba = 'FIVAT00F'
065500021009     C***                else
065600021009     C***                eval      cmzfla = prmfir
065700021009     C***                eval      cmzmba = prmfir
065800021009     C***                endif
065900010201     C                   eval      cmznrr = *zeros
066000020305     C                   move      §ctrokvt      cmznrr
066100021018     C                   eval      cmzlba = vlrfl1
066200010201     C                   call(e)   'TIS711C'
066300010201     C                   parm                    dscmz
066400010201     C                   parm      *blanks       esito
066500010205     C                   if        %error
066600010205     C                             or cmzerr = '1'
066700010205     C                             or esito  = '1'
066800010205     C                   eval      wrkesito = '3'
066900010205     C                   else
067000010201     C*
067100021113     C* 2° invio FIVAB
067200010201     C                   reset                   dscmz
067300010201     C                   move      vlrpoi        cmzdst
067400010201     C                   eval      cmzfld = vlrfou
067500010201     C                   eval      cmzmbd = vlrhdl
067600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067700021009     C***                if        prmfir = *blanks
067800021113     C                   eval      cmzfla = 'FIVAB00F'
067900021113     C                   eval      cmzmba = 'FIVAB00F'
068000021009     C***                else
068100021009     C***                eval      cmzfla = prmfir
068200021009     C***                eval      cmzmba = prmfir
068300021009     C***                endif
068400010201     C                   eval      cmznrr = *zeros
068500010201     C                   move      §ctrokvb      cmznrr
068600021018     C                   eval      cmzlba = vlrfl1
068700010201     C                   call(e)   'TIS711C'
068800010201     C                   parm                    dscmz
068900010201     C                   parm      *blanks       esito
069000010201     C                   if        %error
069100010201     C                             or cmzerr = '1'
069200010201     C                             or esito  = '1'
069300010201     C                   eval      wrkesito = '3'
069400010201     C                   endif
069500010205     C                   endif
069600990920     C*
069700000613     C                   ENDSR
069800000613     C***
069900070411
070000070411     C     *pssr         BEGSR
070100070411     C*
070200070411     C                   if        %open(tivin00r)
070300070411     C                   close     tivin00r
070400070411     C                   endif
070500070411     C                   if        %open(fivabwwr)
070600070411     C                   close     fivabwwr
070700070411     C                   endif
070800070411     C                   if        %open(fivatwwr)
070900070411     C                   close     fivatwwr
071000070411     C                   endif
071100070411     C*
071200070411     C* Effettuo la chiamata al CLLE preposto
071300070411     C                   call(e)   'TITVVTC'
071400070411     C                   parm                    parccm
071500070411     C                   parm                    parmbr
071600070411     C                   parm      '2'           paropz
071700070411     C*
071800070411     C                   eval      wrkesito = '2'
071900070411     C*
072000070411     C                   seton                                        LR
072100070411     C*
072200070411     C                   ENDSR     '*CANCL'
072300070411     C***
072400070411
072500990910
072600000613     C     *inzsr        BEGSR
072700990910     C*
072800990910     C     *entry        plist
072900990920     C                   parm                    tivlrds
073000990921     C                   parm      wrkesito      esito
073100000724     C                   parm                    prmlit
073200000710     C                   parm                    prmfir
073300000613     C*
073400000830     C* CALCOLA LA DATA CORRENTE
073500100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
073600100324     C                   eval      datcor = %dec(%date() : *ISO)
073700000830     C*
073800000613     C                   ENDSR
073900000613     C***
