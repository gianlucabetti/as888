000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200121001      *
000300121001      * PARTICOLARITA':
000400121001      * Il file è di una riga per collo ma il peso ed i colli sono già il totale per spedizione.
000500121001      * Dall'esempio pare anche il volume e il valore merce dichiarato.
000600121001      * C'è solo il Barcode, la routine per il nr.telefonico è un refuso.
000700121001      *
000800121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000900990908
001000020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
001100021113     FFIVABwwr  O    E             DISK    usropn
001200021113     FFIVATwwr  O    E             DISK    usropn
001300990908
001400000801     D*----------------------------------------------------
001500000801     D* DICHIARAZIOINE VARIABILI DI WRK
001600000801     D*----------------------------------------------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000990920     D tivlrds       e ds                  extname(tivlr00f)
002100070730     D tisi95ds      e ds
002200990910     D esito           s              1
002300000724     D prmlit          s             10
002400000710     D prmfir          s             10
002500990921     D wrkesito        s                   like(esito)
002600000613     D rrnum           s              6  0 INZ(*zeros)
002700110622     D depspe          s             10    INZ(*blanks)
002800110622     D curspe          s             10    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500150325     D w70             s             70
003600000830
003700020725
003800020725     D*------------------
003900020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004000020725     D*------------------
004100070530     D INPUT_DS        DS                  INZ
004200101007     D  VINDTA                     2048
004300101007     D  VINFLG                        1
004400110524     D  VINSPE                        8
004500101007     D  VINRRN                        8  0
004600020725     D*
004700080923     D*------------------
004800080923     D* DS REPERIMENTO NUMERATORE
004900080923     D*------------------
005000080923     D trul33ds      e ds                  inz
005100080923     D*------------------
005200080923     D* DS ARCHITETTURA
005300080923     D*------------------
005400080923     D kpjba         e ds                  inz
005500080923     D*------------------
005600081113
005700081113     D*------------------
005800081113     D* LINKING A DEFINIZIONI ESTERNE
005900081113     D*------------------
006000100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006100081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006200081113
006300990908
006400010201
006500010201
006600080117     C*
006700080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
006800080117     C
006900080117     C/EXEC SQL
007000080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007100080117     C/END-EXEC
007200080117     C
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600000613     C*
007700070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
007800000613     C*
007900010202     C* Effettuo la chiamata al CLLE preposto
008000050128     C                   call(e)   'TITVVTC'
008100010202     C                   parm                    parccm
008200010202     C                   parm                    parmbr
008300010202     C                   parm      '2'           paropz
008400070730     C*
008500070730     C* Effettuo lancio TISI95 solo x chiusura
008600070730     C                   CLEAR                   TISI95DS
008700070730     C                   EVAL      I95TLA = 'C'
008800070730     C                   CALL      'TISI95R'
008900070730     C                   PARM                    TISI95DS
009000000616     C*
009100000801     C
009200010201     C                   seton                                        LR
009300000801
009400910830     C*--------------------------------------------------------
009500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009600910830     C*--------------------------------------------------------
009700070530     C     RWFILE        BEGSR
009800990910     C*
009900990914     C                   if        not %open(tivin00r)
010000990908     C                   open      tivin00r
010100990914     C                   endif
010200021113     C                   if        not %open(fivabwwr)
010300021113     C                   open      fivabwwr
010400990914     C                   endif
010500070530     C*
010600021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010700020305     C                   exsr      prevat
010800010201     C*
010900010202     C                   if        chkcall = '0'
011000010202     C*
011100021113     C                   if        not %open(fivatwwr)
011200021113     C                   open      fivatwwr
011300010201     C                   endif
011400080117     C*
011500080117     C                   EXSR      INZVAR
011600080117     C                   EXSR      DEFCAM
011700990910     C*
011800010201     C                   clear                   §CTROKVB          5 0
011900020305     C                   clear                   §CTROKVT          5 0
012000000801     C                   clear                   §CTRMO            5 0
012100000801     C                   clear                   §CTRNO            5 0
012200990910     C*
012300020725     C/EXEC SQL
012400020725     C+ declare C1 cursor for select
012500110622     C+ vindta, vinflg, substr(vindta, 187, 10) as sped, rrn(tivin00r)
012600060223     C+ from tivin00r
012700060223     C+ order by sped
012800020725     C+ for read only
012900020725     C/END-EXEC
013000020725     C
013100020725     C/EXEC SQL
013200020725     C+ open C1
013300020725     C/END-EXEC
013400020725     C
013500020725     C/EXEC SQL
013600070530     C+ Fetch C1 into :INPUT_DS
013700020725     C/END-EXEC
013800020725     C*
013900020725     C                   dow       sqlcod = *zeros
014000990913     C*
014100020725     C                   if        vindta > *blanks
014200000613     C                   add       1             rrnum
014300000801     C*
014400020725     C                   if        vinflg = *blanks
014500020725     C                             or vinflg = '0'
014600020725     C                             or vinflg = '2'
014700000801     C*
014800020725     C                   clear                   x_vinmsg
014900020725     C                   eval      x_vinflg = '1'
015000010305     C*
015100010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015200121001     C                   EVAL      PiStr=%trim(%subst(vindta:163:16))
015300020305     C                   MOVEL(p)  PiStr         curspe
015400010305     C*
015500071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015600080923     C                   if        *in50 = *off
015700071003     C                   exsr      impvab
015800071003     C                   exsr      wrivab
015900121001     C***                exsr      wrivat_b                                     => carico VAT
016000071003     C                   else
016100080923     C*
016200071009     C                   if        wDISK = 'D'
016300071009     C                   exsr      impvab
016400071009     C                   exsr      wrivab
016500121001     C***                exsr      wrivat_b                                     => carico VAT
016600071009     C                   exsr      wrivat_e                                     => carico VAT
016700071009     C                   else
016800071009     C*
016900010305     C                   if        depspe = *blanks                             => 1° giro
017000010305     C                   eval      depspe = curspe                              => memorizz. spediz
017100080117     C                   clear                   tivinds
017200020305     C                   exsr      impvab
017300121001     C***                exsr      wrivat_b                                     => carico VAT
017400071003     C   50              exsr      wrivat_e                                     => carico VAT
017500010305     C                   else
017600020725     C                   if        depspe <> curspe                             => rottura di spediz
017700010305     C                   eval      depspe = curspe                              => memorizz. spediz
017800070928     C                   exsr      wrivab
017900080117     C                   clear                   tivinds
018000020305     C                   exsr      impvab
018100121001     C***                exsr      wrivat_b                                     => carico VAT
018200071003     C   50              exsr      wrivat_e                                     => carico VAT
018300020305     C                   else                                                   => x stessa spediz
018400090204     C***                exsr      impvab
018500121001     C***                exsr      wrivat_b                                     => carico VAT
018600071003     C   50              exsr      wrivat_e                                     => carico VAT
018700010305     C                   endif
018800010305     C                   endif
018900010305     C                   endif
019000071003     C                   endif
019100071009     C                   endif
019200000905     C*
019300000905     C                   else
019400020725     C                   eval      x_vinflg = '1'
019500000905     C                   endif
019600000905     C*
019700020725     C     VINRRN        chain     tivin000
019800020725     C                   if        %found(tivin00r)
019900020725     C                   eval      y_vinflg = x_vinflg
020000020725     C                   eval      y_vinmsg = x_vinmsg
020100020725     C                   update    tivin000
020200020725     C                   endif
020300020725     C*
020400020725     C/EXEC SQL
020500070530     C+ Fetch C1 into :INPUT_DS
020600020725     C/END-EXEC
020700020725     C*
020800020725     C                   enddo
020900020725     C*
021000020725     C/EXEC SQL
021100020725     C+ close C1
021200020725     C/END-EXEC
021300000905     C*
021400020305     C* Scarico i VAB rimasti "in sospeso"
021500071009     C                   if        wDISK = 'C'
021600071009     C                   exsr      wrivab
021700071009     C                   endif
021800010202     C*
021900010202     C                   endif
022000990910
022100990910     C* Se non ci sono record con errori ...
022200000710     C                   if        §ctrno = 0
022300990910     C* ... restituisco esito OK.
022400990921     C                   eval      wrkesito = '0'
022500990910     C                   else
022600010201     C                   if        §ctrokvb > 0
022700990921     C                   eval      wrkesito = '1'
022800000710     C                   else
022900000710     C                   eval      wrkesito = '2'
023000990910     C                   endif
023100000710     C                   endif
023200990910     C*
023300990914     C                   if        %open(tivin00r)
023400990908     C                   close     tivin00r
023500990914     C                   endif
023600021113     C                   if        %open(fivabwwr)
023700021113     C                   close     fivabwwr
023800990914     C                   endif
023900021113     C                   if        %open(fivatwwr)
024000021113     C                   close     fivatwwr
024100010201     C                   endif
024200990910     C*
024300010201     C                   if        §ctrokvb > 0
024400000724     C                             and vlrpoi <> *zeros
024500010202     C                   exsr      invio
024600990920     C                   endif
024700990920     C*
024800910830     C                   ENDSR
024900000613     C***
025000010305
025100010305     C*----------------------------------------------------*
025200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025300010305     C*----------------------------------------------------*
025400020305     C     WRIVAB        BEGSR
025500070730     C*
025600071003     C* Considerazioni finali
025700071003     C                   if        VABRMA = *blanks
025800071003     C                   movel(P)  VABRMN        VABRMA
025900071003     C                   endif
026000071003     C*
026100021113     C                   write     fivab000                                     => scarico il VAB
026200010305     C*
026300010305     C                   ENDSR
026400990920
026500000801     C*----------------------------------------------------*
026600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026700000801     C*----------------------------------------------------*
026800010201     C     INZVAR        BEGSR
026900000801     C*
027000010201     C                   Z-ADD     *zeros        Num5_0
027100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027200020725     C                   MOVEL     '0'           FlgCAS            1
027300000801     C*
027400000801     C                   ENDSR
027500000801     C*----------------------------------------------------*
027600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
027700000801     C*----------------------------------------------------*
027800020904     C     DEFCAM        BEGSR
027900080923     C*
028000080923     C                   SETOFF                                       5051
028100000801     C*
028200021113     C                   CLEAR                   FIVAB000
028300021113     C                   CLEAR                   FIVAT000
028400070730     C* Imposto i valori di default...
028500121001     C                   EVAL      VABCCM = 2494811
028600121001     C                   EVAL      VATCCM = 2494811
028700121001     C                   EVAL      VABLNP = 249
028800121001     C                   EVAL      VATLNP = 249
028900120628     C                   EVAL      VABCTR = 000
029000120628     C                   EVAL      VABCTM = '7Q'
029100070730     C                   EVAL      VABCBO = '1'
029200070222     C* ... e poi verifico se sono stati passati come parametri
029300070222     C                   IF        vlrppt > *blanks
029400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
029500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
029600070222     C                   EXSR      CHKNUM
029700070222     C                   IF        PiInt=*on
029800070222     C                   Z-ADD     PiVal         VABCCM
029900070222     C                   Z-ADD     PiVal         VATCCM
030000070222     C                   ENDIF
030100070222     C                   ENDIF
030200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
030300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
030400070222     C                   EXSR      CHKNUM
030500070222     C                   IF        PiInt=*on
030600070222     C                   Z-ADD     PiVal         VABLNP
030700070222     C                   Z-ADD     PiVal         VATLNP
030800070222     C                   ENDIF
030900070222     C                   ENDIF
031000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
031100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
031200070222     C                   EXSR      CHKNUM
031300070222     C                   IF        PiInt=*on
031400070222     C                   Z-ADD     PiVal         VABCTR
031500070222     C                   ENDIF
031600070928     C                   ENDIF
031700071009     C                   MOVEL     *blanks       wDISK             1
031800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
031900071009     C                   IF        wDISK <> *blanks
032000070928     C                   SETON                                        50
032100070222     C                   ENDIF
032200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
032300080923     C                   SETON                                        51
032400080923     C                   ENDIF
032500070222     C                   ENDIF
032600071009     C*
032700071009     C   50              EVAL      VABCTM = '7Q'
032800000801     C*
032900000801     C                   ENDSR
033000000801     C*----------------------------------------------------*
033100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033200000801     C*----------------------------------------------------*
033300010201     C     IMPVAB        BEGSR
033400070530     C*
033500070530     C                   SETOFF                                       3132
033600070928     C*
033700070928     C                   EXSR      INZVAR
033800070928     C                   EXSR      DEFCAM
033900070928     C*
034000070928     C                   MOVE(P)   vlrpoi        VABFGS
034100070928     C                   MOVE(P)   vlrpoi        VATFGS
034200070928     C*
034300070928     C                   MOVEL     datcor        VABAAS
034400070928     C                   MOVEL     datcor        VATAAS
034500070928     C                   MOVE      datcor        VABMGS
034600121001     C                   EVAL      VABRSD=%trim(%subst(vindta:11:40))
034700070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
034800070928     C     '@':'A'       XLATE     VABRSD        VABRSD
034900070928     C* ==
035000121001     C                   EVAL      VABIND=%trim(%subst(vindta:91:32))
035100121001     C                   EVAL      VABLOD=%trim(%subst(vindta:123:32))
035200121001     C                   EVAL      VABPRD=%trim(%subst(vindta:155:2))
035300121001     C                   EVAL      VABRMA=%trim(%subst(vindta:163:16))
035400150325     C                   EVAL      w70=%trim(%subst(vindta:426:50)) +
035500150325     C                                 %trim(%subst(vindta:286:20))
035600150325     C                   EVAL      VABNOT=%subst(w70:1:35)
035700150325     C                   EVAL      VABNT2=%subst(w70:36:35)
035800151211     C                   IF        %trim(%subst(vindta:344:2)) = 'PT'
035900151211     C                   EVAL      VABTC1='A'
036000151211     C                   ENDIF
036100160503     C                   IF        %trim(%subst(vindta:344:2)) = 'PE'
036200160503     C                   EVAL      VABTSP='E'
036300160503     C                   ENDIF
036400160726     C                   EVAL      VABRMO=%trim(%subst(vindta:385:40))
036500160726     C                   EVAL      VABNAS=%trim(%subst(vindta:207:15))
036600120717     C*
036700071003     C* CAD
036800121001     C                   EVAL      PiStr=%trim(%subst(vindta:157:5))
036900121001     C                   EXSR      CHKNUM
037000121001     C                   IF        PiInt=*on
037100121001     C                   Z-ADD     PiVal         Num5_0
037200121001     C                   MOVEL     Num5_0        VABCAD
037300121001     C                   ELSE
037400121001     C                   SETON                                        32
037500121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
037600121001     C                             + ' ' + 'VABCAD'
037700121001     C                   ENDIF
037800151211     C* DCR
037900151211     C                   IF        %trim(%subst(vindta:201:6)) <> *blanks AND
038000151211     C                             %trim(%subst(vindta:201:6)) <> *zeros
038100151211     C                   EVAL      PiStr='20'+%trim(%subst(vindta:201:6))
038200151211     C                   EXSR      CHKNUM
038300151211     C                   IF        PiInt=*on
038400151211     C                   Z-ADD     PiVal         VABDCR
038500151211     C                   ELSE
038600151211     C                   SETON                                        32
038700151211     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038800151211     C                             + ' ' + 'VABDCR'
038900151211     C                   ENDIF
039000151211     C                   ENDIF
039100080923     C* RMN
039200121001     C                   EVAL      PiStr=%trim(%subst(vindta:163:9))
039300121001     C                   EXSR      CHKNUM
039400121001     C                   IF        PiInt=*on
039500121001     C                   Z-ADD     PiVal         VABRMN
039600121001     C                   ELSE
039700121001     C                   SETON                                        32
039800121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
039900121001     C                             + ' ' + 'VABRMN'
040000121001     C                   ENDIF
040100080923     C* NSP
040200080923     C                   IF        *in51 = *on
040300080923     C* NSP => Stacco un numeratore da AZNUM
040400080923     C                   clear                   TRUL33DS
040500080923     C                   eval      I33OPE = *zeros
040600080923     C                   eval      I33CNU = 302
040700080923     C                   eval      I33NUM = 1
040800080923     C                   movel     TRUL33DS      KPJBU
040900080923     C                   call      'TRUL33R'
041000080923     C                   parm                    KPJBA
041100080923     C                   movel     KPJBU         TRUL33DS
041200080923     C                   if        O33ERR = *zeros
041300080923     C                   z-add     O33NRF        VABNSP
041400080923     C                   z-add     O33NRF        VATNSP
041500080923     C                   else
041600080923     C                   SETON                                        31
041700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
041800080923     C                             + ' ' + 'VABNSP VATNSP'
041900080923     C                   endif
042000080923     C                   ELSE
042100110622     C                   EVAL      PiStr=%trim(%subst(vindta:187:10))
042200080923     C                   EXSR      CHKNUM
042300080923     C                   IF        PiInt=*on
042400080923     C                   Z-ADD     PiVal         VABNSP
042500080923     C                   Z-ADD     PiVal         VATNSP
042600080923     C                   ELSE
042700080923     C                   SETON                                        31
042800080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
042900080923     C                             + ' ' + 'VABNSP VATNSP'
043000080923     C                   ENDIF
043100080923     C                   ENDIF
043200071003     C* NCL
043300121001     C                   EVAL      PiStr=%trim(%subst(vindta:222:5))
043400070928     C                   EXSR      CHKNUM
043500070928     C                   IF        PiInt=*on
043600070928     C                   Z-ADD     PiVal         VABNCL
043700070928     C                   ELSE
043800070928     C                   SETON                                        32
043900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044000070928     C                             + ' ' + 'VABNCL'
044100070928     C                   ENDIF
044200071003     C* PKB
044300121001     C                   EVAL      PiStr=%trim(%subst(vindta:227:5))
044400070928     C                   EXSR      CHKNUM
044500070928     C                   IF        PiNum=*on
044600070928     C                   Z-ADD     PiVal         VABPKB
044700070928     C                   ELSE
044800070928     C                   SETON                                        32
044900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045000070928     C                             + ' ' + 'VABPKB'
045100070928     C                   ENDIF
045200121001     C*VLB
045300121001     C                   EVAL      PiStr=%trim(%subst(vindta:232:5))
045400121001     C                   EXSR      CHKNUM
045500121001     C                   IF        PiNum=*on
045600121001     C* il volume ha 2 decimali ma viene scritto senza separatore decimale
045700161006     C                   EVAL      PiVal = PiVal/100
045800161006     C                   Z-ADD     PiVal         VABVLB
045900121001     C                   ELSE
046000121001     C                   SETON                                        32
046100121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046200121001     C                             + ' ' + 'VABVLB'
046300121001     C                   ENDIF
046400121001     C*VMD
046500121001     C                   EVAL      PiStr=%trim(%subst(vindta:237:13))
046600121001     C                   EXSR      CHKNUM
046700121001     C                   IF        PiNum=*on
046800121001     C* il valore merce dichiarato ha 2 decimali ma viene scritto senza separatore decimale
046900121001     C                   EVAL      VABVMD = PiVal/100
047000160310     C                   EVAL      VABVAD = 'EUR'
047100121001     C                   ELSE
047200121001     C                   SETON                                        32
047300121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047400121001     C                             + ' ' + 'VABVMD'
047500121001     C                   ENDIF
047600080923     C* CAS
047700121001     C* l'importo contrassegno ha 2 decimali ma non si sa se viene scritto senza separatore decimale
047800121001     C* (ipotizziamo senza)
047900161222     C                   EVAL      VABTIC=*blank
048000121001     C                   IF        %trim(%subst(vindta:313:13))<>*blanks
048100080923     C                             AND
048200121001     C                             %trim(%subst(vindta:313:13))<>*zeros
048300080923     C                             AND
048400121001     C                             %trim(%subst(vindta:313:13))<>'0,00'
048500080923     C                             AND
048600121001     C                             %trim(%subst(vindta:313:13))<>'0000000000,00'
048700080923     C                             AND
048800121001     C                             %trim(%subst(vindta:313:13))<>'0000000000.00'
048900070928     C                   MOVEL     '1'           FlgCAS
049000121001     C                   EVAL      PiStr=%trim(%subst(vindta:313:13))
049100070928     C                   EXSR      CHKNUM
049200070928     C                   IF        PiNum=*on
049300170927     C                   EVAL      VABVCA = 'EUR'
049400121001     C                   EVAL      VABCAS = PiVal/100
049500161222     C                   EVAL      VABTIC=%subst(vindta:179:2)
049600070928     C                   ELSE
049700070928     C                   SETON                                        32
049800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049900070928     C                             + ' ' + 'VABCAS'
050000070928     C                   ENDIF
050100070928     C                   ENDIF
050200070730     C*
050300070730     C* Considerazioni finali su CBO/CAS
050400070730     C                   IF        FlgCAS = '1'
050500070730     C                   IF        VABCBO = '1'
050600070730     C                   EVAL      VABCBO = '4'
050700070730     C                   ENDIF
050800070730     C                   IF        VABCBO = '2'
050900070730     C                   EVAL      VABCBO = '6'
051000070730     C                   ENDIF
051100070730     C                   ENDIF
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
053900070928     C                   if        %subst(vindta:92:15) <> *blanks
054000070928     C                   eval      VATTRC = 'B'
054100121001     C                   eval      VATNOT = %trim(%subst(vindta:01:01))
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
055800121001     C                   if        %subst(vindta:355:16) <> *blanks
055900071003     C                   eval      VATTRC = 'E'
056000121001     C                   eval      VATNOT = %trim(%subst(vindta:355:16))
056100071003     C                   write     FIVAT000
056200071003     C                   endif
056300071003     C*
056400071003     C                   ENDSR
056500020904
056600020904
056700010202     C*----------------------------------------------------*
056800021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
056900010202     C*----------------------------------------------------*
057000020305     C     PREVAT        BEGSR
057100010202     C*
057200021113     C* Compongo il nome del membro da dare al FIVATWWR
057300010202     C                   eval      parmbr = vlrhdl
057400010202     C                   movel     'M'           parmbr
057500070530     C                   eval      parccm = %subst(vlrKSC:2:7)
057600010202     C                   eval      paropz = '1'
057700010202     C* Effettuo la chiamata al CLLE preposto
057800050128     C                   call(e)   'TITVVTC'
057900010202     C                   parm                    parccm
058000010202     C                   parm                    parmbr
058100010202     C                   parm                    paropz
058200010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
058300010202     C                   if        %error
058400010202     C                   movel     '1'           chkcall
058500010202     C                   else
058600010202     C                   movel     '0'           chkcall
058700010202     C                   endif
058800010202     C*
058900010202     C                   ENDSR
059000000801     C*----------------------------------------------------*
059100000801     C*  CONTROLLO NUMERICITA' CAMPI
059200000801     C*----------------------------------------------------*
059300000801     C     CHKNUM        BEGSR
059400081113     C*
059500081113     C                   IF        PiDecChr = *blanks
059600110524     C                   EVAL      PiDecChr = '.'
059700081113     C                   ENDIF
059800081113     C*
059900081113     C                   callp(e)  UBISNUM_Check(PiStr
060000081113     C                                          :PiDecChr
060100081113     C                                          :PiVal
060200081113     C                                          :PiNum
060300081113     C                                          :PiInt)
060400081113     C*
060500000801     C                   IF        %error
060600000801     C                   EVAL      PiInt=*off
060700000801     C                   ENDIF
060800000801     C*
060900000801     C                   ENDSR
061000000801     C***
061100000801
061200011113
061300011113     C*----------------------------------------------------*
061400011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
061500011113     C*----------------------------------------------------*
061600011113     C     CHKIMPDIV     BEGSR
061700011113     C*
061800011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
061900011113     C                   Z-ADD     *zeros        wrkDec            9 9
062000011113     C*
062100011113     C* Come prima cosa effettuo considerazioni sulla divisa
062200011113     C                   IF        vabIAS > *zeros
062300011113     C                   IF        vabVAS <> 'EUR'
062400011113     C                   EVAL      vabVAS =  'ITL'
062500011113     C                   ENDIF
062600011113     C                   ENDIF
062700011113     C*
062800011113     C                   IF        vabCAS > *zeros
062900011113     C                   IF        vabVCA <> 'EUR'
063000011113     C                   EVAL      vabVCA =  'ITL'
063100011113     C                   ENDIF
063200011113     C                   ENDIF
063300011113     C*
063400011113     C                   IF        vabVMD > *zeros
063500020305     C                   IF        vabVAD <> 'EUR'
063600011113     C                   EVAL      vabVAD =  'ITL'
063700011113     C                   ENDIF
063800011113     C                   ENDIF
063900011113     C*
064000011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
064100011113     C                   Z-ADD     vabIAS        wrkDec
064200011113     C                   IF        wrkDec > *zeros
064300011113     C                   IF        vabVAS = 'ITL'
064400011113     C                   EVAL      vabIAS = *zeros
064500011113     C                   ENDIF
064600011113     C                   ENDIF
064700011113     C*
064800011113     C* Stabilisco se il contrasegno ha decimali valorizzati
064900011113     C                   Z-ADD     vabCAS        wrkDec
065000011113     C                   IF        wrkDec > *zeros
065100011113     C                   IF        vabVCA = 'ITL'
065200011113     C                   EVAL      vabCAS = *zeros
065300011113     C                   ENDIF
065400011113     C                   ENDIF
065500011113     C*
065600011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
065700011113     C                   Z-ADD     vabVMD        wrkDec
065800011113     C                   IF        wrkDec > *zeros
065900011113     C                   IF        vabVAD = 'ITL'
066000011113     C                   EVAL      vabVMD = *zeros
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C                   ENDSR
066500011113     C***
066600011113
066700011113
066800000801
066900000801
067000990920      /TITLE Invio dei dati al punto operativo.
067100010202     C     invio         BEGSR
067200990920     C*
067300021113     C* 1° invio FIVAT
067400010201     C                   reset                   dscmz
067500010201     C                   move      vlrpoi        cmzdst
067600021113     C                   eval      cmzfld = 'FIVATWWR'
067700010201     C                   eval      cmzmbd = vlrhdl
067800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
067900021009     C***                if        prmfir = *blanks
068000021113     C                   eval      cmzfla = 'FIVAT00F'
068100021113     C                   eval      cmzmba = 'FIVAT00F'
068200021009     C***                else
068300021009     C***                eval      cmzfla = prmfir
068400021009     C***                eval      cmzmba = prmfir
068500021009     C***                endif
068600010201     C                   eval      cmznrr = *zeros
068700020305     C                   move      §ctrokvt      cmznrr
068800021018     C                   eval      cmzlba = vlrfl1
068900010201     C                   call(e)   'TIS711C'
069000010201     C                   parm                    dscmz
069100010201     C                   parm      *blanks       esito
069200010205     C                   if        %error
069300010205     C                             or cmzerr = '1'
069400010205     C                             or esito  = '1'
069500010205     C                   eval      wrkesito = '3'
069600010205     C                   else
069700010201     C*
069800021113     C* 2° invio FIVAB
069900010201     C                   reset                   dscmz
070000010201     C                   move      vlrpoi        cmzdst
070100010201     C                   eval      cmzfld = vlrfou
070200010201     C                   eval      cmzmbd = vlrhdl
070300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
070400021009     C***                if        prmfir = *blanks
070500021113     C                   eval      cmzfla = 'FIVAB00F'
070600021113     C                   eval      cmzmba = 'FIVAB00F'
070700021009     C***                else
070800021009     C***                eval      cmzfla = prmfir
070900021009     C***                eval      cmzmba = prmfir
071000021009     C***                endif
071100010201     C                   eval      cmznrr = *zeros
071200010201     C                   move      §ctrokvb      cmznrr
071300021018     C                   eval      cmzlba = vlrfl1
071400010201     C                   call(e)   'TIS711C'
071500010201     C                   parm                    dscmz
071600010201     C                   parm      *blanks       esito
071700010201     C                   if        %error
071800010201     C                             or cmzerr = '1'
071900010201     C                             or esito  = '1'
072000010201     C                   eval      wrkesito = '3'
072100010201     C                   endif
072200010205     C                   endif
072300990920     C*
072400000613     C                   ENDSR
072500000613     C***
072600070411
072700161006     C     *pssr         BEGSR
072800070411     C*
072900161006     C                   if        %open(tivin00r)
073000161006     C                   close     tivin00r
073100161006     C                   endif
073200161006     C                   if        %open(fivabwwr)
073300161006     C                   close     fivabwwr
073400161006     C                   endif
073500161006     C                   if        %open(fivatwwr)
073600161006     C                   close     fivatwwr
073700161006     C                   endif
073800070411     C*
073900070411     C* Effettuo la chiamata al CLLE preposto
074000161006     C                   call(e)   'TITVVTC'
074100161006     C                   parm                    parccm
074200161006     C                   parm                    parmbr
074300161006     C                   parm      '2'           paropz
074400070411     C*
074500161006     C                   eval      wrkesito = '2'
074600070411     C*
074700161006     C                   seton                                        LR
074800070411     C*
074900161006     C                   ENDSR     '*CANCL'
075000070411     C***
075100070411
075200990910
075300000613     C     *inzsr        BEGSR
075400990910     C*
075500990910     C     *entry        plist
075600990920     C                   parm                    tivlrds
075700990921     C                   parm      wrkesito      esito
075800000724     C                   parm                    prmlit
075900000710     C                   parm                    prmfir
076000000613     C*
076100000830     C* CALCOLA LA DATA CORRENTE
076200100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
076300100324     C                   eval      datcor = %dec(%date() : *ISO)
076400000830     C*
076500000613     C                   ENDSR
076600000613     C***
