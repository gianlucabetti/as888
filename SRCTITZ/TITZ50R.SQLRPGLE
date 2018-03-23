000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200160211      *
000300121001      * PARTICOLARITA':
000400121011      * Anche in caso di spedizione in contrassegno lasciare il campo VABCBO = 1 per spedizione.
000500140306      * Gli indirizzi email possono essere più d'uno divisi da ";"
000600140306      * si prende solo il primo
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
002700121011     D depspe          s              6    INZ(*blanks)
002800121011     D curspe          s              6    INZ(*blanks)
002900010202     D parccm          s              8    INZ(*blanks)
003000010202     D parmbr          s             10    INZ(*blanks)
003100010202     D paropz          s              1    INZ(*blanks)
003200010202     D chkcall         s              1    INZ(*blanks)
003300080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
003400070530     D Num5_0          s              5  0
003500121011     D w70             s             70
003600140306     D wPos            s              3  0
003700151014     D wColloPrg       s              3  0
003800000830
003900020725
004000020725     D*------------------
004100020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004200020725     D*------------------
004300070530     D INPUT_DS        DS                  INZ
004400101007     D  VINDTA                     2048
004500101007     D  VINFLG                        1
004600121011     D  VINSPE                        6
004700101007     D  VINRRN                        8  0
004800020725     D*
004900080923     D*------------------
005000080923     D* DS REPERIMENTO NUMERATORE
005100080923     D*------------------
005200080923     D trul33ds      e ds                  inz
005300080923     D*------------------
005400080923     D* DS ARCHITETTURA
005500080923     D*------------------
005600080923     D kpjba         e ds                  inz
005700080923     D*------------------
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
008200050128     C                   call(e)   'TITVVTC'
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
009500000801
009600910830     C*--------------------------------------------------------
009700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
009800910830     C*--------------------------------------------------------
009900070530     C     RWFILE        BEGSR
010000990910     C*
010100990914     C                   if        not %open(tivin00r)
010200990908     C                   open      tivin00r
010300990914     C                   endif
010400021113     C                   if        not %open(fivabwwr)
010500021113     C                   open      fivabwwr
010600990914     C                   endif
010700070530     C*
010800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
010900020305     C                   exsr      prevat
011000010201     C*
011100010202     C                   if        chkcall = '0'
011200010202     C*
011300021113     C                   if        not %open(fivatwwr)
011400021113     C                   open      fivatwwr
011500010201     C                   endif
011600080117     C*
011700080117     C                   EXSR      INZVAR
011800080117     C                   EXSR      DEFCAM
011900990910     C*
012000010201     C                   clear                   §CTROKVB          5 0
012100020305     C                   clear                   §CTROKVT          5 0
012200000801     C                   clear                   §CTRMO            5 0
012300000801     C                   clear                   §CTRNO            5 0
012400990910     C*
012500020725     C/EXEC SQL
012600020725     C+ declare C1 cursor for select
012700121011     C+ vindta, vinflg, substr(vindta, 3, 6) as sped, rrn(tivin00r)
012800060223     C+ from tivin00r
012900060223     C+ order by sped
013000020725     C+ for read only
013100020725     C/END-EXEC
013200020725     C
013300020725     C/EXEC SQL
013400020725     C+ open C1
013500020725     C/END-EXEC
013600020725     C
013700020725     C/EXEC SQL
013800070530     C+ Fetch C1 into :INPUT_DS
013900020725     C/END-EXEC
014000020725     C*
014100020725     C                   dow       sqlcod = *zeros
014200990913     C*
014300020725     C                   if        vindta > *blanks
014400000613     C                   add       1             rrnum
014500000801     C*
014600020725     C                   if        vinflg = *blanks
014700020725     C                             or vinflg = '0'
014800020725     C                             or vinflg = '2'
014900000801     C*
015000020725     C                   clear                   x_vinmsg
015100020725     C                   eval      x_vinflg = '1'
015200010305     C*
015300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
015400121011     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
015500020305     C                   MOVEL(p)  PiStr         curspe
015600010305     C*
015700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
015800080923     C                   if        *in50 = *off
015900071003     C                   exsr      impvab
016000071003     C                   exsr      wrivab
016100121122     C                   exsr      wrivat_b                                     => carico VAT
016200140304     C                   exsr      wrivat_i                                     => carico VAT
016300071003     C                   else
016400080923     C*
016500071009     C                   if        wDISK = 'D'
016600071009     C                   exsr      impvab
016700071009     C                   exsr      wrivab
016800121122     C                   exsr      wrivat_b                                     => carico VAT
016900140304     C                   exsr      wrivat_i                                     => carico VAT
017000151014     C                   exsr      wrivat_e                                     => carico VAT
017100071009     C                   else
017200071009     C*
017300010305     C                   if        depspe = *blanks                             => 1° giro
017400010305     C                   eval      depspe = curspe                              => memorizz. spediz
017500080117     C                   clear                   tivinds
017600020305     C                   exsr      impvab
017700121122     C                   exsr      wrivat_b                                     => carico VAT
017800140304     C                   exsr      wrivat_i                                     => carico VAT
017900151014     C   50              exsr      wrivat_e                                     => carico VAT
018000010305     C                   else
018100020725     C                   if        depspe <> curspe                             => rottura di spediz
018200010305     C                   eval      depspe = curspe                              => memorizz. spediz
018300070928     C                   exsr      wrivab
018400080117     C                   clear                   tivinds
018500020305     C                   exsr      impvab
018600121122     C                   exsr      wrivat_b                                     => carico VAT
018700140304     C                   exsr      wrivat_i                                     => carico VAT
018800151014     C   50              exsr      wrivat_e                                     => carico VAT
018900020305     C                   else                                                   => x stessa spediz
019000090204     C***                exsr      impvab
019100121122     C***                exsr      wrivat_b                                     => carico VAT
019200151014     C   50              exsr      wrivat_e                                     => carico VAT
019300010305     C                   endif
019400010305     C                   endif
019500010305     C                   endif
019600071003     C                   endif
019700071009     C                   endif
019800000905     C*
019900000905     C                   else
020000020725     C                   eval      x_vinflg = '1'
020100000905     C                   endif
020200000905     C*
020300020725     C     VINRRN        chain     tivin000
020400020725     C                   if        %found(tivin00r)
020500020725     C                   eval      y_vinflg = x_vinflg
020600020725     C                   eval      y_vinmsg = x_vinmsg
020700020725     C                   update    tivin000
020800020725     C                   endif
020900020725     C*
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   enddo
021500020725     C*
021600020725     C/EXEC SQL
021700020725     C+ close C1
021800020725     C/END-EXEC
021900000905     C*
022000020305     C* Scarico i VAB rimasti "in sospeso"
022100071009     C                   if        wDISK = 'C'
022200071009     C                   exsr      wrivab
022300071009     C                   endif
022400010202     C*
022500010202     C                   endif
022600990910
022700990910     C* Se non ci sono record con errori ...
022800000710     C                   if        §ctrno = 0
022900990910     C* ... restituisco esito OK.
023000990921     C                   eval      wrkesito = '0'
023100990910     C                   else
023200010201     C                   if        §ctrokvb > 0
023300990921     C                   eval      wrkesito = '1'
023400000710     C                   else
023500000710     C                   eval      wrkesito = '2'
023600990910     C                   endif
023700000710     C                   endif
023800990910     C*
023900990914     C                   if        %open(tivin00r)
024000990908     C                   close     tivin00r
024100990914     C                   endif
024200021113     C                   if        %open(fivabwwr)
024300021113     C                   close     fivabwwr
024400990914     C                   endif
024500021113     C                   if        %open(fivatwwr)
024600021113     C                   close     fivatwwr
024700010201     C                   endif
024800990910     C*
024900010201     C                   if        §ctrokvb > 0
025000000724     C                             and vlrpoi <> *zeros
025100010202     C                   exsr      invio
025200990920     C                   endif
025300990920     C*
025400910830     C                   ENDSR
025500000613     C***
025600010305
025700010305     C*----------------------------------------------------*
025800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025900010305     C*----------------------------------------------------*
026000020305     C     WRIVAB        BEGSR
026100070730     C*
026200071003     C* Considerazioni finali
026300071003     C                   if        VABRMA = *blanks
026400071003     C                   movel(P)  VABRMN        VABRMA
026500071003     C                   endif
026600071003     C*
026700021113     C                   write     fivab000                                     => scarico il VAB
026800010305     C*
026900010305     C                   ENDSR
027000990920
027100000801     C*----------------------------------------------------*
027200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
027300000801     C*----------------------------------------------------*
027400010201     C     INZVAR        BEGSR
027500000801     C*
027600010201     C                   Z-ADD     *zeros        Num5_0
027700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
027800020725     C                   MOVEL     '0'           FlgCAS            1
027900000801     C*
028000000801     C                   ENDSR
028100000801     C*----------------------------------------------------*
028200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
028300000801     C*----------------------------------------------------*
028400020904     C     DEFCAM        BEGSR
028500080923     C*
028600080923     C                   SETOFF                                       5051
028700000801     C*
028800021113     C                   CLEAR                   FIVAB000
028900021113     C                   CLEAR                   FIVAT000
029000070730     C* Imposto i valori di default...
029100121011     C                   EVAL      VABCCM = 0321551
029200121011     C                   EVAL      VATCCM = 0321551
029300121011     C                   EVAL      VABLNP = 032
029400121011     C                   EVAL      VATLNP = 032
029500120628     C                   EVAL      VABCTR = 000
029600121011     C                   EVAL      VABTSP = 'C'
029700121011     C                   EVAL      VABCBO = '1'
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
033100070222     C                   ENDIF
033200071009     C*
033300071009     C   50              EVAL      VABCTM = '7Q'
033400000801     C*
033500000801     C                   ENDSR
033600000801     C*----------------------------------------------------*
033700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
033800000801     C*----------------------------------------------------*
033900010201     C     IMPVAB        BEGSR
034000070530     C*
034100070530     C                   SETOFF                                       3132
034200070928     C*
034300070928     C                   EXSR      INZVAR
034400070928     C                   EXSR      DEFCAM
034500070928     C*
034600070928     C                   MOVE(P)   vlrpoi        VABFGS
034700070928     C                   MOVE(P)   vlrpoi        VATFGS
034800070928     C*
034900070928     C                   MOVEL     datcor        VABAAS
035000070928     C                   MOVEL     datcor        VATAAS
035100070928     C                   MOVE      datcor        VABMGS
035200121011     C                   EVAL      VABRSD=%trim(%subst(vindta:191:40))
035300070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
035400070928     C     '@':'A'       XLATE     VABRSD        VABRSD
035500070928     C* ==
035600121011     C                   EVAL      VABIND=%trim(%subst(vindta:231:35))
035700121011     C                   EVAL      VABLOD=%trim(%subst(vindta:266:30))
035800121011     C                   EVAL      VABPRD=%trim(%subst(vindta:296:2))
035900121011     C                   EVAL      VABRMA=%trim(%subst(vindta:330:6))
036000160211     C                                    + '-' +
036100160211     C                                    %trim(%subst(vindta:3:6))
036200121122     C                   EVAL      VABNOT=%trim(%subst(vindta:436:30))
036300121122     C                   EVAL      VABNT2='AL '+%trim(%subst(vindta:498:02))+
036400121122     C                                     '/'+ %trim(%subst(vindta:496:02))+
036500121122     C                                     '/'+ %trim(%subst(vindta:492:04))+
036600121122     C                                      ' '+%trim(%subst(vindta:396:40))
036700120717     C*
036800071003     C* CAD
036900170627     C* siccome fa dell'estero, prendiamo il CAP as is
037000170627     C                   EVAL      VABCAD=%trim(%subst(vindta:298:5))
037100170627     C***                EVAL      PiStr=%trim(%subst(vindta:298:5))
037200170627     C***                EXSR      CHKNUM
037300170627     C***                IF        PiInt=*on
037400170627     C***                Z-ADD     PiVal         Num5_0
037500170627     C***                MOVEL     Num5_0        VABCAD
037600170627     C***                ELSE
037700170627     C***                SETON                                        32
037800170627     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
037900170627     C***                          + ' ' + 'VABCAD'
038000170627     C***                ENDIF
038100080923     C* RMN
038200121011     C                   EVAL      PiStr=%trim(%subst(vindta:3:6))
038300121001     C                   EXSR      CHKNUM
038400121001     C                   IF        PiInt=*on
038500121001     C                   Z-ADD     PiVal         VABRMN
038600121001     C                   ELSE
038700121001     C                   SETON                                        32
038800121001     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
038900121001     C                             + ' ' + 'VABRMN'
039000121001     C                   ENDIF
039100080923     C* NSP
039200080923     C                   IF        *in51 = *on
039300080923     C* NSP => Stacco un numeratore da AZNUM
039400080923     C                   clear                   TRUL33DS
039500080923     C                   eval      I33OPE = *zeros
039600080923     C                   eval      I33CNU = 302
039700080923     C                   eval      I33NUM = 1
039800080923     C                   movel     TRUL33DS      KPJBU
039900080923     C                   call      'TRUL33R'
040000080923     C                   parm                    KPJBA
040100080923     C                   movel     KPJBU         TRUL33DS
040200080923     C                   if        O33ERR = *zeros
040300080923     C                   z-add     O33NRF        VABNSP
040400080923     C                   z-add     O33NRF        VATNSP
040500080923     C                   else
040600080923     C                   SETON                                        31
040700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
040800080923     C                             + ' ' + 'VABNSP VATNSP'
040900080923     C                   endif
041000080923     C                   ELSE
041100121011     C*** non è indicata la posizione di questo dato sul file
041200121011     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
041300121011     C***                EXSR      CHKNUM
041400121011     C***                IF        PiInt=*on
041500121011     C***                Z-ADD     PiVal         VABNSP
041600121011     C***                Z-ADD     PiVal         VATNSP
041700121011     C***                ELSE
041800121011     C***                SETON                                        31
041900121011     C***                EVAL      x_vinmsg = %trimr(x_vinmsg)
042000121011     C***                          + ' ' + 'VABNSP VATNSP'
042100121011     C***                ENDIF
042200080923     C                   ENDIF
042300071003     C* NCL
042400121011     C                   EVAL      PiStr=%trim(%subst(vindta:303:4))
042500070928     C                   EXSR      CHKNUM
042600070928     C                   IF        PiInt=*on
042700070928     C                   Z-ADD     PiVal         VABNCL
042800070928     C                   ELSE
042900070928     C                   SETON                                        32
043000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043100070928     C                             + ' ' + 'VABNCL'
043200070928     C                   ENDIF
043300071003     C* PKB
043400121011     C                   EVAL      PiStr=%trim(%subst(vindta:307:8))
043500070928     C                   EXSR      CHKNUM
043600070928     C                   IF        PiNum=*on
043700121011     C* il dato ha 3 decimali
043800121011     C                   EVAL(H)   VABPKB = PiVal / 1000
043900070928     C                   ELSE
044000070928     C                   SETON                                        32
044100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200070928     C                             + ' ' + 'VABPKB'
044300070928     C                   ENDIF
044400121029     C* VLB
044500121029     C                   EVAL      PiStr=%trim(%subst(vindta:469:5))
044600121029     C                   EXSR      CHKNUM
044700121029     C                   IF        PiNum=*on
044800121029     C* il dato ha 3 decimali
044900121029     C                   EVAL(H)   VABVLB = PiVal / 1000
045000121029     C                   ELSE
045100121029     C                   SETON                                        32
045200121029     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045300121029     C                             + ' ' + 'VABVLB'
045400121029     C                   ENDIF
045500080923     C* CAS
045600121011     C* l'importo contrassegno ha 2 decimali e viene scritto senza separatore decimale
045700121011     C                   IF        %trim(%subst(vindta:315:15))<>*blanks
045800080923     C                             AND
045900121011     C                             %trim(%subst(vindta:315:15))<>*zeros
046000121011     C                             AND
046100121011     C                             %trim(%subst(vindta:315:15))<>
046200121011     C                              '              0'
046300121011     C                   EVAL      VABVCA = 'EUR'
046400070928     C                   MOVEL     '1'           FlgCAS
046500121011     C                   EVAL      PiStr=%trim(%subst(vindta:315:15))
046600070928     C                   EXSR      CHKNUM
046700070928     C                   IF        PiNum=*on
046800121011     C                   EVAL(H)   VABCAS = PiVal/100
046900070928     C                   ELSE
047000070928     C                   SETON                                        32
047100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047200070928     C                             + ' ' + 'VABCAS'
047300070928     C                   ENDIF
047400070928     C                   ENDIF
047500070730     C*
047600121106     C* Considerazioni finali su CAS
047700121106     C*
047800121106      * se c/assegno impostato
047900121106     C                   IF        VABCAS > 0
048000121122     C                   EVAL      VABCBO = '4'
048100121106      * metto BM come tipo incasso
048200121106     C                   EVAL      VABTIC = 'BM'
048300121106      * metto come tipo consegna particolare l'appuntamento
048400161207     C***                EVAL      VABTC1 = 'A'
048500121106     C                   ENDIF
048600020305     C*
048700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
048800011113     C                   EXSR      CHKIMPDIV
048900010202     C*
049000000801     C* Ebbene...
049100000801     C                   ADD       1             §CTRMO
049200070530     C                   IF        *in31 <> *zeros OR
049300070530     C                             *in32 <> *zeros
049400000801     C                   ADD       1             §CTRNO
049500020725     C                   EVAL      x_vinflg = '2'
049600000801     C                   ELSE
049700010201     C                   ADD       1             §CTROKVB
049800000801     C                   ENDIF
049900000801     C*
050000000801     C                   ENDSR
050100010201     C*----------------------------------------------------*
050200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
050300010201     C*----------------------------------------------------*
050400071003     C     WRIVAT_B      BEGSR
050500090204     C*
050600121122     C                   eval      vatCCM = vabCCM
050700090204     C                   eval      vatAAS = vabAAS
050800090204     C                   eval      vatLNP = vabLNP
050900090204     C                   eval      vatNRS = vabNRS
051000090204     C                   eval      vatNSP = vabNSP
051100010201     C*
051200101007     C* Valorizzo il buffer di scrittura del FIVAT
051300070928     C* - TIPO RECORD "B"
051400121121     C                   eval      VATNOT = %trim(%subst(vindta:474:17))
051500121121     C                   if        VATNOT <> *blanks
051600070928     C                   eval      VATTRC = 'B'
051700021113     C                   write     FIVAT000
051800020725     C                   endif
051900010201     C*
052000010201     C                   ENDSR
052100140304     C*----------------------------------------------------*
052200140304     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "I"
052300140304     C*----------------------------------------------------*
052400140304     C     WRIVAT_I      BEGSR
052500140306     C*
052600140306     C                   eval      vatCCM = vabCCM
052700140306     C                   eval      vatAAS = vabAAS
052800140306     C                   eval      vatLNP = vabLNP
052900140306     C                   eval      vatNRS = vabNRS
053000140306     C                   eval      vatNSP = vabNSP
053100140304     C*
053200140306     C* gli indirizzi email possono essere più d'uno divisi da ";"
053300140306     C* si prende solo il primo
053400140306     C                   eval      w70 = %trim(%subst(vindta:561:70))
053500140306     C                   EVAL      wPos=%scan(';':w70)
053600140306     C                   SELECT
053700140306     C*
053800140306     C* se il primo ";" darebbe un indirizzo lungo al massimo 35 char, creo solo il TRC = I
053900140306     C                   WHEN      wPos <> 0
054000140306     C                             and wPos < 37
054100140304     C* Valorizzo il buffer di scrittura del FIVAT
054200140304     C* - TIPO RECORD "I"
054300140306     C                   eval      VATNOT = %trim(%subst(vindta:561:wPos-1))
054400140304     C                   if        VATNOT <> *blanks
054500140304     C                   eval      VATTRC = 'I'
054600140304     C                   write     FIVAT000
054700140304     C                   endif
054800140306     C*
054900140306     C* se il primo ";" darebbe un indirizzo lungo più di 35 char, creo i TRC = I e J
055000140306     C                   WHEN      wPos <> 0
055100140306     C                             and wPos >= 37
055200140306     C* Valorizzo il buffer di scrittura del FIVAT
055300140306     C* - TIPO RECORD "I"
055400140306     C                   eval      VATNOT = %trim(%subst(vindta:561:35))
055500140306     C                   if        VATNOT <> *blanks
055600140306     C                   eval      VATTRC = 'I'
055700140306     C                   write     FIVAT000
055800140306     C                   endif
055900140306     C* - TIPO RECORD "J"
056000140306     C                   eval      VATNOT = %trim(%subst(vindta:561+35:wPos-36))
056100140306     C                   if        VATNOT <> *blanks
056200140306     C                   eval      VATTRC = 'J'
056300140306     C                   write     FIVAT000
056400140306     C                   endif
056500140306     C*
056600140306     C* se non trovo il primo ";" all'interno dei 70 char, creo i TRC = I e J
056700140306     C* sapendo che l'indirizzo non è completo
056800140306     C* oppure che è vuoto
056900140306     C* oppure che ce n'è uno solo e manca il ";"
057000140306     C                   OTHER
057100140306     C* Valorizzo il buffer di scrittura del FIVAT
057200140306     C* - TIPO RECORD "I"
057300140306     C                   eval      VATNOT = %trim(%subst(vindta:561:35))
057400140306     C                   if        VATNOT <> *blanks
057500140306     C                   eval      VATTRC = 'I'
057600140306     C                   write     FIVAT000
057700140306     C                   endif
057800140306     C* - TIPO RECORD "J"
057900140306     C                   eval      VATNOT = %trim(%subst(vindta:561+35:35))
058000140306     C                   if        VATNOT <> *blanks
058100140306     C                   eval      VATTRC = 'J'
058200140306     C                   write     FIVAT000
058300140306     C                   endif
058400140306     C*
058500140306     C                   ENDSL
058600140306     C*
058700140304     C                   ENDSR
058800071003     C*----------------------------------------------------*
058900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
059000071003     C*----------------------------------------------------*
059100071003     C     WRIVAT_E      BEGSR
059200090204     C*
059300090204     C                   eval      vatAAS = vabAAS
059400090204     C                   eval      vatLNP = vabLNP
059500090204     C                   eval      vatNRS = vabNRS
059600090204     C                   eval      vatNSP = vabNSP
059700151014     C*
059800151014     C* sviluppo i CHI SONO partendo dall'ID collo iniziale per quanti sono i colli in questo modo;:
059900160211     C* VABRMA (?) + <progressivo collo> (3) + VABNCL (3)
060000151014     C*
060100151014     C                   eval      VATTRC = 'E'
060200151014     C*
060300151014     C     1             DO        VABNCL        wColloPrg
060400160211     C                   EVAL      VATNOT =
060500160211     C                                      %trim(%subst(vindta:330:6)) +
060600151014     C                                      %editc(wColloPrg:'X') +
060700151014     C                                      %editc(%dec(VABNCL:3:0):'X')
060800071003     C*
060900071003     C                   write     FIVAT000
061000151014     C                   ENDDO
061100071003     C*
061200071003     C                   ENDSR
061300020904
061400020904
061500010202     C*----------------------------------------------------*
061600021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061700010202     C*----------------------------------------------------*
061800020305     C     PREVAT        BEGSR
061900010202     C*
062000021113     C* Compongo il nome del membro da dare al FIVATWWR
062100010202     C                   eval      parmbr = vlrhdl
062200010202     C                   movel     'M'           parmbr
062300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
062400010202     C                   eval      paropz = '1'
062500010202     C* Effettuo la chiamata al CLLE preposto
062600050128     C                   call(e)   'TITVVTC'
062700010202     C                   parm                    parccm
062800010202     C                   parm                    parmbr
062900010202     C                   parm                    paropz
063000010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
063100010202     C                   if        %error
063200010202     C                   movel     '1'           chkcall
063300010202     C                   else
063400010202     C                   movel     '0'           chkcall
063500010202     C                   endif
063600010202     C*
063700010202     C                   ENDSR
063800000801     C*----------------------------------------------------*
063900000801     C*  CONTROLLO NUMERICITA' CAMPI
064000000801     C*----------------------------------------------------*
064100000801     C     CHKNUM        BEGSR
064200081113     C*
064300081113     C                   IF        PiDecChr = *blanks
064400110524     C                   EVAL      PiDecChr = '.'
064500081113     C                   ENDIF
064600081113     C*
064700081113     C                   callp(e)  UBISNUM_Check(PiStr
064800081113     C                                          :PiDecChr
064900081113     C                                          :PiVal
065000081113     C                                          :PiNum
065100081113     C                                          :PiInt)
065200081113     C*
065300000801     C                   IF        %error
065400000801     C                   EVAL      PiInt=*off
065500000801     C                   ENDIF
065600000801     C*
065700000801     C                   ENDSR
065800000801     C***
065900000801
066000011113
066100011113     C*----------------------------------------------------*
066200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066300011113     C*----------------------------------------------------*
066400011113     C     CHKIMPDIV     BEGSR
066500011113     C*
066600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
066700011113     C                   Z-ADD     *zeros        wrkDec            9 9
066800011113     C*
066900011113     C* Come prima cosa effettuo considerazioni sulla divisa
067000011113     C                   IF        vabIAS > *zeros
067100011113     C                   IF        vabVAS <> 'EUR'
067200011113     C                   EVAL      vabVAS =  'ITL'
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C                   IF        vabCAS > *zeros
067700011113     C                   IF        vabVCA <> 'EUR'
067800011113     C                   EVAL      vabVCA =  'ITL'
067900011113     C                   ENDIF
068000011113     C                   ENDIF
068100011113     C*
068200011113     C                   IF        vabVMD > *zeros
068300020305     C                   IF        vabVAD <> 'EUR'
068400011113     C                   EVAL      vabVAD =  'ITL'
068500011113     C                   ENDIF
068600011113     C                   ENDIF
068700011113     C*
068800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
068900011113     C                   Z-ADD     vabIAS        wrkDec
069000011113     C                   IF        wrkDec > *zeros
069100011113     C                   IF        vabVAS = 'ITL'
069200011113     C                   EVAL      vabIAS = *zeros
069300011113     C                   ENDIF
069400011113     C                   ENDIF
069500011113     C*
069600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
069700011113     C                   Z-ADD     vabCAS        wrkDec
069800011113     C                   IF        wrkDec > *zeros
069900011113     C                   IF        vabVCA = 'ITL'
070000011113     C                   EVAL      vabCAS = *zeros
070100011113     C                   ENDIF
070200011113     C                   ENDIF
070300011113     C*
070400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
070500011113     C                   Z-ADD     vabVMD        wrkDec
070600011113     C                   IF        wrkDec > *zeros
070700011113     C                   IF        vabVAD = 'ITL'
070800011113     C                   EVAL      vabVMD = *zeros
070900011113     C                   ENDIF
071000011113     C                   ENDIF
071100011113     C*
071200011113     C                   ENDSR
071300011113     C***
071400011113
071500011113
071600000801
071700000801
071800990920      /TITLE Invio dei dati al punto operativo.
071900010202     C     invio         BEGSR
072000990920     C*
072100021113     C* 1° invio FIVAT
072200010201     C                   reset                   dscmz
072300010201     C                   move      vlrpoi        cmzdst
072400021113     C                   eval      cmzfld = 'FIVATWWR'
072500010201     C                   eval      cmzmbd = vlrhdl
072600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
072700021009     C***                if        prmfir = *blanks
072800021113     C                   eval      cmzfla = 'FIVAT00F'
072900021113     C                   eval      cmzmba = 'FIVAT00F'
073000021009     C***                else
073100021009     C***                eval      cmzfla = prmfir
073200021009     C***                eval      cmzmba = prmfir
073300021009     C***                endif
073400010201     C                   eval      cmznrr = *zeros
073500020305     C                   move      §ctrokvt      cmznrr
073600021018     C                   eval      cmzlba = vlrfl1
073700010201     C                   call(e)   'TIS711C'
073800010201     C                   parm                    dscmz
073900010201     C                   parm      *blanks       esito
074000010205     C                   if        %error
074100010205     C                             or cmzerr = '1'
074200010205     C                             or esito  = '1'
074300010205     C                   eval      wrkesito = '3'
074400010205     C                   else
074500010201     C*
074600021113     C* 2° invio FIVAB
074700010201     C                   reset                   dscmz
074800010201     C                   move      vlrpoi        cmzdst
074900010201     C                   eval      cmzfld = vlrfou
075000010201     C                   eval      cmzmbd = vlrhdl
075100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075200021009     C***                if        prmfir = *blanks
075300021113     C                   eval      cmzfla = 'FIVAB00F'
075400021113     C                   eval      cmzmba = 'FIVAB00F'
075500021009     C***                else
075600021009     C***                eval      cmzfla = prmfir
075700021009     C***                eval      cmzmba = prmfir
075800021009     C***                endif
075900010201     C                   eval      cmznrr = *zeros
076000010201     C                   move      §ctrokvb      cmznrr
076100021018     C                   eval      cmzlba = vlrfl1
076200010201     C                   call(e)   'TIS711C'
076300010201     C                   parm                    dscmz
076400010201     C                   parm      *blanks       esito
076500010201     C                   if        %error
076600010201     C                             or cmzerr = '1'
076700010201     C                             or esito  = '1'
076800010201     C                   eval      wrkesito = '3'
076900010201     C                   endif
077000010205     C                   endif
077100990920     C*
077200000613     C                   ENDSR
077300000613     C***
077400070411
077500070411     C     *pssr         BEGSR
077600070411     C*
077700070411     C                   if        %open(tivin00r)
077800070411     C                   close     tivin00r
077900070411     C                   endif
078000070411     C                   if        %open(fivabwwr)
078100070411     C                   close     fivabwwr
078200070411     C                   endif
078300070411     C                   if        %open(fivatwwr)
078400070411     C                   close     fivatwwr
078500070411     C                   endif
078600070411     C*
078700070411     C* Effettuo la chiamata al CLLE preposto
078800070411     C                   call(e)   'TITVVTC'
078900070411     C                   parm                    parccm
079000070411     C                   parm                    parmbr
079100070411     C                   parm      '2'           paropz
079200070411     C*
079300070411     C                   eval      wrkesito = '2'
079400070411     C*
079500070411     C                   seton                                        LR
079600070411     C*
079700070411     C                   ENDSR     '*CANCL'
079800070411     C***
079900070411
080000990910
080100000613     C     *inzsr        BEGSR
080200990910     C*
080300990910     C     *entry        plist
080400990920     C                   parm                    tivlrds
080500990921     C                   parm      wrkesito      esito
080600000724     C                   parm                    prmlit
080700000710     C                   parm                    prmfir
080800000613     C*
080900000830     C* CALCOLA LA DATA CORRENTE
081000100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
081100100324     C                   eval      datcor = %dec(%date() : *ISO)
081200000830     C*
081300000613     C                   ENDSR
081400000613     C***
